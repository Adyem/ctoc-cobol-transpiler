int cblc_generate_cobol(const t_cblc_translation_unit *unit, char **out_text)
{
    t_cobol_text_builder builder;
    char line[256];
    size_t index;
    size_t entry_index;
    const t_cblc_function *entry_function;

    if (!unit || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    if (cblc_translation_unit_calls_are_resolved(unit) != FT_SUCCESS)
        return (FT_FAILURE);
    entry_index = unit->entry_function_index;
    if (entry_index == static_cast<size_t>(-1) || entry_index >= unit->function_count)
        entry_index = 0;
    entry_function = NULL;
    if (unit->function_count > 0 && entry_index < unit->function_count)
        entry_function = &unit->functions[entry_index];
    cobol_text_builder_init(&builder);
    if (cobol_text_builder_append_line(&builder, "       IDENTIFICATION DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (std::snprintf(line, sizeof(line), "       PROGRAM-ID. %s.", unit->program_name) < 0)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       ENVIRONMENT DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       DATA DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       WORKING-STORAGE SECTION.") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit->copy_include_count)
    {
        char copy_name[TRANSPILE_IDENTIFIER_MAX];
        size_t name_index;

        ft_strlcpy(copy_name, unit->copy_includes[index].name, sizeof(copy_name));
        name_index = 0;
        while (copy_name[name_index] != '\0')
        {
            if (copy_name[name_index] >= 'a' && copy_name[name_index] <= 'z')
                copy_name[name_index] = static_cast<char>(copy_name[name_index] - 'a' + 'A');
            name_index += 1;
        }
        if (std::snprintf(line, sizeof(line), "       COPY %s.", copy_name) < 0)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    index = 0;
    while (index < unit->data_count)
    {
        if (unit->data_items[index].is_alias || std::strchr(unit->data_items[index].source_name, '.')
            || std::strstr(unit->data_items[index].source_name, "->")
            || cblc_is_entry_parameter_item(unit, &unit->data_items[index])
            || cblc_is_entry_return_item(unit, &unit->data_items[index]))
        {
            if (cblc_string_item_is_member_storage(&unit->data_items[index])
                && (cblc_string_item_uses_dynamic_storage(&unit->data_items[index])
                    || cblc_string_item_array_uses_dynamic_storage(&unit->data_items[index])))
            {
                if (std::snprintf(line, sizeof(line), "       01 %s-BUF BASED PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            index += 1;
            continue ;
        }
        if (unit->data_items[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (unit->data_items[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu) OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length,
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (unit->data_items[index].initializer_text[0] == '\'')
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu) VALUE \"%c\".",
                            unit->data_items[index].cobol_name,
                            unit->data_items[index].length,
                            unit->data_items[index].initializer_text[1]) < 0)
                        goto cleanup;
                }
                else if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu) VALUE %s.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length,
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu).",
                    unit->data_items[index].cobol_name,
                    unit->data_items[index].length) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_INT)
        {
            if (unit->data_items[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9) OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9) VALUE %s.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9).",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_STRING)
        {
            if (unit->data_items[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s.",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            if (std::snprintf(line, sizeof(line), "          05 %s-LEN PIC 9(4) COMP VALUE %zu.",
                    unit->data_items[index].cobol_name,
                    unit->data_items[index].is_const
                        ? unit->data_items[index].initializer_length
                        : 0) < 0)
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-BUF PIC X(%zu) VALUE %s.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length,
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (cblc_string_item_array_uses_dynamic_storage(&unit->data_items[index]))
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-CAP PIC 9(4) COMP VALUE 0.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line),
                        "          05 %s-PTR USAGE POINTER VALUE NULL.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line), "       01 %s-BUF BASED PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (cblc_string_item_uses_dynamic_storage(&unit->data_items[index]))
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-CAP PIC 9(4) COMP VALUE 0.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line),
                        "          05 %s-PTR USAGE POINTER VALUE NULL.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line), "       01 %s-BUF BASED PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-BUF PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            index += 1;
            continue ;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            if (cblc_emit_struct_instance(unit, &unit->data_items[index], &builder) != FT_SUCCESS)
                goto cleanup;
            index += 1;
            continue ;
        }
        else if (cblc_data_kind_is_pointer(unit->data_items[index].kind))
        {
            if (std::snprintf(line, sizeof(line), "       01 %s USAGE POINTER VALUE NULL.",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
        }
        else
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    if (cblc_unit_has_pointer_data(unit))
    {
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-CURSOR USAGE POINTER VALUE NULL.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-OFFSET PIC S9(9) VALUE 0.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-CHAR BASED PIC X.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT BASED PIC S9(9).") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT-PTR BASED USAGE POINTER.") != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < unit->data_count)
        {
            if (unit->data_items[index].kind == CBLC_DATA_KIND_STRUCT_POINTER)
            {
                char based_name[TRANSPILE_IDENTIFIER_MAX];
                const t_cblc_struct_type *type;

                if (cblc_build_struct_pointer_based_name(unit->data_items[index].cobol_name,
                        based_name, sizeof(based_name)) != FT_SUCCESS)
                    goto cleanup;
                type = cblc_find_struct_type(unit, unit->data_items[index].struct_type_name);
                if (!type)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line), "       01 %s BASED.",
                        based_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (cblc_emit_struct_fields(unit, type, based_name, 5, &builder)
                    != FT_SUCCESS)
                    goto cleanup;
            }
            index += 1;
        }
    }
    if (entry_function && cblc_is_external_entry_function(unit)
        && (entry_function->parameter_count > 0
            || entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID))
    {
        if (cobol_text_builder_append_line(&builder, "       LINKAGE SECTION.") != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < entry_function->parameter_count)
        {
            if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9).",
                        entry_function->parameters[index].cobol_name) < 0)
                    goto cleanup;
            }
            else if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER
                || entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER
                || entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER
                || entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s USAGE POINTER.",
                        entry_function->parameters[index].cobol_name) < 0)
                    goto cleanup;
            }
            else
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            index += 1;
        }
        if (entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID)
        {
            const t_cblc_data_item *return_item;

            if (entry_function->return_item_index < 0
                || static_cast<size_t>(entry_function->return_item_index) >= unit->data_count)
                goto cleanup;
            return_item = &unit->data_items[entry_function->return_item_index];
            if (return_item->kind == CBLC_DATA_KIND_INT)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9).",
                        entry_function->return_cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (return_item->kind == CBLC_DATA_KIND_STRUCT)
            {
                if (cblc_emit_struct_instance(unit, return_item, &builder) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (cblc_data_kind_is_pointer(return_item->kind))
            {
                if (std::snprintf(line, sizeof(line), "       01 %s USAGE POINTER.",
                        entry_function->return_cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else
                goto cleanup;
        }
        ft_strlcpy(line, "       PROCEDURE DIVISION USING", sizeof(line));
        if (entry_function->parameter_count == 0
            && entry_function->return_kind == CBLC_FUNCTION_RETURN_VOID)
            goto cleanup;
        index = 0;
        while (index < entry_function->parameter_count)
        {
            if (std::strlen(line) + 1
                + std::strlen(entry_function->parameters[index].cobol_name)
                + 1 >= sizeof(line))
                goto cleanup;
            ft_strlcat(line, " ", sizeof(line));
            ft_strlcat(line, entry_function->parameters[index].cobol_name, sizeof(line));
            index += 1;
        }
        if (entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID)
        {
            if (std::strlen(line) + 1
                + std::strlen(entry_function->return_cobol_name) + 1 >= sizeof(line))
                goto cleanup;
            ft_strlcat(line, " ", sizeof(line));
            ft_strlcat(line, entry_function->return_cobol_name, sizeof(line));
        }
        if (std::strlen(line) + 2 >= sizeof(line))
            goto cleanup;
        ft_strlcat(line, ".", sizeof(line));
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
    }
    else
    {
        if (cobol_text_builder_append_line(&builder, "       PROCEDURE DIVISION.") != FT_SUCCESS)
            goto cleanup;
    }
    if (unit->function_count > 0)
    {
        if (cblc_emit_function(unit, &unit->functions[entry_index], &builder, 1) != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < unit->function_count)
        {
            if (index != entry_index)
            {
                if (cblc_emit_function(unit, &unit->functions[index], &builder, 0) != FT_SUCCESS)
                    goto cleanup;
            }
            index += 1;
        }
    }
    else
    {
        if (cobol_text_builder_append_line(&builder, "MAIN.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, "           STOP RUN.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, "") != FT_SUCCESS)
            goto cleanup;
    }
    if (std::snprintf(line, sizeof(line), "       END PROGRAM %s.",
            unit->program_name) < 0)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "") != FT_SUCCESS)
        goto cleanup;
    *out_text = builder.data;
    builder.data = NULL;
cleanup:
    cobol_text_builder_dispose(&builder);
    if (!*out_text)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

#include "src/cblc/transpiler_cblc_context_integration.inc"
