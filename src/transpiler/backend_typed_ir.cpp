#include "backend_typed_ir.hpp"

#include <algorithm>
#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <functional>
#include <limits>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace cblc::backend
{

struct typed_storage_binding
{
    std::string cobol_name;
    value_id value = 0;
    place_id place = 0;
};

struct typed_receiver_field_binding
{
    std::string cobol_name;
    std::vector<place_projection> projections;
};

static void trim_expression(const char **begin, const char **end);

static bool fail(verification_diagnostic *diagnostic, verification_error code,
    operation_id operation, const char *message)
{
    if (diagnostic)
    {
        diagnostic->code = code;
        diagnostic->operation = operation;
        diagnostic->message = message;
    }
    return false;
}

static type_id scalar_type_id(t_cblc_data_kind kind)
{
    if (kind == CBLC_DATA_KIND_CHAR || kind == CBLC_DATA_KIND_INT
        || kind == CBLC_DATA_KIND_DOUBLE)
        return static_cast<type_id>(kind) + 1U;
    return 0;
}

static type_id canonical_class_type_id(const char *canonical_name)
{
    const std::uint64_t offset_basis = 14695981039346656037ULL;
    const std::uint64_t prime = 1099511628211ULL;
    const std::uint32_t first_class_id = 0x10001U;
    const std::uint64_t class_id_count =
        static_cast<std::uint64_t>(UINT32_MAX) - first_class_id + 1ULL;
    std::uint64_t hash = offset_basis;

    if (!canonical_name || canonical_name[0] == '\0')
        return 0;
    for (const unsigned char *cursor =
            reinterpret_cast<const unsigned char *>(canonical_name);
        *cursor != '\0'; ++cursor)
    {
        hash ^= *cursor;
        hash *= prime;
    }
    return static_cast<type_id>(first_class_id + hash % class_id_count);
}

static type_id class_type_id(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    if (!unit || !item || item->kind != CBLC_DATA_KIND_STRUCT)
        return 0;
    for (std::size_t index = 0; index < unit->struct_type_count; ++index)
        if (unit->struct_types[index].is_class
            && std::strcmp(unit->struct_types[index].source_name,
                item->struct_type_name) == 0)
            return canonical_class_type_id(unit->struct_types[index].source_name);
    return 0;
}

static type_id declared_struct_type_id(const t_cblc_translation_unit *unit,
    const char *name)
{
    if (!unit || !name)
        return 0;
    for (std::size_t index = 0; index < unit->struct_type_count; ++index)
        if (std::strcmp(unit->struct_types[index].source_name, name) == 0)
            return canonical_class_type_id(unit->struct_types[index].source_name);
    return 0;
}

static type_id canonical_pointer_type_id(const char *pointee_name)
{
    const std::uint64_t offset_basis = 14695981039346656037ULL;
    const std::uint64_t prime = 1099511628211ULL;
    std::uint64_t hash = offset_basis;
    const char *prefix = "pointer<";
    const char *parts[3];
    if (!pointee_name || pointee_name[0] == '\0')
        return 0;
    parts[0] = prefix;
    parts[1] = pointee_name;
    parts[2] = ">";
    for (const char *part : parts)
        for (const unsigned char *cursor =
                reinterpret_cast<const unsigned char *>(part);
            *cursor != '\0'; ++cursor)
        {
            hash ^= *cursor;
            hash *= prime;
        }
    const type_id result = static_cast<type_id>(hash);
    return result != 0 ? result : 1;
}

static bool typed_type_id_is_available(const typed_function *function,
    type_id id, const std::string &canonical_name,
    verification_diagnostic *diagnostic)
{
    if (!function || id == 0 || canonical_name.empty())
        return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
            "canonical typed type identity is incomplete");
    for (const typed_type &candidate : function->types)
        if (candidate.id == id && candidate.canonical_name != canonical_name)
            return fail(diagnostic,
                verification_error::invalid_type_or_projection, 0,
                "canonical typed type ID collides with a different type");
    return true;
}

static bool append_typed_pointer_type(const t_cblc_translation_unit *unit,
    typed_function *function, const char *pointee_name,
    verification_diagnostic *diagnostic)
{
    if (!unit || !function || !pointee_name || pointee_name[0] == '\0')
        return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
            "typed pointer construction requires a concrete pointee name");
    const type_id pointee = declared_struct_type_id(unit, pointee_name);
    if (pointee == 0)
        return fail(diagnostic, verification_error::unsupported_legacy_type, 0,
            "typed pointer pointee is not a canonical declared class type");
    const std::string canonical_name = std::string(pointee_name) + "*";
    const type_id pointer_id = canonical_pointer_type_id(pointee_name);
    if (!typed_type_id_is_available(function, pointer_id, canonical_name,
            diagnostic))
        return false;
    const auto existing = std::find_if(function->types.begin(),
        function->types.end(), [&](const typed_type &candidate)
        {
            return candidate.kind == typed_type_kind::pointer
                && candidate.canonical_name == canonical_name;
        });
    if (existing != function->types.end())
    {
        if (existing->id != pointer_id || existing->pointee_type != pointee)
            return fail(diagnostic,
                verification_error::invalid_type_or_projection, 0,
                "canonical pointer type has inconsistent identity or pointee");
        return true;
    }
    typed_type pointer_type;
    pointer_type.id = pointer_id;
    pointer_type.kind = typed_type_kind::pointer;
    pointer_type.canonical_name = canonical_name;
    pointer_type.pointee_type = pointee;
    function->types.push_back(std::move(pointer_type));
    return true;
}

static bool class_requires_lifecycle(const t_cblc_translation_unit *unit,
    const char *type_name, std::size_t depth = 0)
{
    if (!unit || !type_name || depth >= 64)
        return true;
    for (std::size_t type_index = 0; type_index < unit->struct_type_count;
        ++type_index)
    {
        const t_cblc_struct_type &type = unit->struct_types[type_index];
        if (std::strcmp(type.source_name, type_name) != 0)
            continue;
        /* An available implicit default constructor is not itself a
         * non-trivial lifetime operation. Track classes with explicit
         * construction/copy behavior or destruction work; otherwise ordinary
         * method-bearing value classes have no typed construct operation to
         * pair with the owned-object lifetime state. */
        if (type.constructor_count != 0
            || type.has_copy_constructor || type.has_copy_assignment
            || type.has_destructor || type.has_destructor_definition)
            return true;
        for (std::size_t field_index = 0; field_index < type.field_count;
            ++field_index)
        {
            const t_cblc_struct_field &field = type.fields[field_index];
            if (!field.is_borrowed
                && (field.kind == CBLC_DATA_KIND_VOID_POINTER
                    || field.kind == CBLC_DATA_KIND_CHAR_POINTER
                    || field.kind == CBLC_DATA_KIND_INT_POINTER
                    || field.kind == CBLC_DATA_KIND_STRUCT_POINTER
                    || field.kind == CBLC_DATA_KIND_INT_POINTER_POINTER))
                return true;
            if (field.kind == CBLC_DATA_KIND_STRUCT && !field.is_borrowed
                && class_requires_lifecycle(unit, field.struct_type_name,
                    depth + 1))
                return true;
        }
        return false;
    }
    return true;
}

static bool populate_typed_type_table(const t_cblc_translation_unit *unit,
    typed_function *function, verification_diagnostic *diagnostic)
{
    if (!unit || !function)
        return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
            "typed type-table construction requires a translation unit and function");
    for (int kind = static_cast<int>(CBLC_DATA_KIND_CHAR);
        kind <= static_cast<int>(CBLC_DATA_KIND_INT_POINTER_POINTER); ++kind)
    {
        typed_type primitive;
        primitive.id = static_cast<type_id>(kind + 1);
        primitive.kind = typed_type_kind::primitive;
        primitive.canonical_name = "cblc-data-kind-" + std::to_string(kind);
        function->types.push_back(std::move(primitive));
    }
    for (std::size_t type_index = 0; type_index < unit->struct_type_count;
        ++type_index)
    {
        const t_cblc_struct_type &source_type = unit->struct_types[type_index];
        typed_type concrete_type;
        concrete_type.id = canonical_class_type_id(source_type.source_name);
        if (concrete_type.id == 0)
            return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                "class type has no canonical nonzero type ID");
        concrete_type.kind = typed_type_kind::class_type;
        concrete_type.canonical_name = source_type.source_name;
        if (!typed_type_id_is_available(function, concrete_type.id,
                concrete_type.canonical_name, diagnostic))
            return false;
        concrete_type.fields.reserve(source_type.field_count);
        for (std::size_t field_index = 0; field_index < source_type.field_count;
            ++field_index)
        {
            const t_cblc_struct_field &source_field = source_type.fields[field_index];
            typed_field field;
            field.member_id = static_cast<std::uint32_t>(field_index + 1);
            field.name = source_field.source_name;
            field.array_extent = source_field.array_count;
            field.is_borrowed = source_field.is_borrowed != 0;
            field.is_dependent = source_field.is_template_parameter != 0;
            if (!field.is_dependent)
            {
                if (source_field.kind == CBLC_DATA_KIND_STRUCT_POINTER)
                {
                    if (!append_typed_pointer_type(unit, function,
                            source_field.struct_type_name, diagnostic))
                        return false;
                    field.type = canonical_pointer_type_id(
                        source_field.struct_type_name);
                }
                else
                    field.type = source_field.kind == CBLC_DATA_KIND_STRUCT
                        ? declared_struct_type_id(unit, source_field.struct_type_name)
                        : static_cast<type_id>(source_field.kind) + 1U;
                if (field.type == 0)
                    return fail(diagnostic,
                        verification_error::invalid_type_or_projection, 0,
                        "class field has no canonical resolved type ID");
            }
            concrete_type.fields.push_back(std::move(field));
        }
        function->types.push_back(std::move(concrete_type));
    }
    for (std::size_t item_index = 0; item_index < unit->data_count;
        ++item_index)
    {
        const t_cblc_data_item &item = unit->data_items[item_index];
        if (item.kind == CBLC_DATA_KIND_STRUCT_POINTER
            && !item.is_template_item
            && declared_struct_type_id(unit, item.struct_type_name) != 0
            && !append_typed_pointer_type(unit, function,
                item.struct_type_name, diagnostic))
            return false;
    }
    for (std::size_t type_index = 0; type_index < unit->struct_type_count;
        ++type_index)
    {
        const t_cblc_struct_type &type = unit->struct_types[type_index];
        for (std::size_t method_index = 0; method_index < type.method_count;
            ++method_index)
        {
            const t_cblc_method &method = type.methods[method_index];
            if (method.return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER
                && declared_struct_type_id(unit, method.return_type_name) != 0
                && !append_typed_pointer_type(unit, function,
                    method.return_type_name, diagnostic))
                return false;
            for (std::size_t parameter_index = 0;
                parameter_index < method.parameter_count; ++parameter_index)
                if (method.parameters[parameter_index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER
                    && declared_struct_type_id(unit,
                        method.parameters[parameter_index].type_name) != 0
                    && !append_typed_pointer_type(unit, function,
                        method.parameters[parameter_index].type_name, diagnostic))
                    return false;
        }
        for (std::size_t constructor_index = 0;
            constructor_index < type.constructor_count; ++constructor_index)
        {
            const t_cblc_constructor &constructor =
                type.constructors[constructor_index];
            for (std::size_t parameter_index = 0;
                parameter_index < constructor.parameter_count; ++parameter_index)
                if (constructor.parameters[parameter_index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER
                    && declared_struct_type_id(unit,
                        constructor.parameters[parameter_index].type_name) != 0
                    && !append_typed_pointer_type(unit, function,
                        constructor.parameters[parameter_index].type_name,
                        diagnostic))
                    return false;
        }
    }
    return true;
}

static const t_cblc_data_item *find_class_item(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const char *name)
{
    if (!unit || !function || !name)
        return nullptr;
    std::size_t index = unit->data_count;
    while (index > 0)
    {
        const t_cblc_data_item &item = unit->data_items[--index];
        bool is_parameter_storage = false;
        const bool name_matches = std::strcmp(item.source_name, name) == 0
            || std::strcmp(item.cobol_name, name) == 0
            || std::strcmp(item.local_source_name, name) == 0;
        for (std::size_t parameter_index = 0;
            parameter_index < function->parameter_count; ++parameter_index)
            if (function->parameters[parameter_index].actual_source_name[0] != '\0'
                && std::strcmp(function->parameters[parameter_index].actual_source_name,
                    item.source_name) == 0)
            {
                is_parameter_storage = true;
                break;
            }
        const bool item_is_visible = item.is_function_local
            ? std::strcmp(item.owner_function_name, function->source_name) == 0
            : (item.is_active || is_parameter_storage);
        if ((!item_is_visible && !(item.is_alias && name_matches))
            || item.is_shadowed || class_type_id(unit, &item) == 0)
            continue;
        if (name_matches)
        {
            if (!item.is_alias)
                return &item;
            const type_id alias_type = class_type_id(unit, &item);
            for (std::size_t actual_index = unit->data_count;
                actual_index > 0;)
            {
                const t_cblc_data_item &actual =
                    unit->data_items[--actual_index];
                bool actual_is_parameter = false;
                for (std::size_t parameter_index = 0;
                    parameter_index < function->parameter_count;
                    ++parameter_index)
                    if (function->parameters[parameter_index]
                            .actual_source_name[0] != '\0'
                        && std::strcmp(function->parameters[parameter_index]
                                .actual_source_name, actual.source_name) == 0)
                    {
                        actual_is_parameter = true;
                        break;
                    }
                const bool actual_is_visible = actual.is_function_local
                    ? std::strcmp(actual.owner_function_name,
                        function->source_name) == 0
                    : (actual.is_active || actual_is_parameter);
                if (!actual.is_alias && !actual.is_shadowed && actual_is_visible
                    && class_type_id(unit, &actual) == alias_type
                    && std::strcmp(actual.cobol_name, item.cobol_name) == 0)
                    return &actual;
            }
        }
    }
    return nullptr;
}

static const t_cblc_data_item *find_scalar_item(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const char *name);

static const t_cblc_data_item *find_class_receiver_item(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const char *name)
{
    if (!unit || !function || !name)
        return nullptr;
    for (std::size_t index = unit->data_count; index > 0;)
    {
        const t_cblc_data_item &item = unit->data_items[--index];
        bool is_named_parameter = false;
        const bool name_matches = std::strcmp(item.source_name, name) == 0
            || std::strcmp(item.cobol_name, name) == 0
            || std::strcmp(item.local_source_name, name) == 0;
        for (std::size_t parameter_index = 0;
            parameter_index < function->parameter_count; ++parameter_index)
        {
            const t_cblc_parameter &parameter =
                function->parameters[parameter_index];
            if (std::strcmp(parameter.source_name, name) == 0
                || std::strcmp(parameter.actual_source_name, name) == 0)
                if ((parameter.actual_source_name[0] != '\0'
                        && std::strcmp(parameter.actual_source_name,
                            item.source_name) == 0)
                    || (parameter.cobol_name[0] != '\0'
                        && std::strcmp(parameter.cobol_name,
                            item.cobol_name) == 0))
                {
                    is_named_parameter = true;
                    break;
                }
        }
        const bool owned_class = item.kind == CBLC_DATA_KIND_STRUCT
            && class_type_id(unit, &item) != 0;
        const bool class_pointer = item.kind == CBLC_DATA_KIND_STRUCT_POINTER
            && declared_struct_type_id(unit, item.struct_type_name) != 0;
        const bool indexed_class_borrow = item.is_alias && item.is_borrowed
            && item.kind == CBLC_DATA_KIND_STRUCT_POINTER
            && std::strchr(item.source_name, '[') != nullptr;
        const bool indexed_borrow_scope_visible = indexed_class_borrow
            && item.scope_id < unit->scope_count
            && std::strcmp(unit->scopes[item.scope_id].owner_function_name,
                function->source_name) == 0;
        const bool item_has_function_owner = item.owner_function_name[0] != '\0';
        const bool item_is_visible = item_has_function_owner
            ? std::strcmp(item.owner_function_name, function->source_name) == 0
            : (item.is_active || indexed_borrow_scope_visible);
        if ((!owned_class && !class_pointer)
            || (!item_is_visible && !(item.is_alias && name_matches))
            || item.is_shadowed || (item.is_alias && !indexed_class_borrow))
            continue;
        if (name_matches || is_named_parameter)
            return &item;
    }
    /* Function-local class objects are internally mangled (e.g.
     * main__value), while their source spelling is represented by an alias.
     * Resolve that alias back to the owned storage before typed lifecycle
     * lowering, just as scalar and ordinary class-argument lookup do. */
    for (std::size_t index = unit->data_count; index > 0;)
    {
        const t_cblc_data_item &alias = unit->data_items[--index];
        const bool name_matches = std::strcmp(alias.source_name, name) == 0
            || std::strcmp(alias.cobol_name, name) == 0
            || std::strcmp(alias.local_source_name, name) == 0;
        const bool is_class = alias.kind == CBLC_DATA_KIND_STRUCT
            && class_type_id(unit, &alias) != 0;
        const bool is_pointer = alias.kind == CBLC_DATA_KIND_STRUCT_POINTER
            && declared_struct_type_id(unit, alias.struct_type_name) != 0;
        const bool alias_has_function_scope = alias.scope_id < unit->scope_count
            && unit->scopes[alias.scope_id].owner_function_name[0] != '\0';
        const bool alias_scope_is_visible = !alias_has_function_scope
            || std::strcmp(unit->scopes[alias.scope_id].owner_function_name,
                function->source_name) == 0;

        if (!alias.is_alias || !name_matches
            || (alias.is_shadowed && !alias_has_function_scope)
            || (!is_class && !is_pointer) || !alias_scope_is_visible)
            continue;
        const type_id alias_type = is_class
            ? class_type_id(unit, &alias)
            : canonical_pointer_type_id(alias.struct_type_name);
        for (std::size_t actual_index = unit->data_count; actual_index > 0;)
        {
            const t_cblc_data_item &actual = unit->data_items[--actual_index];
            const bool actual_is_class = actual.kind == CBLC_DATA_KIND_STRUCT
                && class_type_id(unit, &actual) != 0;
            const bool actual_is_pointer =
                actual.kind == CBLC_DATA_KIND_STRUCT_POINTER
                && declared_struct_type_id(unit, actual.struct_type_name) != 0;
            const type_id actual_type = actual_is_class
                ? class_type_id(unit, &actual)
                : (actual_is_pointer
                    ? canonical_pointer_type_id(actual.struct_type_name) : 0);
            const bool actual_has_function_owner =
                actual.owner_function_name[0] != '\0';
            const bool actual_is_visible = actual_has_function_owner
                ? std::strcmp(actual.owner_function_name,
                    function->source_name) == 0
                : actual.is_active;

            if (!actual.is_alias && !actual.is_shadowed && actual_is_visible
                && actual_type == alias_type
                && actual.scope_id == alias.scope_id
                && std::strcmp(actual.cobol_name, alias.cobol_name) == 0)
                return &actual;
        }
    }
    return nullptr;
}

static bool split_single_indexed_name(const char *expression,
    std::string *base_name, std::string *index_expression)
{
    if (!expression || !base_name || !index_expression)
        return false;
    const char *begin = expression;
    const char *end = expression + std::strlen(expression);
    trim_expression(&begin, &end);
    const char *open = static_cast<const char *>(std::memchr(begin, '[',
        static_cast<std::size_t>(end - begin)));
    if (!open || open == begin || end == begin || end[-1] != ']')
        return false;
    const char *close = end - 1;
    if (std::memchr(open + 1, '[', static_cast<std::size_t>(close - open - 1))
        || std::memchr(open + 1, ']', static_cast<std::size_t>(close - open - 1)))
        return false;
    const char *base_begin = begin;
    const char *base_end = open;
    const char *index_begin = open + 1;
    const char *index_end = close;
    trim_expression(&base_begin, &base_end);
    trim_expression(&index_begin, &index_end);
    if (base_begin == base_end || index_begin == index_end
        || base_end != open)
        return false;
    for (const char *cursor = base_begin; cursor < base_end; ++cursor)
        if (!((*cursor >= 'a' && *cursor <= 'z')
                || (*cursor >= 'A' && *cursor <= 'Z')
                || (*cursor >= '0' && *cursor <= '9') || *cursor == '_'))
            return false;
    base_name->assign(base_begin, base_end);
    index_expression->assign(index_begin, index_end);
    return true;
}

static const t_cblc_struct_type *find_class_type(
    const t_cblc_translation_unit *unit, const char *name)
{
    if (!unit || !name)
        return nullptr;
    for (std::size_t index = 0; index < unit->struct_type_count; ++index)
        if (std::strcmp(unit->struct_types[index].source_name, name) == 0)
            return &unit->struct_types[index];
    return nullptr;
}

static bool resolve_scalar_member_expression(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const char *expression, const t_cblc_data_item **root_item,
    std::vector<place_projection> *projections, type_id *result_type,
    access_mode *result_access)
{
    if (!unit || !function || !expression || !root_item || !projections
        || !result_type || !result_access)
        return false;
    *root_item = nullptr;
    projections->clear();
    *result_type = 0;
    const char *cursor = expression;
    while (*cursor == ' ' || *cursor == '\t' || *cursor == '\r'
        || *cursor == '\n')
        ++cursor;
    const char *root_begin = cursor;
    while (*cursor != '\0' && *cursor != '.'
        && !(cursor[0] == '-' && cursor[1] == '>')
        && *cursor != ' ' && *cursor != '\t' && *cursor != '\r'
        && *cursor != '\n')
        ++cursor;
    const std::string root_name(root_begin, cursor);
    const t_cblc_data_item *root = find_class_receiver_item(unit, function,
        root_name.c_str());
    if (!root)
        return false;
    access_mode access = root->is_const
        ? access_mode::read_only : access_mode::read_write;
    std::string current_type_name = root->struct_type_name;
    bool current_is_pointer = root->kind == CBLC_DATA_KIND_STRUCT_POINTER;
    bool saw_member = false;
    while (true)
    {
        while (*cursor == ' ' || *cursor == '\t' || *cursor == '\r'
            || *cursor == '\n')
            ++cursor;
        if (*cursor == '\0')
            break;
        bool pointer_access = false;
        if (cursor[0] == '-' && cursor[1] == '>')
        {
            pointer_access = true;
            cursor += 2;
        }
        else if (*cursor == '.')
            ++cursor;
        else
            return false;
        if (pointer_access != current_is_pointer)
            return false;
        if (current_is_pointer)
        {
            const type_id pointee = declared_struct_type_id(unit,
                current_type_name.c_str());
            if (pointee == 0)
                return false;
            projections->push_back({place_projection::kind::dereference, 0,
                pointee});
            current_is_pointer = false;
        }
        const char *member_begin = cursor;
        while (*cursor != '\0' && *cursor != '.'
            && !(cursor[0] == '-' && cursor[1] == '>')
            && *cursor != ' ' && *cursor != '\t' && *cursor != '\r'
            && *cursor != '\n')
            ++cursor;
        if (cursor == member_begin)
            return false;
        const std::string member_name(member_begin, cursor);
        const t_cblc_struct_type *owner = find_class_type(unit,
            current_type_name.c_str());
        if (!owner)
            return false;
        const t_cblc_struct_field *member = nullptr;
        std::size_t member_index = 0;
        for (; member_index < owner->field_count; ++member_index)
            if (std::strcmp(owner->fields[member_index].source_name,
                    member_name.c_str()) == 0
                || std::strcmp(owner->fields[member_index].cobol_name,
                    member_name.c_str()) == 0)
            {
                member = &owner->fields[member_index];
                break;
            }
        if (!member || member->array_count > 0)
            return false;
        type_id member_type = scalar_type_id(member->kind);
        if (member->kind == CBLC_DATA_KIND_STRUCT)
            member_type = declared_struct_type_id(unit, member->struct_type_name);
        else if (member->kind == CBLC_DATA_KIND_STRUCT_POINTER)
            member_type = canonical_pointer_type_id(member->struct_type_name);
        if (member_type == 0)
            return false;
        projections->push_back({place_projection::kind::field,
            static_cast<std::uint32_t>(member_index + 1), member_type});
        if (member->is_const)
            access = access_mode::read_only;
        current_type_name = member->struct_type_name;
        current_is_pointer = member->kind == CBLC_DATA_KIND_STRUCT_POINTER;
        *result_type = member_type;
        saw_member = true;
    }
    if (!saw_member || current_is_pointer
        || projections->empty()
        || (projections->back().resulting_type != scalar_type_id(CBLC_DATA_KIND_CHAR)
            && projections->back().resulting_type != scalar_type_id(CBLC_DATA_KIND_INT)
            && projections->back().resulting_type != scalar_type_id(CBLC_DATA_KIND_DOUBLE)))
        return false;
    *root_item = root;
    *result_access = access;
    return true;
}

static bool append_scalar_member_place(
    const t_cblc_translation_unit *unit, const t_cblc_function *function,
    const char *expression,
    const std::unordered_map<const t_cblc_data_item *, value_id> &value_by_item,
    const std::unordered_map<const t_cblc_data_item *, place_id> &place_by_item,
    typed_function *typed, place_id *next_place, place_id *resolved_place,
    type_id *resolved_type)
{
    const t_cblc_data_item *root_item = nullptr;
    std::vector<place_projection> projections;
    access_mode access = access_mode::read_only;
    type_id type = 0;
    if (!typed || !next_place || !resolved_place || !resolved_type
        || !resolve_scalar_member_expression(unit, function, expression,
            &root_item, &projections, &type, &access))
        return false;
    const auto root_value = value_by_item.find(root_item);
    const auto root_place = place_by_item.find(root_item);
    if (root_value == value_by_item.end() || root_place == place_by_item.end())
        return false;
    const auto root_place_value = std::find_if(typed->places.begin(),
        typed->places.end(), [&](const typed_place &place)
        {
            return place.id == root_place->second;
        });
    const auto root_typed_value = std::find_if(typed->values.begin(),
        typed->values.end(), [&](const typed_value &value)
        {
            return value.id == root_value->second;
        });
    if (root_place_value == typed->places.end()
        || root_typed_value == typed->values.end())
        return false;
    typed_place member_place = *root_place_value;
    member_place.id = (*next_place)++;
    member_place.type = type;
    member_place.access = access;
    member_place.projections.insert(member_place.projections.end(),
        projections.begin(), projections.end());
    member_place.owner_lifetime_token = root_typed_value->lifetime_token != 0
        ? root_typed_value->lifetime_token : root_typed_value->id;
    *resolved_place = member_place.id;
    *resolved_type = type;
    typed->places.push_back(std::move(member_place));
    return true;
}

static bool split_call_arguments(const char *source,
    std::vector<std::string> *arguments)
{
    if (!source || !arguments)
        return false;
    arguments->clear();
    const char *begin = source;
    const char *cursor = source;
    std::size_t parentheses = 0;
    std::size_t brackets = 0;
    char quote = '\0';
    bool escaped = false;
    while (true)
    {
        const char current = *cursor;
        if (quote != '\0')
        {
            if (escaped)
                escaped = false;
            else if (current == '\\')
                escaped = true;
            else if (current == quote)
                quote = '\0';
        }
        else if (current == '\'' || current == '"')
            quote = current;
        else if (current == '(')
            ++parentheses;
        else if (current == ')')
        {
            if (parentheses == 0)
                return false;
            --parentheses;
        }
        else if (current == '[')
            ++brackets;
        else if (current == ']')
        {
            if (brackets == 0)
                return false;
            --brackets;
        }
        if ((current == ',' && quote == '\0'
                && parentheses == 0 && brackets == 0)
            || current == '\0')
        {
            const char *trimmed_begin = begin;
            const char *trimmed_end = cursor;
            trim_expression(&trimmed_begin, &trimmed_end);
            if (trimmed_begin != trimmed_end)
                arguments->emplace_back(trimmed_begin, trimmed_end);
            else if (current != '\0' || !arguments->empty())
                return false;
            if (current == '\0')
                break;
            begin = cursor + 1;
        }
        if (current == '\0')
            return false;
        ++cursor;
    }
    return quote == '\0' && parentheses == 0 && brackets == 0;
}

static type_id scalar_parameter_type_id(
    t_transpiler_function_parameter_kind kind)
{
    if (kind == TRANSPILE_FUNCTION_PARAMETER_INT)
        return scalar_type_id(CBLC_DATA_KIND_INT);
    if (kind == TRANSPILE_FUNCTION_PARAMETER_DOUBLE)
        return scalar_type_id(CBLC_DATA_KIND_DOUBLE);
    if (kind == TRANSPILE_FUNCTION_PARAMETER_CHAR)
        return scalar_type_id(CBLC_DATA_KIND_CHAR);
    return 0;
}

static type_id method_parameter_type_id(const t_cblc_translation_unit *unit,
    const t_cblc_parameter &parameter)
{
    const type_id scalar_type = scalar_parameter_type_id(parameter.kind);
    if (scalar_type != 0)
        return scalar_type;
    if (parameter.kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT
        && declared_struct_type_id(unit, parameter.type_name) != 0)
        return declared_struct_type_id(unit, parameter.type_name);
    if (parameter.kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER
        && declared_struct_type_id(unit, parameter.type_name) != 0)
        return canonical_pointer_type_id(parameter.type_name);
    return 0;
}

static const t_cblc_constructor *find_typed_copy_constructor(
    const t_cblc_struct_type *type)
{
    if (!type)
        return nullptr;
    for (std::size_t index = 0; index < type->constructor_count; ++index)
        if (type->constructors[index].is_copy_constructor)
            return &type->constructors[index];
    return nullptr;
}

static bool typed_constructor_may_throw(
    const t_cblc_constructor *constructor)
{
    if (!constructor || constructor->exception_types_unknown
        || constructor->exception_type_count != 0)
        return true;
    for (std::size_t index = 0; index < constructor->statement_count; ++index)
    {
        const t_cblc_statement &statement = constructor->statements[index];
        if (statement.type == CBLC_STATEMENT_THROW || statement.exception_may_throw
            || statement.type == CBLC_STATEMENT_CALL
            || statement.type == CBLC_STATEMENT_CALL_ASSIGN
            || statement.type == CBLC_STATEMENT_METHOD_CALL
            || statement.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
            return true;
    }
    return false;
}

static type_id scalar_method_return_type_id(
    t_cblc_function_return_kind kind)
{
    if (kind == CBLC_FUNCTION_RETURN_INT)
        return scalar_type_id(CBLC_DATA_KIND_INT);
    if (kind == CBLC_FUNCTION_RETURN_DOUBLE)
        return scalar_type_id(CBLC_DATA_KIND_DOUBLE);
    if (kind == CBLC_FUNCTION_RETURN_CHAR)
        return scalar_type_id(CBLC_DATA_KIND_CHAR);
    return 0;
}

static const t_cblc_method *find_method_for_typed_arguments(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *name,
    const std::vector<type_id> &argument_types, bool *ambiguous)
{
    const t_cblc_method *match = nullptr;
    std::size_t best_cost = std::numeric_limits<std::size_t>::max();
    if (ambiguous)
        *ambiguous = false;
    if (!type || !name)
        return nullptr;
    for (std::size_t method_index = 0; method_index < type->method_count;
        ++method_index)
    {
        const t_cblc_method &method = type->methods[method_index];
        if (method.parameter_count != argument_types.size()
            || (std::strcmp(method.source_name, name) != 0
                && std::strcmp(method.cobol_name, name) != 0))
            continue;
        bool matches = true;
        std::size_t conversion_cost = 0;
        for (std::size_t parameter_index = 0;
            parameter_index < argument_types.size(); ++parameter_index)
        {
            const type_id parameter_type = method_parameter_type_id(unit,
                method.parameters[parameter_index]);
            const type_id argument_type = argument_types[parameter_index];
            if (parameter_type == argument_type)
                continue;
            if (argument_type == scalar_type_id(CBLC_DATA_KIND_INT)
                && parameter_type == scalar_type_id(CBLC_DATA_KIND_DOUBLE))
            {
                conversion_cost += 1;
                continue;
            }
            else
            {
                matches = false;
                break;
            }
        }
        if (!matches)
            continue;
        if (conversion_cost < best_cost)
        {
            match = &method;
            best_cost = conversion_cost;
            if (ambiguous)
                *ambiguous = false;
        }
        else if (conversion_cost == best_cost)
        {
            if (ambiguous)
                *ambiguous = true;
            match = nullptr;
        }
    }
    return match;
}

static symbol_id method_symbol_id(const t_cblc_struct_type *owner,
    const t_cblc_method *method)
{
    const std::uint64_t offset_basis = 14695981039346656037ULL;
    const std::uint64_t prime = 1099511628211ULL;
    std::uint64_t hash = offset_basis;
    const char *parts[3];
    char overload[32];

    if (!owner || !method)
        return 0;
    std::snprintf(overload, sizeof(overload), "#%zu", method->overload_index);
    parts[0] = owner->source_name;
    parts[1] = "::";
    parts[2] = method->source_name;
    for (const char *part : parts)
        for (const unsigned char *cursor =
                reinterpret_cast<const unsigned char *>(part);
            *cursor != '\0'; ++cursor)
        {
            hash ^= *cursor;
            hash *= prime;
        }
    for (const unsigned char *cursor =
            reinterpret_cast<const unsigned char *>(overload);
        *cursor != '\0'; ++cursor)
    {
        hash ^= *cursor;
        hash *= prime;
    }
    const symbol_id result = static_cast<symbol_id>(hash);
    return result != 0 ? result : 1;
}

static symbol_id function_symbol_id(const t_cblc_function *function,
    const std::vector<type_id> &parameter_types, type_id result_type)
{
    const std::uint64_t prime = 1099511628211ULL;
    std::uint64_t hash = 14695981039346656037ULL;
    if (!function || function->source_name[0] == '\0')
        return 0;
    const std::string signature = std::string(function->source_name) + "(";
    auto append = [&](const std::string &part)
    {
        for (const unsigned char character : part)
        {
            hash ^= character;
            hash *= prime;
        }
    };
    append(signature);
    for (std::size_t index = 0; index < parameter_types.size(); ++index)
    {
        if (index != 0)
            append(",");
        append(std::to_string(parameter_types[index]));
    }
    append(")->");
    append(std::to_string(result_type));
    const symbol_id result = static_cast<symbol_id>(hash);
    return result != 0 ? result : 1;
}

static bool same_typed_callable(const typed_callable &left,
    const typed_callable &right)
{
    return left.id == right.id && left.kind == right.kind
        && left.owner_type == right.owner_type
        && left.overload_index == right.overload_index
        && left.canonical_name == right.canonical_name
        && left.parameter_types == right.parameter_types
        && left.result_type == right.result_type
        && left.exceptions == right.exceptions
        && left.has_definition == right.has_definition
        && left.known_exception_type_ids == right.known_exception_type_ids;
}

static bool append_typed_callable(typed_function *function,
    typed_callable callable)
{
    if (!function || callable.id == 0 || callable.canonical_name.empty()
        || (callable.kind == callable_kind::function
            ? callable.owner_type != 0 : callable.owner_type == 0))
        return false;
    const auto existing = std::find_if(function->callables.begin(),
        function->callables.end(), [&](const typed_callable &candidate)
        {
            return candidate.id == callable.id;
        });
    if (existing != function->callables.end())
        return same_typed_callable(*existing, callable);
    function->callables.push_back(std::move(callable));
    return true;
}

symbol_id canonical_constructor_symbol_id(const t_cblc_struct_type *owner,
    const t_cblc_constructor *constructor)
{
    const std::uint64_t offset_basis = 14695981039346656037ULL;
    const std::uint64_t prime = 1099511628211ULL;
    std::uint64_t hash = offset_basis;
    std::string canonical_signature;
    if (!owner || !constructor)
        return 0;
    canonical_signature = owner->source_name;
    canonical_signature += "::<constructor>(";
    for (std::size_t index = 0; index < constructor->parameter_count; ++index)
    {
        if (index != 0)
            canonical_signature += ',';
        canonical_signature += std::to_string(
            static_cast<int>(constructor->parameters[index].kind));
        canonical_signature += ':';
        canonical_signature += constructor->parameters[index].type_name;
    }
    canonical_signature += ')';
    for (const unsigned char character : canonical_signature)
    {
        hash ^= character;
        hash *= prime;
    }
    const symbol_id result = static_cast<symbol_id>(hash);
    return result != 0 ? result : 1;
}

static symbol_id destructor_symbol_id(const t_cblc_struct_type *owner)
{
    const std::uint64_t prime = 1099511628211ULL;
    std::uint64_t hash = 14695981039346656037ULL;
    if (!owner || owner->source_name[0] == '\0')
        return 0;
    const std::string signature = std::string(owner->source_name)
        + "::~" + owner->source_name;
    for (const unsigned char character : signature)
    {
        hash ^= character;
        hash *= prime;
    }
    const symbol_id result = static_cast<symbol_id>(hash);
    return result != 0 ? result : 1;
}

static bool append_constructor_callable(typed_function *function,
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *owner,
    const t_cblc_constructor *constructor, type_id owner_type,
    callable_kind dispatch_kind, bool may_throw)
{
    if (!constructor)
        return true;
    if (!function || !unit || !owner || owner_type == 0)
        return false;
    typed_callable callable;
    callable.id = canonical_constructor_symbol_id(owner, constructor);
    /* The callable category belongs to the canonical constructor, not to the
     * operation that happened to reference it. The same copy constructor can
     * initialize a local object and materialize a by-value argument; both
     * references must publish an identical descriptor. */
    callable.kind = constructor->is_copy_constructor
        ? callable_kind::copy_constructor
        : dispatch_kind;
    callable.owner_type = owner_type;
    callable.canonical_name = std::string(owner->source_name)
        + "::<constructor>";
    callable.result_type = 0;
    callable.exceptions = may_throw
        ? exception_effect::unknown : exception_effect::non_throwing;
    callable.has_definition = constructor->has_definition != 0;
    for (std::size_t index = 0; index < constructor->parameter_count; ++index)
    {
        const type_id parameter_type = method_parameter_type_id(unit,
            constructor->parameters[index]);
        if (parameter_type == 0)
            return false;
        callable.parameter_types.push_back(parameter_type);
    }
    return append_typed_callable(function, std::move(callable));
}

static bool append_destructor_callable(typed_function *function,
    const t_cblc_struct_type *owner, type_id owner_type)
{
    if (!owner || (!owner->has_destructor && !owner->has_destructor_definition))
        return true;
    if (!function || owner_type == 0)
        return false;
    typed_callable callable;
    callable.id = destructor_symbol_id(owner);
    callable.kind = callable_kind::destructor;
    callable.owner_type = owner_type;
    callable.canonical_name = std::string(owner->source_name)
        + "::~" + owner->source_name;
    callable.exceptions = exception_effect::non_throwing;
    callable.has_definition = owner->has_destructor_definition != 0;
    return append_typed_callable(function, std::move(callable));
}

static void derive_method_exception_summary(const t_cblc_method *method,
    std::vector<std::uint64_t> *known_type_ids, bool *unknown)
{
    if (!method || !known_type_ids || !unknown)
    {
        if (unknown)
            *unknown = true;
        return;
    }
    *unknown = *unknown || method->exception_types_unknown != 0;
    for (std::size_t index = 0; index < method->exception_type_count; ++index)
    {
        const std::uint64_t exception_type_id = method->exception_type_ids[index];
        if (exception_type_id != 0
            && std::find(known_type_ids->begin(), known_type_ids->end(),
                exception_type_id)
                == known_type_ids->end())
            known_type_ids->push_back(exception_type_id);
        else if (exception_type_id == 0)
            *unknown = true;
    }
    if (method->statement_count > 0 && !method->statements)
    {
        *unknown = true;
        return;
    }
    for (std::size_t index = 0; index < method->statement_count; ++index)
    {
        const t_cblc_statement &statement = method->statements[index];
        if (statement.type == CBLC_STATEMENT_THROW)
        {
            if (statement.exception_is_rethrow || statement.exception_type_id == 0)
                *unknown = true;
            else if (std::find(known_type_ids->begin(), known_type_ids->end(),
                    statement.exception_type_id) == known_type_ids->end())
                known_type_ids->push_back(statement.exception_type_id);
        }
        else if (statement.exception_may_throw
            || statement.type == CBLC_STATEMENT_CALL
            || statement.type == CBLC_STATEMENT_CALL_ASSIGN
            || statement.type == CBLC_STATEMENT_METHOD_CALL
            || statement.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
            || statement.type == CBLC_STATEMENT_FILE_OPEN
            || statement.type == CBLC_STATEMENT_FILE_READ
            || statement.type == CBLC_STATEMENT_FILE_WRITE
            || statement.type == CBLC_STATEMENT_FILE_CLOSE)
            *unknown = true;
    }
}

static bool constructor_may_throw(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item, const t_cblc_statement &statement)
{
    const t_cblc_struct_type *type = nullptr;
    if (!unit || !item)
        return false;
    for (std::size_t type_index = 0; type_index < unit->struct_type_count;
        ++type_index)
        if (std::strcmp(unit->struct_types[type_index].source_name,
                item->struct_type_name) == 0)
        {
            type = &unit->struct_types[type_index];
            break;
        }
    if (!type)
        return false;
    const t_cblc_constructor *constructor = cblc_find_constructor_for_arguments(
        unit, type, statement.call_arguments, statement.call_argument_count);
    if (!constructor)
    {
        /* An unresolved explicit overload must conservatively retain a failure edge. */
        return statement.call_argument_count != 0 || type->constructor_count != 0;
    }
    /* Keep construction effects identical wherever the same canonical
     * constructor is referenced (object initialization and by-value call
     * temporaries). A narrower legacy scan here can register one callable as
     * non-throwing and later reject its correctly conservative descriptor. */
    return typed_constructor_may_throw(constructor);
}

static const t_cblc_data_item *find_scalar_item(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const char *name)
{
    std::size_t index = unit->data_count;

    while (index > 0)
    {
        const t_cblc_data_item &item = unit->data_items[index - 1];
        const bool is_function_return_storage = function->return_cobol_name[0] != '\0'
            && std::strcmp(item.cobol_name, function->return_cobol_name) == 0;
        bool is_parameter_storage = false;
        bool is_named_parameter_storage = false;
        for (std::size_t parameter_index = 0;
            parameter_index < function->parameter_count; ++parameter_index)
        {
            const t_cblc_parameter &parameter =
                function->parameters[parameter_index];
            if (parameter.actual_source_name[0] != '\0'
                && std::strcmp(parameter.actual_source_name,
                    item.source_name) == 0)
            {
                is_parameter_storage = true;
                if (std::strcmp(parameter.source_name, name) == 0
                    || std::strcmp(parameter.actual_source_name, name) == 0)
                    is_named_parameter_storage = true;
            }
        }
        if ((is_function_return_storage || (item.is_function_local
                ? std::strcmp(item.owner_function_name, function->source_name) == 0
                : (item.is_active || is_parameter_storage)))
            && !item.is_shadowed && (std::strcmp(item.source_name, name) == 0
                || std::strcmp(item.cobol_name, name) == 0
                || std::strcmp(item.local_source_name, name) == 0
                || is_named_parameter_storage)
            && scalar_type_id(item.kind) != 0)
        {
            if (!item.is_alias)
                return &item;
            std::size_t actual_index = unit->data_count;
            while (actual_index > 0)
            {
                const t_cblc_data_item &actual =
                    unit->data_items[--actual_index];
                bool actual_is_parameter = false;
                for (std::size_t parameter_index = 0;
                    parameter_index < function->parameter_count;
                    ++parameter_index)
                    if (function->parameters[parameter_index].actual_source_name[0]
                            != '\0'
                        && std::strcmp(function->parameters[parameter_index]
                                .actual_source_name, actual.source_name) == 0)
                    {
                        actual_is_parameter = true;
                        break;
                    }
                const bool actual_is_visible = actual.is_function_local
                    ? std::strcmp(actual.owner_function_name,
                        function->source_name) == 0
                    : (actual.is_active || actual_is_parameter);
                if (!actual.is_alias && !actual.is_shadowed && actual_is_visible
                    && scalar_type_id(actual.kind) != 0
                    && std::strcmp(actual.cobol_name, item.cobol_name) == 0)
                    return &actual;
            }
        }
        index -= 1;
    }
    return nullptr;
}

static source_origin statement_origin(const t_cblc_translation_unit *unit,
    const t_cblc_statement &statement, const char *role)
{
    source_origin origin;
    origin.source_unit_id = static_cast<std::uint64_t>(unit->source_file_id) + 1U;
    origin.source_offset = statement.source_offset == static_cast<std::size_t>(-1)
        ? 0 : static_cast<std::uint64_t>(statement.source_offset);
    origin.source_length = static_cast<std::uint32_t>(statement.source_length);
    origin.line = static_cast<std::uint32_t>(statement.source_line);
    origin.column = static_cast<std::uint32_t>(statement.source_column);
    origin.role = role;
    origin.compiler_generated = false;
    return origin;
}

static bool append_typed_operation(typed_function *function, region_id region,
    typed_operation operation, verification_diagnostic *diagnostic,
    bool loop_condition = false)
{
    for (typed_region &candidate : function->regions)
        if (candidate.id == region)
        {
            operation.region = region;
            candidate.operations.push_back(operation.id);
            if (loop_condition)
            {
                if (candidate.kind != region_kind::loop)
                    return fail(diagnostic, verification_error::invalid_region_tree,
                        operation.id, "condition prologue operations belong only to loop regions");
                candidate.condition_operations.push_back(operation.id);
            }
            else
                candidate.sequence.push_back({typed_region::item::kind::operation,
                    operation.id});
            function->operations.push_back(std::move(operation));
            return true;
        }
    return fail(diagnostic, verification_error::invalid_region_tree,
        operation.id, "typed operation target region does not exist");
}

struct lowered_scalar_operand
{
    value_id value = 0;
    type_id type = 0;
    operation_id producer = 0;
};

static void trim_expression(const char **begin, const char **end)
{
    while (*begin < *end && (**begin == ' ' || **begin == '\t'
        || **begin == '\r' || **begin == '\n'))
        *begin += 1;
    while (*end > *begin && ((*end)[-1] == ' ' || (*end)[-1] == '\t'
        || (*end)[-1] == '\r' || (*end)[-1] == '\n'))
        *end -= 1;
}

static bool lower_scalar_operand(const t_cblc_translation_unit *unit,
    const t_cblc_function *source_function, const char *begin, const char *end,
    typed_function *function, region_id region,
    const std::unordered_map<const t_cblc_data_item *, value_id> &value_by_item,
    const std::unordered_map<const t_cblc_data_item *, place_id> &place_by_item,
    place_id *next_place, value_id *next_value, operation_id *next_operation,
    const source_origin &origin, bool loop_condition,
    lowered_scalar_operand *result,
    verification_diagnostic *diagnostic)
{
    std::string token;
    const t_cblc_data_item *item;
    char *literal_end = nullptr;
    long long literal_value;
    bool floating_literal;
    std::size_t arithmetic_position = std::string::npos;
    char arithmetic_operator = '\0';

    trim_expression(&begin, &end);
    if (begin == end || static_cast<std::size_t>(end - begin)
        >= TRANSPILE_STATEMENT_TEXT_MAX)
        return fail(diagnostic, verification_error::unsupported_legacy_statement,
            0, "scalar condition operand is empty or exceeds the adapter limit");
    token.assign(begin, end);
    item = find_scalar_item(unit, source_function, token.c_str());
    if (item)
    {
        const auto place = place_by_item.find(item);
        if (place == place_by_item.end())
            return fail(diagnostic, verification_error::unsupported_legacy_type,
                0, "condition reads a scalar outside finalized function storage");
        typed_value loaded;
        loaded.id = (*next_value)++;
        loaded.type = scalar_type_id(item->kind);
        loaded.category = value_category::scalar;
        loaded.provenance.push_back(origin);
        function->values.push_back(loaded);
        typed_operation load;
        load.id = (*next_operation)++;
        load.operation = opcode::load;
        load.places.push_back(place->second);
        load.results.push_back(loaded.id);
        load.effects.reads.push_back(place->second);
        load.effects.exceptions = exception_effect::non_throwing;
        operation_outcome loaded_outcome;
        loaded_outcome.initialized.push_back(loaded.id);
        load.outcomes.push_back(loaded_outcome);
        load.provenance.push_back(origin);
        if (!append_typed_operation(function, region, std::move(load), diagnostic,
                loop_condition))
            return false;
        result->value = loaded.id;
        result->type = loaded.type;
        result->producer = *next_operation - 1;
        return true;
    }

    place_id member_place = 0;
    type_id member_type = 0;
    if (append_scalar_member_place(unit, source_function, token.c_str(),
            value_by_item, place_by_item, function, next_place, &member_place,
            &member_type))
    {
        typed_value loaded;
        loaded.id = (*next_value)++;
        loaded.type = member_type;
        loaded.category = value_category::scalar;
        loaded.provenance.push_back(origin);
        function->values.push_back(loaded);
        typed_operation load;
        load.id = (*next_operation)++;
        load.operation = opcode::load;
        load.places.push_back(member_place);
        load.results.push_back(loaded.id);
        load.effects.reads.push_back(member_place);
        load.effects.exceptions = exception_effect::non_throwing;
        operation_outcome loaded_outcome;
        loaded_outcome.initialized.push_back(loaded.id);
        load.outcomes.push_back(loaded_outcome);
        load.provenance.push_back(origin);
        if (!append_typed_operation(function, region, std::move(load), diagnostic,
                loop_condition))
            return false;
        result->value = loaded.id;
        result->type = loaded.type;
        result->producer = *next_operation - 1;
        return true;
    }

    /* Integer index and condition expressions use a small typed arithmetic
     * subset. Split at the rightmost top-level additive operator so recursive
     * lowering preserves left associativity (a + b - c == (a + b) - c). */
    {
        std::size_t depth = 0;
        char quote = '\0';
        bool escaped = false;
        for (std::size_t cursor = 0; cursor < token.size(); ++cursor)
        {
            const char current = token[cursor];
            if (quote != '\0')
            {
                if (escaped)
                    escaped = false;
                else if (current == '\\')
                    escaped = true;
                else if (current == quote)
                    quote = '\0';
                continue;
            }
            if (current == '\'' || current == '"')
            {
                quote = current;
                continue;
            }
            if (current == '(' || current == '[')
            {
                ++depth;
                continue;
            }
            if ((current == ')' || current == ']') && depth > 0)
            {
                --depth;
                continue;
            }
            if (depth != 0 || (current != '+' && current != '-'))
                continue;
            if (current == '-')
            {
                bool part_of_cobol_name = false;
                for (std::size_t item_index = 0;
                    item_index < unit->data_count && !part_of_cobol_name;
                    ++item_index)
                {
                    const char *cobol_name =
                        unit->data_items[item_index].cobol_name;
                    const std::size_t name_length = std::strlen(cobol_name);
                    if (name_length == 0 || name_length > token.size())
                        continue;
                    std::size_t name_position = token.find(cobol_name);
                    while (name_position != std::string::npos)
                    {
                        if (cursor >= name_position
                            && cursor < name_position + name_length)
                        {
                            part_of_cobol_name = true;
                            break;
                        }
                        name_position = token.find(cobol_name,
                            name_position + 1);
                    }
                }
                if (part_of_cobol_name)
                    continue;
            }
            std::size_t previous = cursor;
            while (previous > 0 && std::isspace(
                    static_cast<unsigned char>(token[previous - 1])))
                --previous;
            const bool exponent_sign = previous > 1
                && (token[previous - 1] == 'e'
                    || token[previous - 1] == 'E')
                && (std::isdigit(static_cast<unsigned char>(
                        token[previous - 2]))
                    || token[previous - 2] == '.');
            if (previous == 0 || token[previous - 1] == '+'
                || token[previous - 1] == '-' || token[previous - 1] == '*'
                || token[previous - 1] == '/' || exponent_sign)
                continue;
            arithmetic_position = cursor;
            arithmetic_operator = current;
        }
    }
    if (arithmetic_position != std::string::npos)
    {
        lowered_scalar_operand lhs;
        lowered_scalar_operand rhs;
        const char *expression_begin = token.c_str();
        const char *operator_begin = expression_begin + arithmetic_position;
        const char *expression_end = expression_begin + token.size();
        if (!lower_scalar_operand(unit, source_function, expression_begin,
                operator_begin, function, region, value_by_item, place_by_item,
                next_place, next_value, next_operation, origin, loop_condition,
                &lhs, diagnostic)
            || !lower_scalar_operand(unit, source_function,
                operator_begin + 1, expression_end, function, region,
                value_by_item, place_by_item, next_place, next_value,
                next_operation, origin, loop_condition, &rhs, diagnostic))
            return false;
        const type_id integer_type = scalar_type_id(CBLC_DATA_KIND_INT);
        if (lhs.type != integer_type || rhs.type != integer_type)
            return fail(diagnostic,
                verification_error::unsupported_legacy_type, 0,
                "typed additive expressions currently require int operands");
        typed_value computed;
        computed.id = (*next_value)++;
        computed.type = integer_type;
        computed.category = value_category::scalar;
        computed.provenance.push_back(origin);
        function->values.push_back(computed);
        typed_operation arithmetic;
        arithmetic.id = (*next_operation)++;
        arithmetic.operation = arithmetic_operator == '+'
            ? opcode::add : opcode::subtract;
        arithmetic.operands = {lhs.value, rhs.value};
        arithmetic.results = {computed.id};
        if (lhs.producer != 0)
            arithmetic.must_follow.push_back(lhs.producer);
        if (rhs.producer != 0 && rhs.producer != lhs.producer)
            arithmetic.must_follow.push_back(rhs.producer);
        arithmetic.effects.exceptions = exception_effect::non_throwing;
        operation_outcome outcome;
        outcome.initialized.push_back(computed.id);
        arithmetic.outcomes.push_back(std::move(outcome));
        arithmetic.provenance.push_back(origin);
        if (!append_typed_operation(function, region, std::move(arithmetic),
                diagnostic, loop_condition))
            return false;
        result->value = computed.id;
        result->type = computed.type;
        result->producer = *next_operation - 1;
        return true;
    }

    floating_literal = token.find_first_of(".eE") != std::string::npos;
    errno = 0;
    if (floating_literal)
    {
        const double parsed = std::strtod(token.c_str(), &literal_end);
        if (literal_end == token.c_str() || errno == ERANGE
            || *literal_end != '\0' || !std::isfinite(parsed))
        {
            const std::string message =
                "condition adapter accepts only finite numeric literals: '"
                + token + "'";
            return fail(diagnostic,
                verification_error::unsupported_legacy_statement, 0,
                message.c_str());
        }
    }
    else
    {
        literal_value = std::strtoll(token.c_str(), &literal_end, 10);
        if (literal_end == token.c_str() || errno == ERANGE
            || *literal_end != '\0')
        {
            return fail(diagnostic,
                verification_error::unsupported_legacy_statement, 0,
                "condition adapter accepts only a scalar name or numeric literal operand");
        }
    }
    if (literal_end == token.c_str() || *literal_end != '\0')
        return fail(diagnostic, verification_error::unsupported_legacy_statement,
            0, "condition adapter accepts only a scalar name or numeric literal operand");
    typed_value literal;
    literal.id = (*next_value)++;
    literal.type = scalar_type_id(floating_literal
        ? CBLC_DATA_KIND_DOUBLE : CBLC_DATA_KIND_INT);
    literal.category = value_category::scalar;
    literal.provenance.push_back(origin);
    literal.constant_value = floating_literal
        ? token : std::to_string(literal_value);
    function->values.push_back(literal);
    typed_operation literal_operation;
    literal_operation.id = (*next_operation)++;
    literal_operation.operation = opcode::literal;
    literal_operation.results.push_back(literal.id);
    operation_outcome literal_outcome;
    literal_outcome.initialized.push_back(literal.id);
    literal_operation.outcomes.push_back(literal_outcome);
    literal_operation.effects.exceptions = exception_effect::non_throwing;
    literal_operation.provenance.push_back(origin);
    result->value = literal.id;
    result->type = literal.type;
    result->producer = literal_operation.id;
    return append_typed_operation(function, region, std::move(literal_operation),
        diagnostic, loop_condition);
}

static bool lower_scalar_condition(const t_cblc_translation_unit *unit,
    const t_cblc_function *source_function, const char *expression,
    typed_function *function, region_id region,
    const std::unordered_map<const t_cblc_data_item *, value_id> &value_by_item,
    const std::unordered_map<const t_cblc_data_item *, place_id> &place_by_item,
    place_id *next_place, value_id *next_value, operation_id *next_operation,
    const source_origin &origin, bool loop_condition, value_id *condition,
    verification_diagnostic *diagnostic)
{
    const char *begin = expression;
    const char *end;
    const char *operator_begin = nullptr;
    std::size_t operator_length = 0;
    lowered_scalar_operand lhs;
    lowered_scalar_operand rhs;

    if (!expression || !function || !condition)
        return fail(diagnostic, verification_error::unsupported_legacy_statement,
            0, "scalar condition lowering received an incomplete request");
    end = expression + std::strlen(expression);
    trim_expression(&begin, &end);
    for (const char *cursor = begin; cursor < end; ++cursor)
        if (*cursor == '=' || *cursor == '!' || *cursor == '<' || *cursor == '>')
        {
            if (*cursor == '>' && cursor > begin && cursor[-1] == '-')
                continue;
            operator_begin = cursor;
            operator_length = (cursor + 1 < end && cursor[1] == '=') ? 2 : 1;
            break;
        }
    if (!operator_begin)
    {
        if (!lower_scalar_operand(unit, source_function, begin, end, function,
                region, value_by_item, place_by_item, next_place, next_value,
                next_operation, origin,
                loop_condition, &lhs, diagnostic))
            return false;
        *condition = lhs.value;
        return true;
    }
    if ((operator_length == 1 && (*operator_begin == '=' || *operator_begin == '!'))
        || operator_begin == begin || operator_begin + operator_length == end)
        return fail(diagnostic, verification_error::unsupported_legacy_statement,
            0, "condition adapter accepts only relational or equality operators");
    if (!lower_scalar_operand(unit, source_function, begin, operator_begin,
            function, region, value_by_item, place_by_item, next_place,
            next_value, next_operation, origin, loop_condition, &lhs, diagnostic)
        || !lower_scalar_operand(unit, source_function,
            operator_begin + operator_length, end, function, region,
            value_by_item, place_by_item, next_place, next_value,
            next_operation, origin, loop_condition, &rhs, diagnostic))
        return false;
    if (lhs.type != rhs.type)
        return fail(diagnostic, verification_error::unsupported_legacy_type,
            0, "condition comparison requires operands with the same finalized scalar type");

    typed_value comparison_result;
    comparison_result.id = (*next_value)++;
    comparison_result.type = scalar_type_id(CBLC_DATA_KIND_INT);
    comparison_result.category = value_category::scalar;
    comparison_result.provenance.push_back(origin);
    function->values.push_back(comparison_result);
    typed_operation comparison;
    comparison.id = (*next_operation)++;
    comparison.operation = opcode::compare;
    if (operator_length == 2 && *operator_begin == '=' )
        comparison.comparison = comparison_kind::equal;
    else if (operator_length == 2 && *operator_begin == '!')
        comparison.comparison = comparison_kind::not_equal;
    else if (*operator_begin == '<')
        comparison.comparison = operator_length == 2
            ? comparison_kind::less_equal : comparison_kind::less;
    else
        comparison.comparison = operator_length == 2
            ? comparison_kind::greater_equal : comparison_kind::greater;
    comparison.operands = {lhs.value, rhs.value};
    comparison.results.push_back(comparison_result.id);
    comparison.must_follow = {lhs.producer, rhs.producer};
    comparison.effects.exceptions = exception_effect::non_throwing;
    operation_outcome compared;
    compared.initialized.push_back(comparison_result.id);
    comparison.outcomes.push_back(compared);
    comparison.provenance.push_back(origin);
    *condition = comparison_result.id;
    return append_typed_operation(function, region, std::move(comparison),
        diagnostic, loop_condition);
}

static std::uint64_t active_exception_region_id(
    const typed_function &function, region_id active_region)
{
    std::uint64_t handled_region = 0;

    while (active_region != 0)
    {
        const auto region = std::find_if(function.regions.begin(),
            function.regions.end(), [&](const typed_region &candidate)
            {
                return candidate.id == active_region;
            });
        if (region == function.regions.end())
            return 0;
        if (region->kind == region_kind::exception_handler
            && handled_region == 0)
            handled_region = region->source_exception_region_id;
        else if (region->kind == region_kind::exception_try
            && region->source_exception_region_id != handled_region)
            return region->source_exception_region_id;
        active_region = region->parent;
    }
    return 0;
}

static bool lower_scalar_function_impl(const t_cblc_translation_unit *unit,
    std::size_t function_index, typed_function *result,
    verification_diagnostic *diagnostic,
    std::vector<typed_storage_binding> *storage_bindings = nullptr,
    bool defer_verification = false)
{
    const t_cblc_function *function;
    std::unordered_map<const t_cblc_data_item *, value_id> value_by_item;
    std::unordered_map<const t_cblc_data_item *, place_id> place_by_item;
    std::unordered_map<const t_cblc_data_item *, std::vector<value_id>>
        lifecycle_values_by_item;
    std::unordered_map<const t_cblc_data_item *, std::vector<place_id>>
        lifecycle_places_by_item;
    std::unordered_set<value_id> exception_cleaned_values;
    std::unordered_map<value_id, std::vector<value_id>> lifecycle_children_by_value;
    std::unordered_map<value_id, place_id> lifecycle_place_by_value;
    std::unordered_map<std::string, std::vector<value_id>>
        explicitly_constructed_indirect_values;
    std::unordered_map<type_id, const typed_type *> typed_types_by_id;
    value_id next_value;
    place_id next_place;
    symbol_id next_symbol;
    operation_id next_operation;
    region_id current_region;
    region_id next_region;
    struct region_frame
    {
        region_id control;
        region_id parent;
        t_cblc_statement_type statement_kind;
        bool saw_else;
    };
    std::vector<region_frame> region_stack;
    std::size_t index;
    std::function<bool(value_id, type_id, value_id, symbol_id,
        const std::vector<place_projection> &, const std::vector<type_id> &)> add_member_subobjects;

    if (diagnostic)
        *diagnostic = verification_diagnostic{};
    if (!unit || !result || function_index >= unit->function_count)
        return fail(diagnostic, verification_error::missing_function_identity, 0,
            "scalar lowering requires a valid translation unit, output, and function index");
    *result = typed_function{};
    function = &unit->functions[function_index];
    result->source_unit_id = static_cast<std::uint64_t>(unit->source_file_id) + 1U;
    result->function_id = static_cast<std::uint32_t>(function_index) + 1U;
    next_value = 1;
    next_place = 1;
    next_symbol = 1;
    next_operation = 1;
    current_region = 1;
    next_region = 2;
    typed_region root_region;
    root_region.id = 1;
    root_region.kind = region_kind::function_body;
    result->regions.push_back(root_region);
    if (!populate_typed_type_table(unit, result, diagnostic))
        return false;
    for (const typed_type &type : result->types)
        typed_types_by_id.emplace(type.id, &type);
    add_member_subobjects = [&](value_id parent_value, type_id parent_type,
        value_id storage_base, symbol_id storage_symbol,
        const std::vector<place_projection> &parent_path,
        const std::vector<type_id> &ancestry) -> bool
    {
        const auto owner_type = typed_types_by_id.find(parent_type);
        if (owner_type == typed_types_by_id.end()
            || owner_type->second->kind != typed_type_kind::class_type)
            return true;
        if (std::find(ancestry.begin(), ancestry.end(), parent_type) != ancestry.end())
            return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                "recursive by-value class member layout cannot be represented in typed storage");
        std::vector<type_id> child_ancestry = ancestry;
        child_ancestry.push_back(parent_type);
        for (const typed_field &field : owner_type->second->fields)
        {
            const auto member_type = typed_types_by_id.find(field.type);
            if (field.is_dependent || field.is_borrowed
                || member_type == typed_types_by_id.end()
                || member_type->second->kind != typed_type_kind::class_type)
                continue;
            const std::size_t element_count = field.array_extent > 0
                ? field.array_extent : 1;
            for (std::size_t element = 0; element < element_count; ++element)
            {
                typed_value child_value;
                child_value.id = next_value++;
                child_value.type = field.type;
                const auto parent = std::find_if(result->values.begin(),
                    result->values.end(), [&](const typed_value &value)
                    {
                        return value.id == parent_value;
                    });
                const bool borrowed_parent = parent != result->values.end()
                    && parent->category == value_category::borrow;
                child_value.category = borrowed_parent
                    ? value_category::borrow
                    : (class_requires_lifecycle(unit,
                        member_type->second->canonical_name.c_str())
                        ? value_category::owned_object : value_category::scalar);
                child_value.access = access_mode::read_write;
                child_value.lifetime_token = borrowed_parent
                    ? parent->lifetime_token : child_value.id;
                child_value.is_definition = true;
                source_origin member_origin;
                member_origin.source_unit_id = result->source_unit_id;
                member_origin.role = "member-subobject-" + field.name;
                member_origin.compiler_generated = true;
                child_value.provenance.push_back(std::move(member_origin));
                result->values.push_back(child_value);

                typed_place child_place;
                child_place.id = next_place++;
                child_place.base_value = storage_base;
                child_place.type = field.type;
                child_place.access = access_mode::read_write;
                child_place.projections = parent_path;
                child_place.symbol = storage_symbol;
                child_place.projections.push_back({place_projection::kind::field,
                    field.member_id, field.type});
                if (field.array_extent > 0)
                    child_place.projections.push_back({place_projection::kind::index,
                        static_cast<std::uint32_t>(element + 1), field.type});
                result->places.push_back(child_place);
                lifecycle_children_by_value[parent_value].push_back(child_value.id);
                lifecycle_children_by_value.emplace(child_value.id,
                    std::vector<value_id>{});
                lifecycle_place_by_value.emplace(child_value.id, child_place.id);
                std::vector<place_projection> child_path = child_place.projections;
                if (!add_member_subobjects(child_value.id, field.type, storage_base,
                        storage_symbol,
                        child_path, child_ancestry))
                    return false;
            }
        }
        return true;
    };

    for (index = 0; index < unit->data_count; ++index)
    {
        const t_cblc_data_item &item = unit->data_items[index];
        const bool indexed_class_borrow = item.is_alias && item.is_borrowed
            && item.kind == CBLC_DATA_KIND_STRUCT_POINTER
            && std::strchr(item.source_name, '[') != nullptr;
        const bool indexed_borrow_scope_visible = indexed_class_borrow
            && item.scope_id < unit->scope_count
            && std::strcmp(unit->scopes[item.scope_id].owner_function_name,
                function->source_name) == 0;
        const bool is_function_return_storage = function->return_cobol_name[0] != '\0'
            && std::strcmp(item.cobol_name, function->return_cobol_name) == 0;
        bool is_function_parameter_storage = false;
        bool has_external_lifetime = false;
        symbol_id item_symbol = 0;
        typed_value value;
        typed_place place;
        const type_id item_type = scalar_type_id(item.kind) != 0
            ? scalar_type_id(item.kind)
            : (item.kind == CBLC_DATA_KIND_STRUCT_POINTER
                && declared_struct_type_id(unit, item.struct_type_name) != 0
                ? canonical_pointer_type_id(item.struct_type_name)
                : class_type_id(unit, &item));

        for (std::size_t parameter_index = 0;
            parameter_index < function->parameter_count; ++parameter_index)
        {
            const t_cblc_parameter &parameter = function->parameters[parameter_index];
            if (parameter.actual_source_name[0] != '\0'
                && std::strcmp(parameter.actual_source_name, item.source_name) == 0)
            {
                is_function_parameter_storage = true;
                break;
            }
        }
        has_external_lifetime = !item.is_function_local
            && !is_function_parameter_storage;
        if ((!is_function_return_storage && (item.is_function_local
                ? std::strcmp(item.owner_function_name, function->source_name) != 0
                : (!item.is_active && !is_function_parameter_storage
                    && !indexed_borrow_scope_visible)))
            || item.is_shadowed || (item.is_alias && !indexed_class_borrow)
            || item_type == 0)
            continue;
        if (item.source_name[0] != '\0')
        {
            typed_symbol symbol;
            symbol.id = next_symbol++;
            symbol.canonical_name = item.local_source_name[0] != '\0'
                ? item.local_source_name : item.source_name;
            symbol.type = item_type;
            symbol.category = item.is_borrowed
                || (has_external_lifetime
                    && class_type_id(unit, &item) != 0)
                ? value_category::borrow
                : (item.kind == CBLC_DATA_KIND_STRUCT_POINTER
                    ? value_category::pointer_or_handle
                    : (class_type_id(unit, &item) != 0
                    ? (class_requires_lifecycle(unit, item.struct_type_name)
                        ? value_category::owned_object : value_category::scalar)
                    : value_category::scalar));
            symbol.is_parameter = is_function_parameter_storage;
            item_symbol = symbol.id;
            result->symbols.push_back(std::move(symbol));
        }
        const bool is_class_value = class_type_id(unit, &item) != 0;
        const bool is_owned_class = is_class_value && !item.is_borrowed
            && !has_external_lifetime
            && class_requires_lifecycle(unit, item.struct_type_name);
        const std::size_t element_count = is_class_value && item.array_count > 0
            ? item.array_count : 1;
        value_id aggregate_storage = 0;
        if (is_class_value && item.array_count > 0)
        {
            value.id = next_value++;
            value.type = item_type;
            value.category = value_category::compiler_only;
            value.access = item.is_const ? access_mode::read_only : access_mode::read_write;
            value.is_definition = true;
            value.array_extent = item.array_count;
            result->values.push_back(value);
            aggregate_storage = value.id;
        }
        std::vector<value_id> lifecycle_values;
        std::vector<place_id> lifecycle_places;
        lifecycle_values.reserve(element_count);
        lifecycle_places.reserve(element_count);
        for (std::size_t element = 0; element < element_count; ++element)
        {
            value.id = next_value++;
            value.type = item_type;
            value.category = item.is_borrowed
                || (is_class_value && has_external_lifetime)
                ? value_category::borrow
                : (item.kind == CBLC_DATA_KIND_STRUCT_POINTER
                    ? value_category::pointer_or_handle
                    : (is_owned_class
                        ? value_category::owned_object : value_category::scalar));
            value.access = item.is_const ? access_mode::read_only : access_mode::read_write;
            value.lifetime_token = is_owned_class ? next_value : 0;
            value.is_definition = true;
            if (is_class_value && item.array_count > 0)
            {
                source_origin element_origin;
                element_origin.source_unit_id = result->source_unit_id;
                element_origin.role = "array-element-" + std::to_string(element + 1);
                element_origin.compiler_generated = true;
                value.provenance = {std::move(element_origin)};
            }
            place.id = next_place++;
            place.base_value = aggregate_storage != 0 ? aggregate_storage : value.id;
            place.type = item_type;
            place.access = item.is_const ? access_mode::read_only : access_mode::read_write;
            place.symbol = item_symbol;
            place.projections.clear();
            if (aggregate_storage != 0)
                place.projections.push_back({place_projection::kind::index,
                    static_cast<std::uint32_t>(element + 1), item_type});
            result->values.push_back(value);
            result->places.push_back(place);
            lifecycle_values.push_back(value.id);
            lifecycle_places.push_back(place.id);
            lifecycle_place_by_value.emplace(value.id, place.id);
            lifecycle_children_by_value.emplace(value.id, std::vector<value_id>{});
            if (element == 0)
            {
                value_by_item[&item] = value.id;
                place_by_item.emplace(&item, place.id);
                if (storage_bindings)
                    storage_bindings->push_back({item.cobol_name, value.id,
                        place.id});
            }
            if (is_class_value)
            {
                const value_id storage_base = aggregate_storage != 0
                    ? aggregate_storage : value.id;
                const std::vector<type_id> no_ancestry;
                if (!add_member_subobjects(value.id, item_type, storage_base,
                        item_symbol,
                        place.projections, no_ancestry))
                    return false;
            }
            value.provenance.clear();
        }
        if (is_class_value)
        {
            lifecycle_values_by_item.emplace(&item, std::move(lifecycle_values));
            lifecycle_places_by_item.emplace(&item, std::move(lifecycle_places));
        }
    }

    /* Class-valued aliases are borrows into an owning object. Preserve that
     * ownership relation in typed IR instead of leaving their lifetime token
     * unset; copy assignment may read such a member while its owner is live. */
    for (std::size_t alias_index = 0; alias_index < unit->data_count;
        ++alias_index)
    {
        const t_cblc_data_item &alias = unit->data_items[alias_index];
        if (class_type_id(unit, &alias) == 0)
            continue;
        const t_cblc_data_item *owner_item = nullptr;
        std::size_t owner_name_length = 0;
        for (std::size_t candidate_index = 0;
            candidate_index < unit->data_count; ++candidate_index)
        {
            const t_cblc_data_item &candidate =
                unit->data_items[candidate_index];
            const std::size_t candidate_name_length =
                std::strlen(candidate.source_name);
            if (&candidate == &alias || candidate_name_length == 0
                || candidate_name_length <= owner_name_length
                || value_by_item.find(&candidate) == value_by_item.end()
                || std::strncmp(alias.source_name, candidate.source_name,
                    candidate_name_length) != 0
                || (alias.source_name[candidate_name_length] != '.'
                    && alias.source_name[candidate_name_length] != '[')
                || class_type_id(unit, &candidate) == 0)
                continue;
            owner_item = &candidate;
            owner_name_length = candidate_name_length;
        }
        if (!owner_item)
            continue;
        const auto owner_value_id = value_by_item.find(owner_item);
        if (owner_value_id == value_by_item.end())
            continue;
        const auto owner_value = std::find_if(result->values.begin(),
            result->values.end(), [&](const typed_value &value)
            {
                return value.id == owner_value_id->second;
            });
        if (owner_value == result->values.end()
            || owner_value->lifetime_token == 0)
            continue;
        const auto alias_values = lifecycle_values_by_item.find(&alias);
        if (alias_values == lifecycle_values_by_item.end())
            continue;
        for (value_id alias_value_id : alias_values->second)
        {
            const auto alias_value = std::find_if(result->values.begin(),
                result->values.end(), [&](const typed_value &value)
                {
                    return value.id == alias_value_id;
                });
            if (alias_value != result->values.end()
                && alias_value->category == value_category::borrow
                && alias_value->lifetime_token == 0)
                alias_value->lifetime_token = owner_value->lifetime_token;
        }
    }

    std::function<bool(value_id, const t_cblc_data_item *,
        const t_cblc_statement *, const source_origin &)> append_construct_tree;
    std::function<bool(value_id, const source_origin &, bool)> append_destroy_tree;
    std::function<bool(value_id, std::vector<value_id> &)> append_completed_cleanup;
    append_completed_cleanup = [&](value_id object,
        std::vector<value_id> &destruction_order) -> bool
    {
        const auto children = lifecycle_children_by_value.find(object);
        if (children == lifecycle_children_by_value.end())
            return fail(diagnostic, verification_error::invalid_type_or_projection,
                0, "constructor cleanup is missing a completed-subobject graph");
        destruction_order.push_back(object);
        std::size_t child_index = children->second.size();
        while (child_index > 0)
            if (!append_completed_cleanup(children->second[--child_index],
                    destruction_order))
                return false;
        return true;
    };
    append_construct_tree = [&](value_id object, const t_cblc_data_item *source_item,
        const t_cblc_statement *source_statement,
        const source_origin &source_origin_value) -> bool
    {
        const auto children = lifecycle_children_by_value.find(object);
        if (children == lifecycle_children_by_value.end())
            return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                "owned class object is missing its member-lifecycle graph");
        for (value_id child : children->second)
        {
            source_origin member_origin = source_origin_value;
            member_origin.role = "implicit-member-construction";
            member_origin.compiler_generated = true;
            if (!append_construct_tree(child, nullptr, nullptr, member_origin))
                return false;
        }

        const auto object_value = std::find_if(result->values.begin(),
            result->values.end(), [&](const typed_value &value)
            {
                return value.id == object;
            });
        const auto object_place = lifecycle_place_by_value.find(object);
        if (object_value == result->values.end()
            || object_place == lifecycle_place_by_value.end())
            return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                "owned class object has no finalized value/place mapping");

        t_cblc_data_item synthetic_item;
        t_cblc_statement synthetic_statement;
        const t_cblc_data_item *constructor_item = source_item;
        const t_cblc_statement *constructor_statement = source_statement;
        if (!constructor_item || !constructor_statement)
        {
            std::memset(&synthetic_item, 0, sizeof(synthetic_item));
            synthetic_item.kind = CBLC_DATA_KIND_STRUCT;
            const auto object_type = typed_types_by_id.find(object_value->type);
            if (object_type == typed_types_by_id.end()
                || ft_strlcpy(synthetic_item.struct_type_name,
                    object_type->second->canonical_name.c_str(),
                    sizeof(synthetic_item.struct_type_name))
                    >= sizeof(synthetic_item.struct_type_name))
                return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                    "member constructor type has no source-level declaration name");
            std::memset(&synthetic_statement, 0, sizeof(synthetic_statement));
            constructor_item = &synthetic_item;
            constructor_statement = &synthetic_statement;
        }
        const bool may_throw = class_type_id(unit, constructor_item) != 0
            && constructor_may_throw(unit, constructor_item, *constructor_statement);
        const t_cblc_struct_type *constructor_owner = find_class_type(unit,
            constructor_item->struct_type_name);
        const t_cblc_constructor *selected_constructor = nullptr;
        if (!constructor_owner)
            return fail(diagnostic,
                verification_error::invalid_type_or_projection, 0,
                "constructed object has no canonical constructor owner type");
        selected_constructor = cblc_find_constructor_for_arguments(unit,
            constructor_owner, constructor_statement->call_arguments,
            constructor_statement->call_argument_count);
        const bool copy_initialization = selected_constructor
            && selected_constructor->is_copy_constructor;
        value_id copy_source_value = 0;
        place_id copy_source_place = 0;
        operation_id copy_source_producer = 0;
        if (copy_initialization)
        {
            const char *argument_begin = constructor_statement->call_arguments;
            const char *argument_end = argument_begin
                ? argument_begin + std::strlen(argument_begin) : nullptr;
            trim_expression(&argument_begin, &argument_end);
            if (!argument_begin || argument_begin == argument_end
                || constructor_statement->call_argument_count != 1)
                return fail(diagnostic,
                    verification_error::unsupported_legacy_statement, 0,
                    "copy construction requires one directly named class source");
            const std::string source_name(argument_begin, argument_end);
            const t_cblc_data_item *copy_source_item = find_class_item(unit,
                function, source_name.c_str());
            const auto source_values = copy_source_item
                ? lifecycle_values_by_item.find(copy_source_item)
                : lifecycle_values_by_item.end();
            const auto source_places = copy_source_item
                ? lifecycle_places_by_item.find(copy_source_item)
                : lifecycle_places_by_item.end();
            if (!copy_source_item
                || std::strcmp(copy_source_item->struct_type_name,
                    constructor_owner->source_name) != 0
                || source_values == lifecycle_values_by_item.end()
                || source_places == lifecycle_places_by_item.end()
                || source_values->second.size() != 1
                || source_places->second.size() != 1)
                return fail(diagnostic,
                    verification_error::unsupported_legacy_type, 0,
                    "copy construction source must resolve to one live object of the constructor's concrete class");
            copy_source_value = source_values->second.front();
            copy_source_place = source_places->second.front();
            for (auto prior = result->operations.rbegin();
                prior != result->operations.rend(); ++prior)
                if (std::find(prior->results.begin(), prior->results.end(),
                        copy_source_value) != prior->results.end())
                {
                    copy_source_producer = prior->id;
                    break;
                }
        }
        if (!append_constructor_callable(result, unit, constructor_owner,
                selected_constructor, object_value->type,
                callable_kind::constructor, may_throw))
            return fail(diagnostic, verification_error::incomplete_call_target, 0,
                "selected constructor has no complete, conflict-free typed callable descriptor");
        typed_operation construction;
        construction.id = next_operation++;
        construction.operation = copy_initialization
            ? opcode::copy_construct : opcode::construct;
        construction.places.push_back(object_place->second);
        construction.results.push_back(object);
        if (copy_initialization)
        {
            construction.operands.push_back(copy_source_value);
            construction.effects.reads.push_back(copy_source_place);
            if (copy_source_producer != 0)
                construction.must_follow.push_back(copy_source_producer);
        }
        construction.callee = canonical_constructor_symbol_id(constructor_owner,
            selected_constructor);
        construction.effects.constructs = true;
        construction.effects.exceptions = may_throw
            ? exception_effect::unknown : exception_effect::non_throwing;
        construction.effects.may_fail = may_throw;
        operation_outcome completed;
        completed.kind = outcome_kind::normal;
        completed.live.push_back(object);
        construction.outcomes.push_back(completed);
        if (may_throw)
        {
            operation_outcome failed_construction;
            failed_construction.kind = outcome_kind::thrown_exception;
            failed_construction.uninitialized.push_back(object);
            if (source_statement)
                for (std::size_t cleanup_index = 0;
                    cleanup_index < source_statement->exception_cleanup_count;
                    ++cleanup_index)
                {
                    const t_cblc_data_item *cleanup_item = find_class_item(unit,
                        function,
                        source_statement->exception_cleanup_targets[cleanup_index]);
                    const auto cleanup_values = cleanup_item
                        ? lifecycle_values_by_item.find(cleanup_item)
                        : lifecycle_values_by_item.end();
                    if (!cleanup_item
                        || cleanup_values == lifecycle_values_by_item.end())
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            construction.id,
                            "throwing constructor cleanup target has no lifecycle mapping");
                    std::size_t element = cleanup_values->second.size();
                    while (element > 0)
                    {
                        --element;
                        const value_id cleanup_value =
                            cleanup_values->second[element];
                        if (cleanup_value == object)
                        {
                            /* The object whose constructor failed never became
                             * live. Its already-constructed members did, so
                             * unwind those in reverse member order without
                             * scheduling the incomplete object itself. */
                            const auto child_graph =
                                lifecycle_children_by_value.find(cleanup_value);
                            if (child_graph
                                == lifecycle_children_by_value.end())
                                return fail(diagnostic,
                                    verification_error::invalid_type_or_projection,
                                    construction.id,
                                    "failed constructor cleanup is missing its member-lifecycle graph");
                            std::size_t child = child_graph->second.size();
                            while (child > 0)
                                if (!append_completed_cleanup(
                                        child_graph->second[--child],
                                        failed_construction.destroyed))
                                    return false;
                        }
                        else if (!append_completed_cleanup(cleanup_value,
                                failed_construction.destroyed))
                            return false;
                    }
                }
            if (!failed_construction.destroyed.empty())
                construction.effects.destroys = true;
            construction.outcomes.push_back(failed_construction);
        }
        construction.provenance.push_back(source_origin_value);
        return append_typed_operation(result, current_region,
            std::move(construction), diagnostic);
    };
    append_destroy_tree = [&](value_id object,
        const source_origin &source_origin_value, bool scope_exit) -> bool
    {
        const auto object_place = lifecycle_place_by_value.find(object);
        const auto children = lifecycle_children_by_value.find(object);
        const auto object_value = std::find_if(result->values.begin(),
            result->values.end(), [&](const typed_value &value)
            {
                return value.id == object;
            });
        if (object_place == lifecycle_place_by_value.end()
            || children == lifecycle_children_by_value.end()
            || object_value == result->values.end())
            return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                "destroyed class object is missing its member-lifecycle graph");
        const auto object_type = typed_types_by_id.find(object_value->type);
        const t_cblc_struct_type *destructor_owner =
            object_type == typed_types_by_id.end() ? nullptr
                : find_class_type(unit, object_type->second->canonical_name.c_str());
        if (!destructor_owner || !append_destructor_callable(result,
                destructor_owner, object_value->type))
            return fail(diagnostic, verification_error::incomplete_call_target, 0,
                "destroyed object has no canonical destructor dispatch descriptor");
        typed_operation destruction;
        destruction.id = next_operation++;
        destruction.operation = scope_exit ? opcode::scope_exit : opcode::destroy;
        if (destructor_owner->has_destructor
            || destructor_owner->has_destructor_definition)
            destruction.callee = destructor_symbol_id(destructor_owner);
        destruction.places.push_back(object_place->second);
        const auto destroyed_typed_place = std::find_if(result->places.begin(),
            result->places.end(), [&](const typed_place &place)
            {
                return place.id == object_place->second;
            });
        if (destroyed_typed_place != result->places.end())
            for (const place_projection &projection :
                destroyed_typed_place->projections)
                if (projection.index_value != 0)
                    for (const typed_operation &prior : result->operations)
                        if (std::find(prior.results.begin(), prior.results.end(),
                                projection.index_value) != prior.results.end())
                            destruction.must_follow.push_back(prior.id);
        destruction.effects.destroys = true;
        destruction.effects.exceptions = exception_effect::non_throwing;
        operation_outcome destroyed;
        destroyed.destroyed.push_back(object);
        destruction.outcomes.push_back(destroyed);
        destruction.provenance.push_back(source_origin_value);
        if (!append_typed_operation(result, current_region,
                std::move(destruction), diagnostic))
            return false;
        std::size_t child_index = children->second.size();
        while (child_index > 0)
        {
            --child_index;
            source_origin member_origin = source_origin_value;
            member_origin.role = "implicit-member-destruction";
            member_origin.compiler_generated = true;
            if (!append_destroy_tree(children->second[child_index], member_origin,
                    scope_exit))
                return false;
        }
        return true;
    };

    std::function<bool(value_id)> append_entry_live_subobjects;
    append_entry_live_subobjects = [&](value_id object) -> bool
    {
        const auto children = lifecycle_children_by_value.find(object);
        const auto object_value = std::find_if(result->values.begin(),
            result->values.end(), [&](const typed_value &value)
            {
                return value.id == object;
            });
        if (children == lifecycle_children_by_value.end()
            || object_value == result->values.end())
            return fail(diagnostic,
                verification_error::invalid_type_or_projection, 0,
                "by-value parameter is missing its owned-subobject lifecycle graph");
        for (value_id child : children->second)
            if (!append_entry_live_subobjects(child))
                return false;
        if (object_value->category == value_category::owned_object)
            result->entry_live_values.push_back(object);
        return true;
    };

    for (std::size_t item_index = 0; item_index < unit->data_count;
        ++item_index)
    {
        const t_cblc_data_item &item = unit->data_items[item_index];
        if (item.kind != CBLC_DATA_KIND_STRUCT
            || lifecycle_values_by_item.find(&item)
                == lifecycle_values_by_item.end())
            continue;
        bool is_by_value_parameter = false;
        for (std::size_t parameter_index = 0;
            parameter_index < function->parameter_count; ++parameter_index)
        {
            const t_cblc_parameter &parameter = function->parameters[parameter_index];
            if (parameter.kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT
                && std::strcmp(parameter.type_name, item.struct_type_name) == 0
                && parameter.actual_source_name[0] != '\0'
                && std::strcmp(parameter.actual_source_name, item.source_name) == 0)
            {
                is_by_value_parameter = true;
                break;
            }
        }
        if (!is_by_value_parameter)
            continue;
        const auto parameter_values = lifecycle_values_by_item.find(&item);
        for (value_id parameter_value : parameter_values->second)
            if (!append_entry_live_subobjects(parameter_value))
                return false;
    }

    for (index = 0; index < function->statement_count; ++index)
    {
        const t_cblc_statement &statement = function->statements[index];
        source_origin origin = statement_origin(unit, statement, "source-statement");
        if ((statement.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
                || statement.type == CBLC_STATEMENT_DESTRUCT)
            && (statement.source_line == 0 || statement.source_column == 0
                || statement.source_length == 0))
        {
            origin.compiler_generated = true;
            origin.role = statement.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
                ? "implicit-object-construction" : "implicit-object-destruction";
        }

        if (statement.type == CBLC_STATEMENT_IF_BEGIN)
        {
            value_id condition;
            typed_region control;
            typed_region body;
            typed_region *parent = nullptr;

            if (statement.source_line == 0 || statement.source_column == 0
                || statement.source_length == 0
                || !lower_scalar_condition(unit, function, statement.source, result,
                    current_region, value_by_item, place_by_item, &next_place,
                    &next_value, &next_operation, origin, false, &condition,
                    diagnostic))
                return diagnostic && diagnostic->code != verification_error::none
                    ? false : fail(diagnostic,
                        verification_error::missing_provenance, 0,
                        "if condition requires source provenance and a supported scalar expression");
            for (typed_region &candidate : result->regions)
                if (candidate.id == current_region)
                    parent = &candidate;
            if (!parent)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "if statement parent region does not exist");
            control.id = next_region++;
            control.parent = current_region;
            control.kind = region_kind::conditional;
            control.condition = condition;
            parent->children.push_back(control.id);
            parent->sequence.push_back({typed_region::item::kind::child_region,
                control.id});
            body.id = next_region++;
            body.parent = control.id;
            body.kind = region_kind::lexical_scope;
            control.children.push_back(body.id);
            control.sequence.push_back({typed_region::item::kind::child_region,
                body.id});
            result->regions.push_back(control);
            result->regions.push_back(body);
            region_stack.push_back({control.id, current_region,
                CBLC_STATEMENT_IF_BEGIN, false});
            current_region = body.id;
            continue;
        }
        if (statement.type == CBLC_STATEMENT_WHILE_BEGIN)
        {
            value_id condition;
            typed_region control;
            typed_region body;
            typed_region *parent = nullptr;

            if (statement.source_line == 0 || statement.source_column == 0
                || statement.source_length == 0)
                return fail(diagnostic, verification_error::missing_provenance, 0,
                    "while condition requires parser-captured source provenance");
            for (typed_region &candidate : result->regions)
                if (candidate.id == current_region)
                    parent = &candidate;
            if (!parent)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "while statement parent region does not exist");
            control.id = next_region++;
            control.parent = current_region;
            control.kind = region_kind::loop;
            parent->children.push_back(control.id);
            parent->sequence.push_back({typed_region::item::kind::child_region,
                control.id});
            result->regions.push_back(control);
            if (!lower_scalar_condition(unit, function, statement.source, result,
                    control.id, value_by_item, place_by_item, &next_place,
                    &next_value, &next_operation, origin, true, &condition,
                    diagnostic))
                return false;
            for (typed_region &candidate : result->regions)
                if (candidate.id == control.id)
                    candidate.condition = condition;
            body.id = next_region++;
            body.parent = control.id;
            body.kind = region_kind::lexical_scope;
            for (typed_region &candidate : result->regions)
                if (candidate.id == control.id)
                {
                    candidate.children.push_back(body.id);
                    candidate.sequence.push_back({
                        typed_region::item::kind::child_region, body.id});
                    break;
                }
            result->regions.push_back(body);
            region_stack.push_back({control.id, current_region,
                CBLC_STATEMENT_WHILE_BEGIN, false});
            current_region = body.id;
            continue;
        }
        if (statement.type == CBLC_STATEMENT_IF_ELSE)
        {
            typed_region *control = nullptr;

            if (region_stack.empty()
                || region_stack.back().statement_kind != CBLC_STATEMENT_IF_BEGIN
                || region_stack.back().saw_else)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "else marker must match one if region and occur at most once");
            for (typed_region &candidate : result->regions)
                if (candidate.id == region_stack.back().control)
                    control = &candidate;
            if (!control)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "else marker references a missing if region");
            typed_region else_region;
            else_region.id = next_region++;
            else_region.parent = control->id;
            else_region.kind = region_kind::lexical_scope;
            control->children.push_back(else_region.id);
            control->sequence.push_back({typed_region::item::kind::child_region,
                else_region.id});
            result->regions.push_back(else_region);
            region_stack.back().saw_else = true;
            current_region = else_region.id;
            continue;
        }
        if (statement.type == CBLC_STATEMENT_IF_END)
        {
            if (region_stack.empty()
                || region_stack.back().statement_kind != CBLC_STATEMENT_IF_BEGIN)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "if end marker has no matching if region");
            current_region = region_stack.back().parent;
            region_stack.pop_back();
            continue;
        }
        if (statement.type == CBLC_STATEMENT_WHILE_END)
        {
            if (region_stack.empty()
                || region_stack.back().statement_kind != CBLC_STATEMENT_WHILE_BEGIN)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "while end marker has no matching loop region");
            current_region = region_stack.back().parent;
            region_stack.pop_back();
            continue;
        }

        if (statement.type == CBLC_STATEMENT_TRY_BEGIN)
        {
            typed_region control;
            typed_region body;
            typed_region *parent = nullptr;
            for (typed_region &candidate : result->regions)
                if (candidate.id == current_region)
                    parent = &candidate;
            if (!parent)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "try statement parent region does not exist");
            control.id = next_region++;
            control.parent = current_region;
            control.kind = region_kind::exception_try;
            control.source_exception_region_id =
                static_cast<std::uint64_t>(statement.exception_region_id) + 1U;
            parent->children.push_back(control.id);
            parent->sequence.push_back({typed_region::item::kind::child_region,
                control.id});
            result->regions.push_back(control);
            body.id = next_region++;
            body.parent = control.id;
            body.kind = region_kind::lexical_scope;
            for (typed_region &candidate : result->regions)
                if (candidate.id == control.id)
                {
                    candidate.children.push_back(body.id);
                    candidate.sequence.push_back({
                        typed_region::item::kind::child_region, body.id});
                    break;
                }
            result->regions.push_back(body);
            region_stack.push_back({control.id, current_region,
                CBLC_STATEMENT_TRY_BEGIN, false});
            current_region = body.id;
            continue;
        }
        if (statement.type == CBLC_STATEMENT_TRY_BODY_END)
        {
            if (region_stack.empty()
                || region_stack.back().statement_kind != CBLC_STATEMENT_TRY_BEGIN)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "try body end has no matching try region");
            current_region = region_stack.back().control;
            continue;
        }
        if (statement.type == CBLC_STATEMENT_CATCH_BEGIN)
        {
            typed_region handler;
            typed_region *control = nullptr;
            if (region_stack.empty()
                || region_stack.back().statement_kind != CBLC_STATEMENT_TRY_BEGIN
                || static_cast<std::uint64_t>(statement.exception_region_id) + 1U
                    != result->regions[region_stack.back().control - 1].source_exception_region_id
                || (!statement.exception_is_catch_all
                    && (statement.exception_type_id == 0
                        || std::strcmp(statement.exception_type_name, "int") != 0
                        || statement.exception_catch_is_reference)))
                return fail(diagnostic, verification_error::unsupported_legacy_statement,
                    0, "typed IR supports int catches by value and catch-all handlers; other payload forms require typed payload operations");
            for (typed_region &candidate : result->regions)
                if (candidate.id == region_stack.back().control)
                    control = &candidate;
            if (!control)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "catch handler references a missing try region");
            handler.id = next_region++;
            handler.parent = control->id;
            handler.kind = region_kind::exception_handler;
            handler.source_exception_region_id =
                static_cast<std::uint64_t>(statement.exception_region_id) + 1U;
            handler.exception_type_id = statement.exception_type_id;
            handler.catches_all = statement.exception_is_catch_all != 0;
            control->children.push_back(handler.id);
            control->sequence.push_back({typed_region::item::kind::child_region,
                handler.id});
            result->regions.push_back(handler);
            current_region = handler.id;
            if (statement.target[0] != '\0')
            {
                const t_cblc_data_item *binding = find_scalar_item(unit, function,
                    statement.target);
                const auto binding_place = binding
                    ? place_by_item.find(binding) : place_by_item.end();
                if (!binding || binding_place == place_by_item.end()
                    || binding->kind != CBLC_DATA_KIND_INT)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type, 0,
                        "int catch binding must resolve to a local int place");
                typed_operation bind;
                bind.id = next_operation++;
                bind.operation = opcode::catch_bind;
                bind.exception_type_id = statement.exception_type_id;
                bind.source_exception_region_id =
                    static_cast<std::uint64_t>(statement.exception_region_id) + 1U;
                bind.places.push_back(binding_place->second);
                bind.effects.writes.push_back(binding_place->second);
                bind.effects.exceptions = exception_effect::non_throwing;
                bind.outcomes.push_back({});
                source_origin binding_origin = origin;
                binding_origin.role = "catch-payload-binding";
                binding_origin.compiler_generated = true;
                bind.provenance.push_back(std::move(binding_origin));
                if (!append_typed_operation(result, current_region,
                        std::move(bind), diagnostic))
                    return false;
            }
            continue;
        }
        if (statement.type == CBLC_STATEMENT_CATCH_END)
        {
            if (region_stack.empty()
                || region_stack.back().statement_kind != CBLC_STATEMENT_TRY_BEGIN)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "catch end has no matching try region");
            current_region = region_stack.back().control;
            continue;
        }
        if (statement.type == CBLC_STATEMENT_TRY_END)
        {
            if (region_stack.empty()
                || region_stack.back().statement_kind != CBLC_STATEMENT_TRY_BEGIN)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "try end has no matching try region");
            current_region = region_stack.back().parent;
            region_stack.pop_back();
            continue;
        }

        if (statement.type == CBLC_STATEMENT_CALL
            || statement.type == CBLC_STATEMENT_CALL_ASSIGN)
        {
            const t_cblc_function *target = nullptr;
            std::vector<std::string> argument_texts;
            std::vector<lowered_scalar_operand> arguments;
            std::vector<type_id> parameter_types;
            std::vector<value_id> class_argument_temporaries;
            operation_id previous_argument_completion = 0;
            type_id result_type = 0;
            const t_cblc_data_item *return_target = nullptr;
            std::vector<std::uint64_t> exception_ids;
            bool unknown_exception_effect;
            if (statement.call_is_external
                || statement.call_identifier[0] == '\0'
                || !split_call_arguments(statement.call_arguments,
                    &argument_texts)
                || argument_texts.size() != statement.call_argument_count)
                return fail(diagnostic,
                    verification_error::unsupported_legacy_statement,
                    static_cast<operation_id>(index + 1),
                    "typed direct-call lowering requires a local function and a well-formed argument list");
            for (std::size_t candidate_index = 0;
                candidate_index < unit->function_count; ++candidate_index)
            {
                const t_cblc_function &candidate = unit->functions[candidate_index];
                if (std::strcmp(candidate.source_name,
                        statement.call_identifier) != 0
                    || candidate.is_template)
                    continue;
                if (target)
                    return fail(diagnostic,
                        verification_error::incomplete_call_target,
                        static_cast<operation_id>(index + 1),
                        "typed direct call name resolves to multiple concrete declarations");
                target = &candidate;
            }
            if (!target || !target->statements || target->statement_count == 0
                || target->parameter_count != argument_texts.size())
                return fail(diagnostic,
                    verification_error::unsupported_legacy_statement,
                    static_cast<operation_id>(index + 1),
                    "typed direct-call lowering currently defers external, intrinsic, and unresolved calls to the source emitter");

            parameter_types.reserve(target->parameter_count);
            arguments.reserve(target->parameter_count);
            for (std::size_t argument_index = 0;
                argument_index < target->parameter_count; ++argument_index)
            {
                const operation_id argument_operation_start = next_operation;
                const type_id expected_type = method_parameter_type_id(unit,
                    target->parameters[argument_index]);
                if (expected_type == 0)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "typed direct-call parameter has no concrete scalar or class-pointer representation");
                parameter_types.push_back(expected_type);
                lowered_scalar_operand argument;
                const std::string &argument_text = argument_texts[argument_index];
                const t_cblc_data_item *class_argument = find_class_item(unit,
                    function, argument_text.c_str());
                if (class_argument)
                {
                    const t_cblc_struct_type *argument_type = find_class_type(unit,
                        class_argument->struct_type_name);
                    const t_cblc_constructor *copy_constructor =
                        find_typed_copy_constructor(argument_type);
                    const auto source_place = place_by_item.find(class_argument);
                    const auto source_value = value_by_item.find(class_argument);
                    const type_id argument_type_id = class_type_id(unit,
                        class_argument);
                    if (!copy_constructor || !copy_constructor->has_definition
                        || !class_requires_lifecycle(unit,
                            class_argument->struct_type_name)
                        || source_place == place_by_item.end()
                        || source_value == value_by_item.end()
                        || argument_type_id == 0
                        || argument_type_id != expected_type)
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "typed direct by-value class arguments require a matching concrete parameter, live source, and defined copy constructor");

                    const value_id temporary_id = next_value++;
                    const place_id temporary_place_id = next_place++;
                    typed_value temporary_value;
                    temporary_value.id = temporary_id;
                    temporary_value.type = argument_type_id;
                    temporary_value.category = value_category::owned_object;
                    temporary_value.access = access_mode::read_write;
                    temporary_value.lifetime_token = temporary_id;
                    temporary_value.provenance.push_back(origin);
                    result->values.push_back(temporary_value);
                    typed_place temporary_place;
                    temporary_place.id = temporary_place_id;
                    temporary_place.base_value = temporary_id;
                    temporary_place.type = argument_type_id;
                    temporary_place.access = access_mode::read_write;
                    temporary_place.owner_lifetime_token = temporary_id;
                    result->places.push_back(temporary_place);
                    lifecycle_children_by_value.emplace(temporary_id,
                        std::vector<value_id>{});
                    lifecycle_place_by_value.emplace(temporary_id,
                        temporary_place_id);
                    const std::vector<place_projection> no_parent_path;
                    const std::vector<type_id> no_ancestry;
                    if (!add_member_subobjects(temporary_id, argument_type_id,
                            temporary_id, 0, no_parent_path, no_ancestry))
                        return false;
                    const auto temporary_children =
                        lifecycle_children_by_value.find(temporary_id);
                    if (temporary_children == lifecycle_children_by_value.end())
                        return fail(diagnostic,
                            verification_error::invalid_type_or_projection,
                            static_cast<operation_id>(index + 1),
                            "direct-call temporary has no owned-subobject lifecycle graph");
                    for (value_id child : temporary_children->second)
                    {
                        source_origin child_origin = origin;
                        child_origin.role = "direct-call-argument-subobject-construction";
                        child_origin.compiler_generated = true;
                        if (!append_construct_tree(child, nullptr, nullptr,
                                child_origin))
                            return false;
                    }

                    typed_operation copy;
                    copy.id = next_operation++;
                    copy.operation = opcode::copy_construct;
                    copy.callee = canonical_constructor_symbol_id(argument_type,
                        copy_constructor);
                    copy.operands.push_back(source_value->second);
                    copy.places.push_back(temporary_place_id);
                    copy.results.push_back(temporary_id);
                    const auto source_definition = std::find_if(
                        result->operations.rbegin(), result->operations.rend(),
                        [&](const typed_operation &candidate)
                        {
                            return std::find(candidate.results.begin(),
                                candidate.results.end(), source_value->second)
                                != candidate.results.end();
                        });
                    if (source_definition != result->operations.rend())
                        copy.must_follow.push_back(source_definition->id);
                    copy.effects.reads.push_back(source_place->second);
                    copy.effects.writes.push_back(temporary_place_id);
                    copy.effects.constructs = true;
                    const bool copy_may_throw =
                        typed_constructor_may_throw(copy_constructor);
                    if (!append_constructor_callable(result, unit,
                            argument_type, copy_constructor, argument_type_id,
                            callable_kind::copy_constructor, copy_may_throw))
                        return fail(diagnostic,
                            verification_error::incomplete_call_target,
                            copy.id,
                            "direct-call argument copy has no complete typed constructor descriptor");
                    copy.effects.may_fail = copy_may_throw;
                    copy.effects.exceptions = copy_may_throw
                        ? exception_effect::unknown
                        : exception_effect::non_throwing;
                    operation_outcome copied;
                    copied.live.push_back(temporary_id);
                    copy.outcomes.push_back(copied);
                    if (copy_may_throw)
                    {
                        operation_outcome copy_failed;
                        copy_failed.kind = outcome_kind::thrown_exception;
                        copy_failed.uninitialized.push_back(temporary_id);
                        std::size_t child_index = temporary_children->second.size();
                        while (child_index > 0)
                            if (!append_completed_cleanup(
                                    temporary_children->second[--child_index],
                                    copy_failed.destroyed))
                                return false;
                        copy.effects.destroys = !copy_failed.destroyed.empty();
                        copy.outcomes.push_back(copy_failed);
                    }
                    copy.provenance.push_back(origin);
                    if (!append_typed_operation(result, current_region,
                            std::move(copy), diagnostic))
                        return false;
                    argument.value = temporary_id;
                    argument.type = argument_type_id;
                    argument.producer = next_operation - 1;
                    class_argument_temporaries.push_back(temporary_id);
                }
                else
                {
                    const t_cblc_data_item *pointer_argument =
                        find_class_receiver_item(unit, function,
                            argument_text.c_str());
                    if (pointer_argument
                        && pointer_argument->kind
                            == CBLC_DATA_KIND_STRUCT_POINTER)
                    {
                        const auto pointer_place =
                            place_by_item.find(pointer_argument);
                        if (pointer_place == place_by_item.end())
                            return fail(diagnostic,
                                verification_error::unsupported_legacy_type,
                                static_cast<operation_id>(index + 1),
                                "direct class-pointer argument is outside finalized function storage");
                        const type_id pointer_type = canonical_pointer_type_id(
                            pointer_argument->struct_type_name);
                        if (pointer_type == 0 || pointer_type != expected_type
                            || !append_typed_pointer_type(unit, result,
                                pointer_argument->struct_type_name, diagnostic))
                            return fail(diagnostic,
                                verification_error::unsupported_legacy_type,
                                static_cast<operation_id>(index + 1),
                                "direct class-pointer argument does not match the concrete parameter type");
                        typed_value loaded_pointer;
                        loaded_pointer.id = next_value++;
                        loaded_pointer.type = pointer_type;
                        loaded_pointer.category =
                            value_category::pointer_or_handle;
                        loaded_pointer.access = pointer_argument->is_const
                            ? access_mode::read_only : access_mode::read_write;
                        loaded_pointer.provenance.push_back(origin);
                        result->values.push_back(loaded_pointer);
                        typed_operation pointer_load;
                        pointer_load.id = next_operation++;
                        pointer_load.operation = opcode::load;
                        pointer_load.places.push_back(pointer_place->second);
                        pointer_load.results.push_back(loaded_pointer.id);
                        pointer_load.effects.reads.push_back(pointer_place->second);
                        pointer_load.effects.exceptions =
                            exception_effect::non_throwing;
                        operation_outcome pointer_loaded;
                        pointer_loaded.initialized.push_back(loaded_pointer.id);
                        pointer_load.outcomes.push_back(pointer_loaded);
                        pointer_load.provenance.push_back(origin);
                        if (!append_typed_operation(result, current_region,
                                std::move(pointer_load), diagnostic))
                            return false;
                        argument.value = loaded_pointer.id;
                        argument.type = pointer_type;
                        argument.producer = next_operation - 1;
                    }
                    else if (pointer_argument)
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "typed direct class-pointer argument requires a single-level class pointer parameter");
                    else if (!lower_scalar_operand(unit, function,
                            argument_text.data(),
                            argument_text.data() + argument_text.size(), result,
                            current_region, value_by_item, place_by_item,
                            &next_place, &next_value, &next_operation, origin,
                            false, &argument, diagnostic))
                        return false;
                }
                if (argument.type != expected_type)
                {
                    if (argument.type != scalar_type_id(CBLC_DATA_KIND_INT)
                        || expected_type
                            != scalar_type_id(CBLC_DATA_KIND_DOUBLE))
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "typed direct-call argument requires an unsupported implicit conversion");
                    typed_value converted;
                    converted.id = next_value++;
                    converted.type = expected_type;
                    converted.category = value_category::scalar;
                    converted.access = access_mode::read_only;
                    converted.provenance.push_back(origin);
                    result->values.push_back(converted);
                    typed_operation conversion;
                    conversion.id = next_operation++;
                    conversion.operation = opcode::numeric_convert;
                    conversion.operands.push_back(argument.value);
                    conversion.results.push_back(converted.id);
                    conversion.must_follow.push_back(argument.producer);
                    conversion.effects.exceptions = exception_effect::non_throwing;
                    operation_outcome converted_outcome;
                    converted_outcome.initialized.push_back(converted.id);
                    conversion.outcomes.push_back(std::move(converted_outcome));
                    source_origin conversion_origin = origin;
                    conversion_origin.role =
                        "implicit-int-to-double-argument-conversion";
                    conversion_origin.compiler_generated = true;
                    conversion.provenance.push_back(
                        std::move(conversion_origin));
                    if (!append_typed_operation(result, current_region,
                            std::move(conversion), diagnostic))
                        return false;
                    argument.value = converted.id;
                    argument.type = expected_type;
                    argument.producer = next_operation - 1;
                }
                if (previous_argument_completion != 0)
                {
                    const auto first_argument_operation = std::find_if(
                        result->operations.begin(), result->operations.end(),
                        [&](const typed_operation &candidate)
                        {
                            return candidate.id == argument_operation_start;
                        });
                    if (first_argument_operation == result->operations.end())
                        return fail(diagnostic,
                            verification_error::use_before_definition,
                            static_cast<operation_id>(index + 1),
                            "direct-call argument evaluation did not produce an orderable operation");
                    first_argument_operation->must_follow.push_back(
                        previous_argument_completion);
                }
                previous_argument_completion = argument.producer;
                arguments.push_back(argument);
            }

            result_type = scalar_method_return_type_id(target->return_kind);
            if (statement.type == CBLC_STATEMENT_CALL_ASSIGN)
            {
                return_target = find_scalar_item(unit, function,
                    statement.target);
                if (result_type == 0 || !return_target || return_target->is_const
                    || scalar_type_id(return_target->kind) != result_type)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "typed direct-call result requires a matching mutable scalar target");
            }
            else if (target->return_kind != CBLC_FUNCTION_RETURN_VOID)
                return fail(diagnostic,
                    verification_error::unsupported_legacy_statement,
                    static_cast<operation_id>(index + 1),
                    "non-void direct call must store its result in a typed target");

            unknown_exception_effect = target->exception_types_unknown != 0
                || (target->may_throw && target->exception_type_count == 0);
            for (std::size_t exception_index = 0;
                exception_index < target->exception_type_count;
                ++exception_index)
            {
                const std::uint64_t exception_id =
                    target->exception_type_ids[exception_index];
                if (exception_id == 0)
                    unknown_exception_effect = true;
                else if (std::find(exception_ids.begin(), exception_ids.end(),
                        exception_id) == exception_ids.end())
                    exception_ids.push_back(exception_id);
            }
            typed_callable direct_target;
            direct_target.id = function_symbol_id(target, parameter_types,
                result_type);
            direct_target.kind = callable_kind::function;
            direct_target.canonical_name = target->source_name;
            direct_target.parameter_types = parameter_types;
            direct_target.result_type = result_type;
            direct_target.exceptions = unknown_exception_effect
                ? exception_effect::unknown
                : (!exception_ids.empty()
                    ? exception_effect::may_throw_known
                    : exception_effect::non_throwing);
            direct_target.known_exception_type_ids = exception_ids;
            direct_target.has_definition = true;
            if (!append_typed_callable(result, std::move(direct_target)))
                return fail(diagnostic,
                    verification_error::incomplete_call_target,
                    static_cast<operation_id>(index + 1),
                    "direct call has a missing or conflicting canonical function descriptor");

            typed_operation call;
            call.id = next_operation++;
            call.operation = opcode::call;
            call.callee = function_symbol_id(target, parameter_types,
                result_type);
            call.effects.exceptions = unknown_exception_effect
                ? exception_effect::unknown
                : (!exception_ids.empty()
                    ? exception_effect::may_throw_known
                    : exception_effect::non_throwing);
            call.effects.may_fail = call.effects.exceptions
                != exception_effect::non_throwing;
            call.effects.known_exception_type_ids = exception_ids;
            call.source_exception_region_id = active_exception_region_id(
                *result, current_region);
            for (const lowered_scalar_operand &argument : arguments)
            {
                call.operands.push_back(argument.value);
                call.must_follow.push_back(argument.producer);
            }
            if (return_target)
            {
                const auto return_place = place_by_item.find(return_target);
                if (return_place == place_by_item.end())
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        call.id,
                        "direct-call result target is outside finalized function storage");
                typed_value returned;
                returned.id = next_value++;
                returned.type = result_type;
                returned.category = value_category::scalar;
                returned.access = access_mode::read_write;
                returned.provenance.push_back(origin);
                result->values.push_back(returned);
                call.results.push_back(returned.id);
                call.places.push_back(return_place->second);
                call.effects.writes.push_back(return_place->second);
                operation_outcome completed;
                completed.initialized.push_back(returned.id);
                call.outcomes.push_back(std::move(completed));
            }
            else
                call.outcomes.push_back({});
            if (call.effects.may_fail)
            {
                if (unknown_exception_effect)
                {
                    operation_outcome thrown;
                    thrown.kind = outcome_kind::thrown_exception;
                    if (!call.results.empty())
                        thrown.uninitialized.push_back(call.results.front());
                    call.outcomes.push_back(std::move(thrown));
                }
                for (std::size_t exception_index = 0;
                    exception_index < exception_ids.size(); ++exception_index)
                {
                    operation_outcome thrown;
                    thrown.kind = outcome_kind::thrown_exception;
                    thrown.exception_type_id = exception_ids[exception_index];
                    if (!call.results.empty())
                        thrown.uninitialized.push_back(call.results.front());
                    call.outcomes.push_back(std::move(thrown));
                }
            }
            call.provenance.push_back(origin);
            if (!append_typed_operation(result, current_region,
                    std::move(call), diagnostic))
                return false;
            std::size_t temporary_index = class_argument_temporaries.size();
            while (temporary_index > 0)
            {
                --temporary_index;
                source_origin temporary_origin = origin;
                temporary_origin.role = "direct-call-argument-destruction";
                temporary_origin.compiler_generated = true;
                if (!append_destroy_tree(class_argument_temporaries[temporary_index],
                        temporary_origin, true))
                    return false;
            }
            continue;
        }

        if (statement.type == CBLC_STATEMENT_METHOD_CALL
            || statement.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        {
            const char *receiver_name =
                statement.type == CBLC_STATEMENT_METHOD_CALL
                    ? statement.target : statement.source;
            const t_cblc_data_item *receiver = find_class_receiver_item(unit,
                function, receiver_name);
            const t_cblc_struct_type *owner = receiver
                ? find_class_type(unit, receiver->struct_type_name) : nullptr;
            bool ambiguous_method = false;
            std::vector<std::string> argument_texts;
            std::vector<lowered_scalar_operand> lowered_arguments;
            std::vector<type_id> argument_types;
            std::vector<value_id> class_argument_temporaries;
            operation_id previous_argument_completion = 0;
            const t_cblc_method *method = nullptr;
            const auto receiver_place = receiver
                ? place_by_item.find(receiver) : place_by_item.end();
            const bool has_well_formed_arguments =
                split_call_arguments(statement.call_arguments, &argument_texts)
                && argument_texts.size() == statement.call_argument_count;
            if (!receiver || !owner || receiver_place == place_by_item.end()
                || receiver->is_const || !has_well_formed_arguments)
            {
                char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(message, sizeof(message),
                    "typed receiver map: place=%d alias=%d borrow=%d kind=%d active=%d shadow=%d local=%d type=%llu name=%.32s",
                    receiver_place != place_by_item.end(),
                    receiver && receiver->is_alias,
                    receiver && receiver->is_borrowed,
                    receiver ? static_cast<int>(receiver->kind) : -1,
                    receiver && receiver->is_active,
                    receiver && receiver->is_shadowed,
                    receiver && receiver->is_function_local,
                    static_cast<unsigned long long>(receiver
                        ? canonical_pointer_type_id(receiver->struct_type_name) : 0),
                    receiver ? receiver->source_name : "<none>");
                return fail(diagnostic,
                    verification_error::unsupported_legacy_statement,
                    static_cast<operation_id>(index + 1),
                    message);
            }

            place_id receiver_object_place = receiver_place->second;
            if (receiver->kind == CBLC_DATA_KIND_STRUCT_POINTER)
            {
                const auto receiver_value = value_by_item.find(receiver);
                const type_id owner_type_id = declared_struct_type_id(unit,
                    receiver->struct_type_name);
                if (receiver_value == value_by_item.end() || owner_type_id == 0)
                    return fail(diagnostic,
                        verification_error::invalid_type_or_projection,
                        static_cast<operation_id>(index + 1),
                        "borrowed method receiver has no concrete pointer value or pointee type");
                typed_place dereferenced_receiver;
                dereferenced_receiver.id = next_place++;
                dereferenced_receiver.base_value = receiver_value->second;
                dereferenced_receiver.type = owner_type_id;
                dereferenced_receiver.access = access_mode::read_write;
                dereferenced_receiver.projections.push_back({
                    place_projection::kind::dereference, 1U, owner_type_id});
                receiver_object_place = dereferenced_receiver.id;
                result->places.push_back(std::move(dereferenced_receiver));
            }

            lowered_arguments.reserve(argument_texts.size());
            argument_types.reserve(argument_texts.size());
            for (const std::string &argument_text : argument_texts)
            {
                const operation_id argument_operation_start = next_operation;
                lowered_scalar_operand argument;
                const t_cblc_data_item *class_argument = find_class_item(unit,
                    function, argument_text.c_str());
                if (class_argument)
                {
                    const t_cblc_struct_type *argument_type = find_class_type(unit,
                        class_argument->struct_type_name);
                    const t_cblc_constructor *copy_constructor =
                        find_typed_copy_constructor(argument_type);
                    const auto source_place = place_by_item.find(class_argument);
                    const auto source_value = value_by_item.find(class_argument);
                    if (!copy_constructor || !copy_constructor->has_definition
                        || !class_requires_lifecycle(unit,
                            class_argument->struct_type_name)
                        || source_place == place_by_item.end()
                        || source_value == value_by_item.end())
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "typed by-value class arguments require a defined copy constructor and a live source object");

                    const type_id argument_type_id = class_type_id(unit,
                        class_argument);
                    const value_id temporary_id = next_value++;
                    const place_id temporary_place_id = next_place++;
                    typed_value temporary_value;
                    temporary_value.id = temporary_id;
                    temporary_value.type = argument_type_id;
                    temporary_value.category = value_category::owned_object;
                    temporary_value.access = access_mode::read_write;
                    temporary_value.lifetime_token = temporary_id;
                    temporary_value.provenance.push_back(origin);
                    result->values.push_back(temporary_value);
                    typed_place temporary_place;
                    temporary_place.id = temporary_place_id;
                    temporary_place.base_value = temporary_id;
                    temporary_place.type = argument_type_id;
                    temporary_place.access = access_mode::read_write;
                    temporary_place.owner_lifetime_token = temporary_id;
                    result->places.push_back(temporary_place);
                    lifecycle_children_by_value.emplace(temporary_id,
                        std::vector<value_id>{});
                    lifecycle_place_by_value.emplace(temporary_id,
                        temporary_place_id);
                    const std::vector<place_projection> no_parent_path;
                    const std::vector<type_id> no_ancestry;
                    if (!add_member_subobjects(temporary_id, argument_type_id,
                            temporary_id, 0, no_parent_path, no_ancestry))
                        return false;
                    const auto temporary_children =
                        lifecycle_children_by_value.find(temporary_id);
                    if (temporary_children == lifecycle_children_by_value.end())
                        return fail(diagnostic,
                            verification_error::invalid_type_or_projection,
                            static_cast<operation_id>(index + 1),
                            "by-value call temporary has no owned-subobject lifecycle graph");
                    for (value_id child : temporary_children->second)
                    {
                        source_origin child_origin = origin;
                        child_origin.role = "by-value-argument-subobject-construction";
                        child_origin.compiler_generated = true;
                        if (!append_construct_tree(child, nullptr, nullptr,
                                child_origin))
                            return false;
                    }

                    typed_operation copy;
                    copy.id = next_operation++;
                    copy.operation = opcode::copy_construct;
                    copy.callee = canonical_constructor_symbol_id(argument_type,
                        copy_constructor);
                    copy.operands.push_back(source_value->second);
                    copy.places.push_back(temporary_place_id);
                    copy.results.push_back(temporary_id);
                    const auto source_definition = std::find_if(
                        result->operations.rbegin(), result->operations.rend(),
                        [&](const typed_operation &candidate)
                        {
                            return std::find(candidate.results.begin(),
                                candidate.results.end(), source_value->second)
                                != candidate.results.end();
                        });
                    if (source_definition != result->operations.rend())
                        copy.must_follow.push_back(source_definition->id);
                    copy.effects.reads.push_back(source_place->second);
                    copy.effects.writes.push_back(temporary_place_id);
                    copy.effects.constructs = true;
                    const bool copy_may_throw =
                        typed_constructor_may_throw(copy_constructor);
                    if (!append_constructor_callable(result, unit,
                            argument_type, copy_constructor, argument_type_id,
                            callable_kind::copy_constructor, copy_may_throw))
                    {
                        std::string detail =
                            "by-value copy construction has no complete typed callable descriptor for '";
                        detail += argument_type->source_name;
                        detail += "' (constructor signature";
                        for (std::size_t parameter_index = 0;
                            parameter_index < copy_constructor->parameter_count;
                            ++parameter_index)
                        {
                            const t_cblc_parameter &parameter =
                                copy_constructor->parameters[parameter_index];
                            detail += parameter_index == 0 ? " " : ", ";
                            detail += parameter.type_name;
                            detail += " [type id ";
                            detail += std::to_string(method_parameter_type_id(
                                unit, parameter));
                            detail += "]";
                        }
                        detail += ")";
                        const symbol_id constructor_id =
                            canonical_constructor_symbol_id(argument_type,
                                copy_constructor);
                        for (const typed_callable &existing : result->callables)
                            if (existing.id == constructor_id)
                            {
                                detail += "; existing descriptor is kind=";
                                detail += std::to_string(
                                    static_cast<int>(existing.kind));
                                detail += ", owner=";
                                detail += std::to_string(existing.owner_type);
                                detail += ", exceptions=";
                                detail += std::to_string(
                                    static_cast<int>(existing.exceptions));
                                detail += ", defined=";
                                detail += existing.has_definition ? "true" : "false";
                                detail += ", parameters=";
                                for (std::size_t parameter_index = 0;
                                    parameter_index < existing.parameter_types.size();
                                    ++parameter_index)
                                {
                                    if (parameter_index != 0)
                                        detail += ',';
                                    detail += std::to_string(
                                        existing.parameter_types[parameter_index]);
                                }
                                break;
                            }
                        return fail(diagnostic,
                            verification_error::incomplete_call_target,
                            copy.id,
                            detail.c_str());
                    }
                    copy.effects.may_fail = copy_may_throw;
                    copy.effects.exceptions = copy_may_throw
                        ? exception_effect::unknown
                        : exception_effect::non_throwing;
                    operation_outcome copied;
                    copied.live.push_back(temporary_id);
                    copy.outcomes.push_back(copied);
                    if (copy_may_throw)
                    {
                        operation_outcome copy_failed;
                        copy_failed.kind = outcome_kind::thrown_exception;
                        copy_failed.uninitialized.push_back(temporary_id);
                        std::size_t child_index = temporary_children->second.size();
                        while (child_index > 0)
                            if (!append_completed_cleanup(
                                    temporary_children->second[--child_index],
                                    copy_failed.destroyed))
                                return false;
                        copy.effects.destroys = !copy_failed.destroyed.empty();
                        copy.outcomes.push_back(copy_failed);
                    }
                    copy.provenance.push_back(origin);
                    if (!append_typed_operation(result, current_region,
                            std::move(copy), diagnostic))
                        return false;
                    argument.value = temporary_id;
                    argument.type = argument_type_id;
                    argument.producer = next_operation - 1;
                    class_argument_temporaries.push_back(temporary_id);
                }
                else
                {
                    const t_cblc_data_item *pointer_argument =
                        find_class_receiver_item(unit, function,
                            argument_text.c_str());
                    if (pointer_argument
                        && pointer_argument->kind == CBLC_DATA_KIND_STRUCT_POINTER)
                    {
                        const auto pointer_place = place_by_item.find(pointer_argument);
                        const auto pointer_value = value_by_item.find(pointer_argument);
                        if (pointer_place == place_by_item.end()
                            || pointer_value == value_by_item.end())
                            return fail(diagnostic,
                                verification_error::unsupported_legacy_type,
                                static_cast<operation_id>(index + 1),
                                "class-pointer argument is outside finalized function storage");

                        const type_id pointer_type = canonical_pointer_type_id(
                            pointer_argument->struct_type_name);
                        if (pointer_type == 0
                            || !append_typed_pointer_type(unit, result,
                                pointer_argument->struct_type_name, diagnostic))
                            return false;
                        typed_value loaded_pointer;
                        loaded_pointer.id = next_value++;
                        loaded_pointer.type = pointer_type;
                        loaded_pointer.category = value_category::pointer_or_handle;
                        loaded_pointer.access = pointer_argument->is_const
                            ? access_mode::read_only : access_mode::read_write;
                        loaded_pointer.provenance.push_back(origin);
                        result->values.push_back(loaded_pointer);
                        typed_operation pointer_load;
                        pointer_load.id = next_operation++;
                        pointer_load.operation = opcode::load;
                        pointer_load.places.push_back(pointer_place->second);
                        pointer_load.results.push_back(loaded_pointer.id);
                        pointer_load.effects.reads.push_back(pointer_place->second);
                        pointer_load.effects.exceptions = exception_effect::non_throwing;
                        operation_outcome pointer_loaded;
                        pointer_loaded.initialized.push_back(loaded_pointer.id);
                        pointer_load.outcomes.push_back(pointer_loaded);
                        pointer_load.provenance.push_back(origin);
                        if (!append_typed_operation(result, current_region,
                                std::move(pointer_load), diagnostic))
                            return false;
                        argument.value = loaded_pointer.id;
                        argument.type = pointer_type;
                        argument.producer = next_operation - 1;
                    }
                    else if (!lower_scalar_operand(unit, function,
                            argument_text.data(),
                            argument_text.data() + argument_text.size(), result,
                            current_region, value_by_item, place_by_item,
                            &next_place, &next_value,
                            &next_operation, origin, false, &argument, diagnostic))
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "typed method arguments support scalar values, class-pointer borrows, and copyable RAII class values");
                }
                if (previous_argument_completion != 0)
                {
                    const auto first_argument_operation = std::find_if(
                        result->operations.begin(), result->operations.end(),
                        [&](const typed_operation &candidate)
                        {
                            return candidate.id == argument_operation_start;
                        });
                    if (first_argument_operation == result->operations.end())
                        return fail(diagnostic,
                            verification_error::use_before_definition,
                            static_cast<operation_id>(index + 1),
                            "method argument evaluation did not produce an orderable operation");
                    first_argument_operation->must_follow.push_back(
                        previous_argument_completion);
                }
                previous_argument_completion = argument.producer;
                lowered_arguments.push_back(argument);
                argument_types.push_back(argument.type);
            }
            method = find_method_for_typed_arguments(unit, owner,
                statement.call_identifier, argument_types, &ambiguous_method);
            if (!method || method->return_kind != CBLC_FUNCTION_RETURN_VOID)
            {
                if (!method || ambiguous_method)
                    return fail(diagnostic,
                        ambiguous_method
                            ? verification_error::incomplete_call_target
                            : verification_error::unsupported_legacy_statement,
                        static_cast<operation_id>(index + 1),
                        ambiguous_method
                            ? "instance call resolves to multiple methods with the same typed signature"
                            : "instance call does not resolve to a concrete typed-signature method");
                if (method->return_kind != CBLC_FUNCTION_RETURN_VOID
                    && statement.type == CBLC_STATEMENT_METHOD_CALL)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_statement,
                        static_cast<operation_id>(index + 1),
                        "non-void method call must store its result in a typed target");
            }

            for (std::size_t argument_index = 0;
                argument_index < lowered_arguments.size(); ++argument_index)
            {
                lowered_scalar_operand &argument = lowered_arguments[argument_index];
                const type_id parameter_type = method_parameter_type_id(unit,
                    method->parameters[argument_index]);
                if (parameter_type == 0)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "resolved method parameter has no concrete typed-IR representation");
                if (argument.type == parameter_type)
                    continue;
                if (argument.type != scalar_type_id(CBLC_DATA_KIND_INT)
                    || parameter_type != scalar_type_id(CBLC_DATA_KIND_DOUBLE))
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "resolved method argument requires an unsupported implicit conversion");

                typed_value converted;
                converted.id = next_value++;
                converted.type = parameter_type;
                converted.category = value_category::scalar;
                converted.access = access_mode::read_only;
                converted.provenance.push_back(origin);
                result->values.push_back(converted);
                typed_operation conversion;
                conversion.id = next_operation++;
                conversion.operation = opcode::numeric_convert;
                conversion.operands.push_back(argument.value);
                conversion.results.push_back(converted.id);
                conversion.must_follow.push_back(argument.producer);
                conversion.effects.exceptions = exception_effect::non_throwing;
                operation_outcome converted_outcome;
                converted_outcome.initialized.push_back(converted.id);
                conversion.outcomes.push_back(std::move(converted_outcome));
                source_origin conversion_origin = origin;
                conversion_origin.role = "implicit-int-to-double-argument-conversion";
                conversion_origin.compiler_generated = true;
                conversion.provenance.push_back(std::move(conversion_origin));
                if (!append_typed_operation(result, current_region,
                        std::move(conversion), diagnostic))
                    return false;
                argument.value = converted.id;
                argument.type = parameter_type;
                argument.producer = next_operation - 1;
            }

            const t_cblc_data_item *return_target = nullptr;
            type_id return_type = 0;
            bool borrow_result = false;
            if (statement.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
            {
                return_type = scalar_method_return_type_id(method->return_kind);
                if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER
                    && declared_struct_type_id(unit,
                        method->return_type_name) != 0)
                {
                    return_type = canonical_pointer_type_id(
                        method->return_type_name);
                    return_target = find_class_receiver_item(unit, function,
                        statement.target);
                    borrow_result = true;
                }
                else
                    return_target = find_scalar_item(unit, function,
                        statement.target);
                if (return_type == 0 || !return_target || return_target->is_const
                    || (borrow_result
                        ? (return_target->kind != CBLC_DATA_KIND_STRUCT_POINTER
                            || canonical_pointer_type_id(
                                return_target->struct_type_name) != return_type)
                        : scalar_type_id(return_target->kind) != return_type))
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "typed method-result adapter requires a matching mutable scalar or concrete class-pointer borrow slot");
            }

            typed_operation call;
            std::vector<std::uint64_t> known_exception_type_ids;
            bool unknown_exception_effect = false;
            derive_method_exception_summary(method,
                &known_exception_type_ids, &unknown_exception_effect);
            call.id = next_operation++;
            call.operation = opcode::call;
            call.callee = method_symbol_id(owner, method);
            call.places.push_back(receiver_object_place);
            call.effects.reads.push_back(receiver_object_place);
            call.effects.writes.push_back(receiver_object_place);
            if (return_target)
            {
                const auto return_place = place_by_item.find(return_target);
                if (return_place == place_by_item.end())
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        call.id,
                        "method result target is outside the finalized function storage map");
                typed_value returned;
                returned.id = next_value++;
                returned.type = return_type;
                returned.category = borrow_result
                    ? value_category::borrow : value_category::scalar;
                returned.access = access_mode::read_write;
                returned.provenance.push_back(origin);
                result->values.push_back(returned);
                call.results.push_back(returned.id);
                call.places.push_back(return_place->second);
                call.effects.writes.push_back(return_place->second);
            }
            for (const lowered_scalar_operand &argument : lowered_arguments)
            {
                call.operands.push_back(argument.value);
                call.must_follow.push_back(argument.producer);
            }
            call.effects.may_fail = !known_exception_type_ids.empty()
                || unknown_exception_effect;
            call.source_exception_region_id = active_exception_region_id(
                *result, current_region);
            call.effects.exceptions = unknown_exception_effect
                ? exception_effect::unknown
                : (!known_exception_type_ids.empty()
                    ? exception_effect::may_throw_known
                    : exception_effect::non_throwing);
            for (std::uint64_t exception_id : known_exception_type_ids)
            {
                if (exception_id == 0
                    || std::find(call.effects.known_exception_type_ids.begin(),
                        call.effects.known_exception_type_ids.end(), exception_id)
                        != call.effects.known_exception_type_ids.end())
                    return fail(diagnostic,
                        verification_error::missing_effect_or_outcome,
                        call.id,
                        "resolved method has a zero or duplicate exception type ID");
                call.effects.known_exception_type_ids.push_back(exception_id);
            }
            operation_outcome normal_completion;
            if (!call.results.empty())
                normal_completion.initialized.push_back(call.results.front());
            call.outcomes.push_back(std::move(normal_completion));
            if (call.effects.may_fail)
            {
                if (unknown_exception_effect)
                {
                    operation_outcome thrown;
                    thrown.kind = outcome_kind::thrown_exception;
                    if (!call.results.empty())
                        thrown.uninitialized.push_back(call.results.front());
                    call.outcomes.push_back(std::move(thrown));
                }
                for (std::size_t exception_index = 0;
                    exception_index
                        < call.effects.known_exception_type_ids.size();
                    ++exception_index)
                {
                    operation_outcome thrown;
                    thrown.kind = outcome_kind::thrown_exception;
                    thrown.exception_type_id =
                        call.effects.known_exception_type_ids[exception_index];
                    if (!call.results.empty())
                        thrown.uninitialized.push_back(call.results.front());
                    call.outcomes.push_back(std::move(thrown));
                }
            }
            typed_callable method_target;
            method_target.id = call.callee;
            method_target.kind = callable_kind::method;
            method_target.owner_type = declared_struct_type_id(unit,
                owner->source_name);
            method_target.overload_index = method->overload_index;
            method_target.canonical_name = std::string(owner->source_name)
                + "::" + method->source_name;
            method_target.parameter_types.reserve(method->parameter_count);
            for (std::size_t parameter_index = 0;
                parameter_index < method->parameter_count; ++parameter_index)
            {
                const type_id parameter_type = method_parameter_type_id(unit,
                    method->parameters[parameter_index]);
                if (parameter_type == 0)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        call.id,
                        "resolved method signature contains an unsupported parameter type");
                method_target.parameter_types.push_back(parameter_type);
            }
            method_target.result_type = return_type;
            method_target.exceptions = call.effects.exceptions;
            method_target.known_exception_type_ids =
                call.effects.known_exception_type_ids;
            method_target.has_definition = method->has_definition != 0;
            if (!append_typed_callable(result, std::move(method_target)))
                return fail(diagnostic,
                    verification_error::incomplete_call_target,
                    call.id,
                    "resolved method call has a missing or conflicting canonical callable descriptor");
            call.provenance.push_back(origin);
            if (!append_typed_operation(result, current_region,
                    std::move(call), diagnostic))
                return false;
            std::size_t temporary_index = class_argument_temporaries.size();
            while (temporary_index > 0)
            {
                --temporary_index;
                source_origin temporary_origin = origin;
                temporary_origin.role = "by-value-call-argument-destruction";
                temporary_origin.compiler_generated = true;
                if (!append_destroy_tree(class_argument_temporaries[temporary_index],
                        temporary_origin, true))
                    return false;
            }
            continue;
        }

        if (statement.type != CBLC_STATEMENT_ASSIGNMENT
            && statement.type != CBLC_STATEMENT_COMPUTE
            && statement.type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
            && statement.type != CBLC_STATEMENT_DESTRUCT
            && statement.type != CBLC_STATEMENT_THROW
            && statement.type != CBLC_STATEMENT_RETURN
            && statement.type != CBLC_STATEMENT_IF_BEGIN
            && statement.type != CBLC_STATEMENT_IF_ELSE
            && statement.type != CBLC_STATEMENT_IF_END)
            return fail(diagnostic, verification_error::unsupported_legacy_statement,
                static_cast<operation_id>(index + 1),
                "legacy adapter currently lowers only scalar assignment and return statements");
        if (statement.type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
            && statement.type != CBLC_STATEMENT_DESTRUCT
            && (statement.source_line == 0 || statement.source_column == 0))
            return fail(diagnostic, verification_error::unsupported_legacy_statement,
                static_cast<operation_id>(index + 1),
                "typed IR migration adapter defers statements without parser-captured source coordinates to the legacy emitter");

        const char *expression = statement.source;
        while (*expression == ' ' || *expression == '\t' || *expression == '\r'
            || *expression == '\n')
            expression += 1;
        if (statement.type == CBLC_STATEMENT_ASSIGNMENT
            || statement.type == CBLC_STATEMENT_COMPUTE)
        {
            if (statement.type == CBLC_STATEMENT_ASSIGNMENT)
            {
                const t_cblc_data_item *class_target = find_class_item(unit,
                    function, statement.target);
                const t_cblc_data_item *class_source = find_class_item(unit,
                    function, expression);
                if (class_target || class_source)
                {
                    if (!class_target || !class_source
                        || class_target->kind != CBLC_DATA_KIND_STRUCT
                        || class_source->kind != CBLC_DATA_KIND_STRUCT
                        || class_target->array_count != 0
                        || class_source->array_count != 0
                        || std::strcmp(class_target->struct_type_name,
                            class_source->struct_type_name) != 0
                        || class_target->is_const)
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "typed class assignment requires mutable, same-typed scalar class objects");
                    const t_cblc_struct_type *class_type = find_class_type(unit,
                        class_target->struct_type_name);
                    const auto target_place = place_by_item.find(class_target);
                    const auto source_place = place_by_item.find(class_source);
                    const auto target_value = value_by_item.find(class_target);
                    const auto source_value = value_by_item.find(class_source);
                    bool ambiguous_assignment = false;
                    const std::vector<type_id> assignment_argument_types{
                        class_type ? class_type_id(unit, class_target) : 0};
                    const t_cblc_method *assignment_method = class_type
                        ? find_method_for_typed_arguments(unit, class_type,
                            "operator=", assignment_argument_types,
                            &ambiguous_assignment) : nullptr;
                    if (!class_type || !class_type->has_copy_assignment
                        || !assignment_method || ambiguous_assignment
                        || !assignment_method->has_definition
                        || assignment_method->return_kind != CBLC_FUNCTION_RETURN_VOID
                        || assignment_method->exception_types_unknown
                        || assignment_method->exception_type_count != 0
                        || target_place == place_by_item.end()
                        || source_place == place_by_item.end()
                        || target_value == value_by_item.end()
                        || source_value == value_by_item.end())
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "typed class assignment requires one defined, non-throwing matching copy-assignment overload and finalized storage");
                    for (std::size_t method_statement = 0;
                        method_statement < assignment_method->statement_count;
                        ++method_statement)
                    {
                        const t_cblc_statement_type method_statement_type =
                            assignment_method->statements[method_statement].type;
                        if (method_statement_type == CBLC_STATEMENT_THROW
                            || method_statement_type == CBLC_STATEMENT_CALL
                            || method_statement_type == CBLC_STATEMENT_CALL_ASSIGN
                            || method_statement_type == CBLC_STATEMENT_METHOD_CALL
                            || method_statement_type == CBLC_STATEMENT_METHOD_CALL_ASSIGN
                            || assignment_method->statements[method_statement].exception_may_throw)
                            return fail(diagnostic,
                                verification_error::unsupported_legacy_type,
                                static_cast<operation_id>(index + 1),
                                "typed copy-assignment dispatch currently requires a non-throwing body without nested calls");
                    }
                    typed_callable assignment_target;
                    assignment_target.id = method_symbol_id(class_type,
                        assignment_method);
                    assignment_target.kind = callable_kind::copy_assignment;
                    assignment_target.owner_type = class_type_id(unit,
                        class_target);
                    assignment_target.overload_index =
                        assignment_method->overload_index;
                    assignment_target.canonical_name = std::string(
                        class_type->source_name) + "::"
                        + assignment_method->source_name;
                    assignment_target.parameter_types = assignment_argument_types;
                    assignment_target.exceptions = exception_effect::non_throwing;
                    assignment_target.has_definition = assignment_method->has_definition;
                    if (assignment_target.id == 0
                        || assignment_target.owner_type == 0
                        || !append_typed_callable(result,
                            std::move(assignment_target)))
                        return fail(diagnostic,
                            verification_error::incomplete_call_target,
                            static_cast<operation_id>(index + 1),
                            "typed copy-assignment target has a missing or conflicting canonical callable descriptor");
                    typed_operation copy_assignment;
                    copy_assignment.id = next_operation++;
                    copy_assignment.operation = opcode::copy_assign;
                    copy_assignment.callee = method_symbol_id(class_type,
                        assignment_method);
                    copy_assignment.operands.push_back(source_value->second);
                    copy_assignment.places.push_back(target_place->second);
                    copy_assignment.effects.reads.push_back(target_place->second);
                    copy_assignment.effects.writes.push_back(target_place->second);
                    copy_assignment.effects.exceptions = exception_effect::non_throwing;
                    copy_assignment.outcomes.push_back({});
                    for (const typed_operation &prior : result->operations)
                        if (std::find(prior.results.begin(), prior.results.end(),
                                target_value->second) != prior.results.end()
                            || std::find(prior.results.begin(), prior.results.end(),
                                source_value->second) != prior.results.end())
                            copy_assignment.must_follow.push_back(prior.id);
                    copy_assignment.provenance.push_back(origin);
                    if (!append_typed_operation(result, current_region,
                            std::move(copy_assignment), diagnostic))
                        return false;
                    continue;
                }
            }
            const t_cblc_data_item *target = find_scalar_item(unit, function, statement.target);
            place_id target_place_id = 0;
            type_id target_type = target ? scalar_type_id(target->kind) : 0;
            if (target)
            {
                const auto target_place = place_by_item.find(target);
                if (target_place == place_by_item.end())
                    return fail(diagnostic, verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "scalar assignment target is outside the finalized function storage map");
                target_place_id = target_place->second;
                if (target->is_const)
                    return fail(diagnostic, verification_error::immutable_write,
                        static_cast<operation_id>(index + 1),
                        "scalar assignment target is immutable");
            }
            else if (!append_scalar_member_place(unit, function, statement.target,
                    value_by_item, place_by_item, result, &next_place,
                    &target_place_id, &target_type))
                return fail(diagnostic, verification_error::unsupported_legacy_type,
                    static_cast<operation_id>(index + 1),
                    "scalar adapter requires a mutable scalar or resolved scalar member assignment target");
            const auto target_place_value = std::find_if(result->places.begin(),
                result->places.end(), [&](const typed_place &place)
                {
                    return place.id == target_place_id;
                });
            if (target_place_value == result->places.end()
                || target_place_value->access != access_mode::read_write)
                return fail(diagnostic, verification_error::unsupported_legacy_type,
                    static_cast<operation_id>(index + 1),
                    "scalar assignment target is not writable typed storage");

            typed_operation assignment;
            assignment.id = next_operation++;
            assignment.operation = opcode::assign;
            assignment.places.push_back(target_place_id);
            assignment.effects.writes.push_back(target_place_id);
            assignment.effects.exceptions = exception_effect::non_throwing;
            assignment.outcomes.push_back({});
            assignment.provenance.push_back(origin);

            if (target_type == 0)
                return fail(diagnostic, verification_error::unsupported_legacy_type,
                    assignment.id, "assignment target has no supported scalar type");
            lowered_scalar_operand source;
            if (!lower_scalar_operand(unit, function, statement.source,
                    statement.source + std::strlen(statement.source), result,
                    current_region, value_by_item, place_by_item, &next_place,
                    &next_value, &next_operation, origin, false, &source,
                    diagnostic))
                return false;
            if (source.type != target_type)
                return fail(diagnostic,
                    verification_error::unsupported_legacy_type,
                    assignment.id,
                    "scalar assignment source and target types do not match");
            assignment.operands.push_back(source.value);
            if (source.producer != 0)
                assignment.must_follow.push_back(source.producer);
            assignment.effects.may_fail = false;
            if (!append_typed_operation(result, current_region,
                    std::move(assignment), diagnostic))
                return false;
            continue;
        }

        if (statement.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT)
        {
            if (statement.is_move_construct || statement.is_relocate_construct)
            {
                auto resolve_indexed_base = [&](const char *expression_text,
                    std::string *base_name, value_id *index_value,
                    operation_id *index_producer) -> bool
                {
                    std::string index_expression;
                    if (!split_single_indexed_name(expression_text, base_name,
                            &index_expression))
                    {
                        if (!expression_text)
                            return false;
                        const char *begin = expression_text;
                        const char *end = expression_text
                            + std::strlen(expression_text);
                        trim_expression(&begin, &end);
                        if (begin == end)
                            return false;
                        base_name->assign(begin, end);
                        *index_value = 0;
                        *index_producer = 0;
                        return true;
                    }
                    lowered_scalar_operand lowered_index;
                    if (!lower_scalar_operand(unit, function,
                            index_expression.c_str(),
                            index_expression.c_str() + index_expression.size(),
                            result, current_region, value_by_item, place_by_item,
                            &next_place, &next_value, &next_operation, origin,
                            false, &lowered_index, diagnostic))
                        return false;
                    if (lowered_index.type != scalar_type_id(CBLC_DATA_KIND_INT))
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "indexed lifecycle operand must resolve to the canonical int type");
                    *index_value = lowered_index.value;
                    *index_producer = lowered_index.producer;
                    return true;
                };
                std::string destination_base_name;
                std::string source_base_name;
                value_id destination_index_value = 0;
                value_id source_index_value = 0;
                operation_id destination_index_producer = 0;
                operation_id source_index_producer = 0;
                if (!resolve_indexed_base(statement.target,
                        &destination_base_name, &destination_index_value,
                        &destination_index_producer)
                    || !resolve_indexed_base(statement.call_arguments,
                        &source_base_name, &source_index_value,
                        &source_index_producer))
                {
                    if (diagnostic && diagnostic->code != verification_error::none)
                        return false;
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_statement,
                        static_cast<operation_id>(index + 1),
                        "typed move/relocation index expressions must be a single canonical int operand");
                }
                const t_cblc_data_item *destination_item =
                    find_class_receiver_item(unit, function,
                        destination_base_name.c_str());
                const t_cblc_data_item *source_receiver_item =
                    find_class_receiver_item(unit, function,
                        source_base_name.c_str());
                const t_cblc_data_item *source_item = source_receiver_item
                    && source_receiver_item->kind == CBLC_DATA_KIND_STRUCT
                    ? source_receiver_item : nullptr;
                const t_cblc_data_item *source_pointer_item = source_receiver_item
                    && source_receiver_item->kind == CBLC_DATA_KIND_STRUCT_POINTER
                    ? source_receiver_item : nullptr;
                const t_cblc_struct_type *owner = nullptr;
                const t_cblc_constructor *move_constructor = nullptr;
                const t_cblc_constructor *copy_constructor = nullptr;
                const t_cblc_constructor *selected_constructor = nullptr;
                const auto destination_pointer_value = destination_item
                    ? value_by_item.find(destination_item) : value_by_item.end();
                const type_id owner_type = destination_item
                    ? declared_struct_type_id(unit,
                        destination_item->struct_type_name) : 0;
                bool use_move = statement.is_move_construct != 0;

                if (!destination_item
                    || destination_item->kind != CBLC_DATA_KIND_STRUCT_POINTER
                    || owner_type == 0)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_statement,
                        static_cast<operation_id>(index + 1),
                        "typed move/relocation destination must resolve to a declared class pointer");
                if (destination_pointer_value == value_by_item.end())
                    return fail(diagnostic,
                        verification_error::invalid_lifetime_transition,
                        static_cast<operation_id>(index + 1),
                        "typed move/relocation destination pointer has no tracked lifetime value");
                if (source_item == nullptr && source_pointer_item == nullptr)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "typed move/relocation source must resolve to a class object or class pointer");
                if (source_item
                    && (source_item->kind != CBLC_DATA_KIND_STRUCT
                        || source_item->array_count != 0
                        || std::strcmp(source_item->struct_type_name,
                            destination_item->struct_type_name) != 0))
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "typed move/relocation source class must match its destination pointee type");
                if (source_pointer_item
                    && (source_pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER
                        || std::strcmp(source_pointer_item->struct_type_name,
                            destination_item->struct_type_name) != 0))
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "typed move/relocation source pointer must match its destination pointee type");

                owner = find_class_type(unit, destination_item->struct_type_name);
                if (!owner || owner_type == 0)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "typed move/relocation has no canonical pointee class declaration");
                for (std::size_t constructor_index = 0;
                    constructor_index < owner->constructor_count;
                    ++constructor_index)
                {
                    const t_cblc_constructor &candidate =
                        owner->constructors[constructor_index];
                    if (candidate.is_move_constructor)
                        move_constructor = &candidate;
                    if (candidate.is_copy_constructor)
                        copy_constructor = &candidate;
                }
                if (statement.is_relocate_construct)
                    use_move = move_constructor && move_constructor->has_definition
                        && !typed_constructor_may_throw(move_constructor);
                selected_constructor = use_move
                    ? move_constructor : copy_constructor;
                if (!selected_constructor || !selected_constructor->has_definition
                    || selected_constructor->parameter_count != 1)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "typed relocation requires a defined nothrow move constructor or a defined copy constructor; explicit move requires a defined move constructor");
                const t_cblc_parameter &constructor_parameter =
                    selected_constructor->parameters[0];
                const bool parameter_matches =
                    (constructor_parameter.kind
                            == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER
                        || constructor_parameter.kind
                            == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
                    && std::strcmp(constructor_parameter.type_name,
                        owner->source_name) == 0;
                if (!parameter_matches)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "typed move/copy constructor parameter must name the same concrete class");
                if (explicitly_constructed_indirect_values.find(
                        statement.target)
                    != explicitly_constructed_indirect_values.end())
                    return fail(diagnostic,
                        verification_error::invalid_lifetime_transition,
                        static_cast<operation_id>(index + 1),
                        "placement construction cannot begin while this pointer target already has a tracked live object");

                typed_value destination;
                destination.id = next_value++;
                destination.type = owner_type;
                /* The destination address is borrowed, but successful placement
                 * construction creates a separately owned object lifetime there.
                 * Keep the storage token for provenance while tracking the
                 * object's live/moved/destroyed state as an owned value. */
                destination.category = value_category::owned_object;
                destination.access = access_mode::read_write;
                destination.lifetime_token = destination_pointer_value->second;
                destination.provenance.push_back(origin);
                result->values.push_back(destination);
                typed_place destination_place;
                destination_place.id = next_place++;
                destination_place.base_value = destination_pointer_value->second;
                destination_place.type = owner_type;
                destination_place.access = access_mode::read_write;
                destination_place.owner_lifetime_token =
                    destination_pointer_value->second;
                const type_id destination_pointer_type =
                    canonical_pointer_type_id(
                        destination_item->struct_type_name);
                std::vector<place_projection> destination_path;
                if (destination_index_value != 0)
                    destination_path.push_back({place_projection::kind::index,
                        0, destination_pointer_type, destination_index_value});
                destination_path.push_back({
                    place_projection::kind::dereference, 0, owner_type});
                destination_place.projections = destination_path;
                result->places.push_back(destination_place);
                lifecycle_place_by_value.emplace(destination.id,
                    destination_place.id);
                lifecycle_children_by_value.emplace(destination.id,
                    std::vector<value_id>{});
                const auto destination_pointer_place = place_by_item.find(
                    destination_item);
                const auto destination_symbol = destination_pointer_place
                    == place_by_item.end()
                    ? result->symbols.end()
                    : std::find_if(result->symbols.begin(), result->symbols.end(),
                        [&](const typed_symbol &symbol)
                        {
                            return symbol.id != 0
                                && std::any_of(result->places.begin(),
                                    result->places.end(),
                                    [&](const typed_place &candidate)
                                    {
                                        return candidate.id
                                                == destination_pointer_place->second
                                            && candidate.symbol == symbol.id;
                                    });
                        });
                const std::vector<type_id> no_ancestry;
                if (destination_symbol != result->symbols.end()
                    && !add_member_subobjects(destination.id, owner_type,
                        destination_pointer_value->second,
                        destination_symbol->id, destination_path, no_ancestry))
                    return false;

                value_id source_value_id = 0;
                place_id source_place_id = 0;
                operation_id source_preparation_id = 0;
                if (source_item)
                {
                    const auto source_values =
                        lifecycle_values_by_item.find(source_item);
                    const auto source_places =
                        lifecycle_places_by_item.find(source_item);
                    if (source_values == lifecycle_values_by_item.end()
                        || source_places == lifecycle_places_by_item.end()
                        || source_values->second.size() != 1
                        || source_places->second.size() != 1)
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "typed move/relocation source object has no finalized place/value mapping");
                    source_value_id = source_values->second.front();
                    source_place_id = source_places->second.front();
                    for (auto prior = result->operations.rbegin();
                        prior != result->operations.rend(); ++prior)
                        if (std::find(prior->results.begin(), prior->results.end(),
                                source_value_id) != prior->results.end())
                        {
                            source_preparation_id = prior->id;
                            break;
                        }
                }
                else
                {
                    const auto source_pointer_value = value_by_item.find(
                        source_pointer_item);
                    if (source_pointer_value == value_by_item.end())
                        return fail(diagnostic,
                            verification_error::unsupported_legacy_type,
                            static_cast<operation_id>(index + 1),
                            "typed move/relocation source pointer has no finalized value mapping");
                    typed_place source_place;
                    source_place.id = next_place++;
                    source_place.base_value = source_pointer_value->second;
                    source_place.type = owner_type;
                    source_place.access = access_mode::read_only;
                    source_place.owner_lifetime_token =
                        source_pointer_value->second;
                    if (source_index_value != 0)
                        source_place.projections.push_back({
                            place_projection::kind::index, 0,
                            canonical_pointer_type_id(
                                source_pointer_item->struct_type_name),
                            source_index_value});
                    source_place.projections.push_back({
                        place_projection::kind::dereference, 0, owner_type});
                    result->places.push_back(source_place);
                    source_place_id = source_place.id;
                    typed_value source;
                    source.id = next_value++;
                    source.type = owner_type;
                    source.category = value_category::borrow;
                    source.access = access_mode::read_only;
                    source.lifetime_token = source_pointer_value->second;
                    source.provenance.push_back(origin);
                    result->values.push_back(source);
                    source_value_id = source.id;
                    typed_operation load;
                    load.id = next_operation++;
                    load.operation = opcode::load;
                    load.places.push_back(source_place_id);
                    load.results.push_back(source_value_id);
                    load.effects.reads.push_back(source_place_id);
                    load.effects.exceptions = exception_effect::non_throwing;
                    if (source_index_producer != 0)
                        load.must_follow.push_back(source_index_producer);
                    operation_outcome loaded;
                    loaded.initialized.push_back(source_value_id);
                    load.outcomes.push_back(loaded);
                    load.provenance.push_back(origin);
                    source_preparation_id = load.id;
                    if (!append_typed_operation(result, current_region,
                            std::move(load), diagnostic))
                        return false;
                }

                const bool may_throw = typed_constructor_may_throw(
                    selected_constructor);
                const callable_kind dispatch_kind = use_move
                    ? callable_kind::move_constructor
                    : callable_kind::copy_constructor;
                if (!append_constructor_callable(result, unit, owner,
                        selected_constructor, owner_type, dispatch_kind,
                        may_throw))
                    return fail(diagnostic,
                        verification_error::incomplete_call_target,
                        static_cast<operation_id>(index + 1),
                        "typed move/copy construction has no complete callable descriptor");
                typed_operation construction;
                construction.id = next_operation++;
                construction.operation = use_move
                    ? opcode::move_construct : opcode::copy_construct;
                construction.callee = canonical_constructor_symbol_id(owner,
                    selected_constructor);
                construction.operands.push_back(source_value_id);
                construction.places.push_back(destination_place.id);
                construction.results.push_back(destination.id);
                construction.effects.reads.push_back(source_place_id);
                construction.effects.writes.push_back(destination_place.id);
                construction.effects.constructs = true;
                construction.effects.may_fail = may_throw;
                construction.effects.exceptions = may_throw
                    ? exception_effect::unknown
                    : exception_effect::non_throwing;
                if (source_preparation_id != 0)
                    construction.must_follow.push_back(source_preparation_id);
                if (destination_index_producer != 0)
                    construction.must_follow.push_back(
                        destination_index_producer);
                if (source_index_producer != 0
                    && source_preparation_id == 0)
                    construction.must_follow.push_back(source_index_producer);
                operation_outcome constructed;
                constructed.live.push_back(destination.id);
                if (use_move)
                    constructed.moved_from.push_back(source_value_id);
                construction.outcomes.push_back(constructed);
                if (may_throw)
                {
                    operation_outcome failed_construction;
                    failed_construction.kind = outcome_kind::thrown_exception;
                    failed_construction.uninitialized.push_back(destination.id);
                    construction.outcomes.push_back(failed_construction);
                }
                construction.provenance.push_back(origin);
                if (!append_typed_operation(result, current_region,
                        std::move(construction), diagnostic))
                    return false;
                explicitly_constructed_indirect_values[statement.target]
                    .push_back(destination.id);
                continue;
            }
            const t_cblc_data_item *item = find_scalar_item(unit, function,
                statement.target);
            if (!item)
                item = find_class_item(unit, function, statement.target);
            if (!item)
                return fail(diagnostic, verification_error::unsupported_legacy_type,
                    static_cast<operation_id>(index + 1),
                    "lifecycle adapter cannot resolve the constructed value to local typed storage");
            const auto item_place = place_by_item.find(item);
            const auto item_value = value_by_item.find(item);
            const auto lifecycle_places = lifecycle_places_by_item.find(item);
            const auto lifecycle_values = lifecycle_values_by_item.find(item);
            if (item_place == place_by_item.end() || item_value == value_by_item.end())
                return fail(diagnostic, verification_error::unsupported_legacy_type,
                    static_cast<operation_id>(index + 1),
                    "constructed scalar is outside the finalized function storage map");
            if (lifecycle_places == lifecycle_places_by_item.end()
                || lifecycle_values == lifecycle_values_by_item.end()
                || lifecycle_places->second.size() != lifecycle_values->second.size())
                return fail(diagnostic, verification_error::unsupported_legacy_type,
                    static_cast<operation_id>(index + 1),
                    "constructed class has no complete element lifecycle map");
            for (std::size_t element = 0; element < lifecycle_values->second.size(); ++element)
                if (!append_construct_tree(lifecycle_values->second[element],
                        item, &statement, origin))
                    return false;
            continue;
        }

        if (statement.type == CBLC_STATEMENT_DESTRUCT)
        {
            const auto indirect_values =
                explicitly_constructed_indirect_values.find(statement.target);
            if (indirect_values
                != explicitly_constructed_indirect_values.end())
            {
                std::size_t element = indirect_values->second.size();
                while (element > 0)
                {
                    --element;
                    source_origin destruction_origin = origin;
                    destruction_origin.role =
                        "explicit-indirect-object-destruction";
                    if (!append_destroy_tree(indirect_values->second[element],
                            destruction_origin, false))
                        return false;
                }
                explicitly_constructed_indirect_values.erase(indirect_values);
                continue;
            }
            const t_cblc_data_item *item = find_class_item(unit, function,
                statement.target);
            if (!item)
                return fail(diagnostic, verification_error::unsupported_legacy_type,
                    static_cast<operation_id>(index + 1),
                    "destroyed object cannot be resolved to class storage");
            const auto lifecycle_places = lifecycle_places_by_item.find(item);
            const auto lifecycle_values = lifecycle_values_by_item.find(item);
            if (lifecycle_places == lifecycle_places_by_item.end()
                || lifecycle_values == lifecycle_values_by_item.end()
                || lifecycle_places->second.size() != lifecycle_values->second.size())
                return fail(diagnostic, verification_error::unsupported_legacy_type,
                    static_cast<operation_id>(index + 1),
                    "destroyed object is outside finalized typed storage");
            const bool already_destroyed_for_throw = !lifecycle_values->second.empty()
                && std::all_of(lifecycle_values->second.begin(),
                    lifecycle_values->second.end(), [&](value_id value)
                    {
                        return exception_cleaned_values.find(value)
                            != exception_cleaned_values.end();
                    });
            if (already_destroyed_for_throw)
                continue;
            std::size_t element = lifecycle_values->second.size();
            while (element > 0)
            {
                --element;
                /* Parser-generated RAII cleanup is guarded by object-completion state. */
                if (!append_destroy_tree(lifecycle_values->second[element], origin,
                        true))
                    return false;
            }
            continue;
        }

        if (statement.type == CBLC_STATEMENT_THROW)
        {
            std::uint64_t source_exception_region_id = 0;
            if (statement.exception_is_rethrow
                || statement.exception_payload_is_struct
                || statement.exception_payload_is_message
                || std::strcmp(statement.exception_type_name, "int") != 0)
                return fail(diagnostic, verification_error::unsupported_legacy_statement,
                    static_cast<operation_id>(index + 1),
                    "typed adapter currently lowers integer throws; rethrow and managed payloads require explicit typed payload operations");
            lowered_scalar_operand payload;
            const char *payload_begin = statement.source;
            const char *payload_end = statement.source + std::strlen(statement.source);
            if (!lower_scalar_operand(unit, function, payload_begin, payload_end,
                    result, current_region, value_by_item, place_by_item,
                    &next_place, &next_value,
                    &next_operation, origin, false, &payload, diagnostic))
                return false;
            if (payload.type != scalar_type_id(CBLC_DATA_KIND_INT))
                return fail(diagnostic, verification_error::unsupported_legacy_type,
                    static_cast<operation_id>(index + 1),
                    "integer exception payload must resolve to the canonical int type");

            for (std::size_t cleanup_index = 0;
                cleanup_index < statement.exception_cleanup_count; ++cleanup_index)
            {
                const t_cblc_data_item *cleanup_item = find_class_item(unit, function,
                    statement.exception_cleanup_targets[cleanup_index]);
                const auto cleanup_values = cleanup_item
                    ? lifecycle_values_by_item.find(cleanup_item)
                    : lifecycle_values_by_item.end();
                if (!cleanup_item || cleanup_values == lifecycle_values_by_item.end())
                    return fail(diagnostic, verification_error::unsupported_legacy_type,
                        static_cast<operation_id>(index + 1),
                        "throw cleanup target has no finalized class lifecycle mapping");
                std::size_t element = cleanup_values->second.size();
                while (element > 0)
                {
                    --element;
                    source_origin cleanup_origin = origin;
                    cleanup_origin.role = "exception-scope-exit-cleanup";
                    cleanup_origin.compiler_generated = true;
                    if (!append_destroy_tree(cleanup_values->second[element],
                            cleanup_origin, true))
                        return false;
                    exception_cleaned_values.insert(cleanup_values->second[element]);
                }
            }

            typed_operation thrown;
            thrown.id = next_operation++;
            thrown.operation = opcode::throw_value;
            thrown.exception_type_id = statement.exception_type_id;
            if (statement.exception_has_region)
            {
                const std::uint64_t parser_region_id =
                    static_cast<std::uint64_t>(statement.exception_region_id) + 1U;
                bool inside_matching_handler = false;
                region_id ancestor = current_region;
                while (ancestor != 0)
                {
                    const auto found_region = std::find_if(result->regions.begin(),
                        result->regions.end(), [&](const typed_region &candidate)
                        {
                            return candidate.id == ancestor;
                        });
                    if (found_region == result->regions.end())
                        return fail(diagnostic,
                            verification_error::invalid_region_tree,
                            static_cast<operation_id>(index + 1),
                            "throw source region has a broken typed-region ancestry");
                    if (found_region->kind == region_kind::exception_handler
                        && found_region->source_exception_region_id == parser_region_id)
                    {
                        inside_matching_handler = true;
                        break;
                    }
                    ancestor = found_region->parent;
                }
                if (!inside_matching_handler)
                    source_exception_region_id = parser_region_id;
                else if (statement.exception_to_outer_region)
                {
                    std::size_t frame_index = region_stack.size();
                    while (frame_index > 0)
                    {
                        const region_frame &frame = region_stack[--frame_index];
                        if (frame.statement_kind != CBLC_STATEMENT_TRY_BEGIN)
                            continue;
                        const auto outer_try = std::find_if(result->regions.begin(),
                            result->regions.end(), [&](const typed_region &candidate)
                            {
                                return candidate.id == frame.control
                                    && candidate.kind == region_kind::exception_try;
                            });
                        if (outer_try != result->regions.end()
                            && outer_try->source_exception_region_id
                                != parser_region_id)
                        {
                            source_exception_region_id =
                                outer_try->source_exception_region_id;
                            break;
                        }
                    }
                }
            }
            thrown.source_exception_region_id = source_exception_region_id;
            thrown.operands.push_back(payload.value);
            thrown.must_follow.push_back(payload.producer);
            thrown.effects.exceptions = exception_effect::may_throw_known;
            thrown.effects.known_exception_type_ids.push_back(
                statement.exception_type_id);
            operation_outcome thrown_outcome;
            thrown_outcome.kind = outcome_kind::thrown_exception;
            thrown.outcomes.push_back(thrown_outcome);
            thrown.provenance.push_back(origin);
            if (!append_typed_operation(result, current_region,
                    std::move(thrown), diagnostic))
                return false;
            continue;
        }

        typed_operation return_operation;
        return_operation.id = next_operation++;
        return_operation.operation = opcode::return_value;
        return_operation.effects.exceptions = exception_effect::non_throwing;
        return_operation.outcomes.push_back({});
        return_operation.provenance.push_back(origin);
        if (*expression != '\0')
        {
            region_id return_region = current_region;

            while (return_region != 0)
            {
                const auto region = std::find_if(result->regions.begin(),
                    result->regions.end(), [&](const typed_region &candidate)
                    {
                        return candidate.id == return_region;
                    });
                if (region == result->regions.end())
                    return fail(diagnostic,
                        verification_error::invalid_region_tree,
                        return_operation.id,
                        "typed scalar return has a broken enclosing region chain");
                if (region->kind == region_kind::exception_handler)
                    return fail(diagnostic,
                        verification_error::unsupported_legacy_statement,
                        return_operation.id,
                        "scalar returns from exception handlers remain on the legacy lowering path until handler CFG exits are modeled");
                return_region = region->parent;
            }
            lowered_scalar_operand returned;
            const char *return_begin = expression;
            const char *return_end = expression + std::strlen(expression);
            type_id expected_return_type = 0;

            if (!lower_scalar_operand(unit, function, return_begin, return_end,
                    result, current_region, value_by_item, place_by_item,
                    &next_place, &next_value, &next_operation, origin, false,
                    &returned, diagnostic))
                return false;
            if (function->return_kind == CBLC_FUNCTION_RETURN_INT)
                expected_return_type = scalar_type_id(CBLC_DATA_KIND_INT);
            else if (function->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
                expected_return_type = scalar_type_id(CBLC_DATA_KIND_DOUBLE);
            else if (function->return_kind == CBLC_FUNCTION_RETURN_CHAR)
                expected_return_type = scalar_type_id(CBLC_DATA_KIND_CHAR);
            if (expected_return_type == 0 || returned.type != expected_return_type)
                return fail(diagnostic, verification_error::invalid_type_or_projection,
                    return_operation.id,
                    "typed scalar return expression does not match the declared return type");
            return_operation.operands.push_back(returned.value);
            if (returned.producer != 0)
                return_operation.must_follow.push_back(returned.producer);
        }
        /* Evaluate/materialize the return expression before ending by-value
         * parameter lifetimes.  This matters when the returned scalar is a
         * member of an owned class parameter. */
        for (auto parameter = result->entry_live_values.rbegin();
            parameter != result->entry_live_values.rend(); ++parameter)
        {
            bool already_destroyed = false;
            std::size_t prior_statement = index;
            while (prior_statement > 0)
            {
                const t_cblc_statement &prior_source =
                    function->statements[--prior_statement];
                if (prior_source.type != CBLC_STATEMENT_DESTRUCT)
                    break;
                const t_cblc_data_item *destroyed_item = find_class_item(unit,
                    function, prior_source.target);
                const auto destroyed_values = destroyed_item
                    ? lifecycle_values_by_item.find(destroyed_item)
                    : lifecycle_values_by_item.end();
                if (destroyed_values != lifecycle_values_by_item.end()
                    && std::find(destroyed_values->second.begin(),
                        destroyed_values->second.end(), *parameter)
                        != destroyed_values->second.end())
                {
                    already_destroyed = true;
                    break;
                }
            }
            if (already_destroyed)
                continue;
            source_origin cleanup_origin = origin;
            cleanup_origin.role = "implicit-by-value-parameter-destruction";
            cleanup_origin.compiler_generated = true;
            if (!append_destroy_tree(*parameter, cleanup_origin, true))
                return false;
        }
        if (!append_typed_operation(result, current_region,
                std::move(return_operation), diagnostic))
            return false;
    }
    if (!region_stack.empty())
        return fail(diagnostic, verification_error::invalid_region_tree, 0,
            "typed lowering ended with an unclosed structured region");
    if (!defer_verification && !verify_typed_function(*result, diagnostic))
        return false;
    return true;
}

bool lower_scalar_function(const t_cblc_translation_unit *unit,
    std::size_t function_index, typed_function *result,
    verification_diagnostic *diagnostic)
{
    typed_function candidate;

    if (!result)
        return lower_scalar_function_impl(unit, function_index, result, diagnostic);
    if (!lower_scalar_function_impl(unit, function_index, &candidate, diagnostic))
    {
        *result = typed_function{};
        return false;
    }
    *result = std::move(candidate);
    return true;
}

static bool global_lifecycle_destructor_is_nothrow(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    std::size_t depth)
{
    if (!unit || !type || depth >= 64)
        return false;
    if (type->has_destructor_definition)
    {
        for (std::size_t statement_index = 0;
            statement_index < type->destructor_statement_count; ++statement_index)
        {
            const t_cblc_statement &statement =
                type->destructor_statements[statement_index];
            if (statement.type == CBLC_STATEMENT_THROW
                || statement.exception_may_throw)
                return false;
            if ((statement.type == CBLC_STATEMENT_CALL
                    || statement.type == CBLC_STATEMENT_CALL_ASSIGN)
                && std::strncmp(statement.call_identifier, "std::free",
                    sizeof(statement.call_identifier)) != 0
                && std::strncmp(statement.call_identifier, "CBLC-DELETE",
                    std::strlen("CBLC-DELETE")) != 0)
                return false;
            if (statement.type == CBLC_STATEMENT_METHOD_CALL
                || statement.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
                return false;
        }
    }
    for (std::size_t field_index = 0; field_index < type->field_count;
        ++field_index)
    {
        const t_cblc_struct_field &field = type->fields[field_index];
        if (!field.is_borrowed && field.kind == CBLC_DATA_KIND_STRUCT
            && class_requires_lifecycle(unit, field.struct_type_name)
            && !global_lifecycle_destructor_is_nothrow(unit,
                find_class_type(unit, field.struct_type_name), depth + 1))
            return false;
    }
    return true;
}

static bool entry_has_lifecycle_operation(const t_cblc_function *entry,
    t_cblc_statement_type operation, const char *target)
{
    if (!entry || !target)
        return false;
    for (std::size_t index = 0; index < entry->statement_count; ++index)
    {
        const t_cblc_statement &statement = entry->statements[index];
        if (statement.type == operation
            && std::strcmp(statement.target, target) == 0)
            return true;
    }
    return false;
}

bool verify_global_object_lifetimes(const t_cblc_translation_unit *unit,
    verification_diagnostic *diagnostic)
{
    if (diagnostic)
        *diagnostic = verification_diagnostic{};
    if (!unit)
        return fail(diagnostic, verification_error::unsupported_legacy_type,
            0, "global object lifetime checking requires a translation unit");
    const t_cblc_function *entry = nullptr;
    if (unit->entry_function_index < unit->function_count)
        entry = &unit->functions[unit->entry_function_index];
    for (std::size_t item_index = 0; item_index < unit->data_count;
        ++item_index)
    {
        const t_cblc_data_item &item = unit->data_items[item_index];
        if (item.kind != CBLC_DATA_KIND_STRUCT || item.is_function_local
            || item.is_borrowed || item.is_alias || item.is_template_item
            || item.owner_function_name[0] != '\0'
            || std::strchr(item.source_name, '.')
            || std::strstr(item.source_name, "->"))
            continue;
        /* Exception payloads are compiler-owned static slots, not ordinary
         * program-scope objects. Their lifetime starts only when throw
         * lowering constructs the typed payload and ends in the generated
         * CBLC-EX-CLEAR-PAYLOAD cleanup paragraph. Requiring a program-entry
         * initializer here would reject that explicit runtime lifecycle. */
        if (std::strncmp(item.source_name, "__cblc_exception_payload_",
                std::strlen("__cblc_exception_payload_")) == 0)
            continue;
        if (!class_requires_lifecycle(unit, item.struct_type_name))
            continue;
        /* The current root unwinder is emitted in a single-module `main`
         * program. External/library entry points need the linker's full init
         * graph and shutdown ABI before they may own globals. */
        if (!entry || std::strcmp(entry->source_name, "main") != 0)
        {
            const std::string message = "global object '"
                + std::string(item.source_name)
                + "' requires the executable root startup/shutdown entry";
            return fail(diagnostic,
                verification_error::unsupported_dynamic_initialization, 0,
                message.c_str());
        }
        const t_cblc_struct_type *type =
            find_class_type(unit, item.struct_type_name);
        const t_cblc_constructor *constructor = type
            ? cblc_find_constructor_for_arguments(unit, type,
                item.constructor_arguments, item.constructor_argument_count)
            : nullptr;
        if (!entry || !type
            || (type->constructor_count > 0
                && (!constructor || !constructor->has_definition))
            || !global_lifecycle_destructor_is_nothrow(unit, type, 0)
            || (constructor
                && !entry_has_lifecycle_operation(entry,
                    CBLC_STATEMENT_DEFAULT_CONSTRUCT, item.cobol_name))
            || ((type->has_destructor || type->has_destructor_definition)
                && !entry_has_lifecycle_operation(entry, CBLC_STATEMENT_DESTRUCT,
                    item.cobol_name)))
        {
            const std::string message = "global object '"
                + std::string(item.source_name)
                + "' has no verified construction/destruction lifecycle or its destructor may throw";
            return fail(diagnostic,
                verification_error::unsupported_dynamic_initialization,
                0, message.c_str());
        }
    }
    return true;
}

bool lower_scalar_method(const t_cblc_translation_unit *unit,
    std::size_t type_index, std::size_t method_index, typed_function *result,
    verification_diagnostic *diagnostic)
{
    if (diagnostic)
        *diagnostic = verification_diagnostic{};
    if (!unit || !result || type_index >= unit->struct_type_count
        || method_index >= unit->struct_types[type_index].method_count)
    {
        if (result)
            *result = typed_function{};
        return fail(diagnostic, verification_error::missing_function_identity, 0,
            "method lowering requires a valid translation unit, class method, and output");
    }

    const t_cblc_struct_type &owner_type = unit->struct_types[type_index];
    const t_cblc_method &method = owner_type.methods[method_index];
    if (!method.has_definition)
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::unsupported_legacy_statement, 0,
            "method lowering requires a method definition");
    }
    if (owner_type.is_template)
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::unsupported_legacy_type, 0,
            "unspecialized generic method bodies cannot be lowered to typed IR");
    }
    if (owner_type.is_template_instantiation)
        for (std::size_t statement_index = 0;
            statement_index < method.statement_count; ++statement_index)
        {
            const t_cblc_statement &statement = method.statements[statement_index];
            if (statement.type != CBLC_STATEMENT_CALL
                && statement.type != CBLC_STATEMENT_CALL_ASSIGN)
                continue;
            bool has_local_target = false;
            for (std::size_t function_index = 0;
                function_index < unit->function_count; ++function_index)
                if (std::strcmp(unit->functions[function_index].source_name,
                        statement.call_identifier) == 0
                    && !unit->functions[function_index].is_template
                    && unit->functions[function_index].statements
                    && unit->functions[function_index].statement_count > 0)
                {
                    has_local_target = true;
                    break;
                }
            if (!has_local_target)
            {
                *result = typed_function{};
                return fail(diagnostic,
                    verification_error::unsupported_legacy_statement,
                    static_cast<operation_id>(statement_index + 1),
                    "concrete template method contains a direct call whose canonical local callable is not available to typed lowering");
            }
        }

    /*
     * Methods share the typed-operation lowering rules with free functions.
     * Adapt only the declaration envelope; source statements, storage,
     * parameters, and canonical class/type metadata remain owned by the
     * original translation unit.
     */
    t_cblc_function adapted_function{};
    t_cblc_translation_unit adapted_unit = *unit;
    std::vector<t_cblc_data_item> adapted_data_items;
    std::vector<t_cblc_statement> adapted_statements;
    std::vector<typed_receiver_field_binding> receiver_fields;
    std::unordered_set<std::string> method_scoped_storage_names;
    t_cblc_data_item receiver_item{};
    char receiver_source_name[TRANSPILE_IDENTIFIER_MAX];
    if (ft_strlcpy(adapted_function.source_name, method.scope_source_name,
            sizeof(adapted_function.source_name))
            >= sizeof(adapted_function.source_name)
        || ft_strlcpy(adapted_function.cobol_name, method.cobol_name,
            sizeof(adapted_function.cobol_name))
            >= sizeof(adapted_function.cobol_name)
        || method.parameter_count > TRANSPILE_FUNCTION_PARAMETER_MAX
        || method.exception_type_count > CBLC_EXCEPTION_THROW_SET_MAX)
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::missing_function_identity, 0,
            "method declaration exceeds the typed lowering identity limits");
    }
    const int receiver_name_length = std::snprintf(receiver_source_name,
        sizeof(receiver_source_name), "%s__this", method.scope_source_name);
    if (receiver_name_length < 0)
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::missing_function_identity, 0,
            "implicit method receiver identity could not be formatted");
    }
    if (static_cast<std::size_t>(receiver_name_length)
        >= sizeof(receiver_source_name))
    {
        const char *receiver_base_name = "__cblc_implicit_this";
        std::size_t receiver_suffix = 0;
        bool receiver_name_available = false;
        while (!receiver_name_available)
        {
            int fallback_length;
            fallback_length = receiver_suffix == 0
                ? std::snprintf(receiver_source_name,
                    sizeof(receiver_source_name), "%s", receiver_base_name)
                : std::snprintf(receiver_source_name,
                    sizeof(receiver_source_name), "%s_%zu",
                    receiver_base_name, receiver_suffix);
            if (fallback_length < 0
                || static_cast<std::size_t>(fallback_length)
                    >= sizeof(receiver_source_name))
            {
                *result = typed_function{};
                return fail(diagnostic,
                    verification_error::missing_function_identity, 0,
                    "compact implicit receiver identity exceeds the typed lowering limits");
            }
            receiver_name_available = std::none_of(adapted_data_items.begin(),
                adapted_data_items.end(), [&](const t_cblc_data_item &item)
                {
                    return std::strcmp(item.source_name,
                        receiver_source_name) == 0;
                });
            ++receiver_suffix;
        }
    }
    if (unit->data_count > 0)
        adapted_data_items.assign(unit->data_items,
            unit->data_items + unit->data_count);
    for (t_cblc_data_item &item : adapted_data_items)
        if (std::strcmp(item.owner_function_name, method.scope_source_name) == 0
            && !item.is_function_local)
        {
            item.is_active = 1;
        }
    for (const t_cblc_data_item &item : adapted_data_items)
        if (!item.is_alias
            && std::strcmp(item.owner_function_name,
                method.scope_source_name) == 0)
            method_scoped_storage_names.emplace(item.cobol_name);
    for (t_cblc_data_item &alias : adapted_data_items)
        if (alias.is_alias
            && method_scoped_storage_names.find(alias.cobol_name)
                != method_scoped_storage_names.end())
            alias.is_active = 1;
    std::function<bool(const t_cblc_struct_type &, const std::string &,
        const std::string &, std::vector<place_projection>,
        std::unordered_set<std::size_t>)> collect_receiver_fields;
    collect_receiver_fields = [&](const t_cblc_struct_type &declaring_type,
        const std::string &source_prefix, const std::string &cobol_prefix,
        std::vector<place_projection> path,
        std::unordered_set<std::size_t> ancestry) -> bool
    {
        const std::size_t declaring_index = static_cast<std::size_t>(
            &declaring_type - unit->struct_types);
        if (!ancestry.insert(declaring_index).second)
            return true;
        for (std::size_t field_index = 0;
            field_index < declaring_type.field_count; ++field_index)
        {
            const t_cblc_struct_field &field = declaring_type.fields[field_index];
            if (field.array_count > 0)
                continue;
            const std::string source_name = source_prefix.empty()
                ? std::string(field.source_name)
                : source_prefix + "." + field.source_name;
            const std::string cobol_name = cobol_prefix + "-" + field.cobol_name;
            const type_id field_scalar_type = scalar_type_id(field.kind);
            if (field_scalar_type != 0)
            {
                t_cblc_data_item field_item{};
                if (source_name.size() >= sizeof(field_item.source_name)
                    || cobol_name.size() >= sizeof(field_item.cobol_name))
                    return false;
                ft_strlcpy(field_item.source_name, source_name.c_str(),
                    sizeof(field_item.source_name));
                ft_strlcpy(field_item.cobol_name, cobol_name.c_str(),
                    sizeof(field_item.cobol_name));
                ft_strlcpy(field_item.local_source_name, field.source_name,
                    sizeof(field_item.local_source_name));
                field_item.kind = field.kind;
                field_item.length = field.length;
                ft_strlcpy(field_item.owner_function_name,
                    method.scope_source_name,
                    sizeof(field_item.owner_function_name));
                field_item.is_function_local = 1;
                field_item.is_active = 1;
                field_item.is_const = field.is_const;
                adapted_data_items.push_back(field_item);

                typed_receiver_field_binding binding;
                binding.cobol_name = cobol_name;
                binding.projections = path;
                binding.projections.push_back({place_projection::kind::field,
                    static_cast<std::uint32_t>(field_index + 1),
                    field_scalar_type});
                receiver_fields.push_back(std::move(binding));
                continue;
            }
            if (field.kind == CBLC_DATA_KIND_STRUCT_POINTER
                && field.struct_type_name[0])
            {
                const type_id pointer_type =
                    canonical_pointer_type_id(field.struct_type_name);
                t_cblc_data_item field_item{};
                if (source_name.size() >= sizeof(field_item.source_name)
                    || cobol_name.size() >= sizeof(field_item.cobol_name)
                    || pointer_type == 0)
                    return false;
                ft_strlcpy(field_item.source_name, source_name.c_str(),
                    sizeof(field_item.source_name));
                ft_strlcpy(field_item.cobol_name, cobol_name.c_str(),
                    sizeof(field_item.cobol_name));
                ft_strlcpy(field_item.local_source_name, field.source_name,
                    sizeof(field_item.local_source_name));
                ft_strlcpy(field_item.struct_type_name, field.struct_type_name,
                    sizeof(field_item.struct_type_name));
                field_item.kind = CBLC_DATA_KIND_STRUCT_POINTER;
                field_item.length = field.length;
                field_item.is_borrowed = 1;
                field_item.is_function_local = 1;
                field_item.is_active = 1;
                field_item.is_const = field.is_const;
                ft_strlcpy(field_item.owner_function_name,
                    method.scope_source_name,
                    sizeof(field_item.owner_function_name));
                adapted_data_items.push_back(field_item);

                typed_receiver_field_binding binding;
                binding.cobol_name = cobol_name;
                binding.projections = path;
                binding.projections.push_back({place_projection::kind::field,
                    static_cast<std::uint32_t>(field_index + 1), pointer_type});
                receiver_fields.push_back(std::move(binding));
                continue;
            }
            if (field.kind != CBLC_DATA_KIND_STRUCT || !field.struct_type_name[0])
                continue;
            const t_cblc_struct_type *nested_type = nullptr;
            std::size_t nested_index = 0;
            for (; nested_index < unit->struct_type_count; ++nested_index)
                if (std::strcmp(unit->struct_types[nested_index].source_name,
                        field.struct_type_name) == 0)
                {
                    nested_type = &unit->struct_types[nested_index];
                    break;
                }
            if (!nested_type || ancestry.find(nested_index) != ancestry.end())
                continue;
            std::vector<place_projection> nested_path = path;
            const type_id nested_type_id = canonical_class_type_id(
                nested_type->source_name);
            nested_path.push_back({place_projection::kind::field,
                static_cast<std::uint32_t>(field_index + 1),
                nested_type_id});
            if (!field.is_borrowed)
            {
                t_cblc_data_item field_item{};
                if (source_name.size() >= sizeof(field_item.source_name)
                    || cobol_name.size() >= sizeof(field_item.cobol_name)
                    || nested_type_id == 0)
                    return false;
                ft_strlcpy(field_item.source_name, source_name.c_str(),
                    sizeof(field_item.source_name));
                ft_strlcpy(field_item.cobol_name, cobol_name.c_str(),
                    sizeof(field_item.cobol_name));
                ft_strlcpy(field_item.local_source_name, field.source_name,
                    sizeof(field_item.local_source_name));
                ft_strlcpy(field_item.struct_type_name,
                    nested_type->source_name,
                    sizeof(field_item.struct_type_name));
                field_item.kind = CBLC_DATA_KIND_STRUCT;
                field_item.length = field.length;
                field_item.is_const = field.is_const;
                field_item.is_borrowed = 1;
                field_item.is_function_local = 1;
                field_item.is_active = 1;
                ft_strlcpy(field_item.owner_function_name,
                    method.scope_source_name,
                    sizeof(field_item.owner_function_name));
                adapted_data_items.push_back(field_item);

                typed_receiver_field_binding binding;
                binding.cobol_name = cobol_name;
                binding.projections = path;
                binding.projections.push_back({place_projection::kind::field,
                    static_cast<std::uint32_t>(field_index + 1), nested_type_id});
                receiver_fields.push_back(std::move(binding));
            }
            if (!collect_receiver_fields(*nested_type, source_name, cobol_name,
                    std::move(nested_path), ancestry))
                return false;
        }
        return true;
    };
    if (!collect_receiver_fields(owner_type, "", "CBLC-THIS", {}, {}))
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::missing_function_identity,
            0, "implicit receiver field path exceeds typed lowering limits");
    }
    receiver_item.kind = CBLC_DATA_KIND_STRUCT_POINTER;
    ft_strlcpy(receiver_item.source_name, receiver_source_name,
        sizeof(receiver_item.source_name));
    ft_strlcpy(receiver_item.local_source_name, "this",
        sizeof(receiver_item.local_source_name));
    ft_strlcpy(receiver_item.cobol_name, "CBLC-THIS",
        sizeof(receiver_item.cobol_name));
    ft_strlcpy(receiver_item.struct_type_name, owner_type.source_name,
        sizeof(receiver_item.struct_type_name));
    ft_strlcpy(receiver_item.owner_function_name, method.scope_source_name,
        sizeof(receiver_item.owner_function_name));
    receiver_item.is_function_local = 1;
    receiver_item.is_active = 1;
    adapted_data_items.push_back(receiver_item);

    adapted_function.parameter_count = method.parameter_count;
    if (method.parameter_count > 0)
        std::memcpy(adapted_function.parameters, method.parameters,
            method.parameter_count * sizeof(adapted_function.parameters[0]));
    adapted_statements.reserve(method.statement_count);
    for (std::size_t statement_index = 0;
        statement_index < method.statement_count; ++statement_index)
    {
        const t_cblc_statement &statement = method.statements[statement_index];
        bool materialized_parameter_cleanup = false;

        if (statement.type == CBLC_STATEMENT_DESTRUCT)
            for (std::size_t parameter_index = 0;
                parameter_index < method.parameter_count; ++parameter_index)
                if (parameter_index < std::numeric_limits<unsigned int>::digits
                    && (method.template_value_parameter_cleanup_mask
                            & (1U << parameter_index)) != 0
                    && std::strcmp(statement.target,
                        method.parameters[parameter_index].cobol_name) == 0)
                {
                    materialized_parameter_cleanup = true;
                    break;
                }
        if (!materialized_parameter_cleanup)
            adapted_statements.push_back(statement);
    }
    adapted_function.statements = adapted_statements.data();
    adapted_function.statement_count = adapted_statements.size();
    adapted_function.return_kind = method.return_kind;
    adapted_function.template_return_pointer_depth =
        method.template_return_pointer_depth;
    adapted_function.may_throw = method.exception_type_count > 0
        || method.exception_types_unknown;
    adapted_function.exception_type_count = method.exception_type_count;
    std::copy_n(method.exception_type_ids, method.exception_type_count,
        adapted_function.exception_type_ids);
    adapted_function.exception_types_unknown = method.exception_types_unknown;
    if (method.return_type_name[0] != '\0'
        && ft_strlcpy(adapted_function.return_type_name, method.return_type_name,
            sizeof(adapted_function.return_type_name))
            >= sizeof(adapted_function.return_type_name))
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::missing_function_identity, 0,
            "method return type exceeds the typed lowering identity limits");
    }
    if (method.return_kind != CBLC_FUNCTION_RETURN_VOID
        && ft_strlcpy(adapted_function.return_cobol_name, "CBLC-THIS-RET",
            sizeof(adapted_function.return_cobol_name))
            >= sizeof(adapted_function.return_cobol_name))
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::missing_function_identity, 0,
            "method result storage identity could not be represented");
    }
    adapted_unit.functions = &adapted_function;
    adapted_unit.function_count = 1;
    adapted_unit.entry_function_index = 0;
    adapted_unit.data_items = adapted_data_items.data();
    adapted_unit.data_count = adapted_data_items.size();
    adapted_unit.data_capacity = adapted_data_items.size();
    typed_function candidate;
    std::vector<typed_storage_binding> storage_bindings;
    if (!lower_scalar_function_impl(&adapted_unit, 0, &candidate, diagnostic,
            &storage_bindings, true))
    {
        *result = typed_function{};
        return false;
    }
    const auto receiver_binding = std::find_if(storage_bindings.begin(),
        storage_bindings.end(), [](const typed_storage_binding &binding)
        {
            return binding.cobol_name == "CBLC-THIS";
        });
    const auto receiver_type = std::find_if(candidate.types.begin(),
        candidate.types.end(), [&](const typed_type &type)
        {
            return type.kind == typed_type_kind::class_type
                && type.canonical_name == owner_type.source_name;
        });
    if (receiver_binding == storage_bindings.end()
        || receiver_type == candidate.types.end())
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
            "typed method receiver has no canonical borrow or owner type");
    }
    const auto receiver_place = std::find_if(candidate.places.begin(),
        candidate.places.end(), [&](const typed_place &place)
        {
            return place.id == receiver_binding->place;
        });
    if (receiver_place == candidate.places.end() || receiver_place->symbol == 0)
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
            "typed method receiver has no source-symbol binding");
    }
    const symbol_id receiver_symbol = receiver_place->symbol;
    auto receiver_value = std::find_if(candidate.values.begin(),
        candidate.values.end(), [&](const typed_value &value)
        {
            return value.id == receiver_binding->value
                && value.category == value_category::pointer_or_handle;
        });
    if (receiver_value == candidate.values.end())
    {
        *result = typed_function{};
        return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
            "typed method receiver is not represented as a pointer borrow");
    }
    receiver_value->lifetime_token = receiver_value->id;
    for (const typed_receiver_field_binding &receiver_field : receiver_fields)
    {
        const auto field_binding = std::find_if(storage_bindings.begin(),
            storage_bindings.end(), [&](const typed_storage_binding &binding)
            {
                return binding.cobol_name == receiver_field.cobol_name;
            });
        if (field_binding == storage_bindings.end())
            continue;
        auto field_place = std::find_if(candidate.places.begin(),
            candidate.places.end(), [&](const typed_place &place)
            {
                return place.id == field_binding->place;
            });
        if (field_place == candidate.places.end())
        {
            *result = typed_function{};
            return fail(diagnostic, verification_error::invalid_type_or_projection,
                0, "typed receiver field has no canonical place");
        }
        const auto field_value = std::find_if(candidate.values.begin(),
            candidate.values.end(), [&](const typed_value &value)
            {
                return value.id == field_place->base_value;
            });
        if (field_value != candidate.values.end()
            && (field_value->category == value_category::owned_object
                || field_value->category == value_category::borrow))
        {
            field_place->storage_base_value = receiver_binding->value;
            if (field_value->category == value_category::borrow)
                field_value->lifetime_token = receiver_value->lifetime_token;
        }
        else
            field_place->base_value = receiver_binding->value;
        field_place->symbol = receiver_symbol;
        field_place->owner_lifetime_token = receiver_value->lifetime_token;
        field_place->projections.push_back({place_projection::kind::dereference,
            0, receiver_type->id});
        field_place->projections.insert(field_place->projections.end(),
            receiver_field.projections.begin(), receiver_field.projections.end());
    }
    if (!verify_typed_function(candidate, diagnostic))
    {
        *result = typed_function{};
        return false;
    }
    *result = std::move(candidate);
    return true;
}

bool verify_typed_function(const typed_function &function,
    verification_diagnostic *diagnostic)
{
    std::unordered_map<type_id, const typed_type *> type_table;
    std::unordered_map<symbol_id, const typed_symbol *> symbols;
    std::unordered_map<symbol_id, const typed_callable *> callables;
    std::unordered_map<value_id, const typed_value *> values;
    std::unordered_map<place_id, const typed_place *> places;
    std::unordered_map<operation_id, const typed_operation *> operations;
    std::unordered_map<region_id, const typed_region *> regions;
    std::unordered_map<operation_id, region_id> operation_regions;
    std::unordered_map<value_id, operation_id> producers;
    std::unordered_map<operation_id, std::size_t> indegree;
    std::unordered_map<operation_id, std::vector<operation_id>> successors;
    std::size_t visited;

    if (diagnostic)
        *diagnostic = verification_diagnostic{};
    if (function.schema_version != typed_ir_schema_version)
        return fail(diagnostic, verification_error::unsupported_schema, 0,
            "typed IR schema version is unsupported");
    if (function.source_unit_id == 0 || function.function_id == 0)
        return fail(diagnostic, verification_error::missing_function_identity, 0,
            "typed function requires stable source-unit and function IDs");

    for (const typed_type &type : function.types)
    {
        if (type.id == 0 || type.canonical_name.empty()
            || !type_table.emplace(type.id, &type).second)
            return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                "typed type IDs must be unique/nonzero and have canonical names");
    }
    for (const typed_type &type : function.types)
    {
        std::unordered_set<std::uint32_t> member_ids;
        if ((type.kind == typed_type_kind::primitive
                && (!type.fields.empty() || type.pointee_type != 0))
            || (type.kind == typed_type_kind::class_type
                && type.pointee_type != 0))
            return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                "primitive/class typed types cannot carry pointer pointee metadata");
        if (type.kind == typed_type_kind::pointer)
        {
            const auto pointee = type_table.find(type.pointee_type);
            if (!type.fields.empty() || type.pointee_type == 0
                || pointee == type_table.end()
                || pointee->second->kind != typed_type_kind::class_type
                || type.canonical_name != pointee->second->canonical_name + "*")
                return fail(diagnostic,
                    verification_error::invalid_type_or_projection, 0,
                    "typed pointer must identify one canonical class pointee and no fields");
        }
        for (const typed_field &field : type.fields)
        {
            if (field.member_id == 0 || field.name.empty()
                || !member_ids.insert(field.member_id).second
                || (!field.is_dependent &&
                    (field.type == 0 || type_table.find(field.type) == type_table.end())))
                return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                    "typed class fields require unique resolved member IDs and concrete field types");
        }
    }

    for (const typed_callable &callable : function.callables)
    {
        const auto owner = type_table.find(callable.owner_type);
        const bool valid_kind = callable.kind == callable_kind::method
            || callable.kind == callable_kind::copy_assignment
            || callable.kind == callable_kind::move_assignment
            || callable.kind == callable_kind::constructor
            || callable.kind == callable_kind::copy_constructor
            || callable.kind == callable_kind::move_constructor
            || callable.kind == callable_kind::destructor
            || callable.kind == callable_kind::function;
        const bool valid_owner = callable.kind == callable_kind::function
            ? callable.owner_type == 0
            : (owner != type_table.end()
                && owner->second->kind == typed_type_kind::class_type
                && callable.canonical_name.compare(0,
                    owner->second->canonical_name.size(),
                    owner->second->canonical_name) == 0
                && callable.canonical_name.size()
                    > owner->second->canonical_name.size() + 2
                && callable.canonical_name.compare(
                    owner->second->canonical_name.size(), 2, "::") == 0);
        if (callable.id == 0 || callable.canonical_name.empty()
            || !valid_kind || !valid_owner
            || (callable.result_type != 0
                && type_table.find(callable.result_type) == type_table.end())
            || (callable.exceptions == exception_effect::non_throwing
                && !callable.known_exception_type_ids.empty())
            || (callable.exceptions == exception_effect::may_throw_known
                && callable.known_exception_type_ids.empty())
            || !callables.emplace(callable.id, &callable).second)
            return fail(diagnostic, verification_error::incomplete_call_target, 0,
                "typed callable requires a unique identity, canonical class owner, and resolved result type");
        {
            std::unordered_set<std::uint64_t> known_exceptions;
            for (std::uint64_t exception_type_id
                : callable.known_exception_type_ids)
                if (exception_type_id == 0
                    || !known_exceptions.insert(exception_type_id).second)
                    return fail(diagnostic,
                        verification_error::incomplete_call_target, 0,
                        "typed callable exception IDs must be unique and nonzero");
        }
        for (type_id parameter_type : callable.parameter_types)
            if (parameter_type == 0 || type_table.find(parameter_type) == type_table.end())
                return fail(diagnostic,
                    verification_error::incomplete_call_target, 0,
                    "typed callable parameter types must be concrete and declared");
    }

    for (const typed_symbol &symbol : function.symbols)
    {
        if (symbol.id == 0 || symbol.canonical_name.empty() || symbol.type == 0
            || type_table.find(symbol.type) == type_table.end()
            || !symbols.emplace(symbol.id, &symbol).second)
            return fail(diagnostic, verification_error::duplicate_or_zero_id, 0,
                "typed symbols require unique nonzero IDs, canonical names, and concrete types");
    }

    for (const typed_value &value : function.values)
    {
        if (value.id == 0 || value.type == 0 || !value.is_definition
            || type_table.find(value.type) == type_table.end()
            || !values.emplace(value.id, &value).second)
            return fail(diagnostic, verification_error::duplicate_or_zero_id, 0,
                "value IDs must be unique/nonzero and values must have a concrete type definition");
    }
    {
        std::unordered_set<value_id> unique_entry_values;
        for (value_id value_id_value : function.entry_live_values)
        {
            const auto value = values.find(value_id_value);
            if (value == values.end()
                || value->second->category != value_category::owned_object
                || !unique_entry_values.insert(value_id_value).second)
                return fail(diagnostic,
                    verification_error::invalid_outcome_state, 0,
                    "function entry may mark only unique defined owned objects as already live");
        }
    }
    for (const typed_place &place : function.places)
    {
        if (place.id == 0 || place.type == 0 || !places.emplace(place.id, &place).second)
            return fail(diagnostic, verification_error::duplicate_or_zero_id, 0,
                "place IDs must be unique and nonzero");
        const auto base = values.find(place.base_value);
        if (base == values.end())
            return fail(diagnostic, verification_error::dangling_reference, 0,
                "place base value does not exist");
        const auto storage_base = place.storage_base_value == 0
            ? base : values.find(place.storage_base_value);
        if (storage_base == values.end())
            return fail(diagnostic, verification_error::dangling_reference, 0,
                "place storage root does not exist");
        if (place.storage_base_value != 0
            && ((base->second->category != value_category::owned_object
                    && base->second->category != value_category::borrow)
                || (base->second->category == value_category::borrow
                    && (base->second->lifetime_token == 0
                        || place.owner_lifetime_token
                            != base->second->lifetime_token))
                || base->second->type != place.type
                || place.projections.empty()))
            return fail(diagnostic, verification_error::invalid_type_or_projection,
                0, "subobject place requires a same-typed owned value or lifetime-bound borrow and a projected storage root");
        if (place.symbol != 0)
        {
            const auto symbol = symbols.find(place.symbol);
            if (symbol == symbols.end()
                || symbol->second->type != storage_base->second->type)
                return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                    "source-bound place must reference a symbol matching its storage root type");
        }
        if (place.projections.empty() && storage_base->second->type != place.type)
            return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                "unprojected place type must match its storage root type");
        type_id projected_type = storage_base->second->type;
        std::size_t projected_extent = storage_base->second->array_extent;
        for (const place_projection &projection : place.projections)
        {
            const bool malformed_index =
                projection.projection_kind == place_projection::kind::index
                && ((projection.resolved_member == 0)
                    == (projection.index_value == 0));
            const bool malformed_non_index =
                projection.projection_kind != place_projection::kind::index
                && projection.index_value != 0;
            if (projection.resulting_type == 0 || malformed_index
                || malformed_non_index
                || (projection.projection_kind == place_projection::kind::field
                    && projection.resolved_member == 0))
                return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                    "field/index projections require a resolved member, and every projection requires a resulting type");
            if (projection.projection_kind == place_projection::kind::index)
            {
                const auto indexed_type = type_table.find(projected_type);
                const bool pointer_index = indexed_type != type_table.end()
                    && indexed_type->second->kind == typed_type_kind::pointer;
                if (projection.index_value != 0)
                {
                    const auto index_value = values.find(projection.index_value);
                    if (index_value == values.end()
                        || index_value->second->type
                            != scalar_type_id(CBLC_DATA_KIND_INT))
                        return fail(diagnostic,
                            verification_error::invalid_type_or_projection, 0,
                            "runtime index projection requires a defined canonical int value");
                }
                if (projection.resulting_type != projected_type
                    || (pointer_index && projected_extent != 0)
                    || (!pointer_index
                        && (projected_extent == 0
                            || (projection.index_value == 0
                                && projection.resolved_member > projected_extent))))
                    return fail(diagnostic,
                        verification_error::invalid_type_or_projection, 0,
                        "index projection must preserve a pointer type or resolve within the current aggregate extent");
            }
            if (projection.projection_kind == place_projection::kind::field)
            {
                const auto owner_type = type_table.find(projected_type);
                if (owner_type == type_table.end()
                    || owner_type->second->kind != typed_type_kind::class_type)
                    return fail(diagnostic,
                        verification_error::invalid_type_or_projection, 0,
                        "field projection requires a resolved class type");
                const auto field = std::find_if(owner_type->second->fields.begin(),
                    owner_type->second->fields.end(), [&](const typed_field &candidate)
                    {
                        return candidate.member_id == projection.resolved_member;
                    });
                if (field == owner_type->second->fields.end() || field->is_dependent
                    || field->type != projection.resulting_type)
                    return fail(diagnostic,
                        verification_error::invalid_type_or_projection, 0,
                        "field projection does not match the canonical member and type");
                projected_type = field->type;
                projected_extent = field->array_extent;
            }
            else if (projection.projection_kind == place_projection::kind::index)
                projected_extent = 0;
            else
            {
                const auto pointer_type = type_table.find(projected_type);
                if (pointer_type == type_table.end()
                    || pointer_type->second->kind != typed_type_kind::pointer
                    || pointer_type->second->pointee_type
                        != projection.resulting_type
                    || type_table.find(projection.resulting_type) == type_table.end())
                    return fail(diagnostic,
                        verification_error::invalid_type_or_projection, 0,
                        "dereference projection must match the pointer's canonical pointee type");
                projected_type = projection.resulting_type;
                projected_extent = 0;
            }
        }
        if (!place.projections.empty()
            && (place.projections.back().resulting_type != place.type
                || projected_type != place.type))
            return fail(diagnostic, verification_error::invalid_type_or_projection, 0,
                "projected place type must match its final projection type");
    }
    for (const typed_operation &operation : function.operations)
    {
        if (operation.id == 0 || !operations.emplace(operation.id, &operation).second)
            return fail(diagnostic, verification_error::duplicate_or_zero_id,
                operation.id, "operation IDs must be unique and nonzero");
        indegree.emplace(operation.id, 0);
    }
    {
        std::size_t root_count = 0;
        region_id root_id = 0;

        for (const typed_region &region : function.regions)
        {
            if (region.id == 0 || !regions.emplace(region.id, &region).second)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "region IDs must be unique and nonzero");
            if (region.parent == 0)
            {
                root_count += 1;
                root_id = region.id;
            }
        }
        if (root_count != 1)
            return fail(diagnostic, verification_error::invalid_region_tree, 0,
                "typed function must have exactly one root region");
        for (const typed_region &region : function.regions)
        {
            std::unordered_set<region_id> unique_children;
            std::unordered_set<operation_id> sequenced_operations;
            std::unordered_set<operation_id> condition_operations;
            std::unordered_set<region_id> sequenced_children;

            if (region.parent != 0 && regions.find(region.parent) == regions.end())
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "region parent does not exist");
            for (region_id child_id : region.children)
            {
                const auto child = regions.find(child_id);
                if (child == regions.end() || child->second->parent != region.id
                    || !unique_children.insert(child_id).second)
                    return fail(diagnostic, verification_error::invalid_region_tree, 0,
                        "region child links must be unique, reciprocal, and parent-consistent");
            }
            for (const typed_region::item &entry : region.sequence)
            {
                if (entry.id == 0)
                    return fail(diagnostic, verification_error::invalid_region_tree, 0,
                        "region sequence entries require nonzero IDs");
                if (entry.item_kind == typed_region::item::kind::operation)
                {
                    const operation_id operation = static_cast<operation_id>(entry.id);
                    if (std::find(region.operations.begin(), region.operations.end(), operation)
                            == region.operations.end()
                        || !sequenced_operations.insert(operation).second)
                        return fail(diagnostic, verification_error::invalid_region_tree,
                            operation, "region sequence must list each direct operation exactly once");
                }
                else if (entry.item_kind == typed_region::item::kind::child_region)
                {
                    const region_id child = static_cast<region_id>(entry.id);
                    if (std::find(region.children.begin(), region.children.end(), child)
                            == region.children.end()
                        || !sequenced_children.insert(child).second)
                        return fail(diagnostic, verification_error::invalid_region_tree, 0,
                            "region sequence must list each direct child region exactly once");
                }
                else
                    return fail(diagnostic, verification_error::invalid_region_tree, 0,
                        "region sequence has an unknown item kind");
            }
            for (operation_id operation : region.condition_operations)
                if (std::find(region.operations.begin(), region.operations.end(), operation)
                        == region.operations.end()
                    || !condition_operations.insert(operation).second)
                    return fail(diagnostic, verification_error::invalid_region_tree,
                        operation, "loop condition operations must be unique members of their loop region");
            for (operation_id operation : condition_operations)
                if (sequenced_operations.find(operation) != sequenced_operations.end())
                    return fail(diagnostic, verification_error::invalid_region_tree,
                        operation, "loop condition operation cannot also appear in the body sequence");
            if (!region.condition_operations.empty() && region.kind != region_kind::loop)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "only loop regions may contain condition-prologue operations");
            if ((!region.sequence.empty()
                    && (sequenced_operations.size() + condition_operations.size()
                            != region.operations.size()
                        || sequenced_children.size() != region.children.size()))
                || (region.sequence.empty()
                    && (sequenced_operations.size() + condition_operations.size()
                            != region.operations.size()
                        || !region.children.empty())))
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "region sequence must contain every direct operation and child region exactly once");
            if ((region.kind == region_kind::conditional || region.kind == region_kind::loop)
                != (region.condition != 0))
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "conditional and loop regions require exactly one condition value");
            if (region.kind == region_kind::conditional
                && (region.children.empty() || region.children.size() > 2))
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "conditional region requires a then body and at most one else body");
            if (region.kind == region_kind::loop && region.children.size() != 1)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "loop region requires exactly one body region");
            if (region.kind == region_kind::exception_try)
            {
                std::size_t body_count = 0;
                std::size_t handler_count = 0;
                if (region.source_exception_region_id == 0
                    || region.exception_type_id != 0 || region.catches_all
                    || region.children.size() < 2)
                    return fail(diagnostic, verification_error::invalid_region_tree,
                        0, "try region requires a source exception identity, body, and handler");
                for (region_id child_id : region.children)
                {
                    const typed_region *child = regions.at(child_id);
                    if (child->kind == region_kind::lexical_scope)
                        body_count += 1;
                    else if (child->kind == region_kind::exception_handler
                        && child->source_exception_region_id
                            == region.source_exception_region_id)
                        handler_count += 1;
                }
                if (body_count != 1 || handler_count == 0
                    || body_count + handler_count != region.children.size())
                    return fail(diagnostic, verification_error::unsupported_cfg_region,
                        0, "typed try region requires one protected body and valid handlers");
            }
            else if (region.kind == region_kind::exception_handler)
            {
                if ((!region.catches_all && region.exception_type_id == 0)
                    || (region.catches_all && region.exception_type_id != 0)
                    || region.source_exception_region_id == 0 || region.parent == 0
                    || regions.at(region.parent)->kind != region_kind::exception_try
                    || regions.at(region.parent)->source_exception_region_id
                        != region.source_exception_region_id)
                    return fail(diagnostic, verification_error::unsupported_cfg_region,
                        0, "only a matching catch-all handler under a try region is supported");
            }
            else if (region.source_exception_region_id != 0
                || region.exception_type_id != 0 || region.catches_all)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "exception-region metadata is restricted to try and handler regions");
            if (region.condition != 0)
            {
                const auto condition_value = values.find(region.condition);
                if (condition_value == values.end()
                    || condition_value->second->category != value_category::scalar)
                    return fail(diagnostic, verification_error::invalid_region_tree, 0,
                        "control-flow condition must reference a defined scalar value");
            }
            if (region.parent != 0)
            {
                const typed_region *parent = regions.at(region.parent);
                if (std::find(parent->children.begin(), parent->children.end(), region.id)
                    == parent->children.end())
                    return fail(diagnostic, verification_error::invalid_region_tree, 0,
                        "non-root region must be listed by its parent");
            }
        }
        std::vector<region_id> pending_regions{root_id};
        std::unordered_set<region_id> reached_regions;
        while (!pending_regions.empty())
        {
            const region_id current = pending_regions.back();
            pending_regions.pop_back();
            if (!reached_regions.insert(current).second)
                return fail(diagnostic, verification_error::invalid_region_tree, 0,
                    "region tree contains a cycle or repeated child path");
            for (region_id child : regions.at(current)->children)
                pending_regions.push_back(child);
        }
        if (reached_regions.size() != regions.size())
            return fail(diagnostic, verification_error::invalid_region_tree, 0,
                "all regions must be reachable from the function root");
    }
    for (const typed_region &region : function.regions)
    {
        for (operation_id operation_id_value : region.operations)
        {
            const auto operation = operations.find(operation_id_value);
            if (operation == operations.end() || operation->second->region != region.id
                || !operation_regions.emplace(operation_id_value, region.id).second)
                return fail(diagnostic, verification_error::invalid_region_tree,
                    operation_id_value, "each operation must occur exactly once in its declared region");
        }
    }
    for (const typed_operation &operation : function.operations)
    {
        if (regions.find(operation.region) == regions.end()
            || operation_regions.find(operation.id) == operation_regions.end())
            return fail(diagnostic, verification_error::invalid_region_tree, operation.id,
                "operation must reference and belong to an existing region");
    }

    for (const typed_operation &operation : function.operations)
    {
        if (operation.provenance.empty())
            return fail(diagnostic, verification_error::missing_provenance, operation.id,
                "operation requires source or compiler-generated provenance");
        for (const source_origin &origin : operation.provenance)
        {
            if (origin.source_unit_id != function.source_unit_id || origin.role.empty()
                || (!origin.compiler_generated
                    && (origin.line == 0 || origin.column == 0
                        || origin.source_length == 0)))
                return fail(diagnostic, verification_error::missing_provenance, operation.id,
                    "provenance requires matching source identity and a nonempty source span or generated-node role");
        }
        if (operation.outcomes.empty())
            return fail(diagnostic, verification_error::missing_effect_or_outcome,
                operation.id, "operation must declare at least one completion outcome");
        if (operation.operation == opcode::call
            && ((operation.callee == 0) == !operation.has_dependent_lookup))
            return fail(diagnostic, verification_error::incomplete_call_target, operation.id,
                "call requires exactly one resolved callee or dependent lookup context");
        if (operation.operation == opcode::call && operation.callee != 0)
        {
            const auto callable = callables.find(operation.callee);
            const bool is_method = callable != callables.end()
                && callable->second->kind == callable_kind::method;
            const typed_place *first_place = !operation.places.empty()
                && places.find(operation.places.front()) != places.end()
                ? places.at(operation.places.front()) : nullptr;
            const type_id result_type = operation.results.empty() ? 0
                : (operation.results.size() == 1
                    && values.find(operation.results.front()) != values.end()
                    ? values.at(operation.results.front())->type : 0);
            const bool result_place_matches = operation.results.empty()
                ? operation.places.size() == (is_method ? 1U : 0U)
                : operation.places.size() == (is_method ? 2U : 1U)
                    && places.find(operation.places.back()) != places.end()
                    && places.at(operation.places.back())->type == result_type
                    && places.at(operation.places.back())->access
                        == access_mode::read_write;
            bool arguments_match = callable != callables.end()
                && operation.operands.size()
                    == callable->second->parameter_types.size();
            if (arguments_match)
                for (std::size_t argument_index = 0;
                    argument_index < operation.operands.size(); ++argument_index)
                {
                    const auto argument = values.find(
                        operation.operands[argument_index]);
                    if (argument == values.end()
                        || argument->second->type
                            != callable->second->parameter_types[argument_index])
                    {
                        arguments_match = false;
                        break;
                    }
                }
            if (callable == callables.end()
                || !callable->second->has_definition
                || (is_method
                    ? (!first_place
                        || first_place->type != callable->second->owner_type)
                    : (callable->second->kind != callable_kind::function
                        || callable->second->owner_type != 0))
                || !result_place_matches
                || operation.results.size() > 1
                || result_type != callable->second->result_type
                || !arguments_match
                || operation.effects.exceptions != callable->second->exceptions
                || operation.effects.known_exception_type_ids
                    != callable->second->known_exception_type_ids)
                return fail(diagnostic,
                    verification_error::incomplete_call_target,
                    operation.id,
                    "method call requires a canonical defined target matching receiver, arguments, result, and exception effects");
        }
        if ((operation.operation == opcode::throw_value
                && operation.exception_type_id == 0)
            || (operation.operation == opcode::catch_bind
                && (operation.exception_type_id == 0
                    || operation.source_exception_region_id == 0))
            || (operation.operation != opcode::throw_value
                && operation.operation != opcode::catch_bind
                && operation.exception_type_id != 0)
            || (operation.operation != opcode::throw_value
                && operation.operation != opcode::catch_bind
                && operation.source_exception_region_id != 0
                && !operation.effects.may_fail))
            return fail(diagnostic, verification_error::missing_effect_or_outcome,
                operation.id,
                "throw and catch-binding operations require consistent exception type and region identities");
        if ((operation.operation == opcode::construct
                || operation.operation == opcode::copy_construct
                || operation.operation == opcode::move_construct)
            && !operation.effects.constructs)
            return fail(diagnostic, verification_error::missing_effect_or_outcome,
                operation.id,
                "construction opcodes must explicitly record their construction effect");
        if (operation.operation == opcode::construct
            || operation.operation == opcode::copy_construct
            || operation.operation == opcode::move_construct)
        {
            const bool is_copy = operation.operation == opcode::copy_construct;
            const bool is_move = operation.operation == opcode::move_construct;
            const typed_value *destination_value = operation.results.size() == 1
                && values.find(operation.results.front()) != values.end()
                ? values.at(operation.results.front()) : nullptr;
            const typed_place *destination_place = operation.places.size() == 1
                && places.find(operation.places.front()) != places.end()
                ? places.at(operation.places.front()) : nullptr;
            const typed_value *source_value = (is_copy || is_move)
                && operation.operands.size() == 1
                && values.find(operation.operands.front()) != values.end()
                ? values.at(operation.operands.front()) : nullptr;

            if (!destination_value || !destination_place
                || destination_value->type != destination_place->type
                || destination_place->access != access_mode::read_write
                || operation.operands.size() != (is_copy || is_move ? 1U : 0U)
                || ((is_copy || is_move)
                    && (!source_value || source_value->type != destination_value->type
                        || (source_value->category != value_category::owned_object
                            && source_value->category != value_category::borrow)))
                || (is_move && source_value->id == destination_value->id))
                return fail(diagnostic,
                    verification_error::invalid_type_or_projection,
                    operation.id,
                    "construction requires one mutable same-typed destination; copy/move construction also requires one compatible live source");
            if (operation.callee != 0)
            {
                const auto callable = callables.find(operation.callee);
                const callable_kind expected_kind = is_copy
                    ? callable_kind::copy_constructor
                    : (is_move ? callable_kind::move_constructor
                        : callable_kind::constructor);
                bool callable_source_matches = !is_copy && !is_move;
                if ((is_copy || is_move) && source_value
                    && callable != callables.end()
                    && callable->second->parameter_types.size() == 1)
                {
                    const type_id parameter_type =
                        callable->second->parameter_types.front();
                    callable_source_matches = parameter_type == source_value->type;
                    if (!callable_source_matches)
                    {
                        const auto parameter = type_table.find(parameter_type);
                        callable_source_matches = parameter != type_table.end()
                            && parameter->second->kind == typed_type_kind::pointer
                            && parameter->second->pointee_type
                                == source_value->type;
                    }
                }
                if (callable == callables.end()
                    || callable->second->kind != expected_kind
                    || callable->second->owner_type != destination_value->type
                    || !callable->second->has_definition
                    || callable->second->exceptions != operation.effects.exceptions
                    || !callable_source_matches)
                    return fail(diagnostic,
                        verification_error::incomplete_call_target,
                        operation.id,
                        "construction requires a defined canonical constructor target matching its kind, owner, source object or pointer parameter, and exception effect");
            }
        }
        if (operation.operation == opcode::destroy && !operation.effects.destroys)
            return fail(diagnostic, verification_error::missing_effect_or_outcome,
                operation.id,
                "destroy opcodes must explicitly record their destruction effect");
        if ((operation.operation == opcode::destroy
                || operation.operation == opcode::scope_exit)
            && operation.callee != 0)
        {
            const typed_place *destroyed_place = operation.places.size() == 1
                && places.find(operation.places.front()) != places.end()
                ? places.at(operation.places.front()) : nullptr;
            const auto callable = callables.find(operation.callee);
            if (!destroyed_place || callable == callables.end()
                || callable->second->kind != callable_kind::destructor
                || callable->second->owner_type != destroyed_place->type
                || !callable->second->has_definition
                || callable->second->exceptions != exception_effect::non_throwing
                || !callable->second->parameter_types.empty()
                || callable->second->result_type != 0)
                return fail(diagnostic,
                    verification_error::incomplete_call_target,
                    operation.id,
                    "destruction requires a defined, non-throwing canonical destructor matching the destroyed object type");
        }
        if (operation.operation == opcode::scope_exit && !operation.effects.destroys)
            return fail(diagnostic, verification_error::missing_effect_or_outcome,
                operation.id,
                "scope-exit cleanup must explicitly record its destruction effect");
        for (value_id operand : operation.operands)
        {
            if (values.find(operand) == values.end())
                return fail(diagnostic, verification_error::dangling_reference, operation.id,
                    "operation references an unknown operand value");
        }
        if (operation.operation == opcode::compare
            && (operation.operands.size() != 2 || !operation.places.empty()
                || operation.results.size() != 1
                || operation.comparison == comparison_kind::none
                || values.at(operation.operands[0])->category != value_category::scalar
                || values.at(operation.operands[1])->category != value_category::scalar
                || values.at(operation.operands[0])->type
                    != values.at(operation.operands[1])->type
                || values.find(operation.results[0]) == values.end()
                || values.at(operation.results[0])->category != value_category::scalar
                || operation.effects.may_fail || operation.effects.allocates
                || operation.effects.deallocates || operation.effects.constructs
                || operation.effects.destroys))
            return fail(diagnostic, verification_error::invalid_type_or_projection,
                operation.id,
                "scalar comparison requires two same-typed scalar operands and one scalar result with no storage effects");
        if (operation.operation == opcode::add
            || operation.operation == opcode::subtract)
        {
            const typed_value *lhs = operation.operands.size() == 2
                && values.find(operation.operands[0]) != values.end()
                ? values.at(operation.operands[0]) : nullptr;
            const typed_value *rhs = operation.operands.size() == 2
                && values.find(operation.operands[1]) != values.end()
                ? values.at(operation.operands[1]) : nullptr;
            const typed_value *destination = operation.results.size() == 1
                && values.find(operation.results[0]) != values.end()
                ? values.at(operation.results[0]) : nullptr;
            const type_id integer_type = scalar_type_id(CBLC_DATA_KIND_INT);
            if (!lhs || !rhs || !destination
                || lhs->category != value_category::scalar
                || rhs->category != value_category::scalar
                || destination->category != value_category::scalar
                || lhs->type != integer_type || rhs->type != integer_type
                || destination->type != integer_type || !operation.places.empty()
                || !operation.effects.reads.empty()
                || !operation.effects.writes.empty()
                || operation.effects.may_fail || operation.effects.allocates
                || operation.effects.deallocates
                || operation.effects.exceptions != exception_effect::non_throwing
                || operation.effects.constructs || operation.effects.destroys
                || operation.must_follow.empty()
                || operation.outcomes.size() != 1
                || operation.outcomes.front().kind != outcome_kind::normal
                || operation.outcomes.front().initialized.size() != 1
                || operation.outcomes.front().initialized.front()
                    != destination->id
                || !operation.outcomes.front().live.empty()
                || !operation.outcomes.front().moved_from.empty()
                || !operation.outcomes.front().uninitialized.empty()
                || !operation.outcomes.front().destroyed.empty())
                return fail(diagnostic,
                    verification_error::invalid_type_or_projection,
                    operation.id,
                    "integer add/subtract requires two int operands, one int result, explicit operand order, and a normal initialized outcome");
        }
        if (operation.operation == opcode::numeric_convert)
        {
            const typed_value *source = operation.operands.size() == 1
                && values.find(operation.operands.front()) != values.end()
                ? values.at(operation.operands.front()) : nullptr;
            const typed_value *destination = operation.results.size() == 1
                && values.find(operation.results.front()) != values.end()
                ? values.at(operation.results.front()) : nullptr;
            if (!source || !destination
                || source->category != value_category::scalar
                || destination->category != value_category::scalar
                || source->type != scalar_type_id(CBLC_DATA_KIND_INT)
                || destination->type != scalar_type_id(CBLC_DATA_KIND_DOUBLE)
                || !operation.places.empty()
                || !operation.effects.reads.empty()
                || !operation.effects.writes.empty()
                || operation.effects.may_fail
                || operation.effects.allocates || operation.effects.deallocates
                || operation.effects.exceptions != exception_effect::non_throwing
                || operation.effects.constructs || operation.effects.destroys
                || operation.must_follow.size() != 1
                || operation.outcomes.size() != 1
                || operation.outcomes.front().kind != outcome_kind::normal
                || operation.outcomes.front().initialized.size() != 1
                || operation.outcomes.front().initialized.front() != destination->id
                || !operation.outcomes.front().live.empty()
                || !operation.outcomes.front().moved_from.empty()
                || !operation.outcomes.front().uninitialized.empty()
                || !operation.outcomes.front().destroyed.empty())
                return fail(diagnostic,
                    verification_error::invalid_type_or_projection,
                    operation.id,
                    "numeric conversion must explicitly widen one int scalar to one double scalar without storage or failure effects");
        }
        if (operation.operation == opcode::copy_assign
            || operation.operation == opcode::move_assign)
        {
            const bool is_move = operation.operation == opcode::move_assign;
            const typed_place *target = operation.places.size() == 1
                && places.find(operation.places.front()) != places.end()
                ? places.at(operation.places.front()) : nullptr;
            const typed_value *source = operation.operands.size() == 1
                && values.find(operation.operands.front()) != values.end()
                ? values.at(operation.operands.front()) : nullptr;
            if (!target || !source || !operation.results.empty())
                return fail(diagnostic, verification_error::invalid_type_or_projection,
                    operation.id,
                    "copy/move assignment requires one target place and one source value");
            if (target->access != access_mode::read_write
                || target->type != source->type
                || !values.count(target->base_value))
                return fail(diagnostic, verification_error::invalid_type_or_projection,
                    operation.id,
                    "copy/move assignment requires a mutable target with the source's canonical type");
            const typed_value *target_object = values.at(target->base_value);
            const bool owned_target = target_object->category
                == value_category::owned_object;
            const bool borrowed_target = target_object->category
                    == value_category::borrow
                && target_object->lifetime_token != 0
                && target->owner_lifetime_token
                    == target_object->lifetime_token;
            if (!owned_target && !borrowed_target)
                return fail(diagnostic, verification_error::invalid_type_or_projection,
                    operation.id,
                    "copy/move assignment target must be owned or a mutable borrow tied to its owner's lifetime");
            const bool owned_source = source->category
                == value_category::owned_object;
            const bool borrowed_copy_source = !is_move
                && source->category == value_category::borrow
                && source->lifetime_token != 0;
            if ((!owned_source && !borrowed_copy_source)
                || operation.effects.reads.size() != 1
                || operation.effects.reads.front() != operation.places.front()
                || operation.effects.writes.size() != 1
                || operation.effects.writes.front() != operation.places.front()
                || operation.effects.constructs || operation.effects.destroys)
            {
                std::string source_name = "<unbound>";
                for (const typed_place &source_place : function.places)
                {
                    if (source_place.base_value != source->id
                        || source_place.symbol == 0)
                        continue;
                    const auto source_symbol = std::find_if(
                        function.symbols.begin(), function.symbols.end(),
                        [&](const typed_symbol &symbol)
                        {
                            return symbol.id == source_place.symbol;
                        });
                    if (source_symbol != function.symbols.end())
                        source_name = source_symbol->canonical_name;
                    break;
                }
                const std::string reason =
                    "copy assignment requires an owned or lifetime-bound borrowed source; move assignment requires an owned source and exact read/write effects on its target (source-category="
                    + std::to_string(static_cast<int>(source->category))
                    + ", source-lifetime="
                    + std::to_string(source->lifetime_token)
                    + ", source-name=" + source_name
                    + ", reads=" + std::to_string(operation.effects.reads.size())
                    + ", writes=" + std::to_string(operation.effects.writes.size())
                    + ", target-place=" + std::to_string(operation.places.front())
                    + ", read-place=" + (operation.effects.reads.empty()
                        ? std::string("none")
                        : std::to_string(operation.effects.reads.front()))
                    + ", write-place=" + (operation.effects.writes.empty()
                        ? std::string("none")
                        : std::to_string(operation.effects.writes.front())) + ")";
                return fail(diagnostic, verification_error::invalid_type_or_projection,
                    operation.id, reason.c_str());
            }
            const auto callable = callables.find(operation.callee);
            const callable_kind expected_kind = is_move
                ? callable_kind::move_assignment
                : callable_kind::copy_assignment;
            if (callable == callables.end()
                || callable->second->kind != expected_kind
                || callable->second->owner_type != target->type
                || callable->second->parameter_types.size() != 1
                || callable->second->parameter_types.front() != source->type
                || callable->second->result_type != 0
                || callable->second->exceptions != exception_effect::non_throwing
                || !callable->second->has_definition)
                return fail(diagnostic,
                    verification_error::incomplete_call_target,
                    operation.id,
                    "copy/move assignment requires a canonical, defined, same-typed non-throwing dispatch target");
            if (is_move && target->base_value != source->id)
                for (const operation_outcome &outcome : operation.outcomes)
                    if (outcome.kind == outcome_kind::normal
                        && std::find(outcome.moved_from.begin(), outcome.moved_from.end(),
                            source->id) == outcome.moved_from.end())
                        return fail(diagnostic,
                            verification_error::invalid_outcome_state,
                            operation.id,
                            "successful move assignment must mark its source moved-from");
            if (is_move && target->base_value == source->id)
                for (const operation_outcome &outcome : operation.outcomes)
                    if (std::find(outcome.moved_from.begin(), outcome.moved_from.end(),
                            source->id) != outcome.moved_from.end())
                        return fail(diagnostic,
                            verification_error::invalid_outcome_state,
                            operation.id,
                            "self-move assignment must preserve the target's live lifetime state");
        }
        const auto assign_target = operation.places.empty()
            ? places.end() : places.find(operation.places.front());
        const bool assigns_owned_target = assign_target != places.end()
            && values.at(assign_target->second->base_value)->category
                == value_category::owned_object;
        const bool assigns_owned_source = !operation.operands.empty()
            && values.at(operation.operands.front())->category
                == value_category::owned_object;
        if (operation.operation == opcode::assign
            && (assigns_owned_target || assigns_owned_source))
            return fail(diagnostic, verification_error::invalid_type_or_projection,
                operation.id,
                "owned objects require explicit copy_assign or move_assign operations");
        if (operation.operation != opcode::compare
            && operation.comparison != comparison_kind::none)
            return fail(diagnostic, verification_error::invalid_type_or_projection,
                operation.id,
                "comparison predicate may appear only on a compare operation");
        for (place_id place_id_value : operation.places)
        {
            const auto place = places.find(place_id_value);
            if (place == places.end())
                return fail(diagnostic, verification_error::dangling_reference, operation.id,
                    "operation references an unknown place");
            if ((operation.operation == opcode::store || operation.operation == opcode::assign
                    || operation.operation == opcode::copy_assign
                    || operation.operation == opcode::move_assign)
                && place->second->access != access_mode::read_write)
                return fail(diagnostic, verification_error::immutable_write, operation.id,
                    "write operation targets an immutable place");
        }
        if (operation.operation == opcode::catch_bind)
        {
            const typed_region *handler = regions.at(operation.region);
            const typed_place *binding_place = operation.places.size() == 1
                ? places.at(operation.places.front()) : nullptr;
            if (handler->kind != region_kind::exception_handler
                || handler->catches_all
                || handler->source_exception_region_id
                    != operation.source_exception_region_id
                || handler->exception_type_id != operation.exception_type_id
                || operation.operands.size() != 0 || !operation.results.empty()
                || !binding_place || binding_place->type
                    != scalar_type_id(CBLC_DATA_KIND_INT)
                || binding_place->access != access_mode::read_write
                || operation.effects.reads.size() != 0
                || operation.effects.writes.size() != 1
                || operation.effects.writes.front() != binding_place->id
                || operation.effects.may_fail
                || operation.effects.exceptions != exception_effect::non_throwing
                || operation.outcomes.size() != 1
                || operation.outcomes.front().kind != outcome_kind::normal)
                return fail(diagnostic,
                    verification_error::invalid_type_or_projection,
                    operation.id,
                    "catch binding must copy one matching int payload into a mutable local int place without throwing");
        }
        for (value_id result : operation.results)
        {
            if (values.find(result) == values.end())
                return fail(diagnostic, verification_error::dangling_reference, operation.id,
                    "operation result does not have a value definition");
            if (!producers.emplace(result, operation.id).second)
                return fail(diagnostic, verification_error::duplicate_or_zero_id,
                    operation.id, "each value definition may have only one producing operation");
        }
        if ((operation.operation == opcode::store || operation.operation == opcode::assign
                || operation.operation == opcode::copy_assign
                || operation.operation == opcode::move_assign)
            && !operation.places.empty() && !operation.operands.empty()
            && places.at(operation.places.front())->type
                != values.at(operation.operands.front())->type)
            return fail(diagnostic, verification_error::invalid_type_or_projection,
                operation.id, "assignment source and destination types must match after explicit conversion");
        if (operation.operation == opcode::load
            && (operation.places.size() != 1 || operation.results.size() != 1
                || places.at(operation.places.front())->type
                    != values.at(operation.results.front())->type))
            return fail(diagnostic, verification_error::invalid_type_or_projection,
                operation.id, "load requires one place and one same-typed result");
        for (place_id place : operation.effects.reads)
        {
            if (places.find(place) == places.end())
                return fail(diagnostic, verification_error::dangling_reference, operation.id,
                    "effect summary reads an unknown place");
        }
        for (place_id place : operation.effects.writes)
        {
            const auto found = places.find(place);
            if (found == places.end())
                return fail(diagnostic, verification_error::dangling_reference, operation.id,
                    "effect summary writes an unknown place");
            if (found->second->access != access_mode::read_write)
                return fail(diagnostic, verification_error::immutable_write, operation.id,
                    "effect summary writes an immutable place");
        }
        if (operation.effects.exceptions == exception_effect::unknown && !operation.effects.may_fail)
            return fail(diagnostic, verification_error::missing_effect_or_outcome,
                operation.id, "unknown exception effects must conservatively be marked as may-fail");
        {
            std::unordered_set<std::uint64_t> exception_type_ids;
            for (std::uint64_t exception_type_id
                : operation.effects.known_exception_type_ids)
                if (exception_type_id == 0
                    || !exception_type_ids.insert(exception_type_id).second)
                    return fail(diagnostic,
                        verification_error::missing_effect_or_outcome,
                        operation.id,
                        "known exception type IDs must be unique and nonzero");
            if ((operation.effects.exceptions == exception_effect::non_throwing
                    && !operation.effects.known_exception_type_ids.empty())
                || (operation.effects.exceptions == exception_effect::may_throw_known
                    && operation.effects.known_exception_type_ids.empty()))
                return fail(diagnostic,
                    verification_error::missing_effect_or_outcome,
                    operation.id,
                    "exception effect category must agree with its known exception type set");
            if (operation.operation == opcode::throw_value
                && (operation.effects.known_exception_type_ids.size() != 1
                    || operation.effects.known_exception_type_ids.front()
                        != operation.exception_type_id))
                return fail(diagnostic,
                    verification_error::missing_effect_or_outcome,
                    operation.id,
                    "explicit throw effect must list exactly its thrown exception type");
        }
        {
            std::size_t normal_outcomes = 0;
            std::size_t failure_outcomes = 0;

            for (const operation_outcome &outcome : operation.outcomes)
            {
                if (outcome.kind == outcome_kind::normal)
                    normal_outcomes += 1;
                else
                {
                    failure_outcomes += 1;
                    if (outcome.kind == outcome_kind::thrown_exception
                        && operation.effects.exceptions == exception_effect::non_throwing)
                        return fail(diagnostic, verification_error::missing_effect_or_outcome,
                            operation.id, "non-throwing operation cannot declare a thrown outcome");
                }
            }
            const bool explicit_throw = operation.operation == opcode::throw_value;
            if ((explicit_throw
                    && (operation.effects.may_fail || normal_outcomes != 0
                        || failure_outcomes != 1))
                || (!explicit_throw && operation.effects.may_fail
                    && (normal_outcomes != 1 || failure_outcomes == 0))
                || (!explicit_throw && !operation.effects.may_fail
                    && failure_outcomes != 0))
                return fail(diagnostic, verification_error::missing_effect_or_outcome,
                    operation.id, "failure effects and declared normal/failure outcomes must agree");
        }
        for (const operation_outcome &outcome : operation.outcomes)
        {
            std::unordered_set<value_id> completion_states;
            if ((outcome.exception_type_id != 0
                    && outcome.kind != outcome_kind::thrown_exception)
                || (outcome.exception_type_id != 0
                    && std::find(operation.effects.known_exception_type_ids.begin(),
                        operation.effects.known_exception_type_ids.end(),
                        outcome.exception_type_id)
                        == operation.effects.known_exception_type_ids.end()))
                return fail(diagnostic,
                    verification_error::missing_effect_or_outcome,
                    operation.id,
                    "typed exception outcomes must name a declared thrown type");
            for (value_id value : outcome.initialized)
            {
                if (values.find(value) == values.end())
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "outcome initializes an unknown value");
                if (!completion_states.insert(value).second)
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "outcome contains duplicate completion state");
            }
            for (value_id value : outcome.live)
            {
                if (values.find(value) == values.end())
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "outcome marks an unknown value live");
                if (!completion_states.insert(value).second)
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "a value cannot have multiple completion states on one outcome");
                if (operation.operation == opcode::call
                    && values.at(value)->category == value_category::owned_object
                    && (!operation.effects.constructs
                        || std::find(operation.results.begin(),
                            operation.results.end(), value)
                            == operation.results.end()))
                    return fail(diagnostic,
                        verification_error::invalid_outcome_state,
                        operation.id,
                        "a call may complete an owned-object lifetime only by constructing a declared result");
            }
            for (value_id value : outcome.moved_from)
            {
                if (values.find(value) == values.end())
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "outcome marks an unknown value moved-from");
                if (!completion_states.insert(value).second)
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "a value cannot have multiple completion states on one outcome");
            }
            for (value_id value : outcome.uninitialized)
            {
                if (values.find(value) == values.end())
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "outcome marks an unknown value uninitialized");
                if (!completion_states.insert(value).second)
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "a value cannot have multiple completion states on one outcome");
            }
            for (value_id value : outcome.destroyed)
            {
                if (values.find(value) == values.end())
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "outcome marks an unknown value destroyed");
                if (!completion_states.insert(value).second)
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id, "a value cannot have multiple completion states on one outcome");
                const bool constructor_failure_cleanup =
                    (operation.operation == opcode::construct
                        || operation.operation == opcode::copy_construct
                        || operation.operation == opcode::move_construct)
                    && outcome.kind == outcome_kind::thrown_exception
                    && operation.effects.destroys;
                if (operation.operation != opcode::destroy
                    && operation.operation != opcode::scope_exit
                    && !constructor_failure_cleanup)
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id,
                        "only destruction, guarded scope exit, or constructor-failure cleanup may end an object's lifetime");
            }
            for (value_id result : operation.results)
            {
                if (completion_states.find(result) == completion_states.end())
                    return fail(diagnostic, verification_error::invalid_outcome_state,
                        operation.id,
                        "every operation result must have exactly one state on every declared outcome");
                const typed_value *result_value = values.at(result);
                if (result_value->category == value_category::owned_object)
                {
                    const bool normal_live = outcome.kind == outcome_kind::normal
                        && std::find(outcome.live.begin(), outcome.live.end(), result)
                            != outcome.live.end();
                    const bool failure_uninitialized = outcome.kind != outcome_kind::normal
                        && std::find(outcome.uninitialized.begin(),
                            outcome.uninitialized.end(), result)
                            != outcome.uninitialized.end();
                    if (!normal_live && !failure_uninitialized)
                        return fail(diagnostic, verification_error::invalid_outcome_state,
                            operation.id,
                            "owned construction results must be live on success and uninitialized on failure");
                }
                if (operation.operation == opcode::construct
                    || operation.operation == opcode::copy_construct
                    || operation.operation == opcode::move_construct)
                {
                    const bool initialized = std::find(outcome.initialized.begin(),
                        outcome.initialized.end(), result) != outcome.initialized.end();
                    const bool live = std::find(outcome.live.begin(),
                        outcome.live.end(), result) != outcome.live.end();
                    const bool uninitialized = std::find(
                        outcome.uninitialized.begin(), outcome.uninitialized.end(),
                        result) != outcome.uninitialized.end();
                    if ((outcome.kind == outcome_kind::normal
                            && initialized == live)
                        || (outcome.kind != outcome_kind::normal
                            && !uninitialized))
                        return fail(diagnostic,
                            verification_error::invalid_outcome_state,
                            operation.id,
                            "construction must initialize exactly one destination state on success and leave it uninitialized on every failure outcome");
                }
            }
            if (operation.operation == opcode::copy_construct
                || operation.operation == opcode::move_construct)
            {
                const value_id source = operation.operands.front();
                const bool source_marked_moved = std::find(
                    outcome.moved_from.begin(), outcome.moved_from.end(), source)
                    != outcome.moved_from.end();
                if (source_marked_moved
                    != (operation.operation == opcode::move_construct
                        && outcome.kind == outcome_kind::normal))
                    return fail(diagnostic,
                        verification_error::invalid_outcome_state,
                        operation.id,
                        "copy construction preserves its source, while move construction marks the source moved-from only after successful completion");
            }
        }
        for (operation_id predecessor : operation.must_follow)
        {
            if (operations.find(predecessor) == operations.end())
                return fail(diagnostic, verification_error::dangling_reference,
                    operation.id, "evaluation-order edge references an unknown operation");
            successors[predecessor].push_back(operation.id);
            indegree[operation.id] += 1;
        }
    }

    for (const typed_operation &operation : function.operations)
    {
        for (value_id operand : operation.operands)
        {
            const auto producer = producers.find(operand);
            if (producer != producers.end() && producer->second == operation.id)
                return fail(diagnostic, verification_error::use_before_definition,
                    operation.id,
                    "operation cannot consume a value that it defines itself");
            if (producer != producers.end())
            {
                std::vector<operation_id> pending{operation.id};
                std::unordered_set<operation_id> visited_operations;
                bool ordered_after_producer = false;

                while (!pending.empty() && !ordered_after_producer)
                {
                    const operation_id current = pending.back();
                    pending.pop_back();
                    if (!visited_operations.insert(current).second)
                        continue;
                    if (current == producer->second)
                    {
                        ordered_after_producer = true;
                        break;
                    }
                    const auto current_operation = operations.find(current);
                    if (current_operation != operations.end())
                        for (operation_id predecessor : current_operation->second->must_follow)
                            pending.push_back(predecessor);
                }
                if (!ordered_after_producer)
                    return fail(diagnostic, verification_error::use_before_definition,
                        operation.id,
                        "operation consuming a produced value must be ordered after its definition");
            }
        }
        for (place_id place_id_value : operation.places)
        {
            const auto place = places.find(place_id_value);
            if (place == places.end())
                continue;
            for (const place_projection &projection :
                place->second->projections)
            {
                if (projection.index_value == 0)
                    continue;
                const auto producer = producers.find(projection.index_value);
                if (producer == producers.end())
                    continue;
                if (producer->second == operation.id)
                    return fail(diagnostic,
                        verification_error::use_before_definition,
                        operation.id,
                        "operation cannot use a dynamic index value that it defines itself");
                std::vector<operation_id> pending{operation.id};
                std::unordered_set<operation_id> visited_operations;
                bool ordered_after_producer = false;
                while (!pending.empty() && !ordered_after_producer)
                {
                    const operation_id current = pending.back();
                    pending.pop_back();
                    if (!visited_operations.insert(current).second)
                        continue;
                    if (current == producer->second)
                    {
                        ordered_after_producer = true;
                        break;
                    }
                    const auto current_operation = operations.find(current);
                    if (current_operation != operations.end())
                        for (operation_id predecessor
                            : current_operation->second->must_follow)
                            pending.push_back(predecessor);
                }
                if (!ordered_after_producer)
                    return fail(diagnostic,
                        verification_error::use_before_definition,
                        operation.id,
                        "operation using a projected place must follow the definition of its dynamic index");
            }
        }
    }

    std::vector<operation_id> ready;
    for (const auto &entry : indegree)
        if (entry.second == 0)
            ready.push_back(entry.first);
    visited = 0;
    while (!ready.empty())
    {
        const operation_id current = ready.back();
        ready.pop_back();
        visited += 1;
        for (operation_id successor : successors[current])
        {
            indegree[successor] -= 1;
            if (indegree[successor] == 0)
                ready.push_back(successor);
        }
    }
    if (visited != function.operations.size())
        return fail(diagnostic, verification_error::cyclic_evaluation_order, 0,
            "evaluation-order edges must be acyclic");
    return true;
}

bool build_linear_control_flow_graph(const typed_function &function,
    control_flow_graph *result, verification_diagnostic *diagnostic)
{
    std::unordered_map<operation_id, const typed_operation *> operations;
    std::unordered_map<operation_id, basic_block_id> operation_blocks;
    std::unordered_map<operation_id, std::size_t> operation_positions;
    const typed_region *root;
    basic_block_id next_block_id;
    basic_block_id normal_exit_id;
    bool needs_normal_exit;
    std::size_t index;

    if (diagnostic)
        *diagnostic = verification_diagnostic{};
    if (!result)
        return fail(diagnostic, verification_error::invalid_cfg_block, 0,
            "CFG construction requires an output graph");
    *result = control_flow_graph{};
    if (!verify_typed_function(function, diagnostic))
        return false;
    if (function.regions.size() != 1 || !function.regions.front().children.empty()
        || function.regions.front().kind != region_kind::function_body)
        return fail(diagnostic, verification_error::unsupported_cfg_region, 0,
            "CFG lowering currently requires a single function-body region");
    root = &function.regions.front();
    for (index = 0; index < function.operations.size(); ++index)
        operations.emplace(function.operations[index].id, &function.operations[index]);
    for (index = 0; index < root->operations.size(); ++index)
    {
        const operation_id operation = root->operations[index];
        const auto found = operations.find(operation);
        if (found == operations.end())
            return fail(diagnostic, verification_error::invalid_cfg_block, operation,
                "root operation sequence references an unknown operation");
        operation_positions.emplace(operation, index);
        operation_blocks.emplace(operation, static_cast<basic_block_id>(index + 1));
        for (operation_id predecessor : found->second->must_follow)
        {
            const auto predecessor_position = operation_positions.find(predecessor);
            if (predecessor_position == operation_positions.end()
                || predecessor_position->second >= index)
                return fail(diagnostic, verification_error::use_before_definition,
                    operation, "region operation order contradicts an evaluation-order dependency");
        }
    }
    result->schema_version = typed_ir_schema_version;
    result->source_unit_id = function.source_unit_id;
    result->function_id = function.function_id;
    result->entry = 1;
    needs_normal_exit = root->operations.empty();
    if (!root->operations.empty())
    {
        const typed_operation *last = operations.at(root->operations.back());
        needs_normal_exit = last->operation != opcode::return_value
            && last->operation != opcode::throw_value;
    }
    {
        std::size_t sink_count = 0;

        for (operation_id operation_id_value : root->operations)
        {
            const typed_operation *operation = operations.at(operation_id_value);
            if (operation->operation == opcode::throw_value)
                sink_count += 1;
            else if (operation->effects.may_fail)
                for (const operation_outcome &outcome : operation->outcomes)
                    if (outcome.kind != outcome_kind::normal)
                        sink_count += 1;
        }
        result->blocks.reserve(root->operations.size() + sink_count
            + (needs_normal_exit ? 1U : 0U));
    }
    index = 0;
    while (index < root->operations.size())
    {
        basic_block block;
        block.id = static_cast<basic_block_id>(index + 1);
        block.operations.push_back(root->operations[index]);
        result->blocks.push_back(block);
        index += 1;
    }
    next_block_id = static_cast<basic_block_id>(root->operations.size() + 1);
    normal_exit_id = 0;
    if (needs_normal_exit)
    {
        basic_block normal_exit;
        normal_exit.id = next_block_id++;
        normal_exit.terminator.kind = cfg_terminator_kind::normal_exit;
        normal_exit_id = normal_exit.id;
        result->blocks.push_back(normal_exit);
    }
    for (index = 0; index < root->operations.size(); ++index)
    {
        const operation_id operation_id_value = root->operations[index];
        const typed_operation &operation = *operations.at(operation_id_value);
        basic_block &block = result->blocks[index];
        const basic_block_id next_id = index + 1 < root->operations.size()
            ? static_cast<basic_block_id>(index + 2) : normal_exit_id;

        block.terminator.operation = operation.id;
        if (operation.operation == opcode::return_value)
        {
            block.terminator.kind = cfg_terminator_kind::return_value;
            continue;
        }
        if (operation.operation == opcode::throw_value)
        {
            basic_block uncaught;
            uncaught.id = next_block_id++;
            uncaught.terminator.kind = cfg_terminator_kind::uncaught_exception_exit;
            result->blocks.push_back(uncaught);
            block.terminator.kind = cfg_terminator_kind::throw_value;
            block.terminator.successors.push_back({uncaught.id,
                outcome_kind::thrown_exception, operation.exception_type_id});
            continue;
        }
        if (operation.effects.may_fail)
        {
            block.terminator.kind = cfg_terminator_kind::invoke;
            for (const operation_outcome &outcome : operation.outcomes)
            {
                if (outcome.kind == outcome_kind::normal)
                {
                    if (next_id == 0)
                        return fail(diagnostic, verification_error::invalid_cfg_edge,
                            operation.id, "may-fail operation has no normal continuation");
                    block.terminator.successors.push_back({next_id, outcome.kind});
                }
                else
                {
                    basic_block exit_block;
                    exit_block.id = next_block_id++;
                    exit_block.terminator.kind = outcome.kind == outcome_kind::fatal_termination
                        ? cfg_terminator_kind::fatal_exit
                        : cfg_terminator_kind::uncaught_exception_exit;
                    result->blocks.push_back(exit_block);
                    block.terminator.successors.push_back({exit_block.id, outcome.kind});
                }
            }
            continue;
        }
        block.terminator.kind = cfg_terminator_kind::jump;
        if (next_id == 0)
            return fail(diagnostic, verification_error::invalid_cfg_edge, operation.id,
                "nonterminating operation has no successor");
        block.terminator.successors.push_back({next_id, outcome_kind::normal});
    }
    if (!verify_control_flow_graph(function, *result, diagnostic)
        || !verify_lifetime_transitions(function, *result, diagnostic))
    {
        *result = control_flow_graph{};
        return false;
    }
    return true;
}

static bool typed_region_falls_through(const typed_region &region,
    const std::unordered_map<operation_id,
        const typed_operation *> &operations,
    const std::unordered_map<region_id, const typed_region *> &regions)
{
    if (region.kind == region_kind::exception_try)
    {
        bool clause_falls = false;
        for (region_id clause_id : region.children)
            if (typed_region_falls_through(*regions.at(clause_id), operations,
                    regions))
                clause_falls = true;
        return clause_falls;
    }
    for (const typed_region::item &entry : region.sequence)
    {
        if (entry.item_kind == typed_region::item::kind::operation)
        {
            const typed_operation *operation = operations.at(
                static_cast<operation_id>(entry.id));
            if (operation->operation == opcode::return_value
                || operation->operation == opcode::throw_value)
                return false;
        }
        else
        {
            const typed_region *child = regions.at(static_cast<region_id>(entry.id));
            if (child->kind == region_kind::conditional)
            {
                const bool then_falls = typed_region_falls_through(
                    *regions.at(child->children[0]), operations, regions);
                const bool else_falls = child->children.size() == 1
                    || typed_region_falls_through(
                        *regions.at(child->children[1]), operations, regions);
                if (!then_falls && !else_falls)
                    return false;
            }
            else if (child->kind != region_kind::loop
                && !typed_region_falls_through(*child, operations, regions))
                return false;
        }
    }
    return true;
}

struct exception_handler_entry
{
    region_id region = 0;
    basic_block_id block = 0;
    std::uint64_t exception_type_id = 0;
    bool catches_all = false;
};

using exception_handler_entries = std::unordered_map<std::uint64_t,
    std::vector<exception_handler_entry>>;

static bool append_cfg_operation(const typed_operation &operation,
    basic_block_id continuation, control_flow_graph *graph,
    basic_block_id *next_id, basic_block_id *entry,
    const exception_handler_entries &handlers,
    verification_diagnostic *diagnostic)
{
    basic_block block;
    block.id = (*next_id)++;
    block.operations.push_back(operation.id);
    block.terminator.operation = operation.id;
    if (operation.operation == opcode::return_value)
        block.terminator.kind = cfg_terminator_kind::return_value;
    else if (operation.operation == opcode::throw_value)
    {
        block.terminator.kind = cfg_terminator_kind::throw_value;
        const auto handler = handlers.find(operation.source_exception_region_id);
        const exception_handler_entry *selected_handler = nullptr;
        if (handler != handlers.end())
        {
            for (const exception_handler_entry &candidate : handler->second)
                if (candidate.catches_all
                    || candidate.exception_type_id == operation.exception_type_id)
                {
                    selected_handler = &candidate;
                    break;
                }
        }
        if (selected_handler)
        {
            block.terminator.exception_handler_region = selected_handler->region;
            block.terminator.successors.push_back({selected_handler->block,
                outcome_kind::thrown_exception, operation.exception_type_id,
                operation.operands.size() == 1
                    ? operation.operands.front() : 0});
        }
        else
        {
            basic_block uncaught;
            uncaught.id = (*next_id)++;
            uncaught.terminator.kind = cfg_terminator_kind::uncaught_exception_exit;
            graph->blocks.push_back(uncaught);
            block.terminator.successors.push_back({uncaught.id,
                outcome_kind::thrown_exception, operation.exception_type_id,
                operation.operands.size() == 1
                    ? operation.operands.front() : 0});
        }
    }
    else if (operation.effects.may_fail)
    {
        if (continuation == 0)
            return fail(diagnostic, verification_error::invalid_cfg_edge,
                operation.id, "may-fail operation requires a normal continuation");
        block.terminator.kind = cfg_terminator_kind::invoke;
        for (const operation_outcome &outcome : operation.outcomes)
        {
            if (outcome.kind == outcome_kind::normal)
                block.terminator.successors.push_back({continuation, outcome.kind});
            else if (outcome.kind == outcome_kind::thrown_exception)
            {
                const auto handler = handlers.find(
                    operation.source_exception_region_id);
                const exception_handler_entry *selected_handler = nullptr;

                if (handler != handlers.end())
                    for (const exception_handler_entry &candidate : handler->second)
                        if (candidate.catches_all
                            || (outcome.exception_type_id != 0
                                && candidate.exception_type_id
                                    == outcome.exception_type_id))
                        {
                            selected_handler = &candidate;
                            break;
                        }
                if (selected_handler)
                    block.terminator.successors.push_back({
                        selected_handler->block, outcome.kind,
                        outcome.exception_type_id, 0});
                else
                {
                    basic_block sink;
                    sink.id = (*next_id)++;
                    sink.terminator.kind = cfg_terminator_kind::uncaught_exception_exit;
                    graph->blocks.push_back(sink);
                    block.terminator.successors.push_back({sink.id, outcome.kind,
                        outcome.exception_type_id, 0});
                }
            }
            else
            {
                basic_block sink;
                sink.id = (*next_id)++;
                sink.terminator.kind = outcome.kind == outcome_kind::fatal_termination
                    ? cfg_terminator_kind::fatal_exit
                    : cfg_terminator_kind::uncaught_exception_exit;
                graph->blocks.push_back(sink);
                block.terminator.successors.push_back({sink.id, outcome.kind,
                    0, 0});
            }
        }
    }
    else
    {
        if (continuation == 0)
            return fail(diagnostic, verification_error::invalid_cfg_edge,
                operation.id, "nonterminating operation requires a continuation");
        block.terminator.kind = cfg_terminator_kind::jump;
        block.terminator.successors.push_back({continuation, outcome_kind::normal});
    }
    *entry = block.id;
    graph->blocks.push_back(block);
    return true;
}

static bool append_cfg_region(const typed_region &region,
    const std::unordered_map<operation_id, const typed_operation *> &operations,
    const std::unordered_map<region_id, const typed_region *> &regions,
    basic_block_id continuation, control_flow_graph *graph,
    basic_block_id *next_id, basic_block_id *entry,
    exception_handler_entries *handlers,
    verification_diagnostic *diagnostic)
{
    basic_block_id current = continuation;
    std::size_t reverse = region.sequence.size();

    if (region.kind != region_kind::function_body
        && region.kind != region_kind::lexical_scope
        && region.kind != region_kind::conditional
        && region.kind != region_kind::loop
        && region.kind != region_kind::exception_try
        && region.kind != region_kind::exception_handler)
        return fail(diagnostic, verification_error::unsupported_cfg_region, 0,
            "typed CFG lowering does not yet support this structured region");
    while (reverse > 0)
    {
        const typed_region::item &item = region.sequence[reverse - 1];
        if (item.item_kind == typed_region::item::kind::operation)
        {
            basic_block_id operation_entry;
            if (!append_cfg_operation(*operations.at(static_cast<operation_id>(item.id)),
                    current, graph, next_id, &operation_entry, *handlers,
                    diagnostic))
                return false;
            current = operation_entry;
        }
        else
        {
            const typed_region &child = *regions.at(static_cast<region_id>(item.id));
            basic_block_id child_entry;
            if (child.kind == region_kind::conditional)
            {
                basic_block_id then_entry;
                basic_block_id else_entry = current;
                if (!append_cfg_region(*regions.at(child.children[0]), operations,
                        regions, current, graph, next_id, &then_entry, handlers,
                        diagnostic))
                    return false;
                if (child.children.size() == 2
                    && !append_cfg_region(*regions.at(child.children[1]), operations,
                        regions, current, graph, next_id, &else_entry, handlers,
                        diagnostic))
                    return false;
                basic_block branch;
                branch.id = (*next_id)++;
                branch.terminator.kind = cfg_terminator_kind::branch;
                branch.terminator.condition = child.condition;
                branch.terminator.control_region = child.id;
                branch.terminator.successors.push_back({then_entry,
                    outcome_kind::normal});
                branch.terminator.successors.push_back({else_entry,
                    outcome_kind::normal});
                child_entry = branch.id;
                graph->blocks.push_back(branch);
            }
            else if (child.kind == region_kind::loop)
            {
                basic_block branch;
                branch.id = (*next_id)++;
                branch.terminator.kind = cfg_terminator_kind::branch;
                branch.terminator.condition = child.condition;
                branch.terminator.control_region = child.id;
                branch.terminator.successors.push_back({0, outcome_kind::normal});
                branch.terminator.successors.push_back({current,
                    outcome_kind::normal});
                graph->blocks.push_back(branch);
                basic_block_id condition_entry = branch.id;
                std::size_t condition_index = child.condition_operations.size();
                while (condition_index > 0)
                {
                    basic_block_id operation_entry;
                    if (!append_cfg_operation(*operations.at(
                                child.condition_operations[condition_index - 1]),
                            condition_entry, graph, next_id, &operation_entry,
                            *handlers, diagnostic))
                        return false;
                    condition_entry = operation_entry;
                    condition_index -= 1;
                }
                basic_block_id body_entry;
                if (!append_cfg_region(*regions.at(child.children[0]), operations,
                        regions, condition_entry, graph, next_id, &body_entry,
                        handlers, diagnostic))
                    return false;
                for (basic_block &candidate : graph->blocks)
                    if (candidate.id == branch.id)
                        candidate.terminator.successors[0].target = body_entry;
                child_entry = condition_entry;
            }
            else if (child.kind == region_kind::exception_try)
            {
                const typed_region *body = nullptr;
                std::vector<const typed_region *> handlers_in_order;
                std::vector<exception_handler_entry> handler_entries;
                std::unordered_set<std::uint64_t> handled_type_ids;
                bool saw_catch_all = false;
                if (child.source_exception_region_id == 0
                    || child.children.size() < 2)
                    return fail(diagnostic, verification_error::invalid_region_tree,
                        0, "try region requires a protected body and handler");
                for (region_id child_id : child.children)
                {
                    const typed_region &part = *regions.at(child_id);
                    if (part.kind == region_kind::exception_handler)
                    {
                        if (saw_catch_all
                            || part.source_exception_region_id
                                != child.source_exception_region_id)
                            return fail(diagnostic,
                                verification_error::unsupported_cfg_region, 0,
                                "catch handlers must match their try and follow source order");
                        if (part.catches_all)
                            saw_catch_all = true;
                        else if (part.exception_type_id == 0
                            || !handled_type_ids.insert(part.exception_type_id).second)
                            return fail(diagnostic,
                                verification_error::unsupported_cfg_region, 0,
                                "typed catch handlers require unique nonzero exception type IDs");
                        handlers_in_order.push_back(&part);
                    }
                    else if (!body && part.kind == region_kind::lexical_scope)
                        body = &part;
                    else
                        return fail(diagnostic,
                            verification_error::unsupported_cfg_region, 0,
                            "try region contains an unsupported nested clause");
                }
                if (!body || handlers_in_order.empty())
                    return fail(diagnostic, verification_error::invalid_region_tree,
                        0, "try region could not resolve its protected body or handlers");
                for (const typed_region *handler : handlers_in_order)
                {
                    basic_block_id handler_entry = 0;
                    if (!append_cfg_region(*handler, operations, regions, current,
                            graph, next_id, &handler_entry, handlers, diagnostic))
                        return false;
                    handler_entries.push_back({handler->id, handler_entry,
                        handler->exception_type_id, handler->catches_all});
                }
                (*handlers)[child.source_exception_region_id] =
                    std::move(handler_entries);
                if (!append_cfg_region(*body, operations, regions, current,
                        graph, next_id, &child_entry, handlers, diagnostic))
                    return false;
            }
            else if (!append_cfg_region(child, operations, regions, current,
                    graph, next_id, &child_entry, handlers, diagnostic))
                return false;
            current = child_entry;
        }
        reverse -= 1;
    }
    *entry = current;
    if (region.kind == region_kind::exception_handler)
    {
        basic_block handler_entry;
        if (current == 0)
            return fail(diagnostic, verification_error::invalid_cfg_edge, 0,
                "exception handler requires a normal continuation block");
        handler_entry.id = (*next_id)++;
        handler_entry.terminator.kind = cfg_terminator_kind::handler_entry;
        handler_entry.terminator.handler_entry_region = region.id;
        handler_entry.terminator.successors.push_back({current,
            outcome_kind::normal});
        graph->blocks.push_back(handler_entry);
        *entry = handler_entry.id;
    }
    return true;
}

static bool build_structured_control_flow_graph_impl(
    const typed_function &function, control_flow_graph *result,
    lifecycle_cleanup_plan *cleanup_plan,
    verification_diagnostic *diagnostic,
    const lifetime_analysis_policy &policy)
{
    std::unordered_map<operation_id, const typed_operation *> operations;
    std::unordered_map<region_id, const typed_region *> regions;
    const typed_region *root = nullptr;
    basic_block_id next_id = 1;
    basic_block_id normal_exit = 0;
    exception_handler_entries handlers;
    bool falls_through;

    if (diagnostic)
        *diagnostic = verification_diagnostic{};
    if (!result)
        return fail(diagnostic, verification_error::invalid_cfg_block, 0,
            "structured CFG construction requires an output graph");
    *result = control_flow_graph{};
    if (!verify_typed_function(function, diagnostic))
        return false;
    for (const typed_operation &operation : function.operations)
        operations.emplace(operation.id, &operation);
    for (const typed_region &region : function.regions)
    {
        regions.emplace(region.id, &region);
        if (region.parent == 0)
            root = &region;
    }
    if (!root || root->kind != region_kind::function_body)
        return fail(diagnostic, verification_error::invalid_region_tree, 0,
            "structured CFG requires a function-body root region");
    result->source_unit_id = function.source_unit_id;
    result->function_id = function.function_id;
    falls_through = typed_region_falls_through(*root, operations, regions);
    if (falls_through)
    {
        basic_block exit;
        exit.id = next_id++;
        exit.terminator.kind = cfg_terminator_kind::normal_exit;
        normal_exit = exit.id;
        result->blocks.push_back(exit);
    }
    if (!append_cfg_region(*root, operations, regions, normal_exit, result,
            &next_id, &result->entry, &handlers, diagnostic))
    {
        *result = control_flow_graph{};
        return false;
    }
    if (!verify_control_flow_graph(function, *result, diagnostic)
        || (cleanup_plan
            ? !derive_lifecycle_cleanup_plan(function, *result,
                cleanup_plan, diagnostic, policy)
            : !verify_lifetime_transitions(function, *result, diagnostic,
                policy)))
    {
        *result = control_flow_graph{};
        return false;
    }
    return true;
}

bool build_structured_control_flow_graph(const typed_function &function,
    control_flow_graph *result, verification_diagnostic *diagnostic,
    const lifetime_analysis_policy &policy)
{
    return build_structured_control_flow_graph_impl(function, result, nullptr,
        diagnostic, policy);
}

bool build_structured_control_flow_graph_with_cleanup(
    const typed_function &function, control_flow_graph *result,
    lifecycle_cleanup_plan *cleanup_plan,
    verification_diagnostic *diagnostic,
    const lifetime_analysis_policy &policy)
{
    if (!cleanup_plan)
        return fail(diagnostic, verification_error::invalid_cfg_block, 0,
            "structured CFG cleanup lowering requires a cleanup-plan output");
    *cleanup_plan = lifecycle_cleanup_plan{};
    return build_structured_control_flow_graph_impl(function, result,
        cleanup_plan, diagnostic, policy);
}

bool verify_control_flow_graph(const typed_function &function,
    const control_flow_graph &graph, verification_diagnostic *diagnostic)
{
    std::unordered_map<basic_block_id, const basic_block *> blocks;
    std::unordered_map<value_id, const typed_value *> values;
    std::unordered_map<operation_id, const typed_operation *> operations;
    std::unordered_map<region_id, const typed_region *> regions;
    std::unordered_map<region_id, basic_block_id> control_blocks;
    std::unordered_map<region_id, basic_block_id> handler_entry_blocks;
    std::unordered_map<operation_id, basic_block_id> operation_owner;
    std::unordered_map<value_id, operation_id> value_producers;
    std::unordered_map<operation_id, std::size_t> region_order;
    std::unordered_map<basic_block_id, std::vector<basic_block_id>> successors;
    std::unordered_map<basic_block_id, std::unordered_set<basic_block_id>> predecessors;
    std::unordered_map<basic_block_id, std::unordered_set<basic_block_id>> dominators;
    std::unordered_set<basic_block_id> reachable;
    std::unordered_set<basic_block_id> dominance_roots;
    std::vector<basic_block_id> pending;
    const typed_region *root = nullptr;
    bool flat_function;

    if (diagnostic)
        *diagnostic = verification_diagnostic{};
    if (!verify_typed_function(function, diagnostic))
        return false;
    if (graph.schema_version != typed_ir_schema_version
        || graph.source_unit_id != function.source_unit_id
        || graph.function_id != function.function_id)
        return fail(diagnostic, verification_error::invalid_cfg_block, 0,
            "CFG identity and schema must match its typed function");
    for (const typed_value &value : function.values)
        values.emplace(value.id, &value);
    for (const typed_operation &operation : function.operations)
    {
        operations.emplace(operation.id, &operation);
        for (value_id result : operation.results)
            value_producers.emplace(result, operation.id);
    }
    for (const typed_region &region : function.regions)
    {
        regions.emplace(region.id, &region);
        if (region.parent == 0)
            root = &region;
    }
    if (!root || root->kind != region_kind::function_body)
        return fail(diagnostic, verification_error::invalid_region_tree, 0,
            "CFG verification requires a function-body root region");
    flat_function = function.regions.size() == 1 && root->children.empty();
    if (flat_function)
        for (std::size_t index = 0; index < root->operations.size(); ++index)
            region_order.emplace(root->operations[index], index);
    for (const basic_block &block : graph.blocks)
        if (block.id == 0 || !blocks.emplace(block.id, &block).second)
            return fail(diagnostic, verification_error::invalid_cfg_block, 0,
                "basic block IDs must be unique and nonzero");
    if (blocks.find(graph.entry) == blocks.end())
        return fail(diagnostic, verification_error::invalid_cfg_block, 0,
            "CFG entry block does not exist");

    if (flat_function && !root->operations.empty())
    {
        const operation_id first_operation = root->operations.front();
        for (const basic_block &block : graph.blocks)
            if (!block.operations.empty() && block.operations.front() == first_operation
                && graph.entry != block.id)
                return fail(diagnostic, verification_error::invalid_cfg_block,
                    first_operation, "CFG entry must contain the first operation of the function region");
    }
    else if (root->sequence.empty() && blocks.at(graph.entry)->terminator.kind
        != cfg_terminator_kind::normal_exit)
        return fail(diagnostic, verification_error::invalid_cfg_terminator, 0,
            "an empty function must enter its normal-exit block");

    for (const basic_block &block : graph.blocks)
    {
        if (block.operations.size() > 1)
            return fail(diagnostic, verification_error::invalid_cfg_block, 0,
                "current CFG normalization requires at most one operation per block");
        for (operation_id operation_id_value : block.operations)
        {
            if (operations.find(operation_id_value) == operations.end()
                || (flat_function
                    && region_order.find(operation_id_value) == region_order.end())
                || !operation_owner.emplace(operation_id_value, block.id).second)
                return fail(diagnostic, verification_error::invalid_cfg_block,
                    operation_id_value, "each CFG operation must exist and occur in exactly one block");
        }
        for (const cfg_edge &edge : block.terminator.successors)
        {
            if (blocks.find(edge.target) == blocks.end())
                return fail(diagnostic, verification_error::invalid_cfg_edge,
                    block.terminator.operation, "CFG edge targets a missing block");
            const cfg_terminator_kind target_kind = blocks.at(edge.target)->terminator.kind;
            bool invalid_edge = false;
            if (edge.outcome == outcome_kind::fatal_termination)
                invalid_edge = target_kind != cfg_terminator_kind::fatal_exit;
            else if (edge.outcome == outcome_kind::normal)
                invalid_edge = target_kind == cfg_terminator_kind::fatal_exit
                    || target_kind == cfg_terminator_kind::uncaught_exception_exit;
            else if (edge.outcome == outcome_kind::thrown_exception)
            {
                const bool explicit_throw_handler =
                    block.terminator.kind == cfg_terminator_kind::throw_value
                    && block.terminator.exception_handler_region != 0
                    && target_kind == cfg_terminator_kind::handler_entry;
                const bool invoke_exception_target =
                    block.terminator.kind == cfg_terminator_kind::invoke
                    && (target_kind == cfg_terminator_kind::uncaught_exception_exit
                        || target_kind == cfg_terminator_kind::handler_entry)
                    && edge.exception_payload == 0;
                if (block.terminator.kind == cfg_terminator_kind::throw_value)
                    invalid_edge = target_kind
                            != cfg_terminator_kind::uncaught_exception_exit
                        && !explicit_throw_handler;
                else if (block.terminator.kind == cfg_terminator_kind::invoke)
                    invalid_edge = !invoke_exception_target;
                else
                    invalid_edge = true;
                invalid_edge = invalid_edge || (block.terminator.kind
                    == cfg_terminator_kind::throw_value
                    && edge.exception_payload == 0);
            }
            else
                invalid_edge = target_kind
                    != cfg_terminator_kind::uncaught_exception_exit;
            invalid_edge = invalid_edge || (edge.outcome != outcome_kind::thrown_exception
                && edge.exception_payload != 0);
            if (invalid_edge)
                return fail(diagnostic, verification_error::invalid_cfg_edge,
                    block.terminator.operation,
                    "CFG outcome edge must target the matching normal, exceptional, or fatal continuation");
            successors[block.id].push_back(edge.target);
        }
        const cfg_terminator &terminator = block.terminator;
        const typed_operation *operation = nullptr;
        if (!block.operations.empty())
        {
            if (terminator.operation != block.operations.front())
                return fail(diagnostic, verification_error::invalid_cfg_terminator,
                    terminator.operation, "operation block terminator must name its block operation");
            operation = operations.at(terminator.operation);
        }
        else if (terminator.operation != 0)
            return fail(diagnostic, verification_error::invalid_cfg_terminator,
                terminator.operation, "synthetic exit block cannot name a source operation");

        switch (terminator.kind)
        {
            case cfg_terminator_kind::jump:
                if (terminator.condition != 0 || !operation || operation->effects.may_fail
                    || terminator.handler_entry_region != 0
                    || terminator.exception_handler_region != 0
                    || operation->operation == opcode::return_value
                    || operation->operation == opcode::throw_value
                    || terminator.successors.size() != 1
                    || terminator.successors[0].outcome != outcome_kind::normal)
                    return fail(diagnostic, verification_error::invalid_cfg_terminator,
                        terminator.operation, "jump terminator must have one normal successor after a nonterminating operation");
                break;
            case cfg_terminator_kind::branch:
            {
                const auto condition = values.find(terminator.condition);
                if (operation || terminator.handler_entry_region != 0
                    || terminator.exception_handler_region != 0
                    || terminator.condition == 0
                    || condition == values.end()
                    || condition->second->category != value_category::scalar
                    || regions.find(terminator.control_region) == regions.end()
                    || regions.at(terminator.control_region)->condition
                        != terminator.condition
                    || (regions.at(terminator.control_region)->kind
                            != region_kind::conditional
                        && regions.at(terminator.control_region)->kind
                            != region_kind::loop)
                    || terminator.successors.size() != 2
                    || terminator.successors[0].outcome != outcome_kind::normal
                    || terminator.successors[1].outcome != outcome_kind::normal)
                    return fail(diagnostic, verification_error::invalid_cfg_terminator,
                        0, "branch terminator requires a scalar condition and two normal edges");
                if (!control_blocks.emplace(terminator.control_region, block.id).second)
                    return fail(diagnostic, verification_error::invalid_cfg_terminator,
                        0, "each structured control region must have exactly one branch block");
                break;
            }
            case cfg_terminator_kind::invoke:
            {
                std::unordered_map<outcome_kind, std::size_t> declared;
                std::unordered_map<outcome_kind, std::size_t> emitted;

                if (terminator.condition != 0 || terminator.handler_entry_region != 0
                    || terminator.exception_handler_region != 0
                    || !operation || !operation->effects.may_fail
                    || terminator.successors.size() < 2)
                    return fail(diagnostic, verification_error::invalid_cfg_terminator,
                        terminator.operation, "invoke terminator requires a may-fail operation and normal/failure edges");
                for (const operation_outcome &outcome : operation->outcomes)
                    declared[outcome.kind] += 1;
                for (const cfg_edge &edge : terminator.successors)
                    emitted[edge.outcome] += 1;
                if (declared != emitted || declared[outcome_kind::normal] != 1)
                    return fail(diagnostic, verification_error::invalid_cfg_terminator,
                        terminator.operation, "invoke edges must represent every declared operation outcome exactly once");
                break;
            }
            case cfg_terminator_kind::return_value:
                if (terminator.condition != 0 || terminator.handler_entry_region != 0
                    || terminator.exception_handler_region != 0
                    || !operation || operation->operation != opcode::return_value
                    || !terminator.successors.empty())
                    return fail(diagnostic, verification_error::invalid_cfg_terminator,
                        terminator.operation, "return terminator must end a return operation block");
                break;
            case cfg_terminator_kind::throw_value:
                if (terminator.condition != 0 || !operation || operation->operation != opcode::throw_value
                    || terminator.handler_entry_region != 0
                    || terminator.successors.size() != 1
                    || operation->operands.size() != 1
                    || terminator.successors[0].outcome != outcome_kind::thrown_exception
                    || terminator.successors[0].exception_type_id
                        != operation->exception_type_id
                    || terminator.successors[0].exception_payload
                        != operation->operands.front()
                    || values.at(operation->operands.front())->type
                        != scalar_type_id(CBLC_DATA_KIND_INT)
                    || ((terminator.exception_handler_region == 0)
                        != (operation->source_exception_region_id == 0)))
                    return fail(diagnostic, verification_error::invalid_cfg_terminator,
                        terminator.operation,
                        "throw terminator must propagate exactly one typed outcome and consistently identify its handler scope");
                break;
            case cfg_terminator_kind::handler_entry:
                if (operation || terminator.condition != 0
                    || terminator.control_region != 0
                    || terminator.exception_handler_region != 0
                    || terminator.handler_entry_region == 0
                    || regions.find(terminator.handler_entry_region) == regions.end()
                    || regions.at(terminator.handler_entry_region)->kind
                        != region_kind::exception_handler
                    || terminator.successors.size() != 1
                    || terminator.successors.front().outcome != outcome_kind::normal
                    || !handler_entry_blocks.emplace(
                        terminator.handler_entry_region, block.id).second)
                    return fail(diagnostic, verification_error::invalid_cfg_terminator,
                        0, "handler-entry block must uniquely identify a handler region and continue normally");
                break;
            case cfg_terminator_kind::normal_exit:
            case cfg_terminator_kind::uncaught_exception_exit:
            case cfg_terminator_kind::fatal_exit:
                if (operation || terminator.condition != 0
                    || terminator.handler_entry_region != 0
                    || terminator.exception_handler_region != 0
                    || !terminator.successors.empty())
                    return fail(diagnostic, verification_error::invalid_cfg_terminator, 0,
                        "exit terminators cannot contain operations or successors");
                break;
        }
    }
    if (operation_owner.size() != operations.size())
        return fail(diagnostic, verification_error::invalid_cfg_block, 0,
            "every typed operation must appear in exactly one basic block");
    for (const typed_region &region : function.regions)
        if ((region.kind == region_kind::conditional || region.kind == region_kind::loop)
            != (control_blocks.find(region.id) != control_blocks.end()))
            return fail(diagnostic, verification_error::invalid_cfg_terminator, 0,
                "every conditional or loop region must map to exactly one CFG branch block");
    for (const typed_region &region : function.regions)
        if ((region.kind == region_kind::exception_handler)
                != (handler_entry_blocks.find(region.id)
                    != handler_entry_blocks.end()))
            return fail(diagnostic, verification_error::invalid_cfg_terminator, 0,
                "every exception handler must have exactly one explicit CFG entry block");

    std::function<basic_block_id(region_id)> region_entry;
    std::function<basic_block_id(const typed_region::item &)> item_entry;
    region_entry = [&](region_id region_id_value) -> basic_block_id
    {
        const typed_region &region = *regions.at(region_id_value);
        if (region.kind == region_kind::exception_handler)
            return handler_entry_blocks.at(region.id);
        if (!region.condition_operations.empty())
            return operation_owner.at(region.condition_operations.front());
        if (region.sequence.empty())
            return 0;
        return item_entry(region.sequence.front());
    };
    item_entry = [&](const typed_region::item &item) -> basic_block_id
    {
        if (item.item_kind == typed_region::item::kind::operation)
            return operation_owner.at(static_cast<operation_id>(item.id));
        const typed_region &child = *regions.at(static_cast<region_id>(item.id));
        if (child.kind == region_kind::conditional
            || (child.kind == region_kind::loop
                && child.condition_operations.empty()))
            return control_blocks.at(child.id);
        return region_entry(child.id);
    };
    for (const auto &handler_entry : handler_entry_blocks)
    {
        const typed_region &handler = *regions.at(handler_entry.first);
        const basic_block &entry_block = *blocks.at(handler_entry.second);
        if (!handler.sequence.empty()
            && entry_block.terminator.successors.front().target
                != item_entry(handler.sequence.front()))
            return fail(diagnostic, verification_error::invalid_cfg_edge, 0,
                "handler-entry continuation must enter the first binding or source operation");
    }
    for (const basic_block &block : graph.blocks)
        if (block.terminator.kind == cfg_terminator_kind::throw_value
            && block.terminator.exception_handler_region != 0)
        {
            const typed_operation *operation = operations.at(
                block.terminator.operation);
            const auto handler = regions.find(
                block.terminator.exception_handler_region);
            const typed_region *try_region = nullptr;
            region_id expected_handler = 0;
            for (const typed_region &candidate : function.regions)
                if (candidate.kind == region_kind::exception_try
                    && candidate.source_exception_region_id
                        == operation->source_exception_region_id)
                {
                    try_region = &candidate;
                    break;
                }
            if (try_region)
                for (region_id handler_id : try_region->children)
                {
                    const typed_region *candidate = regions.at(handler_id);
                    if (candidate->kind == region_kind::exception_handler
                        && (candidate->catches_all
                            || candidate->exception_type_id
                                == operation->exception_type_id))
                    {
                        expected_handler = candidate->id;
                        break;
                    }
                }
            if (handler == regions.end()
                || handler->second->kind != region_kind::exception_handler
                || handler->second->source_exception_region_id
                    != operation->source_exception_region_id
                || handler->first != expected_handler
                || block.terminator.successors.front().target
                    != handler_entry_blocks.at(handler->first)
                || block.terminator.successors.front().exception_payload
                    != operation->operands.front())
                return fail(diagnostic, verification_error::invalid_cfg_edge,
                    operation->id,
                    "thrown exception must enter the first matching typed or catch-all handler");
        }
    for (const basic_block &block : graph.blocks)
        if (block.terminator.kind == cfg_terminator_kind::invoke)
        {
            const typed_operation *operation = operations.at(
                block.terminator.operation);
            for (const cfg_edge &edge : block.terminator.successors)
            {
                if (edge.outcome != outcome_kind::thrown_exception)
                    continue;
                const typed_region *try_region = nullptr;
                if (operation->source_exception_region_id != 0)
                    for (const typed_region &candidate : function.regions)
                        if (candidate.kind == region_kind::exception_try
                            && candidate.source_exception_region_id
                                == operation->source_exception_region_id)
                        {
                            try_region = &candidate;
                            break;
                        }
                const typed_region *expected_handler = nullptr;
                if (try_region)
                    for (region_id handler_id : try_region->children)
                    {
                        const typed_region *candidate = regions.at(handler_id);
                        if (candidate->kind == region_kind::exception_handler
                            && (candidate->catches_all
                                || (edge.exception_type_id != 0
                                    && candidate->exception_type_id
                                        == edge.exception_type_id)))
                        {
                            expected_handler = candidate;
                            break;
                        }
                    }
                const basic_block &target = *blocks.at(edge.target);
                if (edge.exception_payload != 0
                    || (expected_handler
                        ? (target.terminator.kind
                                != cfg_terminator_kind::handler_entry
                            || target.terminator.handler_entry_region
                                != expected_handler->id
                            || edge.target
                                != handler_entry_blocks.at(expected_handler->id))
                        : target.terminator.kind
                            != cfg_terminator_kind::uncaught_exception_exit))
                    return fail(diagnostic, verification_error::invalid_cfg_edge,
                        operation->id,
                        "invoke exception must enter the first matching typed or catch-all handler, or the uncaught exit");
            }
        }
    if (root->sequence.empty())
    {
        if (blocks.at(graph.entry)->terminator.kind != cfg_terminator_kind::normal_exit)
            return fail(diagnostic, verification_error::invalid_cfg_block, 0,
                "empty function must enter its normal exit");
    }
    else if (region_entry(root->id) != graph.entry)
        return fail(diagnostic, verification_error::invalid_cfg_block, 0,
            "CFG entry must enter the first operation or control region in source order");

    for (const typed_region &region : function.regions)
    {
        for (std::size_t condition_index = 0;
            condition_index < region.condition_operations.size(); ++condition_index)
        {
            const operation_id operation_id_value =
                region.condition_operations[condition_index];
            const basic_block &block = *blocks.at(
                operation_owner.at(operation_id_value));
            const basic_block_id expected = condition_index + 1
                < region.condition_operations.size()
                ? operation_owner.at(region.condition_operations[condition_index + 1])
                : control_blocks.at(region.id);
            basic_block_id normal_target = 0;
            for (const cfg_edge &edge : block.terminator.successors)
                if (edge.outcome == outcome_kind::normal)
                    normal_target = edge.target;
            if (normal_target != expected)
                return fail(diagnostic, verification_error::invalid_cfg_edge,
                    operation_id_value,
                    "loop condition operations must execute in order immediately before the branch test");
        }
        std::size_t item_index = 0;
        while (item_index < region.sequence.size())
        {
            const typed_region::item &item = region.sequence[item_index];
            if (item.item_kind == typed_region::item::kind::operation
                && item_index + 1 < region.sequence.size())
            {
                const operation_id operation_id_value =
                    static_cast<operation_id>(item.id);
                const typed_operation &operation = *operations.at(operation_id_value);
                const basic_block &block = *blocks.at(operation_owner.at(operation_id_value));
                basic_block_id expected = 0;
                std::size_t next_index = item_index + 1;
                while (next_index < region.sequence.size() && expected == 0)
                    expected = item_entry(region.sequence[next_index++]);
                if (expected != 0)
                {
                    basic_block_id normal_target = 0;
                    if (operation.operation == opcode::return_value
                        || operation.operation == opcode::throw_value)
                        return fail(diagnostic, verification_error::invalid_cfg_edge,
                            operation.id,
                            "terminating operation cannot precede another reachable region item");
                    for (const cfg_edge &edge : block.terminator.successors)
                        if (edge.outcome == outcome_kind::normal)
                            normal_target = edge.target;
                    if (normal_target != expected)
                        return fail(diagnostic, verification_error::invalid_cfg_edge,
                            operation.id,
                            "normal operation continuation must enter the next region item");
                }
            }
            if (item.item_kind == typed_region::item::kind::child_region)
            {
                const typed_region &child = *regions.at(
                    static_cast<region_id>(item.id));
                if (child.kind == region_kind::conditional)
                {
                    const basic_block &branch = *blocks.at(control_blocks.at(child.id));
                    for (std::size_t arm = 0; arm < child.children.size(); ++arm)
                    {
                        const basic_block_id expected = region_entry(child.children[arm]);
                        if (expected != 0
                            && branch.terminator.successors[arm].target != expected)
                            return fail(diagnostic, verification_error::invalid_cfg_edge,
                                0, "conditional successor must enter its corresponding then/else region");
                    }
                }
                else if (child.kind == region_kind::loop)
                {
                    const basic_block_id expected = region_entry(child.children[0]);
                    if (expected != 0
                        && blocks.at(control_blocks.at(child.id))->terminator
                            .successors[0].target != expected)
                        return fail(diagnostic, verification_error::invalid_cfg_edge,
                            0, "loop true edge must enter its body region");
                }
            }
            item_index += 1;
        }
    }

    /* The flat adapter has no structured region graph to validate, so preserve
     * its exact source sequencing here. Structured functions are validated by
     * explicit region/branch links and graph reachability below. */
    for (std::size_t index = 0; flat_function
        && index < root->operations.size(); ++index)
    {
        const operation_id operation_id_value = root->operations[index];
        const basic_block_id owner_id = operation_owner.at(operation_id_value);
        const basic_block &owner = *blocks.at(owner_id);
        const typed_operation &operation = *operations.at(operation_id_value);
        const bool has_next = index + 1
            < root->operations.size();
        const operation_id next_operation = has_next
            ? root->operations[index + 1] : 0;
        const basic_block_id next_block = has_next
            ? operation_owner.at(next_operation) : 0;

        if (operation.operation == opcode::return_value
            || operation.operation == opcode::throw_value)
            continue;
        if (operation.effects.may_fail)
        {
            std::size_t normal_count = 0;
            for (const cfg_edge &edge : owner.terminator.successors)
                if (edge.outcome == outcome_kind::normal)
                {
                    normal_count += 1;
                    if ((!has_next
                            && blocks.at(edge.target)->terminator.kind
                                != cfg_terminator_kind::normal_exit)
                        || (has_next && edge.target != next_block))
                        return fail(diagnostic,
                            verification_error::invalid_cfg_edge,
                            operation.id,
                            "invoke normal edge must continue at the next source operation or function exit");
                }
            if (normal_count != 1)
                return fail(diagnostic, verification_error::invalid_cfg_edge,
                    operation.id,
                    "invoke must have exactly one normal continuation");
        }
        else
        {
            if (owner.terminator.successors.size() != 1
                || owner.terminator.successors.front().outcome
                    != outcome_kind::normal
                || (has_next && owner.terminator.successors.front().target
                    != next_block)
                || (!has_next
                    && blocks.at(owner.terminator.successors.front().target)
                        ->terminator.kind != cfg_terminator_kind::normal_exit))
                return fail(diagnostic, verification_error::invalid_cfg_edge,
                    operation.id,
                    "linear operation must continue at the next source operation or function exit");
        }
    }
    pending.push_back(graph.entry);
    while (!pending.empty())
    {
        const basic_block_id current = pending.back();
        pending.pop_back();
        if (!reachable.insert(current).second)
            continue;
        for (basic_block_id successor : successors[current])
            pending.push_back(successor);
    }
    dominance_roots.insert(graph.entry);
    for (const auto &handler_entry : handler_entry_blocks)
        if (reachable.find(handler_entry.second) == reachable.end())
        {
            dominance_roots.insert(handler_entry.second);
            pending.push_back(handler_entry.second);
            while (!pending.empty())
            {
                const basic_block_id current = pending.back();
                pending.pop_back();
                if (!reachable.insert(current).second)
                    continue;
                for (basic_block_id successor : successors[current])
                    pending.push_back(successor);
            }
        }
    if (reachable.size() != blocks.size())
        return fail(diagnostic, verification_error::unreachable_cfg_block, 0,
            "every emitted basic block must be reachable from function flow or a source handler entry");

    for (const auto &entry : successors)
        for (basic_block_id successor : entry.second)
            predecessors[successor].insert(entry.first);
    for (basic_block_id block : reachable)
        dominators[block] = dominance_roots.find(block) != dominance_roots.end()
            ? std::unordered_set<basic_block_id>{block} : reachable;
    {
        bool changed = true;
        while (changed)
        {
            changed = false;
            for (basic_block_id block : reachable)
            {
                if (dominance_roots.find(block) != dominance_roots.end())
                    continue;
                const auto pred = predecessors.find(block);
                if (pred == predecessors.end() || pred->second.empty())
                    return fail(diagnostic, verification_error::invalid_cfg_edge, 0,
                        "reachable non-entry block must have an incoming edge");
                auto predecessor = pred->second.begin();
                std::unordered_set<basic_block_id> common = dominators.at(*predecessor);
                ++predecessor;
                while (predecessor != pred->second.end())
                {
                    for (auto candidate = common.begin(); candidate != common.end();)
                        if (dominators.at(*predecessor).find(*candidate)
                            == dominators.at(*predecessor).end())
                            candidate = common.erase(candidate);
                        else
                            ++candidate;
                    ++predecessor;
                }
                common.insert(block);
                if (common != dominators.at(block))
                {
                    dominators[block] = std::move(common);
                    changed = true;
                }
            }
        }
    }
    for (const typed_operation &operation : function.operations)
    {
        const basic_block_id use_block = operation_owner.at(operation.id);
        for (value_id operand : operation.operands)
        {
            const auto producer = value_producers.find(operand);
            if (producer != value_producers.end())
            {
                const basic_block_id producer_block = operation_owner.at(producer->second);
                if (producer_block == use_block
                    || dominators.at(use_block).find(producer_block)
                        == dominators.at(use_block).end())
                    return fail(diagnostic, verification_error::use_before_definition,
                        operation.id,
                        "value-producing operation must dominate every CFG operand use");
            }
        }
        for (operation_id predecessor : operation.must_follow)
        {
            const basic_block_id predecessor_block = operation_owner.at(predecessor);
            if (predecessor_block == use_block
                || dominators.at(use_block).find(predecessor_block)
                    == dominators.at(use_block).end())
                return fail(diagnostic, verification_error::use_before_definition,
                    operation.id,
                    "required evaluation predecessor must dominate its CFG operation");
        }
    }
    for (const basic_block &block : graph.blocks)
        if (block.terminator.kind == cfg_terminator_kind::branch)
        {
            const auto producer = value_producers.find(block.terminator.condition);
            if (producer != value_producers.end())
            {
                const basic_block_id producer_block = operation_owner.at(producer->second);
                if (producer_block == block.id
                    || dominators.at(block.id).find(producer_block)
                        == dominators.at(block.id).end())
                    return fail(diagnostic, verification_error::use_before_definition,
                        producer->second,
                        "branch condition definition must dominate its branch block");
            }
        }
    return true;
}

static bool analyze_lifetime_transitions(const typed_function &function,
    const control_flow_graph &graph, verification_diagnostic *diagnostic,
    lifecycle_cleanup_plan *cleanup_plan,
    const lifetime_analysis_policy &policy)
{
    enum class lifetime_state { uninitialized, live, moved_from, destroyed };
    using lifetime_path = std::pair<std::vector<lifetime_state>,
        std::vector<value_id>>;
    std::unordered_map<basic_block_id, const basic_block *> blocks;
    std::unordered_map<operation_id, const typed_operation *> operations;
    std::unordered_map<place_id, const typed_place *> places;
    std::unordered_map<value_id, std::size_t> tracked_values;
    std::unordered_map<basic_block_id, std::set<lifetime_path>> incoming;
    std::set<std::pair<basic_block_id, lifetime_path>> pending;
    std::size_t edge_transfer_count = 0;
    std::function<bool(const typed_operation &, const operation_outcome &,
        std::vector<lifetime_state> &, std::vector<value_id> &)> apply_outcome;
    auto describe_value = [&](value_id value)
    {
        for (const typed_place &place : function.places)
        {
            if (place.base_value != value || place.symbol == 0)
                continue;
            const auto symbol = std::find_if(function.symbols.begin(),
                function.symbols.end(), [&](const typed_symbol &candidate)
                {
                    return candidate.id == place.symbol;
                });
            if (symbol != function.symbols.end())
                return symbol->canonical_name;
        }
        return std::string("<temporary>");
    };

    if (diagnostic)
        *diagnostic = verification_diagnostic{};
    if (!verify_control_flow_graph(function, graph, diagnostic))
        return false;
    for (const typed_value &value : function.values)
        if (value.category == value_category::owned_object)
            tracked_values.emplace(value.id, tracked_values.size());
    for (const typed_place &place : function.places)
        places.emplace(place.id, &place);
    for (const typed_operation &operation : function.operations)
        operations.emplace(operation.id, &operation);
    for (const basic_block &block : graph.blocks)
        blocks.emplace(block.id, &block);
    std::vector<lifetime_state> initial(tracked_values.size(),
        lifetime_state::uninitialized);
    std::vector<value_id> entry_construction_order;
    entry_construction_order.reserve(function.entry_live_values.size());
    for (value_id value : function.entry_live_values)
    {
        const auto tracked = tracked_values.find(value);
        if (tracked == tracked_values.end())
            return fail(diagnostic,
                verification_error::invalid_lifetime_transition, 0,
                "entry-live value is missing from the owned-object lifetime map");
        initial[tracked->second] = lifetime_state::live;
        entry_construction_order.push_back(value);
    }
    lifetime_path initial_path{std::move(initial),
        std::move(entry_construction_order)};
    incoming[graph.entry].insert(initial_path);
    pending.insert({graph.entry, std::move(initial_path)});
    apply_outcome = [&](const typed_operation &operation,
        const operation_outcome &outcome, std::vector<lifetime_state> &state,
        std::vector<value_id> &construction_order)
    {
        std::string transition_failure;
        for (value_id value : outcome.initialized)
            if (tracked_values.find(value) != tracked_values.end())
                return fail(diagnostic,
                    verification_error::invalid_lifetime_transition,
                    operation.id,
                    "owned objects require lifecycle states, not scalar initialized");
        auto transition = [&](const std::vector<value_id> &values,
            lifetime_state desired) -> bool
        {
            for (value_id value : values)
            {
                const auto tracked = tracked_values.find(value);
                if (tracked == tracked_values.end())
                    continue;
                lifetime_state &prior = state[tracked->second];
                if (desired == lifetime_state::live)
                {
                    if ((prior != lifetime_state::uninitialized
                            && prior != lifetime_state::destroyed)
                        || (!operation.effects.constructs
                            && operation.operation != opcode::call))
                        return false;
                    construction_order.push_back(value);
                }
                else if (desired == lifetime_state::moved_from)
                {
                    if (prior != lifetime_state::live
                        || (operation.operation != opcode::move_construct
                            && operation.operation != opcode::return_value
                            && operation.operation != opcode::call
                            && operation.operation != opcode::move_assign))
                        return false;
                }
                else if (desired == lifetime_state::destroyed)
                {
                    const bool constructor_failure_cleanup =
                        (operation.operation == opcode::construct
                            || operation.operation == opcode::copy_construct
                            || operation.operation == opcode::move_construct)
                        && outcome.kind == outcome_kind::thrown_exception
                        && operation.effects.destroys;
                    if (operation.operation == opcode::scope_exit
                        && (prior == lifetime_state::uninitialized
                            || prior == lifetime_state::destroyed))
                        continue;
                    if ((prior != lifetime_state::live
                            && prior != lifetime_state::moved_from)
                        || (operation.operation != opcode::destroy
                            && operation.operation != opcode::scope_exit
                            && !constructor_failure_cleanup)
                        || construction_order.empty()
                        || construction_order.back() != value)
                    {
                        transition_failure = "destruction of value "
                            + std::to_string(value)
                            + " (" + describe_value(value) + ")"
                            + " does not match the live construction stack (top="
                            + std::to_string(construction_order.empty()
                                ? 0 : construction_order.back())
                            + ", top-name=" + (construction_order.empty()
                                ? std::string("<empty>")
                                : describe_value(construction_order.back()))
                            + ", state=" + std::to_string(static_cast<int>(prior))
                            + ", opcode="
                            + std::to_string(static_cast<int>(operation.operation))
                            + ")";
                        return false;
                    }
                    construction_order.pop_back();
                }
                else if (desired == lifetime_state::uninitialized
                    && prior != lifetime_state::uninitialized
                    && prior != lifetime_state::destroyed)
                    return false;
                prior = desired;
            }
            return true;
        };
        if (!transition(outcome.live, lifetime_state::live))
            return fail(diagnostic,
                verification_error::invalid_lifetime_transition, operation.id,
                "live outcome requires a fresh object and a construction operation");
        if (!transition(outcome.moved_from, lifetime_state::moved_from))
            return fail(diagnostic,
                verification_error::invalid_lifetime_transition, operation.id,
                "moved-from outcome requires a live object and a move operation");
        if (!transition(outcome.uninitialized, lifetime_state::uninitialized))
            return fail(diagnostic,
                verification_error::invalid_lifetime_transition, operation.id,
                "failed construction may only leave its result uninitialized");
        if (!transition(outcome.destroyed, lifetime_state::destroyed))
            return fail(diagnostic,
                verification_error::invalid_lifetime_transition, operation.id,
                transition_failure.empty()
                    ? "destruction outcome must end the most recently completed live object"
                    : transition_failure.c_str());
        return true;
    };
    auto normalize_edge_state = [](std::vector<lifetime_state> &state)
    {
        /*
         * A dead automatic object has the same state at a CFG join whether
         * its declaration was skipped or its prior iteration/scope destroyed
         * it. Keep the distinction within a block (where it diagnoses
         * use-after-destroy), but canonicalize it when control leaves that
         * block so loop back-edges and conditional scopes can re-enter.
         */
        for (lifetime_state &value_state : state)
            if (value_state == lifetime_state::destroyed)
                value_state = lifetime_state::uninitialized;
    };
    auto record_exit = [&](basic_block_id block_id, cfg_terminator_kind exit_kind,
        const std::vector<lifetime_state> &state,
        const std::vector<value_id> &construction_order,
        operation_id operation) -> bool
    {
        std::vector<value_id> destruction_order;
        std::unordered_set<value_id> ordered_values;
        std::size_t reverse = construction_order.size();

        while (reverse > 0)
        {
            const value_id value = construction_order[--reverse];
            const auto tracked = tracked_values.find(value);
            if (tracked == tracked_values.end()
                || (state[tracked->second] != lifetime_state::live
                    && state[tracked->second] != lifetime_state::moved_from))
                return fail(diagnostic,
                    verification_error::invalid_lifetime_transition, operation,
                    "construction order contains an object that is not live at the exit");
            destruction_order.push_back(value);
            ordered_values.insert(value);
        }
        for (const auto &tracked : tracked_values)
            if ((state[tracked.second] == lifetime_state::live
                    || state[tracked.second] == lifetime_state::moved_from)
                && ordered_values.find(tracked.first) == ordered_values.end())
                return fail(diagnostic,
                    verification_error::invalid_lifetime_transition, operation,
                    "live object is missing from the completed-construction order");
        if (!cleanup_plan && !destruction_order.empty())
            return fail(diagnostic, verification_error::live_object_at_exit,
                operation,
                "normal and uncaught-exception exits require every owned object to be destroyed or uninitialized");
        if (cleanup_plan)
            cleanup_plan->exits.push_back({block_id, exit_kind,
                std::move(destruction_order)});
        return true;
    };

    while (!pending.empty())
    {
        const basic_block_id block_id = pending.begin()->first;
        lifetime_path path = pending.begin()->second;
        pending.erase(pending.begin());
        const basic_block &block = *blocks.at(block_id);
        std::vector<lifetime_state> state = std::move(path.first);
        std::vector<value_id> construction_order = std::move(path.second);
        const typed_operation *operation = block.operations.empty()
            ? nullptr : operations.at(block.operations.front());

        if (operation)
        {
            if (operation->operation == opcode::copy_assign
                || operation->operation == opcode::move_assign)
            {
                const typed_place &target_place = *places.at(operation->places.front());
                const auto target = tracked_values.find(target_place.base_value);
                const value_id source_id = operation->operands.front();
                const auto source = tracked_values.find(source_id);
                const auto source_value = std::find_if(function.values.begin(),
                    function.values.end(), [&](const typed_value &value)
                    {
                        return value.id == source_id;
                    });
                const auto target_value = std::find_if(function.values.begin(),
                    function.values.end(), [&](const typed_value &value)
                    {
                        return value.id == target_place.base_value;
                    });
                if (target_value == function.values.end())
                    return fail(diagnostic,
                        verification_error::dangling_reference,
                        operation->id,
                        "assignment target has no canonical value identity");
                lifetime_state source_state = source == tracked_values.end()
                    ? lifetime_state::uninitialized : state[source->second];
                if (source_value != function.values.end()
                    && source_value->category == value_category::borrow
                    && source_value->lifetime_token != 0)
                {
                    for (const typed_value &owner_value : function.values)
                    {
                        if (owner_value.category != value_category::owned_object
                            || owner_value.lifetime_token
                                != source_value->lifetime_token)
                            continue;
                        const auto owner = tracked_values.find(owner_value.id);
                        if (owner != tracked_values.end()
                            && (state[owner->second] == lifetime_state::live
                                || state[owner->second]
                                    == lifetime_state::moved_from))
                        {
                            source_state = lifetime_state::live;
                            break;
                        }
                    }
                }
                const bool target_is_live = target_value->category
                        == value_category::borrow
                    ? target_value->lifetime_token != 0
                        && target_place.owner_lifetime_token
                            == target_value->lifetime_token
                    : target != tracked_values.end()
                        && (state[target->second] == lifetime_state::live
                            || state[target->second] == lifetime_state::moved_from);
                if (!target_is_live
                    || (source_state != lifetime_state::live
                        && source_state != lifetime_state::moved_from))
                    return fail(diagnostic,
                        verification_error::use_after_lifetime_end,
                        operation->id,
                        "copy/move assignment requires a live source and target; borrowed copy sources must have a live owner");
            }
            if (operation->operation == opcode::destroy)
            {
                bool has_owned_target = false;
                for (const operation_outcome &outcome : operation->outcomes)
                    for (value_id value : outcome.destroyed)
                    {
                        const auto tracked = tracked_values.find(value);
                        if (tracked == tracked_values.end())
                            continue;
                        has_owned_target = true;
                        const lifetime_state prior = state[tracked->second];
                        if (prior != lifetime_state::live
                            && prior != lifetime_state::moved_from)
                            return fail(diagnostic,
                                verification_error::invalid_lifetime_transition,
                                operation->id,
                                "destroy requires an object that is live or moved-from on entry");
                    }
                if (!has_owned_target)
                    return fail(diagnostic, verification_error::invalid_lifetime_transition,
                        operation->id,
                        "destroy must explicitly end the lifetime of an owned object");
            }
            for (place_id place_id_value : operation->effects.reads)
            {
                const typed_place *place = places.at(place_id_value);
                const auto tracked = tracked_values.find(place->base_value);
                if (tracked != tracked_values.end()
                    && (state[tracked->second] == lifetime_state::uninitialized
                        || state[tracked->second] == lifetime_state::destroyed)
                    && operation->operation != opcode::construct)
                {
                    const std::string message = "operation reads owned object "
                        + std::to_string(place->base_value) + " ("
                        + describe_value(place->base_value)
                        + ") outside its live lifetime";
                    return fail(diagnostic,
                        verification_error::use_after_lifetime_end,
                        operation->id, message.c_str());
                }
            }
            for (place_id place_id_value : operation->effects.writes)
            {
                const typed_place *place = places.at(place_id_value);
                const auto tracked = tracked_values.find(place->base_value);
                if (tracked != tracked_values.end()
                    && (state[tracked->second] == lifetime_state::uninitialized
                        || state[tracked->second] == lifetime_state::destroyed)
                    && operation->operation != opcode::construct
                    && operation->operation != opcode::copy_construct
                    && operation->operation != opcode::move_construct)
                    return fail(diagnostic, verification_error::use_after_lifetime_end,
                        operation->id,
                        "operation writes an owned object outside its live lifetime");
            }
        }

        if ((block.terminator.kind == cfg_terminator_kind::normal_exit
                || block.terminator.kind == cfg_terminator_kind::uncaught_exception_exit)
            && !record_exit(block.id, block.terminator.kind, state,
                construction_order, operation ? operation->id : 0))
            return false;

        std::vector<const cfg_edge *> ordered_edges;

        ordered_edges.reserve(block.terminator.successors.size());
        for (const cfg_edge &edge : block.terminator.successors)
            ordered_edges.push_back(&edge);
        std::sort(ordered_edges.begin(), ordered_edges.end(),
            [](const cfg_edge *left, const cfg_edge *right)
            {
                if (left->target != right->target)
                    return left->target < right->target;
                if (left->outcome != right->outcome)
                    return static_cast<int>(left->outcome)
                        < static_cast<int>(right->outcome);
                if (left->exception_type_id != right->exception_type_id)
                    return left->exception_type_id < right->exception_type_id;
                return left->exception_payload < right->exception_payload;
            });
        for (const cfg_edge *edge_pointer : ordered_edges)
        {
            const cfg_edge &edge = *edge_pointer;

            if (edge_transfer_count
                    == std::numeric_limits<std::size_t>::max()
                || edge_transfer_count >= policy.max_edge_transfers)
            {
                const std::string observed = edge_transfer_count
                        == std::numeric_limits<std::size_t>::max()
                    ? "counter_overflow"
                    : std::to_string(edge_transfer_count + 1);
                const std::string message =
                    "resource_limit_exceeded phase=lifetime-dataflow dimension=cfg_edge_transfers configured_limit="
                    + std::to_string(policy.max_edge_transfers)
                    + " observed=" + observed;

                return fail(diagnostic,
                    verification_error::resource_limit_exceeded,
                    operation ? operation->id : 0, message.c_str());
            }
            ++edge_transfer_count;
            std::vector<lifetime_state> edge_state = state;
            if (operation)
            {
                const auto outcome = std::find_if(operation->outcomes.begin(),
                    operation->outcomes.end(), [&](const operation_outcome &candidate)
                    {
                        return candidate.kind == edge.outcome;
                    });
                if (outcome == operation->outcomes.end())
                    return fail(diagnostic, verification_error::invalid_lifetime_transition,
                        operation->id,
                        "CFG edge has no corresponding lifetime outcome");
                std::vector<value_id> edge_construction_order = construction_order;
                if (!apply_outcome(*operation, *outcome, edge_state,
                        edge_construction_order))
                    return false;
                if (cleanup_plan
                    && edge.outcome != outcome_kind::normal
                    && edge.outcome != outcome_kind::fatal_termination)
                {
                    std::vector<value_id> edge_destruction_order =
                        outcome->destroyed;
                    if (edge.outcome == outcome_kind::thrown_exception
                        && operation->operation == opcode::throw_value
                        && !operation->provenance.empty())
                    {
                        const std::uint64_t throw_source_offset =
                            operation->provenance.front().source_offset;
                        for (const typed_operation &cleanup_operation :
                            function.operations)
                        {
                            if (cleanup_operation.operation != opcode::scope_exit
                                || cleanup_operation.provenance.empty()
                                || cleanup_operation.provenance.front().role
                                    != "exception-scope-exit-cleanup"
                                || cleanup_operation.provenance.front().source_offset
                                    != throw_source_offset)
                                continue;
                            for (const operation_outcome &cleanup_outcome :
                                cleanup_operation.outcomes)
                                if (cleanup_outcome.kind == outcome_kind::normal)
                                    edge_destruction_order.insert(
                                        edge_destruction_order.end(),
                                        cleanup_outcome.destroyed.begin(),
                                        cleanup_outcome.destroyed.end());
                        }
                    }
                    const auto target = blocks.find(edge.target);
                    if (edge.outcome != outcome_kind::normal
                        && edge.outcome != outcome_kind::fatal_termination
                        && target != blocks.end()
                        && target->second->terminator.kind
                            == cfg_terminator_kind::uncaught_exception_exit)
                    {
                        std::size_t completed = edge_construction_order.size();
                        while (completed > 0)
                        {
                            const value_id value =
                                edge_construction_order[--completed];
                            const auto tracked = tracked_values.find(value);
                            if (tracked == tracked_values.end()
                                || (edge_state[tracked->second]
                                        != lifetime_state::live
                                    && edge_state[tracked->second]
                                        != lifetime_state::moved_from))
                                continue;
                            if (std::find(edge_destruction_order.begin(),
                                    edge_destruction_order.end(), value)
                                == edge_destruction_order.end())
                                edge_destruction_order.push_back(value);
                        }
                    }
                    cleanup_plan->failure_edges.push_back({block_id,
                        edge.target, operation->id, edge.outcome,
                        std::move(edge_destruction_order)});
                }
                normalize_edge_state(edge_state);
                lifetime_path edge_path{std::move(edge_state),
                    std::move(edge_construction_order)};
                if (incoming[edge.target].insert(edge_path).second)
                    pending.insert({edge.target, std::move(edge_path)});
            }
            else
            {
                normalize_edge_state(edge_state);
                lifetime_path edge_path{std::move(edge_state), construction_order};
                if (incoming[edge.target].insert(edge_path).second)
                    pending.insert({edge.target, std::move(edge_path)});
            }
        }

        if (operation && block.terminator.kind == cfg_terminator_kind::return_value)
        {
            const outcome_kind completion = block.terminator.kind
                == cfg_terminator_kind::return_value
                ? outcome_kind::normal : outcome_kind::thrown_exception;
            const auto outcome = std::find_if(operation->outcomes.begin(),
                operation->outcomes.end(), [&](const operation_outcome &candidate)
                {
                    return candidate.kind == completion;
                });
            if (outcome == operation->outcomes.end())
                return fail(diagnostic, verification_error::invalid_lifetime_transition,
                    operation->id, "terminating operation lacks its lifetime outcome");
            if (!apply_outcome(*operation, *outcome, state, construction_order))
                return false;
            if (!record_exit(block.id, block.terminator.kind, state,
                    construction_order, operation->id))
                return false;
        }
    }
    if (cleanup_plan)
    {
        std::sort(cleanup_plan->exits.begin(), cleanup_plan->exits.end(),
            [](const cleanup_site &left, const cleanup_site &right)
            {
                if (left.exit_block != right.exit_block)
                    return left.exit_block < right.exit_block;
                if (left.exit_kind != right.exit_kind)
                    return static_cast<int>(left.exit_kind)
                        < static_cast<int>(right.exit_kind);
                return left.destruction_order < right.destruction_order;
            });
        std::sort(cleanup_plan->failure_edges.begin(),
            cleanup_plan->failure_edges.end(),
            [](const cleanup_edge &left, const cleanup_edge &right)
            {
                if (left.source_block != right.source_block)
                    return left.source_block < right.source_block;
                if (left.operation != right.operation)
                    return left.operation < right.operation;
                if (left.target_block != right.target_block)
                    return left.target_block < right.target_block;
                if (left.outcome != right.outcome)
                    return static_cast<int>(left.outcome)
                        < static_cast<int>(right.outcome);
                return left.destruction_order < right.destruction_order;
            });
        cleanup_plan->exits.erase(std::unique(cleanup_plan->exits.begin(),
            cleanup_plan->exits.end(), [](const cleanup_site &left,
                const cleanup_site &right)
            {
                return left.exit_block == right.exit_block
                    && left.exit_kind == right.exit_kind
                    && left.destruction_order == right.destruction_order;
            }), cleanup_plan->exits.end());
        cleanup_plan->failure_edges.erase(std::unique(
            cleanup_plan->failure_edges.begin(), cleanup_plan->failure_edges.end(),
            [](const cleanup_edge &left, const cleanup_edge &right)
            {
                return left.source_block == right.source_block
                    && left.target_block == right.target_block
                    && left.operation == right.operation
                    && left.outcome == right.outcome
                    && left.destruction_order == right.destruction_order;
            }), cleanup_plan->failure_edges.end());
    }
    return true;
}

bool verify_lifetime_transitions(const typed_function &function,
    const control_flow_graph &graph, verification_diagnostic *diagnostic,
    const lifetime_analysis_policy &policy)
{
    return analyze_lifetime_transitions(function, graph, diagnostic, nullptr,
        policy);
}

bool derive_lifecycle_cleanup_plan(const typed_function &function,
    const control_flow_graph &graph, lifecycle_cleanup_plan *result,
    verification_diagnostic *diagnostic,
    const lifetime_analysis_policy &policy)
{
    lifecycle_cleanup_plan plan;

    if (!result)
        return fail(diagnostic, verification_error::invalid_cfg_block, 0,
            "cleanup plan output is required");
    plan.source_unit_id = function.source_unit_id;
    plan.function_id = function.function_id;
    if (!analyze_lifetime_transitions(function, graph, diagnostic, &plan,
            policy))
        return false;
    *result = std::move(plan);
    return true;
}

}
