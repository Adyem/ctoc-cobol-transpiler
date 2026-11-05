static void cobol_text_builder_init(t_cobol_text_builder *builder)
{
    if (!builder)
        return ;
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
    builder->free_format = 0;
    builder->control_flow_depth = 0;
    builder->based_name_slots = NULL;
    builder->based_name_capacity = 0;
    builder->based_name_count = 0;
    builder->deferred_procedure_text = NULL;
    builder->deferred_procedure_length = 0;
    builder->deferred_procedure_capacity = 0;
    builder->emitted_method_paragraphs = NULL;
    builder->method_paragraph_return_targets = NULL;
    builder->known_paragraph_definitions = NULL;
    builder->lifecycle_constructor_requests.clear();
    builder->active_function_name = NULL;
    builder->active_method = NULL;
    builder->active_constructor = NULL;
    builder->active_statement = NULL;
    builder->active_statement_index = static_cast<size_t>(-1);
    builder->diagnostic_code = 0;
    builder->diagnostic_message[0] = '\0';
}

static void cobol_text_builder_set_diagnostic(t_cobol_text_builder *builder,
    int code, const char *message)
{
    if (!builder || builder->diagnostic_code != 0
        || builder->diagnostic_message[0] != '\0' || !message)
        return ;
    builder->diagnostic_code = code;
    ft_strlcpy(builder->diagnostic_message, message,
        sizeof(builder->diagnostic_message));
}

static void cobol_text_builder_set_control_flow_depth(
    t_cobol_text_builder *builder, int control_flow_depth)
{
    if (!builder)
        return ;
    builder->control_flow_depth = control_flow_depth > 0 ? control_flow_depth : 0;
}

static void cobol_text_builder_dispose(t_cobol_text_builder *builder)
{
    if (!builder)
        return ;
    if (builder->data)
        cma_free(builder->data);
    if (builder->deferred_procedure_text)
        cma_free(builder->deferred_procedure_text);
    if (builder->based_name_slots)
    {
        size_t index;

        index = 0;
        while (index < builder->based_name_capacity)
        {
            if (builder->based_name_slots[index])
                cma_free(builder->based_name_slots[index]);
            index += 1;
        }
        cma_free(builder->based_name_slots);
    }
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
    builder->free_format = 0;
    builder->control_flow_depth = 0;
    builder->based_name_slots = NULL;
    builder->based_name_capacity = 0;
    builder->based_name_count = 0;
    builder->deferred_procedure_text = NULL;
    builder->deferred_procedure_length = 0;
    builder->deferred_procedure_capacity = 0;
    builder->diagnostic_code = 0;
    builder->diagnostic_message[0] = '\0';
}

static uint64_t cobol_text_builder_based_name_hash(const char *name)
{
    uint64_t hash;

    hash = UINT64_C(1469598103934665603);
    while (*name)
    {
        hash ^= static_cast<unsigned char>(*name);
        hash *= UINT64_C(1099511628211);
        name += 1;
    }
    return (hash);
}

static int cobol_text_builder_rehash_based_names(
    t_cobol_text_builder *builder, size_t new_capacity)
{
    char **new_slots;
    size_t index;

    new_slots = static_cast<char **>(cma_calloc(new_capacity, sizeof(char *)));
    if (!new_slots)
        return (FT_FAILURE);
    index = 0;
    while (index < builder->based_name_capacity)
    {
        char *name;
        size_t slot;

        name = builder->based_name_slots[index];
        if (name)
        {
            slot = cobol_text_builder_based_name_hash(name) % new_capacity;
            while (new_slots[slot])
                slot = (slot + 1) % new_capacity;
            new_slots[slot] = name;
        }
        index += 1;
    }
    if (builder->based_name_slots)
        cma_free(builder->based_name_slots);
    builder->based_name_slots = new_slots;
    builder->based_name_capacity = new_capacity;
    return (FT_SUCCESS);
}

static int cobol_text_builder_has_based_name(
    const t_cobol_text_builder *builder, const char *name)
{
    size_t slot;

    if (!builder || !name || builder->based_name_capacity == 0)
        return (0);
    slot = cobol_text_builder_based_name_hash(name) % builder->based_name_capacity;
    while (builder->based_name_slots[slot])
    {
        if (std::strcmp(builder->based_name_slots[slot], name) == 0)
            return (1);
        slot = (slot + 1) % builder->based_name_capacity;
    }
    return (0);
}

static int cobol_text_builder_register_based_name(
    t_cobol_text_builder *builder, const char *name)
{
    size_t slot;
    char *copy;

    if (!builder || !name || name[0] == '\0')
        return (FT_FAILURE);
    if (cobol_text_builder_has_based_name(builder, name))
        return (FT_SUCCESS);
    if (builder->based_name_capacity == 0
        || (builder->based_name_count + 1) * 10
            >= builder->based_name_capacity * 7)
    {
        size_t capacity;

        capacity = builder->based_name_capacity == 0
            ? 64 : builder->based_name_capacity * 2;
        if (cobol_text_builder_rehash_based_names(builder, capacity)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    copy = static_cast<char *>(cma_calloc(std::strlen(name) + 1, sizeof(char)));
    if (!copy)
        return (FT_FAILURE);
    std::strcpy(copy, name);
    slot = cobol_text_builder_based_name_hash(name) % builder->based_name_capacity;
    while (builder->based_name_slots[slot])
        slot = (slot + 1) % builder->based_name_capacity;
    builder->based_name_slots[slot] = copy;
    builder->based_name_count += 1;
    return (FT_SUCCESS);
}

static int cobol_text_builder_track_based_declaration(
    t_cobol_text_builder *builder, const char *line)
{
    static const char prefix[] = "       01 ";
    const char *name_start;
    const char *suffix;
    size_t name_length;
    char *name;
    int status;

    if (!builder || !line || std::strncmp(line, prefix, sizeof(prefix) - 1) != 0)
        return (FT_SUCCESS);
    name_start = line + sizeof(prefix) - 1;
    suffix = std::strstr(name_start, " BASED.");
    if (!suffix || suffix[7] != '\0' || suffix == name_start)
        return (FT_SUCCESS);
    name_length = static_cast<size_t>(suffix - name_start);
    name = static_cast<char *>(cma_calloc(name_length + 1, sizeof(char)));
    if (!name)
        return (FT_FAILURE);
    std::memcpy(name, name_start, name_length);
    status = cobol_text_builder_register_based_name(builder, name);
    cma_free(name);
    return (status);
}

static int cobol_text_builder_reserve(t_cobol_text_builder *builder, size_t desired_capacity)
{
    char *new_data;
    size_t new_capacity;
    const size_t maximum_capacity = static_cast<size_t>(-1);

    if (!builder)
        return (FT_FAILURE);
    if (builder->capacity >= desired_capacity)
        return (FT_SUCCESS);
    new_capacity = builder->capacity < 128 ? 128 : builder->capacity;
    while (new_capacity < desired_capacity)
    {
        if (new_capacity > maximum_capacity / 2)
        {
            new_capacity = desired_capacity;
            break ;
        }
        new_capacity *= 2;
    }
    new_data = static_cast<char *>(cma_calloc(new_capacity, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    if (builder->data && builder->length > 0)
        std::memcpy(new_data, builder->data, builder->length);
    if (builder->data)
        cma_free(builder->data);
    builder->data = new_data;
    builder->capacity = new_capacity;
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_span(t_cobol_text_builder *builder, const char *text, size_t length)
{
    if (!builder)
        return (FT_FAILURE);
    if (!text && length > 0)
        return (FT_FAILURE);
    if (length == 0)
        return (FT_SUCCESS);
    if (builder->length >= static_cast<size_t>(-1)
        || length > static_cast<size_t>(-1) - builder->length - 1)
        return (FT_FAILURE);
    if (cobol_text_builder_reserve(builder, builder->length + length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    std::memcpy(builder->data + builder->length, text, length);
    builder->length += length;
    builder->data[builder->length] = '\0';
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_deferred(t_cobol_text_builder *builder,
    const char *text, size_t length)
{
    char *new_text;
    size_t new_capacity;

    if (!builder || (!text && length > 0)
        || length > static_cast<size_t>(-1) - builder->deferred_procedure_length - 1)
        return (FT_FAILURE);
    if (length == 0)
        return (FT_SUCCESS);
    if (builder->deferred_procedure_capacity
        < builder->deferred_procedure_length + length + 1)
    {
        new_capacity = builder->deferred_procedure_capacity < 256
            ? 256 : builder->deferred_procedure_capacity;
        while (new_capacity < builder->deferred_procedure_length + length + 1)
        {
            if (new_capacity > static_cast<size_t>(-1) / 2)
            {
                new_capacity = builder->deferred_procedure_length + length + 1;
                break ;
            }
            new_capacity *= 2;
        }
        new_text = static_cast<char *>(cma_calloc(new_capacity, sizeof(char)));
        if (!new_text)
            return (FT_FAILURE);
        if (builder->deferred_procedure_text
            && builder->deferred_procedure_length > 0)
            std::memcpy(new_text, builder->deferred_procedure_text,
                builder->deferred_procedure_length);
        if (builder->deferred_procedure_text)
            cma_free(builder->deferred_procedure_text);
        builder->deferred_procedure_text = new_text;
        builder->deferred_procedure_capacity = new_capacity;
    }
    std::memcpy(builder->deferred_procedure_text
            + builder->deferred_procedure_length, text, length);
    builder->deferred_procedure_length += length;
    builder->deferred_procedure_text[builder->deferred_procedure_length] = '\0';
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_string(t_cobol_text_builder *builder, const char *text)
{
    if (!text)
        return (FT_SUCCESS);
    return (cobol_text_builder_append_span(builder, text, std::strlen(text)));
}

static int cobol_text_builder_append_line(t_cobol_text_builder *builder, const char *text)
{
    const char *to_clause;
    const char *break_cursor;
    size_t text_length;
    size_t label_end;
    int is_paragraph_header;

    if (!builder)
        return (FT_FAILURE);
    if (!text)
        return (FT_SUCCESS);
    text_length = std::strlen(text);
    break_cursor = text;
    while (*break_cursor == ' ' || *break_cursor == '\t')
        break_cursor += 1;
    is_paragraph_header = 0;
    if (text_length > 8 && std::strncmp(text, "       ", 7) == 0
        && !std::isspace(static_cast<unsigned char>(text[7])))
    {
        label_end = 7;
        while (label_end < text_length && text[label_end] != '.')
        {
            if (std::isspace(static_cast<unsigned char>(text[label_end])))
                break ;
            label_end += 1;
        }
        if (label_end == text_length - 1 && text[label_end] == '.')
            is_paragraph_header = 1;
    }
    if (builder->control_flow_depth > 0 && text_length > 0
        && text[text_length - 1] == '.' && !is_paragraph_header)
    {
        char *periodless_line;
        int status;

        periodless_line = static_cast<char *>(cma_calloc(text_length, sizeof(char)));
        if (!periodless_line)
            return (FT_FAILURE);
        std::memcpy(periodless_line, text, text_length - 1);
        status = cobol_text_builder_append_line(builder, periodless_line);
        cma_free(periodless_line);
        return (status);
    }
    if (cobol_text_builder_track_based_declaration(builder, text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (builder->free_format)
    {
        if (cobol_text_builder_append_string(builder, text) != FT_SUCCESS
            || cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
            return (FT_FAILURE);
        if (is_paragraph_header && builder->known_paragraph_definitions)
            builder->known_paragraph_definitions->insert(std::string(text + 7,
                label_end - 7));
        return (FT_SUCCESS);
    }
    to_clause = std::strstr(text, "TO ");
    if (text_length > 72 && to_clause && to_clause > text)
    {
        if (cobol_text_builder_append_span(builder, text,
                static_cast<size_t>(to_clause - text)) != FT_SUCCESS
            || cobol_text_builder_append_string(builder, " \n") != FT_SUCCESS
            || cobol_text_builder_append_string(builder, "           ") != FT_SUCCESS
            || cobol_text_builder_append_string(builder, to_clause) != FT_SUCCESS
            || cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (text_length > 72)
    {
        size_t break_limit;

        break_limit = 65;
        if (break_limit >= text_length)
            break_limit = text_length - 1;
        break_cursor = text + break_limit;
        while (break_cursor > text + 11 && *break_cursor != ' ')
            break_cursor -= 1;
        if (break_cursor > text + 11 && *break_cursor == ' ')
        {
            if (cobol_text_builder_append_span(builder, text,
                    static_cast<size_t>(break_cursor - text)) != FT_SUCCESS
                || cobol_text_builder_append_string(builder, " \n") != FT_SUCCESS
                || cobol_text_builder_append_string(builder, "           ") != FT_SUCCESS
                || cobol_text_builder_append_string(builder, break_cursor + 1) != FT_SUCCESS
                || cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
    }
    if (cobol_text_builder_append_string(builder, text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
        return (FT_FAILURE);
    if (is_paragraph_header && builder->known_paragraph_definitions)
        builder->known_paragraph_definitions->insert(std::string(text + 7,
            label_end - 7));
    return (FT_SUCCESS);
}

static void cobol_text_builder_normalize_procedure_scope_periods(
    t_cobol_text_builder *builder)
{
    size_t read_offset;
    size_t write_offset;
    size_t scope_depth;
    int in_procedure;

    if (!builder || !builder->data)
        return ;
    read_offset = 0;
    write_offset = 0;
    scope_depth = 0;
    in_procedure = 0;
    while (read_offset < builder->length)
    {
        size_t line_end = read_offset;
        size_t trimmed = read_offset;
        size_t content_length;
        int procedure_header;
        int paragraph_header;
        int scope_close;
        int scope_open;
        int omit_period;

        while (line_end < builder->length && builder->data[line_end] != '\n')
            line_end += 1;
        content_length = line_end - read_offset;
        while (trimmed < line_end
            && (builder->data[trimmed] == ' '
                || builder->data[trimmed] == '\t'))
            trimmed += 1;
        procedure_header = content_length >= sizeof("       PROCEDURE DIVISION") - 1
            && std::strncmp(builder->data + read_offset,
                "       PROCEDURE DIVISION",
                sizeof("       PROCEDURE DIVISION") - 1) == 0;
        paragraph_header = content_length > 8
            && std::strncmp(builder->data + read_offset, "       ", 7) == 0
            && !std::isspace(static_cast<unsigned char>(
                builder->data[read_offset + 7]));
        if (paragraph_header)
        {
            size_t token_end = read_offset + 7;

            while (token_end < line_end && builder->data[token_end] != '.')
            {
                if (std::isspace(static_cast<unsigned char>(
                        builder->data[token_end])))
                {
                    paragraph_header = 0;
                    break ;
                }
                token_end += 1;
            }
            if (token_end != line_end - 1 || builder->data[token_end] != '.')
                paragraph_header = 0;
        }
        if (procedure_header)
            in_procedure = 1;
        scope_close = 0;
        scope_open = 0;
        if (in_procedure && !procedure_header && !paragraph_header)
        {
            if ((line_end - trimmed >= sizeof("END-IF") - 1
                    && std::strncmp(builder->data + trimmed, "END-IF",
                        sizeof("END-IF") - 1) == 0)
                || (line_end - trimmed >= sizeof("END-PERFORM") - 1
                    && std::strncmp(builder->data + trimmed, "END-PERFORM",
                        sizeof("END-PERFORM") - 1) == 0))
            {
                if (scope_depth > 0)
                    scope_depth -= 1;
                scope_close = 1;
            }
            else if ((line_end - trimmed >= sizeof("IF ") - 1
                    && std::strncmp(builder->data + trimmed, "IF ",
                        sizeof("IF ") - 1) == 0)
                || (line_end - trimmed >= sizeof("PERFORM UNTIL ") - 1
                    && std::strncmp(builder->data + trimmed, "PERFORM UNTIL ",
                        sizeof("PERFORM UNTIL ") - 1) == 0))
            {
                scope_depth += 1;
                scope_open = 1;
            }
        }
        omit_period = in_procedure && !procedure_header && !paragraph_header
            && scope_depth > 0 && content_length > 0
            && builder->data[line_end - 1] == '.';
        if (content_length > static_cast<size_t>(omit_period))
        {
            size_t copy_length = content_length
                - static_cast<size_t>(omit_period);

            std::memmove(builder->data + write_offset,
                builder->data + read_offset, copy_length);
            write_offset += copy_length;
        }
        if (line_end < builder->length)
            builder->data[write_offset++] = '\n';
        read_offset = line_end
            + static_cast<size_t>(line_end < builder->length);
        (void)scope_close;
        (void)scope_open;
    }
    builder->length = write_offset;
    builder->data[write_offset] = '\0';
}

static int cblc_match_keyword(const char **cursor, const char *keyword)
{
    size_t length;

    if (!cursor || !*cursor || !keyword)
        return (0);
    length = std::strlen(keyword);
    if (std::strncmp(*cursor, keyword, length) != 0)
        return (0);
    if ((*cursor)[length] != '\0' && ((*cursor)[length] == '_' || ((*cursor)[length] >= 'a'
                && (*cursor)[length] <= 'z') || ((*cursor)[length] >= 'A'
                && (*cursor)[length] <= 'Z') || ((*cursor)[length] >= '0'
                && (*cursor)[length] <= '9')))
        return (0);
    *cursor += length;
    return (1);
}

static int cblc_parse_boolean_literal(const char **cursor, int *out_value)
{
    const char *start;

    if (!cursor || !*cursor || !out_value)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_match_keyword(cursor, "true"))
    {
        *out_value = 1;
        return (FT_SUCCESS);
    }
    *cursor = start;
    if (cblc_match_keyword(cursor, "false"))
    {
        *out_value = 0;
        return (FT_SUCCESS);
    }
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_normalize_boolean_literals(const char *source, char *destination,
    size_t destination_size)
{
    size_t read_index;
    size_t write_index;
    char quote;
    int escaped;

    if (!source || !destination || destination_size == 0)
        return (FT_FAILURE);
    read_index = 0;
    write_index = 0;
    quote = '\0';
    escaped = 0;
    while (source[read_index] != '\0')
    {
        if (quote != '\0')
        {
            if (write_index + 1 >= destination_size)
                return (FT_FAILURE);
            destination[write_index++] = source[read_index];
            if (escaped)
                escaped = 0;
            else if (source[read_index] == '\\')
                escaped = 1;
            else if (source[read_index] == quote)
                quote = '\0';
            read_index += 1;
            continue ;
        }
        if (source[read_index] == '\'' || source[read_index] == '"')
        {
            quote = source[read_index];
            if (write_index + 1 >= destination_size)
                return (FT_FAILURE);
            destination[write_index++] = source[read_index++];
            continue ;
        }
        if ((source[read_index] >= 'a' && source[read_index] <= 'z')
            || (source[read_index] >= 'A' && source[read_index] <= 'Z')
            || source[read_index] == '_')
        {
            size_t token_start = read_index;
            size_t token_length;
            const char *replacement;

            while ((source[read_index] >= 'a' && source[read_index] <= 'z')
                || (source[read_index] >= 'A' && source[read_index] <= 'Z')
                || (source[read_index] >= '0' && source[read_index] <= '9')
                || source[read_index] == '_')
                read_index += 1;
            token_length = read_index - token_start;
            replacement = NULL;
            if (token_length == 4
                && std::strncmp(source + token_start, "true", 4) == 0)
                replacement = "1";
            else if (token_length == 5
                && std::strncmp(source + token_start, "false", 5) == 0)
                replacement = "0";
            if (replacement)
            {
                if (write_index + 1 >= destination_size)
                    return (FT_FAILURE);
                destination[write_index++] = replacement[0];
            }
            else
            {
                if (token_length >= destination_size - write_index)
                    return (FT_FAILURE);
                std::memcpy(destination + write_index, source + token_start,
                    token_length);
                write_index += token_length;
            }
            continue ;
        }
        if (write_index + 1 >= destination_size)
            return (FT_FAILURE);
        destination[write_index++] = source[read_index++];
    }
    destination[write_index] = '\0';
    return (FT_SUCCESS);
}

static void cblc_skip_whitespace(const char **cursor)
{
    if (!cursor || !*cursor)
        return ;
    while (**cursor == ' ' || **cursor == '\t' || **cursor == '\n' || **cursor == '\r')
        *cursor += 1;
}

static int cblc_is_identifier_char(char character)
{
    if (character == '_')
        return (1);
    if (character >= 'a' && character <= 'z')
        return (1);
    if (character >= 'A' && character <= 'Z')
        return (1);
    if (character >= '0' && character <= '9')
        return (1);
    return (0);
}

static int cblc_parse_identifier(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t length;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    length = 0;
    while (cblc_is_identifier_char((*cursor)[length]))
        length += 1;
    while ((*cursor)[length] == ':' && (*cursor)[length + 1] == ':'
        && (*cursor)[length + 2] != '\0'
        && cblc_is_identifier_char((*cursor)[length + 2]))
    {
        length += 2;
        while (cblc_is_identifier_char((*cursor)[length]))
            length += 1;
    }
    if (length == 0)
        return (FT_FAILURE);
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer, *cursor, length);
    buffer[length] = '\0';
    *cursor += length;
    return (FT_SUCCESS);
}

static int cblc_cobol_identifier_is_reserved(const char *identifier)
{
    static const char *reserved_words[] = {
        "ACCEPT", "ADD", "ALLOCATE", "ALTER", "AND", "CALL", "CANCEL",
        "CLOSE", "COMPUTE", "CONFIGURATION", "DATA", "DELETE", "DISPLAY",
        "DIVIDE", "DIVISION", "ELSE", "END-IF", "END-PERFORM", "END-READ",
        "END-WRITE", "ENVIRONMENT", "EVALUATE", "FD", "FILE", "FREE",
        "GOBACK", "IF", "IN", "INSPECT", "INPUT-OUTPUT", "MOVE", "MULTIPLY",
        "NOT", "NULL", "OF", "ON", "OPEN", "OR", "OUTPUT", "PERFORM", "PIC",
        "PROCEDURE", "PROGRAM-ID", "READ", "RECORD", "REDEFINES", "RETURNING",
        "SECTION", "SET", "STOP", "STRING", "SUBTRACT", "THEN", "TO", "UNTIL",
        "USING", "VALUE", "WHEN", "WRITE", "WORKING-STORAGE"
    };
    size_t index;

    if (!identifier || identifier[0] == '\0')
        return (0);
    index = 0;
    while (index < sizeof(reserved_words) / sizeof(reserved_words[0]))
    {
        if (std::strcmp(identifier, reserved_words[index]) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

static void cblc_identifier_to_cobol(const char *identifier, char *buffer, size_t buffer_size)
{
    size_t index;
    size_t output_index;
    static const char operator_plus_assignment[] = "operator+=";
    static const char operator_plus_assignment_upper[] = "OPERATOR+=";
    static const char operator_plus_assignment_cobol[] = "OPERATOR-PLUS-ASSIGN";
    static const char operator_plus[] = "operator+";
    static const char operator_plus_upper[] = "OPERATOR+";
    static const char operator_plus_cobol[] = "OPERATOR-PLUS";
    static const char operator_assignment[] = "operator=";
    static const char operator_assignment_cobol[] = "OPERATOR-ASSIGN";

    if (!identifier || !buffer || buffer_size == 0)
        return ;
    if (std::strcmp(identifier, "NULL") == 0
        || std::strcmp(identifier, "null") == 0)
    {
        ft_strlcpy(buffer, "NULL", buffer_size);
        return ;
    }
    index = 0;
    output_index = 0;
    while (identifier[index] != '\0' && output_index + 1 < buffer_size)
    {
        char character;

        if (std::strncmp(identifier + index, operator_plus_assignment,
                sizeof(operator_plus_assignment) - 1) == 0
            || std::strncmp(identifier + index, operator_plus_assignment_upper,
                sizeof(operator_plus_assignment_upper) - 1) == 0)
        {
            size_t replacement_index;

            replacement_index = 0;
            while (operator_plus_assignment_cobol[replacement_index] != '\0'
                && output_index + 1 < buffer_size)
            {
                buffer[output_index] =
                    operator_plus_assignment_cobol[replacement_index];
                output_index += 1;
                replacement_index += 1;
            }
            index += sizeof(operator_plus_assignment) - 1;
            continue ;
        }
        if (std::strncmp(identifier + index, operator_plus,
                sizeof(operator_plus) - 1) == 0
            || std::strncmp(identifier + index, operator_plus_upper,
                sizeof(operator_plus_upper) - 1) == 0)
        {
            size_t replacement_index;

            replacement_index = 0;
            while (operator_plus_cobol[replacement_index] != '\0'
                && output_index + 1 < buffer_size)
            {
                buffer[output_index] = operator_plus_cobol[replacement_index];
                output_index += 1;
                replacement_index += 1;
            }
            index += sizeof(operator_plus) - 1;
            continue ;
        }
        if (std::strncmp(identifier + index, operator_assignment,
                sizeof(operator_assignment) - 1) == 0
            || std::strncmp(identifier + index, "OPERATOR=",
                sizeof(operator_assignment) - 1) == 0)
        {
            size_t replacement_index;

            replacement_index = 0;
            while (operator_assignment_cobol[replacement_index] != '\0'
                && output_index + 1 < buffer_size)
            {
                buffer[output_index] = operator_assignment_cobol[replacement_index];
                output_index += 1;
                replacement_index += 1;
            }
            index += sizeof(operator_assignment) - 1;
            continue ;
        }
        if (std::strncmp(identifier + index, "operator[]", sizeof("operator[]") - 1) == 0
            || std::strncmp(identifier + index, "OPERATOR[]", sizeof("OPERATOR[]") - 1) == 0)
        {
            static const char operator_index_cobol[] = "OPERATOR-INDEX";
            size_t replacement_index;

            replacement_index = 0;
            while (operator_index_cobol[replacement_index] != '\0'
                && output_index + 1 < buffer_size)
            {
                buffer[output_index] = operator_index_cobol[replacement_index];
                output_index += 1;
                replacement_index += 1;
            }
            index += sizeof("operator[]") - 1;
            continue ;
        }
        if (std::strncmp(identifier + index, "operator->", sizeof("operator->") - 1) == 0
            || std::strncmp(identifier + index, "OPERATOR->", sizeof("operator->") - 1) == 0)
        {
            static const char operator_arrow_cobol[] = "OPERATOR-ARROW";
            size_t replacement_index;

            replacement_index = 0;
            while (operator_arrow_cobol[replacement_index] != '\0'
                && output_index + 1 < buffer_size)
            {
                buffer[output_index] = operator_arrow_cobol[replacement_index];
                output_index += 1;
                replacement_index += 1;
            }
            index += sizeof("operator->") - 1;
            continue ;
        }
        character = identifier[index];
        if (character >= 'a' && character <= 'z')
            character = static_cast<char>(character - 'a' + 'A');
        else if (character == '_')
            character = '-';
        buffer[output_index] = character;
        output_index += 1;
        index += 1;
    }
    buffer[output_index] = '\0';
    if (cblc_cobol_identifier_is_reserved(buffer))
    {
        char safe_buffer[TRANSPILE_IDENTIFIER_MAX];

        if (std::snprintf(safe_buffer, sizeof(safe_buffer), "CBLC-USER-%s", buffer) >= 0)
            ft_strlcpy(buffer, safe_buffer, buffer_size);
    }
}

static void cblc_build_string_component_name(const char *base, const char *suffix, char *buffer,
    size_t buffer_size)
{
    if (!buffer || buffer_size == 0)
        return ;
    if (!base)
    {
        buffer[0] = '\0';
        return ;
    }
    ft_strlcpy(buffer, base, buffer_size);
    if (suffix)
        ft_strlcat(buffer, suffix, buffer_size);
}

static int cblc_string_item_uses_dynamic_storage(const t_cblc_data_item *item)
{
    if (!item || item->kind != CBLC_DATA_KIND_CHAR_BUFFER)
        return (0);
    if (cblc_native_standard_library_expression_mode_enabled())
        return (0);
    if (item->is_const || item->array_count > 0 || item->is_alias)
        return (0);
    if (cblc_native_standard_library_expression_mode_enabled()
        && !item->is_function_local && item->owner_function_name[0] != '\0')
        return (0);
    if (std::strstr(item->source_name, "->"))
        return (0);
    return (1);
}

static int cblc_string_field_uses_dynamic_storage(const t_cblc_struct_field *field)
{
    if (!field || field->kind != CBLC_DATA_KIND_CHAR_BUFFER)
        return (0);
    if (field->is_const)
        return (0);
    return (1);
}

static int cblc_string_item_array_uses_dynamic_storage(const t_cblc_data_item *item)
{
    if (!item || item->kind != CBLC_DATA_KIND_CHAR_BUFFER)
        return (0);
    if (item->is_const || item->array_count == 0 || item->is_alias)
        return (0);
    if (std::strstr(item->source_name, "->"))
        return (0);
    return (1);
}

static int cblc_string_item_is_member_storage(const t_cblc_data_item *item)
{
    if (!item || item->kind != CBLC_DATA_KIND_CHAR_BUFFER)
        return (0);
    if (!std::strchr(item->source_name, '.'))
        return (0);
    return (1);
}

static int cblc_emit_dynamic_string_ensure_capacity(const t_cblc_data_item *item,
    size_t desired_capacity, t_cobol_text_builder *builder)
{
    char line[256];
    char cap_name[TRANSPILE_IDENTIFIER_MAX];
    char ptr_name[TRANSPILE_IDENTIFIER_MAX];
    char buffer_name[TRANSPILE_IDENTIFIER_MAX];

    if (!item || !builder)
        return (FT_FAILURE);
    if (!cblc_string_item_uses_dynamic_storage(item))
        return (FT_SUCCESS);
    if (desired_capacity == 0)
        desired_capacity = 1;
    cblc_build_string_component_name(item->cobol_name, "-CAP", cap_name, sizeof(cap_name));
    cblc_build_string_component_name(item->cobol_name, "-PTR", ptr_name, sizeof(ptr_name));
    cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_name, sizeof(buffer_name));
    if (std::snprintf(line, sizeof(line), "           IF %s < %zu", cap_name,
            desired_capacity) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               IF %s NOT = NULL", ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "                   FREE %s", ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line),
            "               ALLOCATE %zu CHARACTERS RETURNING %s",
            desired_capacity, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s TO %s",
            buffer_name, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               COMPUTE %s = %zu",
            cap_name, desired_capacity) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, "           END-IF."));
}

static int cblc_emit_dynamic_string_ensure_capacity_for_references(const char *cap_name,
    const char *ptr_name, const char *buffer_name, size_t desired_capacity,
    t_cobol_text_builder *builder)
{
    char line[256];

    if (!cap_name || !ptr_name || !buffer_name || !builder)
        return (FT_FAILURE);
    if (desired_capacity == 0)
        desired_capacity = 1;
    if (std::snprintf(line, sizeof(line), "           IF %s < %zu", cap_name,
            desired_capacity) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               IF %s NOT = NULL", ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "                   FREE %s", ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line),
            "               ALLOCATE %zu CHARACTERS RETURNING %s",
            desired_capacity, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               SET ADDRESS OF %s TO %s",
            buffer_name, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "               COMPUTE %s = %zu",
            cap_name, desired_capacity) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    return (cobol_text_builder_append_line(builder, "           END-IF."));
}

static int cblc_parse_dynamic_string_buffer_reference(const t_cblc_translation_unit *unit,
    const char *reference, const t_cblc_data_item **out_item, char *buffer_name,
    size_t buffer_name_size, char *cap_name, size_t cap_name_size, char *ptr_name,
    size_t ptr_name_size)
{
    char base_name[TRANSPILE_IDENTIFIER_MAX];
    const char *suffix;
    const char *index_start;
    const char *index_end;
    const t_cblc_data_item *item;
    size_t base_length;
    size_t index_length;

    if (!unit || !reference || !out_item || !buffer_name || buffer_name_size == 0
        || !cap_name || cap_name_size == 0 || !ptr_name || ptr_name_size == 0)
        return (FT_FAILURE);
    suffix = std::strstr(reference, "-BUF");
    if (!suffix)
        return (FT_FAILURE);
    base_length = static_cast<size_t>(suffix - reference);
    if (base_length == 0 || base_length + 1 > sizeof(base_name))
        return (FT_FAILURE);
    std::memcpy(base_name, reference, base_length);
    base_name[base_length] = '\0';
    suffix += std::strlen("-BUF");
    item = cblc_find_data_item_by_cobol(unit, base_name);
    if (!item || item->kind != CBLC_DATA_KIND_CHAR_BUFFER)
        return (FT_FAILURE);
    if (*suffix == '\0')
    {
        if (!cblc_string_item_uses_dynamic_storage(item))
            return (FT_FAILURE);
        ft_strlcpy(buffer_name, reference, buffer_name_size);
        cblc_build_string_component_name(item->cobol_name, "-CAP", cap_name, cap_name_size);
        cblc_build_string_component_name(item->cobol_name, "-PTR", ptr_name, ptr_name_size);
        *out_item = item;
        return (FT_SUCCESS);
    }
    if (!cblc_string_item_array_uses_dynamic_storage(item))
        return (FT_FAILURE);
    if (*suffix != '(')
        return (FT_FAILURE);
    index_start = suffix + 1;
    index_end = std::strchr(index_start, ')');
    if (!index_end || index_end[1] != '\0')
        return (FT_FAILURE);
    index_length = static_cast<size_t>(index_end - index_start);
    if (index_length == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer_name, buffer_name_size, "%s-BUF", item->cobol_name) < 0)
        return (FT_FAILURE);
    if (std::snprintf(cap_name, cap_name_size, "%s-CAP(%.*s)", item->cobol_name,
            static_cast<int>(index_length), index_start) < 0)
        return (FT_FAILURE);
    if (std::snprintf(ptr_name, ptr_name_size, "%s-PTR(%.*s)", item->cobol_name,
            static_cast<int>(index_length), index_start) < 0)
        return (FT_FAILURE);
    *out_item = item;
    return (FT_SUCCESS);
}

static int cblc_prepare_dynamic_string_buffer_reference(const t_cblc_translation_unit *unit,
    const char *reference, size_t desired_capacity, t_cobol_text_builder *builder,
    char *out_reference, size_t out_reference_size)
{
    char buffer_name[TRANSPILE_IDENTIFIER_MAX];
    char cap_name[TRANSPILE_STATEMENT_TEXT_MAX];
    char ptr_name[TRANSPILE_STATEMENT_TEXT_MAX];
    const t_cblc_data_item *item;
    char line[256];

    if (!unit || !reference || !builder || !out_reference || out_reference_size == 0)
        return (FT_FAILURE);
    if (cblc_parse_dynamic_string_buffer_reference(unit, reference, &item, buffer_name,
            sizeof(buffer_name), cap_name, sizeof(cap_name), ptr_name, sizeof(ptr_name))
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_emit_dynamic_string_ensure_capacity_for_references(cap_name, ptr_name,
            buffer_name, desired_capacity, builder) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "           SET ADDRESS OF %s TO %s",
            buffer_name, ptr_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_strlcpy(out_reference, buffer_name, out_reference_size);
    (void)item;
    return (FT_SUCCESS);
}

static int cblc_prepare_dynamic_string_buffer_view(const t_cblc_translation_unit *unit,
    const char *reference, t_cobol_text_builder *builder, char *out_reference,
    size_t out_reference_size)
{
    return (cblc_prepare_dynamic_string_buffer_reference(unit, reference, 1, builder,
            out_reference, out_reference_size));
}

static int cblc_parse_numeric_literal(const char **cursor, size_t *out_value)
{
    size_t value;
    const char *start;

    if (!cursor || !*cursor || !out_value)
        return (FT_FAILURE);
    value = 0;
    start = *cursor;
    while (**cursor >= '0' && **cursor <= '9')
    {
        value = value * 10 + static_cast<size_t>(**cursor - '0');
        *cursor += 1;
    }
    if (*cursor == start)
        return (FT_FAILURE);
    *out_value = value;
    return (FT_SUCCESS);
}
