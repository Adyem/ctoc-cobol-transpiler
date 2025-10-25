#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

#define COPYBOOK_GRAPH_MAX_SEGMENTS 64

typedef struct s_copybook_collection
{
    char (*names)[TRANSPILE_FILE_PATH_MAX];
    size_t count;
    size_t capacity;
}   t_copybook_collection;

typedef struct s_copybook_edge
{
    char from[TRANSPILE_FILE_PATH_MAX];
    char to[TRANSPILE_FILE_PATH_MAX];
}   t_copybook_edge;

typedef struct s_copybook_edge_list
{
    t_copybook_edge *edges;
    size_t count;
    size_t capacity;
}   t_copybook_edge_list;

static void copybook_collection_init(t_copybook_collection *collection)
{
    if (!collection)
        return ;
    collection->names = NULL;
    collection->count = 0;
    collection->capacity = 0;
}

static void copybook_collection_dispose(t_copybook_collection *collection)
{
    if (!collection)
        return ;
    if (collection->names)
        cma_free(collection->names);
    collection->names = NULL;
    collection->count = 0;
    collection->capacity = 0;
}

static int copybook_collection_reserve(t_copybook_collection *collection, size_t desired_capacity)
{
    char (*entries)[TRANSPILE_FILE_PATH_MAX];
    size_t index;

    if (!collection)
        return (FT_FAILURE);
    if (collection->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    entries = static_cast<char (*)[TRANSPILE_FILE_PATH_MAX]>(cma_calloc(desired_capacity,
            sizeof(*entries)));
    if (!entries)
        return (FT_FAILURE);
    index = 0;
    while (index < collection->count)
    {
        ft_strlcpy(entries[index], collection->names[index], TRANSPILE_FILE_PATH_MAX);
        index += 1;
    }
    if (collection->names)
        cma_free(collection->names);
    collection->names = entries;
    collection->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int copybook_collection_contains(const t_copybook_collection *collection, const char *name)
{
    size_t index;

    if (!collection)
        return (0);
    if (!name)
        return (0);
    index = 0;
    while (index < collection->count)
    {
        if (ft_strncmp(collection->names[index], name, TRANSPILE_FILE_PATH_MAX) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

static int copybook_collection_add(t_copybook_collection *collection, const char *name)
{
    if (!collection)
        return (FT_FAILURE);
    if (!name || name[0] == '\0')
        return (FT_SUCCESS);
    if (copybook_collection_contains(collection, name))
        return (FT_SUCCESS);
    if (collection->count >= collection->capacity)
    {
        if (copybook_collection_reserve(collection,
                collection->capacity == 0 ? 4 : collection->capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(collection->names[collection->count], name, TRANSPILE_FILE_PATH_MAX);
    collection->count += 1;
    return (FT_SUCCESS);
}

static size_t copybook_collection_index_of(const t_copybook_collection *collection, const char *name)
{
    size_t index;

    if (!collection)
        return (0);
    if (!name)
        return (collection->count);
    index = 0;
    while (index < collection->count)
    {
        if (ft_strncmp(collection->names[index], name, TRANSPILE_FILE_PATH_MAX) == 0)
            return (index);
        index += 1;
    }
    return (collection->count);
}

static void copybook_edge_list_init(t_copybook_edge_list *list)
{
    if (!list)
        return ;
    list->edges = NULL;
    list->count = 0;
    list->capacity = 0;
}

static void copybook_edge_list_dispose(t_copybook_edge_list *list)
{
    if (!list)
        return ;
    if (list->edges)
        cma_free(list->edges);
    list->edges = NULL;
    list->count = 0;
    list->capacity = 0;
}

static int copybook_edge_list_reserve(t_copybook_edge_list *list, size_t desired_capacity)
{
    t_copybook_edge *edges;
    size_t index;

    if (!list)
        return (FT_FAILURE);
    if (list->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    edges = static_cast<t_copybook_edge *>(cma_calloc(desired_capacity, sizeof(*edges)));
    if (!edges)
        return (FT_FAILURE);
    index = 0;
    while (index < list->count)
    {
        edges[index] = list->edges[index];
        index += 1;
    }
    if (list->edges)
        cma_free(list->edges);
    list->edges = edges;
    list->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int copybook_edge_list_add(t_copybook_edge_list *list, const char *from, const char *to)
{
    size_t index;

    if (!list)
        return (FT_FAILURE);
    if (!from || !to)
        return (FT_FAILURE);
    if (from[0] == '\0' || to[0] == '\0')
        return (FT_SUCCESS);
    index = 0;
    while (index < list->count)
    {
        if (ft_strncmp(list->edges[index].from, from, TRANSPILE_FILE_PATH_MAX) == 0
            && ft_strncmp(list->edges[index].to, to, TRANSPILE_FILE_PATH_MAX) == 0)
            return (FT_SUCCESS);
        index += 1;
    }
    if (list->count >= list->capacity)
    {
        if (copybook_edge_list_reserve(list, list->capacity == 0 ? 4 : list->capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(list->edges[list->count].from, from, TRANSPILE_FILE_PATH_MAX);
    ft_strlcpy(list->edges[list->count].to, to, TRANSPILE_FILE_PATH_MAX);
    list->count += 1;
    return (FT_SUCCESS);
}

void transpiler_copybook_normalize_name(const char *source, char *destination, size_t destination_size)
{
    char normalized[TRANSPILE_FILE_PATH_MAX];
    size_t index;
    size_t dest_index;
    size_t position_count;
    size_t positions[COPYBOOK_GRAPH_MAX_SEGMENTS];

    if (!destination || destination_size == 0)
        return ;
    destination[0] = '\0';
    if (!source)
        return ;
    index = 0;
    while (source[index] != '\0' && index + 1 < sizeof(normalized))
    {
        char character;

        character = source[index];
        if (character == '\\')
            character = '/';
        if (character >= 'a' && character <= 'z')
            character = static_cast<char>(character - ('a' - 'A'));
        normalized[index] = character;
        index += 1;
    }
    normalized[index] = '\0';
    dest_index = 0;
    position_count = 0;
    index = 0;
    while (normalized[index] != '\0')
    {
        size_t start;
        size_t length;
        size_t segment_index;
        char segment[TRANSPILE_FILE_PATH_MAX];

        start = index;
        while (normalized[index] != '\0' && normalized[index] != '/')
            index += 1;
        length = index - start;
        if (length == 0)
        {
            if (normalized[index] == '/')
                index += 1;
            continue ;
        }
        segment_index = 0;
        while (segment_index < length && segment_index + 1 < sizeof(segment))
        {
            segment[segment_index] = normalized[start + segment_index];
            segment_index += 1;
        }
        segment[segment_index] = '\0';
        if (segment_index == 1 && segment[0] == '.')
        {
        }
        else if (segment_index == 2 && segment[0] == '.' && segment[1] == '.')
        {
            if (position_count > 0)
            {
                dest_index = positions[position_count - 1];
                destination[dest_index] = '\0';
                position_count -= 1;
            }
        }
        else
        {
            if (dest_index != 0)
            {
                if (dest_index + 1 >= destination_size)
                {
                    destination[destination_size - 1] = '\0';
                    return ;
                }
                destination[dest_index] = '/';
                dest_index += 1;
                destination[dest_index] = '\0';
            }
            segment_index = 0;
            while (segment_index < length && dest_index + 1 < destination_size)
            {
                destination[dest_index] = segment[segment_index];
                dest_index += 1;
                segment_index += 1;
            }
            destination[dest_index] = '\0';
            if (position_count < COPYBOOK_GRAPH_MAX_SEGMENTS)
            {
                positions[position_count] = dest_index;
                position_count += 1;
            }
        }
        if (normalized[index] == '/')
            index += 1;
    }
    if (destination[0] == '\0')
        ft_strlcpy(destination, normalized, destination_size);
}

static int transpiler_copybook_graph_collect(const t_ast_node *node, t_copybook_collection *collection)
{
    size_t index;

    if (!node)
        return (FT_SUCCESS);
    if (node->kind == AST_NODE_COPYBOOK_INCLUDE)
    {
        const t_ast_node *name_node;
        char normalized[TRANSPILE_FILE_PATH_MAX];

        name_node = NULL;
        if (node->child_count > 0)
            name_node = node->children[0];
        if (name_node && name_node->token.lexeme)
        {
            normalized[0] = '\0';
            transpiler_copybook_normalize_name(name_node->token.lexeme, normalized, sizeof(normalized));
            if (normalized[0] == '\0')
                ft_strlcpy(normalized, name_node->token.lexeme, sizeof(normalized));
            if (copybook_collection_add(collection, normalized) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    index = 0;
    while (index < node->child_count)
    {
        if (transpiler_copybook_graph_collect(node->children[index], collection) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static const t_transpiler_copybook *transpiler_copybook_graph_find_canonical(const t_transpiler_context *context, const char *canonical_name)
{
    size_t index;

    if (!context)
        return (NULL);
    if (!canonical_name || canonical_name[0] == '\0')
        return (NULL);
    index = 0;
    while (index < context->copybook_count)
    {
        if (ft_strncmp(context->copybooks[index].canonical_name, canonical_name, TRANSPILE_FILE_PATH_MAX) == 0)
            return (&context->copybooks[index]);
        index += 1;
    }
    return (NULL);
}

static int transpiler_copybook_graph_traverse_copybook(const t_transpiler_context *context, const char *canonical_name,
    t_copybook_collection *nodes, t_copybook_collection *visited, t_copybook_edge_list *edges)
{
    const t_transpiler_copybook *copybook;
    size_t dependency_index;

    if (!canonical_name || canonical_name[0] == '\0')
        return (FT_SUCCESS);
    if (copybook_collection_contains(visited, canonical_name))
        return (FT_SUCCESS);
    if (copybook_collection_add(visited, canonical_name) != FT_SUCCESS)
        return (FT_FAILURE);
    copybook = transpiler_copybook_graph_find_canonical(context, canonical_name);
    if (!copybook)
        return (FT_SUCCESS);
    dependency_index = 0;
    while (dependency_index < copybook->dependency_count)
    {
        const char *dependency;

        dependency = copybook->dependencies[dependency_index];
        if (dependency && dependency[0] != '\0')
        {
            if (copybook_collection_add(nodes, dependency) != FT_SUCCESS)
                return (FT_FAILURE);
            if (copybook_edge_list_add(edges, canonical_name, dependency) != FT_SUCCESS)
                return (FT_FAILURE);
            if (transpiler_copybook_graph_traverse_copybook(context, dependency, nodes, visited, edges) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        dependency_index += 1;
    }
    return (FT_SUCCESS);
}

static void transpiler_copybook_graph_escape_label(const char *source, char *destination, size_t destination_size)
{
    size_t source_index;
    size_t destination_index;

    if (!destination || destination_size == 0)
        return ;
    destination[0] = '\0';
    if (!source)
        return ;
    source_index = 0;
    destination_index = 0;
    while (source[source_index] != '\0')
    {
        char character;

        character = source[source_index];
        if (character == '"' || character == '\\')
        {
            if (destination_index + 2 >= destination_size)
                break ;
            destination[destination_index] = '\\';
            destination[destination_index + 1] = character;
            destination_index += 2;
        }
        else if (character < 32 || character > 126)
        {
            if (destination_index + 1 >= destination_size)
                break ;
            destination[destination_index] = '?';
            destination_index += 1;
        }
        else
        {
            if (destination_index + 1 >= destination_size)
                break ;
            destination[destination_index] = character;
            destination_index += 1;
        }
        source_index += 1;
    }
    if (destination_index >= destination_size)
        destination_index = destination_size - 1;
    destination[destination_index] = '\0';
}

static int transpiler_copybook_graph_write_literal(t_runtime_file *file, const char *text)
{
    size_t length;

    if (!file || !text)
        return (FT_FAILURE);
    length = ft_strlen(text);
    if (runtime_file_write(file, text, length) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int transpiler_copybook_graph_emit(const t_transpiler_context *context, const t_ast_node *program, const char *root_label, const char *path)
{
    t_copybook_collection includes;
    t_copybook_collection nodes;
    t_copybook_collection visited;
    t_copybook_edge_list edges;
    t_runtime_file file;
    char escaped_label[TRANSPILE_FILE_PATH_MAX];
    int status;
    size_t index;

    if (!program || !path)
        return (FT_FAILURE);
    copybook_collection_init(&includes);
    copybook_collection_init(&nodes);
    copybook_collection_init(&visited);
    copybook_edge_list_init(&edges);
    if (transpiler_copybook_graph_collect(program, &includes) != FT_SUCCESS)
    {
        copybook_collection_dispose(&includes);
        copybook_collection_dispose(&nodes);
        copybook_collection_dispose(&visited);
        copybook_edge_list_dispose(&edges);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < includes.count)
    {
        if (copybook_collection_add(&nodes, includes.names[index]) != FT_SUCCESS)
        {
            copybook_collection_dispose(&includes);
            copybook_collection_dispose(&nodes);
            copybook_collection_dispose(&visited);
            copybook_edge_list_dispose(&edges);
            return (FT_FAILURE);
        }
        index += 1;
    }
    index = 0;
    while (index < includes.count)
    {
        if (transpiler_copybook_graph_traverse_copybook(context, includes.names[index], &nodes, &visited, &edges) != FT_SUCCESS)
        {
            copybook_collection_dispose(&includes);
            copybook_collection_dispose(&nodes);
            copybook_collection_dispose(&visited);
            copybook_edge_list_dispose(&edges);
            return (FT_FAILURE);
        }
        index += 1;
    }
    runtime_file_init(&file);
    if (runtime_file_open_write(&file, path) != FT_SUCCESS)
    {
        copybook_collection_dispose(&includes);
        copybook_collection_dispose(&nodes);
        copybook_collection_dispose(&visited);
        copybook_edge_list_dispose(&edges);
        return (FT_FAILURE);
    }
    status = FT_FAILURE;
    if (transpiler_copybook_graph_write_literal(&file, "digraph Copybooks {\n") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_copybook_graph_write_literal(&file,
            "    node [shape=box, fontname=\"Courier\"];\n") != FT_SUCCESS)
        goto cleanup;
    if (!root_label || root_label[0] == '\0')
        root_label = "translation-unit";
    transpiler_copybook_graph_escape_label(root_label, escaped_label, sizeof(escaped_label));
    if (escaped_label[0] == '\0')
        ft_strlcpy(escaped_label, "translation-unit", sizeof(escaped_label));
    {
        char line[TRANSPILE_FILE_PATH_MAX * 2];

        if (pf_snprintf(line, sizeof(line), "    root [label=\"%s\"];\n", escaped_label) < 0)
            goto cleanup;
        if (transpiler_copybook_graph_write_literal(&file, line) != FT_SUCCESS)
            goto cleanup;
    }
    index = 0;
    while (index < nodes.count)
    {
        char node_label[TRANSPILE_FILE_PATH_MAX * 2];
        char line[TRANSPILE_FILE_PATH_MAX * 2];

        transpiler_copybook_graph_escape_label(nodes.names[index], node_label, sizeof(node_label));
        if (pf_snprintf(line, sizeof(line), "    copybook_%lu [label=\"%s\"];\n",
                static_cast<unsigned long>(index), node_label) < 0)
            goto cleanup;
        if (transpiler_copybook_graph_write_literal(&file, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    index = 0;
    while (index < includes.count)
    {
        size_t target_index;
        char line[128];

        target_index = copybook_collection_index_of(&nodes, includes.names[index]);
        if (target_index < nodes.count)
        {
            if (pf_snprintf(line, sizeof(line), "    root -> copybook_%lu;\n",
                    static_cast<unsigned long>(target_index)) < 0)
                goto cleanup;
            if (transpiler_copybook_graph_write_literal(&file, line) != FT_SUCCESS)
                goto cleanup;
        }
        index += 1;
    }
    index = 0;
    while (index < edges.count)
    {
        size_t from_index;
        size_t to_index;
        char line[128];

        from_index = copybook_collection_index_of(&nodes, edges.edges[index].from);
        to_index = copybook_collection_index_of(&nodes, edges.edges[index].to);
        if (from_index < nodes.count && to_index < nodes.count)
        {
            if (pf_snprintf(line, sizeof(line), "    copybook_%lu -> copybook_%lu;\n",
                    static_cast<unsigned long>(from_index),
                    static_cast<unsigned long>(to_index)) < 0)
                goto cleanup;
            if (transpiler_copybook_graph_write_literal(&file, line) != FT_SUCCESS)
                goto cleanup;
        }
        index += 1;
    }
    if (transpiler_copybook_graph_write_literal(&file, "}\n") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (runtime_file_close(&file) != FT_SUCCESS)
        status = FT_FAILURE;
    copybook_collection_dispose(&includes);
    copybook_collection_dispose(&nodes);
    copybook_collection_dispose(&visited);
    copybook_edge_list_dispose(&edges);
    return (status);
}
