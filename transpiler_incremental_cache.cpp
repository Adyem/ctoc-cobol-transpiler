#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <system_error>

#include "libft/CMA/CMA.hpp"
#include "cblc_transpiler.hpp"

static void incremental_cache_entry_reset(t_transpiler_incremental_cache_entry *entry)
{
    if (!entry)
        return ;
    entry->input_path[0] = '\0';
    entry->output_path[0] = '\0';
    entry->ast_path[0] = '\0';
    entry->input_timestamp = 0;
    entry->input_size = 0;
    entry->output_timestamp = 0;
    entry->output_size = 0;
    entry->ast_timestamp = 0;
    entry->ast_size = 0;
    entry->copybook_signature = 0;
}

static int incremental_cache_reserve_entries(t_transpiler_incremental_cache *cache, size_t desired_capacity)
{
    t_transpiler_incremental_cache_entry *entries;

    if (!cache)
        return (FT_FAILURE);
    if (cache->entry_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    entries = static_cast<t_transpiler_incremental_cache_entry *>(cma_calloc(desired_capacity,
        sizeof(t_transpiler_incremental_cache_entry)));
    if (!entries)
        return (FT_FAILURE);
    if (cache->entries)
    {
        size_t index;

        index = 0;
        while (index < cache->entry_count)
        {
            entries[index] = cache->entries[index];
            index += 1;
        }
        cma_free(cache->entries);
    }
    cache->entries = entries;
    cache->entry_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static long long incremental_cache_convert_timestamp(const std::filesystem::file_time_type &time)
{
    std::chrono::nanoseconds duration;

    duration = std::chrono::duration_cast<std::chrono::nanoseconds>(time.time_since_epoch());
    return (duration.count());
}

static int incremental_cache_probe_file(const char *path, long long *timestamp, unsigned long long *size)
{
    std::error_code error;
    std::filesystem::path fs_path;
    std::filesystem::file_time_type time;
    std::uintmax_t length;

    if (timestamp)
        *timestamp = 0;
    if (size)
        *size = 0;
    if (!path)
        return (0);
    if (path[0] == '\0')
        return (0);
    fs_path = std::filesystem::path(path);
    if (!std::filesystem::exists(fs_path, error) || error)
        return (0);
    time = std::filesystem::last_write_time(fs_path, error);
    if (error)
        return (0);
    length = std::filesystem::file_size(fs_path, error);
    if (error)
        return (0);
    if (timestamp)
        *timestamp = incremental_cache_convert_timestamp(time);
    if (size)
        *size = static_cast<unsigned long long>(length);
    return (1);
}

static t_transpiler_incremental_cache_entry *incremental_cache_find_entry(t_transpiler_incremental_cache *cache,
    const char *input_path, const char *output_path)
{
    size_t index;

    if (!cache)
        return (NULL);
    if (!input_path || !output_path)
        return (NULL);
    index = 0;
    while (index < cache->entry_count)
    {
        if (ft_strncmp(cache->entries[index].input_path, input_path, TRANSPILE_FILE_PATH_MAX) == 0
            && ft_strncmp(cache->entries[index].output_path, output_path, TRANSPILE_FILE_PATH_MAX) == 0)
            return (&cache->entries[index]);
        index += 1;
    }
    return (NULL);
}

static int incremental_cache_parse_line(const std::string &line, t_transpiler_incremental_cache_entry *entry)
{
    std::string fields[10];
    size_t field_index;
    size_t cursor;
    size_t separator;

    if (!entry)
        return (FT_FAILURE);
    if (line.empty())
        return (FT_FAILURE);
    if (line[0] == '#')
        return (FT_FAILURE);
    incremental_cache_entry_reset(entry);
    field_index = 0;
    cursor = 0;
    while (cursor <= line.size() && field_index < 10)
    {
        separator = line.find('|', cursor);
        if (separator == std::string::npos)
        {
            fields[field_index] = line.substr(cursor);
            field_index += 1;
            break ;
        }
        fields[field_index] = line.substr(cursor, separator - cursor);
        field_index += 1;
        cursor = separator + 1;
    }
    if (field_index != 10)
        return (FT_FAILURE);
    ft_strlcpy(entry->input_path, fields[0].c_str(), TRANSPILE_FILE_PATH_MAX);
    ft_strlcpy(entry->output_path, fields[1].c_str(), TRANSPILE_FILE_PATH_MAX);
    ft_strlcpy(entry->ast_path, fields[2].c_str(), TRANSPILE_FILE_PATH_MAX);
    entry->input_timestamp = std::strtoll(fields[3].c_str(), NULL, 10);
    entry->input_size = std::strtoull(fields[4].c_str(), NULL, 10);
    entry->output_timestamp = std::strtoll(fields[5].c_str(), NULL, 10);
    entry->output_size = std::strtoull(fields[6].c_str(), NULL, 10);
    entry->ast_timestamp = std::strtoll(fields[7].c_str(), NULL, 10);
    entry->ast_size = std::strtoull(fields[8].c_str(), NULL, 10);
    entry->copybook_signature = std::strtoull(fields[9].c_str(), NULL, 10);
    return (FT_SUCCESS);
}

int transpiler_incremental_cache_init(t_transpiler_incremental_cache *cache)
{
    if (!cache)
        return (FT_FAILURE);
    cache->entries = NULL;
    cache->entry_count = 0;
    cache->entry_capacity = 0;
    cache->manifest_path[0] = '\0';
    cache->dirty = 0;
    cache->loaded = 0;
    return (FT_SUCCESS);
}

void transpiler_incremental_cache_dispose(t_transpiler_incremental_cache *cache)
{
    if (!cache)
        return ;
    if (cache->entries)
        cma_free(cache->entries);
    cache->entries = NULL;
    cache->entry_count = 0;
    cache->entry_capacity = 0;
    cache->manifest_path[0] = '\0';
    cache->dirty = 0;
    cache->loaded = 0;
}

int transpiler_incremental_cache_set_manifest(t_transpiler_incremental_cache *cache, const char *path)
{
    if (!cache)
        return (FT_FAILURE);
    if (!path)
        return (FT_FAILURE);
    if (ft_strlcpy(cache->manifest_path, path, TRANSPILE_FILE_PATH_MAX) >= TRANSPILE_FILE_PATH_MAX)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int transpiler_incremental_cache_load(t_transpiler_incremental_cache *cache)
{
    std::ifstream stream;
    std::string line;

    if (!cache)
        return (FT_FAILURE);
    cache->entry_count = 0;
    cache->dirty = 0;
    if (cache->manifest_path[0] == '\0')
    {
        cache->loaded = 1;
        return (FT_SUCCESS);
    }
    if (!std::filesystem::exists(cache->manifest_path))
    {
        cache->loaded = 1;
        return (FT_SUCCESS);
    }
    stream.open(cache->manifest_path);
    if (!stream.is_open())
        return (FT_FAILURE);
    while (std::getline(stream, line))
    {
        t_transpiler_incremental_cache_entry entry;

        if (incremental_cache_parse_line(line, &entry) != FT_SUCCESS)
            continue ;
        if (incremental_cache_reserve_entries(cache, cache->entry_count + 1) != FT_SUCCESS)
        {
            stream.close();
            return (FT_FAILURE);
        }
        cache->entries[cache->entry_count] = entry;
        cache->entry_count += 1;
    }
    stream.close();
    cache->loaded = 1;
    return (FT_SUCCESS);
}

static int incremental_cache_write_manifest_directory(const char *path)
{
    std::filesystem::path fs_path;
    std::filesystem::path parent;
    std::error_code error;

    if (!path)
        return (FT_FAILURE);
    fs_path = std::filesystem::path(path);
    parent = fs_path.parent_path();
    if (parent.empty())
        return (FT_SUCCESS);
    std::filesystem::create_directories(parent, error);
    if (error)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int transpiler_incremental_cache_save(t_transpiler_incremental_cache *cache)
{
    std::ofstream stream;
    size_t index;

    if (!cache)
        return (FT_FAILURE);
    if (cache->manifest_path[0] == '\0')
        return (FT_SUCCESS);
    if (!cache->dirty)
        return (FT_SUCCESS);
    if (incremental_cache_write_manifest_directory(cache->manifest_path) != FT_SUCCESS)
        return (FT_FAILURE);
    stream.open(cache->manifest_path, std::ios::out | std::ios::trunc);
    if (!stream.is_open())
        return (FT_FAILURE);
    index = 0;
    while (index < cache->entry_count)
    {
        const t_transpiler_incremental_cache_entry *entry;

        entry = &cache->entries[index];
        stream << entry->input_path << '|' << entry->output_path << '|' << entry->ast_path << '|'
            << entry->input_timestamp << '|' << entry->input_size << '|' << entry->output_timestamp << '|'
            << entry->output_size << '|' << entry->ast_timestamp << '|' << entry->ast_size << '|'
            << entry->copybook_signature << '\n';
        index += 1;
    }
    stream.close();
    cache->dirty = 0;
    return (FT_SUCCESS);
}

int transpiler_incremental_cache_should_skip(t_transpiler_incremental_cache *cache, const char *input_path,
    const char *output_path, unsigned long long copybook_signature, int *should_skip)
{
    t_transpiler_incremental_cache_entry *entry;
    long long timestamp;
    unsigned long long size;

    if (should_skip)
        *should_skip = 0;
    if (!cache)
        return (FT_FAILURE);
    if (!cache->loaded)
        return (FT_SUCCESS);
    entry = incremental_cache_find_entry(cache, input_path, output_path);
    if (!entry)
        return (FT_SUCCESS);
    if (!incremental_cache_probe_file(input_path, &timestamp, &size))
        return (FT_SUCCESS);
    if (timestamp != entry->input_timestamp || size != entry->input_size)
        return (FT_SUCCESS);
    if (!incremental_cache_probe_file(output_path, &timestamp, &size))
        return (FT_SUCCESS);
    if (timestamp != entry->output_timestamp || size != entry->output_size)
        return (FT_SUCCESS);
    if (entry->ast_path[0] != '\0')
    {
        if (!incremental_cache_probe_file(entry->ast_path, &timestamp, &size))
            return (FT_SUCCESS);
        if (timestamp != entry->ast_timestamp || size != entry->ast_size)
            return (FT_SUCCESS);
    }
    if (entry->copybook_signature != copybook_signature)
        return (FT_SUCCESS);
    if (should_skip)
        *should_skip = 1;
    return (FT_SUCCESS);
}

static void incremental_cache_update_entry(t_transpiler_incremental_cache_entry *entry, const char *input_path,
    const char *output_path, const char *ast_path)
{
    ft_strlcpy(entry->input_path, input_path, TRANSPILE_FILE_PATH_MAX);
    ft_strlcpy(entry->output_path, output_path, TRANSPILE_FILE_PATH_MAX);
    if (ast_path)
        ft_strlcpy(entry->ast_path, ast_path, TRANSPILE_FILE_PATH_MAX);
    else
        entry->ast_path[0] = '\0';
}

int transpiler_incremental_cache_record(t_transpiler_incremental_cache *cache, const char *input_path,
    const char *output_path, const char *ast_path, unsigned long long copybook_signature)
{
    t_transpiler_incremental_cache_entry *entry;
    long long input_timestamp;
    unsigned long long input_size;
    long long output_timestamp;
    unsigned long long output_size;
    long long ast_timestamp;
    unsigned long long ast_size;

    if (!cache)
        return (FT_FAILURE);
    if (!cache->loaded)
        return (FT_SUCCESS);
    if (!incremental_cache_probe_file(input_path, &input_timestamp, &input_size))
        return (FT_FAILURE);
    if (!incremental_cache_probe_file(output_path, &output_timestamp, &output_size))
        return (FT_FAILURE);
    ast_timestamp = 0;
    ast_size = 0;
    if (ast_path && ast_path[0] != '\0')
    {
        if (!incremental_cache_probe_file(ast_path, &ast_timestamp, &ast_size))
            return (FT_FAILURE);
    }
    entry = incremental_cache_find_entry(cache, input_path, output_path);
    if (!entry)
    {
        if (incremental_cache_reserve_entries(cache, cache->entry_count + 1) != FT_SUCCESS)
            return (FT_FAILURE);
        entry = &cache->entries[cache->entry_count];
        incremental_cache_entry_reset(entry);
        cache->entry_count += 1;
    }
    incremental_cache_update_entry(entry, input_path, output_path, ast_path);
    entry->input_timestamp = input_timestamp;
    entry->input_size = input_size;
    entry->output_timestamp = output_timestamp;
    entry->output_size = output_size;
    entry->ast_timestamp = ast_timestamp;
    entry->ast_size = ast_size;
    entry->copybook_signature = copybook_signature;
    cache->dirty = 1;
    return (FT_SUCCESS);
}
