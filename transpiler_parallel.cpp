#include <atomic>
#include <cstdlib>
#include <new>
#include <thread>

#include "cblc_transpiler.hpp"

#include "libft/CMA/CMA.hpp"
#include "libft/Printf/printf.hpp"

typedef struct s_transpiler_parallel_worker_context
{
    const t_cblc_translation_unit *const *units;
    const char *const *source_paths;
    t_transpiler_parallel_result *results;
    size_t job_count;
    std::atomic<size_t> *next_index;
}   t_transpiler_parallel_worker_context;

static int transpiler_parallel_parse_worker_count(const char *value, size_t *out_count)
{
    size_t index;
    size_t result;

    if (!value || !out_count)
        return (FT_FAILURE);
    if (value[0] == '\0')
        return (FT_FAILURE);
    result = 0;
    index = 0;
    while (value[index] != '\0')
    {
        char digit;

        digit = value[index];
        if (digit < '0' || digit > '9')
            return (FT_FAILURE);
        result = result * 10 + static_cast<size_t>(digit - '0');
        index += 1;
    }
    if (result == 0)
        return (FT_FAILURE);
    *out_count = result;
    return (FT_SUCCESS);
}

static size_t transpiler_parallel_detect_worker_count(size_t job_count)
{
    const char *env_value;
    size_t parsed;
    size_t count;
    unsigned int hardware;

    env_value = std::getenv("CTOC_PARALLELISM");
    parsed = 0;
    if (env_value && env_value[0] != '\0')
    {
        if (transpiler_parallel_parse_worker_count(env_value, &parsed) != FT_SUCCESS)
            parsed = 0;
    }
    if (parsed > 0)
        count = parsed;
    else
    {
        hardware = std::thread::hardware_concurrency();
        if (hardware == 0)
            hardware = 1;
        count = hardware;
    }
    if (count > job_count)
        count = job_count;
    if (count == 0)
        count = 1;
    return (count);
}

static void transpiler_parallel_execute_job(t_transpiler_parallel_result *result,
    const t_cblc_translation_unit *unit, const char *source_path)
{
    char *generated_text;

    if (!result || !unit)
        return ;
    generated_text = NULL;
    result->status = FT_FAILURE;
    result->error_message[0] = '\0';
    if (cblc_generate_cobol(unit, &generated_text) != FT_SUCCESS)
    {
        if (source_path && pf_snprintf(result->error_message,
                sizeof(result->error_message),
                "Failed to generate COBOL for '%s'", source_path) < 0)
            result->error_message[0] = '\0';
        if (generated_text)
            cma_free(generated_text);
        return ;
    }
    if (transpiler_validate_generated_cobol(generated_text) != FT_SUCCESS)
    {
        if (source_path && pf_snprintf(result->error_message,
                sizeof(result->error_message),
                "Generated COBOL failed validation for '%s'", source_path) < 0)
            result->error_message[0] = '\0';
        if (generated_text)
            cma_free(generated_text);
        return ;
    }
    result->text = generated_text;
    result->status = FT_SUCCESS;
}

static void transpiler_parallel_worker_run(t_transpiler_parallel_worker_context *context)
{
    size_t index;

    if (!context)
        return ;
    while (1)
    {
        index = context->next_index->fetch_add(1);
        if (index >= context->job_count)
            break ;
        transpiler_parallel_execute_job(&context->results[index],
            context->units[index], context->source_paths[index]);
    }
}

int transpiler_parallel_generate_cobol(const t_cblc_translation_unit *const *units,
    const char *const *source_paths, size_t job_count,
    t_transpiler_parallel_result **out_results)
{
    t_transpiler_parallel_result *results;
    t_transpiler_parallel_worker_context worker_context;
    std::thread *threads;
    std::atomic<size_t> next_index(0);
    size_t worker_count;
    size_t index;
    int overall_status;

    if (!out_results)
        return (FT_FAILURE);
    *out_results = NULL;
    if (job_count == 0)
        return (FT_SUCCESS);
    if (!units || !source_paths)
        return (FT_FAILURE);
    results = static_cast<t_transpiler_parallel_result *>(cma_calloc(job_count,
        sizeof(*results)));
    if (!results)
        return (FT_FAILURE);
    index = 0;
    while (index < job_count)
    {
        results[index].text = NULL;
        results[index].status = FT_FAILURE;
        results[index].error_message[0] = '\0';
        index += 1;
    }
    worker_context.units = units;
    worker_context.source_paths = source_paths;
    worker_context.results = results;
    worker_context.job_count = job_count;
    worker_context.next_index = &next_index;
    worker_count = transpiler_parallel_detect_worker_count(job_count);
    threads = NULL;
    if (worker_count <= 1)
    {
        while (1)
        {
            size_t job_index;

            job_index = worker_context.next_index->fetch_add(1);
            if (job_index >= job_count)
                break ;
            transpiler_parallel_execute_job(&results[job_index], units[job_index],
                source_paths[job_index]);
        }
    }
    else
    {
        threads = new (std::nothrow) std::thread[worker_count];
        if (!threads)
        {
            transpiler_parallel_results_dispose(results, job_count);
            return (FT_FAILURE);
        }
        index = 0;
        while (index < worker_count)
        {
            threads[index] = std::thread(transpiler_parallel_worker_run, &worker_context);
            index += 1;
        }
        index = 0;
        while (index < worker_count)
        {
            if (threads[index].joinable())
                threads[index].join();
            index += 1;
        }
        delete [] threads;
    }
    overall_status = FT_SUCCESS;
    index = 0;
    while (index < job_count)
    {
        if (results[index].status != FT_SUCCESS)
            overall_status = FT_FAILURE;
        index += 1;
    }
    *out_results = results;
    return (overall_status);
}

void transpiler_parallel_results_dispose(t_transpiler_parallel_result *results, size_t job_count)
{
    size_t index;

    if (!results)
        return ;
    index = 0;
    while (index < job_count)
    {
        if (results[index].text)
            cma_free(results[index].text);
        index += 1;
    }
    cma_free(results);
}
