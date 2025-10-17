#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

#include "libft/CMA/CMA.hpp"

#include "round_trip_pipeline_helpers.hpp"

static const char *g_message_showcase_expected_output =
    "CBL-C FEATURE SHOWCASE\n"
    "STAGE 1\n"
    "Load shared buffers\n"
    "STAGE 2\n"
    "Copy headline into detail\n"
    "CBL-C FEATURE SHOWCASE\n"
    "STAGE 3\n"
    "Reset headline\n"
    "SHOWCASE COMPLETE\n"
    "STAGE 4\n"
    "Perform numeric calculations\n"
    "Set base count\n"
    "+000000027\n"
    "Set processed count\n"
    "+000000015\n"
    "Combined total (base + processed)\n"
    "+000000042\n"
    "Difference (base - processed)\n"
    "+000000012\n"
    "Double total (combined * 2)\n"
    "+000000084\n"
    "Average batch (combined / 2)\n"
    "+000000021\n"
    "RUN COMPLETE\n";

FT_TEST(test_cblc_message_showcase_translates_to_cobol_and_executes)
{
    t_cblc_translation_unit unit;
    char cblc_buffer[32768];
    char directory[256];
    char source_path[256];
    char binary_path[256];
    char compile_log_path[256];
    char output_path[256];
    char output_buffer[1024];
    char *generated_cobol;
    const char *expected_output;
    const char *log_path;
    const char *expected_cobol;
    int status;
    int directory_created;

    FT_REQUIRE_COBC();
    cblc_translation_unit_init(&unit);
    generated_cobol = NULL;
    directory[0] = '\0';
    source_path[0] = '\0';
    binary_path[0] = '\0';
    compile_log_path[0] = '\0';
    output_path[0] = '\0';
    output_buffer[0] = '\0';
    log_path = NULL;
    directory_created = 0;
    status = FT_FAILURE;
    expected_output = g_message_showcase_expected_output;
    expected_cobol =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MAIN.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 HEADLINE PIC X(40).\n"
        "       01 STATUS-LINE PIC X(32).\n"
        "       01 DETAIL-LINE PIC X(48).\n"
        "       01 BASE-COUNT PIC S9(9).\n"
        "       01 PROCESSED-COUNT PIC S9(9).\n"
        "       01 COMBINED-TOTAL PIC S9(9).\n"
        "       01 DIFFERENCE-TOTAL PIC S9(9).\n"
        "       01 DOUBLED-TOTAL PIC S9(9).\n"
        "       01 AVERAGE-BATCH PIC S9(9).\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           MOVE \"CBL-C FEATURE SHOWCASE\" TO HEADLINE\n"
        "           DISPLAY HEADLINE\n"
        "           MOVE \"STAGE 1\" TO STATUS-LINE\n"
        "           DISPLAY STATUS-LINE\n"
        "           MOVE \"Load shared buffers\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           MOVE \"STAGE 2\" TO STATUS-LINE\n"
        "           DISPLAY STATUS-LINE\n"
        "           MOVE \"Copy headline into detail\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           MOVE HEADLINE TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           MOVE \"STAGE 3\" TO STATUS-LINE\n"
        "           DISPLAY STATUS-LINE\n"
        "           MOVE \"Reset headline\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           MOVE \"SHOWCASE COMPLETE\" TO HEADLINE\n"
        "           DISPLAY HEADLINE\n"
        "           MOVE \"STAGE 4\" TO STATUS-LINE\n"
        "           DISPLAY STATUS-LINE\n"
        "           MOVE \"Perform numeric calculations\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           MOVE \"Set base count\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           COMPUTE BASE-COUNT = 27\n"
        "           DISPLAY BASE-COUNT\n"
        "           MOVE \"Set processed count\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           COMPUTE PROCESSED-COUNT = 15\n"
        "           DISPLAY PROCESSED-COUNT\n"
        "           MOVE \"Combined total (base + processed)\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           COMPUTE COMBINED-TOTAL = BASE-COUNT + PROCESSED-COUNT\n"
        "           DISPLAY COMBINED-TOTAL\n"
        "           MOVE \"Difference (base - processed)\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           COMPUTE DIFFERENCE-TOTAL = BASE-COUNT - PROCESSED-COUNT\n"
        "           DISPLAY DIFFERENCE-TOTAL\n"
        "           MOVE \"Double total (combined * 2)\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           COMPUTE DOUBLED-TOTAL = COMBINED-TOTAL * 2\n"
        "           DISPLAY DOUBLED-TOTAL\n"
        "           MOVE \"Average batch (combined / 2)\" TO DETAIL-LINE\n"
        "           DISPLAY DETAIL-LINE\n"
        "           COMPUTE AVERAGE-BATCH = COMBINED-TOTAL / 2\n"
        "           DISPLAY AVERAGE-BATCH\n"
        "           DISPLAY \"RUN COMPLETE\"\n"
        "           STOP RUN.\n"
        "\n";
    if (test_read_text_file("samples/feature_showcase/message_showcase.cblc", cblc_buffer,
            sizeof(cblc_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_parse_translation_unit(cblc_buffer, &unit),
            "message_showcase.cblc should parse") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(cblc_generate_cobol(&unit, &generated_cobol),
            "message_showcase.cblc should convert to COBOL") != FT_SUCCESS)
        goto cleanup;
    if (test_expect_success(transpiler_validate_generated_cobol(generated_cobol),
            "generated COBOL should validate") != FT_SUCCESS)
        goto cleanup;
    if (ft_strncmp(generated_cobol, expected_cobol,
            ft_strlen(expected_cobol) + 1) != 0)
    {
        pf_printf("Assertion failed: translated message_showcase COBOL should match expected text\n");
        goto cleanup;
    }
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        goto cleanup;
    directory_created = 1;
    if (test_join_path(directory, "message_showcase_generated.cob", source_path,
            sizeof(source_path)) != FT_SUCCESS)
        goto cleanup;
    if (test_write_text_file(source_path, generated_cobol) != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "message_showcase_compile.log", compile_log_path,
            sizeof(compile_log_path)) != FT_SUCCESS)
        goto cleanup;
    log_path = compile_log_path;
    if (test_join_path(directory, "message_showcase_generated.bin", binary_path,
            sizeof(binary_path)) != FT_SUCCESS)
        goto cleanup;
    if (compile_generated_cobol(binary_path, source_path, compile_log_path,
            "cobc should compile translated message_showcase program") != FT_SUCCESS)
        goto cleanup;
    if (test_join_path(directory, "message_showcase_output.txt", output_path,
            sizeof(output_path)) != FT_SUCCESS)
        goto cleanup;
    if (execute_binary(directory, "message_showcase_generated.bin",
            "message_showcase_output.txt",
            "translated message_showcase binary should execute successfully") != FT_SUCCESS)
        goto cleanup;
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_equal(output_buffer, expected_output) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: translated message_showcase binary should emit expected output\\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (directory_created)
    {
        test_cleanup_example_artifacts_with_log(source_path, binary_path, output_path,
            log_path);
        test_remove_directory(directory);
    }
    if (generated_cobol)
        cma_free(generated_cobol);
    cblc_translation_unit_dispose(&unit);
    return (status);
}

FT_TEST(test_message_showcase_sample_make_pipeline)
{
    const char *expected_output;
    const char *clean_command;
    char transcript[2048];
    int cleanup_status;
    int command_status;
    int status;

    clean_command = "cd samples/feature_showcase && make clean";
    expected_output = g_message_showcase_expected_output;
    status = FT_FAILURE;
    cleanup_status = 0;
    command_status = 0;
    if (test_run_command_capture_status(clean_command, &command_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected to invoke sample clean pipeline\\n");
        goto cleanup;
    }
    if (command_status != 0)
    {
        pf_printf("Assertion failed: sample clean pipeline exited with status %d\\n",
            command_status);
        goto cleanup;
    }
    if (!test_cobc_available())
    {
        status = FT_SUCCESS;
        goto cleanup;
    }
    test_remove_file("samples/feature_showcase/message_output.txt");
    if (test_run_command_capture_status("cd samples/feature_showcase && make", &command_status)
        != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected to invoke sample make pipeline\\n");
        goto cleanup;
    }
    if (command_status != 0)
    {
        pf_printf("Assertion failed: sample make pipeline exited with status %d\\n",
            command_status);
        goto cleanup;
    }
    if (test_run_command_capture_status(
            "cd samples/feature_showcase && ./message_showcase > message_output.txt",
            &command_status)
        != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected to execute sample showcase binary\\n");
        goto cleanup;
    }
    if (command_status != 0)
    {
        pf_printf("Assertion failed: sample showcase binary exited with status %d\\n",
            command_status);
        goto cleanup;
    }
    if (test_read_text_file("samples/feature_showcase/message_output.txt", transcript,
            sizeof(transcript)) != FT_SUCCESS)
        goto cleanup;
    if (test_expect_transcript_equal(transcript, expected_output) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: sample showcase transcript should match expected output\\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    test_remove_file("samples/feature_showcase/message_output.txt");
    if (test_run_command_capture_status(clean_command, &cleanup_status) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected to clean sample showcase artifacts\\n");
        status = FT_FAILURE;
    }
    else if (cleanup_status != 0)
    {
        pf_printf("Assertion failed: sample showcase clean exited with status %d\\n",
            cleanup_status);
        status = FT_FAILURE;
    }
    return (status);
}
