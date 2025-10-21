#include "../../test_suites.hpp"

#include "../compiler_test_support.hpp"

#include "libft/CMA/CMA.hpp"

#include "round_trip_pipeline_helpers.hpp"

static const char *g_message_showcase_expected_output =
    "DAILY INSURANCE SNAPSHOT\n"
    "STEP 1\n"
    "Load underwriting context\n"
    "Detail summary: Load underwriting context\n"
    "STEP 2\n"
    "Sync policy counters\n"
    "Detail summary: Sync policy counters\n"
    "Copy header into report buffer\n"
    "DAILY INSURANCE SNAPSHOT\n"
    "STEP 3\n"
    "Evaluate reserve impact\n"
    "Reserve impact review complete\n"
    "Policies in force\n"
    "+000001250\n"
    "Claims reported\n"
    "+000000032\n"
    "Open investigations\n"
    "+000000005\n"
    "Loss ratio percent\n"
    "+000000015\n"
    "Claim clearance delta\n"
    "+000000027\n"
    "STEP 4\n"
    "All metrics published\n"
    "AUDIT COMPLETE\n";

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
    FT_REQUIRE_FORWARD_TRANSLATION();
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
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MAIN.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 AUDIT-HEADER PIC X(40).\n"
        "       01 STAGE-LABEL PIC X(32).\n"
        "       01 STAGE-DETAIL PIC X(64).\n"
        "       01 REPORT-LINE PIC X(64).\n"
        "       01 DETAIL-SUMMARY PIC X(64).\n"
        "       01 POLICIES-IN-FORCE PIC S9(9).\n"
        "       01 CLAIMS-REPORTED PIC S9(9).\n"
        "       01 OPEN-INVESTIGATIONS PIC S9(9).\n"
        "       01 NET-PREMIUM PIC S9(9).\n"
        "       01 CLAIM-PAYOUT PIC S9(9).\n"
        "       01 LOSS-RATIO-PERCENT PIC S9(9).\n"
        "       01 CLAIM-CLEARANCE-DELTA PIC S9(9).\n"
        "       01 PAYOUT-SCALED PIC S9(9).\n"
        "       01 AUDIT-HEADER-LENGTH PIC S9(9).\n"
        "       01 REPORT-LINE-LENGTH PIC S9(9).\n"
        "       01 CBLC-LITERAL-1 PIC X(24).\n"
        "       01 CBLC-HELPER-STATUS PIC S9(9).\n"
        "       01 CBLC-LITERAL-2 PIC X(6).\n"
        "       01 CBLC-LITERAL-3 PIC X(25).\n"
        "       01 CBLC-LITERAL-4 PIC X(41).\n"
        "       01 CBLC-LITERAL-5 PIC X(6).\n"
        "       01 CBLC-LITERAL-6 PIC X(20).\n"
        "       01 CBLC-LITERAL-7 PIC X(36).\n"
        "       01 CBLC-LITERAL-8 PIC X(30).\n"
        "       01 CBLC-LITERAL-9 PIC X(6).\n"
        "       01 CBLC-LITERAL-10 PIC X(23).\n"
        "       01 CBLC-LITERAL-11 PIC X(30).\n"
        "       01 CBLC-LITERAL-12 PIC X(17).\n"
        "       01 CBLC-LITERAL-13 PIC X(15).\n"
        "       01 CBLC-LITERAL-14 PIC X(19).\n"
        "       01 CBLC-LITERAL-15 PIC X(18).\n"
        "       01 CBLC-LITERAL-16 PIC X(21).\n"
        "       01 CBLC-LITERAL-17 PIC X(6).\n"
        "       01 CBLC-LITERAL-18 PIC X(21).\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           MOVE \"DAILY INSURANCE SNAPSHOT\" TO CBLC-LITERAL-1\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE AUDIT-HEADER BY VALUE 40 BY REFERENCE CBLC-LITERAL-1 BY VALUE 24 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY AUDIT-HEADER\n"
        "           CALL 'CBLC-STRLEN' USING BY REFERENCE AUDIT-HEADER BY VALUE 40 BY REFERENCE AUDIT-HEADER-LENGTH\n"
        "           DISPLAY \"Header length\"\n"
        "           DISPLAY AUDIT-HEADER-LENGTH\n"
        "           MOVE \"STEP 1\" TO CBLC-LITERAL-2\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE STAGE-LABEL BY VALUE 32 BY REFERENCE CBLC-LITERAL-2 BY VALUE 6 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY STAGE-LABEL\n"
        "           MOVE \"Load underwriting context\" TO CBLC-LITERAL-3\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE STAGE-DETAIL BY VALUE 64 BY REFERENCE CBLC-LITERAL-3 BY VALUE 25 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY STAGE-DETAIL\n"
        "           MOVE \"Detail summary: Load underwriting context\" TO CBLC-LITERAL-4\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE DETAIL-SUMMARY BY VALUE 64 BY REFERENCE CBLC-LITERAL-4 BY VALUE 41 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY DETAIL-SUMMARY\n"
        "           MOVE \"STEP 2\" TO CBLC-LITERAL-5\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE STAGE-LABEL BY VALUE 32 BY REFERENCE CBLC-LITERAL-5 BY VALUE 6 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY STAGE-LABEL\n"
        "           MOVE \"Sync policy counters\" TO CBLC-LITERAL-6\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE STAGE-DETAIL BY VALUE 64 BY REFERENCE CBLC-LITERAL-6 BY VALUE 20 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY STAGE-DETAIL\n"
        "           MOVE \"Detail summary: Sync policy counters\" TO CBLC-LITERAL-7\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE DETAIL-SUMMARY BY VALUE 64 BY REFERENCE CBLC-LITERAL-7 BY VALUE 36 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY DETAIL-SUMMARY\n"
        "           COMPUTE POLICIES-IN-FORCE = 1250\n"
        "           COMPUTE CLAIMS-REPORTED = 32\n"
        "           COMPUTE OPEN-INVESTIGATIONS = 5\n"
        "           MOVE \"Copy header into report buffer\" TO CBLC-LITERAL-8\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE STAGE-DETAIL BY VALUE 64 BY REFERENCE CBLC-LITERAL-8 BY VALUE 30 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY STAGE-DETAIL\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE REPORT-LINE BY VALUE 64 BY REFERENCE AUDIT-HEADER BY VALUE 40 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY REPORT-LINE\n"
        "           CALL 'CBLC-STRLEN' USING BY REFERENCE REPORT-LINE BY VALUE 64 BY REFERENCE REPORT-LINE-LENGTH\n"
        "           DISPLAY \"Report line length\"\n"
        "           DISPLAY REPORT-LINE-LENGTH\n"
        "           MOVE \"STEP 3\" TO CBLC-LITERAL-9\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE STAGE-LABEL BY VALUE 32 BY REFERENCE CBLC-LITERAL-9 BY VALUE 6 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY STAGE-LABEL\n"
        "           MOVE \"Evaluate reserve impact\" TO CBLC-LITERAL-10\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE STAGE-DETAIL BY VALUE 64 BY REFERENCE CBLC-LITERAL-10 BY VALUE 23 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY STAGE-DETAIL\n"
        "           MOVE \"Reserve impact review complete\" TO CBLC-LITERAL-11\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE DETAIL-SUMMARY BY VALUE 64 BY REFERENCE CBLC-LITERAL-11 BY VALUE 30 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY DETAIL-SUMMARY\n"
        "           COMPUTE NET-PREMIUM = 480000\n"
        "           COMPUTE CLAIM-PAYOUT = 72000\n"
        "           COMPUTE PAYOUT-SCALED = CLAIM-PAYOUT * 100\n"
        "           COMPUTE LOSS-RATIO-PERCENT = PAYOUT-SCALED / NET-PREMIUM\n"
        "           COMPUTE CLAIM-CLEARANCE-DELTA = CLAIMS-REPORTED - OPEN-INVESTIGATIONS\n"
        "           MOVE \"Policies in force\" TO CBLC-LITERAL-12\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE REPORT-LINE BY VALUE 64 BY REFERENCE CBLC-LITERAL-12 BY VALUE 17 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY REPORT-LINE\n"
        "           DISPLAY POLICIES-IN-FORCE\n"
        "           MOVE \"Claims reported\" TO CBLC-LITERAL-13\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE REPORT-LINE BY VALUE 64 BY REFERENCE CBLC-LITERAL-13 BY VALUE 15 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY REPORT-LINE\n"
        "           DISPLAY CLAIMS-REPORTED\n"
        "           MOVE \"Open investigations\" TO CBLC-LITERAL-14\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE REPORT-LINE BY VALUE 64 BY REFERENCE CBLC-LITERAL-14 BY VALUE 19 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY REPORT-LINE\n"
        "           DISPLAY OPEN-INVESTIGATIONS\n"
        "           MOVE \"Loss ratio percent\" TO CBLC-LITERAL-15\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE REPORT-LINE BY VALUE 64 BY REFERENCE CBLC-LITERAL-15 BY VALUE 18 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY REPORT-LINE\n"
        "           DISPLAY LOSS-RATIO-PERCENT\n"
        "           MOVE \"Claim clearance delta\" TO CBLC-LITERAL-16\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE REPORT-LINE BY VALUE 64 BY REFERENCE CBLC-LITERAL-16 BY VALUE 21 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY REPORT-LINE\n"
        "           DISPLAY CLAIM-CLEARANCE-DELTA\n"
        "           MOVE \"STEP 4\" TO CBLC-LITERAL-17\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE STAGE-LABEL BY VALUE 32 BY REFERENCE CBLC-LITERAL-17 BY VALUE 6 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY STAGE-LABEL\n"
        "           MOVE \"All metrics published\" TO CBLC-LITERAL-18\n"
        "           CALL 'CBLC-STRCPY' USING BY REFERENCE STAGE-DETAIL BY VALUE 64 BY REFERENCE CBLC-LITERAL-18 BY VALUE 21 BY REFERENCE CBLC-HELPER-STATUS\n"
        "           DISPLAY STAGE-DETAIL\n"
        "           DISPLAY \"AUDIT COMPLETE\"\n"
        "           STOP RUN.\n"
        "\n"
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

    FT_REQUIRE_COBC();
    FT_REQUIRE_FORWARD_TRANSLATION();
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
