#include "test_suites.hpp"

static int golden_expect_file_matches(const char *path, const char *expected,
    const char *message)
{
    char buffer[4096];

    if (test_read_text_file(path, buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_cstring_equal(buffer, expected, message) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_copy_file_matches_golden)
{
    static const char expected[] =
        "file in \"input.txt\";\n"
        "file out \"output.txt\";\n"
        "char line[256];\n\n"
        "function process_file() {\n"
        "    open(in, \"r\");\n"
        "    open(out, \"w\");\n"
        "    while (read(in, line)) {\n"
        "        write(out, line);\n"
        "    }\n"
        "    close(in);\n"
        "    close(out);\n"
        "}\n\n"
        "function main() {\n"
        "    process_file();\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/copy_file.cblc", expected,
            "copy_file.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_filter_prefix_matches_golden)
{
    static const char expected[] =
        "file in \"input.txt\";\n"
        "file out \"filtered.txt\";\n"
        "char line[128];\n\n"
        "function filter_prefix() {\n"
        "    open(in, \"r\");\n"
        "    open(out, \"w\");\n"
        "    while (read(in, line)) {\n"
        "        if (starts_with(line, \"ERR\")) {\n"
        "            write(out, line);\n"
        "        }\n"
        "    }\n"
        "    close(in);\n"
        "    close(out);\n"
        "}\n\n"
        "function main() {\n"
        "    filter_prefix();\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/filter_prefix.cblc", expected,
            "filter_prefix.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_record_writer_matches_golden)
{
    static const char expected[] =
        "record Person {\n"
        "    char name[40];\n"
        "    char id[10];\n"
        "};\n\n"
        "file people \"people.dat\";\n"
        "Person person;\n\n"
        "function write_records() {\n"
        "    person.name = \"ALICE\";\n"
        "    person.id = \"0001\";\n\n"
        "    open(people, \"w\");\n"
        "    write(people, person);\n"
        "    close(people);\n"
        "}\n\n"
        "function main() {\n"
        "    write_records();\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/record_writer.cblc", expected,
            "record_writer.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_record_summary_matches_golden)
{
    static const char expected[] =
        "record RecordEntry {\n"
        "    char status[2];\n"
        "    int amount;\n"
        "};\n\n"
        "file input \"records.dat\";\n"
        "RecordEntry entry;\n"
        "int accepted_count;\n"
        "int total_amount;\n\n"
        "function summarize_records() {\n"
        "    accepted_count = 0;\n"
        "    total_amount = 0;\n\n"
        "    open(input, \"r\");\n"
        "    while (read(input, entry)) {\n"
        "        if (starts_with(entry.status, \"A\")) {\n"
        "            accepted_count = accepted_count + 1;\n"
        "            total_amount = total_amount + entry.amount;\n"
        "        }\n"
        "    }\n"
        "    close(input);\n"
        "}\n\n"
        "function main() {\n"
        "    summarize_records();\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/record_summary.cblc", expected,
            "record_summary.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_reverse_constructs_matches_golden)
{
    static const char expected[] =
        "function MAIN() {\n"
        "    open(INPUT_FILE, \"r\");\n"
        "    while (!(EOF_FLAG == true)) {\n"
        "        read(INPUT_FILE, OUTPUT_RECORD);\n"
        "        if (EOF_FLAG == false) {\n"
        "            write(OUTPUT_FILE, OUTPUT_RECORD);\n"
        "        } else {\n"
        "            EOF_FLAG = true;\n"
        "        }\n"
        "    }\n"
        "    close(INPUT_FILE);\n"
        "    return ;\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/reverse_constructs.cblc", expected,
            "reverse_constructs.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_reverse_normalization_matches_golden)
{
    static const char expected[] =
        "function ENTRY_PARAGRAPH() {\n"
        "    SCRATCH_NOTE = \"mixED Case value\";\n"
        "    RUNNING_TOTAL_VALUE = 0;\n"
        "    STATUS_FLAG = true;\n"
        "    return ;\n"
        "}\n\n"
        "function NORMALIZE_VALUES() {\n"
        "    RUNNING_TOTAL_VALUE = 7;\n"
        "    SCRATCH_NOTE = \"done\";\n"
        "    return ;\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/reverse_normalization.cblc", expected,
            "reverse_normalization.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_reverse_control_flow_matches_golden)
{
    static const char expected[] =
        "function MAIN() {\n"
        "    if (!(FLAG == true)) {\n"
        "        while (!(COUNT > LIMIT)) {\n"
        "            LIMIT = COUNT;\n"
        "        }\n"
        "    } else {\n"
        "        INDEX = 0;\n"
        "        while (!(INDEX >= LIMIT)) {\n"
        "            RESULT = INDEX;\n"
        "            INDEX++;\n"
        "        }\n"
        "    }\n"
        "    return ;\n"
        "}\n\n"
        "function NEXT() {\n"
        "    FLAG = true;\n"
        "    return ;\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/reverse_control_flow.cblc", expected,
            "reverse_control_flow.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_copy_file_matches_golden)
{
    static const char expected[] =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. COPY-FILE.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       INPUT-OUTPUT SECTION.\n"
        "       FILE-CONTROL.\n"
        "           SELECT INPUT-FILE ASSIGN TO \"input.dat\".\n"
        "           SELECT OUTPUT-FILE ASSIGN TO \"output.dat\".\n"
        "       DATA DIVISION.\n"
        "       FILE SECTION.\n"
        "       FD  INPUT-FILE.\n"
        "       01  INPUT-RECORD PIC X(256).\n"
        "       FD  OUTPUT-FILE.\n"
        "       01  OUTPUT-RECORD PIC X(256).\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  EOF-FLAG PIC X VALUE 'N'.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           OPEN INPUT INPUT-FILE.\n"
        "           OPEN OUTPUT OUTPUT-FILE.\n"
        "           PERFORM UNTIL EOF-FLAG = 'Y'\n"
        "               READ INPUT-FILE\n"
        "                   AT END\n"
        "                       MOVE 'Y' TO EOF-FLAG\n"
        "                   NOT AT END\n"
        "                       MOVE INPUT-RECORD TO OUTPUT-RECORD\n"
        "                       WRITE OUTPUT-RECORD\n"
        "               END-READ\n"
        "           END-PERFORM.\n"
        "           CLOSE INPUT-FILE.\n"
        "           CLOSE OUTPUT-FILE.\n"
        "           STOP RUN.\n";

    if (golden_expect_file_matches("samples/cobol/copy_file.cob", expected,
            "copy_file.cob should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_filter_prefix_matches_golden)
{
    static const char expected[] =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. FILTER-PREFIX.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       INPUT-OUTPUT SECTION.\n"
        "       FILE-CONTROL.\n"
        "           SELECT SOURCE-FILE ASSIGN TO \"source.dat\".\n"
        "           SELECT TARGET-FILE ASSIGN TO \"target.dat\".\n"
        "       DATA DIVISION.\n"
        "       FILE SECTION.\n"
        "       FD  SOURCE-FILE.\n"
        "       01  SOURCE-RECORD.\n"
        "           05  SOURCE-LINE PIC X(256).\n"
        "       FD  TARGET-FILE.\n"
        "       01  TARGET-RECORD.\n"
        "           05  TARGET-LINE PIC X(256).\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  PREFIX PIC X(8) VALUE \"ALLOW\".\n"
        "       01  EOF-FLAG PIC X VALUE 'N'.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           OPEN INPUT SOURCE-FILE.\n"
        "           OPEN OUTPUT TARGET-FILE.\n"
        "           PERFORM UNTIL EOF-FLAG = 'Y'\n"
        "               READ SOURCE-FILE\n"
        "                   AT END\n"
        "                       MOVE 'Y' TO EOF-FLAG\n"
        "                   NOT AT END\n"
        "                       IF SOURCE-LINE(1:5) = PREFIX(1:5)\n"
        "                           MOVE SOURCE-RECORD TO TARGET-RECORD\n"
        "                           WRITE TARGET-RECORD\n"
        "                       END-IF\n"
        "               END-READ\n"
        "           END-PERFORM.\n"
        "           CLOSE SOURCE-FILE.\n"
        "           CLOSE TARGET-FILE.\n"
        "           STOP RUN.\n";

    if (golden_expect_file_matches("samples/cobol/filter_prefix.cob", expected,
            "filter_prefix.cob should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_record_writer_matches_golden)
{
    static const char expected[] =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. RECORD-WRITER.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       INPUT-OUTPUT SECTION.\n"
        "       FILE-CONTROL.\n"
        "           SELECT REPORT-FILE ASSIGN TO \"report.dat\".\n"
        "       DATA DIVISION.\n"
        "       FILE SECTION.\n"
        "       FD  REPORT-FILE.\n"
        "       01  REPORT-RECORD.\n"
        "           05  REPORT-ID PIC X(4).\n"
        "           05  REPORT-NAME PIC X(24).\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  REPORT-COUNT PIC 9(4) VALUE 0.\n"
        "       01  WORK-REPORT.\n"
        "           05  WORK-ID PIC X(4).\n"
        "           05  WORK-NAME PIC X(24).\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           OPEN OUTPUT REPORT-FILE.\n"
        "           MOVE \"0001\" TO WORK-ID.\n"
        "           MOVE \"INITIAL ENTRY\" TO WORK-NAME.\n"
        "           MOVE WORK-REPORT TO REPORT-RECORD.\n"
        "           WRITE REPORT-RECORD.\n"
        "           ADD 1 TO REPORT-COUNT.\n"
        "           CLOSE REPORT-FILE.\n"
        "           STOP RUN.\n";

    if (golden_expect_file_matches("samples/cobol/record_writer.cob", expected,
            "record_writer.cob should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_record_summary_matches_golden)
{
    static const char expected[] =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. RECORD-SUMMARY.\n"
        "       ENVIRONMENT DIVISION.\n"
        "       INPUT-OUTPUT SECTION.\n"
        "       FILE-CONTROL.\n"
        "           SELECT INPUT-FILE ASSIGN TO \"records.dat\".\n"
        "       DATA DIVISION.\n"
        "       FILE SECTION.\n"
        "       FD  INPUT-FILE.\n"
        "       01  INPUT-RECORD.\n"
        "           05  RECORD-STATUS PIC X.\n"
        "           05  RECORD-AMOUNT PIC 9(6).\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  EOF-FLAG PIC X VALUE 'N'.\n"
        "       01  TOTAL-AMOUNT PIC 9(7) VALUE 0.\n"
        "       01  ACCEPTED-COUNT PIC 9(4) VALUE 0.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "           OPEN INPUT INPUT-FILE.\n"
        "           PERFORM UNTIL EOF-FLAG = 'Y'\n"
        "               READ INPUT-FILE\n"
        "                   AT END\n"
        "                       MOVE 'Y' TO EOF-FLAG\n"
        "                   NOT AT END\n"
        "                       IF RECORD-STATUS = \"A\"\n"
        "                           ADD 1 TO ACCEPTED-COUNT\n"
        "                           ADD RECORD-AMOUNT TO TOTAL-AMOUNT\n"
        "                       END-IF\n"
        "               END-READ\n"
        "           END-PERFORM.\n"
        "           CLOSE INPUT-FILE.\n"
        "           STOP RUN.\n";

    if (golden_expect_file_matches("samples/cobol/record_summary.cob", expected,
            "record_summary.cob should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_golden_file_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"cblc_copy_file_matches_golden", test_cblc_copy_file_matches_golden},
        {"cblc_filter_prefix_matches_golden", test_cblc_filter_prefix_matches_golden},
        {"cblc_record_writer_matches_golden", test_cblc_record_writer_matches_golden},
        {"cblc_record_summary_matches_golden", test_cblc_record_summary_matches_golden},
        {"cblc_reverse_constructs_matches_golden", test_cblc_reverse_constructs_matches_golden},
        {"cblc_reverse_normalization_matches_golden", test_cblc_reverse_normalization_matches_golden},
        {"cblc_reverse_control_flow_matches_golden", test_cblc_reverse_control_flow_matches_golden},
        {"cobol_copy_file_matches_golden", test_cobol_copy_file_matches_golden},
        {"cobol_filter_prefix_matches_golden", test_cobol_filter_prefix_matches_golden},
        {"cobol_record_writer_matches_golden", test_cobol_record_writer_matches_golden},
        {"cobol_record_summary_matches_golden", test_cobol_record_summary_matches_golden}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
