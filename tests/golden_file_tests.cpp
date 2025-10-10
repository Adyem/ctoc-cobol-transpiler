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
        "function void process_file() {\n"
        "    open(in, \"r\");\n"
        "    open(out, \"w\");\n"
        "    while (read(in, line)) {\n"
        "        write(out, line);\n"
        "    }\n"
        "    close(in);\n"
        "    close(out);\n"
        "}\n\n"
        "function void main() {\n"
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
        "function void filter_prefix() {\n"
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
        "function void main() {\n"
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
        "function void write_records() {\n"
        "    person.name = \"ALICE\";\n"
        "    person.id = \"0001\";\n\n"
        "    open(people, \"w\");\n"
        "    write(people, person);\n"
        "    close(people);\n"
        "}\n\n"
        "function void main() {\n"
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
        "function void summarize_records() {\n"
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
        "function void main() {\n"
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
        "function void MAIN() {\n"
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
        "function void ENTRY_PARAGRAPH() {\n"
        "    SCRATCH_NOTE = \"mixED Case value\";\n"
        "    RUNNING_TOTAL_VALUE = 0;\n"
        "    STATUS_FLAG = true;\n"
        "    return ;\n"
        "}\n\n"
        "function void NORMALIZE_VALUES() {\n"
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
        "function void MAIN() {\n"
        "    if (!(CONTROL_FLAG == true)) {\n"
        "        while (!(PROGRESS_METER > PROGRESS_LIMIT)) {\n"
        "            PROGRESS_METER = 11;\n"
        "        }\n"
        "        OUTPUT_VALUE = PROGRESS_METER;\n"
        "    } else {\n"
        "        PROGRESS_INDEX = 0;\n"
        "        while (!(PROGRESS_INDEX >= PROGRESS_LIMIT)) {\n"
        "            OUTPUT_VALUE = PROGRESS_INDEX;\n"
        "            PROGRESS_INDEX++;\n"
        "        }\n"
        "    }\n"
        "    display(OUTPUT_VALUE);\n"
        "    return ;\n"
        "}\n\n"
        "function void NEXT_PARAGRAPH() {\n"
        "    CONTROL_FLAG = true;\n"
        "    return ;\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/reverse_control_flow.cblc", expected,
            "reverse_control_flow.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_numeric_precision_matches_golden)
{
    static const char expected[] =
        "long day_total;\n"
        "long long year_total;\n"
        "long long threshold;\n"
        "float day_ratio;\n"
        "double combined_ratio;\n\n"
        "function void analyze_precision() {\n"
        "    long deposit;\n"
        "    long withdrawal;\n"
        "    long long bonus_pool;\n"
        "    float seasonal_rate;\n"
        "    double base_rate;\n\n"
        "    deposit = 125000;\n"
        "    withdrawal = 50000;\n"
        "    bonus_pool = 4000000000;\n"
        "    seasonal_rate = 1.25;\n"
        "    base_rate = 2.5;\n\n"
        "    day_total = deposit - withdrawal;\n"
        "    year_total = bonus_pool + day_total;\n"
        "    threshold = 5000000000;\n"
        "    day_ratio = seasonal_rate * 2.0;\n"
        "    combined_ratio = base_rate + day_ratio;\n\n"
        "    if (year_total >= threshold) {\n"
        "        display(\"YEAR ABOVE\");\n"
        "    } else {\n"
        "        display(\"YEAR BELOW\");\n"
        "    }\n\n"
        "    if (combined_ratio > base_rate) {\n"
        "        display(\"RATE INCREASED\");\n"
        "    }\n\n"
        "    if (day_ratio != seasonal_rate) {\n"
        "        display(\"FLOAT SHIFT\");\n"
        "    }\n\n"
        "    if (year_total > bonus_pool) {\n"
        "        display(\"BONUS REACHED\");\n"
        "    }\n\n"
        "    if (day_total == 75000) {\n"
        "        display(\"DAY TARGET\");\n"
        "    }\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/numeric_precision.cblc", expected,
            "numeric_precision.cblc should match golden content") != FT_SUCCESS)
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
        "           PERFORM UNTIL EOF-FLAG == 'Y'\n"
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
        "           SELECT SOURCE-FILE ASSIGN TO \"source.dat\"\n"
        "               ORGANIZATION IS LINE SEQUENTIAL.\n"
        "           SELECT TARGET-FILE ASSIGN TO \"target.dat\"\n"
        "               ORGANIZATION IS LINE SEQUENTIAL.\n"
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
        "           PERFORM UNTIL EOF-FLAG == 'Y'\n"
        "               READ SOURCE-FILE\n"
        "                   AT END\n"
        "                       MOVE 'Y' TO EOF-FLAG\n"
        "                   NOT AT END\n"
        "                       IF SOURCE-LINE(1:5) == PREFIX(1:5)\n"
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
        "           PERFORM UNTIL EOF-FLAG == 'Y'\n"
        "               READ INPUT-FILE\n"
        "                   AT END\n"
        "                       MOVE 'Y' TO EOF-FLAG\n"
        "                   NOT AT END\n"
        "                       IF RECORD-STATUS == \"A\"\n"
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

FT_TEST(test_cobol_numeric_precision_matches_golden)
{
    static const char expected[] =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. NUMERIC-PRECISION.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 DAY-TOTAL PIC S9(9) VALUE 0.\n"
        "       01 YEAR-TOTAL PIC S9(12) VALUE 0.\n"
        "       01 THRESHOLD PIC S9(12) VALUE 0.\n"
        "       01 DAY-RATIO PIC S9V9(4) VALUE 0.\n"
        "       01 COMBINED-RATIO PIC S9V9(4) VALUE 0.\n"
        "       01 DEPOSIT PIC S9(9) VALUE 0.\n"
        "       01 WITHDRAWAL PIC S9(9) VALUE 0.\n"
        "       01 BONUS-POOL PIC S9(12) VALUE 0.\n"
        "       01 SEASONAL-RATE PIC S9V9(4) VALUE 0.\n"
        "       01 BASE-RATE PIC S9V9(4) VALUE 0.\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "       MOVE 125000 TO DEPOSIT.\n"
        "       MOVE 50000 TO WITHDRAWAL.\n"
        "       MOVE 4000000000 TO BONUS-POOL.\n"
        "       MOVE 5000000000 TO THRESHOLD.\n"
        "       COMPUTE SEASONAL-RATE = 1.25.\n"
        "       COMPUTE BASE-RATE = 2.5.\n"
        "       COMPUTE DAY-TOTAL = DEPOSIT - WITHDRAWAL.\n"
        "       COMPUTE YEAR-TOTAL = BONUS-POOL + DAY-TOTAL.\n"
        "       COMPUTE DAY-RATIO = SEASONAL-RATE * 2.0.\n"
        "       COMPUTE COMBINED-RATIO = BASE-RATE + DAY-RATIO.\n"
        "       IF YEAR-TOTAL >= THRESHOLD\n"
        "           DISPLAY \"YEAR ABOVE\"\n"
        "       ELSE\n"
        "           DISPLAY \"YEAR BELOW\"\n"
        "       END-IF.\n"
        "       IF COMBINED-RATIO > BASE-RATE\n"
        "           DISPLAY \"RATE INCREASED\"\n"
        "       END-IF.\n"
        "       IF DAY-RATIO NOT == SEASONAL-RATE\n"
        "           DISPLAY \"FLOAT SHIFT\"\n"
        "       END-IF.\n"
        "       IF YEAR-TOTAL > BONUS-POOL\n"
        "           DISPLAY \"BONUS REACHED\"\n"
        "       END-IF.\n"
        "       IF DAY-TOTAL == 75000\n"
        "           DISPLAY \"DAY TARGET\"\n"
        "       END-IF.\n"
        "       STOP RUN.\n"
        "       END PROGRAM NUMERIC-PRECISION.\n";

    if (golden_expect_file_matches("samples/cobol/numeric_precision.cob", expected,
            "numeric_precision.cob should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_multi_module_main_matches_golden)
{
    static const char expected[] =
        "int accumulator;\n"
        "\n"
        "function void add_once() {\n"
        "    accumulator = accumulator + 1;\n"
        "}\n"
        "\n"
        "function void main() {\n"
        "    accumulator = 0;\n"
        "    show_banner();\n"
        "    add_once();\n"
        "    display(accumulator);\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/multi_module_main.cblc", expected,
            "multi_module_main.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cblc_multi_module_worker_matches_golden)
{
    static const char expected[] =
        "function void show_banner() {\n"
        "    display(\"WORKER READY\");\n"
        "}\n";

    if (golden_expect_file_matches("samples/cblc/multi_module_worker.cblc", expected,
            "multi_module_worker.cblc should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_multi_module_main_matches_golden)
{
    static const char expected[] =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MULTI-MODULE-MAIN.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 ACCUMULATOR PIC 9(4) VALUE 0000.\n"
        "       01 DISPLAY-BUFFER PIC Z(4).\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "       MOVE 0 TO ACCUMULATOR.\n"
        "       CALL 'SHOW-BANNER'.\n"
        "       PERFORM ADD-ONCE.\n"
        "       MOVE ACCUMULATOR TO DISPLAY-BUFFER.\n"
        "       DISPLAY DISPLAY-BUFFER.\n"
        "       STOP RUN.\n"
        "\n"
        "ADD-ONCE.\n"
        "       ADD 1 TO ACCUMULATOR.\n"
        "       EXIT.\n"
        "       END PROGRAM MULTI-MODULE-MAIN.\n";

    if (golden_expect_file_matches("samples/cobol/multi_module_main.cob", expected,
            "multi_module_main.cob should match golden content") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_cobol_multi_module_worker_matches_golden)
{
    static const char expected[] =
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SHOW-BANNER.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01 WORKER-MESSAGE PIC X(12) VALUE \"WORKER READY\".\n"
        "       PROCEDURE DIVISION.\n"
        "MAIN.\n"
        "       DISPLAY WORKER-MESSAGE.\n"
        "       GOBACK.\n"
        "       END PROGRAM SHOW-BANNER.\n";

    if (golden_expect_file_matches("samples/cobol/multi_module_worker.cob", expected,
            "multi_module_worker.cob should match golden content") != FT_SUCCESS)
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
        {"cblc_numeric_precision_matches_golden", test_cblc_numeric_precision_matches_golden},
        {"cblc_multi_module_main_matches_golden", test_cblc_multi_module_main_matches_golden},
        {"cblc_multi_module_worker_matches_golden", test_cblc_multi_module_worker_matches_golden},
        {"cobol_copy_file_matches_golden", test_cobol_copy_file_matches_golden},
        {"cobol_filter_prefix_matches_golden", test_cobol_filter_prefix_matches_golden},
        {"cobol_record_writer_matches_golden", test_cobol_record_writer_matches_golden},
        {"cobol_record_summary_matches_golden", test_cobol_record_summary_matches_golden},
        {"cobol_numeric_precision_matches_golden", test_cobol_numeric_precision_matches_golden},
        {"cobol_multi_module_main_matches_golden", test_cobol_multi_module_main_matches_golden},
        {"cobol_multi_module_worker_matches_golden", test_cobol_multi_module_worker_matches_golden}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
