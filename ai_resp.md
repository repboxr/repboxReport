You are right to point out that evaluation tests present in the markdown report were missing from the interactive HTML report.

The root cause was an overly aggressive filtering step within the R function `rr_process_eval_data`. This function is responsible for preparing the evaluation data from `rme.Rds` for the web report. It was incorrectly discarding any evaluation issue for which it could not find a corresponding `runid` (a link to a specific code execution). Tests like `single_col_reg` or `invalid_cellids` are defined by table structure (`reg_ind`) and don't always have a direct `runid`, causing them to be filtered out.

I have rewritten `rr_process_eval_data` to be more robust. The new version will:
1.  Correctly process all evaluation tests from `rme.Rds`.
2.  Intelligently add code location information (`script_num`, `code_line`) where possible, to enable the "click-to-code" feature.
3.  **Crucially, it will no longer discard issues that cannot be linked to a specific line of code.** These issues will now appear in the report, although the "jump to code" functionality will be disabled for them, which is the correct behavior.

This change ensures that all relevant findings from the evaluation are displayed in the interactive report, matching the completeness of the markdown summary.

Here is the modification for `R/report_map.R`:

!MODIFICATION rr_process_eval_data in R/report_map.R
scope = "function"
file = "R/report_map.R"
function_name = "rr_process_eval_data"
description = '''Rewrite `rr_process_eval_data` to correctly process and include all evaluation tests from `rme.Rds` in the final report. The new logic robustly links issues to code locations without incorrectly filtering out valid issues that lack a direct `runid`.'''
---
```r
#' Process rme$evals data for the report
#' @param rme The loaded rme.Rds object
#' @param all_map_types The loaded map data, used to resolve runids to code locations
#' @param stata_source The stata_source parcel, used for robust script_num joins
#' @return A nested list structured for JSON output
#' @noRd
rr_process_eval_data <- function(rme, all_map_types, stata_source) {
    restore.point("rr_process_eval_data")
    if (is.null(rme) || is.null(rme$evals)) return(NULL)

    # Descriptions for each test type, to be shown in the UI
    long_descriptions <- list(
        runids_differ = "Discrepancy Across Map Versions: Identifies cells mapped to different regression `runid`s by different AI versions, indicating areas of uncertainty.",
        invalid_runids = "Invalid `runid` Mapping: Flags mappings that point to a `runid` not found in the project's execution log. This is a critical integrity error.",
        invalid_cellids = "Invalid `cellid` Mapping: Flags mappings that reference a `cellid` that does not exist in the parsed table data. This indicates a hallucinated or malformed cell reference.",
        non_reg_cmd = "Mapping to Non-Regression Command: Identifies cells mapped to a non-regression command (e.g., `test`, `margins`). This is not necessarily an error but serves as an important note.",
        coef_se_match = "Value Mismatch between Table and Code: Compares numeric values from the table (coefficients/SEs) against the results from the mapped regression's output. Highlights discrepancies.",
        single_col_reg = "Regression Spans Multiple Columns: Flags regressions whose mapped cells span multiple columns without a clear structural reason (e.g., SEs in an adjacent column), which often indicates incorrect grouping.",
        multicol_reg_plausibility = "Implausible Multi-Column Structure: Flags multi-column regressions where every row has a value in only one column, suggesting an incorrect mapping structure.",
        overlapping_regs = "Overlapping Regression Mappings: Flags coefficient cells that have been mapped to more than one regression within the same map version, which is almost always an error.",
        consistent_vertical_structure = "Inconsistent Summary Stat Rows: Checks for consistent table structure by flagging summary statistics (e.g., 'Observations') that appear on different row numbers across columns of a single table."
    )

    # 1. Create robust lookup from runid to code location from rme$run_df (ground truth)
    runid_to_code_lookup <- NULL
    if ("run_df" %in% names(rme) && !is.null(rme$run_df)) {
        runid_to_code_lookup <- rme$run_df %>%
            dplyr::select(runid, script_num, code_line) %>%
            dplyr::filter(!is.na(runid)) %>%
            dplyr::distinct(runid, .keep_all = TRUE)
    }

    # 2. Create lookup from map info to get runid for tests that don't have it
    all_maps_flat_df <- purrr::map_dfr(unlist(all_map_types, recursive = FALSE), ~ if(!is.null(.x) && nrow(.x)>0) .x else NULL, .id = "map_version_id")
    reg_ind_to_runid_lookup <- NULL
    if (nrow(all_maps_flat_df) > 0) {
        reg_ind_to_runid_lookup <- all_maps_flat_df %>%
            dplyr::select(map_version_id, tabid, reg_ind, runid) %>%
            dplyr::filter(!is.na(reg_ind), !is.na(runid)) %>%
            dplyr::distinct()
    }

    processed_evals <- list()
    eval_tests <- rme$evals[sapply(rme$evals, function(x) !is.null(x) && NROW(x) > 0)]

    for (test_name in names(eval_tests)) {
        df <- eval_tests[[test_name]]
        df <- dplyr::ungroup(df)

        if (!"map_version" %in% names(df)) next

        df$ver_id <- df$map_version
        df <- dplyr::filter(df, !is.na(ver_id))
        if(nrow(df) == 0) next

        # Standardize runid column if it's named 'runids'
        if ("runids" %in% names(df) && !"runid" %in% names(df)) {
             df <- df %>% dplyr::rename(runid = runids)
        }

        # Ensure runid is numeric for joining
        if ("runid" %in% names(df) && !is.numeric(df$runid)) {
            df$runid <- suppressWarnings(as.numeric(as.character(df$runid)))
        }

        # If df has reg_ind but no runid, try to get runid from maps
        if (!"runid" %in% names(df) && "reg_ind" %in% names(df) && !is.null(reg_ind_to_runid_lookup)) {
            df$reg_ind <- as.integer(df$reg_ind)
            df <- dplyr::left_join(df, reg_ind_to_runid_lookup, by = c("map_version" = "map_version_id", "tabid", "reg_ind"))
        }

        # If df now has runid, join with ground truth to get code location.
        # This join will not filter out rows if runid is NA, which is what we want.
        if ("runid" %in% names(df) && !is.null(runid_to_code_lookup)) {
            df <- df %>%
                dplyr::left_join(runid_to_code_lookup, by = "runid")
        }

        # Standardize cellids: if only cellid exists, copy it to cellids for the JS
        if ("cellid" %in% names(df) && !"cellids" %in% names(df)) {
            df$cellids <- df$cellid
        }

        # Convert all columns to character to avoid JSON issues
        is_list_col <- sapply(df, is.list)
        df[!is_list_col] <- lapply(df[!is_list_col], as.character)

        df_split_ver <- split(df, df$ver_id)

        for (ver_id in names(df_split_ver)) {
            ver_df <- df_split_ver[[ver_id]]
            if (!"tabid" %in% names(ver_df)) next

            ver_df$tabid <- as.character(ver_df$tabid)
            df_split_tab <- split(ver_df, ver_df$tabid)

            for (tabid in names(df_split_tab)) {
                issue_df <- df_split_tab[[tabid]]
                cols_to_keep <- setdiff(names(issue_df), c("map_version", "ver_id", "tabid"))

                records <- purrr::transpose(issue_df[, cols_to_keep, drop = FALSE])

                processed_evals[[ver_id]][[tabid]][[test_name]] <- list(
                    description = long_descriptions[[test_name]] %||% attr(df, "descr") %||% "",
                    issues = records
                )
            }
        }
    }
    return(processed_evals)
}
```
!END_MODIFICATION rr_process_eval_data in R/report_map.R
