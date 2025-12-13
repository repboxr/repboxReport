```r
# START_BLOCK rr_single_table
rr_single_table = function(project_dir,
  tabid,
  output_dir = file.path(project_dir, "reports_tables"),
  doc_type = "art",
  opts = NULL,
  map_prod_id = NULL,
  map_version_id = NULL,
  table_png = TRUE,
  png_zoom = 2,
  png_delay = 0.2,
  png_vwidth = 1400,
  png_vheight = 900,
  plain_html_with_issue = FALSE,
  mark_issue_cells_table_only = FALSE
) {
  restore.point("rr_single_table")

  pkgs = c("dplyr", "tidyr", "stringi", "htmltools", "jsonlite", "purrr", "randtoolbox")
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }

  if (isTRUE(table_png)) {
    if (!requireNamespace("webshot2", quietly = TRUE)) {
      stop("table_png=TRUE requires package 'webshot2' (Suggests). Please install it.", call. = FALSE)
    }
  }

  if (is.null(opts)) {
    opts = rr_map_report_opts()
  }

  tabids = as.character(tabid)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Decide on show_wrong_number_report if it's NA (same logic as rr_map_report)
  rme_file_for_check = file.path(project_dir, "fp", paste0("eval_", doc_type), "rme.Rds")
  if (is.null(opts$show_wrong_number_report)) opts$show_wrong_number_report = NA
  if (is.na(opts$show_wrong_number_report)) {
    opts$show_wrong_number_report = !file.exists(rme_file_for_check)
  }

  # Load parcels and table html
  parcels = repboxDB::repdb_load_parcels(project_dir, c("stata_source", "stata_run_cmd", "stata_run_log", "stata_cmd"))

  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  tab_main_info = rai_pick_tab_ver(fp_dir, "tab_main")
  if (nrow(tab_main_info) == 0) {
    stop(paste0("Could not find a suitable 'tab_main' product for doc_type '", doc_type, "'"), call. = FALSE)
  }
  tab_main = fp_load_prod_df(tab_main_info$ver_dir)

  tab_main$tabid = as.character(tab_main$tabid)

  if (length(tabids) == 0) tabids = unique(tab_main$tabid)

  tab_rows = dplyr::filter(tab_main, tabid %in% tabids)
  missing_tabs = setdiff(tabids, tab_rows$tabid)
  if (length(missing_tabs) > 0) {
    stop(paste0("Could not find tabid(s) in tab_main: ", paste(missing_tabs, collapse = ", ")), call. = FALSE)
  }

  # Load maps (all types in opts, but we will choose one for output)
  all_map_types = list()
  for (prod_id in opts$map_prod_ids) {
    map_list = rr_load_all_map_versions(project_dir, doc_type, prod_id = prod_id)
    if (length(map_list) > 0) {
      all_map_types[[prod_id]] = map_list
    }
  }
  if (length(all_map_types) == 0) {
    stop("No map versions found for any prod_id in opts$map_prod_ids.", call. = FALSE)
  }

  if (is.null(map_prod_id)) {
    map_prod_id = opts$map_prod_ids[1]
  }
  if (!(map_prod_id %in% names(all_map_types))) {
    stop(paste0("map_prod_id '", map_prod_id, "' not found. Available: ", paste(names(all_map_types), collapse = ", ")), call. = FALSE)
  }

  available_versions = names(all_map_types[[map_prod_id]])
  if (length(available_versions) == 0) {
    stop(paste0("No versions available for map_prod_id '", map_prod_id, "'."), call. = FALSE)
  }

  if (is.null(map_version_id)) {
    map_version_id = sort(available_versions)[length(available_versions)]
  }
  if (!(map_version_id %in% available_versions)) {
    stop(paste0(
      "map_version_id '", map_version_id, "' not found for map_prod_id '", map_prod_id, "'. Available: ",
      paste(available_versions, collapse = ", ")
    ), call. = FALSE)
  }

  # Build global reg color map for consistency (same idea as rr_map_report)
  all_map_dfs = unlist(all_map_types, recursive = FALSE)
  all_regids = unique(unlist(lapply(all_map_dfs, function(df) {
    if (!is.null(df) && "regid" %in% names(df)) unique(df$regid) else NULL
  })))
  all_regids = stats::na.omit(all_regids)
  reg_color_map = rr_make_distinct_colors(length(all_regids))
  names(reg_color_map) = all_regids

  # Evaluation data (optional)
  processed_eval_data = NULL
  rme_file = file.path(project_dir, "fp", paste0("eval_", doc_type), "rme.Rds")
  if (file.exists(rme_file)) {
    tryCatch({
      rme = readRDS(rme_file)
      processed_eval_data = rr_process_eval_data(rme, all_map_types, parcels$stata_source$script_source, opts)
    }, error = function(e) {
      warning(paste0("Could not load or process rme.Rds: ", e$message))
      processed_eval_data = NULL
    })
  }

  # Per-table outputs
  out_rows = list()
  for (i in seq_len(nrow(tab_rows))) {
    row = tab_rows[i, ]
    tabid_i = as.character(row$tabid)

    map_df = all_map_types[[map_prod_id]][[map_version_id]]
    if (is.null(map_df) || nrow(map_df) == 0) next

    map_df$tabid = as.character(map_df$tabid)
    map_df_tab = dplyr::filter(map_df, tabid == tabid_i)

    mapping = rr_process_single_map_for_js(map_df_tab, reg_color_map, parcels$stata_source$script_source)

    eval_for_tab = NULL
    if (!is.null(processed_eval_data) && map_version_id %in% names(processed_eval_data)) {
      ver_block = processed_eval_data[[map_version_id]]
      if (!is.null(ver_block) && tabid_i %in% names(ver_block)) {
        eval_for_tab = ver_block[[tabid_i]]
      }
    }

    issue_cellids_for_table_only = NULL
    if (isTRUE(mark_issue_cells_table_only)) {
      issue_cellids_for_table_only = rr_collect_issue_cellids(
        mapping = mapping,
        show_wrong_number = isTRUE(opts$show_wrong_number_report),
        eval_for_tab = eval_for_tab
      )
    }

    base_name = rr_safe_filename(paste0("tab", tabid_i, "__", map_prod_id, "__", map_version_id))
    file_table_only = file.path(output_dir, paste0(base_name, "__table_only.html"))
    file_with_issues = file.path(output_dir, paste0(base_name, "__with_issues.html"))
    file_png = file.path(output_dir, paste0(base_name, ".png"))

    html_only = rr_make_single_table_html(
      tabtitle = row$tabtitle,
      tabhtml = row$tabhtml,
      mapping = mapping,
      show_wrong_number = isTRUE(opts$show_wrong_number_report),
      include_issues = FALSE,
      eval_for_tab = NULL,
      issue_cellids = issue_cellids_for_table_only,
      mark_issue_cells = isTRUE(mark_issue_cells_table_only)
    )

    if (isTRUE(plain_html_with_issue)) {
      html_issues = rr_make_single_table_plain_issues_html(
        tabtitle = row$tabtitle,
        tabhtml = row$tabhtml,
        mapping = mapping,
        show_wrong_number = isTRUE(opts$show_wrong_number_report),
        eval_for_tab = eval_for_tab
      )
    } else {
      html_issues = rr_make_single_table_html(
        tabtitle = row$tabtitle,
        tabhtml = row$tabhtml,
        mapping = mapping,
        show_wrong_number = isTRUE(opts$show_wrong_number_report),
        include_issues = TRUE,
        eval_for_tab = eval_for_tab
      )
    }

    htmltools::save_html(html_only, file = file_table_only)
    htmltools::save_html(html_issues, file = file_with_issues)

    png_done = FALSE
    if (isTRUE(table_png)) {
      # Render only the table container. This keeps the PNG clean.
      tryCatch({
        webshot2::webshot(
          url = file_table_only,
          file = file_png,
          selector = "#rr-table-container",
          zoom = png_zoom,
          delay = png_delay,
          vwidth = png_vwidth,
          vheight = png_vheight
        )
        png_done = TRUE
      }, error = function(e) {
        warning(paste0("PNG rendering failed for tabid ", tabid_i, ": ", e$message))
        png_done = FALSE
      })
    }

    out_rows[[length(out_rows) + 1]] = data.frame(
      tabid = tabid_i,
      map_prod_id = map_prod_id,
      map_version_id = map_version_id,
      table_only_html = file_table_only,
      with_issues_html = file_with_issues,
      png = if (png_done) file_png else NA_character_,
      stringsAsFactors = FALSE
    )
  }

  res = if (length(out_rows) == 0) {
    data.frame(
      tabid = character(0),
      map_prod_id = character(0),
      map_version_id = character(0),
      table_only_html = character(0),
      with_issues_html = character(0),
      png = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    dplyr::bind_rows(out_rows)
  }

  message(paste0("rr_single_table wrote outputs to: ", normalizePath(output_dir, mustWork = FALSE)))
  invisible(res)
}
# END_BLOCK rr_single_table
```

```r
# START_BLOCK rr_make_single_table_html
rr_make_single_table_html = function(tabtitle,
  tabhtml,
  mapping,
  show_wrong_number = TRUE,
  include_issues = FALSE,
  eval_for_tab = NULL,
  issue_cellids = NULL,
  mark_issue_cells = FALSE
) {
  restore.point("rr_make_single_table_html")

  reg_info_json = jsonlite::toJSON(mapping$reg_info %||% list(), auto_unbox = TRUE, null = "null", na = "null")
  wrong_info_json = jsonlite::toJSON(mapping$wrong_number_info %||% list(), auto_unbox = TRUE, null = "null", na = "null")

  issues_html = ""
  issues_cellids_json = "[]"

  if (isTRUE(include_issues)) {
    issues_pack = rr_build_issues_block(mapping = mapping, show_wrong_number = show_wrong_number, eval_for_tab = eval_for_tab)
    issues_html = issues_pack$issues_html
    issues_cellids_json = issues_pack$issues_cellids_json
  } else if (isTRUE(mark_issue_cells) && !is.null(issue_cellids) && length(issue_cellids) > 0) {
    issue_cellids = unique(stats::na.omit(as.character(issue_cellids)))
    issues_cellids_json = jsonlite::toJSON(as.list(issue_cellids), auto_unbox = TRUE, null = "null", na = "null")
  }

  css = rr_single_table_css()

  js = rr_single_table_js(
    reg_info_json = reg_info_json,
    wrong_info_json = wrong_info_json,
    show_wrong_number = isTRUE(show_wrong_number),
    issues_cellids_json = issues_cellids_json
  )

  htmltools::tagList(
    htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$meta(charset = "UTF-8"),
        htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        htmltools::tags$title(paste0("Table ", htmltools::htmlEscape(as.character(tabtitle)))),
        htmltools::tags$style(htmltools::HTML(css))
      ),
      htmltools::tags$body(
        htmltools::tags$div(
          id = "rr-page",
          htmltools::tags$div(
            id = "rr-table-container",
            htmltools::tags$div(class = "art-tab-div",
              htmltools::tags$h4(class = "rr-title", htmltools::htmlEscape(as.character(tabtitle))),
              htmltools::HTML(tabhtml)
            )
          ),
          if (isTRUE(include_issues)) htmltools::HTML(issues_html) else NULL
        ),
        htmltools::tags$script(htmltools::HTML(js))
      )
    )
  )
}
# END_BLOCK rr_make_single_table_html
```

```r
# START_BLOCK rr_single_table_css
rr_single_table_css = function() {
  paste0(
    "html, body { margin: 0; padding: 0; background: #ffffff; }\n",
    "#rr-page { padding: 0; margin: 0; }\n",
    "#rr-table-container { padding: 0; margin: 0; display: inline-block; }\n",
    ".rr-title { font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif; ",
    "font-size: 16px; margin: 6px 6px 2px 6px; color: #222; }\n",
    ".art-tab-div table { font-size: 12px; font-family: Trebuchet MS, Arial Narrow, Tahoma, sans-serif; ",
    "width: auto; white-space: nowrap; border-collapse: collapse; margin: 6px; }\n",
    ".art-tab-div table td, .art-tab-div table th { padding: 2px 4px; border: 1px solid #ddd; text-align: center; position: relative; }\n",
    ".art-tab-div table th { background-color: #f5f5f5; }\n",
    ".statically-colored { transition: background-color 0.3s ease; }\n",
    ".wrong-number-cell { }\n",
    ".issue-cell { box-shadow: inset 0 0 0 2px #dc3545 !important; }\n",
    ".rr-issues { font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif; ",
    "font-size: 13px; padding: 10px 12px; border-top: 1px solid #ddd; margin-top: 6px; }\n",
    ".rr-issues h5 { margin: 0 0 8px 0; font-size: 15px; }\n",
    ".rr-issues h6 { margin: 10px 0 4px 0; font-size: 14px; }\n",
    ".rr-issues .rr-descr { color: #555; font-style: italic; margin: 0 0 6px 0; }\n",
    ".rr-issues table { border-collapse: collapse; width: 100%; max-width: 1200px; }\n",
    ".rr-issues th, .rr-issues td { border: 1px solid #ddd; padding: 4px 6px; vertical-align: top; }\n",
    ".rr-issues th { background: #f7f7f7; }\n",
    ".rr-badge { display: inline-block; padding: 1px 6px; border-radius: 10px; background: #eef; margin-left: 6px; font-size: 12px; }\n"
  )
}
# END_BLOCK rr_single_table_css
```

```r
# START_BLOCK rr_plain_issue_helpers
rr_sanitize_issue_name = function(x) {
  x = as.character(x)
  x = stringi::stri_trim_both(x)
  x = stringi::stri_trans_tolower(x)
  x = stringi::stri_replace_all_regex(x, "[^a-z0-9]+", "_")
  x = stringi::stri_replace_all_regex(x, "_{2,}", "_")
  x = stringi::stri_replace_all_regex(x, "^_+|_+$", "")
  if (nchar(x) == 0) x = "issue"
  x
}

rr_collect_issue_cellids = function(mapping, show_wrong_number = TRUE, eval_for_tab = NULL) {
  cids = character(0)

  if (isTRUE(show_wrong_number) &&
      !is.null(mapping$wrong_number_info) &&
      is.data.frame(mapping$wrong_number_info) &&
      nrow(mapping$wrong_number_info) > 0 &&
      "cell_id" %in% names(mapping$wrong_number_info)) {
    cids = c(cids, as.character(mapping$wrong_number_info$cell_id))
  }

  if (!is.null(eval_for_tab) && length(eval_for_tab) > 0) {
    test_names = names(eval_for_tab)
    for (test_name in test_names) {
      test_block = eval_for_tab[[test_name]]
      if (is.null(test_block) || is.null(test_block$issues)) next
      issues = test_block$issues
      if (is.null(issues) || length(issues) == 0) next
      cids = c(cids, rr_extract_cellids_from_issue_records(issues))
    }
  }

  unique(stats::na.omit(stringi::stri_trim_both(as.character(cids))))
}

rr_build_issue_cell_class_map = function(mapping, show_wrong_number = TRUE, eval_for_tab = NULL) {
  # Returns a named list: names are cellids, values are character vectors of issue names (sanitized).
  out = list()

  add_one = function(cellid, issue_name) {
    cellid = stringi::stri_trim_both(as.character(cellid))
    issue_name = rr_sanitize_issue_name(issue_name)
    if (is.na(cellid) || nchar(cellid) == 0) return(invisible(NULL))
    if (is.null(out[[cellid]])) {
      out[[cellid]] = issue_name
    } else {
      out[[cellid]] = unique(c(out[[cellid]], issue_name))
    }
    invisible(NULL)
  }

  if (isTRUE(show_wrong_number) &&
      !is.null(mapping$wrong_number_info) &&
      is.data.frame(mapping$wrong_number_info) &&
      nrow(mapping$wrong_number_info) > 0 &&
      "cell_id" %in% names(mapping$wrong_number_info)) {
    wdf = mapping$wrong_number_info
    for (i in seq_len(nrow(wdf))) {
      add_one(wdf$cell_id[i], "wrong_number")
    }
  }

  if (!is.null(eval_for_tab) && length(eval_for_tab) > 0) {
    test_names = names(eval_for_tab)
    for (test_name in test_names) {
      test_block = eval_for_tab[[test_name]]
      if (is.null(test_block) || is.null(test_block$issues)) next
      issues = test_block$issues
      if (is.null(issues) || length(issues) == 0) next

      cids = rr_extract_cellids_from_issue_records(issues)
      if (length(cids) == 0) next

      for (cid in cids) {
        add_one(cid, test_name)
      }
    }
  }

  out
}

rr_add_issue_classes_to_tabhtml = function(tabhtml, class_map) {
  # Adds classes to <td>/<th> tags by matching id="...".
  # For each issue name, adds "has_issue_{issue_name}" as a class.
  if (is.null(class_map) || length(class_map) == 0) return(tabhtml)

  html = as.character(tabhtml)

  add_for_one_id = function(html_in, cellid, issue_names) {
    if (is.null(issue_names) || length(issue_names) == 0) return(html_in)

    issue_names = unique(rr_sanitize_issue_name(issue_names))
    cls_add = paste0("has_issue_", issue_names, collapse = " ")
    cellid_esc = stringi::stri_escape_regex(as.character(cellid))

    # Case A: id appears before class=
    pat1 = paste0('(<(td|th)\\b[^>]*\\bid="', cellid_esc, '"[^>]*\\bclass=")([^"]*)(")')
    if (grepl(pat1, html_in, perl = TRUE)) {
      return(sub(pat1, paste0("\\1\\3 ", cls_add, "\\4"), html_in, perl = TRUE))
    }

    # Case B: class= appears before id
    pat2 = paste0('(<(td|th)\\b[^>]*\\bclass=")([^"]*)("([^>]*\\bid="', cellid_esc, '"[^>]*)>)')
    if (grepl(pat2, html_in, perl = TRUE)) {
      return(sub(pat2, paste0("\\1\\2 ", cls_add, "\\3\\4"), html_in, perl = TRUE))
    }

    # Case C: no class attribute in the tag, insert class before closing >
    pat3 = paste0('(<(td|th)\\b[^>]*\\bid="', cellid_esc, '"[^>]*)>')
    if (grepl(pat3, html_in, perl = TRUE)) {
      return(sub(pat3, paste0('\\1 class="', cls_add, '">'), html_in, perl = TRUE))
    }

    html_in
  }

  ids = names(class_map)
  for (cellid in ids) {
    html = add_for_one_id(html, cellid, class_map[[cellid]])
  }

  html
}

rr_make_single_table_plain_issues_html = function(tabtitle, tabhtml, mapping, show_wrong_number = TRUE, eval_for_tab = NULL) {
  restore.point("rr_make_single_table_plain_issues_html")

  class_map = rr_build_issue_cell_class_map(
    mapping = mapping,
    show_wrong_number = isTRUE(show_wrong_number),
    eval_for_tab = eval_for_tab
  )

  tabhtml2 = rr_add_issue_classes_to_tabhtml(tabhtml, class_map)

  issue_lines = character(0)

  if (isTRUE(show_wrong_number) &&
      !is.null(mapping$wrong_number_info) &&
      is.data.frame(mapping$wrong_number_info) &&
      nrow(mapping$wrong_number_info) > 0 &&
      "cell_id" %in% names(mapping$wrong_number_info)) {
    w_cids = unique(stats::na.omit(stringi::stri_trim_both(as.character(mapping$wrong_number_info$cell_id))))
    if (length(w_cids) > 0) {
      issue_lines = c(issue_lines, paste0("has_issue_wrong_number: ", paste(w_cids, collapse = ",")))
    }
  }

  if (!is.null(eval_for_tab) && length(eval_for_tab) > 0) {
    test_names = sort(names(eval_for_tab))
    for (test_name in test_names) {
      test_block = eval_for_tab[[test_name]]
      if (is.null(test_block) || is.null(test_block$issues)) next
      issues = test_block$issues
      if (is.null(issues) || length(issues) == 0) next

      cids = unique(stats::na.omit(stringi::stri_trim_both(rr_extract_cellids_from_issue_records(issues))))
      if (length(cids) == 0) next

      key = rr_sanitize_issue_name(test_name)
      issue_lines = c(issue_lines, paste0("has_issue_", key, ": ", paste(cids, collapse = ",")))
    }
  }

  issues_block = if (length(issue_lines) == 0) {
    ""
  } else {
    paste0("<pre>", htmltools::htmlEscape(paste(issue_lines, collapse = "\n")), "</pre>")
  }

  htmltools::tagList(
    htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$meta(charset = "UTF-8"),
        htmltools::tags$title(paste0("Table ", htmltools::htmlEscape(as.character(tabtitle))))
      ),
      htmltools::tags$body(
        htmltools::tags$h4(htmltools::htmlEscape(as.character(tabtitle))),
        htmltools::HTML(tabhtml2),
        htmltools::tags$div(id = "issues", htmltools::HTML(issues_block))
      )
    )
  )
}
# END_BLOCK rr_plain_issue_helpers
```
