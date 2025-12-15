# FILE: rr_table.R

example = function() {
  library(repboxReport)
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"

  options(warn=2)
  opts = rr_map_report_opts(embed_data = FALSE, only_tests = "multicol_reg_plausibility")
  res = rr_single_table(project_dir, tabid="3", opts=opts)

  # This should now show classes like "issue_wrong_number" and a list at the bottom
  browseURL(res$with_issues_html[[1]])

  rstudioapi::filesPaneNavigate(project_dir)
}

#' Create per-table HTML (and optional PNG) snapshots from the map report inputs
#'
#' @param project_dir The root directory of the project.
#' @param tabid One or more table ids (character or numeric).
#' @param output_dir Directory for outputs. Defaults to 'reports_tables' in project_dir.
#' @param doc_type Document type (e.g., "art", "app1").
#' @param opts Options list, typically from rr_map_report_opts().
#' @param map_prod_id Which map product id to use for coloring. Defaults to the first in opts$map_prod_ids.
#' @param map_version_id Which map version id to use.
#' @param table_png Logical. If TRUE, write a PNG of the colored table using webshot2.
#' @param png_zoom Zoom factor for PNG rendering.
#' @param png_delay Delay (seconds) before screenshot.
#' @param png_vwidth Viewport width for rendering.
#' @param png_vheight Viewport height for rendering.
#' @return Invisibly returns a data.frame with produced file paths.
#' @export
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
  rme = NULL
) {
  restore.point("rr_single_table")

  pkgs = c("dplyr", "tidyr", "stringi", "htmltools", "jsonlite", "purrr", "randtoolbox")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' needed. Please install it."), call. = FALSE)
    }
  }

  if (isTRUE(table_png) && !requireNamespace("webshot2", quietly = TRUE)) {
    stop("table_png=TRUE requires package 'webshot2'.", call. = FALSE)
  }

  if (is.null(opts)) opts = rr_map_report_opts()

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Check wrong number reporting option
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
    stop(paste0("Could not find 'tab_main' for doc_type '", doc_type, "'"), call. = FALSE)
  }
  tab_main = fp_load_prod_df(tab_main_info$ver_dir)
  tab_main$tabid = as.character(tab_main$tabid)

  tabids = as.character(tabid)
  if (length(tabids)==0) tabids=unique(tab_main$tabid)
  tab_rows = dplyr::filter(tab_main, tabid %in% tabids)

  # Load maps
  all_map_types = list()
  for (prod_id in opts$map_prod_ids) {
    map_list = rr_load_all_map_versions(project_dir, doc_type, prod_id = prod_id)
    if (length(map_list) > 0) all_map_types[[prod_id]] = map_list
  }

  if (length(all_map_types) == 0) stop("No map versions found.", call. = FALSE)
  if (is.null(map_prod_id)) map_prod_id = opts$map_prod_ids[1]

  available_versions = names(all_map_types[[map_prod_id]])
  if (is.null(map_version_id)) map_version_id = sort(available_versions)[length(available_versions)]

  # Color map
  all_map_dfs = unlist(all_map_types, recursive = FALSE)
  all_regids = stats::na.omit(unique(unlist(lapply(all_map_dfs, function(df) if ("regid" %in% names(df)) unique(df$regid) else NULL))))
  reg_color_map = rr_make_distinct_colors(length(all_regids))
  names(reg_color_map) = all_regids

  # Evaluation data
  processed_eval_data = NULL
  if (is.null(rme)) {
    rme_file = file.path(project_dir, "fp", paste0("eval_", doc_type), "rme.Rds")
    if (file.exists(rme_file)) {
      tryCatch({
        rme = readRDS(rme_file)

      }, error = function(e) warning(paste0("Could not load rme.Rds: ", e$message)))
    }
  }
  processed_eval_data = rr_process_eval_data(rme, all_map_types, parcels$stata_source$script_source, opts)

  out_rows = list()
  for (i in seq_len(nrow(tab_rows))) {
    row = tab_rows[i, ]
    tabid_i = as.character(row$tabid)

    map_df = all_map_types[[map_prod_id]][[map_version_id]]
    if (is.null(map_df) || nrow(map_df) == 0) next

    map_df_tab = dplyr::filter(map_df, as.character(tabid) == tabid_i)
    mapping = rr_process_single_map_for_js(map_df_tab, reg_color_map, parcels$stata_source$script_source)

    eval_for_tab = NULL
    if (!is.null(processed_eval_data) && map_version_id %in% names(processed_eval_data)) {
      if (tabid_i %in% names(processed_eval_data[[map_version_id]])) {
        eval_for_tab = processed_eval_data[[map_version_id]][[tabid_i]]
      }
    }

    # Extract issues for classification
    issues_info = rr_extract_all_issues(mapping, eval_for_tab, isTRUE(opts$show_wrong_number_report))

    base_name = rr_safe_filename(paste0("tab", tabid_i, "__", map_prod_id, "__", map_version_id))
    file_table_only = file.path(output_dir, paste0(base_name, "__table_only.html"))
    file_with_issues = file.path(output_dir, paste0(base_name, "__with_issues.html"))
    file_png = file.path(output_dir, paste0(base_name, ".png"))

    # 1. Visual HTML (JS/CSS) for PNG
    html_visual = rr_make_single_table_html(
      tabtitle = row$tabtitle,
      tabhtml = row$tabhtml,
      mapping = mapping,
      show_wrong_number = isTRUE(opts$show_wrong_number_report),
      issue_cellids = names(issues_info)
    )

    # 2. AI Prompt HTML (Semantic HTML + Issue List)
    # Inject classes into the table
    ai_table = rr_make_ai_table_html(row$tabhtml, issues_info)
    # Generate the text/table summary of issues
    ai_issues_list = rr_make_ai_issue_summary(mapping, eval_for_tab, isTRUE(opts$show_wrong_number_report))

    # Combine
    full_ai_html = paste0("<html><body>", ai_table, "\n<br><hr>\n", ai_issues_list, "</body></html>")

    htmltools::save_html(html_visual, file = file_table_only)
    writeLines(full_ai_html, file_with_issues)

    png_done = FALSE
    if (isTRUE(table_png)) {
      tryCatch({
        webshot2::webshot(url = file_table_only, file = file_png, selector = "#rr-table-container",
          zoom = png_zoom, delay = png_delay, vwidth = png_vwidth, vheight = png_vheight, quiet = TRUE)
        png_done = TRUE
      }, error = function(e) warning(paste0("PNG failed for tab ", tabid_i, ": ", e$message)))
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

  if (length(out_rows) == 0) return(invisible(data.frame()))
  res = dplyr::bind_rows(out_rows)
  message(paste0("rr_single_table wrote outputs to: ", normalizePath(output_dir, mustWork = FALSE)))
  invisible(res)
}


# ---- Helpers ----

rr_extract_all_issues = function(mapping, eval_for_tab, show_wrong_number) {
  issues_map = list()
  add_issue = function(cids, cls) {
    if (length(cids) == 0) return()
    cids = unique(cids)
    for (cid in cids) issues_map[[cid]] <<- c(issues_map[[cid]], cls)
  }

  if (isTRUE(show_wrong_number) && !is.null(mapping$wrong_number_info) && is.data.frame(mapping$wrong_number_info)) {
    if ("cell_id" %in% names(mapping$wrong_number_info)) {
       add_issue(as.character(mapping$wrong_number_info$cell_id), "issue_wrong_number")
    }
  }

  if (!is.null(eval_for_tab)) {
    for (test_name in names(eval_for_tab)) {
      issues = eval_for_tab[[test_name]]$issues
      if (!is.null(issues)) {
        cids = rr_extract_cellids_from_issue_records(issues)
        add_issue(cids, paste0("issue_", test_name))
      }
    }
  }
  issues_map
}


#' Injects issue classes into HTML table string. Handles loose whitespace in attributes.
rr_make_ai_table_html = function(tabhtml, issues_info) {
  if (length(issues_info) == 0) return(tabhtml)

  res_html = tabhtml
  affected_cells = names(issues_info)

  for (cid in affected_cells) {
    classes = paste(unique(issues_info[[cid]]), collapse = " ")

    # 1. Check if class attribute already exists on this cell
    # Pattern: <td ... id = "cid" ... class = "..."
    # We use \\s* to handle "id =" and "id="
    pat_existing_class = paste0("(<td\\b[^>]*\\bid\\s*=\\s*['\"]", cid, "['\"][^>]*\\bclass\\s*=\\s*['\"])([^'\"]*)")

    if (stringi::stri_detect_regex(res_html, pat_existing_class)) {
      # Append new classes to existing class attribute
      res_html = stringi::stri_replace_first_regex(res_html, pat_existing_class, paste0("$1$2 ", classes, " "))
    } else {
      # 2. No class attribute exists
      # Pattern: <td ... id = "cid" ... >
      pat_no_class = paste0("(<td\\b[^>]*\\bid\\s*=\\s*['\"]", cid, "['\"][^>]*)>")
      # Insert class attribute before closing >
      res_html = stringi::stri_replace_first_regex(res_html, pat_no_class, paste0("$1 class=\"", classes, "\">"))
    }
  }
  res_html
}


#' Generates a static HTML summary of issues for the AI prompt
rr_make_ai_issue_summary = function(mapping, eval_for_tab, show_wrong_number) {
  parts = list("<h3>Issues Report</h3>")

  # 1. Wrong Number Cases
  if (isTRUE(show_wrong_number) && !is.null(mapping$wrong_number_info) && is.data.frame(mapping$wrong_number_info) && nrow(mapping$wrong_number_info) > 0) {
    parts[[length(parts)+1]] = "<h4>issue_wrong_number</h4>"
    wdf = mapping$wrong_number_info
    # Select only useful columns for AI to understand the mismatch
    cols = intersect(names(wdf), c("cell_id", "wrong_number_in_cell", "number_in_stata_output", "runid", "script_num", "code_line"))
    parts[[length(parts)+1]] = rr_df_to_html_table(wdf[, cols, drop=FALSE])
  }

  # 2. Evaluation Tests
  if (!is.null(eval_for_tab)) {
    for (test_name in sort(names(eval_for_tab))) {
      test_block = eval_for_tab[[test_name]]
      issues = test_block$issues
      if (is.null(issues) || length(issues) == 0) next

      parts[[length(parts)+1]] = paste0("<h4>issue_", test_name, "</h4>")
      if (!is.null(test_block$description)) {
        parts[[length(parts)+1]] = paste0("<p><i>", htmltools::htmlEscape(test_block$description), "</i></p>")
      }

      # Convert issues list to DF for printing
      df = tryCatch({
        dplyr::bind_rows(lapply(issues, as.data.frame, stringsAsFactors=FALSE))
      }, error = function(e) NULL)

      if (!is.null(df) && nrow(df) > 0) {
        parts[[length(parts)+1]] = rr_df_to_html_table(df)
      }
    }
  }

  if (length(parts) == 1) return("<p>No issues found.</p>")
  paste(unlist(parts), collapse="\n")
}

rr_df_to_html_table = function(df) {
  if (nrow(df) == 0) return("")
  # Convert all to character for safe printing
  df[] = lapply(df, as.character)

  header = paste0("<tr>", paste0("<th>", names(df), "</th>", collapse=""), "</tr>")
  rows = apply(df, 1, function(row) {
    paste0("<tr>", paste0("<td>", htmltools::htmlEscape(row), "</td>", collapse=""), "</tr>")
  })

  paste0("<table border='1' style='border-collapse:collapse; width:100%;'>",
         header, paste(rows, collapse="\n"), "</table>")
}

# (Existing helpers: rr_safe_filename, rr_single_table_css, rr_single_table_js, rr_extract_cellids_from_issue_records, rr_make_single_table_html remain the same)
rr_safe_filename = function(x) {
  x = as.character(x)
  x = stringi::stri_replace_all_regex(x, "[^A-Za-z0-9_\\-\\.]+", "_")
  x = stringi::stri_replace_all_regex(x, "_{2,}", "_")
  x = stringi::stri_replace_all_regex(x, "^_+|_+$", "")
  x
}
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
    ".issue-cell { outline: 2px solid #dc3545 !important; outline-offset: -2px; }\n"
  )
}
rr_single_table_js = function(reg_info_json, wrong_info_json, show_wrong_number = TRUE, issues_cellids_json = "[]") {
  show_flag = if (isTRUE(show_wrong_number)) "true" else "false"
  paste0(
    "(function(){\n",
    "  var reg_info = ", reg_info_json, " || {};\n",
    "  var wrong_info = ", wrong_info_json, " || [];\n",
    "  var show_wrong = ", show_flag, ";\n",
    "  var issue_cellids = ", issues_cellids_json, " || [];\n",
    "\n",
    "  function apply_static_coloring() {\n",
    "    for (var regid in reg_info) {\n",
    "      if (!reg_info.hasOwnProperty(regid)) continue;\n",
    "      var info = reg_info[regid];\n",
    "      if (!info || !info.color || !info.cell_ids) continue;\n",
    "      var ids = String(info.cell_ids).split(',');\n",
    "      for (var i = 0; i < ids.length; i++) {\n",
    "        var cid = ids[i].trim();\n",
    "        if (!cid) continue;\n",
    "        var el = document.getElementById(cid);\n",
    "        if (!el) continue;\n",
    "        el.style.setProperty('background-color', info.color, 'important');\n",
    "        el.classList.add('statically-colored');\n",
    "      }\n",
    "    }\n",
    "  }\n",
    "\n",
    "  function apply_wrong_number_info() {\n",
    "    if (!show_wrong) return;\n",
    "    if (!wrong_info || !Array.isArray(wrong_info) || wrong_info.length === 0) return;\n",
    "\n",
    "    var cell_to_color = {};\n",
    "    for (var regid in reg_info) {\n",
    "      if (!reg_info.hasOwnProperty(regid)) continue;\n",
    "      var info = reg_info[regid];\n",
    "      if (!info || !info.color || !info.cell_ids) continue;\n",
    "      String(info.cell_ids).split(',').forEach(function(x){ cell_to_color[x.trim()] = info.color; });\n",
    "    }\n",
    "\n",
    "    wrong_info.forEach(function(case_item){\n",
    "      if (!case_item || !case_item.cell_id) return;\n",
    "      var cid = String(case_item.cell_id).trim();\n",
    "      var el = document.getElementById(cid);\n",
    "      if (!el) return;\n",
    "      el.classList.add('wrong-number-cell');\n",
    "      var reg_color = cell_to_color[cid] || '#f0f0f0';\n",
    "      var gradient = 'linear-gradient(45deg, #cccccc, ' + reg_color + ')';\n",
    "      el.style.setProperty('background-image', gradient, 'important');\n",
    "    });\n",
    "  }\n",
    "\n",
    "  function apply_issue_cell_outlines() {\n",
    "    if (!issue_cellids || !Array.isArray(issue_cellids) || issue_cellids.length === 0) return;\n",
    "    issue_cellids.forEach(function(cid_raw){\n",
    "      var cid = String(cid_raw || '').trim();\n",
    "      if (!cid) return;\n",
    "      var el = document.getElementById(cid);\n",
    "      if (!el) return;\n",
    "      el.classList.add('issue-cell');\n",
    "    });\n",
    "  }\n",
    "\n",
    "  apply_static_coloring();\n",
    "  apply_wrong_number_info();\n",
    "  apply_issue_cell_outlines();\n",
    "})();\n"
  )
}
rr_extract_cellids_from_issue_records = function(issues) {
  cids = character(0)
  for (i in seq_along(issues)) {
    rec = issues[[i]]
    if (is.null(rec) || !is.list(rec)) next

    if (!is.null(rec$cellids)) {
      vals = unlist(stringi::stri_split_fixed(as.character(rec$cellids), ",", omit_empty = TRUE))
      vals = stringi::stri_trim_both(vals)
      cids = c(cids, vals)
    } else if (!is.null(rec$cellid)) {
      vals = stringi::stri_trim_both(as.character(rec$cellid))
      cids = c(cids, vals)
    }
  }
  cids
}
rr_make_single_table_html = function(tabtitle, tabhtml, mapping, show_wrong_number = TRUE, issue_cellids = NULL) {
  restore.point("rr_make_single_table_html")

  reg_info_json = jsonlite::toJSON(mapping$reg_info %||% list(), auto_unbox = TRUE, null = "null", na = "null")
  wrong_info_json = jsonlite::toJSON(mapping$wrong_number_info %||% list(), auto_unbox = TRUE, null = "null", na = "null")

  issues_cellids_json = if (!is.null(issue_cellids)) {
     jsonlite::toJSON(as.list(unique(issue_cellids)), auto_unbox = TRUE, null = "null", na = "null")
  } else {
     "[]"
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
          )
        ),
        htmltools::tags$script(htmltools::HTML(js))
      )
    )
  )
}
