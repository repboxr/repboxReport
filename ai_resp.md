!MODIFICATION add webshot2 to DESCRIPTION
scope = "lines"
file = "DESCRIPTION"
insert_after_lines = "         dplyr, tidyr, purrr, jsonlite, randtoolbox, FuzzyProduction, repboxAI"
description = '''Add webshot2 as an optional dependency for rendering per-table PNG screenshots.'''
---------------------------------------------------------------------------------------------------

```
Suggests: webshot2
```

!END_MODIFICATION add webshot2 to DESCRIPTION

!MODIFICATION add rr_single_table.R
scope = "file"
file = "R/rr_single_table.R"
is_new_file = true
description = '''Add rr_single_table to generate per-table HTML (minimal for PNG rendering + full with issues), using the same reg color coding and optional wrong-number + evaluation issues rendering.'''
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
#' Create per-table HTML (and optional PNG) snapshots from the map report inputs
#'
#' Generates, for each requested table id, two HTML files:
#' 1) a minimal HTML that only contains the colored table (suitable for webshot PNG),
#' 2) a larger HTML that additionally lists issues (wrong-number cases + evaluation issues),
#'    respecting opts$only_tests and opts$ignore_tests.
#'
#' Optionally creates a PNG screenshot of the colored table using webshot2.
#'
#' @param project_dir The root directory of the project.
#' @param tabid One or more table ids (character or numeric).
#' @param output_dir Directory for outputs. Defaults to 'reports_tables' in project_dir.
#' @param doc_type Document type (e.g., "art", "app1").
#' @param opts Options list, typically from rr_map_report_opts(). only_tests/ignore_tests are respected.
#' @param map_prod_id Which map product id to use for coloring. Defaults to the first in opts$map_prod_ids.
#' @param map_version_id Which map version id to use. If NULL, a deterministic "latest" is chosen (lexicographically last).
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
  png_vheight = 900
) {
  restore.point("rr_single_table")

  pkgs = c("dplyr", "tidyr", "stringi", "htmltools", "jsonlite", "purrr", "randtoolbox")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Please install the '", pkg, "' package."), call. = FALSE)
    }
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
  if (length(tabids) == 0) stop("tabid must contain at least one table id.", call. = FALSE)

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
      eval_for_tab = NULL
    )

    html_issues = rr_make_single_table_html(
      tabtitle = row$tabtitle,
      tabhtml = row$tabhtml,
      mapping = mapping,
      show_wrong_number = isTRUE(opts$show_wrong_number_report),
      include_issues = TRUE,
      eval_for_tab = eval_for_tab
    )

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

# ---- Internal helpers ----

rr_safe_filename = function(x) {
  x = as.character(x)
  x = stringi::stri_replace_all_regex(x, "[^A-Za-z0-9_\\-\\.]+", "_")
  x = stringi::stri_replace_all_regex(x, "_{2,}", "_")
  x = stringi::stri_replace_all_regex(x, "^_+|_+$", "")
  x
}

rr_make_single_table_html = function(tabtitle, tabhtml, mapping, show_wrong_number = TRUE, include_issues = FALSE, eval_for_tab = NULL) {
  restore.point("rr_make_single_table_html")

  reg_info_json = jsonlite::toJSON(mapping$reg_info %||% list(), auto_unbox = TRUE, null = "null", na = "null")
  wrong_info_json = jsonlite::toJSON(mapping$wrong_number_info %||% list(), auto_unbox = TRUE, null = "null", na = "null")

  issues_html = ""
  issues_cellids_json = "[]"

  if (isTRUE(include_issues)) {
    issues_pack = rr_build_issues_block(mapping = mapping, show_wrong_number = show_wrong_number, eval_for_tab = eval_for_tab)
    issues_html = issues_pack$issues_html
    issues_cellids_json = issues_pack$issues_cellids_json
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
    ".issue-cell { outline: 2px solid #dc3545 !important; outline-offset: -2px; }\n",
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

rr_build_issues_block = function(mapping, show_wrong_number = TRUE, eval_for_tab = NULL) {
  restore.point("rr_build_issues_block")

  issue_cellids = character(0)

  parts = list()
  parts[[length(parts) + 1]] = '<div class="rr-issues">'
  parts[[length(parts) + 1]] = '<h5>Issues</h5>'

  # Wrong number cases
  if (isTRUE(show_wrong_number) && !is.null(mapping$wrong_number_info) && is.data.frame(mapping$wrong_number_info) && nrow(mapping$wrong_number_info) > 0) {
    wdf = mapping$wrong_number_info
    if ("cell_id" %in% names(wdf)) issue_cellids = c(issue_cellids, as.character(wdf$cell_id))

    parts[[length(parts) + 1]] = paste0('<h6>Discrepancies (wrong-number cases) <span class="rr-badge">', nrow(wdf), "</span></h6>")
    parts[[length(parts) + 1]] = "<table><thead><tr>",
    parts[[length(parts) + 1]] = "<th>cell_id</th><th>wrong_number_in_cell</th><th>number_in_stata_output</th><th>runid</th><th>script_num</th><th>code_line</th>",
    parts[[length(parts) + 1]] = "</tr></thead><tbody>"
    for (i in seq_len(nrow(wdf))) {
      parts[[length(parts) + 1]] = paste0(
        "<tr>",
        "<td><code>", htmltools::htmlEscape(as.character(wdf$cell_id[i])), "</code></td>",
        "<td>", htmltools::htmlEscape(as.character(wdf$wrong_number_in_cell[i])), "</td>",
        "<td>", htmltools::htmlEscape(as.character(wdf$number_in_stata_output[i])), "</td>",
        "<td>", htmltools::htmlEscape(as.character(wdf$runid[i])), "</td>",
        "<td>", htmltools::htmlEscape(as.character(wdf$script_num[i])), "</td>",
        "<td>", htmltools::htmlEscape(as.character(wdf$code_line[i])), "</td>",
        "</tr>"
      )
    }
    parts[[length(parts) + 1]] = "</tbody></table>"
  } else {
    parts[[length(parts) + 1]] = "<p class='rr-descr'>No wrong-number discrepancies reported (or disabled by option).</p>"
  }

  # Evaluation issues (from rme.Rds processed via rr_process_eval_data)
  if (!is.null(eval_for_tab) && length(eval_for_tab) > 0) {
    test_names = sort(names(eval_for_tab))
    for (test_name in test_names) {
      test_block = eval_for_tab[[test_name]]
      if (is.null(test_block) || is.null(test_block$issues)) next

      issues = test_block$issues
      if (is.null(issues) || length(issues) == 0) next

      # Extract cellids for highlighting
      issue_cellids = c(issue_cellids, rr_extract_cellids_from_issue_records(issues))

      parts[[length(parts) + 1]] = paste0(
        "<h6>Test: <code>", htmltools::htmlEscape(test_name), "</code>",
        " <span class='rr-badge'>", length(issues), "</span></h6>"
      )
      if (!is.null(test_block$description) && nchar(as.character(test_block$description)) > 0) {
        parts[[length(parts) + 1]] = paste0("<p class='rr-descr'>", htmltools::htmlEscape(as.character(test_block$description)), "</p>")
      }

      parts[[length(parts) + 1]] = rr_issue_records_to_html_table(issues)
    }
  } else {
    parts[[length(parts) + 1]] = "<p class='rr-descr'>No evaluation issues for this table/version (or no rme.Rds found).</p>"
  }

  parts[[length(parts) + 1]] = "</div>"

  issue_cellids = unique(stats::na.omit(issue_cellids))
  issue_cellids_json = jsonlite::toJSON(as.list(issue_cellids), auto_unbox = TRUE, null = "null", na = "null")

  list(
    issues_html = paste0(unlist(parts), collapse = "\n"),
    issues_cellids_json = issue_cellids_json
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

rr_issue_records_to_html_table = function(issues) {
  if (is.null(issues) || length(issues) == 0) return("")

  df = tryCatch({
    as.data.frame(purrr::map_dfc(issues, function(x) x), stringsAsFactors = FALSE)
  }, error = function(e) NULL)

  # If conversion fails (ragged records), fall back to row-wise union of keys
  if (is.null(df)) {
    all_keys = unique(unlist(lapply(issues, names)))
    df = do.call(rbind, lapply(issues, function(rec) {
      row = setNames(as.list(rep("", length(all_keys))), all_keys)
      for (k in names(rec)) row[[k]] = as.character(rec[[k]])
      as.data.frame(row, stringsAsFactors = FALSE)
    }))
  }

  # Ensure character columns
  for (j in seq_along(df)) df[[j]] = as.character(df[[j]])

  headers = names(df)
  th = paste0("<th>", htmltools::htmlEscape(headers), "</th>", collapse = "")
  rows = lapply(seq_len(nrow(df)), function(i) {
    tds = paste0("<td>", htmltools::htmlEscape(df[i, , drop = TRUE]), "</td>", collapse = "")
    paste0("<tr>", tds, "</tr>")
  })

  paste0(
    "<table><thead><tr>", th, "</tr></thead><tbody>",
    paste0(unlist(rows), collapse = "\n"),
    "</tbody></table>"
  )
}
```

!END_MODIFICATION add rr_single_table.R
