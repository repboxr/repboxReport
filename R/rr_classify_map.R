# An extended version of rr_map.R

# Makes reports with 3 columns

# Left: Do code
# Center: tables
# Right: information on regression classification (new)
#        default: summary overview for whole table
#        when clicking on a cell for a particular regression
#        possibly show more details

# Classification results are store in product reg_classify.

# rr_map only has left and center columns


example = function() {
  library(repboxReport)
  # The project_dir needs to be set to a valid repbox project
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"

  options(warn=2)
  # Generate report with default options (embedded data)
  opts = rr_map_report_opts(embed_data = FALSE)
  rep_file = rr_classify_map_report(project_dir,opts = opts)
  browseURL(rep_file)
  rstudioapi::filesPaneNavigate(rep_file)

  rstudioapi::filesPaneNavigate(project_dir)


  # Example with external JSON files
  # This report will need to be viewed via a web server.
  opts <- rr_map_report_opts(embed_data = FALSE)
  opts$output_for <- "all"
  output_dir_ext = file.path(project_dir, "reports_external")
  rr_map_report(project_dir, opts = opts, output_dir = output_dir_ext, output_file = "map_report_full_logs.html")
  # To view, you would run this in the R console:
  # servr::httd(output_dir_ext)

  rstudioapi::filesPaneNavigate(project_dir)
}
# FILE: rr_classify_map.R

#' Load the regression classification product and associated map info
rr_load_reg_classify = function(project_dir, doc_type="art") {
  restore.point("rr_load_reg_classify")

  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  prod_id = "reg_classify"
  ver_dirs = fp_all_ok_ver_dirs(fp_dir, prod_id = prod_id)

  if (length(ver_dirs) == 0) return(NULL)

  ver_dir = ver_dirs[1]
  class_df = fp_load_prod_df(ver_dir)

  pru_file = file.path(ver_dir, "pru.Rds")
  map_ver_info = NULL
  if (file.exists(pru_file)) {
    pru = readRDS(pru_file)
    if (!is.null(pru$map_ver_info)) {
      map_ver_info = pru$map_ver_info
    }
  }

  list(
    class_df = class_df,
    map_ver_info = map_ver_info,
    ver_dir = ver_dir,
    ver_id = basename(ver_dir)
  )
}

#' Create the 3-column Interactive HTML Report with Classifications
#' @export
rr_classify_map_report = function(project_dir,
                                  output_dir = file.path(project_dir, "reports"),
                                  output_file = "classify_map_report.html",
                                  doc_type = "art",
                                  opts = rr_map_report_opts()) {
  restore.point("rr_classify_map_report")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  rr_copy_pkg_assets(output_dir)

  class_info = rr_load_reg_classify(project_dir, doc_type = doc_type)

  if (is.null(class_info) || is.null(class_info$class_df)) {
    warning("No reg_classify data found. Falling back to standard map report.")
    return(rr_map_report(project_dir, output_dir, output_file, doc_type, opts))
  }

  class_df = class_info$class_df
  if ("tabid" %in% names(class_df)) class_df$tabid = as.character(class_df$tabid)
  if ("regid" %in% names(class_df)) class_df$regid = as.character(class_df$regid)

  if (is.null(class_info$map_ver_info)) {
    stop("Could not find map_ver_info in pru.Rds of the classification product.")
  }

  map_prod_id = class_info$map_ver_info$prod_id
  map_ver_id = class_info$map_ver_info$ver_id
  map_ver_dir = class_info$map_ver_info$ver_dir

  if (!dir.exists(map_ver_dir)) {
    stop(paste("The map version directory does not exist:", map_ver_dir))
  }

  map_df = fp_load_prod_df(map_ver_dir)

  all_map_types = list()
  all_map_types[[map_prod_id]] = list()
  all_map_types[[map_prod_id]][[map_ver_id]] = map_df

  parcels = repboxDB::repdb_load_parcels(project_dir, c("stata_source", "stata_cmd", "stata_run_cmd", "stata_run_log"))
  parcels$stata_source$script_source$script_file <- basename(parcels$stata_source$script_source$file_path)

  fp_dir = file.path(project_dir, "fp", paste0("prod_", doc_type))
  tab_main_info = rai_pick_tab_ver(fp_dir, "tab_main")
  tab_main = fp_load_prod_df(tab_main_info$ver_dir)

  do_panel_html = rr_make_do_panel_html(
    parcels$stata_source$script_source,
    parcels$stata_cmd$stata_cmd,
    parcels$stata_run_cmd$stata_run_cmd,
    parcels$stata_run_log$stata_run_log,
    opts,
    all_map_types
  )
  tab_panel_html = rr_make_tab_panel_html(tab_main)

  controls_html = paste0(
    '<div style="display:none;">',
      '<select id="map_type_selector"><option value="', map_prod_id, '" selected>', map_prod_id, '</option></select>',
      '<select id="version_selector"><option value="', map_ver_id, '" selected>', map_ver_id, '</option></select>',
    '</div>'
  )

  classify_panel_html = rr_make_classify_panel_html()

  js_maps_data = "{}"
  js_manifest_data = "{}"
  js_class_data = "null"
  js_class_file = "null"

  all_regids = if("regid" %in% names(map_df)) unique(stats::na.omit(map_df$regid)) else character(0)
  reg_color_map = rr_make_distinct_colors(length(all_regids))
  names(reg_color_map) = all_regids

  if (isTRUE(opts$embed_data)) {
    processed_map = rr_process_single_map_for_js(map_df, reg_color_map, parcels$stata_source$script_source)
    maps_struct = list(); maps_struct[[map_prod_id]] = list(); maps_struct[[map_prod_id]][[map_ver_id]] = processed_map
    js_maps_data = jsonlite::toJSON(maps_struct, auto_unbox = TRUE, null = "null", na = "null")
    js_class_data = jsonlite::toJSON(class_df, auto_unbox = TRUE, null = "null", na = "null")
  } else {
    processed_map = rr_process_single_map_for_js(map_df, reg_color_map, parcels$stata_source$script_source)
    maps_data_dir = file.path(output_dir, "maps_data")
    if (!dir.exists(maps_data_dir)) dir.create(maps_data_dir, recursive = TRUE)

    map_filename = paste0(map_prod_id, "_", map_ver_id, ".json")
    map_json = jsonlite::toJSON(processed_map, auto_unbox = TRUE, null = "null", na = "null")
    writeLines(map_json, file.path(maps_data_dir, map_filename))

    manifest = list(); manifest[[map_prod_id]] = list(); manifest[[map_prod_id]][[map_ver_id]] = file.path("maps_data", map_filename)
    js_manifest_data = jsonlite::toJSON(manifest, auto_unbox = TRUE)

    class_data_dir = file.path(output_dir, "class_data")
    if (!dir.exists(class_data_dir)) dir.create(class_data_dir, recursive = TRUE)
    class_filename = paste0("class_", class_info$ver_id, ".json")
    class_json = jsonlite::toJSON(class_df, auto_unbox = TRUE, null = "null", na = "null")
    writeLines(class_json, file.path(class_data_dir, class_filename))
    js_class_file = jsonlite::toJSON(file.path("class_data", class_filename), auto_unbox = TRUE)
  }

  html_content = htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$title(paste("Classification:", basename(project_dir))),
      htmltools::tags$link(href = "shared/bootstrap.min.css", rel = "stylesheet"),
      htmltools::tags$link(href = "shared/repbox.css", rel = "stylesheet"),
      htmltools::tags$style(htmltools::HTML("
        /* 3rd Column Layout */
        #classify-col-div { height: 100%; padding: 0; border: 1px solid #ddd; border-left: none; border-radius: 0 4px 4px 0; overflow-y: auto; background: #fafafa; }
        #classify-content { padding: 10px; }

        /* Compact Line Numbers Overrides */
        #do-col-div { padding-left: 2px !important; }
        .code-line-td { padding-left: 1px !important; padding-right: 3px !important; width: 1%; white-space: nowrap; font-size: 0.9em; }
        .do-pre { padding-left: 2px !important; }

        /* Classification Styling */
        .class-section { margin-bottom: 15px; border-bottom: 1px solid #eee; padding-bottom: 10px; }
        .class-section:last-child { border-bottom: none; }
        .class-header { font-size: 1.1em; font-weight: bold; margin-bottom: 5px; color: #337ab7; }
        .class-desc { font-style: italic; color: #555; margin-bottom: 8px; }

        .tag-container { margin-bottom: 8px; line-height: 1.8; }
        .label-tag { font-size: 85%; margin-right: 4px; display: inline-block; padding: .2em .6em .3em; }

        .kv-row { font-size: 0.9em; margin-bottom: 4px; }
        .kv-label { font-weight: bold; color: #666; width: 120px; display: inline-block; vertical-align: top;}
        .kv-val { color: #222; display: inline-block; width: calc(100% - 125px); }

        .class-table { width: 100%; font-size: 0.85em; margin-top: 5px; background: #fff; border: 1px solid #ddd; }
        .class-table th { background: #f0f0f0; padding: 4px; border-bottom: 2px solid #ddd; font-weight: 600; color: #444; }
        .class-table td { padding: 4px; border-bottom: 1px solid #eee; vertical-align: top; }
        .class-table tr:last-child td { border-bottom: none; }

        .var-type-badge { font-size: 0.8em; padding: 1px 4px; border-radius: 3px; background: #eee; color: #555; border: 1px solid #ccc; white-space: nowrap; }

        /* Summary Grouping Styles */
        .summary-group { background: #fff; border: 1px solid #e5e5e5; border-radius: 4px; padding: 8px; margin-bottom: 10px; border-left: 3px solid #337ab7; }
        .summary-reg-list { font-family: monospace; color: #333; word-break: break-all; }
        .summary-dim-line { font-size: 0.9em; color: #666; margin-top: 4px; }

        /* Interactive Rows */
        .interactive-row { cursor: pointer; transition: background-color 0.1s; }
        .interactive-row:hover { background-color: #e6f2ff; }

        /* Active Row in Right Panel (Yellow Highlight) */
        .active-class-row {
            background-color: #ffffd0 !important;
            outline: 1px solid #e6c600;
        }

        /* Active Regression Highlight in Table (Red Borders) */
        .active-regression-group {
            background-color: rgba(217, 83, 79, 0.05) !important; /* Very light red tint */
            border-left: 2px solid #d9534f !important; /* Bootstrap 'danger' red */
            border-right: 2px solid #d9534f !important;
        }

        /* Ensure selected cell (yellow) stays on top */
        .cell-highlight {
            outline: 3px solid #ffd700 !important;
            z-index: 10;
            position: relative;
        }
      "))
    ),
    htmltools::tags$body(
      htmltools::tags$div(class = "container-fluid",
        htmltools::HTML(controls_html),
        htmltools::tags$div(class = "row", style = "height: 95vh;",
          htmltools::tags$div(id = "do-col-div", class = "col-sm-4", htmltools::HTML(do_panel_html)),
          htmltools::tags$div(id = "tabs-col-div", class = "col-sm-4", htmltools::HTML(tab_panel_html)),
          htmltools::tags$div(id = "classify-col-div", class = "col-sm-4", htmltools::HTML(classify_panel_html))
        )
      ),
      htmltools::tags$script(src = "shared/jquery.min.js"),
      htmltools::tags$script(src = "shared/bootstrap.min.js"),

      htmltools::tags$script(htmltools::HTML(paste0("var data_is_embedded = ", tolower(isTRUE(opts$embed_data)), ";"))),
      htmltools::tags$script(htmltools::HTML(paste0("var show_wrong_number_report_opt = ", tolower(isTRUE(opts$show_wrong_number_report)), ";"))),
      htmltools::tags$script(htmltools::HTML(paste0("var all_maps = ", js_maps_data, ";"))),
      htmltools::tags$script(htmltools::HTML(paste0("var report_manifest = ", js_manifest_data, ";"))),
      htmltools::tags$script(htmltools::HTML(paste0("var all_classifications = ", js_class_data, ";"))),
      htmltools::tags$script(htmltools::HTML(paste0("var classification_file = ", js_class_file, ";"))),

      htmltools::tags$script(htmltools::HTML("var cell_conflict_data = {}; var all_evals = {}; var eval_manifest = {};")),

      htmltools::tags$script(src = "shared/report_map.js"),
      htmltools::tags$script(src = "shared/rr_classify_map.js")
    )
  )

  report_path = file.path(output_dir, output_file)
  htmltools::save_html(html_content, file = report_path)
  return(invisible(report_path))
}

rr_make_classify_panel_html = function() {
  paste0(
    '<div id="classify-content">',
    '  <h4>Regression Classification</h4>',
    '  <div id="classify-details"></div>',
    '</div>'
  )
}
