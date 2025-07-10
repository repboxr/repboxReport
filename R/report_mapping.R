example = function() {
  project_dir = "/home/rstudio/repbox/projects_gha_new/aejapp_10_4_6"

  rai_mapping_report(project_dir)
  rstudioapi::filesPaneNavigate(project_dir)
}

#' Creates an interactive HTML report to visualize mappings between
#' Stata do-files and regression tables.
#'
#' This function generates a self-contained HTML report with two main panels.
#' The left panel displays the Stata do-files with syntax highlighting and
#' collapsible log outputs for executed commands. The right panel displays
#' the article's regression tables. The report is interactive: clicking on a
#' mapped regression result in a table highlights the corresponding code, and
#' clicking on a mapped regression command highlights the corresponding table cells.
#' It also supports switching between different mapping versions (e.g., from
#' different AI models).
#'
#' @param project_dir The root directory of the project, containing the 'fp' and 'parcels' subdirectories.
#' @param output_dir The directory where the final report and its assets will be saved. Defaults to a 'reports' subdirectory in the project_dir.
#' @param output_file The name of the HTML report file.
#' @param doc_type The document type (e.g., "art", "app1") to generate the report for.
#' @param mapping_prod_id The product ID for the regression mappings to be used.
#' @return The path to the generated HTML report file.
#' @export
#' @examples
#' \dontrun{
#' # Assuming you have a project set up in "/path/to/my_project"
#' create_mapping_report(project_dir = "/path/to/my_project", doc_type = "art")
#' }
rai_mapping_report <- function(project_dir,
                                  output_dir = file.path(project_dir, "reports"),
                                  output_file = "mapping_report.html",
                                  doc_type = "art",
                                  mapping_prod_id = "map_reg_run") {
  
  restore.point("rai_mapping_report")
  # --- 0. Check dependencies ---
  pkgs <- c("dplyr", "tidyr", "stringi", "htmltools", "jsonlite", "purrr", "FuzzyProduction")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Please install the '", pkg, "' package."), call. = FALSE)
    }
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  assets_dir <- file.path(output_dir, "shared")
  if (!dir.exists(assets_dir)) {
    dir.create(assets_dir)
  }

  # --- 2. Load data ---
  cat("\nLoading data...")
  parcels <- repboxDB::repdb_load_parcels(project_dir, c("stata_source", "stata_run_cmd", "stata_run_log", "stata_cmd"))
  
  fp_dir <- file.path(project_dir, "fp", paste0("prod_", doc_type))
  tab_main_info <- rai_pick_tab_ver(fp_dir, "tab_main")
  if(nrow(tab_main_info) == 0) {
      stop("Could not find a suitable 'tab_main' product for doc_type '", doc_type, "'")
  }
  tab_main <- fp_load_prod_df(tab_main_info$ver_dir)
  
  mapping_list <- rai_load_all_mapping_versions(project_dir, doc_type, prod_id = mapping_prod_id)
  if(length(mapping_list) == 0) {
      warning("No mapping versions found for prod_id '", mapping_prod_id, "'. The report will be generated without interactive links.")
  }

  # --- 3. Generate HTML & JS components ---
  message("Generating HTML components...")
  js_mappings_data <- generate_js_mappings(mapping_list)
  
  stata_source_df <- script_df_shorten_file(parcels$stata_source$script_source)
  
  do_panel_html <- generate_do_panel_html(
    stata_source = stata_source_df,
    stata_cmd = parcels$stata_cmd$stata_cmd,
    stata_run_cmd = parcels$stata_run_cmd$stata_run_cmd,
    stata_run_log = parcels$stata_run_log$stata_run_log
  )
  
  tab_panel_html <- generate_tab_panel_html(tab_main)
  
  version_selector_html <- generate_version_selector_html(names(mapping_list))
  
  # --- 4. Write JS and CSS assets ---
  cat("\nWriting JS and CSS assets...")
  write_report_js(output_dir)
  write_report_css(output_dir)
  copy_bs_assets(assets_dir)

  # --- 5. Assemble and write final HTML report ---
  cat("\nAssembling final HTML report...")
  
  html_content <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$meta(`http-equiv` = "Content-Type", content = "text/html; charset=utf-8"),
      htmltools::tags$title(paste("Mapping Report:", basename(project_dir))),
      htmltools::tags$script(src = "shared/jquery.min.js"),
      htmltools::tags$link(href = "shared/bootstrap.min.css", rel = "stylesheet"),
      htmltools::tags$script(src = "shared/bootstrap.min.js"),
      htmltools::tags$link(href = "shared/repbox.css", rel = "stylesheet"),
      htmltools::tags$script(src = "shared/report_mapping.js")
    ),
    htmltools::tags$body(
      htmltools::tags$div(class = "container-fluid",
        htmltools::HTML(version_selector_html),
        htmltools::tags$div(class = "row", style = "height: 95vh;",
          htmltools::tags$div(id = "do-col-div", class = "col-sm-7", style = "overflow-y: scroll; height:100%; padding: 5px;",
            htmltools::HTML(do_panel_html)
          ),
          htmltools::tags$div(id = "tabs-col-div", class = "col-sm-5", style = "overflow-y: scroll; height:100%; padding: 5px;",
            htmltools::HTML(tab_panel_html)
          )
        )
      ),
      htmltools::tags$script(
        htmltools::HTML(paste0("var all_mappings = ", js_mappings_data, ";"))
      )
    )
  )

  report_path <- file.path(output_dir, output_file)
  htmltools::save_html(html_content, file = report_path, libdir = "lib")
  
  # Cleanup libdir as we are using shared assets
  lib_path <- file.path(output_dir, "lib")
  if(dir.exists(lib_path)) unlink(lib_path, recursive = TRUE)

  cat(paste("\nReport generated successfully at:", report_path))
  return(invisible(report_path))
}


# --- Helper Functions ---

#' Load all available versions of a given product
rai_load_all_mapping_versions <- function(project_dir, doc_type, prod_id) {
  restore.point("rai_load_all_mapping_versions")
  fp_dir <- file.path(project_dir, "fp", paste0("prod_", doc_type))
  prod_path <- file.path(fp_dir, prod_id)
  if (!dir.exists(prod_path)) return(list())
  
  proc_dirs <- list.dirs(prod_path, recursive = FALSE, full.names = TRUE)
  if (length(proc_dirs) == 0) return(list())
  
  pd = proc_dirs[1]
  ver_list <- lapply(proc_dirs, function(pd) {
    proc_id = fp_proc_dir_to_proc_id(pd)
    ver_dirs <- fp_proc_dir_to_ver_dirs(pd)
    if (length(ver_dirs)==0) return(NULL)
    # TO DO: allow multiple ver_dir
    # but add options that can limit max no
    ver_dir = ver_dirs[1]
    df = tryCatch(fp_load_prod_df(ver_dir), error = function(e) NULL)
    list(
      proc_id = proc_id,
      df = df 
    )
  })
  
  ver_list <- ver_list[!sapply(ver_list, is.null)]
  ver_list <- ver_list[!sapply(ver_list, function(x) is.null(x$df) || nrow(x$df) == 0)]

  if(length(ver_list) == 0) return(list())
  
  df_list <- lapply(ver_list, `[[`, "df")
  names(df_list) <- sapply(ver_list, `[[`, "proc_id")
  return(df_list)
}

#' Generate JS object with mappings for interactivity
generate_js_mappings <- function(mapping_list) {
  all_mappings <- lapply(mapping_list, function(map_df) {
    if (is.null(map_df) || nrow(map_df) == 0) return(list(cell_to_code = list(), code_to_cells = list()))

    # Forward mapping: cell_id -> code location
    cell_map_df <- map_df %>%
      dplyr::filter(!is.na(cell_ids) & cell_ids != "") %>%
      dplyr::mutate(cell_id = strsplit(as.character(cell_ids), ",")) %>%
      tidyr::unnest(cell_id) %>%
      dplyr::mutate(cell_id = trimws(cell_id)) %>%
      dplyr::select(cell_id, runid, script_num, code_line)

    cell_to_code <- setNames(
      lapply(1:nrow(cell_map_df), function(i) as.list(cell_map_df[i, c("runid", "script_num", "code_line")])),
      cell_map_df$cell_id
    )

    # Backward mapping: code_line -> cell_ids
    code_map_df <- map_df %>%
      dplyr::filter(!is.na(code_line) & !is.na(script_num)) %>%
      dplyr::select(script_num, code_line, tabid, cell_ids) %>%
      dplyr::distinct()

    code_to_cells <- setNames(
      lapply(1:nrow(code_map_df), function(i) as.list(code_map_df[i, c("tabid", "cell_ids")])),
      paste0("s", code_map_df$script_num, "_l", code_map_df$code_line)
    )
    
    list(cell_to_code = cell_to_code, code_to_cells = code_to_cells)
  })
  
  jsonlite::toJSON(all_mappings, auto_unbox = TRUE, null = "null", na = "null")
}

#' Generate HTML for the Stata do-file panel
generate_do_panel_html <- function(stata_source, stata_cmd, stata_run_cmd, stata_run_log) {
  restore.point("generate_do_panel_html")
  # --- Data Preparation ---
  
  run_df <- stata_run_cmd %>% 
    left_join(stata_source %>% select(artid, file_path, script_num), by=c("artid", "file_path")) %>%
    left_join(stata_run_log, by = c("artid", "runid", "script_num"))
  
  # Base data frame: one row per line in each script
  ldf <- stata_source %>%
    dplyr::mutate(text_lines = stringi::stri_split_lines(text)) %>%
    dplyr::select(script_num, file_path, text_lines) %>%
    tidyr::unnest(text_lines) %>%
    dplyr::group_by(script_num) %>%
    dplyr::mutate(orgline = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Expand multi-line commands
  cmd_info <- stata_cmd %>%
    dplyr::select(file_path, line, orgline_start, orgline_end, is_reg, cmd, cmdline) %>%
    dplyr::mutate(orgline = purrr::map2(orgline_start, orgline_end, seq)) %>%
    tidyr::unnest(orgline)
  
  # Join lines with command info
  ldf <- dplyr::left_join(ldf, cmd_info, by = c("file_path", "orgline"))

  # Aggregate run info by line
  run_info <- run_df %>%
    dplyr::group_by(file_path, line) %>%
    dplyr::summarise(
      runs = list(dplyr::tibble(runid, logtxt, errcode, missing_data)),
      .groups = "drop"
    )

  first_lines <- stata_cmd %>%
    dplyr::select(file_path, line, orgline = orgline_start) %>%
    dplyr::distinct() %>%
    dplyr::left_join(run_info, by = c("file_path", "line"))

  ldf <- dplyr::left_join(ldf, first_lines, by = c("file_path", "line", "orgline"))
  
  # --- HTML Generation ---
  script_tabs_content <- lapply(split(ldf, ldf$script_num), function(df) {
    restore.point("sdhskf")
    script_num_val <- df$script_num[1]
    
    i = 1
    rows_html <- purrr::map(1:nrow(df), function(i) {
      
      row <- df[i,]
      
      line_class <- "norun-line"
      title <- "NA"
      run_info_list <- row$runs[[1]]
      
      if (!is.null(run_info_list) && nrow(run_info_list) > 0) {
        first_run <- run_info_list[1,]
        if (any(is.true(run_info_list$errcode != 0))) line_class <- "err-line" else line_class <- "noerr-line"
        if (any(is.true(run_info_list$missing_data))) line_class <- "mida-line"
        if (isTRUE(row$is_reg)) line_class <- paste(line_class, "reg-cmd")
        
        title <- paste0("runid: ", paste(run_info_list$runid, collapse=", "))
        if (any(run_info_list$missing_data)) title <- paste(title, " missing data")
      }

      code_tag <- htmltools::tags$code(id = paste0("L", row$orgline, "___", script_num_val), class = line_class, title = title, htmltools::htmlEscape(row$text_lines))
      
      log_div <- if (!is.null(run_info_list) && nrow(run_info_list) > 0) {
        log_html <- generate_log_html(run_info_list)
        htmltools::tags$div(class = "collapse", id = paste0("loginfo-", row$orgline, "-", script_num_val), htmltools::HTML(log_html))
      } else { "" }
      
      button_td <- if (!is.null(run_info_list) && nrow(run_info_list) > 0) {
        htmltools::tags$td(
          htmltools::tags$a(class="btn btn-xs", role="button", `data-toggle`="collapse", href=paste0("#loginfo-", row$orgline, "-", script_num_val), `aria-expanded`="false", htmltools::HTML("▼"))
        )
      } else { htmltools::tags$td() }
      
      htmltools::tags$tr(
        button_td,
        htmltools::tags$td(class = "code-line-td", row$orgline),
        htmltools::tags$td(
          htmltools::tags$pre(class = "do-pre",
            code_tag,
            log_div
          )
        )
      )
    })
    
    htmltools::tags$div(class = if(script_num_val==1) "tab-pane active" else "tab-pane", id = paste0("dotab_", script_num_val),
      htmltools::tags$table(class = "code-tab",
        htmltools::tags$tr(
          htmltools::tags$td(colspan="3",
            htmltools::tags$button(class="toogle-all-results btn btn-xs", title="Show or hide all results", 
            onclick=paste0("$('#dotab_", script_num_val, " .collapse').collapse('toggle');"), htmltools::HTML("▼"))
          )
        ),
        rows_html
      )
    )
  })
  
  script_pills <- htmltools::tags$ul(id = "dotabs", class="nav nav-pills", role="tablist",
    lapply(1:nrow(stata_source), function(i) {
      row <- stata_source[i, ]
      active_class <- if(i == 1) "active" else ""
      htmltools::tags$li(class = active_class,
        htmltools::tags$a(href=paste0("#dotab_", row$script_num), role="tab", `data-toggle`="tab", row$script_file)
      )
    })
  )
  
  as.character(htmltools::tagList(script_pills, htmltools::tags$div(class="tab-content", script_tabs_content)))
}

#' Generate log HTML for a line, handling multiple runs with tabs
generate_log_html <- function(runs_for_line) {
  if (is.null(runs_for_line) || nrow(runs_for_line) == 0) return("")
  
  random_id <- function() paste0(sample(c(letters, LETTERS), 12, replace = TRUE), collapse = "")
  
  if (nrow(runs_for_line) == 1) {
    run <- runs_for_line[1, ]
    return(as.character(htmltools::tags$pre(id = paste0("runid-", run$runid), class = "logtxt-pre",
      htmltools::tags$code(class = "logtxt-code", htmltools::htmlEscape(run$logtxt)))))
  }

  tabset_id <- paste0("tabset_", random_id())
  tabs_data <- lapply(1:nrow(runs_for_line), function(i) list(id = paste0("tab_", random_id()), run = runs_for_line[i,]))
  
  tab_pills <- htmltools::tags$ul(id = tabset_id, class = "nav nav-tabs small-tab-ul", role = "tablist",
    lapply(1:length(tabs_data), function(i) {
      htmltools::tags$li(class = if (i == 1) "active" else "",
        htmltools::tags$a(href = paste0("#", tabs_data[[i]]$id), role = "tab", `data-toggle` = "tab", paste("Run", i)))
    })
  )
  
  tab_content <- htmltools::tags$div(class = "tab-content",
    lapply(1:length(tabs_data), function(i) {
      run <- tabs_data[[i]]$run
      htmltools::tags$div(class = if (i == 1) "tab-pane active" else "tab-pane", id = tabs_data[[i]]$id,
        htmltools::tags$pre(id = paste0("runid-", run$runid), class = "logtxt-pre",
          htmltools::tags$code(class = "logtxt-code", htmltools::htmlEscape(run$logtxt))))
    })
  )
  as.character(htmltools::tagList(tab_pills, tab_content))
}


#' Generate HTML for the table display panel
generate_tab_panel_html <- function(tab_main) {
  restore.point("generate_tab_panel_html")
  tab_pills <- htmltools::tags$ul(id="tabtabs", class="nav nav-pills", role="tablist",
    lapply(1:nrow(tab_main), function(i) {
      row <- tab_main[i,]
      active_class <- if(i == 1) "active" else ""
      htmltools::tags$li(class = active_class,
        htmltools::tags$a(href=paste0("#tabtab", row$tabid), role="tab", `data-toggle`="tab", paste("Tab", row$tabid))
      )
    })
  )
  
  tab_content <- htmltools::tags$div(class="tab-content",
    lapply(1:nrow(tab_main), function(i) {
      row <- tab_main[i,]
      active_class <- if(i == 1) "tab-pane active" else "tab-pane"
      htmltools::tags$div(class = active_class, id = paste0("tabtab", row$tabid),
        htmltools::tags$div(class="art-tab-div",
          htmltools::h5(row$tabtitle),
          htmltools::HTML(row$tabhtml)
        )
      )
    })
  )
  as.character(htmltools::tagList(tab_pills, tab_content))
}

#' Generate HTML for the mapping version selector
generate_version_selector_html <- function(versions) {
  if (length(versions) <= 1) return("")
  
  options <- lapply(versions, function(v) htmltools::tags$option(value = v, v))
  
  as.character(
    htmltools::tags$div(style="padding: 10px;",
      htmltools::tags$label(`for`="version_selector", "Mapping Version: "),
      htmltools::tags$select(id = "version_selector", class="form-control", style="width: auto; display: inline-block;",
        options
      )
    )
  )
}

#' Write the main JS file for the report
write_report_js <- function(output_dir) {
  js_code <- '
var active_mapping = {};
var last_code_highlight = "";
var last_cell_highlights = [];

function clear_all_highlights() {
    if (last_code_highlight) {
        $(last_code_highlight).removeClass("code-highlight");
        last_code_highlight = "";
    }
    last_cell_highlights.forEach(function(id) {
        $(id).removeClass("cell-highlight");
    });
    last_cell_highlights = [];
}

function highlight_code(script_num, line_num) {
    $("#dotabs a[href=\'#dotab_" + script_num + "\']").tab("show");
    const code_id = "#L" + line_num + "___" + script_num;
    $(code_id).addClass("code-highlight");
    last_code_highlight = code_id;
    const scroll_target_id = "#B" + Math.max(1, line_num - 3) + "___" + script_num;
    const targetElement = document.querySelector(scroll_target_id);
    if(targetElement) {
        targetElement.scrollIntoView({ behavior: "smooth", block: "center" });
    }
}

function highlight_cells(tabid, cell_ids_string) {
    if (!tabid || !cell_ids_string) return;
    $("#tabtabs a[href=\'#tabtab" + tabid + "\']").tab("show");
    const cell_ids = cell_ids_string.split(",");
    cell_ids.forEach(id => {
        const cell_selector = "#" + id.trim();
        $(cell_selector).addClass("cell-highlight");
        last_cell_highlights.push(cell_selector);
    });
    if (cell_ids.length > 0) {
        const targetElement = document.querySelector("#" + cell_ids[0].trim());
        if(targetElement) {
           targetElement.scrollIntoView({ behavior: "smooth", block: "center" });
        }
    }
}

$(document).ready(function() {
    const versions = Object.keys(all_mappings);
    if (versions.length > 0) {
        active_mapping = all_mappings[versions[0]];
    }

    $("#version_selector").on("change", function() {
        const selected_version = $(this).val();
        active_mapping = all_mappings[selected_version];
        clear_all_highlights();
    });

    $(document).on("click", ".tabnum, [id^=c][id*=_]", function(event) {
        clear_all_highlights();
        const cell_id = event.currentTarget.id;
        if (active_mapping && active_mapping.cell_to_code && active_mapping.cell_to_code[cell_id]) {
            const mapping = active_mapping.cell_to_code[cell_id];
            $(event.currentTarget).addClass("cell-highlight");
            last_cell_highlights.push("#" + cell_id);
            highlight_code(mapping.script_num, mapping.code_line);
        }
    });

    $(document).on("click", ".reg-cmd", function(event) {
        clear_all_highlights();
        const code_el = $(event.currentTarget);
        const code_id_parts = code_el.attr("id").split("___");
        const line = code_id_parts[0].substring(1);
        const script_num = code_id_parts[1];
        const lookup_key = "s" + script_num + "_l" + line;
        
        if (active_mapping && active_mapping.code_to_cells && active_mapping.code_to_cells[lookup_key]) {
            const mapping = active_mapping.code_to_cells[lookup_key];
            code_el.addClass("code-highlight");
            last_code_highlight = "#" + code_el.attr("id");
            highlight_cells(mapping.tabid, mapping.cell_ids);
        }
    });
});
  '
  writeLines(js_code, file.path(output_dir, "shared", "report_mapping.js"))
}

#' Write the custom CSS file for the report
write_report_css <- function(output_dir) {
  css_code <- '
body { font-family: sans-serif; }
.code-tab, .art-tab-div table { width: 100%; border-collapse: collapse; }
.code-line-td { vertical-align: top; padding-right: 10px; text-align: right; color: #888; font-family: monospace; }
.do-pre { margin: 0; padding: 2px; border: none; background-color: transparent; white-space: pre-wrap; word-wrap: break-word; }
.logtxt-pre { white-space: pre-wrap; word-wrap: break-word; font-size: 0.9em; }
.code-highlight { background-color: #ffd700 !important; }
.cell-highlight { background-color: #ffd700 !important; }
.small-tab-ul { font-size: 0.8em; }

/* Line Styles */
.noerr-line { color: #006400; }
.err-line { color: #aa0000; }
.mida-line { color: #ff8c00; }
.norun-line { color: #555; }
.reg-cmd { font-weight: bold; }
  '
  writeLines(css_code, file.path(output_dir, "shared", "repbox.css"))
}

#' Copy Bootstrap and jQuery assets to the report directory
copy_bs_assets <- function(assets_dir) {
    # In a real package scenario, these would be included in inst/
    # For this script, we assume they are available locally or we create them.
    bs_js <- 'https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js'
    bs_css <- 'https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css'
    jquery_js <- 'https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js'
    
    tryCatch({
        if(!file.exists(file.path(assets_dir, "bootstrap.min.js"))) download.file(bs_js, destfile = file.path(assets_dir, "bootstrap.min.js"))
        if(!file.exists(file.path(assets_dir, "bootstrap.min.css"))) download.file(bs_css, destfile = file.path(assets_dir, "bootstrap.min.css"))
        if(!file.exists(file.path(assets_dir, "jquery.min.js"))) download.file(jquery_js, destfile = file.path(assets_dir, "jquery.min.js"))
    }, error = function(e) {
        warning("Could not download Bootstrap/jQuery assets. The report may not render correctly. Please ensure an internet connection or place the files manually in the 'shared' directory.")
    })
}
