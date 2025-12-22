Here are the updated files implementing the improvements:

### 1. `rr_classify_map.R`

**Changes:**
- Added CSS for `.interactive-row` (pointer cursor, hover effect).
- Added CSS for `.active-regression-group` (background tint and side borders for the regression column).
- Enhanced `.cell-highlight` to ensure selected cells stay visible above the regression group highlighting.
- Removed padding from `do-col-div` and code lines to save space.

```r
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

        /* Tags */
        .tag-container { margin-bottom: 8px; line-height: 1.8; }
        .label-tag { font-size: 85%; margin-right: 4px; display: inline-block; padding: .2em .6em .3em; }

        /* Key-Value pairs */
        .kv-row { font-size: 0.9em; margin-bottom: 4px; }
        .kv-label { font-weight: bold; color: #666; width: 120px; display: inline-block; vertical-align: top;}
        .kv-val { color: #222; display: inline-block; width: calc(100% - 125px); }

        /* Tables */
        .class-table { width: 100%; font-size: 0.85em; margin-top: 5px; background: #fff; border: 1px solid #ddd; }
        .class-table th { background: #f0f0f0; padding: 4px; border-bottom: 2px solid #ddd; font-weight: 600; color: #444; }
        .class-table td { padding: 4px; border-bottom: 1px solid #eee; vertical-align: top; }
        .class-table tr:last-child td { border-bottom: none; }
        
        .cell-link { display: none; } /* Hidden, interaction is via row click */
        .var-type-badge { font-size: 0.8em; padding: 1px 4px; border-radius: 3px; background: #eee; color: #555; border: 1px solid #ccc; white-space: nowrap; }
        
        /* Summary Grouping Styles */
        .summary-group { background: #fff; border: 1px solid #e5e5e5; border-radius: 4px; padding: 8px; margin-bottom: 10px; border-left: 3px solid #337ab7; }
        .summary-reg-list { font-family: monospace; color: #333; word-break: break-all; }
        .summary-dim-line { font-size: 0.9em; color: #666; margin-top: 4px; }
        
        /* Interactive Rows */
        .interactive-row { cursor: pointer; transition: background-color 0.1s; }
        .interactive-row:hover { background-color: #e6f2ff; }

        /* Active Regression Highlight in Table */
        .active-regression-group {
            background-color: rgba(51, 122, 183, 0.1) !important; /* Light tint */
            border-left: 2px solid #337ab7 !important;
            border-right: 2px solid #337ab7 !important;
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
```

### 2. `rr_classify_map.js`

**Changes:**
- Implemented `highlight_regression_group` to visually highlight all cells of a regression using the new `.active-regression-group` class.
- Updated `update_classification_panel`:
    - Stores the active `regid` in the container for context.
    - Adds `.interactive-row` and `data-cellid` to variable/stat rows.
    - Removes explicit link icons.
- Updated event handlers:
    - Clicking a table cell now triggers `highlight_regression_group` (Action 3).
    - Clicking a row in the right panel now triggers:
        - `highlight_regression_group` (Action 3).
        - `highlight_cells` for the specific variable (Action 2).
        - `highlight_code` for the specific variable (Action 1).

```javascript
// FILE: rr_classify_map.js

/**
 * Helper: Resolve dimension inheritance and clean vars for grouping comparison.
 */
function get_resolved_structure(reg, all_regs_in_table) {
    let dims = reg.dimensions;
    if (reg.dimensions_same_as_regid) {
        const parent = all_regs_in_table.find(r => r.regid === reg.dimensions_same_as_regid);
        if (parent && parent.dimensions) {
            dims = parent.dimensions;
        }
    }
    
    let dims_sig = dims ? JSON.parse(JSON.stringify(dims)) : [];
    let vars_sig = reg.vars ? JSON.parse(JSON.stringify(reg.vars)) : [];
    vars_sig.forEach(v => { delete v.cell_id_estimate; });

    return {
        tags: reg.regression_tags || "",
        vars: vars_sig,
        dims: dims_sig
    };
}

/**
 * Renders a compact summary of all regressions in a table.
 */
function update_table_summary_panel(tabid) {
    // Clear regression highlight when going back to summary
    $(".active-regression-group").removeClass("active-regression-group");
    if(window.clear_all_highlights) window.clear_all_highlights();

    const $detail = $("#classify-details");
    $detail.empty();

    if (!window.all_classifications || !Array.isArray(window.all_classifications)) {
        if (typeof window.classification_file !== 'undefined' && window.classification_file) {
             $detail.append('<div class="alert alert-info">Loading classification data...</div>');
             return; 
        }
        $detail.html('<p class="text-muted">No classification data available.</p>');
        return;
    }

    const table_regs = window.all_classifications.filter(r => String(r.tabid) === String(tabid));
    
    if (table_regs.length === 0) {
        $detail.html('<p class="text-muted">No classification data found for Table ' + tabid + '.</p>');
        return;
    }

    $detail.append('<h4>Table ' + tabid + ' Overview</h4>');
    $detail.append('<p class="text-muted small">Regressions grouped by identical structure.</p>');

    const groups = [];
    table_regs.forEach(reg => {
        const struc = get_resolved_structure(reg, table_regs);
        const sig = JSON.stringify(struc);
        let group = groups.find(g => g.sig === sig);
        if (!group) {
            group = { sig: sig, regids: [], data: struc };
            groups.push(group);
        }
        group.regids.push(reg.regid);
    });

    groups.forEach(g => {
        const regList = g.regids.join(", ");
        let html = `<div class="summary-group">`;
        html += `<div><strong>Regressions:</strong> <span class="summary-reg-list">${regList}</span></div>`;

        if (g.data.tags) {
             const tags = g.data.tags.split(',').map(t => t.trim()).filter(t => t);
             if (tags.length > 0) {
                html += '<div class="tag-container" style="margin-top:5px;">';
                tags.forEach(tag => {
                    let cls = 'label-info';
                    if (tag === 'main_result') cls = 'label-primary';
                    else if (tag === 'robustness') cls = 'label-default';
                    else if (tag.indexOf('experiment') > -1) cls = 'label-success';
                    html += `<span class="label label-tag ${cls}">${tag}</span>`;
                });
                html += '</div>';
             }
        }

        if (g.data.vars && g.data.vars.length > 0) {
             html += '<table class="class-table table-condensed" style="margin-top:5px; margin-bottom:5px;">';
             html += '<thead><tr><th style="width:10%">Role</th><th>Label</th><th>Code</th></tr></thead><tbody>';
             g.data.vars.forEach(v => {
                 let typeBadge = v.var_type;
                 if (v.var_type === 'd') typeBadge = '<span class="var-type-badge" style="background:#dff0d8;border-color:#d6e9c6;color:#3c763d;">Dep</span>';
                 else if (v.var_type === 'x_eff') typeBadge = '<span class="var-type-badge" style="background:#d9edf7;border-color:#bce8f1;color:#31708f;">Eff</span>';
                 else if (v.var_type === 'fe') typeBadge = '<span class="var-type-badge">FE</span>';
                 else if (v.var_type === 'x_co') typeBadge = '<span class="var-type-badge">Ctrl</span>';
                 
                 html += `<tr>
                    <td>${typeBadge}</td>
                    <td>${v.label_in_article || '-'} ${v.unit ? '<small class="text-muted">('+v.unit+')</small>' : ''}</td>
                    <td><code>${v.var_in_code || '-'}</code></td>
                 </tr>`;
             });
             html += '</tbody></table>';
        }

        if (g.data.dims && g.data.dims.length > 0) {
            html += '<div class="summary-dim-line"><strong>Dims:</strong> ';
            const dimStrs = g.data.dims.map(d => {
                let txt = `${d.dim_class}:${d.dim_type}`;
                if(d.var_in_code) txt += ` (<code>${d.var_in_code}</code>)`;
                else if(d.dummy_set) txt += ` (<code>${d.dummy_set}</code>)`;
                return txt;
            });
            html += dimStrs.join(', ');
            html += '</div>';
        }
        html += `</div>`;
        $detail.append(html);
    });
}

/**
 * Highlights the regression group (column) in the table.
 */
function highlight_regression_group(regid) {
    // Clear existing group highlights
    $(".active-regression-group").removeClass("active-regression-group");
    
    if (!regid || !window.active_mapping || !window.active_mapping.reg_info) return;
    
    const regInfo = window.active_mapping.reg_info[regid];
    if (regInfo && regInfo.cell_ids) {
        const ids = regInfo.cell_ids.split(',');
        ids.forEach(id => {
            $("#" + id.trim()).addClass("active-regression-group");
        });
    }
}

/**
 * Updates the right-hand panel with DETAILED classification info.
 */
function update_classification_panel(tabid, regid) {
    const $detail = $("#classify-details");
    $detail.empty();
    
    // Store current active regression ID for context
    $detail.data("regid", regid);

    if (!window.all_classifications || !Array.isArray(window.all_classifications)) return;

    const record = window.all_classifications.find(r => String(r.tabid) === String(tabid) && String(r.regid) === String(regid));

    if (!record) {
        $detail.append('<div class="alert alert-warning">No classification info found for <strong>Tab ' + tabid + ', Reg ' + regid + '</strong></div>');
        return;
    }
    
    // Action 3: Highlight the regression in the table
    highlight_regression_group(regid);
    
    $detail.append(`<button class="btn btn-xs btn-default pull-right" onclick="update_table_summary_panel('${tabid}')"><span class="glyphicon glyphicon-arrow-left"></span> Summary</button>`);

    function buildSection(title, contentHtml) {
        if (!contentHtml) return;
        $detail.append(`
            <div class="class-section">
                <div class="class-header">${title}</div>
                ${contentHtml}
            </div>
        `);
    }

    let headerHtml = `<div class="class-desc">${record.short_descr || "No description provided."}</div>`;

    if (record.regression_tags) {
        const tags = record.regression_tags.split(',').map(t => t.trim()).filter(t => t);
        if (tags.length > 0) {
            headerHtml += '<div class="tag-container">';
            tags.forEach(tag => {
                let cls = 'label-info';
                if (tag === 'main_result') cls = 'label-primary';
                if (tag === 'robustness') cls = 'label-default';
                if (tag.indexOf('experiment') > -1) cls = 'label-success';
                headerHtml += `<span class="label label-tag ${cls}">${tag}</span>`;
            });
            headerHtml += '</div>';
        }
    }

    if (record.standard_error_type) {
        headerHtml += `<div class="kv-row"><span class="kv-label">SE Type:</span><span class="kv-val">${record.standard_error_type}</span></div>`;
    }
    if (record.error_in_prompt_or_media) {
         headerHtml += `<div class="alert alert-danger" style="margin-top:5px; padding:5px;"><strong>Warning:</strong> ${record.error_in_prompt_or_media}</div>`;
    }

    buildSection(`Regression: ${record.regid}`, headerHtml);

    if (record.vars && Array.isArray(record.vars) && record.vars.length > 0) {
        let varsHtml = '<table class="class-table"><thead><tr><th>Role</th><th>Article Label</th><th>Code Var</th></tr></thead><tbody>';

        record.vars.forEach(v => {
            const label = v.label_in_article || '<span class="text-muted">-</span>';
            const codeVar = v.var_in_code ? `<code>${v.var_in_code}</code>` : '<span class="text-muted">-</span>';
            let typeBadge = v.var_type;
            if (v.var_type === 'd') typeBadge = '<span class="var-type-badge" style="background:#dff0d8;border-color:#d6e9c6;color:#3c763d;">Dep</span>';
            else if (v.var_type === 'x_eff') typeBadge = '<span class="var-type-badge" style="background:#d9edf7;border-color:#bce8f1;color:#31708f;">Eff</span>';
            else if (v.var_type === 'fe') typeBadge = '<span class="var-type-badge">FE</span>';
            else if (v.var_type === 'x_co') typeBadge = '<span class="var-type-badge">Ctrl</span>';

            let unitStr = v.unit ? `<br><small class="text-muted">Unit: ${v.unit}</small>` : '';
            
            const cellAttr = v.cell_id_estimate ? ` data-cellid="${v.cell_id_estimate}"` : '';
            const rowClass = 'interactive-row';

            varsHtml += `<tr class="${rowClass}"${cellAttr}>
                <td>${typeBadge}</td>
                <td>${label}${unitStr}</td>
                <td>${codeVar}</td>
            </tr>`;
        });
        varsHtml += '</tbody></table>';
        buildSection("Variables", varsHtml);
    }

    let dimsToRender = record.dimensions;
    let dimNote = "";
    if (record.dimensions_same_as_regid) {
        const otherRegId = record.dimensions_same_as_regid;
        const otherRecord = window.all_classifications.find(r => String(r.tabid) === String(tabid) && String(r.regid) === String(otherRegId));
        if (otherRecord && otherRecord.dimensions) {
            dimsToRender = otherRecord.dimensions;
            dimNote = `<div class="small text-muted" style="margin-bottom:4px;">(Same as <strong>${otherRegId}</strong>)</div>`;
        } else {
             dimNote = `<div class="small text-danger">Inherits from ${otherRegId} (not found)</div>`;
        }
    }

    if (dimsToRender && Array.isArray(dimsToRender) && dimsToRender.length > 0) {
        let dimHtml = dimNote;
        dimHtml += '<table class="class-table"><thead><tr><th>Class</th><th>Type</th><th>Code Var</th></tr></thead><tbody>';
        dimsToRender.forEach(d => {
            const cls = d.dim_class || '';
            let type = d.dim_type || '';
            if (type === 'other' && d.other_dim_type) type = d.other_dim_type;
            let codeRep = d.var_in_code ? `<code>${d.var_in_code}</code>` : (d.dummy_set ? `<code>${d.dummy_set}</code>` : '<span class="text-muted">-</span>');
            
            dimHtml += `<tr>
                <td>${cls}</td>
                <td>${type}</td>
                <td>${codeRep}</td>
            </tr>`;
        });
        dimHtml += '</tbody></table>';
        buildSection("Dimensions", dimHtml);
    } else if (dimNote) {
        buildSection("Dimensions", dimNote);
    }

    if (record.reported_stats && Array.isArray(record.reported_stats) && record.reported_stats.length > 0) {
        let statHtml = '<table class="class-table"><thead><tr><th>Statistic</th><th>Table Val</th><th>Code Val</th></tr></thead><tbody>';
        record.reported_stats.forEach(s => {
            const label = s.stat_label || '?';
            const valTab = (s.value_table !== null) ? s.value_table : '<span class="text-muted">-</span>';
            const valCode = (s.value_code !== null) ? s.value_code : '<span class="text-muted">-</span>';
            const cellAttr = s.cell_id ? ` data-cellid="${s.cell_id}"` : '';
            const rowClass = 'interactive-row';

            statHtml += `<tr class="${rowClass}"${cellAttr}>
                <td>${label}</td>
                <td>${valTab}</td>
                <td>${valCode}</td>
            </tr>`;
        });
        statHtml += '</tbody></table>';
        buildSection("Reported Statistics", statHtml);
    }
}

// Events
$(document).ready(function() {
    
    // Data Loading
    if ((typeof window.all_classifications === 'undefined' || window.all_classifications === null) && 
         typeof window.classification_file !== 'undefined' && window.classification_file) {
        
        fetch(window.classification_file)
            .then(response => {
                if (!response.ok) throw new Error("HTTP error " + response.status);
                return response.json();
            })
            .then(data => {
                window.all_classifications = data;
                const activeTab = $("#tabtabs li.active > a").attr("href");
                if(activeTab) {
                    const tabid = activeTab.replace("#tabtab", "");
                    update_table_summary_panel(tabid);
                }
            })
            .catch(error => {
                $("#classify-details").html('<div class="alert alert-danger">Failed to load data.</div>');
            });
    } else {
        const activeTab = $("#tabtabs li.active > a").attr("href");
        if(activeTab) {
            const tabid = activeTab.replace("#tabtab", "");
            update_table_summary_panel(tabid);
        }
    }

    // Tab Change
    $(document).on('shown.bs.tab', '#tabtabs a[data-toggle="tab"]', function (e) {
        const target = $(e.target).attr("href");
        const tabid = target.replace("#tabtab", "");
        if(window.clear_all_highlights) window.clear_all_highlights();
        update_table_summary_panel(tabid);
    });

    // Tab Click
    $("#tabtabs").on('click', 'a[data-toggle="tab"]', function (e) {
        const target = $(this).attr("href");
        const parentLi = $(this).parent();
        if(parentLi.hasClass('active')) {
             const tabid = target.replace("#tabtab", "");
             if(window.clear_all_highlights) window.clear_all_highlights();
             update_table_summary_panel(tabid);
        }
    });

    // Right Panel Row Click
    $(document).on("click", "#classify-col-div .interactive-row", function(e) {
        const cellId = $(this).data("cellid");
        const currentRegId = $("#classify-details").data("regid");
        
        // 1. Clear previous but re-apply regression highlight
        if(window.clear_all_highlights) window.clear_all_highlights();
        if(currentRegId) highlight_regression_group(currentRegId);

        if (cellId) {
            // 2. Highlight cell (Action 2)
            const match = cellId.match(/^c(\d+)_/);
            const tabid = match ? match[1] : null;
            if (tabid && window.highlight_cells) {
                window.highlight_cells(tabid, cellId);
            }

            // 3. Highlight code (Action 1)
            if (window.active_mapping && window.active_mapping.cell_map) {
                const info = window.active_mapping.cell_map[cellId];
                if (info && info.script_num && info.code_line && window.highlight_code) {
                     window.highlight_code(info.script_num, info.code_line, info.runid, null);
                }
            }
        }
    });

    // Table Cell Click
    $(document).on("click", ".tabnum, [id^=c][id*=_]", function(event) {
        const cell_id = event.currentTarget.id;
        if (window.active_mapping && window.active_mapping.cell_map && window.active_mapping.cell_map[cell_id]) {
            const info = window.active_mapping.cell_map[cell_id];
            
            const $tabPane = $(event.currentTarget).closest('.tab-pane[id^="tabtab"]');
            const tabid = $tabPane.length ? $tabPane.attr('id').replace('tabtab', '') : null;

            if (info.regid && tabid) {
                // update_classification_panel will call highlight_regression_group
                update_classification_panel(tabid, info.regid);
            }
        }
    });
});
```
