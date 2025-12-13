// FILE: report_map.js

// Global variables set by the R backend
// var data_is_embedded = true;
// var show_wrong_number_report_opt = true;
// var all_maps = { ... }; // Populated if data_is_embedded is true
// var report_manifest = { ... }; // Populated if data_is_embedded is false
// var cell_conflict_data = { ... }; // Populated with pre-computed conflict info
// var all_evals = {}; // Populated with evaluation data if embedded
// var eval_manifest = {}; // Populated with paths to evaluation data if external

var active_map_type = "";
var active_version = "";
var active_mapping = {};
var active_eval_data = null;
var cell_issue_lookup = {}; // New: Lookup for cell issues from rme.Rds

var last_code_highlight = "";
var last_cell_highlights = [];

// Globals for robust and performant number highlighting
var original_log_htmls = {}; // Cache for pristine log HTML, keyed by the log <pre> element's ID.
var last_highlighted_log_id = null; // ID of the last log <pre> element that was modified.

function clear_all_highlights() {
    if (last_code_highlight) {
        $(last_code_highlight).removeClass("code-highlight");
        last_code_highlight = "";
    }

    last_cell_highlights.forEach(function(id) {
        $(id).removeClass("cell-highlight");
    });
    last_cell_highlights = [];

    $(".wrong-number-report-highlight").removeClass("wrong-number-report-highlight");
    // Clear highlights from eval items
    $(".eval-issue-item.cell-highlight").removeClass("cell-highlight");

    // Only restore the last modified log. The new highlight function will handle its own state.
    if (last_highlighted_log_id && original_log_htmls[last_highlighted_log_id]) {
        const log_code_element = $("#" + last_highlighted_log_id).find('.logtxt-code');
        if (log_code_element.length > 0) {
            log_code_element.html(original_log_htmls[last_highlighted_log_id]);
        }
    }
    last_highlighted_log_id = null;
}

// Find and highlight a number in a log output
function highlight_number_in_log(log_element, raw_number_str) {
    const log_id = log_element.attr('id');
    const log_code_element = log_element.find('.logtxt-code');
    if (log_code_element.length === 0) return;

    // 1. Ensure original HTML is cached.
    if (!original_log_htmls[log_id]) {
        original_log_htmls[log_id] = log_code_element.html();
    }

    // 2. Always start the search from the pristine, original HTML.
    const log_html = original_log_htmls[log_id];

    // 3. Find the best match.
    let number_str = String(raw_number_str).trim();
    let cleaned_str = number_str.replace(/[(),*]/g, '');
    let target_num = parseFloat(cleaned_str);
    if (isNaN(target_num)) return;

    const for_decimal_places = number_str.replace(/[^\d.]/g, '');
    const decimal_places = (for_decimal_places.split('.')[1] || '').length;

    const number_regex = /-?\d*\.?\d+/g;
    let best_match = null;
    let min_diff = Infinity;
    let match;

    while ((match = number_regex.exec(log_html)) !== null) {
        const num_in_log_str = match[0];
        if (num_in_log_str === '.' || num_in_log_str === '-') continue;
        const num_in_log = parseFloat(num_in_log_str);
        if (isNaN(num_in_log)) continue;

        const scale = Math.pow(10, decimal_places);
        const rounded_log_num = Math.round(num_in_log * scale) / scale;
        const rounded_target_num = Math.round(target_num * scale) / scale;

        if (rounded_log_num === rounded_target_num) {
            const diff = Math.abs(num_in_log - target_num);
            if (diff < min_diff) {
                min_diff = diff;
                best_match = match;
            }
        }
    }

    // 4. Apply the new highlight.
    if (best_match) {
        const original_text = best_match[0];
        const new_html = log_html.substring(0, best_match.index) +
                         '<span class="number-highlight">' + original_text + '</span>' +
                         log_html.substring(best_match.index + original_text.length);
        log_code_element.html(new_html);
        last_highlighted_log_id = log_id;
    } else {
        log_code_element.html(log_html); // Restore if no match
    }
}

function clear_static_coloring() {
    $(".statically-colored").each(function() {
        this.style.removeProperty('background-color');
        $(this).removeClass("statically-colored");
    });
}

function is_in_viewport(element) {
    if (!element) return false;
    const container = element.closest('.tab-content');
    if (!container) return false;
    const containerRect = container.getBoundingClientRect();
    const elementRect = element.getBoundingClientRect();
    return (
        elementRect.top >= containerRect.top &&
        elementRect.bottom <= containerRect.bottom
    );
}

function apply_static_coloring(mapping) {
    clear_static_coloring();
    if (!mapping || !mapping.reg_info) return;
    const reg_info = mapping.reg_info;
    for (const regid in reg_info) {
        if (reg_info.hasOwnProperty(regid)) {
            const info = reg_info[regid];
            if (info.color && info.cell_ids) {
                const cell_ids = info.cell_ids.split(',');
                cell_ids.forEach(id => {
                    const cell_selector = "#" + id.trim();
                    const element = $(cell_selector);
                    if (element.length > 0) {
                        element[0].style.setProperty('background-color', info.color, 'important');
                        element.addClass('statically-colored');
                    }
                });
            }
        }
    }
}

function apply_wrong_number_info(mapping) {
    $(".wrong-number-report").remove();
    $(".wrong-number-cell").each(function() {
        this.style.removeProperty('background-image');
        $(this).removeClass("wrong-number-cell");
    });

    if (!show_wrong_number_report_opt) {
        return;
    }

    if (!mapping || !mapping.wrong_number_info || !Array.isArray(mapping.wrong_number_info) || mapping.wrong_number_info.length === 0) {
        return;
    }

    const cell_to_color = {};
    if (mapping.reg_info) {
        for (const regid in mapping.reg_info) {
            const info = mapping.reg_info[regid];
            if (info.color && info.cell_ids) {
                info.cell_ids.split(',').forEach(id => {
                    cell_to_color[id.trim()] = info.color;
                });
            }
        }
    }

    const wrong_cases_by_tab = {};
    mapping.wrong_number_info.forEach(case_item => {
        const tabid = String(case_item.tabid);
        if (!wrong_cases_by_tab[tabid]) wrong_cases_by_tab[tabid] = [];
        wrong_cases_by_tab[tabid].push(case_item);
        const cell_element = $("#" + case_item.cell_id);
        if (cell_element.length > 0) {
            cell_element.addClass("wrong-number-cell");
            const reg_color = cell_to_color[case_item.cell_id] || '#f0f0f0';
            const gradient = `linear-gradient(45deg, #cccccc, ${reg_color})`;
            cell_element[0].style.setProperty('background-image', gradient, 'important');
        }
    });

    for (const tabid in wrong_cases_by_tab) {
        if (wrong_cases_by_tab.hasOwnProperty(tabid)) {
            const cases_for_tab = wrong_cases_by_tab[tabid];
            let report_html = '<div class="wrong-number-report">';
            report_html += '<h6>Discrepancies Found (click to locate):</h6><ul>';
            cases_for_tab.forEach(case_item => {
                report_html += `<li class="wrong-number-report-item" data-cell-id="${case_item.cell_id}" data-runid="${case_item.runid}" data-script-num="${case_item.script_num}" data-code-line="${case_item.code_line}" data-stata-number="${case_item.number_in_stata_output}">Cell <code>${case_item.cell_id}</code>: Table shows ${case_item.wrong_number_in_cell}, but script output is ${case_item.number_in_stata_output}.</li>`;
            });
            report_html += '</ul></div>';
            const table_container = $("#tabtab" + tabid + " .art-tab-div");
            if (table_container.length > 0) {
                table_container.append(report_html);
            }
        }
    }
}

function highlight_code(script_num, line_num, runid_to_show, number_to_find) {
    $("#dotabs a[href='#dotab_" + script_num + "']").tab("show");

    setTimeout(function() {
        const code_id = "#L" + line_num + "___" + script_num;
        $(code_id).addClass("code-highlight");
        last_code_highlight = code_id;

        const targetElement = document.querySelector(code_id);
        if (targetElement && !is_in_viewport(targetElement)) {
            targetElement.scrollIntoView({ behavior: "smooth", block: "start" });
        }

        if (runid_to_show && runid_to_show !== 'null') {
            const log_container = $('#loginfo-' + line_num + '-' + script_num);

            const do_log_show_and_scroll = function() {
                const run_pre = $('#runid-' + runid_to_show);
                if (run_pre.length === 0) return;

                const scroll_and_highlight = function(pre_element) {
                    if (!is_in_viewport(pre_element[0])) {
                        pre_element[0].scrollIntoView({ behavior: 'smooth', block: 'start' });
                    }
                    if (typeof number_to_find !== 'undefined' && number_to_find !== null) {
                        highlight_number_in_log(pre_element, number_to_find);
                    }
                };

                const tab_pane = run_pre.closest('.tab-pane');

                // Case 1: Log is part of a multi-run tab set inside the log container
                if (tab_pane.length > 0) {
                    const is_active = tab_pane.hasClass('active');

                    if (is_active) {
                        // If tab is already visible, highlight after a zero-delay timeout
                        // to ensure the browser has processed the 'clear' from the click event.
                        setTimeout(() => scroll_and_highlight(run_pre), 0);
                    } else {
                        // If tab is not visible, use the Bootstrap event to highlight after it's shown.
                        const pane_id = tab_pane.attr('id');
                        $('a[href="#' + pane_id + '"]').one('shown.bs.tab', function() {
                             scroll_and_highlight(run_pre);
                        }).tab('show');
                    }
                }
                // Case 2: Log is a single run (no inner tabs)
                else {
                    // Highlight after a zero-delay timeout.
                    setTimeout(() => scroll_and_highlight(run_pre), 0);
                }
            };

            // Main entry point for showing/scrolling logs.
            // Check if the whole log container needs to be expanded first.
            if (log_container.hasClass('in')) {
                do_log_show_and_scroll();
            } else {
                log_container.one('shown.bs.collapse', do_log_show_and_scroll).collapse('show');
            }
        }
    }, 150);
}

function highlight_cells(tabid, cell_ids_string, use_red_outline = false) {
    if (!tabid || !cell_ids_string) return;
    $("#tabtabs a[href='#tabtab" + tabid + "']").tab("show");

    setTimeout(function() {
        const cell_ids = cell_ids_string.split(",");
        cell_ids.forEach(id => {
            const cell_selector = "#" + id.trim();
            $(cell_selector).addClass("cell-highlight");
            if (use_red_outline) {
                $(cell_selector).addClass("wrong-number-report-highlight");
            }
            last_cell_highlights.push(cell_selector);
        });
        if (cell_ids.length > 0) {
            const targetElement = document.querySelector("#" + cell_ids[0].trim());
            if(targetElement) {
               targetElement.scrollIntoView({ behavior: "smooth", block: "center" });
            }
        }
    }, 150);
}

function update_version_selector() {
    const source = data_is_embedded ? all_maps : report_manifest;
    const versions = Object.keys(source[active_map_type] || {});
    const version_selector = $("#version_selector");
    version_selector.empty();
    versions.forEach(function(v) {
        version_selector.append($("<option></option>").attr("value", v).text(v));
    });
}

function update_all_cell_titles() {
    const cell_map = active_mapping ? active_mapping.cell_map : null;
    $("[id^=c][id*=_]").each(function() {
        const cell_id = this.id;
        let title_parts = [`cell_id: ${cell_id}`];
        let has_issue_or_conflict = false;
        $(this).removeClass("conflict-indicator");

        // 1. Info from current map
        if (cell_map && cell_map[cell_id]) {
            const info = cell_map[cell_id];
            if (info.regid != null) title_parts.push(`regid: ${info.regid}`);
            if (info.runid != null) title_parts.push(`runid: ${info.runid}`);
            if (info.script_file != null && info.code_line != null) {
                title_parts.push(`script: ${info.script_file}, line: ${info.code_line}`);
            }
        }

        // 2. Cross-version mapping conflicts
        if (typeof cell_conflict_data !== 'undefined' && cell_conflict_data[cell_id]) {
            title_parts.push(cell_conflict_data[cell_id]);
            has_issue_or_conflict = true;
        }

        // 3. Wrong number cases from map data (if option is on)
        if (show_wrong_number_report_opt && active_mapping && active_mapping.wrong_number_info) {
             const wrong_num_case = active_mapping.wrong_number_info.find(c => c.cell_id === cell_id);
             if (wrong_num_case) {
                  title_parts.push(`Discrepancy: Table(${wrong_num_case.wrong_number_in_cell}) vs Code(${wrong_num_case.number_in_stata_output})`);
                  has_issue_or_conflict = true;
             }
        }

        // 4. Issues from rme.Rds evaluation data
        if (cell_issue_lookup && cell_issue_lookup[cell_id]) {
            title_parts = title_parts.concat(cell_issue_lookup[cell_id]);
            has_issue_or_conflict = true;
        }

        $(this).attr('title', title_parts.join('\n'));
        if (has_issue_or_conflict) {
            $(this).addClass("conflict-indicator");
        }
    });
}

// --- Evaluation Report Functions ---

function clear_eval_reports() {
    $(".eval-report-container").remove();
    active_eval_data = null;
}

function load_and_display_eval_data(version_id) {
    clear_eval_reports();
    cell_issue_lookup = {};

    const on_eval_data_loaded = (data) => {
        active_eval_data = data;
        if (active_eval_data) {
            // Build lookup for tooltips
            for (const tabid in active_eval_data) {
                const tests = active_eval_data[tabid];
                for (const test_name in tests) {
                    const test_data = tests[test_name];
                    if (test_data.issues) {
                        test_data.issues.forEach(issue => {
                            let issue_summary = `[${test_name}]`;
                            if (issue.issue) issue_summary += `: ${issue.issue}`;
                            else if (issue.runids) issue_summary += `: runids ${issue.runids}`;

                            const cellids = (issue.cellids || issue.cellid || '').split(',');
                            cellids.forEach(cid_raw => {
                                const cid = cid_raw.trim();
                                if (cid) {
                                    if (!cell_issue_lookup[cid]) cell_issue_lookup[cid] = [];
                                    cell_issue_lookup[cid].push(issue_summary);
                                }
                            });
                        });
                    }
                }
            }
            render_eval_reports(active_eval_data);
        }
        update_all_cell_titles();
    };

    if (!version_id) {
        on_eval_data_loaded(null);
        return;
    }

    if (data_is_embedded) {
        on_eval_data_loaded(typeof all_evals !== 'undefined' ? all_evals[version_id] : null);
    } else {
        const eval_file_path = typeof eval_manifest !== 'undefined' ? eval_manifest[version_id] : null;
        if (!eval_file_path) {
            on_eval_data_loaded(null);
            return;
        }
        fetch(eval_file_path)
            .then(response => {
                if (!response.ok) {
                    if (response.status === 404) return null; // Don't error on 404, just means no eval file
                    throw new Error(`HTTP error! status: ${response.status}`);
                }
                return response.json();
            })
            .then(data => on_eval_data_loaded(data))
            .catch(error => {
                console.warn(`Could not load evaluation data for version ${version_id} from ${eval_file_path}:`, error);
                on_eval_data_loaded(null);
            });
    }
}

function render_eval_reports(eval_data_for_version) {
    for (const tabid in eval_data_for_version) {
        const table_container = $("#tabtab" + tabid + " .art-tab-div");
        if (table_container.length === 0) continue;

        const tab_eval_data = eval_data_for_version[tabid];
        const accordion_id = `eval-accordion-${tabid}`;
        let accordion_html = `<div class="panel-group" id="${accordion_id}" role="tablist" aria-multiselectable="true">`;
        let test_counter = 0;

        for (const test_name in tab_eval_data) {
            const test_data = tab_eval_data[test_name];
            const issues = test_data.issues;
            if (!issues || issues.length === 0) continue;

            const panel_id = `eval-panel-${tabid}-${test_counter}`;
            const collapse_id = `eval-collapse-${tabid}-${test_counter}`;

            accordion_html += `
            <div class="panel panel-default">
                <div class="panel-heading" role="tab" id="${panel_id}-heading">
                    <h4 class="panel-title">
                        <a role="button" data-toggle="collapse" data-parent="#${accordion_id}" href="#${collapse_id}" aria-expanded="false" aria-controls="${collapse_id}">
                            Test: ${test_name} <span class="badge">${issues.length}</span>
                        </a>
                    </h4>
                </div>
                <div id="${collapse_id}" class="panel-collapse collapse" role="tabpanel" aria-labelledby="${panel_id}-heading">
                    <div class="panel-body">
                        <p class="eval-description">${test_data.description || ''}</p>
                        ${format_issues_html(test_name, issues)}
                    </div>
                </div>
            </div>`;
            test_counter++;
        }
        accordion_html += `</div>`;
        if (test_counter > 0) {
            // The original code appended the h5 and the accordion as siblings.
            // This change wraps them in a single div with the 'eval-report-container' class.
            // This ensures clear_eval_reports() removes the entire block, preventing duplicate headers.
            // It also makes the CSS for the h5 header apply correctly.
            const report_html = `
                <div class="eval-report-container">
                    <h5>Mapping Evaluation Results</h5>
                    ${accordion_html}
                </div>`;
            table_container.append(report_html);
        }
    }
}


function format_issues_html(test_name, issues) {
    const custom_formatters = {
        "invalid_cellids": (issue) => `<strong>Reg. ${issue.regid}</strong>: Affects cells <code>${issue.cellids}</code>`,
        "single_col_reg": (issue) => `<strong>Reg. ${issue.regid}</strong>: Affects cells <code>${issue.cellids}</code>`,
        "multicol_reg_plausibility": (issue) => `<strong>Reg. ${issue.regid}</strong>: Implausible structure for columns <code>${issue.cols}</code>. (Cells: <code>${issue.cellids}</code>)`
    };

    if (custom_formatters[test_name]) {
        let list_html = '<ul>';
        issues.forEach((issue, index) => {
            list_html += `<li class="eval-issue-item" data-test="${test_name}" data-index="${index}">${custom_formatters[test_name](issue)}</li>`;
        });
        return list_html + '</ul>';
    }

    // Default to a table
    const headers = Object.keys(issues[0] || {});
    const ignore_cols = ['script_num', 'code_line', 'wrong_number_in_cell', 'number_in_stata_output', 'details', 'partner_cellid'];
    const display_headers = headers.filter(h => !ignore_cols.includes(h));

    let table_html = '<div class="table-responsive"><table class="table table-condensed table-bordered eval-issue-table"><thead><tr>';
    display_headers.forEach(h => table_html += `<th>${h}</th>`);
    table_html += '</tr></thead><tbody>';

    issues.forEach((issue, index) => {
        table_html += `<tr class="eval-issue-item" data-test="${test_name}" data-index="${index}">`;
        display_headers.forEach(h => {
            let val = issue[h];
            if(val === null || typeof val === 'undefined') val = '';
            if (typeof val === 'number') val = Number(val.toPrecision(4));
            // Add title for long content like 'details'
            let title = (h === 'issue' && issue.details) ? `title="${issue.details}"` : '';
            table_html += `<td ${title}>${val}</td>`;
        });
        table_html += '</tr>';
    });

    return table_html + '</tbody></table></div>';
}

// --- Main Event Handling ---

function handle_map_change() {
    clear_all_highlights();

    const apply_updates = (map_data) => {
        active_mapping = map_data || {};
        apply_static_coloring(active_mapping);
        apply_wrong_number_info(active_mapping);
        load_and_display_eval_data(active_version);
    };

    if (data_is_embedded) {
        const map_data = all_maps[active_map_type] ? all_maps[active_map_type][active_version] : null;
        apply_updates(map_data);
    } else {
        const file_path = report_manifest[active_map_type]?.[active_version];
        if (!file_path) {
            apply_updates(null);
            return;
        }
        const selectors = $("#map_type_selector, #version_selector");
        selectors.prop("disabled", true);
        fetch(file_path)
            .then(response => {
                if (!response.ok) throw new Error(`HTTP error! status: ${response.status}`);
                return response.json();
            })
            .then(data => apply_updates(data))
            .catch(error => {
                console.error("Failed to fetch map data:", error);
                alert("Failed to load map data. Please ensure you are viewing this report via a web server and the file exists: " + file_path);
                apply_updates(null);
            })
            .finally(() => selectors.prop("disabled", false));
    }
}

$(document).ready(function() {
    const map_types = Object.keys(data_is_embedded ? all_maps : report_manifest);
    if (map_types.length === 0) {
        $(".controls-div").hide();
        return;
    }
    $("#map_type_selector").on("change", function() {
        active_map_type = $(this).val();
        update_version_selector();
        $("#version_selector").trigger("change");
    });
    $("#version_selector").on("change", function() {
        active_version = $(this).val();
        handle_map_change();
    });
    $(document).on("click", ".tabnum, [id^=c][id*=_]", function(event) {
        clear_all_highlights();
        const cell_id = event.currentTarget.id;
        const cell_content = $(event.currentTarget).text();
        if (active_mapping && active_mapping.cell_to_code_idx && active_mapping.code_locations) {
            const location_idx = active_mapping.cell_to_code_idx[cell_id];
            if (typeof location_idx !== 'undefined') {
                const location_data = active_mapping.code_locations[location_idx];
                highlight_cells(null, cell_id);
                const runid = location_data[0];
                const script_num = location_data[1];
                const code_line = location_data[2];
                highlight_code(script_num, code_line, runid, cell_content);
            }
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
    $(document).on("click", ".wrong-number-report-item", function() {
        clear_all_highlights();
        const el = $(this);
        const cell_id = el.data("cell-id");
        const runid = el.data("runid");
        const script_num = el.data("script-num");
        const code_line = el.data("code-line");
        const stata_number = el.data("stata-number");

        if (cell_id) {
            const tab_pane_id = $("#" + cell_id).closest('.tab-pane[id^=tabtab]').attr('id');
            highlight_cells(tab_pane_id.replace('tabtab',''), cell_id, true);
        }
        if (script_num && script_num !== 'null' && code_line && code_line !== 'null') {
            highlight_code(script_num, code_line, runid, stata_number);
        }
    });
    $(document).on("click", ".eval-issue-item", function() {
        clear_all_highlights();
        const el = $(this);
        el.addClass('cell-highlight');

        const test_name = el.data("test");
        const index = el.data("index");
        const tabid = el.closest(".art-tab-div").parent().attr("id").replace("tabtab", "");
        if (!active_eval_data || !active_eval_data[tabid] || !active_eval_data[tabid][test_name]) return;
        const issue = active_eval_data[tabid][test_name].issues[index];

        const cellids_str = issue.cellids || issue.cellid || '';
        const runid = issue.runid;
        const script_num = issue.script_num;
        const code_line = issue.code_line;
        const stata_number = issue.number_in_stata_output || issue.closest_reg_val;

        if (cellids_str) {
            highlight_cells(tabid, cellids_str, true);
        }

        if (script_num && code_line) {
             highlight_code(script_num, code_line, runid, stata_number);
        }
    });
    active_map_type = $("#map_type_selector").val();
    update_version_selector();
    $("#version_selector").trigger("change");
});
