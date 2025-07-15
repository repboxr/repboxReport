// FILE: report_map.js

// Global variables set by the R backend
// var data_is_embedded = true;
// var all_maps = { ... }; // Populated if data_is_embedded is true
// var report_manifest = { ... }; // Populated if data_is_embedded is false
// var cell_conflict_data = { ... }; // Populated with pre-computed conflict info

var active_map_type = "";
var active_version = "";
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
    // Also clear highlights from discrepancy report clicks
    $(".wrong-number-report-highlight").removeClass("wrong-number-report-highlight");
}

function clear_static_coloring() {
    // We must iterate over each element to remove the specific style
    // property that was set with '!important'. Standard .css() won't work.
    $(".statically-colored").each(function() {
        this.style.removeProperty('background-color');
        $(this).removeClass("statically-colored");
    });
}

function apply_static_coloring(mapping) {
    clear_static_coloring();
    if (!mapping || !mapping.reg_info) return;

    const reg_info = mapping.reg_info;
    for (const reg_ind in reg_info) {
        if (reg_info.hasOwnProperty(reg_ind)) {
            const info = reg_info[reg_ind];
            if (info.color && info.cell_ids) {
                const cell_ids = info.cell_ids.split(',');
                cell_ids.forEach(id => {
                    const cell_selector = "#" + id.trim();
                    const element = $(cell_selector);

                    if (element.length > 0) {
                        // Use the DOM element's setProperty method to add '!important'.
                        // This ensures our coloring rule has the highest priority and will
                        // override any conflicting styles (like from Bootstrap or other CSS).
                        element[0].style.setProperty('background-color', info.color, 'important');
                        element.addClass('statically-colored');
                    }
                });
            }
        }
    }
}

function apply_wrong_number_info(mapping) {
    // Clear previous reports and styling
    $(".wrong-number-report").remove();
    $(".wrong-number-cell").each(function() {
        this.style.removeProperty('background-image');
        $(this).removeClass("wrong-number-cell");
    });

    if (!mapping || !mapping.wrong_number_info || !Array.isArray(mapping.wrong_number_info) || mapping.wrong_number_info.length === 0) {
        return;
    }

    // Build a map from cell ID to its regression color for this specific map version
    const cell_to_color = {};
    if (mapping.reg_info) {
        for (const reg_ind in mapping.reg_info) {
            const info = mapping.reg_info[reg_ind];
            if (info.color && info.cell_ids) {
                info.cell_ids.split(',').forEach(id => {
                    cell_to_color[id.trim()] = info.color;
                });
            }
        }
    }

    const wrong_cases_by_tab = {};
    mapping.wrong_number_info.forEach(case_item => {
        const tabid = String(case_item.tabid); // Ensure tabid is a string
        if (!wrong_cases_by_tab[tabid]) {
            wrong_cases_by_tab[tabid] = [];
        }
        wrong_cases_by_tab[tabid].push(case_item);

        // Apply styling to the cell
        const cell_element = $("#" + case_item.cell_id);
        if (cell_element.length > 0) {
            cell_element.addClass("wrong-number-cell");
            const reg_color = cell_to_color[case_item.cell_id] || '#f0f0f0'; // Default color if not found
            const gradient = `linear-gradient(45deg, #cccccc, ${reg_color})`;
            // This will be rendered on top of the background-color set by static coloring
            cell_element[0].style.setProperty('background-image', gradient, 'important');
        }
    });

    // Generate and append reports for each table that has wrong numbers
    for (const tabid in wrong_cases_by_tab) {
        if (wrong_cases_by_tab.hasOwnProperty(tabid)) {
            const cases_for_tab = wrong_cases_by_tab[tabid];
            let report_html = '<div class="wrong-number-report">';
            report_html += '<h6>Discrepancies Found (click to locate):</h6><ul>';
            cases_for_tab.forEach(case_item => {
                report_html += `<li class="wrong-number-report-item" data-cell-id="${case_item.cell_id}">Cell <code>${case_item.cell_id}</code>: Table shows ${case_item.wrong_number_in_cell}, but script output is ${case_item.number_in_stata_output}.</li>`;
            });
            report_html += '</ul></div>';

            // Find the correct table div to append to
            const table_container = $("#tabtab" + tabid + " .art-tab-div");
            if (table_container.length > 0) {
                table_container.append(report_html);
            }
        }
    }
}


function highlight_code(script_num, line_num) {
    $("#dotabs a[href='#dotab_" + script_num + "']").tab("show");

    // Defer actions to allow tab to show first
    setTimeout(function() {
        const code_id = "#L" + line_num + "___" + script_num;
        $(code_id).addClass("code-highlight");
        last_code_highlight = code_id;

        const targetElement = document.querySelector(code_id);
        if(targetElement) {
            targetElement.scrollIntoView({ behavior: "smooth", block: "center" });
        }
    }, 150);
}

function highlight_cells(tabid, cell_ids_string) {
    if (!tabid || !cell_ids_string) return;
    $("#tabtabs a[href='#tabtab" + tabid + "']").tab("show");

    setTimeout(function() {
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
    }, 150);
}

// Function to update the version selector based on the active map type
function update_version_selector() {
    // The source of map keys depends on the data loading mode.
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

        // 1. Add info from the currently active map version
        if (cell_map && cell_map[cell_id]) {
            const info = cell_map[cell_id];
            if (info.reg_ind !== null) title_parts.push(`reg_ind: ${info.reg_ind}`);
            if (info.runid !== null) title_parts.push(`runid: ${info.runid}`);
            if (info.script_num !== null && info.code_line !== null) {
                title_parts.push(`script: ${info.script_num}, line: ${info.code_line}`);
            }
        }

        // 2. Add static conflict info if it exists
        if (typeof cell_conflict_data !== 'undefined' && cell_conflict_data[cell_id]) {
            title_parts.push(cell_conflict_data[cell_id]);
        }

        $(this).attr('title', title_parts.join('\n'));
    });
}

function handle_map_change() {
    clear_all_highlights();

    const apply_updates = (map_data) => {
        active_mapping = map_data || {};
        apply_static_coloring(active_mapping);
        apply_wrong_number_info(active_mapping);
        update_all_cell_titles();
    };

    if (data_is_embedded) {
        // EMBEDDED MODE: Simply look up the data in the global object
        const map_data = all_maps[active_map_type] ? all_maps[active_map_type][active_version] : null;
        apply_updates(map_data);
    } else {
        // EXTERNAL MODE: Fetch the data from the corresponding JSON file
        const file_path = report_manifest[active_map_type]?.[active_version];
        if (!file_path) {
            apply_updates(null);
            return;
        }

        const selectors = $("#map_type_selector, #version_selector");
        selectors.prop("disabled", true); // Disable controls during fetch

        fetch(file_path)
            .then(response => {
                if (!response.ok) {
                    throw new Error(`HTTP error! status: ${response.status}`);
                }
                return response.json();
            })
            .then(data => {
                apply_updates(data);
            })
            .catch(error => {
                console.error("Failed to fetch map data:", error);
                alert("Failed to load map data. Please ensure you are viewing this report via a web server and the file exists: " + file_path);
                apply_updates(null);
            })
            .finally(() => {
                selectors.prop("disabled", false); // Re-enable controls
            });
    }
}


$(document).ready(function() {
    const map_types = Object.keys(data_is_embedded ? all_maps : report_manifest);
    if (map_types.length === 0) {
        $(".controls-div").hide();
        return;
    }

    // --- 1. DEFINE EVENT HANDLERS FIRST ---

    // Change map type
    $("#map_type_selector").on("change", function() {
        active_map_type = $(this).val();
        update_version_selector();
        $("#version_selector").trigger("change"); // Trigger version change to update view
    });

    // Change map version
    $("#version_selector").on("change", function() {
        active_version = $(this).val();
        handle_map_change();
    });

    // Click on a table cell
    $(document).on("click", ".tabnum, [id^=c][id*=_]", function(event) {
        clear_all_highlights();
        const cell_id = event.currentTarget.id;

        if (active_mapping && active_mapping.cell_to_code_idx && active_mapping.code_locations) {
            const location_idx = active_mapping.cell_to_code_idx[cell_id];
            if (typeof location_idx !== 'undefined') {
                const location_data = active_mapping.code_locations[location_idx];
                $(event.currentTarget).addClass("cell-highlight");
                last_cell_highlights.push("#" + cell_id);
                const script_num = location_data[1];
                const code_line = location_data[2];
                highlight_code(script_num, code_line);
            }
        }
    });

    // Click on a regression command in a do-file
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

    // Click on an item in the discrepancy report
    $(document).on("click", ".wrong-number-report-item", function() {
        clear_all_highlights();
        const cell_id = $(this).data("cell-id");
        if (!cell_id) return;
        const cell_element = $("#" + cell_id);
        if (cell_element.length > 0) {
            const tab_pane = cell_element.closest('.tab-pane[id^=tabtab]');
            if (tab_pane.length > 0) {
                const tab_id = tab_pane.attr('id');
                $('#tabtabs a[href="#' + tab_id + '"]').tab('show');
            }
            setTimeout(function() {
                cell_element[0].scrollIntoView({ behavior: "smooth", block: "center" });
                cell_element.addClass("wrong-number-report-highlight");
            }, 200);
        }
    });

    // --- 2. RUN INITIALIZATION LOGIC ---
    active_map_type = $("#map_type_selector").val();
    update_version_selector();
    $("#version_selector").trigger("change");
});
