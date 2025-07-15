// FILE: report_map.js

// Global variables set by the R backend
// var data_is_embedded = true;
// var all_maps = { ... }; // Populated if data_is_embedded is true
// var report_manifest = { ... }; // Populated if data_is_embedded is false
// var cell_info_map = { ... }; // Populated with aggregated cell info

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

function applyCellTitles() {
    // cell_info_map is loaded from the HTML script tag
    if (typeof cell_info_map === 'undefined' || Object.keys(cell_info_map).length === 0) return;

    // Iterate over all potential table cells
    $("[id^=c][id*=_]").each(function() {
        const cell_id = this.id;
        const info = cell_info_map[cell_id];
        let title_parts = [`cell_id: ${cell_id}`];

        if (info) {
            if (info.reg_inds && info.reg_inds.length > 0) {
                // reg_inds are already sorted and unique from R
                title_parts.push(`reg_ind(s): ${info.reg_inds.join(', ')}`);
                if (info.reg_inds.length > 1) {
                    title_parts.push('(Note: Mapped to multiple regressions across different map versions)');
                }
            }
            if (info.is_wrong) {
                title_parts.push('(Note: Marked as having a wrong number in at least one map version)');
            }
        }
        $(this).attr('title', title_parts.join('\n'));
    });
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
        // Trigger the version change to update the mapping and view
        $("#version_selector").trigger("change");
    });

    // Change map version
    $("#version_selector").on("change", function() {
        active_version = $(this).val();
        clear_all_highlights();

        if (data_is_embedded) {
            // EMBEDDED MODE: Simply look up the data in the global object
            if (active_map_type && active_version && all_maps[active_map_type] && all_maps[active_map_type][active_version]) {
                active_mapping = all_maps[active_map_type][active_version];
            } else {
                active_mapping = {};
            }
            apply_static_coloring(active_mapping);
            apply_wrong_number_info(active_mapping);
        } else {
            // EXTERNAL MODE: Fetch the data from the corresponding JSON file
            const file_path = report_manifest[active_map_type]?.[active_version];
            if (!file_path) {
                active_mapping = {};
                apply_static_coloring(active_mapping);
                apply_wrong_number_info(active_mapping);
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
                    active_mapping = data;
                    apply_static_coloring(active_mapping);
                    apply_wrong_number_info(active_mapping);
                })
                .catch(error => {
                    console.error("Failed to fetch map data:", error);
                    alert("Failed to load map data. Please ensure you are viewing this report via a web server and the file exists: " + file_path);
                    active_mapping = {};
                    apply_static_coloring(active_mapping);
                    apply_wrong_number_info(active_mapping);
                })
                .finally(() => {
                    selectors.prop("disabled", false); // Re-enable controls
                });
        }
    });

    // Click on a table cell
    $(document).on("click", ".tabnum, [id^=c][id*=_]", function(event) {
        clear_all_highlights();
        const cell_id = event.currentTarget.id;

        // Check for the new properties in the active mapping
        if (active_mapping && active_mapping.cell_to_code_idx && active_mapping.code_locations) {
            // Find the index for our cell's code location
            const location_idx = active_mapping.cell_to_code_idx[cell_id];

            // Check that the index is valid (not undefined)
            if (typeof location_idx !== 'undefined') {
                // Retrieve the location data using the index
                const location_data = active_mapping.code_locations[location_idx];
                // location_data is an array: [runid, script_num, code_line]

                $(event.currentTarget).addClass("cell-highlight");
                last_cell_highlights.push("#" + cell_id);

                // Access data by index
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

            // Defer scroll and highlight to ensure tab is visible
            setTimeout(function() {
                cell_element[0].scrollIntoView({ behavior: "smooth", block: "center" });
                cell_element.addClass("wrong-number-report-highlight");
            }, 200);
        }
    });

    // --- 2. RUN INITIALIZATION LOGIC ---

    // Apply titles to table cells (this is static for the report)
    applyCellTitles();

    // Set the initial map type from the first option in the selector
    active_map_type = $("#map_type_selector").val();

    // Populate the version selector based on the initial map type
    update_version_selector();

    // Now, trigger the change event on the version selector.
    // Since the handler is already attached, this will correctly
    // set the initial active_mapping and apply static colors (or fetch data).
    $("#version_selector").trigger("change");
});
