// FILE: report_map.js

// Global variables set by the R backend
// var data_is_embedded = true;
// var all_maps = { ... }; // Populated if data_is_embedded is true
// var report_manifest = { ... }; // Populated if data_is_embedded is false

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
        } else {
            // EXTERNAL MODE: Fetch the data from the corresponding JSON file
            const file_path = report_manifest[active_map_type]?.[active_version];
            if (!file_path) {
                active_mapping = {};
                apply_static_coloring(active_mapping);
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
                })
                .catch(error => {
                    console.error("Failed to fetch map data:", error);
                    alert("Failed to load map data. Please ensure you are viewing this report via a web server and the file exists: " + file_path);
                    active_mapping = {};
                    apply_static_coloring(active_mapping);
                })
                .finally(() => {
                    selectors.prop("disabled", false); // Re-enable controls
                });
        }
    });

// --- In file: report_map.js ---

    // The selector is: $(document).on("click", ".tabnum, [id^=c][id*=_]", ...
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

    // --- 2. RUN INITIALIZATION LOGIC ---

    // Set the initial map type from the first option in the selector
    active_map_type = $("#map_type_selector").val();

    // Populate the version selector based on the initial map type
    update_version_selector();

    // Now, trigger the change event on the version selector.
    // Since the handler is already attached, this will correctly
    // set the initial active_mapping and apply static colors (or fetch data).
    $("#version_selector").trigger("change");
});
