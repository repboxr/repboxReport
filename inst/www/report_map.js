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
    $(".statically-colored").removeClass("statically-colored").css("background-color", "");
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
                    $(cell_selector).css('background-color', info.color).addClass('statically-colored');
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
    const versions = Object.keys(all_maps[active_map_type] || {});
    const version_selector = $("#version_selector");
    version_selector.empty();
    versions.forEach(function(v) {
        version_selector.append($("<option></option>").attr("value", v).text(v));
    });
}


$(document).ready(function() {
    const map_types = Object.keys(all_maps);
    if (map_types.length === 0) {
        $(".controls-div").hide();
        return;
    }

    // -- Initialize Controls and State --
    active_map_type = $("#map_type_selector").val();
    update_version_selector();
    $("#version_selector").trigger("change"); // Set initial mapping and coloring

    // -- Event Handlers --

    // Change map type
    $("#map_type_selector").on("change", function() {
        active_map_type = $(this).val();
        update_version_selector();
        $("#version_selector").trigger("change"); // Cascade change to update mapping
    });

    // Change map version
    $("#version_selector").on("change", function() {
        active_version = $(this).val();
        if (active_map_type && active_version && all_maps[active_map_type] && all_maps[active_map_type][active_version]) {
            active_mapping = all_maps[active_map_type][active_version];
        } else {
            active_mapping = {};
        }
        clear_all_highlights();
        apply_static_coloring(active_mapping);
    });

    // Click on a table cell
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
});
