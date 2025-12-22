// FILE: rr_classify_map.js

/**
 * Helper: Resolve dimension inheritance and clean vars for grouping comparison.
 * Returns a simplified object representing the regression's structure.
 */
function get_resolved_structure(reg, all_regs_in_table) {
    // 1. Resolve dimensions
    let dims = reg.dimensions;
    if (reg.dimensions_same_as_regid) {
        // Find the parent regression within the same table context
        const parent = all_regs_in_table.find(r => r.regid === reg.dimensions_same_as_regid);
        if (parent && parent.dimensions) {
            dims = parent.dimensions;
        }
    }

    // Clone and clean dimensions for signature (remove fields that might vary like cell_ids if present)
    let dims_sig = dims ? JSON.parse(JSON.stringify(dims)) : [];

    // 2. Clone and clean variables for signature
    // We remove 'cell_id_estimate' so that variables appearing in different cells
    // but representing the same concept are grouped together.
    let vars_sig = reg.vars ? JSON.parse(JSON.stringify(reg.vars)) : [];
    vars_sig.forEach(v => { delete v.cell_id_estimate; });

    return {
        tags: reg.regression_tags || "",
        vars: vars_sig,
        dims: dims_sig
    };
}

/**
 * Renders a compact summary of all regressions in a table, grouped by identical structure.
 */
function update_table_summary_panel(tabid) {
    const $detail = $("#classify-details");
    $detail.empty();

    // Check data availability
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

    // Header
    $detail.append('<h4>Table ' + tabid + ' Overview</h4>');
    $detail.append('<p class="text-muted small">Regressions grouped by identical structure.</p>');

    // Grouping Logic
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

    // Rendering Groups
    groups.forEach(g => {
        const regList = g.regids.join(", ");

        let html = `<div class="summary-group">`;
        html += `<div><strong>Regressions:</strong> <span class="summary-reg-list">${regList}</span></div>`;

        // 1. Tags
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

        // 2. Variables (Compact Table)
        if (g.data.vars && g.data.vars.length > 0) {
             html += '<table class="class-table table-condensed" style="margin-top:5px; margin-bottom:5px;">';
             html += '<thead><tr><th style="width:10%">Role</th><th>Label</th><th>Code</th></tr></thead><tbody>';
             g.data.vars.forEach(v => {
                 // Format Role Badge
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

        // 3. Dimensions (Inline list)
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

        html += `</div>`; // End summary-group
        $detail.append(html);
    });
}

/**
 * Updates the right-hand panel with DETAILED classification info for a specific regression.
 */
function update_classification_panel(tabid, regid) {
    const $detail = $("#classify-details");
    $detail.empty();

    if (!window.all_classifications || !Array.isArray(window.all_classifications)) return;

    const record = window.all_classifications.find(r => String(r.tabid) === String(tabid) && String(r.regid) === String(regid));

    if (!record) {
        $detail.append('<div class="alert alert-warning">No classification info found for <strong>Tab ' + tabid + ', Reg ' + regid + '</strong></div>');
        return;
    }

    // Add a "Back" button to return to summary
    $detail.append(`<button class="btn btn-xs btn-default pull-right" onclick="update_table_summary_panel('${tabid}')"><span class="glyphicon glyphicon-arrow-left"></span> Summary</button>`);

    // --- Helper: Section Builder ---
    function buildSection(title, contentHtml) {
        if (!contentHtml) return;
        $detail.append(`
            <div class="class-section">
                <div class="class-header">${title}</div>
                ${contentHtml}
            </div>
        `);
    }

    // --- 1. Header & Description ---
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

    // --- 2. Variables (Vars) ---
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
            let cellLink = '';
            if (v.cell_id_estimate) {
                cellLink = ` <span class="cell-link class-cell-interaction" data-cellid="${v.cell_id_estimate}" title="Show in table"> <span class="glyphicon glyphicon-map-marker"></span></span>`;
            }

            varsHtml += `<tr>
                <td>${typeBadge}</td>
                <td>${label}${unitStr}</td>
                <td>${codeVar}${cellLink}</td>
            </tr>`;
        });
        varsHtml += '</tbody></table>';
        buildSection("Variables", varsHtml);
    }

    // --- 3. Dimensions ---
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

    // --- 4. Reported Stats ---
    if (record.reported_stats && Array.isArray(record.reported_stats) && record.reported_stats.length > 0) {
        let statHtml = '<table class="class-table"><thead><tr><th>Statistic</th><th>Table Val</th><th>Code Val</th></tr></thead><tbody>';
        record.reported_stats.forEach(s => {
            const label = s.stat_label || '?';
            const valTab = (s.value_table !== null) ? s.value_table : '<span class="text-muted">-</span>';
            const valCode = (s.value_code !== null) ? s.value_code : '<span class="text-muted">-</span>';
            let cellLink = s.cell_id ? ` <span class="cell-link class-cell-interaction" data-cellid="${s.cell_id}"><span class="glyphicon glyphicon-map-marker"></span></span>` : '';

            statHtml += `<tr>
                <td>${label}</td>
                <td>${valTab}${cellLink}</td>
                <td>${valCode}</td>
            </tr>`;
        });
        statHtml += '</tbody></table>';
        buildSection("Reported Statistics", statHtml);
    }
}

// Initialization and Events
$(document).ready(function() {

    // 1. Data Loading (External mode support)
    if ((typeof window.all_classifications === 'undefined' || window.all_classifications === null) &&
         typeof window.classification_file !== 'undefined' && window.classification_file) {

        fetch(window.classification_file)
            .then(response => {
                if (!response.ok) throw new Error("HTTP error " + response.status);
                return response.json();
            })
            .then(data => {
                window.all_classifications = data;
                // Once loaded, trigger update for current tab if any
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
        // Data already embedded, init first view
        const activeTab = $("#tabtabs li.active > a").attr("href");
        if(activeTab) {
            const tabid = activeTab.replace("#tabtab", "");
            update_table_summary_panel(tabid);
        }
    }

    // 2. Tab Change Listener -> Show Summary
    $(document).on('shown.bs.tab', '#tabtabs a[data-toggle="tab"]', function (e) {
        const target = $(e.target).attr("href"); // activated tab
        const tabid = target.replace("#tabtab", "");
        // Clear highlights and show summary
        if(window.clear_all_highlights) window.clear_all_highlights();
        update_table_summary_panel(tabid);
    });

    // 3. Tab Click Listener -> Force Summary if already active
    $("#tabtabs").on('click', 'a[data-toggle="tab"]', function (e) {
        const target = $(this).attr("href");
        const parentLi = $(this).parent();
        if(parentLi.hasClass('active')) {
             const tabid = target.replace("#tabtab", "");
             // Clear highlights and show summary
             if(window.clear_all_highlights) window.clear_all_highlights();
             update_table_summary_panel(tabid);
        }
    });

    // 4. Cell Click -> Show Details
    $(document).on("click", ".tabnum, [id^=c][id*=_]", function(event) {
        const cell_id = event.currentTarget.id;
        if (window.active_mapping && window.active_mapping.cell_map && window.active_mapping.cell_map[cell_id]) {
            const info = window.active_mapping.cell_map[cell_id];

            // Get tabid from the container if not explicit
            const $tabPane = $(event.currentTarget).closest('.tab-pane[id^="tabtab"]');
            const tabid = $tabPane.length ? $tabPane.attr('id').replace('tabtab', '') : null;

            if (info.regid && tabid) {
                update_classification_panel(tabid, info.regid);
            }
        }
    });

    // 5. Interaction from right-panel back to table
    $(document).on("click", ".class-cell-interaction", function(e) {
        e.preventDefault();
        const cellId = $(this).data("cellid");
        if (cellId && window.highlight_cells) {
            const match = cellId.match(/^c(\d+)_/);
            const tabid = match ? match[1] : null;
            if (tabid) {
                if (window.clear_all_highlights) window.clear_all_highlights();
                window.highlight_cells(tabid, cellId);
            }
        }
    });
});
