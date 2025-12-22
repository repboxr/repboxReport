// FILE: rr_classify_map.js

/**
 * Updates the right-hand panel with classification details for a specific regression.
 */
function update_classification_panel(tabid, regid) {
    const $detail = $("#classify-details");
    $detail.empty();

    // Check if data is loaded
    if (!window.all_classifications || !Array.isArray(window.all_classifications)) {
        if (typeof window.classification_file !== 'undefined' && window.classification_file) {
             $detail.append('<div class="alert alert-info">Loading classification data...</div>');
             return; // Data is loading asynchronously, will need user to click again or we trigger auto-update later
        }
        return;
    }

    const record = window.all_classifications.find(r => String(r.tabid) === String(tabid) && String(r.regid) === String(regid));

    if (!record) {
        $detail.append('<div class="alert alert-warning">No classification info found for <strong>Tab ' + tabid + ', Reg ' + regid + '</strong></div>');
        return;
    }

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
                if (tag.includes('experiment')) cls = 'label-success';
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
            if (v.var_type === 'd') typeBadge = '<span class="var-type-badge" style="background:#dff0d8;border-color:#d6e9c6;color:#3c763d;">Dep. Var</span>';
            else if (v.var_type === 'x_eff') typeBadge = '<span class="var-type-badge" style="background:#d9edf7;border-color:#bce8f1;color:#31708f;">Effect</span>';
            else if (v.var_type === 'fe') typeBadge = '<span class="var-type-badge">FE</span>';
            else if (v.var_type === 'x_co') typeBadge = '<span class="var-type-badge">Control</span>';
            else typeBadge = `<span class="var-type-badge">${v.var_type || '?'}</span>`;

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

            let codeRep = d.var_in_code ? `<code>${d.var_in_code}</code>` : '';
            if (d.dummy_set) codeRep = `<code>${d.dummy_set}</code> <small>(dummies)</small>`;
            if (!codeRep) codeRep = '<span class="text-muted">-</span>';
            let unitStr = d.unit ? `<br><small class="text-muted">Unit: ${d.unit}</small>` : '';

            dimHtml += `<tr>
                <td>${cls}</td>
                <td>${type}${unitStr}</td>
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
            const valTab = (s.value_table !== null && s.value_table !== undefined) ? s.value_table : '<span class="text-muted">-</span>';
            const valCode = (s.value_code !== null && s.value_code !== undefined) ? s.value_code : '<span class="text-muted">-</span>';
            let cellLink = '';
            if (s.cell_id) {
                cellLink = ` <span class="cell-link class-cell-interaction" data-cellid="${s.cell_id}" title="Show in table"><span class="glyphicon glyphicon-map-marker"></span></span>`;
            }

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

// Initialization
$(document).ready(function() {

    // Load external classification data if needed
    if ((typeof window.all_classifications === 'undefined' || window.all_classifications === null) &&
         typeof window.classification_file !== 'undefined' && window.classification_file) {

        fetch(window.classification_file)
            .then(response => {
                if (!response.ok) throw new Error("HTTP error " + response.status);
                return response.json();
            })
            .then(data => {
                window.all_classifications = data;
                console.log("External classification data loaded.");
            })
            .catch(error => {
                console.error("Failed to load classification data:", error);
                $("#classify-details").html('<div class="alert alert-danger">Failed to load classification data from ' + window.classification_file + '</div>');
            });
    }

    // Event Listeners
    $(document).on("click", ".tabnum, [id^=c][id*=_]", function(event) {
        const cell_id = event.currentTarget.id;
        if (window.active_mapping && window.active_mapping.cell_map && window.active_mapping.cell_map[cell_id]) {
            const info = window.active_mapping.cell_map[cell_id];
            let tabid = null;
            const $tabPane = $(event.currentTarget).closest('.tab-pane[id^="tabtab"]');
            if ($tabPane.length) {
                tabid = $tabPane.attr('id').replace('tabtab', '');
            }

            if (info.regid && tabid) {
                update_classification_panel(tabid, info.regid);
            }
        }
    });

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
