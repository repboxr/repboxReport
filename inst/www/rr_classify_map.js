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
