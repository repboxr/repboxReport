Hello! As an expert R programmer, I've reviewed your project files and the issues you've described. I've identified the causes and prepared the necessary fixes for your JavaScript code. The issues are related to how highlights are cleared and how scrolling is handled. Here are the detailed changes:

### Summary of Changes

1.  **Number Highlighting Fix:** The logic for clearing number highlights in the log files was not robust, causing subsequent highlights to fail. I've changed the method from replacing the highlight `<span>` with its text to using jQuery's `unwrap()` method. This correctly restores the underlying structure of the text, allowing new highlights to be applied reliably.
2.  **Scrolling Behavior Fixes:**
    *   I've added a check to prevent the do-file pane from scrolling if the relevant code line is already visible on the screen.
    *   When scrolling is necessary (for both the code line and the log output), I've changed the scroll alignment from `"center"` to `"start"`. This will position the target element at the top of its pane, which addresses your request to maximize the visible area for the log output below the regression command.

I will now provide the modifications for the `inst/www/report_map.js` file.

!MODIFICATION clear_all_highlights in report_map.js
scope = "function"
file = "inst/www/report_map.js"
function_name = "clear_all_highlights"
description = '''Fix number re-highlighting by using `unwrap()` to clear previous highlights. This is more robust for restoring the DOM state of the log text and solves the issue where number highlighting only worked on the first click.'''
---
```javascript
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

    // Clear number highlights in logs by unwrapping the highlight span
    $('.number-highlight').contents().unwrap();

    // Normalize parent to merge adjacent text nodes that might result from unwrapping
    $('.logtxt-code').each(function(){
        if(this.normalize) this.normalize();
    });
}
```
!END_MODIFICATION clear_all_highlights in report_map.js

!MODIFICATION highlight_code in report_map.js
scope = "function"
file = "inst/www/report_map.js"
function_name = "highlight_code"
description = '''Update scrolling logic: 1. Prevent scrolling if the target code line is already in view. 2. Change scroll alignment to the top (`block: "start"`) of the pane to maximize visible log area, instead of the center.'''
---
```javascript
function highlight_code(script_num, line_num, runid_to_show, number_to_find) {
    $("#dotabs a[href='#dotab_" + script_num + "']").tab("show");

    // Defer actions to allow do-file tab to show first
    setTimeout(function() {
        // Highlight code line
        const code_id = "#L" + line_num + "___" + script_num;
        $(code_id).addClass("code-highlight");
        last_code_highlight = code_id;

        // Scroll code line into view first, if it's not already visible.
        const targetElement = document.querySelector(code_id);
        if (targetElement && !is_in_viewport(targetElement)) {
            targetElement.scrollIntoView({ behavior: "smooth", block: "start" });
        }

        // Show and scroll to log output if a valid runid is provided
        if (runid_to_show && runid_to_show !== 'null') {
            const log_container = $('#loginfo-' + line_num + '-' + script_num);

            const do_log_show_and_scroll = function() {
                const run_pre = $('#runid-' + runid_to_show);
                if (run_pre.length > 0) {
                    const scroll_and_highlight = function(pre_element) {
                        // Only scroll if the log is not already visible in its container
                        if (!is_in_viewport(pre_element[0])) {
                            pre_element[0].scrollIntoView({ behavior: 'smooth', block: 'start' });
                        }
                        // Highlight number in log
                        if (typeof number_to_find !== 'undefined' && number_to_find !== null) {
                            highlight_number_in_log(pre_element, number_to_find);
                        }
                    };

                    const tab_pane = run_pre.closest('.tab-pane');
                    if (tab_pane.length > 0) {
                         const pane_id = tab_pane.attr('id');
                         // Use event to ensure multi-run tab is shown before scroll
                         $('a[href="#' + pane_id + '"]').one('shown.bs.tab', function() {
                             scroll_and_highlight(run_pre);
                         }).tab('show');
                    } else {
                       // No tabs inside log, just scroll and highlight
                       scroll_and_highlight(run_pre);
                    }
                }
            };

            // If log is not visible, open it and then scroll. If already visible, just scroll.
            // Note: Bootstrap 3 uses 'in' class for visible collapsed elements
            if (log_container.hasClass('in')) {
                do_log_show_and_scroll();
            } else {
                log_container.one('shown.bs.collapse', do_log_show_and_scroll).collapse('show');
            }
        }
    }, 150);
}
```
!END_MODIFICATION highlight_code in report_map.js

These changes should resolve all the issues you mentioned. Please apply them to your project, rebuild the package, and regenerate a report to see the fixes in action. Let me know if you have any other questions
