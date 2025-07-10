#' This function provides a list of default settings that can be passed to `rr_map_report`.
#'
#' **Options:**
#' * `output_for`: Determines for which commands the log output is shown.
#'   - `"reg"`: (Default) Show output only for regression commands (`is_reg=TRUE`). This is recommended to keep the report size manageable.
#'   - `"all"`: Show output for all executed commands.
#'   - `"none"`: Do not show any log output.
#' * `map_prod_ids`: A character vector of map product IDs to load and display in the report. The user can switch between these map types in the UI.
#'
#' @return A list of default options.
#' @export
rr_map_report_opts <- function(output_for = c("all", "reg","none")[2] , map_prod_ids = c("map_reg_run", "map_inv_reg_run", "map_reg_static")) {
  as.list(environment())
}
