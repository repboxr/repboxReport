#' Standardize regression indices across different map types.
#'
#' This function ensures that the same regression (defined by a unique set of
#' cells in a table) has the same `reg_ind` across all loaded map types.
#' This is crucial for consistent color-coding in the report.
#'
#' @param all_map_types A list of lists of data frames, as loaded in `rr_map_report`.
#'                      Structure: `list(map_type_id = list(version_id = data.frame))`.
#' @return The modified `all_map_types` list with a standardized `reg_ind` column.
#'
#' @details
#' The process works as follows:
#' 1. It collects all unique regressions (defined by tabid + cell_ids) from all maps.
#' 2. It creates a canonical representation for each regression's cell_ids (sorted, unique).
#' 3. It creates a master mapping from `tabid` + `canonical_cell_ids` to a new, globally consistent `reg_ind`.
#'    The new `reg_ind` values are assigned by iterating through tables in sorted order.
#' 4. It applies this master mapping back to every data frame in `all_map_types`,
#'    overwriting the original `reg_ind` column.
rr_standardize_reg_ind <- function(all_map_types) {

  # Helper to canonicalize cell_id strings
  # e.g., "c2_12, c2_10, c2_10 " -> "c2_10,c2_12"
  canonicalize_cell_ids <- function(cell_ids_str) {
    if (is.na(cell_ids_str) || cell_ids_str == "") return(NA_character_)
    ids <- trimws(unlist(strsplit(cell_ids_str, ",")))
    ids <- ids[ids != ""]
    if (length(ids) == 0) return(NA_character_)
    paste(sort(unique(ids)), collapse = ",")
  }

  # 1. Collect all unique regressions from all maps
  all_map_dfs <- unlist(all_map_types, recursive = FALSE)

  # Filter out empty or invalid dataframes
  all_map_dfs <- all_map_dfs[sapply(all_map_dfs, function(df) !is.null(df) && nrow(df) > 0 && "cell_ids" %in% names(df) && "tabid" %in% names(df))]

  if (length(all_map_dfs) == 0) {
    return(all_map_types)
  }

  # Combine all data frames into one
  combined_df <- dplyr::bind_rows(all_map_dfs) %>%
    dplyr::select(tabid, cell_ids) %>%
    dplyr::filter(!is.na(tabid), !is.na(cell_ids), cell_ids != "") %>%
    dplyr::distinct()

  if (nrow(combined_df) == 0) {
      return(all_map_types)
  }

  # 2. Create canonical representation for cell_ids
  combined_df$canonical_cells <- sapply(combined_df$cell_ids, canonicalize_cell_ids)

  master_reg_df <- combined_df %>%
    dplyr::select(tabid, canonical_cells) %>%
    dplyr::filter(!is.na(canonical_cells)) %>%
    dplyr::distinct()

  # 3. Create the master mapping to new reg_ind values
  master_reg_df <- master_reg_df %>%
    dplyr::arrange(tabid, canonical_cells) %>%
    dplyr::mutate(new_reg_ind = dplyr::row_number())

  master_lookup <- master_reg_df %>%
    dplyr::select(tabid, canonical_cells, new_reg_ind)

  # 4. Apply this master mapping back to every data frame
  updated_all_map_types <- purrr::map(all_map_types, function(map_list_for_type) {
    purrr::map(map_list_for_type, function(map_df) {
      if (is.null(map_df) || nrow(map_df) == 0 || !"cell_ids" %in% names(map_df) || !"tabid" %in% names(map_df)) {
        return(map_df)
      }

      map_df_with_canonical <- map_df %>%
        dplyr::mutate(canonical_cells = sapply(cell_ids, canonicalize_cell_ids))

      updated_df <- dplyr::left_join(
        map_df_with_canonical,
        master_lookup,
        by = c("tabid", "canonical_cells")
      )

      updated_df$reg_ind <- updated_df$new_reg_ind

      updated_df <- updated_df %>%
        dplyr::select(-canonical_cells, -new_reg_ind)

      return(updated_df)
    })
  })

  return(updated_all_map_types)
}
