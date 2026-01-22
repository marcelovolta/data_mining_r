#' Basic cleanup of a dataset
#'
#' Applies standard cleaning rules:
#' - Normalizes column names
#' - Removes duplicates
#' - Empty strings set as NA
#'
#' @param df data.frame or tibble
#' @return clean tibble l
#' @examples
#' df_clean <- clean_data(df)
clean_data <- function(df) {
  stopifnot(is.data.frame(df))
  
  df %>%
    dplyr::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ dplyr::na_if(.x, "")
      )
    )
}
