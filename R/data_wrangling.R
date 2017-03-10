#' multi.fun
#'
#' Combined function for summary statistics
#'
#' @param x column of values for which to calculate statistics
#' @keywords summary
#' @export
#' @examples
#' multi.fun(x)

multi.fun <- function(x) {
  c(min = min(x), mean = mean(x), max = max(x), sd = sd(x))
}

#' Create summary of a graphene curve fit data frame
#'
#' Takes the results of gRdata_read and computes summary statistics
#'
#' @importFrom magrittr %>%
#' @param df Computes summary (min, mean, max and sd) for \code{df}
#' @keywords summary
#' @export
#' @examples
#' gR_summary()

gR_summary <- function(df) {
  df_summary <- df %>%
    dplyr::select(-x, -y, -xy) %>%
    purrr::map(multi.fun) %>%
    tibble::as_tibble()

  df_summary <- dplyr::bind_cols(dplyr::tibble(key = c('min', 'mean', 'max', 'sd')), df_summary)

  df_summary
}


#' Saves summary statistics for graphene curve fit
#'
#' Saves the summary df to a csv file (default name is summary.csv)
#'
#' @importFrom magrittr %>%
#' @param df_summary Dataframe containing summary statistics
#' @param filename Filename of csv file (defaults to summary.csv)
#' @keywords summary
#' @export
#' @examples
#' gR_summary_save()
gR_summary_save <- function(df_summary, filename = 'summary.csv') {
  readr::write_excel_csv(data_summary, filename)
}
