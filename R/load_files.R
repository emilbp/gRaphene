#' Load results of graphene curve fit
#'
#' Attempts to load all curve fits in txt files from the specified directory
#'
#' @importFrom magrittr %>%
#' @param path Directory containing the data-files
#' @param ext Extension of data-files (default is 'txt')
#' @param peaks Regex string detailing the peaks to look for
#' @param measure Features to look for, for each peak
#' @keywords raman, curve fit
#' @export
#' @examples
#' gRdata_read()


gRdata_read <- function(path, ext = "txt", peaks = "^((D\\s)|(G\\s)|(2D\\s)|(aC\\s)|(DDp\\s)|(2Dp\\s)|(2Dpp\\s)|(SiO2\\s))", measure = "FWHM|pos|int") {
  filenames <- list.files(pattern = stringr::str_c('*.',ext), path = path)
  data <- tibble::tibble(filename = filenames) %>%
    dplyr::mutate(path = stringr::str_c(path, "/", filename),
      data = purrr::map(path, readr::read_tsv, col_names = c("x", "y", "value"), skip = 1),
      measure = stringr::str_c(stringr::str_extract(filename, peaks), stringr::str_extract(filename, measure))) %>%
    tidyr::unnest() %>%
    dplyr::mutate(xy = stringr::str_c(x, ", ", y)) %>%
    dplyr::select(xy, x, y, measure, value) %>%
    tidyr::spread(key = measure, value = value) %>%
    dplyr::mutate(`D/G-ratio` = `D int` / `G int`,
      `2D/G-ratio` = `2D int` / `G int`)
  data
}
