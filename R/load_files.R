#' Load results of graphene curve fit
#'
#' Attempts to load all curve fits in txt files from the specified directory
#'
#' All files should be named as "<peak> <feature>.txt", e.g. "2D FWHM.txt" and be put in the folder specified under \code{path}.
#' The dataframe that is returned will contain the "<peak> <feature>" as the column names - for this to work, the filenames should only contain letters, numbers and spaces.
#'
#' @importFrom magrittr %>%
#' @param path Directory containing the data-files
#' @param ext Extension of data-files (default is 'txt')
#' @keywords raman, curve fit
#' @family curve fit functions
#' @export
#' @examples
#' gr_cf_read("data")
#' gr_cf_read(system.file('extdata/graphene_curve_fit_export', package = 'gRaphene'))


gr_cf_read <- function(path, ext = "txt") {
  filenames <- list.files(pattern = stringr::str_c('*.',ext), path = path)

  data <- tibble::tibble(filename = filenames) %>%
    dplyr::mutate(path = stringr::str_c(path, "/", filename),
      data = purrr::map(path, readr::read_tsv, col_names = c("x", "y", "value"), skip = 1),
      measure = stringr::str_extract(filenames, pattern = "[0-9a-zA-ZæøåÆØÅ\\s]*")) %>%
    tidyr::unnest() %>%
    dplyr::mutate(xy = stringr::str_c(x, ", ", y)) %>%
    dplyr::select(xy, x, y, measure, value) %>%
    tidyr::spread(key = measure, value = value) %>%
    dplyr::mutate(id = 1:n()) %>%
    dplyr::select(id, everything(), -xy)

  if (all(c('D int', 'G int') %in% colnames(data))) {
    data <- data %>% dplyr::mutate(`D/G-ratio` = `D int` / `G int`)
  }
  if (all(c('2D int', 'G int') %in% colnames(data))) {
    data <- data %>% dplyr::mutate(`2D/G-ratio` = `2D int` / `G int`)
  }
  if (all(c('Dp int', 'D int') %in% colnames(data))) {
    data <- data %>% dplyr::mutate(`D/Dp-ratio` = `D int` / `Dp int`)
  }
  data
}


#' Load complete spectrum map from exported txt file
#'
#' Load all spectra and corresponding (x,y) coordinates and wrangles into a tidy tibble
#' Expects a tab-separated file with 4 columns (5 if z-value is present - not implemented yet!)
#'
#' @importFrom magrittr %>%
#' @param path Path to txt file containing the spectrum map
#' @keywords raman, spectrum map
#' @export
#' @examples
#' gr_map_read("graphene_map.txt")
#' gr_map_read(system.file('extdata', 'graphene_spectrum_map_export.txt', package = 'gRaphene'))

gr_map_read <- function(path) {
  data <- readr::read_tsv(path, col_names = c('x', 'y', 'wavenumber', 'intensity'), skip = 1) %>%
    dplyr::mutate(id = stringr::str_c(x, ", ", y)) %>%
    tidyr::spread(key = wavenumber, value = intensity) %>%
    dplyr::select(-id)
}
