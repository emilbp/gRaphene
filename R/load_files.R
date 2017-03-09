#' Load results of graphene curve fit
#'
#' Attempts to load all curve fits in txt files from the specified directory
#' @param path Directory containing the data-files
#' @param ext Extension of data-files (default is 'txt')
#' @param peaks Regex string detailing the peaks to look for
#' @param measure Features to look for, for each peak
#' @keywords raman, curve fit
#' @export
#' @examples
#' gRdata_read()


gRdata_read <- function(path, ext = "txt", peaks = "^((D\\s)|(G\\s)|(2D\\s)|(aC\\s)|(DDp\\s)|(2Dp\\s)|(2Dpp\\s)|(SiO2\\s))", measure = "FWHM|pos|int") {
  filenames <- list.files(pattern = str_c('*.',ext), path = path)
  data <- tibble(filename = filenames) %>%
    mutate(path = str_c(path, "/", filename),
      data = map(path, read_tsv, col_names = c("x", "y", "value"), skip = 1),
      measure = str_c(str_extract(filename, peaks), str_extract(filename, measure))) %>%
    unnest() %>%
    mutate(xy = str_c(x, ", ", y)) %>%
    #       id = unique(xy) %>% length() %>% seq(1, ., 1) %>% rep(15)) %>%
    select(xy, x, y, measure, value) %>%
    spread(key = measure, value = value) %>%
    mutate(`D/G-ratio` = `D int` / `G int`,
      `2D/G-ratio` = `2D int` / `G int`)
  data
}
