#' Plot graphene curve fit
#'
#' Accepts a dataframe with columns x, y and value to plot a raster map
#'
#' @param df Dataframe with columns x, y and value
#' @param name Name of the plot (goes on the colorbar)
#' @param scalebar Size of the scalebar (defaults to 1 um)
#' @keywords raman, curve fit, map
#' @export
#' @examples
#' gr_cf_plot(df, "2D/G-ratio", scalebar = 5)

gr_cf_plot <- function(df, name, scalebar = 1, palette = "Spectral", interpolate = FALSE) {
  sb = tibble::tibble(x1 = max(df$x) - 0.1, x2 = x1 - scalebar, y1 = max(df$y) - 0.1, y2 = y1 - scalebar/8)
  scalebar = tibble::tribble(
    ~x, ~y,
    sb$x1, sb$y1,
    sb$x1, sb$y2,
    sb$x2, sb$y2,
    sb$x2, sb$y1
  )

  ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_raster(interpolate = interpolate) +
    ggplot2::coord_equal() +
    ggplot2::theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    ) +
    ggplot2::scale_fill_distiller(name = name, palette = palette) +
    ggplot2::geom_polygon(data = scalebar, ggplot2::aes(x = x, y = y), fill = "white") +
    ggplot2::scale_y_reverse()
}
