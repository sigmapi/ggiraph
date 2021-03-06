#' @title Vertical interactive reference line
#'
#' @description
#' The geometry is based on \code{\link[ggplot2]{geom_vline}}.
#' See the documentation for those functions for more details.
#'
#' @param ... arguments passed to base geometry.
#' @examples
#' # add interactive reference lines to a ggplot -------
#' @example examples/geom_vline_interactive.R
#' @seealso \code{\link{girafe}}
#' @export
geom_vline_interactive <- function(...) {
  layer_interactive(geom_vline, ...)
}

#' @rdname ggiraph-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveVline <- ggproto(
  "GeomInteractiveVline",
  GeomVline,
  default_aes = add_default_interactive_aes(GeomVline),
  draw_panel = function(data, panel_params, coord) {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- data$xintercept
    data$xend <- data$xintercept
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]

    GeomInteractiveSegment$draw_panel(unique(data), panel_params, coord)
  }
)
