#' @importFrom ggplot2 position_nudge
#' @title interactive textual annotations.
#'
#' @description
#' The geometry is based on \code{\link[ggplot2]{geom_label}}.
#' See the documentation for those functions for more details.
#'
#' @seealso \code{\link{ggiraph}}
#' @inheritParams geom_point_interactive
#' @param parse See \code{\link[ggplot2]{geom_point}}.
#' @param nudge_x,nudge_y See \code{\link[ggplot2]{geom_point}}.
#' @param check_overlap See \code{\link[ggplot2]{geom_point}}.
#' @examples
#' # add interactive polygons to a ggplot -------
#' @example examples/geom_text_interactive.R
#' @export
geom_label_interactive <- function(mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    ...,
    parse = FALSE,
    nudge_x = 0,
    nudge_y = 0,
    label.padding = unit(0.25, "lines"),
    label.r = unit(0.15, "lines"),
    label.size = 0.25,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomInteractiveLabel,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
          parse = parse,
          label.padding = label.padding,
          label.r = label.r,
          label.size = label.size,
          na.rm = na.rm,
          ...
      )
  )
}


#' @rdname ggiraph-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveLabel <- ggproto(
  "GeomInteractiveLabel",
  Geom,
  required_aes = c("x", "y", "label"),

  default_aes = aes(
      colour = "black", fill = "white", size = 3.88, angle = 0,
      hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
      lineheight = 1.2,
      tooltip = NULL, onclick = NULL, data_id = NULL
  ),

  draw_panel = function(self, data, panel_params, coord, parse = FALSE,
      na.rm = FALSE,
      label.padding = unit(0.25, "lines"),
      label.r = unit(0.15, "lines"),
      label.size = 0.25) {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    grobs <- lapply(1:nrow(data), function(i) {
          row <- data[i, , drop = FALSE]
          interactive_label_grob(lab[i],
              x = unit(row$x, "native"),
              y = unit(row$y, "native"),
              tooltip = row$tooltip,
              onclick = row$onclick,
              data_id = row$data_id,
              just = c(row$hjust, row$vjust),
              padding = label.padding,
              r = label.r,
              text.gp = gpar(
                  col = row$colour,
                  fontsize = row$size * .pt,
                  fontfamily = row$family,
                  fontface = row$fontface,
                  lineheight = row$lineheight
              ),
              rect.gp = gpar(
                  col = if (isTRUE(all.equal(label.size, 0))) NA else row$colour,
                  fill = alpha(row$fill, row$alpha),
                  lwd = label.size * .pt
              )
          )
        })
    class(grobs) <- "gList"

    ggname("geom_label_interactive", grobTree(children = grobs))
  },

  draw_key = draw_key_label
)

ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

