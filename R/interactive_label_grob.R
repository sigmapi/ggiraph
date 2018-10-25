#' @title Generate interactive grob label
#' @description This function can be used to generate interactive grob
#' label.
#'
#' @inheritParams grid::textGrob
#' @param tooltip tooltip associated with rectangles
#' @param onclick javascript action to execute when rectangle is clicked
#' @param data_id identifiers to associate with rectangles
#' @export
interactive_label_grob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    tooltip = NULL,
    onclick = NULL,
    data_id = NULL,
    just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
    default.units = "npc", name = NULL,
    text.gp = gpar(), rect.gp = gpar(fill = "white"), vp = NULL) {

  stopifnot(length(label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
      name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "interactive_label_grob")
}


#' @export
makeContent.labelgrob <- function(x) {
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)

  t <- textGrob(
      x$label,
      x$x + 2 * (0.5 - hj) * x$padding,
      x$y + 2 * (0.5 - vj) * x$padding,
      just = c(hj, vj),
      gp = x$text.gp,
      name = "text"
  )

  r <- roundrectGrob(x$x, x$y, default.units = "native",
      width = grobWidth(t) + 2 * x$padding,
      height = grobHeight(t) + 2 * x$padding,
      just = c(hj, vj),
      r = x$r,
      gp = x$rect.gp,
      name = "box"
  )

  setChildren(x, gList(r, t))
}

#' @export
#' @title interactive_label_grob drawing
#' @description draw an interactive_label_grob
#' @inheritParams grid::drawDetails
drawDetails.interactive_label_grob <- function(x,recording) {
  rvg_tracer_on()
  argnames = setdiff( names(x), c("tooltip", "onclick", "data_id") )
  do.call( grid.text, x[argnames] )

  ids = rvg_tracer_off()
  if( length( ids ) > 0 ) {
    if( !is.null( x$tooltip ))
      set_attr( ids = as.integer( ids ), str = encode_cr(x$tooltip), attribute = "title" )
    if( !is.null( x$onclick ))
      set_attr( ids = as.integer( ids ), str = x$onclick, attribute = "onclick" )
    if( !is.null( x$data_id ))
      set_attr( ids = as.integer( ids ), str = x$data_id, attribute = "data-id" )
  }
  invisible()
}
