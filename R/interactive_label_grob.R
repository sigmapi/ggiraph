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

  gTree(label = label, x = x, y = y,
        tooltip = tooltip, onclick = onclick, data_id = data_id,
        just = just, padding = padding, r = r,
      name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "interactive_label_grob")
}


#' @export
makeContent.interactive_label_grob <- function(x) {
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)

  t <- grob(
       tooltip = x$tooltip,
       onclick = x$onclick,
       data_id = x$data_id,
       label = x$label,
       x = x$x + 2 * (0.5 - hj) * x$padding,
       y = x$y + 2 * (0.5 - vj) * x$padding,
       just = c(hj, vj),
       name = "text",
       gp = x$text.gp,
       cl="interactive_text_grob")

  r <- roundrectGrob(
      x$x,
      x$y,
      default.units = "native",
      width = grobWidth(t) + 2 * x$padding,
      height = grobHeight(t) + 2 * x$padding,
      just = c(hj, vj),
      r = x$r,
      gp = x$rect.gp,
      name = "box"
  )

  setChildren(x, gList(r, t))
}
