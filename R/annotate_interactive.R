#' @title interactive annotations
#'
#' @description
#' Create interactive annotations, similar to ggplot2::annotate.
#' See the documentation for that function for more details.
#'
#' @seealso \code{\link{ggiraph}}
#' @export
annotate_interactive <-
    function (geom,
        x = NULL,
        y = NULL,
        xmin = NULL,
        xmax = NULL,
        ymin = NULL,
        ymax = NULL,
        xend = NULL,
        yend = NULL,
        ...,
        na.rm = FALSE)
{
  geom <- check_interactive_class(geom, env = parent.frame())
  annotate(
      geom = geom,
      x = x,
      y = y,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      xend = xend,
      yend = yend,
      ...,
      na.rm = na.rm
  )
}

# adapted from gglpot2 check_subclass in layer.r
check_interactive_class <- function(x,
    env = parent.frame()) {
  if (inherits(x, "Geom")) {
    name <- eval(quote(deparse(substitute(x))), envir = parent.frame())
  } else if (is.character(x) && length(x) == 1) {
    name <- x
    if (name == "histogram") {
      name <- "bar"
    }
    if (!startsWith(name, "GeomInteractive")) {
      name <- paste0("GeomInteractive", camelize(name, first = TRUE))
    }
  } else {
    stop(
        "`geom` must be either a string or a GeomInteractive* object, ",
        "not ", obj_desc(x),
        call. = FALSE
    )
  }
  obj <- find_global(name, env = env)
  if (is.null(obj) || !inherits(obj, "Geom")) {
    stop("Can't find interactive geom function called \"", x, "\"", call. = FALSE)
  } else {
    obj
  }
}

# from gglpot2 scale-type.r
find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("ggiraph")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}

# from gglpot2 utilities.r
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

# from gglpot2 utilities.r
firstUpper <- function(s) {
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "")
}

