% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_bar_interactive.R
\name{geom_bar_interactive}
\alias{geom_bar_interactive}
\title{interactive bars}
\usage{
geom_bar_interactive(...)
}
\arguments{
\item{...}{arguments passed to base geometry.}
}
\description{
The geometry is based on \code{\link[ggplot2]{geom_bar}}.
See the documentation for those functions for more details.
}
\examples{
# add interactive bar -------
library(ggplot2)
library(ggiraph)

p <- ggplot(mpg, aes( x = class, tooltip = class,
        data_id = class ) ) +
  geom_bar_interactive()

x <- girafe(ggobj = p)
if( interactive() ) print(x)

dat <- data.frame( name = c( "David", "Constance", "Leonie" ),
    gender = c( "Male", "Female", "Female" ),
    height = c(172, 159, 71 ) )
p <- ggplot(dat, aes( x = name, y = height, tooltip = gender,
        data_id = name ) ) +
  geom_bar_interactive(stat = "identity")

x <- girafe(ggobj = p)
if( interactive() ) print(x)

p <- ggplot(diamonds, aes(carat)) +
  geom_histogram_interactive(bins=30, aes(tooltip = ..count..,
                             data_id = carat) )

x <- girafe(ggobj = p)
if( interactive() ) print(x)
}
\seealso{
\code{\link{girafe}}
}
