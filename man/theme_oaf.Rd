% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_oaf.R
\name{theme_oaf}
\alias{theme_oaf}
\title{Overlay ggplot graph with 1AF theme}
\usage{
theme_oaf(base_size = 11, base_family = "Calibri")
}
\description{
Overlays ggplot objects with 1AF branded theme.
Please note that group colors only support categorical variables. This
should be enough for almost all needs, but if not refer to
\code{\link[=ggplot2]{ggplot2::scale_colour_gradient}}.
}
\examples{
p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
colour = factor(gear))) + facet_wrap(~am)
p + theme_oaf() adding in 1AF theme
}
