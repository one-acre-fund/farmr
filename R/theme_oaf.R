#' Overlay ggplot graph with 1AF theme
#'
#' Overlays ggplot objects with 1AF branded theme.
#' Please note that group colors only support categorical variables. This
#' should be enough for almost all needs, but if not refer to
#' \code{\link[=ggplot2]{ggplot2::scale_colour_gradient}}.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour = factor(gear))) + facet_wrap(~am)
#' p + theme_oaf() adding in 1AF theme
#' @export

### bbc example https://github.com/bbc/bbplot/blob/master/R/bbc_style.R

## future versions can also add a continuous/manual distinction

theme_oaf <- function(base_size = 11, base_family = "Calibri") {
  list(
    ggplot2::theme_gray(base_size = base_size, base_family = base_family)
    # overriding base theme
    %+replace%
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold",
          margin = unit(c(0, 0, .5, 0), "lines")),
        plot.margin = unit(c(1, 2, 1, 2), "lines"),
        plot.background = ggplot2::element_rect(fill="#f2f2f2"),
        panel.background = ggplot2::element_rect(fill = "#f2f2f2", color=0),
        panel.grid = ggplot2::element_line(size=0, color="#f2f2f2"),
        panel.grid.minor.y = ggplot2::element_line(size=0),
        panel.grid.major.y = ggplot2::element_line(colour = "#e7e7e7",
          size = 1),
        panel.grid.minor.x = ggplot2::element_line(size=0),
        panel.grid.major.x = ggplot2::element_line(size=0, color="#f2f2f2"),
        axis.ticks = ggplot2::element_line(size=0, color="#f2f2f2"),
        axis.line = ggplot2::element_line(size=0, color="#f2f2f2"),
        panel.border = ggplot2::element_rect(fill = NA, color = "#f2f2f2"),
        panel.spacing = unit(1, "lines"),
        legend.key = ggplot2::element_rect(fill = NA, color = "#f2f2f2"),
        legend.background = ggplot2::element_rect(fill = NA,
          color = "#f2f2f2"),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 0)
      ),
    ggplot2::scale_fill_manual(values=c("#929948", "#3D6066", "#bf5138", "#6b7150", "#ebbd5e", "#595959", "#E57838", "#e2ab21",
                               "#E33227", "#D0A44B", "#ED811E", "#E2AB21", "#CDC45D", "#C3C082", "#E57838", "#E55E29")),
    ggplot2::scale_color_manual(values=c("#929948", "#3D6066", "#bf5138", "#6b7150", "#ebbd5e", "#595959", "#E57838", "#e2ab21",
                                "#E33227", "#D0A44B", "#ED811E", "#E2AB21", "#CDC45D", "#C3C082", "#E57838", "#E55E29"))
  )
}
