#' This function adds USAID branding to a ggplot.
#'
#' @import ggplot2
#'
#' @export
usaid_plot <- function(){
  requireNamespace(extrafont, quietly=TRUE)
  requireNamespace(extrafontdb, quietly=TRUE)
  requireNamespace(ggplot2, quietly=TRUE)
  list(
    ggplot2::theme(legend.position = "NA"
                   , legend.background = ggplot2::element_blank()
                   , legend.title = ggplot2::element_blank()
                   , legend.key = ggplot2::element_blank()
                   , axis.ticks = ggplot2::element_blank()
                   , axis.line = ggplot2::element_blank()
                   , panel.grid.minor = ggplot2::element_blank()
                   , panel.grid.major.y = ggplot2::element_line(color = "#6C6463")
                   , panel.grid.major.x = ggplot2::element_line(color = "#6C6463")
                   , panel.background = ggplot2::element_rect(fill="#CFCDC9")
                   , strip.background = ggplot2::element_rect(fill = "#CFCDC9")
                   , plot.background = ggplot2::element_rect(fill = "#CFCDC9")
                   , plot.title.position = "plot"
                   , plot.title = element_text(size = 28, family = "Gill Sans MT", color = "black")
                   , plot.subtitle = element_text(size = 20, family = "Gill Sans MT", color = "black")
                   , plot.caption = element_text(size = 16, family = "Gill Sans MT", color = "black")
                   , text=element_text(size = 22, family = "Gill Sans MT", color = "black")
    )
    , ggplot2::scale_fill_manual(values = rep(c("#002F6C", "#BA0C2F", "#0067B9", "#6C6463", "#651D32", "#A7C6ED", "#8C8985"), 500))
    , ggplot2::scale_color_manual(values = rep(c("#002F6C", "#BA0C2F", "#0067B9", "#6C6463", "#651D32", "#A7C6ED", "#8C8985"), 500))
  )
}
