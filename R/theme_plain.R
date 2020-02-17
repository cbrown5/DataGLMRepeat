#' A plain theme for ggplot2
#'
#' @Usage theme_plain(base_size = 12)
#'
#' @param base_size a \code{numeric} giving basic size of text
#'
#' @return NULL
#' @Details A theme for ggplots that has black and white axes and
#' and no grid lines.
#' @author Christopher J. Brown
#' @rdname theme_plain
#' @export


theme_plain <- function(base_size = 12){
    structure(list(
        theme(
        panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
        strip.text=element_text(hjust=0.05),
        strip.background = element_rect(fill='white', colour = 'white'),
        axis.line = element_line(color = "black"),
        axis.ticks.length = unit(0.25, "cm"),
        plot.background =   element_rect(colour = "white", fill = "white"),
        axis.text = element_text(size = base_size * 0.8 ,
            lineheight = 0.9, colour = "black", vjust = 1, margin = unit(0.1, "lines")),
        panel.background = element_rect(fill = "white", colour = NA)
        )
      ), class = "options")
}
