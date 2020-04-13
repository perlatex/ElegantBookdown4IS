# https://github.com/kjhealy/myriad/blob/master/R/myriad.r
theme_mystyle <- function() {
	ret <- ggplot2::theme_minimal()
	
	ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
	ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = "gray90", size = 0.10))
	ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "gray90", size = 0.1))
	ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = "gray90", size = 0.1))
	
	ret <- ret + ggplot2::theme(panel.spacing.x = grid::unit(2, "lines"))
	ret <- ret + ggplot2::theme(panel.spacing.y = grid::unit(2, "lines"))
	
	
	ret <- ret + ggplot2::theme(plot.margin = grid::unit(c(5.5,12,5.5,5.5), "pt"))
	ret <- ret + ggplot2::theme(panel.spacing = ggplot2::unit(0.5, "lines"))
	
	ret
}