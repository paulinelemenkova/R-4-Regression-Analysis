theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(family = "Times New Roman", face = 2, size = 10),
		panel.background=ggplot2::element_rect(fill = "white"),
		legend.justification = "bottom", 
		legend.position = "bottom",
		legend.box.just = "right",
		legend.direction = "horizontal",
		legend.box = "horizontal",
		legend.box.background = element_rect(colour = "honeydew4",size=0.2),
		legend.background = element_rect(fill = "white"),
		legend.key.width = unit(.1,"cm"),
		legend.key.height = unit(.1,"cm"),
		legend.spacing.x = unit(.2,"cm"),
		legend.spacing.y = unit(.1,"cm"),
		legend.text = element_text(family = "Arial", colour="black", size=6, face=1),
		legend.title = element_text(family = "Arial", colour="black", size=6, face=1),
		strip.text.x = element_text(colour = "white", family = "Arial", size=6, face=1),
		panel.grid.major = element_line("gray24", size = 0.1, linetype = "solid"),
		panel.grid.minor = element_line("gray24", size = 0.1, linetype = "dotted"),
		axis.text.x = element_text(family = "Arial", face = 3, color = "gray24",size = 5, angle = 15),
		axis.text.y = element_text(family = "Arial", face = 3, color = "gray24",size = 4, angle = 15),
		axis.ticks.length=unit(.1,"cm"),
		axis.line = element_line(size = .3, colour = "grey80"),
		axis.title.y = element_text(margin = margin(t = 20, r = .3), family = "Times New Roman", face = 2, size = 8),
		axis.title.x = element_text(family = "Times New Roman", face = 2, size = 8, margin = margin(t = .2))) +
	guides(col = guide_legend(nrow = 2, ncol = 3, byrow = TRUE)) # вытягиваем легенду вниз по вертикали. 
	
ggsave("My_plot_R.pdf", device = cairo_pdf, fallback_resolution = 300)
#A4
ggsave("figure.pdf", device = cairo_pdf, fallback_resolution = 300, width = 210, height = 297, units = "mm")