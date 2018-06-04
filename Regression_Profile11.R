# Regression Analysis
# ЧАСТЬ 1: делаем data.frame
	# шаг-1. вчитываем таблицу. делаем из нее датафрейм.
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")

	# шаг-2. чистим датафрейм от NA значений
MDF <- na.omit(MDepths) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) # проверяем, удалил ли все NA
sum(row.has.na) # суммируем все NA, должно получиться: [1] 0
head(MDF) # смотрим очищенный датафрейм. теперь с ним работаем.
 
# ЧАСТЬ 2: делаем регрессионный анализ и рисуем его график. (Мат. метод вкл. 3 вида кривой лесса с доверительными интервалами и квантилями).
	# шаг-3. (здесь: профиль №11) 
Loess_profile11 <- ggplot(MDF, aes(x = observ, y = profile11)) +
	geom_point(aes(x = observ, y = profile11, colour = "Samples", shape = "Samples"), show.legend=TRUE) +
	geom_smooth(aes(x = observ, y = profile11, colour = "Loess method"), method = loess, se = TRUE, span = .4, size=.3, linetype = "solid", show.legend=TRUE) +
	geom_smooth(aes(x = observ, y = profile11, colour = "Glm method"), method = glm, se = FALSE, span = .4, size=.4, linetype = "dotted", show.legend=TRUE) +
	geom_smooth(aes(x = observ, y = profile11, colour = "Lm method"), method = lm, se = TRUE, size=.3, linetype = "solid", show.legend=TRUE) +
	geom_quantile(aes(x = observ, y = profile11, colour = "Quantiles"), linetype = "solid", show.legend=TRUE) +
	xlab("Observations") + 
	ylab("Depths, m") +
	scale_color_manual(values = c("Samples" = "seagreen", "Loess method" = "red", "Lm method" = "blue", "Glm method" = "orange", "Quantiles" = "purple")) + # задаем значения цветов элементов
	scale_shape_manual(values = c("Samples" = 1)) + # задаем значения формы точки (здесь: №1 - "прозрачный кружок")
#	scale_size_manual(values = c(profile11 = 1)) +
	labs(title="马里亚纳海沟。剖面11。Mariana Trench, Profile 11.", 
	subtitle = "统计图表。线性回归。Local Polynomial Regression, \nConfidence Interval, Quantiles \n(LOESS method: locally weighted scatterplot \nsmoothing for non-parametric regression)",
	caption = "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS") +
	theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(family = "Kai", face = "bold", size = 8),
		plot.subtitle = element_text(family = "Hei", face = "bold", size = 6),
		plot.caption = element_text(face = 2, size = 6),
		panel.background=ggplot2::element_rect(fill = "white"),
		legend.justification = "bottom", 
		legend.position = "bottom",
		legend.box.just = "right",
		legend.direction = "horizontal",
		legend.box = "horizontal",
		legend.box.background = element_rect(colour = "honeydew4",size=0.2),
		legend.background = element_rect(fill = "white"),
		legend.key.width = unit(1,"cm"),
		legend.key.height = unit(.5,"cm"),
		legend.spacing.x = unit(.2,"cm"),
		legend.spacing.y = unit(.1,"cm"),
		legend.text = element_text(colour="black", size=6, face=1),
		legend.title = element_text(colour="black", size=6, face=1),
		strip.text.x = element_text(colour = "white", size=6, face=1),
		panel.grid.major = element_line("gray24", size = 0.1, linetype = "solid"),
		panel.grid.minor = element_line("gray24", size = 0.1, linetype = "dotted"),
		axis.text.x = element_text(face = 3, color = "gray24", size = 6, angle = 15),
		axis.text.y = element_text(face = 3, color = "gray24", size = 6, angle = 15),
		axis.ticks.length=unit(.1,"cm"),
		axis.line = element_line(size = .3, colour = "grey80"),
		axis.title.y = element_text(margin = margin(t = 20, r = .3), face = 2, size = 8),
		axis.title.x = element_text(face = 2, size = 8, margin = margin(t = .2))) +
		guides(col = guide_legend(nrow = 1, ncol = 6, byrow = TRUE)) # подправляем дизайн легенды.
Loess_profile11