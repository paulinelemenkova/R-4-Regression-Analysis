# QQ t.test
# шаг-1. вчитываем таблицу. делаем из нее датафрейм. говорим сколько колонок и строк.
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")
#MDepth<- data.frame(MDepths, nrow = 1008, ncol = 25)

# шаг-2. чистим датафрейм от NA значений
df<- na.omit(MDepths) 
row.has.na <- apply(df, 1, function(x){any(is.na(x))}) # проверяем, удалил ли все NA
sum(row.has.na) # суммируем все NA, должно получиться: [1] 0
head(df) # смотрим очищенный датафрейм. теперь с ним работаем.

#шаг-3. создаем ячейки датафрейма, вчитываем туда столбцы-колонки из таблицы
nr_point<- as.numeric(df$observ)
profile1 <- as.numeric(df$profile1)
profile2 <- as.numeric(df$profile2)
profile3 <- as.numeric(df$profile3)
profile4 <- as.numeric(df$profile4)
profile5 <- as.numeric(df$profile5)
profile6 <- as.numeric(df$profile6)
profile7 <- as.numeric(df$profile7)
profile8 <- as.numeric(df$profile8)
profile9 <- as.numeric(df$profile9)
profile10 <- as.numeric(df$profile10)
profile11 <- as.numeric(df$profile11)
profile12 <- as.numeric(df$profile12)
profile13 <- as.numeric(df$profile13)
profile14 <- as.numeric(df$profile14)
profile15 <- as.numeric(df$profile15)
profile16 <- as.numeric(df$profile16)
profile17 <- as.numeric(df$profile17)
profile18 <- as.numeric(df$profile18)
profile19 <- as.numeric(df$profile19)
profile20 <- as.numeric(df$profile20)
profile21 <- as.numeric(df$profile21)
profile22 <- as.numeric(df$profile22)
profile23 <- as.numeric(df$profile23)
profile24 <- as.numeric(df$profile24)
profile25 <- as.numeric(df$profile25)

# шаг-4. формируем датафрейм
MDF<- data.frame(nr_point, profile1, profile2, profile3, profile4, profile5, profile6, profile7, profile8, profile9, profile10, profile11, profile12, profile13, profile14, profile15, profile16, profile17, profile18, profile19, profile20, profile21, profile22, profile23, profile24, profile25)
head(MDF)
 
# шаг-6. рисуем один график (здесь: профиль №18) по кривой лесса с доверительными интервалами и квантилями
Loess_profile18 <- ggplot(MDF, aes(x = nr_point, y = profile18)) +
	geom_point(aes(x = nr_point, y = profile18, colour = "Samples", shape = "Samples"), show.legend=TRUE) +
	geom_smooth(aes(x = nr_point, y = profile18, colour = "Loess method"), method = loess, se = TRUE, span = .4, size=.3, linetype = "solid", show.legend=TRUE) +
	geom_smooth(aes(x = nr_point, y = profile18, colour = "Glm method"), method = glm, se = FALSE, span = .4, size=.4, linetype = "dotted", show.legend=TRUE) +
	geom_smooth(aes(x = nr_point, y = profile18, colour = "Lm method"), method = lm, se = TRUE, size=.3, linetype = "solid", show.legend=TRUE) +
	geom_quantile(aes(x = nr_point, y = profile18, colour = "Quantiles"), linetype = "solid", show.legend=TRUE) +
	xlab("Observations") + 
	ylab("Depths, m") +
	scale_color_manual(values = c("Samples" = "seagreen", "Loess method" = "red", "Glm method" = "orange", "Lm method" = "blue", "Quantiles" = "purple")) + # задаем значения цветов элементов
	scale_shape_manual(values = c("Samples" = 1)) + # задаем значения формы точки (здесь: №1 - "прозрачный кружок")
#	scale_size_manual(values = c(profile11 = 1)) +
	labs(title="马里亚纳海沟。剖面18。Mariana Trench, Profile Nr.18.", 
	subtitle = "统计图表。线性回归。Local Polynomial Regression, \nConfidence Interval, Quantiles \n(LOESS method: locally weighted scatterplot \nsmoothing for non-parametric regression)",
	caption =  "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS") +
	theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(family = "Kai", face = 2, size = 8),
		plot.subtitle = element_text(family = "Hei", face = 1, size = 6),
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
Loess_profile18
	
ggsave("Loess_Profile18.pdf", device = cairo_pdf, fallback_resolution = 300)