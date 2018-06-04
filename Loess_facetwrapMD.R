# Regression Analysis Facet_Wrap
# ЧАСТЬ 1: делаем data.frame
	# шаг-1. вчитываем таблицу. делаем из нее датафрейм.
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")
	# шаг-2. чистим датафрейм от NA значений
MDF <- na.omit(MDepths) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) # проверяем, удалил ли все NA
sum(row.has.na) # суммируем все NA, должно получиться: [1] 0
head(MDF) # смотрим очищенный датафрейм. теперь с ним работаем.

# ЧАСТЬ 2: сшиваем все колонки типа "профиль№" в одну группу профилей. 
	# шаг-3. Значение id - номера измерений "observ" (везде одинаково -1:518). При этом вообще не трогаем группу колонок "тектоника" (здесь они не нужны, поэтому игнорируем, он их не будет учитывать)
MarDF_NEW<- melt(MDF, id.vars = c('observ'), measure.vars = c("profile1", "profile2", "profile3", "profile4", "profile5", "profile6", "profile7", "profile8", "profile9", "profile10","profile11", "profile12", "profile13", "profile14", "profile15", "profile16", "profile17", "profile18", "profile19", "profile20","profile21", "profile22", "profile23", "profile24", "profile25"), variable.name ='profiles')
head(MarDF_NEW)

# ЧАСТЬ 3: делаем регрессионный анализ и рисуем его график. (Мат. метод вкл. 3 вида кривой лесса с доверительными интервалами и квантилями()
	# шаг-4. рисуем множественный фасетный график по кривой лесса с доверительными интервалами
Loess_MD <- ggplot(MarDF_NEW, aes(x = observ, y = value, shape = "Observation points", color = "Observation points", size = "Observation points")) + 
	facet_wrap(~ profiles) +
	geom_point(aes(x = observ, y = value), show.legend = FALSE) +
	geom_smooth(aes(x = observ, y = value, colour = "Loess method"), method = loess, se = TRUE, span = .4, size=.2, linetype = "solid", show.legend =  TRUE) +
	geom_smooth(aes(x = observ, y = value, colour = "Glm method"), method = glm, se = TRUE, span = .4, size=.2, linetype = "dotted", show.legend = TRUE) +
	geom_smooth(aes(x = observ, y = value, colour = "Lm method"), method = lm, se = TRUE, size=.2, linetype = "solid", show.legend = TRUE) +
	geom_quantile(aes(x = observ, y = value, colour = "Quantiles"), linetype = "solid", show.legend = TRUE) +
	xlab("Observations") + 
	ylab("Depths, m") +
	scale_color_manual(name = "Legend:", values = c("Observation points" = "seagreen", "Loess method" = "red", "Glm method" = "orange", "Lm method" = "blue", "Quantiles" = "purple")) + # задаем значения цветов элементов
	scale_shape_manual(values = c("Observation points" = 1)) +
	scale_size_manual(values = c("Observation points" = 0.1)) +
	labs(title="马里亚纳海沟。剖面1-25。Mariana Trench, Profiles Nr.1-25.", 
	subtitle = "统计图表。线性回归。Local Polynomial Regression, Confidence Interval, Quantiles \n(LOESS method: locally weighted scatterplot smoothing for non-parametric regression)",
	caption = "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS") +
	theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(family = "Kai", face = "bold", size = 8), # китайский шрифт "Кай"
		plot.subtitle = element_text(family = "Hei", face = "bold", size = 6), # китайский шрифт "Хэй"
		plot.caption = element_text(face = 2, size = 6),
		panel.background=ggplot2::element_rect(fill = "white"),
		legend.justification = "bottom", 
		legend.position = "bottom",
		legend.box.just = "right",
		legend.direction = "horizontal",
		legend.box = "horizontal",
		legend.box.background = element_rect(colour = "white",size=0.2),
		legend.background = element_rect(fill = "white"),
		legend.key.width = unit(.1,"cm"),
		legend.key.height = unit(.1,"cm"),
		legend.spacing.x = unit(.2,"cm"),
		legend.spacing.y = unit(.1,"cm"),
		legend.text = element_text(colour="black", size=6, face=1),
		legend.title = element_text(colour="black", size=6, face=1),
		strip.text.x = element_text(colour = "white", size=6, face=1),
		panel.grid.major = element_line("gray24", size = 0.1, linetype = "solid"),
		panel.grid.minor = element_line("gray24", size = 0.1, linetype = "dotted"),
		axis.text.x = element_text(face = 3, color = "gray24",size = 6, angle = 15),
		axis.text.y = element_text(face = 3, color = "gray24",size = 6, angle = 15),
		axis.ticks.length=unit(.1,"cm"),
		axis.line = element_line(size = .3, colour = "grey80"),
		axis.title.y = element_text(margin = margin(t = 20, r = .3), face = 2, size = 8),
		axis.title.x = element_text(face = 2, size = 8, margin = margin(t = .2)))
#	guides(col = guide_legend(nrow = 2, ncol = 1, byrow = TRUE)) # подправляем дизайн легенды.
Loess_MD 
