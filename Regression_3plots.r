# совемещаем три графика. Располагаем внизу совместную легенду.
library(ggpubr)
#ggarrange function

Regression_Profiles111824 <- ggarrange(Loess_profile11, Loess_profile18, Loess_profile24, labels = c("1", "2", "3"), ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")


