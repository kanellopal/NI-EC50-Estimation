#### Package Load ----
library(readxl)
library(ggplot2)
library(dplyr)
library(sicegar)
library(gridExtra)
library(SciViews)
library(patchwork)
library(rstatix)
library(agricolae)
library(viridis)
library(scales)
library(writexl)

#### Insert your data ---- 
data <- read.table("mat.txt", header = T, sep = "", dec = ".") 
doses <- as.numeric(levels(as.factor(data$Dose)))

#### Data Processing ----
a_control <- coef(lm(Nitrite~Time, filter(data, Dose == doses[1])))["Time"]
a_NI_1 <- coef(lm(Nitrite~Time, filter(data, Dose == doses[2])))["Time"]
a_NI_2 <- coef(lm(Nitrite~Time, filter(data, Dose == doses[3])))["Time"]
a_NI_3 <- coef(lm(Nitrite~Time, filter(data, Dose == doses[4])))["Time"]
a_NI_4 <- coef(lm(Nitrite~Time, filter(data, Dose == doses[5])))["Time"]

AOI_1 <- (a_control - a_NI_1) * 100 / a_control
AOI_2 <- (a_control - a_NI_2) * 100 / a_control
AOI_3 <- (a_control - a_NI_3) * 100 / a_control
AOI_4 <- (a_control - a_NI_4) * 100 / a_control

AOIs <- c(0, AOI_1,AOI_2,AOI_3,AOI_4)

#### Visualisation ---- 
legend_order <- doses
adj_r_squared_values <- paste("The adjusted R-squared values are", "Control:", round(summary(lm(Nitrite~Time, filter(data, Dose == doses[1])))$adj.r.squared,4), "Dose 1:", round(summary(lm(Nitrite~Time, filter(data, Dose == doses[2])))$adj.r.squared,4), "Dose 2:", round(summary(lm(Nitrite~Time, filter(data, Dose == doses[3])))$adj.r.squared,4), "Dose 3:", round(summary(lm(Nitrite~Time, filter(data, Dose == doses[4])))$adj.r.squared,4), "Dose 4:", round(summary(lm(Nitrite~Time, filter(data, Dose == doses[5])))$adj.r.squared,4))
adj_r_squared_values
linear_regression_plot <- ggplot(data, aes(x = Time, y = Nitrite, colour = factor(Dose))) + theme_light() + labs("Linear Regression Plot", x = "Time (h)", y = "[Nitrite] (μM)", colour = "Dose") + theme(axis.title = element_text(size = 28), axis.text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1, size = 16), legend.text = element_text(size = 20), legend.title = element_text(size = 20), legend.position = "bottom",) + scale_y_continuous(breaks = seq(0,1000, by = 200)) + ylim(0,1000) + geom_smooth(aes(x = Time, y = Nitrite, colour = factor(Dose)), method = lm, linetype = "dashed", linewidth = 0.6, se = F) + annotate("text", x = Inf, y = -Inf, hjust = 0, vjust = 0, label = adj_r_squared_values)
print(linear_regression_plot)


#### EC50 calculation ---- 
if (summary(lm(AOIs~as.numeric(doses)))$adj.r.squared > 0.9) {
  AOIs <- AOIs
  linear_regression <- lm(AOIs~as.numeric(doses))
  linear_regression
  plot_AOI_vs_dose <- plot(doses,AOIs, type = "o", ylab = "Ammonia Oxidation Inhibition %", xlab = "Dose") 
  abline(lm(AOIs~as.numeric(doses)), col = "navyblue")
  text(0,60,paste("EC50: ",round((50 - coef(linear_regression)["(Intercept)"])/coef(linear_regression)["as.numeric(doses)"], 2)),pos=4)
  text(0,50,paste("Adjsuted R - squared:",round(summary(lm(AOIs~as.numeric(doses)))$adj.r.squared,4)),pos=4)
  text(0,40,paste("p-value:", round(summary(linear_regression)$coefficients[2,4],4)),pos=4)
EC50 <- round((50 - coef(linear_regression)["(Intercept)"])/coef(linear_regression)["as.numeric(doses)"], 2)
EC50
} else {
doses <- doses[1:4]
AOIs <- AOIs[1:4]
linear_regression <- lm(AOIs~as.numeric(doses))
linear_regression
plot_AOI_vs_dose <- plot(doses,AOIs, type = "o", ylab = "Ammonia Oxidation Inhibition %", xlab = "Dose") 
abline(lm(AOIs~as.numeric(doses)), col = "navyblue")
text(0,60,paste("EC50: ",round((50 - coef(linear_regression)["(Intercept)"])/coef(linear_regression)["as.numeric(doses)"], 2)),pos=4)
text(0,50,paste("Adjsuted R - squared:",round(summary(lm(AOIs~as.numeric(doses)))$adj.r.squared,4)),pos=4)
text(0,40,paste("p-value:", round(summary(linear_regression)$coefficients[2,4],4)),pos=4)
EC50 <- round((50 - coef(linear_regression)["(Intercept)"])/coef(linear_regression)["as.numeric(doses)"], 2)
EC50
}

#### Export the report files ---- 
cairo_pdf("model_output.pdf", height = 6, width = 6, onefile = TRUE)
plot_AOI_vs_dose <- plot(doses,AOIs, type = "o", ylab = "Ammonia Oxidation Inhibition %", xlab = "Dose") 
abline(lm(AOIs~as.numeric(doses)), col = "navyblue")
text(0,60,paste("EC50: ",round((50 - coef(linear_regression)["(Intercept)"])/coef(linear_regression)["as.numeric(doses)"], 2)),pos=4)
text(0,50,paste("Adjsuted R - squared:",round(summary(lm(AOIs~as.numeric(doses)))$adj.r.squared,4)),pos=4)
text(0,40,paste("p-value:", round(summary(linear_regression)$coefficients[2,4],4)),pos=4)
dev.off()

cairo_pdf("linear_regressions_plot_2.pdf", height = 6, width = 6, onefile = TRUE)
print(linear_regression_plot)
plot.new()
text(0.5,0.5, adj_r_squared_values,col="blue")
dev.off()
