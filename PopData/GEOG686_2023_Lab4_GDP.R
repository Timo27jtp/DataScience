

# Open libraries for the assignment
library(dplyr)
library(ggplot2)
library(corrplot)
library(psych)
library(car)
library(broom)
library(foreign)
library(tidyr)
library(forecast)
library(tseries)


rm(list = ls())


setwd("E:/University of Calgary/M.eng Geomatics/GEOG686_GeoStats/Lab4") # Update this path yourself

options(digits = 9)

data <- read.csv("AhPopData.csv")
attach(data)

names(data)


pdf("LineGraph_D0GDP.pdf")
ggplot(data, aes(x = YEAR, y = AbGDP, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) + # Add points in color red with size 2 (optional)
  ggtitle("GDP in Alberta from 1962 to 1994") + 
  xlab("Year") +
  ylab("GDP (CAD)") +
  scale_x_continuous(breaks = seq(1962, 1994, by = 1)) +
  scale_y_continuous(breaks = seq(0, 80000, by = 5000)) +
  coord_cartesian(xlim = c(1962, 1994), ylim = c(0, 80000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()

# ++++++++++++++++++++++++++++++


# Dicky-Fuller Test
adf.test(AbGDP)
# ++++++++++++++++++++++++++++++



# 1st Derivative
d1GDP <- diff(AbGDP, differences = 1)

# Visualisze the differenced data by line graph
dataD1 <- data[-1, ]
dataD1$d1GDP <- d1GDP 

summary(dataD1$d1GDP)
mean(dataD1$d1GDP)
var(dataD1$d1GDP)

pdf("LineGraph_D1GDP.pdf")
ggplot(dataD1, aes(x = YEAR, y = d1GDP, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) + 
  ggtitle("1st derivative of Alberta GDP") + 
  xlab("Year") +
  ylab("Differenced GDP (CAD)") +
  scale_x_continuous(breaks = seq(1963, 1994, by = 1)) +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000)) +
  coord_cartesian(xlim = c(1963, 1994), ylim = c(-10000, 10000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()

#barplot
ggplot(dataD1, aes(x = YEAR, y = d1GDP)) +
  geom_bar(stat = "identity", color = "blue", fill = "yellow") +
  labs(title = "Distribution of Daily Mean Temperature",
       x = "Day of the Month",
       y = "Temperature (Degrees Celsius)") +
  scale_x_continuous(breaks = seq(0, 30, by = 1)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5))


# 2nd Derivation
d2GDP <- diff(d1GDP, differences = 1)
dataD2 <- data[-c(1, 2), ] 
dataD2$d2GDP <- d2GDP

summary(dataD2$d2GDP)
mean(dataD2$d2GDP)
var(dataD2$d2GDP)

#Plot
pdf("LineGraph_D2GDP.pdf")
ggplot(dataD2, aes(x = YEAR, y = d2GDP, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) + 
  ggtitle("2nd derivative of Alberta GDP") + 
  xlab("Year") +
  ylab("Differenced GDP (CAD)") +
  scale_x_continuous(breaks = seq(1963, 1994, by = 1)) +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000)) +
  coord_cartesian(xlim = c(1963, 1994), ylim = c(-12000, 10000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()

#barplot
ggplot(dataD2, aes(x = YEAR, y = d2GDP)) +
  geom_bar(stat = "identity", color = "blue", fill = "yellow") +
  labs(title = "Distribution of Daily Mean Temperature",
       x = "Day of the Month",
       y = "Temperature (Degrees Celsius)") +
  scale_x_continuous(breaks = seq(0, 30, by = 1)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5))

#3rd derivative
d3GDP <- diff(d2GDP, differences = 1)
dataD3 <- data[-c(1, 2, 3), ] 
dataD3$d3GDP <- d3GDP

summary(dataD3$d3GDP)
mean(dataD3$d3GDP)
var(dataD3$d3GDP)

#Plot
pdf("LineGraph_D3GDP.pdf")
ggplot(dataD3, aes(x = YEAR, y = d3GDP, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) +
  ggtitle("3rd derivative of Alberta GDP") + 
  xlab("Year") +
  ylab("Differenced GDP (CAD)") +
  scale_x_continuous(breaks = seq(1963, 1994, by = 1)) +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000)) +
  coord_cartesian(xlim = c(1963, 1994), ylim = c(-15000, 25000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()

# ...
# 4th Derivation
d4GDP <- diff(d3GDP, differences = 1)
dataD4 <- data[-c(1, 2, 3, 4), ] 
dataD4$d4GDP <- d4GDP

summary(dataD4$d4GDP)
mean(dataD4$d4GDP)
var(dataD4$d4GDP)

#Plot
pdf("LineGraph_D4GDP.pdf")
ggplot(dataD4, aes(x = YEAR, y = d4GDP, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) +
  ggtitle("4th derivative of Alberta GDP") + 
  xlab("Year") +
  ylab("Differenced GDP (CAD)") +
  scale_x_continuous(breaks = seq(1963, 1994, by = 1)) +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000)) +
  coord_cartesian(xlim = c(1963, 1994), ylim = c(-10000, 10000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()

# 5th Derivation
d5GDP <- diff(d4GDP, differences = 1)
dataD5 <- data[-c(1, 2, 3, 4, 5), ] 
dataD5$d5GDP <- d5GDP

summary(dataD5$d5GDP)
mean(dataD5$d5GDP)
var(dataD5$d5GDP)

#Plot
pdf("LineGraph_D5GDP.pdf")
ggplot(dataD5, aes(x = YEAR, y = d5GDP, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) + 
  ggtitle("5th derivative of Alberta GDP") + 
  xlab("Year") +
  ylab("Differenced GDP (CAD)") +
  scale_x_continuous(breaks = seq(1963, 1994, by = 1)) +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000)) +
  coord_cartesian(xlim = c(1963, 1994), ylim = c(-10000, 10000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()


# 6th Derivation
d6GDP <- diff(d5GDP, differences = 1)
dataD6 <- data[-c(1, 2, 3, 4, 5, 6), ] 
dataD6$d6GDP <- d6GDP

summary(dataD6$d6GDP)
mean(dataD6$d6GDP)
var(dataD6$d6GDP)

#Plot
pdf("LineGraph_D6GDP.pdf")
ggplot(dataD6, aes(x = YEAR, y = d6GDP, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) + 
  ggtitle("6th derivative of Alberta GDP") + 
  xlab("Year") +
  ylab("Differenced GDP (CAD)") +
  scale_x_continuous(breaks = seq(1963, 1994, by = 1)) +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1000)) +
  coord_cartesian(xlim = c(1963, 1994), ylim = c(-10000, 10000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()



#=================================================================================
# Autocorrelation function plot
pdf("ACF_d1GDP.pdf")
acfD1 <- acf(data = dataD1, d1GDP) 
dev.off()
acfD1 


pdf("PACF_d1GDP.pdf")
pacfD1 <- pacf(data = dataD1, d1GDP) 
dev.off()
pacfD1 # Display the result in console window


acfD1Val <- t(data.frame(acfD1$acf))
pacfD1Val <- t(data.frame(pacfD1$acf)) 

write.csv(acfD1Val, "ACF_D1.csv")
write.csv(pacfD1Val, "PACF_D1.csv")



pdf("ACF_d2GDP.pdf")
acfD2 <- acf(data = dataD2, d2GDP) 
dev.off()
acfD2 

# Partial autocorrelation function plot
pdf("PACF_d2GDP.pdf")
pacfD2 <- pacf(data = dataD2, d2GDP) 
dev.off()
pacfD2 


acfD2Val <- t(data.frame(acfD2$acf))
pacfD2Val <- t(data.frame(pacfD2$acf)) 

write.csv(acfD2Val, "ACF_D2.csv")
write.csv(pacfD2Val, "PACF_D2.csv")


pdf("ACF_d3GDP.pdf")
acfD3 <- acf(data = dataD3, d3GDP) 
dev.off()
acfD3 

# Partial autocorrelation function plot
pdf("PACF_d3GDP.pdf")
pacfD3 <- pacf(data = dataD3, d3GDP) 
dev.off()
pacfD3 

# Put ACF and PACF result into data frame so you can export the result to a CSV file
acfD3Val <- t(data.frame(acfD3$acf))
pacfD3Val <- t(data.frame(pacfD3$acf)) 

write.csv(acfD3Val, "ACF_D3.csv")
write.csv(pacfD3Val, "PACF_D3.csv")

pdf("ACF_d4GDP.pdf")
acfD4 <- acf(data = dataD4, d4GDP) 
dev.off()
acfD4 

# Partial autocorrelation function plot
pdf("PACF_d4GDP.pdf")
pacfD4 <- pacf(data = dataD4, d4GDP) 
dev.off()
pacfD4 

acfD4Val <- t(data.frame(acfD4$acf))
pacfD4Val <- t(data.frame(pacfD4$acf)) 

write.csv(acfD3Val, "ACF_D4.csv")
write.csv(pacfD3Val, "PACF_D4.csv")

# ================================================================================
# ARIMA model
# AR model with lag 1 and 1 differencing
ar_1_1_0 <- arima(AbGDP, order = c(1, 1, 0))
ar_1_1_0

ar2_1_2_0 <- arima(d2GDP, order = c(1, 2, 0))
ar2_1_2_0

ar3_1_3_0 <- arima(d3GDP, order = c(1, 3, 0))
ar3_1_3_0

ar4_1_4_0 <- arima(d4GDP, order = c(1, 4, 0))
ar4_1_4_0
# ar_2_1_0
ar_2_1_0 <- arima(d1GDP, order = c(2, 1, 0))
ar_2_1_0

ar2_2_2_0 <- arima(d2GDP, order = c(2, 2, 0))
ar2_2_2_0

ar3_2_3_0 <- arima(d3GDP, order = c(2, 3, 0))
ar3_2_3_0

ar4_2_4_0 <- arima(d4GDP, order = c(2, 4, 0))
ar4_2_4_0

# ...
# ar_3_1_0
ar_3_1_0 <- arima(d1GDP, order = c(3, 1, 0))
ar_3_1_0

ar2_3_2_0 <- arima(d2GDP, order = c(3, 2, 0))
ar2_3_2_0

ar3_3_3_0 <- arima(d3GDP, order = c(3, 3, 0))
ar3_3_3_0

ar4_3_4_0 <- arima(d4GDP, order = c(3, 4, 0))
ar4_3_4_0


# ================================================================================

