#++++USED CAR PRICE ANALYSIS USING MULTIVARIATE REGRESSION

#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("psych")
#install.packages("car")
#install.packages("broom")
#install.packages("foreign")
#install.packages("readxl")
#install.packages("e1071")
#install.packages("MASS")
#install.packages("moments")


#open library
library(dplyr)
library(ggplot2)
library(corrplot)
library(psych)
library(car)
library(broom)
library(foreign)
library(readxl)
library(e1071)
library(stats)
library(MASS)


rm(list =ls())

#Working directory
setwd("E:/University of Calgary/M.eng Geomatics/GEOG686_GeoStats/Project/CarPrice")

#Numeric digits to work with
options(digits = 9)

#Reading Data
data <-  read.csv("scrap_Carprice_corrected.csv")
attach(data)
names(data)

#Checking for missing values
missing_counts <- colSums(is.na(data))
print(missing_counts)


#Checking for duplicate values
#data <- subset(data, select = -c(X, ID))
duplicate <- duplicated(data)
#Unique names
unique_name <- unique(data$name)
#Organising the names
data <- data %>%
  mutate(Car_name = sub("^\\s*(\\S+).*", "\\1", name))
replacements <- c("maxda" = "mazda", "nissan" = "Nissan", "porcshce" = "porsche", "toyouta" = "toyota", "vokswagen" = "volkswagen", "vw" = "volkswagen")
data <- data %>%
  mutate(name = ifelse(name %in% names(replacements), replacements[name], name))
data<- subset(data, select = - c(name))
#Data mutation
data_clean <- data %>%
  mutate(avg_mpg = data$citympg+data$highwaympg/2)
data <- data_clean 
rm(data_clean) 
attach(data)
data <- subset(data, select = -c(X, ID, symboling, enginelocation, enginetype, fuelsystem, citympg, highwaympg))
data$carbody <- as.numeric(data$carbody)
data$cylindernumber <- as.numeric(data$cylindernumber)


new_order <- c("Car_name", "fueltypes", "aspiration", "doornumbers", "carbody", "drivewheels",
               "cylindernumber", "wheelbase", "carlength", "carwidth", "carheight", "curbweight", 
               "enginesize", "boreratio", "stroke", "compressionratio", "horsepower", "peakrpm",
               "avg_mpg","price")
data <- data[new_order]
data <- data %>%
  select(new_order)

write.csv(data, "scrap_Carprice_corrected_2.csv")

columns_to_normalize <- 2:20
data_log1 <- data
data_log1[, columns_to_normalize] <- log(data[, columns_to_normalize])
data_log2 <- data_log1 
data_log2[, columns_to_normalize] <- log(data_log1[, columns_to_normalize])

write.csv(data_log2, "scrap_Carprice_normalised.csv")

desc_stats1 <- summary(data_log2)

#Taking summary for individual variables
summary(data_log2$price)
mean(data_log2$price)
var(data_log2$price)

desc_stats2 <- describe(data_log2)


#Writing the data
write.csv(desc_stats1, "CarPrice_Descriptive1.csv")
write.csv(desc_stats2, "CarPrice_Descriptive2.csv")

#===============================================================================
#normality

shapiro.test(data_log2$price) 
shapiro.test(data_log2$horsepower)
shapiro.test(data_log2$avg_mpg)
shapiro.test(data_log2$enginesize)
shapiro.test(data_log2$curbweight)


#===============================================================================
# Visualizing data
#===============================================================================
#Distribution

# Define the layout for the 3x3 grid
par(mfrow = c(3, 3))

# Create histograms for each column with adjusted margin settings
par(mar = c(4, 4, 2, 1))

hist(data_log2$wheelbase, main = "Histogram wheelbase", xlab = "Value", ylab = "Frequency", cex.main = 1.5, cex.lab = 1.4)

hist(data_log2$carlength, main = "Histogram carlength", xlab = "Value", ylab = "Frequency", cex.main = 1.5, cex.lab = 1.4)

hist(data_log2$carwidth, main = "Histogram carwidth", xlab = "Value", ylab = "Frequency", cex.main = 1.5, cex.lab = 1.4)

hist(data_log2$curbweight, main = "Histogram curbweight", xlab = "Value", ylab = "Frequency", cex.main = 1.5, cex.lab = 1.4)

hist(data_log2$boreratio, main = "Histogram boreratio", xlab = "Value", ylab = "Frequency", cex.main = 1.5, cex.lab = 1.4)

hist(data_log2$horsepower, main = "Histogram horsepower", xlab = "Value", ylab = "Frequency", cex.main = 1.5, cex.lab = 1.4)

hist(data_log2$enginesize, main = "Histogram enginesize", xlab = "Value", ylab = "Frequency", cex.main = 1.5, cex.lab = 1.4)

hist(data_log2$avg_mpg, main = "Histogram Average MPG", xlab = "Value", ylab = "Frequency", cex.main = 1.4, cex.lab = 1.4)

hist(data_log2$price, main = "Histogram Price", xlab = "Value", ylab = "Frequency", cex.main = 1.5, cex.lab = 1.4)

par(mfrow = c(1, 1))


pdf("Histogram_price.pdf")
ggplot(data, aes(x = data_log2$price)) +
  geom_histogram(
    binwidth = 0.05,  
    fill = "lightblue",  
    color = "black",    
    alpha = 1         
  ) +
  labs(
    title = "Histogram Plot",  
    x = "Price",              
    y = "Frequency"           
  )
dev.off()

s <- sd(data_log2$price) # SD of the variable
n <- nrow(data_log2$price)
bin_size <- 3.5 * s * (n ^ (-1/3))
num_of_bins <- ceiling((max(data_log2$price) - min(data_log2$price)) / bin_size)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Outliers

options(repr.plot.width = 15, repr.plot.height = 4)

par(cex = 4, mfrow = c(1, 5))

boxplot(data_log2$price, main = "Price", col = "lightblue", cex.axis = 1, cex.lab = 3)
boxplot(data_log2$enginesize, main = "Engine Size", col = "lightblue", cex.axis = 1, cex.lab = 3)
boxplot(data_log2$horsepower, main = "Horsepower", col = "lightblue", cex.axis = 1, cex.lab = 3)
boxplot(data_log2$avg_mpg, main = "Average MPG", col = "lightblue", cex.axis = 1, cex.lab = 3)
boxplot(data_log2$curbweight, main = "Curbweight", col = "lightblue", cex.axis = 1, cex.lab = 3)

par(cex = 4, mfrow = c(1, 1))

#pdf("Boxplot_price.pdf")
ggplot(data_log2, aes(y = as.numeric(price), x = "")) +
  geom_boxplot() + 
  ggtitle("Box plot of price") +
  xlab("Variable 'price'") + 
  ylab("Price") +
  scale_y_continuous(breaks = seq(0, 240000, by = 20000))
#dev.off()

#===============================================================================

# Q-Q plot of Price

ggplot(data_log2, aes(sample = price)) +
  geom_qq() +
  stat_qq_line(intercept = 0, slope = 1, col = "red", linetype = "dashed") +
  ggtitle("Normal Q-Q Plot for Price") +
  xlab("Theoretical Quantiles of Price") +
  ylab("Sample Quantiles of Price")


#scatter-plot
x= avg_mpg
y = price
ggplot(data_log2, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Adding a fitted line
  ggtitle("Scatter plot of Average mileage and Price") +
  xlab("Average Mileage") +
  ylab("Price") +
  scale_y_continuous(breaks = seq(0, 240000, by = 20000))


x= horsepower
y = price
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Adding a fitted line
  ggtitle("Scatter plot of Horsepower and Price") +
  xlab("Horse Power") +
  ylab("Price") +
  scale_y_continuous(breaks = seq(0, 240000, by = 20000))

pairs(~price + horsepower + avg_mpg + enginesize + curbweight, data = data,
      main = "Scatterplot Matrix")

# ================================================================================
par(mfrow = c(1, 1))
# Correlation between variables
correlation_P <- cor(data_log2[, 7 : 20], use = "complete.obs", method = "pearson")

corrplot(correlation_P, 
         type = "upper", 
         order = "hclust", 
         method = "color",
         tl.cex = 0.7, 
         tl.col = "black", 
         tl.srt = 45)

# Export the result in a CSV file
write.csv(correlation_P, "carPriceCorrelation_P.csv")

#===============================================================================
# Create linear regression models
# Initial regression model, namely 'modelA'
modelA <- lm(price ~ horsepower + enginesize + avg_mpg + cylindernumber + wheelbase + carwidth + carlength +
               curbweight + boreratio + doornumbers + carheight + stroke + compressionratio + peakrpm, data = data_log2)
summary(modelA)


#Final regrssion model

modelB <- lm(price ~ horsepower + enginesize + avg_mpg + curbweight + carlength , data = data_log2)
summary(modelB)

#===============================================================================
# Model diagnostics

# Save fitted values and residuals into dataset
data_log2$residuals <- residuals(modelB) 
data_log2$fitted <- fitted(modelB) 
detach(data_log2)
attach(data_log2)

# Test normality of residuals with Shapiro-Wilk's W test
shapiro.test(data_log2$residuals)

# Plot residual vs fitted data
plot(x = data_log2$fitted, y = data_log2$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16, col = "blue")

#horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

lines(lowess(data_log2$fitted, data_log2$residuals), col = "orange", lty = 2)

legend("topright", legend = c("Residuals", "Zero Line", "Smooth Line"),
       col = c("blue", "red", "orange"), lty = 1:2, cex = 0.8)

# Histogram for residuals
pdf("Histogram_residuals.pdf") 
ggplot(data, aes(x = data_log2$residuals)) +
  geom_histogram(col = "black", fill = "grey", bins = 15) + 
  ggtitle("Distribution of residuals - final regression model") + 
  xlab("Residuals") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(-40000, 100000, by = 20000)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) 
dev.off()

# Scatter plot of prdiction and residuals
pdf("ResidualPlot_modelA.pdf")
ggplot(data, aes(x = data_log2$fitted, y = data_log2$residuals)) +
  geom_point() +
  ggtitle("Scatter plot of residuals") +
  xlab("Predicted Price") +
  ylab("Residual") +
  scale_x_continuous(breaks = seq(30000, 180000, by = 30000)) +
  scale_y_continuous(breaks = seq(-30000, 30000, by = 10000))
dev.off()
#==============================================================================

# Hypothesis testing for 'modelB' final multivariate regression model
# Step 1: State the Hypotheses
# Null Hypothesis (H0): The coefficient for 'horsepower' is zero.
# Alternative Hypothesis (H1): The coefficient for 'horsepower' is not equal to zero.

# Step 2: Specify the Significance Level
alpha <- 0.05

# Step 3: Compute the Test Statistic
coef_horsepower <- coef(modelB)["horsepower"]
se_horsepower <- sqrt(diag(vcov(modelB))["horsepower"])
t_stat <- coef_horsepower / se_horsepower

# Step 4: Determine the Critical Region
df <- df.residual(modelB)
critical_value_upper <- qt(1 - alpha/2, df)
critical_value_lower <- qt(alpha/2, df)

# Step 5: Make a Decision
if (abs(t_stat) > critical_value_upper || abs(t_stat) < critical_value_lower) {
  decision <- "Reject H0"
} else {
  decision <- "Fail to reject H0"
}

# Step 6: Draw a Conclusion
if (decision == "Reject H0") {
  cat("There is enough evidence to reject the null hypothesis.\n")
} else {
  cat("Fail to reject the null hypothesis.\n")
}

cat("Test Statistic:", t_stat, "\n")
cat("Critical Value (Upper):", critical_value_upper, "\n")
cat("Critical Value (Lower):", critical_value_lower, "\n")

#===============================================================================

# Plot the t-distribution
x <- seq(-4, 4, length.out = 1000)
y <- dt(x, df)

plot(x, y, type = "l", col = "blue", lwd = 2, ylab = "Density", xlab = "t-values",
     main = "T-Distribution with Critical Region")

polygon(c(x[x <= critical_value_lower], rev(x[x <= critical_value_lower])),
        c(dt(x[x <= critical_value_lower], df), rep(0, sum(x <= critical_value_lower))),
        col = adjustcolor("red", alpha.f = 0.3), border = NA)

polygon(c(x[x >= critical_value_upper], rev(x[x >= critical_value_upper])),
        c(dt(x[x >= critical_value_upper], df), rep(0, sum(x >= critical_value_upper))),
        col = adjustcolor("red", alpha.f = 0.3), border = NA)

abline(v = c(critical_value_lower, critical_value_upper), col = "red", lty = 2)

legend("topright", legend = c("T-Distribution", "Critical Region", "Critical Values"),
       fill = c("blue", adjustcolor("red", alpha.f = 0.3), "red"), cex = 0.8)

#===============================================================================
