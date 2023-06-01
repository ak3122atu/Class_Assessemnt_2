#converting the dataset to a dataframe

sleep_data <- read.csv("sleep.csv")
str(sleep_data)

# Renaming Sleeping hours as sleeping hours and snoring rate 
# are both renamed sr 
colnames(sleep_data)[colnames(sleep_data) == "sr.1"] <- "sh"

names(sleep_data)
# Examine initial linearity between variables in the dataset
library(psych)
pairs.panels(sleep_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Examine linearity in more detail
scatter.smooth(x = sleep_data$sr,
               y = sleep_data$sl,
               xlab = "Snoring Rate",
               ylab = "Stress Level", main = "Correlation of stress ~ Snoring rate")

# chart appears to have high correlation

# Examining correlation between sleeping hours and stress levels
cor(sleep_data$sr, sleep_data$sl)

# Examining correlation between respiration rate and stress levels
scatter.smooth(x = sleep_data$rr,
               y = sleep_data$sl,
               xlab = "Respiration Rate",
               ylab = "Stress Level", main = "Correlation of stress ~ Respiration rate")
cor(sleep_data$rr, sleep_data$sl)

# Examining correlation between body temperature and stress levels
scatter.smooth(x = sleep_data$t,
               y = sleep_data$sl,
               xlab = "Body Temperature",
               ylab = "Stress Level", main = "Correlation of stress ~ Body Temperature")
cor(sleep_data$t, sleep_data$sl)

# Examining correlation between limb movements and stress levels
scatter.smooth(x = sleep_data$lm,
               y = sleep_data$sl,
               xlab = "Limb Movements",
               ylab = "Stress Level", main = "Correlation of stress ~ Limb movements")
cor(sleep_data$lm, sleep_data$sl)

# Examining correlation between blood oxygen and stress levels
scatter.smooth(x = sleep_data$bo,
               y = sleep_data$sl,
               xlab = "Blood Oxygen",
               ylab = "Stress Level", main = "Correlation of stress ~ Blood Oxygen")
cor(sleep_data$bo, sleep_data$sl)

# Examining correlation between rem and stress levels
scatter.smooth(x = sleep_data$rem,
               y = sleep_data$sl,
               xlab = "Rapid Eye Movements",
               ylab = "Stress Level", main = "Correlation of stress ~ REM")
cor(sleep_data$rem, sleep_data$sl)

# Examining correlation between sleeping hours and stress levels
scatter.smooth(x = sleep_data$sh,
               y = sleep_data$sl,
               xlab = "Sleeping hours",
               ylab = "Stress Level", main = "Correlation of stress ~ Sleeping hours")
cor(sleep_data$sh, sleep_data$sl)

# Examining correlation between heart rate and stress levels
scatter.smooth(x = sleep_data$hr,
               y = sleep_data$sl,
               xlab = "Heart rate",
               ylab = "Stress Level", main = "Correlation of stress ~ Heart rate")
cor(sleep_data$hr, sleep_data$sl)

# Check for outliers
opar <- par(no.readonly = TRUE)

attach(sleep_data)
boxplot(sr,
        main = "Snoring rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(sr)$out)) # box plot for 'snoring rate'

attach(sleep_data)
boxplot(rr,
        main = "Respiration rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(rr)$out)) # box plot for 'respiration rate'

attach(sleep_data)
boxplot(t,
        main = "Body temperature",
        sub = paste("Outlier rows: ",
                    boxplot.stats(t)$out)) # box plot for 'Body temperature'

attach(sleep_data)
boxplot(lm,
        main = "Limb Movements",
        sub = paste("Outlier rows: ",
                    boxplot.stats(lm)$out)) # box plot for 'Limb Movements'

attach(sleep_data)
boxplot(bo,
        main = "Blood oxygen",
        sub = paste("Outlier rows: ",
                    boxplot.stats(bo)$out)) # box plot for 'Blood oxygen'

attach(sleep_data)
boxplot(rem,
        main = "Rapid Eye Movement",
        sub = paste("Outlier rows: ",
                    boxplot.stats(rem)$out)) # box plot for 'REM'

attach(sleep_data)
boxplot(sh,
        main = "Sleeping hours",
        sub = paste("Outlier rows: ",
                    boxplot.stats(sh)$out)) # box plot for 'Sleeping hours'

attach(sleep_data)
boxplot(hr,
        main = "Heart Rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(hr)$out)) # box plot for 'Heart rate'

detach(sleep_data)
par(opar)

# Check for normality
# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical
plot(density(sleep_data$sr),
     main = "Density plot : Snoring Rate",
     ylab = "Frequency", xlab = "Snoring Rate",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$sr), 2)))
# fill the area under the plot
polygon(density(sleep_data$sr), col = "red")

plot(density(sleep_data$rr),
     main = "Density plot : Respiration Rate",
     ylab = "Frequency", xlab = "Respiration Rate",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$rr), 2)))
polygon(density(sleep_data$rr), col = "red")

plot(density(sleep_data$t),
     main = "Density plot : Body Temperature",
     ylab = "Frequency", xlab = "Body Temperature",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$t), 2)))
polygon(density(sleep_data$t), col = "red")

plot(density(sleep_data$lm),
     main = "Density plot : Limb Movements",
     ylab = "Frequency", xlab = "Limb Movements",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$lm), 2)))
polygon(density(sleep_data$lm), col = "red")

plot(density(sleep_data$bo),
     main = "Density plot : Blood Oxygen",
     ylab = "Frequency", xlab = "Blood Oxygen",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$bo), 2)))
polygon(density(sleep_data$bo), col = "red")

plot(density(sleep_data$rem),
     main = "Density plot : Rapid Eye Movements",
     ylab = "Frequency", xlab = "Rapid Eye Movements",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$rem), 2)))
polygon(density(sleep_data$rem), col = "red")

plot(density(sleep_data$sh),
     main = "Density plot : Sleeping Hours",
     ylab = "Frequency", xlab = "Sleeping Hours",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$sh), 2)))
polygon(density(sleep_data$sh), col = "red")

plot(density(sleep_data$sl),
     main = "Density plot : Stress Level",
     ylab = "Frequency", xlab = "Stress Level",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$sl), 2)))
polygon(density(sleep_data$sl), col = "red")

plot(density(sleep_data$hr),
     main = "Density plot : Heart Rate",
     ylab = "Frequency", xlab = "Heart Rate",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$hr), 2)))
polygon(density(sleep_data$hr), col = "red")
par(opar)

# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0-5 = approx symetric.

paste("Skewness for Snoring Rate : ", round(e1071::skewness(sleep_data$sr), 2))
paste("Skewness for Respiration Rate : ", round(e1071::skewness(sleep_data$rr), 2))
paste("Skewness for Body Temperature : ", round(e1071::skewness(sleep_data$t), 2))
paste("Skewness for Limb Movements : ", round(e1071::skewness(sleep_data$lm), 2))
paste("Skewness for Blood Oxygen : ", round(e1071::skewness(sleep_data$bo), 2))
paste("Skewness for Rapid Eye Movements : ", round(e1071::skewness(sleep_data$rem), 2))
paste("Skewness for Sleeping Hours : ", round(e1071::skewness(sleep_data$sh), 2))
paste("Skewness for Heart Rate : ", round(e1071::skewness(sleep_data$hr), 2))
paste("Skewness for Stress Level : ", round(e1071::skewness(sleep_data$sl), 2))

#rem = -0.57 =  moderately skewed
#everuthing seems okay

#checking normality of all the variables

shapiro.test(sleep_data$sr)
shapiro.test(sleep_data$rr)
shapiro.test(sleep_data$t)
shapiro.test(sleep_data$lm)
shapiro.test(sleep_data$bo)
shapiro.test(sleep_data$rem)
shapiro.test(sleep_data$sh)
shapiro.test(sleep_data$hr)
shapiro.test(sleep_data$sl)

# If p-value < 0.05 then variable
# is not normally distributed

# snoring rate is not normally distributed (p-value = 0.00000000000000022)
# respiration rate is not normally distributed (p-value = 0.000000000000001571)
# body temperature is not normally distributed (p-value = 0.000000006969)
# limb movements is not normally distributed (p-value = 0.00000000000003211)
# blood oxygen is not normally distributed (p-value = 0.00000000001324)
# rem is not normally distributed (p-value = 0.000000000000001313)
# sh is not normally distributed 
# sl is not normally distributed

#all the variables need to be transformed.
library(MASS)
# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$sr ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_sr = (sleep_data$sr ^ lambda - 1)/lambda
hist(transformed_sr)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_sr)

#Sr is still not normally distributed

# Tukey’s Ladder of Powers transformation
# also indicates that the best value for 
# lambda is -0.6 for SR variable
library(rcompanion)

#transform_tukey_sr = transformTukey(sleep_data$sr, plotit=FALSE)
# -0.6 indaictes that "-1 * x ^ lambda" is required
#transformed_sr <- -1 * sleep_data$sr ^ -0.625
#shapiro.test(transformed_sr)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$rr ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_rr = (sleep_data$rr ^ lambda - 1)/lambda
hist(transformed_rr)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_rr)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$t ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_t = (sleep_data$t ^ lambda - 1)/lambda
hist(transformed_t)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_t)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$lm ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_lm = (sleep_data$t ^ lambda - 1)/lambda
hist(transformed_lm)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_lm)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$bo ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_bo = (sleep_data$bo ^ lambda - 1)/lambda
hist(transformed_bo)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_t)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$bo ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_bo = (sleep_data$bo ^ lambda - 1)/lambda
hist(transformed_bo)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_bo)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$rem ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_rem = (sleep_data$rem ^ lambda - 1)/lambda
hist(transformed_rem)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_rem)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$sh ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_sh = (sleep_data$sh ^ lambda - 1)/lambda
hist(transformed_sh)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_rem)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$hr ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_hr = (sleep_data$sh ^ lambda - 1)/lambda
hist(transformed_hr)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_hr)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleep_data$sl ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_sl = (sleep_data$sl ^ lambda - 1)/lambda
hist(transformed_sl)

# p-value indictes that the data is still not normally distributed
shapiro.test(transformed_sl)

# Converting data in data frame
sleep_data$transformed_sr <- transformed_sr
sleep_data$transformed_rr <- transformed_rr
sleep_data$transformed_t <- transformed_t
sleep_data$transformed_lm <- transformed_lm
sleep_data$transformed_bo <- transformed_bo
sleep_data$transformed_rem <- transformed_rem
sleep_data$transformed_sh <- transformed_sh
sleep_data$transformed_hr <- transformed_hr
sleep_data$transformed_sl <- transformed_sl

attach(sleep_data)

# Split the data into training and testing
set.seed(1)
no_rows_data <- nrow(sleep_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- sleep_data[sample, ]
testing_data <- sleep_data[-sample, ]

#Now we can build the linear model using the training data.

linearMod <- lm(sl ~ sr + 
                  rr + t + lm + 
                  + rem + sh + 
                  hr, data = training_data)
linearMod
summary(linearMod)
AIC(linearMod)
BIC(linearMod)
#Now we will predict distance values and see how they compare with the testing data.
predicted_sl <- predict(linearMod, testing_data)

# make actuals_predicted dataframe.
actuals_predictions <- data.frame(cbind(actuals = testing_data$sl, predicted = predicted_sl))
head(actuals_predictions)
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy

# MAPE
MAPE <- function(actuals_predictions, predicted_sl) {
  n <- length(actuals_predictions)
  sum(abs((actuals_predictions - predicted_sl) / actuals_predictions)) * 100 / n
}

MAPE
mape_value <- MAPE(actuals_predictions, predicted_sl)
print(mape_value)

install.packages("DAAG")
library(DAAG)
cvResults <- suppressWarnings(CVlm(data = sleep_data, 
                                   form.lm = sl ~ +sr + 
                                     rr + t + lm + 
                                     + rem + sh + 
                                     hr, 
                                   m = 5, 
                                   dots = FALSE, 
                                   seed = 1, 
                                   legend.pos = "topleft", 
                                   printit = FALSE, 
                                   main = "Small symbols are predicted values while bigger ones are actuals."));


