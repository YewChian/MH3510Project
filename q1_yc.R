data_raw <- read.table("~/school/linalg/aadt.txt", header=FALSE)
data <- data.frame(y=data_raw$V1,x1=data_raw$V2,x2=data_raw$V3,x3=data_raw$V4,x4=data_raw$V5)

data$x4_1 <- ifelse(data$x4 == 1, 1, 0)
data$x4_2 <- ifelse(data$x4 == 2, 1, 0)

plot(data)
print(data)
mlr_full <- lm(y ~ x1+x2+x3+x4_1, data=data)
summary(mlr_full)
# seems like x1, x2, and x4 are significant predictors based on the t-test
# adjusted r-square suggests that about 74.4% of the variance is explained by the model which is decent
# based on f-statistic, the model is significant too

# reduce the model by removing x3
mlr_reduced <- lm(y ~ x1+x2+x4_1, data=data)
summary(mlr_reduced)
# t-test shows that all predictors are significant
# adjusted r-square suggests that about 74.5% of variance is explained by model
# f-test shows that the model is significant

# predict: x1=50000  x2=3  x3=60, x4=2
test_full_data <- data.frame(x1=150, x2=3, x3=60, x4_1=0)
predict(mlr_full, newdata = test_full_data)

test_reduced_data <- data.frame(x1=150, x2=3, x4_1=0)
predict(mlr_reduced, newdata = test_reduced_data)

test_reduced_temp_data <- data.frame(x1=15000, x2=3, x4_1=0)
predict(mlr_reduced, newdata = test_reduced_temp_data)

# ==========================================================
# redundant stuff

# investigate linear regression of x3 and y
mlr_x3 <- lm(y ~x3, data=data)
summary(mlr_x3)
plot(mlr_x3$fitted.values, mlr_x3$residuals)
# investigate shape of x3
# note that x3 is the width of road (in feet)
x3_y_data <- data[, c("y", "x3")]
x3_y_data
plot(x3_y_data$x3, x3_y_data$y)
abline(mlr_x3)

#try doing a sqrt on x3?
root_x3 = sqrt(data$x3)
mlr_root_x3 <- lm(data$y ~ root_x3)
plot(root_x3, data$y)
