# End-to-End-Projects
# NHANES Blood Pressure Analysis 🩺📊

This project uses regression and variable selection methods to analyze the relationship between smoking and average systolic blood pressure (BPsysAve) using a sample from the NHANES dataset.

## 📂 Project Structure
- `
# Final Project - Ali Krisht

# Load libraries
library(NHANES)
library(tidyverse)
library(glmnet)
library(rms)
library(Matrix)
library(carData)
library(car)

# Data cleaning and preparation
small.nhanes <- na.omit(NHANES[NHANES$SurveyYr=="2011_12" & NHANES$Age > 17,c(1,3,4,8:11,13,17,20,21,25,46,50,51,52,61)])
small.nhanes <- as.data.frame(small.nhanes %>% group_by(ID) %>% filter(row_number()==1))
set.seed(1003376544)
train <- small.nhanes[sample(seq_len(nrow(small.nhanes)), size = 400),]
test <- small.nhanes[!small.nhanes$ID %in% train$ID,]

# Model diagnostics
model.lm <- lm(BPSysAve ~ ., data = train[, -c(1)])
summary(model.lm)
outlierTest(model.lm)
qqPlot(model.lm, main="QQ Plot")
leveragePlots(model.lm)

# Influence diagnostics
D = cooks.distance(model.lm)
dfits = dffits(model.lm)
dfb = dfbetas(model.lm)
cutoff <- 4 / ((nrow(train) - length(model.lm$coefficients) - 2))
plot(model.lm, which=4, cook.levels=cutoff)
influencePlot(model.lm, id.method="identify", main="Influence Plot", sub="Circle size ~ Cook's D")

# Predictions and transformation
pred.y <- predict(model.lm, newdata = test, type = "response")
mean((test$BPSysAve - pred.y)^2)

# Transformation
resid <- rstudent(model.lm)
fitted <- predict(model.lm)
qqnorm(resid)
qqline(resid)
plot(resid ~ fitted, col="red")
lines(lowess(fitted, resid), col="blue")
abline(lm(resid ~ fitted), col="blue")

# Power transform
mult <- lm(BPSysAve ~ ., data = train[, -c(1)])
bc <- powerTransform(mult)
summary(bc)
mult1 <- lm((BPSysAve)^-1 ~ ., data = train[, -c(1)])
summary(mult1)

# VIF and variable reduction
vif(mult1)
reg1 <- lm((BPSysAve)^-1 ~ Age + Race3 + Education + MaritalStatus + Poverty + Depressed + SleepHrsNight +
           SleepTrouble + PhysActive + SmokeNow, data = train[, -c(1)])

# Stepwise selection
n <- nrow(train)
sel.var.aic <- step(reg1, trace = 0, k = log(n), direction = "both")
model.lm1 <- lm(BPSysAve ~ Age, data = train[, -c(1)])
model.lm12 <- lm(BPSysAve ~ Age + SmokeNow, data = train[, -c(1)])

# Forward selection
sel.var.aic1 <- step(reg1, trace = 0, k = log(n), direction = "forward")

# Backward selection
sel.var.aic2 <- step(reg1, trace = 0, k = log(n), direction = "backward")

# LASSO regression
x = model.matrix(BPSysAve ~ Age + Race3 + Education + MaritalStatus + Poverty + Depressed + SleepHrsNight +
                 SleepTrouble + PhysActive + SmokeNow, data=train)[,-1]
cv.out <- cv.glmnet(x, y = train$BPSysAve, standardize = TRUE, alpha = 1)
best.lambda <- cv.out$lambda.1se
co <- coef(cv.out, s = "lambda.1se")

# Ridge regression
model.ridge <- glmnet(x = model.matrix( ~ ., data = train[,-c(1,12)]), y = train$BPSysAve, standardize = TRUE, alpha = 0)
pred.y.ridge <- predict(model.ridge, newx = model.matrix( ~ ., data = test[,-c(1,12)]), type = "response")
mean((test$BPSysAve - pred.y.ridge)^2)

# Shrinkage variable selection
thresh <- 0.00
inds <- which(abs(co) > thresh)
variables <- row.names(co)[inds]
sel.var.lasso <- variables[!(variables %in% '(Intercept)')]

# AIC/BIC selection
sel.var.aic <- attr(terms(step(reg1, trace = 0, k = 2)), "term.labels")
sel.var.bic <- attr(terms(step(reg1, trace = 0, k = log(n))), "term.labels")

# Cross-validation calibration
ols.aic <- ols(BPSysAve ~ ., data = train[, which(colnames(train) %in% c(sel.var.aic, "BPSysAve","SmokeNow"))], x=TRUE, y=TRUE, model=TRUE)
ols.bic <- ols(BPSysAve ~ ., data = train[, which(colnames(train) %in% c(sel.var.bic, "BPSysAve","SmokeNow"))], x=TRUE, y=TRUE, model=TRUE)
ols.lasso <- ols(BPSysAve ~ ., data = train[, which(colnames(train) %in% c(sel.var.lasso, "BPSysAve","SmokeNow"))], x=TRUE, y=TRUE, model=TRUE)

aic.cross <- calibrate(ols.aic, method = "crossvalidation", B = 10)
bic.cross <- calibrate(ols.bic, method = "crossvalidation", B = 10)
lasso.cross <- calibrate(ols.lasso, method = "crossvalidation", B = 10)

# Prediction errors
pred.aic <- predict(ols.aic, newdata = test[, which(colnames(train) %in% c(sel.var.aic, "BPSysAve","SmokeNow"))])
mean((test$BPSysAve - pred.aic)^2)

pred.bic <- predict(ols.bic, newdata = test[, which(colnames(train) %in% c(sel.var.bic, "BPSysAve","SmokeNow"))])
mean((test$BPSysAve - pred.bic)^2)

model.lasso <- glmnet(x = model.matrix( ~ ., data = train[,-c(1,12)]), y = train$BPSysAve, standardize = TRUE, alpha = 1)
pred.y.lasso <- predict(model.lasso, newx = model.matrix( ~ ., data = test[,-c(1,12)]), type = "response")
mean((test$BPSysAve - pred.y.lasso)^2)
`: Contains the main RMarkdown analysis file.
- [final project.pdf](https://github.com/user-attachments/files/21372509/final.project.pdf)
`: Final academic paper (PDF).


## 🧠 Objective
To determine whether smoking (`SmokeNow`) significantly affects systolic blood pressure, using:
- Multiple Linear Regression
- Residual Diagnostics (QQ plots, Cook's Distance)
- Transformations
- Variable Selection (AIC, BIC, LASSO)
- Cross-Validation

## 🧪 Methods Used
- R (base R, `glmnet`, model diagnostics)
- Linear Modeling
- Feature Selection: Stepwise, Forward, Backward
- Cross-Validation: MAE comparison across models

## 🔍 Key Findings
- `SmokeNow` was **not** a significant predictor of BPsysAve.
- The best model (AIC-based) identified `Age` and `Poverty` as the most important variables.
- LASSO performed worse in prediction accuracy compared to AIC/BIC.

## 📉 Visuals
The analysis includes:
- Residual and QQ plots
- Cook’s Distance for outlier detection
- Cross-validation calibration curves
- Model comparison using AIC, BIC, LASSO

## 👤 Author
Ali Krisht   
[LinkedIn](https://linkedin.com/in/alikrisht)
