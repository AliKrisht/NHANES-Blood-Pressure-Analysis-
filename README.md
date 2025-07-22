# End-to-End-Projects
# NHANES Blood Pressure Analysis 🩺📊

This project uses regression and variable selection methods to analyze the relationship between smoking and average systolic blood pressure (BPsysAve) using a sample from the NHANES dataset.

## 📂 Project Structure
- `report`: Contains the main RMarkdown analysis file.
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
