# Load the required library
library(eha)
data(child)

# Load the survival package
library(survival)

# Fit Cox Proportional Hazard model
cox_model <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)

# Plot survival curves by gender
plot(survfit(cox_model, data = child), col = c("blue", "red"), lty = 1,
     lwd = 2, main = "Survival Curves by Gender", xlab = "Time", 
     ylab = "Survival Probability")
legend("bottomright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1, lwd = 2)

# Display summary of the Cox model
summary(cox_model)

# Test for autocorrelation in the Cox model
library(autoReg)
autoReg(cox_model)

# Plot forest plot for Cox regression model
library(survminer) 
ggforest(cox_model, child)
ggforest(cox_model, child,
         main = "Hazard ratio",
         cpositions = c(0.02,-0.15, 0.25), 
         fontsize = 0.8, #
         refLabel = "reference",
         noDigits = 2)

# Check for the proportional hazards assumption
cox.zph_result <- cox.zph(cox_model)
cox.zph_result
