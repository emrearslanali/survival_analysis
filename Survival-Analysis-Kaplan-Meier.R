library(survival)
subject <- c(1, 2, 3, 4, 5)
time <- c(2, 3, 4, 6, 7)
status <- c(1, 0, 1, 1, 0)

df <- data.frame(subject, time, status)

surv_obj <- Surv(time = df$time, event = df$status)
print(surv_obj)

#Fit Kaplan-Meier model
km_fit <- survfit(surv_obj ~ 1)
summary(km_fit)


#Survival Curve
plot(km_fit, 
     main = "Kaplan-Meier Survival Curve", 
     xlab = "Time", 
     ylab = "Probability of Surviving",
     col = "darkred", 
     lwd = 3,
     mark.time = TRUE)


#Using lung dataset
head(lung)

#Fit kaplan-meier
km_fit_sex <- survfit(Surv(time, status) ~ sex, data = lung)

#Curves
plot(km_fit_sex, 
     col = c("blue", "red"),  
     lty = 1:2,                     
     main = "Kaplan-Meier Curves With Respect To Sex",
     xlab = "Time(Day)",
     ylab = "Probability of survivng S(t)")

#Cox model with sex and age
cox_model <- coxph(Surv(time, status) ~ sex + age, data = lung)
summary(cox_model)
