# ref = https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)

# read jointly created vegetables table
plants <- read.table('plants.csv', sep = ',', header = T)

# TEMPORARILY excluding non-usable data
plants <- plants[,1:5]
colnames(plants) <- c('name', 'space', 'when_to_plant', 'min_grow_t', 'max_grow_t')

# mean survival time (discussion required)
plants$time <- as.numeric(( plants$max_grow_t + plants$min_grow_t ) / 2)

# Kaplan Meier Analysis
km <- with(plants, Surv(time))
km_fit <- survfit(Surv(time) ~ 1, data=plants)
summary(km_fit, times = c(1, 30, 60, 90 * (1:10)))
plot(km_fit, xlab="Days", main = 'Kaplan Meyer Plot')

# Kaplan Meier Analysis by 'space'
km_name_fit <- survfit(Surv(time) ~ space, data=plants)
plot(km_name_fit, xlab="Days", main = 'Kaplan Meyer Plot') # too few data (check auto-plot)

plants_mut <- mutate(plants, space = ifelse((space < 77), "LT77", "OV77"), #77 is mean space
                      space = factor(space))
km_space_fit <- survfit(Surv(time) ~ space, data=plants_mut)
plot(km_space_fit, xlab="Days", main = 'Kaplan Meyer Plot')

# Cox Proportional Hazards Model
cox <- coxph(Surv(time) ~ space + when_to_plant + min_grow_t + max_grow_t, data = plants_mut)

cox_fit <- survfit(cox)
plot(cox_fit, main = "cph model", xlab="Days")
aa_fit <-aareg(Surv(time) ~ space + min_grow_t + max_grow_t, #attention, time-demanding!
                data = plants_mut)
plot(aa_fit)
summary(aa_fit)

# Random Forest Ensemble Model for Prob. of Survival
r_fit <- ranger(Surv(time) ~ space + min_grow_t + max_grow_t,
                 data = plants_mut,
                 mtry = 2,
                 importance = "permutation",
                 splitrule = "extratrees",
                verbose = TRUE)

death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)

# run all together ...
### start
plot(r_fit$unique.death.times,r_fit$survival[1,], 
      type = "l", 
      ylim = c(0,1),
      col = "red",
      xlab = "Days",
      ylab = "survival",
      main = "Patient Survival Curves")
cols <- colors()
for (n in sample(c(2:dim(plants_mut)[1]), 20)){
   lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
 }
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))
### end

vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

# Area under ROC
cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)


