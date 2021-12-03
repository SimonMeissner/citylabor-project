# ref = https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)

# read jointly created vegetables table
plants <- read.table('plants.csv', sep = ',', header = T)

# TEMPORARILY excluding non-usable data
plants <- plants[,1:6]
colnames(plants) <- c('name', 'space', 'when_to_plant', 'min_grow_t', 'max_grow_t', 'when_to_harvest')

# mean survival time (discussion required)
plants$time <- as.numeric(( plants$max_grow_t + plants$min_grow_t ) / 2)

plants_mut <- mutate(plants, space = ifelse((space < 77), "LT77", "OV77"), #77 is mean space
                      space = factor(space))

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

##############

# using user data 
size = 40 #squared inches size #user defined

wtp = "01.06.2021" #user defined
wth = "01.08.2021" #user defined
wtp <- as.Date(wtp, '%d.%m.%Y')
wth <- as.Date(wth, '%d.%m.%Y')
gt = as.integer(wth - wtp)

dt <- c()
for (i in 1:length(death_times)){
  dt[i] <- abs(death_times[i] - gt)
}

min_dt <- min(dt)
for (i in 1:length(death_times)){
  if (dt[i] == min(dt)){
    item = i
  }
}

wtp = as.integer(format(wtp, "%m"))
wth = as.integer(format(wth, "%m"))    

#final filtering
f <- surv_prob[,item]

ls <- c()
for ( i in 1:length(f)){
  if (f[i] > 0.6){
    ls <- c(ls, i)
  }
}
ls

nls <- c()
for ( i in ls){

  if (grepl(wtp, plants[i, 3]) == 1 & grepl(wth, plants[i, 6]) == 1) {

    nls <- c(nls, i)

  }
}
nls

plants <- plants[nls,]
plants$surv_p <- round(surv_prob[nls, item], 2)*100
plants <- plants[which(plants$min_grow_t <= gt + 15 & plants$max_grow_t >= gt - 15), ]
plants <- plants[which(plants$space <= size + 15 ),]
if ( is.null(plants) == 1){

  print("Sorry, no recommendations for you. Please try other dates.")

}else{

  for (i in 1:nrow(plants)){
    cat(
      "We recommend you ",
      plants[i, 1],
      "with a",
      as.character(plants[i,'surv_p']),
      "% chance of success!",
      "You're harvesting between", plants[i, 4], "and", plants[i, 5], "days, approximatedly", 
      "\n\n"
    ) 
  }
}

plants
# get user's location

# get user's climate

# recommend based on climate printing min and max growing time
