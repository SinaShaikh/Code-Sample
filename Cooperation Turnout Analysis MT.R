##############################
## PSCI 207 
## Survey Cooperation and Turnout
## Sina Shaikh
##############################


#Requires some other libraries I might need
library(rio)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ppcor)
library(lubridate)
library(bit64)
library(xtable)
library(usmap)

# Set WD
setwd("~/Desktop/2021-2022/PORES Fellowship")

# Load data
load("./DispositionDataForAnalysis.Rdata")

# Make an untouched copy of the data
dat_untouched <- dat

# Subset data to only include those contacted who have non-NA values for turnout
dat_contacted <- dat[dat$contact == 1 & 
                       is.na(dat$contact) == 0 &
                       is.na(dat$turnout20) == 0,]

# How many cooperated
nrow(dat_contacted[dat_contacted$coop == 1,])

# Correlation between dat_contacted$coop and dat_contacted$turnout20
simple_fit <- lm(coop~turnout20, dat_contacted)
summary(simple_fit)

### Let's try another method to measure the relationship between cooperation'
### and turnout

# Take difference of means of turnout when coop is 1 and 0
mean(dat_contacted$turnout20[dat_contacted$coop == 1], na.rm = TRUE) - 
  mean(dat_contacted$turnout20[dat_contacted$coop == 0], na.rm = TRUE)

### Is that true by party?

# Subset data
dat_dem <- dat_contacted[dat_contacted$likely.party == 'D', ]
dat_ind <- dat_contacted[dat_contacted$likely.party == 'I', ]
dat_rep <- dat_contacted[dat_contacted$likely.party == 'R', ]

# Take difference of means of turnout when coop is 1 and 0 for democrats
dem_overall <- mean(dat_dem$turnout20[dat_dem$coop == 1], na.rm = TRUE) - 
  mean(dat_dem$turnout20[dat_dem$coop == 0], na.rm = TRUE)

# For Independents
ind_overall <- mean(dat_ind$turnout20[dat_ind$coop == 1], na.rm = TRUE) - 
  mean(dat_ind$turnout20[dat_ind$coop == 0], na.rm = TRUE)

# And for Republicans
rep_overall <- mean(dat_rep$turnout20[dat_rep$coop == 1], na.rm = TRUE) - 
  mean(dat_rep$turnout20[dat_rep$coop == 0], na.rm = TRUE)

### Now we will break the data up by state

# Look at which states are in the sample
statesList = unique(dat_contacted$state)

# Remove NA
statesList = statesList[!is.na(statesList)]

# Create array to store results
results <- data.frame(matrix(ncol = length(statesList), nrow = 3))
colnames(results) <- statesList
rownames(results) <- c('Democrats', 'Independents', 'Republicans')


# Repeat above process for every state
for (i in 1:length(statesList)){
  
  # Subset data by state
  dat_temp_state = dat_contacted[dat_contacted$state == statesList[i], ]
  
  # Subset data by party
  dat_dem <- dat_temp_state[dat_temp_state$likely.party == 'D', ]
  dat_ind <- dat_temp_state[dat_temp_state$likely.party == 'I', ]
  dat_rep <- dat_temp_state[dat_temp_state$likely.party == 'R', ]
  
  # Repeat differences in means done above
  results[1, i] <- mean(dat_dem$turnout20[dat_dem$coop == 1], na.rm = TRUE) - 
    mean(dat_dem$turnout20[dat_dem$coop == 0], na.rm = TRUE)
  
  results[2, i] <- mean(dat_ind$turnout20[dat_ind$coop == 1], na.rm = TRUE) - 
    mean(dat_ind$turnout20[dat_ind$coop == 0], na.rm = TRUE)
  
  results[3, i] <- mean(dat_rep$turnout20[dat_rep$coop == 1], na.rm = TRUE) - 
    mean(dat_rep$turnout20[dat_rep$coop == 0], na.rm = TRUE)
  
}

# Round table results to two significant digits
results <- signif(results,digits = 2)

### Now let's create a dot plot

# Make sure data subsets aren't overwritten
dat_dem <- dat_contacted[dat_contacted$likely.party == 'D', ]
dat_ind <- dat_contacted[dat_contacted$likely.party == 'I', ]
dat_rep <- dat_contacted[dat_contacted$likely.party == 'R', ]


#Create a function which draws a point and it's standard error
plot_point <- function(dataf, coop, y_val, point_type, color) {
  
  vector = dataf$turnout20[dataf$coop == coop]
  
  points(mean(vector, na.rm =TRUE), 
         y_val, pch = point_type, col = color, cex = 1.5)
  
  se_temp <- sd(vector, na.rm = TRUE) / 
    sqrt(sum(!is.na(dataf$coop)))
  arrows(x0 = mean(vector, na.rm = TRUE) - 2 * se_temp,
         y0 = y_val, 
         x1 = mean(vector, na.rm = TRUE) + 2 * se_temp,
         y1 = y_val, 
         code = 3, 
         angle = 90, 
         length = 0.02, 
         col = color, 
         lwd = .5)
}

# Create plot and add points
# Square points when they do cooperate, circles when they don't
plot(1,  main="Turnout among survey cooperators 
and non-cooperators",
     type = "n",
     xlab = "Turnout in 2020",
     ylab = "",
     yaxt="n",
     xlim = c(.84, .95),
     ylim = c(.4, .9))

# Add points and confidence intervals
plot_point(dat_contacted,
           1,
           .7,
           15, 
           "Black")
  
plot_point(dat_contacted,
           0,
           .7,
           16,
           "Black")
# Add a legend
legend(.85,.5, legend=c("Cooperated", "Didn't Cooperate"),
       col=c("black", "black"), pch=15:16, cex=0.8)


# Create plot and add points
# Square points when they do cooperate, circles when they don't
colors <- c("Dodger Blue", "Purple", "Firebrick")
dataframes <- list(dat_dem, dat_ind, dat_rep)
plot(1,  main="Turnout among survey cooperators 
and non-cooperators by party",
     type = "n",
     xlab = "Turnout in 2020",
     ylab = "",
     yaxt="n",
     xlim = c(.75, .95),
     ylim = c(0, 1))


# Add points and confidence intervals
for(i in 1:3){
  
  plot_point(dataframes[[i]],
             1,
             .7 - i*.1,
             15,
             colors[i])
  
  plot_point(dataframes[[i]],
             0,
             .7 - i*.1,
             16,
             colors[i])
  
}

mtext("Democrats   ", side = 2, las = 1, cex = 1, at = .6)
mtext("Independents   ", side = 2, las = 1, cex = 1, at = .5)
mtext("Republicans   ", side = 2, las = 1, cex = 1, at = .4)
par(mar=c(5,8,4,4))
# Add a legend
legend(.75,.25, legend=c("Cooperated", "Didn't Cooperate"),
       col=c("black", "black"), pch=15:16, cex=0.8)


### Now lets repeat that for every state

par(mar=c(5,4,4,4))
# First create the empty graph
plot(1,  main="Turnout among survey cooperators 
and non-cooperators by party",
     type = "n",
     xlab = "Turnout in 2020",
     ylab = "",
     yaxt="n",
     xlim = c(.55, 1),
     ylim = c(1.5, 10.7))
# Now loop through states each time decreasing where you've drawn it and adding
# a horizontal bar
for (i in length(statesList):1){
  
  # Subset data by state
  dat_temp_state = dat_contacted[dat_contacted$state == statesList[i], ]
  
  # Subset data by party
  dat_dem <- dat_temp_state[dat_temp_state$likely.party == 'D', ]
  dat_ind <- dat_temp_state[dat_temp_state$likely.party == 'I', ]
  dat_rep <- dat_temp_state[dat_temp_state$likely.party == 'R', ]
  
  dataframes <- list(dat_dem, dat_ind, dat_rep)
  
  for(j in 1:3){
    
    plot_point(dataframes[[j]],
               1,
               11 - i + .7 - j*.1,
               15,
               colors[j])
    
    plot_point(dataframes[[j]],
               0,
               11 - i + .7 - j*.1,
               16,
               colors[j])
    
  }

  mtext(paste(statesList[i], "   "), side = 2, las = 1, at = c(.5 - i + 11))
  
  abline(h = i, lty = 2)
  
}  


# Add a legend
legend(.55,9.5, legend=c("Cooperated", "Didn't Cooperate"),
       col=c("black", "black"), pch=15:16, cex=0.8)

### Fixed Effects Regression
library(lfe)
#felm(dv ~ iv1 + iv2 | fixed.effect | instrumental variable | cluster, data=)

# What are our options?
# Likely party?
# Don't have to worry about things which effect all people in a party the same
temp_random <- dat_contacted[sample(1:nrow(dat_contacted), size = 50),]

# Ok let's make the fixed effects model with fips code as the fixed effect
# felm(dv ~ iv1 + iv2 | fixed.effect | instrumental variable | cluster, data=)
m.fe1 <- felm(turnout20 ~ coop | fips | 0 | fips,
              data = dat_contacted)
summary(m.fe1)

# Now let's add likely party as a second independent variable
m.fe2 <- felm(turnout20 ~ coop + likely.party | fips | 0 | fips,
              data = dat_contacted)
summary(m.fe2)

# But because cooperation also variables by party we need to include
# the interaction between the two variables
m.fe3 <- felm(turnout20 ~ coop*likely.party | fips | 0 | fips,
              data = dat_contacted)
summary(m.fe3)

# Let's add some other variables 
m.fe4 <- felm(turnout20 ~ coop*likely.party + female + white + black + 
                hispanic + density| fips | 0 | fips,
              data = dat_contacted)
summary(m.fe4)

## Create a map

# Create a new empty dataframe
mapping <- data.frame(matrix(ncol = 2, nrow = length(statesList)))
colnames(mapping) = c("state", "coefficient")

# Calculate the difference between nat trust and state trust
for (i in 1:length(statesList)){
  m.lp <- felm(turnout20 ~ coop*likely.party + female + white + black + 
                  hispanic + density| fips | 0 | fips,
                data = dat_contacted[dat_contacted$state == statesList[i],])
  
  mapping[i,2] <- m.lp$coefficients[1]
}

# Put that into our datafrme
mapping[,1] <- statesList

# Create plot of US
plot_usmap(include = statesList, data = mapping, values = "coefficient") +
  scale_fill_continuous(low = "white", high = "red", 
                        label = scales::comma) +
  theme(legend.position = "right")

## Create a second map

# Create a new empty dataframe
mapping <- data.frame(matrix(ncol = 2, nrow = length(statesList)))
colnames(mapping) = c("state", "coefficient")

# Calculate the difference between nat trust and state trust
for (i in 1:length(statesList)){
  m.lp <- felm(turnout20 ~ coop*likely.party + female + white + black + 
                 hispanic + density| fips | 0 | fips,
               data = dat_contacted[dat_contacted$state == statesList[i],])
  
  mapping[i,2] <- m.lp$coefficients[11]
}

# Put that into our datafrme
mapping[,1] <- statesList

# Create plot of US
plot_usmap(include = statesList, data = mapping, values = "coefficient") +
  scale_fill_continuous(low = "red", high = "white", 
                        label = scales::comma) +
  theme(legend.position = "right")


# Wisconsin Specific
mean(!is.na(dat$turnout20[dat$state == "WI"]))

