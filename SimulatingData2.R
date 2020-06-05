library(tidyverse)
library(dplyr)
library(gridExtra)
library(factoextra)

#setting the seed so the values stay the same throughout the project
set.seed(123) 

#creating the first column quality product which contains values -2,-1,0, 1,2
quality_product <- sample(c(-2,-1,0,1,2), 1000, replace = TRUE, prob = NULL)

#creating a data frame out of it
quality_product <- as.data.frame(quality_product)

#creating a new data frame which will contain the first data frame as the column
data <- quality_product

#adding three more variables with the same parameters as the first variable
data$waste_management <- sample(c(-2,-1,0,1,2), 1000, replace = TRUE, prob = NULL)
data$durability <- sample(c(-2,-1,0,1,2), 1000, replace = TRUE, prob = NULL)
data$toxicity <- sample(c(-2,-1,0,1,2), 1000, replace = TRUE, prob = NULL)

#using a sample like I did in the first four variables
recycled_biomass <- sample(1:100, 1000, replace = TRUE, prob = NULL)
summary(recycled_biomass)
recycled_biomass

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
recycled_biomass <- range01(recycled_biomass)
recycled_biomass <- round(recycled_biomass, digits = 2)
recycled_biomass <- as.data.frame(recycled_biomass)

#assigning a score based on the percentage score
recycled_biomass <- recycled_biomass %>% 
  mutate(recycled_biomass = case_when(
    recycled_biomass <= 0.10 ~ -2,
    recycled_biomass > 0.10 & recycled_biomass <= 0.20 ~ -1.5,
    recycled_biomass > 0.20 & recycled_biomass <= 0.30 ~ -1, 
    recycled_biomass > 0.30 & recycled_biomass <= 0.40 ~ -0.5, 
    recycled_biomass > 0.40 & recycled_biomass <= 0.50 ~ 0,
    recycled_biomass > 0.50 & recycled_biomass <= 0.60 ~ 0.5, 
    recycled_biomass > 0.60 & recycled_biomass <= 0.70 ~ 1,
    recycled_biomass > 0.70 & recycled_biomass <= 0.80 ~ 1.5,
    recycled_biomass > 0.80 ~ 2, 
    TRUE ~ -3 # 
  ))

data$recycled_biomass <- recycled_biomass$recycled_biomass

#companies who are a certified wooden resource get 1 and others get 0 
data$sustainability_of_biomass <- sample(c(0,1), 1000, replace = TRUE, prob = NULL)

#innovative companies get 1 and the others get a 0
data$innovation <- sample(c(0,1), 1000, replace = TRUE, prob = NULL)

#data found online
annual_production <- as.data.frame(c(9100, 4810, 4680, 6000, 27500, 20400, 13000, 13000, 7280))
annual_kWh_usage <- as.data.frame(c(842000, 1787904, 1892352, 3962400, 5918400, 11469248, 1119966, 2185920, 927168))

#calculating the Energy Usage (the annual kWh usage / annual production in mbf)
kWh_usage_real <- annual_kWh_usage / annual_production

#using the min and max values from the data that I got up there to simulate our own data
annual_production_sim <- as.data.frame(sample(min(annual_production):max(annual_production), 1000, replace = TRUE, prob = NULL))
annual_kWh_usage_sim <- as.data.frame(sample(min(annual_kWh_usage):max(annual_kWh_usage), 1000, replace = TRUE, prob = NULL))

#calculating the kWh usage for the simulated data + doing some data wrangling with column names
kWh_usage2 <- annual_kWh_usage_sim / annual_production_sim
kWh_usage2 <- as.data.frame(kWh_usage2)
kWh_usage <- kWh_usage2
colnames(kWh_usage)
names(kWh_usage)[names(kWh_usage) == "sample(min(annual_kWh_usage):max(annual_kWh_usage), 1000, replace = TRUE, prob = NULL)"] <- "kWh_usage"

#adding it to our data simulation set
data$kWh_usage <- kWh_usage

#scaling data between 0 and 1 for the kWh_usage
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
data$kWh_usage <- range01(data$kWh_usage)
#rounding the digits to two decimal places
data$kWh_usage <- round(data$kWh_usage, digits = 2)

data <- data %>% 
  mutate(kWh_score = case_when(
    kWh_usage <= 0.10 ~ 2,
    kWh_usage > 0.10 & kWh_usage <= 0.20 ~ 1.5, 
    kWh_usage > 0.20 & kWh_usage <= 0.30 ~ 1,
    kWh_usage > 0.30 & kWh_usage <= 0.40 ~ 0.5,
    kWh_usage > 0.40 & kWh_usage <= 0.50 ~ 0, 
    kWh_usage > 0.50 & kWh_usage <= 0.60 ~ -0.5,
    kWh_usage > 0.60 & kWh_usage <= 0.70 ~ -1,
    kWh_usage > 0.70 & kWh_usage <= 0.80 ~ -1.5,
    kWh_usage > 0.80 ~ -2,
    TRUE ~ -3 # 
  ))

#selecting everything except for the kWh_usage
data2 <- data %>% select(-kWh_usage)

#calculating the final score by adding all the previous scores together
data2$FINAL_SCORE <- data2$quality_product + data2$waste_management + data2$durability + data2$toxicity + data2$sustainability_of_biomass +
  data2$innovation + data2$recycled_biomass + data2$kWh_score

summary(data2$FINAL_SCORE)

#-12 as the lowest score combined and 14 as the maximum we need to scale it to get numbers from 0 to 1
MAX <- 14
MIN <- -12
range02 <- function(x){(x-MIN)/(MAX-MIN)}

#adding it to our data2 dataframe and rounding it
data2$FINAL_SCORE_SCALED <- range02(data2$FINAL_SCORE)
data2$FINAL_SCORE_SCALED <- round(data2$FINAL_SCORE_SCALED, digits = 3)

#distribution
hist(data2$FINAL_SCORE, main="Histogram of Improved Final Scores", 
     xlab="Improved Final Score", 
     border="black", 
     col= "#ca0020")
data2 %>% select(FINAL_SCORE) %>% distinct()

#how many ties there are in scores
number_of_ties <- data2 %>% group_by(FINAL_SCORE) %>% count(FINAL_SCORE)
number_of_ties <- as.data.frame(number_of_ties)
number_of_ties

#removing the FINAL_SCORES from our dataset because we don't need it
data3 <- data2[order(-data2$FINAL_SCORE),]

#subsetting data based on the available scores
subset.1 <- subset(data2, select=c(quality_product, waste_management, durability, toxicity)) + 3
subset.2 <- subset(data2, select=c(sustainability_of_biomass, innovation))
subset.3 <- subset(data2, select=c(kWh_score, recycled_biomass))

#working with the first subset first
fa.subset.1 <- factanal(x = subset.1, factors = 1, scores = "regression")

subset.1.scores <- as.vector(fa.subset.1$scores)
subset.1.scores
summary(subset.1.scores)

fa1.loadings <- fa.subset.1$loadings[,1]

#weighted standard deviation
weighted.sd <- function(x, w) {
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  x.sd.w <- sqrt(abs((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2)))
  return(x.sd.w)
}

#rescaling the scores
re.scale <- function(f.scores, raw.data, loadings) {
  fz.scores <- (f.scores + mean(f.scores)) /(sd(f.scores))
  means <- apply(raw.data, 1, weighted.mean, w = loadings)
  sds <- apply(raw.data, 1, weighted.sd, w = loadings)
  grand.mean <- mean(means)
  grand.sd <- mean(sds)
  final.scores <- ((fz.scores * grand.sd) + grand.mean)
  return(final.scores)
}

#final rescaled scores for the first subset
final.scores.ss1 <- re.scale(subset.1.scores, subset.1, fa1.loadings)
final.scores.ss1 <- final.scores.ss1
summary(final.scores.ss1)

#making the composite data by adding together the subsets 2 and 3, and the rescaled scores
composite.data <- data.frame(final.scores.ss1, subset.2, subset.3)
composite.data$final.score <- final.scores.ss1 + composite.data$sustainability_of_biomass + composite.data$innovation + composite.data$kWh_score + composite.data$recycled_biomass
composite.data$final.score <- round(composite.data$final.score, digits = 3)

#final table with sorted composite scores
composite.data[order(-composite.data$final.score),]