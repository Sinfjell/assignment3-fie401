# Install libraries
require(AER)

# BEGIN: yzqjx8d02wtr
# Read the finlit.csv file
setwd("/Users/sinfjell/Library/CloudStorage/OneDrive-NorgesHandelshøyskole/FIE401/assignment-3")
finlit <- read.csv("finlit.csv")

# View the first few rows of the data
head(finlit)
finlit <- na.omit(finlit)

# Create binary age variables
finlit$AGE_UNDER_30 <- as.integer(finlit$age == "<30")
finlit$AGE_30_40 <- as.integer(finlit$age == "30s")
finlit$AGE_40_50 <- as.integer(finlit$age == "40s")
finlit$AGE_50_60 <- as.integer(finlit$age == "50s")
finlit$AGE_OVER_60 <- as.integer(finlit$age == ">60")

# Create binary variables for nonequity.wealth.cat
finlit$wq2 <- as.integer(finlit$nonequity.wealth.cat == 2)
finlit$wq3 <- as.integer(finlit$nonequity.wealth.cat == 3)
finlit$wq4 <- as.integer(finlit$nonequity.wealth.cat == 4)

# ------ Linear Probability Model ------
# Fitting a Linear Probability Model
linear_model <- lm(mkt.part ~ adv.lit.index + AGE_30_40 + AGE_40_50 + AGE_50_60 + AGE_OVER_60 +
    edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + 
    wq2 + wq3 + wq4, data=finlit)

# Summary of the model to get coefficients and other statistics
summary(linear_model)

# ------------- First stage IV model ---------------

# Create binary variables for sibling's financial situation and parent's financial knowledge
finlit$sibling_worse <- as.integer(!is.na(finlit$f10) & finlit$f10 == "worse")
finlit$sibling_better <- as.integer(!is.na(finlit$f10) & finlit$f10 == "better")
finlit$parent_intermediate_high <- as.integer(!is.na(finlit$f15) & (finlit$f15 == "intermediate or high"))
finlit$parent_dont_know <- as.integer(!is.na(finlit$f15) & finlit$f15 == "dont know")


# Fitting a Linear Probability Model for the first stage of the IV regression
linear_model_1st_stage <- lm(adv.lit.index ~ sibling_worse + sibling_better + parent_intermediate_high + parent_dont_know +
    AGE_30_40 + AGE_40_50 + AGE_50_60 + AGE_OVER_60 +
    edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + 
    wq2 + wq3 + wq4, data=finlit)

# Summary of the model to get coefficients and other statistics
summary(linear_model_1st_stage)

fit2 <- ivreg(mkt.part ~ adv.lit.index + AGE_30_40 + AGE_40_50 + AGE_50_60 + AGE_OVER_60 +
    edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + 
    wq2 + wq3 + wq4 | sibling_worse + sibling_better + parent_intermediate_high + parent_dont_know + 
    AGE_30_40 + AGE_40_50 + AGE_50_60 + AGE_OVER_60 +
    edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + 
    wq2 + wq3 + wq4, data=finlit)

summary(fit2)

# Load stargazer library
library(stargazer)

# Print stargazer for linear_model, linear_model_1st_stage, and fit2
stargazer(linear_model, linear_model_1st_stage, fit2, type = "text", 
dep.var.labels = c("Standard OLS", "First stage IV", "Second stage IV in one go"))

# ----------- exogeneity
# First checking for relevance 
# if F is under 10 we have weak instrumental variables that explain little of the variation in the endogenous variable
#Define H0
myH0 <- c("sibling_worse = 0", "sibling_better = 0", "parent_dont_know = 0", "parent_intermediate_high = 0")
#F-statistic 
linearHypothesis(linear_model_1st_stage, myH0) # over 15, good

finlit$res <- residuals(fit2)

fit_test <- lm(res ~ sibling_worse + sibling_better + parent_intermediate_high + parent_dont_know + 
    AGE_30_40 + AGE_40_50 + AGE_50_60 + AGE_OVER_60 +
    edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + 
    wq2 + wq3 + wq4, data = finlit)

Fstat <- linearHypothesis(fit_test, myH0)$F[2] # F-statistic
m <- 4 # number of instruments
k <- 1 # number of endogenous variables
testval <- m * Fstat # test statistic
pval <- 1 - pchisq(testval, (m - k)) # p-value
