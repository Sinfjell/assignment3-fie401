# Install libraries
require(AER)
library(sandwich)
library(lmtest)
library(stargazer)

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

# ------------- Second stage IV model ---------------
# Fitting a Linear Probability Model for the second stage of the IV regression at one pass
fit2 <- ivreg(mkt.part ~ adv.lit.index + AGE_30_40 + AGE_40_50 + AGE_50_60 + AGE_OVER_60 +
    edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + 
    wq2 + wq3 + wq4 | sibling_worse + sibling_better + parent_intermediate_high + parent_dont_know + 
    AGE_30_40 + AGE_40_50 + AGE_50_60 + AGE_OVER_60 +
    edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + 
    wq2 + wq3 + wq4, data=finlit)

# CORRECTING FOR STANDARD ERRORS
# First we are performing Breusch-Pagan test to check for heteroscedasity
# Perform Breusch-Pagan test on the linear_model
bp_test_ols <- bptest(linear_model)
bp_test_1stage <- bptest(linear_model_1st_stage)
bp_test_2stage <- bptest(fit2)

# Print the result
cat("Breusch-Pagan Test Results:\n")
cat("Standard OLS: p-value =", bp_test_ols$p.value, "\n")
cat("First stage IV: p-value =", bp_test_1stage$p.value, "\n")
cat("Second stage IV: p-value =", bp_test_2stage$p.value, "\n")

# There is extremely small p-value in all three cases, so we reject the null hypothesis that there is no heteroscedasity. 
# We need to correct for heteroscedasity

# Robust standard errors for linear_model
robust_se_linear_model <- vcovHC(linear_model, type = "HC3")
coeftest(linear_model, robust_se_linear_model)

# Robust standard errors for linear_model_1st_stage
robust_se_linear_model_1st_stage <- vcovHC(linear_model_1st_stage, type = "HC3")
coeftest(linear_model_1st_stage, robust_se_linear_model_1st_stage)

# Robust standard errors for fit2
robust_se_fit2 <- sandwich(fit2)
coeftest(fit2, robust_se_fit2)

# Stargazer for linear_model, linear_model_1st_stage, and fit2
stargazer <- stargazer(linear_model, linear_model_1st_stage, fit2, type = "html", 
          se = list(sqrt(diag(robust_se_linear_model)), sqrt(diag(robust_se_linear_model_1st_stage)), sqrt(diag(robust_se_fit2))),
          dep.var.labels = c("Linear probability model", "First stage IV", "Second stage IV in one go"), 
          covariate.labels = c("Advanced literacy index", "Sibling less financial knowledge", "Sibling better financial knowledge",
          "Parents intermediate or high financial knowledge", "Parents unknown financial knowledge", "Age 30-40", "Age 40-50", "Age 50-60", "Age over 60", "Intermidiate vocational education", "Secondary pre-university education", 
          "Higher vocational education", "University education", "Male", "Partner", "Number of children", "Retired", "Self-employed", 
          "Log of income", "Wealth quartile 2", "Wealth quartile 3", "Wealth quartile 4"))

write(stargazer, file = "summary_table.html")

# ----------- exogeneity
# First checking for relevance 
# if F is under 10 we have weak instrumental variables that explain little of the variation in the endogenous variable
# Define H0
myH0 <- c("sibling_worse = 0", "sibling_better = 0", "parent_dont_know = 0", "parent_intermediate_high = 0") # null hypothesis

#F-statistic 
linearHypothesis(linear_model_1st_stage, myH0) # over 15, good

# Checking for exogeneity
finlit$res <- residuals(fit2) # residuals from the second stage regression
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
pval
testval
# pval is 11.5% so we cannot reject the null hypothesis that the instruments are exogenous

