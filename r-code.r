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

# END: yzqjx8d02wtr
