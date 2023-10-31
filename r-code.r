# BEGIN: yzqjx8d02wtr
# Read the finlit.csv file
setwd("/Users/sinfjell/Library/CloudStorage/OneDrive-NorgesHandelshøyskole/FIE401/assignment-3")
finlit <- read.csv("finlit.csv")

# View the first few rows of the data
head(finlit)

# Create binary age variables
finlit$AGE_UNDER_30 <- as.integer(finlit$age == "<30")
finlit$AGE_30_40 <- as.integer(finlit$age %in% c("<30", "30s"))
finlit$AGE_40_50 <- as.integer(finlit$age == "40s")
finlit$AGE_50_60 <- as.integer(finlit$age == "50s")
finlit$AGE_OVER_60 <- as.integer(finlit$age == ">60")

# Create a new dataframe with age binary variables and age variable
age_df <- data.frame(
    AGE_UNDER_30 = finlit$AGE_UNDER_30,
    AGE_30_40 = finlit$AGE_30_40,
    AGE_40_50 = finlit$AGE_40_50,
    AGE_50_60 = finlit$AGE_50_60,
    AGE_OVER_60 = finlit$AGE_OVER_60,
    age = finlit$age
)

finlit$nonequity.wealth.cat
# Create binary variables for nonequity.wealth.cat
finlit$wq2 <- as.integer(finlit$nonequity.wealth.cat == 2)
finlit$wq3 <- as.integer(finlit$nonequity.wealth.cat == 3)
finlit$wq4 <- as.integer(finlit$nonequity.wealth.cat == 4)

age_df_2 <- data.frame(
    non_equity = finlit$nonequity.wealth.cat,
    AGE_30_40 = finlit$wq2,
    AGE_40_50 = finlit$wq3,
    AGE_50_60 = finlit$wq4
)

# View the first few rows of the data with binary age variables
head(finlit)

# View the first few rows of the data with age dummies
head(finlit)

# Fitting a Linear Probability Model
linear_model <- lm(mkt.part ~ adv.lit.index + AGE_UNDER_30 + AGE_30_40 + AGE_40_50 + AGE_50_60 + AGE_OVER_60 +
    edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + 
    wq2 + wq3 + wq4, data=finlit)

# Summary of the model to get coefficients and other statistics
summary(linear_model)

# First stage regression
first_stage <- lm(adv.lit.index ~ f10 + f15 + 
                  edu2 + edu3 + edu4 + edu5 + edu6 +
                  male + partner + numkids + retired + selfempl + lincome + nonequity.wealth.cat, 
                  data=finlit)

finlit$predicted_adv_lit_index <- predict(first_stage, newdata=finlit)

# Assuming 'predicted_adv_lit_index' contains the predicted values from the first stage
second_stage <- lm(mkt.part ~ predicted_adv_lit_index + 
                   edu2 + edu3 + edu4 + edu5 + edu6 +
                   male + partner + numkids + retired + selfempl + lincome + nonequity.wealth.cat, 
                   data=finlit)

# Load the stargazer package
library(stargazer)

# Create a list of the three models
models_list <- list(linear_model, first_stage, second_stage)

# Use stargazer to print the summaries side by side
# Print summaries with stargazer
stargazer(linear_model, first_stage, second_stage, type = "text", 
          title = "Summary of Linear Probability Model and First Stage Regression",
          align = TRUE
          #dep.var.labels = c("Market Participation", "Predicted Adult Literacy Index", "Market Participation")
         )
