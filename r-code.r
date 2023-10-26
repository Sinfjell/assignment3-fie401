# BEGIN: yzqjx8d02wtr
# Read the finlit.csv file
finlit <- read.csv("finlit.csv")

# View the first few rows of the data
head(finlit)

# Fitting a Linear Probability Model
linear_model <- lm(mkt.part ~ adv.lit.index + 
    edu2 + edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + nonequity.wealth.cat, data=finlit)

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
# Rename variables for more understandable names in printout
names(linear_model)[names(linear_model) == "adv.lit.index"] <- "Market Participation"
names(first_stage)[names(first_stage) == "adv.lit.index"] <- "Predicted Adult Literacy Index"

# Print summaries with stargazer
stargazer(linear_model, first_stage, second_stage, type = "text", 
          title = "Summary of Linear Probability Model and First Stage Regression",
          align = TRUE
          #dep.var.labels = c("Market Participation", "Predicted Adult Literacy Index", "Market Participation")
         )


