
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

linear_model_2st_stage <- lm(mkt.part ~ x_hat + AGE_30_40 + AGE_40_50 + AGE_50_60 + AGE_OVER_60 +
    edu3 + edu4 + edu5 + edu6 
    + male + partner + numkids + retired + selfempl + lincome + 
    wq2 + wq3 + wq4, data=finlit)

summary(linear_model_2st_stage)
x_hat <- fitted(linear_model_1st_stage)