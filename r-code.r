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
summary(lpm_model)

# Summary of the model to get coefficients and other statistics
summary(linear_model)





