# 41100 PS5

setwd("/Users/ben/dropbox/Chicago Booth/41100 Regressions/Homework 5")

##################### Question 1 ###########################
# 1 Can observational studies replicate experiments?
# In class we studied data from the National Supported Work (NSW) experiment. Men were 
# randomized to either receive job training (the treatment) or not (control). We found the 
# average treatment effect was $1,794.34: treated men could expect to earn this much more than 
# controls. What if we didn’t have a randomized experiment? Could we still estimate this causal
# effect from observational data? Answering these questions is the goal of this problem.

# Suppose that the same 185 men received job training, and we want to know what 
# effect this training had on income, but we do not have access to the NSW control sample. 
# That is, no experiment was done. We need a control sample to compare too, and for this we 
# have 2490 men drawn from the Panel Study of Income Dynamics (PSID). This is now observational 
# data. The full data consists of the 185 treated men and these 2490 men to act as controls 
# (file nsw_psid.csv).

nsw_psid <- read.csv("nsw_psid.csv")

# (a) Using the PSID control sample as though it were the control group for a randomized trial, 
# estimate the average treatment effect.

# Run a regression of training (the treatment) on income after
tr.reg1 <- lm(nsw_psid$income.after ~ nsw_psid$treat)

tr.reg1$coefficients[2]
# The average treatment is -$15,204.

# (b) Does the PSID sample appear to be a good control group for this purpose? What 
# characteristics of the men help answer this question? Provide data-based evidence and 
# discussion for your conclusion, either way you decide. How does this shed light on your 
# finding in (a)

# PSID sample may not be a good control.  We get a negative average treatment effect.

# Compare the two groups
summary(nsw_psid[nsw_psid$treat==0,])
summary(nsw_psid[nsw_psid$treat==1,])

# Income before is very different.  Outside of any statistical test, this seems like the most
# important control.  Education, percent Black, percent married and age are also different.
# This can be seen by the differences in the summary tables.

# (c) Using the above analysis to guide you, build a regression that attempts to control 
# for any sources of nonrandomization. Does using the partial F test help you further 
# select/remove variables? Does your regression-based treatment effect estimate recover 
# the experimental benchmark treat- ment effect estimate? Discuss the uncertainty of your 
# regression-based estimate.

# First, try a regression with just income.
tr.reg2 <- lm(nsw_psid$income.after ~ nsw_psid$treat + nsw_psid$income.before1 + 
                nsw_psid$income.before2)

summary(tr.reg2)
# We can see the treatment is reversed now.

# Try a kitchen sink regression
tr.reg3 <- lm(nsw_psid$income.after ~ nsw_psid$treat + nsw_psid$income.before1 
              + nsw_psid$income.before2 + nsw_psid$education + nsw_psid$black
              + nsw_psid$married + nsw_psid$age)

summary(tr.reg3)

# Both are significant in terms of the F test. Does the kitchen sink add anything?

# Use a partial F test to figure that out:
anova(tr.reg2, tr.reg3)

# Yes, the kitchen sink is helpful.

# We now get the effect of treatment to be $978. This is much smaller than the effect from 
# the real experiment.  We have not fully recovered the impact because there are probably
# things we are not controling for, for example geography. This analysis is not 
# valid for capturing the exact impact of training.

##################### Question 2 ###########################
# 2 Infant Nutrition
# Revisit the infant nutrition data from homeworks 3 and 4.

# In homework 3, we fit three different models:

# E[woh|age] = β0 + β1age
# E[woh|age] = β0 + β1age + β2age2
# E[woh|age] = β0 + β1age + β21{age > 7} + β3age × 1{age > 7}

# Then in homework 4 we used our residual diagnostics to select one of these models. 
# Now we will revisit this question using the model building techniques we’ve learned so far.

nutrition <- read.csv("nutrition.csv")
woh <- nutrition$woh
age <- nutrition$age

# (a) In homework 3, residual plots motivated us to consider age2. Does the F test agree that
# this addition is worthwhile? What order polynomial fit is suggested by F testing?

# Run a regression on just age
nut.reg1 <- lm(woh ~ age)

# Runa regression that includes higher order polynomial terms
age2 <- age^2
age3 <- age^3
age4 <- age^4
age5 <- age^5
age6 <- age^6


nut.reg2 <- lm(woh ~ age + age2)
nut.reg3 <- lm(woh ~ age + age2 + age3)
nut.reg4 <- lm(woh ~ age + age2 + age3 + age4)
nut.reg5 <- lm(woh ~ age + age2 + age3 + age4 + age5)
nut.reg6 <- lm(woh ~ age + age2 + age3 + age4 + age5 + age6)



# Compare with a partial f test
anova(nut.reg1, nut.reg2)
anova(nut.reg2, nut.reg3)
anova(nut.reg3, nut.reg4)
anova(nut.reg4, nut.reg5)
anova(nut.reg5, nut.reg6)

# According to the partial F tests, we should use the 5th 

# (b) Compare the first model (just age) to the final model you tested in (a). 
# What do these results tell you? How would you proceed?

# Calculate model with dummy variable
dummy7 <- c(rep(0,7),rep(1,length(age)-7))
nut.reg.d <- lm(woh ~ dummy7*age)
xgrid3 <- data.frame(age,dummy7)

# Plot the 5th degree and dummy regressions
xgrid2 <- data.frame(age=0:72, age2=(0:72)^2, age3=(0:72)^3, age4=(0:72)^4, age5=(0:72)^5)
par(mfrow=c(1,1))
plot(age, woh, pch=20, xlab = "Age in Months", ylab = "Weight/Height Ratio", main = "Age and Weight/Height Ratio")
lines(age[1:7],predict(nut.reg.d, data = xgrid3)[1:7],  col = 3, lwd = 2)
lines(age[8:72],predict(nut.reg.d, data = xgrid3)[8:72],  col = 3, lwd = 2)
lines(xgrid2$age, predict(nut.reg5, newdata=xgrid2), col = 4)
legend("bottomright", legend = c("Dummy","5th Degree"), lty = 1, col = c(3,4))


# The two regressiobns look similar

# (c) Using our suite of diagnostic tools, evaluate the final model you’ve selected (i.e. the 
# highest order polynomial) against the simple linear model (just age) and the third model 
# displayed above, with a dummy for the first 7 observations. Based on the residual plots, 
# what do you conclude about each model?

# Plot diagnostics for dummy regression
par(mfrow=c(1,3))
plot(nut.reg.d$fitted.values,rstudent(nut.reg.d), pch=20, main = "Fitted Values and Studentized Residuals")
abline(h=0)
hist(rstudent(nut.reg.d))
qqnorm(rstudent(nut.reg.d))
abline(a=0,b=1)



# Plot diagnostics for 5th order regression
par(mfrow=c(1,3))
plot(nut.reg5$fitted.values,rstudent(nut.reg5), pch=20, main = "Fitted Values and Studentized Residuals")
abline(h=0)
hist(rstudent(nut.reg5))
qqnorm(rstudent(nut.reg5))
abline(a=0,b=1)


# (d) Upon a scatterplot of the data, overlap predicted regression lines and preditive 
# intervals for the third model and the model you selected in (a) and (b).

# Create the prediction intervals
nut.reg.d.pred <- predict(nut.reg.d, newdata = xgrid3, interval = "prediction", level = 0.95)
nut.reg5.pred <- predict(nut.reg5, newdata = xgrid2, interval = "prediction", level = 0.95)


# Plot the points
par(mfrow=c(1,1))
plot(age, woh, pch=20, xlab = "Age in Months", ylab = "Weight/Height Ratio", main = "Age and Weight/Height Ratio")

# Plot the predictions of the dummy regression
lines(age[1:7],nut.reg.d.pred[1:7,1],  col = 3, lwd = 2)
lines(age[8:72],nut.reg.d.pred[8:72,1],  col = 3, lwd = 2)

# Plot the lower confidence interval of the dummy regression
lines(age[1:7],nut.reg.d.pred[1:7,2],  col = 3, lwd = 1, lty = 2)
lines(age[8:72],nut.reg.d.pred[8:72,2],  col = 3, lwd = 1, lty = 2)

# Plot the upper confidence interval of the dummy regression
lines(age[1:7],nut.reg.d.pred[1:7,3],  col = 3, lwd = 1, lty = 2)
lines(age[8:72],nut.reg.d.pred[8:72,3],  col = 3, lwd = 1, lty = 2)

#Plot the predictions of the 5th order polynomial regression
lines(xgrid2$age, nut.reg5.pred[,1], col = 4, lwd = 2)

#Plot the upper and lower confidence interval of the 5th order polynomial regression
lines(xgrid2$age, nut.reg5.pred[,2], col = 4, lwd = 1, lty = 2)
lines(xgrid2$age, nut.reg5.pred[,3], col = 4, lwd = 1, lty = 2)

# Add a legend
legend("bottomright", legend = c("Dummy","5th Degree"), lty = 1, col = c(3,4))

# (e) Overall, what model do you prefer and why?

# Overall, I prefer the regression with the dummy variable.  It is simpler, and has less
# risk of overfitting the data.  Most importantly, we have a non-statistical reason to
# treat the first 7 dataponts differently.

##################### Question 3 ###########################
# 3 Beef – It’s What’s for Dinner
# Revisit the cattle ranch and voting data from homework 3.
# We have vote results (% YES), average SIZE of farm (hundreds of acres), and average VAL of 
# products sold annually by each farm (in $ thousands) for each of Montana’s 56 counties.

# In homework 3 we tried three models:
# > reg1 <- lm(YES ~ SIZE + log(VAL))
# > reg2 <- lm(YES ~ SIZE)
# > reg3 <- lm(YES ~ SIZE*log(VAL))

beef <- read.csv("beef.csv")
yes <- beef$YES
size <- beef$SIZE
val <- beef$VAL
log.val <- log(val)


# (a) Create log(YES). Does using this as the dependent variable lead to better regressions in a 
# demonstrable sense? If yes, how so?


# Run the old regression without interacting log.val and size
reg.beef1 <- lm(yes ~ log.val + size)
summary(reg.beef1)

# Print our diagnostic plots
par(mfrow=c(1,3))
plot(reg.beef1$fitted.values,rstudent(reg.beef1), pch=20, main = "Fitted Values and Studentized Residuals")
abline(h=0)
hist(rstudent(reg.beef1))
qqnorm(rstudent(reg.beef1))
abline(a=0,b=1)

# Now run a regression without interaction, but taking the log of YES
reg.beef4 <- lm(log(yes) ~ log.val + size)
summary(reg.beef4)

# Print our diagnostic plots
par(mfrow=c(1,3))
plot(reg.beef4$fitted.values,rstudent(reg.beef4), pch=20, main = "Fitted Values and Studentized Residuals")
abline(h=0)
hist(rstudent(reg.beef4))
qqnorm(rstudent(reg.beef4))
abline(a=0,b=1)

# Taking the log of YES makes the diagnostic plots look maybe slightly more normal

# Run the old with just size
reg.beef2 <- lm(yes ~ size)
summary(reg.beef2)

# Run the old regression interacting the dependent variables
reg.beef3 <- lm(yes ~ log.val*size)
summary(reg.beef3)

# Print our diagnostic plots
par(mfrow=c(1,3))
plot(reg.beef3$fitted.values,rstudent(reg.beef3), pch=20, main = "Fitted Values and Studentized Residuals")
abline(h=0)
hist(rstudent(reg.beef3))
qqnorm(rstudent(reg.beef3))
abline(a=0,b=1)

# Run the regression with interaction, taking Log of YES
reg.beef6 <- lm(log(yes) ~ log.val*size)
summary(reg.beef6)

# Print our diagnostic plots
par(mfrow=c(1,3))
plot(reg.beef6$fitted.values,rstudent(reg.beef6), pch=20, main = "Fitted Values and Studentized Residuals")
abline(h=0)
hist(rstudent(reg.beef6))
qqnorm(rstudent(reg.beef6))
abline(a=0,b=1)

# Log Yes looks a little worse here, if anything.

# (b) Put aside log(YES). Compare reg1 and reg2 using the partial F test. From these results, 
# is it advisable to pursue reg3?

anova(reg.beef2,reg.beef1)

# According to the partial f F test, adding log.val does not help.  So we dont even need
# to look into model 3.

# (c) What if we pursue reg3 anyway? Is this worthwhile compared to reg1? Compare your results 
# to part (b) and discuss your findings. What model do you prefer based on this?

anova(reg.beef2,reg.beef3)

# With the interaction term, log.val is significant.  It is worthwhile.  I prefer this model.

# (d) Based on all these results so far, run one more partial F test to find the best model.
reg.beef7 <- lm(yes ~ log.val*size - log.val)
summary(reg.beef7)

anova(reg.beef7,reg.beef3)
# This says the most complex model is the best.
