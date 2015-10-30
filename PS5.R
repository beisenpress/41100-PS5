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
