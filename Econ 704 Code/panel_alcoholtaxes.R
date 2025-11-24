# ==============================================================================
# Do alcohol taxes reduce traffic fatalities? 
# ==============================================================================
# R Example for Econ 704: Topic 1 Panel Data
# Adapted from Introduction to Econometrics with R
# https://www.econometrics-with-r.org/10-rwpd.html
# ==============================================================================


# ==============================================================================
# Load Packages and Data
# ==============================================================================

# Load Packages
library(lfe)

# Import CSV
Fatalities <- read.csv("fatalities.csv")

View(Fatalities) # view data set; check codebook for variable definition

# Is this panel balanced?
table(Fatalities$state, Fatalities$year)
table(Fatalities$state)

# Construct Outcome of Interest
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000


# ==============================================================================
# Estimation and Inference
# ==============================================================================

# OLS --------------------------------------------------------------------------
# We will use the felm function from the LFE package. This function is designed
# to accommodate many fixed effects. The syntax is:
# felm(y~x | fixed effects | instruments | clustering variable, data = dataset)
OLS <- felm(fatal_rate ~ beertax |0|0|0, data = Fatalities)
summary(OLS)

# Positive and significant relationship between beer tax and 
# traffic fatalities. 

# However, default standard errors in R is homoskedastic. 
# Let's try with heteroskedasticity consistent standard errors: 

summary(OLS, robust = T)

# plot the observations and the estimated regression line 
plot(x = as.double(Fatalities$beertax), 
     y = as.double(Fatalities$fatal_rate), 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes",
     #ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(OLS, lwd = 1.5, col="darkred")
legend("topright",lty=1,col="darkred","OLS Regression Line")


# Do we believe that alcohol taxes cause more traffic deaths? 
# Seems like the OLS specification suffers from omitted variable bias

# For example, income:
cor(Fatalities$income, Fatalities$fatal_rate)
cor(Fatalities$income, Fatalities$beertax)

# The omitted bias variable formula suggests that this will lead to positive
# bias in OLS

OLS_income <- felm(fatal_rate ~ beertax + income|0|0|0, data = Fatalities)
summary(OLS_income, robust = T) 
summary(OLS, robust = T)


# Fixed Effects Model ----------------------------------------------------------

# But there are many variables that we could possibly be missing. E.g. drinking
# culture, distribution of rural/urban areas etc

FE_oneway_hc <- felm(fatal_rate ~ beertax + log(income)|state|0|0, data = Fatalities)
summary(FE_oneway_hc, robust = T)

# Heteroskedasticity consistent standard errors are not correct with panel data
# V^FE which we derived in the lectures is equivalent to clustering at the 
# state level. We will return to this in the topic on Inference with Dependent
# Data. 

FE_oneway_hc <- felm(fatal_rate ~ beertax + log(income)|state|0|state, data = Fatalities)
summary(FE_oneway_hc) 
# once we include a clustering variable in felm, summary() automatically 
# produces the appropriate variance 

# Let's add other variables as controls

# Create variable for punishment
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
                                             labels = c("no", "yes")))


# Specification with most plausible controls:
FE_oneway_full <- felm(fatal_rate ~ beertax + drinkage 
                       + punish + miles + unemp + log(income) |state |0| state, data = Fatalities)
summary(FE_oneway_full) # adding other controls makes the effect of beer tax much more negative


# Control for time varying heterogeneity as well:
FE_twoway_full <- felm(fatal_rate ~ beertax + drinkage
                       + punish + miles + unemp + log(income) 
                       + dry+ youngdrivers |state + year |0| state, data = Fatalities)
summary(FE_twoway_full)

# Interestingly, most of the confounding variation is at the state level
# States with more irresponsible drinking also have higher beer taxes?
FE_twoway_timeonly <- felm(fatal_rate ~ beertax + drinkage
                       + punish + miles + unemp + log(income) 
                       + dry+ youngdrivers | year |0| state, data = Fatalities)
summary(FE_twoway_timeonly)

# ==============================================================================
# Summary
# ==============================================================================
 
# Negative effect of alcohol taxes on traffic fatalities
# However, it is estimated imprecisely owever, 
# May not be causal effect of interest
# Many omitted variables that differ across states and change over time
# These biases will not be eliminated by the within transformation



