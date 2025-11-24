library(lfe)

data <- read.csv("mqy.csv")

# Column 1 in MQY with HC1
yearfe <- felm(ldeath_b ~ lgrain_pred_famdum5860 + lgrain_pred +
                  lurbpop + ltotpop | year | 0 | 0, data = data)
summary(yearfe, robust = T)

# Column 1, clustered at province level
yearfe <- felm(ldeath_b ~ lgrain_pred_famdum5860 + lgrain_pred +
                 lurbpop + ltotpop | year | 0 | province, data = data)
summary(yearfe)

# Column 1, clustered at province + time level
yearfe <- felm(ldeath_b ~ lgrain_pred_famdum5860 + lgrain_pred +
                 lurbpop + ltotpop | year | 0 | province + year, data = data)
summary(yearfe)

# Number of provinces:
length(unique(data$province))

# Number of time periods:
length(unique(data$year))


# Can also consider two-way fixed effects specification:
twfe <- felm(ldeath_b ~ lgrain_pred_famdum5860 + lgrain_pred +
               lurbpop + ltotpop | year + province | 0 | 0, data = data)
summary(twfe)