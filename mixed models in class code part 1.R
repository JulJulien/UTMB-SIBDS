library(multilevelmod)

# see original data
riesby

# use the condensed version for our analysis
dat = read.csv("C:/Users/cajgarza/Documents/UTMB-SIBDS/riesby.csv")
length(unique(dat$id))

boxplot(hamd~week, data=dat)

library(tidyr)
dat_wide <- dat |>
  pivot_wider(
    names_from = week,      # Column whose values will become new column names
    values_from = hamd    # Column providing the values for the new columns
  )

library(GGally)
library(dplyr)

dtpair = dat_wide %>% select("0", "1", "2", "3", "4", "5")
ggpairs(dtpair)

# line plots for all subjects
interaction.plot(dat$week, dat$id, dat$hamd, legend = F, col=c(1:nrow(dat_wide)))
# stratify by endog
end0 = subset(dat, endog==0)
end1 = subset(dat, endog==1)
interaction.plot(end0$week, end0$id, end0$hamd, legend = F, col=c(1:nrow(dat_wide)))
interaction.plot(end1$week, end1$id, end1$hamd, legend = F, col=c(1:nrow(dat_wide)))

library(lme4)
library(lmerTest)

# include intercept and week as random, use REML method (default)
hamd_full.mod = lmer(hamd~1+week+endog+week:endog+(1+week|id), REML=F, data=dat)
summary(hamd_full.mod)

# include only intercept as random, use REML method (default)
hamd_rint.mod = lmer(hamd~1+week+endog+week:endog+(1|id), REML=F, data=dat)
summary(hamd_rint.mod)

# compare the nested models
anova(hamd_rint.mod, hamd_full.mod, refit=T)

# fit main effects model with intercept and slope as random
hamd_main.mod = lmer(hamd~1+week+endog+(1+week|id), REML=F, data=dat)
summary(hamd_main.mod)

# compare the nested models to test if interaction is significant
anova(hamd_main.mod, hamd_full.mod, refit=T)
