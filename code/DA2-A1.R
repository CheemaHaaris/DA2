library(tidyverse)
library(lspline)
library(cowplot)
library(boot)
library(estimatr)
library(huxtable)
library(stargazer)
library(modelsummary)

dataset <- read.csv(url('https://raw.githubusercontent.com/CheemaHaaris/DA2/main/data/morg-2014-emp.csv'))

# subsetting for analysts in financial specialist


df <- subset(dataset,dataset$occ2012 == 0800 |dataset$occ2012 == 0810 | dataset$occ2012 == 0820 | dataset$occ2012 == 0830 | dataset$occ2012 == 0840| dataset$occ2012 == 0850| dataset$occ2012 == 0860| dataset$occ2012 == 0900| dataset$occ2012 == 0910| dataset$occ2012 == 0930| dataset$occ2012 == 0940| dataset$occ2012 == 0950)
df <- filter(df, df$grade92 == 43 | df$grade92 == 44 | df$grade92 == 45 | df$grade92 == 46)

# Recoding Variables - sex = 0 for male

df$sex <- ifelse(df$sex == 1, 0, 1)

df <- df %>% mutate(female=as.numeric(sex==1)) %>%
  mutate(w=earnwke/uhours) %>%
  mutate(lnw=log(w))


# Unconditional gender gap

reg1 <- lm_robust(lnw ~ female, data = df, se_type = "HC1")
summary(reg1)

#Interpretation to be done


# Gender gap with the level of education

df <- df %>% mutate(  ed_MA=as.numeric(grade92==44),
                      ed_Profess = as.numeric(grade92==45),
                      ed_PhD = as.numeric(grade92==46))

reg2 <- lm_robust(lnw ~ female, data=df,se_type = "HC1")
reg3 <- lm_robust(lnw ~ female + ed_Profess + ed_PhD, data=df, se_type = "HC1")
reg4 <- lm_robust(lnw ~ female + ed_MA + ed_PhD, data=df, se_type = "HC1")

huxreg(reg2, reg3, reg4, statistics = c(N = "nobs", R2 = "r.squared"))


