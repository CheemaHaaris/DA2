library(tidyverse)
library(lspline)
library(cowplot)
library(boot)
library(estimatr)
library(huxtable)
library(stargazer)
library(modelsummary)
library(fixest)
library(moments)
library(kableExtra)

dataset <- read_csv(url('https://raw.githubusercontent.com/CheemaHaaris/DA2/main/data/morg-2014-emp.csv?token=AVUUXBYSLRF7RRRYTEH4HPDBVHHOS'))


df <- subset(dataset,dataset$occ2012 == 0800 |dataset$occ2012 == 0810 |dataset$occ2012 == 0820 | dataset$occ2012 == 0830| dataset$occ2012 == 0840| dataset$occ2012 == 0850| dataset$occ2012 == 0860| dataset$occ2012 == 0900| dataset$occ2012 == 0910| dataset$occ2012 == 0930| dataset$occ2012 == 0940| dataset$occ2012 == 0950)
df <- filter(df, df$grade92 == 43 | df$grade92 == 44 | df$grade92 == 45 | df$grade92 == 46)

# Recoding Variables - sex = 0 for male

df$sex <- ifelse(df$sex == 1, 0, 1)

df <- df %>% mutate(female=as.numeric(sex==1)) %>%
  mutate(w=earnwke/uhours) %>%
  mutate(lnw=log(w))

df <- df %>% mutate(  ed_BA = as.numeric(grade92 == 43),
                      ed_MA = as.numeric(grade92 == 44),
                      ed_Profess = as.numeric(grade92 == 45),
                      ed_PHD = as.numeric(grade92 == 46))

# EDA

ggplot(data = df,aes(x = occ2012)) +
  geom_histogram(fill= "orangered4", col= "salmon") +
  theme_bw() + 
  labs(title = "Distribution of individuals by occupation")

ggplot(data = df, aes(x = female)) +
  geom_bar(fill= "orangered4", col= "salmon") +
  theme_bw() +
  labs(title = "Distribution of individuals by gender")


P95 <- function(x){ quantile(x,.95,na.rm=T)}
datasummary( earnwke + uhours + w + grade92 ~ Mean + SD + Min + 
               Max + Median + N , data = df )


## Regressions
# Unconditional gender gap

reg1 <- lm_robust(lnw ~ female, data = df, se_type = "HC1")

reg2 <- lm_robust(w ~ female, data = df, se_type = "HC1" )

huxreg(reg1, reg2, statistics = c(N = "nobs", R2 = "r.squared"))

# Gender gap with the level of education

reg3 <- lm_robust(lnw ~ female + ed_Profess + ed_MA + ed_PHD, data = df, se_type = "HC1")
reg4 <- lm_robust(lnw ~ female + age + ed_Profess + ed_MA + ed_PHD, data=df, se_type = "HC1")
huxreg(reg1, reg3, reg4, statistics = c(N = "nobs", R2 = "r.squared"))


## Fitted values and residuals

df$lnw_yhat <- reg4$fitted.values
df$lnw_resid <- df$lnw - df$lnw_yhat

#Graphs
ggplot(data = df, aes(x = lnw_yhat, y = lnw)) +
  geom_point( size = 1.2, col='red', alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="lm",formula=y~x,se=F) +
  labs(x = "ln(predicted wage) ",y = "ln(wage)") +
  theme_bw()

ggplot(data = df, aes(x = lnw_yhat, y = lnw_resid)) +
  geom_point(color = 'red', size = 2 ) + 
  geom_smooth( method="lm", colour='blue', se=F , formula = y ~ x) +
  labs(x = "ln(predict wage)",y = "Residuals")+
  theme_bw()








