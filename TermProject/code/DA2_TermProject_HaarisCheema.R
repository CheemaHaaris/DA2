#####################################
### Term Project - Coding 1 & DA2 ###
#####################################

library(modelsummary)
library(fixest)
library(lubridate)
library(raster)
library(tidyverse)
library(fixest)
library(lspline)
library(corrplot)
library(car)
library(estimatr)
library(gridExtra)

##################################################################################################################################

# Clearing the environment

rm(list= ls())

## Loading and combining the weather data

# loading the first city's weather data

weather <- read_csv(url('https://raw.githubusercontent.com/CheemaHaaris/DA2/main/TermProject/data/raw/weather/bournemouth1.csv'))

# saving the list of cities to a vector

teams <- c('bournemouth2','wolverhampton1','wolverhampton2','watford1','watford2',
           'southampton','selhurst1','selhurst2','newcastle','manchester','london','liverpool','leicester1',
           'leicester2', 'huddersfield','cardiff','burnley2','burnley1', 'brighton2', 'brighton1')

# defining the common start to all csv's names

urlstart <- 'https://raw.githubusercontent.com/CheemaHaaris/DA2/main/TermProject/data/raw/weather/'

# defining the common end to all csv's names

urlend <- '.csv'

# defining the urls for city data

url2 <- paste0(urlstart,teams,urlend)

# looping through the city csv's and row binding it to the weather df

for (i in 1:length(url2)) {
  weather <- rbind(read_csv(url2[i]), weather)
}

# selecting the appropriate columns from the weather df

weather <- weather[,c(1,2,11)]

# renaming the date and name variables

weather <- weather %>% rename( date = datetime,
                               City = name)

# changing city names to lower case to facilitate the join

weather$City <- tolower(weather$City)

# changing date format to character to facilitate the join

weather$date <- as.character(weather$date)


#############################################################################################################################
## Loading, formatting and merging the cities and football data

# Loading the cities data

cities <- read_csv(url('https://raw.githubusercontent.com/CheemaHaaris/DA2/main/TermProject/data/raw/cities/cities.csv'))

# Loading the football data (df)

df <- read_csv(url('https://raw.githubusercontent.com/CheemaHaaris/DA2/main/TermProject/data/raw/football/epl2019.csv'))

# Checking the df at a glance 

summary(df)

glimpse(df)

# Renaming the home team name variable to facilitate the join

df <- df %>% rename( Team = home_team_name)

# Joining the cities dataframe with the main df on the basis of the common 'Team' variable

df <- inner_join(df, cities, by = 'Team') 

# Changing city contents to lower case to facilitate the join

df$City <- tolower(df$City)

# Creating new variables: total corners, total yellow cards, total red cards, total shots, total shots on target

df <- df %>% mutate(cnr = away_team_corner_count + home_team_corner_count,
                    yc = away_team_yellow_cards + home_team_yellow_cards,
                    rc = away_team_red_cards + home_team_red_cards,
                    shots = away_team_shots + home_team_shots,
                    shots_t = away_team_shots_on_target + home_team_shots_on_target )

# Renaming the dependent variable

df <- df %>% rename( tgc = total_goal_count)  

# Selecting relevant columns into a new dataframe

football <- df[c(2,15,65:70)]

# Changing the date format to bring it to the same format as in the weather data

football$date <- paste0(substr(football$date_GMT,1,6),',',substr(football$date_GMT,7,12))

football$date <- format(as.Date(football$date, format = "%b %d, %Y"), "%Y/%m/%d")

football$date <- paste0(substr(football$date,1,4), '-',substr(football$date,6,7), '-',substr(football$date,9,10))

# Changing one city's name to bring it to the same format as in the weather data

football$City <- sub("burnley, lancashire", "burnley", football$City)

# Converting red cards to binary 

football$rc <- ifelse(football$rc < 1, 0, 1)

############################################################################################################################

# Merging the weather data with the football data

final_df <- inner_join(football, weather, by=c("date","City"))

# Converting rain as a binary variable for precipitation 

final_df$rain <- ifelse(final_df$precip > 0 , 1,0)

# Viewing the data at a very holistic level

summary(final_df)

glimpse(final_df)

table(is.na(final_df))

############################################################################################################################
## Summary statistics and Exploratory Data Analysis 

# Defining the 95th percentile function
P95 <- function(x){ quantile(x,.95,na.rm=T)}

# Running the data summary
datasummary(tgc + rain  + cnr + yc + rc + shots + shots_t  ~ Min + Max + P25 + Median + P75 + Mean + SD + P95 + N, data = final_df, title = 'Descriptive statistics' ) %>% 
  kableExtra::kable_styling(latex_options = "hold_position")

#################################################################################################################################

## Adding custom theme

source('https://raw.githubusercontent.com/CheemaHaaris/DA2/main/TermProject/code/theme_function.R')
theme_set(theme_custom())

#################################################################################################################################

## Variable Distributions # ADD LINES TO THE HISTOGRAMS


# Distribution for total goals 

h1 <- ggplot(data = final_df) + 
        geom_histogram( aes( x = tgc ) , fill = 'blue', col = 'black', binwidth = 1, alpha = 0.5)  +
          labs( x = "Total goal count", y = "Relative Frequency")

# Distribution for daily precipitation (in mm) - try logs? - fix start

h2 <- ggplot(data = final_df) + 
       geom_histogram(aes(x = rain) , fill = 'blue', col = 'black', binwidth = 0.5, alpha = 0.5) +
        labs( x = "Rain", y = "Relative Frequency")

# Distribution for number of corners

h3 <- ggplot(data = final_df) + 
        geom_histogram( aes( x = cnr ) , fill = 'blue', col = 'black', binwidth = 2, alpha = 0.5) +
         labs( x = "Number of Corners", y = "Relative Frequency")


# Distribution for number of yellow cards

h4 <- ggplot(data = final_df) + 
        geom_histogram( aes( x = yc ) , fill = 'blue', col = 'black', binwidth = 1, alpha = 0.5) +
          labs( x = "Number of Yellow Cards", y = "Relative Frequency")


# Distribution for number of red cards

h5 <- ggplot(data = final_df) + 
        geom_histogram( aes( x = rc ) , fill = 'blue', col = 'black', binwidth = 0.5, alpha = 0.5) +
          labs( x = "Number of Red Cards", y = "Relative Frequency")


# Distribution for number of shots in a match 

h6 <- ggplot(data = final_df) + 
        geom_histogram( aes( x = shots ) , fill = 'blue',col = 'black', binwidth = 3 , alpha = 0.5) +
          labs( x = "Total Number of Shots", y = "Relative Frequency")


# Distribution for number of shots on target in a match

h7 <- ggplot(data = final_df) + 
        geom_histogram( aes( x = shots_t) , fill = 'blue',col = 'black', binwidth =  3 , alpha = 0.5) +
          labs( x = "Total Number of Shots on Target", y = "Relative Frequency")

grid.arrange(h1, h2, h3, h4, h5, h6, h7, ncol = 2)

###########################################################################################################################

## Checking association between tgc and all independent vars through lowess curves

a1 <- ggplot(data = final_df, aes(x= rain, y= tgc)) +
  geom_smooth(method="loess", formula = y ~ x)  +
    geom_point(col="red") +
      annotate("text", x = 0.5, y = 4, label = "No obvious association observed", size=4)+
        scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.5))+
          scale_y_continuous(expand = c(0.01,0.01), limits = c(0,8), breaks = seq(0,8,2)) +
            labs(x = "Rain",y = "Total Number of Goals")



a2 <- ggplot(data = final_df, aes(x= cnr, y= tgc)) +
  geom_smooth(method="loess", formula = y ~ x) +
    geom_point(col="red") +
      annotate("text", x = 10, y = 4.5, label = "Linear negative association observed", size=4) +
        scale_x_continuous(expand = c(0.01,0.01), limits = c(2,22), breaks = seq(2,22,4)) +
          scale_y_continuous(expand = c(0.01,0.01), limits = c(0,8), breaks = seq(0,8,2)) +
            labs(x = "Number of Corners",y = "Total Number of Goals")


a3 <- ggplot(data = final_df, aes(x= yc, y= tgc)) +
  geom_smooth(method="loess", formula = y ~ x) +
    geom_point(col= "red") +
      annotate("text", x = 4.5, y = 4.5, label = "Changes in slope direction, need to add knots at yc = 4 and 6 for lsp", size=3)+
        scale_x_continuous(expand = c(0.01,0.01), limits = c(0,9), breaks = seq(0,9,3))+
          scale_y_continuous(expand = c(0.01,0.01), limits = c(0,8), breaks = seq(0,8,2)) +
            labs(x = "Number of Yellow Cards",y = "Total Number of Goals") 
  
  
a4 <- ggplot(data = final_df, aes(x= shots, y= tgc)) +
  geom_smooth(method="loess", formula = y ~ x) +
  geom_point( col = "red") +
  annotate("text", x = 16, y = 3.5, label = "More or less linear positive association", size=4) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(7,34), breaks = seq(7,34,3))+
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,8), breaks = seq(0,8,2)) +
  labs(x = "Total Number of Shots",y = "Total Number of Goals ")  
  

a5 <- ggplot(data = final_df, aes(x = shots_t, y = tgc)) +
  geom_smooth(method="loess", formula = y ~ x) +
    geom_point(col = "red") +
      annotate("text", x = 16, y = 5.7, label = "Change in slope direction, need to add knot here for lsp", size=4) +
        scale_x_continuous(expand = c(0.01,0.01), limits = c(2,22), breaks = seq(2,22,4))+
         scale_y_continuous(expand = c(0.01,0.01), limits = c(0,8), breaks = seq(0,8,2)) +
            labs(x = "Shots on target",y = "Total Number of Goals") 

grid.arrange(a1,a2,a3,a4,a5, ncol = 2)

# Creating squared term for shots_t based on pattern of association

final_df$shots_t_sq <- (final_df$shots_t)^2

#################################################################################################################################
## Examining the correlation coefficients between the causal variable and the conditioning variables

# Subsetting the data for the correlations

cor_data <- (final_df[,c(11,4:8)])

# Creating a correlation matrix

res <- cor(cor_data)

# Displaying the matrix with 2 decimal places

round(res, 2)

# Visualizing the correlation

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Independent variable regressions

model1 <- lm(rain ~ cnr + yc + rc + shots + shots_t , data = final_df)
model2 <- lm(cnr ~ rain + yc + rc + shots + shots_t , data = final_df)
model3 <- lm(yc ~ rain + cnr +rc +shots + shots_t, data = final_df)
model4 <- lm(rc ~ rain + cnr + yc + shots + shots_t, data = final_df)
model5 <- lm(shots ~ rain + cnr + yc + rc + shots_t, data = final_df)
model6 <- lm(shots_t ~ rain + cnr + yc + rc + shots, data = final_df)

#vif(model1)
#vif(model2)
#vif(model3)
#vif(model4)
#vif(model5)
#vif(model6)



#################################################################################################################################  

## Regressions

# 5 models

reg1 <- feols( tgc ~ rain + cnr + yc + rc + shots + shots_t , data = final_df, vcov = 'hetero')

reg2 <- feols( tgc ~ rain + cnr + yc + rc  + shots_t  , data = final_df , vcov = 'hetero' )

reg3 <- feols( tgc ~ rain + cnr + lspline(yc,c(4,6)) + rc + shots + lspline(shots_t,17)  , data = final_df , vcov = 'hetero' )

reg4 <- feols( tgc ~ rain + cnr + lspline(yc,c(4,6)) + rc  + lspline(shots_t,17)  , data = final_df , vcov = 'hetero' )

reg5 <- feols( tgc ~ rain + cnr + lspline(yc,c(4,6)) + rc + shots + shots_t_sq  , data = final_df , vcov = 'hetero' )


# Displaying all regressions for comparison

etable(reg1, reg2,reg3, reg4, reg5)

# Inference Testing for chosen model

summary(reg4)


#################################################################################################################################

## Rough Work - Feel free to ignore this part. 

# Splines - counts in each category

final_df$check1 <- ifelse(final_df$shots_t < 17, 0, 1)

table(final_df$check1)  # only 4.2 % of matches have shots > 17

final_df %>% filter(final_df$yc >= 6) # 226 obs with less than 4 yc, 109 obs for b/w 4 and 6, 45 obs with more than 6

