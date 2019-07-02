### TRAFFIC FATALITIES DUE TO DRINKING PANEL DATA REGRESSION ### 

library(AER)

data("Fatalities")
is.data.frame(Fatalities)
dim(Fatalities)

head(Fatalities)
summary(Fatalities)
summary(Fatalities[,c(1,2)])

#Panel Data is Balanced since the summary results indicate that 7 x 48 = 336 observations exists. 
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

#Subset Data for each year
Fatalities1982 <- subset(Fatalities, year == 1982)
Fatalities1985 <- subset(Fatalities, year == 1985)
Fatalities1988 <- subset(Fatalities, year == 1988)

#Create one-variable models 
fatal1982 <- lm(fatal_rate ~ beertax, data = Fatalities1982)
summary(fatal1982)

#t-test for coefficients = allows to obtain coefficients based on robust standard errors 
coeftest(fatal1982, vcov. = vcovHC, type = "HC1")

plot( x = Fatalities1982$beertax,
      y = Fatalities1982$fatal_rate,
      xlab = "Beer Tax in 1982 (dollars)",
      ylab = "Fatal Rate in 1982",
      main = "Traffic Fatality Rates and Beer Taxes in 1982",
      ylim = c(0, 4.5),
      pch = 20,
      col = "steelblue")

abline(fatal1982, lwd = 1.5)

## 1985 Data Coefficients 
fatal1985 <- lm(fatal_rate ~ beertax, data = Fatalities1985)
summary(fatal1985)
coeftest(fatal1985, vcov. = vcovHC, type = "HC1")

plot( x = Fatalities1985$beertax,
      y = Fatalities1985$fatal_rate,
      xlab = "Beer Tax in 1985 (dollars)",
      ylab = "Fatal Rate in 1985",
      main = "Traffic Fatality Rates and Beer Taxes in 1985",
      ylim = c(0, 4.5),
      pch = 20,
      col = "steelblue")

abline(fatal1985, lwd = 1.5)

## 1988 Data Coefficients
fatal1988 <- lm(fatal_rate ~ beertax, data = Fatalities1988)
summary(fatal1988)
coeftest(fatal1988, vcov. = vcovHC, type = "HC1")

plot( x = Fatalities1988$beertax,
      y = Fatalities1988$fatal_rate,
      xlab = "Beer Tax in 1988 (dollars)",
      ylab = "Fatal Rate in 1988",
      main = "Traffic Fatality Rates and Beer Taxes in 1988",
      ylim = c(0, 4.5),
      pch = 20,
      col = "steelblue")

abline(fatal1988, lwd = 1.5)


#Before and After Comparisons 

diff_fatal_rate <- Fatalities1982$fatal_rate - Fatalities1988$fatal_rate
diff_beer_tax <- Fatalities1982$beertax - Fatalities1988$beertax

fatal_diff <- lm(diff_fatal_rate ~ diff_beer_tax )
coeftest(fatal_diff, vcov = vcovHC, type = "HC1")

plot( x = diff_beer_tax,
      y = diff_fatal_rate,
      xlab = "Change in Beer Tax",
      ylab = "Change in Fatal Rate",
      main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
      xlim = c(-0.6,0.6),
      ylim = c(-1.5, 1),
      pch = 20,
      col = "steelblue")

abline(fatal_diff, lwd = 1.5)

mean(Fatalities$fatal_rate)

####################################################################################################
##                        
##  Rising beer tax by 1$ causes traffic fatalities to decrease by 1.04 per 10000ppl
##  This is very large! check the mean fatality rate. Mean fatality rate is 2.0404
##  This means that, there are some unobserved effects. (Omitted variables).  
##  There are 2 methods used in panel data to account for those omitted variables. 
##  Method 1: Dummy Variables and Method 2: Using the demeaned data
##  Method1 is straightforward. Method2 measures the variation from the mean than estimates
##  coefficients accordingly. 
##
##################################################################################################


#Fixed Effects Linear Regression - Method 1: Dummy Variable to Measure fixed effects of each state 

fatal_fixed_effect_lm <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
fatal_fixed_effect_lm

##There is a 2nd method to find the fixed effects coefficient. Method 2: Obtained the demeaned data
## ave function is good for computing group averages

Fatalities_demeaned <-with(Fatalities,
                           data.frame(fatal_rate = fatal_rate - ave(fatal_rate,state),
                                      beer_tax = beertax - ave(beertax, state)))

summary(lm(fatal_rate ~ beer_tax - 1, data = Fatalities_demeaned))

#Fixed Effects Panel Linear Regression

library(plm)
fatal_fixed_effect_plm <- plm(fatal_rate ~ beertax,
                           data = Fatalities,
                           index = c("state", "year"),
                           model = "within")

coeftest(fatal_fixed_effect_plm, vcov. = vcovHC, type = "HC1")

#Fixed Time Effects Linear Regression - Method 1: Dummy Variable to Measure fixed effects of each state and year 
#!!! the "lm" function converts factor type variables into dummies automatically! 

class(Fatalities$year)
class(Fatalities$state)

fatal_time_fixed_effect_lm <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities)
fatal_time_fixed_effect_lm

fatal_time_fixed_effect_plm <- plm(fatal_rate ~ beertax,
                              data = Fatalities,
                              index = c("state", "year"),
                              model = "within",
                              effect = "twoways")

coeftest(fatal_time_fixed_effect_plm, vcov. = vcovHC, type = "HC1")



####################################################################################################
##                        
## The estimated beertax coefficient is -0.63. Meaning, 1$ increase in beertax will led to 
## 0.63 decrease in the traffic fatality rate which is still un realistic. Other omitted
## variables are the economic conditions and traffic rules. We can incorporate them in 
## our model. Unemployment Rate, Per capita income, miles ( state average miles per driver), 
## Drinking Age(18-19-20-21). Punishment ( whether drunk dirving is severely punished or not)
##
##################################################################################################

##Organized the data for drinking age. Data is not discrete, therefor we should convert numbers to ranges 
## [18,19) , [19,20), [20,21), [21,22) -- min legal drinking age 
## cut function divides range into intervals, the leftmost interval is the level 1

Fatalities$drinkagec <- cut(Fatalities$drinkage,
                            breaks = 18:22,
                            include.lowest = TRUE,
                            right = FALSE)

class(Fatalities$drinkagec)
summary(Fatalities)
str(Fatalities)


## set the [21,22] as the base level 

Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")

class(Fatalities$drinkagec)
summary(Fatalities)
str(Fatalities)


## Organize the Punishment Data 

Fatalities$punish <- with(Fatalities, factor( jail =="yes" | service == "yes",
                                             labels = c("no","yes")))

## this is note a matrix: Fatalities_1982_1988 <- with(Fatalities, year == 1982 | year == 1988) 

Fatalities_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988),]

## Estimate the all 7 models using plm()

fatalities_mod1 <- lm(fatal_rate ~ beertax, data = Fatalities)

fatalities_mod2 <- plm(fatal_rate ~ beertax + state, data = Fatalities)

fatalities_mod3 <- plm(fatal_rate ~ beertax + state + year, 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod4 <- plm(fatal_rate ~ beertax + state + year + drinkagec
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod5 <- plm(fatal_rate ~ beertax + state + year + drinkagec
                       + punish + miles, 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod6 <- plm(fatal_rate ~ beertax + state + year + drinkage
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod7 <- plm(fatal_rate ~ beertax + state + year + drinkagec
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities_1982_1988)

fatalities_mod8 <- plm(fatal_rate ~ beertax + state + year + drinkagec
                       + punish + miles, 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities_1982_1988)


# Create a regression table 

library(stargazer)

rob_se <- list(sqrt(diag(vcovHC(fatalities_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod4, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod5, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod6, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod7, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod8, type = "HC1")))
               )

stargazer(fatalities_mod1,fatalities_mod2,fatalities_mod3,fatalities_mod4,
          fatalities_mod5,fatalities_mod6,fatalities_mod7,fatalities_mod8,
          digits = 3,
          header = FALSE,
          type = "html",
          out = "file:///Users/aysetugbaozturk/Documents/Fatalities_Regression_Table.html",
          se = rob_se,
          title = " Linear and Panel Regression Models of Traffic Fatalities",
          model.numbers = FALSE,
          column.labels = c("(1)","(state)","(state + year)","(4)","(5)","(6)","(7)","(8)"))


