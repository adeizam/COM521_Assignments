setwd("~/Documents/GitHub/Data_Set")
library(stargazer)
library(readstata13)

#PC0. Load up your dataset as you did in Week 3 PC2.
load("week2_dataset-matthew.rdata")
week2 <- week2.dataset

week3 <- read.csv("week3_dataset-matthew.csv")

View(week3)

#PC1. If you recall from Week PC6, x and y seemed like they linearly related. 
#We now have the tools and terminology to describe this relationship and to estimate just how related they are. 
#Run a t.test between x and y in the dataset and be ready to interpret the results for the class.
t.test(week3$x, week3$y)

#PC2. Estimate how correlated x and y are with each other.

cor(week3$x, week3$y) #0.8482357

#PC3. Recode your data in the way that I laid out in Week 3 PC7.
week3$i <- as.logical(week3$i)
week3$j <- as.logical(week3$j)
week3$k <- as.factor(week3$k)
levels(week3$k) <- c("none", "some", "lots", "all")

#PC4. Generate a set of three linear models and be ready to intrepret the coefficients, standard errors, t-statistics, p-values, and R 2 {\displaystyle \mathrm {R} ^{2}} {\displaystyle \mathrm {R} ^{2}} for each:
  
#(a) y ^ = β 0 + β 1 x + ε 
summary(lm(y ~ x, data=week3))

#(b) y ^ = β0 + β1x + β2i + β3j + ε 

summary(lm(y ~ x + i + j, data=week3))

#(c) y ^ = β0 + β1x + β2i + β3j + βk + ε

summary(lm(y ~ x + i + j + k, data=week3))

##PC5. Generate a set of residual plots for the final model (c) and be ready to interpret your model in terms of each of these:
#First save model (c) as a lm and then apply residuals to it

week3.cmodel <- (lm(y ~ x + i + j + k, data=week3))

cmodels.residuals <- residuals(week3.cmodel)

#(a) A histogram of the residuals.
hist(cmodels.residuals)

#(b) Plot the residuals by your values of x, i, j, and k (four different plots).

plot(week3$x, residuals(week3.cmodel)) #Plot for x residuals
plot(week3$i, residuals(week3.cmodel)) #Plot for i residuals
plot(week3$j, residuals(week3.cmodel)) #Plot for j residuals
plot(week3$k, residuals(week3.cmodel)) #Plot for k residuals

#(c) A QQ plot to evaluate the normality of residuals assumption.

qqnorm(residuals(week3.cmodel)) #Seems pretty normal

##PC6. Generate a nice looking publication-ready table with a series of fitted models and put them in a Word document.
#First, generate the models
model.one <- lm(y ~ x, data=week3)
model.two <- lm(y ~ x + i + j, data=week3)
model.three <- lm(y ~ x + i + j + k, data=week3)

stargazer(model.one, model.two, model.three, type="html")



###Now, lets go back to the Michelle Obama dataset we used last week the week 7 problem set's programming challenges.

##PC7. Load up the dataset once again and fit the following linear models and be ready to interpret them similar to the way you did above in PC4

halloween <- read.dta13("Halloween2012.dta")
View(halloween)

#(a) fruit ^ = β0 + β1obama + ε 

summary(lm(fruit ~ obama, data=halloween))

#(b) Add a control for age and a categorical version of a control for year to the model in (a).

halloween$year <- as.factor(halloween$year)

summary(lm(fruit ~ obama + age + year, data=halloween))

##PC8. Take a look at the residuals for your model in (a) and try to interpret these as you would in PC4 above. What do you notice?

obama.one <- lm(fruit ~ obama, data=halloween)

residuals(obama.one) #This is too much to read so I am going to plot it

plot(residuals(obama.one)) #There are lots of residuals at both extremes

## PC9. Run the simple model in (a) three times on three subsets of the dataset: just 2012, 2014, and 2015. Be ready to talk through the results.

#First, subset the the data

b.2012 <- subset(halloween, year == 2012)
b.2014 <- subset(halloween, year == 2014)
b.2015 <- subset(halloween, year == 2015)

#Now run the model
summary(lm(fruit ~ obama, data=b.2012))

summary(lm(fruit ~ obama, data=b.2014))

summary(lm(fruit ~ obama, data=b.2015))


