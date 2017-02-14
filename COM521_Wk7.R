

setwd("~/Documents/GitHub/Data_Set")
library(readstata13)
library(gmodels)

#Question 1
#First read in the data
halloween <- read.dta13("Halloween2012.dta")

obama.table <- table(halloween$obama, halloween$fruit)

prop.test(obama.table)
chisq.test(obama.table)
#Both returned: X-squared = 1.8637, df = 1, p-value = 0.1722
#This doesn't seem to indicate dependence because of high p-value


#Question 2 First create the dataset

mako <- c(42, 31)
tommy <- c(19, 14)

com.dat <- cbind.data.frame(mako, tommy)

rownames(com.dat) <- c("DayOne", "DayTwo")

#prop.test(31, 42, .95) I initially tried them individually but discovered it won't address the question
#prop.test(14, 19, .95) 

prop.test(com.dat$tommy, com.dat$mako, correct = FALSE)

#Output 
#X-squared = 4.2475e-05, df = 1, p-value = 0.9948
#95 percent confidence interval:
#  -0.2302034  0.2317395 
#sample estimates:
#  prop 1    prop 2 
#0.4523810 0.4516129 = both have about the same attrition rate 
# Very high p-value indicating no dependence


chisq.test(com.dat)
# X-squared = 3.0045e-31, df = 1, p-value = 1
#Because p-value is so high, there doesn't seem to be a dependence


#Question 3

#First read the data in

lily <- read.dta13("lilypad_anonymized.dta")
View(lily) #View the data
#Reproduct table of customer by gender
customers.table <- table(lily$gender, lily$order_type)
prop.customer.table <- prop.table(customers.table) * 100

#Reproduce table of US customers only
#First add a new column for US customers only
lily$US <- lily$country == "United States"

#Then subset only US customers and do necessary tests
lily.us <- subset(lily, US == "TRUE")
us.customers <- table(lily$gender, lily$order_type)
prop.us.customers <- prop.table(us.customers) * 100

#b Run Chi Sqaures test on both tables

all.customers.test <- chisq.test(customers.table) # X-squared = 644.3, df = 4, p-value < 2.2e-16
us.customers.test <- chisq.test(us.customers) # X-squared = 644.3, df = 4, p-value < 2.2e-16

#c Run Cross Tables 
cross.tables.all <- CrossTable(customers.table)
cross.tables.us <- CrossTable(us.customers)

#d Write tables to file

write.table(cross.tables.all, "Final Cross Tables") #Since it doesn't open well in my computer, 
                                                      #I will also save a csv, just in case.

write.csv2(cross.tables.all, "Final Cross Tables")

# US Cross Tables
write.table(cross.tables.us, "US Cross Tables") 
write.csv2(cross.tables.us, "US Cross Tables")

