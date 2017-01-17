
#First load and read the data

load("~/Documents/GitHub/uwcom521-assignments/week_02/week2_dataset-matthew.RData")
matt.wk2 <- week2.dataset
matt.wk3 <- read.csv("~/Documents/GitHub/uwcom521-assignments/week_03/week3_dataset-matthew.csv")

#Let's view the data in a tab

View(matt.wk3)

#Let's do a general summary of the data

summary(matt.wk3) #All the variables return the basic stats, though it seems to me that columns "j", "i", and "k" are logical or categorical data

#Now let go into specific measures

ncol(matt.wk3) #No of columns
nrow(matt.wk3) #No of rows
wk3.range <- apply(matt.wk3, 2, range)
wk3.mean <- apply(matt.wk3, 2, mean)
wk3.sd <- apply(matt.wk3, 2, sd)

#Draw histograms to get a visual sense of the data

hist(matt.wk3$x)
hist(matt.wk3$i)
hist(matt.wk3$j)
hist(matt.wk3$k)
hist(matt.wk3$y)

#Let's compare week two data to this one

wk2.wk3 <- matt.wk3$x == matt.wk2 #All the numbers in mine returns false so they may be similar but definitely not the same numbers

#Let's do some pretty visualization
#"x" and "y" work fine but "i" is a continous variable and so cannot be mapped to shape or color or size
wk.visual <- ggplot(data=matt.wk3) + geom_point() + aes(x=x, y=y, shape=i, color=j, size=k) 

#Do some data cleaning

matt.wk3$i <- as.logical(matt.wk3$i) #Change variable "i" to logical. Interesting that the class test shows "integer" though it is now a logical
matt.wk3$j <- as.logical(matt.wk3$j) #Change vairbale "j" to logical

matt.wk3$k <- as.factor(matt.wk3$k) #Change "k" to factor

levels(matt.wk3$k) <- c("none", "some", "lots", "all") #Rename "k" levels

#Change column "i" observations to NA if they are FALSE

matt.wk3$i[matt.wk3$i == FALSE] <- NA

#Change it back to FALSE

is.na(matt.wk3$i) #Check for all NA available
matt.wk3$i[is.na(matt.wk3$i)] #Subsect all the NA in the variable
matt.wk3$i[is.na(matt.wk3$i)] <- FALSE #Then replace all instances of NA with FALSE

#Finally going to do summary statistics again.

summary(matt.wk3) #The answers have changed and using "na.rm=TRUE" doesn't make a difference

#First, specific measures

ncol(matt.wk3) #No of columns
nrow(matt.wk3) #No of rows
wk3.range <- apply(matt.wk3, 2, range) #Categorical and logical variables do not have summary statistics anymore
wk3.mean <- apply(matt.wk3, 2, mean) #Categorical and logical variables do not have summary statistics anymore
wk3.sd <- apply(matt.wk3, 2, sd) #Categorical and logical variables do not have summary statistics anymore

#Draw histograms to get a visual sense of the data

hist(matt.wk3$x)
hist(matt.wk3$i) #Variables "i", "j" and "k" can no longer create histograms because they are not supposed to anyways.
hist(matt.wk3$j)
hist(matt.wk3$k)
hist(matt.wk3$y)


#Trying the visualization again
#It is interesting that all the variables appear in the visualization, even if they are not "advised."

wk.visual <- ggplot(data=matt.wk3) + geom_point() + aes(x=x, y=y, shape=i, color=j, size=k) 


