#First load the data
red.dye40 <- read.xls("owan.data.week6.xls")

#Then change the each column into a two-column dataframe
#Control Group
control_group <- red.dye40$X1
control_group_names <- rep("Control",length(red.dye40$X1))
control_group_df <- data.frame(control_group_names, control_group)
control.grp <- data.frame(Group = control_group_names, Lifespan = control_group)

#Low dosage group
low_group <- red.dye40$X2
low_group_names <- rep("Low_Dosage",length(red.dye40$X2))
low_group_df <- data.frame(low_group_names, low_group)
low.dosage.grp <- data.frame(Group = low_group_names, Lifespan = low_group)

#Medium dosage group
medium_group <- red.dye40$X3
medium_group_names <- rep("Medium_Dosage",length(red.dye40$X3))
medium_group_df <- data.frame(medium_group_names, medium_group)
medium.dosage.grp <- data.frame(Group = medium_group_names, Lifespan = medium_group)

#High dosage group
high_group <- red.dye40$X4
high_group_names <- rep("High_Dosage",length(red.dye40$X4))
high_group_df <- data.frame(high_group_names, high_group)
high.dosage.grp <- data.frame(Group = high_group_names, Lifespan = high_group)

Red.Dye40.df <- rbind.data.frame(control.grp, low.dosage.grp, medium.dosage.grp, high.dosage.grp)

#Since it works now, let's do some summary stats
#tapply would be super helpful here 

tapply(Red.Dye40.df$Lifespan, Red.Dye40.df$Group, summary) #summary stats
tapply(Red.Dye40.df$Lifespan, Red.Dye40.df$Group, hist) #histogram to visualize the data

#Find the global mean

mean(Red.Dye40.df$Lifespan, na.rm = TRUE) #75.55263

#t-test between control group and all the other groups
#First, combine data from the other groups together. This turns out to be more complicated than I thought.
t.test(Lifespan ~ Group, data=Red.Dye40.df) #However, because it has more than two levels, it complicates things.

#Anova analysis
summary(aov(Lifespan ~ Group, data=Red.Dye40.df)) #Yes, it could be done by subsetting but that is not the goal of the assignment. 
#I want to be able to do it using the formula notation.





