library(dplyr) # great library for massaging data
library(ggplot2) #library for making great plots
library(ez) # library for doing ANOVA statistics easily


#For all dplyr commands, first argument is the data frame, and subsequent arguments describe what to do with data, resulting in a new dataframe
data = read.csv(file="exp1.csv",header=T,sep="") # read in data with headers for column names.  You'll need to change the directory path to point to where you put the file on your computer.
head(data) #show what first 10 rows of data look like, just to get oriented
dim(data) #tells us how many rows and columns data has
length(unique(data$subject)) # report number of subjects in experiment
table(data$subject) #show counts for each value of subject (i.e. for each subject) - make sure that a subject didn't quit out early

#The following are examples of basic data munging commands in dplyr
filtered<-filter(data,subject==2 & trial_type=="test") # == for logical equals.  Returns only rows satisfying the condition.  Use | for OR
ordered<-arrange(filtered,desc(trial)) #arrange filtered in descending order of trials
select(ordered,RT,hit) #select only RT and hit columns from ordered.  Unlike "filter" because it takes a subset of the columns, whereas filter takes a subset of rows
addedInfo<-mutate(data,trialwiblock=trial %% 24) # add a new column to original data based on trial
summarize(data,mean(RT)) # just gives mean of entire dataset.  Same as mean(data$rt) but useful with other aggregating commands

bysubject<-group_by(data,subject) #create separate groups of data, one for each subject
print(bysubject)
averages<-summarize(bysubject,count=n(),rt=mean(RT),hit=mean(hit)) #gives average accuracy and RT per subject, plus the counts
print(averages)
byseveral<-group_by(data,subject,schedule,group,studied,trial_type) #break data down more, by subject, schedule, studied AND group.  Even though group never changes within subject, it can be listed so that the variable is included
averages<-summarize(byseveral,rt=mean(RT),hit=mean(hit))
#useful functions in addition to mean: median, max, min, n,n_distinct,sum,sd,IQR
badsubjects <- averages$subject[averages$hit < 0.5] # make a list of bad subjects.  Could argue that bad subjects are < .5
filtered <- filter(averages,!(subject %in% badsubjects)) # only include data from good subjects.  ! = not
testonly<-filter(filtered,trial_type=="test") # only look at test data, not training data
table1 <- tapply(X=testonly$hit,INDEX=list(testonly$group,testonly$schedule,testonly$studied),FUN=mean) #apply mean function to accuracy (hits) broken down 3 ways
nums<-c(2,19,3,20,4,21,5,22) #a more elementary example to show what tapply does - it applies function to a list, breaking it down by an index
tapply(nums,INDEX=c(1,2,1,2,1,2,1,2),FUN=mean) #produces means for each element of INDEX, basically breaking nums down into two groups

#in ggplot, first specify data (testonly), then aes (for aesthetics) specifies variables. coord_cartesian() to limit Y axis to small range, stat_summary to use bars to show means.  Then, a second stat_summary to plot error bars, "dodged" so that they don't overlap
ggplot(averages,aes(x=schedule,y=hit,fill=group))+coord_cartesian(ylim = c(0.9, 1))+ stat_summary(fun=mean,geom="bar",position=position_dodge(width=0.9))+stat_summary(fun.data=mean_cl_normal,geom="errorbar",position=position_dodge(0.9))+xlab("Schedule")+ylab("Percent Correct")
# Second example, showing how to break down the data still further, with a facet for each kind of stimulus (studied or novel)
ggplot(averages,aes(x=schedule,y=hit,fill=group))+coord_cartesian(ylim = c(0.9, 1))+ stat_summary(fun=mean,geom="bar",position=position_dodge(width=0.9))+stat_summary(fun.data=mean_cl_normal,geom="errorbar",position=position_dodge(0.9))+xlab("Schedule")+ylab("Percent Correct")+facet_wrap(~studied)
# example of line graph - almost exactly the same except "bar" -> "point" and add lines 
ggplot(averages,aes(x=schedule,y=hit,color=group,group=group,shape=group))+coord_cartesian(ylim = c(0.9, 1))+ stat_summary(fun=mean,geom="point",aes(size=3))+stat_summary(fun=mean,geom="line")+stat_summary(fun.data=mean_cl_normal,geom="errorbar")+xlab("Schedule")+ylab("Percent Correct")+ theme (text=element_text(size=20))+guides(size = FALSE)
print(averages)

#Note, need to force testonly as a real data.frame because it comes out of dplyr in a non-standard data.frame format
ezANOVA(data=data.frame(testonly),dv=hit,within=c(schedule,studied), between=c(group),wid=subject) # conduct a repeated measures ANOVA - dv = dependent variable.  within = a list of all of the within subject variables.  wid = variable that is used to group data by subject

#How many hotdogs my imagined friends can eat when their parts are separated or combined
hotdogs<-data.frame(competitor=c("fred","jane","kayla","sebastian"),separated=c(8,10,6,6),combined=c(5,7,4,6))

#reshape data from wide to long. data=hotdogs, varying = multiple variables measured per subject, v.name=name of variable combined by multiple variables
#timevar = superordinate name for all combined variables, times = names given to different variables, direction=long because all variables now in one column
longhotdogs<-reshape(hotdogs,varying=c("separated","combined"),v.name="eaten",timevar="style",times=c("separated","combined"),direction="long")

#reshape data from wide to long.  idvar = variables that don't vary within subject, timevar=variables to break into different columns
widehotdogs<-reshape(longhotdogs,idvar="competitor",timevar="style",direction="wide")
