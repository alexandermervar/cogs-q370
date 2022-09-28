library(ez) #for doing the ANOVAs
library(dplyr) # great library for massaging data
library(ggplot2)

# I put the data file in a specific folder.  If you don't have the same folder structures as me, you'll have to change the directory path to wherever you put the data file
data<-read.table("/Users/rgoldsto/word_proc_docs/q370/featureSearchResults.csv", stringsAsFactors=TRUE,sep=',', header = TRUE)
data<-filter(data,trial_type=="visual-search-circle") # only include actual feature search trials, not "trials" in which instructions or debriefings were shown
data<-droplevels(data) #eliminate NULLS left over from instructions.  Only include factor levels that actually exist in data
sizes<-c(1,5,15,30) # create a list of the set sizes in their desired order
data$set_size<-factor(data$set_size, levels = sizes) #reorder the four levels of set_size so that they are increasing order
table(data$subject_id) # make sure that everybody did the same number of trials
par(mar = c(4,4,4,4),mfrow=c(1, 1) )  # reset plots just in case it's necessary from whatever was being plotted before.  Quite wide margins on top, left, and bottom for the axes titles

#For looking at whether and how response time is influenced by the experimental variables
searchTimes<-filter(data, correct==1 ) # only include correct trials for response time analyses.
byseveral<-group_by(searchTimes,subject_id,target_presence,set_size,task) #break data down by subject, target presence, set size AND task
rtaverages<-summarize(byseveral,count=n(),rt=mean(rt, trim=0.1),year=max(year)) #show averages for each subject for each cell of the task X set_size X presence design.  Trimmed mean so not too influenced by outliers
View(rtaverages) # if you want to see the whole frame with everybody's averages
hist(rtaverages$count,breaks=40) #make sure that there's enough correct data from each of the cells
rtaverages[rtaverages$count<5,] # report subject_id and condition in which low counts are found.  If everybody supplied enough data, then this will be empty.
unique(rtaverages$subject_id) # tells us how many subjects we have

# cleaning of data - throw out "bad" subjects
unusualsubjects <- rtaverages$subject_id[rtaverages$count < 5 | rtaverages$count > 60 ] # make a list of subjects without enough data or too much data, suggesting that multiple subjects used the same name or that a subject reran the experiment
rtaverages <- filter(rtaverages,!(subject_id %in% unusualsubjects)) # only include data from good subjects.  ! = not.  Put data from acceptable subjects right back in the same data frame
rtaverages<-droplevels(rtaverages) #drops levels (subjects) that are no longer represented in data after exclusion criteria
unique(rtaverages$subject_id) # tells us how many remaining subjects we have

goodData<-filter(data,!(subject_id %in% unusualsubjects)) # make a data frame that only includes data from "good" subjects
goodData<-droplevels(goodData) #eliminate levels of dropped subjects
goodSearchTimes<-filter(goodData,correct==1) # for RT analysis, it is typical to only look at correct trials
hist(searchTimes$rt,breaks=200,xlim=c(0,6000),xlab="RT",ylab="Frequency") # get an overall feel for the RT distribution
hist(filter(searchTimes,task=="conjunctive" & set_size=="30" & target_presence=="absent")$rt,breaks=200,xlim=c(0,6000)) #an example of showing distribution of RTs from just one condition
# Below is code to look overall at speed and accuracy of each subject
bysubject<-group_by(goodData,subject_id) # break down data by subject - notice that we're using ALL data, not only correct trials
averages<-summarize(bysubject,rt=mean(rt,trim=0.1),accuracy=sum(correct=="1")/n(),year=max(year)) #for each subject, record their overall accuracy and RT.  Note, taking of year is just a kludge to get a single value of year for each subject.  But all values of year should be the same for a subject so it doesn't really matter if we use min, max, or mean
plot(averages$rt,averages$accuracy,type="n") # is there a speed-accuracy tradeoff?  As people get faster do they get more error-prone?
text(averages$rt,averages$accuracy,averages$subject_id)
cor.test(averages$rt,averages$accuracy) # is there a speed-accuracy tradeoff?  Tends to be positive - as RT goes up, so does accuracy.  So: yes. People are choosing to be accurate or speedy
averages$color<-recode(averages$year,"2016"="red","2017"="orange","2018"="green","2019"="blue") # Dplyr recode command to convert years to colors, for ease of plotting
plot(averages$rt,averages$accuracy,pch=16,col=averages$color,xlab="Response Time",ylab="Accuracy") # is there a Flynn effect?  Are Q370 students getting faster/more accurate over years?

table1 <- tapply(X=rtaverages$rt,INDEX=list(rtaverages$set_size,rtaverages$target_presence,rtaverages$task),FUN=mean, trim=.1) #apply mean function to RT broken down 3 ways
table1 # show means so that one can begin to interpret the data
group_by(goodSearchTimes,set_size,target_presence,task) %>% summarize(mean(rt,trim=0.1)) # an alternative way to show means broken down by set_size and presence but collapsing over subjects, using dplyr

#Code to conduct actual ANOVA of the Response Times results
model<-ezANOVA(data=XXX,dv=XXX,within=c(XXX,XXX,XXX),wid=XXX) # You need to fill in the XXXs with the correct variable names within the variable containing all of the correct RTs.  conduct a repeated measures ANOVA - dv = dependent variable.  within = a list of all of the within subject variables.  wid = variable that is used to group data by subject
# could include year as a variable with "between=year".  Main effect maybe but would hope there wouldn't be (m)any interactionsw
model # show results of the ANOVA model
table1 <- #<DELETED piece OF CODE> #find breakdown just of setsize and task - less broken down than the above tapply code, obtained just by deleting one item from the INDEX list "INDEX=list(rtaverages$target_presence,rtaverages$set_size,rtaverages$task)" above
table1 #show means so that one can begin to interpret the data.  You'll break down rtaverages in different ways to get the different mean RTs that you need for your report

# The next bit of code is to reproduce Treisman and Gelade's Figure 1, including best lines of fit
rtaverages$set_size_num<-sizes[rtaverages$set_size] # added a new column to rtaverage data frame which is the numeric/continuous version of the nominal/categorical set_size factor which will be useful for predicting RT from set_size 
# You need to fill in the XXXs in the ggplot below.  Think about what you want to appear on Y axes, and what you want broken down by shape and color
ggplot(rtaverages,aes(x=set_size_num,y=XXX,shape=XXX,color=XXX))+ stat_summary(fun=mean,geom="point",aes(size=2))+geom_smooth(method=lm,se=FALSE)+stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.4) + xlab("Set Size") + ylab("Response Time (msec)")+ guides(size=FALSE)
#stat_summary makes a summary of the points, first showing data as points, then adding line of best fit, then adding error bars (95% confidence intervals), then adding labels, then turning off "size" from appearing in legend
ca<-lm(rt~set_size_num,data=filter(rtaverages,task=="conjunctive" & target_presence=="absent")) # if you want to see the actual slope and intercept for one of the lines: conjunective absent trials
cp<-#Deleted piece of code to determine the slope and intercept of the best fitting line for the conjunctive present results

byseveral <- #<DELETED piece OF CODE>  #Should be just like the analogous code above, but remember that we need to include ALL feature search trials, not just the correct trials
pcaverages<-summarize(byseveral,percentCorrect=sum(correct==1)/n()) #gives average accuracy broken down 3 ways. % correct = count of correct trials divided by total number of trials n()
model <- #<DELETED piece OF CODE> #conduct a repeated measures ANOVA - dv = dependent variable.  within = a list of all of the within subject variables.  wid = variable that is used to group data by subject
model # show results of the ANOVA model
table1 <- #<DELETED piece OF CODE>  #apply mean function to percent correct broken down 3 ways
table1 # show means so that one can begin to interpret the data

pcaverages$set_size_num<-sizes[pcaverages$set_size] # added a new column to rtaverage data frame which is the numeric/continuous version of the nominal/categorical set_size factor which will be useful for predicting RT from set_size 
# Plot the results (which turn out to be surprising) that mirror the response times results, but for accuracy instead.  You need to fill in the XXXs below
ggplot(pcaverages,aes(x=XXX,y=XXX,shape=XXX,color=XXX))+ stat_summary(fun=mean,geom="point",aes(size=2))+stat_summary(fun=mean,geom="line")+stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.4) + xlab("Set Size") + ylab("Percent Correct")+ guides(size=FALSE) #stat_summary makes a summary of the points - one for each cell in the design, defaulting to mean


#The following analysis begins to look at learning effects.  Are there improvements over all trials within the experiment?
byseveral<-group_by(goodSearchTimes,task,trial_index) #break data down by task and which trial number.
averages<-summarize(byseveral,rt=mean(rt,trim=.1)) #show averages for each subject for each cell of the task X set_size X presence design
ggplot(averages,aes(x=trial_index,y=rt,colour=task))+geom_point(shape=1,alpha=1/2)+ geom_smooth(method="loess",span=.25, fill="blue",alpha=.35,level=.95,n=25) # build up a ggplot by specifying in the "aesthetics" aes() which are x and y variables and which variable determines what different lines are plotted

#Are there improvements over trials within each block?
goodSearchTimes$trialInBlock<-(goodSearchTimes$trial_index-1) %% 52 # add another colums to describe which trial (e.g. first, second, third...) within a block the data are from.  x %% y gives remainder of x when x is divided by y 
byseveral<-group_by(goodSearchTimes,subject_id,task,trialInBlock) #break data down by subject, schedule, studied AND group.
averages<-summarize(byseveral,rt=mean(rt,trim=.3)) #show averages for each subject for each cell of the task X set_size X presence design
ggplot(averages,aes(x=trialInBlock,y=rt,colour=task))+geom_point(shape=1,alpha=1/2)+ geom_smooth(method="loess",span=.25, fill="blue",alpha=.35,level=.95,n=25)+ylim(0,3000) + xlab("Trial # Within Block") + ylab("Response Time (msec.)") #first define what are x, y, and grouping variables, then add points, then add smoothing line with confidence bands


