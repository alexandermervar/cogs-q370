library(ez) #for doing the ANOVAs
library(dplyr) # great library for massaging data
library(ggplot2) # a great package for making pretty plots
library(car) # to use recode function 
library(reshape2) #To convert "long format" in R (all dependendent variables in one column/variable) to wide format (different groups in different columns)

# I put the data file in my Q370 file.  You'll have to change the directory path to wherever you put the data file
data<-read.table("/Users/rgoldsto/word_proc_docs/q370/wordsup.csv", sep=',', header = TRUE) # read data from experiment.  Files includes header for variables in the first row
head(data) # see what all of the variables are called, and see what beginning of data looks like
data<-filter(data,trial_type=="multi-stim-multi-response") # only include trials from experiment itself, not instructions or debriefing
data<-droplevels(data) #eliminate NULLS left over from instructions.  Only include factor levels that actually exist in data
table(data$subject_id) # make sure that everybody did the same number of trials.  They didn't.  We'll get rid of them in a bit

class(data$resptime) # this is just to show you what the old variable's type was.  It is a factor - not a continuously valued variable
data$rt<-as.numeric(data$resptime)
#data$rt<-as.numeric(levels(data$resptime))[data$resptime] #ugly code needed because Dplyr took perfectly good RTs and converted them to factors.  To convert back to a real number, this code is needed
class(data$rt) # a proper, continuously valued dependent variable
with(data,sort(table(duration))) #look to see if people used other durations than the recommended 32

length(unique(data$subject_id)) # to reveal how many subjects we have
byseveral <- group_by(data,subject_id)
# First, lets create a summary for each subject, to see who we want to throw out
# Each subject will give us a count of all of their data, their accuracy, and the last segment with the which.max command is to find the value of duration that is most common for each person, which in our case will always be the ONLY value of duration for a subject
subjectSummary <- summarize(byseveral,count=n(),accuracy=sum(correctness=="1")/n(),duration = as.numeric(names(table(duration))[which.max(table(duration))])) 
hist(subjectSummary$count) #notice that some subjects didn't give us complete data
plot(subjectSummary$duration,subjectSummary$accuracy) # do people differ in how easy/challenging they want their task to be?
unusualsubjects <- subjectSummary$subject_id[subjectSummary$count < 160 | subjectSummary$accuracy<0.55] # trim data for subjects who don't give us enough data or whose accuracy is less than 55%
data <- filter(data,!(subject_id %in% unusualsubjects))
data$year<-as.factor(data$year) #year was read in as an integer, but it should just be a qualitative factor

responseTimes<-filter(data, correctness==1 ) # don't include incorrect trials for response time analysis
hist(responseTimes$rt,breaks=2000,xlim=c(0,5000),xlab="RT",ylab="Frequency") # get an overall feel for the RT distribution


#For looking at whether and how percent correct is influenced by word type and block
byseveral<-group_by(data,subject_id,stim_type,block) #break data down by subject, block, and word/nonword type
pcaverages<-summarize(byseveral,percentCorrect=sum(correctness==1)/n()) #gives average accuracy broken down by block and stimulus type. % correct = count of correct trials divided by total number of trials n()
model<-ezANOVA(data=pcaverages,dv=percentCorrect,within=c(stim_type,block),wid=subject_id) # conduct a repeated measures ANOVA - dv = dependent variable.  within = a list of all of the within subject variables.  wid = variable that is used to group data by subject
model # show results of the ANOVA model
table1 <- tapply(X=pcaverages$percentCorrect,INDEX=list(pcaverages$stim_type,pcaverages$block),FUN=mean) #apply mean function to RT broken down by block and type of stimulus
table1 # show means so that one can begin to interpret the data

ggplot(pcaverages,aes(x=block,y=percentCorrect,fill=stim_type,color=stim_type))+ stat_summary(fun=mean,geom="bar",position=position_dodge(width=0.9))+stat_summary(fun.data=mean_cl_normal,geom="errorbar",position=position_dodge(0.9))+coord_cartesian(ylim=c(0.7,1))+ xlab("Block") + ylab("Probability Correct") +guides(fill=guide_legend("Kind of Context")) + guides(color=FALSE)
# There's a lot going on this above ggplot, and I built it up through iterative adjustment as I saw errors in it
# I specified both fill and color to reflect the stimulus type (word or non-word) because the error bars use line COLOR, and the bars use color FILL
# stat_summary makes a summary of the points - one for each cell in the design, geom specifies a bar plot, position_dodge so that bars for word and non-word trials are not on top of each other
# second stat_summary to create error bars, also "dodged" so that they are side-by-side
# coord_cartesian used to zoom into a range of the plot because everybody's accuracy was pretty good.  Didn't want to start at 0% accuracy
# guides added so that I could give the legend a custom title
# I noticed that there were redundant legend elements for color and fill so I remove the the color legend with the final segment

#the basic word superiority effect can also be tested with a paired t-test
byseveral<-group_by(data,subject_id,stim_type) #break data down by subject, block, and word/nonword type
pcaverages<-summarize(byseveral,percentCorrect=sum(correctness==1)/n()) #gives average accuracy broken down by each subject and by word vs. nonword context. % correct = count of correct trials divided by total number of trials n()
#dcast is a useful function to convert from long to wide format.  "Melt" goes the other way.  For dcast, subject_id is how we want different rows broken down by, stim_type is how we want to break down our columns, and value.var is the actual value we want to fill in the columns
pcwide<-dcast(pcaverages,subject_id~stim_type,value.var="percentCorrect") #convert from long format to wide format, with two columns of percent corrects - one for words and one for non-words
t.test(pcwide$word,pcwide$nonword,paired=TRUE) #now that the data is in wide format, we can run our standard paired samples t-test

#For looking at whether and how response time is influenced by the stimulus type
#ignore super long RTs, just for ggplot. break data down by subject and stimulus type (word/nonword)
# below is good example of a dplyr pipeline of processing
rtaverages<-filter(data,correctness=="1" & rt < 8000) %>% group_by(subject_id,stim_type,block) %>%summarize(count=n(),rt=mean(rt, trim=0.1)) #show averages for each subject for each stimulus type  Trimmed mean so not too influenced by outliers
View(rtaverages) # if you want to see the whole frame with everybody's averages
model<-ezANOVA(data=rtaverages,dv=rt,within=c(stim_type,block),wid=subject_id) # conduct a repeated measures ANOVA - dv = dependent variable.  within = a list of all of the within subject variables.  wid = variable that is used to group data by subject
model # show results of the ANOVA model
ggplot(rtaverages,aes(x=block,y=rt,fill=stim_type,color=stim_type))+ stat_summary(fun=mean,geom="bar",position=position_dodge(width=0.9))+stat_summary(fun.data=mean_cl_normal,geom="errorbar",position=position_dodge(0.9))+ xlab("Block") + ylab("RT (msec)") +guides(fill=guide_legend("Kind of Context")) + guides(color=FALSE)

# Just out of curiosity, let's look at how the distribution of RTs is influenced by whether the responses were correct or incorrect, and stimulus type
responseTimes<-filter(data, rt < 4000 ) # This time, only filter by whether trial is an actual experiment trial, including errors and correct responses
responseTimes$correctness<-recode(responseTimes$correctness,"0='Incorrect';1='Correct'") # Make graph a little easier to read because legend shows "correct" and "incorrect" rather than 0 and 1
ggplot(responseTimes,aes(rt,fill=stim_type,color=stim_type))+geom_density(alpha=0.2) + xlim(0,4000) # show whole distributions of RTs broken down by stimulus type (word or nonword).  Warnings because we're not plotting all RTs
ggplot(responseTimes,aes(rt,fill=correctness,color=correctness))+geom_density(alpha=0.2) + xlim(0,4000) #show RT distribution broken down by correctness.  Error trials are also longer
ggplot(responseTimes,aes(rt,fill=stim_type,color=correctness))+geom_density(alpha=0.2) + xlim(0,4000)+ scale_fill_manual(values=c("orange", "green")) #show RT distribution broken down by correctness and stimulus type

