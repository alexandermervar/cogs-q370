library(ez) #for doing the ANOVAs
library(dplyr) # great library for massaging data
library(ggplot2)

# IUiqs<-c(122,116,123,125,106,112) #create a set of IQs from 6 IU students
# Purdueiqs<-c(101,97,107,105,107,114) # 6 Purdue student IQs
# t.test(IUiqs,Purdueiqs) # this will be run as an unpaired T-test because we can't pair up the students.
# iqs<-c(IUiqs,Purdueiqs) # just for illustration purposes, let's imagine that all of the scores are in one column
# schools<-c(rep("IU",6),rep("Purdue",6)) # and another column containing the school information.  rep(x,y) gives us y repetitions of x
# t.test (iqs~schools) # the t-test should be the same as the previous unpaired t-test.
# iqFrame<-data.frame(school=schools,iqs=iqs) # make a dataframe out of data
# ggplot(iqFrame,aes(x=school,y=iqs,fill=school))+stat_summary(fun=mean,geom="bar")+stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) + theme (text=element_text(size=20))  # building up a plot the ggplot way. Use aes to specify aesthetics, and add two summary graphics - one for the means (shown in bars), and one for the error bars
# 
# # an alternative way to get in the data above
# iqs = read.csv(file="IQsPurdueIU.txt",header=T,sep=" ")
# mean(iqs$IQ) #report mean of all IQs
# filter(iqs,school=="Purdue") %>% summarize(mean(IQ)) # the Pipelines way of reporting mean of just a subset of data
# sd(iqs$IQ) #report standard deviation of all IQs
# group_by(iqs,school) %>% summarize(mean(IQ)) # show how you would create a table of IQs, grouped by school
# t.test(iqs$IQ ~ iqs$school,paired=FALSE)
# 
# 
# rts=data.frame(morningRT=c(430,480,410,520,540,480),eveningRT=c(320,340,390,510,480,450)) #Create a data frame for every subject's morning and evening RTs
# rts = read.csv(file="/word_proc_docs/q370/morningeveningRT.txt",header=T,sep=" ") # alternative if want to read in file
# t.test(rts$morningRT,rts$eveningRT,paired=TRUE) # This time, each subject provides two scores and we use a paired T-test to pair up these two scores.
# mean(rts$morning)
# longRts<-reshape(rts,v.names="RT",times=c("Morning","Evening"),varying=c("morningRT","eveningRT"),direction="long") # an example of how to convert data frame from wide to long format because bargraph.ggplot requires all dependent variables in a single column with another column saying which group they belong to
# ggplot(longRts,aes(x=time,y=RT,fill=time))+stat_summary(fun=mean,geom="bar")+stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2)+xlab("Time of Day")+ylab("RT (Msec.)") # building up a plot the ggplot way. Use aes to specify aesthetics, and add two summary graphics - one for the means (shown in bars), and one for the error bars
# 
# # I put the data file in a folder called q370.  If you don't have this folder, you'll have to change the directory path to wherever you put the data file
# cities<-read.table("/word_proc_docs/q370/top200cities.txt", header = TRUE)
# 
# t.test(cities$pop2010,cities$pop2015) #unpaired T-test for two groups.  Test does not know that two populations in a row come from the same city
# t.test(cities$pop2010,cities$pop2015,paired=TRUE)  #Now assume that the matched rows come from the same city
# #10^-14 meters is about the size of a proton.  Atoms are 10^-10 meters.
# 
# cor (cities$pop2010,cities$pop2015) # is the population in 2010 correlated with the population in 2015
# cor.test(cities$pop2010,cities$pop2015) #Tells us whether the correlation is significant
# 
# par(mar = c(4,4,4,0),mfrow=c(1, 1) )  # reset plots just in case it's necessary from whatever was going on before

recalled<-read.table("contextreinstatement.txt", header = TRUE,stringsAsFactors=TRUE) #read in data from context-reinstatement experiment, set stringsAsFactors so that text descriptions are automatically encoded as factor levels
ezANOVA(data=recalled,dv=recall,within=c(study,test),wid=subject) # conduct a repeated measures ANOVA - dv = dependent variable.  within = a list of all of the within subject variables.  wid = variable that is used to group data by subject
ggplot(recalled,aes(x=study,y=recall,fill=test))+stat_summary(fun=mean,geom="bar",position = position_dodge(width = 0.90))+stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2,position = position_dodge(width = 0.90))+xlab("Study environment")+ylab("Words Recalled") + theme (text=element_text(size=20)) #using a bar plot
ggplot(recalled,aes(x=study,y=recall,color=test, group=test))+stat_summary(fun=mean,geom="line")+stat_summary(fun=mean,geom="point",aes(shape=test,size=3),show.legend = FALSE)+stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2)+xlab("Study environment")+ylab("Words Recalled") + theme (text=element_text(size=20)) #using a line graph

ezStats(data=recalled,dv=recall,within=c(study),wid=subject) # just show the means for studying in the bedroom and kitchen
group_by(recalled,study,test) %>% summarize(mean(recall)) # an alternative way to show means broken down by study and test, using dplyr

uni<-lm(recalled$recall~recalled$study*recalled$test) # This is the univariate version of an ANOVA in which case we are not taking advantage of the knowledge that the same subject produced results in four conditions
summary(uni)
#to specify a model in lm, dependent_variable ~ independent_variables.  Main effects separated by +.  Use * to specify all main effects and interactions involving variables, or : to only introduce specific interactions
#So, the above model is equivalent to uni<-lm(recalled$recall~recalled$study+recalled$test+recalled$study:recalled$test) because * introduces main effects of study and test AND their interaction


quantGRE<-c(430,480,410,520,540,480) # a set of Quantitative GRE scores
verbalGRE<-c(520,540,590,610,680,550) # a set of verbal GRE scores for the same people, in the same order as the quantitative scores
cor.test(quantGRE,verbalGRE)
model <- lm(verbalGRE~quantGRE) #linear model, predicting Verbal GRE score from Quantitative GRE score
summary(model) #shows results from model, including the best fitting values for slope and intercept of the line relating verbal and quantitative GRE scores
plot(quantGRE,verbalGRE) #plot the relation between the two GRE scores with Quantitative score on the X-axis
abline(model) # Add a line, specifying its slope and intercept from the best fitting model
# Note that the square of the correlation coefficient from cor.test is the same value as the "Multiple R-squared" value from the linear model, and the p-value is identical

iqs<-read.table("/word_proc_docs/q370/regressionexample.csv", header = TRUE) #read in example of predicting IQ from height and weight and shoe size
fit<-lm(data=iqs,iq ~ shoeSize + height + weight) # conduct a linear regression where all three independent variables are continuous.  Can add in an interaction by hand with height : weight or main effects and interactions with height * weight
summary(fit)

#create data with a known quadratic trend
x<-seq(-1,1,by=.01) # create a variable that goes from -1 to 1 my small steps
y<-(3*x^2)+runif(length(x),0,4) #runif = random uniform distribution.  Argument 1: how many data points to create, Arg 2: minimum value, Arg 3: maximum
plot(x,y)
fit<-lm(y~x)
summary(fit) # not significant because poor fit
abline(fit) # show bad fit
fit<-lm(y~poly(x,3,raw=TRUE)) # fit an Order 3 (cubic) polynomial to x.  Will include a linear x term, a quadratic x^2 term, and cubic x^3 term
# note, if you don't specify raw=true above, then default is to compute orthogonal polynomials.
summary(fit) # linear term not significant, but quadratic term is hugely significant.  Also note that R does a good job of reconstructing the parameters that generated the data
plot(x,y)
lines(x,fitted.values(fit),col="red") #fitted.values(fit) shows the Y values that are predicted for each x value

style<-read.table("/word_proc_docs/q370/carryingStyle.csv", header = TRUE) 
tbl =table(style$Sex,style$CarryingStyle) #gives table of carrying style counts broken down by sex
chisq.test(tbl) #show chi-square analysis of contingency between sex and carrying style.  Significance here means that there we can reject the null hypothesis that there is no relation between sex and carrying style
chisq.test(tbl,correct=F) #If we don't use a continuity correction, then the X-squared value is the same as the one computed by hand

