library(dplyr)
library(ggplot2)
#anything after the "#" is treated as a comment by R
names<-c("Doc","Happy","Bashful","Sleepy","Grumpy","Dopey","Sneezy") #create a list of names and send it to the variable called "names"
iqs <- c(138,119,97,104,112,89,115) # create a list of IQs, and put it the variable "iqs"
iqs #convince ourselves that iqs has the scores that it should
dwarves <- data.frame(name=names,iq=iqs) #make a data structure called "dwarves" with two variables in it 
dwarves # let's see what our data structure looks like
dwarves$name #see what one variable of the data structure looks like.  "$" used to identify which variable
dwarves[2] #another way to access one variable of the data structure.  This will print out all of the dwarves' IQs
dwarves[3,2] #access the 3rd ROW of the second COLUMN in the data structure.  Should be 97
dwarves[2,] #access ALL (because left unspecified) of the COLUMNNS of the 2nd ROW.  Should be Happy's information
dwarves[,1] #access ALL of the ROWS of the 1st COLUMN.  Should be all of the names
dwarves$name=="Bashful" #for each dwarf, see whether his name is "Bashful".  TRUE = yes, FALSE=no
dwarves[dwarves$name=="Bashful",] #access whole row (because nothing after ",") of data for Dwarf meeting criterion.  Notice "==" used for logical operator "is left side equal to right side" because "=" used for assigning a variable
dwarves$iq[dwarves$name=="Bashful"] #Gives Bashful's IQ?  Note no commma because we're using the $IQ to select just one column

dwarves[dwarves$name %in% c("Doc","Grumpy"),2] #give only IQ for all rows satisfying the criterion that the name is in the list made up of "Doc" and "Grumpy"
dwarves$iq>100 #for each dwarf, see whether his IQ is greater than 100.  TRUE = yes, FALSE = no
smarties<-dwarves[dwarves$iq>100,] #make a new data frame which is the subset of the old frame in which IQs are greater than 100.  Select ALL columns
nrow(smarties) #the number of rows in smarties = the number of dwarves with IQs > 100
ncol(smarties) # the number of columns in the smarties dataframe
orderedDwarves <- dwarves[rev(order(dwarves$iq)), ] #orders the dwarves downwards by IQ  rev = reverses list, order = put list in order.  Note commas to include all rows/columns.  Understand this by building from inside out
#The dplyr way to do this is: dwarves[dwarves$iq %>% order %>% rev,]
orderedDwarves$name[1:4] #give the names of the smartest 4 dwarves
dwarves %>% filter(iq>100) %>% select(name) %>% arrange # dplyr-based example: alphabetically ordering the dwarves who have better than average IQs 
dwarves %>% mutate (smart = case_when(iq >= 100 ~ "smart", iq < 100 ~ "notSoSmart")) %>% group_by(smart) %>% summarize(mean(iq)) # second dplyr example: show IQs separately for dwarves < and > 100

script = readLines("snowwhite.txt")  # harvested from http://disneyprincess.wikia.com/wiki/Snow_White_and_the_Seven_Dwarfs_(Transcript)
script_names<-paste(names,":",sep="") #add colons to names for searching script, sep="" means no space.  If don't have colons, then when somebody talks about Doc, Doc's line count would increase
dialog_lines <- NULL # start with nothing in this variable which will contain each dwarf's number of lines of dialog
for (i in 1:length(script_names)) #make a variable i that loops from 1 to the last name (7)
  {
    dialog_lines[i] <- length(grep(script_names[i],script)) #grep looks for name in each line of script
    #grep can match any regular expression (regexp).  For example, regexp to match email addresses is ^[[:alnum:]._-]+@[[:alnum:].-]+$
  }
dwarves<-cbind(dwarves,dialog=dialog_lines) # cbind= column bind.  Add a new column to our data frame, and give it the name "dialog"
mean(dwarves$dialog) #reports the mean number of lines of dialog
barplot(dwarves$dialog,names=dwarves$name,ylab="Lines of Dialog",space=0,col=1:8) # show a plot of each dwarve's number of lines of dialog
plot(dwarves$iq,dwarves$dialog,xlab="IQ",ylab="Lines of Dialog",type="n") # scatterplot showing relation between a dwarf's IQ and number of lines of dialog.  Type="n" means that nothing is actually plotted yet
text(dwarves$iq,dwarves$dialog, labels=dwarves$name, cex= 0.7) # add labels to scatterplot

double <- function(x) #create a new function, in this case, a function that doubles whatever number is fed in as input
{return (x*2)} #return the value of 2 X whatever the input was
double (7) # check to see if function works.  Should return 14

#Another way to count the lines of dialog without using a loop
count_lines <- function (name)
  {
  return(length(grep(name,script))) #find name in script, and count (by reporting length) all cases where it is found
  }
dialog_lines <- apply(as.array(script_names),1,count_lines) #apply will apply count_lines function to every row (because second argument is 1, not 2 for columns) of script_names. "Apply" expects an array as first argument, so "as.array" is used to convert script_names into an array

nums <- 1:100 # create a series of numbers from 1 to 100
plot(nums, nums^2) # make a plot of the function f(x)= x^2.  First argument is X value, second is Y
plot(nums,sin(nums),type="p") #for this sin function, plot it as a line (type = "l"), not as points (type = "p")

library(dplyr) # general tools for massaging data
dwarves %>% filter(iq>100) %>% dplyr::select(name) #alternative to show names of dwarves with iqs over 100.  In this case, used dplyr:: in order to specify the right "select" command because another library already had something called select
dwarves %>% arrange(dialog_lines) %>% mutate (lettersName=name %>% as.character %>% nchar) #add a column to dwarves which the number of letters in the dwarf's name
eightDwarves <- add_row(dwarves,name="snazzy",iq=44) #another dplyr command, to easily add a new row

