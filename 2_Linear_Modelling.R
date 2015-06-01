
#Getting Help within R

help.search('linear model') #

#scrolling down the list of matches in the R documentation to the query 'linear model' there is the entry

# stats::lm               Fitting Linear Models

# this is telling you that in the package 'stats' there is a command called 'lm' which can be used to fit linear models
# 'stats' is loaded when you start R so you can go right ahead and use 'lm'
?lm # displays the help page for the command 'lm'

# now that you know how to read about what a function does I'm going to annotate the code a little less heavily :) if you get confused try the help file for the command where you lost track of things

# and never forget google I frequently paste my error messages from R directly into the search box

# other packages are not loaded automatically when you open R

#if they are installed they can be loaded like this:

library('rgl')
#or just use the RStudio package menu

# if you need to install a package

install.packages('rgl') # and select a local mirror to download them from (you will need to be connected to the internet)

# packages often have demonstrations  e.g.
demo('rgl')

#how to cite R packages:
citation('rgl')

#see sample code from bottom of help pages under the Examples section (copy and paste it straight into the console to see what it does and how it does it)
?persp

rm(list=ls()) #clear the workspace

# Here is how I generated the data for Exercises 1 before I exported it and mess it up in a spreadsheet program to resemble to common messy formats you receive data in ;)

# you can practise rearranging the Regression.xls supplied with this code file and saving it to .csv and importing it or you can just run the code below in this R session to make the correctly formatted Data

### Making and exporting the Data 

x1 <- seq(0,60,2)

y1 <- 5.4*x1-0.1*x1^2

e1 <- rnorm(n=length(x1),mean=0,sd=30)

y1.e <- y1+e1

x2 <- seq(1,61,2)

y2 <- 5.4*x1-0.15*x1^2

e2 <- rnorm(n=length(x1),mean=0,sd=30)

y2.e <- y2+e2

x.l <- c(x1,x2)

y.l <- c(y1.e,y2.e)

f.l <- c(rep('green',length(y1.e)),rep('red',length(y1.e)))

plot(x.l,y.l,col=as.character(f.l))

Data <- data.frame(x=x.l,y=y.l,f=f.l)

#####################
#                   #
#  Exercise 1       #
#  Linear Modeling  #
#                   #
#####################

#Open Regression.xls in your choice of spreadsheet program

#we want to model y as function of x and the factor with levels 'green' and 'red'

#R expects variables in columns, observations in rows so you will need to reorganise the data a little

# then save as .csv

#then you can read the data in creating a dataframe called 'Data'

Data <- read.csv(file=file.choose(),header=TRUE) # instead of file.choose() you can put in a file path to the file in quotes (in Windows right click on the file, select properties to find the file path) 

with(Data,plot(x,y,col=as.character(f)))

?lm

#let's start with a simple linear regression

m1 <- with(Data,lm(y~x)) 

abline(m1) # add the line predicted by m1 to the plot

summary(m1)

dev.new() # open a new plot device

par(mfcol=c(2,2)) # set up a 2 by 2 plot panel

plot(m1) # produce diagnostics plots for the model 'm1'


#now suppose we want to see if a quadratic is better fit

# R won't understand something like

with(Data,lm(y~x+x^2)) #see how it fails to include a x^2 term in the model

x.2 <- Data$x^2 # so we have to define a new variable containing the x^2 values

Data <- data.frame(Data,x.2) # then to keep things tidy let's bind it to the end of the dataframe

head(Data) # have look at the top few rows to check out attachment of the 'x.2' column to the dataframe 'Data' worked

m2 <- with(Data,lm(y~x+x.2))

summary(m2)

# like what you see? generate the LaTeX code for this table with the 'xtable' package
library(xtable)
xtable(summary(m2))

dev.new() # open a new plot device

par(mfcol=c(2,2)) # set up a 2 by 2 plot

plot(m2) # produce diagnostics for the model 'm2'

dev.off()

# abline() only works for linear fits so

pred.at <- seq(from=0,to=60,by=0.5) # x values to predict at

m2.pred <- predict(object=m2,newdata=data.frame(x=pred.at,x.2=pred.at^2)) # predicting at these x values from the model 'm2'

with(Data,plot(x,y,col=as.character(f)))

abline(m1)

lines(x=pred.at,y=m2.pred,col='blue') # adding these point predictions to the plot

summary(m2)

# what about the effect of the 'colour' factor interactions with other model terms?

m4 <- with(Data,lm(y~f*+1+x+x.2)) # separate intercepts for the levels of the 'f' factor (intercept is 'green'), fred value is how we modify the intercept value when the 'f' factor level is 'red'

m4.pred.red <- predict(object=m4,newdata=data.frame(x=pred.at,x.2=pred.at^2,f=rep('red',length(pred.at)))) # predicting at these x values from the model 'm2'

#adding these to the plots

lines(x=pred.at,y=m4.pred.red,col='red') # adding these point predictions to the plot and joining them with lines so they look like a smooth curve

m4.pred.green <- predict(object=m4,newdata=data.frame(x=pred.at,x.2=pred.at^2,f=rep('green',length(pred.at)))) # predicting at these x values from the model 'm2' and f= 'green'

lines(x=pred.at,y=m4.pred.green,col='green') # adding these point predictions to the plot

m5 <- with(Data,lm(y~f*+1+x+f:x+x.2)) #model where intercept and 'x' term have interactions with 'f'; fred:x (read as  (f level red) interaction with x) is the modification of the slope for f level red

m5.pred.red <- predict(object=m5,newdata=data.frame(x=pred.at,x.2=pred.at^2,f=rep('red',length(pred.at)))) # predicting at these x values from the model 'm2'

lines(x=pred.at,y=m5.pred.red,col='red',lty=2) # adding these point predictions to the plot and joining them with lines so they look like a smooth curve

m5.pred.green <- predict(object=m5,newdata=data.frame(x=pred.at,x.2=pred.at^2,f=rep('green',length(pred.at)))) # predicting at these x values from the model 'm2'

lines(x=pred.at,y=m5.pred.green,col='green',lty=2) # adding these point predictions to the plot

m6 <- with(Data,lm(y~f*+1+x+f:x+x.2+f:x.2)) #model where intercept,'x' and the x^2 terms have interactions with 'f'; fred:x (read ad  (f level red) interaction with x) is the modification of the slope for f level red

m6.pred.red <- predict(object=m6,newdata=data.frame(x=pred.at,x.2=pred.at^2,f=rep('red',length(pred.at)))) # predicting at these x values from the model 'm2'

lines(x=pred.at,y=m6.pred.red,col='red',lty=3) # adding these point predictions to the plot and joining them with lines so they look like a smooth curve

m6.pred.green <- predict(object=m6,newdata=data.frame(x=pred.at,x.2=pred.at^2,f=rep('green',length(pred.at)))) # predicting at these x values from the model 'm2'

lines(x=pred.at,y=m6.pred.green,col='green',lty=3) # adding these point predictions to the plot

legend(x='bottomleft',lty=c(1,1,1,1,2,2,3,3),col=c('black','blue','green','red','green','red','green','red'),legend=c('m1','m2','m4.green','m4.red','m5.green','m5.red','m6.green','m6.red')) # adding a legend to the plot

#the plot is a little crowded so let's make a panel

dev.new()

par(mfrow=c(2,3)) # 2x2 panel of plots filled 'by-row'

with(Data,plot(x,y,col=as.character(f),main='Linear Model')) #1st item in panel

abline(m1)

with(Data,plot(x,y,col=as.character(f),main='Quadratic Model'))

lines(x=pred.at,y=m2.pred,col='blue')

with(Data,plot(x,y,col=as.character(f),main='Quadratic Model with Interaction in Intercept'))

lines(x=pred.at,y=m4.pred.red,col='red')

lines(x=pred.at,y=m4.pred.green,col='green') 

with(Data,plot(x,y,col=as.character(f),main='Quadratic Model with Interaction in Intercept and Linear Term'))

lines(x=pred.at,y=m5.pred.red,col='red')

lines(x=pred.at,y=m5.pred.green,col='green') 

with(Data,plot(x,y,col=as.character(f),main='Quadratic Model with full Interactions'))

lines(x=pred.at,y=m6.pred.red,col='red')

lines(x=pred.at,y=m6.pred.green,col='green') 


#maximise the window and the titles fit ;) 

# Note: while we have quadratic models all these models are fitted by linear least squares regression
# for non-linear least squares check out nls()
?nls     

##
# which of m1 to m6 is best?
# one way to answer this question is with stepwise variable selection using the Akike Information Criterion (AIC)

 step(object=m1,scope=list(lower=m1,upper=m6),direction=c('both')) # stepwise variable selection with the AIC as the criterion used to choose between models

#essentially what this is doing is starting with the model provided as the 'object' argument and taking the model provided as 'lower' as the simplest model and the model provided as 'upper' as the most complex model
#then looking at all possible variable additions and deletions from the model
#calculating the AIC change that would be associated with each of these additions or deletions
#then making the change that results in the best change in AIC
#continuing to do this until there is no addition/deletion that would improve the AIC anymore
#if direction=c('forward') only additions to the model are considered at each step
#if direction=c('backward') only deletions from the model are considered each step

#############################
#                           #
#   End of Exercise 1       #
#            &              #
# End of BRAG_Intro_to_R.R  #
#                           #
#############################
