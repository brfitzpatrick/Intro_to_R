#     This R Code File 1 for the Introduction to R Course available at
#     git@github.com:brfitzpatrick/Presents_Intro_to_R 
#     Copyright (C) 2015  Ben R. Fitzpatrick.
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    The course author may be contacted by email at 
#    <ben.r.fitzpatrick@gmail.com>


# Hello, this is an R code file (notice the .R extension).
# In this course we will be using the RStudio Integrated Development Environment
# to author R scripts and interact with the R program.
# If you have not done so already please:
#    1) download and install R from <http://cran.r-project.org/>
#    2) download and install RStudion from <http://rstudio.org/>
#    3) open RStudio
#    4) from the RStudio File Menue select 'Open File'
#    5) select this .R file to open it with RStudio
#    6) continue with to work through the code below in RStudio

# Any text on a line after the hash symbol # R treats as a comment.
# R will not attempt to evaluate comments and thus they will not cause errors
# if you attempt to run code that contains comments.

# Once you have this file open in RStudio please send the non-commented lines to
# R by placing the cursor on the appropriate line then clicking the 'Run' button

# The Run button is located in the top right of the upper right hand panein the
# default RStudio window, it looks like a white box with a green arrow pointing
# to the right of the white box and has the word 'Run' written on it.
# Please try to run the following line of code:

print(c('Hello world.')) 

# This is the traditional start to any coding lesson.
# Notice how we have instructed RStudio to send the above print( ) command to
# the R process for execution and the result of executing the command has been
# displayed in the 'Console' pane of the RStudio window below the 'Script' pane
# of the RStudio window.


# You can also execute multiple lines code by highlighting them with the mouse
# (or keyboard) then clicking the 'Run' button again.
# Try executing the following two lines of code by highlighting them then
# clicking the 'Run' button:

print(c('Hello world.'), quote = FALSE)
print(c('Hello'), quote = FALSE)
print(c('World'), quote = FALSE)

# If you are doing a lot of typing and prefer not to reach for the mouse so much
# you can execute a line of code by placing the cursor on it with the arrow keys
# on you keyboard the holding the 'Control' (or 'Command') key on your keyboard
# and with the 'Control'/'Command' key depressed pressing the 'Return'/'Enter'
# key

# Try executing this comment line.
# Note how the R process simply displays the text without producing and error as
# it will if you try to evaluate the following line which is written in the
# English language rather than the R language:

Executing this line will produce an Error since this line isn't in the R language




# Theto follow the exercises (you can send the comments to but all that will do is display their text in the console)



# Basics of command line computing in R

2+3 #simple arithmetic
2*3

x <- 2 # assign variable 'x' the value of 2, <- is like an arrow: 'put 2 in x'

x

#note this is one of the couple of instances in R where a space makes an impact on the meaning

x < -3

x

x <- -3

x

# one of the many mistakes syntax highlighting as provided by an Integrated Development Environment (e.g. RStudio, Notepad++, ESS etc.) can  help you avoid as the assignment symbol <- should display as a different colour to < - in any decent IDE for R

# it is good practise and will make your code more readable if you use <- for assignment and save = for supplying information to the arguments of functions

y <- 3

y

x*y

x/y

x*(x+(x^2))

# paren matching (click/move cursor next to a bracket and it's matched bracket is highlighted) is also a very handy feature provided by good IDEs
# neither paren matching nor syntax highlighting are available at the R console in R Base on Windows or at the Linux/Unix terminal
#try it here:

z <- x*(x+(x^2))

z

ls() # list the variables you have defined this R session

# see also the workspace window in RStudio

# to save the workspace so we can have all our named assignments (custom variables, dataframes, functions etc.) next time we open R

# via the RStudio menu or...

save.image(file='/home/ben/PhD/My_R_Course/Day_1/BRAG_R_Course_Day_1.R') # Naturally you will want to a supply a file path that reflects the file structure on your computer

#close R 

rm(list=ls())   # or clear the workspace

ls() # see all your assignment have been cleared

# open it again and load the workspace either with the RStudio menu or

load(file='/home/ben/PhD/My_R_Course/Day_1/BRAG_R_Course_Day_1.R')

ls()   # and they're back

# Data Classes and Data Manipulation in R base:

# assigning a vector of numbers to the letter 'a'

a <- c(1,2,3,2,1) #think 'c' for column 

a

# yes I know R prints it out as a row but that's just to save space

# if you 'bind' two of these 'c'olumns together ...

b <- seq(from=1,to=5,by=1)

cbind(a,b) 

# ... you can see that R does actually treat them as columns

# so we have defined to vectors

class(a)

class(b)   #  numeric vectors to be precise
   
c <- cbind(a,b)

c

class(c)

# and binding two vectors together makes a matrix

#just as


d <- matrix(data= c(1,2,3,4,5,6,7,8,9,10),byrow=FALSE,nrow=5) # create a 5x2 matrix from the data supplied to the data argument (as a vector) filling the matrix by column


class(d)

e <- matrix(data= c(1,2,3,4,5,6,7,8,9,10),byrow=TRUE,nrow=5) # create a 5x2 matrix from the data supplied to the data argument (as a vector) filling the matrix by row

t(e) # transpose a matrix with t()

# matrix operations require the %% symbols around the operation e.g. %*% otherwise non-matrix operations are carried out (by element)

d*e

sm <- d%*%t(e) # matrix multiplication

sm

g <- var(sm)  # "If the argument to var() is an n-by-p matrix the value is a p-by-p sample covariance matrix got by regarding the rows as independent p-variate sample vectors." - http://cran.r-project.org/doc/manuals/R-intro.html

g

diag(g) # extract the diagonal of a matrix as a vector

 cov(sm) # another way of getting the covariance matrix

cov(sm)-var(sm) # the same...

#whereas if the object supplied to var() is a numeric vector you get the variance of the numbers listed therein

var(a)

a+b # vector addition

a*b # multiplication of the elements of two vectors

plot(a,b) # a simple plot

# this is a good time to point out that all functions in R have arguments

?plot # and see the arguments section of the page

#but I didn't use them above when I wrote plot(a,b)

# this is because if you don't specify which arguments you are providing information for R will assume you are doing it in the order they are listed in the help file

#that is:

plot(a,b,main='x and y arguments not specified by name')

dev.new() # opens a new plotting device (if you have your setup such that plots display in a new window ...this call won't work in RStudio instead use the arrow buttons above the plot to move between the plots in the order you created them)

plot(x=a,y=b,main='x and y arguments specified by name')

dev.new()

plot(main='x and y arguments specified by name out of order',y=b,x=a) # when you specify the arguments by name you can do so in order you like

dev.off()  # close the current graphics device 

graphics.off() # close all open graphics devices

f <- c('green','green','red','red','red')

class(f) # a character vector

a+f # doesn't work ...doesn't make sense to add '1' to 'green' etc

Data <- data.frame(a,b,f) # create a dataframe (a collection of named columns of numbers or factor levels) and unlike a matrix it can have a some columns of  factor levels as words and some columns of numbers

j <- as.matrix(Data) # coercing Data to a matrix

j # as some entries were non-numeric the whole matrix has been coerced to a character matrix

j$a # you can't extract columns by name from a matrix like you can from a data frame

j[,1] # even columns of numbers have been coerced to characters (note the quote marks); j[,1] means the column '1' of matrix 'j' just as j[1,] would mean row '1' of matrix 'j'

#and so they don't behave like numbers anymore

sum(j[,1]) #doesn't work because R thinks j[,1] is a character vector 

class(j[,1])

#indeed to get them to behave like numbers to we need to coerce them back to being of class numeric

sum(as.numeric(j[,1]))

class(Data)

Data

head(Data) # top 6 rows... good for peaking at big data.frames without filling up your console

Data$a # the column of 'Data' called 'a'

Data[,1] # first column of 'Data'

Data[1,] # first row of 'Data'

plot(Data[,2],Data[,1],col=as.character(Data[,3])) # R is just treating Data[,3] as it would any factor of two levels and seeing as we have asked for the plot to be coloured by these factor levels it does so choosing the colours itself

plot(Data$b,Data$a,col=as.character(f))  # if we protect 'f' with an 'as.character' call the original intent is produced and the points coloured red and green

# rather with typing Data$ again and again in a single command you can encapsulate that command in a with() command with the first argument being the dataframe in which the variables can be found for the remainder of the command like so

with(Data,plot(b,a,col=as.character(f)))

rm(list=ls())  #


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

#Thanks for reading chances are I'll be running this course again in the future so if you have questions or comments about this material please get in touch: ben.r.fitzpatrick@gmail.com

#The next module in this series is an introduction to R graphics with 'ggplot2' and 'rgl'
