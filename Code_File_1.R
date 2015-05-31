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

Executing this line will produce an Error

# The above error was produced as the above line was not in the R language

################################################################################
#                                                                              #
#                  The Basics of Command Line Computing in R                   #
#                                                                              #
################################################################################


################################################################################
#                                                                              #
#                     Simple Mathematical Operations in R:                     #
#                                                                              #
################################################################################

# Addition:
2+3

# Subtraction:
2-3

# Multiplication:
2*3

# Division:
6/2

# Indicies:
2^3
8^(1/3)

# The Order of Operations in Implemented as per BIMDAS.
2*3-1
2*(3-1)

# So far this is just like a simple calculator program run on the commandline.
# The next level of complexity arrises from the ability to store values as named
# objects.

################################################################################
#                                                                              #
#                         Creating and Naming Objects                          #
#                                                                              #
################################################################################

# For example, we can store the value 2 as the object x.
# To be precise, we are creating the object 'x' and storing a value in it with
# the following single line of code:

x <- 2

# One way to remember this syntax is to think of the assignment operator
# <- like an arrow that 'put's 2 into x'

x

# Note: this is one of the instances in R where a space has an impact on the
# meaning:

# Is x less than -3?
x < -3

# Why not? Because x has the values of 2 stored in it.
x

# However we can overwrite the contents of x by using the assignment operator

x <- 3

x

# The difference between <- and < - is something syntax highlighting
# (as provided by an Integrated Development Environment such as RStudio)
# can make apparent in having the assignment operator <- displayed as a
# different colour to < -

x <- 3

x < -3

# It is good practise and will make your code more readable if you use <-
# for assigning values and save = for supplying information to the arguments of
# functions.  Setting good coding habits now will save you time later.

# Let's now create an object called 'y' and assign it the value of 2

y <- 2

y

# We can then use the object names 'x' and 'y' as surrogates for the values they
# contain.

x*y

x/y

x*(x+(x^2))

# This may seem a little unnecessary for single values but later we will see
# that in addition to storing a single number in an object we can store a
# vector in an object or indeed a matrix or dataframe.

# This is a good time to pause and appreciate the benefits of an IDE that makes
# clear which open bracket ( is matched with which closed bracket ) through a
# feature called 'paren matching'.
# In the line of code below position the cursor next to a bracket and it's 
# matched bracket is highlighted.

z <- (x*(x+(x^2))/3+y)^2

z

# Paren matching becomes increasingly useful as you write increasing complex
# commands and functions.

# So far in this tutorial we have created three objects:
x
y
z

# You can see the list of objects you have created in the upper right hand pane
# of the RStudio window under the 'Environment' Tab.

# You can also use the ls() command to have R display the list of object you
# have created within this R session:

ls()

################################################################################
#                                                                              #
#                       Saving and Loading Workspaces                          #
#                                                                              #
################################################################################

# Should you wish to close R and be able to open it again at a later date and
# have these same objects available for use you need to save your R 'Workspace'
# prior to closing R (or RStudio which ammounts to the same thing).

# To save the workspace via the RStudio menu select open the 'Environment' Tab
# in the upper right hand pane of the RStudio window and click the 'Save' button
# (the 'Save' button resembels a blue upside down, floppy disk).
# Clicking the 'Save' button will open the 'Save Workspace' Dialogue Box in
# which you may choose where in your computer's file structure you would like to
# save the workspace.  I suggest creating a folder for this course and inside
# that folder another folder called 'Workspaces'.

# We can also use the save.image( ) command to save the workspace.
# Naturally you will want to a supply a file path that reflects the file
# structure on your computer:

save.image(file='~/Intro_to_R/Workspaces/Day_1.RData')

# On machines running MS Windows the file path will likely start with a C://
# Most operating systems allow you to view the file path of a file or folder
# by examining the properties of that file or folder.

# Once you have saved the Workspace in a location which you can remember please
# close RStudio (which also closes R).

# When you reopen RStudio the last workspace you created will be loaded by
# default (see the Environment pane for the list of variable loaded).

# To clear this list of variables you can click the broom shaped 'Clear' button
# in the Environment pane or you can execute the following command:

rm(list=ls())

# Once you have cleared the Workspace there will be no assigned objects and you
# should see the 'Environment is Empty' message displayed in the middle of the
# Environemtn pane.
# Similarly the ls() command should return and empty string represented as
# 'character(0)'

ls()

# We may now practise loading a previously saved Workspace.
# In the 'Environment' pane of the RStudio window click the 'Load Workspace'
# button (this button resembles a yellow manilla folder with a green arrow
# emerging from it and pointing to the right).
# This will open the 'Load Workspace' dialogue box through which you may
# navigate to the location in which you save the above, select this workspace,
# then load it.

# Alternatively this same operation may be achieved with the load command:

load(file='~/Intro_to_R/Workspaces/Day_1.RData')

# Note the the previously defined objects: x, y and z are now available again
# and are listed in the Environment.

# If we no longer want 'z' we can remove it with the rm( ) command as follows:

rm('z')

ls()

################################################################################
#                                                                              #
#               Data Classes and Data Manipulation in R base core              #
#                                                                              #
################################################################################

# Four useful object classes in R are: Vectors, Matrices, Dataframes, and Lists.


# The simplest way to think of a vector vector is as a collection of elements in
# a particular order.
# In R there are multiple types of vectors.

# Numeric vectors contain numbers.
# For statistical purposes a numeric vector might contain observations from a
# numeric variable.
a <- c(1, 2, 3, 2, 1, 3) # think 'c' for concatenate

a

class(a)

# some functions will output a numeric vector such as the function to make a
# sequence of numbers seq( )

b <- seq(from = 1, to = 6, by = 1)

b

class(b)

# Character vectors contain letters or words.
# For statistical purposes a character vector could store the observations of a
# categorical variable.

c <- c('a', 'a', 'b', 'b', 'c', 'd')

c

class(c)

d <- c('apple', 'apple', 'banana', 'banana', 'orange', 'pear')

d

# The var( ) command will return the variance of matrix supplied to it

var(a)

# vector addition
a+b 

# multiplication of the elements of two vectors
a*b

# a simple plot
plot(a, b)

# this is a good time to point out that all functions in R have arguments

?plot # and see the arguments section of the page

# but I didn't use them above when I wrote plot(a,b)

# this is because if you don't specify which arguments you are providing
# information for R will either assuming you are doing so in the order in which
# these arguments are listed in the help file or guess which arguments are best
# suited by which inputs
# this can be risky...

plot(a, b, main='x and y arguments not specified by name')

plot(x = b, y = a, main='x and y arguments specified by name')

# when you specify the arguments by name you can do so in order you like
plot(main = 'x and y arguments specified by name out of order', y = b, x = a)

f <- c('blue','green','yellow','orange','red')

class(f) # a character vector

a+f # doesn't work ...doesn't make sense to add '1' to 'green' etc

plot(y = b, x = a, col = f, pch = 1)
plot(y = b, x = a, col = f, pch = 2, cex = 2)
plot(y = b, x = a, col = f, pch = 3, cex = 5)

# the following command runs across two lines so you will need to execute both
# of these lines to complete the command:
plot(y = b, x = a, col = f, pch = 19, cex = 2, xlab = 'x axis title',
     ylab = 'ylab title')



# Matrices are rectangular arrays with numbers of characters in the rows and
# columns

# if you 'bind' two or more 'c'olumns of equal length together you create a
# matrix

mat.1 <- cbind(a, b)

mat.1

class(mat.1)

# We can find the dimensions of a matrix with the dim( ) command:

dim(mat.1)

mat.2 <- cbind(c, d)

mat.2

# We can also bind matrices of the appropriate dimensions together to create
# larger matrices:

mat.3 <- cbind(mat.1, mat.1)

mat.3

# The matrix( ) command also creates a matrix directly from a vector of numbers
# supplied to the 'data' argument and the appropriate dimension supplied to
# either the 'nrow' or 'ncol' argument.  The 'byrow' argument takes a logical
# value i.e. either a TRUE or FALSE value and determines whether the data are
# populated into the matrix row by row or column by column.

# The command belwo will create a 5x2 matrix from the data supplied filling the
# matrix column by column
mat.4 <- matrix(data = seq(from = 1, to = 10, by = 1), byrow = FALSE, nrow = 5)
mat.4

class(mat.4)
dim(mat.4)

# To demonstrate the difference between filling by columns and filling by rows
# the command below creates a matrix with the same data and dimensions as mat.4
# but fills the data into the matrix row by:

mat.5 <- matrix(data = seq(from = 1, to = 10, by = 1), byrow = TRUE, nrow = 5) 

# Transpose a matrix with t( ) command
t(mat.5) 

# Matrix operations require the %% symbols around the operator
# e.g. %*% performs matrix multiplication whereas a simple *  performs
# non-matrix (i.e. element by element) multiplication

mat.4 * mat.5

sm <- mat.4%*%t(mat.5) # matrix multiplication

sm

# "If the argument to var() is an n-by-p matrix the value is a p-by-p
#  sample covariance matrix got by regarding the rows as independent
#  p-variate sample vectors."
#  - http://cran.r-project.org/doc/manuals/R-intro.html

cov.sm <- var(sm)  

cov.sm

diag(sm) # extract the diagonal of a matrix as a vector

cov(sm) # another way of getting the covariance matrix

# we can check that this is indeed the case by doing an element by element
# substraction:
cov(sm)-var(sm) 


Mat.6 <- cbind(mat.1, mat.2)
Mat.6[,1]
mean(Mat.6[,1])

# numbers have been converted to characters

# a dataframe allows us to have numeric variables in some column and character
# variables in other columns

Data <- data.frame(mat.1, mat.2)
mean(Data$a)

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


# End of Module 1
