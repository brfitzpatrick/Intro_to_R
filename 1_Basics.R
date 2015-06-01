#     This the first R Code File for the Introduction to R Course available at
#     git@github.com:brfitzpatrick/Intro_to_R 
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

################################################################################
#                                                                              #
#                   Code File to Accompany Course Module 1                     #
#                                                                              #
#                  The Basics of Command Line Computing in R                   #
#                                                                              #
################################################################################

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

f <- c('red','yellow','orange','green','blue','violet')

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

mat.5

# Note we can access particular rows or column of a matrix by using the square
# bracket [ ] based subsetting syntax.

# To access the contents of the first column of mat.5 use:
mat.5[,1]

# To access the contents of the second column of mat.5 use:
mat.5[,2]


# To access the contents of the first row of mat.5 use:
mat.5[1,]

# To access the contents of the fourth row of mat.5 use:
mat.5[4,]

# To access the contents of rows 1 to 4 of mat.5 use:
mat.5[1:4,]

# Transpose a matrix with t( ) command
t(mat.5) 

# Matrix operations require the %% symbols around the operator
# e.g. %*% performs matrix multiplication whereas a simple *  performs
# non-matrix (i.e. element by element) multiplication

mat.4 * mat.5

sm <- mat.4%*%t(mat.5) 

sm

# "If the argument to var() is an n-by-p matrix the value is a p-by-p
#  sample covariance matrix got by regarding the rows as independent
#  p-variate sample vectors."
#  - http://cran.r-project.org/doc/manuals/R-intro.html

cov.sm <- var(sm)  

cov.sm

# Another way of getting the covariance matrix is with the cov( ) command
cov(sm)

# We can check that this is indeed the case by doing an element by element
# substraction:
cov(sm)-var(sm) 

# Extract the diagonal of a matrix as a vector with the diag( ) command:
diag(sm) 

# Recall that mat.1 contains numbers
mat.1

# while mat.2 contains characters.
mat.2

# Thus it makes sense to calculate the column means of mat.1
colMeans(mat.1)

# but is does not makes sense to try and calculate the column means of mat.2
colMeans(mat.2)

# We can still combine mat.1 and mat.2 into a new matrix

mat.6 <- cbind(mat.1, mat.2)

mat.6

# However note that quote marks are displayed around the number now.
# This is R telling us that it  is interpreting these numbers as characters now.
# Matrices in R can contain either number or characters (not both).
# Subsequently we can no longer calculate the means of any of the columns 
mean(mat.6[,1])

# If we wish to have a object which is like a matrix with numbers in some
# columns and characters in other column we need to use a Dataframe object
# We can create a Dataframe object with the data.frame( ) command:

Data <- data.frame(mat.1, mat.2)
Data

# We can access the columns of a Dataframe by name using the $ operator

# To access the column called 'a' from the Dataframe called 'Data' use:
Data$a

# We can then calculate the mean of the entries in this column as they have been
# retained as numbers

mean(Data$a)


# We can also create a dataframe from a collections of vectors: 
Data <- data.frame(a, b, f)
Data

# We can also rename the vectors in the dataframe

Data <- data.frame(x1 = a, x2 = b, point.colour = f)
Data

# The plot( ) command may also be used to create plots from the columns of a
# dataframe as follows

plot(x = Data$x1, y = Data$x2, col = Data$point.colour, pch = 19, cex = 2)

# If all the objects to be plotted are in the same dataframe we can encase the
# plot command within a with( ) command which tells plot( ) to look for all the
# objects within the dataframe supplied with the with( ) command

with(Data, plot(x = x1, y = x2, col = point.colour, pch = 19, cex = 2))

################################################################################
#                                                                              #
#                                 Exercise                                     #
#              Creating a Dataframe, Summarising and Plotting the Contents     #
#                                                                              #
################################################################################

# Choose something easy to observe in the room in which the course is being run
# e.g. the laptops course participants are using and create a dataframe
# containing observations of characteristics of these laptops e.g.
# manufactuer, operating system, case colour, screen size, weight (if we have
# scales)...try to record at least two characteristics which have numerical
# values.
# If we are sitting at tables also include a table number/name for grouping.
# This is just a practice exercise so don't worry too much about the details,
# the main thing is to get some categories and numbers in columns next to each
# other.
#
# For ease of use with modeling functions and plotting functions have each
# variable as a named column and each observation (i.e. individual laptops) as
# an individual row.
# You could set up your dataframe to look a bit like this table

# Owner    |  Brand   |    OS   | Colour | Screen | Table
#----------------------------------------------------------
#  Jim     | Toshiba  | Windows |  Grey  |  15.6  |   1
# Jessie   |  Apple   |  MacOS  |  Grey  |  15    |   1
#  Nick    |  Apple   |  MacOS  |  Grey  |  15    |   2
# Brenda   |  Apple   |  MacOS  |  Grey  |  15    |   2 
# Mahasen  |  Dell    | Windows |  Grey  |  15.6  |   3
# Ramethaa |  Dell    | Windows |  Grey  |  15.6  |   3
# Paul     | Gigabyte | Windows |  Grey  |  14    |   4
# Mischa   |    HP    | Windows |  Grey  |  15.6  |   4  
#  Ben     |   Dell   |  Linux  |  Black |  14    |   5
#   .      |    .     |    .    |     .  |   .    |   .
#   .      |    .     |    .    |     .  |   .    |   .
#   .      |    .     |    .    |     .  |   .    |   .

# Please call your dataframe Laptops
# R provides a basic, graphical data input facility:
Laptops <- data.frame()
Laptops <- edit(Laptops)
# Right click column names and select rename
# Use the mouse or arrow keys to move between cells
# input values with the keyboard

Laptops

# We can get summary statistics for each of the columns in our new dataframe
# with the summary( ) command

summary(Laptops)

# In categorical variables can be thought of as factors
# We can convert character vectors in to factors with the factor command

factor(Laptops$OS)

# Note that factors levels are counted in the summary
summary(Laptops)

# Factors are important if you want to use categorical variables in modelling.

# Furthermore we can overwrite character vectors in a dataframe with a factor
# created from this same character vector

Laptops$OS <- factor(Laptops$OS)

# We can also make plots of the columns of this dataframe as we did above

# Note that the plot( ) command will try to produce the most appropriate type
# of plot given the nature of the supplied data

# e.g. a Histograms of factor levels:
with(Laptops, plot(OS))

# Boxplots of continuous variables grouped by factor leve:
with(Laptops, plot(OS, Screen))

# scatterplots of pairs of continuos variables:
with(Laptops, plot(Weight, Screen))

with(Laptops, plot(Weight, Screen, col = OS))

with(Laptops, plot(Weight, Screen, col = OS, pch = as.numeric(OS)))

with(Laptops, plot(Weight, Screen, col = OS, pch = as.numeric(OS), cex = 2))

# We will return to making plots in R with the specialised graphics package
# 'ggplot2' in a later module of this course.

################################################################################
#                                                                              #
#                    End of Code File 1 that Accompanies                       #
#                                                                              #
#                              Course Module 1                                 #
#                                                                              #
#                  The Basics of Command Line Computing in R                   #
#                                                                              #
################################################################################
