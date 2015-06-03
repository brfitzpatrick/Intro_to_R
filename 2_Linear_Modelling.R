#     This the second R Code File for the Introduction to R Course available at
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
#                   Code File to Accompany Course Module 2                     #
#                                                                              #
#                          Linear Regression in R                              #
#                                                                              #
################################################################################

# Read in the data



setwd('~/Intro_to_R/Data/Linear_Modelling/')

Data <- read.csv(file = 'Multiple_Regression_Data.csv')

p <- ggplot(aes(x = x1, y = x2, colour = y), data = Data) + coord_equal()

p + geom_point() + scale_colour_gradientn(colours = rev(rainbow(start = 0, end = 0.7, n = 1e3)))

X <- data.frame(x1 = Data$x1,
                x2 = Data$x2,
                x1.2 = Data$x1^2,
                x2.2 = Data$x2^2,
                x1.3 = Data$x1^3,
                x2.3 = Data$x2^3,
                x1.4 = Data$x1^4,
                x2.4 = Data$x2^4,
                x1x2 = Data$x1*Data$x2)

m1.lm = lm(Data$y ~ +1, data = X)

m2.lm = lm(Data$y ~ ., data = X)

m.select <- step(object = m1.lm, scope = list(lower = m1.lm, upper = m2.lm), direction = 'both', steps = 1e5)

par(mfcol = c(2,2))
plot(m.select)

Pred.at.df <- expand.grid(seq(from = min(X$x1), to = max(X$x1), length.out = 500),seq(from = min(X$x2), to = max(X$x2), length.out = 500))

colnames(Pred.at.df) <- c('x1','x2')

Pred.at.df.full <- data.frame(x1 = Pred.at.df$x1,
                x2 = Pred.at.df$x2,
                x1.2 = Pred.at.df$x1^2,
                x2.2 = Pred.at.df$x2^2,
                x1.3 = Pred.at.df$x1^3,
                x2.3 = Pred.at.df$x2^3,
                x1.4 = Pred.at.df$x1^4,
                x2.4 = Pred.at.df$x2^4,
                x1x2 = Pred.at.df$x1*Pred.at.df$x2)

Obj <- predict(object = m.select, newdata = Pred.at.df.full)

Y.Pred <- data.frame(y = Obj, x1 = Pred.at.df.full$x1, x2 = Pred.at.df.full$x2)

scale.limits = range(Data$y)

p2 <- ggplot(aes(x = x1, y = x2, fill = y), data = Y.Pred) + coord_equal()
p2 + geom_raster() + scale_fill_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)))
p2 + geom_raster() + scale_fill_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)), limits = scale.limits) + coord_equal()  + geom_point(colour = 'black', size = 3, data = Data) + geom_point(aes(colour = y), size = 2, data = Data) + scale_colour_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)), limits = scale.limits )

### End New Code

## Next up adding rgl surface & points plot
##  rgl confidence surfaces  i.e. edit the following to work:

library('rgl')
with(Data, rgl.spheres(x = x1, z = x2, y = y/max(abs(y)), radius = 0.005, color = 'cyan', alpha = 0.5))
axes3d(color = 'white',alpha = 1)
title3d(xlab = 'x1', ylab = 'y', zlab = 'x2', color = 'white', size = 11)

#####################
#                   #
#       Add a       #
#  semi-transparent #
#   green plane to  #
#   represent the   #
#  predictions from #
#  the linear model #
#                   #
#####################


####

n.pred = 1e4
pred.at.x1 <- seq(from = min(Data$x1), to = max(Data$x1), length.out = sqrt(n.pred))
pred.at.x2 <- seq(from = min(Data$x2), to = max(Data$x2), length.out = sqrt(n.pred))

# make a function that accepts pred.at.x1, pred.at.x2, n.pred as arguments and returns pred.mat
pred.at.df <- expand.grid(pred.at.x1, pred.at.x2)
head(pred.at.df)
colnames(pred.at.df) <- c('x1', 'x2')
Pred.at.df.full <- data.frame(x1 = pred.at.df$x1,
                x2 = pred.at.df$x2,
                x1.2 = pred.at.df$x1^2,
                x2.2 = pred.at.df$x2^2,
                x1.3 = pred.at.df$x1^3,
                x2.3 = pred.at.df$x2^3,
                x1.4 = pred.at.df$x1^4,
                x2.4 = pred.at.df$x2^4,
                x1x2 = pred.at.df$x1*pred.at.df$x2)

pred.mat <- matrix(data = NA, nrow = length(pred.at.x1), ncol = length(pred.at.x2))

for(i in 1:length(pred.at.x1)){

    for(j in 1:length(pred.at.x2)){

        pred.mat[i,j] <- predict(object = m.select, newdata = Pred.at.df.full[Pred.at.df.full$x1 == pred.at.x1[i] & Pred.at.df.full$x2 == pred.at.x2[j], ])}}


rgl.surface(x = pred.at.x1, z = pred.at.x2, y = pred.mat/max(abs(Data$y)), alpha = 0.25, col = 'green')


#####################
#                   #
#  Add a vertical   #
#  white lines to   #
#    represent      #
#  residuals from   #
#    the fitted     #
#      model        #
#                   #
#####################

for(i in 1:nrow(X)){
    lines3d(x = c(X[i,'x1'],X[i,'x1']), z = c(X[i,'x2'],X[i,'x2']), y =  c(predict(object = m.select, newdata = X[i, ]), Data[i,'y'])/max(abs(Data$y)), col = 'white')}



### Edited to here
## Next add in 95% confidence interval




for(i in 1:n.bs){
    Data.bs = Data[sample(x = 1:nrow(Data), size = nrow(Data), replace = TRUE),]
    m.i = lm(y ~ x + z, data = Data.bs)
    pred.y.bs[,i] <- predict(object = m.i, newdata = pred.at.df)} # requires ~ 45 sec

# ... any questions so far?

dim(pred.y.bs)
pred.y.bs.stats <- data.frame(matrix(data = NA, nrow = n.pred, ncol = 2))
colnames(pred.y.bs.stats) <- c('lower', 'upper')
head(pred.y.bs.stats)
for(i in 1:nrow(pred.y.bs)){
    pred.y.bs.stats[i,] <- quantile(x = pred.y.bs[i,], prob = c(0.025, 0.975))}
pred.y.bs.stats <- data.frame(pred.at.df, pred.y.bs.stats)
dim(pred.y.bs.stats)
bs.L.pred.mat <- matrix(data = NA, nrow = length(pred.at.x), ncol = length(pred.at.z))
for(i in 1:length(pred.at.x)){
    for(j in 1:length(pred.at.z)){
        bs.L.pred.mat[i,j] <- pred.y.bs.stats[pred.y.bs.stats$x == pred.at.x[i] & pred.y.bs.stats$z == pred.at.z[j],'lower']}}
rgl.surface(x = pred.at.x, z = pred.at.z, y = bs.L.pred.mat, alpha = 0.25, col = 'blue')



###



#####################
#                   #
#      Exercise     #
#                   #




## Change this to prediction and confidence intervals

#####################
#
# Bootstrap the data
# and refit the model
# to each boostrap
# resampling of
# the data
# predicting at our
# prediction grid
# with each of these
# model then calculate
# the bounds of intervals
# containing 95% of the
# predictions at each of
# these locations in
# the prediction grid
# (values of the explanatory
# variable x and z)
# plot the bounds of these
# intervals as red and blue
# surfaces
#
#####################

n.pred = 1e4
pred.at.x = seq(from = -200, to = 200, length = sqrt(n.pred))
pred.at.z <- pred.at.x
pred.at.df <- expand.grid(pred.at.x, pred.at.z)
head(pred.at.df)
colnames(pred.at.df) <- c('x', 'z')
n.bs = 1e4
pred.y.bs <- matrix(data = NA, nrow = n.pred, ncol = n.bs)
for(i in 1:n.bs){
    Data.bs = Data[sample(x = 1:nrow(Data), size = nrow(Data), replace = TRUE),]
    m.i = lm(y ~ x + z, data = Data.bs)
    pred.y.bs[,i] <- predict(object = m.i, newdata = pred.at.df)} # requires ~ 45 sec

# ... any questions so far?

dim(pred.y.bs)
pred.y.bs.stats <- data.frame(matrix(data = NA, nrow = n.pred, ncol = 2))
colnames(pred.y.bs.stats) <- c('lower', 'upper')
head(pred.y.bs.stats)
for(i in 1:nrow(pred.y.bs)){
    pred.y.bs.stats[i,] <- quantile(x = pred.y.bs[i,], prob = c(0.025, 0.975))}
pred.y.bs.stats <- data.frame(pred.at.df, pred.y.bs.stats)
dim(pred.y.bs.stats)
bs.L.pred.mat <- matrix(data = NA, nrow = length(pred.at.x), ncol = length(pred.at.z))
for(i in 1:length(pred.at.x)){
    for(j in 1:length(pred.at.z)){
        bs.L.pred.mat[i,j] <- pred.y.bs.stats[pred.y.bs.stats$x == pred.at.x[i] & pred.y.bs.stats$z == pred.at.z[j],'lower']}}
rgl.surface(x = pred.at.x, z = pred.at.z, y = bs.L.pred.mat, alpha = 0.25, col = 'blue')
rgl.surface(x = pred.at.x, z = pred.at.z, y = bs.L.pred.mat, alpha = 0.5, col = 'white', front = c('line'), back = 'line')
bs.U.pred.mat <- matrix(data = NA, nrow = length(pred.at.x), ncol = length(pred.at.z))
for(i in 1:length(pred.at.x)){
    for(j in 1:length(pred.at.z)){
        bs.U.pred.mat[i,j] <- pred.y.bs.stats[pred.y.bs.stats$x == pred.at.x[i] & pred.y.bs.stats$z == pred.at.z[j],'upper']}}
rgl.surface(x = pred.at.x, z = pred.at.z, y = bs.U.pred.mat, alpha = 0.25, col = 'red')
rgl.surface(x = pred.at.x, z = pred.at.z, y = bs.U.pred.mat, alpha = 0.5, col = 'white', front = c('line'), back = 'line')

#######################
#                     #
# Specify the point   #
# of view numerically #
#                     #
#######################

view3d(theta = 45, phi = 15, zoom = 1)
view3d(theta = 45, phi = 15, zoom = 0.75)
view3d(theta = 45, phi = 15, zoom = 0.5)
view3d(theta = 45, phi = 15, zoom = 0.25)

view3d(theta = 45, phi = 15, zoom = 0.25)
view3d(theta = 45, phi = 20, zoom = 0.25)
view3d(theta = 45, phi = 25, zoom = 0.25)
view3d(theta = 45, phi = 30, zoom = 0.25)
view3d(theta = 45, phi = 45, zoom = 0.25)

view3d(theta = 45, phi = 45, zoom = 0.25)
view3d(theta = 40, phi = 45, zoom = 0.25)
view3d(theta = 35, phi = 45, zoom = 0.25)
view3d(theta = 30, phi = 45, zoom = 0.25)

########################
#                      #
#    Write out the     #
#  individual frames   #
#    for animation     #
#  setting up vectors  #
#   of point of view   #
#  control parameters  #
#  to use throughout   #
# during the animation #
#                      #
########################

fps <- 25
sec <- 20
n.frame = fps * sec
theta.v = seq(from = 0 , to = 360, length.out = n.frame)
zoom.v = c(seq(from = 0.75, to = 0.25, length.out = n.frame/2), seq(from = 0.25, to = 0.75, length.out = n.frame/2))
setwd('/home/ben/PhD/BRAG_Visualisation_Course/rgl_talk/3D_LM_Demo/Frames_1_AA/')
rgl.light(viewpoint.rel = TRUE)
for(i in 1:length(theta.v)){
    view3d(theta = theta.v[i], phi = 5, zoom = zoom.v[i])
    rgl.snapshot(filename = paste(i,'.png',sep = ''))}

# Open the frames as layers in GIMP
# GIMP = GNU Image Manipulation Program
# www.gimp.org
# Use the GAP = GIMP Animation Package
# to covert the frames into a video format
# .avi is the default format

#####################
#                   #
#      Exercise     #
#                   #
#####################
#                   #
# Create Surfaces   #
# from the bounds   #
# of 95% Prediction #
#  and Confidence   #
#   Intervals       #
# and Graphically   #
# compare the two   #
#                   #
#####################

Pred.Int <- predict(object = m.select, newdata = pred.at.df, interval = 'prediction', level = 0.95)
open3d(antialias = 4)
rgl.bg()
with(Data, rgl.spheres(x = x, y = y, z = z, radius = 1.5))
axes3d(color = 'white',alpha = 1)
title3d(xlab = 'x', ylab = 'y', zlab = 'z', color = 'white', size = 11)
head(Pred.Int.df)
Pred.Int.df <- data.frame(pred.at.df, Pred.Int)
head(Pred.Int.df)
PI.L.pred.mat <- matrix(data = NA, nrow = length(pred.at.x), ncol = length(pred.at.z))
for(i in 1:length(pred.at.x)){
    for(j in 1:length(pred.at.z)){
        PI.L.pred.mat[i,j] <- Pred.Int.df[Pred.Int.df$x == pred.at.x[i] & Pred.Int.df$z == pred.at.z[j],'lwr']}}
rgl.surface(x = pred.at.x, z = pred.at.z, y = PI.L.pred.mat, alpha = 0.25, col = 'blue')
rgl.surface(x = pred.at.x, z = pred.at.z, y = PI.L.pred.mat, alpha = 0.5, col = 'white', front = c('line'), back = 'line')
PI.U.pred.mat <- matrix(data = NA, nrow = length(pred.at.x), ncol = length(pred.at.z))
for(i in 1:length(pred.at.x)){
    for(j in 1:length(pred.at.z)){
        PI.U.pred.mat[i,j] <- Pred.Int.df[Pred.Int.df$x == pred.at.x[i] & Pred.Int.df$z == pred.at.z[j],'upr']}}
rgl.surface(x = pred.at.x, z = pred.at.z, y = PI.U.pred.mat, alpha = 0.25, col = 'blue')
rgl.surface(x = pred.at.x, z = pred.at.z, y = PI.U.pred.mat, alpha = 0.5, col = 'white', front = c('line'), back = 'line')
Conf.Int <- predict(object = m.select, newdata = pred.at.df, interval = 'confidence', level = 0.95)
Conf.Int.df <- data.frame(pred.at.df, Conf.Int)
CI.L.pred.mat <- matrix(data = NA, nrow = length(pred.at.x), ncol = length(pred.at.z))
for(i in 1:length(pred.at.x)){
    for(j in 1:length(pred.at.z)){
        CI.L.pred.mat[i,j] <- Conf.Int.df[Conf.Int.df$x == pred.at.x[i] & Conf.Int.df$z == pred.at.z[j],'lwr']}}
rgl.surface(x = pred.at.x, z = pred.at.z, y = CI.L.pred.mat, alpha = 0.25, col = 'blue')
rgl.surface(x = pred.at.x, z = pred.at.z, y = CI.L.pred.mat, alpha = 0.5, col = 'white', front = c('line'), back = 'line')
CI.U.pred.mat <- matrix(data = NA, nrow = length(pred.at.x), ncol = length(pred.at.z))
for(i in 1:length(pred.at.x)){
    for(j in 1:length(pred.at.z)){
        CI.U.pred.mat[i,j] <- Conf.Int.df[Conf.Int.df$x == pred.at.x[i] & Conf.Int.df$z == pred.at.z[j],'upr']}}
rgl.surface(x = pred.at.x, z = pred.at.z, y = CI.U.pred.mat, alpha = 0.25, col = 'blue')
rgl.surface(x = pred.at.x, z = pred.at.z, y = CI.U.pred.mat, alpha = 0.5, col = 'white', front = c('line'), back = 'line')

#####################
#                   #
#    Graphically    #
#    Compare the    #
#  Three Interavls  #
#      we have      #
# Calculated today  #
#                   #
#####################

open3d(antialias = 4)
rgl.bg()
rgl.light(viewpoint.rel = TRUE)
with(Data, rgl.spheres(x = x, y = y, z = z, radius = 5))
axes3d(color = 'white',alpha = 1)
title3d(xlab = 'x', ylab = 'y', zlab = 'z', color = 'white', size = 11)
rgl.surface(x = pred.at.x, z = pred.at.z, y = CI.L.pred.mat, col = 'blue', alpha = 0.225, front = c('line'), back = 'line')
rgl.surface(x = pred.at.x, z = pred.at.z, y = CI.U.pred.mat, col = 'blue',alpha = 0.225, front = c('line'), back = 'line')
rgl.surface(x = pred.at.x, z = pred.at.z, y = PI.L.pred.mat, col = 'green',alpha = 0.225, front = c('line'), back = 'line')
rgl.surface(x = pred.at.x, z = pred.at.z, y = PI.U.pred.mat, col = 'green',alpha = 0.225, front = c('line'), back = 'line')
rgl.surface(x = pred.at.x, z = pred.at.z, y = CI.L.pred.mat, col = 'blue', alpha = 0.225)
rgl.surface(x = pred.at.x, z = pred.at.z, y = CI.U.pred.mat, col = 'blue',alpha = 0.225)
rgl.surface(x = pred.at.x, z = pred.at.z, y = PI.L.pred.mat, col = 'green',alpha = 0.225)
rgl.surface(x = pred.at.x, z = pred.at.z, y = PI.U.pred.mat, col = 'green',alpha = 0.225)

fps <- 25
sec <- 20
n.frame = fps * sec
theta.v = seq(from = 0 , to = 360, length.out = n.frame)
zoom.v = c(seq(from = 0.75, to = 0.15, length.out = n.frame/2), seq(from = 0.15, to = 0.75, length.out = n.frame/2))
setwd('/home/ben/PhD/BRAG_Visualisation_Course/rgl_talk/3D_LM_Demo/Frame_95CI_cf_95PI/')

for(i in 1:length(theta.v)){
    view3d(theta = theta.v[i], phi = 5, zoom = zoom.v[i])
    rgl.snapshot(filename = paste(i,'.png',sep = ''))}







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

m.select <- with(Data,lm(y~x)) 

abline(m.select) # add the line predicted by m.select to the plot

summary(m.select)

dev.new() # open a new plot device

par(mfcol=c(2,2)) # set up a 2 by 2 plot panel

plot(m.select) # produce diagnostics plots for the model 'm.select'


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

abline(m.select)

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

legend(x='bottomleft',lty=c(1,1,1,1,2,2,3,3),col=c('black','blue','green','red','green','red','green','red'),legend=c('m.select','m2','m4.green','m4.red','m5.green','m5.red','m6.green','m6.red')) # adding a legend to the plot

#the plot is a little crowded so let's make a panel

dev.new()

par(mfrow=c(2,3)) # 2x2 panel of plots filled 'by-row'

with(Data,plot(x,y,col=as.character(f),main='Linear Model')) #1st item in panel

abline(m.select)

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
# which of m.select to m6 is best?
# one way to answer this question is with stepwise variable selection using the Akike Information Criterion (AIC)

 step(object=m.select,scope=list(lower=m.select,upper=m6),direction=c('both')) # stepwise variable selection with the AIC as the criterion used to choose between models

#essentially what this is doing is starting with the model provided as the 'object' argument and taking the model provided as 'lower' as the simplest model and the model provided as 'upper' as the most complex model
#then looking at all possible variable additions and deletions from the model
#calculating the AIC change that would be associated with each of these additions or deletions
#then making the change that results in the best change in AIC
#continuing to do this until there is no addition/deletion that would improve the AIC anymore
#if direction=c('forward') only additions to the model are considered at each step
#if direction=c('backward') only deletions from the model are considered each step

################################################################################
#                                                                              #
#                End of Code File to Accompany Course Module 2                 #
#                                                                              #
#                          Linear Regression in R                              #
#                                                                              #
################################################################################


