#     This the third R Code File for the Introduction to R Course available at
#     https://github.com/brfitzpatrick/Intro_to_R
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
#                   Code File to Accompany Course Module 3                     #
#                                                                              #
#                          Linear Regression in R                              #
#                                                                              #
################################################################################

# Hello and welcome to the code file to accompany Module III of this course.
# You will notice this code file is a lot less heavily annotated than that from
# the first module.
# If you get confuse try looking at the page for the function on the line that
# confuses you.
# To view a help page for a function simply type a  ? followed by the function
# name (with no space in between) at the terminal and execute this command.
# For example to view the help page for the lm( ) command execute the following

?lm

# This module requires R packages 'ggplot2' and 'rgl' so if you have not already
# please install these packages (you will be prompted to choose a CRAN mirror
# from which to download them - naturally your computer will need to be
# connected to the internet):

install.packages('ggplot2')

install.packages('rgl')

# Once the packages are installed you will be able to load them into memory (and
# thus make the functions they contain available for use) with the library( )
# command:

library('ggplot2')
library('rgl')

# ggplot2 has a great documentation website http://docs.ggplot2.org/current/
# with lots of example and pictures of the types of graphs different commands
# will create.

# This Module will focus modelling the data in the file names
# 'Multiple_Regression_Data.csv' which is available on the GitHub repository.
# If you downloaded and unzipped the entire repository (or indeed if you
# cloned it) the file will be at the following file path:
# ~/Intro_to_R/Data/Linear_Modelling/Multiple_Regression_Data.csv
# where the ~ represents the file path to the location of the Intro_to_R
# directory on your hard drive.

# To Read in these data navigate to and Select the
# 'Multiple_Regression_Data.csv' file in the dialogue box the that executing
# the following command opens:

Data <- read.csv(file = file.choose())

# This same take may also be accomplished by setting the working directory to
# wherever on you harddrive you saved the course files:

setwd('~/Intro_to_R/Data/Linear_Modelling/')

# then  executing
Data <- read.csv(file = 'Multiple_Regression_Data.csv')

# View a summary of the data

summary(Data)

# Plot the observations dependent variable 'y' against the observations of the
# first independent variable 'x1'
 
p1 <- ggplot(aes(x = x1, y = y), data = Data)
p1

# Note the error as we have yet to request anything be drawn having only set
# the coordinate systems
# to produce a scatter plot use the 'points' geometry

p1 + geom_point( )
p1 + geom_point(alpha = 0.5)

p1 <- p1 + geom_point(alpha = 0.5)
p1

# Plot the observations dependent variable 'y' against the observations of the
# second independent variable 'x2'

p2 <- ggplot(aes(x = x2, y = y), data = Data)
p2 + geom_point()
p2 + geom_point(alpha = 0.5)

################################################################################
#                                                                              #
#            Linear Regression with a Single Independent Variable              #
#                                                                              #
################################################################################

# Let's start with independent variable 'x1' alone and fit a linear model
# predicting 'y' from an intercept (assumed) and the independent variable 'x1'

model.1 <- lm(y ~ x1, data = Data)
model.1
class(model.1)

# we can produce a two by two grid of diagnotic plots for the linear model
# object model.1 as follows:

par(mfcol = c(2,2))
plot(model.1)

# the diagnostic plots suggest a strong trend in the residuals, this is usually
# a good indication that we need more explanatory variables in the model.

# for completeness you may view the coefficient estimate as p-values associated
# with tests of whether the coefficients are significantly different from zero
# as follows with the summary( ) command (however with diagnostic plots like
# these it would be unwise to put to much stock by the results of these tests)

summary(model.1)

# We can also extract the coefficient estimates from a linear model object with
# the coef( ) command and use them to add a line representing the predictions
# from the linear model to a scatter plot of the data as follows:

model.1
coef(model.1)
coef(model.1)[1]
coef(model.1)[2]
p1
p1 + geom_abline(intercept = coef(model.1)[1], slope = coef(model.1)[2], colour = 'blue')

# Let's calculate a 95% confidence band for the fit from model.1

# First we need to set up a vector of x1 values at which to conduct the calculations

x1.seq <- with(Data, seq(from = min(x1), to = max(x1), length.out = 500))
summary(x1.seq)
Pred.seq <- data.frame(x1 = x1.seq)
head(Pred.seq)

# then we can use the predict( ) command to calculate the fit and confidence
# band lower and upper bound values at each independent variable value in this
# vector (which we have stored in the dataframe Pred.seq)

Pred.m1 <- predict(object = model.1, newdata = Pred.seq, interval = 'confidence', level = 0.95)

head(Pred.m1)

Pred.m1 <- data.frame(x1 = x1.seq, Pred.m1)

head(Pred.m1)

# To add this data to p1 we need data that includes all the column names used to
# produce p1
colnames(Pred.m1) <- c('x1', 'y', 'lwr', 'upr')
p1

p1 + geom_line(aes(x = x1, y = y), colour = 'blue', data = Pred.m1)

p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), data = Pred.m1)

p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), fill = 'blue', alpha = 0.5, data = Pred.m1)

p1

p1 <- p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), fill = 'blue', alpha = 0.5, data = Pred.m1)
p1 <- p1 + geom_line(aes(x = x1, y = y), colour = 'blue', data = Pred.m1)
p1

################################################################################
#                                                                              #
#       Linear Regression with a Linear and a Quadratic Term for the Same      #
#                             Independent Variable                             #
#                                                                              #
################################################################################

# Let's try adding a quadratic term for x1

Data.2 <- data.frame(Data, x1.2 = Data$x1^2)
head(Data.2)
model.2 <- lm(y ~ x1 + x1.2, data = Data.2)

par(mfcol = c(2,2))
plot(model.2)
plot(model.1)

# we see less of a trend in the residuals vs fitted values from model.2 compared
# to model.1

summary(model.2)

head(Pred.seq)

# Let's calculate the predicted y values  and confidence band for model.2 and
# add them to p1 (in green)

Pred.seq <- data.frame(Pred.seq, x1.2 = Pred.seq$x1^2)
head(Pred.seq)
Pred.m2 <- predict(object = model.2, newdata = Pred.seq, interval = 'confidence', level = 0.95)

head(Pred.m2)

Pred.m2 <- data.frame(x1 = x1.seq, Pred.m2)

colnames(Pred.m2) <- c('x1', 'y', 'lwr', 'upr')
p1
p1 + geom_line(aes(x = x1, y = y), colour = 'green', data = Pred.m2)

p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), fill = 'green', alpha = 0.5, data = Pred.m2)

p1 <- p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), fill = 'green', alpha = 0.5, data = Pred.m2)
p1


################################################################################
#                                                                              # 
#                                  Exercise:                                   #
#                                                                              #
################################################################################
#                                                                              #
#      Fit a new model which uses x1 as linear, quadratic and cubic terms      #
#                                                                              #
#  then repeat the above steps:                                                #
#    1) produce the diagnostic plots                                           #
#    2) calculate a 95% confidence band for the fit                            #
#    3) add the predicted values and the confidence band to the plot 'p1'      #
#       using a different colour.                                              #
#                                                                              #
################################################################################




# Recall that the second independent variable 'x2' also seemed to be influencing
# the dependent variable y

p2 + geom_point(alpha = 0.5)

# If you'd like some more practice try doing all of the above again for x2

################################################################################
#                                                                              #
#            Linear Regression with Multiple Independent Variables,            #
#             Polynomial Terms for each Independent Variable and a             #
#                           Linear Interaction Term                            #
#                                                                              #
################################################################################

# Let's now look at linear models that use both 'x1' and 'x2'

# First of all let's plot x1, x2 and y together:

p <- ggplot(aes(x = x1, y = x2, colour = y), data = Data) + coord_equal()
p + geom_point( )
p + geom_point(size = 6)

p + geom_point(size = 6) + scale_colour_gradientn(colours = rev(heat.colors(n = 1e3)))

p + geom_point(size = 3) + scale_colour_gradientn(colours = rev(rainbow(start = 0, end = 0.7, n = 1e3)))
p + geom_point(size = 6, alpha = 0.25) + scale_colour_gradientn(colours = rev(rainbow(start = 0, end = 0.7, n = 1e3)))

# We've seen above the polynomial terms are useful for predicting y from the covariate x1
# Let's now create a dataframe containing polynomial terms up to order four for each covariate
# along with a linear interaction term:

X <- data.frame(x1 = Data$x1,
                x2 = Data$x2,
                x1.2 = Data$x1^2,
                x2.2 = Data$x2^2,
                x1.3 = Data$x1^3,
                x2.3 = Data$x2^3,
                x1.4 = Data$x1^4,
                x2.4 = Data$x2^4,
                x1x2 = Data$x1*Data$x2)

# Counting the intercept, there are 10 potential terms that could either be in
# or out of the model if we allow for polynomial terms to order four and a
# pairwise linear interaction term.
# Thus given the above full design matrix X we have

choose(10,2)

# potential different models we could fit and compare.
# With increasing number of covariate terms this number rapidly expands.

# A family of simple techniques for exploring such a potential model space
# (rather than fitting every single possible model and comparing them) is to
# employ a stepwise variable selection proceedure.
# Essentially what such techniques do is starting with either and empty
# (intercept only) model or a full (intercept plus all covariate terms) model
# the algorithm with sequentially add and remove covariate terms from the model
# until the improvement in some criteria from making further additions or
# deletions falls below some threshold amount, the available degrees of
# freedom are exhausted or an empty (intercept only) model is arrived at.
# A common and simple way to choose which covariates to add or remove at each
# step of the algorithm is to choose the addition or substraction of a covariate
# term that yield the best improvement in some information criterion (e.g. AIC,
# BIC, etc. ) relative to the current model and to halt when the improvement in
# this information criterion from subsequent additions or deletions of covariate
# terms is less than some threshold amount or a user specified maximum number of
# addition/deletion steps has been performed.

# The first step to prepare for such a search is to create model object to bound
# this search i.e. an intercept only model:

m1.lm = lm(Data$y ~ +1, data = X)
summary(m1.lm)

# and a full model containing an intercept and all the covariate terms:

m2.lm = lm(Data$y ~ ., data = X)
summary(m2.lm)

# Once we have these boundaries of the potential model space defined we are
# ready to commence our stepwise variable selection proceedure with the step( )
# function see ?step for details:

m.select <- step(object = m1.lm, scope = list(lower = m1.lm, upper = m2.lm), direction = 'both', steps = 1e5)

# step( ) prints out each step it takes in the model fitting and stores the
# returns the final model

m.select

# if you set direction = c('forward') only additions to the model are considered
# at each step i.e. you'd want to start with an empty model by setting
# object = m1.lm

m.fwrd.select <- step(object = m1.lm, scope = list(lower = m1.lm, upper = m2.lm), direction = 'forward', steps = 1e5)


# if direction = c('backward') only deletions from the model are considered
# at each step i.e. you'd want to start with a full model by setting
# object = m2.lm

m.bwrd.select <- step(object = m2.lm, scope = list(lower = m1.lm, upper = m2.lm), direction = 'backward', steps = 1e5)

# if direction = c('both') only additions or deletions from the model are
# considered at each step (where possible) so you can start with either a full
# model or and empty model or something inbetween:

m.fb.select <- step(object = m1.lm, scope = list(lower = m1.lm, upper = m2.lm), direction = 'both', steps = 1e5)

# We can compare the selected models by sorting the fitted covariates as follows
sort(coef(m.fwrd.select))
sort(coef(m.bwrd.select))
sort(coef(m.fb.select))

# Here we have the fortuitious situtaion of having arrived at the same model
# by each of these three searches so there is no need to choose between their
# results.  When forward, backwards and forward/backward variable selection
# searches yield different final models from the same full potential design
# matrix a simple way to choose between them is to choose the model with the
# overall best infromation criterion value if two (or more) models are almost
# identical in this regard this is worth reporting! (All model are at best
# approximations of the processes that produced the data).

m.select <- m.fb.select

# Next let's examine the diagnostic plots for the selected model

par(mfcol = c(2,2))
plot(m.select)
# (much improved)

# As we have two explanatory variables ('x1' and 'x2') and single response
# variable 'y' predictions from the selected model will form a surface in
# three dimensional space.
# A surface in three dimensional space be readily represented by a coloured
# two dimensional image where colour is proportional to the coordinate in the
# third dimension.
# To produce a plot of this predicted surface we create a regular grid of points
# on the x1 - x2 plane at which to evaluated the selected model:

Pred.at.df <- expand.grid(seq(from = min(X$x1), to = max(X$x1), length.out = 500),seq(from = min(X$x2), to = max(X$x2), length.out = 500))

head(Pred.at.df)
dim(Pred.at.df)

colnames(Pred.at.df) <- c('x1','x2')

# We then need to expand this set of points to include the polynomial terms and
# interaction term for each of the points in the grid

Pred.at.df.full <- data.frame(x1 = Pred.at.df$x1,
                x2 = Pred.at.df$x2,
                x1.2 = Pred.at.df$x1^2,
                x2.2 = Pred.at.df$x2^2,
                x1.3 = Pred.at.df$x1^3,
                x2.3 = Pred.at.df$x2^3,
                x1.4 = Pred.at.df$x1^4,
                x2.4 = Pred.at.df$x2^4,
                x1x2 = Pred.at.df$x1*Pred.at.df$x2)

# Equipped with this regular grid of covariate values we can then evalate the
# model at each of the points in the grid

y.hat <- predict(object = m.select, newdata = Pred.at.df.full)

# To plot the predictions we need them in a dataframe alongside the explanatory
# variable values from which they were calculated:

Prediction <- data.frame(y = y.hat, x1 = Pred.at.df.full$x1, x2 = Pred.at.df.full$x2)

p2 <- ggplot(aes(x = x1, y = x2, fill = y), data = Prediction) + coord_equal()
# the default colour scale is similar to a grey scale but for hues of blue:
p2 + geom_raster()
# often a grey scale image will be required by journals unless you wish to pay
# (a lot) for a colour graphic:
p2 + geom_raster() + scale_fill_gradientn(colours = grey(level = (1:1e4)/1e4))
# contour plots are also an addition
p2 + geom_raster()  + scale_fill_gradientn(colours = grey(level = (1:1e4)/1e4))+ geom_contour(aes(z = y), colour = 'white')
# a gradient from red through yellow to white is one popular choice if a colour
# scale is permissable
p2 + geom_raster() + scale_fill_gradientn(colours = heat.colors(n = 1e3))
# a rainbow colour gradient is another one popular choice if a colour scale is
# permissable bear in mind though you may have colourblind readers/audience
# members for whom such a colour scale will likely confound interpretation of
# your figure:
p2 + geom_raster() + scale_fill_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)))
# terrain themed colours are another option
p2 + geom_raster() + scale_fill_gradientn(colours = terrain.colors(n = 1e4, alpha = 0.4))



# In order to depict the proximity of the predicted surface to the observed data
# we may plot the predicted values as a raster on the x1 - x2 plane with colour
# proportional to the predicted y values then superimpose on this plot points
# where the observed y value is proportional to the point colour.
# For this graphic to be most easily understood it is best to use the same
# colour scale and colour scale limits for the observed and predicted y values.

range(Data$y)
range(Prediction$y)

# Of the observed and predicted y values the observed y values has the most
# extreme values so we will use those to define the colour scale:

scale.limits = range(Data$y)

# 

# Please choose a colour scale that is intelligible to you and update the p2
# object to include the raster plot of the predicted y values that uses this
# colour scale e.g.

p2 <- p2 + geom_raster() + scale_fill_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)), limits = scale.limits)

p2

# It will be useful if the coloured points representing the observations have
# black borders to differentiate them from the surrounding predicted values.
# One simple way to achieve this is to overlay black points with smaller
# coloured points:

p2 + geom_point(colour = 'black', size = 3, data = Data) + geom_point(aes(colour = y), size = 2, data = Data) + scale_colour_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)), limits = scale.limits)



p2 <- p2 + geom_point(colour = 'black', size = 3, data = Data)
p2 <- p2 + geom_point(aes(colour = y), size = 2, data = Data) + scale_colour_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)), limits = scale.limits)
p2

################################################################################
#                                                                              #
#        The final section contains a little programming so don't feel         #
#       you need to be able to follow the code exaclty until you've done       #
#                    the programming module of this course                     #
#                                                                              #
################################################################################
#                                                                              #
# 3 Dimensional plots are useful in this scenario for displaying the bounds of #
#            confidence or prediction regions around the fit                   #
#                                                                              #
################################################################################

# for 3D ploting we are going to use the R package 'rgl' :

library('rgl')
open3d(antialias = 4, bg = list(color = 'black'))
# if this throws an error delete the 'antialias = 4' in the above command
with(Data, rgl.spheres(x = x1, z = x2, y = y/max(abs(y)), radius = 0.005, color = 'cyan', alpha = 0.5))
axes3d(color = 'white',alpha = 1)
title3d(xlab = 'x1', ylab = 'y', zlab = 'x2', color = 'white', size = 11)
# click and drag the plot with the mouse to rotate the plot
# use the mouse wheel to zoom in and out on the plot

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

n.pred = 1e4
n1e2.pred.at.x1 <- seq(from = min(Data$x1), to = max(Data$x1), length.out = sqrt(n.pred))
n1e2.pred.at.x2 <- seq(from = min(Data$x2), to = max(Data$x2), length.out = sqrt(n.pred))

n1e4.pred.at.df <- expand.grid(n1e2.pred.at.x1, n1e2.pred.at.x2)
head(n1e4.pred.at.df)
colnames(n1e4.pred.at.df) <- c('x1', 'x2')
n1e4.pred.at.df.full <- data.frame(x1 = n1e4.pred.at.df$x1,
                x2 = n1e4.pred.at.df$x2,
                x1.2 = n1e4.pred.at.df$x1^2,
                x2.2 = n1e4.pred.at.df$x2^2,
                x1.3 = n1e4.pred.at.df$x1^3,
                x2.3 = n1e4.pred.at.df$x2^3,
                x1.4 = n1e4.pred.at.df$x1^4,
                x2.4 = n1e4.pred.at.df$x2^4,
                x1x2 = n1e4.pred.at.df$x1*n1e4.pred.at.df$x2)

n1e4.lwr.mat <- matrix(data = NA, nrow = length(n1e2.pred.at.x1), ncol = length(n1e2.pred.at.x2))
n1e4.fit.mat <- matrix(data = NA, nrow = length(n1e2.pred.at.x1), ncol = length(n1e2.pred.at.x2))
n1e4.upr.mat <- matrix(data = NA, nrow = length(n1e2.pred.at.x1), ncol = length(n1e2.pred.at.x2))

for(i in 1:length(n1e2.pred.at.x1)){
    for(j in 1:length(n1e2.pred.at.x2)){
         pred.ij = predict(object = m.select, newdata = n1e4.pred.at.df.full[n1e4.pred.at.df.full$x1 == n1e2.pred.at.x1[i] & n1e4.pred.at.df.full$x2 == n1e2.pred.at.x2[j], ], interval = 'prediction', level = 0.95)
        n1e4.lwr.mat[i,j] <- pred.ij[,'lwr']
        n1e4.fit.mat[i,j] <- pred.ij[,'fit']
        n1e4.upr.mat[i,j] <- pred.ij[,'upr']}}


rgl.surface(x = n1e2.pred.at.x1, z = n1e2.pred.at.x2, y = n1e4.fit.mat/max(abs(Data$y)), alpha = 0.25, col = 'green')

##############################
#                            #
#  Surfaces defined by 80%   #
#   Prediction Intervals     #
#                            #
##############################
rgl.pop()

rgl.surface(x = n1e2.pred.at.x1, z = n1e2.pred.at.x2, y = n1e4.lwr.mat/max(abs(Data$y)), alpha = 0.25, col = 'yellow')

rgl.surface(x = n1e2.pred.at.x1, z = n1e2.pred.at.x2, y = n1e4.upr.mat/max(abs(Data$y)), alpha = 0.25, col = 'orange')

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

# Warning this takes a while:

for(i in 1:nrow(X)){
    lines3d(x = c(X[i,'x1'],X[i,'x1']), z = c(X[i,'x2'],X[i,'x2']), y =  c(predict(object = m.select, newdata = X[i, ]), Data[i,'y'])/max(abs(Data$y)), col = 'white')}

rgl.surface(x = n1e2.pred.at.x1, z = n1e2.pred.at.x2, y = n1e4.fit.mat/max(abs(Data$y)), alpha = 0.25, col = 'green')

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
# Practice producing   #
#  individual frames   #
#  for an animation    #
#  setting up vectors  #
#   of point of view   #
#  control parameters  #
#  to use during the   #
#     animation        #
#                      #
########################

fps <- 25
sec <- 20
n.frame = fps * sec
theta.v = seq(from = 0 , to = 360, length.out = n.frame)
zoom.v = c(seq(from = 0.75, to = 0.25, length.out = n.frame/2), seq(from = 0.25, to = 0.75, length.out = n.frame/2))
rgl.light(viewpoint.rel = TRUE)
for(i in 1:length(theta.v)){
    view3d(theta = theta.v[i], phi = 5, zoom = zoom.v[i])}

########################
#                      #
#    Write out the     #
#  individual frames   #
#  for an animation    #
#  setting up vectors  #
#   of point of view   #
#  control parameters  #
#  to use throughout   #
# during the animation #
#                      #
########################

# Create a directory in which to store the frame for the animation.
# Then set the R working directory to this directory e.g.:
setwd('~/Animation/MLR_80pc_Pred_Int/Frames/')

fps <- 25
sec <- 20
n.frame = fps * sec
theta.v = seq(from = 0 , to = 360, length.out = n.frame)
zoom.v = c(seq(from = 0.75, to = 0.25, length.out = n.frame/2), seq(from = 0.25, to = 0.75, length.out = n.frame/2))
rgl.light(viewpoint.rel = TRUE)
for(i in 1:length(theta.v)){
    view3d(theta = theta.v[i], phi = 5, zoom = zoom.v[i])
    rgl.snapshot(filename = paste(i,'.png',sep = ''))}

# To Convert the Frames into an Animation:
#    1) Open the first frame '1.png' with the GIMP
#       (GIMP = GNU Image Manipulation Program
#        www.gimp.org)
#    2) Use the GAP (GIMP Animation Package)
#       to covert the frames into a video.
#       (From the 'Video' menu select 'Master Video Encoder')
#       .avi is the default format.
#  
#    Note you may need to install the GAP add-on to the GIMP
#    or it may install by default (if you don't have a 'Video'
#    menu then you need to install the GAP.
#    
#    See Example: https://youtu.be/mIDuPWqu0_4

################################################################################
#                                                                              #
#                End of Code File to Accompany Course Module 3                 #
#                                                                              #
#                          Linear Regression in R                              #
#                                                                              #
################################################################################
