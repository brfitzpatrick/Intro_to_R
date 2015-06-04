#     This the second R Code File for the Introduction to R Course available at
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
#                   Code File to Accompany Course Module 2                     #
#                                                                              #
#                          Linear Regression in R                              #
#                                                                              #
################################################################################

# This module requires R packages 'ggplot2' and 'rgl' so if you have not already
# please install these packages (you will be prompted to choose a CRAN mirror
# from which to download them - naturally your computer will need to be
# connected to the internet):

install.packages('ggplot2')

install.packages('rgl')

# Read in the data

setwd('~/Intro_to_R/Data/Linear_Modelling/')

Data <- read.csv(file = 'Multiple_Regression_Data.csv')

library('ggplot2')

summary(Data)

p1 <- ggplot(aes(x = x1, y = y), data = Data)
p1 + geom_point() #+ geom_smooth()
p1 + geom_point(alpha = 0.5)
p1 <- p1 + geom_point(alpha = 0.5)


p2 <- ggplot(aes(x = x2, y = y), data = Data)
p2 + geom_point() #+ geom_smooth()
p2 + geom_point(alpha = 0.5) #+ geom_smooth()

## Let's start with independent variable 'x1' alone

model.1 <- lm(y ~ x1, data = Data)
model.1
class(model.1)
summary(model.1)

par(mfcol = c(2,2))
plot(model.1)


model.1
coef(model.1)
coef(model.1)[1]
coef(model.1)[2]

p1 + geom_abline(intercept = coef(model.1)[1], slope = coef(model.1)[2], colour = 'blue')

# Let's calculate a 95% confidence band for the fit from model.1

# First we need to set up a vector of x1 values at which to conduct the calculations

x1.seq <- with(Data, seq(from = min(x1), to = max(x1), length.out = 500))

Pred.seq <- data.frame(x1 = x1.seq)


Pred.m1 <- predict(object = model.1, newdata = Pred.seq, interval = 'confidence', level = 0.95)

head(Pred.m1)

Pred.m1 <- data.frame(x1 = x1.seq, Pred.m1)

head(Pred.m1)

colnames(Pred.m1) <- c('x1', 'y', 'lwr', 'upr')

p1 + geom_line(aes(x = x1, y = y), colour = 'blue', data = Pred.m1)

p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), data = Pred.m1)


p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), fill = 'blue', alpha = 0.5, data = Pred.m1)


p1 <- p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), fill = 'blue', alpha = 0.5, data = Pred.m1)
p1 <- p1 + geom_line(aes(x = x1, y = y), colour = 'blue', data = Pred.m1)
p1

# Let's try adding a quadratic term for x1

Data.2 <- data.frame(Data, x1.2 = Data$x1^2)

model.2 <- lm(y ~ x1 + x1.2, data = Data.2)

par(mfcol = c(2,2))
plot(model.2)
plot(model.1)
# so we have an improvement

summary(model.2)

head(Pred.seq)

Pred.seq <- data.frame(Pred.seq, x1.2 = Pred.seq$x1^2)

Pred.m2 <- predict(object = model.2, newdata = Pred.seq, interval = 'confidence', level = 0.95)

head(Pred.m2)

Pred.m2 <- data.frame(x1 = x1.seq, Pred.m2)

colnames(Pred.m2) <- c('x1', 'y', 'lwr', 'upr')

p1 + geom_line(aes(x = x1, y = y), colour = 'green', data = Pred.m2)

p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), fill = 'green', alpha = 0.5, data = Pred.m2)

p1 <- p1 + geom_ribbon(aes(x = x1, ymin = lwr, ymax = upr), fill = 'green', alpha = 0.5, data = Pred.m2)

# Your turn - fit a new model with uses x1 as linear, quadratic and cubic terms
# then repeat the above steps:
#    1) produce the diagnostic plots
#    2) calculate a 95% confidence band for the fit
#    3) add the predicted values and the confidence band to the fit using a different colour

# Recall that the second independent variable 'x2' also seemed to be influencing the dependent variable y

p2 + geom_point(alpha = 0.5)


# If you'd like some more practice try doing all of the above again for x2

# Let's now look at linear models that use both 'x1' and 'x2'

# First of all let's plot x1, x2 and y together:

p <- ggplot(aes(x = x1, y = x2, colour = y), data = Data) + coord_equal()
p + geom_point()
p + geom_point() + scale_colour_gradientn(colours = heat.colors(n = 1e3))
p + geom_point() + scale_colour_gradientn(colours = rev(rainbow(start = 0, end = 0.7, n = 1e3)))

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

m1.lm = lm(Data$y ~ +1, data = X)

m2.lm = lm(Data$y ~ ., data = X)

# which of model is best?
# one way to answer this question is with stepwise variable selection using the Akike Information Criterion (AIC)

m.select <- step(object = m1.lm, scope = list(lower = m1.lm, upper = m2.lm), direction = 'both', steps = 1e5)

#essentially what this is doing is starting with the model provided as the 'object' argument and taking the model provided as 'lower' as the simplest model and the model provided as 'upper' as the most complex model
#then looking at all possible variable additions and deletions from the model
#calculating the AIC change that would be associated with each of these additions or deletions
#then making the change that results in the best change in AIC
#continuing to do this until there is no addition/deletion that would improve the AIC anymore
#if direction=c('forward') only additions to the model are considered at each step
#if direction=c('backward') only deletions from the model are considered each step


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

y.hat <- predict(object = m.select, newdata = Pred.at.df.full)

Prediction <- data.frame(y = y.hat, x1 = Pred.at.df.full$x1, x2 = Pred.at.df.full$x2)

range(Data$y)
range(Prediction$y)

scale.limits = range(Data$y)

p2 <- ggplot(aes(x = x1, y = x2, fill = y), data = Prediction) + coord_equal()

p2 + geom_raster() + scale_fill_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)))

p2 + geom_raster() + scale_fill_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)), limits = scale.limits) + coord_equal()  + geom_point(colour = 'black', size = 3, data = Data) + geom_point(aes(colour = y), size = 2, data = Data) + scale_colour_gradientn(colours = rev(rainbow(n = 1e3, start = 0, end = 0.7)), limits = scale.limits )


################################################################################
#                                                                              #
# 3 Dimensional plots are useful in this scenario for displaying the bounds of #
#            confidence or prediction regions around the fit                   #
#                                                                              #
################################################################################

# for 3D ploting we are going to use the R package 'rgl' :

library('rgl')
open3d(antialias = 4, bg = list(color = 'black'))
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

n.pred = 1e4
n1e2.pred.at.x1 <- seq(from = min(Data$x1), to = max(Data$x1), length.out = sqrt(n.pred))
n1e2.pred.at.x2 <- seq(from = min(Data$x2), to = max(Data$x2), length.out = sqrt(n.pred))

# make a function that accepts n1e2.pred.at.x1, n1e2.pred.at.x2, n.pred as arguments and returns pred.mat
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
         pred.ij = predict(object = m.select, newdata = n1e4.pred.at.df.full[n1e4.pred.at.df.full$x1 == n1e2.pred.at.x1[i] & n1e4.pred.at.df.full$x2 == n1e2.pred.at.x2[j], ], interval = 'prediction', level = 0.8)
        n1e4.lwr.mat[i,j] <- pred.ij[,'lwr']
        n1e4.fit.mat[i,j] <- pred.ij[,'fit']
        n1e4.upr.mat[i,j] <- pred.ij[,'upr']}}


rgl.surface(x = n1e2.pred.at.x1, z = n1e2.pred.at.x2, y = n1e4.pred.mat/max(abs(Data$y)), alpha = 0.25, col = 'green')

##############################
#                            #
#  Surfaces defined by 95%   #
#   Confidence Intervals     #
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

for(i in 1:nrow(X)){
    lines3d(x = c(X[i,'x1'],X[i,'x1']), z = c(X[i,'x2'],X[i,'x2']), y =  c(predict(object = m.select, newdata = X[i, ]), Data[i,'y'])/max(abs(Data$y)), col = 'white')}


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
setwd('~/Animation/MLR_80pc_Pred_Int/Frames/')
open3d(antialias = 4)
rgl.bg()
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

################################################################################
#                                                                              #
#                End of Code File to Accompany Course Module 2                 #
#                                                                              #
#                          Linear Regression in R                              #
#                                                                              #
################################################################################
