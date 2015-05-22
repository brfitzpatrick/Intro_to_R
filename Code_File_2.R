# Code File for Quneensland University of Technology: Bayesian Research and Applications Group's Introduction to R Course
# Course prepared and presented by Ben R. Fitzpatrick (ben.r.fitzpatrick@gmail.com) 7th of March 2012

# R is all about the free sharing of code with the proper acknowledgement of authorship so feel free share this file (just don't delete my name from it ;) and only add yours if you've improved it)

#############################
#                           #
# This is code file 3       #
# 2d plotting with ggplot2  #
#                           #
#############################

# plotting in the base R environment is cumbersome

# ggplot2 streamlines plotting code and has a universal syntax across all it's functions (the arguments that work for one function from ggplot2 work for all of it's functions - not the case for base graphics plotting functions)

# ggplot2 can generate it's own legends - (legends can be time consuming and fiddly in base graphics, especially if you want them outside the plotting box)

# ggplot2 can plot with transparency - not something base graphics can not do

# writing extensively about 'ggplot2' is largely redundant given the excellent book by the package author:
#   H. Wickham. ggplot2: elegant graphics for data analysis. Springer New   York, 2009.
# and his similarly excellent website:  http://had.co.nz/ggplot2/

# so I'll confine myself here to whetting your appetite for this package with a few examples of it's many features and capabilities

# (in case you are wondering what the 'gg' stands for: ggplot2 is based on 'The Grammar of Graphics' (Wilkinson 2005))

library(ggplot2) #load package 'ggplot2'

 # the dataframe diamonds comes with 'ggplot2'

dim(diamonds) # and it's quite large ( 53940 rows and 10 columns i.e.  53940 observations of ten variables)

head(diamonds)

summary(diamonds) # summary statistics for numeric variables in 'diamonds', numbers of observations at each factor levels for variables that are factors

qplot(carat,price,data = diamonds) # qplot is the high level, easier to use function, it's name stands for 'quick plot'

# there is also the lower level ggplot() function which qplot() calls

dsmall <-  diamonds[sample(nrow(diamonds),100),] # taking a random sample of 100 of these rows to make a smaller dataframe 

#qplot can map a factor to point colour, shape or size

qplot(carat,price,data = dsmall, colour = color) # note the automatic legend reflects the name of the variable mapped to colour and it's actual factor levels

levels(dsmall$color)

qplot(carat,price,data = dsmall, shape = cut)

levels(dsmall$cut)

# plot point colour, shape, size are all aesthetic attributes which can have a variable mapped to them with a scale function

#ggplot2 will create a scale function by itself

#scale functions may also be overwritten e.g. here to plot all points as 'red'

qplot(carat,price,data = dsmall, shape = cut,colour = I('red')) # the I() protects the value 'red' forcing ggplot2 to use it for each point

# we could achieve the same effect by binding an additional column consisting purely of the word 'red' to the data and mapping this column to point colour like so

dsmall <- data.frame(dsmall,new.col=rep('red',nrow(dsmall)))

qplot(carat,price,data = dsmall, shape = cut,colour = new.col)

# we can also set a single transparency value for all data to get a feeling for just how much overlap there is in the data
qplot(carat,price,data = diamonds, colour = I('red'),alpha=I(1/100)) # alpha=I(1/100) means that if 100 points overlap the plot there will be opaque black 
# above is the plotting with the 'points' geometry (qplot guesses which geom to use based on the data) these may also be set and there are lots of possibles see:  http://had.co.nz/ggplot2/

#for instance:

#interesting alternative to boxplots
qplot(cut,price/carat,data = diamonds, geom = 'jitter', alpha = I(1/40))

# quick p.d.f. estimates
qplot(carat,data=diamonds,geom = 'density') # overall fit for probability density function for all the carat values of all data

#mapping categorical variables to an aesthetic will automatically divide and plot the data accordingly e.g.

qplot(carat,data=diamonds,geom = 'density' ,colour = cut) # gives one density for carat values for each grouping of the data by 'cut' quality
#or 
qplot(carat,data=diamonds,geom = 'density' ,linetype = cut) # gives one density for carat values for each grouping of the data by 'cut' quality

# faceting making panels of figures
#just map the variable you want to facet by to facets e.g.
qplot(price/carat, data=diamonds,facets= color ~ ., geom= 'histogram') # creating panels of plots is called faceting and is very efficient to code compared to doing this in base graphics

qplot(price/carat, data=diamonds,facets= color ~ cut, geom= 'density') # map interactions of variables to facets with '~' to produce two way panels displaying plots for all groups of the data formed by combinations of the two factors

###


# Plotting confidence intervals around model predictions as semi-transparent bands

# (would work equally well with any other estimate of variability around some trend line)

# Here we apply this method to plotting the confidence intervals around the different model predictions the model selection exercise from Code File 1 (BRAG_Intro_to_R.R)

# first we generate the data

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

Data <- data.frame(x=x.l,y=y.l,f=factor(f.l))

# then we fit the models and make the predictions

m1 <- with(Data,lm(y~x)) 

x.2 <- Data$x^2 # so we have to define a new variable containing the x^2 values

Data <- data.frame(Data,x.2) # then to keep things tidy let's bind it to the end of the dataframe

head(Data) # have look at the top few rows to check out attachment of the 'x.2' column to the dataframe 'Data' worked

m2 <- with(Data,lm(y~x+x.2))

# abline() only works for linear fits so

pred.at <- seq(from=0,to=61,by=0.5) # x values to predict at

m2.pred <- predict(object=m2,newdata=data.frame(x=pred.at,x.2=pred.at^2), interval = c("confidence"),level=0.95)  # predicting at these x values from the model 'm2' (predicting the response and recording the bound of the 95% confidence interval of this predicition...no reason you couldn't supply the code below this credible interval bounds though or indeed bootstrap confidence interval bounds)

class(m2.pred)

m2.pred <- data.frame(m2.pred) # coercing to a data frame so we can extract variables by name

head(m2.pred)

m6 <- with(Data,lm(y~f*+1+x+f:x+x.2+f:x.2)) #model where intercept,'x' and the x^2 terms have interactions with 'f'; fred:x (read ad  (f level red) interaction with x) is the modification of the slope for f level red

m6.pred.red <- data.frame(predict(object=m6,newdata=data.frame(x=pred.at,x.2=pred.at^2,f=rep('red',length(pred.at))), interval = c("confidence"),level=0.95)) # predicting at these x values from the model 'm2'

m6.pred.green <- data.frame(predict(object=m6,newdata=data.frame(x=pred.at,x.2=pred.at^2,f=rep('green',length(pred.at))), interval = c("confidence"),level=0.95)) # predicting at these x values from the model 'm2'

y1 <- m2.pred$fit # coercing predictions from matrix to dataframe so we can extract columns by name
y2 <- m6.pred.red$fit
y3 <- m6.pred.green$fit

y1min <- m2.pred$lwr

y1max <- m2.pred$upr

y2min <- m6.pred.red$lwr

y2max <- m6.pred.red$upr

y3min <- m6.pred.green$lwr

y3max <- m6.pred.green$upr

EvPts <- pred.at

# Now the plotting of these data with ggplot2 

# this is somewhat inelegant - ggplot2 is difficult to use when the data you want plot are spread across multiple dataframes as they are here

# however it is possible - if you have better way to achieve the same plot please get in touch :)

# the variables mapped to the aesthetics must be present in both dataframe with the same range of values (this is why I bind a column of 0s to the dataframe 'Data' and call it 'Fit', it already shares columns 'x','y' and 'f' with the dataframe 'Preds.m2.m6')

# ggplot2 expects groups to be defined by rows

# so rather than having separate columns for y1, y2 and y3 we need them all in the one column (one after the other) differentiated by the values in another index column (my 'Fit' column below), with the data in this format aesthetics (line type, colour etc.) may be set by mapping particular variables (columns) to that aesthetic 

Preds.m2.m6 <- data.frame(EvPts,y=c(y1,y2,y3),y.min=c(y1min,y2min,y3min),y.max=c(y1max,y2max,y3max),Fit=c(rep(1,length(y1)),rep(2,length(y2)),rep(3,length(y2))),f=c(rep('blue',length(y1)),rep('red',length(y2)),rep('green',length(y3))))  # creating such a dataframe

head(Preds.m2.m6)

Data <- data.frame(Data,Fit=rep(0,nrow(Data)))

qplot(x=EvPts,y=y,data=Preds.m2.m6) # plotting EvPts against y values  (qplot chooses a scatter plot for use seeing as it seems to suit the data)

#but we want lines...(i.e. these points joined by line segments to approximate the smooth curves of the predictions)

qplot(x=EvPts,y=y,data=Preds.m2.m6,geom='line')

# however we need to specify a 'group' in order to produce 3 distinct lines

qplot(x=EvPts,y=y,data=Preds.m2.m6,geom='line',group=Fit)

# in ggplot2 we can name plots assigning them to a objects e.g. my.plot where they are stored and may be 'added to' see below

my.plot <-qplot(x=EvPts,y=y,group=Fit,geom='line',xlab='x label',ylab='y label',data=Preds.m2.m6)

# the name of the plot may then be entered at the console to produce it

my.plot

# then successive elements may be added to the plot like so:

my.plot <- my.plot + aes(linetype=Fit) # mapping the variable 'Fit' to the plotting aesthetic 'linetype'

my.plot

my.plot <- my.plot + scale_linetype_identity(breaks=c(0,1,2,3),labels=c('','Quadratic Model (No Interactions)','Quadratic Model Full Interactions (f=Red)','Quadratic Model Full Interactions (f=Green)')) # adding a legend by defining a character vector of labels to associate with variable ('Fit') which as been mapped to line type (the reason for a break at 0 with no label I'll return to below)

my.plot

# or we can do this all with one assignment

h <-qplot(x=EvPts,y=y,group=as.character(Fit),geom='line',xlab='x label',ylab='y label',data=Preds.m2.m6) +  aes(linetype=Fit)  + scale_linetype_identity(breaks=c(0,1,2,3),labels=c('','Quadratic Model (No Interactions)','Quadratic Model Full Interactions (f=Red)','Quadratic Model Full Interactions (f=Green)'))

h

h <- h + geom_ribbon(aes(x=EvPts,ymin=y.min,ymax=y.max,fill=f),alpha=0.1,data=Preds.m2.m6)  # adding confidence intervals to the fit with opacity = 0.1 (10 things on top of each other would be opaque)

h

# we get separate filled ribbons for each level of 'f' but qplot() is just taking the colours in 'f' as different factor levels not actual colours
# to get our 'red' band assigned to confidence interval for the predictions where f=red and green band assigned to the confidence interval for predictions when f=green we need to manually specify the scale (which also allows us to control directly what is written in the legend): 

h <-  h + scale_fill_identity(name=c('95% C.I.'),labels=c('Full Interaction Quadratic Model (f=Green)','Full Interaction Quadratic Model (f=Red)','Quadratic Model (No Interactions)'),breaks=c('green','red','blue')) #seeing as 'f' already contains colour names we just need an identity mapping from 'f' to the ribbon fill colours

h

# now to add the raw data to the plot
# this where complications arise as they don't fit into Preds.m2.m6 so I'm drawing them from the dataframe 'Data' and plotting with ggplot2 and data spread across 2 or more dataframes gets somewhat messy (see above comments about adding a vector of zeros called 'Fit' to the dataframe 'Data')

h <-h + geom_point(aes(x,y=y,col=f),size=I(4),data=Data) + scale_colour_identity(name=c('Observed Data'),labels=c('f=Green','f=Red'),breaks=c('green','red')) # note we could map a variable to 'size' and have point size controlled by some variable in the dataframe but as we want all the points to be the same size we encapsulate this numeric size in an I() to preserve it's value and have it applied to all points

h # re-size the window a bit with the mouse to get the plot square (or to a shape you like)

# now to write the plot to an external image file

#saving graphics for use with other programs
# vector (.ps, .eps) c.f. raster graphics (.jpg, .png, ...)
# vector graphics are procedural and therefore 'infinitely' magnifiable 
# raster graphics are stored as arrays of pixels and thus will get 'blocky' as you zoom in

ggsave(filename='Plot.ps',plot=h) # your plot 'h' is written to your current working directory as a .ps file

getwd() # find your current working directory

setwd('/home/ben/Plots') # set your working directory (on Windows your file path will start like this 'C://')

ggsave(filename='/home/ben/PhD/My_R_Course/QI.jpg',plot=h)

# images are saved the with aspect ratio taken from the current plot device i.e. what you see in the window is what is written to the the file

?ggsave

# ' ‘ggsave’ currently recognises the extensions eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).'
# just write the file extension you want onto the end of you filename in ggsave()


#############################
#                           #
# End of code file 3        #
# 2d plotting with ggplot2  #
#                           #
#############################
