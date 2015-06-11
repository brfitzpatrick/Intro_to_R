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
#                           Graphics with 'ggplot2'                            #
#                                                                              #
################################################################################

# Hello and welcome to the code file to accompany Module III of this course.
# You will notice that this code file is a mess (sorry about that) I'll talk
# you all through it and will upload a neater, more clearly annotated  version
# to the repository with edits inspired by how the module goes when I
# demonstrate it live.  
# Thanks for you patience and thank you to my faithful testers!

# To begin at the beginning:

## ggplot2 is exceptionally well documented online

# here is an introductory tutorial by it's author Hadely Wickham:
# <http://rpubs.com/hadley/ggplot-intro>

# comprensive manual page style help files for each element of ggplot2 may be
# found here:
# <http://docs.ggplot2.org/current/>

# so honestly you don't really need me to learn this stuff but seeing as we're
# all here in this nice place...

# Here is a quick introductory example to demonstrate the power of the grammar
# of graphics on the 'diamonds' data which are included in the 'ggplot2' package

# It all begins with loading the 'ggplot2' package (this loads all the functions
# and data from the package into memory making them available for use in R)

library('ggplot2')

# first I maps a variable, 'carat', to an aethetic, here the horizontal axis:

p <- ggplot(aes(x = carat), data = diamonds)

# next I add on a geometry and map a second variable, 'cut', to a second
# aethetic, the colour filling the bars, setting all outlines to be black
# (rather than coloured by 'cut' or another variable)

p <- p + geom_bar(aes(fill = cut), colour = 'black')

p # an we have histogram

# now by swithing the coordinate system

p + coord_polar(theta = 'y')

p + coord_polar(theta = 'x')

# we can make two very different graphs depicting the frequency with which
# different carat dimonds were of the different cut classes

# if you like coordinate systems check out the 2D projections of the spherical
# earth in this example:
## <http://docs.ggplot2.org/current/coord_map.html>

# note how little additional code was required for these transformations, such
# is the power of the grammar of graphics

# Now for some details
# let's load the 'crabs' dataframe that comes with the MASS package:

library(MASS)
attach(crabs)
summary(crabs)
?crabs

################################################################################
################################################################################
###                                                                          ###
###                                                                          ###
###                    Introducing the Grammar of Graphics                   ###
###                             with Scatter Plots                           ###
###                                                                          ###
###                                                                          ###
################################################################################
################################################################################

# Let's start with a scatter plot of carapace width against carapace length

# First we define a plotting object, let's call it clcw.sp
# We maps two variables to two different aesthetics of the plot nameley

clcw.sp <- ggplot(aes(x = CL, y = CW), data = crabs)

# so far we have only specified which variables to maps the spatial coordinates
# in the plot, CL on the horizontal and CW on the vertical axis
# until we specify a geometry nothing will be drawn

clcw.sp

# Let's request the points geometry be drawn

clcw.sp + geom_point()

# this produces a scatter plot

# now as the vertical and horizontal axes have the same units (mm) we may want
# both axes to have the same scale

clcw.sp + geom_point() + coord_equal()

# I have a few plans for additional arguments we'll pass to the points geometry
# so we wont store that in clcw.sp just yet but we'll store the specification
# that the horiztonal and vertical axes should have the same scale

clcw.sp <- clcw.sp + coord_equal()

clcw.sp # still no geometry stored yet as we are still experimenting with it

clcw.sp + geom_point()
# but we have stored the specification that the coordinates shouhld be on the
# same scale

# next up we can make the axis labels more informative:
clcw.sp <- clcw.sp +  labs(x = 'Carapace Width (mm)', y = 'Carapace Length (mm)')

clcw.sp + geom_point()

# ok now let's add some more information to plot using colour to represent the
# species of the crab being measured 

clcw.sp + geom_point(aes(colour = sp))

# now we will update the labels to include a more informative colour label

clcw.sp + geom_point(aes(colour = sp)) + labs(colour = 'Species')

# if we check 

?crabs

# we see the sp = B represents the blue species and sp = O represents the
# organge species so we may wish to colour the points accordingly
# to this we need manually specify the scale by which the categorical variable
# sp is mapped to the colour aesthetic

clcw.sp +
    geom_point(aes(colour = sp)) +
    labs(colour = 'Species') +
    scale_colour_manual(values = c("cyan3", "orange"))

# for complete control you can use rgb hex values to specify colour:
clcw.sp +
    geom_point(aes(colour = sp)) +
    labs(colour = 'Species') +
     scale_colour_manual(values = c("#008B8B", "#ffa500"))

clcw.sp +
    geom_point(aes(colour = sp)) +
    labs(colour = 'Species') +
    scale_colour_manual(values = c("cyan3", "orange"))

clcw.sp +
    geom_point(aes(colour = sp)) +
    labs(colour = 'Species') +
+ scale_colour_manual(values = c("cyan3", "orange"), labels = c('Blue', 'Orange'))


clcw.sp <- clcw.sp +
              labs(colour = 'Species') +
              scale_colour_manual(values = c("cyan3", "orange"), labels = c('Blue', 'Orange'))


clcw.sp + geom_point(aes(colour = sp))

# Now let's map the crab sex to the aesthetic controlling point shape

clcw.sp + geom_point(aes(colour = sp, shape = sex))

# There's quite a bit of overlap among the data so we could se the  points to be
# semitransparent

clcw.sp + geom_point(aes(colour = sp, shape = sex), alpha = 0.5)

# with semitransparent points we can have them a bit bigger

clcw.sp + geom_point(aes(colour = sp, shape = sex), alpha = 0.25, size = 3)

# In this case transparency doesn help a great deal

# Instead for such situtations I recommend using faceting to produce
# Edward Tufte style small multiples
# the best way to understand what this means is to do it

clcw.sp +
    geom_point(aes(colour = sp, shape = sex), alpha = 0.5, size = 3) +
    facet_wrap( ~ sp)

clcw.sp +
    geom_point(aes(colour = sp, shape = sex), alpha = 0.5, size = 3) +
    facet_grid(sex ~ sp)

# the mapping of species to colour and sex to shape is now redundant

clcw.sp +
    geom_point(alpha = 0.5, size = 3) +
    facet_grid(sex ~ sp)

# It's acutally easier to create new factors than to change facet labels

Crabs.2 <- crabs

Crabs.2$Sex <- as.character(crabs$sex)
Crabs.2[Crabs.2$Sex == 'M', 'Sex'] <- 'Male'
Crabs.2[Crabs.2$Sex == 'F', 'Sex'] <- 'Female'
Crabs.2$Sex <- factor(Crabs.2$Sex)

Crabs.2$Species <- as.character(crabs$sp)
Crabs.2[Crabs.2$Species == 'O', 'Species'] <- 'Orange'
Crabs.2[Crabs.2$Species == 'B', 'Species'] <- 'Blue'
Crabs.2$Species <- factor(Crabs.2$Species)

summary(Crabs.2)

class(Crabs.2$sex)

clcw.sp2 <- ggplot(aes(x = CL, y = CW), data = Crabs.2)
clcw.sp2 <- clcw.sp2 + coord_equal()
clcw.sp2 <- clcw.sp2 +
    labs(x = 'Carapace Width (mm)', y = 'Carapace Length (mm)')

clcw.sp2 + 
    geom_point(alpha = 0.5, size = 2) +
    facet_grid(Sex ~ Species)

clcw.sp2 <- clcw.sp2 +
               geom_point(alpha = 0.25, size = 2) +
               facet_grid(Sex ~ Species)

clcw.sp2

setwd('~/Desktop')

ggsave(plot = clcw.sp2, filename = 'clcw.jpg' , width = 10, height = 10,
       units = 'cm', dpi = 600)

clcw.sp2 <- clcw.sp2 + theme_bw()
clcw.sp2

ggsave(plot = clcw.sp2, filename = 'clcw.png' , width = 10, height = 10,
       units = 'cm', dpi = 600)

ggsave(plot = clcw.sp2, filename = 'clcw.pdf' , width = 10, height = 10,
       units = 'cm')

################################################################################
################################################################################
###                                                                          ###
###                                                                          ###
###                     Distributions of Single Variable                     ###
###                     Histograms and Denisty Estimates                     ###
###                                                                          ###
################################################################################
################################################################################

cw.p <- ggplot(aes(x = CW), data = crabs)
cw.p + geom_histogram()
cw.p + geom_density()
cw.p + geom_density(aes(colour = sp))
cw.p + geom_density(aes(colour = sp, linetype = sex))

cw.p +
    geom_histogram(aes( y = ..density..), fill = 'grey', colour = 'black') +
    geom_density(aes(colour = sp, linetype = sex), size = 1.5)

cw.p +
    geom_histogram(aes( y = ..density..), fill = 'grey', colour = 'black') +
    geom_density(aes(colour = sp, linetype = sex), size = 1.5) +
    facet_grid(sp ~ sex)

################################################################################
################################################################################
###                                                                          ###
###                                                                          ###
###               Exercise Exploring Possumn Skull Morphology                ###
###                            with Scatter Plots                            ###
###                                                                          ###
###                                                                          ###
################################################################################
################################################################################

# the DAAG package includes a some data on possum skull morphology
library(DAAG)
# you may need to install the DAAG package
install.packages('DAAG')
library('DAAG')
attach(possum)
?possum

# Please Make some scatter plots to explore the Possum skull data:
#   i) Try using point shape and colour to convey additional information of your
#      scatter plot
#  ii) Try faceting to create small multiples of subplots whereby subsets of the 
#      data are displayed in different pannels of the plot
# iii) Practise saving a copy of your finalised plot.

































# Don't look at these until you have had a go at the exercise yourself!

possum.p <- ggplot(aes(x = belly, y = chest, shape = sex), data = possum)
possum.p + geom_point(alpha = 0.25)
possum.p + geom_jitter() + facet_wrap(~ sex)

possum.p <- ggplot(aes(x = hdlngth, y = skullw, shape = sex), data = possum)
possum.p + geom_point(alpha = 0.25)
possum.p + geom_jitter() + facet_wrap(~ sex)

possum.p <- ggplot(aes(x = sex, y = skullw, shape = sex), data = possum)
possum.p +
    geom_boxplot(outlier.size = 0) +
    geom_point(position = position_jitter(width = 0.3, height = 0)) +
    coord_flip()

################################################################################
################################################################################
###                                                                          ###
###                                                                          ###
###                                                                          ###
###                      Visualising Satellite Imagery                       ### 
###                                                                          ###
###                                                                          ###
###                                                                          ###
################################################################################
################################################################################

# for some geospatial visualisations I'm going to use another package 'raster'
# to do the majority of the work but illustrate that 'ggplot2' is capable of
# visualising raster imagery once you convert the imagery into 'long' format
# if you're going to be doing lots of geospatial modelling and visualisation
# it may well be easier to work with the base graphics functions provided by
# with the 'raster' package which will avoid the necessity to transform all the
# rasters to long format which as rasters get bigger gets increasingly
# computationall expensive and annoying
# However for small to medium sized rasters (relative to your computers' RAM)
# 'ggplot2' can produce nice geostatial visualisations in a timely fashion

library('raster')

Desert.rst <- raster(x = file.choose())
# and select '~Intro_2_R/Data/landsat_image.png' from the dialoguge box
# or just use
setwd('~/Intro_to_R/Data/Graphics_with_ggplot2/')
Desert.rst <- raster(x = 'landsat_image.png')

plot(Desert.rst, col = grey(level = 1:1e4/1e4))
Polygon <- drawPoly(sp = FALSE)
par(new = TRUE)
plot(Polygon)

class(Polygon)

head(Polygon)

summary(Polygon)

summary(Desert.rst)

class(Desert.rst)
coordinates(Desert.rst)

Desert.df <- data.frame(coordinates(Desert.rst),
                        extract(x = Desert.rst, y = coordinates(Desert.rst)))

head(Desert.df)

colnames(Desert.df) <- c('Pixels_East', 'Pixels_North', 'Value')

U.p <- ggplot(aes(x = Pixels_East, y = Pixels_North, fill = Value),
              data = Desert.df) +
                  coord_equal()

U.p + geom_raster() + scale_fill_gradientn(colours = grey(level = 1:1e4/1e4))

Polygon.df <- data.frame(Polygon)

colnames(Polygon.df) <- c('Pixels_East', 'Pixels_North')

Polygon.df$Value = rep(1,nrow(Polygon.df))

U.p <- U.p + geom_raster() +
             scale_fill_gradientn(colours = grey(level = 1:1e4/1e4))

U.p <- U.p + geom_path(data = Polygon.df, col = 'green')

U.p + geom_polygon(data = Polygon.df, fill = 'green', alpha = 0.5)

U.p + annotate(geom = 'text', x = min(Polygon.df[,1]),
               y = min(Polygon.df[,2])-25, label = 'An area in the desert...',
               colour = 'green', hjust = 0)

U.p <- U.p + annotate(geom = 'text', x = max(Polygon.df[,1])+25,
                      y = median(Polygon.df[,2]),
                      label = 'An area in the desert...',
                      colour = 'green', hjust = 0, size = 8)

PG.x <- seq(from = min(Polygon.df[,1]), to = max(Polygon.df[,1]),
            length.out = 500)

PG.y <- seq(from = min(Polygon.df[,2]), to = max(Polygon.df[,2]),
            length.out = 500)

PG.full <- expand.grid(PG.x, PG.y)

PG <- PG.full[sample(x = 1:nrow(PG.full), size = 25), ]

PG$Value <- rep(1,nrow(PG))

PG$Member <- point.in.polygon(point.x = PG[,1], point.y = PG[,2],
                              pol.x = Polygon.df[,1], pol.y = Polygon.df[,2])

unique(PG$Member)

PG$Member.Factor <- factor(levels =  c('In','Out'),
                           x = vector(mode = 'character', length = nrow(PG)))

PG[PG$Member == 1, 'Member.Factor'] <- 'In'

PG[PG$Member == 0, 'Member.Factor'] <- 'Out'

summary(PG$Member.Factor)

colnames(PG)

colnames(PG) <- c('Pixels_East', 'Pixels_North', 'Value', 'Member',
                  'Member.Factor')

U.p + geom_point(colour = 'black', size = 2, data = PG) +
      geom_point(aes(colour = Member.Factor), size = 1, data = PG) +
      scale_colour_manual(values = c('green', 'red'))

U.p + geom_point(colour = 'black', size = 2, data = PG) +
      geom_point(aes(colour = Member.Factor), size = 1, data = PG) +
      scale_colour_manual(values = c('green', 'red')) +
      xlim(0, 1600)


U.p + geom_point(colour = 'black', size = 2, data = PG) +
      geom_point(aes(colour = Member.Factor), size = 1, data = PG) +
      scale_colour_manual(values = c('green', 'red')) +
      xlim(0, 750) +
      ylim(450, 1050) +
      annotate(geom = 'text', x = min(Polygon.df[,1]),
               y = min(Polygon.df[,2])-25, label = 'An area in the desert...',
               colour = 'black', hjust = 0)

U.p <- U.p + geom_point(colour = 'black', size = 2, data = PG) +
      geom_point(aes(colour = Member.Factor), size = 1, data = PG) +
      scale_colour_manual(values = c('green', 'red')) +
      xlim(0, 750) +
      ylim(450, 1050) +
      annotate(geom = 'text', x = min(Polygon.df[,1]),
               y = min(Polygon.df[,2])-25, label = 'An area in the desert...',
               colour = 'black', hjust = 0)

U.p

U.p + geom_path(data = PG[PG$Member.Factor == 'In',], colour = 'green')

# what else might a path geometry be good for (outside a spatial context...) ?

getwd()
setwd('/home/ben/Documents/')
ggsave(plot = U.p, filename = 'desert.pdf' , width = 16,
       height = 9, units = 'in')

################################################################################
#                                                                              #
#               End of Code File to Accompany Course Module 3                  #
#                                                                              #
#                         Graphics with 'ggplot2'                              #
#                                                                              #
################################################################################
