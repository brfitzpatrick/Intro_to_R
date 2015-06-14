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

# Here is a quick example to demonstrate the power of the grammar of graphics:

library('ggplot2')

px <- ggplot(aes(x = cut, fill = cut), data = diamonds) + geom_bar(width = 1)

# A bar chart:
px

# A coxcomb plot:
px + coord_polar()

# Something else:
px + coord_polar(theta = 'y')

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
clcw.sp <- clcw.sp + labs(x = 'Carapace Width (mm)', y = 'Carapace Length (mm)')

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
   scale_colour_manual(values = c("cyan3", "orange"), labels = c('Blue', 'Orange'))


head(crabs)
tail(crabs)

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
    facet_grid(sex ~ sp) +
    theme_bw()

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
    facet_grid(Sex ~ Species) + 
    theme_bw()

clcw.sp2 <- clcw.sp2 +
               geom_point(alpha = 0.25, size = 2) +
               facet_grid(Sex ~ Species) +
              theme_bw()

clcw.sp2

setwd('~/Desktop')

ggsave(plot = clcw.sp2, filename = 'clcw.jpg' , width = 10, height = 10,
       units = 'cm', dpi = 600)


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
cw.p + geom_histogram(fill = 'grey', colour = 'black')
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

summary(possum)
# Are longer possums fatter?
p2 <- ggplot(aes(x = totlngth, y = belly), data = possum )
p2 + geom_point(alpha = 0.25, size = 4) + facet_grid(Pop ~ sex) + theme_bw() + geom_smooth() + theme(text = element_text(size = 18)) + labs(x = 'Total Length (cm)', y = 'Belly Circumference (cm)', title = 'Possums')
p2 + geom_jitter(alpha = 0.25, size = 4)
p2

# Are older possums longer? Is gender correlated with the shapes of these length at age curves?
p3 <- ggplot(aes(x = age, y = totlngth), data = possum)
p3 + geom_point(alpha = 0.25, size = 4) + facet_wrap( ~ sex) + theme_bw() + geom_smooth() + theme(text = element_text(size = 18)) + labs(x = 'Age (yrs)', y = 'Total Length (cm)', title = 'Possums')

# What was the gender balance in size classes like at different sites?

p4 <- ggplot(aes(x = totlngth), data = possum)
p4 + geom_histogram()
p4 + geom_histogram(aes(fill = sex), colour = 'black', breaks = with(possum, seq(from = min(totlngth), to = max(totlngth), length.out = 10)))

p4 + geom_histogram(aes(fill = sex), colour = 'black', breaks = with(possum, seq(from = min(totlngth), to = max(totlngth), length.out = 10))) + facet_wrap( ~ Pop)


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

# 
library('ggplot2')
library('raster')
# you may need to install it
# and install the rgdal package
# this bit needs a different IDE
Desert.rst <- raster(x = file.choose())
# and select '~Intro_2_R/Data/landsat_crop.tif' from the dialoguge box
# or just use
setwd('~/Intro_to_R/Data/Graphics_with_ggplot2/')
Desert.rst <- raster(x = 'landsat_crop.tif')

# for the polygon drawing below to work we need to get R to open an external
# graphics device normally we'd use dev.new() for a platform independent way
# to do this however RStudio and dev.new() don't play nicely together so

# on MS Windows systems try:
windows()

# on MacOS systems try:
quartz()

# or
x11()

# on GNU+Linux systems running X11 try
x11()

plot(Desert.rst, col = grey(level = 1:1e4/1e4))

# now lets use the mouse to mark out a polygon on the plot
Polygon <- drawPoly(sp = FALSE)
# once you've marked out the polygon either right click or right click and
# select 'close'

# Next we need take the data from raster object Desert.rst and transcribe it
# into a long data format dataframe

Desert.df <- data.frame(coordinates(Desert.rst),
                        extract(x = Desert.rst, y = coordinates(Desert.rst)))

head(Desert.df)

colnames(Desert.df) <- c('Easting', 'Northing', 'Value')

U.p <- ggplot(aes(x = Easting, y = Northing, fill = Value),
              data = Desert.df) +
                  coord_equal()

U.p + geom_raster() + scale_fill_gradientn(colours = grey(level = 1:1e4/1e4),
                                           guide = FALSE)

Polygon.df <- data.frame(Polygon)

colnames(Polygon.df) <- c('Easting', 'Northing')

Polygon.df$Value = rep(mean(Desert.df$Value),nrow(Polygon.df))

U.p <- U.p + geom_raster() +
             scale_fill_gradientn(colours = grey(level = 1:1e4/1e4),
                                  guide = FALSE)

U.p

U.p + geom_path(data = Polygon.df, col = 'blue')

U.p <- U.p + geom_path(data = Polygon.df, col = 'blue')

U.p + geom_polygon(data = Polygon.df, fill = 'green', alpha = 0.5)

U.p + annotate(geom = 'text', x = min(Polygon.df[,1]),
               y = min(Polygon.df[,2])-250, label = 'An area in the desert...',
               colour = 'blue', hjust = 0)

U.p

# I don't have any coordinates to annotate this aerial image with so I'm going
# to generate some below (don't worry too much about this code unless you want
# to the point of this example is to show how to mark points on a raster). 

PG.x <- seq(from = min(Polygon.df[,1]), to = max(Polygon.df[,1]),
            length.out = 500)

PG.y <- seq(from = min(Polygon.df[,2]), to = max(Polygon.df[,2]),
            length.out = 500)

PG.full <- expand.grid(PG.x, PG.y)

PG <- PG.full[sample(x = 1:nrow(PG.full), size = 50), ]

PG$Value <- rep(mean(Desert.df$Value),nrow(PG))

PG$Member <- point.in.polygon(point.x = PG[,1], point.y = PG[,2],
                              pol.x = Polygon.df[,1], pol.y = Polygon.df[,2])

unique(PG$Member)

PG$Member.Factor <- factor(levels =  c('In','Out'),
                           x = vector(mode = 'character', length = nrow(PG)))

PG[PG$Member == 1, 'Member.Factor'] <- 'In'

PG[PG$Member == 0, 'Member.Factor'] <- 'Out'

summary(PG$Member.Factor)

colnames(PG)

colnames(PG) <- c('Easting', 'Northing', 'Value', 'Member',
                  'Member.Factor')

# Adding the points onto the raster

U.p + geom_point(colour = 'black', size = 2, data = PG) +
      geom_point(aes(colour = Member.Factor), size = 1, data = PG) +
      scale_colour_manual(values = c('green', 'red')) +
      labs(colour = 'In Polygon')

# zooming in to the polygon

Poly.ext <- as.matrix(extent(Polygon))

Poly.ext[1,1]

U.p + geom_point(colour = 'black', size = 2, data = PG) +
      geom_point(aes(colour = Member.Factor), size = 1, data = PG) +
      scale_colour_manual(values = c('green', 'red')) +
      labs(colour = 'In Polygon') +          
      xlim(Poly.ext[1,1] -250, Poly.ext[1,2] + 250) +
      ylim(Poly.ext[2,1] -250, Poly.ext[2,2] + 250)

U.p <- U.p + geom_point(colour = 'black', size = 2, data = PG) +
      geom_point(aes(colour = Member.Factor), size = 1, data = PG) +
      scale_colour_manual(values = c('green', 'red')) +
      labs(colour = 'In Polygon') +
      annotate(geom = 'text', x = min(Polygon.df[,1]),
               y = min(Polygon.df[,2])-250, label = 'Some hills in the desert.',
               colour = 'blue', hjust = 0)

U.p

U.p + geom_path(data = PG[PG$Member.Factor == 'In',], colour = 'green')

# what else might a path geometry be good for (outside a spatial context...) ?

getwd()
setwd('/home/ben/Documents/')
ggsave(plot = U.p, filename = 'desert.pdf' , width = 9,
       height = 9, units = 'in')

################################################################################
#                                                                              #
# We now have an hour for you to experiment with visualising some data of your #
#     own wiht 'ggplot2'. Don't forget <http://docs.ggplot2.org/current/>      #
#            and feel free to raise you hand if you need some help.            #
#                                                                              #
# If you didn't bring any data there's plenty online so why not pick some that #
#                    interests you from the list hosted at                     #
#             <http://mran.revolutionanalytics.com/documents/data/>            #
#                                                                              #
################################################################################




################################################################################
#                                                                              #
#               End of Code File to Accompany Course Module 3                  #
#                                                                              #
#                         Graphics with 'ggplot2'                              #
#                                                                              #
################################################################################
