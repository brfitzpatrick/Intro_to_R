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
#                           Graphics with 'ggplot2'                            #
#                                                                              #
################################################################################

# Hello and welcome to the code file to accompany Module II of this course.
# The grammar of graphics inspired R package 'ggplot2' is exceptionally well
# documented online.

# Here is an introductory tutorial by the author of `ggplot2 Hadely Wickham:
# <http://rpubs.com/hadley/ggplot-intro>

# Comprensive manual page style help files for each component of 'ggplot2' may
# be found here:
# <http://docs.ggplot2.org/current/>

# In this module I will attempt to introduce the key concept necessary to use
# `ggplot2'.

# It all begins with loading the 'ggplot2' package, this loads all the functions
# and data from the package into memory making them available for use in R:

library('ggplot2')

# Before we begin formally here is a quick example to demonstrate the
# flexibility and concieseness of a ploting syntax based on the the grammar of
# graphics.
# (This example uses the 'diamonds' data which are included in the 'ggplot2'
# package:

# Don't worry too much about the syntax just yet, but instead notice how with
# the line of code below a create a plot object and assign it a name:

px <- ggplot(aes(x = cut, fill = cut), data = diamonds) + geom_bar(width = 1)

# Executing the named plot object produces a bar chart:
px

# Changing the coordinate system produces a coxcomb plot:
px + coord_polar()

# then changine which variable is mapped to angle in the polar coordinate system
# produces a different plot again:

px + coord_polar(theta = 'y')

# Note how little additional code was required for these transformations...

# Each of these plots would have had to be produced separately in R base
# graphics i.e. we could not have simply altered the coordinate system of a bar
# plot and produced a coxcomb plot.

# If you like coordinate systems check out the 2D projections of the spherical
# Earth in this example:
## <http://docs.ggplot2.org/current/coord_map.html>

# Now let's begin the formal introduction.
# We'll use the 'crabs' dataframe that comes with the MASS package:

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

# First we define a plotting object, let's call it clcw.sp.
# To do this we first define which columns of the data will be mapped to spatial
# coordinates in the plot.  Spatial coordinates are one of the 'aethetics' of a
# plot produced with ggplot2 - when we map column of a dataframe to particular
# 'aesthetics' of a plot with the ggplot command we need to do so via a comma
# separated list with the aes( ) component of the ggplot( ) command.
# Below we map the column named 'CL' from the dataframe 'crabs' to horizontal
# spatial coordinates in the plot and we map column named 'CW', also from the
# dataframe 'crabs' to vertical coordinates of the plot. 

clcw.sp <- ggplot(aes(x = CL, y = CW), data = crabs)

# So far we have only specified which variables to maps the spatial coordinates
# in the plot.  Until we specify a geometry nothing will be drawn.

clcw.sp

# Let's request the points geometry be drawn (i.e. one point will be a drawn at
# the coordinates represented by each row in the dataframe crabs when we take
# the column 'CL' as the horizontal coordinates of the points and column 'CW' as
# vertical coordinates of the points).

clcw.sp + geom_point()

# this produces a scatter plot

# ggplot2 includes quite a few geometries that can be added to plots.
# each geometry has a help page with examples on the
# <http://docs.ggplot2.org/current/coord_map.html>

# now as the vertical and horizontal axes have the same units (mm) we may want
# both axes to have the same scale (i.e. have one unit of CL map to the same
# spatial displacement (horizontally) as as one unit of CW maps to vertically).

clcw.sp + geom_point() + coord_equal()

# I have a few plans for additional arguments we'll pass to the points geometry
# so we won't store that in clcw.sp just yet but we'll store the specification
# that the horiztonal and vertical axes should have the same scale

clcw.sp <- clcw.sp + coord_equal()

clcw.sp # still no geometry stored yet as we are still experimenting with it

clcw.sp + geom_point()
# but we have stored the specification that the coordinates should be on the
# same scale

# next up we can make the axis labels more informative:
clcw.sp <- clcw.sp + labs(x = 'Carapace Width (mm)', y = 'Carapace Length (mm)')

clcw.sp + geom_point()

# ok now let's add some more information to plot using colour to represent the
# species of the crab being measured.
# point colour is another aesthetic to which we may map a column in the data
# (in this case the column containing the one letter code representing the
# crabs' sex) as such the mapping of 'sp' to 'colour' occurs within the
# aes( ) component of the point geometry as we are only mapping 'sp' values to
# the colour of the points

clcw.sp + geom_point(aes(colour = sp))

# if we had say both point and line geometries in the plot then using
# '+ geom_point(aes(colour = sp))' would map 'sp' to the colour of the points
# alone e.g.

clcw.sp + geom_point(aes(colour = sp)) + geom_line()

# had we wanted to have 'sp' mapped to the colour of every geometry drawn we
# would have need to make this mapping global by including it in the original
# aes( ) component of the ggplot(aes( ) ) command where we specified which
# columns to map to spatial positioning as a global mapping that would be
# applied to all subsequent geometries which we added to the plot 

# now we will update the labels to include a more informative colour label

clcw.sp + geom_point(aes(colour = sp)) + labs(colour = 'Species')

# if we check 

?crabs

# we see that values of 'B' in the column 'sp' represent the blue form of this
# species and values of 'O' in this column represent the orange form of this
# species so we may wish to colour the points accordingly
# to this we need manually specify the scale by which the categorical variable
# sp is mapped to the colour aesthetic
# in ggplot2 'scales' control the mapping of column in the data (variables) to
# 'aesthetics' of the plot

clcw.sp +
    geom_point(aes(colour = sp)) +
    labs(colour = 'Species') +
    scale_colour_manual(values = c("cyan3", "orange"))

# Scrolling down <http://docs.ggplot2.org/current/coord_map.html> will take you
# to the section that lists help pages on 'scales'

# A long list of named colours available for use in R may be found at:
# <http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf>

# If you have three or more levels of a variable you wish to depict on a graph
# via a discrete colour scale you may find <http://colorbrewer2.org/> useful

# you may specify colour by hex values directly:
clcw.sp +
    geom_point(aes(colour = sp)) +
    labs(colour = 'Species') +
    scale_colour_manual(values = c("#008B8B", "#ffa500"))

clcw.sp +
    geom_point(aes(colour = sp)) +
    labs(colour = 'Species') +
    scale_colour_manual(values = c("cyan3", "orange"))

# You may also update the what is printed next to each colour in the legend with
# the 'labels' argument of the scale command:

clcw.sp +
    geom_point(aes(colour = sp)) +
    labs(colour = 'Species') +
    scale_colour_manual(values = c("cyan3", "orange"),
                        labels = c('Blue', 'Orange'))

# this all looks good so lets add the additional lables and the scale
# specification to the features of plot stored in the plot object clcw.sp

clcw.sp <- clcw.sp +
              labs(colour = 'Species') +
              scale_colour_manual(values = c("cyan3", "orange"), labels = c('Blue', 'Orange'))

clcw.sp + geom_point(aes(colour = sp))

# Now let's map the crab sex to the aesthetic of the points whic controls the
# shape of the points

clcw.sp + geom_point(aes(colour = sp, shape = sex))

# There's quite a bit of overlap among the data so we could set the  points to
# be semitransparent

clcw.sp + geom_point(aes(colour = sp, shape = sex), alpha = 0.5)

# note I could have mapped another variable to the transparency by assigning
# some variable to 'alpha' within the aes( ) component but instead I am setting
# all points to have the same alpha of 0.5 (i.e. be 50% transparent) by equating
# alpha with a single number outside the aes( ) component of the command

# similarly I can set all the points to be a particular size by setting 'size'
# equal to a single number outside the aes( ) component of the command
# with semitransparent points we can have them a bit bigger

clcw.sp + geom_point(aes(colour = sp, shape = sex), alpha = 0.25, size = 3)

# In this case transparency doesn help a great deal

# Instead for such situtations I recommend using faceting to produce
# Edward Tufte style small multiples
# the best way to understand what this means is to do it

clcw.sp +
    geom_point(aes(colour = sp, shape = sex), alpha = 0.5, size = 3) +
    facet_wrap( ~ sp)

# what we have done here is create two subsets of the data one for the rows
# where sp = B (i.e. the blue crabs) and another for the rows where sp = O
# i.e. the orange crabs
# two otherwise identical plots have been produced side by side one of which
# plots the subset of the data where sp = B (the blue crabs) and the other where
# sp = O (the orange crabs)

# we can also facet by two categorical variable here 'sp' and 'sex':

clcw.sp +
    geom_point(aes(colour = sp, shape = sex), alpha = 0.5, size = 3) +
    facet_grid(sex ~ sp)

# which results in four plots each containing the data from a different unique
# subset of the full dataframe where these subsets were defined by the 4 unique
# combinations of crab colour form and sex

# the mapping of species to colour and sex to shape is now redundant (i.e. we
# represented this information by both the faceting and by characteristics of
# the points)

# if we were restricted to black and white or grey scale graphics (as is often
# the case with graphics for journal articles) we can still have the values of
# all four of the variables 'CL', 'CW', 'sp' and 'sex' for each crab conveyed by
# a black and white plot via the use of faceting

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

# Once we have plot we are happy with we will often want to save it to an
# external file
# The ggsave( ) command gives us control over the file format, resolution and
# dimensions of the plot produced.

# Setting the working directory with the setwd( ) command allows us to specify
# where this file will be created

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

# Please make some histograms & density plots to explore the Possum skull data:
#   i) Try using the colour filling the bars of the histograms to convey
#      additional information about the proportions of observations in each bin
#      that had particular levels of a categorical variable associated with them
#  ii) Try faceting to create small multiples of subplots whereby subsets of the 
#      data are displayed in different pannels of the plot
# iii) Practise saving a copy of your finalised plot.

# Please make some scatter plots to explore the Possum skull data:
#   i) Try using point shape and colour to convey additional information of your
#      scatter plot
#  ii) Try faceting to create small multiples of subplots whereby subsets of the 
#      data are displayed in different pannels of the plot
# iii) Practise saving a copy of your finalised plot.

















































# Here are some examples but please try to make your own graphs before running these

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

# ggplot2 is capable of geospatial visualisations (e.g. of raster data such as
# georeferenced satellite imagery or elevation data and of spatial polygons
# e.g. statistical local areas)

# For this geospatial visualisation example I'm going to use another package
# 'raster' (which in turn uses functions from the 'rgdal' package) to do the
# majority of the work but I will visualise the results with 'ggplot2'

# 'ggplot2' is capable of visualising raster imagery once you convert the
# imagery into 'long' format which in the case of a raster equates to a
# dataframe with columns for the coordinates and value of each pixel and pixels
# are listed as subsequent rows

# if you're going to be doing lots of geospatial modelling and visualisation
# it may well be easier to work with the base graphics functions provided by
# with the 'raster' package which will avoid the necessity to transform all the
# rasters to long format which as rasters get bigger gets increasingly
# computationall expensive and annoying

# However for small to medium sized rasters (relative to your computers' RAM)
# 'ggplot2' can produce good geostatial visualisations in a timely fashion

# if you are starting to run this code file from here you will need to load the
# ggplot2 package into memory:

library('ggplot2')

# and everyone will need to load the 'raster' package into memory

library('raster')

# (hopefully you were all able to install the R package 'raster'
# you may have needed to install the R package 'rgdal' first

# first we begin by reading the raster (in this case a GeoTIFF) into R 
# to do this run the line of code below and select file 'landsat_crop.tif' from
# the dialoguge box that opens
# this file is located at '~Intro_2_R/Data/landsat_crop.tif' where the ~
# represents the file path to where every you extracted the Intro_to_R directory
# to when you first downloaded it from GitHub

Desert.rst <- raster(x = file.choose())

# alternatively use

setwd('~/Intro_to_R/Data/Graphics_with_ggplot2/')
Desert.rst <- raster(x = 'landsat_crop.tif')

# for the polygon drawing below to work we need to get R to open an external
# graphics device. normally we'd use dev.new() for a platform independent way
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

# then we're ready to plot the data with ggplot2

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
# to, the point of this example is to show how to mark points on a raster for
# most applications I can think of you'll have the coordinates of the points you
# wish to mark out (they might be locations sampled for some variable of
# interest or proposed target locations for some sampling scheme etc. )

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
#               End of Code File to Accompany Course Module 2                  #
#                                                                              #
#                         Graphics with 'ggplot2'                              #
#                                                                              #
################################################################################
