###


# Some simple examples:

# Here is a quick introductory example to demonstrate the power of the grammar
# of graphics

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

# note how little additional code was required for these transformations...

# Now for some details
# let's load the 'crabs' dataframe that comes with the MASS package:
library(MASS)
attach(crabs)
summary(crabs)
?crabs

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

###

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
class(Crabs.2$Sex)
######################


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







scale_colour_manual(values = c("cyan3", "orange"), labels = c('Blue', 'Orange'))

###
library(DAAG)
install.packages('DAAG')
library('DAAG')
attach(possum)

# case = observation number
# site = one of seven locations where possums were trapped
# Pop = a factor which classifies the sites as Vic Victoria, other New South Wales or Queensland
# sex = a factor with levels f female, m male 
# age = age
# hdlngth = head length
# skullw = skull width
# totlngth = total length
# taill = tail length
# footlgth = foot length
# earconch = ear conch length
# eye = distance from medial canthus to lateral canthus of right eye
# chest = chest girth (in cm)
# belly = belly girth (in cm) 

possum.p <- ggplot(aes(x = belly, y = chest, shape = sex), data = possum)
possum.p + geom_point(alpha = 0.25)
possum.p + geom_jitter() + facet_wrap(~ sex)


possum.p <- ggplot(aes(x = hdlngth, y = skullw, shape = sex), data = possum)
possum.p + geom_point(alpha = 0.25)
possum.p + geom_jitter() + facet_wrap(~ sex)

possum.p <- ggplot(aes(x = sex, y = skullw, shape = sex), data = possum)
possum.p + geom_boxplot(outlier.size = 0) + geom_point(position = position_jitter(width = 0.3, height = 0)) + coord_flip()
