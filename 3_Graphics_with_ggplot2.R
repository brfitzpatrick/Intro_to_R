###


# Some simple examples:


p <- ggplot(aes(x = carat), data = diamonds)

p + geom_bar(aes(fill = cut), colour = 'black')

p + geom_bar(aes(fill = cut), colour = 'black') + coord_polar(theta = 'y')

p + geom_bar(aes(fill = cut), colour = 'black') + coord_polar(theta = 'x')




attach(diamonds)
summary(diamonds)


library(MASS)
attach(crabs)
ls()
summary(crabs)


# sp =  species - "B" or "O" for blue or orange.
# sex =  as it says.
# index =  index 1:50 within each of the four groups.
# FL =  frontal lobe size (mm).
# RW =  rear width (mm).
# CL =  carapace length (mm).
# CW =  carapace width (mm).
# BD =  body depth (mm).



library(ggplot2)
p <- ggplot(aes(x = CL, y = CW), data = crabs) + coord_equal() +
    labs(x = 'Carapace Width (mm)', y = 'Carapace Length (mm)')
p + geom_point(aes(colour = sp, shape = sex), alpha = 0.5) + facet_wrap( ~ sp)

p + geom_point(aes(colour = sp, shape = sex), alpha = 0.5) + facet_grid(sex ~ sp)

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
