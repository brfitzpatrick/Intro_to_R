#     This the R Code File Containing the Solutions to the Collaborative
#     Exercise that concludes the Introduction to R Course available at:
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


# This exercise uses some publically available data from the Dryad Repository
# Please download a copy of 'Data appendix.xlsx' from
# <http://dx.doi.org/10.5061/dryad.r36n0>

# When using these data, please cite the original publication:

# Gibb H, Sanders NJ, Dunn RR, Watson S, Photakis M, Abril S, Andersen AN, Angulo E, Armbrecht I, Arnan X, Baccaro FB, Bishop TR, Boulay R, Castracani C, Del Toro I, Delsinne T, Diaz M, Donoso DA, Enríquez ML, Fayle TM, Feener DH, Fitzpatrick MC, Gómez C, Grasso DA, Groc S, Heterick B, Hoffmann BD, Lach L, Lattke J, Leponce M, Lessard J, Longino J, Lucky A, Majer J, Menke SB, Mezger D, Mori A, Munyai TC, Paknia O, Pearce-Duvet J, Pfeiffer M, Philpott SM, de Souza JLP, Tista M, Vasconcelos HL, Vonshak M, Parr CL (2015) Climate mediates the effects of disturbance on ant assemblage structure. Proceedings of the Royal Society B 282(1808): 20150418. http://dx.doi.org/10.1098/rspb.2015.0418

# Additionally, please cite the Dryad data package:

# Gibb H, Sanders NJ, Dunn RR, Watson S, Photakis M, Abril S, Andersen AN, Angulo E, Armbrecht I, Arnan X, Baccaro FB, Bishop TR, Boulay R, Castracani C, Del Toro I, Delsinne T, Diaz M, Donoso DA, Enríquez ML, Fayle TM, Feener DH, Fitzpatrick MC, Gómez C, Grasso DA, Groc S, Heterick B, Hoffmann BD, Lach L, Lattke J, Leponce M, Lessard J, Longino J, Lucky A, Majer J, Menke SB, Mezger D, Mori A, Munyai TC, Paknia O, Pearce-Duvet J, Pfeiffer M, Philpott SM, de Souza JLP, Tista M, Vasconcelos HL, Vonshak M, Parr CL (2015) Data from: Climate mediates the effects of disturbance on ant assemblage structure. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.r36n0

# I have removed the second column ('Source') from the original file `Data.xlsx' before converting it to the file `Ants.csv' as we don't need the full references for each data source in the data we read into R.

# Read the Data into R:

Data <- read.csv(file = '/home/ben/Intro_to_R/Capstone_Collaborative_Exercise/Data/Ants.csv')

# Examine the top six rows of all the columns:

head(Data)

# View the Column Names:

colnames(Data)

# they're all pretty self explanatory except for PIE
# PIE = Probability of Interspecific Encounter = Probability of Two randomly selected ants being of different species
# PIE is another response variable we could attempt to model with environmental, spatial and survey characteristics
# In this example I'm going to examine the correlations between the Ant Species Richness and the covariates:
  # "Mean.annual.temperature"
  # "Total.annual.precipitation"
  # "Temperature.range"
  # "Disturbance"               
  # "Hemisphere"
  # "Continent"                 
  # "Pitfall.days"
  # "Transect.length"           

# load the 'ggplot2' library

library('ggplot2')

install.packages('maps')
install.packages('mapproj')

# maps example from http://docs.ggplot2.org/current/coord_map.html

library('maps')
library('mapproj')

world <- map_data("world")

worldmap <- ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = 'grey') +
  scale_y_continuous(breaks=(-2:2) * 30) +
  scale_x_continuous(breaks=(-4:4) * 45)

# adding the sampled locations from the Ants data to this plot via an annotation layer

worldmap +
    theme(panel.background = element_rect(fill = 'darkblue')) +
    annotate(geom = 'point', x = Data$Longitude, y = Data$Latitude, size =  2, colour = 'red')

worldmap +
    theme(panel.background = element_rect(fill = 'black'), plot.background = element_rect(fill = 'black')) +
    annotate(geom = 'point', x = Data$Longitude, y = Data$Latitude, size =  3, colour = 'red') +
    coord_map("ortho", orientation=c(0, -74, 0))

worldmap.ants <- worldmap +
    theme(panel.background = element_rect(fill = 'black'), plot.background = element_rect(fill = 'black')) +
    annotate(geom = 'point', x = Data$Longitude, y = Data$Latitude, size =  3, colour = 'red')

worldmap.ants + coord_map("ortho", orientation=c(0, 135, 0))

worldmap.ants + coord_map("ortho", orientation=c(0, -70, 0))

worldmap.ants + coord_map("ortho", orientation=c(0, -35, 0))

worldmap.ants + coord_map("ortho", orientation=c(0, 0, 0))

worldmap.ants + coord_map("ortho", orientation=c(0, 35, 0))

# This takes a few minutes to run
for(i in 1:361){
    ggsave(filename = paste(i,'.png',sep =''),
           plot = worldmap.ants + coord_map("ortho", orientation=c(0, 0, 0)) + coord_map("ortho", orientation=c(0, i - 180, 0)))}

# Plot Species Richness against each of the potential covariates

SR.MT <- ggplot(aes(x = Mean.annual.temperature, y = Species.richness, colour = Continent), data = Data)

SR.MT + geom_point(alpha = 0.5)

SR.TP <- ggplot(aes(x = Total.annual.precipitation, y = Species.richness, colour = Continent), data = Data)

SR.MP + geom_point(alpha = 0.5)

SR.TP <- ggplot(aes(x = Total.annual.precipitation, y = Species.richness, colour = Continent), data = Data)

SR.MP + geom_point(alpha = 0.5)

SR.MAT <- ggplot(aes(x = Mean.annual.temperature, y = Species.richness), data = Data)

SR.MAT + geom_point(alpha = 0.5)

SR.TR <- ggplot(aes(x = Temperature.range, y = Species.richness), data = Data)

SR.TR + geom_point(alpha = 0.5)
                
SR.D <- ggplot(aes(x = Disturbance, y = Species.richness), data = Data)

SR.D + geom_boxplot()
               
SR.TAP <- ggplot(aes(x = Total.annual.precipitation, y = Species.richness), data = Data)

SR.TAP + geom_point(alpha = 0.5)
                 
SR.H <- ggplot(aes(x = Hemisphere, y = Species.richness), data = Data)

SR.H + geom_boxplot()
               
SR.C <- ggplot(aes(x = Continent, y = Species.richness), data = Data)

SR.C + geom_boxplot()
               
SR.PD <- ggplot(aes(x = Pitfall.days, y = Species.richness), data = Data)

SR.PD + geom_point(alpha = 0.5)
                
SR.TL <- ggplot(aes(x = Transect.length, y = Species.richness), data = Data)

SR.TL + geom_point(alpha = 0.5)

# It is good practise to recenter and rescale all the continuous covariates to the same mean and magnitude
# This is especially important as we are going to be using polynomial and interaction terms

Continuous.Covariates <- Data[ ,  c('Mean.annual.temperature', 'Total.annual.precipitation', 'Temperature.range', 'Pitfall.days', 'Transect.length')]

summary(Continuous.Covariates)

CC.RCRS <- scale(x = Continuous.Covariates, center = TRUE, scale = TRUE)

summary(CC.RCRS)

colMeans(CC.RCRS^2)

summary(C.RCRS)

cor(CC.RCRS)

image(abs(cor(CC.RCRS)))

colnames(CC.RCRS)

# The covariate names are a bit long so to save time let's abreivate them

# 'MAT' = 'Mean.annual.temperature'
# 'TAP' = 'Total.annual.precipitation'
# 'TR' = 'Temperature.range'
# 'PD' = 'Pitfall.days'              
# 'TL' = 'Transect.length'           

data.frame(Full.Name = colnames(CC.RCRS), Abbrv = c('MAT', 'TAP', 'TR', 'PD', 'TL'))


colnames(CC.RCRS) <- c('MAT', 'TAP', 'TR', 'PD', 'TL')

class(CC.RCRS)

# Creating a dataframe with polynomial effects up to order 3 for each continuous covariate term

CC.RCRS <- data.frame(CC.RCRS)

summary(CC.RCRS)

CC.RCRS.PE <- data.frame(CC.RCRS,
                         MAT.2 = CC.RCRS$MAT^2 ,
                         MAT.3 = CC.RCRS$MAT^3 ,
                         TAP.2 = CC.RCRS$TAP^2 ,
                         TAP.3 = CC.RCRS$TAP^3 ,
                          TR.2 = CC.RCRS$TR^2 ,
                          TR.3 = CC.RCRS$TR^3 ,
                          PD.2 = CC.RCRS$PD^2 ,
                          PD.3 = CC.RCRS$PD^3 ,
                          TL.2 = CC.RCRS$TL^2 ,
                          TL.3 = CC.RCRS$TL^3)

# Adding on the categorical covariates

C.RCRS <- data.frame(CC.RCRS.PE, Data[, c('Disturbance', 'Hemi', 'Continent')])

colnames(C.RCRS)

colnames(C.RCRS)[(ncol(C.RCRS) - 2) : ncol(C.RCRS)] 

# Renaming categorical covariates to have more abreviated names

colnames(C.RCRS)[(ncol(C.RCRS) - 2) : ncol(C.RCRS)] <- c('Dist', 'Hemi', 'Cont')

colnames(C.RCRS)

# Examining a summary of our new dataframe

summary(C.RCRS)

summary(Data$Species.richness)

## Next to create some interaction terms
# for simiplicity we'll create the interaction terms for the continuous covariates first:
colnames(C.RCRS)

CC.RCRS.Lin <- C.RCRS[,c('MAT', 'TAP', 'TR', 'PD', 'TL')] 

head(CC.RCRS.Lin)

dim(CC.RCRS.Lin)

choose(n = ncol(CC.RCRS.Lin), k = 2)

Int.Ind <- combn(x = 1:ncol(CC.RCRS.Lin), m = 2)

Int.Ind

Int <- data.frame(matrix(data = NA, nrow = nrow(C.RCRS), ncol = choose(n = ncol(CC.RCRS.Lin), k = 2)))

for(i in 1:ncol(Int.Ind)){    
    Var1 = colnames(CC.RCRS.Lin)[Int.Ind[1,i]]
    Var2 = colnames(CC.RCRS.Lin)[Int.Ind[2,i]]
    Int[,i] <- CC.RCRS.Lin[, Var1] * CC.RCRS.Lin[, Var2]
    colnames(Int)[i] <- paste(Var1, Var2, sep = '.')
}

head(Int)

C.RCRS.Int <- data.frame(C.RCRS, Int)

head(C.RCRS.Int)

# I can't think of how to avoid doing Cont:Discrete Interactions by hand unfortunately...

# Let's set up for a stepwise variable selection
# We need to bound the search of possible model with and 'empty' intercept only model
# and a 'full' model that contains all the covariate terms

nrow(C.RCRS.Int)

# as we have 1128 observations we have sufficient degrees of freedom to fit a full model here
# this may not be the case with other data...

Empty.lm <- lm(Data$Species.richness ~ +1 , data = C.RCRS.Int)

Full.lm <- lm(Data$Species.richness ~ . + Dist*Hemi + Dist*Cont + Hemi*Cont + Dist*MAT + Dist*TAP + Dist*TR + Dist*PD + Dist*TL + Hemi*MAT + Hemi*TAP + Hemi*TR + Hemi*PD + Hemi*TL + Cont*MAT + Cont*TAP + Cont*TR + Cont*PD + Cont*TL, data = C.RCRS.Int)

summary(Full.lm)

# Note we have some NA's in the estimated coefficients:
  # don't worry, not that sort of singularity ;-)

C.RCRS.Int[C.RCRS.Int$Cont == 'Asia', ]

# we just don't have much data from Asia we may need to abandon the idea of having Continent interaction terms with the other covariates:
Empty1.lm <- lm(Data$Species.richness ~ +1, data = C.RCRS.Int)

Full1.lm <- lm(Data$Species.richness ~ . + Dist*Hemi + Dist*MAT + Dist*TAP + Dist*TR + Dist*PD + Dist*TL + Hemi*MAT + Hemi*TAP + Hemi*TR + Hemi*PD + Hemi*TL, data = C.RCRS.Int)

summary(Full1.lm)

S1.lm <- step(object = Empty.lm, scope = list(lower = Empty.lm, upper = Full.lm), direction = 'both')

par(mfcol = c(2,2))
plot(S.lm)

summary(S1.lm)

####

# We could drop points with inordinately high leverage on the fit... this is somewhat controversial though

Empty2.lm <- lm(Data$Species.richness[-c(113,117,189,616,940,989,1075)] ~ +1 , data = C.RCRS.Int[-c(113,117,189,616,940,989,1075),])

Full2.lm <- lm(Data$Species.richness[-c(113,117,189,616,940,989,1075)] ~ . + Dist*MAT + Dist*TAP + Dist*TR + Dist*PD + Dist*TL + Hemi*MAT + Hemi*TAP + Hemi*TR + Hemi*PD + Hemi*TL, data = C.RCRS.Int[-c(113,117,189,616,940,989,1075),])

S2.lm <- step(object = Empty2.lm, scope = list(lower = Empty2.lm, upper = Full2.lm), direction = 'both')

par(mfcol = c(2,2))
plot(S.lm)

# Droping observations with high leverage doesn't alter the number of covariates selected:

length(coef(S1.lm))

length(coef(S2.lm))

# of the selected covariates only three are different:

summary(names(coef(S1.lm)) %in% names(coef(S2.lm)))

##############################
##                          ##
##                          ##
##        Extensions:       ##
##                          ##
##                          ##
##                          ##
##############################

# Generalized Linear Model to use a better choice of error distribution for the species richness

# Generalized Linear Mixed Effects model to incorporate random effects for Cluster

?step # also works for objects of class glm( )
?glm
?family

# how would you go about modelling PIE?
# Recall PIE is a probability and so 0 <= PIE <= 1
# how about a logit transform? this is what Gibb et al did
# you'll need to add something to PIE before logit transforming it??? :-)




#####
#
#
#  Some Visualisation Extension
#
#
#####

world <- map_data("world")

class(world)

head(world)

colnames(world)[1:2] <- c('Longitude', 'Latitude')


worldmap <- ggplot(world, aes(x=Longitude, y=Latitude, group=group)) +
  geom_polygon(fill = 'grey') +
  scale_y_continuous(breaks=(-2:2) * 30, label = NULL) +
  scale_x_continuous(breaks=(-4:4) * 45, label = NULL)

worldmap

Data2 <- Data

Data2$group <- 1 # Dummy variable to satisfy ggplot2 when it looks for 'group' in Data2 we're not actually going to use it in the plot

worldmap +
    theme(panel.background = element_rect(fill = 'white')) +
    geom_point(aes(x = Longitude, y = Latitude, size = Species.richness), colour = 'red', data = Data2) +
    labs(size = 'Ant Species \nRichness')

ant.map <- worldmap +
        theme(panel.background = element_rect(fill = 'white'), text = element_text(size = 16)) +
        geom_point(aes(x = Longitude, y = Latitude, size = Species.richness), colour = 'red', data = Data2) +
        labs(size = 'Ant Species \nRichness')

ant.map + coord_map("ortho", orientation=c(0, -74, 0))

ggsave(filename = '/home/ben/Intro_to_R/Capstone_Collaborative_Exercise/Capstone_Slides_Source/Images/Ant_Sp_Rich_Map.pdf')
           
##
library(grid)

ant.map2 <- worldmap +        
        labs(size = 'Ant Species \nRichness', x = NULL, y = NULL)

ants.map2 <- ant.map2 + geom_point(aes(x = Longitude, y = Latitude, size = Species.richness), colour = 'red', data = Data2) + coord_map("ortho", orientation=c(-30, 135, 0)) + theme(panel.background = element_rect(fill = 'white'), text = element_text(size = 16), axis.ticks.length = unit(0, units = 'cm') )

ggsave(filename = '/home/ben/Intro_to_R/Capstone_Collaborative_Exercise/Capstone_Slides_Source/Images/Ant_Sp_Rich_Globe_Map.pdf', plot = ants.map2)



