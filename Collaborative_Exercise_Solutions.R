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

Data <- read.csv(file = '/home/ben/Intro_to_R/Capstone_Collaborative_Exercise/Data/Ants.csv')

head(Data)

colnames(Data)

"Species.richness"

#

"Mean.annual.temperature"
"Temperature.range"
"Disturbance"               
"Total.annual.precipitation"
"Hemisphere"
"Continent"                 
"Pitfall.days"
"Transect.length"           


library('ggplot2')

SR.MT <- ggplot(aes(x = Mean.annual.temperature, y = Species.richness, colour = Continent), data = Data)

SR.MT + geom_point(alpha = 0.5)

###

SR.TP <- ggplot(aes(x = Total.annual.precipitation, y = Species.richness, colour = Continent), data = Data)

SR.MP + geom_point(alpha = 0.5)

###

SR.TP <- ggplot(aes(x = Total.annual.precipitation, y = Species.richness, colour = Continent), data = Data)

SR.MP + geom_point(alpha = 0.5)


###

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

Continuous.Covariates <- Data[ ,  c('Mean.annual.temperature', 'Total.annual.precipitation', 'Temperature.range', 'Pitfall.days', 'Transect.length')]

summary(Continuous.Covariates)

CC.RCRS <- scale(x = Continuous.Covariates, center = TRUE, scale = TRUE)

summary(CC.RCRS)

colMeans(CC.RCRS^2)

summary(C.RCRS)

cor(CC.RCRS)

image(abs(cor(CC.RCRS)))

colnames(CC.RCRS)

# 'MAT' = 'Mean.annual.temperature'
# 'TAP' = 'Total.annual.precipitation'
# 'TR' = 'Temperature.range'
# 'PD' = 'Pitfall.days'              
# 'TL' = 'Transect.length'           

data.frame(Full.Name = colnames(CC.RCRS), Abbrv = c('MAT', 'TAP', 'TR', 'PD', 'TL'))


colnames(CC.RCRS) <- c('MAT', 'TAP', 'TR', 'PD', 'TL')

class(CC.RCRS)

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

C.RCRS <- data.frame(CC.RCRS.PE, Data[, c('Disturbance', 'Hemisphere', 'Continent')])

colnames(C.RCRS)

summary(C.RCRS)

summary(Data$Species.richness)

Full.lm <- lm(Data$Species.richness ~ . , data = C.RCRS)

Empty.lm <- lm(Data$Species.richness ~ +1 , data = C.RCRS)

nrow(Data)

S.lm <- step(object = Empty.lm, scope = list(lower = Empty.lm, upper = Full.lm), direction = 'both')

summary(S.lm)

par(mfcol = c(2,2))
plot(S.lm)

# now for a demonstration of what leverage means... let's drop the two observations with the highest leverage on the fit

predict(S.lm)

?step


summary(Full.lm)






###

C.RCRS.OD <- C.RCRS[-c(189,117),]

Full.lm <- lm(Data$Species.richness[-c(189,117)] ~ . , data = C.RCRS.OD)

Empty.lm <- lm(Data$Species.richness[-c(189,117)] ~ +1 , data = C.RCRS.OD)

nrow(Data)

S.lm <- step(object = Empty.lm, scope = list(lower = Empty.lm, upper = Full.lm), direction = 'both')

summary(S.lm)

par(mfcol = c(2,2))
plot(S.lm)

###


## Ok next up some interaction terms

# Let's just do first order interactions for now

Full.lm <- lm(Data$Species.richness[-c(189,117)] ~ . , data = C.RCRS.OD)

Empty.lm <- lm(Data$Species.richness[-c(189,117)] ~ +1 , data = C.RCRS.OD)




# Extensions:
# Generalized Linear Model to use a better choice of error distribution
# Generalized Linear Mixed Effects model to incorporate random effects for Cluster

colnames(C.RCRS)

Empty.lm <- lm(Data$Species.richness ~ +1 , data = C.RCRS)

CC.RCRS.Lin <- C.RCRS[,c('MAT', 'TAP', 'TR', 'PD', 'TL')] #, 'Disturbance', 'Hemisphere', 'Continent')]

dim(CC.RCRS.Lin)

choose(n = ncol(CC.RCRS.Lin), k = 2)

Int.Ind <- combn(x = 1:ncol(CC.RCRS.Lin), m = 2)

Int.Ind

Int <- data.frame(matrix(data = NA, nrow = nrow(C.RCRS), ncol = choose(n = ncol(CC.RCRS.Lin), k = 2)))

for(i in 1:ncol(Int.Ind)){    

    Var1 = colnames(CC.RCRS.Lin)[Int.Ind[1,i]]
    Var2 = colnames(CC.RCRS.Lin)[Int.Ind[2,i]]

    Int[,i] <- CC.RCRS.Lin[, Var1] * CC.RCRS.Lin[, Var2]

    colnames(Int)[i] <- paste(Var1, Var2, sep = ':')

}
    
head(Int)

# Will have to do Cont:Discrete Interaction by hand, damit,


Full.lm <- lm(Data$Species.richness ~ . , data = C.RCRS)


, 'Disturbance', 'Hemisphere', 'Continent')]

Test <- lm(Data$Species.richness ~ Disturbance*MAT + Disturbance*TAP + Disturbance*TR + Disturbance*PD + Disturbance*TL + Hemisphere*MAT + Hemisphere*TAP + Hemisphere*TR + Hemisphere*PD + Hemisphere*TL + Continent*MAT + Continent*TAP + Continent*TR + Continent*PD + Continent*TL, data = C.RCRS)

Test <- lm(Data$Species.richness ~ . + Disturbance*MAT + Disturbance*TAP + Disturbance*TR + Disturbance*PD + Disturbance*TL + Hemisphere*MAT + Hemisphere*TAP + Hemisphere*TR + Hemisphere*PD + Hemisphere*TL + Continent*MAT + Continent*TAP + Continent*TR + Continent*PD + Continent*TL, data = C.RCRS)

summary(Test)


Empty.lm <- lm(Data$Species.richness ~ +1 , data = C.RCRS)

Full.lm <- lm(Data$Species.richness ~ . + Disturbance*MAT + Disturbance*TAP + Disturbance*TR + Disturbance*PD + Disturbance*TL + Hemisphere*MAT + Hemisphere*TAP + Hemisphere*TR + Hemisphere*PD + Hemisphere*TL + Continent*MAT + Continent*TAP + Continent*TR + Continent*PD + Continent*TL, data = C.RCRS)

S.lm <- step(object = Empty.lm, scope = list(lower = Empty.lm, upper = Full.lm), direction = 'both')

par(mfcol = c(2,2))
plot(S.lm)

##

summary(S.lm)

# so some combinations of continent and continuous covariates lack data so we'll drop those interaction terms

Empty.lm <- lm(Data$Species.richness ~ +1 , data = C.RCRS)

Full.lm <- lm(Data$Species.richness ~ . + Disturbance*MAT + Disturbance*TAP + Disturbance*TR + Disturbance*PD + Disturbance*TL + Hemisphere*MAT + Hemisphere*TAP + Hemisphere*TR + Hemisphere*PD + Hemisphere*TL, data = C.RCRS)

S.lm <- step(object = Empty.lm, scope = list(lower = Empty.lm, upper = Full.lm), direction = 'both')

par(mfcol = c(2,2))
plot(S.lm)


summary(S.lm)

####

Empty.lm <- lm(Data$Species.richness[-c(117,1075)] ~ +1 , data = C.RCRS[-c(117,1075),])

Full.lm <- lm(Data$Species.richness[-c(117,1075)] ~ . + Disturbance*MAT + Disturbance*TAP + Disturbance*TR + Disturbance*PD + Disturbance*TL + Hemisphere*MAT + Hemisphere*TAP + Hemisphere*TR + Hemisphere*PD + Hemisphere*TL, data = C.RCRS[-c(117,1075),])

S.lm <- step(object = Empty.lm, scope = list(lower = Empty.lm, upper = Full.lm), direction = 'both')

par(mfcol = c(2,2))
plot(S.lm)


summary(S.lm)

?step # also works for objects of class glm( )
?glm
?family
