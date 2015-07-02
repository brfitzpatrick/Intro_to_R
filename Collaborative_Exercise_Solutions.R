# When using this data, please cite the original publication:

# Gibb H, Sanders NJ, Dunn RR, Watson S, Photakis M, Abril S, Andersen AN, Angulo E, Armbrecht I, Arnan X, Baccaro FB, Bishop TR, Boulay R, Castracani C, Del Toro I, Delsinne T, Diaz M, Donoso DA, Enríquez ML, Fayle TM, Feener DH, Fitzpatrick MC, Gómez C, Grasso DA, Groc S, Heterick B, Hoffmann BD, Lach L, Lattke J, Leponce M, Lessard J, Longino J, Lucky A, Majer J, Menke SB, Mezger D, Mori A, Munyai TC, Paknia O, Pearce-Duvet J, Pfeiffer M, Philpott SM, de Souza JLP, Tista M, Vasconcelos HL, Vonshak M, Parr CL (2015) Climate mediates the effects of disturbance on ant assemblage structure. Proceedings of the Royal Society B 282(1808): 20150418. http://dx.doi.org/10.1098/rspb.2015.0418

# Additionally, please cite the Dryad data package:

# Gibb H, Sanders NJ, Dunn RR, Watson S, Photakis M, Abril S, Andersen AN, Angulo E, Armbrecht I, Arnan X, Baccaro FB, Bishop TR, Boulay R, Castracani C, Del Toro I, Delsinne T, Diaz M, Donoso DA, Enríquez ML, Fayle TM, Feener DH, Fitzpatrick MC, Gómez C, Grasso DA, Groc S, Heterick B, Hoffmann BD, Lach L, Lattke J, Leponce M, Lessard J, Longino J, Lucky A, Majer J, Menke SB, Mezger D, Mori A, Munyai TC, Paknia O, Pearce-Duvet J, Pfeiffer M, Philpott SM, de Souza JLP, Tista M, Vasconcelos HL, Vonshak M, Parr CL (2015) Data from: Climate mediates the effects of disturbance on ant assemblage structure. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.r36n0


Data <- read.csv(file = '/home/ben/Downloads/Ants.csv')

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






