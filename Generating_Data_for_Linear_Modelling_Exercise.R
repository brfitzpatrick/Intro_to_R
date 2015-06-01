#     This an R Code File for the Introduction to R Course available at
#     git@github.com:brfitzpatrick/Intro_to_R 
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
#                   Code File to Generate the Data for Module 2                #
#                                                                              #
#                          Linear Regression in R                              #
#                                                                              #
################################################################################

# Aim for y = b0 + b1*x1 + b2*x2^2 + b3*x1^3 + b4*x2^2 + b5*x2^4 + b6*x1*x2

n <- 500

x1 <- seq(from = -1, to = 1, length.out = n)

x2 <- seq(from = -1, to = 1, length.out = n) + rnorm(n = n, mean = 0, sd = 3)

x2 <- x2/(max(abs(x2)))

summary(x1)

summary(x2)

beta <- sample(x = seq(from = -5, to = 5, by = 0.05), size = 6, replace = TRUE)

beta

X <- data.frame(x1,
                x2,
                x1.2 = x1^2,
                x2.2 = x2^2,
                x1.3 = x1^3,
                x2.3 = x2^3,
                x1.4 = x1^4,
                x2.4 = x2^4,
                x1x2 = x1*x2)

y <- beta[1] + beta[1]*X$x1 + beta[2]*X$x2.2 + beta[3]*X$x1.3 + beta[4]*X$x2.2 + beta[5]*X$x2.4 + beta[6]*X$x1*X$x2

summary(y)

y <- y + rnorm(n = n, mean = 0, sd = 3.5)

m1.lm = lm(y ~ +1, data = X)

m2.lm = lm(y ~ ., data = X)

summary(m2.lm)
head(X)

step(object = m1.lm, scope = list(lower = m1.lm, upper = m2.lm), direction = 'both', steps = 1e5)

# beta: -4.90 -4.40  1.90  2.50 -2.75  3.50

Ex2.Data <- data.frame(y, x1, x2)

write.csv(x = Ex2.Data, row.names = FALSE, file = '~/Intro_to_R/Data/Linear_Modelling/Ex2_Data.csv')


###########
#
#  Data V2
#  

n = 500

x1 <- seq(from = -1, to = 1, length.out = n)

x2 <- seq(from = -1, to = 1, length.out = n)

Full.Comb <- expand.grid(x1, x2)

colnames(Full.Comb) <- c('x1', 'x2')

dim(Full.Comb)
head(Full.Comb)

Sample = Full.Comb[sample(x = 1:nrow(Full.Comb), size = 500, replace = FALSE),]

beta <- c(-4.90, -4.40,  1.90,  2.50, -2.75,  3.50)

y <- beta[1] + beta[1]*X$x1 + beta[2]*X$x2.2 + beta[3]*X$x1.3 + beta[4]*X$x2.2 + beta[5]*X$x2.4 + beta[6]*X$x1*X$x2

summary(y)

y <- y + rnorm(n = n, mean = 0, sd = 2.5)

Ex2.Data.v2 <- data.frame(y, Sample)

write.csv(x = Ex2.Data.v2, row.names = FALSE, file = '~/Intro_to_R/Data/Linear_Modelling/Ex2_Data_v2.csv')

