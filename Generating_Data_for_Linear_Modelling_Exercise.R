#     This an R Code File for the Introduction to R Course available at
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
#                   Code File to Generate the Data for Module 2                #
#                                                                              #
#                          Linear Regression in R                              #
#                                                                              #
################################################################################

n = 5000

x1 <- seq(from = -1, to = 1, length.out = n)

x2 <- seq(from = -1, to = 1, length.out = n)

Full.Comb <- expand.grid(x1, x2)

colnames(Full.Comb) <- c('x1', 'x2')

Sample = Full.Comb[sample(x = 1:nrow(Full.Comb), size = 2000, replace = FALSE),]

X <- data.frame(x1   = Sample$x1,
                x2   = Sample$x2,
                x1.2 = Sample$x1^2,
                x2.2 = Sample$x2^2,
                x1.3 = Sample$x1^3,
                x2.3 = Sample$x2^3,
                x1.4 = Sample$x1^4,
                x2.4 = Sample$x2^4,
                x1x2 = Sample$x1* Sample$x2)

beta <- c(-1, 1,  4,  2, -2,  -1, 3)

y <- beta[1] +
     beta[2]*X$x1 +
     beta[3]*X$x1.2 +
     beta[4]*X$x1.3 +
     beta[5]*X$x2.2 +
     beta[6]*X$x2.4 +
     beta[7]*X$x1*X$x2

y <- y + rnorm(n = length(y), mean = 0, sd = 0.75)

Data <- data.frame(y, Sample)

setwd('~/Intro_to_R/Data/Linear_Modelling/')

write.csv(x = Data, row.names = FALSE, file = 'Multiple_Regression_Data.csv')

################################################################################
#                                                                              #
#               End of Code File to Generate the Data for Module 2             #
#                                                                              #
#                          Linear Regression in R                              #
#                                                                              #
################################################################################
