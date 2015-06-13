# Code File for Quneensland University of Technology: Bayesian Research and Applications Group's Introduction to R Course
# Course prepared and presented by Ben R. Fitzpatrick (ben.r.fitzpatrick@gmail.com) 14th of March 2012

# R is all about the free sharing of code with the proper acknowledgement of authorship so feel free share this file (just don't delete my name from it ;) and only add yours if you've improved it)

##############################
#                            #
#   This is code file 4      #
#   Intro to R Programming   #
#                            #
##############################


# Repetitions of a particular sequence of operations on successive selections of data possibly with a changes in the operations may be efficiently accomplished by utilising

# Control Flow

?Control

# and Logical statements

?Logic

# if(condition) action  # read as: if condition is true then perform actions

# Sorts of Conditions that might be used in an if() statement:

x <- 3

#Equality

x == 3 # logical test of whether the statement x = 3 is true

x == 4 # logical test of the whether the statement x = 4 is true

!(x == 4) # think of this as the negation of the result of the test of whether x = 4 is true

x == 2 & 3  # logical test of whether x = both 2 and 3

x == 2 | 3 # | is an 'inclusive or' so the test here is whether x (2, 3 or 2 & 3) if x = any of these then the statement is true

# works with vectors by testing element by element equality

y <- c(1,2,3)

z <- seq(1:3)

z == y

y[1] | y[2] == z[1] | z[3] # logical test of whether the 1st element of y or the 2nd element of y equal the 1st element of z or the 3rd element of z

# works for character vectors too

a <- c('alpha')

b <- c('alpha','beta','gamma')

a == b

# Relative Size

x < 3

x <= 3

x > 1

#  if(cond) expr

x

if(x==3) print('x = 3') # if x = 3 print 'x = 3'

if(x==3) print('x = 3') else print('x not equal to 3') # if x = 4 print 'x = 3', if x does not = 3 print 'x not equal to 4'

x <- 4

if(x==3) print('x = 3') else print('x not equal to 3')

# it's a bit hard to see the point of if() statements without a function
# so let's make a function that uses 'if else' statement to tell you whether a number is odd or even

# for the algorithm I have in mind we'll need the floor() command to round down (note: its opposite is ceiling() )

floor(35.5)
floor(1/3)
floor(4/3)

# and the paste command to create our output 

paste(4,c('is even'))

x

paste(x)

paste(x,c('is even'))

  if(x/2 == floor(x/2)) {paste(x,c('is even'))} else {paste(paste(x),c('is odd')) }

# try for a few x values

#e.g.

x <- 933

  if(x/2 == floor(x/2)) {paste(x,c('is even'))} else {paste(x,c('is odd')) }

#

# the process of defining x then re-entering the test can be streamlined by defining a function
# defining a function is creating you own custom R command

even <- function(x){

    if(x/2 == floor(x/2)) {paste(paste(x),c('is even'))} else {paste(paste(x),c('is odd')) }

  }

even(x=121)

even(121) # assumes arguments are supplied in order

even(124)

# can you make a function to test if a number is odd?


even  #recover the R code for a function by entering it without any brackets or arguments

#include R functions provided in R base

lm

#  '!'  negation symbol, read as 'not'

if(!x == 2) {paste(x,'is not 2')}




### for(variable in sequence) action

1:3 # quick way to define a sequece 

for(i in 1:3) {print(paste(i))}


for(i in seq(3,27,3)) {print(paste(i))}

# we can store the results of for() loops in variables

x <- rep(0,6)

x

# refer to elements of a vector by number

x[2] # 2nd element of x

#define elements of a vector by number

x[2] <- 3

x

length(x) # length of vector x

# so we can use the sequence of a for() loop to perform actions on objects by element/row/column number e.g.

# storing the results of a for loop in a vector

for(i in 1:length(x)){
  #we can let for() loops span several lines for readability much as we did for function definitions
  x[i] <- i^2 # store i^2 in the i'th element of x
} # end of for loop

x

# we can excecute all sorts of commands across a for() loop

n <- 600

x <- seq(1:n)

y <- rnorm(n=n,mean=0,sd=1)

plot(x,y)

rainbow(5) # rainbow(n)  'Creates a vector of ‘n’ contiguous colors' in this case equally spaced across the rainbow see also heat.colors(),terrain.colors() ... etc.

?rainbow

for(i in 2:length(x)){

  lines(x[(i-1):i],y[(i-1):i],col=rainbow(length(x))[i]) 

}  # connects point (x[(i-1)],y[(i-1)]) to point (x[i],y[i]) by a line of colour given by element 'i' of rainbow(length(x)) (a list of colour codes with the same number of elements as there are in vector 'x' and the colours equally spaced across the full visual spectrum of the rainbow) and does this for all 'i' in order from 'i=2' to 'i=length(x)'

# it is necessary to start at 'i=2' since our first vector index 'x[(i-1)]' and we wish to start at the first element of 'x'

y.i.mean <- rep(0,length(y))

for(i in 1:length(y)){

  y.i.mean[i] <- mean(y[1:i]) # calculate the mean of elements 1 to i of vector y and store this number in y.i.meam element i

}

plot(x,y.i.mean,type='l',xlab='sample size',ylab='mean')
abline(h=0)

plot(x[-seq(1:50)],y.i.mean[-seq(1:50)],type='l',xlab='sample size',ylab='mean') # plot vector x (after having droped the first 50 entries) against vector y.i.mean (again after having dropped the first 50 entries)
abline(h=0)

###  while() loops

x <- 1
    
while(length(x)<10) x<-c(x,x[length(x)]+1)  # while the length of vector x is less than 10 add another element onto the end of x of value eqaul to 1 + the last element of x

x

# so we could make our own sequence creation function:

my.seq <- function(from,to,by){

  x <- from

  length.max <- floor((to-from)/by)

  while(length(x)<=length.max) x<-c(x,x[length(x)]+by)

  return(x)

}

my.seq(1,10,1)

my.seq(3,27,3)

# but probably a more useful application is repeating some process until some level of precision is attained
# say increasing the sample size by 1 until the sample approximations to the population mean has remained within a particular precision around the population mean for 500 additions to the sample size

y <- rnorm(n=700,mean=0,sd=15) # start with a random sample of 700 observations from a normal distribution with a mean of 0 and a standard deviation of 15

#y

accr <- 5e-2 # this is the accuracy which we would like the estimate

# the first step is to have a function that for any particular vector v (of length > 500) will return the maximum absolute value of the means of elements 1 to the length of vector v, 1 to the 1 less than the length of vector v, 1 to the 2 less than the length of vector v,..., 1 to the 500 less than the length of vector v
# so that we may test if the maximum absolute value of these values is less the the accuracy (remember the the population mean is 0 here so sample mean - population mean = sample mean)

l500mm <- function(v){

  l500m <- rep(0,500)

  for(i in 1:500){

    l500m[i] <- mean(v[1:(length(v)-i+1)])

  }

  mx.abs <- max(abs(l500m))

  return(mx.abs)
  
}

#then we may use this function in a while() loop such that while the maximum of the means of 1 to each of the last 500 values of y is greater than the required accuracy (accr) additional samples of size one are drawn from the normal distribution and appended to the end of vector y

while(l500mm(y)>accr) y <- c(y,rnorm(n=1,mean=0,sd=15))


summary(y)

length(y)

cut <- 0.2*length(y)  # the first 20% will probably be quite variable and we are interested in the covergence to 0 so I only plot the last 80% of the data

  plot(0,0,ylim=c(-1.9*abs(mean(y[1:(cut+50)])),1.9*abs(mean(y[1:(cut+50)]))),xlim=c(cut,length(y)),type='n',xlab='sample size',ylab='sample mean') # setting up the plot box (just the x and y limits and the axis lables)

  x <- 1:length(y) # the index to plot the sample means against


for(i in cut:length(y)){

  lines(x[(i-1):i],c(mean(y[1:(i-1)]),mean(y[1:i])),col=rainbow(2*length(x))[i]) # joining the sample mean for y length (i-1) to the sample mean for y length i by lines (x coordinates are our count of sample size at y[i]) coloured in proportion to i

}

# and now the visual check that the sample mean has no strayed more than +/- accr from 0 for the last 500 additions to y

abline(h=accr,col='cyan')

abline(h=-accr,col='cyan')

abline(v=(length(y)-500),col='red')

mean(y)

################################
#                              #
#   Programming Excercise 1    #
#                              #
#    Create a solar system     #
#          emulation           #
#                              #
################################

n.step <-  500 # number of animation frames to create

# Solution

start <- 0

end <- 2*pi

theta <- seq(from=start,to=end,length.out=n.step) #angle controlling rotation of Planet 1 by changing the angle gradually with a for() loop we can generate movement along a orbital path

Orbit1 <- data.frame(theta,x=rep(0,length(theta)),y=rep(0,length(theta))) # dataframe to store the x and y coordinates of the orbital path of Planet 1
#producing each new frame of video by ploting Planet 1 at the position in a row of this dataframe and advancing through these rows in order we can create the emulate the movement of  planet 1 around the sun (note this is an emulation not a simulation there are no graviational law calculations in this code)

r <- 0.75 # radius of orbit of Planet 1

for(i in 1:nrow(Orbit1)){

  Orbit1[i,2] <- r*cos(Orbit1[i,1]) # Planet 1 x-coordinate at angle theta[i] i.e. row i

  Orbit1[i,3] <- r*sin(Orbit1[i,1]) # Planet 1 y-coordinate at angle theta[i] i.e. row i

}

# same proceedure as for Planet 1 just changing all the numbers a bit so Planet 2 follows a different path 

Orbit2 <- data.frame(theta,x=rep(0,length(theta)),y=rep(0,length(theta)))

r <- 0.3 # radius

for(i in 1:nrow(Orbit2)){

  Orbit2[i,2] <- r*cos(-(2*Orbit2[i,1]+pi/4))

  Orbit2[i,3] <- r*sin(-(2*Orbit2[i,1]+pi/4))

}

# same proceedure as for Planet 1 just changing all the numbers a bit so Planet 2 follows a different path 

Orbit3 <- data.frame(theta,x=rep(0,length(theta)),y=rep(0,length(theta)))

r <- 0.9 #radius

for(i in 1:nrow(Orbit3)){

  Orbit3[i,2] <- r*cos((3.5*Orbit3[i,1]+pi/4))

  Orbit3[i,3] <- r*sin((3.5*Orbit3[i,1]+pi/4))

}

# adding a moon orbiting around Planet 2

phi <- 6*(theta+pi/4) 

O2.moon <- data.frame(phi,x=rep(0,length(theta)),y=rep(0,length(theta)))

r.O2m <- 0.15


for(i in 1:nrow(Orbit2)){

  O2.moon[i,2] <- Orbit1[i,2] + r.O2m*cos(O2.moon[i,1]-pi/4)

  O2.moon[i,3] <- Orbit1[i,3] + r.O2m*sin(O2.moon[i,1]-pi/4)

}

# adding a moon around Planet 3

eta <- 9*(theta+pi/4)

O3.moon <- data.frame(eta,x=rep(0,length(theta)),y=rep(0,length(theta)))

r.O3m <- 0.15


for(i in 1:nrow(O3.moon)){

  O3.moon[i,2] <- Orbit3[i,2] + r.O3m*cos(O3.moon[i,1]+pi/4)

  O3.moon[i,3] <- Orbit3[i,3] + r.O3m*sin(O3.moon[i,1]+pi/4)

}



# Warning for epileptics this plot flashes a lot as it progresses through the for() loop

setwd('/home/ben/animation') # change the file path to a file path on your computer where you would like the sequence frames (images) to be saved that we will stick together into an animation below

# to preview the animation in R use this code which has the image saving bits commented out 

for(i in 1:nrow(Orbit1)){

  jpeg(filename = paste('image',paste(i),'.jpg',sep=''),width = 800, height = 800)  

  plot(0,0,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=5,col='yellow',pch=19,xlab='',ylab='')
  par(new=T)
  plot(Orbit1[i,2],Orbit1[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=3,col='blue',pch=19,xlab='',ylab='')
  par(new=T)
  plot(Orbit2[i,2],Orbit2[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=1,pch=19,col='red',xlab='',ylab='')
  par(new=T)
  plot(O2.moon[i,2],O2.moon[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=1,pch=19,col='grey',xlab='',ylab='')
  par(new=T)
  plot(O3.moon[i,2],O3.moon[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=0.5,pch=19,col='grey',xlab='',ylab='')
  par(new=T)
  plot(Orbit3[i,2],Orbit3[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=1.5,pch=19,col='green',xlab='',ylab='')
  par(new=T)  
  plot(0,0,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=5,col='yellow',pch=19,xlab='',ylab='')       #par(new=T)
  dev.off()
  #dev.new()

}

# to write the image files that we will use to create the animation use this code:

for(i in 1:nrow(Orbit1)){

  jpeg(filename = paste('image',paste(i),'.jpg',sep=''),width = 800, height = 800)  
  plot(0,0,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=5,col='yellow',pch=19,xlab='',ylab='')
  par(new=T)
  plot(Orbit1[i,2],Orbit1[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=3,col='blue',pch=19,xlab='',ylab='')
  par(new=T)
  plot(Orbit2[i,2],Orbit2[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=1,pch=19,col='red',xlab='',ylab='')
  par(new=T)
  plot(O2.moon[i,2],O2.moon[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=1,pch=19,col='grey',xlab='',ylab='')
  par(new=T)
  plot(O3.moon[i,2],O3.moon[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=0.5,pch=19,col='grey',xlab='',ylab='')
  par(new=T)
  plot(Orbit3[i,2],Orbit3[i,3],xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=1.5,pch=19,col='green',xlab='',ylab='')
  par(new=T)  
  plot(0,0,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),cex=5,col='yellow',pch=19,xlab='',ylab='')       #par(new=T)
  dev.off()
  #dev.new()

}


graphics.off()

# the 3D version

rgl.open()

counter=1

for(i in 1:(0.5*nrow(Orbit1))){

#for(i in 1:250){

  counter=counter+1

  view3d(theta=0, phi=-30)

  rgl.spheres(c(1.1),c(1.1),c(1.1),radius=0.05,xlab='',ylab='',interactive=F,alpha=0)

  rgl.spheres(c(1.1),c(1.1),c(-1.1),radius=0.05,xlab='',ylab='',interactive=F,alpha=0)


  rgl.spheres(c(1.1),c(-1.1),c(1.1),radius=0.05,xlab='',ylab='',interactive=F,alpha=0)

  rgl.spheres(c(1.1),c(-1.1),c(-1.1),radius=0.05,xlab='',ylab='',interactive=F,alpha=0)

  
  rgl.spheres(c(-1.1),c(1.1),c(-1.1),radius=0.05,xlab='',ylab='',interactive=F,alpha=0)

  rgl.spheres(c(-1.1),c(1.1),c(1.1),radius=0.05,xlab='',ylab='',interactive=F,alpha=0)


  rgl.spheres(c(-1.1),c(-1.1),c(-1.1),radius=0.05,xlab='',ylab='',interactive=F,alpha=0)

  rgl.spheres(c(-1.1),c(-1.1),c(1.1),radius=0.05,xlab='',ylab='',interactive=F,alpha=0)

  
  rgl.spheres(0,0,0,radius=0.25,col='yellow',xlab='',ylab='',interactive=F)
  
  rgl.spheres(Orbit1[i,2],Orbit1[i,3],0,radius=0.05,col='blue',xlab='',ylab='',interactive=F)
  
  rgl.spheres(Orbit2[i,2],Orbit2[i,3],0,radius=0.04,col='red',xlab='',ylab='',interactive=F)
  
  rgl.spheres(O2.moon[i,2],O2.moon[i,3],0,radius=0.02,col='grey',xlab='',ylab='',interactive=F)
  
  rgl.spheres(O3.moon[i,2],O3.moon[i,3],0,radius=0.02,col='grey',xlab='',ylab='',interactive=F)
  
  rgl.spheres(Orbit3[i,2],Orbit3[i,3],0,radius=0.08,col='green',xlab='',ylab='',interactive=F)

  rgl.snapshot(filename=paste('image',paste(counter),'.png',sep=''), fmt="png")
   
  clear3d(type=c('shapes'))

}


#now the fun bit who can introduce a moon into this system
#how about a commet  (commets have elliptical orbits)
# collision detection???? ;-)

#at the Linux Terminal

# ffmpeg -f image2 -r 1 -i image%d.jpg -r 1 -s 800x800 joo.avi

#ffmpeg -i joo.avi -f yuv4mpegpipe - | yuvfps -s 30:1 -r 30:1 | ffmpeg -f yuv4mpegpipe -i - -b 28800k aa30fps.avi





#########################################
#                                        #
#   Programming Excercise 2              #
#                                        #
#   Prime Numbers and Interger Divisors  #
#                                        #
##########################################

# The Excercise
# Can you make a function to test whether a number is prime then if it is not return it's interger divisors other than 1 and itself?
# Can you modify your function so that it can perform this sequence of actions for each element in a supplied vector of numbers to test?
# How long does your function take to run?
# Can you alter your code to make it run faster?











prime.test <- function(v){ # now just define prime.test2() once, deleted all the repeates of the same calculations
  
  Out = data.frame(Number=v,Is.Prime=factor(rep(NA,length=length(v)),levels=c('Prime','Not.Prime')),N.Divs=numeric(length=length(v)))

  n.divs = rep(0,(max(v)-2))

   prime.test2 = function(x){ 

      if(x<1) {return(paste('Not.Prime'))}

      if(x==1 | x==2 | x==3) {return(list('Prime',0))}

      pf = my.seq(from=2,to=(x-1),by=1)  # potential factors
      
      n.div.i = sum(as.numeric((x/pf-floor(x/pf))==0))

      if(n.div.i==0) result = list('Prime',0) else result = list('Not.Prime',n.div.i)

      return(result)

      }      

  for(i in 1:length(v)){
   
    op = prime.test2(x=v[i])

    Out[i,2] = paste(op[1])

    Out[i,3] = op[2]

  }

  max.divs = max(Out[,3])  # number of extra columns required (add them all here once outside any loop to avoid recreating the dataframe with each iteration of the loop

  Out = data.frame(Out,matrix(0,nrow=nrow(Out),ncol=max.divs))

  for(i in 1:length(v)){

    x = v[i]

    pf = my.seq(from=2,to=(x-1),by=1)

    x.pf = as.numeric((x/pf-floor(x/pf))==0)
    
    Divs = cbind(x.pf,pf) #making another matrix inside a loop further slowing things

    counter = 0

    for(j in 1:nrow(Divs)){  #nested loop, oh no!

      if(Divs[j,1]==1)  counter = counter + 1

      if(Divs[j,1]==1) Out[i,3+counter] = Divs[j,2] # two separate if() statements using the same condition

    }

  }

  Out=data.frame(Out)

  return(Out)

}  

system.time(sl <- ptv.slow(v=seq(1e4,1e4+1000,1)))[3]/1000 # 0.400605   sec per element in v

system.time(sl.sp3 <- ptv.sp3(v=seq(1e4,1e4+1000,1)))[3]/1000 # 0.405414   sec per element in v

system.time(sl2 <- ptv.slow(v=seq(1e5,1e5+100,1)))[3]/1000 #    sec per element in v

system.time(sl2.sp3 <- ptv.sp3(v=seq(1e5,1e5+100,1)))[3]/1000 #    sec per element in v


      
#################
############
################





# To Speed Up R Code:

# Replace loops with matrix operations

# Where loops are unavoidable:

# Define you data.storage object once outside the loop and fill it from within the loop (rather than adding new columns or rows with each iteration of the loop- R has to re-define the whole thing each time if you do this )

# try using lapply() instead of for()
#'lapply() can be faster than a carefully crafted for() loop (since C-level code is more efficient in memory allocation)' - Brian D. Ripley, Professor of Applied Statistics, Oxford

# call 'C' or 'fortran' for the slow bits (I can't help with this sorry)

# where job allows for parallel computing split it across multiple processor cores using package 'multicore' e.g. mclapply()  or the REvolution package










#or if you would rather try to emulate solar system producing something along the lines of the animation I made: Solar_System.avi
#(you don't have to make it into an external animation an animation within R is also fine for our purposes here)


# If you'd like more exercises to learn R programming by doing:

# Project Euler has a great series problems designed to be solved with mathematics and programming.

# 'The problems range in difficulty and for many the experience is inductive chain learning. That is, by solving one problem it will expose you to a new concept that allows you to undertake a previously inaccessible problem. So the determined participant will slowly but surely work his/her way through every problem.'

# <https://projecteuler.net/>

# R is a full programming language and so you should be able to solve any of the problems by programming in R

# Remember you've got Wikipedia and Wolfram MathWorld if you need to look up any maths concepts to do a problem

# 
