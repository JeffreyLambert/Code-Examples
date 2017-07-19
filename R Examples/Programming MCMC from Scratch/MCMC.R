#*******************************
# Project: Markov Chain Monte Carlo Programming assignemnt
# Author: Jeff Lambert
# Date: 7/5/2017
# Program name: MCMC.R
# Purpose: Program the Metropolis Hastings MCMC Algorithm from scratch for simulating 
#  a bayesian posterior distribution
#*******************************

#*******************************
# Change Log
# Program Creation - 7/5/2017
#
#*******************************

# Notes: This was dreamt up as an assignment for a set of lectures I presented at an undergraduate
#         statistics summer program at the University of Colorado Anschutz Medical Campus.  
#         This task was presented to the more advanced students to compliment the simulation lecture
#         series.
#        This uses a non-conjugate prior (uniformd distribution), a bernoulli likelihood and a burn
#         in set equal to 10% of the total jumps.  
#         

#****************
# Load Packages
#****************
#######################################

data <- rbinom(10,1,p=.8) #generate a series of 0/1's

# Calculate the likelihood
# We will say that the data is distributed bernoulli
# The likelihood of the data is pdf equation for a single random variable multiplied n times
# where n is the number of random variables in the data set.  In this case 10.
likelihood <- function(theta,data){
  z = sum(data) # Number of successes
  N = length(data) # Total number of trials
  pDataGivenTheta <- (theta**z) * (1-theta)**(N-z) # By hand we've calculated the likelihood and put it in 
  pDataGivenTheta[ theta > 1 | theta < 0 ] = 0 # We are using this constraint in case
                                                                      # later on something wonky happens
  return(pDataGivenTheta)
}

# Calculate the prior
prior <- function(theta){
  #prior = rep(1,length(theta)) Found this suggested as an alternative to the unif
  prior = runif(1) #Chose the uniform as a prior cause it's easier to check that everything worked
                    # Without imposing any prior belief on the values of theta
  prior[ theta > 1 | theta < 0] = 0 #Since we are working with probability, we want to constrain
                                      # Theta to be between 0 and 1
  return(prior)
}

target <- function(theta,data){
  target <- likelihood(theta,data) * prior(theta) #Remember in class we talked about the posterior
                                                    # being approximately this?
  return(target)
  
}

trajLength <- 3000 #Total number of times we will iterate through the MCMC
trajectory <- rep(0,length(trajLength)) # Vector of data to overwrite and store values
trajectory[1] = .5 # This will be where the algorithm starts.  See what happens with other values
burnIn <- (.1*trajLength) # The first couple of walks are generally considered poor approximations
                            # and are usually thrown out.  Here we will throw out the first 10%

nAccepted = 0 # I've set this to keep track of the accepted jumps
nRejected = 0 # I've set this to keep track of the rejected jumps

set.seed(47405) #Seed for reproducibility aka same walk everytime


# The algorithm goes as follows:
# 1) Generate a single observation from a normal distribution as a proposed jump from the current value
    # of theta
# 1.5) Store the current value of theta as something else so you don't accidently overwrite it
# 2) Add the jump value to the current value of theta, let's call this theta_star
# 3) Calculate P(theta_star)/P(theta) = alpha, using the target distribution
# 4) If alpha >= 1 this means that theta_star has a higher probabilty than theta and we therefore
#     accept the proposed jump (current value of theta + value of proposed jump).  If alpha < 1 then
#     the jump is rejected and theta stays at the current value of theta

for( t in 1:(trajLength-1)){
  currentPosition = trajectory[t]
  proposedJump = rnorm(1,mean=0,sd=.1)
  probAccept = min(1,
                   target( currentPosition + proposedJump, data)/target(currentPosition,data)
                   )
  if( runif(1) < probAccept){
    trajectory[t+1] = currentPosition + proposedJump
    if( t > burnIn){ nAccepted = nAccepted + 1}
  } else {
    trajectory[t+1] = currentPosition
    if( t > burnIn){ nRejected = nRejected + 1}
  }
}

acceptedTraj = trajectory[ (burnIn+1) : length(trajectory)]


for( t in 1:(trajLength-1)){
  currentPosition = trajectory[t]
  proposedJump = rnorm(1,mean=0,sd=.1)
  
  alpha = target( currentPosition + proposedJump, data)/target(currentPosition,data)
  
  if( alpha > 1){
    trajectory[t+1] = currentPosition + proposedJump
    if( t > burnIn){ nAccepted = nAccepted + 1}
  } else {
    trajectory[t+1] = currentPosition
    if( t > burnIn){ nRejected = nRejected + 1}
  }
}

acceptedTraj = trajectory[ (burnIn+1) : length(trajectory)]

hist(acceptedTraj)
mean(acceptedTraj)
mean(data)
