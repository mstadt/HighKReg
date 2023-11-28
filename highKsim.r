# This script can be used to simulate a single 50 day simulation
# of high K intake.

# load libraries
library(deSolve)

# load relevant R files
source("setparams.r") # model parameters
source("model.r") # model equations

# Set model parameters
p <- set_params() 

# set initial conditions for first simulation

# TODO:
# - fasting function
#   - NOTE: start time at the end of the meal function
#      so that the time of the meal is accounted for in the insulin function
#      OR SEE IF I CAN GET THE meal_start input....
# - meal function
# - combine functions into one day
# - initial conditions should be steady state
# - meal times need to match what is in the MATLAB code



# TODO:
#   - Do the K intake as "events" in the R model as done in the calcium
#      simulations for teripartide
#      NOTE: this is note
#   