# load libraries
library(deSolve)
library(rootSolve)

# load relevant R files
source("setparams.r") # model parameters
source("model.r") # model equations
source("end_50daysim.r")

# Set model parameters
params <- set_params()

# PT effect OFF
# params$eta_ptKreab_highK = params$eta_ptKreab_base

Kamt_control <- 78/3
Kamt <- 4*Kamt_control

MealTimes <- array(c(6,12,18)) * 60.0



# set IC
IC <- c(M_Kgut = 4.375,
                M_Kplas = 18.92818,
                M_Kinter = 42.06262,
                M_Kmuscle = 3103.76386)

IC0 <- IC

#print('start 50 day sim')
#test_vals <- end_50daysim(IC0, 30, MealTimes, Kamt, params)
#print('done')

test_vals <- main_sim(params)

print(test_vals)