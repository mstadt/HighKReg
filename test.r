# load libraries
library(deSolve)
library(rootSolve)

# load relevant R files
source("setparams.r") # model parameters
source("model.r") # model equations
source("end_50daysim2.r")

# Set model parameters
params <- set_params()

# PT effect OFF
params$eta_ptKreab_highK = params$eta_ptKreab_base

Kamt_control <- 78/3
Kamt <- 4*Kamt_control

MealTimes <- array(c(6,12,18)) * 60.0

# variable names
get_varnames <- function() {
    c("M_Kgut",
    "M_Kplas",
    "M_Kinter",
    "M_Kmuscle")
}
varnames <- get_varnames()

# set initial guess
init_guess <- c(M_Kgut = 4.375,
                M_Kplas = 18.92818,
                M_Kinter = 42.06262,
                M_Kmuscle = 3103.76386)

                # Get SS initial conditions
# optsSS <- list(meal_start = 1000, # so that C_insulin is at SS
#             Kintake = params$Phi_Kin_ss,
#             SS = 1)

# ST <- stode(init_guess, time = 0, func = model,
#                 parms = params,
#                 opts = optsSS)

# Initial condition
IC0 <- init_guess #unlist(ST$y[varnames])

#last_meal = -6*60
#tvals = c(0,MealTimes[1])
#out_fast0 <- fast_sim(IC0, tvals, last_meal, params)
#out_day1 <- day_sim(IC0, 30, MealTimes, Kamt, params)

test_vals <- end_50daysim(IC0, 30, MealTimes, Kamt, params)