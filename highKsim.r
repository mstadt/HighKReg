# This script can be used to simulate a single 50 day simulation
# of high K intake.

# load libraries
library(deSolve)
library(rootSolve)

# load relevant R files
source("setparams.r") # model parameters
source("model.r") # model equations

# Set model parameters
params <- set_params() 
# CONTROL K
Kamt <- 78/3 #4 * 78 / 3
# NOTE: need to commment this for high K simulations
params$eta_ptKreab_highK <- params$eta_ptKreab_base

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
                M_Kmuscle = 3103.72702)

# Get SS initial conditions
optsSS <- list(meal_start = 1000, # so that C_insulin is at SS
            Kintake = params$Phi_Kin_ss,
            SS = 1)

ST <- stode(init_guess, time = 0, func = model,
                parms = params,
                opts = optsSS)

# print(ST$y)

K_plasSS <- ST$y['M_Kplas'] / params$V_plasma
K_interSS <- ST$y['M_Kinter'] / params$V_interstitial
K_muscleSS <- ST$y['M_Kmuscle'] / params$V_muscle

# print('SS')
# print(sprintf("K_plas = %0.3f", K_plasSS))
# print(sprintf("K_inter = %0.3f", K_interSS))
# print(sprintf("K_muscle = %0.3f", K_muscleSS))

# Run ODE model for 50 days simulation

# Fasting simulation
fast_sim <- function(IC, tvals, last_meal, params) {
    opts_fast <- list(meal_start = last_meal,
                        Kintake = 0,
                        SS = 0)
    out <- as.data.frame(lsoda(y = IC,
                                times = tvals,
                                func = model,
                                parms = params,
                                opts = opts_fast,
                                rtol = 1e-8,
                                atol = 1e-10))
    return(out)
} # end of fast_sim

# Meal simulation
meal_sim <- function(IC, t0, len_meal, Kamt, params){
    tf = t0 + len_meal
    tvals = seq(t0,tf,1)
    Kin = Kamt / (len_meal)
    opts_meal <- list(meal_start = t0,
                        Kintake = Kin,
                        SS = 0)
    out <- as.data.frame(lsoda(y = IC,
                                    times = tvals,
                                    func = model,
                                    parms = params,
                                    opts = opts_meal,
                                    rtol = 1e-8,
                                    atol = 1e-10))
    return(out)
} # end of meal_sim

day_sim <- function(IC0, len_meal, MealTimes, Kamt, params) {
    tvals <- seq(0,MealTimes[1],1)
    last_meal <- -6*60
    out_fast0 <- fast_sim(IC, tvals, last_meal, params)

    # Meal 1
    end <- out_fast0[nrow(out_fast0),] # get last row of out fast
    IC <- unlist(end[varnames])
    t0 <- MealTimes[1] #unlist(end['time'])

    out_meal1 <- meal_sim(IC, t0, len_meal, Kamt, params)

    # combine dataframes
    out_all <- rbind(out_fast0, out_meal1)

    # Fasting after Meal 1
    end <- out_meal1[nrow(out_meal1),] # get last row of out_meal1
    IC <- unlist(end[varnames])
    t0 <- unlist(end['time'])
    tvals <- seq(t0, MealTimes[2], 1)

    out_fast1 <- fast_sim(IC, tvals, MealTimes[1], params)
    out_all <- rbind(out_all, out_fast1)
    # Meal 2
    end <- out_fast1[nrow(out_fast1), ] # get last row of out fast
    IC <- unlist(end[varnames])
    t0 <- MealTimes[2]
    out_meal2 <- meal_sim(IC, t0, len_meal, Kamt, params)
    out_all <- rbind(out_all, out_meal2)
    # Fasting after meal 2
    end <- out_meal2[nrow(out_meal2), ] # last row
    IC <- unlist(end[varnames])
    t0 <- unlist(end['time'])
    tvals <- seq(t0, MealTimes[3], 1)
    out_fast2 <- fast_sim(IC, tvals, MealTimes[2], params)
    out_all <- rbind(out_all, out_fast2)
    # Meal 3
    end <- out_fast2[nrow(out_fast2) ,]
    IC <- unlist(end[varnames])
    t0 = MealTimes[3]
    out_meal3 <- meal_sim(IC,t0,len_meal,Kamt,params)
    out_all <-rbind(out_all, out_meal3)

    # Fasting to end of day
    t_day_end <- 24 * 60.0 # end of day
    end <- out_meal3[nrow(out_meal3), ] # last row
    IC <- unlist(end[varnames])
    t0 <- unlist(end['time'])
    tvals <- seq(t0, t_day_end, 1)
    out_fast3 <- fast_sim(IC,tvals,t_day_end,params)

    out_all <-rbind(out_all,out_fast3)
return(out_all)
}

# One day of meals
MealTimes <- array(c(6,12,18)) * 60.0
# fasting simulation starting at IC
IC0 <- unlist(ST$y[varnames])

sim_1day <- day_sim(IC0, 30, MealTimes, Kamt, params)

end_conc = 


# TODO:
# - meal times need to match what is in the MATLAB code
# To optimize Morris Analysis do not save the rbind out code
# because this will slow things down.... I only need the concentrations at the 
# very end of the simulation



# TODO:
#   - Do the K intake as "events" in the R model as done in the calcium
#      simulations for teripartide
#      NOTE: this is note
#   