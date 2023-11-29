# Meal simulation
meal_sim <- function(IC, t0, len_meal, Kamt, params){
    tf = t0 + len_meal
    tvals = seq(t0,tf,1) #c(t0, tf) 
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
    # returns last datapoint
    return(tail(out, n=1))
} # end of meal_sim

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
    # returns last datapoint
    return(tail(out, n=1))
} # end of fast_sim

day_sim <- function(IC0, len_meal, MealTimes, Kamt, params) {
    tvals <- c(0, MealTimes[1])
    last_meal <- -6*60
    out_fast0 <- fast_sim(IC0, tvals, last_meal, params)

    # Meal 1
    IC <- unlist(out_fast0[varnames])
    t0 <- MealTimes[1] 

    out_meal1 <- meal_sim(IC, t0, len_meal, Kamt, params)

    # Fasting after Meal 1
    IC <- unlist(out_meal1[varnames])
    t0 <- unlist(out_meal1['time'])
    tvals <- seq(0, MealTimes[2],1) #c(0, MealTimes[2])

    out_fast1 <- fast_sim(IC, tvals, MealTimes[1], params)
    # Meal 2
    IC <- unlist(out_fast1[varnames])
    t0 <- MealTimes[2]
    out_meal2 <- meal_sim(IC, t0, len_meal, Kamt, params)

    # Fasting after meal 2
    IC <- unlist(out_meal2[varnames])
    t0 <- unlist(out_meal2['time'])
    tvals <- seq(t0, MealTimes[3],1) #c(t0, MealTimes[3]) 
    out_fast2 <- fast_sim(IC, tvals, MealTimes[2], params)

    # Meal 3
    IC <- unlist(out_fast2[varnames])
    t0 = MealTimes[3]
    out_meal3 <- meal_sim(IC,t0,len_meal,Kamt,params)

    # Fasting to end of day
    t_day_end <- 24 * 60.0 # end of day
    IC <- unlist(out_meal3[varnames])
    t0 <- unlist(out_meal3['time'])
    tvals <- seq(t0, t_day_end, 1)
    out_fast3 <- fast_sim(IC,tvals,t_day_end,params)
return(out_fast3)
} # end of day sim

end_50daysim <- function(IC0, len_meal, MealTimes, Kamt, params) {
    # This function gives the final plasma and intracellular
    # K concentrations at the end of a 50 day simulation
    IC <- IC0 # first initial condition for day 1
    endday_val = array( rep(0,2*50), dim=c(2,50)) # row1: Kplas, row2:Kmuscle
    for(ii in 1:50){
        if (ii%%10 == 0){
            print(sprintf("Day: %i", ii))
        }
        day_ii_out <- day_sim(IC, len_meal, MealTimes, Kamt, params)
        endKplas <- day_ii_out$M_Kplas / params$V_plasma
        endKmusc <- day_ii_out$M_Kmuscle / params$V_muscle
        endday_val[1,ii] = endKplas
        endday_val[2,ii] = endKmusc

        IC <- unlist(day_ii_out[varnames])
    }
    return(endday_val)
}