# This is me trying again!
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
    out_fast0 <- fast_sim(IC0, tvals, last_meal, params)

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
} # end of day sim

end_50daysim <- function(IC0, len_meal, MealTimes, Kmt, params) {
    IC <- IC0 # first IC for day 1
    endday_val = array(rep(0,2*50), dim=c(2,50))
    for(ii in 1:50){
        if (ii%%10 == 0){
            print(sprintf("Day: %i", ii))
        }
        day_ii_out = day_sim(IC, len_meal, MealTimes, Kamt, params)
        end <- tail(day_ii_out, n=1)
        endKplas <- end$M_Kplas / params$V_plasma
        endKmusc <- end$M_Kmuscle / params$V_muscle
        endday_val[1,ii] = endKplas
        endday_val[2,ii] = endKmusc

        IC <- unlist(end[varnames])
    }
    return(endday_val)
}