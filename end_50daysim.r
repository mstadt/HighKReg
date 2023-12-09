# This script can be used to find the K_plas and K_muscle
# values at the end of a simulation of 50 days

# Fasting simulation
fast_sim <- function(IC, tvals, last_meal, params) {
    opts_fast <- list(meal_start = last_meal,
                        Kintake = 0,
                        SS = 0)
    out <- as.data.frame(lsoda(y = IC,
                                times = tvals,
                                func = model_Kreg,
                                parms = params,
                                opts = opts_fast,
                                rtol = 1e-8,
                                atol = 1e-8))
    return(tail(out,n=1))
} # end of fast_sim

# Meal simulation
meal_sim <- function(IC, t0, len_meal, Kamt, params){
    tf = t0 + len_meal
    tvals = c(t0,tf) #seq(t0,tf,1)
    Kin = Kamt / (len_meal)
    opts_meal <- list(meal_start = t0,
                        Kintake = Kin,
                        SS = 0)
    out <- as.data.frame(lsoda(y = IC,
                                    times = tvals,
                                    func = model_Kreg,
                                    parms = params,
                                    opts = opts_meal,
                                    rtol = 1e-8,
                                    atol = 1e-8))
    return(tail(out,n=1))
} # end of meal_sim


day_sim <- function(IC0, len_meal, MealTimes, Kamt, params) {
    tvals <- c(0,MealTimes[1])
    last_meal <- -6*60
    out_fast0 <- fast_sim(IC0, tvals, last_meal, params)

    # Meal 1
    # end <- out_fast0 
    IC <- unlist(out_fast0[varnames])
    t0 <- MealTimes[1] #unlist(end['time'])

    out_meal1 <- meal_sim(IC, t0, len_meal, Kamt, params)

    # Fasting after Meal 1
    #end <- out_meal1
    IC <- unlist(out_meal1[varnames])
    t0 <- unlist(out_meal1['time'])
    tvals <- c(t0, MealTimes[2]) 

    out_fast1 <- fast_sim(IC, tvals, MealTimes[1], params)

    # Meal 2
    #end <- out_fast1 
    IC <- unlist(out_fast1[varnames])
    t0 <- MealTimes[2]
    out_meal2 <- meal_sim(IC, t0, len_meal, Kamt, params)

    # Fasting after meal 2
    #end <- out_meal2 
    IC <- unlist(out_meal2[varnames])
    t0 <- unlist(out_meal2['time'])
    tvals <- c(t0, MealTimes[3]) 
    out_fast2 <- fast_sim(IC, tvals, MealTimes[2], params)

    # Meal 3
    #end <- out_fast2
    IC <- unlist(out_fast2[varnames])
    t0 = MealTimes[3]
    out_meal3 <- meal_sim(IC,t0,len_meal,Kamt,params)

    # Fasting to end of day
    t_day_end <- 24 * 60.0 # end of day
    #end <- out_meal3 
    IC <- unlist(out_meal3[varnames])
    t0 <- unlist(out_meal3['time'])
    tvals <- c(t0, t_day_end) 
    out_fast3 <- fast_sim(IC,tvals,t_day_end,params)
return(out_fast3)
} # end of day sim

end_50daysim <- function(IC0, len_meal, MealTimes, Kamt, params) {
    IC <- IC0 # first IC for day 1
    # endday_val = array(rep(0,2*50), dim=c(2,50))
    for(ii in 1:50){
        # if (ii%%10 == 0){
        #     print(sprintf("Day: %i", ii))
        # }
        day_ii_end <- day_sim(IC, len_meal, MealTimes, Kamt, params)
        IC <- unlist(day_ii_end[varnames])
    }

    end_MKplas <- day_ii_end$M_Kplas
    end_MKmusc <- day_ii_end$M_Kmuscle

    return(c(end_MKplas, end_MKmusc))
}

main_sim <- function(params){
    # simulation settings
    Kamt_control <- 78/3
    Kamt <- 4 * Kamt_control  # high K diet
    MealTimes <- array(c(6,12,18)) * 60.0 
    len_meal <- 30
    IC0 <- c(M_Kgut = 4.375,
                M_Kplas = 18.92818,
                M_Kinter = 42.06262,
                M_Kmuscle = 3103.76386)

    vals <- end_50daysim(IC0, 30, MealTimes, Kamt, params)

    return(vals)
}

main_sim_conc <- function(params){
    vals <- main_sim(params)
    end_Kplas <- vals[1]/params['V_plasma']
    end_Kmusc <- vals[2]/params['V_muscle']
    return(c(end_Kplas, end_Kmusc))
}

Kplas_50days_MA <- function(X) {
    #print(class(X))
    #print(X)
    #print(X[1,])
    #temp <- X[1, ]
    #print(temp['kgut'])
    one_par <- function(i){
        pars <- X[i, ]
        vals <- main_sim(pars)
        Vplas <- 4.5 # fixed because not in pars
        end_Kplas <- vals[1] / Vplas
        #end_Kplas <- vals[1] / pars['V_plasma']
        return(end_Kplas) # Kplas
    }
    res_Kplas <- sapply(1:nrow(X), one_par, simplify = TRUE)
    return(res_Kplas)
}

Kmusc_50days_MA <- function(X) {
    one_par <- function(i){
        pars <- X[i, ]
        vals <- main_sim(pars)
        Vmusc <- 24 # fixed because not in pars
        end_Kmusc <- vals[2] / Vmusc
        return(end_Kmusc) # Kmusc
    }
    res_Kmusc <- sapply(1:nrow(X), one_par, simplify = TRUE)
    return(res_Kmusc)
}

# Kplas_50days <- function(pars) {
#     vals <- main_sim(pars)
#     return(vals[1]) # Kplas
# }

# Kmusc_50days <- function(pars) {
#     vals <- main_sim(pars)
#     return(vals[2]) # Kmuscle
# }