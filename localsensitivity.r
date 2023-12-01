# This script will run a local sensitivity analysis
# using all the parameters and changing by a given percent
# then measuring the plasma and intracellular [K+]
library(deSolve)
source('varnames.r')
source('setparams.r')
source('model.r')
source('end_50daysim.r')

# Percent change
per_change <- 0.1

# Baseline parameters
pars_base <- unlist(set_params())

# Variable names
varnames <- get_varnames()

# Baseline values
base_vals <- main_sim_conc(pars_base)

# Initialize empty array
vals_inc_all <- array(NA, dim = c(length(pars_base), 2))
vals_dec_all <- array(NA, dim = c(length(pars_base), 2))

start_all <- Sys.time()
print(start_all)
for (ii in 1:length(pars_base)){
    print(ii)
    # initialize parameter set
    pars <- pars_base

    # increase parameter ii by per_change
    pars[ii] <- (1 + per_change) * pars_base[ii]

    vals_inc <- main_sim_conc(pars)

    vals_inc_all[ii, ] <- vals_inc

    # decrease parameter ii by per_change
    pars[ii] <- (1 - per_change) * pars_base[ii]

    vals_dec <- main_sim_conc(pars)

    vals_dec_all[ii, ] <- vals_dec
    if (ii%%4 == 0) {
        print(Sys.time())
    }
}
print('end local sensitivity')
print(difftime(Sys.time(), start_all, units = "mins"))

# Make data frame
ParNames <- names(pars_base)

save_info = 1
if (save_info) {
    today <- Sys.Date()
    fname_root = paste(today,
                    "_LocalAnalysis_50days",
                    sep = "")
    fname = paste(fname_root,
                    ".RData",
                    sep = "")
    save.image(fname)
    print("results saved to:")
    print(sprintf("%s", fname))

    # Make a dataframe
    df <- data.frame(
    ParNames = ParNames,
    Increase = vals_inc_all,
    Decrease = vals_dec_all
    )

    colnames(df) <- c("ParameterName",
                        "Kplas_Increase",
                        "Kmusc_Increase",
                        "Kplas_Decrease",
                        "Kmusc_Decrease") 
    fname = paste(fname_root,
                    ".csv",
                    sep = "")

    write.csv(df, fname, row.names=FALSE)
}