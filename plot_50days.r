# Plot the results of the 50 days
# get libraries
library(ggplot2)
library(tidyr)
source("setparams.r") # model parameters



# Filename
# TODO: "control", "highK_ptoff", "highK_pttgf", "highK_tgfoff"
pars <- set_params()
notes = "control"
eta_ptKreab = pars$eta_ptKreab_base
alpha_tgf = pars$alpha_TGF


dat_all <- data.frame()


for(ii in 1:50) {
    fname = paste("Sim50Days/",
                today, 
                "_50daysim",
                "_day-", ii,
                "_Kamt-", Kamt,
                "_etaPTKreab-", eta_ptKreab,
                "_alphaTGF-", alpha_tgf,
                "_notes-", notes,
                ".csv",
                sep = "")

    dat <- read.csv(fname)
    dat_all <- rbind(dat_all, dat)
}

# Plot the results

