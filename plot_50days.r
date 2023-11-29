# Plot the results of the 50 days
# get libraries
library(ggplot2)
library(tidyr)
source("setparams.r") # model parameters


# Filename
# TODO: "control", "highK_ptoff", "highK_pttgf", "highK_ptonly"
pars <- set_params()
notes = "highK_ptoff"
eta_ptKreab = pars$eta_ptKreab_highK
alpha_tgf = 0 #pars$alpha_TGF
Kamt = 4*78/3


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
p <- ggplot(dat_all, aes(x = time, y = M_Kplas))+ # Create a plot with the x-axis and y-axis variables
    geom_line() + # Add a line
    labs(x = "Time", y = "MKplasma") + # Add axis labels
    ggtitle("Plot of time and value from lsoda output") # Add title

fname = paste("MKplasma_",
                "notes-", notes,
                ".png",
                sep = "")
ggsave(fname, p)

endvals <- dat_all[nrow(dat_all), ]
print(endvals)

print(sprintf('End K_plas: %0.3f', endvals$M_Kplas/pars$V_plasma))
print(sprintf('End K_muscle: %0.3f', endvals$M_Kmuscle/pars$V_muscle))


