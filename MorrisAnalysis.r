# libraries and source scripts
library(deSolve)
library(sensitivity)
source('varnames.r')
source('model.r')
source('end_50daysim.r')
source('setparams.r')

varnames <- get_varnames()
# Get ranges for parameters
# get testpars, parsinf, parssup
p <- set_params()
source('set_morris.r')

set.seed(56)

# TO DO: change to 1000
rval = 1000 #5 #1000 #5 #100

start_all <- Sys.time()
print(start_all)

# Plasma K+ concentration effects
print('start Kplas Morris')
x_Kplas <- morris(model = Kplas_50days_MA,
                    factors = testpars,
                    r = rval,
                    design = list(type = 'oat',
                                    levels = 6, 
                                    grid.jump = 1),
                    binf = parsinf,
                    bsup = parssup,
                    scale = TRUE)
end_Kplas <- Sys.time()
print(end_Kplas)
print('end Kplas Morris')
print(difftime(end_Kplas, start_all, units = "mins"))


# # Muscle K+ concentration effects
# print('start Kmusc Morris')
# x_Kmuscle <- morris(model = Kmusc_50days,
#                         factors = testpars,
#                         r = rval,
#                         design = list(type = 'oat',
#                                         levels = 6, 
#                                         grid.jump = 1),
#                         binf = parsbinf,
#                         bsup = parsbsup,
#                         x = x_Kplas$X, # use X from previous function
#                         scale = TRUE)
# print(Sys.time())
# print('end Kmusc Morris')
# print(difftime(Sys.time(), end_Kplas, units = "mins"))


# Analysis complete
print('Morris analysis complete')
print(difftime(Sys.time(), start_all, units = "mins"))

save_info = 1
if (save_info) {
    today <- Sys.Date()
    fname = paste(today,
                    "_MorrisAnalysis_50days",
                    ".RData",
                    sep = "")
    save.image(fname)
    print("results saved to:")
    print(sprintf("%s", fname))
}
