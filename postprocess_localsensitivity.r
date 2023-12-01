# Postprocessing script for local sensitivity analysis
fname <- "2023-11-30_LocalAnalysis_50days.RData"

load(fname)

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

today <- "2023-11-30" #Sys.Date()
fname_root = paste(today,
                "_LocalAnalysis_50days",
                sep = "")
fname = paste(fname_root,
                ".csv",
                sep = "")

write.csv(df, fname, row.names=FALSE)

base_vals_df <- data.frame(
                    K_plasma = base_vals[1],
                    K_musc = base_vals[2]
                    )

fname = paste(fname_root,
                "_base_vals",
                ".csv",
                sep = "")
write.csv(base_vals_df, fname, row.names = FALSE)


# Compute the percent change in increase and decrease for
# Kplas and Kmuscle


