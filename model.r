model <- function(Time, State, Pars, opts){
    # This function runs the 
    # Time -- time 
    # State -- state variables (M_Kgut, M_Kplas, M_Kinter, M_Kmuscle)
    # Pars -- parameters

    meal_start <- opts$meal_start
    Kintake <- opts$Kintake

    with(as.list(c(State, Pars)), {
        # set parameters that are fixed (not in Morris Analysis)

        # Get insulin levels
        t_insulin = Time - meal_start # this is going to have to be like this I think...
        # C_insulin units are in nanomole/L
        do_insulin = 1
        if (do_insulin) {
            if (t_insulin <= 0) {
                C_insulin <- 22.6/1000
            } else if ((t_insulin > 0) & (t_insulin < 1.5*60)) {
                C_insulin <- ((325 - 22.6)/(1.5*60)*(t_insulin) + 22.6)/1000
            } else if ((t_insulin >= 1.5*60) & (t_insulin < 6*60)) {
                C_insulin <- ((22.6-325)/((6-1.5)*60)*(t_insulin - 6*60)
                                    + 22.6)/1000
            } else if (t_insulin >= 6*60) {
                C_insulin <- 22.6/1000
            } else {
                print("something went wrong with t_insulin")
            }
            L = 100
            x0 = 0.5381
            k = 1.069
            ins_A = A_insulin
            ins_B = 100 * B_insulin
            temp = (ins_A*(L/(1+exp(-k*(log10(C_insulin)
                    -log10(x0)))))+ ins_B)/100
            rho_insulin = max(1.0, temp)
        }

        # concentrations
        K_plas    <- M_Kplas/V_plasma # plasma K concentration
        K_inter   <- M_Kinter/V_interstitial # interstitial K concentration
        K_muscle  <- M_Kmuscle/V_muscle # intracellular K concentration
        K_ECFtot  <- (M_Kplas + M_Kinter)/(V_plasma + V_interstitial) # total ECF concentration

        # ALD (N_al)
        N_al = exp(m_K_ALDO * (K_ECFtot - Kecf_total))
        C_al = N_al * ALD_eq



        # Gut K (M_Kgut)
        Phi_Kin = Kintake
        Intake2gut = (1 - fecal_excretion) * Phi_Kin
        Gut2plasma = kgut * M_Kgut

        dgut <- Intake2gut - Gut2plasma

        # plasma K (M_Kplas)
        Plas2ECF = P_ECF * (K_plas - K_inter)

        # ALD impact
        gamma_al = A_dtKsec * C_al^B_dtKsec
        lambda_al = A_cdKsec * C_al^B_cdKsec

        # GI FF effect
        gamma_Kin = max(1, FF*(M_Kgut - MKgutSS))

        # Renal K handling

        # baseline proximal segment reabsorption
        eta_psKreab_base = eta_ptKreab_base + eta_LoHKreab

        # PT high K intake effect
        eta_psKreab = eta_ptKreab_highK + eta_LoHKreab
        # TGF effect
        GFR <- GFR_base + alpha_TGF * (eta_psKreab - eta_psKreab_base)

        # filtration
        filK = GFR * K_plas

        # Proximal segment net reabsorption
        psKreab = eta_psKreab * filK

        # Distal tubule
        eta_dtKsec = gamma_al * gamma_Kin
        dtKsec = dtKsec_eq * eta_dtKsec

        # Collecting duct
        eta_cdKsec = lambda_al
        cdKsec = cdKsec_eq * eta_cdKsec

        dtK = filK - dsKreab + dtKsec # flow from distal tubule
        cdKreab = dtK * A_cdKreab

        # Urine
        UrineK <- dtK + cdKsec - cdKreab

        dplas <- Gut2plasma - Plas2ECF - UrineK

        # Interstitial K (M_Kinter)
        # TO DO!




# dydt(2) = Gut2plasma - Plas2ECF - UrineK;

# %% Interstitial K (M_Kinter)
# rho_al = (66.4 + 0.273*C_al)./89.6050;
# % insulin
# L = 100; x0 = 0.5381; k = 1.069;
# ins_A = A_insulin; ins_B = 100*B_insulin;
# temp = (ins_A.*(L./(1+exp(-k.*(log10(C_insulin)-log10(x0)))))+ ins_B)./100;
# if do_insulin
#     rho_insulin = max(1.0,temp);
#     %disp(C_insulin)
#     %disp(temp)
#     %disp(rho_insulin)
# else
#     rho_insulin = 1;
# end
# eta_NKA = rho_insulin * rho_al;

# Inter2Muscle = eta_NKA* ((Vmax * K_inter)/(Km + K_inter));
# Muscle2Inter = P_muscle*(K_muscle - K_inter);

# dydt(3) = Plas2ECF - Inter2Muscle + Muscle2Inter;

# %% Intracellular K (M_Kmuscle)
# dydt(4) = Inter2Muscle - Muscle2Inter;

    # return the derviatives
        return(list(c(dgut, dplas, dinter, dmuscle)))        
    }) # end of with


} # end of function