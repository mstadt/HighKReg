model_Kreg <- function(Time, State, Pars, opts){
    # This function runs the 
    # Time -- time 
    # State -- state variables (M_Kgut, M_Kplas, M_Kinter, M_Kmuscle)
    # Pars -- parameters

    meal_start <- opts$meal_start
    Kintake <- opts$Kintake
    SS <- opts$SS

    with(as.list(c(State, Pars)), {
        # set parameters that are fixed (not in Morris Analysis)
        V_plasma = 4.5
        V_interstitial = 10
        V_muscle = 24
        P_ECF = 0.3
        Vmax = 130
        Km = 1.4
        cdKsec_eq = 0.0022
        A_cdKsec = 0.161275
        B_cdKsec = 0.410711
        k = 1.069
        x0 = 0.5381
        Phi_Kin_ss = 0.04861111
        t_insulin_ss = 270
        fecal_excretion = 0.1
        MKgutSS <- (0.9 * Phi_Kin_ss) / kgut
        Kecf_total = 4.2
        Kmuscle_baseline = 130
        NKAbase <- (Vmax * Kecf_total)/(Km + Kecf_total)
        P_muscle <- NKAbase / (Kmuscle_baseline - Kecf_total)

        if (SS) {
            Kintake <- Phi_Kin_ss # set Kintake to ss if at SS
        }
    
        # Get insulin levels
        if (SS) {
            t_insulin = -10
        } else {
            t_insulin = Time - meal_start 
        }
        
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
            # x0 = 0.5381 # NOTE: added to parameters so can use in MA
            # k = 1.069 # NOTE: added to parameters so can use in MA
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
        #rho_al = (66.4 + 0.273*C_al)/89.6050 # IDEA: change to alpha_al, beta_al
        rho_al = alpha_al + beta_al * C_al # alpha_al = 66.4/89.605

        # GI FF effect
        gamma_Kin = max(1, FF*(M_Kgut - MKgutSS))

        # Renal K handling

        # baseline proximal segment reabsorption
        eta_psKreab_base = eta_ptKreab_base + eta_LoHKreab

        
        if (SS) {
            # no high K effect at SS
            eta_psKreab = eta_ptKreab_base + eta_LoHKreab
            alpha_TGF = 0
        } else {
            # PT high K intake effect
            eta_psKreab = eta_ptKreab_highK + eta_LoHKreab
        }
        
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

        dtK = max(0.0, filK - psKreab + dtKsec) # flow from distal tubule
        cdKreab = dtK * A_cdKreab

        # Urine
        UrineK <- max(dtK + cdKsec - cdKreab, 0.01*filK)

        dplas <- Gut2plasma - Plas2ECF - UrineK

        # Interstitial K (M_Kinter)
        eta_NKA = rho_insulin * rho_al

        Inter2Muscle = eta_NKA * ((Vmax * K_inter)/(Km + K_inter))
        Muscle2Inter = P_muscle * (K_muscle - K_inter)

        dinter <- Plas2ECF - Inter2Muscle + Muscle2Inter
        
        # Intracellular K (M_Kmuscle)
        dmuscle <- Inter2Muscle - Muscle2Inter

        # return the derviatives
        return(list(c(dgut, dplas, dinter, dmuscle)))        
    }) # end of with


} # end of function