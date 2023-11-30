# Set up parameter ranges for Morris Analysis
# TODO

testpars <- c("kgut",
                "V_plasma",
                "V_interstitial",
                "V_muscle",
                "P_ECF",
                "Vmax",
                "Km",
                "GFR_base",
                "eta_ptKreab_base",
                "eta_LoHKreab" ,
                "eta_ptKreab_highK",
                "dtKsec_eq",
                "A_dtKsec",
                "B_dtKsec",
                "cdKsec_eq",
                "A_cdKsec",
                "B_cdKsec",
                "alpha_TGF",
                "A_cdKreab",
                "ALD_eq",
                "m_K_ALDO",
                "FF",
                "A_insulin",
                "B_insulin",
                "alpha_al",
                "beta_al",
                "k",
                "x0")

parsinf <- c(0.5 * p$kgut,
            0.5 * p$V_plasma,
            0.5 * p$V_interstitial,
            0.5 * p$V_muscle,
            0.5 * p$P_ECF,
            0.5 * p$Vmax,
            0.8, # Km Cheng gave range,
            0.100, # GFR_base (100/1000)
            0.5 * p$eta_ptKreab_base,
            0.5 * p$eta_LoHKreab,
            0.5 * p$eta_ptKreab_highK,
            0.5 * p$dtKsec_eq,
            0.5 * p$A_dtKsec,
            0.5 * p$B_dtKsec,
            0.5 * p$cdKsec_eq,
            0.5 * p$A_cdKsec,
            0.5 * p$B_cdKsec,
            0.5 * p$alpha_TGF,
            0.5 * p$A_cdKreab,
            0.5 * p$ALD_eq,
            0.5 * p$m_K_ALDO,
            0.5 * p$FF,
            0.5 * p$A_insulin,
            0.5 * p$B_insulin,
            0.5 * p$alpha_al,
            0.5 * p$beta_al,
            0.5 * p$k,
            0.5 * p$x0
            )


parssup <- c(1.5 * p$kgut,
            1.5 * p$V_plasma,
            1.5 * p$V_interstitial,
            1.5 * p$V_muscle,
            1.5 * p$P_ECF,
            1.5 * p$Vmax,
            1.5, # Km Cheng gave range,
            0.130, # GFR_base (130/1000)
            1.1 * 0.67,
            1.2 * p$eta_LoHKreab,
            1.1 * 0.67, # eta_ptKreab_highK, Up to the largest eta_ptKreab_base
            1.5 * p$dtKsec_eq,
            1.5 * p$A_dtKsec,
            1.5 * p$B_dtKsec,
            1.5 * p$cdKsec_eq,
            1.5 * p$A_cdKsec,
            1.5 * p$B_cdKsec,
            1.5 * p$alpha_TGF,
            1.5 * p$A_cdKreab,
            1.5 * p$ALD_eq,
            1.5 * p$m_K_ALDO,
            1.5 * p$FF,
            1.5 * p$A_insulin,
            1.5 * p$B_insulin,
            1.5 * p$alpha_al,
            1.5 * p$beta_al,
            1.5 * p$k,
            1.5 * p$x0
            )