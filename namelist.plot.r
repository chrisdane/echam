# r

# input for plot_echam.r

# load defaults
repopath <- "~/scripts/r/echam"
repopath <- normalizePath(repopath, mustWork=T) # error if not found
source(paste0(repopath, "/namelist.general.plot.r"))
workpath <- host$workpath

# 1 setting
if (F) { # gregor_and_fay_2021
    do_aggregate_plot_data <- T
    if (F) { # fgco2
        if (F) { # group by wind-product
            prefixes <- c("wind_CCMP2_pco2_CMEMS_FFNN", "wind_CCMP2_pco2_CSIR_ML6", "wind_CCMP2_pco2_JENA_MLS", 
                          "wind_CCMP2_pco2_JMA_MLR", "wind_CCMP2_pco2_MPI_SOMFFN", "wind_CCMP2_pco2_NIES_FNN", 
                          "wind_ERA5_pco2_CMEMS_FFNN", "wind_ERA5_pco2_CSIR_ML6", "wind_ERA5_pco2_JENA_MLS", 
                          "wind_ERA5_pco2_JMA_MLR", "wind_ERA5_pco2_MPI_SOMFFN", "wind_ERA5_pco2_NIES_FNN", 
                          "wind_JRA55_pco2_CMEMS_FFNN", "wind_JRA55_pco2_CSIR_ML6", "wind_JRA55_pco2_JENA_MLS", 
                          "wind_JRA55_pco2_JMA_MLR", "wind_JRA55_pco2_MPI_SOMFFN", "wind_JRA55_pco2_NIES_FNN", 
                          "wind_NCEP1_pco2_CMEMS_FFNN", "wind_NCEP1_pco2_CSIR_ML6", "wind_NCEP1_pco2_JENA_MLS", 
                          "wind_NCEP1_pco2_JMA_MLR", "wind_NCEP1_pco2_MPI_SOMFFN", "wind_NCEP1_pco2_NIES_FNN", 
                          "wind_NCEP2_pco2_CMEMS_FFNN", "wind_NCEP2_pco2_CSIR_ML6", "wind_NCEP2_pco2_JENA_MLS", 
                          "wind_NCEP2_pco2_JMA_MLR", "wind_NCEP2_pco2_MPI_SOMFFN", "wind_NCEP2_pco2_NIES_FNN")
        } else if (T) { # group by pco2-product
            prefixes <- c("wind_CCMP2_pco2_CMEMS_FFNN", "wind_ERA5_pco2_CMEMS_FFNN", "wind_JRA55_pco2_CMEMS_FFNN", 
                          "wind_NCEP1_pco2_CMEMS_FFNN", "wind_NCEP2_pco2_CMEMS_FFNN",  "wind_CCMP2_pco2_CSIR_ML6", 
                          "wind_ERA5_pco2_CSIR_ML6", "wind_JRA55_pco2_CSIR_ML6", "wind_NCEP1_pco2_CSIR_ML6", 
                          "wind_NCEP2_pco2_CSIR_ML6", "wind_CCMP2_pco2_JENA_MLS", "wind_ERA5_pco2_JENA_MLS", 
                          "wind_JRA55_pco2_JENA_MLS", "wind_NCEP1_pco2_JENA_MLS", "wind_NCEP2_pco2_JENA_MLS", 
                          "wind_CCMP2_pco2_JMA_MLR", "wind_ERA5_pco2_JMA_MLR", "wind_JRA55_pco2_JMA_MLR", 
                          "wind_NCEP1_pco2_JMA_MLR", "wind_NCEP2_pco2_JMA_MLR", "wind_CCMP2_pco2_MPI_SOMFFN", 
                          "wind_ERA5_pco2_MPI_SOMFFN", "wind_JRA55_pco2_MPI_SOMFFN", "wind_NCEP1_pco2_MPI_SOMFFN", 
                          "wind_NCEP2_pco2_MPI_SOMFFN", "wind_CCMP2_pco2_NIES_FNN", "wind_ERA5_pco2_NIES_FNN", 
                          "wind_JRA55_pco2_NIES_FNN", "wind_NCEP1_pco2_NIES_FNN", "wind_NCEP2_pco2_NIES_FNN")
        }
        if (T) { # remove NCEP*
            inds <- which(grepl("NCEP", prefixes))
            if (length(inds) > 0) prefixes <- prefixes[-inds]
        }
        varnamesin <- rep("fgco2", t=length(prefixes))
        modes <- rep("fldint", t=length(prefixes))
        cols <- seq_along(prefixes) 
        cols[which(grepl("CMEMS_FFNN", prefixes))] <- 1 # same color for pco2-products
        cols[which(grepl("CSIR_ML6", prefixes))] <- 2
        cols[which(grepl("JENA_MLS", prefixes))] <- 3
        cols[which(grepl("JMA_MLR", prefixes))] <- 4
        cols[which(grepl("MPI_SOMFFN", prefixes))] <- 5
        cols[which(grepl("NIES_FNN", prefixes))] <- 6
        ltys <- rep(1, t=length(prefixes))
        ltys[which(grepl("CCMP2", prefixes))] <- 1 # same lwd for wind-products
        ltys[which(grepl("ERA5", prefixes))] <- 2
        ltys[which(grepl("JRA55", prefixes))] <- 3
        ltys[which(grepl("NCEP", prefixes))] <- 4
        ltys[which(grepl("NCEP2", prefixes))] <- 5
        if (T) { # add aggregated stats
            lwds <- c(rep(1, t=length(prefixes)), 2, 1)
            prefixes <- c(prefixes, "aggregate_18models", "aggregate_18models") # 18: without NCEP*
            varnamesin <- c(varnamesin, "fgco2_mon_mean", "fgco2_mon_median")
            modes <- c(modes, "fldint", "fldint")
            varnames_out_samedims <- "fgco2"
            cols <- c(cols+1, 1, 1)
            ltys <- c(ltys, 1, 1)
        }
        fromsf <- rep(1990, t=length(prefixes))
    } else if (F) { # spco2
        if (T) {
            prefixes <- c("CMEMS_FFNN", "CSIR_ML6", "JENA_MLS", "JMA_MLR", "MPI_SOMFFN", "NIES_FNN")
            varnamesin <- rep("spco2", t=length(prefixes))
        }
        modes <- rep("fldmean", t=length(prefixes))
        cols <- seq_along(prefixes)
        if (T) { # add aggregated stats
            lwds <- c(rep(1, t=length(prefixes)), 2, 1)
            prefixes <- c(prefixes, "aggregate_6models", "aggregate_6models")
            varnamesin <- c(varnamesin, "spco2_mon_mean", "spco2_mon_median")
            modes <- c(modes, "fldmean", "fldmean")
            varnames_out_samedims <- "spco2"
            cols <- c(cols+1, 1, 1)
        }
        fromsf <- rep(1982, t=length(prefixes))
        fromsp <- rep(1990, t=length(prefixes))
    } else if (T) { # dpco2
        prefixes <- c("CMEMS_FFNN", "CSIR_ML6", "JENA_MLS", "JMA_MLR", "MPI_SOMFFN", "NIES_FNN")
        varnamesin <- rep("dpco2", t=length(prefixes))
        modes <- rep("fldmean", t=length(prefixes))
        fromsf <- rep(1982, t=length(prefixes))
        tosf <- rep(2020, t=length(prefixes))
    }
    models <- rep("gregor_and_fay_2021", t=length(prefixes))
    names_short <- prefixes
    names_short <- gsub("wind_", "", names_short) # shorten names in case of fgco2
    names_short <- gsub("_pco2_", " ", names_short)
    names_legend <- names_short
    names_legend <- gsub("_", "-", names_legend)
    names_legend_samedims <- names_legend
    names_legend_samedims[which(grepl("aggregate", prefixes))] <- c("GF21 mean", "GF21 median")
    #areas <- rep("g19_NH-HL", t=length(prefixes))
    #areas <- rep("g19_NH-ST", t=length(prefixes))
    #areas <- rep("g19_EQU", t=length(prefixes))
    #areas <- rep("g19_SH-ST", t=length(prefixes))
    areas <- rep("g19_SH-HL", t=length(prefixes))
    plotprefix <- "gregor_and_fay_2021"
    detrend_ts <- F

} else if (F) { # mhw composite data/seas*100
    workpath <- "/work/ba1103/a270073"
    models <- "fesom"
    prefixes <- "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_anom_pcnt_regular_dx0.250_dy0.250"
    names_short <- "awicm1-recom_mhw_composite_anom_pcnt"
    names_legend <- "Events/Climatology*100"
    #varnamesin <- "mlotst"
    #varnamesin <- "omldamax"
    #varnamesin <- "tau"
    #varnamesin <- "curltau"
    varnamesin <- "ekmanP_ms"
    modes <- "timmean"
    fromsf <- 2842
    tosf <- ""
    seasonsf <- "Jul"

} else if (F) { # en4
    models <- prefixes <- "EN.4.2.2"
    names_short <- names_legend <- "EN4.2.2"
    #varnamesin <- "temperature"
    #varnamesin <- "salinity"
    varnamesin <- "potdens_anomaly"
    modes <- "fldmean"
    fromsf <- 1982
    tosf <- 2019
    depth_fromsp <- 0
    depth_tosp <- 500
    remove_mean_froms <- 1982
    remove_mean_tos <- 2019
    
} else if (F) { # reccap AmC minus DmB
    workpath <- "/work/ollie/cdanek"
    models <- "fesom"
    names_legend <- "(A-C) - (D-B)"
    prefixes <- "reccap_AmC_minus_DmB"
    names_short <- prefixes
    varnamesin <- "CO2f"
    modes <- "fldint"
    fromsf <- 1958
    tosf <- 2019

} else if (F) { # awi-esm-1-1-lr_kh800 piControl chunks 1,2 (og), 3 (my)
    models <- "echam6"
    #models <- "jsbach"
    #models <- "fesom"
    #prefixes <- "awi-esm-1-1-lr_kh800_piControl_og"
    #prefixes <- "awi-esm-1-1-lr_kh800_piControl_og_regular_dx0.250_dy0.250"
    prefixes <- "awi-esm-1-1-lr_kh800_piControl"
    #names_short <- "ar1_piControl_og"
    names_short <- "ar1_piControl"
    names_legend <- "piControl"
    #names_legend <- "AWICM1-RECOM piControl"
    #prefixes <- "awi-esm-1-1-lr_kh800_piControl_og_regular_dx0.250_dy0.250_minus_woa2018_dx0.250_dy0.250_t_an_0m"
    #names_short <- "piControl_minus_woa18"
    #names_legend <- "piControl minus WOA18"
    # echam echamstream
    #varnamesin <- "temp2"
    # echam co2stream
    #varnamesin <- "co2_flux"
    varnamesin <- "co2_flx_ocean"
    areas <- "SO45S"
    #echam6_global_setNA <- "land"
    #varnamesin <- "co2_flx_land" # = npp + resp + herb + fire
    #echam6_global_setNA <- "ocean"
    #addland <- F
    #varnamesin <- "co2_flx_npp"
    #varnamesin <- "co2_flx_resp"
    #varnamesin <- "co2_flx_herb"
    #varnamesin <- "co2_flx_lcc"
    #varnamesin <- "co2_flx_harvest"
    #varnamesin <- "co2_flx_fire"
    #varnamesin <- "nbp" # = co2_flx_land + co2_flx_lcc + co2_flx_harvest
    # jsbach jsbachstream
    #varnamesin <- "CO2_flux_net"
    #varnamesin <- "CO2_flux_dynveg"
    # jsbach yassostream
    #varnamesin <- "soilSlow" # = vertsum(boxYC_humus_1) + vertsum(boxYC_humus_2)
    # fesom
    #varnamesin <- "thetaoga"
    #varnamesin <- "tos"
    #varnamesin <- "thetao"
    #depths <- "0-5900"
    # recom
    #varnamesin <- "bgc03"
    #depths <- "0"
    #varnamesin <- "CO2f"
    #varnamesin <- "NPP"
    #modes <- "select"
    #modes <- "timmean"
    #modes <- "fldmean"
    modes <- "fldint"
    #modes <- "depth"
    #seasonsf <- "annual"
    #fromsf <- 1950 # chunk 1 start: 1950
    fromsf <- 2686 # chunk 3 start: 2686
    #tosf <- 2685 # chunk 1 end: 2585
    #tosf <- 2850
    #tosf <- 2905
    #tosf <- 2995
    #tosf <- 2996
    tosf <- 3000 # chunk 3 end: 3000
    #fromsp <- 948
    tunit <- "model year"
    new_origins <- fromsf - 1950 + 1 # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    #fromsp <- 1051+1-200 # last xxx years of piControl
    add_linear_trend <- F
    #add_linear_trend_froms <- 857
    #remove_mean_froms <- c(1, 1)
    #remove_mean_tos <- remove_mean_froms

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl
    #models <- "echam6"
    #models <- "fesom"
    models <- "recom"
    if (F) {
        #prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_2percatm"
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_2percatm_regular_dx0.250_dy0.250"
        names_short <- "esm_piControl_2percatm"
        names_legend <- "esm-piControl_2percatm"
        #names_legend <- "piControl and esm-piControl"
    } else if (F) {
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_co2fsign"
        #prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_co2fsign_regular_dx0.250_dy0.250"
        names_short <- "esm_piControl_2percboth_co2fsign"
        names_legend <- "esm-piControl_2percboth_co2fsign"
    } else if (F) {
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_restartall"
        #prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_co2fsign_regular_dx0.250_dy0.250"
        names_short <- "esm_piControl_restartall"
        names_legend <- "esm-piControl"
    } else if (F) {
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl"
        names_short <- "esm_piControl"
        names_legend <- "esm-piControl"
    } else if (T) {
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2"
        names_short <- "esm-piControl_wout_talk_rest2"
        names_legend <- "esm-piControl"
    }
    if (T) prefixes <- paste0(prefixes, "_regular_dx1.000_dy1.000")
    #varnamesin <- "temp2"
    #varnamesin <- "CO2"
    #varnamesin <- "co2_flux"
    #varnamesin <- "co2_flx_ocean"
    #varnamesin <- "co2_flx_land"
    #varnamesin <- "co2_flx_lcc"
    #varnamesin <- "co2_burden_corr_acc2"
    #varnamesin <- "nbp"
    #varnamesin <- "co2_flx_total"
    #varnamesin <- "thetaoga"
    #varnamesin <- "tos"
    #varnamesin <- "thetao"
    #varnamesin <- "sos"
    #varnamesin <- "siarean"
    #varnamesin <- "siareas"
    #varnamesin <- "aCO2"
    #varnamesin <- "CO2f"
    #varnamesin <- "pCO2s"
    #varnamesin <- "dpCO2s"
    #varnamesin <- "bgc02"
    #varnamesin <- "bgc03"
    #varnamesin <- "diags3d01"
    #varnamesin <- "diags3d02"
    varnamesin <- "NPPtot"
    #depths <- 0
    #depths <- "0-5900"
    #depths <- "0-5900"
    depthsf <- "_int0-5900m"
    #depth_fromsp <- -50
    #depth_fromsp <- -250
    #depth_fromsp <- -500
    #echam6_global_setNA <- "ocean"
    #echam6_global_setNA <- "land"
    addland <- F
    seasonsf <- "annual"
    #fromsf <- 2586
    #fromsf <- 2686
    #fromsf <- 2817
    #fromsf <- 3001
    fromsf <- 3208
    #tosf <- 2689
    #tosf <- 2703
    #tosf <- 2704
    #tosf <- 2760
    #tosf <- 2916
    #tosf <- 3208
    #tosf <- 3112
    tosf <- 3264
    #tosf <- 3497
    #tosf <- 3574
    #tosf <- 3590
    #tosf <- 3859
    #tosf <- 3945
    #tunit <- "model year"
    tunit <- "esm-piControl year"
    #new_origins <- 1 # esm-piControl
    #new_origins <- fromsf - 2686 + 1 # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    new_origins <- 208
    #fromsp <- 700
    #remove_mean_froms <- c(1, 1)
    #remove_mean_tos <- remove_mean_froms
    #n_mas_an <- 3
    #n_mas_an <- 5
    #n_mas_an <- 10
    #modes <- "select"
    modes <- "timmean"
    #modes <- "fldmean"
    #modes <- "fldint"
    #modes <- "depth"
    #add_linear_trend <- T

} else if (F) { # awi-esm-1-1-lr_kh800 historical historical2
    models <- "echam6"
    #odels <- "jsbach"
    #models <- "fesom"
    #prefixes <- "awi-esm-1-1-lr_kh800_historical"
    #prefixes <- "awi-esm-1-1-lr_kh800_historical2"
    prefixes <- "awi-esm-1-1-lr_kh800_historical_and_ssp126"
    #names_short <- "awi-esm-1-1-lr_kh800_historical"
    names_short <- "awi-esm-1-1-lr_kh800_historical_ssp126"
    #names_legend <- "historical"
    names_legend <- "AWI-ESM-1-1-REcoM-LR"
    varnamesin <- "co2_flx_ocean"
    echam6_global_setNA <- "land"
    addland <- F
    #center_ts <- T
    #detrend_ts <- T
    diff_ts <- T
    cols <- 2
    #varnamesin <- "co2_flx_land"
    #varnamesin <- "tos"
    fromsf <- 1850
    #fromsf <- 1887
    #tosf <- 1906
    tosf <- 2100
    fromsp <- 1990
    tosp <- 2019
    n_mas_an <- 3
    #modes <- "timmean"
    #modes <- "fldmean"
    modes <- "fldint"

} else if (F) { # awi-esm-1-1-lr piControl
    workpath <- "/work/ab0246/a270073"
    models <- "echam6"
    prefixes <- "awi-esm-1-1-lr_piControl"
    names_short <- "awi-esm-1-1-lr_piControl"
    names_legend <- "AWI-ESM-1-1-LR piControl"
    varnamesin <- "co2_flx_ocean"
    modes <- "fldint"
    fromsf <- 1955
    tosf <- 2119
    new_origins <- 1850

} else if (F) { # awi-esm-1-1-lr hist
    #prefixes <- "historical_echam6_echammon_awi-esm-1-1-lr"
    prefixes <- "historical_echam6_echammon_yearmean_awi-esm-1-1-lr"
    models <- "echam6"
    names_short <- "historical_wrt_1961_90"
    names_legend <- "historical"
    fromsf <- 1850
    tosf <- 2014
    varnamesin <- "temp2"
    modes <- "fldmean"
    cols <- "#E41A1C"
    remove_mean_froms <- 1961
    remove_mean_tos <- 1990

} else if (F) { # awi-esm-1-1-lr ssp534-over wrong restart vs ssp585 
    models <- rep("echam6", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_ssp534-over", "awi-esm-1-1-lr_kh800_ssp585")
    names_short <- prefixes
    names_legend <- c("ssp534-os from hist", "ssp585")
    fromsf <- rep(2015, t=2)
    tosf <- rep(2040, t=2)
    #tosf <- rep(2100, t=2)
    #tosp <- rep(2040, t=2)
    varnamesin <- rep("temp2", t=2)
    modes <- rep("timmean", t=2)
    #modes <- rep("fldmean", t=2)

} else if (F) { # Hol-Tx10 on paleosrv or Hol-T on stan
    models <- "echam5"
    #models <- "mpiom1"
    #models <- "jsbach"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_main_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_main_mm_plev"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_wiso_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-T"
    #prefixes <- "cosmos-aso-wiso_Hol-T_main_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-T_main_mm_significance_0.01"
    #prefixes <- "cosmos-aso-wiso_Hol-T_wiso_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-T_wiso_mm_significance_0.01"
    #prefixes <- "Hol-T_stschuett_echam5_wiso" # steffens data
    #prefixes <- "cosmos-aso-wiso_Hol-T_grb_code_2_remapcon2_r120x101"
    #prefixes <- "cosmos-aso-wiso_Hol-T_grb_code_15_remapcon2_r120x101_gt_0.15_times_area"
    #prefixes <- "cosmos-aso-wiso_Hol-T_veg_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-T_fort_75_monmean"
    addland <- F
    #names_short <- "Hol-Tx10"
    names_short <- "Hol-T"
    #names_short <- "Hol-T_st"
    #names_legend <- names_short
    #names_legend <- "Hol-Tx10 DJF"
    #names_legend <- "Hol-Tx10 MAM"
    #names_legend <- "Hol-Tx10 JJA"
    #names_legend <- "Hol-Tx10 SON"
    names_legend <- ""
    #names_legend <- "Annual"
    #names_legend <- "global"
    #names_legend <- "60-90N"
    #names_legend <- "Hol-Tx10 Ladoga"
    #names_legend <- "Hol-T Ladoga"
    #names_legend <- "cosmos-aso-wiso Ladoga"
    #names_legend <- "COSMOS transient"
    #names_legend <- "transient"
    #names_legend <- "Hol-Tx10 Shuchye"
    #names_legend <- "Hol-T Shuchye"
    #names_legend <- "Hol-Tx10 Levinson-Lessing"
    #names_legend <- "Hol-T Levinson-Lessing"
    #names_legend <- "Hol-Tx10 Taymyr"
    #names_legend <- "Hol-T Taymyr"
    #names_legend <- "Hol-Tx10 Emanda"
    #names_legend <- "Hol-T Emanda"
    #names_legend <- "Hol-Tx10 Elgygytgyn"
    #names_legend <- "Hol-T Elgygytgyn"
    #fromsf <- "0001" # Hol-Tx10
    fromsf <- "0004" # Hol-T; beginning of chunk 1
    #fromsf <- "0100"
    #fromsf <- "6971" # last 30 years of Hol-T
    #tosf <- "0129"
    #tosf <- "5903" # Hol-T; end of chunk 2
    #tosf <- "6173" # 
    #tosf <- "6821" # 
    tosf <- "7000" # Hol-T; end of chunk 3
    #tosf <- "7001" # Hol-Tx10
    #new_origins <- -7000 # Hol-Tx10
    #new_origins <- -5050 # Hol-Tx10 plus 1950
    new_origins <- -6996 # Hol-T; model year 1 = 6999 BP -> model year 4 = 6999 BP - 3 = 6996 BP
    #new_origins <- -29 # last 30 years before 1950
    time_ref <- 1950 # any string, e.g. "BP", or number
    #n_mas_an <- 5
    n_mas_an <- 10
    #remove_mean_froms <- -827
    #remove_mean_tos <- -827
    #remove_mean_froms <- -6996
    #remove_mean_tos <- -6996
    #remove_mean_froms <- -7000
    #remove_mean_tos <- -7000
    seasonsf <- "annual"
    #seasonsf <- "Jun"
    #seasonsf <- "Jun"
    #seasonsf <- "Dec"
    #seasonsf <- "yearsum" # should this be kept?
    #seasonsf <- "NDJFM"
    #seasonsp <- "Feb" # cdo's default season timestamps: DJF->Feb, MAM->May, JJA->Aug, SON->Nov
    #seasonsp <- "Mar"
    #seasonsp <- "May"
    #seasonsp <- "Jun"
    #seasonsp <- "Jul"
    #seasonsp <- "Aug"
    #seasonsp <- "Sep"
    #seasonsp <- "Nov"
    #seasonsp <- "Dec"
    #seasonsp <- "DJF"
    #seasonsp <- "MAM"
    #seasonsp <- "JJA"
    #seasonsp <- "SON"
    #seasonsp <- "NDJFMmean"
    #varnamesin <- "temp2"
    #varnamesin <- "tsurf"
    #varnamesin <- "tslm1"
    #varnamesin <- "psl"
    #varnamesin <- "aprt"
    #varnamesin <- "aprs"
    #varnamesin <- "wisoaprt_d"
    #varnamesin <- "wisoaprt_d_post"
    #levs <- 2
    #varnamesin <- "temp2aprt"
    #varnamesin <- "tsurfaprt"
    #varnamesin <- "ptemp"
    #varnamesin <- "srad0"
    #varnamesin <- "srad0d"
    #varnamesin <- "lm_psl_as_time_slope"
    #varnamesin <- "lm_temp2_as_time_slope"
    #varnamesin <- "lm_tsurf_as_time_slope"
    #varnamesin <- "lm_aprt_as_time_slope"
    #varnamesin <- "lm_wisoaprt_d_sellevel_2_as_temp2_slope"
    #varnamesin <- "lm_wisoaprt_d_sellevel_2_as_ptemp_slope"
    varnamesin <- "lm_wisoaprt_d_post_as_time_slope"
    levs <- 2
    #varnamesin <- "quv"
    #varnamesin <- "quv_direction"
    #levsf <- "_int1000-100hPa"
    #varnames_out_samedims <- "quv"
    #names_legend_samedims <- c("qu", "qv", "quv")
    #varnames_uv <- list(quv=c(u="qu", v="qv")) # for quiver
    #varnamesin <- "THO"
    #levs <- 6
    #varnamesin <- "SICOMO"
    #varnamesin <- "act_fpc"
    #varnamesin <- "lm_act_fpc_as_time_slope"
    #codes <- 31
    #levsf <- "_sum1-4lev"
    #varnamesin <- "lm_albedo_as_time_slope"
    #varnamesin <- "amoc"
    #codes <- 101
    #modes <- "select"
    #modes <- "timmean"
    #modes <- "fldmean"
    #modes <- "timmean_yearsum"
    #modes <- "yseasmean"
    modes <- "yearsum"
    #modes <- "zonmean"
    #modes <- "vertsum"
    #modes <- "fldsum"
    #areas <- "northern_hemisphere"
    #areas <- "sibiria"
    #areas <- "60-90N"
    #areas <- "ladoga_remapnn"
    #areas <- "shuchye_remapnn"
    #areas <- "levinson-lessing_remapnn"
    #areas <- "taymyr_remapnn"
    #areas <- "emanda_remapnn"
    #areas <- "elgygytgyn_remapnn"
    #areas <- "two-yurts_remapnn"
    #areas <- "kotokel_remapnn"
    #areas <- "moc26.5N"
    #levs <- "-0to-5420m"
    #regboxes <- list(list(regbox="northeast_europe"))
    regboxes <- list(list(regbox="NAsiberia"))

} else if (F) { # mseifert piControl
    models <- rep("fesom", t=3)
    #models <- rep("recom", t=3)
    prefixes <- c(#"awi-esm-1-1-lr_kh800_piControl_LUtrans1850",
                  "awi-esm-1-1-lr_kh800_piControl_LUtrans1850_levante",
                  "mseifert_coccos_piControl_chris_code",
                  "mseifert_coccos_piControl_original_bionotzero"
                  #, "mseifert_coccos_piControl_original_bugfix"
                  )
    names_short <- c(#"piControl_LUtrans1850", 
                     "piControl_LUtrans1850_levante", "piControl_chris_code", "piControl_original_bionotzero"
                     #, "piControl_original_bugfix"
                     )
    names_legend <- names_short
    names_legend <- c(#"piControl default code (old run)",
                      "piControl default code", "piControl default code repeat", "piControl coccos code (off)")
    ltys <- c(#1,
              1, 2, 1
              #, 3
              )
    #varnamesin <- rep("aCO2", t=3)
    varnamesin <- rep("tos", t=3)
    #modes <- rep("select", t=3)
    modes <- rep("fldmean", t=3)
    fromsf <- c(#2951, 
                3001, 3001, 3001
                #, 3001
                )
    tosf <- c(#3062, 
              3010, 3010, 3010
              #, 3009
              )
    new_origins <- c(1052, 1052, 1052)
    tunit <- "piControl-year"
    fromsp <- c(#2990, 
                NA, NA, NA
                #, NA
                )
    tosp <- c(#3000, 
              NA, NA, NA
              #, NA
              )

} else if (F) { # mseifert esm-piControl
    models <- rep("fesom", t=7)
    #models <- rep("recom", t=7)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2",
                  "awi-esm-1-1-lr_kh800_esm-piControl_start3870",
                  "mseifert_coccos_esm-piControl_original",
                  "mseifert_coccos_esm-piControl_original_bionotzero",
                  "mseifert_coccos_esm-piControl_original_start3870",
                  #"mseifert_coccos_esm-piControl_chris_code", # mistake: restore_alkalinity was true
                  "mseifert_coccos_esm-piControl_chris_code_bugfix",
                  "mseifert_coccos_esm-piControl_chris_code_bugfix_orig_fesom_restart")
    names_short <- c("default", "default_start3870", "original", "original_bionotzero", "original_start3870", 
                     #"chris_code", 
                     "chris_code_bugfix", "chris_code_bugfix_orig_fesom_restart")
    names_legend <- paste0("esm-piControl", c("", "_start3870", "_original", "_original_bionotzero", "_original_start3870", 
                                              #"_chris_code", 
                                              "_chris_code_bugfix", "_chris_code_bugfix_orig_fesom_restart"))
    names_legend <- paste0(seq_along(names_legend), ": ", names_legend)
    ltys <- c(1, 2, 1, 1, 1, 2, 3)
    #varnamesin <- rep("aCO2", t=7)
    varnamesin <- rep("tos", t=7)
    #modes <- rep("select", t=7)
    modes <- rep("fldmean", t=7)
    fromsf <- c(3208, 3871, 3879, 3879, 3871, 3871, 3871)
    #tosf <- c(3945, 3878, 3884, 3886, 3879, 3878, 3878) # aco2
    tosf <- c(3945, 3878, 3884, 3886, 3881, 3878, 3878) # tos
    fromsp <- c(3871, NA, NA, NA, NA, NA)
    tosp <- c(3886, NA, NA, NA, NA, NA)
    plotprefix <- "mseifert_coccos_esm-piControl"

} else if (F) { # mseifert esm-piControl
    models <- rep("fesom", t=2)
    #models <- rep("recom", t=2)
    prefixes <- c("mseifert_coccos_esm_piControl_CONTROL",
                  "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2")
    names_short <- c("esm_piControl_CONTROL",
                     "esm-piControl_wout_talk_rest2")
    names_legend <- names_short
    #varnamesin <- rep("siarean", t=2)
    varnamesin <- rep("siareas", t=2)
    #varnamesin <- rep("aCO2", t=2)
    modes <- rep("select", t=2)
    fromsf <- c(3871, 3208)
    tosf <- c(3971, 3945)
    fromsp <- c(NA, 3871)
    tosp <- c(3945, NA)

} else if (F) { # tnagwekar esm-hist
    #models <- rep("fesom", t=2)
    models <- rep("recom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-hist_regular_dx1.000_dy1.000",
                  "tnagwekar_hist_new_regular_dx1.000_dy1.000")
    names_short <- prefixes
    names_legend <- c("esm-hist chris", "esm-hist tanvi")
    #varnamesin <- rep("tos", t=2)
    #varnamesin <- rep("mlotst", t=2)
    #varnamesin <- rep("sic", t=2)
    #varnamesin <- rep("siarean", t=2)
    #varnamesin <- rep("siareas", t=2)
    #varnamesin <- rep("aCO2", t=2)
    varnamesin <- rep("pCO2s", t=2)
    #modes <- rep("select", t=2)
    modes <- rep("timmean", t=2)
    fromsf <- rep(1995, t=2)
    tosf <- rep(2014, t=2)

} else if (F) { # takahashi_etal_2002
    models <- c(rep("gregor_and_fay_2021", t=4), rep("recom", t=4))
    prefixes <- c(rep("ensmean", t=4), rep("awi-esm-1-1-lr_kh800_historical2_regular_dx1.000_dy1.000", t=4))
    modes <- rep(c("select", "ymonmean", "select", "select"), t=2)
    if (F) {
        varnamesin <- rep(c("T02_timmean_th_minus_nt", "T02_timmean_th_minus_nt", "T02_timmean_th_minus_nt", "T02_yearmean_th_minus_nt"), t=2)
        varnames_out_samedims <- "T02_th_minus_nt"
    } else if (T) {
        varnamesin <- rep(c("T02_timmean_th_over_nt", "T02_timmean_th_over_nt", "T02_timmean_th_over_nt", "T02_yearmean_th_over_nt"), t=2)
        varnames_out_samedims <- "T02_th_over_nt"
    }
    names_short <- rep(c("T02_oneyear_timmean", "T02_ymonmean_timmean", "T02_monthly_timmean", "T02_monthly_yearmean"), t=2)
    names_legend <- rep(c("t=1year; mean=ltm", "t=monthly clim.; mean=ltm", "t=monthly; mean=ltm", "t=monthly; mean=annual"), t=2)
    names_legend_samedims <- names_legend
    fromsf <- c(1995, rep(1982, t=3), 1995, rep(1981, t=3))
    tosf <- rep(c("", rep(2014, t=3)), t=2)
    if (T) {
        inds <- c(1, 5)
        models <- models[inds]; prefixes <- prefixes[inds]; modes <- modes[inds]
        varnamesin <- varnamesin[inds]; names_short <- names_short[inds]; names_legend <- names_legend[inds]
        fromsf <- fromsf[inds]; tosf <- tosf[inds]
    }

} else if (F) { # phd stuff 1 setting
    workpath <- "/work/ba0941/a270073"
    models <- "fesom"
    prefixes <- "LSea5_s5_regular_dx0.250_dy0.250"
    names_short <- "Lsea5_s5"
    varnamesin <- "resolutionkm"
    seasonsf <- fromsf <- tosf <- ""
    modes <- "timmean"

# =====================================
# 2 settings
} else if (F) { # bfritzsch reproducibility
    if (T) {
        models <- rep("echam6", t=2)
        prefixes <- c("awi-esm-1-1-lr_kh800_piControl_LUtrans1850_r8",
                      "awi-esm-1-1-lr_kh800_piControl_LUtrans1850_new")
        varnamesin <- rep("temp2", t=2)
    } else if (F) {
        models <- rep("fesom", t=2)
        prefixes <- c("awi-esm-1-1-lr_kh800_piControl_LUtrans1850_r8_regular_dx1.000_dy1.000",
                      "awi-esm-1-1-lr_kh800_piControl_LUtrans1850_new_regular_dx1.000_dy1.000")
        varnamesin <- rep("tos", t=2)
    }
    names_short <- c("piControl_LUtrans1850_r8", "piControl_LUtrans1850_new")
    names_legend <- c("Jul22", "Dec22")
    modes <- rep("timmean", t=2)
    fromsf <- rep(3001, t=2)
    tosf <- rep(3005, t=2)

} else if (F) { # compare mldHT09 vs fesom1 mlotst
    models <- rep("fesom", t=2)
    prefixes <- rep("awi-esm-1-1-lr_kh800_ssp585", t=2)
    varnamesin <- c("mlotst", "mldepthdensp125_m")
    names_short <- names_legend <- varnamesin
    varnames_out_samedims <- "mlotst"
    names_legend_samedims <- c("fesom:mlotst:0.125", "post processed")
    modes <- rep("fldmean", t=2)
    fromsf <- rep(2015, t=2)
    tosf <- rep(2100, t=2)
    seasonsf <- rep("annual", t=2)
    areas <- rep("NH_65_30W120E", t=2)

} else if (F) { # w/out excluded sea ice locations
    models <- rep("recom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2",
                  "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2_exclude_sic_gt_0")
    names_short <- c("with_sic", "exclude_sic")
    names_legend <- paste0("esm-piControl ", c("with", "without"), " sic")
    varnamesin <- rep("dpCO2s", t=2)
    modes <- rep("fldmean", t=2)
    fromsf <- rep(3208, t=2)
    tosf <- rep(3590, t=2)
    new_origins <- rep(208, t=2)

} else if (F) { # nobio dead ocean
    models <- "recom"
    prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_nobio_spinup"
    if (F) prefixes <- paste0(prefixes, "_regular_dx1.000_dy1.000")
    names_short <- "esm-piControl_nobio_spinup"
    names_legend <- "esm-piControl nobio"
    #varnamesin <- "bgc12"
    #varnamesin <- "diags3d01"
    #varnamesin <- "diags3d02"
    varnamesin <- "NPPtot"
    modes <- "timmean"
    #modes <- "depthint"
    fromsf <- 3208
    #tosf <- 3323
    tosf <- 3264
    seasonsf <- "annual"
    #depthsf <- "_0-5900m"
    depthsf <- "_int0-5900m"
    new_origins <- 208
    #remove_mean_froms <- 208
    #remove_mean_tos <- remove_mean_froms
    #tunit <- "esm-piControl year"
    tunit <- "nobio year"

} else if (F) { # nobio dead ocean vs control
    models <- rep("echam6", t=2)
    #models <- rep("fesom", t=2)
    #models <- rep("recom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2",
                  "awi-esm-1-1-lr_kh800_esm-piControl_nobio_spinup")
    if (F) prefixes <- paste0(prefixes, "_regular_dx1.000_dy1.000")
    names_short <- c("esm-piControl_wout_talk_rest2", "esm-piControl_nobio_spinup")
    names_legend <- paste0("esm-piControl ", c("", "nobio"))
    #varnamesin <- rep("temp2", t=2)
    #varnamesin <- rep("co2_flx_land", t=2)
    #varnamesin <- rep("co2_flx_lcc", t=2)
    varnamesin <- rep("co2_flx_harvest", t=2)
    #varnamesin <- rep("nbp", t=2)
    #varnamesin <- rep("tos", t=2)
    #varnamesin <- rep("siarean", t=2)
    #varnamesin <- rep("siareas", t=2)
    #varnamesin <- rep("aCO2", t=2)
    #varnamesin <- rep("CO2f", t=2)
    #varnamesin <- rep("bgc02", t=2) # DIC
    #varnamesin <- rep("bgc05", t=2) # C nanophy
    #varnamesin <- rep("bgc08", t=2) # C det
    #varnamesin <- rep("bgc10", t=2)  # C het 
    #varnamesin <- rep("bgc12", t=2)  # DOC
    #varnamesin <- rep("bgc14", t=2) # C dia
    #varnamesin <- rep("diags3d01", t=2)
    #varnamesin <- rep("diags3d02", t=2)
    #varnamesin <- rep("NPPtot", t=2)
    #modes <- rep("select", t=2)
    #modes <- rep("timmean", t=2)
    #modes <- rep("fldmean", t=2)
    modes <- rep("fldint", t=2)
    #modes <- rep("depth", t=2)
    fromsf <- c(3208, 3208)
    #fromsf <- c(3517, 3517)
    #fromsf <- c(3926, 3926)
    #tosf <- c(3264, 3264)
    #tosf <- c(3265, 3264)
    #tosf <- c(3265, 3265)
    #tosf <- c(3323, 3264)
    #tosf <- c(3323, 3323)
    #tosf <- c(3536, 3536)
    #tosf <- c(3945, 3264)
    #tosf <- c(3945, 3265)
    #tosf <- c(3945, 3536)
    #tosf <- c(3945, 3327)
    #tosf <- c(3945, 3377)
    #tosf <- c(3945, 3945)
    #tosf <- c(3945, 4311)
    tosf <- c(4527, 4527)
    #seasonsf <- rep("annual", t=2)
    #depthsf <- rep("_0m", t=2)
    #depthsf <- rep("_0-5900m", t=2)
    #depthsf <- rep("_int0-5900m", t=2)
    if (F) {
        new_origins <- c(208, 208)
        tosp <- c(327, NA)
        tunit <- "esm-piControl year"
    } else if (T) {
        new_origins <- c(1, 1)
        #tosp <- c(57, NA)
        #tosp <- c(169, NA)
        #tosp <- c(328, NA)
        tunit <- "nobio year"
        #remove_mean_froms <- c(1, 1)
        #remove_mean_tos <- remove_mean_froms
    }

} else if (F) { # nobio dead ocean vs control and spinup
    models <- rep("recom", t=3)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest",
                  "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2",
                  "awi-esm-1-1-lr_kh800_esm-piControl_nobio_spinup")
    names_short <- c("esm-piControl_wout_talk_rest", "esm-piControl_wout_talk_rest2", "esm-piControl_nobio_spinup")
    names_legend <- paste0("esm-piControl ", c("bio mistral", "bio levante", "nobio levante"))
    varnamesin <- rep("NPPtot", t=3)
    modes <- rep("fldint", t=3)
    fromsf <- c(3151, 3208, 3208)
    tosf <- c(3207, 3217, 3217)
    seasonsf <- rep("annual", t=3)
    depthsf <- rep("_int0-5900m", t=3)
    new_origins <- c(151, 208, 208)
    tunit <- "esm-piControl year"

} else if (F) { # esm-hist_nobio dead ocean
    #models <- rep("echam6", t=2)
    models <- rep("fesom", t=2)
    #models <- rep("recom", t=2)
    if (T) { # absolute values
        prefixes <- c("awi-esm-1-1-lr_kh800_esm-hist",
                      "awi-esm-1-1-lr_kh800_esm-hist_nobio")
        names_short <- c("esm-hist", "esm-hist_nobio")
        names_legend <- paste0("esm-hist ", c("", "nobio"))
    } else if (F) { # anom wrt piControl
        prefixes <- c("awi-esm-1-1-lr_kh800_esm-hist_anom_wrt_piControl",
                      "awi-esm-1-1-lr_kh800_esm-hist_nobio_anom_wrt_piControl")
        names_short <- paste0(c("esm-hist", "esm-hist_nobio"), "_response")
        names_legend <- paste0("esm-hist", c("", " nobio"), " response") 
    }
    if (F) prefixes <- paste0(prefixes, "_regular_dx1.000_dy1.000")
    #varnamesin <- rep("temp2", t=2)
    #varnamesin <- rep("co2_flx_land", t=2)
    #varnamesin <- rep("co2_flx_lcc", t=2)
    #varnamesin <- rep("co2_flx_harvest", t=2)
    #varnamesin <- rep("nbp", t=2)
    varnamesin <- rep("tos", t=2)
    #varnamesin <- rep("siarean", t=2)
    #varnamesin <- rep("siareas", t=2)
    #varnamesin <- rep("aCO2", t=2)
    #varnamesin <- rep("CO2f", t=2)
    #varnamesin <- rep("bgc02", t=2) # DIC
    #varnamesin <- rep("bgc05", t=2) # C nanophy
    #varnamesin <- rep("bgc08", t=2) # C det
    #varnamesin <- rep("bgc10", t=2)  # C het 
    #varnamesin <- rep("bgc12", t=2)  # DOC
    #varnamesin <- rep("bgc14", t=2) # C dia
    #varnamesin <- rep("diags3d01", t=2)
    #varnamesin <- rep("diags3d02", t=2)
    #varnamesin <- rep("NPPtot", t=2)
    #modes <- rep("select", t=2)
    #modes <- rep("timmean", t=2)
    modes <- rep("fldmean", t=2)
    #modes <- rep("fldint", t=2)
    #modes <- rep("depth", t=2)
    fromsf <- rep(1850, t=2)
    tosf <- rep(2014, t=2)
    #seasonsf <- rep("annual", t=2)
    #depthsf <- rep("_0m", t=2)
    #depthsf <- rep("_0-5900m", t=2)
    #depthsf <- rep("_int0-5900m", t=2)

} else if (F) { # mhw composite data vs seas
    workpath <- "/work/ba1103/a270073"
    models <- rep("fesom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_seas_regular_dx0.250_dy0.250",
                  "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_data_regular_dx0.250_dy0.250")
    names_short <- paste0("awicm1-recom_mhw_composite_", c("seas", "data"))
    names_legend <- c("Climatology", "Events")
    #varnamesin <- rep("mlotst", t=2)
    varnamesin <- rep("omldamax", t=2)
    #varnamesin <- rep("tau", t=2)
    #varnames_out_samedims <- "tau"
    #names_legend_samedims <- c("taux", "tauy", "tau")
    #varnamesin <- rep("curltau", t=2)
    #varnamesin <- rep("ekmanP_ms", t=2)
    modes <- rep("timmean", t=2)
    fromsf <- rep(2842, t=2)
    tosf <- rep("", t=2)
    seasonsf <- rep("Jul", t=2)

} else if (F) { # reccap A_minus_C vs D_minus_B
    workpath <- "/work/ollie/cdanek"
    models <- rep("fesom", t=2)
    names_legend <- c("A-C", "D-B")
    prefixes <- c("reccap_A_minus_C", "reccap_D_minus_B")
    names_short <- prefixes
    varnamesin <- rep("CO2f", t=2)
    modes <- rep("fldint", t=2)
    fromsf <- rep(1958, t=2)
    tosf <- rep(2019, t=2)

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl me vs og
    models <- rep("echam6", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl", "awi-esm-1-1-lr_kh800_esm-piControl_og")
    names_short <- c("new", "old")
    varnamesin <- rep("temp2", t=2)
    fromsf <- c(1001, 1850)
    tosf <- c(1010, 1855)
    tunit <- "model year"
    new_origins <- c(1, 1) # spinup years counting from 1
    modes <- c("fldmean", "fldmean_monmean")

} else if (F) { # awi-esm-1-1-lr_kh800 piControl vs esm-piControl
    models <- rep("echam6", t=2)
    #models <- rep("fesom", t=2)
    #prefixes <- c("awi-esm-1-1-lr_kh800_piControl_og", "awi-esm-1-1-lr_kh800_esm-piControl_restartall")
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl", "awi-esm-1-1-lr_kh800_esm-piControl")
    names_short <- c("piControl", "esm-piControl")
    names_legend <- c("piControl", "esm-piControl")
    #varnamesin <- rep("temp2", t=2)
    #varnamesin <- rep("co2_flux", t=2)
    varnamesin <- rep("co2_flx_ocean", t=2)
    #echam6_global_setNA <- "land"
    #varnamesin <- rep("co2_flx_land", t=2)
    #echam6_global_setNA <- "ocean"
    #addland <- F
    #varnamesin <- rep("co2_flx_land", t=2)
    #varnamesin <- rep("co2_flx_npp", t=2)
    #varnamesin <- rep("co2_flx_resp", t=2)
    #varnamesin <- rep("tos", t=2)
    #varnamesin <- rep("thetaoga", t=2)
    #fromsf <- c(1950, 2686) # piControl start
    #fromsf <- c(1950, 3001)
    #fromsf <- c(2586, 2686) # piControl last 100 years
    #fromsf <- c(2680, 2686)
    fromsf <- c(2686, 3001)
    #tosf <- c(2685, 2785) # esm-piControl first 100 years
    #tosf <- c(2685, 2923)
    #tosf <- c(3000, 3131)
    tosf <- c(3000, 3136)
    tunit <- "model year"
    #seasonsf <- rep("annual", t=2)
    new_origins <- c(fromsf[1]-1950+1, fromsf[2]-2686+736+1) # pi: 1950=1; esm-piControl: 2686=737
    #fromsp <- c(736+1-20, NA) # last 20 years of piControl og
    fromsp <- c(1051+1-136, NA) # last xxx years of piControl
    #tosp <- c(NA, 756) # first 20 years of esm-piControl
    #modes <- rep("select", t=2)
    #modes <- rep("timmean", t=2)
    #modes <- c("fldmean_monmean", "fldmean")
    #modes <- rep("fldmean", t=2)
    modes <- rep("fldint", t=2)
    areas <- rep("SO45S", t=2)

} else if (F) { # awi-esm-1-1-lr_kh800 piControl vs piControl_LUtrans1850
    models <- rep("echam6", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl", "awi-esm-1-1-lr_kh800_LUtrans1850")
    names_legend <- c("PI landuseTrans=0", "PI landuseTrans=1850")
    names_short <- prefixes
    #varnamesin <- rep("co2_flx_lcc", t=2)
    #varnamesin <- rep("co2_flx_harvest", t=2)
    #varnamesin <- rep("co2_flx_land", t=2)
    #varnamesin <- rep("nbp", t=2)
    #varnamesin <- rep("co2_flx_ocean", t=2)
    varnamesin <- rep("co2_flx_total", t=2)
    modes <- rep("fldint", t=2)
    fromsf <- c(1950, 2951)
    tosf <- c(3000, 2997)
    new_origins <- c(1, fromsf[2]-1950+1)
    tunit <- "spinup year"
    fromsp <- c(980, NA)
    #fromsp <- c(1002, 1002) # same temporal means
    #tosp <- c(1048, 1048) # same temporal means
    
} else if (F) { # awi-esm-1-1-lr_kh800 piControl vs historical 
    models <- rep("echam6", t=2)
    #models <- rep("fesom", t=2)
    #prefixes <- c("awi-esm-1-1-lr_kh800_piControl_og", "awi-esm-1-1-lr_kh800_historical")
    #prefixes <- c("awi-esm-1-1-lr_kh800_piControl_og", "awi-esm-1-1-lr_kh800_historical_day")
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl", "awi-esm-1-1-lr_kh800_historical")
    names_short <- c("piControl", "historical")
    #names_short <- c("piControl", "historical_day")
    #names_legend <- c("piControl (monthly)", "historical (daily)")
    #names_legend <- c("piControl", "historical")
    #names_legend <- c("piControl", "historical (85-14)")
    ltys <- c(3, 1)
    #varnamesin <- rep("temp2", t=2)
    varnamesin <- rep("co2_flx_ocean", t=2)
    #varnamesin <- rep("tos", t=2)
    #varnamesin <- rep("thetaoga", t=2)
    #fromsf <- c(1950, 1850)
    fromsf <- c(2686, 1850)
    #tosf <- c(2685, 1863)
    #tosf <- c(2936, 2014)
    tosf <- c(3000, 2014)
    #new_origins <- c(1114, NA) # piControl og
    new_origins <- c(1850, NA) # 2686=1850
    #new_origins <- c(2014-29, NA)
    #fromsp <- c(NA, 1985)
    #fromsp <- c(1835, NA)
    fromsp <- c(1980, 1980)
    #tospf <- c(1849, NA)
    tosp <- c(2025, NA)
    #modes <- rep("fldmean", t=2)
    modes <- rep("fldint", t=2)
    #areas <- rep("reccap2_atlantic", t=2)
    #areas <- rep("reccap2_pacific", t=2)
    #areas <- rep("reccap2_indian", t=2)
    #areas <- rep("reccap2_arctic", t=2)
    #areas <- rep("reccap2_southern", t=2)
    # sub-atlantic:
    areas <- rep("reccap2_na_spss", t=2)
    #areas <- rep("reccap2_na_stss", t=2)
    #areas <- rep("reccap2_na_spss", t=2)
    #areas <- rep("reccap2_na_stps", t=2)
    #areas <- rep("reccap2_aequ", t=2)
    #areas <- rep("reccap2_sa_stps", t=2)
    #areas <- rep("reccap2_med", t=2)
    # sub-pacific:
    #areas <- rep("reccap2_np_spss", t=2)
    #areas <- rep("reccap2_np_stss", t=2)
    #areas <- rep("reccap2_np_stps", t=2)
    #areas <- rep("reccap2_pequ_w", t=2)
    #areas <- rep("reccap2_pequ_e", t=2)
    #areas <- rep("reccap2_sp_stps", t=2)

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl co2 flux echam vs recom
    models <- c("echam6", "recom")
    if (T) {
        prefixes <- rep("awi-esm-1-1-lr_kh800_esm-piControl_co2fsign", t=2)
        names_short <- rep("esm_piControl_2percboth_co2fsign", t=2)
        names_legend <- c("echam:co2_flx_ocean*-1", "recom:CO2f")
        fromsf <- rep(2686, t=2)
        #tosf <- rep(2689, t=2)
        tosf <- c(2939, 2689)
        tosp <- rep(2689, t=2)
        #new_origins <- rep(1, t=2)
    } else if (F) {
        prefixes <- rep("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2", t=2)
        names_short <- rep("esm_piControl_wout_talk_rest2", t=2)
        names_legend <- c("echam:co2_flx_ocean*-1", "recom:CO2f")
        fromsf <- rep(3208, t=2)
        tosf <- rep(3590, t=2)
        new_origins <- rep(208, t=2)
    }
    varnamesin <- c("co2_flx_ocean", "CO2f")
    varnames_out_samedims <- "co2flux"
    names_legend_samedims <- c("echam:co2_flx_ocean*-1", "recom:CO2f")
    tunit <- "esm-piControl year"
    modes <- rep("fldint", t=2)

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl co2fsign vs restartall
    #models <- rep("echam6", t=2)
    models <- rep("fesom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_co2fsign", "awi-esm-1-1-lr_kh800_esm-piControl_restartall")
    names_short <- c("co2fsign", "restartall")
    names_legend <- c("restart fesom", "restart all")
    varnamesin <- rep("aCO2", t=2)
    fromsf <- c(2686, 2686)
    tosf <- c(2737, 2702)
    tunit <- "model year"
    new_origins <- c(1, 1)
    tosp <- c(17, 17)
    modes <- rep("select", t=2)

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl2 vs esm-piControl_wout_talk_rest
    #models <- rep("echam6", t=2)
    models <- rep("fesom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl2", "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest")
    names_short <- c("esm-piControl2", "esm-piControl_wout_talk_rest")
    #names_legend <- c("esm-piControl with Talk rest", "esm-piControl wout Talk rest wrong commit")
    names_legend <- c("esm-piControl Talk rest on", "esm-piControl Talk rest off")
    varnamesin <- rep("aCO2", t=2)
    #varnamesin <- rep("bgc02", t=2)
    varnamesin <- rep("bgc03", t=2) # talk
    depths <- rep("0", t=2)
    seasonsf <- rep("annual", t=2)
    #seasonsf <- c(NA, "annual")
    fromsf <- c(3151, 3151)
    #tosf <- c(3168, 3156)
    tosf <- c(3168, 3227)
    tunit <- "model year"
    new_origins <- c(1, 1)
    #tosp <- c(17, 17)
    #modes <- rep("select", t=2)
    modes <- rep("fldmean", t=2)
    legend_pos_ts <- "bottomleft" #"topright"
    add_linear_trend <- T

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl_wout_talk_rest vs esm-piControl_wout_talk_rest2
    #models <- rep("fesom", t=2)
    models <- rep("recom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest", "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2")
    names_short <- c("esm-piControl_wout_talk_rest", "esm-piControl_wout_talk_rest2")
    names_legend <- c("esm-piControl mistral", "esm-piControl levante")
    varnamesin <- rep("aCO2", t=2)
    fromsf <- c(3151, 3208)
    tosf <- c(3227, 3497)
    tunit <- "model year"
    new_origins <- c(151, 208) # esm-piControl year 3001 was 1st year after piControl
    tunit <- "esm-piControl year"
    #fromsp <- c(208, 208)
    #tosp <- c(224, 224)
    modes <- rep("select", t=2)
    #modes <- rep("fldmean", t=2)
    legend_pos_ts <- "bottomleft" #"topright"
    #add_linear_trend <- T

} else if (F) { # awi-esm-1-1-lr vs awi-esm-1-1_kh800 piControl
    models <- rep("fesom", t=2)
    prefixes <- c("awi-esm-1-1-lr_piControl_regular_dx0.250_dy0.250_minus_woa2018_dx0.250_dy0.250_t_an_0m",
                  "awi-esm-1-1-lr_kh800_piControl_og_regular_dx0.250_dy0.250_minus_woa2018_dx0.250_dy0.250_t_an_0m")
    names_short <- c("piControl_kh1500", "piControl_kh800")
    names_legend <- c("piControl kh=1500 minus WOA18", "piControl kh=800 minus WOA18")
    varnamesin <- rep("tos", t=2)
    fromsf <- c(1855, 2586)
    tosf <- c(1954, 2685)
    tunit <- "model year"
    modes <- rep("timmean", t=2)

} else if (F) { # awi-esm-1-1-lr_kh800 piControl_og vs historical
    models <- rep("echam6", t=2)
    #models <- rep("fesom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl_og_day", "awi-esm-1-1-lr_kh800_historical_day")
    names_short <- c("piControl", "historical")
    varnamesin <- rep("temp2", t=2)
    #arnames_in <- rep("tos", t=2)
    #varnamesin <- rep("thetaoga", t=2)
    fromsf <- c(2675, 1850)
    tosf <- c(2685, 1862)
    new_origins <- c(1839, NA) # last 11 years
    modes <- rep("fldmean", t=2)

} else if (F) { # awi-esm-1-1-lr 1pct 4CO2
    #models <- rep("echam6", t=2)
    models <- rep("fesom", t=2)
    if (F) {
        prefixes <- c("awi-esm-1-1-lr_1percCO2_monthly_mean",
                      "awi-esm-1-1-lr_4CO2_monthly_mean")
    }
    names_short <- c("1pctCO2", "abrupt-4xCO2") 
    if (F) {
        varnamesin <- rep("mlotst", t=2)
        varnamesin <- rep("thetao", t=2)
        #varnamesin <- rep("so", t=2)
        #varnamesin <- rep("potdens", t=2)
        #areas <- rep("LSstolpe18", t=2)
        areas <- rep("GIN2", t=2)
        depths <- rep("0-5900", t=2)
        remove_mean_froms <- c(1850, 1850)
        remove_mean_tos <- remove_mean_froms
        depth_fromsp <- rep(-3500, t=2)
    }
    if (F) { # transient pi last 100
        fromsf <- c(1850, 1850)
        tosf <- c(2099, 2099)
    } else if (F) { # ltm last 30
        fromsf <- c(2070, 2070)
        tosf <- c(2099, 2099)
    }
    #n_mas_an <- rep(5, t=2)
    n_mas_an <- rep(3, t=2)
    if (F) {
        names_legend <- c(eval(substitute(expression(paste("1%CO"[2])))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2])))))
    } else if (F) {
        names_legend <- c(eval(substitute(expression(paste("1%CO"[2], " last 30 years mean")))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " last 30 years mean")))))
    } else if (F) {
        names_legend <- c(eval(substitute(expression(paste("1%CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsp[3], from=fromsp[1], to=tosp[1]))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsp[4], from=fromsp[2], to=tosp[2]))))
    }

} else if (F) { # mpi-esm vs awi* mlds semmler et al.
    workpath <- "/work/ab0246/a270073"
    models <- rep("fesom_vs_mpiom1", t=2)
    addland <- F
    prefixes <- c("awi-esm-1-1-lr_minus_mpi-esm1-2-lr_remapbil_r1440x720_conv180_piControl",
                  "awi-cm-1-1-mr_minus_mpi-esm1-2-hr_remapbil_r1440x720_conv180_piControl")
    names_short <- c("AWI_minus_MPI_LR", "AWI_minus_MPI_HR")
    names_legend <- c("AWI minus MPI (piControl LR)", "AWI minus MPI (piControl HR)")
    fromsf <- rep(1910, t=2)
    tosf <- rep(1930, t=2)
    varnamesin <- rep("omldamax", t=2)
    modes <- rep("timmean_monmax", t=2)

} else if (F) { # Hol-T with vs without orbital acceleration
    models <- c("echam5", "echam5")
    #models <- c("mpiom1", "mpiom1")
    #prefixes <- c("cosmos-aso-wiso_Hol-T", "cosmos-aso-wiso_Hol-Tx10")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_main_mm", "cosmos-aso-wiso_Hol-Tx10_main_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_main_mm_plev", "cosmos-aso-wiso_Hol-Tx10_main_mm_plev")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_main_mm", "cosmos-aso-wiso_Hol-Tx10_wiso_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_main_mm_significance_0.01", "cosmos-aso-wiso_Hol-Tx10_wiso_mm_significance_0.01")
    prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "cosmos-aso-wiso_Hol-Tx10_wiso_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm_significance_0.01", "cosmos-aso-wiso_Hol-Tx10_wiso_mm_significance_0.01")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_timeser_ext", "cosmos-aso-wiso_Hol-Tx10_timeser_ext")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_fort_75_monmean", "cosmos-aso-wiso_Hol-Tx10_fort_75_monmean")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_grb_code_183_remapcon2_r120x101",
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_183_remapcon2_r120x101")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "Hol-T_stschuett_echam5_wiso") # me vs st annual
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "Hol-T_echam5_wiso") # me vs me_w_st
    addland <- F
    names_short <- c("Hol-T", "Hol-Tx10")
    #names_short <- c("ch", "st")
    #names_short <- c("ch", "ch_w_st")
    #names_legend <- names_short
    names_legend <- c("COSMOS", "COSMOS x10")
    #names_legend <- c("COSMOS transient", "COSMOS transient x10")
    #names_legend <- c("transient", "transient x10")
    fromsf <- c("0004", "0001")
    #fromsf <- c("0004", "0004")
    #tosf <- c("7000", "7000")
    tosf <- c("7000", "7001")
    new_origins <- c(-6996, -7000)
    #new_origins <- c(-6996, -6996)
    time_ref <- 1950 # any string, e.g. "BP", or number
    #fromsp <- c(-20, -20) # with respect to `new_origin` (if defined) or `fromsf`
    #tosp <- c(0, 0)
    #types <- c("o", "o")
    #varnamesin <- c("temp2", "temp2")
    #varnamesin <- c("aprt", "aprt")
    #varnamesin <- c("tsurf", "tsurf")
    #varnamesin <- c("srad0d", "srad0d")
    #varnamesin <- c("albedo", "albedo")
    #varnamesin <- c("wisoaprt_d", "wisoaprt_d")
    varnamesin <- c("wisoaprt_d_post", "wisoaprt_d_post")
    #varnamesin <- c("lm_wisoaprt_d_post_as_time_slope", "lm_wisoaprt_d_post_as_time_slope")
    levs <- c(2, 2)
    #varnamesin <- c("tsurfaprt", "tsurfaprt")
    #varnamesin <- c("lm_wisoaprt_d_sellevel_2_as_temp2", "lm_wisoaprt_d_sellevel_2_as_temp2")
    #varnamesin <- c("lm_wisoaprt_d_sellevel_2_as_ptemp", "lm_wisoaprt_d_sellevel_2_as_ptemp")
    #varnamesin <- c("lm_wisoaprt_d_sellevel_2_as_tsurf", "lm_wisoaprt_d_sellevel_2_as_tsurf")
    #varnamesin <- c("lm_wisoaprt_d_sellevel_2_as_ptsurf", "lm_wisoaprt_d_sellevel_2_as_ptsurf")
    #varnamesin <- c("lm_temp2_as_time_slope", "lm_temp2_as_time_slope")
    #varnamesin <- c("lm_tsurf_as_time_slope", "lm_tsurf_as_time_slope")
    #varnamesin <- c("lm_aprt_as_time_slope", "lm_aprt_as_time_slope")
    #varnamesin <- c("lm_wind10_as_time_slope", "lm_wind10_as_time_slope")
    #varnamesin <- c("c204_ICEARE_GLO", "c204_ICEARE_GLO")
    #varnamesin <- c("c205_ICEVOL_GLO", "c205_ICEVOL_GLO")
    #varnamesin <- c("c204_ICEARE_GLO", "c204_ICEARE_GLO")
    #varnamesin <- c("c64_ICEARE_ARC", "c64_ICEARE_ARC")
    #varnamesin <- c("c65_ICEVOL_ARC", "c65_ICEVOL_ARC")
    #varnamesin <- c("c46_HFL_GIN", "c46_HFL_GIN")
    #varnamesin <- c("c47_WFL_GIN", "c47_WFL_GIN")
    #varnamesin <- c("c44_ICEARE_GIN", "c44_ICEARE_GIN")
    #varnamesin <- c("c45_ICEVOL_GIN", "c45_ICEVOL_GIN")
    #varnamesin <- c("c86_HFL_LAB", "c86_HFL_LAB")
    #varnamesin <- c("c87_WFL_LAB", "c87_WFL_LAB")
    #varnamesin <- c("c84_ICEARE_LAB", "c84_ICEARE_LAB")
    #varnamesin <- c("c85_ICEVOL_LAB", "c85_ICEVOL_LAB")
    #varnamesin <- c("c145_ICEVOL_SO", "c145_ICEVOL_SO")
    #varnamesin <- c("c145_ICEVOL_SO", "c145_ICEVOL_SO")
    #varnamesin <- rep("amoc", t=2)
    #codes <- rep(101, t=2)
    #varnamesin <- c("zmld", "zmld")
    #varnamesin <- c("quv", "quv")
    #varnamesin <- c("quv_direction", "quv_direction")
    #levsf <- c("_int1000-100hPa", "_int1000-100hPa")
    #varnames_out_samedims <- "quv"
    #names_legend_samedims <- c("qu", "qv")
    #modes <- rep("select", t=2)
    modes <- rep("yearsum", t=2)
    #modes <- rep("seassum", t=2)
    #areas <- rep("moc45to60N", t=2)
    #levs <- rep("-285to-2180m", t=2)
    #areas <- rep("moc30to60N", t=2)
    #levs <- rep("-285to-2180m", t=2)
    #areas <- rep("moc26.5N", t=2)
    #areas <- rep("moc50N", t=2)
    #levs <- rep("-0to-5420m", t=2)
    #areas <- c("LSeaSouthmld", "LSeaSouthmld")
    #areas <- c("GINmld", "GINmld")
    #areas <- c("weddelmld", "weddelmld")
    #areas <- c("ladoga_remapnn", "ladoga_remapnn")
    #areas <- c("shuchye_remapnn", "shuchye_remapnn")
    #areas <- c("levinson-lessing_remapnn", "levinson-lessing_remapnn")
    #areas <- c("taymyr_remapnn", "taymyr_remapnn")
    #areas <- c("emanda_remapnn", "emanda_remapnn")
    #areas <- c("kotokel_remapnn", "kotokel_remapnn")
    #areas <- c("elgygytgyn_remapnn", "elgygytgyn_remapnn")
    areas <- c("two-yurts_remapnn", "two-yurts_remapnn")
    seasonsf <- rep("annual", t=2)
    #seasonsf <- rep("yearsum", t=2)
    #seasonsf <- rep("seassum", t=2)
    #seasonsp <- rep("DJF", t=2)
    #seasonsp <- rep("MAM", t=2)
    #seasonsp <- rep("JJA", t=2)
    #seasonsp <- rep("SON", t=2)
    #seasonsp <- rep("Mar", t=2)
    #seasonsp <- rep("Apr", t=2)
    #seasonsp <- rep("Jun", t=2)
    #seasonsp <- rep("Sep", t=2)
    #seasonsp <- rep("Dec", t=2)
    n_mas_an <- c(20.83333, 4.16667) # jqs fig. 6
    #n_mas_an <- c(30, 10)
    #n_mas_an <- c(250, 50) # jan-dec
    #n_mas_an <- c(120, 20) # seasons
    #remove_mean_froms <- c(-179, 0)
    #remove_mean_tos <- c(-179, 0)
    #regboxes <- lapply(vector("list", l=2), base::append, list(regbox="NAsiberia"))

} else if (F) { # two vars of qu, qv, quv
    prefixes <- c("cosmos-aso-wiso_Hol-Tx10_wiso_mm", "cosmos-aso-wiso_Hol-Tx10_wiso_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-Tx10_main_mm_plev", "cosmos-aso-wiso_Hol-Tx10_main_mm_plev")
    models <- c("echam5", "echam5")
    names_short <- c("Hol-Tx10", "Hol-Tx10")
    names_legend <- c("Ladoga Hol-Tx10", "Ladoga Hol-Tx10")
    #names_legend <- c("Ladoga Hol-Tx10", "LadogaLand Hol-Tx10")
    fromsf <- c("0001", "0001") # Hol-Tx10
    tosf <- c("7001", "7001") # Hol-Tx10
    new_origins <- c(-7000, -7000) # Hol-Tx10
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas_an <- c(10, 10)
    modes <- c("select", "select")
    #modes <- c("yearsum", "yearsum")
    #seasonsf <- c("yearsum", "yearsum")
    #seasonsp <- "Feb" # cdo's default season timestamps: Feb, May, Aug, Nov
    #seasonsp <- "May"
    #seasonsp <- "Jun"
    #seasonsp <- "Aug"
    #seasonsp <- "Nov"
    #seasonsp <- "Dec"
    varnamesin <- c("aprl", "aprc")
    varnames_out_samedims <- c("aprl_vs_aprc")
    names_legend_samedims <- c("aprl", "aprc")
    #varnamesin <- c("wisoaprt_d_post", "wisoaprt_d_post")
    #levs <- c(2, 2)
    #varnamesin <- c("qu", "qv")
    #levsf <- c("_int1000-100hPa", "_int1000-100hPa")
    #varnames_out_samedims <- "quv"
    #names_legend_samedims <- c("qu", "qv")
    areas <- c("ladoga_remapnn", "ladoga_remapnn")
    #areas <- c("ladoga_remapnn", "ladogaLand_remapnn")

} else if (F) { # positive/negative north pacific index NPI in Hol-T
    models <- c("echam5", "echam5")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_main_mm", "cosmos-aso-wiso_Hol-T_main_mm")
    prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "cosmos-aso-wiso_Hol-T_wiso_mm")
    names_short <- c("Hol-T_above1.5sd", "Hol-T_below1.5sd")
    fromsf <- c("0004", "0004")
    tosf <- c(7000, 7000)
    new_origins <- c(-6996, -6996)
    time_ref <- 1950
    #varnamesin <- c("psl_gt_1.5_sd_NPI", "psl_lt_1.5_sd_NPI")
    #varnamesin <- c("wind10_gt_1.5_sd_NPI", "wind10_lt_1.5_sd_NPI")
    #varnamesin <- c("u10_gt_1.5_sd_NPI", "u10_lt_1.5_sd_NPI")
    #varnamesin <- c("v10_gt_1.5_sd_NPI", "v10_lt_1.5_sd_NPI")
    #varnamesin <- c("temp2_gt_1.5_sd_NPI", "temp2_lt_1.5_sd_NPI")
    varnamesin <- c("aprt_gt_1.5_sd_NPI", "aprt_lt_1.5_sd_NPI")
    #varnamesin <- c("wisoaprt_d_post_gt_1.5_sd_NPI", "wisoaprt_d_post_lt_1.5_sd_NPI")
    #levs <- c(2, 2)
    #modes <- c("select", "select")
    #modes <- c("timmean", "timmean")
    modes <- c("yearsum", "yearsum")
    #seasonsf <- c("annual", "annual")
    #seasonsf <- c("NDJFMmean", "NDJFMmean")
    #seasonsf <- c("yearsum", "yearsum")
    seasonsf <- c("NDJFMsum", "NDJFMsum")
    regboxes <- lapply(vector("list", l=2), base::append, list(regbox="NAsiberia"))

} else if (F) { # recT106erai vs recT127era5
    models <- c("echam5", "echam6")
    prefixes <- c("echam5_recT106erai_wiso", "echam6_recT127era5_wiso")
    names_short <- c("recT106erai", "recT127era5")
    names_legend <- c("T106 ERA-I", "T127 ERA5")
    fromsf <- c(1958, 1979)
    tosf <- c(2019, 2019)
    n_mas_an <- rep(3, t=2)
    varnamesin <- rep("temp2", t=2)
    #varnamesin <- rep("tsurf", t=2)
    #varnamesin <- rep("aprt", t=2)
    modes <- rep("select", t=2)
    #areas <- rep("ladoga_remapnn", t=2)
    #areas <- rep("shuchye_remapnn", t=2)
    #areas <- rep("levinson-lessing_remapnn", t=2)
    #areas <- rep("taymyr_remapnn", t=2)
    #areas <- rep("emanda_remapnn", t=2)
    #areas <- rep("elgygytgyn_remapnn", t=2)
    areas <- rep("two-yurts_remapnn", t=2)
    #areas <- rep("kotokel_remapnn", t=2)

} else if (F) { # phd stuff 2 settings
    workpath <- "/work/ba0941/a270073"
    models <- rep("fesom", t=2)
    prefixes <- c("Low01_sub_lsea_s52", "LSea5_sub_lsea_s5")
    #prefixes <- c("Low01_s52_regular_dx0.250_dy0.250", "LSea5_s5_regular_dx0.250_dy0.250")
    #prefixes <- c("Low01_s52_regular_dx0.100_dy0.100", "LSea5_s5_regular_dx0.100_dy0.100")
    #prefixes <- c("Low01_s52_regular_dx0.100_dy0.100", "LSea5_sub_lsea_s5_regular_dx0.100_dy0.100")
    #prefixes <- c("Low01_sub_lsea_s52_regular_dx0.100_dy0.100", "LSea5_sub_lsea_s5_regular_dx0.100_dy0.100")
    names_short <- c("Low01_s52", "LSea5_s5") 
    #names_short <- c("Low01_s5_dx0.250", "Lsea5_s5_dx0.250")
    #names_short <- c("Low01_s5_dx0.100", "Lsea5_s5_dx0.100")
    #names_legend <- c("", "")
    names_legend <- c("Low", "High")
    #names_legend <- c("a) Low", "b) High")
    if (F) {
        varnamesin <- rep("resolutionkm", t=2)
        seasonsf <- fromsf <- tosf <- rep("", t=2)
        proj <- "+proj=ortho +lat_0=30 +lon_0=-45"
        proj <- "+proj=ortho +lat_0=40 +lon_0=-45"
    } else if (F) {
        varnamesin <- rep("mixlay", t=2)
    } else if (F) {
        varnamesin <- rep("Ftemp", t=2)
        depths <- c(0, 0)
        load_special_data <- T
        #bilinear_interp_factor <- 4 # useRaster=F
        bilinear_interp_factor <- 10 # useRaster=T
    } else if (F) {
        varnamesin <- rep("divuvt", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        ignore_vars <- c(ignore_vars, "dutempdx", "dvtempdy", "dx_u_times_temp", "dy_v_times_temp")
        #bilinear_interp_factor <- 4 # useRaster=F
        bilinear_interp_factor <- 10 # useRaster=T
    } else if (F) {
        varnamesin <- rep("divuvteddy", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        ignore_vars <- c(ignore_vars, "duteddydx", "dvteddydy", "dx_utemp_eddy", "dy_vtemp_eddy")
        #bilinear_interp_factor <- 4 # useRaster=F
        bilinear_interp_factor <- 10 # useRaster=T
    } else if (F) {
        varnamesin <- rep("divuvttot", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        #bilinear_interp_factor <- 4 # useRaster=F
        bilinear_interp_factor <- 10 # useRaster=T
    } else if (F) {
        varnamesin <- rep("FeKe", t=2)
        depths <- c(0, 0)
        lepos <- "topleft"
        load_special_data <- T
        bilinear_interp_factor <- 10
    } else if (F) {
        varnamesin <- rep("HRS", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        bilinear_interp_factor <- 10
        #add_legend <- F; add_legend_right_yaxis <- T; add_legend_left_yaxis_before <- F
    } else if (F) {
        varnamesin <- rep("VRS", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        add_legend <- F; add_legend_right_yaxis <- F; add_legend_left_yaxis_before <- F
    } else if (F) {
        varnamesin <- rep("KmKe", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
    } else if (T) {
        varnamesin <- rep("wbeddy", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        bilinear_interp_factor <- 10
        add_legend <- F; add_legend_right_yaxis <- T; add_legend_left_yaxis_before <- T
    } else if (F) {
        varnamesin <- rep("eke", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        add_legend <- F; add_legend_right_yaxis <- T; add_legend_left_yaxis_before <- T
    } else if (F) {
        varnamesin <- rep("eke_over_tke", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        add_legend <- F; add_legend_right_yaxis <- T; add_legend_left_yaxis_before <- T
    }
    polyl <- #"/post/fesom/CbSCL_mesh_LS_ge_3000m.and.hvel_lt_5_cms-1_chull.txt"
    polyl <- #"/post/fesom/CbSCL_mesh_LS_ge_3000m.and.hvel_lt_6.1_cms-1_chull.txt" # = LS30l2; grl paper LS int low
    polyl <- "/post/fesom/LSea2_mesh_LS_abs_HRS_wbeddy_gt_10e-6_chull.txt" # = LShrswbeddy10h; grl paper WGC
    polyl <- read.table(paste0(workpath, polyl), header=F, col.names=c("x", "y"))
    polyh <- "/post/fesom/LSea2_mesh_LS_ge_3500m_chull.txt"
    polyh <- #"/post/fesom/CbSCL_mesh_LS_ge_3000m_chull.txt" # = LS30l; grl paper LS int high
    polyh <- read.table(paste0(workpath, polyh), header=F, col.names=c("x", "y"))
    #areas <- rep("lsea", t=2)
    #areas <- c("LS30l2", "LS30l")
    #areas <- rep("LS30l2", t=2)
    areas <- rep("LShrswbeddy10h", t=2)
    fromsf <- rep(1948, t=2)
    tosf <- rep(2009, t=2)
    #seasonsf <- rep("Mar", t=2)
    n_mas_an <- rep(3, t=2)
    #modes <- rep("timmean", t=2)
    modes <- rep("fldmean", t=2)
    #modes <- rep("fldint", t=2)

} else if (F) { # phd Arc22
    models <- rep("fesom", t=2)
    prefixes <- c("Arc22_sub_daily", "Arc22_sub")
    names_short <- c("Arc22_day", "Arc22_mon")
    names_legend <- c("daily model output", "monthly model output")
    #varnamesin <- rep("divuvbtot", t=2)
    varnamesin <- rep("divuvbeddy", t=2); ignore_vars <- c(ignore_vars, "dubeddydx_meanint", "dvbeddydy_meanint")
    depthsf <- rep("_int0-5650m", t=2)
    areas <- rep("arc08fram", t=2)
    fromsf <- rep(2005, t=2)
    tosf <- rep(2009, t=2)
    modes <- rep("fldint", t=2)
    cols <- c(2, 1)
    lepos <- "bottomright"

} else if (T) { # rho online offline frauke
    models <- rep("fesom", t=2)
    prefixes <- rep("fesom1.4_old_Low01_s52", t=2)
    names_short <- prefixes 
    names_legend <- c("online", "offline")
    varnamesin <- c("rho", "rho_insitu")
    varnames_out_samedims <- "rho_insitu"
    names_legend_samedims <- names_legend
    depthsf <- rep("_0-6000m", t=2)
    areas <- rep("global", t=2)
    fromsf <- rep(2009, t=2)
    tosf <- rep(2009, t=2)
    seasonsf <- rep("annual", t=2)
    modes <- rep("depth", t=2)

# =====================================
# 3 settings
} else if (F) { # en4
    models <- prefixes <- rep("EN.4.2.2", t=3)
    names_short <- names_legend <- c("annual", "summer", "winter")
    varnamesin <- rep("mldepthdensp030_m", t=3)
    modes <- rep("fldmean", t=3)
    fromsf <- rep(1980, t=3)
    tosf <- rep(2020, t=3)
    seasonsf <- c("annual", "summer", "winter")
    detrend_ts <- T

} else if (F) { # awi-esm-1-1-lr_kh800 piControl vs historical vs historical2 
    models <- rep("echam6", t=3)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl", "awi-esm-1-1-lr_kh800_historical", "awi-esm-1-1-lr_kh800_historical2")
    names_short <- c("piControl", "historical", "historical2")
    names_legend <- names_short
    #varnamesin <- rep("co2_flx_ocean", t=3)
    #varnamesin <- rep("co2_flx_lcc", t=3)
    varnamesin <- rep("co2_flx_harvest", t=3)
    #varnamesin <- rep("nbp", t=3)
    #varnamesin <- rep("co2_flx_total", t=3)
    modes <- rep("fldint", t=3)
    fromsf <- c(1950, 1850, 1850)
    tosf <- c(2995, 2006, 2006)
    #tosf <- c(2996, 2006, 2006)
    new_origins <- c(1114, NA, NA) # historical starts from pi 2686 = spinup year 737 --> 1850 - 737 + 1 = 1114
    fromsp <- c(1850, NA, NA)
    tosp <- c(2006, NA, NA)
    #n_mas_an <- rep(5, t=3)
    #n_mas_an <- rep(10, t=3)

} else if (F) { # awi-esm-1-1-lr_kh800 piControl vs historical2 vs ssp585
    models <- rep("echam6", t=3)
    #models <- rep("fesom", t=3)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl", "awi-esm-1-1-lr_kh800_historical2", "awi-esm-1-1-lr_kh800_ssp585")
    names_short <- c("piControl", "historical2", "ssp585")
    names_legend <- c("piControl", "historical", "ssp585")
    #varnamesin <- rep("temp2", t=3)
    #varnamesin <- rep("co2_flx_land", t=3)
    #varnamesin <- rep("co2_flx_ocean", t=3)
    #varnamesin <- rep("co2_flx_lcc", t=3)
    #varnamesin <- rep("co2_flx_harvest", t=3)
    varnamesin <- rep("nbp", t=3)
    #varnamesin <- rep("tos", t=3)
    #modes <- rep("fldmean", t=3)
    modes <- rep("fldint", t=3)
    fromsf <- c(1950, 1850, 2015)
    tosf <- c(3000, 2014, 2019)
    new_origins <- c(1114, NA, NA) # historical starts from pi 2686 = spinup year 737 --> 1850 - 737 + 1 = 1114
    #fromsp <- c(1850, NA, NA) # zoom deck period
    #tosp <- c(2100, NA, NA)
    #fromsp <- c(2005, 2005, NA) # zoom 2005-2024
    #tosp <- c(2024, NA, 2024)
    fromsp <- c(2010, 2010, NA) # zoom 2010-2018
    tosp <- c(2018, NA, 2018)
    types <- c("o", "o", "o")
    #n_mas_an <- rep(3, t=3)
    #n_mas_an <- rep(5, t=3)
    #n_mas_an <- rep(10, t=3)

} else if (F) { # awicm1-recom piControl vs esm-piControl
    models <- rep("echam6", t=3)
    prefixes <- c(rep("awi-esm-1-1-lr_kh800_piControl", t=2), "awi-esm-1-1-lr_kh800_esm-piControl")
    names_short <- paste0("ar1_", c(rep("piControl", t=2), "esm-piControl"))
    names_legend <- paste0(c(rep("piControl ", t=2), "esm-piControl "), c(1000, 131, 131), "a")
    varnamesin <- rep("co2_flx_ocean", t=3)
    fromsf <- c(rep(1950, t=2), 3001)
    tosf <- c(rep(3000, t=2), 3131)
    tunit <- "model year"
    new_origins <- c(fromsf[1:2]-1950+1, fromsf[3]-2686+736+1) # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    fromsp <- c(1051+1-c(1000, 131), NA) # last xxx years of piControl
    n_mas_an <- rep(10, t=3)
    add_linear_trend <- T
    modes <- rep("fldint", t=3)

} else if (F) { # awicm1-recom piControl vs piControl_LUtrans1850 vs esm-piControl
    models <- rep("echam6", t=3)
    prefixes <- paste0("awi-esm-1-1-lr_kh800_", c("piControl", "piControl_LUtrans1850", "esm-piControl"))
    names_short <- paste0("ar1_", c("piControl", "piControl_LUtrans1850", "esm-piControl"))
    names_legend <- c("piControl (LUtrans=0)", "piControl (LUtrans=1850)", "esm-piControl")
    #varnamesin <- rep("co2_flx_ocean", t=3)
    #varnamesin <- rep("nbp", t=3)
    varnamesin <- rep("co2_flx_total", t=3)
    fromsf <- c(1950, 2951, 3001)
    tosf <- c(3000, 3062, 3131)
    tunit <- "model year"
    new_origins <- c(fromsf[1]-1950+1, fromsf[2]-1950+1, fromsf[3]-2686+736+1) # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    fromsp <- c(1051+1-852, NA, NA) # last xxx years of piControl
    tosp <- c(1001, 1051, NA)
    n_mas_an <- rep(10, t=3)
    add_linear_trend <- T
    add_linear_trend_froms <- c(700, NA, NA)
    modes <- rep("fldint", t=3)

} else if (F) { # awicm1-recom piControl vs historical vs esm-piControl
    models <- rep("echam6", t=3)
    #models <- rep("fesom", t=3)
    prefixes <- c(#"awi-esm-1-1-lr_kh800_piControl_og",
                  "awi-esm-1-1-lr_kh800_piControl",
                  "awi-esm-1-1-lr_kh800_historical",
                  "awi-esm-1-1-lr_kh800_esm-piControl_restartall")
    names_short <- past0("ar1_", c("piControl", "historical", "esm-piControl"))
    names_legend <- names_short
    #varnamesin <- rep("temp2", t=3)
    varnamesin <- rep("co2_flx_ocean", t=3)
    addland <- F
    echam6_global_setNA <- "land"
    #varnamesin <- rep("co2_flx_land", t=3)
    #echam6_global_setNA <- "ocean"
    #addland <- F
    #varnamesin <- rep("tos", t=3)
    #varnamesin <- rep("aCO2", t=3)
    #fromsf <- c(1950, 1850, 2686)
    fromsf <- c(2806, 1967, 2778)
    #tosf <- c(2685, 1906, 2702)
    tosf <- c(2825, 1986, 2797)
    #new_origins <- c(1114, NA, 1850) # piControl before 1850, esm-piControl from 1850
    new_origins <- c(1850, NA, 1850) # piControl 1850, esm-piControl from 1850
    #fromsp <- c(1750, NA, NA)
    #fromsp <- c(1800, NA, NA)
    #modes <- rep("select", t=3)
    modes <- rep("timmean", t=3)
    #modes <- rep("fldmean", t=3)
    #modes <- rep("fldint", t=3)

} else if (F) { # awicm1-recom esm-piControl
    models <- rep("echam6", t=3)
    #models <- "fesom"
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_2percatm",
                  "awi-esm-1-1-lr_kh800_esm-piControl_2percfalse",
                  "awi-esm-1-1-lr_kh800_esm-piControl_2percboth")
    names_short <- paste0("ar1_", c("2percatm", "2percfalse", "2percboth"))
    names_legend <- names_short
    varnamesin <- rep("co2_flx_ocean", t=3)
    fromsf <- rep(2686, t=3)
    tosf <- c(2700, 2700, 2700)
    #tosf <- c(2760, 2700, 2700)
    new_origins <- rep(1, t=3)
    tosp <- rep(15, t=3)
    modes <- rep("timmean", t=3)
    #modes <- rep("fldint", t=3)

} else if (F) { # awicm1-recom piControl vs historical vs esm-piControl
    models <- rep("recom", t=3)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl",
                  "awi-esm-1-1-lr_kh800_historical2",
                  "awi-esm-1-1-lr_kh800_esm-piControl")
    names_short <- c("piControl", "historical", "esm-piControl")
    varnamesin <- rep("aCO2", t=3)
    #varnamesin <- rep("bgc16", t=3)
    #varnamesin <- rep("bgc17", t=3)
    #varnamesin <- rep("bgc18", t=3)
    #varnamesin <- rep("benSi", t=3)
    #varnamesin <- rep("silicate", t=3)
    #depthsf <- rep("_int0-5900m", t=3)
    fromsf <- c(1950, 1850, 3001)
    tosf <- c(3000, 2014, 3945)
    #seasonsf <- rep("annual", t=3)
    new_origins <- c(1, 737, 1052)
    tunit <- "piControl-year"
    modes <- rep("select", t=3)
    #modes <- rep("fldint", t=3)

} else if (F) { # awi-esm-1-1-lr deck anomlies vs pi
    workpath <- "/work/ab0246/a270073"
    models <- rep("echam6", t=3)
    #models <- rep("fesom", t=3)
    if (F) {
        prefixes <- c("historical_minus_piControl_echam6_echammon_awi-esm-1-1-lr",
                      "1percCO2_minus_piControl_echam6_echammon_awi-esm-1-1-lr",
                      "4CO2_minus_piControl_echam6_echammon_awi-esm-1-1-lr")
        names_short <- c("hist", "1pctCO2", "abrupt-4xCO2") 
    } else if (F) {
        prefixes <- c("awi-esm-1-1-lr_historical_minus_piControl_monthly_mean",
                      "awi-esm-1-1-lr_1percCO2_minus_piControl_monthly_mean",
                      "awi-esm-1-1-lr_4CO2_minus_piControl_monthly_mean")
        names_short <- c("hist", "1pctCO2", "abrupt-4xCO2") 
    }
    if (F) {
        varnamesin <- rep("temp2", t=3)
    } else if (F) {   
        varnamesin <- rep("tos", t=3)
        postpaths <- paste0(workpath, "/post/", models, "/regular_grid/ltm/", mode, "/", varnamesin)
        reg_dxs <- reg_dys <- rep("0.250", t=3)
    }
    fromsf <- c(1985, 2070, 2070)
    tosf <- c(2014, 2099, 2099)
    if (F) {
        names_legend <- c("historical", 
                          eval(substitute(expression(paste("1%CO"[2])))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2])))))
    } else if (F) {
        names_legend <- c(paste0("historical ", fromsf[1], "-", tosf[1], " mean minus piControl"),
                          #paste0("historical ", fromsp[2], "-", tosp[2], " mean"),
                          eval(substitute(expression(paste("1%CO"[2], " last 30 years mean minus piControl")))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " last 30 years mean minus piControl")))))
    }

} else if (F) { # Hol-7 vs Hol-T with vs without orbital acceleration
    models <- rep("echam5", t=3)
    #models <- rep("mpiom1", t=3)
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=3)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm", t=3)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_grb_code_15_remapcon2_r120x101_gt_0.15_times_area", t=3)
    prefixes <- c("cosmos-aso-wiso_Hol-7_wiso_mm", 
                  "cosmos-aso-wiso_Hol-T_wiso_mm", 
                  "cosmos-aso-wiso_Hol-Tx10_wiso_mm") 
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", 
    #              "Hol-T_stschuett_echam5_wiso",
    #              "Hol-T_echam5_wiso")
    #prefixes <- c("cosmos-aso-wiso_Hol-7_fort_75_monmean",
    #              "cosmos-aso-wiso_Hol-T_fort_75_monmean",
    #              "cosmos-aso-wiso_Hol-Tx10_fort_75_monmean")
    #prefixes <- c("cosmos-aso-wiso_Hol-7_timeser_ext",
    #              "cosmos-aso-wiso_Hol-T_timeser_ext",
    #              "cosmos-aso-wiso_Hol-Tx10_timeser_ext")
    #prefixes <- c("cosmos-aso-wiso_Hol-7_grb_code_15_remapcon2_r120x101_gt_0.15_times_area", # sicmomo
    #              "cosmos-aso-wiso_Hol-T_grb_code_15_remapcon2_r120x101_gt_0.15_times_area",
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_15_remapcon2_r120x101_gt_0.15_times_area")
    #prefixes <- c("cosmos-aso-wiso_Hol-7_grb_code_183_remapcon2_r120x101", # zmld
    #              "cosmos-aso-wiso_Hol-T_grb_code_183_remapcon2_r120x101",
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_183_remapcon2_r120x101")
    #prefixes <- rep("cosmos-aso-wiso_mpiom1_Hol-T_grb_code_15_remapcon2_r120x101", t=3) # sicmomo
    #names_short <- rep("Hol-Tx10", t=3)
    #names_short <- rep("Hol-T", t=3)
    names_short <- c("Hol-7", "Hol-T", "Hol-Tx10")
    #names_short <- c("ch", "st", "me_w_st")
    #names_legend <- names_short
    #names_legend <- c("7k ctrl", "transient", "transient x10")
    names_legend <- c("COSMOS 7k ctrl", "COSMOS", "COSMOS x10")
    #names_legend <- c("COSMOS 7k", "COSMOS transient", "COSMOS transient x10")
    #names_legend <- c("Mar", "Sep", "Annual")
    #names_legend <- c("a) Dec", "b) Jun", "c) Annual")
    #cols <- c(3, 2, 1)
    #fromsf <- rep("0001", t=3) # hol-tx10
    #fromsf <- rep("0004", t=3) # hol-t
    #fromsf <- c("0800", "0004", "0001") # hol-7 complete, hol-t, hol-tx10
    #fromsf <- c("2791", "0004", "0001") # hol-7 last 110 years, hol-tx10, hol-t
    fromsf <- c("2800", "0004", "0001") # hol-7 last 101 years, hol-tx10, hol-t
    #fromsf <- c("0004", "0004", "0004") # ch, st, ch_w_st
    tosf <- c("2900", "7000", "7001") # hol-7, hol-t, hol-tx10
    #tosf <- c("7000", "7000", "7000") # ch, st, ch_w_st
    #tosf <- rep("7001", t=3) # hol-tx10
    #tosf <- rep("7000", t=3) # hol-t
    #new_origins <- rep(-7000, t=3) # hol-tx10
    #new_origins <- rep(-6996, t=3) # hol-t
    #new_origins <- c(-9101, -6996, -7000) # hol-7 complete, hol-t, hol-tx10
    #new_origins <- c(-7110, -6996, -7000) # hol-7 last 110 years, hol-t, hol-tx10
    new_origins <- c(-7101, -6996, -7000) # hol-7 last 101 years, hol-t, hol-tx10
    #new_origins <- c(-6996, -6996, -6996) # ch, st, ch_w_st
    time_ref <- 1950 # any string, e.g. "BP", or number
    #remove_mean_froms <- rep(-6996, t=3)
    #remove_mean_tos <- rep(-6996, t=3)
    add_linear_trend <- c(F, T, T)
    #fromsp <- c(-7200, NA, NA)
    #fromsp <- c(-7010, -6996, -7000) # zoom: 7k control/7k transient transition 
    #tosp <- c(-7001, -6980, -6980)
    #fromsp <- c(-7030, -6996, -7000) # zoom: 7k control/7k transient transition 
    #tosp <- c(-7001, -6900, -6900)
    #seasonsf <- rep("annual", t=3)
    #seasonsf <- rep("yearsum", t=3)
    #seasonsf <- rep("seassum", t=3) # cdo default: date is middle of season
    #seasonsf <- c("annual", NA, NA)
    #seasonsf <- c("Mar", "Sep", "annual")
    #seasonsf <- c(NA, NA, "annual")
    #seasonsf <- c("Dec", "Jun", "annual")
    #seasonsp <- rep("DJF", t=3)
    #seasonsp <- rep("MAM", t=3)
    #seasonsp <- rep("JJA", t=3)
    #seasonsp <- rep("SON", t=3)
    #seasonsp <- rep("Jan", t=3)
    #seasonsp <- rep("Feb", t=3)
    #seasonsp <- rep("Mar", t=3)
    #seasonsp <- rep("Jun", t=3)
    #seasonsp <- rep("Sep", t=3)
    #seasonsp <- c(NA, "Mar", "Sep")
    #seasonsp <- c("Dec", "Jun", NA)
    varnamesin <- rep("temp2", t=3)
    #varnamesin <- rep("aprt", t=3)
    #varnamesin <- c("aprt", "aprl", "aprc")
    #varnames_out_samedims <- "aprt"
    #names_legend_samedims <- c("aprt (total)", "aprl (large-scale)", "aprc (convection)")
    #varnamesin <- rep("evap", t=3)
    #varnamesin <- rep("pe", t=3)
    #varnamesin <- rep("ws", t=3)
    #varnamesin <- rep("wisoaprt_d", t=3)
    #varnamesin <- rep("wisoaprt_d_post", t=3)
    #varnamesin <- rep("wisoevap_d_post", t=3)
    #varnamesin <- rep("wisope_d_post", t=3)
    #varnamesin <- rep("c204_ICEARE_GLO", t=3)
    #varnamesin <- rep("c64_ICEARE_ARC", t=3)
    #varnamesin <- rep("c144_ICEARE_SO", t=3)
    #varnamesin <- rep("SICOMO", t=3)
    #varnamesin <- rep("lm_SICOMO_as_time_slope", t=3)
    #varnamesin <- rep("zmld", t=3)
    #varnamesin <- rep("amoc", t=3)
    #codes <- rep(101, t=3)
    #varnamesin <- rep("srad0d", t=3)
    #modes <- rep("select", t=3)
    modes <- rep("fldmean", t=3)
    #modes <- rep("fldsum", t=3)
    #modes <- rep("yearsum", t=3)
    #modes <- rep("seassum", t=3)
    #modes <- rep("zonmean", t=3)
    #areas <- rep("moc26.5N", t=3)
    #levs <- rep("-0to-5420m", t=3)
    #areas <- rep("northern_hemisphere", t=3)
    #areas <- rep("southern_hemisphere", t=3)
    #areas <- rep("weddelmld", t=3)
    #areas <- rep("ladoga_remapnn", t=3)
    #areas <- rep("shuchye_remapnn", t=3)
    #areas <- rep("levinson-lessing_remapnn", t=3)
    #areas <- rep("taymyr_remapnn", t=3)
    #areas <- rep("emanda_remapnn", t=3)
    #areas <- rep("elgygytgyn_remapnn", t=3)
    #areas <- rep("two-yurts_remapnn", t=3)
    #areas <- rep("kotokel_remapnn", t=3)
    #levs <- rep(2, t=3)
    #n_mas_an <- rep(25, t=3) # jqs paper fig 1 srad0d
    n_mas_an <- c(10, 30, 10) # jqs paper fig 1 temp2
    #regboxes <- lapply(vector("list", l=3), base::append, list(regbox="NAsiberia"))

} else if (F) { # Hol-T three var plus vectors: wind10 (u10, v10) or quv (qu, qv)
    models <- rep("echam5", t=3)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm", t=3)
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_main_mm_plev", t=3)
    #names_short <- rep("Hol-T", t=3)
    names_short <- rep("Hol-T_below_minus_above_1.5_sd_NPI", t=3)
    #names_short <- rep("Hol-Tx10", t=3)
    #names_legend <- rep("Ladoga Hol-Tx10", t=3)
    #fromsf <- rep("0001", t=3) # Hol-Tx10
    fromsf <- rep("0004", t=3) # Hol-T
    tosf <- rep("7000", t=3) # Hol-T
    #tosf <- rep("7001", t=3) # Hol-Tx10
    new_origins <- rep(-6996, t=3) # Hol-Tx10
    #new_origins <- rep(-7000, t=3) # Hol-Tx10
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas_an <- c(10, 10, 10)
    #seasonsf <- rep("annual", t=3)
    seasonsf <- rep("NDJFMmean", t=3)
    #seasonsp <- "Feb" # cdo's default season timestamps: Feb, May, Aug, Nov
    #seasonsp <- "May"
    #seasonsp <- "Jun"
    #seasonsp <- "Aug"
    #seasonsp <- "Nov"
    #seasonsp <- "Dec"
    #varnamesin <- c("wind10_gt_1.5_sd_NPI", "u10_gt_1.5_sd_NPI", "v10_gt_1.5_sd_NPI")
    varnamesin <- c("wind10_lt_1.5_sd_NPI_minus_wind10_gt_1.5_sd_NPI", 
                     "u10_lt_1.5_sd_NPI_minus_u10_gt_1.5_sd_NPI", 
                     "v10_lt_1.5_sd_NPI_minus_v10_gt_1.5_sd_NPI")
    #levsf <- c("_int1000-100hPa", "_int1000-100hPa", "_int1000-100hPa")
    varnames_uv <- list(list(uv=1, u=2, v=3))
    varnames_out_samedims <- "wind10_u10_v10"
    #varnames_out_samedims <- "quv"
    #names_legend_samedims <- c("qu", "qv", "quv")
    #modes <- rep("select", t=3)
    modes <- rep("timmean", t=3)
    #areas <- rep("ladoga_remapnn", t=3)
    areas <- rep("NAsiberia", t=3)
    #regboxes <- lapply(vector("list", l=3), base::append, list(regbox="NAsiberia"))

} else if (F) { # echam restart issue
    models <- rep("echam6", t=3)
    prefixes <- paste0("awi-cm-1-1-lr_historical_", c("6hr", "day", "mon"))
    names_short <- c("6hr", "day", "mon")
    fromsf <- rep(1850, t=3)
    tosf <- rep(1851, t=3)
    names_legend <- paste0("temp2 ", names_short)
    varnamesin <- rep("temp2", t=3)
    modes <- rep("fldmean", t=3)

# =====================================
# 4 settings
} else if (F) { # sofia
    # hist cols: c("hist_7001"="#199d76", "hist_7003"="#746fb2", "hist_7005"="#e6aa01", "hist_9211"="#656565")
    workpath <- "/work/ab1095/a270073"
    #workpath <- "/work/ba1103/a270073"
    if (T) { 
        names_short <- c("piControl2", "fwf_01")
        #names_legend <- c("ctrl", "fwf_01")
        names_legend <- c("piControl", "antwater")
        #fromsf <- c(1850, 1870)
        fromsf <- c(1870, 1870)
        #fromsf <- c(1948, 1948)
        #tosf <- c(1967, 1967)
        #fromsf <- c(1951, 1951)
        #tosf <- c(1970, 1967)
        tosf <- c(1970, 1970)
        #tosf <- c(2100, 1967)
        #tosf <- c(2100, 1970)
        #fromsp <- c(1870, NA)
        #tosp <- c(1970, NA)
    } else if (F) {
        if (F) { # hist_7001
            names_short <- c("historical3_and_ssp585_2", "hist_7001")
            names_legend <- c("historical+ssp585", "hist-antwater-70-01")
        } else if (T) { # hist_7005
            names_short <- c("historical3_and_ssp585_2", "hist_7005")
            names_legend <- c("historical+ssp585", "hist-antwater-70-05")
        }
        fromsf <- c(1850, 1970)
        tosf <- c(2100, 2020)
        fromsp <- c(1970, NA)
        tosp <- c(2020, NA)
    } else if (F) {
        if (T) { # hist_7001
            names_short <- c("historical3_and_ssp585_2", "hist_7001", "hist_7005", "hist_9211")
            names_legend <- c("historical+ssp585", "hist-antwater-70-01", "hist-antwater-70-05", "hist-antwater-92-11")
            cols <- mycols(4)[c(1, 4, 3, 2)]
        }
        fromsf <- c(1850, 1970, 1970, 1992)
        tosf <- c(2100, rep(2020, t=3))
        fromsp <- c(1970, rep(NA, t=3))
        tosp <- c(2020, rep(NA, t=3))
    } else if (F) { 
        names_short <- c("piControl2", "antwater")
        names_legend <- c("ctrl", "0.1 Sv paleo")
        fromsf <- c(1961, 1961)
        tosf <- c(1970, 1970)
    } else if (F) {
        names_short <- c("fwf_01", "norm_yes", "norm_no")
        names_legend <- names_short
        fromsf <- rep(1870, t=length(names_short))
        tosf <- c(1970, rep(1879, t=2))
        tosp <- c(1879, NA, NA)
    } else if (F) {
        names_short <- c("piControl2", "antwater", "antwater_1sv", "antwater_10sv")
        names_legend <- c("ctrl", "0.1 Sv paleo", "1 Sv paleo", "10 Sv paleo")
        fromsf <- rep(1870, t=length(names_short))
        tosf <- c(1970, 1970, rep(1879, t=2))
    } else if (F) {
        names_short <- c("piControl2", "antwater", "ant_01sv", "ant_01sv_nocorr")
        names_legend <- c("ctrl", "0.1 Sv paleo", "0.1 Sv", "0.1 Sv no corr")
        fromsf <- rep(1870, t=length(names_short))
        #tosf <- rep(1879, t=4)
        #tosp <- c(1879, rep(NA, t=3))
        #tosp <- rep(1879, t=4)
        #tosf <- c(1970, rep(1879, t=3))
        #tosf <- c(1970, 1970, 1917, 1879)
        tosf <- c(1970, 1970, 1970, 1879)
        #tosf <- c(1970, 1970, rep(1879, t=2))
    } else if (F) {
        names_short <- c("piControl2", "ant_01sv", "antwater", "ptr_01", "fwf_01")
        names_legend <- c("ctrl", "landice", "paleo", "ptr", "fwf")
        fromsf <- rep(1870, t=length(names_short))
        tosf <- rep(1879, t=length(names_short))
        tosf[2] <- 1970
    } else if (F) {
        names_short <- c("piControl2", "fwf_01", "runoff_01")
        names_legend <- c("ctrl", "fwf", "runoff")
        fromsf <- rep(1870, t=length(names_short))
        tosf <- rep(1879, t=length(names_short))
    } else if (T) { # picontrol, antwater, historical, hist_7001, ssp585
        names_short <- c("piControl2", "fwf_01", "historical3", "hist_7001", "ssp585_2")
        names_legend <- c("piControl", "antwater", "historical", "hist-antwater-70-01", "ssp585")
        fromsf <- c(1850, 1870, 1850, 1970, 2015)
        tosf <- c(2100, 1970, 2014, 1992, 2100)
        fromsp <- c(1870, NA, 1870, NA, NA)
        tosp <- c(2014, NA, NA, NA, 2020)
    }
    #tosp <- rep(1879, t=length(names_short))
    #tosp <- rep(1967, t=length(names_short))
    #models <- rep("echam6", t=length(names_short))
    models <- rep("fesom", t=length(names_short))
    prefixes <- paste0("awi-esm-1-1-lr_kh800_", names_short)
    #varnamesin <- rep("temp2", t=length(models))
    #varnamesin <- rep("thetao", t=length(models))
    #varnamesin <- rep("sos", t=length(models))
    #varnamesin <- rep("so", t=length(models))
    #varnamesin <- rep("runoff", t=length(models))
    #varnamesin <- rep("virtual_salt", t=length(models))
    varnamesin <- rep("wnet", t=length(models))
    #varnamesin <- rep("wfo", t=length(models))
    #varnamesin <- rep("wnet", t=length(models))
    #varnamesin <- c("wnet", rep("wnet_fwf", t=3))
    #varnames_out_samedims <- "wnet_fwf"
    #names_legend_samedims <- names_legend
    #depths <- rep("2000", t=length(models))
    #depths <- rep("0-5900", t=length(models))
    #modes <- rep("timmean", t=length(models))
    #modes <- rep("fldmean", t=length(models))
    modes <- rep("fldint", t=length(models))
    #modes <- rep("depth", t=length(models))
    #seasonsf <- rep("annual", t=length(models))
    #remove_mean_froms <- rep(1870, t=length(models))
    #remove_mean_tos <- remove_mean_froms
    #areas <- rep("S60", t=length(models))
    #areas <- rep("g19_SH-HL", t=length(models))
    if (all(modes == "timmean")) prefixes <- paste0(prefixes, "_regular_dx1.000_dy1.000")
    #remove_setting <- "piControl2"

} else if (F) { # reproducibility issue bfritzsch
    if (F) { # with KMP
        models <- rep("fesom", t=4)
        names_short <- c("piControl_LUtrans1850_r8", "piControl_LUtrans1850_new", "piControl_LUtrans1850_newb", "piControl_LUtrans1850_no_kmp")
        names_legend <- c("Jul22", "Dec22", "Dec22 repeat", "Dec22 repeat wout KMP")
    } else if (T) { # wout KMP
        models <- rep("fesom", t=3)
        names_short <- c("piControl_LUtrans1850_r8", "piControl_LUtrans1850_new", "piControl_LUtrans1850_newb")
        names_legend <- c("Jul22", "Dec22", "Dec22 repeat")
    }
    prefixes <- paste0("awi-esm-1-1-lr_kh800_", names_short)
    varnamesin <- rep("tos", t=length(models))
    modes <- rep("fldmean", t=length(models))
    fromsf <- rep(3001, t=length(models))
    tosf <- rep(3005, t=length(models))
    tunit <- "piControl year"

} else if (F) { # piControl esm-piControl chunks
    models <- rep("recom", t=5)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl_LUtrans1850", "awi-esm-1-1-lr_kh800_esm-piControl",
                  "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest", "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2",
                  "awi-esm-1-1-lr_kh800_esm-piControl2")
    names_legend <- c("piControl mistral Talk rest on", "esm-piControl mistral Talk rest on",
                      "esm-piControl mistral Talk rest off (reinit CO2)", "esm-piControl levante Talk rest off",
                      "esm-piControl levante Talk rest off continue")
    names_short <- c("piControl_LUtrans1850", "esm-piControl", 
                     "esm-piControl_wout_talk_rest", "esm-piControl_wout_talk_rest2",
                     "esm-piControl2")
    plotprefix <- "awicm1-recom_esm-piControl_chunks"
    varnamesin <- rep("aCO2", t=5)
    modes <- rep("select", t=5)
    fromsf <- c(2951, 3001, 3151, 3208, 1850)
    tosf <- c(3062, 3150, 3227, 3945, 1968)
    #new_origins <- c(1003, 1052, 1202, 1259) # picontrol years
    new_origins <- c(-112, 1, 151, 208, 946) # esm-picontrol years
    tosp <- c(NA, NA, 207, NA, NA) # remove overlap
    #tunit <- "piControl year"
    tunit <- "esm-piControl year"

} else if (F) { # reccap A B C D
    workpath <- "/work/ollie/cdanek"
    models <- rep("fesom", t=4)
    names_legend <- c("A", "B", "C", "D")
    prefixes <- paste0("reccap_", names_legend)
    names_short <- prefixes
    varnamesin <- rep("CO2f", t=4)
    center_ts <- T
    modes <- rep("fldint", t=4)
    fromsf <- rep(1958, t=4)
    tosf <- rep(2019, t=4)

} else if (F) { # awi-esm-1-1-lr_kh800 piControl and historical w/wout river flux adjustment rfa
    models <- rep("echam6", t=4)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl_rfa", "awi-esm-1-1-lr_kh800_piControl", 
                  "awi-esm-1-1-lr_kh800_historical_rfa", "awi-esm-1-1-lr_kh800_historical")
    names_short <- c("piControl_rfa", "piControl", "historical_rfa", "historical")
    names_legend <- c("piControl + L20", "piControl", "historical + L20", "historical")
    ltys <- c(2, 3, 1, 2)
    cols <- c(1, 1, 2, 2)
    varnamesin <- rep("co2_flx_ocean", t=4)
    fromsf <- c(2686, 2686, 1850, 1850)
    tosf <- c(3000, 3000, 2014, 2014)
    new_origins <- c(1850, 1850, NA, NA) # 2686=1850
    fromsp <- c(1980, 1980, 1980, 1980)
    tosp <- c(2022, 2022, NA, NA)
    modes <- rep("fldint", t=4)
    #areas <- rep("reccap2_atlantic", t=4)
    #areas <- rep("reccap2_pacific", t=4)
    #areas <- rep("reccap2_indian", t=4)
    #areas <- rep("reccap2_arctic", t=4)
    #areas <- rep("reccap2_southern", t=4)
    # sub-atlantic:
    #areas <- rep("reccap2_na_spss", t=4)
    #areas <- rep("reccap2_na_stss", t=4)
    #areas <- rep("reccap2_na_spss", t=4)
    #areas <- rep("reccap2_na_stps", t=4)
    #areas <- rep("reccap2_aequ", t=4)
    #areas <- rep("reccap2_sa_stps", t=4)
    areas <- rep("reccap2_med", t=4)
    # sub-pacific:
    #areas <- rep("reccap2_np_spss", t=4)
    #areas <- rep("reccap2_np_stss", t=4)
    #areas <- rep("reccap2_np_stps", t=4)
    #areas <- rep("reccap2_pequ_w", t=4)
    #areas <- rep("reccap2_pequ_e", t=4)
    #areas <- rep("reccap2_sp_stps", t=4)

} else if (F) { # mpi-esm* vs awi* mlds semmler et al.
    workpath <- "/work/ab0246/a270073"
    if (F) {
        models <- rep("fesom_vs_mpiom1", t=4)
        addland <- F
        prefixes <- c("awi-esm-1-1-lr_minus_mpi-esm1-2-lr_remapbil_r1440x720_conv180_piControl",
                      "awi-cm-1-1-mr_minus_mpi-esm1-2-hr_remapbil_r1440x720_conv180_piControl",
                      "awi-esm-1-1-lr_minus_mpi-esm1-2-lr_remapbil_r1440x720_conv180_1percCO2",
                      "awi-cm-1-1-mr_minus_mpi-esm1-2-hr_remapbil_r1440x720_conv180_1percCO2")
        names_short <- c("AWI-MPI_PI_LR", "AWI-MPI_PI_HR", "AWI-MPI_1pcnt_LR", "AWI-MPI_1pcnt_HR")
        names_legend <- c("AWI-MPI PI LR", "AWI-MPI PI HR", "AWI-MPI 1% LR", "AWI-MPI 1% HR")
        cols <- c(1, 2, 1, 2)
        ltys <- c(1, 1, 2, 2)
    } else if (T) {
        models <- rep(c("fesom", "mpiom1"), t=2)
        if (F) { # remapbil
            prefixes <- c("awi-esm-1-1-lr_1percCO2_minus_piControl_regular_dx0.250_dy0.250",
                          "mpi-esm1-2-lr_remapbil_r1440x720_conv180_1percCO2_minus_piControl",
                          "awi-cm-1-1-mr_1pctCO2_minus_piControl_regular_dx0.250_dy0.250",
                          "mpi-esm1-2-hr_remapbil_r1440x720_conv180_1percCO2_minus_piControl")
        } else if (T) { # remapnn
            prefixes <- c("awi-esm-1-1-lr_1percCO2_minus_piControl_regular_dx0.250_dy0.250",
                          "mpi-esm1-2-lr_remapnn_r1440x720_conv180_1percCO2_minus_piControl",
                          "awi-cm-1-1-mr_1pctCO2_minus_piControl_regular_dx0.250_dy0.250",
                          "mpi-esm1-2-hr_remapnn_r1440x720_conv180_1percCO2_minus_piControl")
        }
        names_short <- c("AWI-LR", "MPI-LR", "AWI-MR", "MPI-HR")
        names_legend <- paste0(names_short, " response")
        ignore_vars <- c(ignore_vars, "uo", "vo")
    }
    #varnamesin <- rep("omldamax", t=4)
    varnamesin <- rep("hvel", t=4)
    #levs <- rep("0-200m_6-182.5", t=4)
    levsf <- rep(c("_0-200m", "_sellevel_6-182.5"), t=2)
    fromsf <- rep(1910, t=4)
    tosf <- rep(1930, t=4)
    #modes <- rep("zonmean_timmean_monmax", t=4)
    #modes <- rep("vertmean_timmean", t=4)
    modes <- rep(c("timmean", "vertmean_timmean"), t=2)

} else if (F) { # awicm1-recom piControl vs historical vs esm-piControl
    models <- rep("echam6", t=4)
    #models <- rep("fesom", t=4)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl_og",
                  "awi-esm-1-1-lr_kh800_piControl",
                  "awi-esm-1-1-lr_kh800_historical",
                  "awi-esm-1-1-lr_kh800_esm-piControl_restartall")
    names_short <- paste0("ar1_", c("piControl_og", "piControl", "historical", "esm-piControl"))
    names_legend <- c("piControl-spinup", "piControl", "historical", "esm-piControl")
    ltys <- c(2, 1, 1, 1)
    cols <- c(1, 1, 2, 3)
    #varnamesin <- rep("temp2", t=4)
    varnamesin <- rep("co2_flx_ocean", t=4)
    #varnamesin <- rep("co2_flx_land", t=4)
    #n_mas_an <- rep(5, t=4)
    #varnamesin <- rep("tos", t=4)
    #varnamesin <- rep("aCO2", t=4)
    fromsf <- c(1950, 2686, 1850, 2686)
    #tosf <- c(2685, 2822, 1986, 2796)
    tosf <- c(2685, 2824, 1986, 2796)
    new_origins <- c(1114, 1850, NA, 1850) # pi before 1850, esm-piControl from 1850
    fromsp <- c(1800, NA, NA, NA)
    #modes <- rep("select", t=4)
    #modes <- rep("timmean", t=4)
    #modes <- rep("fldmean", t=4)
    modes <- rep("fldint", t=4)

} else if (F) { # awi-esm-1-1-lr deck
    workpath <- "/work/ab0246/a270073"
    models <- rep("echam6", t=4)
    #models <- rep("fesom", t=4)
    if (F) { # awi-cm-1-1-lr
        prefixes <- paste0("awi-cm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("awi-cm-1-1-lr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (F) { # awi-esm-1-1-lr
        prefixes <- paste0("awi-esm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("awi-esm-1-1-lr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (F) { # awi-esm-1-2-lr
        prefixes <- paste0("awi-esm-1-2-lr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("awi-esm-1-2-lr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (F) { # awi-cm-1-1-mr
        prefixes <- paste0("awi-cm-1-1-mr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("awi-cm-1-1-mr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (F) { # mpi-esm1-2-lr
        prefixes <- paste0("mpi-esm1-2-lr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("mpi-esm1-2-lr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (T) { # mpi-esm1-2-hr
        prefixes <- paste0("mpi-esm1-2-hr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("mpi-esm1-2-hr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    }
    text_cols <- c("black", "#E41A1C", "#377EB8", "#1B9E77")
    scatterpchs <- c(4, 16, 16, 16)
    #varnamesin <- rep("temp2", t=4)
    #codes <- c(167, "", "", "")
    #varnamesin <- rep("tas", t=4)
    #varnamesin <- rep("srad0", t=4)
    varnamesin <- rep("toa_imbalance", t=4)
    #varnamesin <- rep("tau_aero_550", t=4)
    #codes <- c(11, "", "", "")
    #varnamesin <- rep("srad0d", t=4)
    #codes <- c(184, "", "", "")
    #varnamesin <- rep("MOCw", t=4)
    #areas <- rep("NA", t=4)
    #depths <- rep("0-5900", t=4)
    #moc_lats <- c(26.5, 41)
    #varnamesin <- rep("siarean", t=4)
    #areas <- rep("arctic", t=4)
    #varnamesin <- rep("mlotst", t=4)
    #varnamesin <- rep("thetao", t=4)
    #varnamesin <- rep("so", t=4)
    #varnamesin <- rep("potdens", t=4)
    #areas <- rep("LSstolpe18", t=4)
    #areas <- rep("GIN2", t=4)
    #depths <- rep("0-5900", t=4)
    #depth_fromsp <- rep(-3500, t=4)
    #varnamesin <- rep("tos", t=4)
    #postpaths <- paste0(workpath, "/post/", models, "/regular_grid/ltm/", mode, "/", varnamesin)
    #reg_dxs <- reg_dys <- rep("0.250", t=4)
    if (T) { # transient deck first 150 years
        if (F) { # awi-esm-1-1-lr
            if (F) { # pi before deck
                #fromsf <- c(1855, 1850, 1850, 1850) # PI-CTRL5 wrong labels
                #tosf <- c(1954, 2014, 2099, 2099) # PI-CTRL5 wrong labels
                fromsf <- c(1842, 1850, 1850, 1850) # PI-CTRL5 correct labels
                tosf <- c(1941, 2014, 2099, 2099) # PI-CTRL5 correct labels
                new_origins <- c(1750, NA, NA, NA) # plot pi before historical on time axis
                tosp <- c(NA, NA, rep(1999, t=2))
            } else if (T) { # pi and deck
                fromsf <- c(1942, 1850, 1850, 1850) # PI-CTRL6
                tosf <- c(2091, 2014, 2099, 2099)
                new_origins <- c(1850, NA, NA, NA)
                #tosp <- rep(1999, t=4) # ECS
                tosp <- rep(1989, t=4) # TCR
            }
        } else if (F) { # awi-esm-1-2-lr lars
            fromsf <- c(1046, 1850, 1850, 1850)
            tosf <- c(1195, 2014, 1999, 1999)
            new_origins <- c(1850, NA, NA, NA)
            tosp <- c(NA, 1999, NA, NA)
        } else if (F) { # awi-cm-1-1-mr
            fromsf <- c(2650, 1850, 1850, 1850)
            tosf <- c(2799, 2014, 1999, 1999)
            new_origins <- c(1850, NA, NA, NA)
            #tosp <- c(NA, 1999, NA, NA) # ECS
            tosp <- rep(1989, t=4) # TCR
        } else if (T) { # mpi-esm1-2-lr and mpi-esm1-2-hr
            fromsf <- c(1850, 1850, 1850, 1850)
            tosf <- c(1999, 2014, 1999, 1999)
            #tosp <- c(NA, 1999, NA, NA) ECS
            tosp <- rep(1989, t=4) # TCR
        }
    } else if (F) { # tranient pi last 30
        fromsf <- c(1912, 1850, 1850, 1850)
        tosf <- c(1941, 2014, 2099, 2099)
        new_origins <- c(1912-91, NA, NA, NA)
        fromsp <- c(1849-29, 1850, 1850, 1850)
        tosp <- c(1849, 2014, 2099, 2099)
    } else if (F) { # ltm last 30
        fromsf <- c(1912, 1985, 2070, 2070)
        tosf <- c(1941, 2014, 2099, 2099)
    }
    #remove_mean_froms <- c(1849, 1850, 1850, 1850)
    #remove_mean_tos <- remove_mean_froms
    add_linear_trend <- c(F, F, F, T)
    #add_linear_trend <- c(F, F, T, T)
    modes <- rep("fldmean", t=4)
    seasonsf <- rep("annual", t=4)
    #seasonsp <- rep("JFM", t=4) 
    #seasonsp <- rep("Mar", t=4)
    #seasonsp <- rep("Jul", t=4)
    #n_mas_an <- rep(5, t=4)
    #n_mas_an <- rep(3, t=4)
    if (T) {
        names_legend <- c("piControl", 
                          "historical", 
                          eval(substitute(expression(paste("1%CO"[2])))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2])))))
    } else if (F) {
        names_legend <- c("piControl last 30 years mean",
                          #"piControl last 100 years mean",
                          paste0("historical ", fromsf[2], "-", tosf[2], " mean"),
                          #paste0("historical ", fromsp[2], "-", tosp[2], " mean"),
                          eval(substitute(expression(paste("1%CO"[2], " last 30 years mean")))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " last 30 years mean")))))
    } else if (F) {
        names_legend <- c(paste0("piControl ", seasonsp[1], " ", fromsp[1], "-", tosp[1]),
                          paste0("historical ", seasonsp[2], " ", fromsp[2], "-", tosp[2]),
                          eval(substitute(expression(paste("1%CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsp[3], from=fromsp[3], to=tosp[3]))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsp[4], from=fromsp[4], to=tosp[4]))))
    }
    
} else if (F) { # awi-esm-1-1-lr deck and lgm
    if (F) {
        prefixes <- c("historical_echam6_echammon_awi-esm-1-1-lr",
                      "1percCO2_echam6_echammon_awi-esm-1-1-lr",
                      "4CO2_echam6_echammon_awi-esm-1-1-lr",
                      "MM_01.01_echam_awi-esm-1-1-lr_lgm")
    } else if (F) {
        prefixes <- c("historical_echam6_echam_awi-esm-1-1-lr",
                      "1percCO2_echam6_echam_awi-esm-1-1-lr",
                      "4CO2_echam6_echam_awi-esm-1-1-lr",
                      "MM_01.01_echam_awi-esm-1-1-lr_lgm")
    }
    models <- rep("echam6", t=4)
    names_short <- c("hist", "1pctCO2", "abrupt-4xCO2", "lgm") 
    fromsf <- c(1850, 1850, 1850, 3537)
    #fromsf <- c(rep(1985, t=3), 3843)
    #fromsf <- c(1985, 2070, 2070, 3843)
    #tosf <- c(rep(2014, t=3), 3872)
    tosf <- c(2014, 2099, 2099, 3872)
    seasonsf <- rep("Jan-Dec", t=4)
    if (F) {
        names_legend <- c("historical", 
                          eval(substitute(expression(paste("1%CO"[2])))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2])))),
                          "LGM")
    } else if (F) {
        names_legend <- c(paste0("historical ", seasonsf[1], " ", fromsf[1], "-", tosf[1]),
                          eval(substitute(expression(paste("1%CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsf[2], from=fromsf[2], to=tosf[2]))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsf[3], from=fromsf[3], to=tosf[3]))),
                          paste0("LGM ", seasonsf[4], " ", fromsf[4], "-", tosf[4]))
    }
    n_mas_an <- rep(5, t=4)
    #varnamesin <- rep("temp2", t=4)
    varnamesin <- rep("srad0", t=4)
    
} else if (F) { # compare cdo remap* Hol-T*
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=4)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=4)
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "Hol-T_stschuett_echam5_wiso",
    #              "cosmos-aso-wiso_Hol-Tx10_wiso_mm", "cosmos-aso-wiso_Hol-Tx10_wiso_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-Tx10_timeser_ext", 
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_15_remapcon2_r120x101_gt_0.15_times_area",
    #              "cosmos-aso-wiso_Hol-Tx10_timeser_ext",
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_15_remapcon2_r120x101_gt_0.15_times_area")
    #prefixes <- c(rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=2), rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=2))
    models <- rep("echam5", t=4)
    #models <- rep("mpiom1", t=4)
    #names_short <- rep("Hol-Tx10", t=4)
    names_short <- rep("Hol-T", t=4)
    #names_short <- c("Hol-T_direct", "Hol-T_post", "Hol-Tx10_direct", "Hol-Tx10_post")
    #names_short <- c("c64_ICEARE_ARC", "NH", "c144_ICEARE_SO", "SH")
    #names_short <- c(rep("Hol-T", t=2), rep("Hol-Tx10", t=2))
    #names_legend <- names_short
    #names_legend <- c("Hol-T_direct", "Hol-T_post", "Hol-Tx10_direct", "Hol-Tx10_post")
    #names_legend <- c("nn", "bil", "bic", "dis")
    #names_legend <- c("Hol-T Ladoga", "Hol-T LadogaLand", "Hol-Tx10 Ladoga", "Hol-Tx10 LadogaLand")
    names_legend <- c("DJF", "MAM", "JJA", "SON")
    #fromsf <- rep("0001", t=4) # beginning of Hol-Tx10
    fromsf <- rep("0004", t=4) # beginning of Hol-T
    #fromsf <- c("0004", "0004", "0001", "0001")
    #tosf <- rep("6821", t=4)
    tosf <- rep("7000", t=4) # end of Hol-T
    #tosf <- rep("7001", t=4) # end of Hol-Tx10
    #tosf <- c("7000", "7000", "7001", "7001")
    #seasonsf <- rep("annual", t=4)
    #seasonsf <- c("Jan-Dec", "annual", "Jan-Dec", "annual")
    #seasonsf <- rep("yearsum", t=4)
    #seasonsf <- rep("seassum", t=4)
    #seasonsp <- rep("Jul", t=4)
    #seasonsp <- rep("Jan", t=4)
    seasonsp <- c("DJF", "MAM", "JJA", "SON")
    cols <- c(DJF="blue", MAM="darkgreen", JJA="red", SON="brown")
    remove_mean_froms <- rep(-6996, t=4)
    remove_mean_tos <- rep(-6996, t=4)
    #new_origins <- rep(-6996, t=4)
    #new_origins <- rep(-7000, t=4)
    #new_origins <- c(-6996, -6996, -7000, -7000)
    new_origins <- rep(-6996, t=4)
    time_ref <- 1950 # any string, e.g. "BP", or number
    #n_mas_an <- rep(2.5, t=4)
    n_mas_an <- rep(7.5, t=4)
    #n_mas_an <- rep(10, t=4)
    #varnamesin <- rep("temp2", t=4)
    #varnamesin <- rep("tsurf", t=4)
    #varnamesin <- rep("aprt", t=4)
    #varnamesin <- rep("aprl", t=4)
    #varnamesin <- rep("aprc", t=4)
    #varnamesin <- rep("aprs", t=4)
    #varnamesin <- rep("tsurfaprt", t=4)
    #varnamesin <- c("wisoaprt_d", "wisoaprt_d", "wisoaprt_d", "wisoaprt_d_post")
    #varnamesin <- rep("wisoaprt_d_post", t=4)
    #levs <- c(2, 2, 2, 2)
    #varnamesin <- c("c64_ICEARE_ARC", "SICOMO", "c144_ICEARE_SO", "SICOMO")
    modes <- rep("select", t=4)
    #modes <- c("select", "fldsum", "select", "fldsum") 
    #modes <- rep("yearsum", t=4)
    #modes <- rep("seassum", t=4)
    #varnames_out_samedims <- "SICOMO"
    #names_legend_samedims <- names_legend
    #cols_samedims <- 1:4 
    #ltys_samedims <- rep(1, t=4)
    #areas <- c("global", "northern_hemisphere", "global", "southern_hemisphere")
    #areas <- c("ladoga_remapnn", "ladoga_remapbil", "ladoga_remapbic", "ladoga_remapdis")
    #areas <- c("shuchye_remapnn", "shuchye_remapbil", "shuchye_remapbic", "shuchye_remapdis")
    #areas <- c("levinson-lessing_remapnn", "levinson-lessing_remapbil", "levinson-lessing_remapbic", "levinson-lessing_remapdis")
    #areas <- c("taymyr_remapnn", "taymyr_remapbil", "taymyr_remapbic", "taymyr_remapdis")
    #areas <- c("emanda_remapnn", "emanda_remapbil", "emanda_remapbic", "emanda_remapdis")
    #areas <- c("elgygytgyn_remapnn", "elgygytgyn_remapbil", "elgygytgyn_remapbic", "elgygytgyn_remapdis")
    #areas <- rep(c("ladoga_remapnn", "ladogaLand_remapnn"), t=2)
    areas <- rep("ladoga_remapnn", t=4)

} else if (F) { # awi-esm-1-1-lr cold/warm atmosphere restart problem
    models <- rep("echam6", t=4)
    prefixes <- c("awi-esm-1-1-lr_piControl_477_ollie_echam6",
                  "awi-esm-1-1-lr_midHolocene_warm_echam6",
                  "awi-esm-1-1-lr_midHolocene_cold_echam6",
                  "awi-esm-1-1-lr_piControl_mistral_esm_echam6")
    names_short <- c("PIollie", "MHwarm", "MHcold", "PImistral")
    fromsf <- c(2700, 3106, 3123, 1842)
    tosf <- c(3249, 3205, 3166, 1941)
    new_origins <- c(1, 551, 551, 551)
    #fromsp <- c(546, rep(NA, t=3))
    #tosp <- c(NA, rep(555, t=3))
    n_mas_an <- rep(3, t=4)
    varnamesin <- c("temp2", "tas", "temp2", "tas")
    modes <- rep("fldmean", t=4)
    varnames_out_samedims <- "temp2"
    names_legend_samedims <- names_short
    cols_samedims <- 1:4 
    ltys_samedims <- rep(1, t=4)
    areas <- rep("global", t=4)

} else if (F) { # echam restart issue
    models <- rep("echam6", t=4)
    prefixes <- paste0("awi-cm-1-1-lr_historical_", c("3hr", "6hr", "day", "mon"))
    names_short <- c("3hr", "6hr", "day", "mon")
    #prefixes <- paste0("awi-esm-1-1-lr_historical_", c("3hr", "6hrPlev", "day", "Amon"))
    #names_short <- c("3hr", "6hrPlev", "day", "Amon")
    fromsf <- rep(1850, t=4)
    tosf <- rep(1851, t=4)
    #names_legend <- paste0("st ", names_short)
    #varnamesin <- rep("st", t=4)
    #levs <- rep(47, t=4)
    names_legend <- paste0("lsp ", names_short)
    varnamesin <- rep("lsp", t=4)
    #names_legend <- paste0("tas ", names_short)
    #varnamesin <- rep("tas", t=4)
    modes <- rep("fldmean", t=4)

} else if (F) { # phd stuff divuvt* fldint 4 settings
    workpath <- "/work/ba0941/a270073"
    models <- rep("fesom", t=4)
    if (T) {
        prefixes <- rep("Low01_sub_lsea_s52", t=4)
        names_short <- paste0("Low01_s52", c("divuvttot_plus_divuvsgsttot", "divuvt", "divuvteddy_plus_divuvsgsttot", "divuvsgsttot"))
        areas <- rep("LS30l2", t=4)
        #add_legend <- F; add_legend_right_yaxis <- F; add_legend_left_yaxis_before <- F
    } else if (F) {
        prefixes <- rep("LSea5_sub_lsea_s5", t=4)
        names_short <- paste0("LSea5_s5", c("divuvttot_plus_divuvsgsttot", "divuvt", "divuvteddy_plus_divuvsgsttot", "divuvsgsttot"))
        areas <- rep("LS30l", t=4)
        add_legend <- F; add_legend_right_yaxis <- F; add_legend_left_yaxis_before <- F
    }
    names_legend <- c("divuvttot_plus_divuvsgsttot", "divuvt", "divuvteddy_plus_divuvsgsttot", "divuvsgsttot")
    varnamesin <- c("divuvttot_plus_divuvsgsttot", "divuvt", "divuvteddy_plus_divuvsgsttot", "divuvsgsttot")
    cols <- c(1, 3, 2, 2)
    ltys <- c(1, 1, 1, 2)
    varnames_out_samedims <- "divuvt_budget"
    names_legend_samedims <- names_legend
    ignore_vars <- c(ignore_vars, "dx_uXtemp_meanint", "dy_vXtemp_meanint")
    depthsf <- rep("_int0-MLD", t=4)
    fromsf <- rep(1948, t=4)
    tosf <- rep(2009, t=4)
    n_mas_an <- rep(3, t=4)
    modes <- rep("fldint", t=4)

} else if (F) { # phd stuff lorenz energy cycle fldint 4 settings
    workpath <- "/work/ba0941/a270073"
    models <- rep("fesom", t=4)
    if (T) {
        prefixes <- rep("Low01_sub_lsea_s52", t=4)
        names_short <- paste0("Low01_s52", c("FeKe", "HRS", "VRS", "wbeddy"))
        depthsf <- c("_0m", rep("_int0-3600m", t=3))
        areas <- rep("LS30l2", t=4)
        add_legend <- F
        add_legend_right_yaxis <- F
        add_legend_left_yaxis_before <- F
    } else if (F) {
        prefixes <- rep("LSea5_sub_lsea_s5", t=4)
        names_short <- paste0("LSea5_s5_", c("FeKe", "HRS", "VRS", "wbeddy"))
        depthsf <- c("_0m", rep("_int0-4150m", t=3))
        areas <- rep("LS30l", t=4)
    }
    varnamesin <- c("FeKe", "HRS", "VRS", "wbeddy")
    varnames_out_samedims <- "lorenz_energy_cycle"
    names_legend_samedims <- varnamesin
    fromsf <- rep(1948, t=4)
    tosf <- rep(2009, t=4)
    n_mas_an <- rep(3, t=4)
    modes <- rep("fldint", t=4)

# ==================================================
## 5 settings 
} else if (F) { # awicm1-recom concentration-driven and emission-driven 
    models <- rep("recom", t=5)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl",
                  "awi-esm-1-1-lr_kh800_historical2",
                  "awi-esm-1-1-lr_kh800_esm-piControl",
                  "awi-esm-1-1-lr_kh800_esm-piControl2",
                  "awi-esm-1-1-lr_kh800_esm-hist")
    names_short <- c("piControl", "historical", "esm-piControl", "esm-piControl2", "esm-hist")
    names_legend <- c("piControl", "historical", "esm-piControl-spinup", "esm-piControl", "esm-hist")
    varnamesin <- rep("aCO2", t=5)
    fromsf <- c(1950, 1850, 3001, 1850, 1850)
    tosf <- c(3000, 2014, 3945, 2063, 2014)
    new_origins <- c(1, 737, 1052, 1997, 1997)
    tunit <- "piControl-year"
    modes <- rep("select", t=5)

} else if (F) { # awicm1-recom concentration-driven and emission-driven 
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl",
                  "awi-esm-1-1-lr_kh800_historical2",
                  "awi-esm-1-1-lr_kh800_ssp126",
                  "awi-esm-1-1-lr_kh800_ssp245",
                  "awi-esm-1-1-lr_kh800_ssp534-over",
                  "awi-esm-1-1-lr_kh800_ssp585",
                  "awi-esm-1-1-lr_kh800_esm-piControl",
                  "awi-esm-1-1-lr_kh800_esm-piControl2",
                  "awi-esm-1-1-lr_kh800_esm-hist",
                  "awi-esm-1-1-lr_kh800_esm-ssp126",
                  "awi-esm-1-1-lr_kh800_esm-ssp245",
                  "awi-esm-1-1-lr_kh800_esm-ssp370",
                  "awi-esm-1-1-lr_kh800_esm-ssp534os",
                  "awi-esm-1-1-lr_kh800_esm-ssp585")
    plotprefix <- "awicm-1-recom_conc_emis_driven"
    models <- rep("recom", t=length(prefixes))
    names_short <- gsub("awi-esm-1-1-lr_kh800_", "", prefixes)
    names_legend <- c("piControl", "historical", "ssp126", "ssp245", "ssp534os", "ssp585", 
                      "esm-piControl-spinup", "esm-piControl", "esm-hist", "esm-ssp126", "esm-ssp245", "esm-ssp370", "esm-ssp534os", "esm-ssp585")
    varnamesin <- rep("aCO2", t=length(prefixes))
    fromsf <- c(1950, 1850, rep(2015, t=4), 3001, 1850, 1850, rep(2015, t=5))
    tosf <- c(3000, 2014, rep(2100, t=4), 3945, 2100, 2014, rep(2100, t=5))
    if (F) {
        new_origins <- c(1, 737, rep(902, t=4), 1052, 1997, 1997, rep(2162, t=5))
        tunit <- "piControl year"
    } else if (T) {
        new_origins <- c(-1050, -314, rep(-149, t=4), 1, 946, 946, rep(1111, t=5))
        tunit <- "piControl/esm-piControl year"
        do_aggregate_plot_data <- F
    }
    modes <- rep("select", t=length(prefixes))

} else if (F) { # gregor_etal_2019 lsm comparison
    models <- rep("recom", t=5)
    prefixes <- rep("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2", t=5)
    names_short <- c("NH-HL", "NH-ST", "EQU", "SH-ST", "SH-HL")
    names_legend <- c("NH-HL", "NH-ST", "EQU", "SH-ST", "SH-HL")
    varnamesin <- rep("pCO2s", t=5)
    center_ts <- T
    n_mas_an <- rep(10, t=5)
    fromsf <- rep(3208, t=5)
    tosf <- rep(3574, t=5)
    tunit <- "esm-piControl year"
    new_origins <- rep(208, t=5)
    modes <- rep("fldmean", t=5)
    areas <- c("g19_NH-HL", "g19_NH-ST", "g19_EQU", "g19_SH-ST", "g19_SH-HL")

} else if (F) { # compare Hol-T* seasons and annual
    models <- rep("echam5", t=5)
    #models <- rep("mpiom1", t=5)
    #models <- rep("jsbach", t=5)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm", t=5)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=5)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm_plev", t=5)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_grb", t=5)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_veg_mm", t=5)
    names_short <- rep("Hol-T", t=5)
    names_legend <- c("DJF", "MAM", "JJA", "SON", "Annual")
    #fromsf <- rep("0001", t=5) # beginning of Hol-Tx10
    #fromsf <- rep("0004", t=5) # beginning of Hol-T
    #fromsf <- c("0004", "0004", "0001", "0001")
    fromsf <- rep("6971", t=5) # last 30 years of Hol-T
    #tosf <- rep("6821", t=5)
    tosf <- rep("7000", t=5) # end of Hol-T
    #tosf <- rep("7001", t=5) # end of Hol-Tx10
    #tosf <- c("7000", "7000", "7001", "7001")
    #seasonsf <- rep("annual", t=5)
    #seasonsf <- c("Jan-Dec", "annual", "Jan-Dec", "annual")
    #seasonsf <- rep("yearsum", t=5)
    #seasonsf <- rep("seassum", t=5)
    seasonsf <- c("DJF", "MAM", "JJA", "SON", "Jan-Dec")
    #seasonsf <- c("DJF", "MAM", "JJA", "SON", "annual")
    #seasonsf <- c("DJFmean", "MAMmean", "JJAmean", "SONmean", "annual")
    #seasonsf <- c("yearsum", rep("seassum", t=4))
    #seasonsp <- rep("Jul", t=5)
    #seasonsp <- rep("Jan", t=5)
    #seasonsp <- c("annual", "DJF", "MAM", "JJA", "SON")
    #seasonsp <- c("yearmean", "DJF", "MAM", "JJA", "SON")
    #seasonsp <- c("yearsum", "DJF", "MAM", "JJA", "SON")
    #remove_mean_froms <- rep(-6996, t=5)
    #remove_mean_tos <- rep(-6996, t=5)
    #new_origins <- rep(-6996, t=5)
    #new_origins <- rep(-7000, t=5)
    #new_origins <- c(-6996, -6996, -7000, -7000)
    new_origins <- rep(-6996, t=5)
    time_ref <- 1950 # any string, e.g. "BP", or number
    #n_mas_an <- rep(2.5, t=5)
    n_mas_an <- c(rep(2.5, t=4), 10)
    #n_mas_an <- c(30, rep(7.5, t=5))
    #n_mas_an <- rep(10, t=5)
    #n_mas_an <- c(100, 8.3333, 10, 0.83333)
    varnamesin <- rep("temp2", t=5)
    #varnamesin <- rep("tsurf", t=5)
    #varnamesin <- rep("tsurfaprt", t=5)
    #varnamesin <- rep("aprt", t=5)
    #varnamesin <- rep("aprl", t=5)
    #varnamesin <- rep("aprc", t=5)
    #varnamesin <- rep("aprs", t=5)
    #varnamesin <- rep("evap", t=5)
    #varnamesin <- rep("pe", t=5)
    #varnamesin <- rep("quv", t=5)
    #varnamesin <- rep("quv_direction", t=5)
    #varnamesin <- rep("lm_quv_as_time_slope", t=5)
    #varnames_uv <- rep(list("lm_quv_as_time_slope"=c(u="lm_qu_as_time_slope", v="lm_qv_as_time_slope")), t=5)
    #varnames_out_samedims <- "lm_quv_as_time_slope"
    #levsf <- rep("_int1000-100hPa", t=5)
    #varnamesin <- rep("wisoaprt_d_post", t=5)
    #varnamesin <- rep("lm_wisoaprt_d_post_as_time_slope", t=5)
    #levs <- rep(2, t=5)
    #varnamesin <- rep("lm_temp2_as_time_slope", t=5)
    #varnamesin <- rep("lm_tsurf_as_time_slope", t=5)
    #varnamesin <- rep("lm_aprt_as_time_slope", t=5)
    #varnamesin <- rep("lm_aps_as_time_slope", t=5)
    #varnamesin <- rep("lm_psl_as_time_slope", t=5)
    #varnamesin <- rep("lm_THO_as_time_slope", t=5)
    #codes <- rep(2, t=5)
    #levs <- rep(6, t=5)
    #varnamesin <- rep("lm_aprt_as_time_slope", t=5)
    #varnamesin <- rep("lm_wind10_as_time_slope", t=5)
    #varnames_uv <- rep(list("lm_wind10_as_time_slope"=c(u="lm_u10_as_time_slope", v="lm_v10_as_time_slope")), t=5) # for quiver
    #varnames_out_samedims <- "lm_wind10_as_time_slope"
    #names_legend_samedims <- 
    #varnamesin <- rep("lm_act_fpc_as_time_slope", t=5)
    #codes <- rep(31, t=5)
    #levsf <- rep("_sum1-4lev", t=5)
    modes <- rep("timmean", t=5)
    #modes <- rep("select", t=5)
    #modes <- c("select", "fldsum", "select", "fldsum") 
    #modes <- rep("yearsum", t=5)
    #modes <- rep("seassum", t=5)
    #modes <- rep("vertsum", t=5)
    #modes <- c(rep("seassum", t=4), "yearsum")
    #varnames_out_samedims <- "SICOMO"
    #names_legend_samedims <- names_legend
    #cols <- c(DJF="blue", MAM="darkgreen", JJA="red", SON="brown", "black")
    #cols_samedims <- 1:5
    #ltys_samedims <- rep(1, t=5)
    #areas <- rep("global_remapcon2_r3600x1800", t=5)
    #areas <- rep("ladoga_remapnn", t=5)
    #areas <- rep("shuchye_remapnn", t=5)
    #areas <- rep("levinson-lessing_remapnn", t=5)
    #areas <- rep("taymyr_remapnn", t=5)
    #areas <- rep("emanda_remapnn", t=5)
    #areas <- rep("elgygytgyn_remapnn", t=5)
    #areas <- rep("two-yurts_remapnn", t=5)
    #areas <- rep("kotokel_remapnn", t=5)
    #regboxes <- lapply(vector("list", l=5), base::append, list(regbox="N30-90"))
    regboxes <- lapply(vector("list", l=5), base::append, list(regbox="NAsiberia"))

# ==================================================
# 6 settings
} else if (F) { # fesom fldmean on irregular versus regular mesh
    models <- rep("fesom", t=6)
    names_legend <- c("100m irreg", "100m reg 1°", "100m reg 1/4°", 
                      "580m irreg", "580m reg 1°", "580m reg 1/4°")
    cols <- c(1:3, 1:3)
    ltys <- c(rep(1, t=3), rep(2, t=3))
    names_short <- rep("piControl2", t=6)
    prefixes <- paste0("awi-esm-1-1-lr_kh800_", names_short, 
                       rep(c("", "_regular_dx1.000_dy1.000", "_regular_dx0.250_dy0.250"), t=2))
    varnamesin <- rep("thetao", t=6)
    depths <- c(rep("100", t=3), rep("580", t=3)) 
    fromsf <- rep(1850, t=6)
    tosf <- rep(1851, t=6)
    modes <- rep("fldmean", t=6)

} else if (F) { # awi-esm-1-1-lr_kh800 ensemble: pi (2686 to 2851 (2014) and 2936 (2100)) hist ssp126 ssp245 ssp534-over ssp585
    models <- rep("echam6", t=6)
    #models <- rep("fesom", t=6)
    prefixes <- paste0("awi-esm-1-1-lr_kh800_", 
                       c("piControl", 
                         #"historical", 
                         "historical2", 
                         "ssp126", "ssp245", "ssp534-over", "ssp585"))
    names_short <- c("pi", "hist", "126", "245", "534-over", "585")
    names_legend <- c("piControl", "historical", "ssp126", "ssp245", "ssp534-over", "ssp585")
    cols <- c(1, 3, 4, 5, 6, 2) # special col order
    #varnamesin <- rep("temp2", t=6)
    #varnamesin <- rep("co2_flx_ocean", t=6)
    echam6_global_setNA <- "land"
    addland <- F
    varnamesin <- rep("co2_flx_ocean", t=6)
    #varnamesin <- rep("siextentn", t=6)
    bilinear_interp_factor <- 4
    fromsf <- c(2686, 1850, rep(2015, t=4))
    #tosf <- c(2936, 2014, 2100, 2100, 2100, 2100)
    tosf <- c(3000, 2014, 2100, 2100, 2100, 2100)
    #fromsf <- c(2851-29, 2014-29, rep(2100-29, t=4))
    new_origins <- c(1850, rep(NA, t=5)) # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    #new_origins <- c(2014-29, rep(NA, t=5))
    tosp <- c(2100, rep(NA, t=5))
    #modes <- rep("select", t=6)
    #modes <- rep("fldmean", t=6)
    modes <- rep("fldint", t=6)
    #modes <- rep("timmean", t=6)
    #areas <- rep("reccap2_atlantic", t=6)
    #areas <- rep("reccap2_pacific", t=6)
    #areas <- rep("reccap2_indian", t=6)
    #areas <- rep("reccap2_arctic", t=6)
    #areas <- rep("reccap2_southern", t=6)

} else if (F) { # compare PLOT lakes
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=6)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=6)
    models <- rep("echam5", t=6)
    #names_short <- rep("Hol-Tx10", t=6)
    names_short <- rep("Hol-T", t=6)
    names_legend <- c("Ladoga", "Shuchye", "Levinson-Lessing", "Taymyr", "Emanda", "Elgygytgyn")
    #fromsf <- rep("0001", t=6) # beginning of chunk 1
    fromsf <- rep("0004", t=6)
    tosf <- rep("6821", t=6)
    #tosf <- rep("7001", t=6) # end of chunk 3
    #seasonsp <- rep("Jul", t=6)
    seasonsp <- rep("Jan", t=6)
    new_origins <- rep(-6996, t=6)
    #new_origins <- rep(-7000, t=6)
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas_an <- rep(2.5, t=6)
    #n_mas_an <- rep(10, t=6)
    #varnamesin <- rep("temp2", t=6)
    varnamesin <- rep("aprt", t=6)
    areas <- c("ladoga_remapnn", "shuchye_remapnn", "levinson-lessing_remapnn", "taymyr_remapnn", "emanda_remapnn", "elgygytgyn_remapnn")

} else if (F) { # compare Hol-T* time series
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_fort_75", t=6)
    prefixes <- c(rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=3), rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=3))
    models <- rep("echam5", t=6)
    #models <- rep("mpiom1", t=6)
    names_short <- c(rep("Hol-T", t=3), rep("Hol-Tx10", t=3))
    names_legend <- names_short
    fromsf <- c(rep("0004", t=3), rep("0001", t=3))
    tosf <- c(rep("7000", t=3), rep("7001", t=3))
    new_origins <- c(rep(-6996, t=3), rep(-7000, t=3))
    time_ref <- 1950 # any string, e.g. "BP", or number
    seasonsp <- rep("JJA", t=6)
    #n_mas_an <- rep(2.5, t=6)
    #n_mas_an <- rep(10, t=6)
    n_mas_an <- c(rep(10, t=3), rep(5, t=3))
    #n_mas_an <- c(rep(100, t=3), rep(20, t=3))
    modes <- rep("select", t=6)
    #varnamesin <- rep("amoc", t=6)
    #codes <- rep(101, t=6)
    #areas <- c("moc45to60N", "moc30to60N", "moc50N", "moc45to60N", "moc30to60N", "moc26.5N")
    #levs <- c("-285to-2180m", "-285to-2180m", "-0to-5420m", "-0to-5420m", "-0to-5420m", "-0to-5420m")
    #names_legend <- paste0(areas, " ", levs)
    varnamesin <- rep(c("aprt", "aprl", "aprc"), t=2)
    areas <- rep("ladoga_remapnn", t=6)
    varnames_out_samedims <- "aprt"
    names_legend_samedims <- c("Hol-T aprt (total)", "Hol-T aprl (large-scale)", "Hol-T aprc (convection)",
                               "Hol-Tx10 aprt (total)", "Hol-Tx10 aprl (large-scale)", "Hol-Tx10 aprc (convection)")
    ltys_samedims <- c(rep(1, t=3), rep(2, t=3))
    cols_samedims <- rep(c("black", "#E41A1C", "#377EB8"), t=2)

} else if (F) { # positive/negative north pacific index NPI in Hol-T
    models <- rep("echam5", t=6)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm", t=6)
    names_short <- c(rep("Hol-T_above1.5sd", t=3), rep("Hol-T_below1.5sd", t=3))
    fromsf <- rep("0004", t=6)
    tosf <- rep(7000, t=6)
    new_origins <- rep(-6996, t=6)
    time_ref <- 1950
    varnamesin <- c("wind10_gt_1.5_sd_NPI", "u10_gt_1.5_sd_NPI", "v10_gt_1.5_sd_NPI",
                     "wind10_lt_1.5_sd_NPI", "u10_lt_1.5_sd_NPI", "v10_lt_1.5_sd_NPI")
    varnames_uv <- list(list(uv=1, u=2, v=3),
                        list(uv=4, u=5, v=6))
    varnames_out_samedims <- "wind10_u10_v10"
    modes <- rep("timmean", t=6)
    seasonsf <- rep("annual", t=6)
    areas <- rep("NAsiberia", t=6)
    #regboxes <- lapply(vector("list", l=6), base::append, list(regbox="NAsiberia"))

# ==================================================
# 7 settings
} else if (F) { # compare pi/mh cold/warm atmosphere restart problem
    models <- rep("echam6", t=7)
    prefixes <- c("awi-esm-1-1-lr_pi477_ollie", # temp2 2700 to 3249 -> 2051 to 2600
                  "awi-esm-1-1-lr_piControl_g3bid", # temp2 2701 to 2999 -> 2051 to 2349
                  "awi-esm-1-1-lr_piControl", # tas 1842 to 1941 -> 2350 to 2449
                  "awi-esm-1-1-lr_mh477_ollie", # temp2 2623 to 2657
                  "awi-esm-1-1-lr_mh_new_mistral", # temp2 2624 to 3001
                  "awi-esm-1-1-lr_midHolocene", # tas 3106 to 3205
                  "awi-esm-1-1-lr_mh_cold_mistral") # temp2 3105 to 3207
    names_short <- c("pi477", "piControl_spinup", "piControl", "mh477", "mh_new", "midHolocene", "mh_cold")
    fromsf <- c(2700, 2701, 1842, 2623, 2624, 3106, 3105)
    tosf <- c(3249, 2999, 1941, 2657, 3001, 3205, 3207)
    new_origins <- c(2051, 2051, 2350, NA, NA, NA, NA)
    #fromsp <- c(546, rep(NA, t=3))
    #tosp <- c(NA, rep(555, t=3))
    n_mas_an <- rep(3, t=7)
    varnamesin <- c("temp2", "temp2", "tas", "temp2", "temp2", "tas", "temp2")
    modes <- rep("fldmean", t=7)
    varnames_out_samedims <- "temp2"
    names_legend_samedims <- c("pi477 (2700 to 3249)", "piControl_spinup (2701 to 2999)", "piControl (1842 to 1941)",
                               "mh477 (2623 to 2657)", "mh_new (2624 to 3001)", "midHolocene (3106 to 3205)",
                               "mh_cold (3105 to 3207)")
    cols_samedims <- 1:7
    ltys_samedims <- rep(1, t=7)

# ==================================================
# 8 settings
} else if (F) { # awi-esm-1-1-lr_kh800 ensemble: pi (2686 to 2851 (2014) and 2936 (2100)) hist ssp126 ssp245 ssp534-over ssp585 chau_etal_2020 gregor_and_fay_2021
    models <- c(rep("echam6", t=6), "chau_etal_2020", "gregor_and_fay_2021")
    prefixes <- c(paste0("awi-esm-1-1-lr_kh800_", 
                         c("piControl", "historical", "ssp126", "ssp245", "ssp534-over", "ssp585")),
                  "chau_etal_2020", "gregor_and_fay_2021")
    names_short <- c("pi", "hist", "126", "245", "534-over", "585", "chau_etal_2020", "gregor_and_fay_2021")
    names_legend <- c("piControl", "historical", "ssp126", "ssp245", "ssp534-over", "ssp585", "C20", "GF21")
    cols <- c(1, 3, 4, 5, 6, 2, 1, 1) # special col order
    ltys <- c(rep(1, t=6), 2, 3)
    #varnamesin <- rep("temp2", t=6)
    #varnamesin <- rep("co2_flx_ocean", t=6)
    varnamesin <- c(rep("co2_flx_ocean", t=6), "fgco2", "fgco2_ens_mean")
    varnames_out_samedims <- "co2_flx_ocean"
    names_legend_samedims <- names_legend
    plotprefix <- "awicm-1.0-recom_conc_ensemble_C20_GF21"
    echam6_global_setNA <- "land"
    addland <- F
    bilinear_interp_factor <- 4
    fromsf <- c(2686, 1850, rep(2015, t=4), 1985, 1990)
    tosf <- c(2936, 2014, 2100, 2100, 2100, 2100, 2020, 2019)
    #fromsf <- c(2851-29, 2014-29, rep(2100-29, t=4))
    #tosf <- c(2851, 2014, rep(2100, t=4))
    new_origins <- c(1850, rep(NA, t=7)) # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    #new_origins <- c(2014-29, rep(NA, t=5))
    #modes <- rep("fldmean", t=6)
    modes <- rep("fldint", t=8)
    #modes <- rep("timmean", t=6)
    #areas <- rep("reccap2_atlantic", t=6)
    #areas <- rep("reccap2_pacific", t=6)
    #areas <- rep("reccap2_indian", t=6)
    #areas <- rep("reccap2_arctic", t=6)
    areas <- rep("reccap2_southern", t=8)

} else if (F) { # hol-tx10 vs hol-t
    prefixes <- c(rep("cosmos-aso-wiso_Hol-Tx10_timeser_ext", t=4), 
                  rep("cosmos-aso-wiso_Hol-T_timeser_ext", t=4))
    models <- rep("mpiom1", t=8)
    names_short <- c(rep("Hol-Tx10", t=4), rep("Hol-T", t=4))
    names_legend <- names_short
    fromsf <- c(rep("0001", t=4), rep("0004", t=4))
    tosf <- c(rep("7001", t=4), rep("7000", t=4))
    new_origins <- c(rep(-7000, t=4), rep(-6996, t=4)) 
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas_an <- c(rep(10, t=4), rep(100, t=4))
    cols <- c(rep(1, t=4), rep(2, t=4))
    #varnamesin <- rep(c("c208_SST_GLO", "c210_T200_GLO", "c212_T700_GLO", "c214_T2200_GLO"), t=2)
    #varnamesin <- rep(c("c128_SST_ATL", "c130_T200_ATL", "c132_T700_ATL", "c134_T2200_ATL"), t=2)
    #varnamesin <- rep(c("cSST_GIN", "c50_T200_GIN", "c52_T700_GIN", "c54_T2200_GIN"), t=2)
    #varnamesin <- rep(c("c88_SST_LAB", "c90_T200_LAB", "c92_T700_LAB", "c94_T2200_LAB"), t=2)
    #varnames_out_samedims <- "thetao"
    varnamesin <- rep(c("c209_SSS_GLO", "c211_S200_GLO", "c213_S700_GLO", "c215_S2200_GLO"), t=2)
    #varnamesin <- rep(c("c129_SSS_ATL", "c131_S200_ATL", "c133_S700_ATL", "c135_S2200_ATL"), t=2)
    #varnamesin <- rep(c("c49_SSS_GIN", "c51_S200_GIN", "c53_S700_GIN", "c55_S2200_GIN"), t=2)
    #varnamesin <- rep(c("c89_SSS_LAB", "c91_S200_LAB", "c93_S700_LAB", "c95_S2200_LAB"), t=2)
    varnames_out_samedims <- "so"
    names_legend_samedims <- paste0(names_short, rep(paste0(" ", c("surf", "200m", "700m", "2200m")), t=2))
    cols_samedims <- c(1:4, 1:4)
    ltys_samedims <- c(rep(2, t=4), rep(1, t=4))

} else if (F) { # compare pi/mh cold/warm atmosphere restart problem
    models <- rep("echam6", t=8)
    prefixes <- c("awi-esm-1-1-lr_pi477_ollie", # temp2 2700 to 3249 -> 2051 to 2600
                  "awi-esm-1-1-lr_piControl_g3bid", # temp2 2701 to 2999 -> 2051 to 2349
                  "awi-esm-1-1-lr_piControl", # tas 1842 to 1941 -> old: 2350 to 2449; new: 3000 to 3099 
                  "awi-esm-1-1-lr_mh477_ollie", # temp2 2623 to 2657 -> 2700 to 2734
                  "awi-esm-1-1-lr_mh_new_mistral", # temp2 2624 to 3001 -> 2701 to 3078
                  "awi-esm-1-1-lr_midHolocene", # tas 3106 to 3205 -> 3183 to 3282 
                  "awi-esm-1-1-lr_mh_cold_mistral", # temp2 3123 to 3266 -> 3200 to 3343
                  "awi-esm-1-1-lr_mat_0013") # temp2 1855 to 1968; new: 3100 to 3250
    names_short <- c("pi477", "piControl_spinup", "piControl", "mh477", "mh_new", "midHolocene", "mh_cold", "mat_0013")
    fromsf <-      c(2700, 2701, 1842, 2623, 2624, 3106, 3123, 1855)
    tosf <-        c(3249, 2999, 1941, 2657, 3001, 3205, 3266, 1968)
    new_origins <- c(NA,   NA,   3000, 2700, 2701, 3183, 3200, 3000)
    #fromsp <- c(546, rep(NA, t=3))
    #tosp <- c(NA, rep(555, t=3))
    n_mas_an <- rep(3, t=8)
    codes <- c("", "", "", "", "", "", "", 167)
    varnamesin <- c("temp2", "temp2", "tas", "temp2", "temp2", "tas", "temp2", "temp2")
    modes <- rep("fldmean", t=8)
    varnames_out_samedims <- "temp2"
    names_legend_samedims <- c("1) pi477", "2) piControl_spinup", "3) piControl", "4) mh477", 
                               "5) mh_new", "6) midHolocene", "7) mh_cold", "8) mat_0013")
    cols_samedims <- 1:8
    ltys_samedims <- rep(1, t=8)

} else if (F) {
    models <- c(rep("fesom", t=4), rep("mpiom1", t=4))
    #prefixes <- c("awi-esm-1-1-lr_piControl_regular_dx0.250_dy0.250", "awi-esm-1-1-lr_1percCO2_regular_dx0.250_dy0.250",
    #              "awi-cm-1-1-mr_piControl_regular_dx0.250_dy0.250", "awi-cm-1-1-mr_1pctCO2_regular_dx0.250_dy0.250",
    #              "mpi-esm1-2-lr_piControl_remapbil_r1440x720", "mpi-esm1-2-lr_1percCO2_remapbil_r1440x720",
    #              "mpi-esm1-2-hr_piControl_remapbil_r1440x720", "mpi-esm1-2-hr_1percCO2_remapbil_r1440x720")
    prefixes <- c("awi-esm-1-1-lr_piControl_monmax_regular_dx0.250_dy0.250", "awi-esm-1-1-lr_1percCO2_monmax_regular_dx0.250_dy0.250",
                  "awi-cm-1-1-mr_piControl_monmax_regular_dx0.250_dy0.250", "awi-cm-1-1-mr_1pctCO2_monmax_regular_dx0.250_dy0.250",
                  "mpi-esm1-2-lr_piControl_remapbil_r1440x720", "mpi-esm1-2-lr_1percCO2_remapbil_r1440x720",
                  "mpi-esm1-2-hr_piControl_remapbil_r1440x720", "mpi-esm1-2-hr_1percCO2_remapbil_r1440x720")
    names_short <- c("AWI-LR-pi", "AWI-LR-1pct",
                     "AWI-MR-pi", "AWI-MR-1pct",
                     "MPI-LR-pi", "MPI-LR-1pct",
                     "MPI-HR-pi", "MPI-HR-1pct")
    names_legend <- c("AWI-LR PI", "AWI-LR 1%",
                      "AWI-MR PI", "AWI-MR 1%",
                      "MPI-LR PI", "MPI-LR 1%",
                      "MPI-HR PI", "MPI-HR 1%")
    fromsf <- c(2015, 1910,
                2710, 1910,
                1910, 1910,
                1910, 1910)
    tosf <- c(2035, 1930,
              2730, 1930,
              1930, 1930,
              1930, 1930)
    #seasonsf <- rep("FMA", t=8)
    #seasonsf <- rep("SON", t=8)
    #varnamesin <- rep("mlotst", t=8)
    varnamesin <- rep("omldamax", t=8)
    #modes <- rep("timmean", t=8)
    modes <- c(rep("timmean", t=4), rep("timmean_monmax", t=4))

} else if (F) { # phd eddy volint budget
    workpath <- "/work/ba0941/a270073"
    models <- rep("fesom", t=8)
    if (F) {
        prefixes <- rep("Low01_sub_lsea_s52", t=8)
        names_short <- paste0("L5", c("lhs", "rhs", "divt", "mean", "eddy", "sgs", "F", "rest"))
        depthsf <- rep("_int0-3600m", t=8)
        areas <- rep("LS30l2", t=8)
    } else if (T) {
        prefixes <- rep("LSea5_sub_lsea_s5", t=8)
        names_short <- paste0("H5", c("lhs", "rhs", "divt", "mean", "eddy", "sgs", "F", "rest"))
        depthsf <- rep("_int0-4150m", t=8)
        areas <- rep("LS30l", t=8)
        add_legend <- F
    }
    names_legend <- names_short
    varnamesin <- c("dttemp", "divuvttot_divuvsgsttot_Ftemp", "divuvttot", "divuvt", "divuvteddy", "divuvsgsttot", "Ftemp", "divuvtrest")
    depthsf[which(varnamesin == "Ftemp")] <- "_0m"
    varnames_out_samedims <- "divuvt_budget"
    names_legend_samedims <- names_legend
    cols_samedims <- c("orange", "gray", "black", "blue", "red", "cyan", "magenta", "black")
    ltys_samedims <- c(rep(1, t=7), 2)
    ltws_samedims <- rep(2, t=8)
    modes <- rep("fldint", t=8)
    n_mas_an <- rep(3, t=8)
    fromsf <- rep(1948, t=8)
    tosf <- rep(2009, t=8)

# ==================================================
# 9 settings
} else if (F) { # cmip6 piControl
    models <- c("CESM2", "CESM2-FV2", "CESM2-WACCM", "CESM2-WACCM-FV2", "IPSL-CM6A-LR", 
                "MPI-ESM-1-2-HAM", "MPI-ESM1-2-LR", "NorCPM1", "TaiESM1")
    prefixes <- paste0(models, "_piControl")
    r1i1p1f1_inds <- match(c("CESM2", "CESM2-FV2", "CESM2-WACCM", "CESM2-WACCM-FV2", 
                             "MPI-ESM-1-2-HAM", "MPI-ESM1-2-LR", "NorCPM1", "TaiESM1"), models)
    prefixes[r1i1p1f1_inds] <- paste0(prefixes[r1i1p1f1_inds], "_r1i1p1f1")
    r1i2p1f1_inds <- match(c("IPSL-CM6A-LR"), models)
    prefixes[r1i2p1f1_inds] <- paste0(prefixes[r1i2p1f1_inds], "_r1i2p1f1")
    names_legend <- models
    names_short <- names_legend
    varnamesin <- rep("fHarvest", t=9)
    plotprefix <- "cmip6_piControl"
    modes <- rep("fldint", t=9)
    fromsf <- c(1101, 401, 400, 401, 2000, 2750, 2750, 401, 601) # last 100a
    tosf <- c(1200, 500, 499, 500, 2099, 2849, 2849, 500, 700) # last 100a

# ==================================================
# 11 settings
} else if (F) { # cmip6 piControl
    models <- c("AWI-ESM-1-1-LR", "echam6", "CanESM5", "CESM2-FV2", "CESM2-WACCM", "CESM2-WACCM-FV2", 
                "CMCC-CM2-SR5", "CNRM-ESM2-1", "MIROC-ES2L", "MPI-ESM-1-2-HAM", "MPI-ESM1-2-LR")
    prefixes <- paste0(models, "_piControl")
    r1i1p1f1_inds <- match(c("AWI-ESM-1-1-LR", "CanESM5", "CESM2-FV2", "CESM2-WACCM", "CESM2-WACCM-FV2", 
                             "CMCC-CM2-SR5", "MPI-ESM-1-2-HAM", "MPI-ESM1-2-LR"), models)
    prefixes[r1i1p1f1_inds] <- paste0(prefixes[r1i1p1f1_inds], "_r1i1p1f1")
    r1i1p1f2_inds <- match(c("CNRM-ESM2-1", "MIROC-ES2L"), models)
    prefixes[r1i1p1f2_inds] <- paste0(prefixes[r1i1p1f2_inds], "_r1i1p1f2")
    names_legend <- models
    names_legend[names_legend == "echam6"] <- "AWICM1-RECOM" # my echam6 output
    names_short <- names_legend
    prefixes[which(names_legend == "AWICM1-RECOM")] <- "awi-esm-1-1-lr_kh800_piControl"
    varnamesin <- rep("netAtmosLandCO2Flux", t=11)
    plotprefix <- "cmip6_piControl"
    modes <- rep("fldint", t=11)
    #modes <- rep("fldint_timmean", t=20)
    fromsf <- c(1855, 2686, 6101, 401, 400, 401, 2250, 2250, 2250, 2750, 2750) # last 100a
    tosf <- c(1954, 2850, 6200, 500, 499, 500, 2349, 2349, 2349, 2849, 2849) # last 100a

# ==================================================
# 12 settings
} else if (F) {
    models <- rep("echam6", t=12)
    prefixes <- rep("awi-esm-1-1-lr_kh800_esm-piControl_co2fsign", t=12)
    names_short <- paste0("esm_piControl_", month.abb)
    names_legend <- month.abb
    #varnamesin <- rep("co2_flx_ocean", t=12)
    #echam6_global_setNA <- "land"
    varnamesin <- rep("co2_flx_land", t=12)
    echam6_global_setNA <- "ocean"
    addland <- F
    fromsf <- rep(2686, t=12)
    #tosf <- rep(2703, t=12)
    tosf <- rep(2704, t=12)
    seasonsf <- month.abb
    new_origins <- rep(1, t=12)
    modes <- rep("timmean", t=12)

} else if (F) { # compare jsbach pft levels
    #workpath <- "/work/ab0246/a270073"
    models <- rep("jsbach", t=12)
    #prefixes <- rep("awi-esm-1-1-lr_piControl", t=12)
    #prefixes <- rep("awi-esm-1-1-lr_kh800_piControl_og", t=12)
    #prefixes <- rep("awi-esm-1-1-lr_kh800_piControl", t=12)
    #prefixes <- rep("awi-esm-1-1-lr_kh800_piControl_og_and_esm-piControl_restartall", t=12)
    prefixes <- rep("awi-esm-1-1-lr_kh800_historical2", t=12)
    names_short <- paste0("pft", c(2, 3, 4, 5, 10, 11, 12, 13, 15, 16, 20, 21))
    names_legend <- c("tropical broadleaf evergreen (2)",
                      "tropical broadleaf deciduous (3)",
                      "extra-tropical evergreen (4)",
                      "extra-tropical deciduous (5)",
                      "raingreen shrubs (10)",
                      "deciduous shrubs (11)",
                      "C3 grass (12)",
                      "C4 grass (13)",
                      "C3 pasture (15)",
                      "C4 pasture (16)",
                      "C3 crops (20)",
                      "C4 crops (21)")
    varnamesin <- rep("pft_fract_box", t=12)
    modes <- rep("fldsum", t=12)
    levs <- c(2, 3, 4, 5, 10, 11, 12, 13, 15, 16, 20, 21) # not glacier and not bare land
    cols <- c(rep("#7fc97f", t=4),
              rep("#beaed4", t=2),
              rep("#fdc086", t=2),
              rep("black", t=2),
              rep("#386cb0", t=2))
    ltys <- c(1, 2, 3, 4,
              1, 2,
              1, 2,
              1, 2,
              1, 2)
    lwds <- rep(2, t=12)
    #seasonsf <- rep("annual", t=12)
    fromsf <- rep(1850, t=12)
    #fromsf <- rep(1950, t=12) # piControl chunk 1 start: 1950
    #fromsf <- rep(1955, t=12) # awi-esm-1-1-lr deck piControl spinup year 1942
    tosf <- rep(2014, t=12)
    #tosf <- rep(2104, t=12) # awi-esm-1-1-lr deck piControl spinup year 2091
    #tosf <- rep(2685, t=12)
    #tosf <- rep(2850, t=12)
    #tosf <- rep(2962, t=12) # esm-piControl
    #tosf <- rep(2996, t=12)
    #tunit <- "model year"
    #new_origins <- fromsf - 1950 + 1 # awi-esm-1-1-lr_kh800 piControl: 1950 = 1 --> new_origin = `fromsf` - 1950 + 1 
    #new_origins <- rep(400, t=12) 

} else if (F) { # compare awi-esm-1-recom vs awi-esm-2-recom ying
    names_short <- c("awiesm1-recom", "awiesm2-recom")
    #names_legend <- c("AWI-ESM-1-RECOM", "AWI-ESM-2-RECOM")
    #names_legend <- c("AWI-ESM-1-RECOM esm-piControl", "AWI-ESM-2-RECOM piControl atm. box")
    names_legend <- c("AWI-ESM-1-REcoM esm-piControl", "")
    if (F) {
        models <- rep("echam6", t=2)
        varnamesin <- rep("nbp", t=2)
        modes <- rep("fldint", t=2)
        fromsf <- c(3208, 2001)
        tosf <- c(3859, 2510)
    } else if (F) {
        models <- c("fesom", "fesom2")
        varnamesin <- c("tos", "sst")
        varnames_out_samedims <- "tos"
        names_legend_samedims <- names_legend
        modes <- rep("timmean", t=2)
        #modes <- rep("fldmean", t=2)
        #fromsf <- c(1950, 2001)
        #fromsf <- c(2901, 3440)
        fromsf <- c(3801, 3440)
        #tosf <- c(3000, 3500)
        tosf <- c(3850, 3500)
        bilinear_interp_factor <- 4 
    } else if (T) {
        models <- rep("recom", t=2)
        #varnamesin <- rep("CO2f", t=2)
        #varnamesin <- c("bgc12", "DOC")
        #varnames_out_samedims <- "DOC"
        varnamesin <- c("aCO2", "xCO2")
        varnames_out_samedims <- "aCO2"
        names_legend_samedims <- names_legend
        modes <- rep("select", t=2)
        #modes <- rep("timmean", t=2)
        #modes <- rep("fldint", t=2)
        #fromsf <- c(1950, 2001)
        #fromsf <- c(2901, 3400)
        fromsf <- c(3001, 2001)
        #fromsf <- c(3208, 2001)
        #fromsf <- c(3801, 2001)
        #fromsf <- c(3801, 3400)
        #tosf <- c(3000, 3505)
        #tosf <- c(3850, 3505)
        tosf <- c(3859, 3506)
        #depths <- c(0, 0)
    }
    #prefixes <- c("awi-esm-1-1-lr_kh800_piControl", "awi-esm-2.1-recom-par-tracers_piControl")
    #prefixes <- c("awi-esm-1-1-lr_kh800_piControl_regular_dx1.000_dy1.000", "awi-esm-2.1-recom-par-tracers_piControl_regular_dx1.000_dy1.000")
    #prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2", "awi-esm-2.1-recom-par-tracers_piControl")
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_and_esm-piControl_wout_talk_rest_1_and_2", "awi-esm-2.1-recom-par-tracers_piControl")
    #prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2_regular_dx1.000_dy1.000", "awi-esm-2.1-recom-par-tracers_piControl_regular_dx1.000_dy1.000")
    #prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2_regular_dx0.250_dy0.250", "awi-esm-2.1-recom-par-tracers_piControl_regular_dx0.250_dy0.250")
    #seasonsf <- c(NA, "annual")
    seasonsf <- c("annual", "annual")
    new_origins <- c(1, 1)
    #new_origins <- c(208, 1)
    #fromsp <- c(NA, 100)
    #fromsp <- c(NA, 180)
    #tunit <- "piControl year"
    tunit <- "piControl/esm-piControl year"
    #tunit <- "esm-piControl year"
    #add_linear_trend <- T
    cols <- c(1, NA) # special for judith

# ======================================================
# 16 settings
} else if (F) { # 15/16 reccap2 settings
    models <- c("CCSM-WHOI", # <0: uptake 
                "CESM-ETHZ", "CNRM-ESM2-1", "EC-Earth3", "ECCO-Darwin", 
                "FESOM_REcoM_HR", # says unit "mmol C m-2 s-1" but its "mol" 
                "FESOM_REcoM_LR", "MOM6-COBALT2-Princeton", 
                "MPIOM-HAMOCC", "MRI-ESM2-0", "NorESM-OC1.2", "OCIM-v2014", "OCIM-v2021", 
                "ORCA025-GEOMAR", # says unit "Pg C yr-1" but its "molC m-2 s-1" 
                "ORCA1-LIM3-PISCES")
    fromsf <- c(1958, 1980, 1980, 1980, 1995, 1980, 1980, 1980, 1980, 1980, 
                1980, 1980, 1980, 1980, 1980)
    tosf <- c(2017, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2019, 2018, 
              2018, 2017, 2018, 2018, 2018)
    if (F) { # add regional ROMS if possible; atlantic
        models <- c(models, "ROMS-Atlantic-ETHZ")
        fromsf <- c(fromsf, 1980)
        tosf <- c(tosf, 2019)
    }
    if (F) { # pacific
        models <- c(models, "ROMS-Pacific-ETHZ")
        fromsf <- c(fromsf, 1980)
        tosf <- c(tosf, 2019)
    }
    if (F) { # indian
        models <- c(models, "ROMS-NYUAD")
        fromsf <- c(fromsf, 1980)
        tosf <- c(tosf, 2018)
    }
    if (T) { # southern
        models <- c(models, "ROMS-SouthernOcean-ETHZ")
        fromsf <- c(fromsf, 1980)
        tosf <- c(tosf, 2018)
    }
    names_short <- models
    names_legend <- names_short
    prefixes <- rep("reccap2_A", t=length(models))
    varnamesin <- rep("fgco2", t=length(models))
    modes <- rep("fldint", t=length(models))
    # reccap2 basins:
    #areas <- rep("reccap2_atlantic", t=length(models))
    #areas <- rep("reccap2_pacific", t=length(models))
    #areas <- rep("reccap2_indian", t=length(models))
    #areas <- rep("reccap2_arctic", t=length(models))
    #areas <- rep("reccap2_southern", t=length(models))
    # reccap2 atlantic biomes:
    #areas <- rep("reccap2_na_spss", t=length(models))
    #areas <- rep("reccap2_na_stss", t=length(models))
    #areas <- rep("reccap2_na_stps", t=length(models))
    #areas <- rep("reccap2_aequ", t=length(models))
    #areas <- rep("reccap2_sa_stps", t=length(models))
    #areas <- rep("reccap2_med", t=length(models))
    # reccap2 pacific biomes:
    #areas <- rep("reccap2_np_spss", t=length(models))
    #areas <- rep("reccap2_np_stss", t=length(models))
    #areas <- rep("reccap2_np_stps", t=length(models))
    #areas <- rep("reccap2_pequ_w", t=length(models))
    #areas <- rep("reccap2_pequ_e", t=length(models))
    #areas <- rep("reccap2_sp_stps", t=length(models))
    # gregor_etal_2019 basins:
    #areas <- rep("g19_NH-HL", t=length(models))
    #areas <- rep("g19_NH-ST", t=length(models))
    #areas <- rep("g19_EQU", t=length(models))
    #areas <- rep("g19_SH-ST", t=length(models))
    areas <- rep("g19_SH-HL", t=length(models))
    plotpath <- "/work/ollie/cdanek/plots/reccap2"
    if (!exists("areas")) {
        plotprefix <- paste0(prefixes[1], "_global")
    } else {
        plotprefix <- paste0(prefixes[1], "_", areas[1])
    }

# ======================================================
# 18 settings
} else if (F) { # cmip6 piControl co2_flx_total
    models <- c("ACCESS-ESM1-5", "echam6", "CanESM5", "CanESM5-CanOE", "CESM2", "CESM2-FV2", "CESM2-WACCM-FV2", 
                "CMCC-ESM2", "CNRM-ESM2-1", "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", 
                "MPI-ESM-1-2-HAM", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL")
    prefixes <- paste0(models, "_piControl")
    r1i1p1f1_inds <- match(c("ACCESS-ESM1-5", "echam6", "CanESM5", "CESM2", "CESM2-FV2", "CESM2-WACCM-FV2", 
                             "CMCC-ESM2", "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", 
                             "MPI-ESM-1-2-HAM", "MPI-ESM1-2-LR"), models)
    prefixes[r1i1p1f1_inds] <- paste0(prefixes[r1i1p1f1_inds], "_r1i1p1f1")
    r1i1p2f1_inds <- match(c("CanESM5-CanOE"), models)
    prefixes[r1i1p2f1_inds] <- paste0(prefixes[r1i1p2f1_inds], "_r1i1p2f1")
    r1i1p1f2_inds <- match(c("CNRM-ESM2-1", "MIROC-ES2L", "UKESM1-0-LL"), models)
    prefixes[r1i1p1f2_inds] <- paste0(prefixes[r1i1p1f2_inds], "_r1i1p1f2")
    r1i2p1f1_inds <- match(c("MRI-ESM2-0"), models)
    prefixes[r1i2p1f1_inds] <- paste0(prefixes[r1i2p1f1_inds], "_r1i2p1f1")
    names_legend <- models
    names_legend[names_legend == "echam6"] <- "AWICM1-RECOM" # my echam6 output
    names_short <- names_legend
    prefixes[which(names_legend == "AWICM1-RECOM")] <- "awi-esm-1-1-lr_kh800_piControl"
    varnamesin <- rep("co2_flx_total", t=18)
    plotprefix <- "cmip6_piControl"
    modes <- rep("fldint", t=18)
    fromsf <- c(901, 2896, 6101, 5901, 1101, 401, 401, 2000, 2250, 401, 
                2281, 3097, 3750, 2250, 2750, 2750, 2001, 3740)
    tosf <- c(1000, 2995, 6200, 6000, 1200, 500, 500, 2099, 2349, 500, 
              2380, 3196, 3849, 2349, 2849, 2849, 2100, 3839)
                
# ======================================================
# 24 settings
} else if (F) { # compare jsbach pft levels awi-esm-1-1-lr_kh800 piControl
    models <- rep("jsbach", t=24)
    prefixes <- c(rep("awi-esm-1-1-lr_kh800_piControl", t=12),
                  rep("awi-esm-1-1-lr_kh800_esm-piControl_restartall", t=12))
    names_short <- rep(paste0("pft", c(2, 3, 4, 5, 10, 11, 12, 13, 15, 16, 20, 21)), t=2)
    names_legend <- c("tropical broadleaf evergreen (2)",
                      "tropical broadleaf deciduous (3)",
                      "extra-tropical evergreen (4)",
                      "extra-tropical deciduous (5)",
                      "raingreen shrubs (10)",
                      "deciduous shrubs (11)",
                      "C3 grass (12)",
                      "C4 grass (13)",
                      "C3 pasture (15)",
                      "C4 pasture (16)",
                      "C3 crops (20)",
                      "C4 crops (21)")
    varnamesin <- rep("pft_fract_box", t=24)
    levs <- rep(c(2, 3, 4, 5, 10, 11, 12, 13, 15, 16, 20, 21), t=2) # not glacier and not bare land
    cols <- c(rep("#7fc97f", t=4),
              rep("#beaed4", t=2),
              rep("#fdc086", t=2),
              rep("black", t=2),
              rep("#386cb0", t=2))
    cols <- c(cols, col2rgba(cols, 0.5))
    ltys <- c(1, 2, 3, 4,
              1, 2,
              1, 2,
              1, 2,
              1, 2)
    ltys <- rep(ltys, t=2)
    lwds <- rep(2, t=24)
    seasonsf <- rep("annual", t=24)
    fromsf <- c(rep(1950, t=12), rep(2686, t=12)) # piControl chunk 1 start: 1950
    tosf <- c(rep(2850, t=12), rep(2962, t=12)) # piControl chunk 3 end: 2850
    tunit <- "model year"
    new_origins <- fromsf - 1950 + 1 # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    modes <- rep("fldsum", t=24)

} else if (F) { # cmip6 piControl fgco2
    models <- c("ACCESS-ESM1-5", "echam6", "CanESM5", "CanESM5-CanOE", "CESM2", "CESM2-FV2", 
                "CESM2-WACCM", "CESM2-WACCM-FV2", "CMCC-ESM2", "CNRM-ESM2-1", 
                "EC-Earth3-CC", "GFDL-CM4", "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", 
                "IPSL-CM6A-LR", "MIROC-ES2L", "MPI-ESM-1-2-HAM", "MPI-ESM1-2-HR", 
                "MPI-ESM1-2-LR", "MRI-ESM2-0", "NorCPM1", "NorESM1-F", "UKESM1-0-LL")
    prefixes <- paste0(models, "_piControl")
    r1i1p1f1_inds <- match(c("ACCESS-ESM1-5", "CanESM5", "CESM2", "CESM2-FV2", "CESM2-WACCM", 
                             "CESM2-WACCM-FV2", "CMCC-ESM2", "EC-Earth3-CC", "GFDL-CM4", 
                             "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MPI-ESM-1-2-HAM", 
                             "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "NorCPM1", "NorESM1-F"), models)
    prefixes[r1i1p1f1_inds] <- paste0(prefixes[r1i1p1f1_inds], "_r1i1p1f1")
    r1i1p1f2_inds <- match(c("CNRM-ESM2-1", "MIROC-ES2L", "UKESM1-0-LL"), models)
    prefixes[r1i1p1f2_inds] <- paste0(prefixes[r1i1p1f2_inds], "_r1i1p1f2")
    r1i2p1f1_inds <- match(c("MRI-ESM2-0"), models) 
    prefixes[r1i2p1f1_inds] <- paste0(prefixes[r1i2p1f1_inds], "_r1i2p1f1")
    r1i1p2f1_inds <- match(c("CanESM5-CanOE"), models) 
    prefixes[r1i1p2f1_inds] <- paste0(prefixes[r1i1p2f1_inds], "_r1i1p2f1")
    names_legend <- models
    names_legend[names_legend == "echam6"] <- "AWICM1-RECOM" # my echam6 output
    names_short <- names_legend
    prefixes[which(names_legend == "AWICM1-RECOM")] <- "awi-esm-1-1-lr_kh800_piControl"
    varnamesin <- rep("fgco2", t=24)
    plotprefix <- "cmip6_piControl"
    modes <- rep("fldint", t=24)
    #modes <- rep("fldint_timmean", t=20)
    fromsf <- c(901, 2896, 6101, 5901, 1101, 401, 400, 401, 2000, 2250, 2255, 551, 401, 
                2281, 3097, 3750, 2250, 2750, 2250, 2750, 2001, 401, 1601, 3740) # last 100a
    tosf <- c(1000, 2995, 6200, 6000, 1200, 500, 499, 500, 2099, 2349, 2354, 650, 500, 
              2380, 3196, 3849, 2349, 2849, 2349, 2849, 2100, 500, 1700, 3839) # last 100a
    
# ======================================================
# 25 settings
} else if (F) { # cmip6 piControl npb
    models <- c("ACCESS-ESM1-5", "echam6", "CanESM5", "CanESM5-CanOE", "CESM2", "CESM2-FV2", 
                "CESM2-WACCM-FV2", "CMCC-CM2-SR5", "CMCC-ESM2", "CNRM-ESM2-1", 
                "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", 
                "IPSL-CM6A-LR", "MIROC-ES2L", "MPI-ESM-1-2-HAM", "MPI-ESM1-2-LR", 
                "MRI-ESM2-0", "NorCPM1", "NorESM2-LM", "SAM0-UNICON", "TaiESM1", "UKESM1-0-LL")
    prefixes <- paste0(models, "_piControl")
    r1i1p1f1_inds <- match(c("ACCESS-ESM1-5", "CanESM5", "CESM2", "CESM2-FV2", "CESM2-WACCM-FV2", 
                             "CMCC-CM2-SR5", "CMCC-ESM2", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", 
                             "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", 
                             "MPI-ESM-1-2-HAM", "MPI-ESM1-2-LR", "NorCPM1", "NorESM2-LM", 
                             "SAM0-UNICON", "TaiESM1"), models)
    prefixes[r1i1p1f1_inds] <- paste0(prefixes[r1i1p1f1_inds], "_r1i1p1f1")
    r1i1p2f1_inds <- match(c("CanESM5-CanOE"), models) 
    prefixes[r1i1p2f1_inds] <- paste0(prefixes[r1i1p2f1_inds], "_r1i1p2f1")
    r1i1p1f2_inds <- match(c("CNRM-ESM2-1", "MIROC-ES2L", "UKESM1-0-LL"), models)
    prefixes[r1i1p1f2_inds] <- paste0(prefixes[r1i1p1f2_inds], "_r1i1p1f2")
    r1i2p1f1_inds <- match(c("MRI-ESM2-0"), models)
    prefixes[r1i2p1f1_inds] <- paste0(prefixes[r1i2p1f1_inds], "_r1i2p1f1")
    names_legend <- models
    names_legend[names_legend == "echam6"] <- "AWICM1-RECOM" # my echam6 output
    names_short <- names_legend
    prefixes[which(names_legend == "AWICM1-RECOM")] <- "awi-esm-1-1-lr_kh800_piControl"
    varnamesin <- rep("nbp", t=25)
    plotprefix <- "cmip6_piControl"
    modes <- rep("fldint", t=25)
    fromsf <- c(1001, 2686, 6101, 5951, 1101, 401, 401, 2250, 2000, 2250, 2250, 2701, 401, 
                2281, 3097, 3750, 2250, 2750, 2750, 2001, 401, 2001, 601, 601, 3740)
    tosf <- c(1100, 2850, 6200, 6050, 1200, 500, 500, 2349, 2099, 2349, 2349, 2800, 500, 
              2380, 3196, 3849, 2349, 2849, 2849, 2100, 500, 2100, 700, 700, 3839)

} else if (F) { # 25 levels
    models <- rep("echam6", t=25)
    prefixes <- rep("awi-esm-1-1-lr_kh800_piControl_og_restart_hl_ppm", t=25)
    varnamesin <- rep("CO2", t=25)
    levs <- c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 
              11000, 12000, 13000, 14000, 15000, 16000, 17000, 18000, 19000, 
              20000, 21000, 22000, 23000, 24000)
    names_short <- paste0(levs, "m")
    modes <- rep("select", t=25)
    fromsf <- rep(2685, t=25) # last piControl og year
    tosf <- rep(2685, t=25)
    new_origins <- rep(736, t=25)

# ======================================================
# 27 settings
} else if (F) { # 27 atm levels
    models <- rep("echam6", t=27)
    prefixes <- rep("awi-esm-1-1-lr_kh800_piControl_og_restart_hl_ppm", t=27)
    varnamesin <- rep("CO2", t=27)
    levs <- c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 
              11000, 12000, 13000, 14000, 15000, 16000, 17000, 18000, 19000, 
              20000, 21000, 22000, 23000, 24000, 25000, 26000)
    names_short <- paste0(levs, "m")
    modes <- rep("select", t=27)
    fromsf <- rep(2685, t=27) # last piControl og year
    tosf <- rep(2685, t=27)
    new_origins <- rep(736, t=27)

# ======================================================
# special: cmip6 nml created by ~/slurm/cronjobs/filter_esgf_lists.r
} else if (F) { 
    if (F) { # piControl test
        fnml <- "~/slurm/cronjobs/esgf/namelist.plot_24settings_Omon_fgco2_23models_piControl_2-2nyears_2022-05-05_12-19-46.r"
    } else if (F) { # piControl
        fnml <- "~/slurm/cronjobs/esgf/namelist.plot_23settings_Omon_fgco2_23models_piControl_251-251nyears_2022-05-06_09-04-39.r"
    } else if (F) { # historical test
        fnml <- "~/slurm/cronjobs/esgf/namelist.plot_29settings_Omon_fgco2_29models_historical_2-2nyears_2022-05-06_09-48-29.r"
    } else if (F) { # historical
        #fnml <- "~/slurm/cronjobs/esgf/namelist.plot_29settings_Omon_fgco2_29models_historical_165-165nyears_2022-05-06_09-45-36.r"
        fnml <- "~/slurm/cronjobs/esgf/namelist.plot_44settings_Omon_mlotst_44models_historical_165-165nyears_2022-09-16_09-29-25.r"
    } else if (F) { # ssp126 fgco2
        fnml <- "~/slurm/cronjobs/esgf/namelist.plot_16settings_Omon_fgco2_16models_ssp126_86-86nyears_2022-05-10_16-33-40.r"
    } else if (T) { # ssp585 siareas
        fnml <- "~/slurm/cronjobs/esgf/namelist.plot_38settings_SImon_siconc_38models_ssp585_86-86nyears_2023-03-29_15-10-56.r"
    }
    if (!file.exists(fnml)) stop("could not find file ", fnml)
    message("run `source(\"", fnml, "\")` ...")
    source(fnml)

    # exclude wrong models
    inds <- list()
    inds[[length(inds)+1]] <- which(!is.na(match(varnamesin, c("chl", "dissic", "talk"))) & models == "GISS-E2-1-G") # wrong
    inds[[length(inds)+1]] <- which(grepl("historical", prefixes) & models == "EC-Earth3") # only until 2007 data
    inds[[length(inds)+1]] <- which(grepl("historical", prefixes) & varnamesin == "dpco2" & models == "CESM2-WACCM-FV2") # only 1900-1999 data
    inds[[length(inds)+1]] <- which(grepl("historical", prefixes) & varnamesin == "dissic" & models == "NorESM2-LM") # only 1850-1859
    inds[[length(inds)+1]] <- which(grepl("historical", prefixes) & varnamesin == "talk" & models == "NorCPM1") # wrong historical dates 2015-2029
    inds[[length(inds)+1]] <- which(grepl("ssp", prefixes) & varnamesin == "mlotst" & models == "CESM2") # only 2065-2100 data
    inds[[length(inds)+1]] <- which(varnamesin == "siareas" & models == "AWI-CM-1-1-MR") # cdo: Unsupported file structure
    inds[[length(inds)+1]] <- which(varnamesin == "siconc" & models == "CIESM") # cdo: segmentation fault
    inds[[length(inds)+1]] <- which(varnamesin == "siconc" & models == "CAMS-CSM1-0") # only until 2099
    inds[[length(inds)+1]] <- which(varnamesin == "fgco2" & !is.na(match(models, c("BCC-CSM2-MR", "GISS-E2-1-G", "GISS-E2-1-G-CC")))) # wrong factors; reported to DKRZ
    inds[[length(inds)+1]] <- which(varnamesin == "siareas" & !is.na(match(models, c("AWI-CM-1-1-MR")))) # cdo Unsupported file structure
    inds[[length(inds)+1]] <- which(varnamesin == "siconc" & !is.na(match(models, c("CIESM")))) # cdo Unsupported file structure
    inds[[length(inds)+1]] <- which(grepl("ssp", prefixes) & varnamesin == "siconc" & models == "CAMS-CSM1-0") # only until 2099
    inds <- unique(unlist(inds))
    if (length(inds) > 0) {
        message("\nexclude ", length(inds), " settings:\n", paste(prefixes[inds], collapse="\n"))
        models <- models[-inds]; prefixes <- prefixes[-inds]; names_short <- names_short[-inds]
        names_legend <- names_legend[-inds]; varnamesin <- varnamesin[-inds]; fromsf <- fromsf[-inds]
        tosf <- tosf[-inds]
    }
    
    #modes <- rep("select", t=length(models))
    modes <- rep("fldmean", t=length(models))
    #modes <- rep("fldint", t=length(models))
    new_origins <- rep(NA, t=length(models))
    #center_ts <- T
    #fromsf <- rep(1982, t=length(models))
    #fromsp <- rep(1990, t=length(models))
    #tosp <- rep(2020, t=length(models))
    inds <- grep("piControl", prefixes)
    if (length(inds) > 0) { 
        message("special: adjust ", length(inds), " piControl new_origins ...")
        new_origins[inds] <- 1850
    }
    areas <- rep("50to90S", t=length(models))

    # special mergetime: replace with historical+ssp16 data
    inds <- grep("ssp126", prefixes)
    if (length(inds) > 0) { 
        message("special: replace ", length(inds), " ssp126 prefixes with historical+ssp16")
        prefixes[inds] <- sub("ssp126", "historical_and_ssp126", prefixes[inds])
        fromsf[inds] <- 1850
        tosp[inds] <- 2020
        plotprefix <- "cmip6_historical_and_ssp126"
    }

} else if (F) { # obs vs awicm vs cmip6 historical
    # NorESM has no historical spco2
    models <- c("CanESM5-CanOE", "CanESM5", 
                #"CESM2", 
                #"CESM2-FV2", 
                "CESM2-WACCM", 
                "CNRM-ESM2-1", 
                #"GFDL-CM4", 
                "GFDL-ESM4", 
                "IPSL-CM6A-LR",
                "MIROC-ES2L",
                #"MPI-ESM-1-2-HAM", 
                "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", 
                "NorESM2-LM", "NorESM2-MM",
                "UKESM1-0-LL")
    prefixes <- c("CanESM5-CanOE_historical_and_ssp126_r1i1p2f1", "CanESM5_historical_and_ssp126_r1i1p1f1",
                  #"CESM2-FV2_historical_r1i1p1f1", 
                  #"CESM2_historical_r1i1p1f1", 
                  "CESM2-WACCM_historical_and_ssp126_r1i1p1f1", 
                  "CNRM-ESM2-1_historical_and_ssp126_r1i1p1f2", 
                  #"GFDL-CM4_historical_and_ssp126_r1i1p1f1", 
                  #"GFDL-ESM4_historical_and_ssp119_r1i1p1f1",
                  "GFDL-ESM4_historical_and_ssp126_r1i1p1f1",
                  "IPSL-CM6A-LR_historical_and_ssp126_r1i1p1f1",
                  "MIROC-ES2L_historical_and_ssp126_r1i1p1f2",
                  #"MPI-ESM-1-2-HAM_historical_and_ssp126_r1i1p1f1", 
                  "MPI-ESM1-2-HR_historical_and_ssp126_r1i1p1f1", "MPI-ESM1-2-LR_historical_and_ssp126_r1i1p1f1", 
                  "NorESM2-LM_historical_and_ssp126_r1i1p1f1", "NorESM2-MM_historical_and_ssp126_r1i1p1f1",
                  "UKESM1-0-LL_historical_and_ssp126_r1i1p1f2")
    plotprefix <- "cmip6_historical_and_ssp126"
    if (F) { # tos
        models <- c("fesom", models)
        prefixes <- c("awi-esm-1-1-lr_kh800_historical_and_ssp126", prefixes)
        varnamesin <- rep("tos", t=length(models))
        varnames_out_samedims <- "tos"
        if (F) { # add en4
            plotprefix <- "obs_cmip6_historical_and_ssp126"
            models <- c("EN.4.2.2", models)
            prefixes <- c("EN.4.2.2", prefixes)
            varnamesin <- c("temperature", varnamesin)
            levsf <- c("_sellevidx_1", rep("", t=length(models)-1))
        }
    } else if (F) { # mlotst
        models <- c("fesom", models)
        prefixes <- c("awi-esm-1-1-lr_kh800_historical_and_ssp126", prefixes)
        varnamesin <- c("mldepthdensp030_m", rep("mlotst", t=length(models)-1)) 
        #varnamesin[models == "GFDL-ESM4"] <- "mldepthdensp030_m" # post-processed 0.03 mld
        varnames_out_samedims <- "mlotst"
        if (F) { # add en4
            plotprefix <- "obs_cmip6_historical_and_ssp126"
            models <- c("EN.4.2.2", models)
            prefixes <- c("EN.4.2.2", prefixes)
            varnamesin <- c("mldepthdensp030_m", varnamesin)
        }
    } else if (F) { # spco2
        models <- c("recom", models)
        prefixes <- c("awi-esm-1-1-lr_kh800_historical_and_ssp126", prefixes)
        varnamesin <- c("pCO2s", rep("spco2", t=length(models)-1))
        varnames_out_samedims <- "spco2"
        if (F) { # add gregor_and_fay_2021
            plotprefix <- "obs_cmip6_historical_and_ssp126"
            models <- c("gregor_and_fay_2021", models)
            prefixes <- c("aggregate_6models", prefixes)
            varnamesin <- c("spco2_mon_mean", varnamesin)
            #varnamesin <- c("spco2_mon_median", varnamesin)
            #models <- c("gregor_and_fay_2021", "gregor_and_fay_2021", models)
            #prefixes <- c("aggregate_6models", "aggregate_6models", prefixes)
            #varnamesin <- c("spco2_mon_mean", "spco2_mon_median", varnamesin)
        }
    } else if (F) { # dpco2
        stop("update")
        models <- c("gregor_and_fay_2021", "recom", models)
        prefixes <- c("ens_stats", "awi-esm-1-1-lr_kh800_historical_and_ssp126", prefixes) # add GF21
        varnamesin <- c("dpco2_ens_mean", "dpCO2s", rep("dpco2", t=length(models)-2))
        varnames_out_samedims <- "dpco2"
    } else if (F) { # apco2
        stop("update")
        models <- c("gregor_and_fay_2021", "recom", models)
        prefixes <- c("NOAA", "awi-esm-1-1-lr_kh800_historical_and_ssp126", prefixes) # add GF21
        varnamesin <- c("pco2atm", "pCO2a", rep("apco2", t=length(models)-2))
        varnames_out_samedims <- "apco2"
    } else if (T) { # fgco2
        models <- c("echam6", models)
        prefixes <- c("awi-esm-1-1-lr_kh800_historical_and_ssp126", prefixes)
        varnamesin <- c("co2_flx_ocean", rep("fgco2", t=length(models)-1))
        varnames_out_samedims <- "fgco2"
        if (T) { # add cmip6 mean
            models <- c("cmip6_historical_and_ssp126", models)
            prefixes <- c("aggregate_12models", prefixes)
            varnamesin <- c("fgco2_mon_mean", varnamesin)
        }
        if (T) { # add gregor_and_fay_2021
            plotprefix <- "obs_cmip6_historical_and_ssp126"
            if (F) { # add mean and median
                models <- c("gregor_and_fay_2021", "gregor_and_fay_2021", models)
                prefixes <- c("aggregate_18models", "aggregate_18models", prefixes)
                varnamesin <- c("fgco2_mon_mean", "fgco2_mon_median", rep("fgco2", t=length(models)-3))
            } else if (T) { # add mean
                models <- c("gregor_and_fay_2021", models)
                prefixes <- c("aggregate_18models", prefixes)
                varnamesin <- c("fgco2_mon_mean", varnamesin)
            }
        }
    } else if (F) { # chl
        models <- c("recom", models)
        prefixes <- c("awi-esm-1-1-lr_kh800_historical_and_ssp126", prefixes)
        varnamesin <- rep("chl", t=length(models))
        levsf <- c("_0m", rep("_sellevidx_1", t=length(models)-1))
        varnames_out_samedims <- "chl"
        if (F) { # add obs
            plotprefix <- "obs_cmip6_historical_and_ssp126"
            models <- c("cmems", models)
            prefixes <- c("cmems_mod_glo_bgc_my_0.25_P1M-m_hindcast", prefixes)
            varnamesin <- c("chl", varnamesin)
            levsf <- c(levsf, "")
        }
    } # which variable
    rminds <- list()
    #if (varnames_out_samedims == "mlotst") rminds[[length(rminds)+1]] <- match(c("GFDL-ESM4", "MIROC-ES2L"), models) # no data
    if (varnames_out_samedims == "mlotst") rminds[[length(rminds)+1]] <- match(c("MIROC-ES2L"), models) # no data
    if (F && varnames_out_samedims == "fgco2") rminds[[length(rminds)+1]] <- match(c("CNRM-ESM2-1"), models) # has additional carbon influx from rivers
    if (varnames_out_samedims == "chl") {
        rminds[[length(rminds)+1]] <- which(models == "MIROC-ES2L") # no data
        rminds[[length(rminds)+1]] <- which(models == "CNRM-ESM2-1" | models == "IPSL-CM6A-LR") # strange values
    }
    rminds <- unique(unlist(rminds))
    if (length(rminds) > 0) { 
        models <- models[-rminds]; prefixes <- prefixes[-rminds]; varnamesin <- varnamesin[-rminds]
        if (exists("levsf")) levsf <- levsf[-rminds]
    }
    cols <- rep(NA, t=length(models))
    cols[models == "EN.4.2.2" | models == "gregor_and_fay_2021" | models == "cmems"] <- mycols(1)
    cols[models == "cmip6_historical_and_ssp126"] <- mycols(1) # cmip mean
    cols[models == "echam6" | models == "fesom" | models == "recom"] <- mycols(2)[2]
    cols[models == "CanESM5-CanOE"] <- mycols(3)[3]
    cols[models == "CanESM5"] <- mycols(4)[4]
    cols[models == "CESM2-WACCM"] <- mycols(5)[5]
    cols[models == "CNRM-ESM2-1"] <- mycols(6)[6]
    cols[models == "GFDL-ESM4"] <- mycols(7)[7]
    cols[models == "IPSL-CM6A-LR"] <- mycols(8)[8]
    cols[models == "MIROC-ES2L"] <- mycols(9)[9]
    cols[models == "MPI-ESM1-2-HR"] <- mycols(10)[10]
    cols[models == "MPI-ESM1-2-LR"] <- mycols(11)[11]
    cols[models == "NorESM2-LM"] <- mycols(13)[13]
    cols[models == "NorESM2-MM"] <- mycols(14)[14]
    cols[models == "UKESM1-0-LL"] <- mycols(12)[12]
    if (anyNA(cols)) stop("set fix color for all models")
    lwds <- ltys <- rep(1, t=length(models))
    lwds[] <- 2
    lwds[1] <- lwds[1] + 1 # obs
    lwds[models == "cmip6_historical_and_ssp126"] <- lwds[models == "cmip6_historical_and_ssp126"] + 1 # cmip mean
    lwds[models == "echam6" | models == "fesom" | models == "recom"] <- 3
    ltys[models == "cmip6_historical_and_ssp126"] <- 3 # cmip mean
    ltys[models == "echam6" | models == "fesom" | models == "recom"] <- 3
    names_legend <- models
    names_legend[models == "EN.4.2.2"] <- "EN4.2.2"
    names_legend[models == "gregor_and_fay_2021"] <- "SeaFlux mean"
    #names_legend[models == "gregor_and_fay_2021"] <- c("SeaFlux mean", "SeaFlux median")
    names_legend[models == "cmems"] <- "Hindcast"
    names_legend[models == "cmip6_historical_and_ssp126"] <- "CMIP6 mean"
    names_legend[models == "echam6" | models == "fesom" | models == "recom"] <- "AWI-ESM-1-REcoM"
    names_short <- names_legend
    names_legend_samedims <- names_legend
    #modes <- rep("timmean", t=length(models))
    #modes <- rep("fldmean", t=length(models))
    modes <- rep("fldint", t=length(models))
    fromsf <- rep(1850, t=length(models))
    #fromsf <- rep(1981, t=length(models))
    #fromsf <- rep(1982, t=length(models))
    #fromsf[match(c("CESM2-WACCM", "GFDL-ESM4"), names_legend)] <- 1970
    #fromsf[1] <- 1982 # obs
    #fromsf[2] <- 1982 # obs
    fromsf[1] <- 1990 # obs
    #fromsf[2] <- 1990 # obs
    #fromsf[1] <- 1993 # obs
    #fromsf[2] <- 1850 # awicm1-recom
    #fromsf[2] <- 1982 # awicm1-recom
    #fromsf[3] <- 1982 # awicm1-recom
    #fromsf <- rep(1982, t=length(models))
    #fromsf <- rep(1990, t=length(models))
    #tosf <- rep(2014, t=length(models))
    tosf <- rep(2019, t=length(models))
    #tosf <- rep(2100, t=length(models))
    #tosf[1] <- 2019 # obs
    #tosf[2] <- 2019 # obs
    #tosf[1] <- 2020 # obs
    #tosf[1] <- 2021 # obs
    #fromsp <- rep(1982, t=length(models))
    fromsp <- rep(1990, t=length(models))
    #fromsp[1] <- 1990 # obs
    #fromsp[2] <- 1982 # awicm1-recom
    #tosp <- rep(2014, t=length(models))
    #tosp <- rep(2019, t=length(models))
    center_ts <- F
    detrend_ts <- F
    #areas <- rep("g19_NH-HL", t=length(models))
    #areas <- rep("g19_NH-ST", t=length(models))
    #areas <- rep("g19_EQU", t=length(models))
    #areas <- rep("g19_SH-ST", t=length(models))
    #areas <- rep("g19_SH-HL", t=length(models))
    #areas <- rep("45to70S", t=length(models))
    #areas <- rep("nino34", t=length(models))

} # which setting

# run
source(paste0(repopath, "/plot_echam.r"))

