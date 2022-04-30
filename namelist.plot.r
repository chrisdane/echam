# r

# input for plot_echam.r

# load defaults
repopath <- "~/scripts/r/echam"
repopath <- normalizePath(repopath, mustWork=T) # error if not found
source(paste0(repopath, "/namelist.general.plot.r"))

# 1 setting
if (F) { # mhw composite data/seas*100
    workpaths <- "/work/ba1103/a270073"
    models <- "fesom"
    prefixes <- "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_anom_pcnt_regular_dx0.250_dy0.250"
    names_short <- "awicm1-recom_mhw_composite_anom_pcnt"
    names_legend <- "Events/Climatology*100"
    #varnames_in <- "mlotst"
    #varnames_in <- "omldamax"
    #varnames_in <- "tau"
    #varnames_in <- "curltau"
    varnames_in <- "ekmanP_ms"
    modes <- "timmean"
    fromsf <- 2842
    tosf <- ""
    seasonsf <- "Jul"

} else if (F) { # reccap AmC minus DmB
    workpath <- "/work/ollie/cdanek"
    models <- "fesom"
    names_legend <- "(A-C) - (D-B)"
    prefixes <- "reccap_AmC_minus_DmB"
    names_short <- prefixes
    varnames_in <- "CO2f"
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
    #varnames_in <- "temp2"
    # echam co2stream
    #varnames_in <- "co2_flux"
    varnames_in <- "co2_flx_ocean"
    areas <- "SO45S"
    #echam6_global_setNA <- "land"
    #varnames_in <- "co2_flx_land" # = npp + resp + herb + fire
    #echam6_global_setNA <- "ocean"
    #addland <- F
    #varnames_in <- "co2_flx_npp"
    #varnames_in <- "co2_flx_resp"
    #varnames_in <- "co2_flx_herb"
    #varnames_in <- "co2_flx_lcc"
    #varnames_in <- "co2_flx_harvest"
    #varnames_in <- "co2_flx_fire"
    #varnames_in <- "nbp" # = co2_flx_land + co2_flx_lcc + co2_flx_harvest
    # jsbach jsbachstream
    #varnames_in <- "CO2_flux_net"
    #varnames_in <- "CO2_flux_dynveg"
    # jsbach yassostream
    #varnames_in <- "soilSlow" # = vertsum(boxYC_humus_1) + vertsum(boxYC_humus_2)
    # fesom
    #varnames_in <- "thetaoga"
    #varnames_in <- "tos"
    #varnames_in <- "thetao"
    #depths <- "0-5900"
    # recom
    #varnames_in <- "bgc03"
    #depths <- "0"
    #varnames_in <- "CO2f"
    #varnames_in <- "NPP"
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
    models <- "echam6"
    #models <- "fesom"
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
    } else if (T) {
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl"
        names_short <- "esm_piControl"
        names_legend <- "esm-piControl"
    }
    #varnames_in <- "thetaoga"
    #varnames_in <- "tos"
    #varnames_in <- "thetao"
    #depths <- "0-5900"
    #depth_fromsp <- -50
    #depth_fromsp <- -250
    #depth_fromsp <- -500
    #varnames_in <- "aCO2"
    #varnames_in <- "CO2f"
    #varnames_in <- "CO2"
    #varnames_in <- "co2_flux"
    #varnames_in <- "co2_flx_land"
    #echam6_global_setNA <- "ocean"
    varnames_in <- "co2_flx_ocean"
    #echam6_global_setNA <- "land"
    #varnames_in <- "co2_flx_lcc"
    addland <- F
    #varnames_in <- "co2_burden_corr_acc2"
    #seasonsf <- "annual"
    #fromsf <- 2586
    #fromsf <- 2686
    #fromsf <- 2817
    fromsf <- 3001
    #tosf <- 2689
    #tosf <- 2703
    #tosf <- 2704
    #tosf <- 2760
    #tosf <- 2916
    tosf <- 3112
    #tunit <- "model year"
    tunit <- "esm-piControl year"
    new_origins <- 1 # esm-piControl
    #new_origins <- fromsf - 2686 + 1 # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    #fromsp <- 700
    #remove_mean_froms <- c(1, 1)
    #remove_mean_tos <- remove_mean_froms
    #n_mas <- 12
    n_mas <- 36
    #n_mas <- 60
    #n_mas <- 120
    #modes <- "select"
    #modes <- "timmean"
    #modes <- "fldmean"
    modes <- "fldint"
    #modes <- "depth"

} else if (F) { # awi-esm-1-1-lr_kh800 historical historical2
    #models <- "echam6"
    models <- "jsbach"
    #models <- "fesom"
    #prefixes <- "awi-esm-1-1-lr_kh800_historical"
    #names_short <- "awi-esm-1-1-lr_kh800_historical"
    prefixes <- "awi-esm-1-1-lr_kh800_historical2"
    names_short <- "awi-esm-1-1-lr_kh800_historical2"
    names_legend <- "historical"
    varnames_in <- "co2_flx_ocean"
    echam6_global_setNA <- "land"
    addland <- F
    #varnames_in <- "co2_flx_land"
    #varnames_in <- "tos"
    #fromsf <- 1850
    fromsf <- 1887
    tosf <- 1906
    modes <- "timmean"
    #modes <- "fldmean"
    #modes <- "fldint"

} else if (F) { # awi-esm-1-1-lr piControl
    workpath <- "/work/ab0246/a270073"
    models <- "echam6"
    prefixes <- "awi-esm-1-1-lr_piControl"
    names_short <- "awi-esm-1-1-lr_piControl"
    names_legend <- "AWI-ESM-1-1-LR piControl"
    varnames_in <- "co2_flx_ocean"
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
    varnames_in <- "temp2"
    modes <- "fldmean"
    n_mas <- 1
    cols <- "#E41A1C"
    remove_mean_froms <- 1961
    remove_mean_tos <- 1990

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
    n_mas <- 1
    #n_mas <- 30
    #n_mas <- 5*12
    n_mas <- 100
    #n_mas <- 150
    #n_mas <- 20/3*12
    #n_mas <- 10*12
    #n_mas <- 20*12
    #n_mas <- 3*90
    #n_mas <- 30*12
    #n_mas <- 100*12
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
    #varnames_in <- "temp2"
    #varnames_in <- "tsurf"
    #varnames_in <- "tslm1"
    #varnames_in <- "psl"
    #varnames_in <- "aprt"
    #varnames_in <- "aprs"
    #varnames_in <- "wisoaprt_d"
    #varnames_in <- "wisoaprt_d_post"
    #levs <- 2
    #varnames_in <- "temp2aprt"
    #varnames_in <- "tsurfaprt"
    #varnames_in <- "ptemp"
    #varnames_in <- "srad0"
    #varnames_in <- "srad0d"
    #varnames_in <- "lm_psl_as_time_slope"
    #varnames_in <- "lm_temp2_as_time_slope"
    #varnames_in <- "lm_tsurf_as_time_slope"
    #varnames_in <- "lm_aprt_as_time_slope"
    #varnames_in <- "lm_wisoaprt_d_sellevel_2_as_temp2_slope"
    #varnames_in <- "lm_wisoaprt_d_sellevel_2_as_ptemp_slope"
    varnames_in <- "lm_wisoaprt_d_post_as_time_slope"
    levs <- 2
    #varnames_in <- "quv"
    #varnames_in <- "quv_direction"
    #levsf <- "_int1000-100hPa"
    #varnames_out_samedims <- "quv"
    #names_legend_samedims <- c("qu", "qv", "quv")
    #varnames_uv <- list(quv=c(u="qu", v="qv")) # for quiver
    #varnames_in <- "THO"
    #levs <- 6
    #varnames_in <- "SICOMO"
    #varnames_in <- "act_fpc"
    #varnames_in <- "lm_act_fpc_as_time_slope"
    #codes <- 31
    #levsf <- "_sum1-4lev"
    #varnames_in <- "lm_albedo_as_time_slope"
    #varnames_in <- "amoc"
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

} else if (F) { # phd stuff 1 setting
    workpaths <- "/work/ba0941/a270073"
    models <- "fesom"
    prefixes <- "LSea5_s5_regular_dx0.250_dy0.250"
    names_short <- "Lsea5_s5"
    varnames_in <- "resolutionkm"
    seasonsf <- fromsf <- tosf <- ""
    modes <- "timmean"

# =====================================
# 2 settings
} else if (F) { # mhw composite data vs seas
    workpaths <- "/work/ba1103/a270073"
    models <- rep("fesom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_seas_regular_dx0.250_dy0.250",
                  "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_data_regular_dx0.250_dy0.250")
    names_short <- paste0("awicm1-recom_mhw_composite_", c("seas", "data"))
    names_legend <- c("Climatology", "Events")
    #varnames_in <- rep("mlotst", t=2)
    varnames_in <- rep("omldamax", t=2)
    #varnames_in <- rep("tau", t=2)
    #varnames_out_samedims <- "tau"
    #names_legend_samedims <- c("taux", "tauy", "tau")
    #varnames_in <- rep("curltau", t=2)
    #varnames_in <- rep("ekmanP_ms", t=2)
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
    varnames_in <- rep("CO2f", t=2)
    modes <- rep("fldint", t=2)
    fromsf <- rep(1958, t=2)
    tosf <- rep(2019, t=2)

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl me vs og
    models <- rep("echam6", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl", "awi-esm-1-1-lr_kh800_esm-piControl_og")
    names_short <- c("new", "old")
    varnames_in <- rep("temp2", t=2)
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
    #varnames_in <- rep("temp2", t=2)
    #varnames_in <- rep("co2_flux", t=2)
    varnames_in <- rep("co2_flx_ocean", t=2)
    #echam6_global_setNA <- "land"
    #varnames_in <- rep("co2_flx_land", t=2)
    #echam6_global_setNA <- "ocean"
    #addland <- F
    #varnames_in <- rep("co2_flx_land", t=2)
    #varnames_in <- rep("co2_flx_npp", t=2)
    #varnames_in <- rep("co2_flx_resp", t=2)
    #varnames_in <- rep("tos", t=2)
    #varnames_in <- rep("thetaoga", t=2)
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
    #varnames_in <- rep("co2_flx_lcc", t=2)
    #varnames_in <- rep("co2_flx_harvest", t=2)
    #varnames_in <- rep("co2_flx_land", t=2)
    #varnames_in <- rep("nbp", t=2)
    #varnames_in <- rep("co2_flx_ocean", t=2)
    varnames_in <- rep("co2_flx_total", t=2)
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
    #varnames_in <- rep("temp2", t=2)
    varnames_in <- rep("co2_flx_ocean", t=2)
    #varnames_in <- rep("tos", t=2)
    #varnames_in <- rep("thetaoga", t=2)
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
    models <- c("echam6", "fesom")
    prefixes <- rep("awi-esm-1-1-lr_kh800_esm-piControl_co2fsign", t=2)
    names_short <- rep("esm_piControl_2percboth_co2fsign", t=2)
    names_legend <- c("echam:co2_flx_ocean*-1", "fesom:CO2f")
    varnames_in <- c("co2_flx_ocean", "CO2f")
    varnames_out_samedims <- "co2flux"
    names_legend_samedims <- c("echam:co2_flx_ocean*-1", "fesom:CO2f")
    fromsf <- rep(2686, t=2)
    tosf <- rep(2689, t=2)
    tunit <- "model year"
    new_origins <- rep(1, t=2)
    modes <- rep("fldint", t=2)

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl co2fsign vs restartall
    #models <- rep("echam6", t=2)
    models <- rep("fesom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_co2fsign", "awi-esm-1-1-lr_kh800_esm-piControl_restartall")
    names_short <- c("co2fsign", "restartall")
    names_legend <- c("restart fesom", "restart all")
    varnames_in <- rep("aCO2", t=2)
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
    names_legend <- c("esm-piControl with Talk rest", "esm-piControl wout Talk rest wrong commit")
    varnames_in <- rep("bgc02", t=2)
    depths <- rep("0", t=2)
    fromsf <- c(3151, 3151)
    tosf <- c(3168, 3156)
    tunit <- "model year"
    new_origins <- c(1, 1)
    #tosp <- c(17, 17)
    #modes <- rep("select", t=2)
    modes <- rep("fldmean", t=2)
    legend_pos_ts <- "topright"

} else if (F) { # awi-esm-1-1-lr vs awi-esm-1-1_kh800 piControl
    models <- rep("fesom", t=2)
    prefixes <- c("awi-esm-1-1-lr_piControl_regular_dx0.250_dy0.250_minus_woa2018_dx0.250_dy0.250_t_an_0m",
                  "awi-esm-1-1-lr_kh800_piControl_og_regular_dx0.250_dy0.250_minus_woa2018_dx0.250_dy0.250_t_an_0m")
    names_short <- c("piControl_kh1500", "piControl_kh800")
    names_legend <- c("piControl kh=1500 minus WOA18", "piControl kh=800 minus WOA18")
    varnames_in <- rep("tos", t=2)
    fromsf <- c(1855, 2586)
    tosf <- c(1954, 2685)
    tunit <- "model year"
    modes <- rep("timmean", t=2)

} else if (F) { # awi-esm-1-1-lr_kh800 piControl_og vs historical
    models <- rep("echam6", t=2)
    #models <- rep("fesom", t=2)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl_og_day", "awi-esm-1-1-lr_kh800_historical_day")
    names_short <- c("piControl", "historical")
    varnames_in <- rep("temp2", t=2)
    #arnames_in <- rep("tos", t=2)
    #varnames_in <- rep("thetaoga", t=2)
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
        varnames_in <- rep("mlotst", t=2)
        varnames_in <- rep("thetao", t=2)
        #varnames_in <- rep("so", t=2)
        #varnames_in <- rep("potdens", t=2)
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
    #n_mas <- rep(60, t=2)
    n_mas <- rep(36, t=2)
    #n_mas <- rep(1, t=2)
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
    varnames_in <- rep("omldamax", t=2)
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
    #varnames_in <- c("temp2", "temp2")
    #varnames_in <- c("aprt", "aprt")
    #varnames_in <- c("tsurf", "tsurf")
    #varnames_in <- c("srad0d", "srad0d")
    #varnames_in <- c("albedo", "albedo")
    #varnames_in <- c("wisoaprt_d", "wisoaprt_d")
    varnames_in <- c("wisoaprt_d_post", "wisoaprt_d_post")
    #varnames_in <- c("lm_wisoaprt_d_post_as_time_slope", "lm_wisoaprt_d_post_as_time_slope")
    levs <- c(2, 2)
    #varnames_in <- c("tsurfaprt", "tsurfaprt")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_temp2", "lm_wisoaprt_d_sellevel_2_as_temp2")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_ptemp", "lm_wisoaprt_d_sellevel_2_as_ptemp")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_tsurf", "lm_wisoaprt_d_sellevel_2_as_tsurf")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_ptsurf", "lm_wisoaprt_d_sellevel_2_as_ptsurf")
    #varnames_in <- c("lm_temp2_as_time_slope", "lm_temp2_as_time_slope")
    #varnames_in <- c("lm_tsurf_as_time_slope", "lm_tsurf_as_time_slope")
    #varnames_in <- c("lm_aprt_as_time_slope", "lm_aprt_as_time_slope")
    #varnames_in <- c("lm_wind10_as_time_slope", "lm_wind10_as_time_slope")
    #varnames_in <- c("c204_ICEARE_GLO", "c204_ICEARE_GLO")
    #varnames_in <- c("c205_ICEVOL_GLO", "c205_ICEVOL_GLO")
    #varnames_in <- c("c204_ICEARE_GLO", "c204_ICEARE_GLO")
    #varnames_in <- c("c64_ICEARE_ARC", "c64_ICEARE_ARC")
    #varnames_in <- c("c65_ICEVOL_ARC", "c65_ICEVOL_ARC")
    #varnames_in <- c("c46_HFL_GIN", "c46_HFL_GIN")
    #varnames_in <- c("c47_WFL_GIN", "c47_WFL_GIN")
    #varnames_in <- c("c44_ICEARE_GIN", "c44_ICEARE_GIN")
    #varnames_in <- c("c45_ICEVOL_GIN", "c45_ICEVOL_GIN")
    #varnames_in <- c("c86_HFL_LAB", "c86_HFL_LAB")
    #varnames_in <- c("c87_WFL_LAB", "c87_WFL_LAB")
    #varnames_in <- c("c84_ICEARE_LAB", "c84_ICEARE_LAB")
    #varnames_in <- c("c85_ICEVOL_LAB", "c85_ICEVOL_LAB")
    #varnames_in <- c("c145_ICEVOL_SO", "c145_ICEVOL_SO")
    #varnames_in <- c("c145_ICEVOL_SO", "c145_ICEVOL_SO")
    #varnames_in <- rep("amoc", t=2)
    #codes <- rep(101, t=2)
    #varnames_in <- c("zmld", "zmld")
    #varnames_in <- c("quv", "quv")
    #varnames_in <- c("quv_direction", "quv_direction")
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
    #n_mas <- c(1, 1)
    #n_mas <- c(30, 10)
    #n_mas <- c(90, 30)
    #n_mas <- c(175, 30)
    #n_mas <- c(200, 20)
    n_mas <- c(250, 50) # jqs fig. 6
    #n_mas <- c(30*12, 10*12)
    #n_mas <- c(250*12, 50*12) # jan-dec
    #n_mas <- c(120*12, 20*12) # seasons
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
    n_mas <- c(120, 120)
    modes <- c("select", "select")
    #modes <- c("yearsum", "yearsum")
    #seasonsf <- c("yearsum", "yearsum")
    #seasonsp <- "Feb" # cdo's default season timestamps: Feb, May, Aug, Nov
    #seasonsp <- "May"
    #seasonsp <- "Jun"
    #seasonsp <- "Aug"
    #seasonsp <- "Nov"
    #seasonsp <- "Dec"
    varnames_in <- c("aprl", "aprc")
    varnames_out_samedims <- c("aprl_vs_aprc")
    names_legend_samedims <- c("aprl", "aprc")
    #varnames_in <- c("wisoaprt_d_post", "wisoaprt_d_post")
    #levs <- c(2, 2)
    #varnames_in <- c("qu", "qv")
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
    #varnames_in <- c("psl_gt_1.5_sd_NPI", "psl_lt_1.5_sd_NPI")
    #varnames_in <- c("wind10_gt_1.5_sd_NPI", "wind10_lt_1.5_sd_NPI")
    #varnames_in <- c("u10_gt_1.5_sd_NPI", "u10_lt_1.5_sd_NPI")
    #varnames_in <- c("v10_gt_1.5_sd_NPI", "v10_lt_1.5_sd_NPI")
    #varnames_in <- c("temp2_gt_1.5_sd_NPI", "temp2_lt_1.5_sd_NPI")
    varnames_in <- c("aprt_gt_1.5_sd_NPI", "aprt_lt_1.5_sd_NPI")
    #varnames_in <- c("wisoaprt_d_post_gt_1.5_sd_NPI", "wisoaprt_d_post_lt_1.5_sd_NPI")
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
    n_mas <- rep(36, t=2)
    varnames_in <- rep("temp2", t=2)
    #varnames_in <- rep("tsurf", t=2)
    #varnames_in <- rep("aprt", t=2)
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
        varnames_in <- rep("resolutionkm", t=2)
        seasonsf <- fromsf <- tosf <- rep("", t=2)
        proj <- "+proj=ortho +lat_0=30 +lon_0=-45"
        proj <- "+proj=ortho +lat_0=40 +lon_0=-45"
    } else if (F) {
        varnames_in <- rep("mixlay", t=2)
    } else if (F) {
        varnames_in <- rep("Ftemp", t=2)
        depths <- c(0, 0)
        load_special_data <- T
        #bilinear_interp_factor <- 4 # useRaster=F
        bilinear_interp_factor <- 10 # useRaster=T
    } else if (F) {
        varnames_in <- rep("divuvt", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        ignore_vars <- c(ignore_vars, "dutempdx", "dvtempdy", "dx_u_times_temp", "dy_v_times_temp")
        #bilinear_interp_factor <- 4 # useRaster=F
        bilinear_interp_factor <- 10 # useRaster=T
    } else if (F) {
        varnames_in <- rep("divuvteddy", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        ignore_vars <- c(ignore_vars, "duteddydx", "dvteddydy", "dx_utemp_eddy", "dy_vtemp_eddy")
        #bilinear_interp_factor <- 4 # useRaster=F
        bilinear_interp_factor <- 10 # useRaster=T
    } else if (F) {
        varnames_in <- rep("divuvttot", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        #bilinear_interp_factor <- 4 # useRaster=F
        bilinear_interp_factor <- 10 # useRaster=T
    } else if (F) {
        varnames_in <- rep("FeKe", t=2)
        depths <- c(0, 0)
        lepos <- "topleft"
        load_special_data <- T
        bilinear_interp_factor <- 10
    } else if (F) {
        varnames_in <- rep("HRS", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        bilinear_interp_factor <- 10
        #add_legend <- F; add_legend_right_yaxis <- T; add_legend_left_yaxis_before <- F
    } else if (F) {
        varnames_in <- rep("VRS", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        add_legend <- F; add_legend_right_yaxis <- F; add_legend_left_yaxis_before <- F
    } else if (F) {
        varnames_in <- rep("KmKe", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
    } else if (T) {
        varnames_in <- rep("wbeddy", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        bilinear_interp_factor <- 10
        add_legend <- F; add_legend_right_yaxis <- T; add_legend_left_yaxis_before <- T
    } else if (F) {
        varnames_in <- rep("eke", t=2)
        depthsf <- c("_int0-3600m", "_int0-4150m")
        add_legend <- F; add_legend_right_yaxis <- T; add_legend_left_yaxis_before <- T
    } else if (F) {
        varnames_in <- rep("eke_over_tke", t=2)
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
    n_mas <- rep(36, t=2)
    #modes <- rep("timmean", t=2)
    modes <- rep("fldmean", t=2)
    #modes <- rep("fldint", t=2)

} else if (F) { # phd Arc22
    models <- rep("fesom", t=2)
    prefixes <- c("Arc22_sub_daily", "Arc22_sub")
    names_short <- c("Arc22_day", "Arc22_mon")
    names_legend <- c("daily model output", "monthly model output")
    #varnames_in <- rep("divuvbtot", t=2)
    varnames_in <- rep("divuvbeddy", t=2); ignore_vars <- c(ignore_vars, "dubeddydx_meanint", "dvbeddydy_meanint")
    depthsf <- rep("_int0-5650m", t=2)
    areas <- rep("arc08fram", t=2)
    fromsf <- rep(2005, t=2)
    tosf <- rep(2009, t=2)
    modes <- rep("fldint", t=2)
    cols <- c(2, 1)
    lepos <- "bottomright"

# =====================================
# 3 settings
} else if (F) { # awi-esm-1-1-lr_kh800 piControl vs historical vs historical2 
    models <- rep("echam6", t=3)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl", "awi-esm-1-1-lr_kh800_historical", "awi-esm-1-1-lr_kh800_historical2")
    names_short <- c("piControl", "historical", "historical2")
    names_legend <- names_short
    #varnames_in <- rep("co2_flx_ocean", t=3)
    #varnames_in <- rep("co2_flx_lcc", t=3)
    varnames_in <- rep("co2_flx_harvest", t=3)
    #varnames_in <- rep("nbp", t=3)
    #varnames_in <- rep("co2_flx_total", t=3)
    modes <- rep("fldint", t=3)
    fromsf <- c(1950, 1850, 1850)
    tosf <- c(2995, 2006, 2006)
    #tosf <- c(2996, 2006, 2006)
    new_origins <- c(1114, NA, NA) # historical starts from pi 2686 = spinup year 737 --> 1850 - 737 + 1 = 1114
    fromsp <- c(1850, NA, NA)
    tosp <- c(2006, NA, NA)
    #n_mas <- rep(5*12, t=3)
    #n_mas <- rep(10*12, t=3)

} else if (F) { # awi-esm-1-1-lr_kh800 piControl vs historical2 vs ssp585
    models <- rep("echam6", t=3)
    #models <- rep("fesom", t=3)
    prefixes <- c("awi-esm-1-1-lr_kh800_piControl", "awi-esm-1-1-lr_kh800_historical2", "awi-esm-1-1-lr_kh800_ssp585")
    names_short <- c("piControl", "historical2", "ssp585")
    names_legend <- c("piControl", "historical", "ssp585")
    #varnames_in <- rep("temp2", t=3)
    #varnames_in <- rep("co2_flx_land", t=3)
    #varnames_in <- rep("co2_flx_ocean", t=3)
    #varnames_in <- rep("co2_flx_lcc", t=3)
    #varnames_in <- rep("co2_flx_harvest", t=3)
    varnames_in <- rep("nbp", t=3)
    #varnames_in <- rep("tos", t=3)
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
    #n_mas <- rep(3*12, t=3)
    #n_mas <- rep(5*12, t=3)
    #n_mas <- rep(10*12, t=3)

} else if (F) { # awicm1-recom piControl vs esm-piControl
    models <- rep("echam6", t=3)
    prefixes <- c(rep("awi-esm-1-1-lr_kh800_piControl", t=2), "awi-esm-1-1-lr_kh800_esm-piControl")
    names_short <- paste0("ar1_", c(rep("piControl", t=2), "esm-piControl"))
    names_legend <- paste0(c(rep("piControl ", t=2), "esm-piControl "), c(1000, 131, 131), "a")
    varnames_in <- rep("co2_flx_ocean", t=3)
    fromsf <- c(rep(1950, t=2), 3001)
    tosf <- c(rep(3000, t=2), 3131)
    tunit <- "model year"
    new_origins <- c(fromsf[1:2]-1950+1, fromsf[3]-2686+736+1) # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    fromsp <- c(1051+1-c(1000, 131), NA) # last xxx years of piControl
    n_mas <- rep(120, t=3)
    add_linear_trend <- T
    modes <- rep("fldint", t=3)

} else if (F) { # awicm1-recom piControl vs piControl_LUtrans1850 vs esm-piControl
    models <- rep("echam6", t=3)
    prefixes <- paste0("awi-esm-1-1-lr_kh800_", c("piControl", "piControl_LUtrans1850", "esm-piControl"))
    names_short <- paste0("ar1_", c("piControl", "piControl_LUtrans1850", "esm-piControl"))
    names_legend <- c("piControl (LUtrans=0)", "piControl (LUtrans=1850)", "esm-piControl")
    #varnames_in <- rep("co2_flx_ocean", t=3)
    #varnames_in <- rep("nbp", t=3)
    varnames_in <- rep("co2_flx_total", t=3)
    fromsf <- c(1950, 2951, 3001)
    tosf <- c(3000, 3062, 3131)
    tunit <- "model year"
    new_origins <- c(fromsf[1]-1950+1, fromsf[2]-1950+1, fromsf[3]-2686+736+1) # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    fromsp <- c(1051+1-852, NA, NA) # last xxx years of piControl
    tosp <- c(1001, 1051, NA)
    n_mas <- rep(120, t=3)
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
    #varnames_in <- rep("temp2", t=3)
    varnames_in <- rep("co2_flx_ocean", t=3)
    addland <- F
    echam6_global_setNA <- "land"
    #varnames_in <- rep("co2_flx_land", t=3)
    #echam6_global_setNA <- "ocean"
    #addland <- F
    #varnames_in <- rep("tos", t=3)
    #varnames_in <- rep("aCO2", t=3)
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

} else if (F) { # esm-piControl awicm1-recom
    models <- rep("echam6", t=3)
    #models <- "fesom"
    prefixes <- c("awi-esm-1-1-lr_kh800_esm-piControl_2percatm",
                  "awi-esm-1-1-lr_kh800_esm-piControl_2percfalse",
                  "awi-esm-1-1-lr_kh800_esm-piControl_2percboth")
    names_short <- paste0("ar1_", c("2percatm", "2percfalse", "2percboth"))
    names_legend <- names_short
    varnames_in <- rep("co2_flx_ocean", t=3)
    fromsf <- rep(2686, t=3)
    tosf <- c(2700, 2700, 2700)
    #tosf <- c(2760, 2700, 2700)
    new_origins <- rep(1, t=3)
    tosp <- rep(15, t=3)
    modes <- rep("timmean", t=3)
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
        varnames_in <- rep("temp2", t=3)
    } else if (F) {   
        varnames_in <- rep("tos", t=3)
        postpaths <- paste0(workpath, "/post/", models, "/regular_grid/ltm/", mode, "/", varnames_in)
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
    varnames_in <- rep("temp2", t=3)
    #varnames_in <- rep("aprt", t=3)
    #varnames_in <- c("aprt", "aprl", "aprc")
    #varnames_out_samedims <- "aprt"
    #names_legend_samedims <- c("aprt (total)", "aprl (large-scale)", "aprc (convection)")
    #varnames_in <- rep("evap", t=3)
    #varnames_in <- rep("pe", t=3)
    #varnames_in <- rep("ws", t=3)
    #varnames_in <- rep("wisoaprt_d", t=3)
    #varnames_in <- rep("wisoaprt_d_post", t=3)
    #varnames_in <- rep("wisoevap_d_post", t=3)
    #varnames_in <- rep("wisope_d_post", t=3)
    #varnames_in <- rep("c204_ICEARE_GLO", t=3)
    #varnames_in <- rep("c64_ICEARE_ARC", t=3)
    #varnames_in <- rep("c144_ICEARE_SO", t=3)
    #varnames_in <- rep("SICOMO", t=3)
    #varnames_in <- rep("lm_SICOMO_as_time_slope", t=3)
    #varnames_in <- rep("zmld", t=3)
    #varnames_in <- rep("amoc", t=3)
    #codes <- rep(101, t=3)
    #varnames_in <- rep("srad0d", t=3)
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
    #n_mas <- rep(100, t=3)
    #n_mas <- rep(150, t=3)
    #n_mas <- rep(300, t=3) # jqs paper fig 1 srad0d
    #n_mas <- c(30, 3*30, 30) 
    #n_mas <- c(90, 3*90, 90) 
    #n_mas <- rep(120, t=3)
    n_mas <- c(120, 360, 120) # jqs paper fig 1 temp2
    #n_mas <- rep(360, t=3)
    #n_mas <- c(90, 3*150, 150) # compare seasons
    #n_mas <- c(1000, 3*500, 500) # Hol-7 mean
    #n_mas <- c(1000, 6*500, 3*500) # Hol-7 mean
    #n_mas <- c(1200, 12000, 1200)
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
    n_mas <- c(120, 120, 120)
    #seasonsf <- rep("annual", t=3)
    seasonsf <- rep("NDJFMmean", t=3)
    #seasonsp <- "Feb" # cdo's default season timestamps: Feb, May, Aug, Nov
    #seasonsp <- "May"
    #seasonsp <- "Jun"
    #seasonsp <- "Aug"
    #seasonsp <- "Nov"
    #seasonsp <- "Dec"
    #varnames_in <- c("wind10_gt_1.5_sd_NPI", "u10_gt_1.5_sd_NPI", "v10_gt_1.5_sd_NPI")
    varnames_in <- c("wind10_lt_1.5_sd_NPI_minus_wind10_gt_1.5_sd_NPI", 
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
    varnames_in <- rep("temp2", t=3)
    modes <- rep("fldmean", t=3)

# =====================================
# 4 settings
} else if (F) { # reccap A B C D
    workpath <- "/work/ollie/cdanek"
    models <- rep("fesom", t=4)
    names_legend <- c("A", "B", "C", "D")
    prefixes <- paste0("reccap_", names_legend)
    names_short <- prefixes
    varnames_in <- rep("CO2f", t=4)
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
    varnames_in <- rep("co2_flx_ocean", t=4)
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
    #varnames_in <- rep("omldamax", t=4)
    varnames_in <- rep("hvel", t=4)
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
    #varnames_in <- rep("temp2", t=4)
    varnames_in <- rep("co2_flx_ocean", t=4)
    #varnames_in <- rep("co2_flx_land", t=4)
    #n_mas <- rep(60, t=4)
    #varnames_in <- rep("tos", t=4)
    #varnames_in <- rep("aCO2", t=4)
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
    #varnames_in <- rep("temp2", t=4)
    #codes <- c(167, "", "", "")
    #varnames_in <- rep("tas", t=4)
    #varnames_in <- rep("srad0", t=4)
    varnames_in <- rep("toa_imbalance", t=4)
    #varnames_in <- rep("tau_aero_550", t=4)
    #codes <- c(11, "", "", "")
    #varnames_in <- rep("srad0d", t=4)
    #codes <- c(184, "", "", "")
    #varnames_in <- rep("MOCw", t=4)
    #areas <- rep("NA", t=4)
    #depths <- rep("0-5900", t=4)
    #moc_lats <- c(26.5, 41)
    #varnames_in <- rep("siarean", t=4)
    #areas <- rep("arctic", t=4)
    #varnames_in <- rep("mlotst", t=4)
    #varnames_in <- rep("thetao", t=4)
    #varnames_in <- rep("so", t=4)
    #varnames_in <- rep("potdens", t=4)
    #areas <- rep("LSstolpe18", t=4)
    #areas <- rep("GIN2", t=4)
    #depths <- rep("0-5900", t=4)
    #depth_fromsp <- rep(-3500, t=4)
    #varnames_in <- rep("tos", t=4)
    #postpaths <- paste0(workpath, "/post/", models, "/regular_grid/ltm/", mode, "/", varnames_in)
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
    #n_mas <- rep(60, t=4)
    #n_mas <- rep(36, t=4)
    #n_mas <- rep(1, t=4)
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
    n_mas <- rep(60, t=4)
    #varnames_in <- rep("temp2", t=4)
    varnames_in <- rep("srad0", t=4)
    
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
    #n_mas <- rep(30, t=4)
    n_mas <- rep(90, t=4)
    #n_mas <- rep(120, t=4)
    #n_mas <- c(100*12, 10*10, 10*12, 10)
    #varnames_in <- rep("temp2", t=4)
    #varnames_in <- rep("tsurf", t=4)
    #varnames_in <- rep("aprt", t=4)
    #varnames_in <- rep("aprl", t=4)
    #varnames_in <- rep("aprc", t=4)
    #varnames_in <- rep("aprs", t=4)
    #varnames_in <- rep("tsurfaprt", t=4)
    #varnames_in <- c("wisoaprt_d", "wisoaprt_d", "wisoaprt_d", "wisoaprt_d_post")
    #varnames_in <- rep("wisoaprt_d_post", t=4)
    #levs <- c(2, 2, 2, 2)
    #varnames_in <- c("c64_ICEARE_ARC", "SICOMO", "c144_ICEARE_SO", "SICOMO")
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
    n_mas <- rep(36, t=4)
    varnames_in <- c("temp2", "tas", "temp2", "tas")
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
    #varnames_in <- rep("st", t=4)
    #levs <- rep(47, t=4)
    names_legend <- paste0("lsp ", names_short)
    varnames_in <- rep("lsp", t=4)
    #names_legend <- paste0("tas ", names_short)
    #varnames_in <- rep("tas", t=4)
    modes <- rep("fldmean", t=4)

} else if (F) { # phd stuff divuvt* fldint 4 settings
    workpaths <- "/work/ba0941/a270073"
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
    varnames_in <- c("divuvttot_plus_divuvsgsttot", "divuvt", "divuvteddy_plus_divuvsgsttot", "divuvsgsttot")
    cols <- c(1, 3, 2, 2)
    ltys <- c(1, 1, 1, 2)
    varnames_out_samedims <- "divuvt_budget"
    names_legend_samedims <- names_legend
    ignore_vars <- c(ignore_vars, "dx_uXtemp_meanint", "dy_vXtemp_meanint")
    depthsf <- rep("_int0-MLD", t=4)
    fromsf <- rep(1948, t=4)
    tosf <- rep(2009, t=4)
    n_mas <- rep(36, t=4)
    modes <- rep("fldint", t=4)

} else if (F) { # phd stuff lorenz energy cycle fldint 4 settings
    workpaths <- "/work/ba0941/a270073"
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
    varnames_in <- c("FeKe", "HRS", "VRS", "wbeddy")
    varnames_out_samedims <- "lorenz_energy_cycle"
    names_legend_samedims <- varnames_in
    fromsf <- rep(1948, t=4)
    tosf <- rep(2009, t=4)
    n_mas <- rep(36, t=4)
    modes <- rep("fldint", t=4)

# ==================================================
## 5 settings 
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
    #n_mas <- rep(30, t=5)
    n_mas <- c(rep(30, t=4), 4*30)
    #n_mas <- rep(30, t=5)
    #n_mas <- c(4*90, rep(90, t=5))
    #n_mas <- rep(120, t=5)
    #n_mas <- c(100*12, 10*10, 10*12, 10)
    varnames_in <- rep("temp2", t=5)
    #varnames_in <- rep("tsurf", t=5)
    #varnames_in <- rep("tsurfaprt", t=5)
    #varnames_in <- rep("aprt", t=5)
    #varnames_in <- rep("aprl", t=5)
    #varnames_in <- rep("aprc", t=5)
    #varnames_in <- rep("aprs", t=5)
    #varnames_in <- rep("evap", t=5)
    #varnames_in <- rep("pe", t=5)
    #varnames_in <- rep("quv", t=5)
    #varnames_in <- rep("quv_direction", t=5)
    #varnames_in <- rep("lm_quv_as_time_slope", t=5)
    #varnames_uv <- rep(list("lm_quv_as_time_slope"=c(u="lm_qu_as_time_slope", v="lm_qv_as_time_slope")), t=5)
    #varnames_out_samedims <- "lm_quv_as_time_slope"
    #levsf <- rep("_int1000-100hPa", t=5)
    #varnames_in <- rep("wisoaprt_d_post", t=5)
    #varnames_in <- rep("lm_wisoaprt_d_post_as_time_slope", t=5)
    #levs <- rep(2, t=5)
    #varnames_in <- rep("lm_temp2_as_time_slope", t=5)
    #varnames_in <- rep("lm_tsurf_as_time_slope", t=5)
    #varnames_in <- rep("lm_aprt_as_time_slope", t=5)
    #varnames_in <- rep("lm_aps_as_time_slope", t=5)
    #varnames_in <- rep("lm_psl_as_time_slope", t=5)
    #varnames_in <- rep("lm_THO_as_time_slope", t=5)
    #codes <- rep(2, t=5)
    #levs <- rep(6, t=5)
    #varnames_in <- rep("lm_aprt_as_time_slope", t=5)
    #varnames_in <- rep("lm_wind10_as_time_slope", t=5)
    #varnames_uv <- rep(list("lm_wind10_as_time_slope"=c(u="lm_u10_as_time_slope", v="lm_v10_as_time_slope")), t=5) # for quiver
    #varnames_out_samedims <- "lm_wind10_as_time_slope"
    #names_legend_samedims <- 
    #varnames_in <- rep("lm_act_fpc_as_time_slope", t=5)
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
} else if (F) { # awi-esm-1-1-lr_kh800 ensemble: pi (2686 to 2851 (2014) and 2936 (2100)) hist ssp126 ssp245 ssp534-over ssp585
    #models <- rep("echam6", t=6)
    models <- rep("fesom", t=6)
    prefixes <- paste0("awi-esm-1-1-lr_kh800_", 
                       c("piControl", "historical", "ssp126", "ssp245", "ssp534-over", "ssp585"))
    names_short <- c("pi", "hist", "126", "245", "534-over", "585")
    names_legend <- c("piControl", "historical", "ssp126", "ssp245", "ssp534-over", "ssp585")
    cols <- c(1, 3, 4, 5, 6, 2) # special col order
    #varnames_in <- rep("temp2", t=6)
    #varnames_in <- rep("co2_flx_ocean", t=6)
    echam6_global_setNA <- "land"
    addland <- F
    varnames_in <- rep("siextentn", t=6)
    bilinear_interp_factor <- 4
    fromsf <- c(2686, 1850, rep(2015, t=4))
    #tosf <- c(2936, 2014, 2100, 2100, 2100, 2100)
    tosf <- c(3000, 2014, 2100, 2100, 2100, 2100)
    #fromsf <- c(2851-29, 2014-29, rep(2100-29, t=4))
    new_origins <- c(1850, rep(NA, t=5)) # 2686 = 1 --> new_origin = `fromsf` - 2686 + 1 = 
    #new_origins <- c(2014-29, rep(NA, t=5))
    tosp <- c(2100, rep(NA, t=5))
    modes <- rep("select", t=6)
    #modes <- rep("fldmean", t=6)
    #modes <- rep("fldint", t=6)
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
    n_mas <- rep(30, t=6)
    #n_mas <- rep(120, t=6)
    #varnames_in <- rep("temp2", t=6)
    varnames_in <- rep("aprt", t=6)
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
    #n_mas <- rep(30, t=6)
    #n_mas <- rep(120, t=6)
    n_mas <- c(rep(10*12, t=3), rep(5*12, t=3))
    #n_mas <- c(rep(100*12, t=3), rep(20*12, t=3))
    modes <- rep("select", t=6)
    #varnames_in <- rep("amoc", t=6)
    #codes <- rep(101, t=6)
    #areas <- c("moc45to60N", "moc30to60N", "moc50N", "moc45to60N", "moc30to60N", "moc26.5N")
    #levs <- c("-285to-2180m", "-285to-2180m", "-0to-5420m", "-0to-5420m", "-0to-5420m", "-0to-5420m")
    #names_legend <- paste0(areas, " ", levs)
    varnames_in <- rep(c("aprt", "aprl", "aprc"), t=2)
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
    varnames_in <- c("wind10_gt_1.5_sd_NPI", "u10_gt_1.5_sd_NPI", "v10_gt_1.5_sd_NPI",
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
    n_mas <- rep(36, t=7)
    varnames_in <- c("temp2", "temp2", "tas", "temp2", "temp2", "tas", "temp2")
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
    #varnames_in <- rep("temp2", t=6)
    #varnames_in <- rep("co2_flx_ocean", t=6)
    varnames_in <- c(rep("co2_flx_ocean", t=6), "fgco2", "fgco2_ens_mean")
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
    n_mas <- c(rep(120, t=4), rep(1200, t=4))
    cols <- c(rep(1, t=4), rep(2, t=4))
    #varnames_in <- rep(c("c208_SST_GLO", "c210_T200_GLO", "c212_T700_GLO", "c214_T2200_GLO"), t=2)
    #varnames_in <- rep(c("c128_SST_ATL", "c130_T200_ATL", "c132_T700_ATL", "c134_T2200_ATL"), t=2)
    #varnames_in <- rep(c("cSST_GIN", "c50_T200_GIN", "c52_T700_GIN", "c54_T2200_GIN"), t=2)
    #varnames_in <- rep(c("c88_SST_LAB", "c90_T200_LAB", "c92_T700_LAB", "c94_T2200_LAB"), t=2)
    #varnames_out_samedims <- "thetao"
    varnames_in <- rep(c("c209_SSS_GLO", "c211_S200_GLO", "c213_S700_GLO", "c215_S2200_GLO"), t=2)
    #varnames_in <- rep(c("c129_SSS_ATL", "c131_S200_ATL", "c133_S700_ATL", "c135_S2200_ATL"), t=2)
    #varnames_in <- rep(c("c49_SSS_GIN", "c51_S200_GIN", "c53_S700_GIN", "c55_S2200_GIN"), t=2)
    #varnames_in <- rep(c("c89_SSS_LAB", "c91_S200_LAB", "c93_S700_LAB", "c95_S2200_LAB"), t=2)
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
    n_mas <- rep(36, t=8)
    codes <- c("", "", "", "", "", "", "", 167)
    varnames_in <- c("temp2", "temp2", "tas", "temp2", "temp2", "tas", "temp2", "temp2")
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
    #varnames_in <- rep("mlotst", t=8)
    varnames_in <- rep("omldamax", t=8)
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
    varnames_in <- c("dttemp", "divuvttot_divuvsgsttot_Ftemp", "divuvttot", "divuvt", "divuvteddy", "divuvsgsttot", "Ftemp", "divuvtrest")
    depthsf[which(varnames_in == "Ftemp")] <- "_0m"
    varnames_out_samedims <- "divuvt_budget"
    names_legend_samedims <- names_legend
    cols_samedims <- c("orange", "gray", "black", "blue", "red", "cyan", "magenta", "black")
    ltys_samedims <- c(rep(1, t=7), 2)
    ltws_samedims <- rep(2, t=8)
    modes <- rep("fldint", t=8)
    n_mas <- rep(36, t=8)
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
    varnames_in <- rep("fHarvest", t=9)
    plotprefix <- "cmip6"
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
    varnames_in <- rep("netAtmosLandCO2Flux", t=11)
    plotprefix <- "cmip6"
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
    #varnames_in <- rep("co2_flx_ocean", t=12)
    #echam6_global_setNA <- "land"
    varnames_in <- rep("co2_flx_land", t=12)
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
    varnames_in <- rep("pft_fract_box", t=12)
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

# ======================================================
# 16 settings
} else if (T) { # 15/16 reccap2 settings
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
    names_legend <- models
    names_short <- names_legend
    prefixes <- rep("reccap2_A", t=length(models))
    varnames_in <- rep("fgco2", t=length(models))
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
    #areas <- rep("gregor_etal_2019_nh-hl", t=length(models))
    #areas <- rep("gregor_etal_2019_nh-st", t=length(models))
    #areas <- rep("gregor_etal_2019_equ", t=length(models))
    #areas <- rep("gregor_etal_2019_sh-st", t=length(models))
    areas <- rep("gregor_etal_2019_sh-hl", t=length(models))
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
    varnames_in <- rep("co2_flx_total", t=18)
    plotprefix <- "cmip6"
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
    varnames_in <- rep("pft_fract_box", t=24)
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
    varnames_in <- rep("fgco2", t=24)
    plotprefix <- "cmip6"
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
    varnames_in <- rep("nbp", t=25)
    plotprefix <- "cmip6"
    modes <- rep("fldint", t=25)
    fromsf <- c(1001, 2686, 6101, 5951, 1101, 401, 401, 2250, 2000, 2250, 2250, 2701, 401, 
                2281, 3097, 3750, 2250, 2750, 2750, 2001, 401, 2001, 601, 601, 3740)
    tosf <- c(1100, 2850, 6200, 6050, 1200, 500, 500, 2349, 2099, 2349, 2349, 2800, 500, 
              2380, 3196, 3849, 2349, 2849, 2849, 2100, 500, 2100, 700, 700, 3839)

} else if (F) { # 25 levels
    models <- rep("echam6", t=25)
    prefixes <- rep("awi-esm-1-1-lr_kh800_piControl_og_restart_hl_ppm", t=25)
    varnames_in <- rep("CO2", t=25)
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
    varnames_in <- rep("CO2", t=27)
    levs <- c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 
              11000, 12000, 13000, 14000, 15000, 16000, 17000, 18000, 19000, 
              20000, 21000, 22000, 23000, 24000, 25000, 26000)
    names_short <- paste0(levs, "m")
    modes <- rep("select", t=27)
    fromsf <- rep(2685, t=27) # last piControl og year
    tosf <- rep(2685, t=27)
    new_origins <- rep(736, t=27)

} # which settings

# run
source(paste0(repopath, "/plot_echam.r"))

