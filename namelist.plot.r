# input for plot.echam.r

# host options
machine <- system("hostname -f", intern=T)
message(paste0("Run on ", machine, ":"))
if (regexpr("ollie", machine) != -1 ||
    regexpr("prod-", machine) != -1 ||
    regexpr("fat-", machine) != -1) {
    machine_tag <- "ollie"
    homepath <- "~/scripts/r"
    workpath <- "/work/ollie/cdanek"
} else if (regexpr("hpc.dkrz", machine) != -1) {
    machine <- substr(machine, 1, regexpr(".hpc.dkrz", machine) - 1)
    machine_tag <- "mistral"
    homepath <- "~/scripts/r"
    #workpath <- "/work/ba0941/a270073"
    workpath <- "/work/ab0246/a270073"
} else if (regexpr("paleosrv", machine) != -1) {
    machine <- substr(machine, 1, regexpr(".awi.de", machine) - 1)
    machine_tag <- "paleosrv"
    homepath <- "~/scripts/r"
    workpath <- "/isibhv/projects/paleo_work/cdanek"
} else if (regexpr("stan", machine) != -1) {
    machine <- substr(machine, 1, regexpr(".awi.de", machine) - 1)
    machine_tag <- "stan"
    homepath <- "~/scripts/r"
    workpath <- "/ace/user/cdanek"
} else {
    message(paste0("   (unknown machine, use default paths)"))
    homepath <- "~/scripts/r"
    workpath <- homepath
}
message(paste0("   homepath = ", homepath))
message(paste0("   workpath = ", workpath))

# options across settings
# echam:
mode <- "select"
#mode <- "fldmean" 
#mode <- "timmean" 
#mode <- "timsum"
#mode <- "zonmean"
#mode <- "volint"
# fesom:
#mode <- "moc_depth"
#mode <- "moc_ts"
#mode <- "mean"
#mode <- "sum"
#mode <- "area"
#mode <- "depth"

# =====================================
# 1 setting
if (F) { # awi-esm-1-1-lr hist
    #prefixes <- "hist_echam6_echammon_awi-esm-1-1-lr"
    prefixes <- "hist_echam6_echammon_yearmean_awi-esm-1-1-lr"
    models <- "echam6"
    names_short <- "hist_wrt_1961_90"
    names_legend <- "historical"
    fromsf <- 1850
    tosf <- 2014
    varnames_in <- "temp2"
    n_mas <- 1
    cols <- "#E41A1C"
    remove_mean_froms <- 1961
    remove_mean_tos <- 1990

} else if (F) { # Hol-Tx10
    prefixes <- "cosmos-aso-wiso_echam5_Hol-Tx10"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-Tx10_main_mm"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm"
    models <- "echam5"
    names_short <- "Hol-Tx10"
    names_legend <- names_short
    fromsf <- "0001"
    tosf <- "7001"
    new_origins <- -7000
    time_frequencies <- "monthly"
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- 120
    #remove_mean_froms <- 0
    #remove_mean_tos <- 0
    #seasonsp <- "Dec"
    #seasonsp <- "Jun"
    #varnames_in <- "temp2"
    #varnames_in <- "srad0d"
    #varnames_in <- "wisoaprt_d"
    #levs <- 2
    #varnames_in <- "ptemp"
    #varnames_in <- "lm_wisoaprt_d_sellevel_2_as_temp2"
    varnames_in <- "lm_wisoaprt_d_sellevel_2_as_ptemp"

} else if (F) { # Hol-T
    prefixes <- "cosmos-aso-wiso_echam5_Hol-T"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-T_main_mm"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-T_wiso_mm"
    models <- "echam5"
    names_short <- "Hol-T"
    names_legend <- names_short
    fromsf <- "0004" # beginning of chunk 1
    #fromsf <- "0100"
    #tosf <- "0129"
    #tosf <- "5903" # end of chunk 2
    tosf <- "6173" # end of chunk 3
    new_origins <- -6996 # model year 1 = 6999 BP -> model year 4 = 6999 BP - 3 = 6996 BP
    time_frequencies <- "monthly"
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- 120
    #remove_mean_froms <- -827
    #remove_mean_tos <- -827
    #seasonsp <- "Jun"
    #seasonsp <- "Dec"
    #varnames_in <- "temp2"
    #varnames_in <- "tsurf"
    #varnames_in <- "aprt"
    #varnames_in <- "wisoaprt_d"
    #varnames_in <- "ptemp"
    #varnames_in <- "srad0"
    varnames_in <- "lm_wisoaprt_d_sellevel_2_as_temp2"
    #varnames_in <- "lm_wisoaprt_d_sellevel_2_as_ptemp"
    #areas <- "sibiria"
    #levs <- 2

# =====================================
# 2 settings
} else if (F) { # awi-esm-1-1-lr 1pct 4CO2
    #models <- rep("echam6", t=2)
    models <- rep("fesom", t=2)
    if (T) {
        prefixes <- c("awi-esm-1-1-lr_1percCO2_monthly_mean",
                      "awi-esm-1-1-lr_4CO2_monthly_mean")
    }
    names_short <- c("1pctCO2", "abrupt-4xCO2") 
    if (T) {
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
    if (T) { # transient pi last 100
        fromsf <- c(1850, 1850)
        tosf <- c(2099, 2099)
    } else if (F) { # ltm last 30
        fromsf <- c(2070, 2070)
        tosf <- c(2099, 2099)
    }
    #n_mas <- rep(60, t=2)
    n_mas <- rep(36, t=2)
    #n_mas <- rep(1, t=2)
    if (T) {
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

} else if (F) { # Hol-T with versus without orbital acceleration
    prefixes <- c("cosmos-aso-wiso_echam5_Hol-Tx10", "cosmos-aso-wiso_echam5_Hol-T")
    #prefixes <- c("cosmos-aso-wiso_echam5_Hol-Tx10_main_mm", "cosmos-aso-wiso_echam5_Hol-T_main_mm")
    #prefixes <- c("cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm", "cosmos-aso-wiso_echam5_Hol-T_wiso_mm")
    models <- c("echam5", "echam5")
    names_short <- c("Hol-Tx10", "Hol-T")
    names_legend <- names_short
    fromsf <- c("0001", "0004")
    #tosf <- c("7001", "6173")
    tosf <- c("7001", "6821")
    new_origins <- c(-7000, -6996) 
    time_frequencies <- c("monthly", "monthly")
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- c(1, 1)
    n_mas <- c(120, 120)
    #remove_mean_froms <- c(0, -179)
    #remove_mean_tos <- c(0, -179)
    #seasonsf <- c("annual", "annual")
    #seasonsp <- c("Jun", "Jun")
    #seasonsp <- c("Dec", "Dec")
    #seasonsp <- c("annual", "annual")
    #varnames_in <- c("temp2", "temp2")
    #varnames_in <- c("srad0d", "srad0d")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_temp2", "lm_wisoaprt_d_sellevel_2_as_temp2")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_ptemp", "lm_wisoaprt_d_sellevel_2_as_ptemp")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_tsurf", "lm_wisoaprt_d_sellevel_2_as_tsurf")
    varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_ptsurf", "lm_wisoaprt_d_sellevel_2_as_ptsurf")
    
} else if (F) { # temp2 vs ptemp of Hol-T
    prefixes <- rep("cosmos-aso-wiso_echam5_Hol-T_wiso_mm", t=2)
    models <- rep("echam5", t=2)
    names_short <- rep("Hol-T", t=2)
    names_legend <- c("temp2", "ptemp2")
    fromsf <- rep("0004", t=2) # beginning of chunk 1
    #fromsf <- "0100"
    #tosf <- "0129"
    #tosf <- "5903" # end of chunk 2
    tosf <- rep("6173", t=2) # end of chunk 3
    new_origins <- rep(-6996, t=2) # model year 1 = 6999 BP -> model year 4 = 6999 BP - 3 = 6996 BP
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- rep(120, t=2)
    varnames_in <- c("temp2", "ptemp")

# =====================================
# 3 settings
} else if (F) { # awi-esm-1-1-lr deck anomlies vs pi
    models <- rep("echam6", t=3)
    #models <- rep("fesom", t=3)
    if (T) {
        prefixes <- c("hist_minus_piControl_echam6_echammon_awi-esm-1-1-lr",
                      "1percCO2_minus_piControl_echam6_echammon_awi-esm-1-1-lr",
                      "4CO2_minus_piControl_echam6_echammon_awi-esm-1-1-lr")
    } else if (F) {
        prefixes <- c("awi-esm-1-1-lr_hist_minus_piControl_monthly_mean",
                      "awi-esm-1-1-lr_1percCO2_minus_piControl_monthly_mean",
                      "awi-esm-1-1-lr_4CO2_minus_piControl_monthly_mean")
    }
    names_short <- c("hist", "1pctCO2", "abrupt-4xCO2") 
    if (T) {
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
    } else if (T) {
        names_legend <- c(paste0("historical ", fromsf[1], "-", tosf[1], " mean minus piControl"),
                          #paste0("historical ", fromsp[2], "-", tosp[2], " mean"),
                          eval(substitute(expression(paste("1%CO"[2], " last 30 years mean minus piControl")))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " last 30 years mean minus piControl")))))
    }

# =====================================
# 4 settings
} else if (F) { # awi-esm-1-1-lr deck
    #models <- rep("echam6", t=4)
    models <- rep("fesom", t=4)
    if (F) {
        prefixes <- c("piControl_echam6_echammon_awi-esm-1-1-lr",
                      "hist_echam6_echammon_awi-esm-1-1-lr",
                      "1percCO2_echam6_echammon_awi-esm-1-1-lr",
                      "4CO2_echam6_echammon_awi-esm-1-1-lr")
    } else if (F) {
        prefixes <- c("piControl_echam6_echam_awi-esm-1-1-lr",
                      "hist_echam6_echam_awi-esm-1-1-lr",
                      "1percCO2_echam6_echam_awi-esm-1-1-lr",
                      "4CO2_echam6_echam_awi-esm-1-1-lr")
    } else if (F) {
        prefixes <- c("piControl_echam6_echam_yearmean_awi-esm-1-1-lr",
                      "hist_echam6_echam_yearmean_awi-esm-1-1-lr",
                      "1percCO2_echam6_echam_yearmean_awi-esm-1-1-lr",
                      "4CO2_echam6_echam_yearmean_awi-esm-1-1-lr")
    } else if (F) {
        prefixes <- c("piControl_echam6_echammon_yearmean_awi-esm-1-1-lr",
                      "hist_echam6_echammon_yearmean_awi-esm-1-1-lr",
                      "1percCO2_echam6_echammon_yearmean_awi-esm-1-1-lr",
                      "4CO2_echam6_echammon_yearmean_awi-esm-1-1-lr")
    } else if (F) {
        prefixes <- c("piControl_echam6_aeroptmon_awi-esm-1-1-lr",
                      "hist_echam6_echammon_awi-esm-1-1-lr",
                      "1percCO2_echam6_echammon_awi-esm-1-1-lr",
                      "4CO2_echam6_echammon_awi-esm-1-1-lr")
    } else if (T) { # fesom
        prefixes <- c("awi-esm-1-1-lr_piControl_monthly_mean",
                      "awi-esm-1-1-lr_hist_monthly_mean",
                      "awi-esm-1-1-lr_1percCO2_monthly_mean",
                      "awi-esm-1-1-lr_4CO2_monthly_mean")
    }
    names_short <- c("piControl", "hist", "1pctCO2", "abrupt-4xCO2") 
    text_cols <- c("black", "#E41A1C", "#377EB8", "#1B9E77")
    scatterpchs <- c(4, 16, 16, 16)
    codes <- rep("", t=4)
    if (F) {
        varnames_in <- rep("temp2", t=4)
        codes <- c(167, "", "", "")
    } else if (F) {
        varnames_in <- rep("srad0", t=4)
    } else if (F) {
        varnames_in <- rep("toa_imbalance", t=4)
    } else if (F) {
        varnames_in <- rep("tau_aero_550", t=4)
        codes <- c(11, "", "", "")
    } else if (F) {
        varnames_in <- rep("srad0d", t=4)
        codes <- c(184, "", "", "")
    } else if (F) { # fesom
        varnames_in <- rep("MOCw", t=4)
        areas <- rep("NA", t=4)
        depths <- rep("0-5900", t=4)
        moc_lats <- c(26.5, 41)
    } else if (F) {
        varnames_in <- rep("siarean", t=4)
        areas <- rep("arctic", t=4)
    } else if (T) {
        varnames_in <- rep("mlotst", t=4)
        varnames_in <- rep("thetao", t=4)
        #varnames_in <- rep("so", t=4)
        #varnames_in <- rep("potdens", t=4)
        areas <- rep("LSstolpe18", t=4)
        #areas <- rep("GIN2", t=4)
        depths <- rep("0-5900", t=4)
        remove_mean_froms <- c(1849, 1850, 1850, 1850)
        remove_mean_tos <- remove_mean_froms
        depth_fromsp <- rep(-3500, t=4)
    } else if (F) {
        varnames_in <- rep("tos", t=4)
        postpaths <- paste0(workpath, "/post/", models, "/regular_grid/ltm/", mode, "/", varnames_in)
        reg_dxs <- reg_dys <- rep("0.250", t=4)
    }
    if (F) { # transient pi last 100
        fromsf <- c(1842, 1850, 1850, 1850)
        tosf <- c(1941, 2014, 2099, 2099)
        new_origins <- c(1842-91, NA, NA, NA)
        #fromsp <- c(1849-99, 2014-29, 2099-29, 2099-29)
        #tosp <- c(1849, 2014, 2099, 2099)
        fromsp <- c(1849-99, 1850, 1850, 1850)
        tosp <- c(1849, 2014, 2099, 2099)
    } else if (T) { # tranient pi last 30
        fromsf <- c(1912, 1850, 1850, 1850)
        tosf <- c(1941, 2014, 2099, 2099)
        new_origins <- c(1912-91, NA, NA, NA)
        fromsp <- c(1849-29, 1850, 1850, 1850)
        tosp <- c(1849, 2014, 2099, 2099)
    } else if (F) { # ltm last 30
        fromsf <- c(1912, 1985, 2070, 2070)
        tosf <- c(1941, 2014, 2099, 2099)
    }
    #seasonsf <- rep("Jan-Dec", t=4)
    #seasonsp <- seasonsf
    #seasonsp <- rep("JFM", t=4) 
    #seasonsp <- rep("Mar", t=4)
    #seasonsp <- rep("Jul", t=4)
    #n_mas <- rep(60, t=4)
    n_mas <- rep(36, t=4)
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
        prefixes <- c("hist_echam6_echammon_awi-esm-1-1-lr",
                      "1percCO2_echam6_echammon_awi-esm-1-1-lr",
                      "4CO2_echam6_echammon_awi-esm-1-1-lr",
                      "MM_01.01_echam_awi-esm-1-1-lr_lgm")
    } else if (T) {
        prefixes <- c("hist_echam6_echam_awi-esm-1-1-lr",
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
    } else if (T) {
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
    
} else if (T) { # compare cdo remap* Hol-T*
    prefixes <- rep("cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm", t=4)
    #prefixes <- rep("cosmos-aso-wiso_echam5_Hol-T_wiso_mm", t=4)
    models <- rep("echam5", t=4)
    names_short <- rep("Hol-Tx10", t=4)
    #names_short <- rep("Hol-T", t=4)
    names_legend <- c("nn", "bil", "bic", "dis")
    fromsf <- rep("0001", t=4) # beginning of chunk 1
    #fromsf <- rep("0004", t=4)
    #tosf <- rep("6821", t=4)
    tosf <- rep("7001", t=4) # end of chunk 3
    seasonsp <- rep("Jul", t=4)
    #seasonsp <- rep("Jan", t=4)
    #new_origins <- rep(-6996, t=4)
    new_origins <- rep(-7000, t=4)
    time_frequencies <- rep("monthly", t=4)
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- rep(30, t=4)
    #n_mas <- rep(120, t=4)
    varnames_in <- rep("temp2", t=4)
    #areas <- c("ladoga_remapnn", "ladoga_remapbil", "ladoga_remapbic", "ladoga_remapdis")
    #areas <- c("shuchye_remapnn", "shuchye_remapbil", "shuchye_remapbic", "shuchye_remapdis")
    #areas <- c("levinson-lessing_remapnn", "levinson-lessing_remapbil", "levinson-lessing_remapbic", "levinson-lessing_remapdis")
    areas <- c("taymyr_remapnn", "taymyr_remapbil", "taymyr_remapbic", "taymyr_remapdis")
    #areas <- c("emanda_remapnn", "emanda_remapbil", "emanda_remapbic", "emanda_remapdis")
    #areas <- c("elgygytgyn_remapnn", "elgygytgyn_remapbil", "elgygytgyn_remapbic", "elgygytgyn_remapdis")

} # which settings
# ==================================================

# script options
squeeze_data <- T # drop dims with length=1 (e.g. lon and lat after fldmean)

# general plot options
add_title <- T
add_legend <- T
p <- setDefaultPlotOptions(plot_type="png", 
                           #plot_type="pdf",
                           #family_png="Droid Sans Mono", 
                           #family_pdf="Droid Sans Mono"
                           )
                           #family_pdf="CM Roman")
alpha <- 0.2 # transparent: 0,1 (0 fully transparent)

# time series plot options
# woa13 seasons: "JFM" "AMJ" "JAS" "OND"
# other seasons: "Jan-Dec" "DJF" "MAM" "JJA" "SON" "JJ"
# months: "Feb" "Jul" "Jan"  "Jul"
add_xgrid <- F
add_ygrid <- F
add_zeroline <- T
add_unsmoothed <- T
add_smoothed <- T
# time:
add_sd <- F
add_linear_trend <- F
add_nonlinear_trend <- F
scale_ts <- F
ts_highlight_seasons <- list(bool=F, suffix="") # default
if (F) {
    ts_highlight_seasons <- list(bool=T,
                                 seasons=c("DJF", "MAM", "JJA", "SON"),
                                 #t="l",
                                 t="p",
                                 #cols=c("blue", "darkgreen", "red", "brown")
                                 cols=rgb(t(col2rgb(c("blue", "darkgreen", "red", "brown"))/255), alpha=alpha),
                                 ltys=c(1,2,3,4),
                                 lwds=c(1,1,1,1),
                                 #pchs=1:4,
                                 pchs=c(16, 16, 16, 16),
                                 suffix="_highlight_seasons") 
}
add_first_data_point <- F
add_data_right_yaxis_ts <- T
add_data_right_yaxis_ts_mon <- F
add_legend_right_yaxis <- F
plot_scatter_var1_vs_var2 <- T
add_1to1_line_scatter <- T
# time vs depth:
add_ts_to_time_vs_depth <- T

plot_redfit <- F

# map plot options
proj <- "rectangular" #"rectangular"
add_land <- T
reorder_lon_from_0360_to_180180 <- F
add_grid <- F

