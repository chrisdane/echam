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
#mode <- "select"
mode <- "fldmean" 
#mode <- "monmean"
#mode <- "timmean" 
#mode <- "yearsum"
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

} else if (F) { # Hol-Tx10 on paleosrv or Hol-T on stan
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-Tx10"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-Tx10_main_mm"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-T"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-T_main_mm"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-T_wiso_mm"
    prefixes <- "Hol-T_stschuett_echam5_wiso" # steffens data
    models <- "echam5"
    #names_short <- "Hol-Tx10"
    #names_short <- "Hol-T"
    names_short <- "Hol-T_st"
    #names_legend <- names_short
    names_legend <- "global"
    #names_legend <- "60-90N"
    #names_legend <- "Ladoga"
    #names_legend <- "Ladoga (cosmos-aso-wiso)"
    #names_legend <- "Shuchye"
    #names_legend <- "Levinson-Lessing"
    #names_legend <- "Taymyr"
    #names_legend <- "Emanda"
    #names_legend <- "Elgygytgyn"
    #fromsf <- "0001" # Hol-Tx10
    fromsf <- "0004" # Hol-T; beginning of chunk 1
    #fromsf <- "0100"
    #tosf <- "0129"
    #tosf <- "5903" # Hol-T; end of chunk 2
    #tosf <- "6173" # 
    #tosf <- "6821" # 
    tosf <- "7000" # Hol-T; end of chunk 3
    #tosf <- "7001" # Hol-Tx10
    #new_origins <- -7000 # Hol-Tx10
    new_origins <- -6996 # Hol-T; model year 1 = 6999 BP -> model year 4 = 6999 BP - 3 = 6996 BP
    #time_frequencies <- "monthly"
    time_frequencies <- "annual"
    time_ref <- 1950 # any string, e.g. "BP", or number
    #n_mas <- 30
    n_mas <- 150
    #n_mas <- 120
    #n_mas <- 1200
    #remove_mean_froms <- -827
    #remove_mean_tos <- -827
    seasonsf <- "annual"
    #seasonsp <- "May"
    #seasonsp <- "Jun"
    #seasonsp <- "JJA"
    #seasonsp <- "Dec"
    #varnames_in <- "temp2"
    varnames_in <- "tsurf"
    #varnames_in <- "aprt"
    #varnames_in <- "wisoaprt_d"
    #levs <- 2
    #varnames_in <- "temp2aprt"
    #varnames_in <- "tsurfaprt"
    #varnames_in <- "ptemp"
    #varnames_in <- "srad0"
    #varnames_in <- "lm_wisoaprt_d_sellevel_2_as_temp2"
    #varnames_in <- "lm_wisoaprt_d_sellevel_2_as_ptemp"
    #areas <- "sibiria"
    #areas <- "60-90N"
    #areas <- "ladoga_remapnn"
    #areas <- "shuchye_remapnn"
    #areas <- "levinson-lessing_remapnn"
    #areas <- "taymyr_remapnn"
    #areas <- "emanda_remapnn"
    #areas <- "elgygytgyn_remapnn"

# =====================================
# 2 settings
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

} else if (F) { # Hol-T with vs without orbital acceleration
    #prefixes <- c("cosmos-aso-wiso_echam5_Hol-Tx10", "cosmos-aso-wiso_echam5_Hol-T")
    #prefixes <- c("cosmos-aso-wiso_echam5_Hol-Tx10_main_mm", "cosmos-aso-wiso_echam5_Hol-T_main_mm")
    #prefixes <- c("cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm", "cosmos-aso-wiso_echam5_Hol-T_wiso_mm")
    #prefixes <- c("cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm", "cosmos-aso-wiso_echam5_Hol-T_main_mm")
    #prefixes <- c("cosmos-aso-wiso_mpiom1_Hol-Tx10_timeser_ext", "cosmos-aso-wiso_mpiom1_Hol-T_timeser_ext")
    #prefixes <- c("cosmos-aso-wiso_mpiom1_Hol-Tx10_fort_75_monmean", "cosmos-aso-wiso_mpiom1_Hol-T_fort_75_monmean")
    #prefixes <- c("cosmos-aso-wiso_mpiom1_Hol-Tx10_grb_code_183_remapcon2_r120x101",
    #              "cosmos-aso-wiso_mpiom1_Hol-T_grb_code_183_remapcon2_r120x101")
    #prefixes <- c("cosmos-aso-wiso_echam5_Hol-T_wiso_mm", "Hol-T_stschuett_echam5_wiso") # me vs st annual
    prefixes <- c("cosmos-aso-wiso_echam5_Hol-T_wiso_mm", "Hol-T_echam5_wiso") # me vs me_w_st
    models <- c("echam5", "echam5")
    #models <- c("mpiom1", "mpiom1")
    #names_short <- c("Hol-Tx10", "Hol-T")
    #names_short <- c("ch", "st")
    names_short <- c("ch", "ch_w_st")
    names_legend <- names_short
    #names_legend <- c("cosmos x10", "cosmos")
    #fromsf <- c("0001", "0004")
    fromsf <- c("0004", "0004")
    #tosf <- c("7001", "6173")
    #tosf <- c("7001", "6821")
    tosf <- c("7000", "7000")
    #tosf <- c("7001", "7000")
    #new_origins <- c(-7000, -6996)
    new_origins <- c(-6996, -6996)
    time_frequencies <- c("monthly", "monthly")
    #time_frequencies <- c("annual", "annual")
    time_ref <- 1950 # any string, e.g. "BP", or number
    #fromsp <- c(-20, -20) # with respect to `new_origin` (if defined) or `fromsf`
    #tosp <- c(0, 0)
    #types <- c("o", "o")
    #varnames_in <- c("temp2", "temp2")
    #varnames_in <- c("tsurf", "tsurf")
    #varnames_in <- c("srad0d", "srad0d")
    #varnames_in <- c("albedo", "albedo")
    varnames_in <- c("wisoaprt_d", "wisoaprt_d")
    levs <- c(2, 2)
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_temp2", "lm_wisoaprt_d_sellevel_2_as_temp2")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_ptemp", "lm_wisoaprt_d_sellevel_2_as_ptemp")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_tsurf", "lm_wisoaprt_d_sellevel_2_as_tsurf")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_ptsurf", "lm_wisoaprt_d_sellevel_2_as_ptsurf")
    #varnames_in <- c("lm_temp2_as_time", "lm_temp2_as_time")
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
    #areas <- rep("moc45to60N", t=2)
    #levs <- rep("-285to-2180m", t=2)
    #areas <- rep("moc30to60N", t=2)
    #levs <- rep("-285to-2180m", t=2)
    #areas <- rep("moc26.5N", t=2)
    #areas <- rep("moc50N", t=2)
    #levs <- rep("-0to-5420m", t=2)
    #varnames_in <- c("zmld", "zmld")
    #areas <- c("LSeaSouthmld", "LSeaSouthmld")
    #areas <- c("GINmld", "GINmld")
    #areas <- c("weddelmld", "weddelmld")
    #areas <- c("ladoga_remapnn", "ladoga_remapnn")
    #areas <- c("shuchye_remapnn", "shuchye_remapnn")
    #areas <- c("levinson-lessing_remapnn", "levinson-lessing_remapnn")
    #areas <- c("taymyr_remapnn", "taymyr_remapnn")
    #areas <- c("emanda_remapnn", "emanda_remapnn")
    #areas <- c("elgygytgyn_remapnn", "elgygytgyn_remapnn")
    #areas <- c("moc45to60N", "moc30to60N", "moc50N", "moc45to60N", "moc30to60N", "moc26.5N")
    #levs <- c("-285to-2180m", "-285to-2180m", "-0to-5420m", "-0to-5420m", "-0to-5420m", "-0to-5420m")
    #seasonsf <- c("annual", "annual")
    #seasonsp <- c("Mar", "Mar")
    #seasonsp <- c("Apr", "Apr")
    #seasonsp <- c("Jun", "Jun")
    #seasonsp <- c("JJA", "JJA")
    #seasonsp <- c("Sep", "Sep")
    #seasonsp <- c("Dec", "Dec")
    #n_mas <- c(1, 1)
    #n_mas <- c(3, 300)
    #n_mas <- c(10, 100)
    #n_mas <- c(120, 120)
    #n_mas <- c(120, 1200)
    n_mas <- c(1200, 1200)
    #n_mas <- c(1200, 12000)
    #remove_mean_froms <- c(0, -179)
    #remove_mean_tos <- c(0, -179)

# =====================================
# 3 settings
} else if (F) { # awi-esm-1-1-lr deck anomlies vs pi
    models <- rep("echam6", t=3)
    #models <- rep("fesom", t=3)
    if (F) {
        prefixes <- c("hist_minus_piControl_echam6_echammon_awi-esm-1-1-lr",
                      "1percCO2_minus_piControl_echam6_echammon_awi-esm-1-1-lr",
                      "4CO2_minus_piControl_echam6_echammon_awi-esm-1-1-lr")
    } else if (F) {
        prefixes <- c("awi-esm-1-1-lr_hist_minus_piControl_monthly_mean",
                      "awi-esm-1-1-lr_1percCO2_minus_piControl_monthly_mean",
                      "awi-esm-1-1-lr_4CO2_minus_piControl_monthly_mean")
    }
    names_short <- c("hist", "1pctCO2", "abrupt-4xCO2") 
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

} else if (T) { # Hol-7 vs Hol-T with vs without orbital acceleration
    #prefixes <- c("cosmos-aso-wiso_mpiom1_Hol-7_fort_75_monmean",
    #              "cosmos-aso-wiso_mpiom1_Hol-Tx10_fort_75_monmean", 
    #              "cosmos-aso-wiso_mpiom1_Hol-T_fort_75_monmean")
    prefixes <- c("cosmos-aso-wiso_echam5_Hol-T_wiso_mm", 
                  "Hol-T_stschuett_echam5_wiso",
                  "Hol-T_echam5_wiso")
    models <- rep("echam5", t=3)
    #models <- rep("mpiom1", t=3)
    #names_short <- c("Hol-7", "Hol-Tx10", "Hol-T")
    names_short <- c("ch", "st", "me_w_st")
    names_legend <- names_short
    #names_legend <- c("7k ctrl", "7k transient x10", "7k transient")
    #fromsf <- c("0800", "0001", "0004")
    fromsf <- c("0004", "0004", "0004")
    #tosf <- c("2900", "7001", "7000")
    tosf <- c("7000", "7000", "7000")
    seasonsf <- rep("annual", t=3)
    #new_origins <- c(-9101, -7000, -6996)
    new_origins <- c(-6996, -6996, -6996)
    #time_frequencies <- rep("monthly", t=3)
    time_frequencies <- rep("annual", t=3)
    time_ref <- 1950 # any string, e.g. "BP", or number
    varnames_in <- rep("wisoaprt_d", t=3)
    #varnames_in <- rep("amoc", t=3)
    #codes <- rep(101, t=3)
    #areas <- rep("moc26.5N", t=3)
    #levs <- rep("-0to-5420m", t=3)
    levs <- rep(2, t=3)
    n_mas <- rep(120, t=3)
    #n_mas <- c(120, 120, 1200)
    #n_mas <- c(1200, 1200, 12000)
    #fromsp <- c(-7010, -7000, -6996)
    #tosp <- c(-7001, -6980, -6980)
    
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
    } else if (F) { # fesom
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
    } else if (F) {
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
    #seasonsf <- rep("Jan-Dec", t=4)
    #seasonsp <- seasonsf
    #seasonsp <- rep("JFM", t=4) 
    #seasonsp <- rep("Mar", t=4)
    #seasonsp <- rep("Jul", t=4)
    #n_mas <- rep(60, t=4)
    n_mas <- rep(36, t=4)
    #n_mas <- rep(1, t=4)
    if (F) {
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
    } else if (F) {
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
    #prefixes <- rep("cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm", t=4)
    prefixes <- rep("cosmos-aso-wiso_echam5_Hol-T_wiso_mm", t=4)
    models <- rep("echam5", t=4)
    #names_short <- rep("Hol-Tx10", t=4)
    names_short <- rep("Hol-T", t=4)
    names_legend <- c("nn", "bil", "bic", "dis")
    #fromsf <- rep("0001", t=4) # beginning of chunk 1
    fromsf <- rep("0004", t=4)
    tosf <- rep("6821", t=4)
    #tosf <- rep("7001", t=4) # end of chunk 3
    #seasonsp <- rep("Jul", t=4)
    seasonsp <- rep("Jan", t=4)
    new_origins <- rep(-6996, t=4)
    #new_origins <- rep(-7000, t=4)
    time_frequencies <- rep("monthly", t=4)
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- rep(30, t=4)
    #n_mas <- rep(120, t=4)
    #varnames_in <- rep("temp2", t=4)
    varnames_in <- rep("aprt", t=4)
    areas <- c("ladoga_remapnn", "ladoga_remapbil", "ladoga_remapbic", "ladoga_remapdis")
    #areas <- c("shuchye_remapnn", "shuchye_remapbil", "shuchye_remapbic", "shuchye_remapdis")
    #areas <- c("levinson-lessing_remapnn", "levinson-lessing_remapbil", "levinson-lessing_remapbic", "levinson-lessing_remapdis")
    #areas <- c("taymyr_remapnn", "taymyr_remapbil", "taymyr_remapbic", "taymyr_remapdis")
    #areas <- c("emanda_remapnn", "emanda_remapbil", "emanda_remapbic", "emanda_remapdis")
    #areas <- c("elgygytgyn_remapnn", "elgygytgyn_remapbil", "elgygytgyn_remapbic", "elgygytgyn_remapdis")

# ==================================================
# 6 settings
} else if (F) { # compare PLOT lakes
    #prefixes <- rep("cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm", t=6)
    prefixes <- rep("cosmos-aso-wiso_echam5_Hol-T_wiso_mm", t=6)
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
    time_frequencies <- rep("monthly", t=6)
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- rep(30, t=6)
    #n_mas <- rep(120, t=6)
    #varnames_in <- rep("temp2", t=6)
    varnames_in <- rep("aprt", t=6)
    areas <- c("ladoga_remapnn", "shuchye_remapnn", "levinson-lessing_remapnn", "taymyr_remapnn", "emanda_remapnn", "elgygytgyn_remapnn")

} else if (F) { # compare moc time series
    prefixes <- rep("cosmos-aso-wiso_echam5_Hol-Tx10_fort_75", t=6)
    models <- rep("mpiom1", t=6)
    names_short <- rep("Hol-Tx10", t=6)
    fromsf <- rep("0001", t=6) # beginning of chunk 1
    tosf <- rep("7001", t=6) # end of chunk 3
    new_origins <- rep(-7000, t=6)
    time_frequencies <- rep("monthly", t=6)
    time_ref <- 1950 # any string, e.g. "BP", or number
    #n_mas <- rep(30, t=6)
    n_mas <- rep(120, t=6)
    varnames_in <- rep("amoc", t=6)
    codes <- rep(101, t=6)
    areas <- c("moc45to60N", "moc30to60N", "moc50N", "moc45to60N", "moc30to60N", "moc26.5N")
    levs <- c("-285to-2180m", "-285to-2180m", "-0to-5420m", "-0to-5420m", "-0to-5420m", "-0to-5420m")
    names_legend <- paste0(areas, " ", levs)

# ==================================================
# 8 settings
} else if (F) { # hol-tx10 vs hol-t
    prefixes <- c(rep("cosmos-aso-wiso_mpiom1_Hol-Tx10_timeser_ext", t=4), 
                  rep("cosmos-aso-wiso_mpiom1_Hol-T_timeser_ext", t=4))
    models <- rep("mpiom1", t=8)
    names_short <- c(rep("Hol-Tx10", t=4), rep("Hol-T", t=4))
    names_legend <- names_short
    fromsf <- c(rep("0001", t=4), rep("0004", t=4))
    tosf <- c(rep("7001", t=4), rep("7000", t=4))
    new_origins <- c(rep(-7000, t=4), rep(-6996, t=4)) 
    time_frequencies <- rep("monthly", t=8)
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

} # which settings
# ==================================================

# script options
squeeze_data <- T # drop dims with length=1 (e.g. lon and lat after fldmean)
nchar_max_foutname <- 255

## general plot options
add_title <- T
add_legend <- T
p <- setDefaultPlotOptions(plot_type="png", 
                           #plot_type="pdf",
                           #family_png="Droid Sans Mono", 
                           #family_pdf="Droid Sans Mono"
                           )
                           #family_pdf="CM Roman")
alpha <- 0.2 # transparent: 0,1 (0 fully transparent)

## time series plot options
# woa13 seasons: "JFM" "AMJ" "JAS" "OND"
# other seasons: "Jan-Dec" "DJF" "MAM" "JJA" "SON" "JJ"
# months: "Feb" "Jul" "Jan"  "Jul"
add_xgrid <- F
add_ygrid <- F
add_zeroline <- T
add_unsmoothed <- F
add_smoothed <- T
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
ts_plot_each_setting_in_subplot <- F
add_data_right_yaxis_ts <- F
add_cor_data_left_and_right_ts <- T
add_data_right_yaxis_ts_mon <- T
add_data_right_yaxis_ts_an <- T
add_legend_right_yaxis <- F
plot_scatter_s1_vs_s2 <- T
#scatter_s1_vs_s1_varname <- "temp2"
scatter_s1_vs_s1_varname <- "wisoaprt_d"
plot_scatter_v1_vs_v2 <- F
varnamex <- varnamey <- "abc"
if (F) { # TOA imbalance gregory et al. 2004 stuff 
    varnamex <- "temp2"
    varnamey <- "toa_imbalance"
} else if (F) { # temp2 vs precipitation weighted temp2
    varnamex <- "temp2"
    varnamey <- "ptemp"
} else if (F) {
    varnamex <- "temp2"
    varnamey <- "wisoaprt_d"
}
add_1to1_line_scatter <- T

## time vs depth:
add_ts_to_time_vs_depth <- T

## special
plot_redfit <- F

## map plot options
proj <- "rectangular" #"rectangular"
add_land <- T
reorder_lon_from_0360_to_180180 <- T
add_grid <- F

