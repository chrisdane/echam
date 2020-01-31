# input for post.echam.r

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

verbose <- 1 # 0,1
clean <- F # remove tmp files
cdo_silent <- "" # "-s" for silent or ""
cdo_force <- F # redo cdo command although outout file already exists 
cdo_OpenMP_threads <- "-P 4" # "-P n" or "" (will be irgnored on commands that do not support OMP)
cdo_wout_loop <- T # keep true; run one cdo command on all files or loop through all files
cdo_set_rel_time <- T # conversion from absolute to relative time
cdo_run_from_script <- T # create temporary file and run long cdo command from there
cdo_nchar_max_arglist <- 2612710
nco_nchar_max_arglist <- 131071
# --> $(getconf PAGE_SIZE)*32 = 4096*32 = 131072
# --> getconf ARG_MAX                   = 2097152
ncview_min_origin <- -4714 # ncview error: the Gregorian calendar routines; must have year >= -4714
add_my_time <- F # my time for ts output (needs package ncdf4) 

# ======================================================
# 1 setting
if (F) { # old hist
    datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/hist/outdata/echam"
    fpatterns <- "hist_echam6_echammon_<YYYY><MM>.nc"
    #fvarnames <- "temp2"
    fvarnames <- "srad0d"
    models <- "echam6"
    froms <- 1850
    tos <- 1851
    #season_inds <- list(c(12, 1, 2))
    modes <- "fldmean"
    #modes <- "timmean"
    suffixs <- "dynveg"
    
} else if (F) { # pi
    datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam" # 1543:1941
    if (T) {
        fpatterns <- "piControl_echam6_echammon_<YYYY><MM>.grb"
        fvarnames <- "temp2"
        codes <- 167
        #fvarnames <- "srad0d"
        #codes <- 184
    } else if (F) {
        fpatterns <- "piControl_echam6_echam_<YYYY><MM>.grb"
        #fvarnames <- "trad0"
        #codes <- 179
        fvarnames <- "srad0"
        codes <- 178
    } else if (F) {
        fpatterns <- "piControl_echam6_aeroptmon_<YYYY><MM>.grb"
        fvarnames <- "tau_aero_550_pt"
        codes <- 11
    }
    models <- "echam6"
    froms <- 1912 # last 30 years: 1912:1941; last 100 years: 1842:1941 
    tos <- 1941
    #modes <- "fldmean"
    modes <- "timmean"
    suffixs <- "awi-esm-1-1-lr"

} else if (F) { # xiaoxu
    datapaths <- "/mnt/lustre02/work/ba0989/a270064/esm-experiments/lgm_anm/outdata/echam" # 3537:2872 (n=336)
    fpatterns <- "MM_<YYYY>01.01_echam.nc"
    #fvarnames <- "temp2"
    fvarnames <- "trad0"
    models <- "echam6"
    froms <- 3537 # last 30 years start from 3843
    tos <- 3872
    #season_inds <- list(c(12, 1, 2)) # DJF
    #season_inds <- list(c(3, 4, 5)) # MAM
    #season_inds <- list(c(6, 7, 8)) # JJA
    #season_inds <- list(c(9, 10, 11)) # SON
    #modes <- "timmean" 
    modes <- "fldmean"
    suffixs <- "awi-esm-1-1-lr_lgm"

} else if (T) { # Hol-T on stan
    #reorder_inds <- data.frame(stamp_froms=c(2903, 800),
                               #stamp_tos=c(6699, 2902),
                               #calendar_froms=c(6997, 3200),
                               #calendar_tos=c(3201, 1098),
                               #count_froms=c(4, 3801),
                               #count_tos=c(3800, 5903))
    datapaths <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/echam5"
    fpatterns <- "Hol-T_echam5_wiso_mm_<YYYY><MM>.nc"
    ftypes <- "l" # "f" for files (default) or "l" for links
    fvarnames <- "temp2"
    models <- "echam5"
    froms <- "0004" 
    #tos <- "0013" 
    #tos <- "0011"
    #tos <- "0126"
    tos <- "5903"
    #new_time_origins <- -6999 
    #new_time_origins <- -0001
    new_time_origins <- 0000
    #new_time_units <- "years as %Y.%f" # <- no gaps in x-axis with ncview BUT not supported by cdo
    modes <- "fldmean"

# ======================================================
# 2 settings
} else if (F) {
    datapaths <- c("/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/hist/outdata/echam",
                   "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/hist_LUH/outdata/echam")
    fpatterns <- c(#"hist5_echam6_echammon_<YYYY>.grb",
                   "hist_echam6_echammon_<YYYY><MM>.nc",
                   "hist_LUH_echam6_echammon_<YYYY><MM>.grb")
    fvarnames <- c("temp2", "temp2")
    codes <- c(NA, 167) # necessary for grb files
    models <- c("echam6", "echam6") # for check if `cdo -t echam6` can be used
    froms <- c(1850, 1850)
    tos <- c(1851, 1851)
    #modes <- c("fldmean", "fldmean")
    modes <- c("timmean", "timmean")
    suffixs <- c("dynveg_noLUH", "dynveg_LUH")

} else if (F) {
    datapaths <- c("/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/1percCO2/outdata/echam",
                   "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/4CO2/outdata/echam")
    fpatterns <- c("1percCO2_echam6_echammon_<YYYY><MM>.nc",
                   "4CO2_echam6_echammon_<YYYY><MM>.nc")
    fvarnames <- c("temp2", "temp2")
    models <- c("echam6", "echam6") # for check if `cdo -t echam6` can be used
    froms <- c(1850, 1850)
    #tos <- c(1859, 1859)
    tos <- c(2099, 2099)
    modes <- c("fldmean", "fldmean")
    suffixs <- c("dynveg", "dynveg")

# ======================================================
# 3 settings
} else if (F) { # deck hist 1pct 4co2
    datapaths <- c("/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/hist/outdata/echam",
                   "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/1percCO2/outdata/echam",
                   "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/4CO2/outdata/echam")
    fpatterns <- c("hist_echam6_echam_<YYYY><MM>.nc",
                   "1percCO2_echam6_echam_<YYYY><MM>.nc",
                   "4CO2_echam6_echam_<YYYY><MM>.nc")
    #fvarnames <- c("temp2", "temp2", "temp2")
    #fvarnames <- rep("srad0d", t=3)
    #fvarnames <- rep("srad0", t=3)
    fvarnames <- rep("trad0", t=3)
    models <- c("echam6", "echam6", "echam6") # for check if `cdo -t echam6` can be used
    froms <- c(1850, 1850, 1850)
    #froms <- c(1985, 1985, 1985)
    #froms <- c(1985, 2070, 2070)
    #tos <- c(1851, 1851, 1851)
    #tos <- c(2014, 2014, 2014)
    tos <- c(2014, 2099, 2099)
    #season_inds <- list(c(12, 1, 2), c(12, 1, 2), c(12, 1, 2)) # DJF
    #season_inds <- list(3:5, 3:5, 3:5) # MAM
    #season_inds <- list(6:8, 6:8, 6:8) # JJA
    #season_inds <- list(9:11, 9:11, 9:11) # SON
    modes <- c("fldmean", "fldmean", "fldmean")
    #modes <- c("timmean", "timmean", "timmean")
    suffixs <- rep("awi-esm-1-1-lr", t=3)

} else if (F) { # deck pi hist 1pct 4cos2
    datapaths <- c("/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam",
                   "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/hist/outdata/echam",
                   "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/1percCO2/outdata/echam",
                   "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/4CO2/outdata/echam")
    if (F) {
        fpatterns <- c("piControl_echam6_echam_<YYYY><MM>.grb",
                       "hist_echam6_echam_<YYYY><MM>.nc",
                       "1percCO2_echam6_echam_<YYYY><MM>.nc",
                       "4CO2_echam6_echam_<YYYY><MM>.nc")
    } else if (T) {
        fpatterns <- c("piControl_echam6_echammon_<YYYY><MM>.grb",
                       "hist_echam6_echammon_<YYYY><MM>.nc",
                       "1percCO2_echam6_echammon_<YYYY><MM>.nc",
                       "4CO2_echam6_echammon_<YYYY><MM>.nc")
    } else if (F) {
        fpatterns <- c("piControl_echam6_aeroptmon_<YYYY><MM>.grb",
                       "hist_echam6_echammon_<YYYY><MM>.nc",
                       "1percCO2_echam6_echammon_<YYYY><MM>.nc",
                       "4CO2_echam6_echammon_<YYYY><MM>.nc")
    }
    #fvarnames <- rep("temp2", t=4)
    #fvarnames <- rep("srad0d", t=3)
    #fvarnames <- rep("srad0", t=3)
    #fvarnames <- rep("trad0", t=3)
    fvarnames <- rep("tau_aero_550", t=4)
    codes <- rep(11, t=4)
    models <- rep("echam6", t=4) # for check if `cdo -t echam6` can be used
    froms <- c(1842, 1850, 1850, 1850) # pi last 30 years: 1912:1941; pi last 100 years: 1842:1941
    tos <- c(1941, 2014, 2099, 2099)
    #tos <- froms
    #season_inds <- list(c(12, 1, 2), c(12, 1, 2), c(12, 1, 2)) # DJF
    #season_inds <- list(3:5, 3:5, 3:5) # MAM
    #season_inds <- list(6:8, 6:8, 6:8) # JJA
    #season_inds <- list(9:11, 9:11, 9:11) # SON
    #modes <- rep("fldmean", t=4)
    #modes <- rep("timmean", t=4)
    modes <- rep("volint", t=4)
    suffixs <- rep("awi-esm-1-1-lr", t=4)
}

