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
cdo_set_rel_time <- T # conversion from absolute to relative time
cdo_run_from_script <- T # create temporary file and run long cdo command from there
#cdo_nchar_max_arglist <- 2612710 # this worked but not always dont know why
cdo_nchar_max_arglist <- 2612000
nco_nchar_max_arglist <- 131071
# --> $(getconf PAGE_SIZE)*32 = 4096*32 = 131072
# --> getconf ARG_MAX                   = 2097152
ncview_min_origin <- -4714 # ncview error: the Gregorian calendar routines; must have year >= -4714

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
    prefixs <- "dynveg"
    
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
    prefixs <- "awi-esm-1-1-lr"

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
    prefixs <- "awi-esm-1-1-lr_lgm"

} else if (T) { # Hol-T on stan
    datapaths <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/echam5"
    models <- "echam5"
    fpatterns <- "Hol-T_echam5_wiso_mm_<YYYY><MM>.nc"
    ftypes <- "l" # "f" for files (default) or "l" for links
    prefixs <- "cosmos-aso-wiso_echam5_holocene_wiso_mm"
    #fvarnames <- "temp2"
    #fvarnames <- "tsurf"
    #fvarnames <- "aprt"
    #fvarnames <- "wisoaprt"
    fvarnames <- "wisoaprt_d"
    levs_out <- 2
    #fvarnames <- "aprt_times_temp2"
    #fvarnames <- "ptemp"
    #modes <- "select"
    modes <- "fldmean"
    #modes <- "yearsum"
    #modes <- "timsum"
    froms <- "0004" # beginning of chunk 1
    #froms <- "0100"
    #tos <- "0013" 
    #tos <- "0011"
    #tos <- "0129"
    #tos <- "5903" # end of chunk 2
    tos <- "6173"
    #new_time_origins <- -6999 
    #new_time_origins <- -1
    new_time_origins <- 1
    wiso_smow_files <- "~/scripts/r/echam/wiso/SMOW.FAC.T31.nc"
    cdo_codetables <- "~/scripts/r/echam/wiso/CODES.WISO"
    cdo_partablesn <- "~/scripts/r/echam/wiso/CODES.WISO.txt"

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
    prefixs <- c("dynveg_noLUH", "dynveg_LUH")

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
    prefixs <- c("dynveg", "dynveg")

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
    prefixs <- rep("awi-esm-1-1-lr", t=3)

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
    prefixs <- rep("awi-esm-1-1-lr", t=4)
}

# https://gitlab.awi.de/paleodyn/model-analysis/blob/master/previous_scripts/ANALYSIS_calc_wiso_echam5_monmean.sh
# /ace/user/paleo/utils.ace/cosmos-wiso/echam5/calc_wiso_monmean_d.cosmos-aso.sh
cdo_known_cmds <- list("wisoaprt_d"=list(cmd=c("<cdo> -setname,wisoaprt_d -setcode,10 -mulc,1000. -subc,1. -div -div <wisoaprt> <aprt> <wiso_smow_files>")),#)#,
                                               #"-t <cdo_codetables> setpartabn,<cdo_partablesn>")))#,
                       "wisoaprl_d"=list(cmd="<cdo> -setname,wisoaprl_d -setcode,13 -mulc,1000. -subc,1. -div -div <wisoaprl> <aprl> <wiso_smow_files>"),
                       "wisoaprc_d"=list(cmd="<cdo> -setname,wisoaprc_d -setcode,14 -mulc,1000. -subc,1. -div -div <wisoaprc> <aprc> <wiso_smow_files>"),
                       "wisoaprs_d"=list(cmd="<cdo> -setname,wisoaprs_d -setcode,15 -mulc,1000. -subc,1. -div -div <wisoaprs> <aprs> <wiso_smow_files>"),
                       "wisoevap_d"=list(cmd="<cdo> -setname,wisoevap_d -setcode,19 -mulc,1000. -subc,1. -div -div <wisoevap> <evap> <wiso_smow_files>"),
                       "wisope_d"=list(cmd="<cdo> -setname,wisope_d -setcode,20 -mulc,1000. -subc,1. -div -div <wisope> <pe> <wiso_smow_files>"),
                       "wisows_d"=list(cmd="<cdo> -setname,wisows_d -setcode,11 -mulc,1000. -subc,1. -div -div <wisows> <ws> <wiso_smow_files>"),
                       "wisosn_d"=list(cmd="<cdo> -setname,wisosn_d -setcode,12 -mulc,1000. -subc,1. -div -div <wisosn> <sn> <wiso_smow_files>"),
                       "wisosnglac_d"=list(cmd="<cdo> -setname,wisoasnglac_d -setcode,33 -mulc,1000. -subc,1. -div -div <wisosnglac> <snglac> <wiso_smow_files>"),
                       "wisorunoff_d"=list(cmd="<cdo> -setname,wisorunoff_d -setcode,17 -mulc,1000. -subc,1. -div -div <wisorunoff> <runoff> <wiso_smow_files>"),
                       "aprt_times_temp2"=list(cmd=c("<cdo> -setname,aprt_times_temp2 -mul <aprt> <temp2>",
                                                     "<nco_ncatted> -O -a code,aprt_times_temp2,d,,", # delete old code
                                                     "<nco_ncatted> -O -a long_name,aprt_times_temp2,o,c,\"precipitation times temp2\"",
                                                     "<nco_ncatted> -O -a units,aprt_times_temp2,o,c,\"mm/month times degC\"")),
                       #"aprt_times_temp2"=list(cmd=c("<cdo> -setname,aprt_times_temp2 -<modes> -mul <aprt> <temp2>", # todo: mode & mul together?
                       #                              "<nco_ncatted> -O -a code,aprt_times_temp2,d,,")), # delete old code
                       "ptemp"=list(cmd=c("<cdo> -setname,ptemp -setcode,170 -div <aprt_times_temp2> <aprt>",
                                          "<nco_ncatted> -O -a long_name,ptemp,o,c,\"precipitation weighted temp2\"",
                                          "<nco_ncatted> -O -a units,ptemp,o,c,\"degC\""))
                       )

