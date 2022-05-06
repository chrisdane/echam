# r

# input for post_echam.r

# load defaults
repopath <- "~/scripts/r/echam"
repopath <- normalizePath(repopath, mustWork=T) # error if not found
source(paste0(repopath, "/namelist.general.post.r"))
workpath <- host$workpath

# ======================================================
# 1 setting
if (F) { # old hist
    datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/hist/outdata/echam"
    fpatterns <- "hist_echam6_echammon_<year><mon>.nc"
    #fvarnames <- "temp2"
    fvarnames <- "srad0d"
    models <- "echam6"
    froms <- 1850
    tos <- 1851
    #season_inds <- list(c(12, 1, 2))
    modes <- "fldmean"
    #modes <- "timmean"
    prefixes <- "dynveg"
    
} else if (F) { # my stupid cmip6 awi-esm-1-1-lr pi chunks
    if (F) { # chunk 1: restart_from_hu_oceanonly 2701 to 2702 
        datapaths <- "/work/ab0246/a270073/awicm-CMIP6/restart_from_hu_oceanonly/outdata/echam"
        fpatterns <- "restart_from_hu_oceanonly_echam6_g3bid_<year><mon>.grb"
        froms <- 2701
        tos <- 2702
    } else if (F) { # chunk 2: restart_from_restart_from_hu_oceanonly 2703 to 2710
        datapaths <- "/work/ab0246/a270073/awicm-CMIP6/restart_from_restart_from_hu_oceanonly/outdata/echam"
        fpatterns <- "restart_from_restart_from_hu_oceanonly_echam6_g3bid_<year><mon>.grb"
        froms <- 2703
        tos <- 2710
    } else if (F) { # pi-ctrl: 2711-2869
        datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL/outdata/echam"
        fpatterns <- "PI-CTRL_echam6_g3bid_<year><mon>.grb"
        froms <- 2750 # 2711 to 2749 are missing?!
        tos <- 2869
    } else if (F) { # pi-ctrl2: 2870-2899 
        datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL2/outdata/echam"
        fpatterns <- "PI-CTRL2_echam6_g3bid_<year><mon>.grb"
        froms <- 2870
        tos <- 2899
    } else if (F) { # pi-ctrl3: 2900-2910
        datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL3/outdata/echam"
        fpatterns <- "PI-CTRL3_echam6_g3bid_<year><mon>.grb"
        froms <- 2900
        tos <- 2910
    } else if (F) { # pi-ctrl4: 2911:2999
        datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL4/outdata/echam"
        fpatterns <- "PI-CTRL4_echam6_g3bid_<year><mon>.grb"
        froms <- 2911
        tos <- 2999
    }
    models <- "echam6"
    prefixes <- "awi-esm-1-1-lr_piControl_g3bid_daily"
    fvarnames <- "temp2"
    codes <- 167
    modes <- "fldmean"

} else if (F) { # cmip6 deck
    models <- "echam6"
    #models <- "jsbach"
    #models <- "mpiom1"
    #models <- "fesom"
    # awi-cm-1-1-lr
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_false/PI-CTRL_nodynveg2/outdata/echam"
    #datapaths <- "/work/ab0995/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_false/PI-CTRL_nodynveg2/outdata/echam"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_false/historical/outdata/echam"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_false/historical/outdata/jsbach"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_false/1percCO2/outdata/echam"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_false/4CO2/outdata/echam"
    # awi-esm-1-1-lr
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam" # 1543:1941
    datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/outdata/echam" # 1955:2119
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/outdata/jsbach"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/outdata/fesom"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/1percCO2/outdata/fesom"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/day/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/Amon/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/Omon/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/SImon/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/Emon/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/historical/r1i1p1f1/3hr/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/historical/r1i1p1f1/6hrPlev/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/historical/r1i1p1f1/6hrLev/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/historical/r1i1p1f1/day/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/historical/r1i1p1f1/Eday/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/historical/r1i1p1f1/SIday/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/historical/r1i1p1f1/Amon/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/historical/r1i1p1f1/AERmon/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/1pctCO2/r1i1p1f1/Omon/<fvarnames>/gn/v20200212"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/PMIP/AWI/AWI-ESM-1-1-LR/midHolocene/r1i1p1f1/day/<fvarnames>/gn/v20200212" # 3106:3205
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/PMIP/AWI/AWI-ESM-1-1-LR/midHolocene/r1i1p1f1/Amon/<fvarnames>/gn/v20200212" # 3106:3205
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/PMIP/AWI/AWI-ESM-1-1-LR/lgm/r1i1p1f1/Omon/<fvarnames>/gn/v20200212"
    # awi-cm-1-2-lr
    #datapaths <- "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_HIST/outdata/echam"
    # awi-cm-1-1-mr; piControl: 2401-2900; piControl-2650-2799 = deck-1850-1999
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/historical/r1i1p1f1/day/<fvarnames>/gn/v20181218"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/Amon/<fvarnames>/gn/v20191015" 
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/SImon/<fvarnames>/gn/v20181218/"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/SImon/<fvarnames>/gn/v20191015"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/historical/r1i1p1f1/Amon/<fvarnames>/gn/v20200720"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/1pctCO2/r1i1p1f1/Amon/<fvarnames>/gn/v20191015"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/abrupt-4xCO2/r1i1p1f1/Amon/<fvarnames>/gn/v20191015"
    # mpi-esm1-2-lr
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Oday/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Omon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Omon/<fvarnames>/gn/v20200909"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/SImon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/historical/r1i1p1f1/day/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/historical/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/historical/r1i1p1f1/Omon/<fvarnames>/gn/v20190710/"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/1pctCO2/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/1pctCO2/r1i1p1f1/Oday/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/1pctCO2/r1i1p1f1/Omon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/1pctCO2/r1i1p1f1/Omon/<fvarnames>/gn/v20200909"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/1pctCO2/r1i1p1f1/SImon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/abrupt-4xCO2/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    # mpi-esm1-2-hr
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/piControl/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/piControl/r1i1p1f1/Oday/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/piControl/r1i1p1f1/Omon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/piControl/r1i1p1f1/SImon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/historical/r1i1p1f1/day/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/historical/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/1pctCO2/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/1pctCO2/r1i1p1f1/Oday/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/1pctCO2/r1i1p1f1/Omon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/1pctCO2/r1i1p1f1/SImon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/abrupt-4xCO2/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    # awi-cm-1-1-lr
   # fpatterns <- "historical_jsbach_jsbachmon_<year><mon>.grb"
    # awi-esm-1-1-lr
    fpatterns <- "PI-CTRL6_echam6_co2_<year><mon>.grb"
    #fpatterns <- "PI-CTRL6_jsbach_jsbachmon_<year><mon>.grb"
    #fpatterns <- "PI-CTRL6_fesom_<fvarnames>_<year>0101.nc"
    #fpatterns <- "1percCO2_fesom_<fvarnames>_<year>0101.nc"
    #fpatterns <- "historical_echam6_echamday_<year><mon>.grb"
    #fpatterns <- "<fvarnames>_day_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<year><mon_from>-<year><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Emon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<year><mon_from>-<year><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Omon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_3hr_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<year_from>01010300-<year_to>01010000.nc"
    #fpatterns <- "<fvarnames>_6hrPlev_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<year_from>01010300-<year_to>12312100.nc"
    #fpatterns <- "<fvarnames>_6hrLev_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<year_from>01010600-<year_to>01010000.nc"
    #fpatterns <- "<fvarnames>_day_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_AERmon_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Eday_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_SIday_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_AWI-ESM-1-1-LR_1pctCO2_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-ESM-1-1-LR_midHolocene_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_day_AWI-ESM-1-1-LR_midHolocene_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_AWI-ESM-1-1-LR_lgm_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    # awi-cm-1-1-mr
    #fpatterns <- "<fvarnames>_Amon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<year><mon_from>-<year><mon_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_day_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_<year>0101-<year>1231.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_<year><mon_from>-<year><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<year><mon_from>-<year><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-CM-1-1-MR_abrupt-4xCO2_r1i1p1f1_gn_<year><mon_from>-<year><mon_to>.nc"
    # mpi-esm1-2-lr
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Oday_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-LR_historical_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-LR_historical_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_day_MPI-ESM1-2-LR_historical_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-LR_1pctCO2_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Oday_MPI-ESM1-2-LR_1pctCO2_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-LR_1pctCO2_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_MPI-ESM1-2-LR_1pctCO2_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-LR_abrupt-4xCO2_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    # mpi-esm1-2-hr
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-HR_piControl_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Oday_MPI-ESM1-2-HR_piControl_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-HR_piControl_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_MPI-ESM1-2-HR_piControl_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-HR_1pctCO2_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Oday_MPI-ESM1-2-HR_1pctCO2_r1i1p1f1_gn_<year_from>0101-<year_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-HR_1pctCO2_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_MPI-ESM1-2-HR_1pctCO2_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-HR_abrupt-4xCO2_r1i1p1f1_gn_<year_from><mon_from>-<year_to><mon_to>.nc"
    #fpatterns <- "PI-CTRL_nodynveg2_echam6_echam_<year><mon>.grb"
    #fpatterns <- "PI-CTRL_nodynveg2_fesom_<fvarnames>_<year>0101.nc"
    #fpatterns <- "PI-CTRL6_echam6_echam_<year><mon>.grb"
    #fpatterns <- "piControl_echam6_echam_<year><mon>.grb"
    #fpatterns <- "piControl_echam6_echammon_<year><mon>.grb"
    #fpatterns <- "piControl_echam6_aeroptmon_<year><mon>.grb"
    #fpatterns <- "1percCO2_echam6_echam_<year><mon>.grb"
    #fpatterns <- "1percCO2_echam6_echam_<year><mon>.nc"
    #fpatterns <- "4CO2_echam6_echam_<year><mon>.grb"
    #fpatterns <- "4CO2_echam6_echam_<year><mon>.nc"
    #fpatterns <- "CMIP6_HIST_echam6_echam_<year><mon>.grb"
    #prefixes <- "awi-cm-1-1-lr_piControl"
    #prefixes <- "awi-cm-1-1-lr_historical"
    #prefixes <- "awi-cm-1-1-lr_1percCO2"
    #prefixes <- "awi-cm-1-1-lr_4CO2"
    prefixes <- "awi-esm-1-1-lr_piControl"
    #prefixes <- "awi-esm-1-1-lr_piControl_day"
    #prefixes <- "awi-esm-1-1-lr_piControl_Eday"
    #prefixes <- "awi-esm-1-1-lr_piControl_SIday"
    #prefixes <- "awi-esm-1-1-lr_piControl_mistral_esm"
    #prefixes <- "awi-esm-1-1-lr_historical_3hr"
    #prefixes <- "awi-esm-1-1-lr_historical_6hrPlev"
    #prefixes <- "awi-esm-1-1-lr_historical_6hrLev"
    #prefixes <- "awi-esm-1-1-lr_historical_day"
    #prefixes <- "awi-esm-1-1-lr_historical_AERmon"
    #prefixes <- "awi-esm-1-1-lr_historical_Amon"
    #prefixes <- "awi-esm-1-1-lr_1percCO2"
    #prefixes <- "awi-esm-1-2-lr_historical"
    #prefixes <- "awi-esm-1-1-lr_midHolocene"
    #prefixes <- "awi-esm-1-1-lr_midHolocene_day"
    #prefixes <- "awi-esm-1-1-lr_lgm"
    #prefixes <- "awi-cm-1-1-mr_piControl"
    #prefixes <- "awi-cm-1-1-mr_historical_day"
    #prefixes <- "awi-cm-1-1-mr_historical"
    #prefixes <- "awi-cm-1-1-mr_1percCO2"
    #prefixes <- "awi-cm-1-1-mr_4CO2"
    #prefixes <- "mpi-esm1-2-lr_piControl"
    #prefixes <- "mpi-esm1-2-lr_historical"
    #prefixes <- "mpi-esm1-2-lr_historical_day"
    #prefixes <- "mpi-esm1-2-lr_1percCO2"
    #prefixes <- "mpi-esm1-2-lr_4CO2"
    #prefixes <- "mpi-esm1-2-hr_piControl"
    #prefixes <- "mpi-esm1-2-hr_historical"
    #prefixes <- "mpi-esm1-2-hr_historical_day"
    #prefixes <- "mpi-esm1-2-hr_1percCO2"
    #prefixes <- "mpi-esm1-2-hr_4CO2"
    #fvarnames <- "temp2"
    #codes <- 167
    #fvarnames <- "srad0"
    #codes <- 178
    #fvarnames <- "trad0"
    #codes <- 179
    #fvarnames <- "srad0d"
    #codes <- 184
    #fvarnames <- "tau_aero_550_pt"
    #codes <- 11
    #fvarnames <- "od550aer" 
    #fvarnames <- "tsi"
    #codes <- 102
    #fvarnames <- "toa_imbalance"
    #fvarnames <- "tas" # 2m
    #fvarnames <- "ta"
    #cdo_before_calcs <- "ml2pl,50000"
    #fvarnames <- "ts" # = cmor tsurf
    #fvarnames <- "rsdt" # = cmor srad0d
    #fvarnames <- "rsut"
    #fvarnames <- "rlut"
    #fvarnames <- "clt" # = cmor aclcov
    #fvarnames <- "rss" # = cmor srads
    #fvarnames <- "rsus" # = cmor sradsu
    codes <- 6
    fvarnames <- "co2_flx_land"
    #codes <- 7
    #fvarnames <- "co2_flx_ocean"
    #codes <- 8
    #fvarnames <- "co2_burden"
    #codes <- 12
    #fvarnames <- "cover_fract"
    #codes <- 20
    #fvarnames <- "veg_ratio_max_mean"
    #fvarnames <- "tosga"
    #fvarnames <- "tob"
    #fvarnames <- "sitemptop"
    #fvarnames <- "si"
    #fvarnames <- "siextentn"
    #fvarnames <- "siextents"
    #fvarnames <- "omldamax" # mld by mix scheme
    #fvarnames <- "mlotst" # mld by sigma_theta
    #fvarnames <- "uo"
    #fvarnames <- "vo"
    #fvarnames <- "hvel"
    #sellevels <- 6
    #sellevels <- "6,17,27,37,47,57,68.5,82.5,100,122.5,150,182.5"
    #lev_fnames <- "6-182.5"
    if (any(fvarnames == c("uo", "vo"))) mpiom1_remap <- F # vector variables need both components for interp, not one
    mpiom1_remap2lonlat_arg_list <- list(list(#mpiom_grid_files=list(s="/pool/data/MPIOM/TP04/TP04s.nc"),
                                              reg_res=c(nlon=1440, nlat=720),
                                              fout_rename_pattern=prefixes[1]))
    #cdoshifttimes <- "-dt"
    #froms <- 1842 # awi-cm/esm-1-1-lr piControl correct filestamp
    #froms <- 1850
    #froms <- 1855 # awi-cm/esm-1-1-lr piControl wrong filestamp
    #froms <- 1870
    #froms <- 1873
    #froms <- 1910 # TCR average start
    #froms <- 1912 # awi-*-1-1-lr piControl: last 30 years: 1912:1941; last 100 years: 1842:1941 
    #froms <- 1942 # piControl-1942 = deck-1850 (correct filestamp)
    froms <- 1955 # piControl-1955 = deck-1850 (wrong filestamp)
    #froms <- 2401 # awi-cm-1-1-lr piControl-2401 = deck-
    #froms <- 2650
    #froms <- 3106 # awi-esm-1-1-lr midHolocene on esgf
    #froms <- "3901"
    #tos <- 1850
    #tos <- 1851
    #tos <- 1853
    #tos <- 1859
    #tos <- 1869 # 1percCO2: 1850-1869.nc
    #tos <- 1872 # 4CO2: 1850-1872.nc
    #tos <- 1912
    #tos <- 1930 # TCR average end
    #tos <- 1941 # awi-cm/esm-1-1-lr piControl correct filestamp
    #tos <- 1954 # awi-cm/esm-1-1-lr piControl wrong filestamp
    #tos <- 1965
    #tos <- 1999
    #tos <- 2000
    #tos <- 2014
    #tos <- 2099
    #tos <- 2091 # awi-cm/esm-1-1-lr piControl 2000 correct filestamp
    #tos <- 2099
    #tos <- 2104 # awi-cm/esm-1-1-lr piControl-2000 = deck_1999
    tos <- 2119
    #tos <- 2799
    #tos <- 2900 # awi-cm-1-1-lr piControl-2900 = deck-
    #tos <- 3115
    #tos <- 3205 # awi-esm-1-1-lr midHolocene on esgf
    #tos <- "3912"
    #season_inds <- list(c(12, 1, 2)) # DJF
    #season_inds <- list(c(2:4)) # FMA
    #season_inds <- list(c(9:11)) # SON
    #modes <- "select"
    #modes <- "timmean"
    #modes <- "monmean"
    #modes <- "monmax"
    #modes <- "yseasmean" 
    #modes <- "fldmean"
    modes <- "fldint"
    #modes <- list("timmean_monmax"=c("timmean", "monmax"))
    #modes <- "vertmean"
    #modes <- list("vertmean_timmean"=c("vertmean", "timmean"))
    # awi-esm-1-1-lr piControl monthly (1855-1954) -> (1842-1941)
    #new_date_list <- list(list(years=rep(1842:1941, e=12), nc_time_origin=1)) 
    # awi-esm-1-1-lr piControl monthly (1955-2104) -> (1942-2091)
    #new_date_list <- list(list(years=rep(1942:2091, e=12), nc_time_origin=1)) 

} else if (F) { # cold MH issue; hu/xiaoxu
    models <- "echam6"
    #models <- "fesom"
    #datapaths <- "/home/ollie/hyang/work/pi477/cpl_output/copy" # 2700 to 3249
    #fpatterns <- "MM_<year>01.01_echam.nc"
    #datapaths <- "/home/ollie/hyang/work/mh477/cpl_output/copy" # 2623 to 2657
    #fpatterns <- "MM_<year>01.01_echam.nc"
    #datapaths <- "/pf/a/a270064/work/esm-experiments/mh_new/outdata/echam" # 2624 to 3001
    #fpatterns <- "MMnew_echam6_echam_<year>01.nc"
    #datapaths <- "/pf/a/a270064/work/esm-experiments/mh_cold/outdata/echam" # 3105 to 3166 
    #fpatterns <- "mh_cold_echam6_BOT_mm_<year><mon>.nc" # 3105 to 3264 but strange and 3124 is missing
    #fpatterns <- "mh_cold_echam6_echammon_<year><mon>.grb" # 3123 to 3266
    #datapaths <- "/work/ab0246/a270073/out/awi-esm-1-1-lr/mh_cold"
    #fpatterns <- "mh_cold_echam6_BOT_mm_temp2_<year_from>-<year_to>_with_time.nc"
    #fpatterns <- "mh_cold_echam6_BOT_mm_temp2_3105-3207.nc"
    #fpatterns <- "mh_cold_echam6_BOT_mm_temp2_3105-3207_with_time.nc"
    #datapaths <- "/pf/a/a270064/work/esm-experiments/mh_cold/outdata/fesom" 
    #fpatterns <- "mh_cold_fesom_tosga_<year>0101.nc"
    #datapaths <- "/pf/a/a270064/work/esm-experiments/mh_cmip/outdata/echam" # mh branched from piControl: 1955-2105
    #fpatterns <- "mh_cmip_echam6_echammon_<year><mon>.grb"
    datapaths <- "/work/ab0246/a270077/model_acid_tests/mat_0013/outdata/echam" # 1855-1968
    fpatterns <- "mat_0013_echam6_echammon_<year><mon>.grb"
    #prefixes <- "awi-esm-1-1-lr_pi477_ollie"
    #prefixes <- "awi-esm-1-1-lr_lgm"
    #prefixes <- "awi-esm-1-1-lr_mh477_ollie"
    #prefixes <- "awi-esm-1-1-lr_mh_new_mistral"
    #prefixes <- "awi-esm-1-1-lr_mh_cold_mistral"
    #prefixes <- "awi-esm-1-1-lr_mh_cmip"
    prefixes <- "awi-esm-1-1-lr_mat_0013"
    fvarnames <- "temp2"
    codes <- 167
    #fvarnames <- "trad0"
    #fvarnames <- "tosga"
    froms <- 1855
    #froms <- 1955
    #froms <- 2624
    #froms <- 2700 
    #froms <- 3105
    #froms <- 3123
    #froms <- 3537 # last 30 years start from 3843
    tos <- 1968
    #tos <- 2105
    #tos <- 2657
    #tos <- 3001
    #tos <- 3207
    #tos <- 3249
    #tos <- 3266
    #tos <- 3872
    #season_inds <- list(c(12, 1, 2)) # DJF
    #season_inds <- list(c(3, 4, 5)) # MAM
    #season_inds <- list(c(6, 7, 8)) # JJA
    #season_inds <- list(c(9, 10, 11)) # SON
    #modes <- "select"
    #modes <- "timmean" 
    modes <- "fldmean"
    if (prefixes == "awi-esm-1-1-lr_mh_cold_mistral" && 
        grepl("mh_cold_echam6_BOT_mm_", fpatterns) &&
        froms == 3105 && 3124 %in% froms:tos) {
        message("special: make new time with year 3124 missing") 
        new_date_list <- list(list(years=rep(froms:tos, e=12), nc_time_origin=1))
        new_date_list[[1]]$years <- new_date_list[[1]]$years[-(229:240)] # if starting from 3105
    }

} else if (F) { # echam restart issue Jan tests
    models <- "echam6"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1a/outdata/echam"
    #fpatterns <- "Jan1a_echam6_echamday_<year><mon>.grb"
    #prefixes <- "awi-esm-1-1-lr_Jan1a_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1b/work"
    #fpatterns <- "Jan1b_<year><mon>.01_echamday"
    #prefixes <- "awi-esm-1-1-lr_Jan1b_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1c/work"
    #fpatterns <- "Jan1c_<year><mon>.01_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1c/outdata/echam"
    #fpatterns <- "Jan1c_echam6_echamday_<year><mon>.grb"
    #prefixes <- "awi-esm-1-1-lr_Jan1c_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1d/work"
    #fpatterns <- "Jan1d_<year><mon>.01_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1d/outdata/echam"
    #fpatterns <- "Jan1d_echam6_echamday_<year><mon>.grb"
    #prefixes <- "awi-esm-1-1-lr_Jan1d_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1e_timelimit/work" # 1st try
    #fpatterns <- "Jan1e_<year>01.01_g3bid.nc"
    #prefixes <- "awi-esm-1-1-lr_Jan1e_g3bid"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1e/work" # 2nd try
    #fpatterns <- "Jan1e_<year>01.01_g3bid.nc"
    #prefixes <- "awi-esm-1-1-lr_Jan1e2_g3bid"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1f/outdata/echam"
    #fpatterns <- "Jan1f_echam6_g3bid_<year>01.nc"
    #prefixes <- "awi-esm-1-1-lr_Jan1f_g3bid"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1g/outdata/echam"
    #fpatterns <- "Jan1g_echam6_g3bid_<year>01.nc"
    #prefixes <- "awi-esm-1-1-lr_Jan1g_g3bid"
    datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1h/outdata/echam"
    fpatterns <- "Jan1h_echam6_g3bid_<year>01.nc"
    prefixes <- "awi-esm-1-1-lr_Jan1h_g3bid"
    fvarnames <- "temp2"
    #codes <- 167
    froms <- 1850
    #tos <- 1850
    tos <- 1851
    modes <- "fldmean"

} else if (F) { # echam restart issue
    models <- "echam6"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_false/historical/outdata/echam"
    #fpatterns <- "historical_echam6_echam3hr_<year><mon>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_3hr"
    #fpatterns <- "historical_echam6_echam6hr_<year><mon>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_6hr"
    #fpatterns <- "historical_echam6_echamday_<year><mon>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_day"
    #fpatterns <- "historical_echam6_echammon_<year><mon>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_mon"
    #fpatterns <- "historical_echam6_tdiagmon_<year><mon>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_tdiagmon"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL4/restart/echam"
    #fpatterns <- "restart_PI-CTRL4_echam_<year>1231.nc"
    #prefixes <- "awi-esm-1-1-lr_piControl_restart_echam"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL4/outdata/echam"
    #fpatterns <- "PI-CTRL4_echam6_sp6h_<year>12.grb"
    #prefixes <- "awi-esm-1-1-lr_piControl_sp6h"
    datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/restart/echam"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/tar/restart/echam"
    fpatterns <- "restart_PI-CTRL6_echam_<year>1231.nc"
    prefixes <- "awi-esm-1-1-lr_piControl_restart_echam"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/tar/outdata/echam"
    #fpatterns <- "PI-CTRL6_echam6_echamday_<year>.grb"
    #prefixes <- "awi-esm-1-1-lr_piControl_echamday"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical/restart/echam"
    #fpatterns <- "restart_historical_echam_<year>1231.nc"
    #prefixes <- "awi-esm-1-1-lr_historical_restart_echam"
    #codes <- "152"
    #fvarnames <- "lsp"
    #cdo_before_calcs <- "sp2gp"
    #codes <- 130
    #fvarnames <- "st"
    #sellevels <- 47
    #codes <- 167
    #fvarnames <- "temp2"
    #codes <- 129
    fvarnames <- "geosp"
    #froms <- 1850
    #froms <- 1855
    froms <- 1954
    #froms <- 2990
    #tos <- 1851
    #tos <- 1859
    tos <- 1954
    #tos <- 2014
    #tos <- 2999
    #modes <- "select"
    modes <- "fldmean"

} else if (F) { # sara
    models <- "echam6"
    #datapaths <- "/work/aa0238/a270118/Experiments/test_Chris/outdata/echam"
    #fpatterns <- "test_Chris_echam6_echam_<year><mon><DD>.grb"
    #fpatterns <- "test_Chris_echam6_g3b6h_<year><mon><DD>.grb"
    #datapaths <- "/work/aa0238/a270118/Experiments/100y_HICE_LAMO/outdata/echam"
    #fpatterns <- "100y_HICE_LAMO_echam6_g3b6h_<year><mon>.grb"
    datapaths <- "/work/aa0238/a270118/Experiments/100y_HICE_LAMO_old"
    #prefixes <- "test_Chris_echam"
    #prefixes <- "test_Chris_g3b6h"
    prefixes <- "HICE_LAMP_g3b6h"
    fvarnames <- "temp2"
    codes <- 167
    #froms <- 1850
    froms <- 1950
    #tos <- 1850
    tos <- 1955
    modes <- "fldmean"

} else if (F) { # christian
    models <- "echam6"
    datapaths <- "/work/ba1066/a270061/esm_experiments_v4/PI_ctrl_4xCO2_awiesm-2.1_LR/outdata/echam"
    fpatterns <- "PI_ctrl_4xCO2_awiesm-2.1_LR_<year><mon>.01_g3bday"
    prefixes <- "awi-esm-2-1-lr_PI_ctrl_4xCO2_g3bday"
    #fvarnames <- "tslm1"
    #codes <- 139
    fvarnames <- "tsi"
    codes <- 102
    froms <- 2000
    #tos <- 2002
    tos <- 2009
    #tos <- 2890
    modes <- "fldmean"

} else if (F) { # tido
    models <- "echam6"
    datapaths <- "/work/bk0988/awicm/a270062/AWICM/hist1/outdata/echam"
    fpatterns <- "echam6_g3bid_<year>01.nc"
    #fpatterns <- "echam6_g3bday_<year>01.nc"
    prefixes <- "awi-cm-1-1-mr_historical_g3bday"
    fvarnames <- "temp2"
    #fvarnames <- "tsi"
    froms <- 1851
    tos <- 1851
    modes <- "fldmean"

} else if (F) { # lars
    # PI_LA04_cont02: 1016:1054 --> DECK branched from year 1045
    # PI_LA04_cont03: 1055:1363 (ignore the 1364:1367 files)
    #   -> 105501:114912 monthly files
    #   ->   1150:1251   annual files
    #   -> 125201:136312 monthly files
    # PI_LA04_cont04: 1364:1517
    # -> piControl 1850:1858 = PI_LA04_cont02 1046:1054
    # ->           1859:1953 = PI_LA04_cont03 1055:1149 (monthly files)
    # ->           1954:1999 = PI_LA04_cont03 1150:1195 (annual files)
    # ->           2000:2055 = PI_LA04_cont03 1196:1251 (annual files)
    # ->           2056:2099 = PI_LA04_cont03 1252:1295 (monthly files)
    models <- "echam6"
    #datapaths <- "/work/ba0989/a270124/CMIP6_PMIP4/PI_LA04_cont02/outdata/echam"
    #datapaths <- "/work/ba0989/a270124/CMIP6_PMIP4/PI_LA04_cont03/outdata/echam"
    #datapaths <- "/work/ba0989/a270124/CMIP6_PMIP4/PI_LA04_cont04/outdata/echam"
    datapaths <- "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_HIST/outdata/echam"
    #datapaths <- "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_4CO2/outdata/echam"
    #datapaths <- "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_1percCO2/outdata/echam"
    #fpatterns <- "PI_LA04_cont02_echam6_echam_<year><mon>.grb"
    #fpatterns <- "PI_LA04_cont02_echam6_echammon_<year><mon>.grb"
    #fpatterns <- "PI_LA04_cont03_echam6_echam_<year><mon>.grb"
    #fpatterns <- "PI_LA04_cont03_echam6_echam_<year>.grb"
    #fpatterns <- "PI_LA04_cont03_echam6_echammon_<year><mon>.grb"
    #fpatterns <- "PI_LA04_cont03_echam6_echammon_<year>.grb"
    #fpatterns <- "PI_LA04_cont04_echam6_echammon_<year><mon>.grb"
    #fpatterns <- "CMIP6_HIST_echam6_echamday_<year><mon>.grb"
    fpatterns <- "CMIP6_HIST_echam6_echam_<year><mon>.grb"
    #fpatterns <- "CMIP6_HIST_echam6_echammon_<year><mon>.grb"
    #fpatterns <- "CMIP6_4CO2_echam6_echam_<year><mon>.grb"
    #fpatterns <- "CMIP6_1percCO2_echam6_echam_<year><mon>.grb"
    #prefixes <- "awi-esm-1-2-lr_piControl"
    prefixes <- "awi-esm-1-2-lr_historical"
    #prefixes <- "awi-esm-1-2-lr_historical_day"
    #prefixes <- "awi-esm-1-2-lr_4CO2"
    #prefixes <- "awi-esm-1-2-lr_1percCO2"
    #codes <- 167
    #fvarnames <- "temp2"
    #fvarnames <- "tsi"
    #codes <- 178
    #fvarnames <- "srad0"
    #codes <- 179
    #fvarnames <- "trad0"
    fvarnames <- "toa_imbalance"
    #froms <- 1016
    #froms <- 1046
    #froms <- 1055
    #froms <- 1150
    #froms <- 1252
    #froms <- 1364
    froms <- 1850
    #tos <- 1054
    #tos <- 1149
    #tos <- 1195
    #tos <- 1251
    #tos <- 1363
    #tos <- 1517
    #tos <- 1999
    tos <- 2014
    modes <- "fldmean"

} else if (F) { # Hol-Tx10 on paleosrv, Hol-T on stan, Hol-7 on stan
    models <- "echam5"
    #models <- "mpiom1"
    #models <- "jsbach"
    # hol-7 on stan:
    #datapaths <- "/ace/user/pgierz/cosmos-aso-wiso/Hol-7/outdata/echam5"
    #datapaths <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-7/outdata/mpiom"
    # hol-tx10 on paleosrv:
    #datapaths <- "/scratch/simulation_database/incoming/Hol-Tx10/output" # original w 2901-3601 dy=1 timestamps
    #datapaths <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/echam5" # links with dt=10 yrs timestamps
    #datapaths <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/mpiom" # links with dt=10 yrs timestamps
    # hol-t on stan:
    datapaths <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/echam5" # links w correct timestamps
    #datapaths <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom" # links w correct timestamps
    #datapaths <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/jsbach" # links w correct timestamps
    #datapaths <- "/ace/user/stschuet/Hol-T_echam5_wiso_links"
    #fpatterns <- "Hol-Tx10_echam5_main_mm_<year><mon>.nc"
    #fpatterns <- "Hol-T_echam5_main_mm_<year><mon>.nc"
    #fpatterns <- "Hol-7_echam5_wiso_mm_<year><mon>.nc"
    #fpatterns <- "Hol-Tx10_echam5_wiso_mm_<year><mon>.nc"
    fpatterns <- "Hol-T_echam5_wiso_mm_<year><mon>.nc"
    #fpatterns <- "Hol-T_echam5_wiso_link_<year><mon>" # steffens links
    #fpatterns <- "TIMESER.<year>0101_<year>1231.ext.nc"
    #fpatterns <- "fort.75_fort_<year>0101_<year>1231.nc" # daily
    #fpatterns <- "fort.75_fort_<year>0101_<year>1231_monmean.nc" # monthly
    #fpatterns <- "fort.75.<year>0101_<year>1231_monmean" # monthly
    #fpatterns <- "Hol-Tx10_mpiom_<year>0101_<year>1231.grb"
    #fpatterns <- "Hol-T_mpiom_<year>0101_<year>1231.grb"
    #fpatterns <- "Hol-T_mpiom_<year>0101_<year>1231_select_code_2_remapcon2_r120x101.nc" # THO
    #fpatterns <- "Hol-T_mpiom_<year>0101_<year>1231_select_code_5_remapcon2_r120x101.nc" # SAO
    #fpatterns <- "Hol-7_mpiom_<year>0101_<year>1231_select_code_183_remapcon2_r120x101.nc" # zmld
    #fpatterns <- "Hol-Tx10_mpiom_<year>0101_<year>1231_select_code_183_remapcon2_r120x101.nc"
    #fpatterns <- "Hol-T_mpiom_<year>0101_<year>1231_select_code_183_remapcon2_r120x101.nc"
    #fpatterns <- "Hol-7_mpiom_<year>0101_<year>1231_select_code_15_remapcon2_r120x101.nc" # sicmom
    #fpatterns <- "Hol-Tx10_mpiom_<year>0101_<year>1231_select_code_15_remapcon2_r120x101.nc"
    #fpatterns <- "Hol-T_mpiom_<year>0101_<year>1231_select_code_15_remapcon2_r120x101.nc"
    #fpatterns <- "Hol-T_jsbach_veg_mm_<year><mon>.grb" 
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_main_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_main_mm_plev"
    prefixes <- "cosmos-aso-wiso_Hol-T_main_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-T_main_mm_plev"
    #prefixes <- "cosmos-aso-wiso_Hol-7_wiso_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_wiso_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-T_wiso_mm"
    #prefixes <- "Hol-T_echam5_wiso" # steffens files
    #prefixes <- "cosmos-aso-wiso_Hol-7_timeser_ext"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_timeser_ext"
    #prefixes <- "cosmos-aso-wiso_Hol-T_timeser_ext"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_fort_75"
    #prefixes <- "cosmos-aso-wiso_Hol-7_fort_75_monmean"
    #prefixes <- "cosmos-aso-wiso_Hol-T_fort_75_monmean"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_grb"
    #prefixes <- "cosmos-aso-wiso_Hol-T_grb"
    #prefixes <- "cosmos-aso-wiso_Hol-T_grb_code_2_remapcon2_r120x101" # THO
    #prefixes <- "cosmos-aso-wiso_Hol-T_grb_code_5_remapcon2_r120x101" # SAO
    #prefixes <- "cosmos-aso-wiso_Hol-7_grb_code_183_remapcon2_r120x101" # zmld
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_grb_code_183_remapcon2_r120x101"
    #prefixes <- "cosmos-aso-wiso_Hol-T_grb_code_183_remapcon2_r120x101"
    #prefixes <- "cosmos-aso-wiso_Hol-7_grb_code_15_remapcon2_r120x101" # sicomo
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_grb_code_15_remapcon2_r120x101"
    #prefixes <- "cosmos-aso-wiso_Hol-T_grb_code_15_remapcon2_r120x101"
    #prefixes <- "cosmos-aso-wiso_Hol-T_veg_mm"
    #fvarnames <- "temp2"
    #fvarnames <- "tsurf"
    #fvarnames <- "tslm1"
    #fvarnames <- "u10"
    #fvarnames <- "v10"
    #fvarnames <- "wind10"
    #fvarnames <- "srad0"
    #fvarnames <- "srad0d"
    #fvarnames <- "trad0"
    #fvarnames <- "aprt"
    #fvarnames <- "aprl"
    #fvarnames <- "aprc"
    #fvarnames <- "aprs"
    #fvarnames <- "evap"
    #fvarnames <- "pe"
    #fvarnames <- "ws"
    #fvarnames <- "albedo"
    #fvarnames <- "t"
    #fvarnames <- "st" # spectral temperature
    #fvarnames <- "sd" # spectral divergence
    #fvarnames <- "svo" # spectral vorticity
    #fvarnames <- "geosp"
    #fvarnames <- "q"
    #fvarnames <- "aps"
    fvarnames <- "sealevelpressure"
    #fvarnames <- "lm_aps_as_time"
    #fvarnames <- "quv_direction"
    #lev_fnames <- "int1000-100hPa"
    #fvarnames <- "wisoaprt"
    #fvarnames <- "wisoaprt_d"
    #fvarnames <- "wisoevap"
    #fvarnames <- "wisope"
    #fvarnames <- "wisoaprt_d_post"
    #fvarnames <- "wisoevap_d_post"
    #fvarnames <- "wisope_d_post"
    #sellevels <- "2"
    #fvarnames <- "aprt_times_temp2"
    #fvarnames <- "aprt_times_tsurf"
    #fvarnames <- "temp2aprt"
    #fvarnames <- "tsurfaprt"
    #fvarnames <- "ptemp"
    #fvarnames <- "ptsurf"
    #fvarnames <- "c1_PSIGULF" # Maximum_of_Barotropic_Streamfunction_in_Subtropical_Atlantic [m3 s-1]
    #fvarnames <- "c6_PSISPG" # Maximum_of_Barotropic_Streamfunction_in_Subpolar_Atlantic [m3 s-1]
    #fvarnames <- "c208_SST_GLO" # Sea_Surface_Temperature_Global [deg C]
    #fvarnames <- "c209_SSS_GLO" # Sea_Surface_Salinity_Global [psu]
    #fvarnames <- "c210_T200_GLO" # Potential_Temperature_200m_Global [deg C]
    #fvarnames <- "c211_S200_GLO" # Salinity_200m_Global [psu]
    #fvarnames <- "c212_T700_GLO" # Potential_Temperature_700m_Global [deg C]
    #fvarnames <- "c213_S700_GLO" # Salinity_700m_Global [psu]
    #fvarnames <- "c214_T2200_GLO" # Potential_Temperature_2200m_Global [deg C]
    #fvarnames <- "c215_S2200_GLO" # Salinity_2200m_Global [psu]
    #fvarnames <- "c204_ICEARE_GLO" # Seaice_Area_Global [m2]
    #fvarnames <- "c205_ICEVOL_GLO" # Seaice_Volume_Global [m3]
    #fvarnames <- "c64_ICEARE_ARC" # Seaice_Area_Arctic_Ocean [m2]
    #fvarnames <- "c65_ICEVOL_ARC" # Seaice_Volume_Arctic_Ocean [m3]
    #fvarnames <- "c128_SST_ATL" # Sea_Surface_Temperature_Atlantic_Ocean [deg C]
    #fvarnames <- "c129_SSS_ATL" # Sea_Surface_Salinity_Atlantic_Ocean [psu]
    #fvarnames <- "c130_T200_ATL" # Potential_Temperature_200m_Atlantic_Ocean [deg C]
    #fvarnames <- "c131_S200_ATL" # Salinity_200m_Atlantic_Ocean [psu]
    #fvarnames <- "c132_T700_ATL" # Potential_Temperature_700m_Atlantic_Ocean [deg C]
    #fvarnames <- "c133_S700_ATL" # Salinity_700m_Atlantic_Ocean [psu]
    #fvarnames <- "c134_T2200_ATL" # Potential_Temperature_2200m_Atlantic_Ocean [deg C]
    #fvarnames <- "c135_S2200_ATL" # Salinity_2200m_Atlantic_Ocean [psu]
    #fvarnames <- "c44_ICEARE_GIN" # Seaice_Area_GIN_Sea [m2]
    #fvarnames <- "c45_ICEVOL_GIN" # Seaice_Volume_GIN_Sea [m3] 
    #fvarnames <- "c46_HFL_GIN" # Downward_Heatflux_into_GIN_Sea [W]
    #fvarnames <- "c47_WFL_GIN" # Downward_Waterflux_into_GIN_Sea [m3 s-1]
    #fvarnames <- "cSST_GIN"
    #fvarnames <- "c50_T200_GIN"
    #fvarnames <- "c52_T700_GIN"
    #fvarnames <- "c54_T2200_GIN"
    #fvarnames <- "c49_SSS_GIN"
    #fvarnames <- "c51_S200_GIN"
    #fvarnames <- "c53_S700_GIN"
    #fvarnames <- "c55_S2200_GIN"
    #fvarnames <- "c86_HFL_LAB" # Downward_Heatflux_into_Labrador_Sea [W]
    #fvarnames <- "c87_WFL_LAB" # Downward_Waterflux_into_Labrador_Sea [m3 s-1]
    #fvarnames <- "c88_SST_LAB" # Sea_Surface_Temperature_Labrador_Sea [deg C]
    #fvarnames <- "c90_T200_LAB" # Potential_Temperature_200m_Labrador_Sea [deg C]
    #fvarnames <- "c92_T700_LAB" # Potential_Temperature_700m_Labrador_Sea [deg C]
    #fvarnames <- "c94_T2200_LAB" # Potential_Temperature_2200m_Labrador_Sea [deg C]
    #fvarnames <- "c89_SSS_LAB" # Sea_Surface_Salinity_Labrador_Sea [psu]
    #fvarnames <- "c91_S200_LAB" # Salinity_200m_Labrador_Sea [psu]
    #fvarnames <- "c93_S700_LAB" # Salinity_700m_Labrador_Sea [psu]
    #fvarnames <- "c95_S2200_LAB" # Salinity_2200m_Labrador_Sea [psu]
    #fvarnames <- "c84_ICEARE_LAB" # Seaice_Area_Labrador_Sea [m2]
    #fvarnames <- "c85_ICEVOL_LAB" # Seaice_Volume_Labrador_Sea [m3]
    #fvarnames <- "c144_ICEARE_SO" # Seaice_Area_Southern_Ocean [m2]
    #fvarnames <- "c145_ICEVOL_SO" # Seaice_Volume_Southern_Ocean [m3]
    #fvarnames <- "THO"
    #codes <- 2
    #fvarnames <- "SAO"
    #sellevels <- "6"
    #fvarnames <- "zmld"
    #fvarnames <- "SICOMO"
    #codes <- 15
    #areas_out_list <- list(list(name="NA45to90N",
    #                            sellonlatbox=c(lon1=250,lon2=45,lat1=45,lat2=90)))
    #areas_out_list <- list(list(name="weddelmld",
    #                            sellonlatbox=c(lon1=291,lon2=18,lat1=-81,lat2=-45)))
    #areas_out_list <- list(list(name="GINmld",
    #                            sellonlatbox=c(lon1=343,lon2=14,lat1=57.6,lat2=79)))
    #areas_out_list <- list(list(name="LSeaSouthmld",
    #                            sellonlatbox=c(lon1=306,lon2=335,lat1=43,lat2=62)))
    #fvarnames <- "amoc"
    #codes <- 101
    mpiom_moc_make_bottom_topo_arg_list <- list(list(mpiom_model_res=c(setup="GR30", nlev="L40"), 
                                                     reg_res=c(nlon=360, nlat=180)))
    mpiom_moc_extract_ts_arg_list <- list(list(sellevidx=list(c(from=15, to=31), # 1
                                                              c(from=15, to=31), # 2
                                                              c(from=1, to=40), # 3
                                                              c(from=1, to=40), # 4
                                                              c(from=1, to=40), # 5
                                                              c(from=15, to=31), # 6
                                                              c(from=1, to=40), # 7
                                                              c(from=15, to=31) # 8
                                                              ),
                                               sellonlatbox=list(c(lon1=0, lon2=0, lat1=45, lat2=60), # 1
                                                                 c(lon1=0, lon2=0, lat1=30, lat2=60), # 2
                                                                 c(lon1=0, lon2=0, lat1=45, lat2=60), # 3
                                                                 c(lon1=0, lon2=0, lat1=30, lat2=60), # 4
                                                                 c(lon1=0, lon2=0, lat1=26.5, lat2=26.5), # 5
                                                                 c(lon1=0, lon2=0, lat1=26.5, lat2=26.5), # 6
                                                                 c(lon1=0, lon2=0, lat1=50, lat2=50), # 7
                                                                 c(lon1=0, lon2=0, lat1=50, lat2=50) # 8
                                                                 )
                                              ) # setting 1
                                          )
    #fvarnames <- "act_fpc"
    #codes <- 31
    modes <- "select"
    #modes <- "timmean"
    #modes <- "yearmean"
    #modes <- "monmean"
    #modes <- "ymonmean"
    #modes <- "fldmean"
    #modes <- "yearsum"
    #modes <- "seassum"
    #modes <- "timsum"
    #modes <- "zonmean"
    #season_names <- "annual"
    #season_names <- "yearsum"
    #season_names <- "seassum"
    #froms <- "0001" # Hol-Tx10 links: beginning counting from 1
    froms <- "0004" # Hol-T links: beginning of chunk 1
    #froms <- "0100"
    #froms <- "0800" # Hol-7 raw: beginning
    #froms <- "0985" # Hol-T links: 6k mean beginning
    #froms <- "2791" # Hol-7 raw: beginning of most files
    #froms <- "2800" # Hol-7 raw: beginning of main_mm/wiso_mm
    #froms <- "2901" # Hol-Tx10 raw: beginning
    #froms <- "3572"
    #froms <- "6971" # Hol-T links: pi mean beginning
    #tos <- "0011"
    #tos <- "0013" 
    #tos <- "0033"
    #tos <- "0129"
    #tos <- "0809"
    #tos <- "1014" # Hol-T links: 6k mean end
    #tos <- "2900" # Hol-7: end
    #tos <- "2910"
    #tos <- "3601" # Hol-Tx10 raw: end
    #tos <- "5903" # Hol-T links: end of chunk 2
    #tos <- "6821"
    tos <- "7000" # Hol-T links: end of chunk 3
    #tos <- "7001" # Hol-Tx10 links: end counting from 1 
    if (grepl("Hol-Tx10_", fpatterns[1])) {
        if (modes[1] == "timmean") {
            if (froms[1] == "2901" && tos[1] == "3601") {
                new_date_list <- list(list(years=mean(c(1, 7001)), nc_time_origin=1))
            } else {
                stop("asd")
            }
        } else if (modes[1] != "timmean") {
            if (F) { # for links with correct years in filenames: 
                new_date_list <- list(list(use="filename", year_origin=1, nc_time_origin=1))
            } else if (T) { # for files with wrong years in filenames:
                # monthly data:
                new_date_list <- list(list(years=rep(seq(1, b=10, l=length(froms[1]:tos[1])), e=12), 
                                           nc_time_origin=1))
                if (grepl("_main_mm", fpatterns[1])) {
                    # 1 missing Hol-Tx10 *_main_mm_* file: 334812 (Dec 2530 BP; model year 448)
                    if (any(new_date_list[[1]]$years == 4471)) {
                        message("remove Dec of 4471")
                        new_date_list[[1]]$years <- new_date_list[[1]]$years[-(447*12+12)]
                    }
                }
                if (grepl("_wiso_mm", fpatterns[1])) {
                    # 2 missing Hol-Tx10 *_wiso_mm_* files: 334811 and 334812 (Nov+Dec 2530 BP; model year 448)
                    if (any(new_date_list[[1]]$years == 4471)) {
                        message("remove Nov+Dec of 4471")
                        new_date_list[[1]]$years <- new_date_list[[1]]$years[-c(447*12+11, 447*12+12)]
                    }
                }
                if (grepl(".grb", fpatterns[1]) || grepl("grb", prefixes[1])) {
                    # 11 missing Hol-Tx10 mpiom *.grb * files: 3028, 3065, 3153, 3162, 3165, 3316, 3331, 3334, 3348, 3368, 3498
                    missy <- c(3028, 3065, 3153, 3162, 3165, 3316, 3331, 3334, 3348, 3368, 3498)
                    message("remove missing years ", paste(missy, collapse=", "))
                    rminds <- c()
                    for (y in missy) {
                        tmp <- which(new_date_list[[1]]$years == (length(2901:y)-1)*10+1)
                        if (length(tmp) != 0) {
                            message((length(2901:y)-1)*10+1, ": ", paste(tmp, collapse=","))
                            rminds <- c(rminds, tmp)
                        }
                   }
                    new_date_list[[1]]$years <- new_date_list[[1]]$years[-rminds]
                }
            }
        } # new Hol-Tx10 time depending on output frequency
    } else if (grepl("Hol-T_", prefixes[1])) { # for links with correct time stamp
        new_date_list <- list(list(use="filename", year_origin=1, nc_time_origin=1))
    } # new Hol-T time
    # new time depending on setting
    wiso_smow_files <- "~/scripts/r/echam/wiso/SMOW.FAC.T31.nc"
    cdo_codetables <- "~/scripts/r/echam/wiso/CODES.WISO"
    cdo_partablesn <- "~/scripts/r/echam/wiso/CODES.WISO.txt"

} else if (F) { # recT106erai: echam5-wiso, T106L31, ERA-Interim nudging, on stan
    #datapaths <- "/ace/user/mwerner/echam5-wiso/T106L31/EXP007_MB/MONMEAN" # mb
    datapaths <- "/ace/user/mwerner/echam5-wiso/T106L31/EXP020/MONMEAN" # mw
    models <- "echam5"
    #fpatterns <- "EXP007_T106_MB_195801.201312.combined.monmean.wiso.nc"
    fpatterns <- "EXP020_T106_MW_201301.201908.monmean.wiso_d.nc"
    #prefixes <- "echam5_recT106era5_wiso_MB"
    prefixes <- "echam5_recT106erai_wiso_MW"
    fvarnames <- "temp2"
    #fvarnames <- "tsurf"
    modes <- "select"
    #modes <- "fldmean"
    #froms <- 1958 # mb start
    #tos <- 1958
    #tos <- 2012 # mb end
    froms <- 2013 # mw start
    tos <- 2019 # mw end

} else if (F) { # recT127era5: echam6-wiso; T127L95; ERA-5 nudging; ollie
    datapaths <- "/work/ollie/mwerner/echam6-wiso/T127L95/NUDGING_ERA5_T127L95/MONMEAN"
    models <- "echam6"
    fpatterns <- "NUDGING_ERA5_T127L95_echam6_<year>.monmean.wiso.nc"
    prefixes <- "echam6_recT127era5_wiso"
    #fvarnames <- "temp2"
    #fvarnames <- "tsurf"
    fvarnames <- "aprt"
    modes <- "select"
    #modes <- "fldmean"
    froms <- 1979
    tos <- 2019

} else if (F) { # E280_280ppm
    datapaths <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/E280_280ppm/outdata/mpiom"
    models <- "mpiom1"
    fpatterns <- "TIMESER.<year>0101_<year>1231.ext.nc"
    prefixes <- "E280_280ppm_mpiom_timeser"
    fvarnames <- "c25_TMERCI3"
    modes <- "select"
    froms <- "2650"
    tos <- "2749"

} else if (F) { # lacroix_etal_2020
    models <- "lacroix_etal_2020"
    datapaths <- paste0(workpath, "/data/reccap2-ocean")
    fpatterns <- "fgco2_lacroix-river_v20220218.nc" # setgrid to original file; needs cdo >= 2
    prefixes <- "lacroix_etal_2020"
    fvarnames <- "fgco2"
    modes <- "fldint"
    froms <- 2022 # reflecting file version
    tos <- 2022
    mask_list <- list(list(# reccap2 basins:
                           #name="reccap2_atlantic",
                           #cdo_mask=paste0("-eqc,1 -select,name=open_ocean ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_pacific",
                           #cdo_mask=paste0("-eqc,2 -select,name=open_ocean ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_indian",
                           #cdo_mask=paste0("-eqc,3 -select,name=open_ocean ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_arctic",
                           #cdo_mask=paste0("-eqc,4 -select,name=open_ocean ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_southern",
                           #cdo_mask=paste0("-eqc,5 -select,name=open_ocean ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           # reccap2 atlantic biomes: 
                           #name="reccap2_na_spss",
                           #cdo_mask=paste0("-eqc,1 -select,name=atlantic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_na_stss",
                           #cdo_mask=paste0("-eqc,2 -select,name=atlantic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_na_stps",
                           #cdo_mask=paste0("-eqc,3 -select,name=atlantic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_aequ",
                           #cdo_mask=paste0("-eqc,4 -select,name=atlantic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_sa_stps",
                           #cdo_mask=paste0("-eqc,5 -select,name=atlantic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_med",
                           #cdo_mask=paste0("-eqc,6 -select,name=atlantic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           # reccap2 pacific biomes: 
                           #name="reccap2_np_spss",
                           #cdo_mask=paste0("-eqc,1 -select,name=pacific ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_np_stss",
                           #cdo_mask=paste0("-eqc,2 -select,name=pacific ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_np_stps",
                           #cdo_mask=paste0("-eqc,3 -select,name=pacific ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_pequ_w",
                           #cdo_mask=paste0("-eqc,4 -select,name=pacific ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_pequ_e",
                           #cdo_mask=paste0("-eqc,5 -select,name=pacific ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_sp_stps",
                           #cdo_mask=paste0("-eqc,6 -select,name=pacific ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           # reccap2 indian biomes:
                           #name="reccap2_ind_stps",
                           #cdo_mask=paste0("-eqc,1 -select,name=indian ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_ind_interior",
                           #cdo_mask=paste0("-eqc,2 -select,name=indian ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           # reccap2 arctic biomes:
                           #name="reccap2_arctic_ice",
                           #cdo_mask=paste0("-eqc,1 -select,name=arctic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_np_ice",
                           #cdo_mask=paste0("-eqc,2 -select,name=arctic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_na_ice",
                           #cdo_mask=paste0("-eqc,3 -select,name=arctic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_barents",
                           #cdo_mask=paste0("-eqc,4 -select,name=arctic ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           # reccap2 southern biomes:
                           #name="reccap2_so_stss",
                           #cdo_mask=paste0("-eqc,1 -select,name=southern ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_so_spss",
                           #cdo_mask=paste0("-eqc,2 -select,name=southern ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           #name="reccap2_so_ice",
                           #cdo_mask=paste0("-eqc,3 -select,name=southern ", workpath, "/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc")))
                           # gregor_etal_2019 basins:
                           #name="gregor_etal_2019_nh-hl",
                           #cdo_mask=paste0("-eqc,1 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_lon_from_0.5.nc")))
                           #name="gregor_etal_2019_nh-st",
                           #cdo_mask=paste0("-eqc,2 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_lon_from_0.5.nc")))
                           #name="gregor_etal_2019_equ",
                           #cdo_mask=paste0("-eqc,3 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_lon_from_0.5.nc")))
                           #name="gregor_etal_2019_sh-st",
                           #cdo_mask=paste0("-eqc,4 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_lon_from_0.5.nc")))
                           name="gregor_etal_2019_sh-hl",
                           cdo_mask=paste0("-eqc,5 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_lon_from_0.5.nc")))

} else if (F) { # chau_etal_2020
    models <- "chau_etal_2020"
    datapaths <- "/work/ba1103/a270073/data/chau_etal_2020/data"
    fpatterns <- "dataset-carbon-rep-<year_from>-<year_to>_T0000Z_P20210930T1545Z.nc"
    prefixes <- "chau_etal_2020"
    fvarnames <- "fgco2"
    #fvarnames <- "fgco2_uncertainty"
    modes <- "fldint"
    froms <- 1985
    tos <- 2020
    mask_list <- list(list(
                           #name="reccap2_atlantic",
                           #cdo_mask=paste0("-eqc,1 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_pacific",
                           #cdo_mask=paste0("-eqc,2 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_indian",
                           #cdo_mask=paste0("-eqc,3 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_arctic",
                           #cdo_mask=paste0("-eqc,4 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_southern",
                           #cdo_mask=paste0("-eqc,5 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           # sub-atlantic: 
                           #name="reccap2_na_spss",
                           #cdo_mask=paste0("-eqc,1 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_na_stss",
                           #cdo_mask=paste0("-eqc,2 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_na_stps",
                           #cdo_mask=paste0("-eqc,3 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_aequ",
                           #cdo_mask=paste0("-eqc,4 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_sa_stps",
                           #cdo_mask=paste0("-eqc,5 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_med",
                           #cdo_mask=paste0("-eqc,6 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           # sub-pacific: 
                           #name="reccap2_np_spss",
                           #cdo_mask=paste0("-eqc,1 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_np_stss",
                           #cdo_mask=paste0("-eqc,2 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_np_stps",
                           #cdo_mask=paste0("-eqc,3 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_pequ_w",
                           #cdo_mask=paste0("-eqc,4 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           #name="reccap2_pequ_e",
                           #cdo_mask=paste0("-eqc,5 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))
                           name="reccap2_sp_stps",
                           cdo_mask=paste0("-eqc,6 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_chau_etal_2020.nc")))

} else if (F) { # gregor_and_fay_2021
    models <- "gregor_and_fay_2021"
    datapaths <- "/work/ba1103/a270073/data/gregor_and_fay_2021/data"
    fpatterns <- "SeaFlux_v2021.04_fgco2_all_winds_products_ensemble_stats.nc"
    #fpatterns <- "SeaFlux_v2021.04_fgco2_all_winds_products_timmean_ensemble_stats.nc"
    prefixes <- "gregor_and_fay_2021"
    #fvarnames <- "fgco2_ens_mean"
    #fvarnames <- "fgco2_ens_median"
    fvarnames <- "fgco2_ens_sd"
    #fvarnames <- "fgco2_ens_min"
    #fvarnames <- "fgco2_ens_max"
    modes <- "fldint"
    froms <- 1990
    tos <- 2019
    mask_list <- list(list(# reccap2
                           #name="reccap2_atlantic",
                           #cdo_mask=paste0("-eqc,1 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_pacific",
                           #cdo_mask=paste0("-eqc,2 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_indian",
                           #cdo_mask=paste0("-eqc,3 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_arctic",
                           #cdo_mask=paste0("-eqc,4 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_southern",
                           #cdo_mask=paste0("-eqc,5 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           # reccap2 atlantic biomes: 
                           #name="reccap2_na_spss",
                           #cdo_mask=paste0("-eqc,1 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_na_stss",
                           #cdo_mask=paste0("-eqc,2 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_na_stps",
                           #cdo_mask=paste0("-eqc,3 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_aequ",
                           #cdo_mask=paste0("-eqc,4 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_sa_stps",
                           #cdo_mask=paste0("-eqc,5 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_med",
                           #cdo_mask=paste0("-eqc,6 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           # reccap2 pacific biomes: 
                           #name="reccap2_np_spss",
                           #cdo_mask=paste0("-eqc,1 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_np_stss",
                           #cdo_mask=paste0("-eqc,2 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_np_stps",
                           #cdo_mask=paste0("-eqc,3 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_pequ_w",
                           #cdo_mask=paste0("-eqc,4 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_pequ_e",
                           #cdo_mask=paste0("-eqc,5 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_sp_stps",
                           #cdo_mask=paste0("-eqc,6 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           # reccap2 indian biomes:
                           #name="reccap2_ind_stps",
                           #cdo_mask=paste0("-eqc,1 -select,name=indian ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_ind_interior",
                           #cdo_mask=paste0("-eqc,2 -select,name=indian ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           # reccap2 arctic biomes:
                           #name="reccap2_arctic_ice",
                           #cdo_mask=paste0("-eqc,1 -select,name=arctic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_np_ice",
                           #cdo_mask=paste0("-eqc,2 -select,name=arctic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_na_ice",
                           #cdo_mask=paste0("-eqc,3 -select,name=arctic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_barents",
                           #cdo_mask=paste0("-eqc,4 -select,name=arctic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           # reccap2 southern biomes:
                           #name="reccap2_so_stss",
                           #cdo_mask=paste0("-eqc,1 -select,name=southern ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_so_spss",
                           #cdo_mask=paste0("-eqc,2 -select,name=southern ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           #name="reccap2_so_ice",
                           #cdo_mask=paste0("-eqc,3 -select,name=southern ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_gregor_and_fay_2021.nc")))
                           # gregor_etal_2019 basins:
                           name="gregor_etal_2019_nh-hl",
                           cdo_mask=paste0("-eqc,1 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_gregor_and_fay_2021.nc")))
                           #name="gregor_etal_2019_nh-st",
                           #cdo_mask=paste0("-eqc,2 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_gregor_and_fay_2021.nc")))
                           #name="gregor_etal_2019_equ",
                           #cdo_mask=paste0("-eqc,3 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_gregor_and_fay_2021.nc")))
                           #name="gregor_etal_2019_sh-st",
                           #cdo_mask=paste0("-eqc,4 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_gregor_and_fay_2021.nc")))
                           #name="gregor_etal_2019_sh-hl",
                           #cdo_mask=paste0("-eqc,5 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_gregor_and_fay_2021.nc")))

} else if (F) { # awi-esm-1-1-lr_kh800 piControl chunks 1 to 3
    models <- "echam6"
    #models <- "jsbach"
    #models <- "fesom"
    # chunk 1: 1950:2029
    #datapaths <- "/work/ab1095/a270094/AWIESM/SR_output/outdata/echam" # chunk 1
    #datapaths <- "/work/ab1095/a270094/AWIESM/SR_output/outdata/jsbach" # chunk 1
    #datapaths <- "/work/ab1095/a270094/AWIESM/SR_output/outdata/fesom" # chunk 1
    #fpatterns <- "SR_output_echam6_echam_<year>01.nc" # chunk 1
    #fpatterns <- "SR_output_echam6_co2_<year>01.nc" # chunk 1
    #fpatterns <- "SR_output_jsbach_jsbach_<year>01.grb"
    #codes_files <- paste0(datapaths, "/SR_output_195001.01_jsbach.codes")
    #fpatterns <- "SR_output_jsbach_veg_<year>01.grb"
    #codes_files <- paste0(datapaths, "/SR_output_195001.01_veg.codes")
    #fpatterns <- "SR_output_jsbach_yasso_<year>01.grb"
    #codes_files <- paste0(datapaths, "/SR_output_195001.01_yasso.codes")
    # chunk 2: 2030:2685
    #datapaths <- "/work/ba1103/a270094/AWIESM/test/outdata/echam" # chunk 2
    #datapaths <- "/work/ba1103/a270094/AWIESM/test/outdata/jsbach" # chunk 2
    #datapaths <- "/work/ba1103/a270094/AWIESM/test/outdata/fesom" # chunk 2
    #fpatterns <- "test_echam6_g3bid_<year>01.nc" # chunk 2
    #fpatterns <- "test_echam6_echam_<year>01.nc" # chunk 2
    #fpatterns <- "test_echam6_co2_<year>01.nc" # chunk 2
    #fpatterns <- "test_jsbach_jsbach_<year>01.grb" # chunk 2
    #codes_files <- paste0(datapaths, "/test_203001.01_jsbach.codes")
    #fpatterns <- "test_jsbach_veg_<year>01.grb" # chunk 2
    #codes_files <- paste0(datapaths, "/test_203001.01_veg.codes")
    #fpatterns <- "test_jsbach_yasso_<year>01.grb"
    #codes_files <- paste0(datapaths, "/test_203001.01_yasso.codes")
    #fpatterns <- "<fvarnames>_fesom_<year>0101.nc" # chunk 1 and 2 
    # chunk 3 from 2686
    datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/echam" # chunk3
    #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/jsbach" # chunk3
    #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom" # chunk3
    #fpatterns <- "piControl_<year><mon>.01_echam"
    fpatterns <- "piControl_<year><mon>.01_co2"
    #fpatterns <- "piControl_<year><mon>.01_jsbach"
    #fpatterns <- "piControl_<year><mon>.01_veg"
    #fpatterns <- "piControl_<year><mon>.01_yasso"
    #fpatterns <- "<fvarnames>_fesom_<year>0101.nc"
    #
    #prefixes <- "awi-esm-1-1-lr_kh800_piControl_3hr"
    #prefixes <- "awi-esm-1-1-lr_kh800_piControl_day"
    prefixes <- "awi-esm-1-1-lr_kh800_piControl"
    # echam echamstream
    #codes <- 167
    #fvarnames <- "temp2"
    # echam co2stream 
    #fvarnames <- "co2_flux"
    codes <- 7
    fvarnames <- "co2_flx_ocean"
    #fvarnames <- "fgco2"
    #codes <- 6
    #fvarnames <- "co2_flx_land" # = npp + resp + herb + fire
    #fvarnames <- "co2_flx_npp"
    #fvarnames <- "co2_flx_resp"
    #fvarnames <- "co2_flx_herb"
    #fvarnames <- "co2_flx_fire"
    #codes <- 24
    #fvarnames <- "co2_flx_lcc"
    #codes <- 25
    #fvarnames <- "co2_flx_harvest"
    #fvarnames <- "nbp" # = co2_flx_land + co2_flx_lcc + co2_flx_harvest
    #fvarnames <- "co2_flx_total" # = fgco2 + nbp
    # jsbach jsbachstream
    #codes <- 12
    #fvarnames <- "cover_fract"
    #codes <- 20
    #fvarnames <- "veg_ratio_max"
    #codes <- 24
    #fvarnames <- "box_veg_ratio"
    #codes <- 160
    #fvarnames <- "CO2_flux_net"
    #codes <- 161
    #fvarnames <- "CO2_flux_herbivory"
    #codes <- 162
    #fvarnames <- "CO2_emission_landcover_change"
    #codes <- 163
    #fvarnames <- "CO2_emission_harvest"
    #codes <- 164
    #fvarnames <- "CO2_flux_dynveg"
    # jsbach vegstream
    #codes <- 160
    #fvarnames <- "boxC_green"
    #codes <- 161
    #fvarnames <- "boxC_woods"
    #codes <- 162
    #fvarnames <- "boxC_reserve"
    #codes <- 218
    #fvarnames <- "box_Cpool_onSite_avg_LCC"
    #codes <- 220
    #fvarnames <- "box_Cpool_paper_avg_LCC"
    #codes <- 221
    #fvarnames <- "box_Cpool_construction_avg_LCC"
    #codes <- 222
    #fvarnames <- "box_Cpool_paper_harvest_avg"
    #codes <- 223
    #fvarnames <- "box_Cpool_construction_harvest_avg"
    #codes <- 224
    #fvarnames <- "box_Cpool_onSite_harvest_avg"
    #codes <- 225
    #fvarnames <- "boxC_crop_harvest"
    # jsbach yassostream
    #codes <- 31
    #fvarnames <- "boxYC_acid_ag1"
    #codes <- 41
    #fvarnames <- "boxYC_acid_ag2"
    #codes <- 33
    #fvarnames <- "boxYC_water_ag1"
    #codes <- 43
    #fvarnames <- "boxYC_water_ag2"
    #codes <- 35
    #fvarnames <- "boxYC_ethanol_ag1"
    #codes <- 45
    #fvarnames <- "boxYC_ethanol_ag2"
    #codes <- 37
    #fvarnames <- "boxYC_nonsoluble_ag1"
    #codes <- 47
    #fvarnames <- "boxYC_nonsoluble_ag2"
    #codes <- 32
    #fvarnames <- "boxYC_acid_bg1"
    #codes <- 42
    #fvarnames <- "boxYC_acid_bg2"
    #codes <- 34
    #fvarnames <- "boxYC_water_bg1"
    #codes <- 44
    #fvarnames <- "boxYC_water_bg2"
    #codes <- 36
    #fvarnames <- "boxYC_ethanol_bg1"
    #codes <- 46
    #fvarnames <- "boxYC_ethanol_bg2"
    #codes <- 38
    #fvarnames <- "boxYC_nonsoluble_bg1"
    #codes <- 48
    #fvarnames <- "boxYC_nonsoluble_bg2"
    #codes <- 39
    #fvarnames <- "boxYC_humus_1"
    #codes <- 49
    #fvarnames <- "boxYC_humus_2"
    #fvarnames <- "soilSlow" 
    # fesom
    #fvarnames <- "tos"
    #fvarnames <- "thetaoga"
    #fvarnames <- "tauuo"
    #fvarnames <- "tauvo"
    #fvarnames <- "mlotst"
    #fvarnames <- "omldamax"
    #fvarnames <- "siextentn"
    #fvarnames <- "sic"
    # recom
    #fvarnames <- "aCO2"
    #cdoshifttimes <- "-dt" # for fesom
    #cdo_before_calcs <- "monmean"
    #modes <- "select"
    #modes <- "timmean"
    #modes <- "ydaymean"
    #modes <- "fldmean"
    modes <- "fldint"
    #froms <- 1950 # start chunk1: 1950
    #froms <- 2030 # start chunk2: 2030
    #froms <- 2586 # chunk 2 last 100 years
    #froms <- 2666 # chunk 2 last 20 years
    froms <- 2686 # start chunk 3: 2686
    #froms <- 2823
    #froms <- 2996
    #froms <- 2997
    #tos <- 1951
    #tos <- 2029 # end chunk 1: 2029 
    #tos <- 2031
    #tos <- 2685 # end chunk 2: 2685
    #tos <- 2687
    #tos <- 2688
    #tos <- 2824
    #tos <- 2825
    #tos <- 2850
    #tos <- 2895
    #tos <- 2905 
    #tos <- 2995
    #tos <- 2996
    tos <- 3000 # end chunk 3: 3000
    #areas_out_list <- list(list(name="SO45S",
    #                            sellonlatbox=c(lon1=0,lon2=360,lat1=-45,lat2=-90)))
    #mask_list <- list(list(name="reccap2_atlantic",
    #                       cdo_mask=paste0("-eqc,1 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_pacific",
    #                       cdo_mask=paste0("-eqc,2 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_indian",
    #                       cdo_mask=paste0("-eqc,3 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_arctic",
    #                       cdo_mask=paste0("-eqc,4 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_southern",
    #                       cdo_mask=paste0("-eqc,5 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    # sub-atlantic: 
    #mask_list <- list(list(name="reccap2_na_spss",
    #                       cdo_mask=paste0("-eqc,1 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_na_stss",
    #                       cdo_mask=paste0("-eqc,2 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_na_stps",
    #                       cdo_mask=paste0("-eqc,3 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_aequ",
    #                       cdo_mask=paste0("-eqc,4 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_sa_stps",
    #                       cdo_mask=paste0("-eqc,5 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_med",
    #                       cdo_mask=paste0("-eqc,6 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    # sub-pacific: 
    #mask_list <- list(list(name="reccap2_np_spss",
    #                       cdo_mask=paste0("-eqc,1 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_np_stss",
    #                       cdo_mask=paste0("-eqc,2 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_np_stps",
    #                       cdo_mask=paste0("-eqc,3 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_pequ_w",
    #                       cdo_mask=paste0("-eqc,4 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    #mask_list <- list(list(name="reccap2_pequ_e",
    #                       cdo_mask=paste0("-eqc,5 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
    mask_list <- list(list(name="reccap2_sp_stps",
                           cdo_mask=paste0("-eqc,6 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))

} else if (F) { # awi-esm-1-1-lr_kh800 piControl LUtrans1850
    models <- "echam6"
    datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl_LUtrans1850/outdata/echam"
    fpatterns <- "piControl_LUtrans1850_<year><mon>.01_co2"
    prefixes <- "awi-esm-1-1-lr_kh800_piControl_LUtrans1850"
    #codes <- 6
    #fvarnames <- "co2_flx_land"
    codes <- 7
    fvarnames <- "co2_flx_ocean"
    #codes <- 24
    #fvarnames <- "co2_flx_lcc"
    #codes <- 25
    #fvarnames <- "co2_flx_harvest"
    #fvarnames <- "nbp" # = co2_flx_land + co2_flx_lcc + co2_flx_harvest
    #fvarnames <- "fgco2"
    #fvarnames <- "co2_flx_total" # = fgco2 + nbp
    modes <- "fldint"
    froms <- 2951
    tos <- 3062
    areas_out_list <- list(list(name="SO45S",
                                sellonlatbox=c(lon1=0,lon2=360,lat1=-45,lat2=-90)))

} else if (F) { # cmip6 esgf ACCESS-ESM1-5
    models <- "ACCESS-ESM1-5" # ocean: mom5: native: tripolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CSIRO/ACCESS-ESM1-5/piControl/r1i1p1f1/Omon/fgco2/gn/v20191214"
    #fpatterns <- "fgco2_Omon_ACCESS-ESM1-5_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CSIRO/ACCESS-ESM1-5/piControl/r1i1p1f1/Lmon/nbp/gn/v20210316"
    fpatterns <- "nbp_Lmon_ACCESS-ESM1-5_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    prefixes <- "ACCESS-ESM1-5_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 901 # last 100a
    #froms <- 1001
    tos <- 1000 # last 100a
    #tos <- 1100

} else if (F) { # cmip6 esgf AWI-ESM-1-1-LR
    models <- "AWI-ESM-1-1-LR"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/Emon/netAtmosLandCO2Flux/gn/v20200212"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/Emon/fLuc/gn/v20200212"
    fpatterns <- "fLuc_Emon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "fLuc"
    prefixes <- "AWI-ESM-1-1-LR_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 1855 # last 100a
    tos <- 1954 # last 100a

} else if (F) { # cmip6 esgf BCC-ESM1
    models <- "BCC-ESM1" # ocean: mom4: native: curvilinear; fldint todo
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/BCC/BCC-ESM1/piControl/r1i1p1f1/Omon/fgco2/gn/v20191121"
    fpatterns <- "fgco2_Omon_BCC-ESM1_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    prefixes <- "BCC-ESM1_piControl_r1i1p1f1"
    fvarnames <- "fgco2"
    modes <- "fldint"
    froms <- 2201 # last 100a
    tos <- 2300 # last 100a
    if (any(modes == c("fldmean", "fldint"))) cdo_before_calcs <- "remapbil,global_1"

} else if (F) { # cmip6 esgf CanESM5;
    models <- "CanESM5" # ocean: nemo3.4.1: native: orca1 tripolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CCCma/CanESM5/piControl/r1i1p1f1/Omon/fgco2/gn/v20190429"
    #fpatterns <- "fgco2_Omon_CanESM5_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CCCma/CanESM5/piControl/r1i1p1f1/Emon/netAtmosLandCO2Flux/gn/v20190429"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_CanESM5_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CCCma/CanESM5/piControl/r1i1p1f1/Lmon/nbp/gn/v20190429"
    fpatterns <- "nbp_Lmon_CanESM5_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    prefixes <- "CanESM5_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 6101 # last 100a
    tos <- 6200 # last 100a

} else if (F) { # cmip6 esgf CanESM5-CanOE;
    models <- "CanESM5-CanOE" # ocean: nemo3.4.1: native: orca1 tripolar; cdo: curvilinear; fldint ok
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CCCma/CanESM5-CanOE/piControl/r1i1p2f1/Lmon/nbp/gn/v20190429"
    fpatterns <- "nbp_Lmon_CanESM5-CanOE_piControl_r1i1p2f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CCCma/CanESM5-CanOE/piControl/r1i1p2f1/Omon/fgco2/gn/v20190429"
    #fpatterns <- "fgco2_Omon_CanESM5-CanOE_piControl_r1i1p2f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    fvarnames <- "co2_flx_total"
    prefixes <- "CanESM5-CanOE_piControl_r1i1p2f1"
    modes <- "fldint"
    froms <- 5901
    #froms <- 5951 # last 100a
    tos <- 6000
    #tos <- 6050 # last 100a

} else if (F) { # cmip6 esgf CESM2 
    models <- "CESM2" # ocean: pop2: native: ?; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2/piControl/r1i1p1f1/Omon/fgco2/gr/v20190320"
    #fpatterns <- "fgco2_Omon_CESM2_piControl_r1i1p1f1_gr_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2/piControl/r1i1p1f1/Lmon/nbp/gn/v20190320"
    #fpatterns <- "nbp_Lmon_CESM2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2/piControl/r1i1p1f1/Emon/fLuc/gn/v20190320"
    #fpatterns <- "fLuc_Emon_CESM2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fLuc"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2/piControl/r1i1p1f1/Lmon/fHarvest/gn/v20190320"
    fpatterns <- "fHarvest_Lmon_CESM2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "fHarvest"
    #fvarnames <- "co2_flx_total"
    prefixes <- "CESM2_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 1101 # last 100a
    tos <- 1200 # last 100a

} else if (F) { # cmip6 esgf CESM2-FV2
    models <- "CESM2-FV2" # ocean: pop2: native: ?; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-FV2/piControl/r1i1p1f1/Emon/netAtmosLandCO2Flux/gn/v20191120"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_CESM2-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-FV2/piControl/r1i1p1f1/Lmon/nbp/gn/v20191120"
    #fpatterns <- "nbp_Lmon_CESM2-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-FV2/piControl/r1i1p1f1/Omon/fgco2/gn/v20191120"
    #fpatterns <- "fgco2_Omon_CESM2-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-FV2/piControl/r1i1p1f1/Emon/fLuc/gn/v20191120"
    #fpatterns <- "fLuc_Emon_CESM2-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fLuc"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-FV2/piControl/r1i1p1f1/Lmon/fHarvest/gn/v20191120"
    fpatterns <- "fHarvest_Lmon_CESM2-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "fHarvest"
    #fvarnames <- "co2_flx_total"
    prefixes <- "CESM2-FV2_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 401 # last 100a
    tos <- 500 # last 100a

} else if (F) { # cmip6 esgf CESM2-WACCM
    models <- "CESM2-WACCM" # ocean: pop2: native: ?; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-WACCM/piControl/r1i1p1f1/Emon/netAtmosLandCO2Flux/gn/v20190320"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_CESM2-WACCM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-WACCM/piControl/r1i1p1f1/Omon/fgco2/gn/v20190320"
    #fpatterns <- "fgco2_Omon_CESM2-WACCM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-WACCM/piControl/r1i1p1f1/Emon/fLuc/gn/v20190320"
    #fpatterns <- "fLuc_Emon_CESM2-WACCM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fLuc"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-WACCM/piControl/r1i1p1f1/Lmon/fHarvest/gn/v20190320"
    fpatterns <- "fHarvest_Lmon_CESM2-WACCM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "fHarvest"
    prefixes <- "CESM2-WACCM_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 400 # last 100a
    tos <- 499 # last 100a

} else if (F) { # cmip6 esgf CESM2-WACCM-FV2
    models <- "CESM2-WACCM-FV2" # ocean: pop2: native: ?; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-WACCM-FV2/piControl/r1i1p1f1/Emon/netAtmosLandCO2Flux/gn/v20191120"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_CESM2-WACCM-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-WACCM-FV2/piControl/r1i1p1f1/Lmon/nbp/gn/v20191120"
    #fpatterns <- "nbp_Lmon_CESM2-WACCM-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-WACCM-FV2/piControl/r1i1p1f1/Omon/fgco2/gn/v20191120"
    #fpatterns <- "fgco2_Omon_CESM2-WACCM-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-WACCM-FV2/piControl/r1i1p1f1/Emon/fLuc/gn/v20191120"
    #fpatterns <- "fLuc_Emon_CESM2-WACCM-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fLuc"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2-WACCM-FV2/piControl/r1i1p1f1/Lmon/fHarvest/gn/v20191120"
    fpatterns <- "fHarvest_Lmon_CESM2-WACCM-FV2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "fHarvest"
    #fvarnames <- "co2_flx_total"
    prefixes <- "CESM2-WACCM-FV2_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 401 # last 100a
    tos <- 500 # last 100a

} else if (F) { # cmip6 esgf CMCC-CM2-SR5
    models <- "CMCC-CM2-SR5"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CMCC/CMCC-CM2-SR5/piControl/r1i1p1f1/Emon/netAtmosLandCO2Flux/gn/v20200616"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_CMCC-CM2-SR5_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CMCC/CMCC-CM2-SR5/piControl/r1i1p1f1/Lmon/nbp/gn/v20200616"
    #fpatterns <- "nbp_Lmon_CMCC-CM2-SR5_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CMCC/CMCC-CM2-SR5/piControl/r1i1p1f1/Emon/fLuc/gn/v20200616"
    fpatterns <- "fLuc_Emon_CMCC-CM2-SR5_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "fLuc"
    prefixes <- "CMCC-CM2-SR5_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 2250 # last 100a
    tos <- 2349 # last 100a

} else if (F) { # cmip6 esgf CMCC-ESM2
    models <- "CMCC-ESM2" # ocean: nemo3.6: native: ORCA1 tripolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CMCC/CMCC-ESM2/piControl/r1i1p1f1/Omon/fgco2/gn/v20210126"
    #fpatterns <- "fgco2_Omon_CMCC-ESM2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CMCC/CMCC-ESM2/piControl/r1i1p1f1/Lmon/nbp/gn/v20210126"
    fpatterns <- "nbp_Lmon_CMCC-ESM2_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    prefixes <- "CMCC-ESM2_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 2000 # last 100a
    tos <- 2099 # last 100a

} else if (F) { # cmip6 esgf CNRM-ESM2-1
    models <- "CNRM-ESM2-1" # ocean: nemo3.6: native: eORCA1 tripolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CNRM-CERFACS/CNRM-ESM2-1/piControl/r1i1p1f2/Omon/fgco2/gn/v20181115"
    #fpatterns <- "fgco2_Omon_CNRM-ESM2-1_piControl_r1i1p1f2_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CNRM-CERFACS/CNRM-ESM2-1/piControl/r1i1p1f2/Emon/netAtmosLandCO2Flux/gr/v20181115"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_CNRM-ESM2-1_piControl_r1i1p1f2_gr_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CNRM-CERFACS/CNRM-ESM2-1/piControl/r1i1p1f2/Lmon/nbp/gr/v20181115"
    #fpatterns <- "nbp_Lmon_CNRM-ESM2-1_piControl_r1i1p1f2_gr_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CNRM-CERFACS/CNRM-ESM2-1/piControl/r1i1p1f2/Emon/fLuc/gr/v20181115"
    fpatterns <- "fLuc_Emon_CNRM-ESM2-1_piControl_r1i1p1f2_gr_<year_from>01-<year_to>12.nc"
    fvarnames <- "fLuc"
    #fvarnames <- "co2_flx_total"
    prefixes <- "CNRM-ESM2-1_piControl_r1i1p1f2"
    modes <- "fldint"
    froms <- 2250 # last 100a
    tos <- 2349 # last 100a

} else if (F) { # cmip6 esgf EC-Earth3-CC
    models <- "EC-Earth3-CC" # ocean: nemo3.6: native: ORCA1 tripolar; cdo: curvilinear; fldint ok
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/EC-Earth-Consortium/EC-Earth3-CC/piControl/r1i1p1f1/Omon/fgco2/gn/v20210416"
    fpatterns <- "fgco2_Omon_EC-Earth3-CC_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    prefixes <- "EC-Earth3-CC_piControl_r1i1p1f1"
    fvarnames <- "fgco2"
    modes <- "fldint"
    froms <- 2255 # last 100a
    tos <- 2354 # last 100a

} else if (F) { # cmip6 esgf EC-Earth3-Veg
    models <- "EC-Earth3-Veg" # ocean: nemo3.6: native: ORCA1 tripolar; cdo: curvilinear; fldint ok
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/EC-Earth-Consortium/EC-Earth3-Veg/piControl/r1i1p1f1/Lmon/nbp/gr/v20200226"
    fpatterns <- "nbp_Lmon_EC-Earth3-Veg_piControl_r1i1p1f1_gr_<year_from>01-<year_to>12.nc"
    fvarnames <- "nbp"
    prefixes <- "EC-Earth3-Veg_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 2250 # last 100a
    tos <- 2349 # last 100a

} else if (F) { # cmip6 esgf EC-Earth3-Veg-LR
    models <- "EC-Earth3-Veg-LR" # ocean: nemo3.6: native: ORCA1 tripolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/EC-Earth-Consortium/EC-Earth3-CC/piControl/r1i1p1f1/Omon/fgco2/gn/v20210416"
    #fpatterns <- "fgco2_Omon_EC-Earth3-CC_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/EC-Earth-Consortium/EC-Earth3-Veg-LR/piControl/r1i1p1f1/Lmon/nbp/gr/v20201113"
    fpatterns <- "nbp_Lmon_EC-Earth3-Veg-LR_piControl_r1i1p1f1_gr_<year_from>01-<year_to>12.nc"
    fvarnames <- "nbp"
    prefixes <- "EC-Earth3-Veg-LR_piControl_r1i1p1f1"
    modes <- "fldint"
    #froms <- 2255 # last 100a
    froms <- 2701
    #tos <- 2354 # last 100a
    tos <- 2800

} else if (F) { # cmip6 esgf GFDL-CM4
    models <- "GFDL-CM4" # ocean: gfdl-mom6: native: tripolar; cdo: ?
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NOAA-GFDL/GFDL-CM4/piControl/r1i1p1f1/Omon/fgco2/gr/v20180701"
    fpatterns <- "fgco2_Omon_GFDL-CM4_piControl_r1i1p1f1_gr_<year_from>01-<year_to>12.nc"
    prefixes <- "GFDL-CM4_piControl_r1i1p1f1"
    fvarnames <- "fgco2"
    modes <- "fldint"
    froms <- 551 # last 100a
    tos <- 650 # last 100a

} else if (F) { # cmip6 esgf GFDL-ESM4
    models <- "GFDL-ESM4" # ocean: gfdl-mom6: native: tripolar; cdo: ?
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NOAA-GFDL/GFDL-ESM4/piControl/r1i1p1f1/Omon/fgco2/gr/v20180701"
    #fpatterns <- "fgco2_Omon_GFDL-ESM4_piControl_r1i1p1f1_gr_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NOAA-GFDL/GFDL-ESM4/piControl/r1i1p1f1/Lmon/nbp/gr1/v20180701"
    fpatterns <- "nbp_Lmon_GFDL-ESM4_piControl_r1i1p1f1_gr1_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    prefixes <- "GFDL-ESM4_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 401 # last 100a
    tos <- 500 # last 100a

} else if (F) { # cmip6 esgf GISS-E2-1-G
    models <- "GISS-E2-1-G" # ocean: giss_ocean: native: ?; cdo: lonlat; fldint ok
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NASA-GISS/GISS-E2-1-G/piControl/r101i1p1f1/Omon/fgco2/gn/v20190815"
    fpatterns <- "fgco2_Omon_GISS-E2-1-G_piControl_r101i1p1f1_gn_<year_from>01-<year_to>12.nc"
    prefixes <- "GISS-E2-1-G_piControl_r101i1p1f1"
    fvarnames <- "fgco2" # strange large values 1e14 to 1e15
    modes <- "fldint"
    froms <- 1915 # last 100a
    tos <- 2014 # last 100a

} else if (F) { # cmip6 esgf INM-CM4-8
    models <- "INM-CM4-8" # ocean: inm-om5: native: North Pole shifted; cdo ?
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/INM/INM-CM4-8/piControl/r1i1p1f1/Omon/fgco2/gr1/v20190605"
    #fpatterns <- "fgco2_Omon_INM-CM4-8_piControl_r1i1p1f1_gr1_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/INM/INM-CM4-8/piControl/r1i1p1f1/Lmon/nbp/gr1/v20190605"
    fpatterns <- "nbp_Lmon_INM-CM4-8_piControl_r1i1p1f1_gr1_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    prefixes <- "INM-CM4-8_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 2281 # last 100a
    tos <- 2380 # last 100a

} else if (F) { # cmip6 esgf INM-CM5-0
    models <- "INM-CM5-0" # ocean: inm-om5: native: North Pole shifted; cdo ?
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/INM/INM-CM5-0/piControl/r1i1p1f1/Omon/fgco2/gr1/v20190619"
    #fpatterns <- "fgco2_Omon_INM-CM5-0_piControl_r1i1p1f1_gr1_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/INM/INM-CM5-0/piControl/r1i1p1f1/Lmon/nbp/gr1/v20190619"
    fpatterns <- "nbp_Lmon_INM-CM5-0_piControl_r1i1p1f1_gr1_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    prefixes <- "INM-CM5-0_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 3097 # last 100a
    tos <- 3196 # last 100a

} else if (F) { # cmip6 esgf IPSL-CM6A-LR
    models <- "IPSL-CM6A-LR" # ocean: nemo-opa: native: eORCA1.3, tripolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/IPSL/IPSL-CM6A-LR/piControl/r1i1p1f1/Omon/fgco2/gn/v20200326"
    #fpatterns <- "fgco2_Omon_IPSL-CM6A-LR_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/IPSL/IPSL-CM6A-LR/piControl/r1i1p1f1/Lmon/nbp/gr/v20200326"
    #fpatterns <- "nbp_Lmon_IPSL-CM6A-LR_piControl_r1i1p1f1_gr_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/IPSL/IPSL-CM6A-LR/piControl/r1i2p1f1/Emon/fLuc/gr/v20190319"
    #fpatterns <- "fLuc_Emon_IPSL-CM6A-LR_piControl_r1i2p1f1_gr_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fLuc"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/IPSL/IPSL-CM6A-LR/piControl/r1i2p1f1/Lmon/fHarvest/gr/v20190319"
    fpatterns <- "fHarvest_Lmon_IPSL-CM6A-LR_piControl_r1i2p1f1_gr_<year_from>01-<year_to>12.nc"
    fvarnames <- "fHarvest"
    #fvarnames <- "co2_flx_total"
    #prefixes <- "IPSL-CM6A-LR_piControl_r1i1p1f1"
    prefixes <- "IPSL-CM6A-LR_piControl_r1i2p1f1"
    modes <- "fldint"
    froms <- 2000 # last 100a
    #froms <- 3750 # last 100a
    tos <- 2099 # last 100a
    #tos <- 3849 # last 100a

} else if (F) { # cmip6 esgf MIROC-ES2L
    models <- "MIROC-ES2L" # ocean: coco4.9: native: tripolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MIROC/MIROC-ES2L/piControl/r1i1p1f2/Omon/fgco2/gn/v20200124"
    #fpatterns <- "fgco2_Omon_MIROC-ES2L_piControl_r1i1p1f2_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MIROC/MIROC-ES2L/piControl/r1i1p1f2/Emon/netAtmosLandCO2Flux/gn/v20190823"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_MIROC-ES2L_piControl_r1i1p1f2_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MIROC/MIROC-ES2L/piControl/r1i1p1f2/Lmon/nbp/gn/v20190823"
    fpatterns <- "nbp_Lmon_MIROC-ES2L_piControl_r1i1p1f2_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    prefixes <- "MIROC-ES2L_piControl_r1i1p1f2"
    modes <- "fldint"
    froms <- 2250 # last 100a
    tos <- 2349 # last 100a

} else if (F) { # cmip6 esgf MPI-ESM-1-2-HAM
    models <- "MPI-ESM-1-2-HAM" # ocean: mpiom1.63: native: bipolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/HAMMOZ-Consortium/MPI-ESM-1-2-HAM/piControl/r1i1p1f1/Omon/fgco2/gn/v20200120"
    #fpatterns <- "fgco2_Omon_MPI-ESM-1-2-HAM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/HAMMOZ-Consortium/MPI-ESM-1-2-HAM/piControl/r1i1p1f1/Emon/netAtmosLandCO2Flux/gn/v20200120"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_MPI-ESM-1-2-HAM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/HAMMOZ-Consortium/MPI-ESM-1-2-HAM/piControl/r1i1p1f1/Lmon/nbp/gn/v20200120"
    #fpatterns <- "nbp_Lmon_MPI-ESM-1-2-HAM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/HAMMOZ-Consortium/MPI-ESM-1-2-HAM/piControl/r1i1p1f1/Emon/fLuc/gn/v20200120"
    #fpatterns <- "fLuc_Emon_MPI-ESM-1-2-HAM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fLuc"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/HAMMOZ-Consortium/MPI-ESM-1-2-HAM/piControl/r1i1p1f1/Lmon/fHarvest/gn/v20200120"
    fpatterns <- "fHarvest_Lmon_MPI-ESM-1-2-HAM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "fHarvest"
    #fvarnames <- "co2_flx_total"
    prefixes <- "MPI-ESM-1-2-HAM_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 2750 # last 100a
    tos <- 2849 # last 100a

} else if (F) { # cmip6 esgf MPI-ESM1-2-HR
    models <- "MPI-ESM1-2-HR" # ocean: mpiom1.63: native: bipolar; cdo: curvilinear; fldint ok
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/piControl/r1i1p1f1/Omon/fgco2/gn/v20190710"
    fpatterns <- "fgco2_Omon_MPI-ESM1-2-HR_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    prefixes <- "MPI-ESM1-2-HR_piControl_r1i1p1f1"
    fvarnames <- "fgco2"
    modes <- "fldint"
    froms <- 2250 # last 100a
    tos <- 2349 # last 100a

} else if (F) { # cmip6 esgf MPI-ESM1-2-LR
    models <- "MPI-ESM1-2-LR" # ocean: mpiom1.63: native: bipolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Omon/fgco2/gn/v20190710"
    #fpatterns <- "fgco2_Omon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Emon/netAtmosLandCO2Flux/gn/v20190710"
    #fpatterns <- "netAtmosLandCO2Flux_Emon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "netAtmosLandCO2Flux"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Lmon/nbp/gn/v20190710"
    #fpatterns <- "nbp_Lmon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Emon/fLuc/gn/v20190710"
    #fpatterns <- "fLuc_Emon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fLuc"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/Lmon/fHarvest/gn/v20190710"
    fpatterns <- "fHarvest_Lmon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "fHarvest"
    #fvarnames <- "co2_flx_total"
    prefixes <- "MPI-ESM1-2-LR_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 2750 # last 100a
    tos <- 2849 # last 100a

} else if (F) { # cmip6 esgf MRI-ESM2-0 
    models <- "MRI-ESM2-0" # ocean: mri.com4.4: native: tripolar; cdo ?
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MRI/MRI-ESM2-0/piControl/r1i2p1f1/Omon/fgco2/gr/v20200222"
    #fpatterns <- "fgco2_Omon_MRI-ESM2-0_piControl_r1i2p1f1_gr_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MRI/MRI-ESM2-0/piControl/r1i2p1f1/Lmon/nbp/gn/v20200313"
    fpatterns <- "nbp_Lmon_MRI-ESM2-0_piControl_r1i2p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    prefixes <- "MRI-ESM2-0_piControl_r1i2p1f1"
    modes <- "fldint"
    froms <- 2001 # last 100a
    tos <- 2100 # last 100a

} else if (F) { # cmip6 esgf NorCPM1
    models <- "NorCPM1" # ocean: micom1.1: native: ?; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCC/NorCPM1/piControl/r1i1p1f1/Omon/fgco2/gn/v20190914"
    #fpatterns <- "fgco2_Omon_NorCPM1_piControl_r1i1p1f1_gn_<year_from>0?-<year_to>12.nc"
    #fvarnames <- "fgco2"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCC/NorCPM1/piControl/r1i1p1f1/Lmon/nbp/gn/v20190914"
    #fpatterns <- "nbp_Lmon_NorCPM1_piControl_r1i1p1f1_gn_<year_from>0?-<year_to>12.nc"
    #fvarnames <- "nbp"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCC/NorCPM1/piControl/r1i1p1f1/Emon/fLuc/gn/v20190914"
    #fpatterns <- "fLuc_Emon_NorCPM1_piControl_r1i1p1f1_gn_<year_from>0?-<year_to>12.nc"
    #fvarnames <- "fLuc"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCC/NorCPM1/piControl/r1i1p1f1/Lmon/fHarvest/gn/v20190914"
    fpatterns <- "fHarvest_Lmon_NorCPM1_piControl_r1i1p1f1_gn_<year_from>0?-<year_to>12.nc"
    fvarnames <- "fHarvest"
    prefixes <- "NorCPM1_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 401 # last 100a
    tos <- 500 # last 100a

} else if (F) { # cmip6 esgf NorESM1-F
    models <- "NorESM1-F" # ocean: micom: native: ?; cdo: curvilinear; fldint ok
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCC/NorESM1-F/piControl/r1i1p1f1/Omon/fgco2/gn/v20190920"
    fpatterns <- "fgco2_Omon_NorESM1-F_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    prefixes <- "NorESM1-F_piControl_r1i1p1f1"
    fvarnames <- "fgco2"
    modes <- "fldint"
    froms <- 1601 # last 100a
    tos <- 1700 # last 100a

} else if (F) { # cmip6 esgf NorESM2-LM
    models <- "NorESM2-LM" # ocean: micom: native: ?; cdo: curvilinear; fldint ok
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCC/NorESM2-LM/piControl/r1i1p1f1/Lmon/nbp/gn/v20210118"
    fpatterns <- "nbp_Lmon_NorESM2-LM_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "nbp"
    prefixes <- "NorESM2-LM_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 2001 # last 100a
    tos <- 2100 # last 100a

} else if (F) { # cmip6 esgf SAM0-UNICON
    models <- "SAM0-UNICON" # ocean: pop2
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/SNU/SAM0-UNICON/piControl/r1i1p1f1/Lmon/nbp/gn/v20190910"
    #fpatterns <- "nbp_Lmon_SAM0-UNICON_piControl_r1i1p1f1_gn_<year_from>01_<year_to>12.nc"
    #fvarnames <- "nbp"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/SNU/SAM0-UNICON/piControl/r1i1p1f1/Emon/fLuc/gn/v20190910"
    fpatterns <- "fLuc_Emon_SAM0-UNICON_piControl_r1i1p1f1_gn_<year_from>01_<year_to>12.nc"
    fvarnames <- "fLuc"
    prefixes <- "SAM0-UNICON_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 600 # 601 not available
    #froms <- 601 # last 100a
    tos <- 700 # last 100a

} else if (F) { # cmip6 esgf TaiESM1
    models <- "TaiESM1" # ocean: pop2
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AS-RCEC/TaiESM1/piControl/r1i1p1f1/Lmon/nbp/gn/v20200302"
    #fpatterns <- "nbp_Lmon_TaiESM1_piControl_r1i1p1f1_gn_<year_from>0?-<year_to>12.nc"
    #fvarnames <- "nbp"
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AS-RCEC/TaiESM1/piControl/r1i1p1f1/Emon/fLuc/gn/v20200309"
    #fpatterns <- "fLuc_Emon_TaiESM1_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fLuc"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AS-RCEC/TaiESM1/piControl/r1i1p1f1/Lmon/fHarvest/gn/v20200302"
    fpatterns <- "fHarvest_Lmon_TaiESM1_piControl_r1i1p1f1_gn_<year_from>01-<year_to>12.nc"
    fvarnames <- "fHarvest"
    prefixes <- "TaiESM1_piControl_r1i1p1f1"
    modes <- "fldint"
    froms <- 601 # last 100a
    tos <- 700 # last 100a

} else if (F) { # cmip6 esgf UKESM1-0-LL
    models <- "UKESM1-0-LL" # ocean: nemo-hadgem3-go6.0: native: eORCA1 tripolar; cdo: curvilinear; fldint ok
    #datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MOHC/UKESM1-0-LL/piControl/r1i1p1f2/Omon/fgco2/gn/v20200828"
    #fpatterns <- "fgco2_Omon_UKESM1-0-LL_piControl_r1i1p1f2_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "fgco2"
    datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/MOHC/UKESM1-0-LL/piControl/r1i1p1f2/Lmon/nbp/gn/v20200828"
    fpatterns <- "nbp_Lmon_UKESM1-0-LL_piControl_r1i1p1f2_gn_<year_from>01-<year_to>12.nc"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    prefixes <- "UKESM1-0-LL_piControl_r1i1p1f2"
    modes <- "fldint"
    froms <- 3740 # last 100a
    tos <- 3839 # last 100a

} else if (F) { # awi-esm-1-1-lr_kh800 historical historical2
    #models <- "echam6"
    #models <- "jsbach"
    models <- "fesom"
    #post_force <- T
    # historical
    #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical/outdata/echam"
    #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical/outdata/jsbach"
    #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical/outdata/fesom"
    #fpatterns <- "historical_<year>01.01_echam"
    #fpatterns <- "historical_<year>01.01_g3bid"
    #fpatterns <- "historical_<year>01.01_co2"
    #fpatterns <- "historical_<year>01.01_jsbach"
    #fpatterns <- "historical_<year>01.01_nitro"
    #prefixes <- "awi-esm-1-1-lr_kh800_historical"
    #prefixes <- "awi-esm-1-1-lr_kh800_historical_3hr"
    #prefixes <- "awi-esm-1-1-lr_kh800_historical_day"
    # historical2
    #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/echam"
    #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/jsbach"
    datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom"
    #fpatterns <- "historical2_<year><mon>.01_echam"
    #fpatterns <- "historical2_<year><mon>.01_g3bid"
    #fpatterns <- "historical2_<year><mon>.01_co2"
    #fpatterns <- "historical2_<year>01.01_jsbach"
    #fpatterns <- "historical2_<year><mon>.01_nitro"
    fpatterns <- "<fvarnames>_fesom_<year>0101.nc"
    prefixes <- "awi-esm-1-1-lr_kh800_historical2"
    # echam echamstream
    #codes <- 167
    #fvarnames <- "temp2"
    # echam co2stream
    #codes <- 6
    #fvarnames <- "co2_flx_land"
    #codes <- 7
    #fvarnames <- "co2_flx_ocean"
    #fvarnames <- "fgco2"
    #codes <- 24
    #fvarnames <- "co2_flx_lcc"
    #codes <- 25
    #fvarnames <- "co2_flx_harvest"
    #fvarnames <- "nbp" # = co2_flx_land + co2_flx_lcc + co2_flx_harvest
    fvarnames <- "co2_flx_total" # fgco2 + nbp
    # jsbach jsbachstream
    #codes <- 12
    #fvarnames <- "cover_fract"
    #codes <- 20
    #fvarnames <- "veg_ratio_max"
    #codes <- 162
    #fvarnames <- "CO2_emission_landcover_change"
    # jsbach nitrostream
    #codes <- 85
    #fvarnames <- "LCC_flux_box_N2atmos"
    # fesom recom
    #fvarnames <- "aCO2" 
    #fvarnames <- "siarean"
    #fvarnames <- "siareas"
    #fvarnames <- "siextentn"
    fvarnames <- "siextents"
    cdoshifttimes <- "-dt" # for fesom
    #cdo_before_calcs <- "monmean"
    modes <- "select"
    #modes <- "timmean"
    #modes <- "fldmean"
    #modes <- "fldint"
    froms <- 1850
    #froms <- 1966
    #froms <- 1967
    #froms <- 2006
    #froms <- 2007
    #froms <- 2009
    #tos <- 1855
    #tos <- 1985
    #tos <- 1986
    #tos <- 2006
    tos <- 2014

} else if (F) { # awi-esm-1-1-lr_kh800 ssp585
    models <- "echam6"
    #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585b/outdata/echam"
    #fpatterns <- "ssp585b_<year><mon>.01_echam"
    #fpatterns <- "ssp585b_<year><mon>.01_co2"
    #prefixes <- "awi-esm-1-1-lr_kh800_ssp585b"
    datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585/outdata/echam"
    #fpatterns <- "ssp585_<year><mon>.01_echam"
    fpatterns <- "ssp585_<year><mon>.01_co2"
    prefixes <- "awi-esm-1-1-lr_kh800_ssp585"
    #codes <- 167
    #fvarnames <- "temp2"
    #codes <- 6
    #fvarnames <- "co2_flx_land"
    #codes <- 7
    #fvarnames <- "co2_flx_ocean"
    #codes <- 24
    #fvarnames <- "co2_flx_lcc"
    #codes <- 25
    #fvarnames <- "co2_flx_harvest"
    #fvarnames <- "fgco2"
    #fvarnames <- "nbp"
    fvarnames <- "co2_flx_total"
    #modes <- "fldmean"
    modes <- "fldint"
    froms <- 2015
    tos <- 2019

} else if (F) { # awi-esm-1-1-lr_kh800 og esm-piControl test
    models <- "echam6"
    datapaths <- "/mnt/lustre02/work/ba1103/a270094/AWIESM/test_esm3/outdata/echam"
    fpatterns <- "test_esm3_echam6_echam_<year><mon>.nc"
    prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl"
    fvarnames <- "temp2"
    modes <- list("fldmean_monmean"=c("fldmean", "monmean"))
    froms <- 1850
    tos <- 1855

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl init
    models <- "echam6"
    datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl/outdata/echam"
    fpatterns <- "esm-piControl_<year><mon>.01_g3bmon"
    prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl"
    codes <- 167
    fvarnames <- "temp2"
    modes <- "fldmean"
    froms <- 1001
    tos <- 1010

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl
    models <- "echam6"
    #models <- "jsbach"
    #models <- "fesom"
    if (F) {
        datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_2685/outdata/echam"
        #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_2685/outdata/fesom"
        #fpatterns <- "esm-piControl_2685_<year><mon>.01_g3bmon"
        #fpatterns <- "esm-piControl_2685_<year><mon>.01_tracermon"
        fpatterns <- "esm-piControl_2685_<year><mon>.01_co2mon"
        #fpatterns <- "<fvarnames>_fesom_<year>0101.nc"
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_2percatm"
    } else if (F) {
        datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_2percfalse/outdata/echam"
        fpatterns <- "esm-piControl_2percfalse_<year><mon>.01_co2mon"
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_2percfalse"
    } else if (F) {
        datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_2percboth/outdata/echam"
        fpatterns <- "esm-piControl_<year><mon>.01_co2mon"
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_2percboth"
    } else if (F) {
        #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_co2fsign/outdata/echam"
        datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_co2fsign/outdata/fesom"
        #fpatterns <- "esm-piControl_co2fsign_<year><mon>.01_co2mon"
        fpatterns <- "<fvarnames>_fesom_<year>0101.nc"
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_co2fsign"
    } else if (F) {
        #datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_restartall/outdata/echam"
        datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_restartall/outdata/jsbach"
        #fpatterns <- "esm-piControl_restartall_<year><mon>.01_co2mon"
        #fpatterns <- "esm-piControl_restartall_<year><mon>.01_vegmon"
        fpatterns <- "esm-piControl_restartall_<year><mon>.01_jsbachmon"
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_restartall"
    } else if (F) {
        datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl/outdata/echam"
        fpatterns <- "esm-piControl_<year><mon>.01_co2mon"
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl"
    } else if (T) {
        datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest/outdata/echam"
        fpatterns <- "esm-piControl_wout_talk_rest_<year><mon>.01_co2mon"
        prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest"
    }
    #codes <- 167
    #fvarnames <- "temp2"
    #codes <- 1
    #fvarnames <- "CO2"
    #cdo_after_calcs <- list(c("setunit,ppm", "mulc,658267")) # [CO2] (in ppm) = 1e6 * 0.658267 * co2mmr
    #sellevels <- 1 
    #sellevels <- "0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,21000,22000,23000,24000,25000,26000"
    #codes <- 5
    #fvarnames <- "co2_flux"
    #codes <- 6
    #fvarnames <- "co2_flx_land"
    codes <- 7
    fvarnames <- "co2_flx_ocean"
    #codes <- 9
    #fvarnames <- "co2_burden_corr_acc2"
    #codes <- 21
    #fvarnames <- "co2_flx_npp"
    #codes <- 22
    #fvarnames <- "co2_flx_resp"
    #codes <- 24
    #fvarnames <- "co2_flx_lcc"
    #codes <- 160
    #fvarnames <- "boxC_green"
    #codes <- 12
    #fvarnames <- "cover_fract"
    #codes <- 20
    #fvarnames <- "veg_ratio_max"
    #fvarnames <- "thetaoga"
    #fvarnames <- "aCO2"
    #cdoshifttimes <- "-dt" # for fesom
    #modes <- "select"
    #modes <- "timmean"
    #modes <- "fldmean"
    #modes <- "fldsum"
    modes <- "fldint"
    #froms <- 2685 # last piControl og year
    #froms <- 2686 # esm-piControl start from piControl
    #froms <- 2778
    #froms <- 2817
    #froms <- 3001 # esm-piControl start from piControl_LUtrans1850
    froms <- 3151 # esm-piControl_wout_talk_restore
    #tos <- 2687
    #tos <- 2785 # esm-piControl 100 years
    #tos <- 2962
    #tos <- 3112
    #tos <- 3136
    tos <- 3156
    #areas_out_list <- list(list(name="SO45S",
    #                            sellonlatbox=c(lon1=0,lon2=360,lat1=-45,lat2=-90)))

} else if (F) { # mpiesm-1.2.01p5 mpiesm-s
    models <- "jsbach"
    datapaths <- "/work/ba1103/a270073/out/mpiesm-1.2.01p5/mpiesm-s/piControl_2801_ndepo_init_model_restart/outdata/jsbach"
    fpatterns <- "piControl_2801_jsbach_jsbach_<year>.grb"
    prefixes <- "jsbach-3.20p1_piControl_ndepo_init_restart"
    #codes <- 12
    #fvarnames <- "cover_fract"
    codes <- 20
    fvarnames <- "veg_ratio_max"
    modes <- "select"
    froms <- 2801
    tos <- 2802

} else if (F) { # foci from seb wahl
    models <- "echam6"
    datapaths <- "/work/bb0519/b350071/tmp/"
    fpatterns <- "FOCI1.3-SW038_echam6_co2_co2_flx_lcc_<year_from>-<year_to>.nc"
    prefixes <- "FOCI1.3-SW038_piControl"
    fvarnames <- "co2_flx_lcc"
    cdo_before_calcs <- "monmean"
    froms <- 1850
    tos <- 3349
    modes <- "fldint"

# ======================================================
# 2 settings
} else if (F) {
    models <- c("echam6", "echam6")
    datapaths <- c("/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/hist/outdata/echam",
                   "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/hist_LUH/outdata/echam")
    fpatterns <- c(#"hist5_echam6_echammon_<year>.grb",
                   "hist_echam6_echammon_<year><mon>.nc",
                   "hist_LUH_echam6_echammon_<year><mon>.grb")
    fvarnames <- c("temp2", "temp2")
    codes <- c(NA, 167) # necessary for grb files
    froms <- c(1850, 1850)
    tos <- c(1851, 1851)
    #modes <- c("fldmean", "fldmean")
    modes <- c("timmean", "timmean")
    prefixes <- c("dynveg_noLUH", "dynveg_LUH")

} else if (F) {
    datapaths <- c("/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/1percCO2/outdata/echam",
                   "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/4CO2/outdata/echam")
    fpatterns <- c("1percCO2_echam6_echammon_<year><mon>.nc",
                   "4CO2_echam6_echammon_<year><mon>.nc")
    fvarnames <- c("temp2", "temp2")
    models <- c("echam6", "echam6") # for check if `cdo -t echam6` can be used
    froms <- c(1850, 1850)
    #tos <- c(1859, 1859)
    tos <- c(2099, 2099)
    modes <- c("fldmean", "fldmean")
    prefixes <- c("dynveg", "dynveg")

} else if (F) { # recT106erai: echam5-wiso, T106L31, ERA-Interim nudging, MB and MW parts on stan
    datapaths <- c("/ace/user/mwerner/echam5-wiso/T106L31/EXP007_MB/MONMEAN", # mb
                   "/ace/user/mwerner/echam5-wiso/T106L31/EXP020/MONMEAN") # mw
    models <- c("echam5", "echam5")
    fpatterns <- c("EXP007_T106_MB_195801.201312.combined.monmean.wiso.nc", # mb
                   "EXP020_T106_MW_201301.201908.monmean.wiso_d.nc") # mw
    prefixes <- c("echam5_recT106erai_wiso_MB", # mb
                  "echam5_recT106erai_wiso_MW") # mw
    #fvarnames <- c("temp2", "temp2")
    #fvarnames <- c("tsurf", "tsurf")
    fvarnames <- c("aprt", "aprt")
    modes <- c("select", "select")
    froms <- c(1958, 2013) 
    tos <- c(2012, 2019)

# ======================================================
# 3 settings
} else if (F) { # deck hist 1pct 4co2
    models <- c("echam6", "echam6", "echam6") # for check if `cdo -t echam6` can be used
    if (F) { # awi-esm-1-1-rl
        datapaths <- c("/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/hist/outdata/echam",
                       "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/1percCO2/outdata/echam",
                       "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/4CO2/outdata/echam")
        fpatterns <- c("hist_echam6_echam_<year><mon>.nc",
                       "1percCO2_echam6_echam_<year><mon>.nc",
                       "4CO2_echam6_echam_<year><mon>.nc")
        prefixes <- paste0("awi-esm-1-1-lr_", c("historical", "1percCO2", "4CO2"), 
                           "_echam6_echam") 
    } else if (F) { # awi-esm-1-2-lr
        datapaths <- c("/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_HIST/outdata/echam",
                       "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_1percCO2/outdata/echam",
                       "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_4CO2/outdata/echam")
        fpatterns <- c("CMIP6_HIST_echam6_echam_<year><mon>.grb",
                       "CMIP6_1percCO2_echam6_echam_<year><mon>.grb",
                       "CMIP6_4CO2_echam6_echam_<year><mon>.grb")
        prefixes <- paste0("awi-esm-1-2-lr_", c("historical", "1percCO2", "4CO2"), 
                           "_echam6_echam") 
    }
    fvarnames <- rep("temp2", t=3)
    #fvarnames <- rep("srad0d", t=3)
    #fvarnames <- rep("srad0", t=3)
    #fvarnames <- rep("trad0", t=3)
    froms <- c(1850, 1850, 1850)
    #froms <- c(1985, 1985, 1985)
    #froms <- c(1985, 2070, 2070)
    #tos <- c(1851, 1851, 1851)
    #tos <- c(2014, 2014, 2014)
    tos <- c(2014, 2074, 2021) # lars awi-esm-1-2-lr
    #tos <- c(2014, 2099, 2099)
    #season_inds <- list(c(12, 1, 2), c(12, 1, 2), c(12, 1, 2)) # DJF
    #season_inds <- list(3:5, 3:5, 3:5) # MAM
    #season_inds <- list(6:8, 6:8, 6:8) # JJA
    #season_inds <- list(9:11, 9:11, 9:11) # SON
    modes <- rep("fldmean", t=3)
    #modes <- rep("timmean", t=3)

# ======================================================
# 4 settings
} else if (F) { # deck pi hist 1pct 4co2
    models <- rep("echam6", t=4) # for check if `cdo -t echam6` can be used
    if (F) { # awi-cm-1-1-lr
        datapaths <- c("/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_false/PI-CTRL_nodynveg2/outdata/echam",
                       "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_false/historical/outdata/echam",
                       "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_false/1percCO2/outdata/echam",
                       "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_false/4CO2/outdata/echam")
        if (F) { # srad0, trad0
            fpatterns <- c("PI-CTRL_nodynveg2_echam6_echam_<year><mon>.grb",
                           "historical_echam6_echam_<year><mon>.grb",
                           "1percCO2_echam6_echam_<year><mon>.grb",
                           "4CO2_echam6_echam_<year><mon>.grb")
            prefixes <- paste0("awi-cm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"), 
                               "_echam6_echam") 
        } else if (F) { # temp2
            fpatterns <- c("PI-CTRL_nodynveg2_echam6_echammon_<year><mon>.grb",
                           "historical_echam6_echammon_<year><mon>.grb",
                           "1percCO2_echam6_echammon_<year><mon>.grb",
                           "4CO2_echam6_echammon_<year><mon>.grb")
            prefixes <- paste0("awi-cm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"), 
                               "_echam6_echammon") 
        }
        froms <- c(1855, 1850, 1850, 1850) # pi last 30 years: 1924:1954; pi last 100 years: 1855:1954
        tos <- c(1954, 2014, 2099, 2099)
    } else if (F) { # awi-esm-1-1-lr
        datapaths <- c("/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam",
                       "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/hist/outdata/echam",
                       "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/1percCO2/outdata/echam",
                       "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/4CO2/outdata/echam")
        if (F) { # srad0, trad0
            fpatterns <- c("piControl_echam6_echam_<year><mon>.grb",
                           "hist_echam6_echam_<year><mon>.nc",
                           "1percCO2_echam6_echam_<year><mon>.nc",
                           "4CO2_echam6_echam_<year><mon>.nc")
            prefixes <- paste0("awi-esm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"), 
                               "_echam6_echam") 
        } else if (F) { # temp2
            fpatterns <- c("piControl_echam6_echammon_<year><mon>.grb",
                           "hist_echam6_echammon_<year><mon>.nc",
                           "1percCO2_echam6_echammon_<year><mon>.nc",
                           "4CO2_echam6_echammon_<year><mon>.nc")
            prefixes <- paste0("awi-esm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"), 
                               "_echam6_echammon") 
        } else if (F) { # tau_aero_550
            fpatterns <- c("piControl_echam6_aeroptmon_<year><mon>.grb",
                           "hist_echam6_echammon_<year><mon>.nc",
                           "1percCO2_echam6_echammon_<year><mon>.nc",
                           "4CO2_echam6_echammon_<year><mon>.nc")
            prefixes <- paste0("awi-esm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"), 
                               "_echam6_", c("aeroptmon", "echammon", "echammon", "echammon")) 
        }
        froms <- c(1842, 1850, 1850, 1850) # pi last 30 years: 1912:1941; pi last 100 years: 1842:1941
        tos <- c(1941, 2014, 2099, 2099)
    } else if (F) { # awi-esm-1-2-lr
        datapaths <- c("/work/ba0989/a270124/CMIP6_PMIP4/PI_LA04_cont02/outdata/echam",
                       "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_HIST/outdata/echam",
                       "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_1percCO2/outdata/echam",
                       "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_4CO2/outdata/echam")
        fpatterns <- c("PI_LA04_cont02_echam6_echam_<year><mon>.grb",
                       "CMIP6_HIST_echam6_echam_<year><mon>.grb",
                       "CMIP6_1percCO2_echam6_echam_<year><mon>.grb",
                       "CMIP6_4CO2_echam6_echam_<year><mon>.grb")
        prefixes <- paste0("awi-esm-1-2-lr_", c("piControl", "historical", "1percCO2", "4CO2"), 
                           "_echam6_echam")
        froms <- c(1016, 1850, 1850, 1850) # lars
        tos <- c(1045, 2014, 2074, 2021)
    }
    #fvarnames <- rep("temp2", t=4)
    #fvarnames <- rep("srad0d", t=4)
    #fvarnames <- rep("srad0", t=4)
    #fvarnames <- rep("trad0", t=4)
    fvarnames <- rep("toa_imbalance", t=4)
    #fvarnames <- rep("tau_aero_550", t=4)
    #codes <- rep(11, t=4)
    #tos <- froms
    #season_inds <- list(c(12, 1, 2), c(12, 1, 2), c(12, 1, 2)) # DJF
    #season_inds <- list(3:5, 3:5, 3:5) # MAM
    #season_inds <- list(6:8, 6:8, 6:8) # JJA
    #season_inds <- list(9:11, 9:11, 9:11) # SON
    modes <- rep("fldmean", t=4)
    #modes <- rep("timmean", t=4)
    #modes <- rep("volint", t=4)

# ======================================================
# 6 settings
} else if (F) { # awi-esm-1-1-lr_kh800 concentration driven ensemble: pi (2686 to 2851 (2014) and 2936 (2100)) hist ssp126 ssp245 ssp534-over ssp585
    models <- rep("echam6", t=6)
    #models <- rep("fesom", t=6)
    datapaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/", 
                        c("piControl", "historical2", "ssp126", "ssp245", "ssp534-over", "ssp585"),
                        "/outdata/echam"
                        #"/outdata/fesom"
                        )
    fpatterns <- paste0(c("piControl", "historical2", "ssp126", "ssp245", "ssp534-over", "ssp585"), 
                        #"_<year><mon>.01_echam")
                        "_<year><mon>.01_co2")
    #fpatterns <- rep("<fvarnames>_fesom_<year>0101.nc", t=6)
    prefixes <- paste0("awi-esm-1-1-lr_kh800_", 
                       c("piControl", "historical", "ssp126", "ssp245", "ssp534-over", "ssp585"))
    #codes <- rep(167, t=6)
    #fvarnames <- rep("temp2", t=6)
    codes <- rep(7, t=6)
    fvarnames <- rep("co2_flx_ocean", t=6)
    #fvarnames <- rep("siextentn", t=6)
    #fvarnames <- rep("siextents", t=6)
    #cdoshifttimes <- rep("-dt", t=6)
    froms <- c(2686, 1850, rep(2015, t=4))
    #tos <- c(2936, 2014, 2100, 2100, 2100, 2100)
    tos <- c(3000, 2014, 2100, 2100, 2100, 2100)
    #froms <- c(2851-29, 2014-29, rep(2100-29, t=4)) # last 30 years
    #tos <- c(2851, 2014, rep(2100, t=4))
    #modes <- rep("select", t=6)
    #modes <- rep("fldmean", t=6)
    modes <- rep("fldint", t=6)
    #modes <- rep("timmean", t=6)
    mask_list <- lapply(vector("list", l=length(models)), base::append, 
                        # reccap2 basins:
                        #list(name="reccap2_atlantic",
                        #     cdo_mask=paste0("-eqc,1 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_pacific",
                        #     cdo_mask=paste0("-eqc,2 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_indian",
                        #     cdo_mask=paste0("-eqc,3 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_arctic",
                        #     cdo_mask=paste0("-eqc,4 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_southern",
                        #     cdo_mask=paste0("-eqc,5 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        # reccap2 atlantic biomes:
                        #list(name="reccap2_na_spss",
                        #    cdo_mask=paste0("-eqc,1 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_na_stss",
                        #    cdo_mask=paste0("-eqc,2 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_na_stps",
                        #     cdo_mask=paste0("-eqc,3 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_aequ",
                        #     cdo_mask=paste0("-eqc,4 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_sa_stps",
                        #     cdo_mask=paste0("-eqc,5 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_med",
                        #     cdo_mask=paste0("-eqc,6 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        # reccap2 pacific biomes: 
                        #list(name="reccap2_np_spss",
                        #     cdo_mask=paste0("-eqc,1 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_np_stss",
                        #     cdo_mask=paste0("-eqc,2 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_np_stps",
                        #     cdo_mask=paste0("-eqc,3 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_pequ_w",
                        #     cdo_mask=paste0("-eqc,4 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_pequ_e",
                        #     cdo_mask=paste0("-eqc,5 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_sp_stps",
                        #     cdo_mask=paste0("-eqc,6 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        # reccap2 indian biomes: 
                        #list(name="reccap2_ind_stps",
                        #     cdo_mask=paste0("-eqc,1 -select,name=indian ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_ind_interior",
                        #     cdo_mask=paste0("-eqc,2 -select,name=indian ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        # reccap2 arctic biomes: 
                        #list(name="reccap2_arctic_ice",
                        #     cdo_mask=paste0("-eqc,1 -select,name=arctic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_np_ice",
                        #     cdo_mask=paste0("-eqc,2 -select,name=arctic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_na_ice",
                        #     cdo_mask=paste0("-eqc,3 -select,name=arctic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_barents",
                        #     cdo_mask=paste0("-eqc,4 -select,name=arctic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        # reccap2 southern biomes: 
                        #list(name="reccap2_so_stss",
                        #     cdo_mask=paste0("-eqc,1 -select,name=southern ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_so_spss",
                        #     cdo_mask=paste0("-eqc,2 -select,name=southern ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        #list(name="reccap2_so_ice",
                        #     cdo_mask=paste0("-eqc,3 -select,name=southern ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_T63.nc")))
                        # gregor_etal_2019 basins:
                        #list(name="gregor_etal_2019_nh-hl",
                        #     cdo_mask=paste0("-eqc,1 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_T63.nc")))
                        #list(name="gregor_etal_2019_nh-st",
                        #     cdo_mask=paste0("-eqc,2 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_T63.nc")))
                        #list(name="gregor_etal_2019_equ",
                        #     cdo_mask=paste0("-eqc,3 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_T63.nc")))
                        #list(name="gregor_etal_2019_sh-st",
                        #     cdo_mask=paste0("-eqc,4 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_T63.nc")))
                        list(name="gregor_etal_2019_sh-hl",
                             cdo_mask=paste0("-eqc,5 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_T63.nc")))


# ======================================================
# 12 settings
} else if (F) { # 12 months timmeans
    models <- rep("echam6", t=12)
    datapaths <- rep("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_co2fsign/outdata/echam", t=12)
    fpatterns <- rep("esm-piControl_co2fsign_<year><mon>.01_co2mon", t=12)
    prefixes <- rep("awi-esm-1-1-lr_kh800_esm-piControl_co2fsign", t=12)
    #codes <- rep(7, t=12)
    #fvarnames <- rep("co2_flx_ocean", t=12)
    codes <- rep(6, t=12)
    fvarnames <- rep("co2_flx_land", t=12)
    season_inds <- 1:12
    modes <- rep("timmean", t=12)
    froms <- rep(2686, t=12)
    tos <- rep(2704, t=12)

# ======================================================
# 16 settings
} else if (F) { # 15/16 reccap2 settings (global: wout ROMS; regional: with ROMS)
    post_force <- T
    models <- c("CCSM-WHOI", "CESM-ETHZ", "CNRM-ESM2-1", "EC-Earth3", "ECCO-Darwin", 
                "FESOM_REcoM_HR", "FESOM_REcoM_LR", "MOM6-COBALT2-Princeton", 
                "MPIOM-HAMOCC", "MRI-ESM2-0", "NorESM-OC1.2", "OCIM-v2014", "OCIM-v2021", 
                "ORCA025-GEOMAR", "ORCA1-LIM3-PISCES")
    datapaths <- sapply(models, function(x) dir("/work/ollie/ncara/RECCAPv2/reccap_submissions/download_20220124/Models/2D_CO2", pattern=x, full.name=T))
    # set grid:
    datapaths[which(models == "FESOM_REcoM_HR")] <- "/work/ollie/cdanek/data/reccap2-ocean/FESOM_REcoM_HR"
    datapaths[which(models == "FESOM_REcoM_LR")] <- "/work/ollie/cdanek/data/reccap2-ocean/FESOM_REcoM_LR"
    # set grid and NA value (`cdo fldsum` yields NA) and rechunked for faster reading: 
    datapaths[which(models == "OCIM-v2014")] <- "/work/ollie/cdanek/data/reccap2-ocean/OCIM-v2014"
    datapaths[which(models == "OCIM-v2021")] <- "/work/ollie/cdanek/data/reccap2-ocean/OCIM-v2021"
    # set NA value (`cdo fldsum` yields NA):
    datapaths[which(models == "CESM-ETHZ")] <- "/work/ollie/cdanek/data/reccap2-ocean/CESM-ETHZ"
    # set NA value (`cdo fldsum` yields NA) and rechunked for faster reading:
    datapaths[which(models == "ECCO-Darwin")] <- "/work/ollie/cdanek/data/reccap2-ocean/ECCO-Darwin"
    # rechunked for faster reading:
    datapaths[which(models == "CCSM-WHOI")] <- "/work/ollie/cdanek/data/reccap2-ocean/CCSM-WHOI"
    datapaths[which(models == "NorESM-OC1.2")] <- "/work/ollie/cdanek/data/reccap2-ocean/NorESM-OC1.2"
    froms <- c(1980, rep(1980, t=3), 1995, rep(1980, t=3), 1980, rep(1980, t=2), 1980, rep(1980, t=3))
    tos <- c(2017, rep(2018, t=3), 2018, rep(2018, t=3), 2019, rep(2018, t=2), 2017, rep(2018, t=3))
    if (F) { # add regional ROMS possible; atlantic
        models <- c(models, "ROMS-Atlantic-ETHZ")
        # set griddes (xfirst = 0.5 --> -179.5) and rechunked for faster reading and *100 (damian.loher@usys.ethz.ch) 
        datapaths[which(models == "ROMS-Atlantic-ETHZ")] <- "/work/ollie/cdanek/data/reccap2-ocean/ROMS-Atlantic-ETHZ" 
        froms <- c(froms, 1980)
        tos <- c(tos, 2019)
    }
    if (F) { # pacific
        # rechunked for faster reading and *100 (damian.loher@usys.ethz.ch) 
        models <- c(models, "ROMS-Pacific-ETHZ")
        datapaths[which(models == "ROMS-Pacific-ETHZ")] <- "/work/ollie/cdanek/data/reccap2-ocean/ROMS-Pacific-ETHZ" 
        froms <- c(froms, 1980)
        tos <- c(tos, 2019)
    }
    if (F) { # indian
        models <- c(models, "ROMS-NYUAD")
        datapaths[which(models == "ROMS-NYUAD")] <- "/work/ollie/cdanek/data/reccap2-ocean/ROMS-NYUAD"
        froms <- c(froms, 1980)
        tos <- c(tos, 2018)
    }
    if (T) { # southern
        models <- c(models, "ROMS-SouthernOcean-ETHZ")
        # rechunked for faster reading
        datapaths[which(models == "ROMS-SouthernOcean-ETHZ")] <- "/work/ollie/cdanek/data/reccap2-ocean/ROMS-SouthernOcean-ETHZ" 
        froms <- c(froms, 1980)
        tos <- c(tos, 2018)
    }
    # set dates:
    new_date_list <- vector("list", l=length(models))
    new_date_list[[which(models == "CCSM-WHOI")]] <- list(dates=paste0(rep(1958:2017, e=12), "-", rep(1:12, t=length(1958:2017)), "-", rep(15, t=length(1958:2017)*12))) # 1st date: 1958-02-01
    new_date_list[[which(models == "ECCO-Darwin")]] <- list(dates=paste0(rep(1995:2018, e=12), "-", rep(1:12, t=length(1995:2018)), "-", rep(15, t=length(1995:2018)*12))) # 1st date: 0016-01-16 
    new_date_list[[which(models == "FESOM_REcoM_HR")]] <- list(dates=paste0(rep(1980:2018, e=12), "-", rep(1:12, t=length(1980:2018)), "-", rep(15, t=length(1980:2018)*12))) # 1st date: 0000-00-00
    new_date_list[[which(models == "FESOM_REcoM_LR")]] <- new_date_list[[which(models == "FESOM_REcoM_HR")]] # 1st date: 0000-00-00
    fpatterns <- paste0("fgco2_", models, "*A_*_gr_", froms, "-*.nc")
    prefixes <- rep("reccap2_A", t=length(models))
    fvarnames <- rep("fgco2", t=length(models))
    modes <- rep("fldint", t=length(models))
    # reccap2 basins:
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_atlantic"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,1 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_pacific"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,2 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_indian"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,3 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_arctic"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,4 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_southern"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,5 -select,name=open_ocean ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    # reccap2 atlantic biomes:
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_na_spss"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,1 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_na_stss"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,2 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_na_stps"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,3 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_aequ"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,4 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_sa_stps"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,5 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_med"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,6 -select,name=atlantic ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    # reccap2 pacific biomes:
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_np_spss"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,1 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_np_stss"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,2 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_np_stps"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,3 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_pequ_w"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,4 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_pequ_e"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,5 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="reccap2_sp_stps"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,6 -select,name=pacific ", workpath, "/mesh/lsm/reccap2-ocean/RECCAP2_region_masks_all_", models[i], ".nc")
    # gregor_etal_2019 basins:
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="gregor_etal_2019_nh-hl"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,1 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="gregor_etal_2019_nh-st"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,2 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="gregor_etal_2019_equ"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,3 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_", models[i], ".nc")
    #mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="gregor_etal_2019_sh-st"))
    #for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,4 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_", models[i], ".nc")
    mask_list <- lapply(vector("list", l=length(models)), base::append, list(name="gregor_etal_2019_sh-hl"))
    for (i in seq_along(mask_list)) mask_list[[i]]$cdo_mask <- paste0("-eqc,5 -select,name=bio23_5 ", workpath, "/mesh/lsm/gregor_etal_2019/gregor_etal_2019_bio23_5_mask_from_reccap2_", models[i], ".nc")

# ======================================================
# 27 settings
} else if (F) { # 27 levels
    models <- rep("echam6", t=27)
    datapaths <- rep("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl_og_restart_processed/restart", t=27)
    fpatterns <- rep("restart_test_<year>1231_tracer_ncpdq_co2_aps_hl.nc", t=27) # original dims permuted; model levels -> height levels
    prefixes <- rep("awi-esm-1-1-lr_kh800_piControl_restart_hl_ppm", t=27)
    fvarnames <- rep("CO2", t=27)
    cdo_after_calcs <- vector("list", l=27)
    cdo_after_calcs[] <- list(c("setunit,ppm", "mulc,658267")) # [CO2] (in ppm) = 1e6 * 0.658267 * co2mmr
    sellevels <- c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 
                   11000, 12000, 13000, 14000, 15000, 16000, 17000, 18000, 19000, 
                   20000, 21000, 22000, 23000, 24000, 25000, 26000)
    modes <- rep("select", t=27)
    froms <- rep(2685, t=27) # last piControl og year
    tos <- rep(2685, t=27)

# ======================================================
# special: cmip6 nml created by ~/slurm/cronjobs/filter_esgf_lists.r
} else if (T) { # 27 levels
    post_force <- T # for test
    if (F) { # piControl test
        fnml <- "~/slurm/cronjobs/namelist.post_24settings_Omon_fgco2_23models_piControl_2-2nyears_2022-05-05_12-19-46.r"
    } else if (T) { # piControl
        fnml <- "~/slurm/cronjobs/namelist.post_23settings_Omon_fgco2_23models_piControl_251-251nyears_2022-05-06_09-04-39.r"
    }
    if (!file.exists(fnml)) stop("could not find file ", fnml)
    message("run `source(\"", fnml, "\")` ...")
    source(fnml)
    # which mode
    modes <- rep("fldint", t=length(models))
    # do spatial interpolation if necessary
    cdo_before_calcs <- rep("", t=length(models))
    indsmode <- which(!is.na(match(modes, c("fldmean", "fldint"))))
    if (length(indsmode) > 0) {
        indsmodel <- which(models == "BCC-ESM1")
        if (length(indsmodel) > 0) {
            indsmatch <- match(indsmodel, indsmode)
            if (length(indsmatch) > 0) {
                cdo_before_calcs[indsmatch] <- "remapbil,global_1"
            }
        }
    }

} # which setting

# run
source(paste0(repopath, "/post_echam.r"))

