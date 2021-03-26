# input for post.echam.r

verbose <- 1 # 0,1
clean <- T # remove temporary files
cdo_silent <- "" # "-s" for silent or ""
cdo_select_no_history <- "" # "--no_history" or ""
cdo_convert_grb2nc <- T # should post processing result be converted to nc (will be set to T if new dates are wanted)?
cdo_force <- T # redo cdo command although output file already exists 
cdo_OpenMP_threads <- "-P 4" # "-P n" or "" (will be irgnored on commands that do not support OMP)
cdo_set_rel_time <- T # conversion from absolute to relative time
cdo_run_from_script <- T # create temporary file and run long cdo command from there
## maximum number of args cdo
# stan0/1: getconf ARG_MAX 2621440
# paleosrv1: getconf ARG_MAX 2097152
cdo_nchar_max_arglist <- 2350000 # reduce this number if you get segmentation fault on the cdo selection command (many files)
## maximum number of args nco
# $(getconf PAGE_SIZE)*32 = 4096*32 = 131072
nco_nchar_max_arglist <- 131071

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
    prefixes <- "dynveg"
    
} else if (F) { # my stupid cmip6 awi-esm-1-1-lr pi chunks
    if (F) { # chunk 1: restart_from_hu_oceanonly 2701 to 2702 
        datapaths <- "/work/ab0246/a270073/awicm-CMIP6/restart_from_hu_oceanonly/outdata/echam"
        fpatterns <- "restart_from_hu_oceanonly_echam6_g3bid_<YYYY><MM>.grb"
        froms <- 2701
        tos <- 2702
    } else if (F) { # chunk 2: restart_from_restart_from_hu_oceanonly 2703 to 2710
        datapaths <- "/work/ab0246/a270073/awicm-CMIP6/restart_from_restart_from_hu_oceanonly/outdata/echam"
        fpatterns <- "restart_from_restart_from_hu_oceanonly_echam6_g3bid_<YYYY><MM>.grb"
        froms <- 2703
        tos <- 2710
    } else if (F) { # pi-ctrl: 2711-2869
        datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL/outdata/echam"
        fpatterns <- "PI-CTRL_echam6_g3bid_<YYYY><MM>.grb"
        froms <- 2750 # 2711 to 2749 are missing?!
        tos <- 2869
    } else if (F) { # pi-ctrl2: 2870-2899 
        datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL2/outdata/echam"
        fpatterns <- "PI-CTRL2_echam6_g3bid_<YYYY><MM>.grb"
        froms <- 2870
        tos <- 2899
    } else if (F) { # pi-ctrl3: 2900-2910
        datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL3/outdata/echam"
        fpatterns <- "PI-CTRL3_echam6_g3bid_<YYYY><MM>.grb"
        froms <- 2900
        tos <- 2910
    } else if (F) { # pi-ctrl4: 2911:2999
        datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL4/outdata/echam"
        fpatterns <- "PI-CTRL4_echam6_g3bid_<YYYY><MM>.grb"
        froms <- 2911
        tos <- 2999
    }
    models <- "echam6"
    prefixes <- "awi-esm-1-1-lr_piControl_g3bid_daily"
    fvarnames <- "temp2"
    codes <- 167
    modes <- "fldmean"

} else if (T) { # cmip6
    #models <- "echam6"
    models <- "mpiom1"
    #models <- "fesom"
    # awi-cm-1-1-lr
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_false/PI-CTRL_nodynveg2/outdata/echam"
    #datapaths <- "/work/ab0995/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_false/PI-CTRL_nodynveg2/outdata/echam"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_false/historical/outdata/echam"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_false/1percCO2/outdata/echam"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_false/4CO2/outdata/echam"
    # awi-esm-1-1-lr
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam" # 1543:1941
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/outdata/fesom"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/1percCO2/outdata/fesom"
    #cdoshifttimes <- "-1dt"
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
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/piControl/r1i1p1f1/SImon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/historical/r1i1p1f1/day/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/historical/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/historical/r1i1p1f1/Omon/<fvarnames>/gn/v20190710/"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/1pctCO2/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/1pctCO2/r1i1p1f1/Oday/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-LR/1pctCO2/r1i1p1f1/Omon/<fvarnames>/gn/v20190710"
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
    datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/1pctCO2/r1i1p1f1/Oday/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/1pctCO2/r1i1p1f1/Omon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/1pctCO2/r1i1p1f1/SImon/<fvarnames>/gn/v20190710"
    #datapaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/abrupt-4xCO2/r1i1p1f1/Amon/<fvarnames>/gn/v20190710"
    # awi-esm-1-1-lr
    #fpatterns <- "PI-CTRL6_fesom_<fvarnames>_<YYYY>0101.nc"
    #fpatterns <- "1percCO2_fesom_<fvarnames>_<YYYY>0101.nc"
    #fpatterns <- "historical_echam6_echamday_<YYYY><MM>.grb"
    #fpatterns <- "<fvarnames>_day_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<YYYY><MM_from>-<YYYY><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Emon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<YYYY><MM_from>-<YYYY><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Omon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_3hr_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<YYYY_from>01010300-<YYYY_to>01010000.nc"
    #fpatterns <- "<fvarnames>_6hrPlev_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<YYYY_from>01010300-<YYYY_to>12312100.nc"
    #fpatterns <- "<fvarnames>_6hrLev_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<YYYY_from>01010600-<YYYY_to>01010000.nc"
    #fpatterns <- "<fvarnames>_day_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_AERmon_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Eday_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_SIday_AWI-ESM-1-1-LR_historical_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-ESM-1-1-LR_midHolocene_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_day_AWI-ESM-1-1-LR_midHolocene_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_AWI-ESM-1-1-LR_lgm_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    # awi-cm-1-1-mr
    #fpatterns <- "<fvarnames>_Amon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY><MM_from>-<YYYY><MM_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_day_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_<YYYY>0101-<YYYY>1231.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_<YYYY><MM_from>-<YYYY><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<YYYY><MM_from>-<YYYY><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_AWI-CM-1-1-MR_abrupt-4xCO2_r1i1p1f1_gn_<YYYY><MM_from>-<YYYY><MM_to>.nc"
    # mpi-esm1-2-lr
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Oday_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_MPI-ESM1-2-LR_piControl_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-LR_historical_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-LR_historical_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_day_MPI-ESM1-2-LR_historical_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-LR_1pctCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Oday_MPI-ESM1-2-LR_1pctCO2_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-LR_1pctCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_MPI-ESM1-2-LR_1pctCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-LR_abrupt-4xCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    # mpi-esm1-2-hr
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-HR_piControl_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Oday_MPI-ESM1-2-HR_piControl_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-HR_piControl_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_MPI-ESM1-2-HR_piControl_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-HR_1pctCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    fpatterns <- "<fvarnames>_Oday_MPI-ESM1-2-HR_1pctCO2_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #fpatterns <- "<fvarnames>_Omon_MPI-ESM1-2-HR_1pctCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_SImon_MPI-ESM1-2-HR_1pctCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "<fvarnames>_Amon_MPI-ESM1-2-HR_abrupt-4xCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    #fpatterns <- "PI-CTRL_nodynveg2_echam6_echam_<YYYY><MM>.grb"
    #fpatterns <- "PI-CTRL_nodynveg2_fesom_<fvarnames>_<YYYY>0101.nc"
    #fpatterns <- "PI-CTRL6_echam6_echam_<YYYY><MM>.grb"
    #fpatterns <- "piControl_echam6_echam_<YYYY><MM>.grb"
    #fpatterns <- "piControl_echam6_echammon_<YYYY><MM>.grb"
    #fpatterns <- "piControl_echam6_aeroptmon_<YYYY><MM>.grb"
    #fpatterns <- "1percCO2_echam6_echam_<YYYY><MM>.grb"
    #fpatterns <- "1percCO2_echam6_echam_<YYYY><MM>.nc"
    #fpatterns <- "4CO2_echam6_echam_<YYYY><MM>.grb"
    #fpatterns <- "4CO2_echam6_echam_<YYYY><MM>.nc"
    #fpatterns <- "CMIP6_HIST_echam6_echam_<YYYY><MM>.grb"
    #prefixes <- "awi-cm-1-1-lr_piControl"
    #prefixes <- "awi-cm-1-1-lr_historical"
    #prefixes <- "awi-cm-1-1-lr_1percCO2"
    #prefixes <- "awi-cm-1-1-lr_4CO2"
    #prefixes <- "awi-esm-1-1-lr_piControl"
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
    prefixes <- "mpi-esm1-2-hr_1percCO2"
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
    #fvarnames <- "tosga"
    #fvarnames <- "tob"
    #fvarnames <- "sitemptop"
    #fvarnames <- "si"
    #fvarnames <- "siextentn"
    #fvarnames <- "siextents"
    fvarnames <- "omldamax" # mld by mix scheme
    #fvarnames <- "mlotst" # mld by sigma_theta
    mpiom_remap2lonlat_arg_list <- list(list(mpiom_model_grid="/pool/data/MPIOM/TP04/TP04s.nc",
                                             reg_res=c(nlon=1440, nlat=720),
                                             fout_rename_pattern=prefixes[1]))
    #froms <- 1842 # awi-cm/esm-1-1-lr piControl correct filestamp
    #froms <- 1850
    #froms <- 1855 # awi-cm/esm-1-1-lr piControl wrong filestamp
    #froms <- 1870
    #froms <- 1873
    froms <- 1910 # TCR average start
    #froms <- 1912 # awi-*-1-1-lr piControl: last 30 years: 1912:1941; last 100 years: 1842:1941 
    #froms <- 1942 # piControl-1942 = deck-1850 (correct filestamp)
    #froms <- 1955 # piControl-1955 = deck-1850 (wrong filestamp)
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
    tos <- 1930 # TCR average end
    #tos <- 1941 # awi-cm/esm-1-1-lr piControl correct filestamp
    #tos <- 1954 # awi-cm/esm-1-1-lr piControl wrong filestamp
    #tos <- 1999
    #tos <- 2014
    #tos <- 2099
    #tos <- 2091 # awi-cm/esm-1-1-lr piControl 2000 correct filestamp
    #tos <- 2099
    #tos <- 2104 # awi-cm/esm-1-1-lr piControl-2000 = deck_1999
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
    #modes <- "yseasmean" 
    #modes <- "fldmean"
    modes <- list("timmean_monmax"=c("timmean", "monmax"))
    # awi-esm-1-1-lr piControl monthly (1855-1954) -> (1842-1941)
    #new_date_list <- list(list(years=rep(1842:1941, e=12), nc_time_origin=1)) 
    # awi-esm-1-1-lr piControl monthly (1955-2104) -> (1942-2091)
    #new_date_list <- list(list(years=rep(1942:2091, e=12), nc_time_origin=1)) 

} else if (F) { # cold MH issue; hu/xiaoxu
    models <- "echam6"
    #models <- "fesom"
    #datapaths <- "/home/ollie/hyang/work/pi477/cpl_output/copy" # 2700 to 3249
    #fpatterns <- "MM_<YYYY>01.01_echam.nc"
    #datapaths <- "/home/ollie/hyang/work/mh477/cpl_output/copy" # 2623 to 2657
    #fpatterns <- "MM_<YYYY>01.01_echam.nc"
    #datapaths <- "/pf/a/a270064/work/esm-experiments/mh_new/outdata/echam" # 2624 to 3001
    #fpatterns <- "MMnew_echam6_echam_<YYYY>01.nc"
    #datapaths <- "/pf/a/a270064/work/esm-experiments/mh_cold/outdata/echam" # 3105 to 3166 
    #fpatterns <- "mh_cold_echam6_BOT_mm_<YYYY><MM>.nc" # 3105 to 3264 but strange and 3124 is missing
    #fpatterns <- "mh_cold_echam6_echammon_<YYYY><MM>.grb" # 3123 to 3266
    #datapaths <- "/work/ab0246/a270073/out/awi-esm-1-1-lr/mh_cold"
    #fpatterns <- "mh_cold_echam6_BOT_mm_temp2_<YYYY_from>-<YYYY_to>_with_time.nc"
    #fpatterns <- "mh_cold_echam6_BOT_mm_temp2_3105-3207.nc"
    #fpatterns <- "mh_cold_echam6_BOT_mm_temp2_3105-3207_with_time.nc"
    #datapaths <- "/pf/a/a270064/work/esm-experiments/mh_cold/outdata/fesom" 
    #fpatterns <- "mh_cold_fesom_tosga_<YYYY>0101.nc"
    #datapaths <- "/pf/a/a270064/work/esm-experiments/mh_cmip/outdata/echam" # mh branched from piControl: 1955-2105
    #fpatterns <- "mh_cmip_echam6_echammon_<YYYY><MM>.grb"
    datapaths <- "/work/ab0246/a270077/model_acid_tests/mat_0013/outdata/echam" # 1855-1968
    fpatterns <- "mat_0013_echam6_echammon_<YYYY><MM>.grb"
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
    #fpatterns <- "Jan1a_echam6_echamday_<YYYY><MM>.grb"
    #prefixes <- "awi-esm-1-1-lr_Jan1a_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1b/work"
    #fpatterns <- "Jan1b_<YYYY><MM>.01_echamday"
    #prefixes <- "awi-esm-1-1-lr_Jan1b_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1c/work"
    #fpatterns <- "Jan1c_<YYYY><MM>.01_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1c/outdata/echam"
    #fpatterns <- "Jan1c_echam6_echamday_<YYYY><MM>.grb"
    #prefixes <- "awi-esm-1-1-lr_Jan1c_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1d/work"
    #fpatterns <- "Jan1d_<YYYY><MM>.01_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1d/outdata/echam"
    #fpatterns <- "Jan1d_echam6_echamday_<YYYY><MM>.grb"
    #prefixes <- "awi-esm-1-1-lr_Jan1d_echamday"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1e_timelimit/work" # 1st try
    #fpatterns <- "Jan1e_<YYYY>01.01_g3bid.nc"
    #prefixes <- "awi-esm-1-1-lr_Jan1e_g3bid"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1e/work" # 2nd try
    #fpatterns <- "Jan1e_<YYYY>01.01_g3bid.nc"
    #prefixes <- "awi-esm-1-1-lr_Jan1e2_g3bid"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1f/outdata/echam"
    #fpatterns <- "Jan1f_echam6_g3bid_<YYYY>01.nc"
    #prefixes <- "awi-esm-1-1-lr_Jan1f_g3bid"
    #datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1g/outdata/echam"
    #fpatterns <- "Jan1g_echam6_g3bid_<YYYY>01.nc"
    #prefixes <- "awi-esm-1-1-lr_Jan1g_g3bid"
    datapaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/Jan1h/outdata/echam"
    fpatterns <- "Jan1h_echam6_g3bid_<YYYY>01.nc"
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
    #fpatterns <- "historical_echam6_echam3hr_<YYYY><MM>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_3hr"
    #fpatterns <- "historical_echam6_echam6hr_<YYYY><MM>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_6hr"
    #fpatterns <- "historical_echam6_echamday_<YYYY><MM>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_day"
    #fpatterns <- "historical_echam6_echammon_<YYYY><MM>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_mon"
    #fpatterns <- "historical_echam6_tdiagmon_<YYYY><MM>.grb"
    #prefixes <- "awi-cm-1-1-lr_historical_tdiagmon"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL4/restart/echam"
    #fpatterns <- "restart_PI-CTRL4_echam_<YYYY>1231.nc"
    #prefixes <- "awi-esm-1-1-lr_piControl_restart_echam"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL4/outdata/echam"
    #fpatterns <- "PI-CTRL4_echam6_sp6h_<YYYY>12.grb"
    #prefixes <- "awi-esm-1-1-lr_piControl_sp6h"
    datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/restart/echam"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/tar/restart/echam"
    fpatterns <- "restart_PI-CTRL6_echam_<YYYY>1231.nc"
    prefixes <- "awi-esm-1-1-lr_piControl_restart_echam"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/tar/outdata/echam"
    #fpatterns <- "PI-CTRL6_echam6_echamday_<YYYY>.grb"
    #prefixes <- "awi-esm-1-1-lr_piControl_echamday"
    #datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical/restart/echam"
    #fpatterns <- "restart_historical_echam_<YYYY>1231.nc"
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
    #fpatterns <- "test_Chris_echam6_echam_<YYYY><MM><DD>.grb"
    #fpatterns <- "test_Chris_echam6_g3b6h_<YYYY><MM><DD>.grb"
    #datapaths <- "/work/aa0238/a270118/Experiments/100y_HICE_LAMO/outdata/echam"
    #fpatterns <- "100y_HICE_LAMO_echam6_g3b6h_<YYYY><MM>.grb"
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
    fpatterns <- "PI_ctrl_4xCO2_awiesm-2.1_LR_<YYYY><MM>.01_g3bday"
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
    fpatterns <- "echam6_g3bid_<YYYY>01.nc"
    #fpatterns <- "echam6_g3bday_<YYYY>01.nc"
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
    #fpatterns <- "PI_LA04_cont02_echam6_echam_<YYYY><MM>.grb"
    #fpatterns <- "PI_LA04_cont02_echam6_echammon_<YYYY><MM>.grb"
    #fpatterns <- "PI_LA04_cont03_echam6_echam_<YYYY><MM>.grb"
    #fpatterns <- "PI_LA04_cont03_echam6_echam_<YYYY>.grb"
    #fpatterns <- "PI_LA04_cont03_echam6_echammon_<YYYY><MM>.grb"
    #fpatterns <- "PI_LA04_cont03_echam6_echammon_<YYYY>.grb"
    #fpatterns <- "PI_LA04_cont04_echam6_echammon_<YYYY><MM>.grb"
    #fpatterns <- "CMIP6_HIST_echam6_echamday_<YYYY><MM>.grb"
    fpatterns <- "CMIP6_HIST_echam6_echam_<YYYY><MM>.grb"
    #fpatterns <- "CMIP6_HIST_echam6_echammon_<YYYY><MM>.grb"
    #fpatterns <- "CMIP6_4CO2_echam6_echam_<YYYY><MM>.grb"
    #fpatterns <- "CMIP6_1percCO2_echam6_echam_<YYYY><MM>.grb"
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
    #fpatterns <- "Hol-Tx10_echam5_main_mm_<YYYY><MM>.nc"
    #fpatterns <- "Hol-T_echam5_main_mm_<YYYY><MM>.nc"
    #fpatterns <- "Hol-7_echam5_wiso_mm_<YYYY><MM>.nc"
    #fpatterns <- "Hol-Tx10_echam5_wiso_mm_<YYYY><MM>.nc"
    fpatterns <- "Hol-T_echam5_wiso_mm_<YYYY><MM>.nc"
    #fpatterns <- "Hol-T_echam5_wiso_link_<YYYY><MM>" # steffens links
    #fpatterns <- "TIMESER.<YYYY>0101_<YYYY>1231.ext.nc"
    #fpatterns <- "fort.75_fort_<YYYY>0101_<YYYY>1231.nc" # daily
    #fpatterns <- "fort.75_fort_<YYYY>0101_<YYYY>1231_monmean.nc" # monthly
    #fpatterns <- "fort.75.<YYYY>0101_<YYYY>1231_monmean" # monthly
    #fpatterns <- "Hol-Tx10_mpiom_<YYYY>0101_<YYYY>1231.grb"
    #fpatterns <- "Hol-T_mpiom_<YYYY>0101_<YYYY>1231.grb"
    #fpatterns <- "Hol-T_mpiom_<YYYY>0101_<YYYY>1231_select_code_2_remapcon2_r120x101.nc" # THO
    #fpatterns <- "Hol-T_mpiom_<YYYY>0101_<YYYY>1231_select_code_5_remapcon2_r120x101.nc" # SAO
    #fpatterns <- "Hol-7_mpiom_<YYYY>0101_<YYYY>1231_select_code_183_remapcon2_r120x101.nc" # zmld
    #fpatterns <- "Hol-Tx10_mpiom_<YYYY>0101_<YYYY>1231_select_code_183_remapcon2_r120x101.nc"
    #fpatterns <- "Hol-T_mpiom_<YYYY>0101_<YYYY>1231_select_code_183_remapcon2_r120x101.nc"
    #fpatterns <- "Hol-7_mpiom_<YYYY>0101_<YYYY>1231_select_code_15_remapcon2_r120x101.nc" # sicmom
    #fpatterns <- "Hol-Tx10_mpiom_<YYYY>0101_<YYYY>1231_select_code_15_remapcon2_r120x101.nc"
    #fpatterns <- "Hol-T_mpiom_<YYYY>0101_<YYYY>1231_select_code_15_remapcon2_r120x101.nc"
    #fpatterns <- "Hol-T_jsbach_veg_mm_<YYYY><MM>.grb" 
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
    fpatterns <- "NUDGING_ERA5_T127L95_echam6_<YYYY>.monmean.wiso.nc"
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
    fpatterns <- "TIMESER.<YYYY>0101_<YYYY>1231.ext.nc"
    prefixes <- "E280_280ppm_mpiom_timeser"
    fvarnames <- "c25_TMERCI3"
    modes <- "select"
    froms <- "2650"
    tos <- "2749"

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
    prefixes <- c("dynveg_noLUH", "dynveg_LUH")

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
        fpatterns <- c("hist_echam6_echam_<YYYY><MM>.nc",
                       "1percCO2_echam6_echam_<YYYY><MM>.nc",
                       "4CO2_echam6_echam_<YYYY><MM>.nc")
        prefixes <- paste0("awi-esm-1-1-lr_", c("historical", "1percCO2", "4CO2"), 
                           "_echam6_echam") 
    } else if (F) { # awi-esm-1-2-lr
        datapaths <- c("/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_HIST/outdata/echam",
                       "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_1percCO2/outdata/echam",
                       "/work/ba1066/a270124/esm-experiments/awicm_pism/CMIP6_4CO2/outdata/echam")
        fpatterns <- c("CMIP6_HIST_echam6_echam_<YYYY><MM>.grb",
                       "CMIP6_1percCO2_echam6_echam_<YYYY><MM>.grb",
                       "CMIP6_4CO2_echam6_echam_<YYYY><MM>.grb")
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
            fpatterns <- c("PI-CTRL_nodynveg2_echam6_echam_<YYYY><MM>.grb",
                           "historical_echam6_echam_<YYYY><MM>.grb",
                           "1percCO2_echam6_echam_<YYYY><MM>.grb",
                           "4CO2_echam6_echam_<YYYY><MM>.grb")
            prefixes <- paste0("awi-cm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"), 
                               "_echam6_echam") 
        } else if (F) { # temp2
            fpatterns <- c("PI-CTRL_nodynveg2_echam6_echammon_<YYYY><MM>.grb",
                           "historical_echam6_echammon_<YYYY><MM>.grb",
                           "1percCO2_echam6_echammon_<YYYY><MM>.grb",
                           "4CO2_echam6_echammon_<YYYY><MM>.grb")
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
            fpatterns <- c("piControl_echam6_echam_<YYYY><MM>.grb",
                           "hist_echam6_echam_<YYYY><MM>.nc",
                           "1percCO2_echam6_echam_<YYYY><MM>.nc",
                           "4CO2_echam6_echam_<YYYY><MM>.nc")
            prefixes <- paste0("awi-esm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"), 
                               "_echam6_echam") 
        } else if (F) { # temp2
            fpatterns <- c("piControl_echam6_echammon_<YYYY><MM>.grb",
                           "hist_echam6_echammon_<YYYY><MM>.nc",
                           "1percCO2_echam6_echammon_<YYYY><MM>.nc",
                           "4CO2_echam6_echammon_<YYYY><MM>.nc")
            prefixes <- paste0("awi-esm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"), 
                               "_echam6_echammon") 
        } else if (F) { # tau_aero_550
            fpatterns <- c("piControl_echam6_aeroptmon_<YYYY><MM>.grb",
                           "hist_echam6_echammon_<YYYY><MM>.nc",
                           "1percCO2_echam6_echammon_<YYYY><MM>.nc",
                           "4CO2_echam6_echammon_<YYYY><MM>.nc")
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
        fpatterns <- c("PI_LA04_cont02_echam6_echam_<YYYY><MM>.grb",
                       "CMIP6_HIST_echam6_echam_<YYYY><MM>.grb",
                       "CMIP6_1percCO2_echam6_echam_<YYYY><MM>.grb",
                       "CMIP6_4CO2_echam6_echam_<YYYY><MM>.grb")
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
}

# https://gitlab.awi.de/paleodyn/model-analysis/blob/master/previous_scripts/ANALYSIS_calc_wiso_echam5_monmean.sh
# /ace/user/paleo/utils.ace/cosmos-wiso/echam5/calc_wiso_monmean_d.cosmos-aso.sh
cdo_known_cmds <- list("psl"=list(cmd=c("<cdo> merge <aps> <geosp> <t>",
                                        "<cdo> sealevelpressure")),
                       #"toa_imbalance"=list(cmd="<cdo> -setname,toa_imbalance -add <srad0> <trad0>"),
                       "toa_imbalance"=list(cmd="<cdo> -setname,toa_imbalance -enssum <rsdt> -mulc,-1.0 <rsut> -mulc,-1.0 <rlut>"),
                       "quv_direction"=list(cmd=c("<cdo> -setname,quv_direction -divc,3.141593 -mulc,180 -atan2 <qv> <qu>",
                                                  "<nco_ncatted> -O -a long_name,quv_direction,o,c,\"direction of water vapor transport\"",
                                                  "<nco_ncatted> -O -a units,quv_direction,o,c,\"degree\"")),
                       "wisoaprt_d_post"=list(cmd=c("<cdo> -setname,wisoaprt_d -setcode,10 -mulc,1000. -subc,1. -div -div <wisoaprt> <aprt> <wiso_smow_files>",
                                                    "<nco_ncatted> -O -a long_name,wisoaprt_d,o,c,\"delta of total precipitation\"",
                                                    "<nco_ncatted> -O -a units,wisoaprt_d,o,c,\"o/oo\"")),
                       "wisoaprl_d_post"=list(cmd="<cdo> -setname,wisoaprl_d -setcode,13 -mulc,1000. -subc,1. -div -div <wisoaprl> <aprl> <wiso_smow_files>"),
                       "wisoaprc_d_post"=list(cmd="<cdo> -setname,wisoaprc_d -setcode,14 -mulc,1000. -subc,1. -div -div <wisoaprc> <aprc> <wiso_smow_files>"),
                       "wisoaprs_d_post"=list(cmd="<cdo> -setname,wisoaprs_d -setcode,15 -mulc,1000. -subc,1. -div -div <wisoaprs> <aprs> <wiso_smow_files>"),
                       "wisoevap_d_post"=list(cmd=c("<cdo> -setname,wisoevap_d -setcode,19 -mulc,1000. -subc,1. -div -div <wisoevap> <evap> <wiso_smow_files>",
                                                    "<nco_ncatted> -O -a long_name,wisoevap_d,o,c,\"delta of evaporation\"",
                                                    "<nco_ncatted> -O -a units,wisoevap_d,o,c,\"o/oo\"")),
                       "wisope_d_post"=list(cmd=c("<cdo> -setname,wisope_d -setcode,20 -mulc,1000. -subc,1. -div -div <wisope> <pe> <wiso_smow_files>",
                                                  "<nco_ncatted> -O -a long_name,wisope_d,o,c,\"delta of precip minus evap\"",
                                                  "<nco_ncatted> -O -a units,wisope_d,o,c,\"o/oo\"")),
                       "wisows_d_post"=list(cmd="<cdo> -setname,wisows_d -setcode,11 -mulc,1000. -subc,1. -div -div <wisows> <ws> <wiso_smow_files>"),
                       "wisosn_d_post"=list(cmd="<cdo> -setname,wisosn_d -setcode,12 -mulc,1000. -subc,1. -div -div <wisosn> <sn> <wiso_smow_files>"),
                       "wisosnglac_d_post"=list(cmd="<cdo> -setname,wisoasnglac_d -setcode,33 -mulc,1000. -subc,1. -div -div <wisosnglac> <snglac> <wiso_smow_files>"),
                       "wisorunoff_d_post"=list(cmd="<cdo> -setname,wisorunoff_d -setcode,17 -mulc,1000. -subc,1. -div -div <wisorunoff> <runoff> <wiso_smow_files>"),
                       "aprt_times_temp2"=list(cmd=c("<cdo> -setname,aprt_times_temp2 -mul <aprt> <temp2>",
                                                     "<nco_ncatted> -O -a code,aprt_times_temp2,d,,", # delete old `code` attribute
                                                     "<nco_ncatted> -O -a table,aprt_times_temp2,d,,", # delete old `table` attribute
                                                     "<nco_ncatted> -O -a long_name,aprt_times_temp2,o,c,\"aprt times temp2\"",
                                                     "<nco_ncatted> -O -a units,aprt_times_temp2,o,c,\"mm/month degC\"")),
                       "aprt_times_tsurf"=list(cmd=c("<cdo> -setname,aprt_times_tsurf -mul <aprt> <tsurf>",
                                                     "<nco_ncatted> -O -a code,aprt_times_tsurf,d,,",
                                                     "<nco_ncatted> -O -a table,aprt_times_tsurf,d,,",
                                                     "<nco_ncatted> -O -a long_name,aprt_times_tsurf,o,c,\"aprt times tsurf\"",
                                                     "<nco_ncatted> -O -a units,aprt_times_tsurf,o,c,\"mm/month degC\"")),
                       "temp2aprt"=list(cmd=c("<cdo> -setname,temp2aprt -div <aprt_times_temp2> <aprt>",
                                              "<nco_ncatted> -O -a code,temp2aprt,d,,",
                                              "<nco_ncatted> -O -a table,temp2aprt,d,,",
                                              "<nco_ncatted> -O -a long_name,temp2aprt,o,c,\"temp2 weighted by aprt\"",
                                              "<nco_ncatted> -O -a units,temp2aprt,o,c,\"degC\"")),
                       "tsurfaprt"=list(cmd=c("<cdo> -setname,tsurfaprt -div <aprt_times_tsurf> <aprt>",
                                              "<nco_ncatted> -O -a code,tsurfaprt,d,,",
                                              "<nco_ncatted> -O -a table,tsurfaprt,d,,",
                                              "<nco_ncatted> -O -a long_name,tsurfaprt,o,c,\"tsurf weighted by aprt\"",
                                              "<nco_ncatted> -O -a units,tsurfaprt,o,c,\"degC\""))
                      ) # cdo_known_cmds

