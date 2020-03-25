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

verbose <- 1 # 0,1
clean <- F # remove tmp files
cdo_silent <- "" # "-s" for silent or ""
cdo_force <- T # redo cdo command although outout file already exists 
cdo_OpenMP_threads <- "-P 4" # "-P n" or "" (will be irgnored on commands that do not support OMP)
cdo_set_rel_time <- T # conversion from absolute to relative time
cdo_run_from_script <- T # create temporary file and run long cdo command from there
#cdo_nchar_max_arglist <- 2612710 # this worked but not always dont know why
cdo_nchar_max_arglist <- 2612000
nco_nchar_max_arglist <- 131071
# --> $(getconf PAGE_SIZE)*32 = 4096*32 = 131072
# --> getconf ARG_MAX                   = 2097152

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
    
} else if (F) { # pi
    datapaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/echam" # 1543:1941
    if (F) {
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
    prefixes <- "awi-esm-1-1-lr"

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
    prefixes <- "awi-esm-1-1-lr_lgm"

} else if (F) { # Hol-Tx10 on paleosrv
    #datapaths <- "/scratch/simulation_database/incoming/Hol-Tx10/output"
    datapaths <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/echam5"
    #datapaths <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/mpiom"
    models <- "echam5"
    #models <- "mpiom1"
    fpatterns <- "Hol-Tx10_echam5_main_mm_<YYYY><MM>.nc"
    #fpatterns <- "Hol-Tx10_echam5_wiso_mm_<YYYY><MM>.nc"
    #fpatterns <- "TIMESER.<YYYY>0101_<YYYY>1231.ext.nc"
    #fpatterns <- "fort.75_fort_<YYYY>0101_<YYYY>1231.nc"
    #fpatterns <- "Hol-Tx10_mpiom_<YYYY>0101_<YYYY>1231_select_code_183_remapcon2_r120x101.nc"
    prefixes <- "cosmos-aso-wiso_echam5_Hol-Tx10_main_mm"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm"
    #prefixes <- "cosmos-aso-wiso_mpiom1_Hol-Tx10_timeser_ext"
    #prefixes <- "cosmos-aso-wiso_mpiom1_Hol-Tx10_fort_75"
    #prefixes <- "cosmos-aso-wiso_mpiom1_Hol-Tx10_grb_code_183_remapcon2_r120x101"
    #fvarnames <- "temp2"
    #fvarnames <- "tsurf"
    fvarnames <- "wind10"
    #fvarnames <- "srad0"
    #fvarnames <- "srad0d"
    #fvarnames <- "aprt"
    #fvarnames <- "albedo"
    #fvarnames <- "wisoaprt"
    #fvarnames <- "wisoaprt_d"
    #levs_out <- 2
    #fvarnames <- "aprt_times_temp2"
    #fvarnames <- "aprt_times_tsurf"
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
    #fvarnames <- "zmld"
    #areas_out_list <- list(list(name="NA45to90N",
    #                            sellonlatbox=c(lon1=250,lon2=45,lat1=45,lat2=90)))
    #areas_out_list <- list(list(name="weddelmld",
    #                            sellonlatbox=c(lon1=300,lon2=18,lat1=-81,lat2=-57.6)))
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
    modes <- "select"
    #modes <- "timmean"
    #modes <- "yearmean"
    #modes <- "monmean"
    #modes <- "ymonmean"
    #modes <- "fldmean"
    #modes <- "yearsum"
    #modes <- "timsum"
    #modes <- "zonmean"
    froms <- "0001" # Hol-Tx10 links: beginning counting from 1
    #froms <- "2901" # Hol-Tx10 raw: beginning
    #froms <- "3572"
    #tos <- "2910"
    #tos <- "3601" # Hol-Tx10 raw: end
    tos <- "7001" # Hol-Tx10 links: end counting from 1 
    if (modes[1] == "timmean") {
        if (froms[1] == "2901" && tos[1] == "3601") {
            new_date_list <- list(list(years=mean(c(1, 7001)), nc_time_origin=1))
        } else {
            stop("asd")
        }
    } else if (modes[1] != "timmean") {
        if (T) { # for links with correct years in filenames: 
            new_date_list <- list(list(use="filename", year_origin=1, nc_time_origin=1))
        } else if (F) { # for files with wrong years in filenames:
            # monthly:
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
            if (grepl("_remapcon2_", fpatterns[1])) {
                # 11 missing mpiom *.grb * files: 3028, 3065, 3153, 3162, 3165, 3316, 3331, 3334, 3348, 3368, 3498
                if (any(new_date_list[[1]]$years == 4471)) {
                    missy <- c(3028, 3065, 3153, 3162, 3165, 3316, 3331, 3334, 3348, 3368, 3498)
                    message("remove missing years ", paste(missy, collapse=", "))
                    rminds <- c()
                    for (y in missy) {
                        tmp <- which(new_date_list[[1]]$years == (length(2901:y)-1)*10+1)
                        #message((length(2901:y)-1)*10+1, ": ", paste(tmp, collapse=","))
                        rminds <- c(rminds, tmp)
                    }
                    new_date_list[[1]]$years <- new_date_list[[1]]$years[-rminds]
                }
            }
        }
    } # new time depending on output frequency
    wiso_smow_files <- "~/scripts/r/echam/wiso/SMOW.FAC.T31.nc"
    cdo_codetables <- "~/scripts/r/echam/wiso/CODES.WISO"
    cdo_partablesn <- "~/scripts/r/echam/wiso/CODES.WISO.txt"

} else if (T) { # Hol-T on stan
    datapaths <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/echam5"
    #datapaths <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom"
    models <- "echam5"
    #models <- "mpiom1"
    fpatterns <- "Hol-T_echam5_main_mm_<YYYY><MM>.nc"
    #fpatterns <- "Hol-T_echam5_wiso_mm_<YYYY><MM>.nc"
    #fpatterns <- "TIMESER.<YYYY>0101_<YYYY>1231.ext.nc"
    #fpatterns <- "fort.75_fort_<YYYY>0101_<YYYY>1231.nc" # daily
    #fpatterns <- "fort.75_fort_<YYYY>0101_<YYYY>1231_monmean.nc" # daily
    #fpatterns <- "Hol-T_mpiom_<YYYY>0101_<YYYY>1231_select_code_183_remapcon2_r120x101.nc"
    prefixes <- "cosmos-aso-wiso_echam5_Hol-T_main_mm"
    #prefixes <- "cosmos-aso-wiso_echam5_Hol-T_wiso_mm"
    #prefixes <- "cosmos-aso-wiso_mpiom1_Hol-T_timeser_ext"
    #prefixes <- "cosmos-aso-wiso_mpiom1_Hol-T_fort_75_monmean"
    #prefixes <- "cosmos-aso-wiso_mpiom1_Hol-T_grb_code_183_remapcon2_r120x101"
    #fvarnames <- "temp2"
    #fvarnames <- "tsurf"
    fvarnames <- "wind10"
    #fvarnames <- "srad0"
    #fvarnames <- "srad0d"
    #fvarnames <- "trad0"
    #fvarnames <- "aprt"
    #fvarnames <- "albedo"
    #fvarnames <- "wisoaprt"
    #fvarnames <- "wisoaprt_d"
    #levs_out <- 2
    #fvarnames <- "aprt_times_temp2"
    #fvarnames <- "aprt_times_tsurf"
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
    #fvarnames <- "zmld"
    #areas_out_list <- list(list(name="NA45to90N",
    #                            sellonlatbox=c(lon1=250,lon2=45,lat1=45,lat2=90)))
    #areas_out_list <- list(list(name="weddelmld",
    #                            sellonlatbox=c(lon1=300,lon2=18,lat1=-81,lat2=-57.6)))
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
    modes <- "select"
    #modes <- "timmean"
    #modes <- "yearmean"
    #modes <- "monmean"
    #modes <- "ymonmean"
    #modes <- "fldmean"
    #modes <- "yearsum"
    #modes <- "timsum"
    #modes <- "zonmean"
    froms <- "0004" # Hol-T links: beginning of chunk 1
    #froms <- "0100"
    #tos <- "0013" 
    #tos <- "0011"
    #tos <- "0129"
    #tos <- "5903" # end of chunk 2
    #tos <- "6821"
    tos <- "7000" # Hol-T links: end of chunk 3
    new_date_list <- list(list(use="filename", year_origin=1, nc_time_origin=1))
    wiso_smow_files <- "~/scripts/r/echam/wiso/SMOW.FAC.T31.nc"
    cdo_codetables <- "~/scripts/r/echam/wiso/CODES.WISO"
    cdo_partablesn <- "~/scripts/r/echam/wiso/CODES.WISO.txt"

} else if (F) { # echam5-wiso; T106L31; ERA-Interim nudging; stan
    datapaths <- "/ace/user/mwerner/echam5-wiso/T106L31/EXP007_MB/MONMEAN"
    models <- "echam5"
    fpatterns <- "EXP007_T106_MB_195801.201312.combined.monmean.wiso.nc"
    prefixes <- "recT106erai_echam5"
    fvarnames <- "temp2"
    modes <- "select"
    #modes <- "fldmean"
    froms <- 1958
    tos <- 2013

} else if (F) { # echam6-wiso; T127L95; ERA-5 nudging; ollie
    datapaths <- "/work/ollie/mwerner/echam6-wiso/T127L95/NUDGING_ERA5_T127L95/MONMEAN"
    models <- "echam6"
    fpatterns <- "NUDGING_ERA5_T127L95_echam6_<YYYY>.monmean.wiso.nc"
    prefixes <- "recT127era5_echam6"
    fvarnames <- "temp2"
    #modes <- "select"
    modes <- "fldmean"
    froms <- 1979
    tos <- 2018

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
    prefixes <- rep("awi-esm-1-1-lr", t=3)

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
    } else if (F) {
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
    prefixes <- rep("awi-esm-1-1-lr", t=4)
}

# https://gitlab.awi.de/paleodyn/model-analysis/blob/master/previous_scripts/ANALYSIS_calc_wiso_echam5_monmean.sh
# /ace/user/paleo/utils.ace/cosmos-wiso/echam5/calc_wiso_monmean_d.cosmos-aso.sh
cdo_known_cmds <- list("toa_imbalace="=list(cmd="<cdo> -setname,toa_imbalance -add <srad0> <trad0>"),
                       "wisoaprt_d"=list(cmd=c("<cdo> -setname,wisoaprt_d -setcode,10 -mulc,1000. -subc,1. -div -div <wisoaprt> <aprt> <wiso_smow_files>")),#)#,
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
                                                     "<nco_ncatted> -O -a long_name,aprt_times_temp2,o,c,\"aprt times temp2\"",
                                                     "<nco_ncatted> -O -a units,aprt_times_temp2,o,c,\"mm/month degC\"")),
                       #"aprt_times_temp2"=list(cmd=c("<cdo> -setname,aprt_times_temp2 -<modes> -mul <aprt> <temp2>", # todo: mode & mul together?
                       #                              "<nco_ncatted> -O -a code,aprt_times_temp2,d,,")), # delete old code
                       "aprt_times_tsurf"=list(cmd=c("<cdo> -setname,aprt_times_tsurf -mul <aprt> <tsurf>",
                                                     "<nco_ncatted> -O -a code,aprt_times_tsurf,d,,", # delete old code
                                                     "<nco_ncatted> -O -a long_name,aprt_times_tsurf,o,c,\"aprt times tsurf\"",
                                                     "<nco_ncatted> -O -a units,aprt_times_tsurf,o,c,\"mm/month degC\"")),
                       "ptemp"=list(cmd=c("<cdo> -setname,ptemp -setcode,170 -div <aprt_times_temp2> <aprt>",
                                          "<nco_ncatted> -O -a long_name,ptemp,o,c,\"precipitation weighted temp2\"",
                                          "<nco_ncatted> -O -a units,ptemp,o,c,\"degC\"")),
                       "ptsurf"=list(cmd=c("<cdo> -setname,ptsurf -div <aprt_times_tsurf> <aprt>",
                                           "<nco_ncatted> -O -a long_name,ptsurf,o,c,\"precipitation weighted tsurf\"",
                                           "<nco_ncatted> -O -a units,ptsurf,o,c,\"degC\""))
                      ) # cdo_known_cmds

