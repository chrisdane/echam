# mpiom section of echam repo

source("../helper_functions.r")
source("mpiom_functions.r")

# remap mpiom to regular grid
if (T) {
    if (F) { # Hol-7 on stan
        files <- list.files("/ace/user/pgierz/cosmos-aso-wiso/Hol-7/outdata/mpiom",
                            pattern=glob2rx("Hol-7_mpiom_*.grb"), full.names=T)
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-7/outdata/mpiom"
        mpiom_model_grid <- "GR30s.nc"
        fout_rename_variable_pattern <- NULL
        stop("implement fout_rename_pattern")
    } else if (F) { # Hol-Tx10 on paleosrv
        files <- list.files("/scratch/simulation_database/incoming/Hol-Tx10/output",
                            pattern=glob2rx("Hol-Tx10_mpiom_*.grb"), full.names=T)
        outpath <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/mpiom"
        mpiom_model_grid <- "GR30s.nc"
        fout_rename_variable_pattern <- NULL
        stop("implement fout_rename_pattern")
    } else if (F) { # Hol-T chunk 1 & 2 from paul on stan
        files <- list.files("/ace/user/pgierz/cosmos-aso-wiso/Hol-T/outdata_finished/mpiom",
                            pattern=glob2rx("Hol-T_mpiom_*.grb"), full.names=T)
        # from 4407 to 6699 
        #files <- files[3608:length(files)]
        # from 3884 to 3913: 6k mean
        #files <- files[3085:3114]
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom_of_paul"
        mpiom_model_grid <- "GR30s.nc"
        fout_rename_variable_pattern <- NULL
        stop("implement fout_rename_pattern")
    } else if (F) { # Hol-T2 (chunk 3) on stan
        files <- list.files("/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom", 
                            pattern=glob2rx("Hol-T2_mpiom_*.grb"), full.names=T)
        # from 3970 to 3999: PI mean
        #files <- files[1068:1097]
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom"
        mpiom_model_grid <- "GR30s.nc"
        fout_rename_variable_pattern <- NULL
        stop("implement fout_rename_pattern")
    } else if (F) { # already post-processed files
        #files <- "/isibhv/projects/paleo_work/cdanek/post/mpiom1/select/lm_THO_as_time_slope/cosmos-aso-wiso_Hol-Tx10_grb_mpiom1_select_selcode_2_lm_THO_as_time_slope_sellevel_6_global_annual_0001-7001.nc"
        #files <- paste0("/isibhv/projects/paleo_work/cdanek/post/mpiom1/seasmean/lm_THO_as_time_slope/cosmos-aso-wiso_Hol-Tx10_grb_mpiom1_seasmean_selcode_2_lm_THO_as_time_slope_sellevel_6_global_", c("DJF", "MAM", "JJA", "SON"), "_0001-7001.nc")
        #files <- "/ace/user/cdanek/post/mpiom1/select/lm_THO_as_time/cosmos-aso-wiso_Hol-T_grb_mpiom1_select_selcode_2_lm_THO_as_time_slope_sellevel_6_global_annual_0004-7000.nc"
        #files <- paste0("/ace/user/cdanek/post/mpiom1/seasmean/lm_THO_as_time_slope/cosmos-aso-wiso_Hol-T_grb_mpiom1_seasmean_selcode_2_lm_THO_as_time_slope_sellevel_6_global_", c("DJF", "MAM", "JJA", "SON"), "_0004-7000.nc")
        files <- paste0("/isibhv/projects/paleo_work/cdanek/post/mpiom1/select/lm_SICOMO_as_time_slope/cosmos-aso-wiso_Hol-T_grb_mpiom1_select_selcode_15_lm_SICOMO_as_time_slope_global_", c("annual", "Mar", "Sep"), "_0004-7000.nc")
        outpath <- dirname(files[1])
        mpiom_model_grid <- "GR30s.nc"
        #fout_rename_pattern <- "cosmos-aso-wiso_Hol-Tx10" 
        fout_rename_pattern <- "cosmos-aso-wiso_Hol-T"
        fout_rename_variable_pattern <- NULL
    } else if (T) { # mpi-esm lr cmip6
        files <- list(u="/work/ab0246/a270073/post/mpiom1/timmean/uo/mpi-esm1-2-lr_1percCO2_mpiom1_timmean_uo_sellevel_6_global_Jan-Dec_1910-1930.nc",
                      v="/work/ab0246/a270073/post/mpiom1/timmean/vo/mpi-esm1-2-lr_1percCO2_mpiom1_timmean_vo_sellevel_6_global_Jan-Dec_1910-1930.nc")
        mpiom_model_grid <- NULL
        outpath <- "/work/ab0246/a270073/post/mpiom1/timmean/uvo"
        fout_rename_pattern <- "mpi-esm1-2-lr"
        fout_rename_variable_pattern <- c("in"="_uo_", "out"="_uvo_")
    } else if (F) { # mld
        #files <- "/work/ab0246/a270073/post/mpiom1/monmax/omldamax/mpi-esm1-2-lr_piControl_mpiom1_monmax_omldamax_global_Jan-Dec_1850-1853.nc"
        #files <- "/work/ab0246/a270073/post/mpiom1/monmax/omldamax/mpi-esm1-2-lr_piControl_mpiom1_monmax_omldamax_global_Jan-Dec_1850-2000.nc"
        files <- "/work/ab0246/a270073/post/mpiom1/monmax/omldamax/mpi-esm1-2-lr_1percCO2_mpiom1_monmax_omldamax_global_Jan-Dec_1850-2000.nc"
        mpiom_model_grid <- NULL
        outpath <- NULL
        #fout_rename_pattern <- "mpi-esm1-2-lr_piControl"
        fout_rename_pattern <- "mpi-esm1-2-lr_1percCO2"
        fout_rename_variable_pattern <- NULL
    }
    # files=             outpath=           reg_res=           cdo=      convert2nc=            
    # cdo_select=        mpiom_model_grid=  remap_method=      verbose=     
    mpiom_remap2lonlat(files=files, 
                       mpiom_model_grid=mpiom_model_grid, # only needed if input is non-nc
                       #cdo_select="-select,code=2", # THO
                       #cdo_select="-select,code=5", # SAO
                       #cdo_select="-select,code=183", # zmld
                       #cdo_select="-select,code=27", # PSIUWE hor. bar. streamfunction
                       #cdo_select="-select,code=15", # SICOMO ice compactness
                       #reg_res=c(nlon=360, nlat=180), # 1°
                       reg_res=c(nlon=1440, nlat=720), # 0.25°
                       #reg_res=c(nlon=3600, nlat=1800), # 0.1°
                       outpath=outpath,
                       fout_rename_pattern=fout_rename_pattern,
                       fout_rename_variable_pattern=fout_rename_variable_pattern # only needed for vector variables
                       ) 
}

# extract mpiom tar files
if (F) {
    if (T) { # Hol-7 on stan
        fort_tar_files <- list.files("/ace/user/pgierz/cosmos-aso-wiso/Hol-7/outdata/mpiom", 
                                     pattern=glob2rx("*.tar"), full.names=T)  
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-7/outdata/mpiom" 
    } else if (F) { # Hol-Tx10 on paleosrv
        fort_tar_files <- list.files("/scratch/simulation_database/incoming/Hol-Tx10/output", 
                                     pattern=glob2rx("*.tar"), full.names=T)  
        outpath <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/mpiom"
    } else if (F) { # Hol-T on stan
        fort_tar_files <- list.files("/ace/user/pgierz/cosmos-aso-wiso/Hol-T/outdata_finished/mpiom", 
                                     pattern=glob2rx("*.tar"), full.names=T)  
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom_of_paul" 
    } else if (F) { # Hol-T2 on stan
        #fort_tar_files <- list.files("/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom", pattern=glob2rx("*.tar"), full.names=T)  
        fort_tar_files <- c(#"/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom/fort_37390101_37391231.tar"
                            #"/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom/fort_37180101_37181231.tar",
                            #"/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom/fort_37170101_37171231.tar",
                            #"/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom/fort_36960101_36961231.tar",
                            #"/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom/fort_38470101_38471231.tar",
                            #"/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom/fort_38670101_38671231.tar"
                            "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom/fort_38150101_38151231.tar",
                            "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom/fort_39930101_39931231.tar"
                           )
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom" 
    }
    mpiom_extract_fort_tar_data(fort_tar_files, outpath=outpath, 
                                partab_ext="~/scripts/r/echam/mpiom/zeitser-wiso.partab",
                                keep_ext=T, keep_fort.75=F, keep_fort.90=F,
                                verbose=T)
}

# temporal mean of daily fort.75 files
if (F) {
    if (F) { # Hol-Tx10 on paleosrv

    } else if (F) { # Hol-T on stan
        fort.75_files <- list.files("/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom", 
                                    pattern=glob2rx("fort.75_fort_*.nc"), full.names=T)
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom"
    } else if (T) { # Hol-7 on stan
        fort.75_files <- list.files("/ace/user/pgierz/cosmos-aso-wiso/Hol-7/outdata/mpiom", 
                                    pattern=glob2rx("fort.75*"), full.names=T)
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-7/outdata/mpiom" 
    }
    mpiom_fort.75_temporal_mean(fort.75_files=fort.75_files, outpath=outpath,
                                cdo_temporal_mean="monmean") 
}


# ext to nc with partab
if (F) {
    if (T) {
        ext_files <- list.files("/scratch/simulation_database/incoming/E280_280ppm/output", 
                                pattern=glob2rx("*.ext"), full.names=T)
        outpath <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/E280_280ppm/outdata/mpiom"
    } else if (F) {
    }
    mpiom_ext_to_nc(ext_files=ext_files, outpath=outpath,
                    partab_ext="~/scripts/r/echam/mpiom/zeitser-wiso.partab")
}

# get land-sea-mask outline segments
if (F) {
    r <- mpiom_get_lsm_segments(f_data="mpiom_GR30_curvilinear_120x101_data.nc", 
                                f_grid="GR30s.nc")
}

message("\nfinished\n")

