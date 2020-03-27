# r

source("../helper_functions.r")
source("mpiom_functions.r")

# remap mpiom to regular grid
if (F) {
    if (T) { # Hol-Tx10 on paleosrv
        files <- list.files("/scratch/simulation_database/incoming/Hol-Tx10/output",
                            pattern=glob2rx("Hol-Tx10_mpiom_*.grb"), full.names=T)
        outpath <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/mpiom"
    } else if (F) { # Hol-T chunk 1 & 2 from paul on stan
        files <- list.files("/ace/user/pgierz/cosmos-aso-wiso/Hol-T/outdata_finished/mpiom",
                            pattern=glob2rx("Hol-T_mpiom_*.grb"), full.names=T)
        # from 4407 to 6699 
        #files <- files[3608:length(files)]
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom_of_paul"
    } else if (F) { # Hol-T2 on stan
        files <- list.files("/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom", 
                            pattern=glob2rx("Hol-T2_mpiom_*.grb"), full.names=T)
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom"
    }
    mpiom_remap2lonlat(files=files, 
                       #cdo_select="select,code=183", # zmld
                       #cdo_select="select,code=27", # PSIUWE hor. bar. streamfunction
                       cdo_select="select,code=15", # SICOMO ice compactness
                       outpath=outpath) 
}

# extract mpiom tar files
if (F) {
    if (T) { # Hol-Tx10 on paleosrv
        fort_tar_files <- list.files("/scratch/simulation_database/incoming/Hol-Tx10/output", 
                                     pattern=glob2rx("*.tar"), full.names=T)  
        outpath <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/mpiom"
    } else if (F) { # Hol-T on stan
        fort_tar_files <- list.files("/ace/user/pgierz/cosmos-aso-wiso/Hol-T/outdata_finished/mpiom", 
                                     pattern=glob2rx("*.tar"), full.names=T)  
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom_of_paul" 
    } else if (T) { # Hol-T2 on stan
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
                                partab_ext="~/scripts/r/echam/mpiom/zeitser-wiso.partab")
}

# temporal mean of daily fort.75 files
if (T) {
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

message("\nfinished\n")

