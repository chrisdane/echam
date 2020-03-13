# r

source("mpiom_functions.r")

# extract mpiom tar files
if (T) {
    if (F) { # Hol-Tx10 on paleosrv
        fort_tar_files <- list.files("/scratch/simulation_database/incoming/Hol-Tx10/output", pattern=glob2rx("*.tar"), full.names=T)  
        outpath <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/mpiom"
    } else if (F) { # Hol-T on stan
        fort_tar_files <- list.files("/ace/user/pgierz/cosmos-aso-wiso/Hol-T/outdata_finished/mpiom", pattern=glob2rx("*.tar"), full.names=T)  
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom" 
    } else if (T) { # Hol-T2 on stan
        fort_tar_files <- list.files("/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom", pattern=glob2rx("*.tar"), full.names=T)  
        outpath <- "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T2/outdata/mpiom" 
    }
    mpiom_extract_fort_tar_data(fort_tar_files, outpath=outpath, 
                                partab_ext="~/scripts/r/echam/mpiom/zeitser-wiso.partab")
}

# ext to nc with partab
if (F) {
    if (T) {
        ext_files <- list.files("/scratch/simulation_database/incoming/E280_280ppm/output", pattern=glob2rx("*.ext"), full.names=T)
        outpath <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/E280_280ppm/outdata/mpiom"
    } else if (F) {
    }
    mpiom_ext_to_nc(ext_files=ext_files, outpath=outpath,
                    partab_ext="~/scripts/r/echam/mpiom/zeitser-wiso.partab")
}


