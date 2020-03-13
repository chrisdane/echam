# r

source("mpiom_functions.r")

# extract mpiom tar files
if (F) {
    fort_tar_files <- list.files("/scratch/simulation_database/incoming/Hol-Tx10/output", pattern=glob2rx("*.tar"), full.names=T)  
    mpiom_extract_fort_tar_data(fort_tar_files, 
                                outpath="/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-Tx10/outdata/mpiom", 
                                partabn_ext="/home/csys/cdanek/scripts/r/echam/mpiom/mpiom_wiso_zeitser_ext_partabn_corrected.txt")
}

# ext to nc with partab
if (T) {
    ext_files <- list.files("/scratch/simulation_database/incoming/E280_280ppm/output", pattern=glob2rx("*.ext"), full.names=T)
    mpiom_ext_to_nc(ext_files,
                    outpath="/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/E280_280ppm/outdata/mpiom",
                    partabn_ext="/home/csys/cdanek/scripts/r/echam/mpiom/mpiom_wiso_zeitser_ext_partabn_corrected.txt")
}


