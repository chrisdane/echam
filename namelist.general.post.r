# r

# input for post_echam.r

message("###################### namelist.general.post.r start ##########################")

graphics.off()
options(show.error.locations=T)
options(warn=2) # stop on warnings
#options(warn=0) # back to default

# clear work space
if (T) {
    message("\nclear work space ...")
    ws <- ls()
    ws <- ws[-which(ws == "repopath")]
    rm(list=ws)
}

# load helper functions of this repo
script_helper_functions <- paste0(repopath, "/helper_functions.r")
message("\nload `repopath`/helper_functions.r = ", script_helper_functions, " ...")
source(script_helper_functions) # get_host()

# get host options
host <- get_host()
host$repopath <- repopath

# load functions from submodule
message("\nload functions from submodule dir ", host$repopath, "/functions\" ...")
# needed myfunctions.r functions: 
# ht(), is.leap(), identical_list(), make_posixlt_origin(), ncdump_get_filetype()  
for (i in c("myfunctions.r")) source(paste0(host$homepath, "/functions/", i))

# general options
verbose <- 1 # 0,1
post_force <- F # redo calculation although output file already exists 
clean <- T # remove temporary files

# cdo options
cdo_silent <- "" # "-s" for silent or ""
cdo_select_no_history <- "" # "--no_history" or ""
cdo_convert_grb2nc <- T # should post processing result be converted to nc (will be set to T if new dates are wanted)?
cdo_OpenMP_threads <- paste0("-P ", system("nproc", intern=T)) # "-P n" or "" (will be irgnored on commands that do not support OMP)
cdo_set_rel_time <- T # conversion from absolute to relative time
cdo_run_from_script <- T # create temporary file and run long cdo command from there
# maximum number of args cdo
# stan0/1: getconf ARG_MAX 2621440
# paleosrv1: getconf ARG_MAX 2097152
cdo_nchar_max_arglist <- 2350000 # reduce this number if you get segmentation fault on the cdo selection command (many files)

# nco options
# maximum number of args nco
# $(getconf PAGE_SIZE)*32 = 4096*32 = 131072
nco_nchar_max_arglist <- 131071

# model specific general options
mpiom1_remap <- T

# known dimnames; add further
# so far only time needed
known_dimnames <- list(time=c("time", "Time", "TIME", "time_mon", "T", "t"))

# cdo commands for some variables
cdo_known_cmds <- list(
   "psl"=list(cmd=c("<cdo> merge <aps> <geosp> <t>",
                    "<cdo> sealevelpressure")),
   "hvel"=list(cmd=c("<cdo> expr,'hvel=sqrt(uo*uo + vo*vo)' <uvo>")),
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
                          "<nco_ncatted> -O -a units,tsurfaprt,o,c,\"degC\"")),
   "fgco2"=list(cmd=c("<cdo> -setname,fgco2 -mulc,-0.272912 <co2_flx_ocean>", 
                      # into atm --> into ocean; kgCO2 --> kgC
                      "<nco_ncatted> -O -a code,fgco2,d,,",
                      "<nco_ncatted> -O -a table,fgco2,d,,",
                      "<nco_ncatted> -O -a long_name,fgco2,o,c,\"Surface Downward Flux of Total CO2 [kgC m-2 s-1]\"")),
   "nbp"=list(cmd=c("<cdo> -setname,nbp -mulc,-0.272912 -enssum <co2_flx_land> <co2_flx_lcc> <co2_flx_harvest>", 
                    # into atm --> into land; kgCO2 --> kgC; nbp = netAtmosLandCO2Flux
                    "<nco_ncatted> -O -a code,nbp,d,,",
                    "<nco_ncatted> -O -a table,nbp,d,,",
                    "<nco_ncatted> -O -a long_name,nbp,o,c,\"Carbon Mass Flux out of Atmosphere Due to Net Biospheric Production on Land [kgC m-2 s-1]\"")),
   "netAtmosLandCO2Flux"=list(cmd=c("<cdo> -setname,netAtmosLandCO2Flux -mulc,-0.272912 -enssum <co2_flx_land> <co2_flx_lcc> <co2_flx_harvest>", 
                                    # into atm --> into land; kgCO2 --> kgC; netAtmosLandCO2Flux = nbp
                                    "<nco_ncatted> -O -a code,netAtmosLandCO2Flux,d,,",
                                    "<nco_ncatted> -O -a table,netAtmosLandCO2Flux,d,,",
                                    paste0("<nco_ncatted> -O -a long_name,netAtmosLandCO2Flux,o,c,\"Net flux of CO2 between atmosphere and ",
                                           "land (positive into land) as a result of all processes [kgC m-2 s-1]\""))),
   "co2_flx_total"=list(cmd=c("<cdo> -setname,co2_flx_total -add <fgco2> <nbp>",
                              paste0("<nco_ncatted> -O -a long_name,co2_flx_total,o,c,\"Total CO2 flux of ocean and land; ",
                                     "fgco2+nbp (positive into ocean/land) [kgC m-2 s-1]\""))),
   "fLuc"=list(cmd=c("<cdo> -setname,fLuc -mulc,0.272912 <co2_flx_lcc>", # kgCO2 --> kgC 
                     "<nco_ncatted> -O -a code,fLuc,d,,",
                     "<nco_ncatted> -O -a table,fLuc,d,,",
                     "<nco_ncatted> -O -a long_name,fLuc,o,c,\"Net Carbon Mass Flux into Atmosphere due to Land Use Change [kgC m-2 s-1]\"")),
   "litter"=list(cmd=c(paste0("<cdo> -setname,litter -enssum ",
                              "-vertsum <boxYC_acid_ag1> -vertsum <boxYC_acid_ag2> ", # 1: leaf, 2: wood
                              "-vertsum <boxYC_water_ag1> -vertsum <boxYC_water_ag2> ",
                              "-vertsum <boxYC_ethanol_ag1> -vertsum <boxYC_ethanol_ag2> ",
                              "-vertsum <boxYC_nonsoluble_ag1> -vertsum <boxYC_nonsoluble_ag2>"),
                       "<nco_ncatted> -O -a code,litter,d,,",
                       "<nco_ncatted> -O -a table,litter,d,,",
                       "<nco_ncatted> -O -a long_name,litter,o,c,\"Litter carbon (yasso)\"")),
   "soilFast"=list(cmd=c(paste0("<cdo> -setname,soilFast -enssum ",
                                "-vertsum <boxYC_acid_bg1> -vertsum <boxYC_acid_bg2> ", # 1: leaf, 2: wood
                                "-vertsum <boxYC_water_bg1> -vertsum <boxYC_water_bg2> ",
                                "-vertsum <boxYC_ethanol_bg1> -vertsum <boxYC_ethanol_bg2> ",
                                "-vertsum <boxYC_nonsoluble_bg1> -vertsum <boxYC_nonsoluble_bg2>"),
                         "<nco_ncatted> -O -a code,soilFast,d,,",
                         "<nco_ncatted> -O -a table,soilFast,d,,",
                         "<nco_ncatted> -O -a long_name,soilFast,o,c,\"Fast soil carbon (yasso)\"")),
   "cSoilSlow"=list(cmd=c(paste0("<cdo> -setname,cSoilSlow -mulc,0.0120107 -add ", # molC --> kgC
                                "-vertsum <boxYC_humus_1> -vertsum <boxYC_humus_2>"), # 1: leaf, 2: wood
                                # `cdo -add -vertsum <file> -vertsum <file>` is faster than
                                # `cdo -vertsum -add <file> <file>` (tested with 740MB files)
                          "<nco_ncatted> -O -a code,cSoilSlow,d,,",
                          "<nco_ncatted> -O -a table,cSoilSlow,d,,",
                          "<nco_ncatted> -O -a units,cSoilSlow,o,c,\"kgC m-2\"",
                          "<nco_ncatted> -O -a long_name,cSoilSlow,o,c,\"Carbon Mass in Slow Soil Pool\"")),
   "divuvttot"=list(cmd=c(paste0("<cdo> -setname,divuvttot -setname,divuvttot -add ",
                                 "-selvar,divuvt <divuvt> -selvar,divuvteddy <divuvtedd>"),
                          "<nco_ncatted> -O -a long_name,divuvttot,o,c,\"mean + eddy div_h(u_h t)\""))
                      ) # cdo_known_cmds

message("###################### namelist.general.post.r finish ##########################")

