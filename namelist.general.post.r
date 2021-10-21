# r

# input for post_echam.r
message("################################ namelist.general.post.r ################################")

# clear work space
if (T) {
    message("clear work space ...")
    ws <- ls()
    ws <- ws[-which(ws == "repopath")]
    rm(list=ws)
}

# load helper functions of this repo
script_helper_functions <- paste0(repopath, "/helper_functions.r")
message("load `repopath`/helper_functions.r = ", script_helper_functions, " ...")
source(script_helper_functions) # get_host()

# get host options
host <- get_host()
host$repopath <- repopath

# todo: how to load helper functions from another repo without the subrepo hassle?
if (file.exists(paste0(host$homepath, "/functions/myfunctions.r"))) {
    message("\nload ", host$homepath, "/functions/myfunctions.r ...")
    source(paste0(host$homepath, "/functions/myfunctions.r"))
    # dependencies: 
    # ht(), make_posixlt_origin(), is.leap(), tryCatch.W.E(), identical_list(), cdo_get_filetype()
} else {
    stop("\ncould not load ", host$homepath, "/functions/myfunctions.r")
}

# general options
verbose <- 1 # 0,1
post_force <- F # redo calculation although output file already exists 
clean <- T # remove temporary files

# cdo options
cdo_silent <- "" # "-s" for silent or ""
cdo_select_no_history <- "" # "--no_history" or ""
cdo_convert_grb2nc <- T # should post processing result be converted to nc (will be set to T if new dates are wanted)?
cdo_OpenMP_threads <- "-P 4" # "-P n" or "" (will be irgnored on commands that do not support OMP)
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

# cdo commands for some variables
cdo_known_cmds <- list("psl"=list(cmd=c("<cdo> merge <aps> <geosp> <t>",
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
                                              "<nco_ncatted> -O -a units,tsurfaprt,o,c,\"degC\""))
                      ) # cdo_known_cmds

