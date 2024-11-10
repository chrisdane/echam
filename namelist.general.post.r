# r

# input for post_echam.r

message("###################### namelist.general.post.r start ##########################")

graphics.off()
options(show.error.locations=T)
options(warn=0) # default: print warnings at the end
#options(warn=1) # show warnings when they occur
#options(warn=2) # stop on warnings

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
cdo_pedantic <- "--pedantic" # turn cdo warning into cdo error
cdo_silent <- "" # "-s" for silent or ""
cdo_select_no_history <- "" # "--no_history" or ""
cdo_convert_grb2nc <- T # should post processing result be converted to nc (will be set to T if new dates are wanted)?
cdo_OpenMP_threads <- "" # default: no
cdo_OpenMP_threads <- paste0("-P ", max(1, trunc(0.75*as.integer(system("nproc", intern=T))))) # "-P n" or "" (will be irgnored on commands that do not support OMP)
cdo_OpenMP_threads <- "-P 32"
# the error  
#   OMP: Error #34: System unable to allocate necessary resources for OMP thread:
#   OMP: System error #11: Resource temporarily unavailable
#   OMP: Hint Try decreasing the value of OMP_NUM_THREADS.
# may be solved by decreasing -P 
cdo_set_rel_time <- F # conversion from absolute to relative time
cdo_run_from_script <- T # create temporary file and run long cdo command from there
# maximum number of args cdo
# stan0/1: getconf ARG_MAX 2621440
# paleosrv1: getconf ARG_MAX 2097152
cdo_nchar_max_arglist <- 2350000 # reduce this number if you get segmentation fault on the cdo selection command (many files)

# nco options
# maximum number of args nco
# $(getconf PAGE_SIZE)*32 = 4096*32 = 131072
nco_nchar_max_arglist <- 131071

# nice options
# -n, --adjustment=N
#   add integer N to the niceness (default 10)
# Niceness values range from -20 (most favorable to the process) to 19 (least favorable to the process)
# levante: only values >= 0 are allowed
nice_options <- "" # default: do not use nice
#nice_options <- "-n 19"
#nice_options <- "-n 10"
nice_options <- "-n 0"

# ionice options
# -c, --class class
# Specify the name or number of the scheduling class to use; 0 for none, 1 for realtime, 2 for best-effort, 3 for idle.
# -n, --classdata level
# Specify  the scheduling class data.  This only has an effect if the class accepts an argument.  For realtime and best-effort, 0-7 are valid data (priority levels), and 0 represents the highest priority level.
ionice_options <- "" # default: do not use ionice
#ionice_options <- "-c2 -n3"
#ionice_options <- "-c2 -n0"

# model specific general options
mpiom1_remap <- T

# known dimnames; add further
# so far only time needed
known_dimnames <- list(time=c("time", "Time", "TIME", "time_mon", "T", "t"))

# cdo commands for some variables
cdo_known_cmds <- list(
   "psl"=list(cmd=c("<cdo> merge <aps> <geosp> <t>",
                    "<cdo> sealevelpressure")),
   "apr_tot"=list(cmd="<cdo> -setunit,'mm day-1' -setname,apr_tot -mulc,86400 -enssum <aprl> <aprc>"), # kg m-2 s-1 --> mm day-1
   "siarea"=list(cmd=c("<cdo> -setunits,m2 -setname,siarea -mul <gridarea> [ divc,100 <siconc> ]")), # cmip6:siconc is in %
   "hvel"=list(cmd=c("<cdo> expr,'hvel=sqrt(uo*uo + vo*vo)' <uvo>")),
   # TOA imbalance
   # https://github.com/ncar-hackathons/gallery/blob/master/cmip6dpdt_pendergrass/get_cmip6_ECS-alt.ipynb
   # cmor: 
   # N = rsdt - rsut - rlut
   # rsdt = toa_incoming_shortwave_flux = TOA Incident Shortwave Radiation
   # rsut = toa_outgoing_shortwave_flux = TOA Outgoing Shortwave Radiation
   # rlut = toa_outgoing_longwave_flux  = TOA Outgoing Longwave Radiation
   # rtmt = net_downward_radiative_flux_at_top_of_atmosphere_model = Net Downward Radiative Flux at Top of Model
   # echam: 
   # N = trad0 + srad0 (= `cdo add trad0 srad0`)
   # trad0 = top thermal radiation (OLR)
   # srad0 = net top solar radiation
   # srad0d = top incoming SW radiation = rsdt
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
                      # co2_flx_ocean:7
                      # >0 into atm --> >0 into ocean; kgCO2 --> kgC
                      "<nco_ncatted> -O -a code,fgco2,d,,",
                      "<nco_ncatted> -O -a table,fgco2,d,,",
                      "<nco_ncatted> -O -a long_name,fgco2,o,c,\"Surface Downward Flux of Total CO2 [kgC m-2 s-1]\"")),
   "nbp"=list(cmd=c("<cdo> -setname,nbp -mulc,-0.272912 -enssum <co2_flx_land> <co2_flx_lcc> <co2_flx_harvest>", 
                    # co2_flx_land:6 + co2_flx_lcc:24 + co2_flx_harvest:25
                    # >0 into atm --> >0 into land; kgCO2 --> kgC; nbp = netAtmosLandCO2Flux
                    "<nco_ncatted> -O -a code,nbp,d,,",
                    "<nco_ncatted> -O -a table,nbp,d,,",
                    "<nco_ncatted> -O -a long_name,nbp,o,c,\"Carbon Mass Flux out of Atmosphere Due to Net Biospheric Production on Land [kgC m-2 s-1]\"")),
   "netAtmosLandCO2Flux"=list(cmd=c("<cdo> -setname,netAtmosLandCO2Flux -mulc,-0.272912 -enssum <co2_flx_land> <co2_flx_lcc> <co2_flx_harvest>", 
                                    # into atm --> into land; kgCO2 --> kgC; netAtmosLandCO2Flux = nbp
                                    "<nco_ncatted> -O -a code,netAtmosLandCO2Flux,d,,",
                                    "<nco_ncatted> -O -a table,netAtmosLandCO2Flux,d,,",
                                    paste0("<nco_ncatted> -O -a long_name,netAtmosLandCO2Flux,o,c,\"Net flux of CO2 between atmosphere and ",
                                           "land (positive into land) as a result of all processes [kgC m-2 s-1]\""))),
   "co2_flx_nat"=list(cmd=c("<cdo> -setname,co2_flx_nat -enssum <co2_flx_ocean> <co2_flx_land>",
                            paste0("<nco_ncatted> -O -a long_name,co2_flx_nat,o,c,\"Total natural CO2 flux of ocean and land; ",
                                   "co2_flx_ocean+co2_flx_land (positive into atm) [kgCO2 m-2 s-1]\""))),
   # co2_flx_total1 and co2_flx_total2 are very similar
   "co2_flx_total1"=list(cmd=c("<cdo> -setname,co2_flx_total1 -enssum <co2_flx_ocean> <co2_flx_land> <co2_flx_anthro> <co2_flux_corr>",
                               paste0("<nco_ncatted> -O -a long_name,co2_flx_total1,o,c,\"Total CO2 flux of ocean and land; ",
                                      "co2_flx_ocean+co2_flx_land+co2_flx_anthro+co2_flux_corr (positive into atm) [kgCO2 m-2 s-1]\""))),
   "co2_flx_total2"=list(cmd=c("<cdo> -setname,co2_flx_total2 -enssum <co2_flx_ocean> <co2_flx_land> <co2_flx_lcc> <co2_flx_harvest> <co2_emis> <co2_flux_corr>", # problem: co2_emis only in esm-hist/ssp, not esm-piControl
                               paste0("<nco_ncatted> -O -a long_name,co2_flx_total2,o,c,\"Total CO2 flux of ocean and land; ",
                                      "co2_flx_ocean+co2_flx_land+co2_flx_lcc+co2_flx_harvest+co2_flx_anthro+co2_flux_corr (positive into atm) [kgCO2 m-2 s-1]\""))),
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
   "divuvttot"=list(cmd=c(paste0("<cdo> -setname,divuvttot -add ",
                                 "-selvar,divuvt <divuvt> -selvar,divuvteddy <divuvteddy>"),
                          "<nco_ncatted> -O -a long_name,divuvttot,o,c,\"mean + eddy div_h(u_h T)\"")),
   "chl"=list(cmd=c(paste0("<cdo> -setname,npp -add ",
                                  "-selvar,bgc15 <bgc15> -selvar,bgc06 <bgc06>"),
                    paste0("<nco_ncatted> -O -a long_name,chl,o,c,",
                           "\"Mass Concentration of Total Phytoplankton Expressed as Chlorophyll in Sea Water; Chl_diatoms + Chl_phytoplankton\""))),
   "npp_nanophy_dia"=list(cmd=c(paste0("<cdo> -setname,npp_nanophy_dia -add ",
                                       "-selvar,diags3d01 <diags3d01> -selvar,diags3d02 <diags3d02>"),
                                paste0("<nco_ncatted> -O -a long_name,npp_nanophy_dia,o,c,",
                                       "\"net primary production by nanophytoplankton + net primary production by diatoms\""))),
   "pCO2a"=list(cmd=c("<cdo> -setname,pCO2a -sub <pCO2s> <dpCO2s>", # recom in µatm; oce - (oce - air) = oce - oce + air = air
                      "<nco_ncatted> -O -a long_name,pCO2a,o,c,\"Partial pressure of atmospheric CO2\"")),
   "apco2"=list(cmd=c("<cdo> -setname,apco2 -sub <spco2> <dpco2>", # cmip6 in Pa; oce - (oce - air) = oce - oce + air = air
                      "<nco_ncatted> -O -a long_name,apco2,o,c,\"Partial pressure of atmospheric CO2\"")),
   "POCphydiadet"=list(cmd=c("<cdo> -setname,POCphydiadet -enssum <bgc05> <bgc14> <bgc08>", # phyc + diac + detc
                             "<nco_ncatted> -O -a long_name,poc,o,c,\"Carbon from small pyhtoplankton + diatoms + detritus\"")),
   "export_detC_100m"=list(cmd=c("<cdo> -setname,export_detC_100m -mulc,22.88 <detoc>", # 22.88 = 20+0.0288*100; molC m-3 * m day-1 = molC m-2 day-1
                                 "<nco_ncatted> -O -a long_name,export_detC_100m,o,c,\"Carbon export production = (20+0.0288*100)*detoc(100m), detoc: Sum of detrital organic carbon component concentrations\"",
                                 "<nco_ncatted> -O -a units,export_detC_100m,o,c,\"molC d-1\"")), # cmip6:detoc is in mol m-3, new units refer to fldint
   "calcite"=list(cmd=c("<cdo> -setname,calcite -enssum <bgc20> <bgc21>", # phycal + detcal
                        "<nco_ncatted> -O -a long_name,calcite,o,c,\"Calcite from small pyhtoplankton + detritus\"")),
   "sedimentC"=list(cmd=c("<cdo> -setname,sedimentC -enssum <benC> <benCalc>",
                          "<nco_ncatted> -O -a long_name,sedimentC,o,c,\"Benthic carbon and calcium carbonate\"")),
   "silicate"=list(cmd=c("<cdo> -setname,siliate -enssum <bgc16> <bgc17> <bgc18> <benSi>", # (diatom + detritus + dissolved acid + benthic) silicate
                          "<nco_ncatted> -O -a long_name,silicate,o,c,\"Diatoms + detritus + dissolved acid + benthic Silicate\""))
         ) # cdo_known_cmds

message("###################### namelist.general.post.r finish ##########################")

