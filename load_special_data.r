# r

# todo: solve dependencies from other repos: make_posixlt_origin()
#source(paste0(hompath, "/functions/myfunctions.r"))


# echam SLF


# echam OROMEA (= `elevation` of jsbach input)
if (F) {
    f <- paste0(host$repopath, "/echam/T31GR30_OROMEA.nc")
    if (file.exists(f)) {
        message("\ndisable here if you do not want to load echam OROMEA from ", f, " ...")
        ncin <- nc_open(f)
        data360 <- ncvar_get(ncin, "OROMEA")
        lon360 <- ncin$dim$lon$vals
        # convert lons
        data180 <- convert_lon_360_to_180(lon360=lon360, data360=data360, data360_lonind=1)
        # flip lats
        data180$data180 <- data180$data180[,dim(data360)[2]:1] 
        echam_TR31GR30_oromea <- list(lon=data180$lon180, lat=rev(ncin$dim$lat$vals),
                                      OROMEA=data180$data180)
        add_echam_TR31GR30_oromea_contour <- T
        rm(data360, lon360)
        message("set add_echam_TR31GR30_oromea_contour=T if you want to add to lon,lat plot ...\n")
    } else {
        message("file ", f, " does not exist. cannot load echam OROMEA from ...")
    }
} else {
    message("enable here to load echam OROMEA ...")
}


# mpiom land sea mask segments
fs <- paste0(host$repopath, "/mpiom/mpiom_", c("GR30s", "GR15s", "TP04s"), "_land_sea_mask_segments_lon180.txt")
#fs <- paste0(host$repopath, "/mpiom/mpiom_", c("GR30s", "GR15s", "TP04s"), "_land_sea_mask_segments_lon360.txt")
if (F && any(file.exists(fs))) {
    message("\ndisable here if you do not want to load mpiom land sea mask segments ...")
    for (f in fs) {
        if (file.exists(f)) {
            cmd <- paste0(tools::file_path_sans_ext(basename(f)), " <- read.table(f, header=T)")
            message("run `", cmd, "` ...")
            eval(parse(text=cmd))
        } else {
            message("mpiom land sea mask file ", f, " does not exist. run the function mpiom_get_lsm_segments() via ", 
                    host$repopath, "/mpiom/run_mpiom.r to generate the land sea mask segments ...")
        }
    }
} else {
    message("enable here to load mpiom land sea mask segments ...")
}


# reccap2 lsm
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba1103/a270073/data/reccap2-ocean/R2-shared-resources/data/regions/RECCAP2_region_masks_all.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load reccap2 lsm from ", f, " ...")
    reccap2_lsm_ncin <- nc_open(f)
} else {
    message("enable here to load reccap2 lsm ...")
}


# cmip6 co2 hist
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/pool/data/ECHAM6/input/r0007/greenhouse_historical.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load hist CO2 from ", f, " ...")
    co2_hist_ncin <- nc_open(f)
    time <- co2_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*365.25*86400, origin="0000-01-01", tz="UTC", format="%Y")
    co2_hist <- list(file=f,
                     co2_ppm=ncvar_get(co2_hist_ncin, "CO2"), time=timelt, timen=time,
                     text="", col="red", lty=2, lwd=0.5, pch=NA)
    add_co2_hist <- F
    message("set add_co2_hist=T if you want to add to plot")
} else {
    message("enable here to load hist CO2 ...")
}


# cmip6 co2 1pct
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/pool/data/ECHAM6/input/r0008/greenhouse_1pctCO2.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load 1pctCO2 CO2 from ", f, " ...")
    co2_1pct_ncin <- nc_open(f)
    time <- co2_1pct_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*365.25*86400, origin="0000-01-01", tz="UTC", format="%Y")
    co2_1pct <- list(file=f,
                     co2_ppm=ncvar_get(co2_1pct_ncin, "CO2"), time=timelt, timen=time,
                     text="", col="#377EB8", lty=2, lwd=0.5, pch=NA)
    add_co2_1pct <- F
    message("set add_co2_1pct=T if you want to add to plot")
} else {
    message("enable here to load 1pctCO2 CO2 ...")
}


# cmip6 4CO2 
co2_4co2 <- list(co2_ppm=1137.2679,
                 text="", col="#1B9E77", lty=2, lwd=0.5, pch=NA)
message("\nset 4CO2 to ", co2_4co2$co2_ppm, " ppm") 
add_co2_4co2 <- F
message("set add_co2_4co2=T if you want to add to plot")


# nao time series
f <- paste0(host$workpath, "/NAO/nao_pc_djfm.txt")
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load ucar hurrel NAO time series ", f, " ...")
    source(paste0(host$workpath, "/NAO/read_ucar_nao_function.r"))
    nao <- read_ucar_nao_function(f)
} else {
    message("enable here to load ucar hurrel NAO time series ...")
}


# koehler et al. 2017 ghg by paul
f <- ""
if (host$machine_tag == "stan") {
    f <- "/ace/user/pgierz/cosmos-aso-wiso/Hol-T/scripts/Koehler_GHG_forcing_0.001ka_resolution.dat"
} else if (host$machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/data/koehler_etal_2017/Koehler_GHG_forcing_0.001ka_resolution.dat"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load koehler et al. 2017 ghg forcing from ", f, " from paul ...")
    koehler_etal_2017_paul <- read.table(f, col.names=c("year_before_1950", "CO2", "CH4", "N2O"))
    years <- koehler_etal_2017_paul$year_before_1950 # kyr before 1950 in reverse order --> 6.999, 6.998, 6997, ...
    years <- -1000*years # --> -6999, -6998, -6997, ...
    # as.POSIXlt("-0001-01-01") --> negative year gives error 
    # "not in a standard unambiguous format"
    # --> but shifting works
    timelt <- as.POSIXlt("0000-01-01", tz="UTC")
    nyears_to_origin <- timelt$year + 1900 - years[1] + 1
    timelt <- seq.POSIXt(timelt, l=nyears_to_origin, b="-1 year")[nyears_to_origin]
    timelt <- seq.POSIXt(timelt, l=length(years), b=paste0(diff(years)[1], " year"))
    timelt <- as.POSIXlt(timelt)
    ## convert concentration (ppm, ppb, ...) to radiative forcing (W m-2) relative to preindustrial reference concentrations `<gas>_0_<unit>`
    message("convert concentration (ppm, ppb, ...) to radiative forcing (W m-2) relative to reference concentrations after joos and spahni 2008 their table 3 ...")
    # after joos and spahni 2008 table 3
    co2_Wm2_fac <- 5.35 # W m-2
    co2_0_ppm <- 278 # ppm
    #co2_0_ppm <- koehler_etal_2017_paul$CO2[which(timelt$year+1900==0)] 
    ch4_Wm2_fac <- 0.036 # W m-2
    ch4_0_ppb <- 742 # ppb
    #ch4_0_ppb <- koehler_etal_2017_paul$CH4[which(timelt$year+1900==0)]
    n2o_Wm2_fac <- 0.12 # W m-2
    n2o_0_ppb <- 272 # pp
    #n2o_0_ppb <- koehler_etal_2017_paul$N2O[which(timelt$year+1900==0)]
    overlap_function <- function(m, n) {
        0.47*log(1 + 2.01*1e-5*(m*n)^0.75 + 5.31*1e-15*m*(m*n)^1.52)
    }
    co2_wm2 <- co2_Wm2_fac*log(koehler_etal_2017_paul$CO2/co2_0_ppm)
    ch4_wm2 <- ch4_Wm2_fac*(sqrt(koehler_etal_2017_paul$CH4) - sqrt(ch4_0_ppb)) -
        (overlap_function(m=koehler_etal_2017_paul$CH4, n=n2o_0_ppb) - 
         overlap_function(m=ch4_0_ppb, n=n2o_0_ppb))
    n2o_wm2 <- n2o_Wm2_fac*(sqrt(koehler_etal_2017_paul$N2O) - sqrt(n2o_0_ppb)) -
        (overlap_function(m=ch4_0_ppb, n=koehler_etal_2017_paul$N2O) - 
         overlap_function(m=ch4_0_ppb, n=n2o_0_ppb))
    koehler_etal_2017_paul <- list(co2=koehler_etal_2017_paul$CO2,
                                   ch4=koehler_etal_2017_paul$CH4,
                                   n2o=koehler_etal_2017_paul$N2O,
                                   co2_wm2=co2_wm2, ch4_wm2=ch4_wm2, n2o_wm2=n2o_wm2, 
                                   ghg_wm2=co2_wm2 + ch4_wm2 + n2o_wm2,
                                   time=timelt, timen=as.numeric(timelt),
                                   #text="Köhler et al. 2017 (paul)", 
                                   text="GHG forcing anomaly (Köhler et al. 2017)", 
                                   col="red", lty=2, lwd=1, pch=NA)
    add_koehler_etal_2017_paul <- F
    message("set add_koehler_etal_2017_paul=", !add_koehler_etal_2017_paul, 
            " if you ", ifelse(add_koehler_etal_2017_paul, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot\n")
} else {
    message("enable here to load koehler et al. 2017 ghg forcing from paul ...")
}


# koehler et al. 2017 ghg original data
f <- ""
if (host$machine_tag == "stan") {
    f <- "/ace/user/cdanek/data/koehler_etal_2017/datasets/CO2_stack_156K_spline_V2.tab"
    source("/ace/user/cdanek/data/koehler_etal_2017/read_koehler_etal_2017_function.r")
}
if (file.exists(f)) {
    from <- 6999
    to <- 0
    message("\ndisable here if you do not want to load koehler et al. 2017 ghg forcing from ", f, " from ", from, " to ", to, " ...")
    koehler_etal_2017_co2 <- read_koehler_etal_2017(f=f, from=from, to=to)
    # reverse time order from 0-6999 to 6999-0
    koehler_etal_2017_co2 <- koehler_etal_2017_co2[dim(koehler_etal_2017_co2)[1]:1,]
    years <- koehler_etal_2017_co2[,1] # kyr before 1950 --> 6.999, 6.998, 6.997, ...
    years <- -1*years*1000 # --> -6999, -6998, -6997, ...
    # as.POSIXlt("-0001-01-01") --> negative year gives error 
    # "not in a standard unambiguous format"
    # --> but shifting works
    timelt <- as.POSIXlt("0000-01-01", tz="UTC")
    nyears_to_origin <- timelt$year + 1900 - years[1] + 1
    timelt <- seq.POSIXt(timelt, l=nyears_to_origin, b="-1 year")[nyears_to_origin]
    timelt <- seq.POSIXt(timelt, l=length(years), b=paste0(diff(years)[1], " year"))
    timelt <- as.POSIXlt(timelt)
    koehler_etal_2017 <- list(co2=koehler_etal_2017_co2[,2],
                              time=timelt, timen=as.numeric(timelt),
                              text="Köhler et al. 2017", 
                              col="red", lty=2, lwd=0.5, pch=NA)
    add_koehler_etal_2017_co2 <- F
    message("set add_koehler_etal_2017_co2=", !add_koehler_etal_2017_co2, 
            " if you ", ifelse(add_koehler_etal_2017_co2, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
} else {
    message("enable here to load koehler et al. 2017 ghg forcing ...")
}


# cmip6 historical monthly total solar irradiance 
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/cmip6/solar_irradiance/swflux_14band_monthly_1850-2014.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load historical monthly total solar irradiance from ", f, " ...")
    tsi_hist_ncin <- nc_open(f)
    time <- tsi_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*86400, origin="1850-01-01", tz="UTC")
    tsi_hist_monthly <- list(file=f,
                             tsi_hist=ncvar_get(tsi_hist_ncin, "TSI"), time=timelt, timen=time,
                             col="orange2",
                             col_rgb=rgb(t(col2rgb("orange2")/255), alpha=0.5),
                             text="TSI", lty=1, lwd=0.5, pch=NA) 
    add_tsi_hist <- F
    message("set add_tsi_hist=T if you want to add to plot")
} else {
    message("enable here to load historical monthly total solar irradiance  ...")
}


# cmip6 historical annual total solar irradiance 
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/cmip6/solar_irradiance/swflux_14band_annual_1850-2014.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load historical monthly total solar irradiance from ", f, " ...")
    tsi_hist_ncin <- nc_open(f)
    time <- tsi_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*86400, origin="1850-01-01", tz="UTC")
    tsi_hist_annual <- list(file=f,
                            tsi_hist=ncvar_get(tsi_hist_ncin, "TSI"), time=timelt, timen=time,
                            col="orange2",
                            col_rgb=rgb(t(col2rgb("orange2")/255), alpha=0.5),
                            text="TSI", lty=1, lwd=0.5, pch=NA) 
    add_tsi_hist_annual <- F
    message("set add_tsi_hist_annual=T if you want to add to plot")
} else {
    message("enable here to load historical monthly total solar irradiance ...")
}


# hadcrut4 global monthly temperature anomaly wrt 1961-1990
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/HadCRUT4/HadCRUT_global_SAT_anomaly_wrt_1961-1990_HadCRUT.4.6.0.0.monthly_ns_avg.txt.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load hadcrut4 global monthly SAT anomalies wrt to 1961-1990 from ", f, " ...")
    hadcrut4_ncin <- nc_open(f)
    time <- hadcrut4_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    hadcrut4_sat_anom_monthly <- list(hadcrut4_sat_anom=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990"),
                                      hadcrut4_sat_anom_lower_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_lower_uncertainty"),
                                      hadcrut4_sat_anom_upper_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_upper_uncertainty"),
                                      time=timelt, timen=time)
    add_hadcrut4_sat_anom_monthly <- F
    message("set add_hadcrut4_sat_anom_monthly=T if you want to add to plot")
} else {
    message("enable here to load hadcrut4 global monthly SAT anomalies wrt to 1961-1990 from ...")
}


# hadcrut4 global annual temperature anomaly wrt 1961-1990
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/HadCRUT4/HadCRUT_global_SAT_anomaly_wrt_1961-1990_HadCRUT.4.6.0.0.annual_ns_avg.txt.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load hadcrut4 global annual SAT anomalies wrt to 1961-1990 from ", f, " ...")
    hadcrut4_ncin <- nc_open(f)
    time <- hadcrut4_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    hadcrut4_sat_anom_annual <- list(hadcrut4_sat_anom=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990"),
                                     hadcrut4_sat_anom_lower_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_lower_uncertainty"),
                                     hadcrut4_sat_anom_upper_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_upper_uncertainty"),
                                     time=timelt, timen=time, col="black",
                                     col_rgb=rgb(t(col2rgb("black")/255), alpha=0.1),
                                     text="HadCRUT4", lty=1, lwd=1, pch=NA)
    add_hadcrut4_sat_anom_annual <- F
    message("set add_hadcrut4_sat_anom_annual=T if you want to add to plot")
} else {
    message("enable here to load hadcrut4 global annual SAT anomalies wrt to 1961-1990 from ...")
} # hadcrut4 global annual temperature anomaly wrt 1961-1990


# marcott et al temperature anomaly wrt 1961-1990
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/data/marcott_etal_2013/Marcott.SM.database.S1.xlsx"
    source("/isibhv/projects/paleo_work/cdanek/data/marcott_etal_2013/read_marcott_etal_2013_function.r")
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load marcott et al. 2013 temperature anomalies wrt to 1961-1990 from ", f, " ...")
    marcott_etal_2013 <- read_marcott_etal_2013_function(f)
    time <- marcott_etal_2013$year_before_1950 # -50 -30 -10  10 ... 11210 11230 11250 11270 11290
    time <- rev(-1*time) # -11290 -11270 -11250 -11230 -11210 -11190 -11170 ... -70 -50 -30 -10  10  30  50
    timelt <- make_posixlt_origin(time, origin_in=1950, origin_out=1950, verbose=0)
    marcott_etal_2013 <- list(marcott_etal_2013_anom=rev(marcott_etal_2013$"temp_anom_1961-1990_global_5x5"),
                              marcott_etal_2013_anom_sd=rev(marcott_etal_2013$"temp_anom_1961-1990_global_5x5_sd"),
                              time=timelt, timen=as.numeric(timelt), col="black",
                              col_rgb=rgb(t(col2rgb("black")/255), alpha=0.1),
                              text="Marcott et al. 2013", lty=1, lwd=1, pch=NA)
    add_marcott_etal_2013_anom <- F
    message("set add_marcott_etal_2013_anom=", !add_marcott_etal_2013_anom, 
            " if you ", ifelse(add_marcott_etal_2013_anom, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
} else {
    message("enable here to load marcott et al. 2013 temperature anomalies wrt to 1961-1990")
}


# gistempv4 global annual temperature anomaly wrt 1951-1980
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/GISTEMPv4/GISTEMPv4_global_SAT_anomaly_wrt_1951-1980_GLB.Ts+dSST.csv.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load gistempv4 global annual SAT anomalies wrt to 1951-1980 from ", f, " ...")
    gistempv4_ncin <- nc_open(f)
    time <- gistempv4_ncin$dim$time$vals # YYYY
    timelt <- as.POSIXlt(as.Date(paste0(time, "-06-30")), tz="UTC") # use mid-year
    gistempv4_sat_anom_annual <- list(gistempv4_sat_anom=ncvar_get(gistempv4_ncin, "global_SAT_anomaly_wrt_1951-1980"),
                                      time=timelt, timen=time)
    add_gistempv4_sat_anom_annual <- F
    message("set add_gistempv4_sat_anom_annual=T if you want to add to plot")
} else {
    message("enable here to load gistempv4 global annual SAT anomalies wrt to 1951-1980 from ...")
}


# rapid moc
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/RAPID/moc_transports.nc"
    f_err <- "/work/ba0941/a270073/data/RAPID/moc_error.mat"
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load rapid moc from ", f, " ...")
    rapid_ncin <- nc_open(f)
    time <- rapid_ncin$dim$time$vals # "days since 2004-4-1 00:00:00" 
    timelt <- as.POSIXlt(time*86400, origin="2004-04-01T00:00:00Z", tz="UTC") # use mid-year
    moc_rapid <- list(moc=drop(ncvar_get(rapid_ncin, "moc_mar_hc10")), # 12-hourly
                      time=timelt, timen=time,
                      col="black", col_rgb=rgb(t(col2rgb("black")/255), alpha=0.1),
                      text="RAPID", lty=1, lwd=1, pch=NA)
    moc_rapid$moc_annual <- stats::filter(moc_rapid$moc, filter=rep(1/(12*30*2), t=12*30*2))
    moc_rapid$moc_monthly <- stats::filter(moc_rapid$moc, filter=rep(1/(30*2), 30*2))
    if (T) { # also read rapid uncertainty, which is distributed as .mat file. lol.
        suppressMessages(library(R.matlab))
        moc_rapid_error <- readMat(f_err)
        # errors and actual data are not on the same time axis. wtf?
        moc_error_time <- drop(moc_rapid_error$JG[,1])
        moc_error_time <- as.POSIXlt(moc_error_time*86400, origin="2004-04-01T00:00:00Z", tz="UTC")
        moc_error_time_inds <- which(as.numeric(moc_rapid$time) %in% as.numeric(moc_error_time))
        if (length(moc_error_time_inds) > 0) {
            moc_rapid$moc_error <- rep(NA, t=length(moc_rapid$time))
            moc_rapid$moc_error[moc_error_time_inds] <- drop(moc_rapid_error$ERROR[,1]) # 12-hourly
            moc_rapid$moc_error_annual <- stats::filter(moc_rapid$moc_error, filter=rep(1/(12*30*2), t=12*30*2))
            moc_rapid$moc_error_monthly <- stats::filter(moc_rapid$moc_error, filter=rep(1/(30*2), t=30*2))
        } else {
            stop("all rapid moc error times out range of the data time?! this should not happen")
        }
        rm(moc_rapid_error)
    }
    add_moc_rapid <- F
    message("set add_moc_rapid=T if you want to add to plot")
} else {
    message("enable here to load rapid moc from ...")
} 


# monthly nsidc sea ice index northern hemisphere
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ab0246/a270073/data/NSIDC/sea_ice_index/data/N_seaice_extent_monthly_v3.0.csv_1978-10-26_to_2022-03-30.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load northern nsidc sea ice index from ", f, " ...")
    nsidc_ncin <- nc_open(f)
    time <- nsidc_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    nsidc_siextentn_monthly <- list(siextentn=ncvar_get(nsidc_ncin, "siextentn"),
                                    time=timelt, timen=time,
                                    text="NSIDC north", col="black",
                                    lty=1, lwd=1.5, pch=NA)
    add_nsidc_arctic_monthly <- F
    message("set add_nsidc_arctic_monthly=T if you want to add to plot")
} else {
    message("enable here to load northern nsidc sea ice index ...")
}


# monthly nsidc sea ice index southern hemisphere
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ab0246/a270073/data/NSIDC/sea_ice_index/data/S_seaice_extent_monthly_v3.0.csv_1978-10-26_to_2022-03-30.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load monthly southern nsidc sea ice index from ", f, " ...")
    nsidc_ncin <- nc_open(f)
    time <- nsidc_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    nsidc_siextents_monthly <- list(siextents=ncvar_get(nsidc_ncin, "siextents"),
                                    time=timelt, timen=time,
                                    text="NSIDC south", col="black",
                                    lty=3, lwd=1.5, pch=NA)
    add_nsidc_antarctic_monthly <- F
    message("set add_nsidc_antarctic_monthly=T if you want to add to plot")
} else {
    message("enable here to load monthly southern nsidc sea ice index ...")
}


# annual nsidc sea ice index northern hemisphere
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ab0246/a270073/data/NSIDC/sea_ice_index/data/N_seaice_extent_annual_v3.0.csv_1978-10-26_to_2022-03-30.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load annual northern nsidc sea ice index from ", f, " ...")
    nsidc_ncin <- nc_open(f)
    time <- nsidc_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    nsidc_siextentn_annual <- list(siextentn=ncvar_get(nsidc_ncin, "siextentn"),
                                   time=timelt, timen=time,
                                   text="NSIDC north", col="black",
                                   lty=1, lwd=1.5, pch=NA)
    # remove first year 1978 due to data only from 3 months OND
    message("remove first year 1978")
    nsidc_siextentn_annual$siextentn <- nsidc_siextentn_annual$siextentn[-1]
    nsidc_siextentn_annual$time <- nsidc_siextentn_annual$time[-1]
    nsidc_siextentn_annual$timen <- nsidc_siextentn_annual$timen[-1]
    add_nsidc_arctic_annual <- T
    message("set add_nsidc_arctic_annual=T if you want to add to plot")
} else {
    message("enable here to load annual northern nsidc sea ice index from ...")
}


# annual nsidc sea ice index southern hemisphere
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ab0246/a270073/data/NSIDC/sea_ice_index/data/S_seaice_extent_annual_v3.0.csv_1978-10-26_to_2022-03-30.nc"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load annual southern nsidc sea ice index from ", f, " ...")
    nsidc_ncin <- nc_open(f)
    time <- nsidc_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    nsidc_siextents_annual <- list(siextents=ncvar_get(nsidc_ncin, "siextents"),
                                   time=timelt, timen=time,
                                   text="NSIDC south", col="black",
                                   lty=3, lwd=1.5, pch=NA)
    # remove first year 1978 due to data only from 3 months OND
    message("remove first year 1978")
    nsidc_siextents_annual$siextents <- nsidc_siextents_annual$siextents[-1]
    nsidc_siextents_annual$time <- nsidc_siextents_annual$time[-1]
    nsidc_siextents_annual$timen <- nsidc_siextents_annual$timen[-1]
    add_nsidc_antarctic_annual <- T
    message("set add_nsidc_antarctic_annual=T if you want to add to plot")
} else {
    message("enable here to load annual southern nsidc sea ice index ...")
}


# berger holocene transient _accelerated_ orbital parameter from paul
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- "/scratch/simulation_database/incoming/Hol-Tx10/script/HOL_ORB_forcing_0.01ka_resolution_combined.dat"
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load pauls transient accelerated berger orbital parameters from ", f, " ...")
    orb_berger_acc <- read.table(f, col.names=c("year_before_1950", "eccentricity", "precession", "obliquity"))
    years <- orb_berger_acc$year_before_1950 # kyr before 1950 --> 7.00 6.99 6.98 6.97 ... 0.03 0.02 0.01 0.00
    years <- -1*years*1000 # --> -7000 -6990 -6980 -6970 -6960 ... -40 -30 -20 -10   0
    timelt <- make_posixlt_origin(years, origin_in=1950, origin_out=1950, verbose=0)
    orb_berger_acc <- list(eccentricity=orb_berger_acc$eccentricity, 
                       precession=orb_berger_acc$precession,
                       obliquity=orb_berger_acc$obliquity,
                       time=timelt, timen=as.numeric(timelt),
                       text="Berger", col="red", lty=2, lwd=0.5, pch=NA)
    add_orb_berger_acc_eccentricity <- F
    message("set add_orb_berger_acc_eccentricity=", !add_orb_berger_acc_eccentricity, 
            " if you ", ifelse(add_orb_berger_acc_eccentricity, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
    add_orb_berger_acc_precession <- F
    message("set add_orb_berger_acc_precession=", !add_orb_berger_acc_precession, 
            " if you ", ifelse(add_orb_berger_acc_precession, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
    add_orb_berger_acc_obliquity <- F
    message("set add_orb_berger_acc_obliquity=", !add_orb_berger_acc_obliquity, 
            " if you ", ifelse(add_orb_berger_acc_obliquity, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
} else {
    message("enable here to load pauls transient accelerated berger orbital parameters ...")
}


# berger holocene transient _non-accelerated_ orbital parameter from paul
f <- ""
if (host$machine_tag == "stan") {
    f <- "/ace/user/pgierz/cosmos-aso-wiso/Hol-T/scripts/Berger_ORB_forcing_0.001ka_resolution.dat"
} else if (host$machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-T/scripts/Berger_ORB_forcing_0.001ka_resolution.dat"
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load pauls transient non-accelerated berger orbital parameters from ", f, " ...")
    orb_berger <- read.table(f, col.names=c("year_before_1950", "eccentricity", "precession", "obliquity"))
    years <- orb_berger$year_before_1950 # kyr before 1950 --> 6.999, 6.998, 6997, ...
    years <- -1*years*1000 # --> -6999, -6998, -6997, ...
    timelt <- make_posixlt_origin(years, origin_in=1950, origin_out=1950, verbose=0)
    orb_berger <- list(eccentricity=orb_berger$eccentricity, 
                       precession=orb_berger$precession,
                       obliquity=orb_berger$obliquity,
                       time=timelt, timen=as.numeric(timelt),
                       text="Berger", col="red", lty=2, lwd=0.5, pch=NA)
    add_orb_berger_eccentricity <- F
    message("set add_orb_berger_eccentricity=", !add_orb_berger_eccentricity, 
            " if you ", ifelse(add_orb_berger_eccentricity, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
    add_orb_berger_precession <- F
    message("set add_orb_berger_precession=", !add_orb_berger_precession, 
            " if you ", ifelse(add_orb_berger_precession, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
    add_orb_berger_obliquity <- F
    message("set add_orb_berger_obliquity=", !add_orb_berger_obliquity, 
            " if you ", ifelse(add_orb_berger_obliquity, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
} else {
    message("enable here to load pauls transient non-accelerated berger orbital parameters ...")
}


# berger orbital parameter for last 800ka
f <- ""
if (host$machine_tag == "stan") {
    f <- "/home/ace/cdanek/scripts/fortran/berger_1978/berger_1978_years_-800_to_0_kyears_before_1950.txt"
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load my berger orbital parameters from ", f, " ...")
    my_orb_berger <- read.table(f, header=T)
    # column 1: kyear_from_1950 2: ecc 3: obl_deg 4: calendar_day_of_perihelion 5: angle_of_perihelion_deg_from_vernal_equinox
    years <- my_orb_berger$kyear_from_1950 # kyr before 1950 in reverse order --> -800, -799, -798, ...
    years <- years*1000 # -800000, -799000, -798000, ...
    timelt <- make_posixlt_origin(years, origin_in=1950, origin_out=1950, verbose=0)
    my_orb_berger <- list(eccentricity=my_orb_berger$ecc, 
                          obliquity=my_orb_berger$obl_deg,
                          calendar_day_of_perihelion=my_orb_berger$calendar_day_of_perihelion,
                          angle_of_perihelion_deg_from_vernal_equinox=my_orb_berger$angle_of_perihelion_deg_from_vernal_equinox,
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          text="Berger", col="red", lty=2, lwd=0.5, pch=NA)
    add_my_orb_berger_eccentricity <- F
    message("set add_my_orb_berger_eccentricity=", !add_my_orb_berger_eccentricity, 
            " if you ", ifelse(add_my_orb_berger_eccentricity, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
    add_my_orb_berger_precession <- F
    message("set add_my_orb_berger_precession=", !add_my_orb_berger_precession, 
            " if you ", ifelse(add_my_orb_berger_precession, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
    add_my_orb_berger_obliquity <- F
    message("set add_my_orb_berger_obliquity=", !add_my_orb_berger_obliquity, 
            " if you ", ifelse(add_my_orb_berger_obliquity, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
} else {
    message("enable here to load my berger orbital parameters ...")
}


# laskar orbital parameter for last 800ka
f <- ""
if (host$machine_tag == "stan") {
    f <- "/home/ace/cdanek/scripts/fortran/laskar_etal_2004/laskar_etal_2004_years_-800_to_0_kyears_before_2000.txt"
} else if (host$machine_tag == "paleosrv") {
    f <- "/home/csys/cdanek/scripts/fortran/laskar_etal_2004/laskar_etal_2004_years_-800_to_0_kyears_before_2000.txt"
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load laskar orbital parameters from ", f, " ...")
    my_orb_laskar <- read.table(f, header=T)
    # column 1: kyear_from_1950 2: ecc 3: obl_deg 4: angle_of_perihelion_deg_from_vernal_equinox
    years <- my_orb_laskar$kyear_from_2000 # kyr before 2000 in reverse order --> -800, -799, -798, ..., -3, -2, -1,  0
    years <- years*1000 # -800000, -799000, -798000, ..., -3000, -2000, -1000,  0
    timelt <- make_posixlt_origin(years, origin_in=2000, origin_out=1950, verbose=0)
    my_orb_laskar <- list(eccentricity=my_orb_laskar$ecc, 
                          obliquity=my_orb_laskar$obl_deg,
                          angle_of_perihelion_deg_from_vernal_equinox=my_orb_laskar$angle_of_perihelion_deg_from_vernal_equinox,
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          text="Laskar", col="blue", lty=2, lwd=0.5, pch=NA)
    add_my_orb_laskar_eccentricity <- F
    message("set add_my_orb_laskar_eccentricity=", !add_my_orb_laskar_eccentricity, 
            " if you ", ifelse(add_my_orb_laskar_eccentricity, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
    add_my_orb_laskar_precession <- F
    message("set add_my_orb_laskar_precession=", !add_my_orb_laskar_precession, 
            " if you ", ifelse(add_my_orb_laskar_precession, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
    add_my_orb_laskar_obliquity <- F
    message("set add_my_orb_laskar_obliquity=", !add_my_orb_laskar_obliquity, 
            " if you ", ifelse(add_my_orb_laskar_obliquity, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
} else {
    message("enable here to load laskar orbital parameters ...")
}

# berger and laoutre 1999 orbital parameters
# pdoi = "10.1594/PANGAEA.56040"

# PLOT coords as eval list
f <- "~/awi/PLOT/git/PLOT/lakes/lake_coords.txt"
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load PLOT lake coords from ", f, " and save in `PLOT_coords_cmd_list` ...")
    lakes_table <- read.table(f, header=T, stringsAsFactors=F)
    PLOT_coords_cmd_list <- NULL
    lakes <- c("A"="ladoga")
    if (T) {
        message("add shuchye lake to PLOT_coords_cmd_list")
        lakes <- c(lakes, "B"="shuchye")
    } else {
        message("dont add shuchye lake to PLOT_coords_cmd_list")
    }
    lakes <- c(lakes, "C"="kotokel", "D"="emanda", "E"="two-yurts")
    if (T) {
        message("add gygy lake to PLOT_coords_cmd_list")
        lakes <- c(lakes, "F"="elgygytgyn")
    } else {
        message("dont add gygy lake to PLOT_coords_cmd_list")
    }
    col <- "black"
    if (varnames_in[1] == "lm_wisoaprt_d_post_as_time_slope") { # blue letters for significant negative holocene trends
        message("special lm_wisoaprt_d_post_as_time_slope blue PLOT letters in PLOT_coords_cmd_list")
        col <- "blue"
    }
    for (lakei in seq_along(lakes)) {
        cmd <- "text("
        cmd <- paste0(cmd, "x=", lakes_table[which(lakes_table$name == lakes[lakei]),"lon_dec"])
        cmd <- paste0(cmd, ", y=", lakes_table[which(lakes_table$name == lakes[lakei]),"lat_dec"])
        cmd <- paste0(cmd, ", labels=\"", names(lakes)[lakei], "\"")
        cmd <- paste0(cmd, ", cex=1")
        #cmd <- paste0(cmd, ", cex=1.5")
        if (lakes[lakei] == "shuchye") {
            cmd <- paste0(cmd, ", col=\"black\"")
        } else {
            cmd <- paste0(cmd, ", col=\"", col, "\"")
        }
        if (T) cmd <- paste0(cmd, ", font=2") # bold
        cmd <- paste0(cmd, ") # ", lakes[lakei])
        PLOT_coords_cmd_list[[lakei]] <- cmd 
    }
    message()
} else {
    message("enable here to load PLOT lake coords from and save in `PLOT_coords_cmd_list` ...")
}


# hanno meyer et al. PLOT excel sheet
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/data/meyer_etal/PLOT-project_Lacustrine diatom oxygen isotope_Kotokel.xlsx"
    source("/isibhv/projects/paleo_work/cdanek/data/meyer_etal/read_meyer_etal_function.r")
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load hanno meyer et al. PLOT data from ", f)
    message("run read_meyer_etal_function() ...")
    lakes <- c("A"="Lake Ladoga")
    if (F) {
        message("add shuchye lake to `meyer_etal`")
        lakes <- c(lakes, "B"="Lake Bolshoye Shchuchye unpubl.")
    } else {
        message("dont add shuchye lake to `meyer_etal`")
    }
    lakes <- c(lakes, "C"="Lake Kotokel", "D"="Lake Emanda unpubl.", "E"="Two Jurts Lake")
    if (F) {
        message("add gygy lake to `meyer_etal`")
        lakes <- c(lakes, "F"="El'gygytgyn Lake")
    } else {
        message("dont add gygy lake to `meyer_etal`")
    }
    
    tmp <- read_meyer_etal_function(xlsx_file=f, sheets_wanted=lakes, year_from=-7000, verbose=F)
    #tmp <- read_meyer_etal_function(xlsx_file=f, sheets_wanted=lakes, year_from=-10000, verbose=F)
    meyer_etal <- list(data=tmp,
                       type="o", 
                       #col="#E41A1C", # myred
                       col="#377EB8", # myblue
                       #col="#1B9E77", # mygreen
                       lty=1, lwd=1, pch=1, cex=1)
} else {
    message("enable here to load `meyer_etal` hanno meyer et al. PLOT data excel sheet ...")
}


# kostrova et al. 2021 emanda
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/data/kostrova_etal_2021/Kostrova et al_Oxygen isotope composition of diatoms from Lake Emanda (northeastern Siberia, Russia).xlsx"
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load kostrova et al. 2021 emanda data from ", f)
    suppressPackageStartupMessages(library(xlsx))
    kostrova_etal_2021 <- xlsx::read.xlsx(f, 1,
                                          startRow=17, endRow=62, colIndex=2:9, 
                                          header=F, stringsAsFactors=F)
    colnames(kostrova_etal_2021) <- c("core", "sample_depth_cm", "age_cal_ka_bp", "SiO2_pcnt", "Al2O3_pcnt", 
                                      "d18o_meas_perm", "C_cont_pcnt", "d18o_corr_perm")
    kostrova_etal_2021 <- kostrova_etal_2021[dim(kostrova_etal_2021)[1]:1,] # rev
    years <- -1000*kostrova_etal_2021$age_cal_ka_bp
    timelt <- make_posixlt_origin(years, origin_in=1950, origin_out=1950, verbose=0)
    kostrova_etal_2021$time <- timelt
    kostrova_etal_2021 <- list(data=kostrova_etal_2021,
                               type="o", 
                               #col="#E41A1C", # myred
                               col="#377EB8", # myblue
                               #col="#1B9E77", # mygreen
                               lty=1, lwd=1, pch=1, cex=1,
                               loc="Emanda (D)",
                               ref="Kostrova et al. 2021",
                               text="D: Emanda (Kostrova et al. 2021)")
} else {
    message("enable here to load kostrova et al. 2021 emanda data ...")
}


# silt size of hoogakker et al. 2011
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/data/hoogakker_etal_2011/hoogakker2011.xls"
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load hoogakker et al. 2011 silt data from ", f, " ...")
    suppressPackageStartupMessages(library(xlsx))
    # get available sheet names
    wb <- loadWorkbook(f)
    sheets_available <- names(getSheets(wb))
    
    # load data from wanted sheet
    sheets_wanted <- c("Orphan SS", "MD99-2251 SS")
    data <- vector("list", l=length(sheets_wanted))
    for (i in seq_along(sheets_wanted)) {
        sheet <- sheets_wanted[i]
        sheet_ind <- which(sheets_available == sheet)
        if (length(sheet_ind) == 0) stop("this should not happen")

        message("read sheet \"", sheet, "\" ...")

        # sheet specific things
        if (sheet == "Orphan SS") { # "North West Atlantic" in Fig. 3 a
            dat <- xlsx::read.xlsx(f, sheetName=sheet, stringsAsFactors=F, header=T, startRow=3) 
            silt_size <- rev(dat[["X..Mean.sortable.silt.Grain.Size..um"]])
            years <- rev(dat[["Age.yrsBP."]]) # BP = 1950
            years <- -years # 0.203   0.793  1.373  1.939  2.483
            timelt <- make_posixlt_origin(years, origin_in=1950, origin_out=1950, verbose=0)
            data[[i]] <- list(lon=-45.688, lat=50.208,
                              time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                              "silt_size"=silt_size,
                              id="HU91 045 093 HR; MD95 2024",
                              text="Hoogakker et al. 2011 (Orphan Knoll; NWA)")

        } else if (sheet == "MD99-2251 SS") { # "North East Atlantic" in Fig. 3 a
            dat <- xlsx::read.xlsx(f, sheetName=sheet, stringsAsFactors=F, header=T, startRow=3) 
            silt_size <- rev(dat[["X..Mean.sortable.silt.Grain.Size..um"]])
            years <- rev(dat[["Age..years.BP."]]) # BP = 1950
            years <- -years # 0.203   0.793  1.373  1.939  2.483
            timelt <- make_posixlt_origin(years, origin_in=1950, origin_out=1950, verbose=0)
            data[[i]] <- list(lon=-27.913, lat=57.458,
                              time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                              "silt_size"=silt_size,
                              id="MD99 2251",
                              text="Hoogakker et al. 2011 (Gardar Drift; NEA)")
        } # which sheet
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    } # for i sheets_wanted 
    hoogakker_etal_2011 <- list(data=data,
                                    type="o", col="#377EB8",
                                    lty=1, lwd=1, pch=1, cex=1)
    if (save_silt_list) silt_all[[length(silt_all)+1]] <- hoogakker_etal_2011
} else { # if file.exists(f)
    message("enable here to load hoogakker et al. 2011 silt data ...")
} # if silt hoogakker et al. 2011


# silt size change of thornalley et al. 2013
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/data/thornalley_etal_2013/thornalley2013-cop-stack.txt"
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load thornalley et al. 2013 silt data from ", f, " ...")
    dat <- read.table(f, header=T, sep="\t", comment.char="#")
    years <- rev(dat$age_calkaBP) # BP = 1950
    silt_size_change <- rev(dat$SS.depwgt.grp)
    years <- -1000*years # 0.203   0.793  1.373  1.939  2.483
    timelt <- make_posixlt_origin(years, origin_in=1950, origin_out=1950, verbose=0)
    data <- vector("list", l=1)
    data[[1]] <- list(lon=-20.82808, # = mean(c(-17.5895, -24.066667)),
                      lat=61.68942, # = mean(c(60.403333, 62.9755))
                      time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                      "silt_size_change"=silt_size_change,
                      id="Iceland Basin stack",
                      text="Thornalley et al. 2013 (Iceland Basin stack)")
    thornalley_etal_2013 <- list(data=data,
                                     type="o", col="#377EB8",
                                     lty=1, lwd=1, pch=1, cex=1)
    if (length(timelt) > 1) {
        meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
        message("average dt = ", meandt, " days ~ ", meandt/365, " years")
    }
    if (save_silt_list) silt_all[[length(silt_all)+1]] <- thornalley_etal_2013
} else {# if file.exists(f)
    message("enable here to load thornalley et al. 2013 silt data ...")
} # silt size change of thornalley et al. 2013


# silt data from mjell et al. 2015
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/data/mjell_etal_2015/palo20202-sup-0002-tables1.xlsx"
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load mjell et al. 2015 silt data from ", f, " ...")
    library(xlsx)
    dat <- read.xlsx(f, "Ark1")
    years <- rev(dat[["Age..BP."]]) # BP = 1950
    silt_size <- rev(dat[["Sortable.silt.mean.grain.size"]])
    # remove NA for better mov avg
    if (any(is.na(silt_size))) {
        years <- years[-which(is.na(silt_size))]
        silt_size <- silt_size[-which(is.na(silt_size))]
    }
    years <- -years 
    timelt <- make_posixlt_origin(years, origin_in=1950, origin_out=1950, verbose=0)
    data <- vector("list", l=1) 
    data[[1]] <- list(lon=-23.96667, lat=60.31967, 
                      time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                      "silt_size"=silt_size,
                      id="GS06144 08GC",
                      text="Mjell et al. 2015")
    if (length(timelt) > 1) {
        meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
        message("average dt = ", meandt, " days ~ ", meandt/365, " years")
    }
    mjell_etal_2015 <- list(data=data,
                                type="o", col="#377EB8",
                                lty=1, lwd=1, pch=1, cex=1)
    if (save_silt_list) silt_all[[length(silt_all)+1]] <- mjell_etal_2015
} else {  # if file.exists(f)
    message("enable here to load mjell et al. 2015 silt data from ...")
} # if silt data from mjell et al. 2015


# NOAA monthly temp/precip station data from https://www.ncdc.noaa.gov/cdo-web/search
ghcdn_csv <- ""
if (host$machine_tag == "ollie") {
    ghcdn_csv <- list.files("/work/ollie/cdanek/data/NOAA/station_data/GHCDN/monthly",
                            pattern=glob2rx("*.csv"), full.names=T)
} else if (host$machine_tag == "paleosrv") {
    ghcdn_csv <- list.files("/isibhv/projects/paleo_work/cdanek/data/NOAA/station_data/GHCDN/monthly",
                            pattern=glob2rx("*.csv"), full.names=T)
}
if (F && file.exists(ghcdn_csv[1])) {
    message("\ndisable here if you do not want to load NOAA station datasets ...")
    message("station", appendLF=F)
    noaa_ghcdn <- vector("list", l=length(ghcdn_csv))
    for (i in seq_along(ghcdn_csv)) { # one file per station
        message(" ", i, ": \"", basename(ghcdn_csv[i]), "\"", appendLF=F)
        d <- read.csv(ghcdn_csv[i], stringsAsFactors=F)
        tmp <- list(time=as.POSIXlt(paste0(d$DATE, "-15"), tz="UTC"),
                    Tavg=d$TAVG, Tmin=d$TMIN, Tmax=d$TMAX, precip=d$PRCP)
        tmp$timen <- as.numeric(tmp$time)
        #if (i == 8) stop("asd")
        # annual means
        tmp$years <- unique(tmp$time$year+1900)
        Tavg_an <- Tmin_an <- Tmax_an <- precip_an <- nmonths_an <- rep(NA, t=length(tmp$years))
        for (yi in seq_along(tmp$years)) {
            yinds <- which(tmp$time$year+1900 == tmp$years[yi])
            Tavg_an[yi] <- mean(tmp$Tavg[yinds], na.rm=T)
            Tmin_an[yi] <- mean(tmp$Tmin[yinds], na.rm=T)
            Tmax_an[yi] <- mean(tmp$Tmax[yinds], na.rm=T)
            precip_an[yi] <- mean(tmp$precip[yinds], na.rm=T)
            nmonths_an[yi] <- length(yinds)
        }
        tmp$Tavg_an <- Tavg_an
        tmp$Tmin_an <- Tmin_an
        tmp$Tmax_an <- Tmax_an
        tmp$precip_an <- precip_an
        tmp$nmonths_an <- nmonths_an
        # monthly climatologies
        tmp$months <- 1:12 #sort(unique(tmp$time$mon+1))
        Tavg_mon <- Tmin_mon <- Tmax_mon <- precip_mon <- nyears_mon <- rep(NA, t=length(tmp$months))
        for (mi in seq_along(tmp$months)) {
            minds <- which(tmp$time$mon+1 == tmp$months[mi])
            if (length(minds) > 0) {
                Tavg_mon[mi] <- mean(tmp$Tavg[minds], na.rm=T)
                Tmin_mon[mi] <- mean(tmp$Tmin[minds], na.rm=T)
                Tmax_mon[mi] <- mean(tmp$Tmax[minds], na.rm=T)
                precip_mon[mi] <- mean(tmp$precip[minds], na.rm=T)
                nyears_mon[mi] <- length(minds)
            }
        }
        tmp$Tavg_mon <- Tavg_mon
        tmp$Tmin_mon <- Tmin_mon
        tmp$Tmax_mon <- Tmax_mon
        tmp$precip_mon <- precip_mon
        tmp$nyears_mon <- nyears_mon
        noaa_ghcdn[[i]]$coords <- c(lon=d$LONGITUDE[1], lat=d$LATITUDE[1])
        noaa_ghcdn[[i]]$ts <- tmp
        noaa_ghcdn[[i]]$text <- paste0("WMO ", d$STATION[1])
        noaa_ghcdn[[i]]$type <- "o"
        noaa_ghcdn[[i]]$col <- "#377EB8"
        noaa_ghcdn[[i]]$lty <- 1
        noaa_ghcdn[[i]]$lwd <- 1
        noaa_ghcdn[[i]]$pch <- 1
        noaa_ghcdn[[i]]$cex <- 1
        names(noaa_ghcdn)[i] <- tools::file_path_sans_ext(basename(ghcdn_csv[i]))
        if (T) { # add distance to closest lake to legend 
            if (names(noaa_ghcdn)[i] == "RSM00021802_SASKYLAH_RS") {
                noaa_ghcdn[[i]]$text <- paste0("WMO ", d$STATION[1], "; dist(taymyr)=488 km; dist(levinson)=566 km")
            } else if (names(noaa_ghcdn)[i] == "RSM00022802_SORTAVALA_RS") {
                noaa_ghcdn[[i]]$text <- paste0("WMO ", d$STATION[1], "; dist(ladoga)=82 km")
            } else if (names(noaa_ghcdn)[i] == "RSM00023226_VORKUTA_RS") {
                noaa_ghcdn[[i]]$text <- paste0("WMO ", d$STATION[1], "; dist(shuchye)=107 km")
            } else if (names(noaa_ghcdn)[i] == "RSM00024671_TOMPO_RS") {
                noaa_ghcdn[[i]]$text <- paste0("WMO ", d$STATION[1], "; dist(emanda)=147 km")
            } else if (names(noaa_ghcdn)[i] == "RSM00025248_ILIRNEJ_RS") {
                noaa_ghcdn[[i]]$text <- paste0("WMO ", d$STATION[1], "; dist(elgygytgyn)=136 km")
            } else if (names(noaa_ghcdn)[i] == "RSM00030731_GORJACINSK_RS") {
                noaa_ghcdn[[i]]$text <- paste0("WMO ", d$STATION[1], "; dist(kotokel)=24 km")
            } else if (names(noaa_ghcdn)[i] == "RSM00032389_KLJUCHI_RS") {
                noaa_ghcdn[[i]]$text <- paste0("WMO ", d$STATION[1], "; dist(two-yurts)=72 km")
            }
        } # special legend labels
    } # for i ghdcn csv files; one file per station
    message()
} else {
    message("enable here to load NOAA station datasets ...")
} # if NOAA station data


# GNIP monthly station data from https://nucleus.iaea.org/wiser/index.aspx
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- paste0("/isibhv/projects/paleo_work/cdanek/data/iaea_wmo/gnip/post/",
                "gnip_ts_O18_H2_H3_H3_err_precip_tair_vapour_pressure_min_5_consecutive_complete_yrs_1953-1-15_to_2019-12-15.RData2"
                )
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load GNIP monthly station data ...\n")
    load(f) # gnip_list 
} else {
    message("enable here to load monthly GNIP station data ...")
} # load GNIP data


# read bartlein et al. 2011
fs <- ""
if (host$machine_tag == "paleosrv") {
    fs <- c("/isibhv/projects/paleo_work/cdanek/data/bartlein_etal_2011/QRec_2013-12_nc/mat_delta_06ka_ALL_grid_2x2_ex.nc",
            "/isibhv/projects/paleo_work/cdanek/data/bartlein_etal_2011/QRec_2013-12_nc/map_delta_06ka_ALL_grid_2x2_ex.nc")
}
if (T && any(file.exists(fs))) {
    message("\ndisable here if you do not want to load bartlein et al. 2011 data ...")
    bartlein_etal_2011 <- vector("list", l=length(fs))
    for (i in seq_along(fs)) {
        bartlein_etal_2011[[i]] <- list()
        ncin <- nc_open(fs[i])
        if (grepl("mat_delta_06ka", fs[i])) {
            if (F) { # all values
                message("use all data")
                bartlein_etal_2011[[i]]$data <- ncvar_get(ncin, "mat_anm_mean")
            } else if (T) { # only where statistical significant change between MH and PI
                message("use significant data only")
                bartlein_etal_2011[[i]]$data <- ncvar_get(ncin, "mat_sig_val") 
                bartlein_etal_2011[[i]]$data[bartlein_etal_2011[[i]]$data == 0] <- NA
            }
            names(bartlein_etal_2011)[i] <- "mat_MH_minus_PI"
        } else if (grepl("map_delta_06ka", fs[i])) {
            if (F) {
                message("use all data")
                bartlein_etal_2011[[i]]$data <- ncvar_get(ncin, "map_anm_mean")
            } else if (T) {
                message("use significant data only")
                bartlein_etal_2011[[i]]$data <- ncvar_get(ncin, "map_sig_val")
                bartlein_etal_2011[[i]]$data[bartlein_etal_2011[[i]]$data == 0] <- NA
            }
            names(bartlein_etal_2011)[i] <- "map_MH_minus_PI"
        } else {
            stop("not yet")
        }
        bartlein_etal_2011[[i]]$lon <- ncin$dim$lon$vals
        bartlein_etal_2011[[i]]$lat <- ncin$dim$lat$vals
    }
} else {
    message("enable here to load bartlein et al. 2011 data ...")
} # read bartlein et al. 2011


# read kaufman et al. 2020 temp12k
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- paste0("/isibhv/projects/paleo_work/cdanek/data/kaufman_etal_2020_temp12k/",
                "Temp12k_v1_0_0_ts_non-scale_216_records_with_units_degC_and_variableName_temperature_and_seasonality_annual_from_-7000_to_0_from_1950_CE_lm_period_ge_2000_years_lm_p_lt_0.01_6kyr_trend_ge_-7_and_ge_7",
                ".RData2")
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load kaufman et al. 2020 temp12k data from ", f, " ...")
    datnames <- load(f)
    if (length(datnames) != 1) stop("loaded ", length(datnames), " objects: \"",
                                    paste(datnames, collapse="\", \""), 
                                    "\". dont know which one to use")
    eval(parse(text=paste0("kaufman_etal_2020_temp12k <- ", datnames)))
    rm(data, datnames)
} else {
    message("enable here to load kaufman et al. 2020 temp12k data ...")
} # read kaufman et al. 2020 temp12k
    
# read kaufman et al. 2020 temp12k isotope
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- paste0("/isibhv/projects/paleo_work/cdanek/data/kaufman_etal_2020_temp12k/",
                "Temp12k_v1_0_0_ts_non-scale_13_records_with_archiveType_LakeSediment_Ice-other_GlacierIce_proxy_d18O_and_units_permil_and_seasonality_annual_from_-7000_to_0_from_1950_CE_lm_period_ge_2000_years_lm_p_lt_0.01",
                ".RData2")
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load kaufman et al. 2020 temp12k isotope data from ", f, " ...")
    datnames <- load(f)
    if (length(datnames) != 1) stop("loaded ", length(datnames), " objects: \"",
                                    paste(datnames, collapse="\", \""), 
                                    "\". dont know which one to use")
    eval(parse(text=paste0("kaufman_etal_2020_temp12k_d18o <- ", datnames)))
    rm(data, datnames)
} else {
    message("enable here to load kaufman et al. 2020 temp12k isotope data ...")
} # read kaufman et al. 2020 temp12k isotope


# read global holcene lipd temp data
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- paste0("/isibhv/projects/paleo_work/cdanek/data/lipd/global_holocene/",
                "globalHolocene1_0_0_ts_non-scale_383_records_with_units_degC_and_variableName_temperature_or_temperatureComposite_and_area_NAsiberia_from_-7000_to_-50_lm_p_lt_0.001_6kyr_trend_ge_-7_and_le_7",
                ".RData2")
}
if (F && file.exists(f)) {
    message("\ndisable here if you do not want to load global holcene lipd temp data from ", f, " ...")
    datnames <- load(f)
    if (length(datnames) != 1) stop("loaded ", length(datnames), " objects: \"",
                                    paste(datnames, collapse="\", \""), 
                                    "\". dont know which one to use")
    eval(parse(text=paste0("global_holocene_lipd_temp <- ", datnames)))
} else {
    message("enable here to load global holcene lipd temp data ...")
} # read global holcene lipd temp data


# read global holcene lipd precip data
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- paste0("/isibhv/projects/paleo_work/cdanek/data/lipd/global_holocene/",
                "globalHolocene1_0_0_ts_non-scale_150_records_with_units_mm_or_mmyr_and_variableName_precipitation_and_seasonality_annual_from_-7000_to_0_from_1950_CE_lm_period_ge_2000_years_lm_p_lt_0.01",
                ".RData2")
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load global holcene lipd precip data from ", f, " ...")
    datnames <- load(f)
    if (length(datnames) != 1) stop("loaded ", length(datnames), " objects: \"",
                                    paste(datnames, collapse="\", \""), 
                                    "\". dont know which one to use")
    eval(parse(text=paste0("global_holocene_lipd_precip <- ", datnames)))
} else {
    message("enable here to load global holcene lipd precip data ...")
} # read global holcene lipd precip data


# read konecky et al. 2020 iso2k d18o_precip data
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- paste0("/isibhv/projects/paleo_work/cdanek/data/konecky_etal_2020/",
                #"iso2k1_0_0_ts_non-scale_131_records_with_variableName_d18O_from_1967_to_2013_lm_p_lt_0.05",
                #"iso2k1_0_0_ts_non-scale_147_records_with_variableName_d18O_and_units_permil_and_inferredMaterial_precipitation_from_-100_to_63_from_1950_CE",
                #"iso2k1_0_0_ts_non-scale_121_records_with_variableName_d18O_and_units_permil_and_inferredMaterial_precipitation_and_seasonality_annual_from_-100_to_61_from_1950_CE",
                #"iso2k1_0_0_ts_non-scale_37_records_with_variableName_d18O_and_units_permil_and_inferredMaterial_precipitation_from_-7000_to_-50_lm_p_lt_0.05_6kyr_trend_ge_-10_and_le_10",
                "iso2k1_0_0_ts_non-scale_9_records_with_variableName_d18O_and_units_permil_and_inferredMaterial_precipitation_and_seasonality_annual_from_-7000_to_0_from_1950_CE_lm_period_ge_2000_years_lm_p_lt_0.01",
                ".RData2")
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load konecky et al. 2020 iso2k d18o_precip data from ", f, " ...")
    datnames <- load(f)
    if (length(datnames) != 1) stop("loaded ", length(datnames), " objects: \"",
                                    paste(datnames, collapse="\", \""), 
                                    "\". dont know which one to use")
    eval(parse(text=paste0("konecky_etal_2020_iso2k_d18o_precip <- ", datnames)))
} else {
    message("enable here to load konecky et al. 2020 iso2k d18o_precip data ...")
} # read konecky et al. 2020 iso2k d18o_precip data


# read konecky et al. 2020 iso2k d18o_nonprecip data
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- paste0("/isibhv/projects/paleo_work/cdanek/data/konecky_etal_2020/",
                #"iso2k1_0_0_ts_scale_109_records_with_variableName_d18O_and_units_permil_and_inferredMaterial_lake_or_lagoon_or_ground_or_soil_water_from_-7000_to_-50_lm_p_lt_0.05_6kyr_trend_ge_-10_and_le_10",
                #"iso2k1_0_0_ts_scale_14_records_with_variableName_d18O_and_units_permil_and_inferredMaterial_lake_or_lagoon_or_ground_or_soil_water_and_seasonality_annual_from_-7000_to_0_from_1950_CE_lm_period_ge_2000_years_lm_p_lt_0.01",
                "iso2k1_0_0_ts_non-scale_14_records_with_variableName_d18O_and_units_permil_and_inferredMaterial_lake_or_lagoon_or_ground_or_soil_water_and_seasonality_annual_from_-7000_to_0_from_1950_CE_lm_period_ge_2000_years_lm_p_lt_0.01",
                ".RData2")
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load konecky et al. 2020 iso2k d18o_nonprecip data from ", f, " ...")
    datnames <- load(f)
    if (length(datnames) != 1) stop("loaded ", length(datnames), " objects: \"",
                                    paste(datnames, collapse="\", \""), 
                                    "\". dont know which one to use")
    eval(parse(text=paste0("konecky_etal_2020_iso2k_d18o_nonprecip <- ", datnames)))
} else {
    message("enable here to load konecky et al. 2020 iso2k d18o_nonprecip data ...")
} # read konecky et al. 2020 iso2k d18o_nonprecip data


# read comas-bru et al. 2020 sisal data
f <- ""
if (host$machine_tag == "paleosrv") {
    f <- paste0("/isibhv/projects/paleo_work/cdanek/data/comas-bru_etal_2020/",
                #"sisal_2.0_53_records_d18O_c_pdb_d18O_c_smow_d18O_w_smow_-100_to_67_from_1950_CE",
                "sisal_2.0_80_records_d18O_c_pdb_d18O_c_smow_d18O_w_smow_-7000_to_0_from_1950_CE_lm_period_ge_2000_years_lm_p_lt_0.01",
                ".RData2")
}
if (T && file.exists(f)) {
    message("\ndisable here if you do not want to load comas-bru et al. 2020 sisal data from ", f, " ...")
    datnames <- load(f)
    if (length(datnames) != 1) stop("loaded ", length(datnames), " objects: \"",
                                    paste(datnames, collapse="\", \""), 
                                    "\". dont know which one to use")
    eval(parse(text=paste0("comas_bru_etal_2020_sisal_d18o_precip <- ", datnames)))
    # remove "Soreq cave" and "Klapferloch cave" since they are already in Iso2k 1.0.0
    if (all(varnames_in == "lm_wisoaprt_d_post_as_time_slope")) {
        inds <- which(!is.na(match(sapply(comas_bru_etal_2020_sisal_d18o_precip, "[[", "loc"), 
                                   c("Soreq cave", "Klapferloch cave"))))
        if (length(inds) > 0) {
            message("special: remove \"Soreq cave\" and \"Klapferloch cave\" from comas_bru_etal_2020_sisal_d18o_precip ...")
            comas_bru_etal_2020_sisal_d18o_precip <- comas_bru_etal_2020_sisal_d18o_precip[-inds]
        }
    }
} else {
    message("enable here to load comas-bru et al. 2020 sisal data ...")
} # read comas-bru et al. 2020 sisal data


# ERA5 time series data
fs <- ""
if (host$machine_tag == "paleosrv") {
    fs <- "/isibhv/projects/paleo_work/cdanek/data/ERA5/post"
}
if (F && fs != "") {
    message("\ndisable here if you do not want to load ERA5 time series ...")
    fs <- list.files(fs, pattern=glob2rx("era5_select_*"), full.names=T)
    if (length(fs) > 0) { # e.g. "era5_select_viwvn_emanda_remapnn_Jan-Dec_1990-2010.nc"
        era5_ts <- list(fs=fs, name="ERA5", n_ma=36,
                        col="#377EB8", lty=1, lwd=1, pch=1, cex=1)
    } # if any of fs exists
} else {
    message("enable here to load ERA5 time series ...")
} # if ERA5 time series


# ERA5 spatial data
fs <- c("era5_viwv_yseasmean_1990-2010.nc", "era5_viwv_direction_yseasmean_1990-2010.nc")
if (host$machine_tag == "paleosrv") {
    fs <- paste0("/isibhv/projects/paleo_work/cdanek/data/ERA5/post/", fs)
}
if (F && any(file.exists(fs))) {
    message("\ndisable here if you do not want to load ERA5 spatial datasets ...")
    era5_spatial <- vector("list", l=length(fs))
    cnt <- 0
    for (f in fs) {
        if (file.exists(f)) {
            cnt <- cnt + 1
            message("load ERA5 data from \"", f, "\" ...")
            era5_ncin <- nc_open(f)
            lon <- era5_ncin$dim$lon$vals
            lon_orig <- NULL # default
            if (reorder_lon_from_0360_to_180180) {
                lon_orig <- lon
                west_of_180_inds <- lon < 180
                east_of_180_inds <- lon >= 180
                lon <- lon_orig - 180
            }
            lat <- era5_ncin$dim$lat$vals
            lat_orig <- NULL # default
            if (any(diff(lat) < 0)) {
                lat_orig <- lat
                lat <- rev(lat)
            }
            time <- era5_ncin$dim$time$vals
            timelt <- as.POSIXlt(as.Date(time/24, origin="1990-1-1"), tz="UTC")
            tmp <- vector("list", l=era5_ncin$nvars)
            names(tmp) <- names(era5_ncin$var)
            for (vi in seq_along(tmp)) {
                message("   load \"", names(era5_ncin$var)[vi], "\" ...")
                dat <- ncvar_get(era5_ncin, names(era5_ncin$var)[vi])
                if (!is.null(lon_orig)) {
                    if (!any(search() == "package:abind")) library(abind)
                    dat <- abind(dat[east_of_180_inds,,], dat[west_of_180_inds,,], along=1)
                }
                if (!is.null(lat_orig)) dat <- dat[,length(lat):1,]
                attributes(dat) <- list(dim=dim(dat), dims=c("lonlat", "time"))
                tmp[[vi]] <- dat
            }
            tmp <- list(data=tmp, 
                        dims=list(lon=lon, lat=lat, time=timelt, timen=time))
            era5_spatial[[cnt]] <- tmp
            names(era5_spatial)[cnt] <- basename(f)
        } # if f exists
    } # for f in fs
} else {
    message("enable here to load ERA5 spatial datasets ...")
} # if ERA5 spatial data


# post processed aviso data
#fs <- c("aviso_eke_Mar_1993-2009_ltm_global.nc")
#fs <- c("aviso_eke_Mar_1993-2009_ltm_lsea.nc")
fs <- c("aviso_eke_Jan-Dec_1993-2009_ltm_lsea.nc")
if (host$machine_tag == "mistral") {
    fs <- paste0("/work/ba0941/a270073/data/aviso/post/madt/uv/", fs)
}
if (T && any(file.exists(fs))) {
    message("\ndisable here if you do not want to load post processed aviso data ...")
    aviso <- vector("list", l=length(fs))
    cnt <- 0
    for (f in fs) {
        if (file.exists(f)) {
            cnt <- cnt + 1
            message("load aviso data from \"", f, "\" ...")
            aviso_ncin <- nc_open(f)
            lon <- aviso_ncin$dim$lon$vals
            lat <- aviso_ncin$dim$lat$vals
            lat_orig <- NULL # default
            if (any(diff(lat) < 0)) {
                lat_orig <- lat
                lat <- rev(lat)
            }
            varnames <- c("eke_ltm", "n_obs_ltm")
            tmp <- vector("list", l=length(varnames))
            names(tmp) <- varnames
            for (vi in seq_along(tmp)) {
                message("   load \"", varnames[vi], "\" ...")
                dat <- ncvar_get(aviso_ncin, varnames[vi])
                units <- ncatt_get(aviso_ncin, varnames[vi])$units
                # do special stuff
                if (T && varnames[vi] == "eke_ltm") {
                    message("convert aviso eke_ltm m2 s-2 --> cm2 s-2") 
                    dat <- dat*1e4
                    units <- "cm2 s-2"
                }
                if (!is.null(lat_orig)) dat <- dat[,length(lat):1]
                attributes(dat) <- list(dim=dim(dat), dims=c("lon", "lat"), units=units)
                tmp[[vi]] <- dat
            }
            tmp <- list(file=f,
                        dims=list(lon=lon, lat=lat),
                        data=tmp)
            aviso[[cnt]] <- tmp
            names(aviso)[cnt] <- basename(f)
        } # if f exists
    } # for f in fs
} else {
    message("enable here to load post processed aviso data ...")
} # if post processed aviso data


# post processed en4 data
fs <- c("EN4_4.2.1_area_time_mean_Mar_1948-2009_5-5350m_global_mld.ge.0.125kgm3_mldmethod_fesom_EOS80p_ref_0dbar.nc")
#fs <- c("EN4_4.2.1_area_time_mean_Mar_1948-2009_5-5350m_global_mld.ge.0.001kgm3_mldmethod_fesom_EOS80p_ref_0dbar.nc")
if (host$machine_tag == "mistral") {
    fs <- paste0("/work/ba0941/a270073/data/en4/post/", fs)
}
if (T && any(file.exists(fs))) {
    message("\ndisable here if you do not want to load post processed en4 data ...")
    en4 <- vector("list", l=length(fs))
    cnt <- 0
    for (f in fs) {
        if (file.exists(f)) {
            cnt <- cnt + 1
            message("load en4 data from \"", f, "\" ...")
            en4_ncin <- nc_open(f)
            lon <- en4_ncin$dim$lon$vals
            lat <- en4_ncin$dim$lat$vals
            lat_orig <- NULL # default
            if (any(diff(lat) < 0)) {
                lat_orig <- lat
                lat <- rev(lat)
            }
            varnames <- c("bathy", "temp", "salt", "potdens", "mld")
            tmp <- vector("list", l=length(varnames))
            names(tmp) <- varnames
            for (vi in seq_along(tmp)) {
                message("   load \"", varnames[vi], "\" ...")
                dat <- ncvar_get(en4_ncin, varnames[vi])
                units <- ncatt_get(en4_ncin, varnames[vi])$units
                if (!is.null(lat_orig)) dat <- dat[,length(lat):1]
                attributes(dat) <- list(dim=dim(dat), dims=c("lon", "lat"), units=units)
                tmp[[vi]] <- dat
            }
            tmp <- list(file=f,
                        dims=list(lon=lon, lat=lat),
                        data=tmp)
            en4[[cnt]] <- tmp
            names(en4)[cnt] <- basename(f)
        } # if f exists
    } # for f in fs
} else {
    message("enable here to load post processed en4 data ...")
} # if post processed en4 data


# post processed holte et al. 2017 data
fs <- c("Argo_mixedlayers_monthlyclim_03172021_my_lsea.nc")
if (host$machine_tag == "mistral") {
    fs <- paste0("/work/ba0941/a270073/data/holte_etal_2017/", fs)
}
if (F && any(file.exists(fs))) {
    message("\ndisable here if you do not want to load post processed holte et al. 2017 data ...")
    holte_etal_2017 <- vector("list", l=length(fs))
    cnt <- 0
    for (f in fs) {
        if (file.exists(f)) {
            cnt <- cnt + 1
            message("load holte et al. 2017 data from \"", f, "\" ...")
            holte_etal_2017_ncin <- nc_open(f)
            lon <- holte_etal_2017_ncin$dim$lon$vals
            lat <- holte_etal_2017_ncin$dim$lat$vals
            lat_orig <- NULL # default
            if (any(diff(lat) < 0)) {
                lat_orig <- lat
                lat <- rev(lat)
            }
            varnames <- c("mld_da_mean", "mld_dt_mean", # density algorithm, density threshold
                          "mld_da_median", "mld_dt_median", 
                          "mld_da_max", "mld_dt_max")
            tmp <- vector("list", l=length(varnames))
            names(tmp) <- varnames
            for (vi in seq_along(tmp)) {
                message("   load \"", varnames[vi], "\" ...")
                dat <- ncvar_get(holte_etal_2017_ncin, varnames[vi]) # nmonth x nlon x nlat
                units <- ncatt_get(holte_etal_2017_ncin, varnames[vi])$units
                if (!is.null(lat_orig)) dat <- dat[,length(lat):1]
                attributes(dat) <- list(dim=dim(dat), dims=c("month", "lon", "lat"), units=units)
                tmp[[vi]] <- dat
            }
            tmp <- list(file=f,
                        dims=list(lon=lon, lat=lat),
                        data=tmp)
            holte_etal_2017[[cnt]] <- tmp
            names(holte_etal_2017)[cnt] <- basename(f)
        } # if f exists
    } # for f in fs
} else {
    message("enable here to load post processed holte et al. 2017 data ...")
} # if post processed holte et al. 2017 data


if (T) { # post processed gregor_and_fay_2021 data
    message("\ndisable here if you do not want to load post processed gregor_and_fay_2021 data ...")
    gregor_and_fay_2021_areas <- c("global", "reccap2_atlantic", "reccap2_pacific", "reccap2_indian", "reccap2_arctic", "reccap2_southern")
    gregor_and_fay_2021_varnames <- paste0("fgco2_ens_", c("mean", "median", "sd", "max", "min")) # !!! max-->min due to *-1
    gregor_and_fay_2021_labels <- c(paste0("GF21 mmm", plus_minus_symbol, "mmsd"), paste0("GF21 mmmed", plus_minus_symbol, "mmsd"), "GF21 mmsd", "GF21 mmmin", "G21 mmmax")
    gregor_and_fay_2021_col <- mycols(4)[4]
    gregor_and_fay_2021 <- vector("list", l=3)
    names(gregor_and_fay_2021) <- c("Jan-Dec", "annual", "ymonmean")
    for (modei in seq_along(gregor_and_fay_2021)) {
        for (ai in seq_along(gregor_and_fay_2021_areas)) {
            tmpa <- list()
            for (vi in seq_along(gregor_and_fay_2021_varnames)) {
                f <- paste0(host$workpath, "/post/gregor_and_fay_2021/fldint/", gregor_and_fay_2021_varnames[vi], "/",
                            "gregor_and_fay_2021_gregor_and_fay_2021_fldint_", gregor_and_fay_2021_varnames[vi], "_", 
                            gregor_and_fay_2021_areas[ai], "_", names(gregor_and_fay_2021)[modei], "_1990-2019.nc")
                if (file.exists(f)) {
                    #message("load gregor_and_fay_2021 data from \"", f, "\" ...")
                    nc <- nc_open(f)
                    time <- as.POSIXct(nc$dim$time$vals*86400, o="1981-12-15", tz="UTC")
                    years <- as.numeric(format(time, "%Y"))
                    months <- as.numeric(format(time, "%m"))
                    dat <- ncvar_get(nc, gregor_and_fay_2021_varnames[vi])*12.0107/1e3/1e12 # molC->gC, gC->kgC, kgC->PgC
                    if (gregor_and_fay_2021_varnames[vi] != "fgco2_ens_sd") {
                        dat <- dat*-1 # uptake<0->uptake>0
                    }
                    tmpv <- list(vals=dat, size=dim(dat), 
                                 unit=ncatt_get(nc, gregor_and_fay_2021_varnames[vi])$units,
                                 col=gregor_and_fay_2021_col, label=gregor_and_fay_2021_labels[vi])
                    if (names(gregor_and_fay_2021)[modei] == "Jan-Dec") {
                        tmpv <- list(file=f, dimnames="time", dims=list(time=time), data=tmpv)
                    } else if (names(gregor_and_fay_2021)[modei] == "annual") {
                        tmpv <- list(file=f, dimnames="year", dims=list(year=years), data=tmpv)
                    } else if (names(gregor_and_fay_2021)[modei] == "ymonmean") {
                        tmpv <- list(file=f, dimnames="month", dims=list(month=months), data=tmpv)
                    }
                    tmpa[[length(tmpa)+1]] <- tmpv
                    names(tmpa)[length(tmpa)] <- gregor_and_fay_2021_varnames[vi]
                    if (gregor_and_fay_2021_varnames[vi] == "fgco2_ens_min") { # min-->max due to *-1 
                        names(tmpa)[length(tmpa)] <- "fgco2_ens_max"
                    }
                    if (gregor_and_fay_2021_varnames[vi] == "fgco2_ens_max") {
                        names(tmpa)[length(tmpa)] <- "fgco2_ens_min"
                    }
                } # if f exists
            } # for vi
            if (length(tmpa) > 0) {
                gregor_and_fay_2021[[modei]][[ai]] <- tmpa
                names(gregor_and_fay_2021[[modei]])[ai] <- gregor_and_fay_2021_areas[ai]
            }
        } # for ai
    } # for modei
    if (all(sapply(gregor_and_fay_2021, is.null))) {
        message("found zero data --> remove `gregor_and_fay_2021`")
        rm(gregor_and_fay_2021)
    }
} else {
    message("enable here to load post processed gregor_and_fay_2021 data ...")
} # if post processed gregor_and_fay_2021 data


if (T) { # post processed chau_etal_2020 data
    message("\ndisable here if you do not want to load post processed chau_etal_2020 data ...")
    chau_etal_2020_areas <- c("global", "reccap2_atlantic", "reccap2_pacific", "reccap2_indian", "reccap2_arctic", "reccap2_southern")
    chau_etal_2020_varnames <- c("fgco2", "fgco2_uncertainty")
    chau_etal_2020_labels <- c(paste0("C20", plus_minus_symbol, "sd"), "C20 uncert")
    chau_etal_2020_col <- mycols(3)[3]
    chau_etal_2020 <- vector("list", l=3)
    names(chau_etal_2020) <- c("Jan-Dec", "annual", "ymonmean")
    for (modei in seq_along(chau_etal_2020)) {
        for (ai in seq_along(chau_etal_2020_areas)) {
            tmpa <- list()
            for (vi in seq_along(chau_etal_2020_varnames)) {
                f <- paste0(host$workpath, "/post/chau_etal_2020/fldint/", chau_etal_2020_varnames[vi], "/",
                            "chau_etal_2020_chau_etal_2020_fldint_", chau_etal_2020_varnames[vi], "_", 
                            chau_etal_2020_areas[ai], "_", names(chau_etal_2020)[modei], "_1985-2020.nc")
                if (file.exists(f)) {
                    #message("load chau_etal_2020 data from \"", f, "\" ...")
                    nc <- nc_open(f)
                    time <- as.POSIXct(nc$dim$time$vals*3600, o="1950-01-01", tz="UTC")
                    years <- as.numeric(format(time, "%Y"))
                    months <- as.numeric(format(time, "%m"))
                    dat <- ncvar_get(nc, chau_etal_2020_varnames[vi])/1e12*365.25*86400 # kgC->PgC, s-1->yr-1
                    tmpv <- list(vals=dat, size=dim(dat), 
                                 unit=ncatt_get(nc, chau_etal_2020_varnames[vi])$units,
                                 col=chau_etal_2020_col, label=chau_etal_2020_labels[vi])
                    if (names(chau_etal_2020)[modei] == "Jan-Dec") {
                        tmpv <- list(file=f, dimnames="time", dims=list(time=time), data=tmpv)
                    } else if (names(chau_etal_2020)[modei] == "annual") {
                        tmpv <- list(file=f, dimnames="year", dims=list(year=years), data=tmpv)
                    } else if (names(chau_etal_2020)[modei] == "ymonmean") {
                        tmpv <- list(file=f, dimnames="month", dims=list(month=months), data=tmpv)
                    }
                    tmpa[[length(tmpa)+1]] <- tmpv
                    names(tmpa)[length(tmpa)] <- chau_etal_2020_varnames[vi]
                } # if f exists
            } # for vi
            if (length(tmpa) > 0) {
                chau_etal_2020[[modei]][[ai]] <- tmpa
                names(chau_etal_2020[[modei]])[ai] <- chau_etal_2020_areas[ai]
            }
        } # for ai
    } # for modei
    if (all(sapply(chau_etal_2020, is.null))) {
        message("found zero data --> remove `chau_etal_2020`")
        rm(chau_etal_2020)
    }
} else {
    message("enable here to load post processed chau_etal_2020 data ...")
} # if post processed chau_etal_2020 data


# reccap2 data
reccap2_areas <- c("global", "reccap2_atlantic", "reccap2_pacific", "reccap2_indian", "reccap2_arctic", "reccap2_southern")
fs <- paste0("reccap2-ocean_", c(15, 16, 16, 16, 15, 16), 
             "_models_fldint_fgco2_", reccap2_areas, "_Jan-Dec_1958-2019.nc")
fs <- paste0(host$workpath, "/data/reccap2-ocean/", fs)
names(fs) <- reccap2_areas
if (T && any(file.exists(fs))) {
    message("\ndisable here if you do not want to load reccap2 data ...")
    reccap2_varnames <- c("fgco2_an_mean", "fgco2_an_median", "fgco2_an_min", "fgco2_an_max")
    reccap2_labels <- c(paste0("GOBM mmm", plus_minus_symbol, "mmr"), paste0("GOBM mmmed", plus_minus_symbol, "mmr"), "GOBM mmmin", "GOBM mmmax")
    reccap2_cols <- c("black", "black", col2rgba("black", 0.15), col2rgba("black", 0.15))
    reccap2 <- vector("list", l=length(fs))
    names(reccap2) <- names(fs)
    cnt <- 0
    for (fi in seq_along(fs)) {
        f <- fs[fi]
        if (file.exists(f)) {
            cnt <- cnt + 1
            message("load reccap2 data from \"", f, "\" ...")
            reccap2_ncin <- nc_open(f)
            time <- as.POSIXct(reccap2_ncin$dim$time$vals, o="1970-1-1", tz="UTC")
            years <- as.POSIXct(reccap2_ncin$dim$years$vals, o="1970-1-1", tz="UTC")
            years <- as.numeric(format(years, "%Y"))
            tmp <- vector("list", l=length(reccap2_varnames))
            names(tmp) <- reccap2_varnames
            for (vi in seq_along(tmp)) {
                message("   load \"", reccap2_varnames[vi], "\" ...")
                dat <- ncvar_get(reccap2_ncin, reccap2_varnames[vi])
                tmp[[vi]] <- list(vals=dat, size=dim(dat), 
                                  unit=ncatt_get(reccap2_ncin, reccap2_varnames[vi])$units,
                                  col=reccap2_cols[vi], label=reccap2_labels[vi])
                if (length(dat) == length(time)) {
                    tmp[[vi]]$dimnames <- "time"
                } else if (length(dat) == length(years)) {
                    tmp[[vi]]$dimnames <- "years"
                }
            }
            tmp <- list(file=f,
                        dims=list(time=time, years=years),
                        data=tmp)
            reccap2[[cnt]] <- tmp
            names(reccap2)[cnt] <- names(f)
        } # if f exists
    } # for fi
} else {
    message("enable here to load reccap2 data ...")
} # if reccap2 data


# clean work space fom loading special data sets
rmobjs <- c("f", "time", "years", "timelt", "nyears_to_originorigin",
            "pdoi", "tmp", "tmp2", "d", "ghcdn_csv", 
            "Tavg_anTmin_anTmax_anprecip_an", 
            "Tavg_monTmin_monTmax_monprecip_an",
            "cnt", "lonlat", "lon_orig", "lat_orig")
suppressWarnings(rm(list=rmobjs))

message("\n... finished reading special data sets via load_special_data.r")

