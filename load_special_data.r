# r

# todo: solve dependencies from other repos: make_posixlt_origin_function()
#source(paste0(hompath, "/functions/myfunctions.r"))

# mpiom land sea mask segments
if (T) {
    f <- paste0(host$repopath, "/mpiom/mpiom_GR30s_land_sea_mask_segments_lon180.txt")
    #f <- paste0(host$repopath, "/mpiom/mpiom_GR30s_land_sea_mask_segments_lon360.txt")
    if (file.exists(f)) {
        message("\nread mpiom land sea mask segments from ", f, " ...")
        mpiom_GR30_lsm_seg <- read.table(f, header=T)
        add_mpiom_GR30_lsm_seg <- T
        message("set add_mpiom_GR30_lsm_seg=T if you want to add to lon,lat plot")
    } else {
        message("file ", f, " does not exist. run the function mpiom_get_lsm_segments() via ", 
                host$repopath, "/mpiom/run_mpiom.r to generate the land sea mask segments ...")
    }
}

# cmip6 co2 hist
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/pool/data/ECHAM6/input/r0007/greenhouse_historical.nc"
}
if (file.exists(f)) {
    message("\nread hist CO2 from ", f, " ...")
    co2_hist_ncin <- nc_open(f)
    time <- co2_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*365.25*86400, origin="0000-01-01", tz="UTC", format="%Y")
    co2_hist <- list(co2_ppm=ncvar_get(co2_hist_ncin, "CO2"), time=timelt, timen=time,
                     text="", col="red", lty=2, lwd=0.5, pch=NA)
    add_co2_hist <- F
    message("set add_co2_hist=T if you want to add to plot")
}

# cmip6 co2 1pct
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/pool/data/ECHAM6/input/r0008/greenhouse_1pctCO2.nc"
}
if (file.exists(f)) {
    message("\nread 1pct CO2 from ", f, " ...")
    co2_1pct_ncin <- nc_open(f)
    time <- co2_1pct_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*365.25*86400, origin="0000-01-01", tz="UTC", format="%Y")
    co2_1pct <- list(co2_ppm=ncvar_get(co2_1pct_ncin, "CO2"), time=timelt, timen=time,
                     text="", col="#377EB8", lty=2, lwd=0.5, pch=NA)
    add_co2_1pct <- F
    message("set add_co2_1pct=T if you want to add to plot")
}

# cmip6 4CO2 
co2_4co2 <- list(co2_ppm=1137.2679,
                 text="", col="#1B9E77", lty=2, lwd=0.5, pch=NA)
message("\nset 4CO2 to ", co2_4co2$co2_ppm, " ppm") 
add_co2_4co2 <- F
message("set add_co2_4co2=T if you want to add to plot")

# koehler et al. 2017 ghg by paul
f <- ""
if (host$machine_tag == "stan") {
    f <- "/ace/user/pgierz/cosmos-aso-wiso/Hol-T/scripts/Koehler_GHG_forcing_0.001ka_resolution.dat"
} else if (host$machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/data/koehler_etal_2017/Koehler_GHG_forcing_0.001ka_resolution.dat"
}
if (file.exists(f)) {
    message("\nread koehler et al. 2017 ghg forcing from ", f, " from paul ...")
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
    add_koehler_etal_2017_paul <- T
    message("set add_koehler_etal_2017_paul=", !add_koehler_etal_2017_paul, 
            " if you ", ifelse(add_koehler_etal_2017_paul, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
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
    message("\nread koehler et al. 2017 ghg forcing from ", f, " from ", from, " to ", to, " ...")
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
}

# cmip6 historical monthly total solar irradiance 
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/cmip6/solar_irradiance/swflux_14band_monthly_1850-2014.nc"
}
if (file.exists(f)) {
    message("\nread historical monthly total solar irradiance from ", f, " ...")
    tsi_hist_ncin <- nc_open(f)
    time <- tsi_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*86400, origin="1850-01-01", tz="UTC")
    tsi_hist_monthly <- list(tsi_hist=ncvar_get(tsi_hist_ncin, "TSI"), time=timelt, timen=time,
                             col="orange2",
                             col_rgb=rgb(t(col2rgb("orange2")/255), alpha=0.5),
                             text="TSI", lty=1, lwd=0.5, pch=NA) 
    add_tsi_hist <- F
    message("set add_tsi_hist=T if you want to add to plot")
}

# cmip6 historical annual total solar irradiance 
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/cmip6/solar_irradiance/swflux_14band_annual_1850-2014.nc"
}
if (file.exists(f)) {
    message("\nread historical monthly total solar irradiance from ", f, " ...")
    tsi_hist_ncin <- nc_open(f)
    time <- tsi_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*86400, origin="1850-01-01", tz="UTC")
    tsi_hist_annual <- list(tsi_hist=ncvar_get(tsi_hist_ncin, "TSI"), time=timelt, timen=time,
                            col="orange2",
                            col_rgb=rgb(t(col2rgb("orange2")/255), alpha=0.5),
                            text="TSI", lty=1, lwd=0.5, pch=NA) 
    add_tsi_hist_annual <- F
    message("set add_tsi_hist_annual=T if you want to add to plot")
}

# hadcrut4 global monthly temperature anomaly wrt 1961-1990
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/HadCRUT4/HadCRUT_global_SAT_anomaly_wrt_1961-1990_HadCRUT.4.6.0.0.monthly_ns_avg.txt.nc"
}
if (file.exists(f)) {
    message("\nread hadcrut4 global monthly SAT anomalies wrt to 1961-1990 from ", f, " ...")
    hadcrut4_ncin <- nc_open(f)
    time <- hadcrut4_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    hadcrut4_sat_anom_monthly <- list(hadcrut4_sat_anom=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990"),
                                      hadcrut4_sat_anom_lower_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_lower_uncertainty"),
                                      hadcrut4_sat_anom_upper_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_upper_uncertainty"),
                                      time=timelt, timen=time)
    add_hadcrut4_sat_anom_monthly <- F
    message("set add_hadcrut4_sat_anom_monthly=T if you want to add to plot")
}

# hadcrut4 global annual temperature anomaly wrt 1961-1990
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/HadCRUT4/HadCRUT_global_SAT_anomaly_wrt_1961-1990_HadCRUT.4.6.0.0.annual_ns_avg.txt.nc"
}
if (file.exists(f)) {
    message("\nread hadcrut4 global annual SAT anomalies wrt to 1961-1990 from ", f, " ...")
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
} # hadcrut4 global annual temperature anomaly wrt 1961-1990

# marcott et al temperature anomaly wrt 1961-1990
if (F) {
    f <- ""
    if (host$machine_tag == "paleosrv") {
        f <- "/isibhv/projects/paleo_work/cdanek/data/marcott_etal_2013/Marcott.SM.database.S1.xlsx"
        source("/isibhv/projects/paleo_work/cdanek/data/marcott_etal_2013/read_marcott_etal_2013_function.r")
    }
    if (file.exists(f)) {
        message("\ndisable here if you do not want to read marcott et al. 2013 temperature anomalies wrt to 1961-1990 from ", f, " ...")
        marcott_etal_2013 <- read_marcott_etal_2013_function(f)
        time <- marcott_etal_2013$year_before_1950 # -50 -30 -10  10 ... 11210 11230 11250 11270 11290
        time <- rev(-1*time) # -11290 -11270 -11250 -11230 -11210 -11190 -11170 ... -70 -50 -30 -10  10  30  50
        timelt <- make_posixlt_origin_function(time, origin_in=1950, origin_out=1950, verbose=0)
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
    } # marcott et al. 2013
} else {
    message("\nenable here if you want to read marcott et al. 2013 temperature anomalies wrt to 1961-1990")
}

# gistempv4 global annual temperature anomaly wrt 1951-1980
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/GISTEMPv4/GISTEMPv4_global_SAT_anomaly_wrt_1951-1980_GLB.Ts+dSST.csv.nc"
}
if (file.exists(f)) {
    message("\nread gistempv4 global annual SAT anomalies wrt to 1951-1980 from ", f, " ...")
    gistempv4_ncin <- nc_open(f)
    time <- gistempv4_ncin$dim$time$vals # YYYY
    timelt <- as.POSIXlt(as.Date(paste0(time, "-06-30")), tz="UTC") # use mid-year
    gistempv4_sat_anom_annual <- list(gistempv4_sat_anom=ncvar_get(gistempv4_ncin, "global_SAT_anomaly_wrt_1951-1980"),
                                      time=timelt, timen=time)
    add_gistempv4_sat_anom_annual <- F
    message("set add_gistempv4_sat_anom_annual=T if you want to add to plot")
}

# rapid moc
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/RAPID/moc_transports.nc"
    f_err <- "/work/ba0941/a270073/data/RAPID/moc_error.mat"
}
if (file.exists(f)) {
    message("\nread rapid moc from ", f, " ...")
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
        library(R.matlab)
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
}

# monthly nsidc sea ice index northern hemisphere
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/NSIDC/sea_ice_index/data/N_seaice_extent_monthly_v3.0.nc"
}
if (file.exists(f)) {
    message("\nread northern nsidc sea ice index from ", f, " ...")
    nsidc_ncin <- nc_open(f)
    time <- nsidc_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    nsidc_siarean_monthly <- list(siarean=ncvar_get(nsidc_ncin, "siarean"),
                                  time=timelt, timen=time,
                                  text="NSIDC north", col="black",
                                  lty=1, lwd=1.5, pch=NA)
    add_nsidc_arctic_monthly <- F
    message("set add_nsidc_arctic_monthly=T if you want to add to plot")
}

# monthly nsidc sea ice index southern hemisphere
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/NSIDC/sea_ice_index/data/S_seaice_extent_monthly_v3.0.nc"
}
if (file.exists(f)) {
    message("\nread southern nsidc sea ice index from ", f, " ...")
    nsidc_ncin <- nc_open(f)
    time <- nsidc_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    nsidc_siareas_monthly <- list(siareas=ncvar_get(nsidc_ncin, "siareas"),
                                  time=timelt, timen=time,
                                  text="NSIDC south", col="black",
                                  lty=3, lwd=1.5, pch=NA)
    add_nsidc_antarctic_monthly <- F
    message("set add_nsidc_antarctic_monthly=T if you want to add to plot")
}

# annual nsidc sea ice index northern hemisphere
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/NSIDC/sea_ice_index/data/N_seaice_extent_annual_v3.0.nc"
}
if (file.exists(f)) {
    message("\nread northern nsidc sea ice index from ", f, " ...")
    nsidc_ncin <- nc_open(f)
    time <- nsidc_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    nsidc_siarean_annual <- list(siarean=ncvar_get(nsidc_ncin, "siarean"),
                                 time=timelt, timen=time,
                                 text="NSIDC north", col="black",
                                 lty=1, lwd=1.5, pch=NA)
    # remove first year 1978 due to data only from 3 months OND
    message("remove first year 1978")
    nsidc_siarean_annual$siarean <- nsidc_siarean_annual$siarean[-1]
    nsidc_siarean_annual$time <- nsidc_siarean_annual$time[-1]
    nsidc_siarean_annual$timen <- nsidc_siarean_annual$timen[-1]
    add_nsidc_arctic_annual <- F
    message("set add_nsidc_arctic_annual=T if you want to add to plot")
}

# annual nsidc sea ice index southern hemisphere
f <- ""
if (host$machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/NSIDC/sea_ice_index/data/S_seaice_extent_annual_v3.0.nc"
}
if (file.exists(f)) {
    message("\nread southern nsidc sea ice index from ", f, " ...")
    nsidc_ncin <- nc_open(f)
    time <- nsidc_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    nsidc_siareas_annual <- list(siareas=ncvar_get(nsidc_ncin, "siareas"),
                                 time=timelt, timen=time,
                                 text="NSIDC south", col="black",
                                 lty=3, lwd=1.5, pch=NA)
    # remove first year 1978 due to data only from 3 months OND
    message("remove first year 1978")
    nsidc_siareas_annual$siareas <- nsidc_siareas_annual$siareas[-1]
    nsidc_siareas_annual$time <- nsidc_siareas_annual$time[-1]
    nsidc_siareas_annual$timen <- nsidc_siareas_annual$timen[-1]
    add_nsidc_antarctic_annual <- F
    message("set add_nsidc_antarctic_annual=T if you want to add to plot")
}

# berger holocene accelerated orbital parameter from paul
if (F) {
    f <- ""
    if (host$machine_tag == "paleosrv") {
        f <- "/scratch/simulation_database/incoming/Hol-Tx10/script/HOL_ORB_forcing_0.01ka_resolution_combined.dat"
    }
    if (file.exists(f)) {
        message("\ndisable here if you do not want to read pauls accelerated berger orbital parameters from ", f, " ...")
        orb_berger_acc <- read.table(f, col.names=c("year_before_1950", "eccentricity", "precessionobliquity"))
        years <- orb_berger_acc$year_before_1950 # kyr before 1950 --> 7.00 6.99 6.98 6.97 ... 0.03 0.02 0.01 0.00
        years <- -1*years*1000 # --> -7000 -6990 -6980 -6970 -6960 ... -40 -30 -20 -10   0
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
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
    } # berger holocene accelerated orbital parameter from paul
} else {
    message("\nenable here if you want to read pauls accelerated berger orbital parameters")
}

# berger holocene transient orbital parameter from paul
if (F) {
    f <- ""
    if (host$machine_tag == "stan") {
        f <- "/ace/user/pgierz/cosmos-aso-wiso/Hol-T/scripts/Berger_ORB_forcing_0.001ka_resolution.dat"
    } else if (host$machine_tag == "paleosrv") {
        f <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-T/scripts/Berger_ORB_forcing_0.001ka_resolution.dat"
    }
    if (file.exists(f)) {
        message("\ndisable here if you do not want to read pauls transient berger orbital parameters from ", f, " ...")
        orb_berger <- read.table(f, col.names=c("year_before_1950", "eccentricity", "precessionobliquity"))
        years <- orb_berger$year_before_1950 # kyr before 1950 --> 6.999, 6.998, 6997, ...
        years <- -1*years*1000 # --> -6999, -6998, -6997, ...
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
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
    } # berger holocene transient orbital parameter from paul
} else {
    message("\nenable here if you want to read pauls transient berger orbital parameters")
}

# berger orbital parameter for last 800ka
if (F) {
    f <- ""
    if (host$machine_tag == "stan") {
        f <- "/home/ace/cdanek/scripts/fortran/berger_1978/berger_1978_years_-800_to_0_kyears_before_1950.txt"
    }
    if (file.exists(f)) {
        message("\ndisable here if you do not want to read my berger orbital parameters from ", f, " ...")
        my_orb_berger <- read.table(f, header=T)
        # column 1: kyear_from_1950 2: ecc 3: obl_deg 4: calendar_day_of_perihelion 5: angle_of_perihelion_deg_from_vernal_equinox
        years <- my_orb_berger$kyear_from_1950 # kyr before 1950 in reverse order --> -800, -799, -798, ...
        years <- years*1000 # -800000, -799000, -798000, ...
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
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
        if (F) { # plot berger
            message("plot ...")
            xlim <- range(unclass(my_orb_berger$time)$year + 1900)
            xat <- pretty(xlim, n=20)
            if (any(!(xat %in% xlim))) {
                out_inds <- which(xat > max(xlim))
                if (length(out_inds) > 0) xat <- xat[-out_inds]
                out_inds <- which(xat < min(xlim))
                if (length(out_inds) > 0) xat <- xat[-out_inds]
            }
            png("~/berger.png", width=4000, height=8000, res=400)
            par(mfrow=c(4, 1))
            # ecc
            plot(unclass(my_orb_berger$time)$year + 1900, my_orb_berger$eccentricity, t="l", 
                 xaxt="n", yaxt="n", xlab=paste0("kyear before ", origin_out), ylab="Eccentricity")
            axis(1, at=xat, labels=abs(xat)/1000)
            axis(2, at=pretty(my_orb_berger$eccentricity, n=8), las=2)
            # obl
            plot(unclass(my_orb_berger$time)$year + 1900, my_orb_berger$obliquity, t="l", 
                 xaxt="n", yaxt="n", xlab=paste0("kyear before ", origin_out), ylab="Obliquity [deg]")
            axis(1, at=xat, labels=abs(xat)/1000)
            axis(2, at=pretty(my_orb_berger$obliquity, n=8), las=2)
            # calendar_day_of_perihelion
            plot(unclass(my_orb_berger$time)$year + 1900, my_orb_berger$calendar_day_of_perihelion, t="l", 
                 xaxt="n", yaxt="n", xlab=paste0("kyear before ", origin_out), ylab="Calendar day of perihelion")
            axis(1, at=xat, labels=abs(xat)/1000)
            axis(2, at=pretty(my_orb_berger$calendar_day_of_perihelion, n=8), las=2)
            # angle_of_perihelion_deg_from_vernal_equinox
            plot(unclass(my_orb_berger$time)$year + 1900, my_orb_berger$angle_of_perihelion_deg_from_vernal_equinox, t="l", 
                 xaxt="n", yaxt="n", xlab=paste0("kyear before ", origin_out), ylab="Angle of perihelion [deg from v.e.]")
            axis(1, at=xat, labels=abs(xat)/1000)
            axis(2, at=pretty(my_orb_berger$angle_of_perihelion_deg_from_vernal_equinox, n=8), las=2)
            dev.off()
        }
    }
} else {
    message("\nenable here if you want to read my berger orbital parameters")
}

# laskar orbital parameter for last 800ka
if (F) {
    f <- ""
    if (host$machine_tag == "stan") {
        f <- "/home/ace/cdanek/scripts/fortran/laskar_etal_2004/laskar_etal_2004_years_-800_to_0_kyears_before_2000.txt"
    } else if (host$machine_tag == "paleosrv") {
        f <- "/home/csys/cdanek/scripts/fortran/laskar_etal_2004/laskar_etal_2004_years_-800_to_0_kyears_before_2000.txt"
    }
    if (file.exists(f)) {
        message("\ndisable here if you do not want to read laskar orbital parameters from ", f, " ...")
        my_orb_laskar <- read.table(f, header=T)
        # column 1: kyear_from_1950 2: ecc 3: obl_deg 4: angle_of_perihelion_deg_from_vernal_equinox
        years <- my_orb_laskar$kyear_from_2000 # kyr before 2000 in reverse order --> -800, -799, -798, ..., -3, -2, -1,  0
        years <- years*1000 # -800000, -799000, -798000, ..., -3000, -2000, -1000,  0
        timelt <- make_posixlt_origin_function(years, origin_in=2000, origin_out=1950, verbose=0)
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
        if (F) { # plot laskar
            message("plot ...")
            xlim <- range(unclass(my_orb_laskar$time)$year + 1900)
            xat <- pretty(xlim, n=20)
            if (any(!(xat %in% xlim))) {
                out_inds <- which(xat > max(xlim))
                if (length(out_inds) > 0) xat <- xat[-out_inds]
                out_inds <- which(xat < min(xlim))
                if (length(out_inds) > 0) xat <- xat[-out_inds]
            }
            png("~/laskar.png", width=4000, height=6000, res=400)
            par(mfrow=c(3, 1))
            # ecc
            plot(unclass(my_orb_laskar$time)$year + 1900, my_orb_laskar$eccentricity, t="l", 
                 xaxt="n", yaxt="n", xlab=paste0("kyear before ", origin_out), ylab="Eccentricity")
            axis(1, at=xat, labels=abs(xat)/1000)
            axis(2, at=pretty(my_orb_laskar$eccentricity, n=8), las=2)
            # obl
            plot(unclass(my_orb_laskar$time)$year + 1900, my_orb_laskar$obliquity, t="l", 
                 xaxt="n", yaxt="n", xlab=paste0("kyear before ", origin_out), ylab="Obliquity [deg]")
            axis(1, at=xat, labels=abs(xat)/1000)
            axis(2, at=pretty(my_orb_laskar$obliquity, n=8), las=2)
            # angle_of_perihelion_deg_from_vernal_equinox
            plot(unclass(my_orb_laskar$time)$year + 1900, my_orb_laskar$angle_of_perihelion_deg_from_vernal_equinox, t="l", 
                 xaxt="n", yaxt="n", xlab=paste0("kyear before ", origin_out), ylab="Angle of perihelion [deg from v.e.]")
            axis(1, at=xat, labels=abs(xat)/1000)
            axis(2, at=pretty(my_orb_laskar$angle_of_perihelion_deg_from_vernal_equinox, n=8), las=2)
            dev.off()
        }
    }
} else {
    message("\nenable here if you want to read laskar orbital parameters")
}

if (F) { # compare koehler et al. 2017 vs paul
    message("\ncompare pauls and koehlers et al. 2017 CO2 ...")
    xlim <- range(unclass(koehler_etal_2017$time)$year + 1900)
    xlim <- range(xlim, unclass(koehler_etal_2017_paul$time)$year + 1900)
    xat <- pretty(xlim, n=20)
    if (any(!(xat %in% xlim))) {
        out_inds <- which(xat > max(xlim))
        if (length(out_inds) > 0) xat <- xat[-out_inds]
        out_inds <- which(xat < min(xlim))
        if (length(out_inds) > 0) xat <- xat[-out_inds]
    }
    png("~/koehler_etal_2017_vs_paul.png", width=4000, height=2500, res=400)
    par(mar=c(5.1, 6.1, 4.1, 6.1) + 0.1)
    # co2
    ylim <- range(koehler_etal_2017$co2, koehler_etal_2017_paul$co2)
    plot(unclass(koehler_etal_2017$time)$year + 1900, koehler_etal_2017$co2, t="l", 
         xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", 
         xlab="kyear before 1950", ylab="CO2 [µmol/mol]")
    axis(1, at=xat, labels=abs(xat)/1000)
    axis(2, at=pretty(ylim, n=8), las=2)
    lines(unclass(koehler_etal_2017_paul$time)$year + 1900, koehler_etal_2017_paul$co2, col="red")
    legend("topleft", c("Köhler et al. 2017", "Köhler et al. 2017 (paul)"), 
           col=c("black", "red"), lty=1, bty="n")
    # co2 differences
    par(new=T)
    ydiff <- koehler_etal_2017_paul$co2 - koehler_etal_2017$co2
    ylim <- range(ydiff)
    plot(unclass(koehler_etal_2017$time)$year + 1900, ydiff,
         t="l", col="blue",
         axes=F, xlab=NA, ylab=NA)
    abline(h=0, col="blue", lwd=0.5)
    axis(4, at=pretty(ylim, n=15), las=2, col="blue", col.axis="blue", col.ticks="blue")
    mtext(side=4, "Difference Paul minus Köhler [µmol/mol]", line=4.5, cex=0.9, col="blue")
    dev.off()
} else {
    message("\nenable here if you want to compare pauls and koehlers et al. 2017 CO2")
} # compare koehler et al. 2017 vs paul

if (F) { # compare berger and laskar orb
    message("\nplot berger vs laskar ...")
    xlim <- range(unclass(my_orb_laskar$time)$year + 1900)
    xlim <- range(xlim, unclass(my_orb_berger$time)$year + 1900)
    xat <- pretty(xlim, n=20)
    if (any(!(xat %in% xlim))) {
        out_inds <- which(xat > max(xlim))
        if (length(out_inds) > 0) xat <- xat[-out_inds]
        out_inds <- which(xat < min(xlim))
        if (length(out_inds) > 0) xat <- xat[-out_inds]
    }
    png("~/berger_vs_laskar.png", width=4000, height=7500, res=400)
    par(mfrow=c(3, 1))
    par(mar=c(5.1, 6.1, 4.1, 6.1) + 0.1)
    for (i in 1:3) {
        if (i == 1) {
            ylab <- "Obliquity [deg]"
            yberger <- my_orb_berger$obliquity
            ylaskar <- my_orb_laskar$obliquity
        } else if (i == 2) {
            ylab <- "Eccentricity"
            yberger <- my_orb_berger$eccentricity
            ylaskar <- my_orb_laskar$eccentricity
        } else if (i == 3) {
            ylab <- "Anlge of perihelion [deg]"
            yberger <- my_orb_berger$angle_of_perihelion_deg_from_vernal_equinox
            ylaskar <- my_orb_laskar$angle_of_perihelion_deg_from_vernal_equinox
        }
        # obl
        ylim <- range(yberger, ylaskar)
        plot(unclass(my_orb_berger$time)$year + 1900, yberger, t="l", 
             xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", 
             xlab=paste0("kyear before ", origin_out), ylab=ylab)
        axis(1, at=xat, labels=abs(xat)/1000)
        axis(2, at=pretty(ylim, n=8), las=2)
        lines(unclass(my_orb_laskar$time)$year + 1900, ylaskar, col="red")
        legend("bottomleft", c("Berger", "Laskar"), col=c("black", "red"), lty=1, bty="n")
        # obl differences
        par(new=T)
        ydiff <- yberger - ylaskar
        ylim <- range(ydiff)
        plot(unclass(my_orb_berger$time)$year + 1900, ydiff,
             t="l", col="blue",
             axes=F, xlab=NA, ylab=NA)
        abline(h=0, col="blue", lwd=0.5)
        axis(4, at=pretty(ylim, n=15), las=2, col="blue", col.axis="blue", col.ticks="blue")
        mtext(side=4, "Difference Berger minus Laskar", line=4.5, cex=0.9, col="blue")
    }
    dev.off()
} else {
    message("\nenable here if you want to compare berger and laskar orbital parameter")
} # comapre berger and laskar orb

# PLOT coords as eval list
if (T) {
    f <- "~/scripts/r/PLOT/lakes/lake_coords.txt"
    if (file.exists(f)) {
        message("\nread PLOT lake coords from ", f, " and save in `PLOT_coords_cmd_list` ...")
        lakes_table <- read.table(f, header=T, stringsAsFactors=F)
        PLOT_coords_cmd_list <- NULL
        lakes <- c("ladoga", "shuchye", "kotokel", "emanda", "two-jurts", "elgygytgyn")
        for (lakei in seq_along(lakes)) {
            cmd <- "text("
            cmd <- paste0(cmd, "x=", lakes_table[which(lakes_table$name == lakes[lakei]),"lon_dec"])
            cmd <- paste0(cmd, ", y=", lakes_table[which(lakes_table$name == lakes[lakei]),"lat_dec"])
            cmd <- paste0(cmd, ", labels=\"", LETTERS[lakei], "\", cex=1")
            cmd <- paste0(cmd, ") # ", lakes[lakei])
            PLOT_coords_cmd_list[[lakei]] <- cmd 
        }
    } # if file.exists(f)
} # of load PLOT coords

# hanno meyer et al. PLOT excel sheet
if (F) {
    f <- ""
    if (host$machine_tag == "paleosrv") {
        f <- "/isibhv/projects/paleo_work/cdanek/data/meyer_etal/PLOT-project_Lacustrine diatom oxygen isotope_Kotokel.xlsx"
        source("/isibhv/projects/paleo_work/cdanek/data/meyer_etal/read_meyer_etal_function.r")
    }
    if (file.exists(f)) {
        message("\ndisable here if you do not want to read hanno meyer et al. PLOT data from ", f)
        message("run read_meyer_etal_function() ...")
        #tmp <- read_meyer_etal_function(xlsx_file=f)
        #tmp <- read_meyer_etal_function(xlsx_file=f, year_from=-7000, verbose=F)
        tmp <- read_meyer_etal_function(xlsx_file=f, year_from=-10000, verbose=F)
        #tmp <- read_meyer_etal_function(xlsx_file=f, year_from=-7000, sheets_wanted="Lake Ladoga")
        #tmp <- read_meyer_etal_function(xlsx_file=f, sheets_wanted="Lake Bolshoye Shchuchye unpubl.")
        #tmp <- read_meyer_etal_function(xlsx_file=f, sheets_wanted="Lake Emanda unpubl.")
        #tmp <- read_meyer_etal_function(xlsx_file=f, sheets_wanted="El'gygytgyn Lake")
        #tmp <- read_meyer_etal_function(xlsx_file=f, sheets_wanted="Two Jurts Lake", year_from=-7000, verbose=T)
        #tmp <- read_meyer_etal_function(xlsx_file=f, sheets_wanted="Lake Kotokel", verbose=F)
        meyer_etal <- list(data=tmp,
                           type="o", 
                           #col="#E41A1C", # myred
                           col="#377EB8", # myblue
                           #col="#1B9E77", # mygreen
                           lty=1, lwd=1, pch=1, cex=1)
        if (F) { # plot meyer et al data
            for (i in seq_along(meyer_etal$data)) {
                plotname <- gsub(" ", "_", names(meyer_etal$data)[i])
                plotname <- gsub("[[:punct:]]", "_", plotname)
                plotname <- paste0(plotname, "_", paste(range(meyer_etal$data[[i]]$data$timelt$year+1900), collapse="_to_"))
                plotname <- paste0(dirname(f), "/", plotname, ".png")
                message("save ", plotname, " ...")
                png(plotname, width=p$ts_width, height=p$ts_height, res=p$dpi)
                par(mar=c(5.1, 5.1, 4.1, 2.1))
                plot(meyer_etal$data[[i]]$data$time, meyer_etal$data[[i]]$data$d18o_corr_perm,
                     t="o", xaxt="n", yaxt="n",
                     xlab="year from 1950", ylab=NA)
                title(names(meyer_etal$data)[i])
                axis.POSIXct(1, at=pretty(meyer_etal$data[[i]]$data$time, n=20))
                axis(2, at=pretty(meyer_etal$data[[i]]$data$d18o_corr_perm, n=8), las=2)
                mtext(side=2, line=3, expression(paste(delta^{18}, "O diatom (‰)")))
                dev.off()
            } # for all excel sheets
            #stop("asd")
        } # if plot meyer et al data
    } # if file.exists(f)
} else {
    message("\nenable here if you want to read hanno meyer et al. PLOT data excel sheet")
}

if (F) { # ladoga; kostrova et al. 2019: https://doi.pangaea.de/10.1594/PANGAEA.899329
    library(pangaear)
    pdoi <- "10.1594/PANGAEA.899329" 
    dat_kostrova_etal_2019 <- pg_data(doi=pdoi)
    years <- dat_kostrova_etal_2019[[1]]$data$"Cal age [ka BP]" # 0.203  0.793  1.373  1.939 ... 9.662 10.062 10.428 10.750
    years <- -1*rev(years*1000) # -10750 -10428 -10062  -9662 ... -1939  -1373   -793   -203
    timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
    kostrova_etal_2019 <- list(time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                               d18o=rev(dat_kostrova_etal_2019[[1]]$data$"Diatoms δ18O [‰ SMOW] (Contamination corrected)"),
                               text="Kostrova et al. 2019", 
                               type="o", col="#377EB8",
                               lty=1, lwd=1, pch=1, cex=1)
}

if (F) { # elgygytgyn; swann et al. 2010: https://doi.pangaea.de/10.1594/PANGAEA.856095
    pdoi <- "10.1016/j.quascirev.2009.11.024"
}

save_PaTh_list <- F
if (save_PaTh_list) { # load all Pa/Th into one list
    dat_PaTh_all <- list()
}

if (F) { # 231Pa/230Th of ng et al. 2018
    library(pangaear)
    pdoi <- c("10.1594/PANGAEA.890927", "10.1594/PANGAEA.890928", "10.1594/PANGAEA.890929", "10.1594/PANGAEA.890930") 
    message("\nload Pa/Th data from ng et al. 2018 ...")
    data <- vector("list", l=length(pdoi))
    for (i in seq_along(pdoi)) {
        tmp <- pg_data(doi=pdoi[i])
        years <- tmp[[1]]$data[["Age [ka BP]"]] # 4.425  5.547  7.656  8.568  9.763 10.686  ... 21.648 22.251 23.141 24.210
        PaTh <- tmp[[1]]$data[["231Pa/230Th xs,0"]]
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                PaTh <- rev(PaTh)
            }
        }
        years <- years*-1000 # -24210 -23141 -22251 -21648 -20257 ... -11376 -10686  -9763  -8568  -7656  -5547  -4425 
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=as.numeric(tmp[[1]]$metadata$events$LONGITUDE),
                          lat=as.numeric(tmp[[1]]$metadata$events$LATITUDE),
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "Pa/Th"=PaTh,
                          id=names(tmp[[1]]$metadata$events)[1],
                          text="Ng et al. 2018")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    }
    dat_ng_etal_2018 <- list(data=data, 
                             type="o", col="#377EB8",
                             lty=1, lwd=1, pch=1, cex=1)
    if (save_PaTh_list) dat_PaTh_all[[length(dat_PaTh_all)+1]] <- dat_ng_etal_2018
} # 231Pa/230Th of ng et al. 2018

if (F) {  # 231Pa/230Th of gherardi et al. 2009
    library(pangaear)
    pdoi <- "10.1594/PANGAEA.760022"
    tmp <- pg_data(pdoi) # "Age [ka BP]", "231Pa/230Th xs,0"
    tmp_names <- unique(tmp[[1]]$data$Event)
    message("\nload Pa/Th data from gherardi et al. 2009 ...")
    if (any(tmp_names == "MD95-2037")) {
        message("use MD95-2037 of lippold et al. 2012 instead of gherardi et al. 2009")
        tmp_names <- tmp_names[-which(tmp_names == "MD95-2037")]
    }
    if (any(tmp_names == "SU90-44")) {
        message("use SU90-44 of lippold et al. 2012 instead of gherardi et al. 2009")
        tmp_names <- tmp_names[-which(tmp_names == "SU90-44")]
    }
    data <- vector("list", l=length(tmp_names))
    for (i in seq_along(tmp_names)) {
        inds <- which(tmp[[1]]$data$Event == tmp_names[i])
        if (length(inds) == 0) stop("this should not happen")
        if (tmp_names[i] == "SU90-44") {
            lon <- -17.910000; lat <- 50.103333
        } else if (tmp_names[i] == "MD95-2027") {
            lon <- -47.413200; lat <- 41.744500
        } else if (tmp_names[i] == "MD95-2037") {
            lon <- -32.031167; lat <- 37.087167  
        } else {
            stop("this should not happen")
        }
        years <- tmp[[1]]$data[["Age [ka BP]"]][inds]
        PaTh <- tmp[[1]]$data[["231Pa/230Th xs,0"]][inds]
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                PaTh <- rev(PaTh)
            }
        }
        years <- years*-1000
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=lon, lat=lat,
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "Pa/Th"=PaTh,
                          id=tmp_names[i],
                          text="Gherardi et al. 2009")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    }
    dat_gherardi_etal_2009 <- list(data=data, 
                                   type="o", col="#377EB8",
                                   lty=1, lwd=1, pch=1, cex=1)
    if (save_PaTh_list) dat_PaTh_all[[length(dat_PaTh_all)+1]] <- dat_gherardi_etal_2009
} # 231Pa/230Th gherardi et al. 2009 

if (F) { # 231Pa/230Th of süfke et al. 2019
    library(pangaear)
    pdoi <- c("10.1594/PANGAEA.908149", "10.1594/PANGAEA.908150", "10.1594/PANGAEA.908151", "10.1594/PANGAEA.908152",
              "10.1594/PANGAEA.908153", # updated GeoB1523-1 of lippold et al. 2016
              "10.1594/PANGAEA.908154") # updated KNR140-12JPC of lippold et al. 2016
    data <- vector("list", l=length(pdoi))
    message("\nload Pa/Th data from süfke et al. 2019 ...")
    for (i in seq_along(pdoi)) {
        tmp <- pg_data(doi=pdoi[i])
        if (names(tmp[[1]]$metadata$events)[1] == "KNR140-12JPC (KNR140-2-12JPC)") {
            message("use shorter name \"KNR140-12JPC\" instead of \"", names(tmp[[1]]$metadata$events)[1], "\"")
            names(tmp[[1]]$metadata$events)[1] <- "KNR140-12JPC"
        }
        years <- tmp[[1]]$data[["Age [ka BP]"]] # 4.425  5.547  7.656  8.568  9.763 10.686  ... 21.648 22.251 23.141 24.210
        PaTh <- tmp[[1]]$data[["231Pa/230Th"]]
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                PaTh <- rev(PaTh)
            }
        }
        years <- years*-1000 # -24210 -23141 -22251 -21648 -20257 ... -11376 -10686  -9763  -8568  -7656  -5547  -4425 
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=as.numeric(tmp[[1]]$metadata$events$LONGITUDE),
                          lat=as.numeric(tmp[[1]]$metadata$events$LATITUDE),
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "Pa/Th"=PaTh,
                          id=names(tmp[[1]]$metadata$events)[1],
                          text="Süfke et al. 2019")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    }
    dat_süfke_etal_2019 <- list(data=data, 
                                type="o", col="#377EB8",
                                lty=1, lwd=1, pch=1, cex=1)
    if (save_PaTh_list) dat_PaTh_all[[length(dat_PaTh_all)+1]] <- dat_süfke_etal_2019
} # 231Pa/230Th of süfke et al. 2019

if (F) { # 231Pa/230Th of süfke et al. 2019: different data structure than süfke et al. 2019 above
    library(pangaear)
    pdoi <- "10.1594/PANGAEA.908155" # updated GeoB1515-1 of lippold et al. 2016
    data <- vector("list", l=length(pdoi))
    message("\nload Pa/Th data from süfke et al. 2019 ...")
    for (i in seq_along(pdoi)) {
        tmp <- pg_data(pdoi[i]) # "Age [ka BP]", "231Pa/230Th xs,0"
        tmp_names <- unique(tmp[[1]]$data$Event)
        if (length(tmp_names) == 2 && 
            tmp_names[1] == "GeoB1515-1" && tmp_names[2] == "GeoB1516-2") {
            message("for simplicity, name two locations GeoB1515-1 and GeoB1516-2 only GeoB1515-1")
            inds <- which(tmp[[1]]$data$Event == tmp_names[1] | tmp[[1]]$data$Event == tmp_names[2])
            tmp_names <- "GeoB1515-1"
        } else {
            inds <- which(tmp[[1]]$data$Event == tmp_names[i])
        }
        if (length(tmp_names) != 1) stop("found more than one event: ", paste(tmp_names, collapse=","))
        if (length(inds) == 0) stop("this should not happen")
        if (tmp_names[i] == "GeoB1515-1") {
            lon <- -43.666667; lat <- 4.238333
        } else {
            stop("this should not happen")
        }
        years <- tmp[[1]]$data[["Age [ka BP]"]][inds]
        PaTh <- tmp[[1]]$data[["231Pa/230Th"]][inds]
        if (pdoi[i] == "10.1594/PANGAEA.908155") {
            message("instead of erroneous pangaea data:")
            message(paste(PaTh, collapse=","))
            message("use values from palo20807-sup-0001-2019pa003737-ts01.xls:")
            PaTh <- c(0.060,0.047,0.064,0.082,0.090,0.096,0.078,0.070,0.079,0.088,0.073,0.078,0.083,0.081)
            message(paste(PaTh, collapse=","))
            if (length(years) != length(PaTh)) stop("years and PaTh are of different length")
        }
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                PaTh <- rev(PaTh)
            }
        }
        years <- years*-1000
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=lon, lat=lat,
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "Pa/Th"=PaTh,
                          id=tmp_names,
                          text="Süfke et al. 2019")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    } # for i pdoi
    dat_süfke_etal_2019_updated <- list(data=data, 
                                        type="o", col="#377EB8",
                                        lty=1, lwd=1, pch=1, cex=1)
    if (save_PaTh_list) dat_PaTh_all[[length(dat_PaTh_all)+1]] <- dat_süfke_etal_2019_updated
} # 231Pa/230Th of süfke et al. 2019: updated ones of lippold et al. 2016

if (F) { # 231Pa/230Th of lippold et al. 2016
    library(pangaear)
    pdoi <- "10.1594/PANGAEA.863978" 
    tmp <- pg_data(pdoi) # "Longitude", "Latitude", "Age [ka BP]", "(231Pa/230Th)"
    tmp_names <- unique(tmp[[1]]$data$Event)
    message("\nload Pa/Th data from lippold et al. 2016 ...")
    if (T) { # use updated values from süfke et al. 2019
        if (any(tmp_names == "GeoB1515-1")) {
            message("use GeoB1515-1 of süfke et al. 2019 instead of lippold et al. 2016")
            tmp_names <- tmp_names[-which(tmp_names == "GeoB1515-1")]
        }
        if (any(tmp_names == "GeoB1523-1")) {
            message("use GeoB1523-1 of süfke et al. 2019 instead of lippold et al. 2016")
            tmp_names <- tmp_names[-which(tmp_names == "GeoB1523-1")]
        }
        if (any(tmp_names == "KNR140-12JPC")) {
            message("use KNR140-12JPC of süfke et al. 2019 instead of lippold et al. 2016")
            tmp_names <- tmp_names[-which(tmp_names == "KNR140-12JPC")]
        }
    }
    data <- vector("list", l=length(tmp_names))
    for (i in seq_along(tmp_names)) {
        inds <- which(tmp[[1]]$data$Event == tmp_names[i])
        if (length(inds) == 0) stop("this should not happen")
        lon <- unique(tmp[[1]]$data$Longitude[inds])
        if (length(lon) != 1) stop("found more than 1 lon")
        lat <- unique(tmp[[1]]$data$Latitude[inds])
        if (length(lat) != 1) stop("found more than 1 lat")
        years <- tmp[[1]]$data[["Age [ka BP]"]][inds]
        PaTh <- tmp[[1]]$data[["(231Pa/230Th)"]][inds]
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                PaTh <- rev(PaTh)
            }
        }
        years <- years*-1000
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=lon, lat=lat,
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "Pa/Th"=PaTh,
                          id=tmp_names[i],
                          text="Lippold et al. 2016")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    }
    dat_lippold_etal_2016 <- list(data=data, 
                                  type="o", col="#377EB8",
                                  lty=1, lwd=1, pch=1, cex=1)
    if (save_PaTh_list) dat_PaTh_all[[length(dat_PaTh_all)+1]] <- dat_lippold_etal_2016
} # 231Pa/230Th of lippold et al. 2016

if (F) { # 231Pa/230Th of lippold et al. 2012 
    library(pangaear)
    pdoi <- "10.1594/PANGAEA.788550"
    tmp <- pg_data(pdoi) # "Longitude", "Latitude", "Age [ka BP]", "(231Pa/230Th)"
    tmp_names <- unique(tmp[[1]]$data$Event)
    message("\nload Pa/Th data from lippold et al. 2012 ...")
    if (any(tmp_names == "TN057-13")) {
        message("throw out very low Pa/Th values at 53.3° south")
        tmp_names <- tmp_names[-which(tmp_names == "TN057-13")]
    }
    if (any(tmp_names == "V22-182")) {
        message("throw out very high Pa/Th values at -0.533° south")
        tmp_names <- tmp_names[-which(tmp_names == "V22-182")]
    }
    if (any(tmp_names == "177-1089A")) {
        message("use 177-1089A from lippold et al. 2016")
        tmp_names <- tmp_names[-which(tmp_names == "177-1089A")]
    }
    if (any(tmp_names == "M35003-4")) {
        message("use M35003-4 from lippold et al. 2016")
        tmp_names <- tmp_names[-which(tmp_names == "M35003-4")]
    }
    if (any(tmp_names == "GeoB1515-1")) {
        message("use GeoB1515-1 from süfke et al. 2019")
        tmp_names <- tmp_names[-which(tmp_names == "GeoB1515-1")]
    }
    if (any(tmp_names == "GeoB1523-1")) {
        message("use GeoB1523-1 from süfke et al. 2019")
        tmp_names <- tmp_names[-which(tmp_names == "GeoB1523-1")]
    }
    if (any(tmp_names == "KNR140-12JPC")) {
        message("use KNR140-12JPC from süfke et al. 2019")
        tmp_names <- tmp_names[-which(tmp_names == "KNR140-12JPC")]
    }
    data <- vector("list", l=length(tmp_names))
    for (i in seq_along(tmp_names)) {
        inds <- which(tmp[[1]]$data$Event == tmp_names[i])
        if (length(inds) == 0) stop("this should not happen")
        lon <- unique(tmp[[1]]$data$Longitude[inds])
        if (length(lon) != 1) stop("found more than 1 lon")
        lat <- unique(tmp[[1]]$data$Latitude[inds])
        if (length(lat) != 1) stop("found more than 1 lat")
        years <- tmp[[1]]$data[["Age [ka BP]"]][inds]
        PaTh <- tmp[[1]]$data[["231Pa/230Th"]][inds]
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                PaTh <- rev(PaTh)
            }
        }
        years <- years*-1000
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=lon, lat=lat,
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "Pa/Th"=PaTh,
                          id=tmp_names[i],
                          text="Lippold et al. 2012")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    }
    dat_lippold_etal_2012 <- list(data=data, 
                                  type="o", col="#377EB8",
                                  lty=1, lwd=1, pch=1, cex=1)
    if (save_PaTh_list) dat_PaTh_all[[length(dat_PaTh_all)+1]] <- dat_lippold_etal_2012
} # 231Pa/230Th of lippold et al. 2012

if (F) { # 231Pa/230Th of lippold et al. 2009
    library(pangaear)
    pdoi <- "10.1594/PANGAEA.763199"
    tmp <- pg_data(pdoi) 
    tmp_names <- unique(tmp[[1]]$data$Event)
    data <- vector("list", l=length(tmp_names))
    message("\nload Pa/Th data from lippold et al. 2009 ...")
    for (i in seq_along(tmp_names)) {
        inds <- which(tmp[[1]]$data$Event == tmp_names[i])
        if (length(inds) == 0) stop("this should not happen")
        if (tmp_names[i] == "172-1063B") {
            lon <- -57.614940; lat <- 33.686470
        } else if (tmp_names[i] == "172-1063D") {
            lon <- -57.615110; lat <- 33.686190
        } else {
            stop("this should not happen")
        }
        years <- tmp[[1]]$data[["Age [ka BP]"]][inds]
        PaTh <- tmp[[1]]$data[["231Pa/230Th xs,0"]][inds]
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                PaTh <- rev(PaTh)
            }
        }
        years <- years*-1000
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=lon, lat=lat,
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "Pa/Th"=PaTh,
                          id=tmp_names[i],
                          text="Lippold et al. 2009")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    }
    dat_lippold_etal_2009 <- list(data=data, 
                                  type="o", col="#377EB8",
                                  lty=1, lwd=1, pch=1, cex=1)
    if (save_PaTh_list) dat_PaTh_all[[length(dat_PaTh_all)+1]] <- dat_lippold_etal_2009
} # 231Pa/230Th of lippold et al. 2009

if (F) { # 231Pa/230Th of meckler et al. 2013 
    library(pangaear)
    pdoi <- "10.1594/PANGAEA.810309"
    tmp <- pg_data(pdoi) 
    data <- vector("list", l=length(pdoi))
    message("\nload Pa/Th data from meckler et al. 2013 ...")
    for (i in seq_along(pdoi)) {
        tmp <- pg_data(doi=pdoi[i])
        years <- tmp[[1]]$data[["Age model [ka]"]] # 4.425  5.547  7.656  8.568  9.763 10.686  ... 21.648 22.251 23.141 24.210
        PaTh <- tmp[[1]]$data[["Pa/Th"]]
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                PaTh <- rev(PaTh)
            }
        }
        years <- years*-1000 # -24210 -23141 -22251 -21648 -20257 ... -11376 -10686  -9763  -8568  -7656  -5547  -4425 
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=as.numeric(tmp[[1]]$metadata$events$LONGITUDE),
                          lat=as.numeric(tmp[[1]]$metadata$events$LATITUDE),
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "Pa/Th"=PaTh,
                          id=names(tmp[[1]]$metadata$events)[1],
                          text="Meckler et al. 2013")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    }
    dat_meckler_etal_2013 <- list(data=data, 
                                  type="o", col="#377EB8",
                                  lty=1, lwd=1, pch=1, cex=1)
    if (save_PaTh_list) dat_PaTh_all[[length(dat_PaTh_all)+1]] <- dat_meckler_etal_2013
} # 231Pa/230Th of meckler et al. 2013

if (F) { # 231Pa/230Th of mulitza et al. 2017 
    library(pangaear)
    pdoi <- "10.1594/PANGAEA.877699"
    tmp <- pg_data(pdoi) 
    data <- vector("list", l=length(pdoi))
    message("\nload Pa/Th data from mulitza et al. 2017 ...")
    for (i in seq_along(pdoi)) {
        tmp <- pg_data(doi=pdoi[i])
        years <- tmp[[1]]$data[["Age [ka BP] (median)"]] # 4.425  5.547  7.656  8.568  9.763 10.686  ... 21.648 22.251 23.141 24.210
        PaTh <- tmp[[1]]$data[["Pa/Th"]]
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                PaTh <- rev(PaTh)
            }
        }
        years <- years*-1000 # -24210 -23141 -22251 -21648 -20257 ... -11376 -10686  -9763  -8568  -7656  -5547  -4425 
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=as.numeric(tmp[[1]]$metadata$events$LONGITUDE),
                          lat=as.numeric(tmp[[1]]$metadata$events$LATITUDE),
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "Pa/Th"=PaTh,
                          id=names(tmp[[1]]$metadata$events)[1],
                          text="Mulitza et al. 2017")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    }
    dat_mulitza_etal_2017 <- list(data=data, 
                                  type="o", col="#377EB8",
                                  lty=1, lwd=1, pch=1, cex=1)
    if (save_PaTh_list) dat_PaTh_all[[length(dat_PaTh_all)+1]] <- dat_mulitza_etal_2017
} # 231Pa/230Th of mulitza et al. 2017

save_silt_list <- F
if (save_silt_list) { # load all silt into one list
    dat_silt_all <- list()
}

if (F) { # silt size from praetorius et al. 2008
    library(pangaear)
    pdoi <- "10.1594/PANGAEA.769648"
    # [1] "DEPTH, sediment/rock [m] (Depth)"                                    
    # [2] "AGE [ka BP] (Age)"                                                   
    # [3] "Silt-mean, sortable [µm] (Mean silt s)"                              
    # [4] "Grain size, mean [µm] (Grain size mean)"                             
    # [5] "Grain size, mean [µm] (Grain size mean)"                             
    # [6] "Size fraction (Fraction)"                                            
    # [7] "Size fraction 0.063-0.010 mm, sortable silt [%] (63-10 µm sort silt)"
    # [8] "Grain size, mean [µm] (Grain size mean)"                             
    # [9] "Mean, standard deviation [±] (Mean std dev)"   
    data <- vector("list", l=length(pdoi))
    message("\nload silt data from praetorius et al. 2008 ...")
    for (i in seq_along(pdoi)) {
        tmp <- pg_data(doi=pdoi[i])
        tmp <- tmp[2] # stores the silt dat
        years <- tmp[[1]]$data[[2]] # 4.425  5.547  7.656  8.568  9.763 10.686  ... 21.648 22.251 23.141 24.210
        silt_fraction <- tmp[[1]]$data[[7]]
        if (length(years) > 1) {
            if (!all(diff(years) < 0) && !all(diff(years) > 0)) { # if time has jumps
                message("sort years ", paste(years, collapse=","), " to")
                sortinds <- sort(years, index.return=T)$ix
                years <- years[sortinds]
                message(paste(years, collapse=","))
                PaTh <- PaTh[sortinds]
            }
            if (!all(diff(years) < 0)) { # for make_posixlt_origin_function(): 1,2,3 ka BP -> 3,2,1 ka BP
                years <- rev(years)
                silt_fraction <- rev(silt_fraction)
            }
        }
        years <- years*-1000 # -24210 -23141 -22251 -21648 -20257 ... -11376 -10686  -9763  -8568  -7656  -5547  -4425 
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data[[i]] <- list(lon=as.numeric(tmp[[1]]$metadata$events$LONGITUDE),
                          lat=as.numeric(tmp[[1]]$metadata$events$LATITUDE),
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "silt_fraction"=silt_fraction,
                          id=names(tmp[[1]]$metadata$events)[1],
                          text="Praetorius et al. 2008")
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
    }
    dat_praetorius_etal_2008 <- list(data=data, 
                                     type="o", col="#377EB8",
                                     lty=1, lwd=1, pch=1, cex=1)
    if (save_silt_list) dat_silt_all[[length(dat_silt_all)+1]] <- dat_praetorius_etal_2008
} # silt size from praetorius et al. 2008

if (F) { # silt size of hoogakker et al. 2011
    f <- ""
    if (host$machine_tag == "paleosrv") {
        f <- "/isibhv/projects/paleo_work/cdanek/data/hoogakker_etal_2011/hoogakker2011.xls"
    }
    if (file.exists(f)) {
        message("\nread hoogakker et al. 2011 silt data from ", f, " ...")
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
                timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
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
                timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
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
        dat_hoogakker_etal_2011 <- list(data=data,
                                        type="o", col="#377EB8",
                                        lty=1, lwd=1, pch=1, cex=1)
        if (save_silt_list) dat_silt_all[[length(dat_silt_all)+1]] <- dat_hoogakker_etal_2011
    } # if file.exists(f)
} # if silt hoogakker et al. 2011

if (F) { # silt size change of thornalley et al. 2013
    f <- ""
    if (host$machine_tag == "paleosrv") {
        f <- "/isibhv/projects/paleo_work/cdanek/data/thornalley_etal_2013/thornalley2013-cop-stack.txt"
    }
    if (file.exists(f)) {
        message("\nread thornalley et al. 2013 silt data from ", f, " ...")
        dat <- read.table(f, header=T, sep="\t", comment.char="#")
        years <- rev(dat$age_calkaBP) # BP = 1950
        silt_size_change <- rev(dat$SS.depwgt.grp)
        years <- -1000*years # 0.203   0.793  1.373  1.939  2.483
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
        data <- vector("list", l=1)
        data[[1]] <- list(lon=-20.82808, # = mean(c(-17.5895, -24.066667)),
                          lat=61.68942, # = mean(c(60.403333, 62.9755))
                          time=timelt, timen=as.numeric(timelt), origin=timelt$origin,
                          "silt_size_change"=silt_size_change,
                          id="Iceland Basin stack",
                          text="Thornalley et al. 2013 (Iceland Basin stack)")
        dat_thornalley_etal_2013 <- list(data=data,
                                         type="o", col="#377EB8",
                                         lty=1, lwd=1, pch=1, cex=1)
        if (length(timelt) > 1) {
            meandt <- mean(difftime(timelt[2:length(timelt)], timelt[1:(length(timelt)-1)], units="days"))
            message("average dt = ", meandt, " days ~ ", meandt/365, " years")
        }
        if (save_silt_list) dat_silt_all[[length(dat_silt_all)+1]] <- dat_thornalley_etal_2013
    } # if file.exists(f)
} # silt size change of thornalley et al. 2013

if (F) { # silt data from moffa-sanchez et al. 2015; past 3ka
    pdoi <- c("10.1594/PANGAEA.899381", "10.1594/PANGAEA.899382")

} # if silt data from moffa-sanchez et al. 2015

if (F) { # silt data from miettinen et al. 2012; past 3ka

} # if silt data from miettinen et al. 2012

if (F) { # silt data from mjell et al. 2015
    f <- ""
    if (host$machine_tag == "paleosrv") {
        f <- "/isibhv/projects/paleo_work/cdanek/data/mjell_etal_2015/palo20202-sup-0002-tables1.xlsx"
    }
    if (file.exists(f)) {
        message("\nread mjell et al. 2015 silt data from ", f, " ...")
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
        timelt <- make_posixlt_origin_function(years, origin_in=1950, origin_out=1950, verbose=0)
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
        dat_mjell_etal_2015 <- list(data=data,
                                    type="o", col="#377EB8",
                                    lty=1, lwd=1, pch=1, cex=1)
        if (save_silt_list) dat_silt_all[[length(dat_silt_all)+1]] <- dat_mjell_etal_2015
    } # if file.exists(f)
} # if silt data from mjell et al. 2015

# plot silt data
if (F && save_silt_list) {
    x <- y <- list()
    legend_text <- c()
    cnt <- 0
    #silt_from <- -10000
    silt_from <- -7000
    for (i in seq_along(dat_silt_all)) {
        for (j in seq_along(dat_silt_all[[i]]$data)) {
            cnt <- cnt + 1
            if (silt_from == "all") {
                tinds <- seq_along(dat_silt_all[[i]]$data[[j]]$time)
            } else { 
                # find all closest values to wanted limit and take the closest in terms of index
                tinds <- which(abs(dat_silt_all[[i]]$data[[j]]$time$year+1900 - silt_from) == 
                               min(abs(dat_silt_all[[i]]$data[[j]]$time$year+1900 - silt_from)))
                if (all(silt_from >= dat_silt_all[[i]]$data[[j]]$time[tinds])) {
                    tinds <- (tinds[length(tinds)]):length(dat_silt_all[[i]]$data[[j]]$time)
                } else {
                    tinds <- 1:tinds[1]
                }
            }
            if (dat_silt_all[[i]]$data[[j]]$text == "Praetorius et al. 2008") {
                y[[cnt]] <- scale(dat_silt_all[[i]]$data[[j]]$silt_fraction[tinds])
                legend_text[cnt] <- paste0("A ", dat_silt_all[[i]]$data[[j]]$text)
            } else if (dat_silt_all[[i]]$data[[j]]$text == "Hoogakker et al. 2011 (Gardar Drift; NEA)") {
                y[[cnt]] <- filter(scale(dat_silt_all[[i]]$data[[j]]$silt_size[tinds]), filter=rep(1/30, t=30))
                legend_text[cnt] <- paste0("B ", dat_silt_all[[i]]$data[[j]]$text)
            } else if (dat_silt_all[[i]]$data[[j]]$text == "Thornalley et al. 2013 (Iceland Basin stack)") {
                y[[cnt]] <- scale(dat_silt_all[[i]]$data[[j]]$silt_size_change[tinds])
                legend_text[cnt] <- paste0("C ", dat_silt_all[[i]]$data[[j]]$text)
            } else if (dat_silt_all[[i]]$data[[j]]$text == "Mjell et al. 2015") {
                y[[cnt]] <- filter(scale(dat_silt_all[[i]]$data[[j]]$silt_size[tinds]), filter=rep(1/30, t=30))
                legend_text[cnt] <- paste0("D ", dat_silt_all[[i]]$data[[j]]$text)
            } else {
                cnt <- cnt - 1
                next # data of ref
            }
            x[[cnt]] <- dat_silt_all[[i]]$data[[j]]$time[tinds]
        } # j
    } # i
    xlim <- range(lapply(x, as.numeric))
    ylim <- range(y, na.rm=T)
    library(RColorBrewer) # https://www.r-bloggers.com/palettes-in-r/
    cols_silt <- c("black", "#E41A1C", "#377EB8", 
                   brewer.pal(max(3, length(x)), "Dark2")[1:(length(x)-3)])[seq_along(x)]
    plotname <- paste0("silt_ts_from", silt_from, ".png")
    message("\nplot ", plotname, " ...")
    png(plotname, width=2000, height=1666, res=300)
    plot(0, t="n", xlim=xlim, ylim=ylim,
         xlab="year before 1950", ylab="Silt fraction/size/size change Index",
         xaxt="n", yaxt="n")
    axis.POSIXct(1, at=pretty(as.POSIXct(xlim, o="1970-1-1", tz="UTC"), n=20))
    axis(2, at=pretty(ylim, n=8), las=2)
    abline(h=0, col="gray", lwd=0.5)
    for (i in seq_along(x)) {
        lines(x[[i]], y[[i]], col=cols_silt[i])
    }
    legend("topright", legend_text, col=cols_silt, lty=1, pch=NA,
           bty="n", x.intersp=0.2)
    dev.off()
} # if plot silt data

# plot all Pa/Th data
if (F && save_PaTh_list) {
    #PaTh_from <- "all"
    #PaTh_from <- -12000
    #PaTh_from <- -10000
    PaTh_from <- -7000
    message("\nplot all ", length(dat_PaTh_all), " Pa/Th proxy data from ", PaTh_from, " ...") 
    library(RColorBrewer) # https://www.r-bloggers.com/palettes-in-r/
    cols_PaTh <- c("black", "#E41A1C", "#377EB8", 
                   brewer.pal(max(3, length(dat_PaTh_all)), "Dark2")[1:(length(dat_PaTh_all)-3)])
    tmp <- lonlim <- latlim <- tlim <- zlim <- list()
    cnt <- 0
    for (i in seq_along(dat_PaTh_all[[i]])) {
        for (j in seq_along(dat_PaTh_all[[i]]$data)) {
            if (PaTh_from == "all") {
                tinds <- seq_along(dat_PaTh_all[[i]]$data[[j]]$time)
            } else { 
                # find all closest values to wanted limit and take the closest in terms of index
                tinds <- which(abs(dat_PaTh_all[[i]]$data[[j]]$time$year+1900 - PaTh_from) == 
                               min(abs(dat_PaTh_all[[i]]$data[[j]]$time$year+1900 - PaTh_from)))
                if (all(PaTh_from >= dat_PaTh_all[[i]]$data[[j]]$time[tinds])) {
                    tinds <- (tinds[length(tinds)]):length(dat_PaTh_all[[i]]$data[[j]]$time)
                } else {
                    tinds <- 1:tinds[1]
                }
            }
            if (length(tinds) > 1) {
                cnt <- cnt + 1
                lonlim[[cnt]] <- range(dat_PaTh_all[[i]]$data[[j]]$lon)
                latlim[[cnt]] <- range(dat_PaTh_all[[i]]$data[[j]]$lat)
                tlim[[cnt]] <- range(dat_PaTh_all[[i]]$data[[j]]$timen[tinds])
                zlim[[cnt]] <- range(dat_PaTh_all[[i]]$data[[j]][["Pa/Th"]][tinds], na.rm=T)
            }
        }
    }
    lonlim <- range(lonlim); latlim <- range(latlim); tlim <- range(tlim); zlim <- rev(range(zlim, na.rm=T))
    # plot 1: time series of Pa/Th
    if (F) {
        tlim <- as.POSIXlt(tlim, o="1970-1-1", tz="UTC")
        if (PaTh_from == "all") {
        } else {
            tlim$year[1] <- PaTh_from - 1900
        }
        tlim <- as.numeric(tlim)
    }
    plotname <- paste0("PaTh_ts_from", PaTh_from, ".png")
    message("\nplot ", plotname, " ...")
    png(plotname, width=2000, height=4000, res=300)
    plot(0, t="n", xlim=tlim, ylim=zlim,
         xlab="year before 1950", ylab="Pa/Th",
         xaxt="n", yaxt="n")
    axis.POSIXct(1, at=pretty(as.POSIXct(tlim, o="1970-1-1", tz="UTC"), n=20))
    axis(2, at=pretty(zlim, n=8), las=2)
    rect(par("usr")[1], 0.04, par("usr")[2], 0.06, 
         col=rgb(t(col2rgb("blue")/255), alpha=0.1), border=NA) # low Pa/Th values
    rect(par("usr")[1], 0.08, par("usr")[2], 0.1, 
         col=rgb(t(col2rgb("red")/255), alpha=0.1), border=NA) # high Pa/Th values
    abline(h=0.093) # production ratio
    cnt <- 0
    legend_names <- legend_ids <- legend_cols <- legend_ltys <- c()
    for (i in seq_along(dat_PaTh_all[[i]])) {
        for (j in seq_along(dat_PaTh_all[[i]]$data)) {
            if (PaTh_from == "all") {
                tinds <- seq_along(dat_PaTh_all[[i]]$data[[j]]$time)
            } else {
                # find all closest values to wanted limit and take the closest in terms of index
                tinds <- which(abs(dat_PaTh_all[[i]]$data[[j]]$time$year+1900 - PaTh_from) == 
                               min(abs(dat_PaTh_all[[i]]$data[[j]]$time$year+1900 - PaTh_from)))
                if (all(PaTh_from >= dat_PaTh_all[[i]]$data[[j]]$time[tinds])) {
                    tinds <- (tinds[length(tinds)]):length(dat_PaTh_all[[i]]$data[[j]]$time)
                } else {
                    tinds <- 1:tinds[1]
                }
            }
            x <- dat_PaTh_all[[i]]$data[[j]]$timen[tinds]
            y <- dat_PaTh_all[[i]]$data[[j]][["Pa/Th"]][tinds]
            if (any(is.na(y))) {
                nainds <- which(is.na(y))
                x <- x[-nainds]
                y <- y[-nainds]
            }
            if (length(x) > 1) {
                cnt <- cnt + 1
                first_time_ind <- which.min(x) 
                last_time_ind <- which.max(x)
                if (T) { # color by negative (blue) or positive (red) trend
                    if (y[last_time_ind] - y[first_time_ind] < 0) {
                        col <- "red"
                    } else {
                        col <- "blue"
                    }
                    dat_PaTh_all[[i]]$data[[j]]$trend_col <- col
                } else { # color by author
                    col <- cols_PaTh[i]
                }
                lines(x, y, 
                      col=col, 
                      #type=dat_PaTh_all[[i]]$type,
                      #lty=dat_PaTh_all[[i]]$lty, 
                      lty=cnt,
                      lwd=dat_PaTh_all[[i]]$lwd)
                # add counter to plot
                text(x[first_time_ind], y[first_time_ind],
                     labels=cnt, col=col, cex=0.5)
                text(x[last_time_ind], y[last_time_ind],
                     labels=cnt, col=col, cex=0.5)
                legend_ids[cnt] <- dat_PaTh_all[[i]]$data[[j]]$id 
                legend_names[cnt] <- paste0(cnt, " (", i, ",", j, ") ", dat_PaTh_all[[i]]$data[[j]]$text, 
                                            " (", legend_ids[cnt], ")")
                legend_cols[cnt] <- col
                legend_ltys[cnt] <- cnt
            } # if more than 1 timepoint
        } # j
    } # i
    legend("bottomleft", legend=legend_names, ncol=3, 
           col=legend_cols, lty=legend_ltys, pch=NA, bty="n",
           x.intersp=0.2, cex=0.45)
    dev.off()
    if (any(duplicated(legend_ids))) {
        message("there are duplicated ids: ", 
                paste(legend_ids[duplicated(legend_ids)], collapse=","))
    }
    # plot 2: map of proxy locations
    if (exists("dat_praetorius_etal_2008")) {
        lonlim <- range(lonlim, dat_praetorius_etal_2008$data[[1]]$lon)
        latlim <- range(latlim, dat_praetorius_etal_2008$data[[1]]$lat) 
    }
    if (exists("dat_hoogakker_etal_2011")) {
        lonlim <- range(lonlim, dat_hoogakker_etal_2011$data[[2]]$lon)
        latlim <- range(latlim, dat_hoogakker_etal_2011$data[[2]]$lat) 
    }
    if (exists("dat_thornalley_etal_2013")) {
        lonlim <- range(lonlim, dat_thornalley_etal_2013$data[[1]]$lon)
        latlim <- range(latlim, dat_thornalley_etal_2013$data[[1]]$lat) 
    }
    if (exists("dat_mjell_etal_2015")) {
        lonlim <- range(lonlim, dat_mjell_etal_2015$data[[1]]$lon)
        latlim <- range(latlim, dat_mjell_etal_2015$data[[1]]$lat) 
    }
    cnt <- 0
    plotname <- paste0("PaTh_map_from", PaTh_from, ".png")
    message("\nplot ", plotname, " ...")
    png(plotname, width=1666, height=2000, res=300)
    plot(0, 0, t="n", xlim=lonlim, ylim=latlim,
         xlab="lon", ylab="lat", xaxt="n", yaxt="n")
    axis(1, pretty(lonlim, n=10))
    axis(2, pretty(latlim, n=10), las=2)
    map("world", add=T, xlim=lonlim, ylim=latlim, interior=F)
    for (i in seq_along(dat_PaTh_all)) {
        for (j in seq_along(dat_PaTh_all[[i]]$data)) {
            if (T) {
                if (any(names(dat_PaTh_all[[i]]$data[[j]]) == "trend_col")) {
                    cnt <- cnt + 1
                    text(dat_PaTh_all[[i]]$data[[j]]$lon, dat_PaTh_all[[i]]$data[[j]]$lat, 
                         #labels=paste0(i, ",", j), 
                         labels=cnt,
                         col=dat_PaTh_all[[i]]$data[[j]]$trend_col,
                         cex=0.66)
                }
            } else {
                text(dat_PaTh_all[[i]]$data[[j]]$lon, dat_PaTh_all[[i]]$data[[j]]$lat, 
                     labels=paste0(i, ",", j), col=cols_PaTh[i],
                     cex=0.66)
            }
        }
    }
    # add silt locations
    if (exists("dat_praetorius_etal_2008")) {
        text(dat_praetorius_etal_2008$data[[1]]$lon, dat_praetorius_etal_2008$data[[1]]$lat, 
             labels="A", cex=0.66)
    }
    if (exists("dat_hoogakker_etal_2011")) {
        text(dat_hoogakker_etal_2011$data[[2]]$lon, dat_hoogakker_etal_2011$data[[2]]$lat, 
             labels="B", cex=0.66)
    }
    if (exists("dat_thornalley_etal_2013")) {
        text(dat_thornalley_etal_2013$data[[1]]$lon, dat_thornalley_etal_2013$data[[1]]$lat, 
             labels="C", cex=0.66)
    }
    if (exists("dat_mjell_etal_2015")) {
        text(dat_mjell_etal_2015$data[[1]]$lon, dat_mjell_etal_2015$data[[1]]$lat, 
             labels="D", cex=0.66)
    }
    dev.off()
} # if save_PaTh_list

# NOAA monthly station data from https://www.ncdc.noaa.gov/cdo-web/search
if (F) {
    ghcdn_csv <- ""
    if (host$machine_tag == "ollie") {
        ghcdn_csv <- list.files("/work/ollie/cdanek/data/NOAA/station_data/GHCDN/monthly",
                                pattern=glob2rx("*.csv"), full.names=T)
    } else if (host$machine_tag == "paleosrv") {
        ghcdn_csv <- list.files("/isibhv/projects/paleo_work/cdanek/data/NOAA/station_data/GHCDN/monthly",
                                pattern=glob2rx("*.csv"), full.names=T)
    }
    if (file.exists(ghcdn_csv[1])) {
        message("\ndisable here if you do not want to load NOAA station datasets ...")
        message("load noaa ghcdn monthly station data ...")
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
                    noaa_ghcdn[[i]]$text <- paste0("WMO ", d$STATION[1], "; dist(two-jurts)=72 km")
                }
            } # special legend labels
        } # for i ghdcn csv files; one file per station
        message()
    } # if ghcdn files present
} else {
    message("\nenable here if you want to load NOAA station datasets ...")
} # if NOAA station data

# ERA5 time series data
if (F) {
    fs <- ""
    if (host$machine_tag == "paleosrv") {
        fs <- "/isibhv/projects/paleo_work/cdanek/data/ERA5/post"
    }
    if (fs != "") {
        message("\ndisable here if you do not want to load ERA5 time series ...")
        fs <- list.files(fs, pattern=glob2rx("era5_select_*"), full.names=T)
        if (length(fs) > 0) { # e.g. "era5_select_viwvn_emanda_remapnn_Jan-Dec_1990-2010.nc"
            era5_ts <- list(fs=fs, name="ERA5", n_ma=36,
                            col="#377EB8", lty=1, lwd=1, pch=1, cex=1)
        } # if any of fs exists
    } # if fs != ""
} else {
    message("\nenable here if you want to load ERA5 time series ...")
} # if ERA5 time series

# ERA5 spatial data
if (F) {
    fs <- c("era5_viwv_yseasmean_1990-2010.nc", "era5_viwv_direction_yseasmean_1990-2010.nc")
    if (host$machine_tag == "paleosrv") {
        fs <- paste0("/isibhv/projects/paleo_work/cdanek/data/ERA5/post/", fs)
    }
    if (any(file.exists(fs))) {
        era5_spatial <- vector("list", l=length(fs))
        cnt <- 0
        for (f in fs) {
            if (file.exists(f)) {
                cnt <- cnt + 1
                if (cnt == 1) message("\ndisable here if you do not want to load ERA5 spatial datasets ...")
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
    } # if any of fs exists
} else {
    message("\nenable here if you want to load ERA5 spatial datasets ...")
} # if ERA5 spatial data

# clean work space fom loading special data sets
objs <- c("f", "time", "years", "timelt", "nyears_to_originorigin",
          "pdoi", "tmp", "tmp2", "d", "ghcdn_csv", 
          "Tavg_anTmin_anTmax_anprecip_an", 
          "Tavg_monTmin_monTmax_monprecip_an",
          "cnt", "lonlat", "lon_orig", "lat_orig")
suppressWarnings(rm(list=objs))

message("... finished reading special data sets via load_special_data.r")
