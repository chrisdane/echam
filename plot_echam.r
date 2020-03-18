## R

#options(warn = 2) # stop on warnings
if (T) {
    rm(list=ls())
    fctbackup <- `[`; `[` <- function(...) { fctbackup(..., drop=F) }
    # `[` <- fctbackup 
}
graphics.off()

# load packages if not run with my default .Rprofile
if (!interactive()) {
    library(ncdf4)
}

# helper functions
message("\n", "Read helper_functions.r ...")
source("helper_functions.r")

# user input
fnml <- "namelist.plot.r"
message("\n", "Read ", fnml, " ...")
source(fnml)

# ignore these variables
ignore_vars <- c("time_bnds", "timestamp", 
                 "hyai", "hybi", "hyam", "hybm",
                 "depthvec", 
                 "moc_reg_lat")

# load special data
message("\n", "start reading special data sets ...")

# cmip6 co2 hist
f <- ""
if (machine_tag == "mistral") {
    f <- "/pool/data/ECHAM6/input/r0007/greenhouse_historical.nc"
}
if (file.exists(f)) {
    message("\n", "read hist CO2 from ", f, " ...")
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
if (machine_tag == "mistral") {
    f <- "/pool/data/ECHAM6/input/r0008/greenhouse_1pctCO2.nc"
}
if (file.exists(f)) {
    message("\n", "read 1pct CO2 from ", f, " ...")
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
message("\n", "set 4CO2 to ", co2_4co2$co2_ppm, " ppm") 
add_co2_4co2 <- F
message("set add_co2_4co2=T if you want to add to plot")

# koehler et al. 2017 ghg by paul
f <- ""
if (machine_tag == "stan") {
    f <- "/ace/user/pgierz/cosmos-aso-wiso/Hol-T/scripts/Koehler_GHG_forcing_0.001ka_resolution.dat"
}
if (file.exists(f)) {
    message("\n", "read koehler et al. 2017 ghg forcing from ", f, " from paul ...")
    koehler_etal_2017_paul <- read.table(f, col.names=c("year_before_1950", "CO2", "CH4", "N2O"))
    years <- koehler_etal_2017_paul$year_before_1950 # kyr before 1950 in reverse order --> 6.999, 6.998, 6997, ...
    years <- -1*years*1000 # --> -6999, -6998, -6997, ...
    # as.POSIXlt("-0001-01-01") --> negative year gives error 
    # "not in a standard unambiguous format"
    # --> but shifting works
    timelt <- as.POSIXlt("0000-01-01", tz="UTC")
    nyears_to_origin <- timelt$year + 1900 - years[1] + 1
    timelt <- seq.POSIXt(timelt, l=nyears_to_origin, b="-1 year")[nyears_to_origin]
    timelt <- seq.POSIXt(timelt, l=length(years), b=paste0(diff(years)[1], " year"))
    timelt <- as.POSIXlt(timelt)
    koehler_etal_2017_paul <- list(co2=koehler_etal_2017_paul$CO2,
                                   ch4=koehler_etal_2017_paul$CH4,
                                   n2o=koehler_etal_2017_paul$N2O,
                                   time=timelt, timen=as.numeric(timelt),
                                   text="Köhler et al. 2017 (paul)", 
                                   col="red", lty=2, lwd=0.5, pch=NA)
    add_koehler_etal_2017_paul <- F
    message("set add_koehler_etal_2017_paul=", !add_koehler_etal_2017_paul, 
            " if you ", ifelse(add_koehler_etal_2017_paul, 
                               "dont want (or set add_data_right_yaxis_ts=F)", 
                               "want (set also add_data_right_yaxis_ts=T)"), 
            " to add this data to plot")
}

# koehler et al. 2017 ghg original data
f <- ""
if (machine_tag == "stan") {
    f <- "/ace/user/cdanek/data/koehler_etal_2017/datasets/CO2_stack_156K_spline_V2.tab"
    source("/ace/user/cdanek/data/koehler_etal_2017/read_koehler_etal_2017_function.r")
}
if (file.exists(f)) {
    from <- 6999
    to <- 0
    message("\n", "read koehler et al. 2017 ghg forcing from ", f, " from ", from, " to ", to, " ...")
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
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/cmip6/solar_irradiance/swflux_14band_monthly_1850-2014.nc"
}
if (file.exists(f)) {
    message("\n", "read historical monthly total solar irradiance from ", f, " ...")
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
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/cmip6/solar_irradiance/swflux_14band_annual_1850-2014.nc"
}
if (file.exists(f)) {
    message("\n", "read historical monthly total solar irradiance from ", f, " ...")
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
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/HadCRUT4/HadCRUT_global_SAT_anomaly_wrt_1961-1990_HadCRUT.4.6.0.0.monthly_ns_avg.txt.nc"
}
if (file.exists(f)) {
    message("\n", "read hadcrut4 global monthly SAT anomalies wrt to 1961-1990 from ", f, " ...")
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
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/HadCRUT4/HadCRUT_global_SAT_anomaly_wrt_1961-1990_HadCRUT.4.6.0.0.annual_ns_avg.txt.nc"
}
if (file.exists(f)) {
    message("\n", "read hadcrut4 global annual SAT anomalies wrt to 1961-1990 from ", f, " ...")
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
f <- ""
if (machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/data/marcott_etal_2013/Marcott.SM.database.S1.xlsx"
    source("/isibhv/projects/paleo_work/cdanek/data/marcott_etal_2013/read_marcott_etal_2013_function.r")
}
if (file.exists(f)) {
    message("\n", "read marcott et al. 2013 temperature anomalies wrt to 1961-1990 from ", f, " ...")
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

# gistempv4 global annual temperature anomaly wrt 1951-1980
f <- ""
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/GISTEMPv4/GISTEMPv4_global_SAT_anomaly_wrt_1951-1980_GLB.Ts+dSST.csv.nc"
}
if (file.exists(f)) {
    message("\n", "read gistempv4 global annual SAT anomalies wrt to 1951-1980 from ", f, " ...")
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
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/RAPID/moc_transports.nc"
    f_err <- "/work/ba0941/a270073/data/RAPID/moc_error.mat"
}
if (file.exists(f)) {
    message("\n", "read rapid moc from ", f, " ...")
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
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/NSIDC/sea_ice_index/data/N_seaice_extent_monthly_v3.0.nc"
}
if (file.exists(f)) {
    message("\n", "read northern nsidc sea ice index from ", f, " ...")
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
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/NSIDC/sea_ice_index/data/S_seaice_extent_monthly_v3.0.nc"
}
if (file.exists(f)) {
    message("\n", "read southern nsidc sea ice index from ", f, " ...")
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
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/NSIDC/sea_ice_index/data/N_seaice_extent_annual_v3.0.nc"
}
if (file.exists(f)) {
    message("\n", "read northern nsidc sea ice index from ", f, " ...")
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
if (machine_tag == "mistral") {
    f <- "/work/ba0941/a270073/data/NSIDC/sea_ice_index/data/S_seaice_extent_annual_v3.0.nc"
}
if (file.exists(f)) {
    message("\n", "read southern nsidc sea ice index from ", f, " ...")
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
f <- ""
if (machine_tag == "paleosrv") {
    f <- "/scratch/simulation_database/incoming/Hol-Tx10/script/HOL_ORB_forcing_0.01ka_resolution_combined.dat"
}
if (file.exists(f)) {
    message("\n", "read pauls accelerated berger orbital parameters from ", f, " ...")
    orb_berger_acc <- read.table(f, col.names=c("year_before_1950", "eccentricity", "precession", "obliquity"))
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

# berger holocene transient orbital parameter from paul
f <- ""
if (machine_tag == "stan") {
    f <- "/ace/user/pgierz/cosmos-aso-wiso/Hol-T/scripts/Berger_ORB_forcing_0.001ka_resolution.dat"
} else if (machine_tag == "paleosrv") {
    f <- "/isibhv/projects/paleo_work/cdanek/out/cosmos-aso-wiso/Hol-T/scripts/Berger_ORB_forcing_0.001ka_resolution.dat"
}
if (F && file.exists(f)) {
    message("\n", "read pauls transient berger orbital parameters from ", f, " ...")
    orb_berger <- read.table(f, col.names=c("year_before_1950", "eccentricity", "precession", "obliquity"))
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

# berger orbital parameter for last 800ka
f <- ""
if (machine_tag == "stan") {
    f <- "/home/ace/cdanek/scripts/fortran/berger_1978/berger_1978_years_-800_to_0_kyears_before_1950.txt"
}
if (file.exists(f)) {
    message("\n", "read my berger orbital parameters from ", f, " ...")
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

# laskar orbital parameter for last 800ka
f <- ""
if (machine_tag == "stan") {
    f <- "/home/ace/cdanek/scripts/fortran/laskar_etal_2004/laskar_etal_2004_years_-800_to_0_kyears_before_2000.txt"
} else if (machine_tag == "paleosrv") {
    f <- "/home/csys/cdanek/scripts/fortran/laskar_etal_2004/laskar_etal_2004_years_-800_to_0_kyears_before_2000.txt"
}
if (file.exists(f)) {
    message("\n", "read laskar orbital parameters from ", f, " ...")
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

if (F) { # compare koehler et al. 2017 vs paul

    message("plot koehler et al. 2017 vs paul ...")
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

} # compare koehler et al. 2017 vs paul

if (F) { # compare berger and laskar orb

    message("plot berger vs laskar ...")
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

} # comapre berger and laskar orb

# clean
for (obj in c("f", "time", "years", "timelt", "nyears_to_origin", "origin")) {
    if (exists(obj)) rm(obj)
}

message("\n", "finished reading special data sets ...")
# finished reading extra datasets depending on machine


# check user input and defaults
nsettings <- length(prefixes)
if (!exists("codes")) codes <- rep("", t=nsettings)
if (!exists("levs")) levs <- rep("", t=nsettings)
if (!exists("depths")) depths <- rep("", t=nsettings)
codesf <- codes
codesf[codes != ""] <- paste0("_selcode_", codesf[codes != ""])
if (!exists("fromsp")) fromsp <- rep(NA, t=nsettings)
if (!exists("tosp")) tosp <- rep(NA, t=nsettings)
froms_plot <- tos_plot <- rep(NA, t=nsettings)
if (!exists("new_origins")) new_origins <- rep(NA, t=nsettings)
if (!exists("time_ref")) time_ref <- NA # only one
if (!exists("seasonsf")) seasonsf <- rep("Jan-Dec", t=nsettings)
if (!exists("seasonsp")) seasonsp <- seasonsf
season_check <- list(string="DJFMAMJJASOND", inds=c(12, 1:12), names=month.abb[1:12])
if (!exists("n_mas")) n_mas <- rep(1, t=nsettings) # 1 = no moving average effect
if (all(n_mas == 1)) {
    if (add_unsmoothed == F) {
        message("\n", "all `n_mas` = 1, change `add_unsmoothed` from F to T ...")
        add_unsmoothed <- T
    }
    if (add_smoothed == T) {
        message("\n", "all `n_mas` = 1, change `add_smoothed` from T to F ...")
        add_smoothed <- F
    }
}
levsf <- levs
levsf[levs != ""] <- paste0("_sellevel_", levs[levs != ""])
depthsf <- rep("", t=nsettings)
depthsf[depths != ""] <- paste0("_", depths[depths != ""], "m")
if (!exists("depth_fromsf")) depth_fromsf <- rep(NA, t=nsettings)
if (!exists("depth_tosf")) depth_tosf <- rep(NA, t=nsettings)
if (!exists("depth_fromsp")) depth_fromsp <- depth_fromsf
if (!exists("depth_tosp")) depth_tosp <- depth_tosf
if (!exists("areas")) areas <- rep("global", t=nsettings)
if (!exists("reg_dxs")) reg_dxs <- rep("", t=nsettings)
if (!exists("reg_dys")) reg_dys <- rep("", t=nsettings)
reg_dxsf <- reg_dxs
reg_dxsf[reg_dxs != ""] <- paste0("_regular_dx", reg_dxs[reg_dxs != ""])
reg_dysf <- reg_dys
reg_dysf[reg_dys != ""] <- paste0("_dy", reg_dys[reg_dys != ""])
if (!exists("remove_setting")) remove_setting <- NA
if (!exists("remove_mean_froms")) remove_mean_froms <- rep(NA, t=nsettings) 
if (!exists("remove_mean_tos")) remove_mean_tos <- rep(NA, t=nsettings) 
if (!is.na(remove_setting)
    && any(!is.na(remove_mean_froms))) {
    stop("both `remove_setting` and `remove_mean_froms` have non-NA values. choose 1")
}
if (!exists("ltys")) ltys <- rep(1, t=nsettings)
if (!exists("lwds")) lwds <- rep(1, t=nsettings)
if (!exists("pchs")) pchs <- rep(NA, t=nsettings)
if (!exists("scatterpchs")) scatterpchs <- rep(16, t=nsettings)
if (!exists("scatterpchs_vstime")) scatterpchs_vstime <- 1:nsettings
if (!exists("scattercexs")) scattercexs <- rep(1, t=nsettings)
if (!exists("cols")) {
    if (nsettings == 1) {
        cols <- "black"
    } else if (nsettings == 2) {
        cols <- c("black", "#E41A1C")
    } else if (nsettings >= 3) {
        # my default: (black, red, blue) instead of R default (black, blue, red)
        cols <- c("black", "#E41A1C", "#377EB8")
        if (nsettings > 3) {
            if (F) {
                cols <- c(cols, 4:nsettings)
            } else if (T) {
                library(RColorBrewer) # https://www.r-bloggers.com/palettes-in-r/
                cols <- c(cols, brewer.pal(max(3, nsettings), "Dark2")[1:(nsettings-3)])
            }
        }
    }
}
if (!exists("text_cols")) text_cols <- rep("black", t=nsettings)
if (!exists("postpaths")) { # default from post_echam.r
    postpaths <- rep(paste0(workpath, "/post"), t=nsettings)
}
if (!exists("plotpath")) { # default from post_echam.r
    plotpath <- paste0(workpath, "/plots/", paste(unique(models), collapse="_vs_"))
}
base <- 10
power <- 0 # default: 0 --> 10^0 = 1e0 = 1 --> nothing happens
cols_rgb <- rgb(t(col2rgb(cols)/255), alpha=alpha)


# allocate
datas <- vector("list", l=nsettings)
names(datas) <- names_short
data_infos <- dims <- dims_per_setting_in <- datas

# read data
message("\n", "Read data ...")
for (i in 1:nsettings) {

    message("\n", "*********************************************")
    message("setting ", i, "/", nsettings, ": ", names_short[i], " ...")
    inpath <- paste0(postpaths[i], "/", models[i], "/", mode, "/", varnames_in[i])
    
    fname <- paste0(prefixes[i], "_", mode, 
                    codesf[i], "_", varnames_in[i], 
                    levsf[i], depthsf[i], "_",
                    areas[i], "_", 
                    seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                    reg_dxsf[i], reg_dysf[i],
                    ".nc") 

    message("\n", "open ", inpath, "/", fname, " ...")
    ncin <- nc_open(paste0(inpath, "/", fname))

    # get dims of file
    message("\n", "get dims ...")
    dims_per_setting_in[[i]] <- names(ncin$dim)
    dimtmp <- vector("list", l=ncin$ndims)
    names(dimtmp) <- dims_per_setting_in[[i]]
    for (di in 1:length(dimtmp)) {
        message(di, ": \"", dims_per_setting_in[[i]][di], "\", n=", length(ncin$dim[[di]]$vals))
        dimtmp[[di]] <- ncin$dim[[di]]$vals
    }
    dims[[i]] <- dimtmp
    rm(dimtmp)

    # drop time dim if not longer than 1
    # todo: why not checking for all length-1-dims here?
    if (any(names(dims[[i]]) == "time")) {
        if (length(dims[[i]]$time) == 1) {
            message("\n", "detected time dim but its length is 1. drop this dim ...")
            dims[[i]]$time <- NULL
        }
    }
    
    # time dim as posix object
    if (any(names(dims[[i]]) == "time")) {

        timein_units <- ncin$dim$time$units
        message("\n", "detected time dim -> make POSIXlt object from timein_units: \"", timein_units, "\"")
        # convert any unit to seconds for POSIX,e.g. 
        # "days since 1538-1-1 00:00:00"
        # "day as %Y%m%d.%f"
        if (regexpr(" since ", timein_units) == -1 &&
            regexpr(" as ", timein_units) == -1) {
            stop("cannot handle timein_units=", timein_units)
        }

        # case 1: e.g. "days since 1538-1-1 00:00:00"  
        if (regexpr(" since ", timein_units) != -1) {
            timein_unit <- substr(timein_units, 1, regexpr(" since ", timein_units) - 1)
            if (any(timein_unit == c("second", "seconds"))) {
                timein_fac <- 1
            } else if (any(timein_unit == c("day", "days"))) {
                timein_fac <- 86400
            } else {
                stop("timein_unit=", timein_unit, " not defined")
            }
            timein_origin <- substr(timein_units, regexpr(" since ", timein_units) + 7, nchar(timein_units))
            #timein_ct <- as.POSIXct(timein*timein_fac, origin=timein_origin, tz="UTC")
            timein_lt <- as.POSIXlt(dims[[i]]$time*timein_fac, origin=timein_origin, tz="UTC")

            # case 2: e.g. "day as %Y%m%d.%f"
        } else if (regexpr(" as ", timein_units) != -1) { 
            timein_unit <- substr(timein_units, 1, regexpr(" as ", timein_units) - 1)
            timein_format <- substr(timein_units, regexpr(" as ", timein_units) + 4, nchar(timein_units))
            if (timein_format == "%Y%m%d.%f") { # e.g. "29991201.9944444"
                hours <- 24*(dims[[i]]$time - floor(dims[[i]]$time))
                mins <- 60*(hours - floor(hours))
                secs <- 60*(mins - floor(mins))
                hours <- floor(hours)
                mins <- floor(mins)
                secs <- floor(secs)
                timein_lt <- as.POSIXlt(paste0(substr(dims[[i]]$time, 1, 4), "-", 
                                               substr(dims[[i]]$time, 5, 6), "-",
                                               substr(dims[[i]]$time, 7, 8), " ",
                                               hours, ":", mins, ":", secs), tz="UTC")
            } else {
                stop("timein_format=", timein_format, " not defined")
            }
        } # which timein_units "days since", "day as", etc.
        message("range(timein_lt) = ", appendLF=F)
        print(range(timein_lt))

        # find out temoral interval (e.g. 1hr, 3hr, 6hr, day, week, month, year)
        if (exists("time_frequencies")) {
            if (length(time_frequencies) != nsettings || class(time_frequencies) != "character") {
                stop("\n`time_frequencies` is set but not of class \"character\" and/or of length ", nsettings)
            } else {
                message("\nprovided `time_frequencies[", i, "]` = \"", time_frequencies[i], "\"")
                dims[[i]]$time_frequency <- time_frequencies[i]
            }
        } else {
            message("\n`time_frequencies` not set")
            dims[[i]]$time_frequency <- ""
        }
        # finished getting tempral interval (e.g. 1hr, 3hr, 6hr, day, week, month, year)

        # shift times due to e.g. senseless spinup years
        # as.POSIXlt's 'year' starts at 1900
        if (!is.na(new_origins[i])) {
            # from year in  = min(timein_lt$year) + 1900
            message("\n", "new_origins is given --> new_origins[", i, "] = ", new_origins[i])
            message("shift range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))
            #shift_by <- -(timein_lt$year[1] + 1900 - new_origins[i]) 
            shift_by <- new_origins[i] - (timein_lt$year[1] + 1900) #- 1
            message("by shift_by = ", 
                    new_origins[i], " - ", timein_lt$year[1] + 1900, #" - 1 ", 
                    " = ", shift_by, " years") 
            timein_lt$year <- timein_lt$year + shift_by
            message("--> new range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))

        } # if !is.na(new_origins[i])
        # set new origin

        # find temporal subset based on given fromsp and tosp
        if (!is.na(fromsp[i]) || !is.na(tosp[i])) {
            message("\n", appendLF=F)
            if (!is.na(fromsp[i])) {
                message("fromsp is given --> fromsp[", i, "] = ", fromsp[i])
                if (fromsp[i] < 0) {
                    # as.POSIXlt("-0001-01-01") --> negative year gives error 
                    # "not in a standard unambiguous format"
                    # --> but shifting works
                    nyears_to_new_origin <- fromsp[i] - (timein_lt$year[1] + 1900) + 1
                    fromsplt <- seq.POSIXt(timein_lt[1], l=nyears_to_new_origin, b="year")[nyears_to_new_origin]
                } else {
                    fromsplt <- as.POSIXlt(paste0(fromsp[i], "-01-01 00:00:00"), tz="UTC")
                }
            } else {
                fromsplt <- timein_lt[1]
                if (i == 1) fromsp <- rep(NA, t=nsettings)
                fromsp[i] <- fromsplt$year + 1900
            }
            if (!is.na(tosp[i])) {
                message("tosp is given --> tosp[", i, "] = ", tosp[i])
                if (tosp[i] < 0) {
                    # as.POSIXlt("-0001-01-01") --> negative year gives error 
                    # "not in a standard unambiguous format"
                    # --> but shifting works
                    nyears_to_new_end <- abs(tosp - (timein_lt$year[length(timein_lt)] + 1900)) + 1
                    tosplt <- seq.POSIXt(timein_lt[length(timein_lt)], l=nyears_to_new_end, b="-1 year")[nyears_to_new_end]
                } else {
                    tosplt <- as.POSIXlt(paste0(tosp[i], "-12-31 23:59:59"), tz="UTC")
                }
            } else {
                tosplt <- timein_lt[length(timein_lt)]
                if (i == 1) tosp <- rep(NA, t=nsettings)
                tosp[i] <- tosplt$year + 1900
            }
            message(" --> find indices for temporal subset between ", 
                    fromsplt, " and ", tosplt, " ...")
            
            # take temporal subset
            time_inds <- which(timein_lt >= fromsplt & timein_lt <= tosplt)
            if (length(time_inds) == 0) {
                stop("temporal subset is of length 0")
            } else {
                if (length(time_inds) != length(timein_lt)) { 
                    message("found temporal subset of length ", length(time_inds), " out of ", 
                            length(dims[[i]]$time), " total time points ...")
                    message("before range(timein_lt) = ", appendLF=F)
                    print(range(timein_lt))
                    timein_lt <- timein_lt[time_inds]
                    message("after range(timein_lt) = ", appendLF=F)
                    print(range(timein_lt))
                    # cut from time dimension
                    dims[[i]]$time <- dims[[i]]$time[time_inds]
                } else {
                    message(" --> use complete time dimension of ", 
                            length(dims[[i]]$time), " time points ...")
                }
            }

        } else {
            fromsp[i] <- timein_lt$year[1] + 1900
            tosp[i] <- timein_lt$year[length(timein_lt)] + 1900
            time_inds <- NULL # default
            
        } # if exists("fromsp") || exists("tosp")
        # cut year range if wanted

        # subset seasons from data if wanted (=seasonsp)
        # check which seasonsf and seasonp differ
        if (seasonsp[i] != seasonsf[i]) {
            
            # special case:  
            if (seasonsp[i] == "annual" && seasonsf[i] == "Jan-Dec") {

            # all other season cases:
            } else { # not seasonsp="annual" & seasonsf="Jan-Dec"
                message("\n", "cut season from `seasonsf[", i, "]` = \"", seasonsf[i], 
                        "\" to `seasonsp[", i, "] = \"", seasonsp[i], "\" ...")
                if (is.character(seasonsp[i])) { # "DJF" or "Jul"
                    # check if substring is in DJFMAM ...
                    season_inds <- regexpr(seasonsp[i], season_check$string)
                    if (any(season_inds != -1)) {
                        season_inds <- season_check$inds[season_inds:(season_inds+attributes(season_inds)$match.length-1)]
                    } else {
                        # check if "Jul", etc ...
                        season_inds <- regexpr(seasonsp[i], season_check$names)
                        if (length(which(season_inds != -1)) == 1) {
                            season_inds <- which(season_inds != -1)
                        } else {
                            stop("do not understand `seasonsp[", i, "]` = \"", seasonsp[i], "\".")
                        }
                    }
                } else if (is.numeric(seasonsp[i])) {
                    stop("not yet")
                }
                months_in <- unclass(timein_lt)$mon + 1
                month_inds <- months_in %in% season_inds
                month_inds <- which(month_inds)
                message("found ", length(month_inds), " month_inds")
                timein_lt <- timein_lt[month_inds]
                if (!is.null(time_inds)) {
                    time_inds <- time_inds[month_inds]
                } else {
                    time_inds <- month_inds
                }
            } # if (seasonsp[i] == "annual" && seasonsf[i] == "Jan-Dec") 
        } # if seasonsp[i] != seasonsf[i]
        # cut season if wanted

        # finished time stuff
        timein_ct <- as.POSIXct(timein_lt)
        if (!is.null(time_inds)) {
            if (class(time_inds) == "logical") {
                dims[[i]]$time_inds <- which(time_inds)
            } else {
                dims[[i]]$time_inds <- time_inds
            }
        }
        dims[[i]]$timen <- dims[[i]]$time # replace original numeric time with POSIX time object
        dims[[i]]$timelt <- timein_lt
        dims[[i]]$time <- timein_ct
        if (!is.null(time_inds)) {
            dims[[i]]$timen <- dims[[i]]$timen[time_inds]
        }
        dims[[i]]$timeunits <- timein_units
        
        # POSIXlt as numeric
        #dims[[i]]$timen <- lapply(dims[[i]]$time, as.numeric)

        message("\nmin/max(dims[[", i, "]]$timelt) = ", 
                min(dims[[i]]$timelt),  " / ", max(dims[[i]]$timelt))
        message("min/max(dims[[", i, "]]$timelt$mon+1) = ", 
                min(dims[[i]]$timelt$mon+1),  " / ", max(dims[[i]]$timelt$mon+1))

    } else { # if none of file dims is "time"

    } # if any of file dims is "time"
    # finfished time dim stuff
    froms_plot[i] <- fromsf[i] # default
    tos_plot[i] <- tosf[i]
    if (!is.na(fromsp[i])) froms_plot[i] <- fromsp[i]
    if (!is.na(tosp[i])) tos_plot[i] <- tosp[i]
    #stop("asd")

    # get depth inds
    if (any(names(dims[[i]]) == "depth")) {
        depth_fromsf[i] <- min(dims[[i]]$depth)
        depth_tosf[i] <- max(dims[[i]]$depth)
        if (is.na(depth_fromsp[i])) depth_fromsp[i] <- depth_fromsf[i]
        if (is.na(depth_tosp[i])) depth_tosp[i] <- depth_tosf[i]
        message("\n", "find depth subsets from depth_fromsp[", i, "]=", 
                depth_fromsp[i], " to depth_tosp[", i, "]=", depth_tosp[i], " ...")
        # find depth subset based on given depth_fromsp depth_tosp
        depth_inds <- which(dims[[i]]$depth >= depth_fromsp[i] & dims[[i]]$depth <= depth_tosp[i])
        # take depth subset
        if (length(depth_inds) > 0 && length(depth_inds) != length(dims[[i]]$depth)) { 
            message("found depth subset of length ", length(depth_inds), " out of ", 
                    length(dims[[i]]$depth), " total depth points ...")
            message("before range(dims[[i]]$depth) = ", appendLF=F)
            print(range(dims[[i]]$depth))
            dims[[i]]$depth <- dims[[i]]$depth[depth_inds]
            message("after range(dims[[i]]$depth) = ", appendLF=F)
            print(range(dims[[i]]$depth))
            dims[[i]]$depth_inds <- depth_inds
        } else {
            if (length(depth_inds) == 0) {
                stop("depth subset is of length 0")
            }
        }
    
    } # if any of file dims is "depth"
    #stop("asd")

    # get vars of file
    message("\n", "get variables ...")
    vars_per_file <- names(ncin$var)
    vars <- vector("list", l=ncin$nvars)
    var_infos <- vars
    for (vi in 1:length(vars)) {
        message(vi, "/", length(vars), ": ", vars_per_file[vi])
        
        # ignore variable
        if (any(ignore_vars == vars_per_file[vi])) {
            message(" --> this variable is included in `ignore_vars` --> ignore this variable ...")
            next # variable of this setting
        }
        
        if (vars_per_file[vi] == paste0("var", codes[i])) {
            message("variable name of nc file \"", vars_per_file[i], 
                    "\" equals \"var`codes[", i, "]` = \"var", codes[i], 
                    "\". use `varnames_in[", i, "]` = \"", varnames_in[i], "\" from now on ...")
            names(vars)[i] <- varnames_in[i]
        } else {
            names(vars)[vi] <- vars_per_file[vi]
        }
        vars[[vi]] <- ncvar_get(ncin, vars_per_file[vi], collapse_degen=squeeze_data) 

        # special
        if (names_short[i] == "Hol-Tx10" && mode == "zonmean" && vars_per_file[vi] == "srad0d" && seasonsf[i] == "annual") {
            message("\nspecial: set ", dims[[i]]$timelt[448], " to NA ...\n")
            vars[[vi]][,448] <- NA
        }

        # get infos of variable
        names(var_infos)[vi] <- names(vars)[vi]
        var_infos[[vi]] <- ncatt_get(ncin, vars_per_file[vi])

        # get dimensions of variable
        dimids <- ncin$var[[vars_per_file[vi]]]$dimids # get dims of data
        dimids <- dimids + 1 # nc dim ids start counting from zero
        if (squeeze_data) { # drop dims with len=1
            dim_lengths <- sapply(ncin$var[[vars_per_file[vi]]]$dim, "[", "len")
            names(dim_lengths) <- sapply(ncin$var[[vars_per_file[vi]]]$dim, "[", "name")
            if (any(dim_lengths == 1)) {
                len1_dim_inds <- which(dim_lengths == 1)
                message(" --> drop dims of length 1: \"", 
                        paste0(names(len1_dim_inds), collapse="\",\""), "\" ...")
                dimids <- dimids[-len1_dim_inds]
                for (di in seq_along(len1_dim_inds)) {
                    dims[[i]][names(len1_dim_inds)[di]] <- NULL
                }
            } # if var has dims of length 1 
        } else {
            stop("not implemented")
        } # if squeeze_data
        attributes(vars[[vi]]) <- list(dim=dim(vars[[vi]]), dims=dims_per_setting_in[[i]][dimids])
        #cmd <- paste0("tmp <- list(", paste0(dims_per_setting_in[[i]][dimids], "=ncin$dim[[", dimids, "]]$vals", collapse=", "), ")")
    } # for vi nvars per setting
    
    # finally remove all variables to ignore
    if (any(sapply(vars, is.null))) {
        if (all(sapply(vars, is.null))) {
            stop("removed all data of this setting!")
        }
        ignore_inds <- which(sapply(vars, is.null))
        vars <- vars[-ignore_inds]
        var_infos <- var_infos[-ignore_inds]
    }
    datas[[i]] <- vars
    data_infos[[i]] <- var_infos
    rm(vars, var_infos)

    # update dimensions per setting
    dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")

    # cut temporal subset from data
    if (!is.null(dims[[i]]$time_inds)) {
        # check for variables that have time dim
        vars_with_timedim_inds <- lapply(dims_per_setting, function(x) grep("time", x) != -1)
        vars_with_timedim_inds <- which(sapply(vars_with_timedim_inds, any))
        if (length(vars_with_timedim_inds) > 0) {
            for (vi in 1:length(vars_with_timedim_inds)) {
                var_with_timedim_ind <- vars_with_timedim_inds[vi]
                dims_of_var <- attributes(datas[[i]][[var_with_timedim_ind]])$dims # e.g. "time", "lon", "lat"
                dim_lengths_of_var <- dim(datas[[i]][[var_with_timedim_ind]])
                timedimind <- which(dims_of_var == "time")
                time_dim_length <- dim_lengths_of_var[timedimind]
                if (length(dims[[i]]$time_inds) != time_dim_length) {
                    message("\n", "cut subset from time dim ...")
                    cmd <- rep(",", t=length(dims_of_var))
                    cmd[timedimind] <- paste0("dims[[", i, "]]$time_inds")
                    cmd <- paste0("datas[[", i, "]][[", var_with_timedim_ind, "]] <- ",
                                  "datas[[", i, "]][[", var_with_timedim_ind, "]][", paste0(cmd, collapse=""), "]")
                    message(cmd)
                    eval(parse(text=cmd))
                    # subsetting removed attributes, apply again
                    attributes(datas[[i]][[var_with_timedim_ind]]) <- list(dim=dim(datas[[i]][[var_with_timedim_ind]]), 
                                                                           dims=dims_of_var)
                } # if time subset inds are of different length then the given time dimension of the data 
            } # vi vars per file with time dim
        } # if there are varbels with time dimension
    } # cut temporal subset

    # cut depth subset from data
    if (!is.null(dims[[i]]$depth_inds)) {
        # check for variables that have depth dim
        vars_with_depthdim_inds <- lapply(dims_per_setting, function(x) grep("depth", x) != -1)
        vars_with_depthdim_inds <- which(sapply(vars_with_depthdim_inds, any))
        if (length(vars_with_depthdim_inds) > 0) {
            message("\n", "cut subset from depth dim ...")
            for (vi in 1:length(vars_with_depthdim_inds)) {
                var_with_depthdim_ind <- vars_with_depthdim_inds[vi]
                dims_of_var <- attributes(datas[[i]][[var_with_depthdim_ind]])$dims # e.g. "time", "depth"
                depthdimind <- which(dims_of_var == "depth")
                cmd <- rep(",", t=length(dims_of_var))
                cmd[depthdimind] <- paste0("dims[[", i, "]]$depth_inds")
                cmd <- paste0("datas[[", i, "]][[", var_with_depthdim_ind, "]] <- ",
                              "datas[[", i, "]][[", var_with_depthdim_ind, "]][", paste0(cmd, collapse=""), "]")
                message(cmd)
                eval(parse(text=cmd))
                # subsetting removed attributes, apply again
                attributes(datas[[i]][[var_with_depthdim_ind]]) <- list(dim=dim(datas[[i]][[var_with_depthdim_ind]]), 
                                                                       dims=dims_of_var)

            } # vi vars per file with depth dim
        } # if there are varbels with depth dimension
    } # cut depth subset
    
    # reorder lons to (-180,...,180) if wanted and necessary
    if (any(names(dims[[i]]) == "lon")) {
        message("\ndetected lon dimension min/max = ", min(dims[[i]]$lon), "/", max(dims[[i]]$lon), " degree")
        if (reorder_lon_from_0360_to_180180) {
            if (any(dims[[i]]$lon < 180) && any(dims[[i]]$lon >= 180)) {
                message("`reorder_lon_from_0360_to_180180` = T AND any(lon < 180) && any(lon >= 180)", "\n", 
                        "--> reorder longitudes from (0,...,360) to (-180,...,180) degree ...")
                dims[[i]]$lon_orig <- dims[[i]]$lon
                if (i == 1) library(abind)
                west_of_180_inds <- which(dims[[i]]$lon < 180)
                east_of_180_inds <- which(dims[[i]]$lon >= 180)
                dims[[i]]$lon <- dims[[i]]$lon_orig - 180
                message("reorder lons at indices\n",
                        paste0(range(west_of_180_inds), collapse=",...,"), ",",
                        paste0(range(east_of_180_inds), collapse=",...,"), " (",
                        paste0(range(dims[[i]]$lon_orig[west_of_180_inds]), collapse=",...,"), ",", 
                        paste0(range(dims[[i]]$lon_orig[east_of_180_inds]), collapse=",...,"), ") deg to\n",
                        paste0(range(east_of_180_inds), collapse=",...,"), ",",
                        paste0(range(west_of_180_inds), collapse=",...,"), " (",
                        paste0(range(dims[[i]]$lon[west_of_180_inds]), collapse=",...,"), ",", 
                        paste0(range(dims[[i]]$lon[east_of_180_inds]), collapse=",...,"), ") deg ...",
                        " (are these numbers correct?!)")

                # check for variables that have lon dim
                vars_with_londim_inds <- which(lapply(dims_per_setting, function(x) grep("lon", x)) == 1)
                if (length(vars_with_londim_inds) > 0) {
                    for (vi in 1:length(vars_with_londim_inds)) {
                        var_with_londim_ind <- vars_with_londim_inds[vi]
                        dims_of_var <- attributes(datas[[i]][[var_with_londim_ind]])$dims # e.g. "lon", "lat"
                        londimind <- which(dims_of_var == "lon")

                        # case 1: only 1 dim (lon)
                        if (length(dims_of_var) == 1) {
                            stop("not implemented")

                            # case 2: more than 1 dim (and one of them is lon) 
                        } else if (length(dims_of_var) > 1) {
                            cmdeast <- rep(",", t=length(dims_of_var)) 
                            cmdeast[londimind] <- "east_of_180_inds"
                            cmdeast <- paste0(cmdeast, collapse="")
                            cmdwest <- rep(",", t=length(dims_of_var)) 
                            cmdwest[londimind] <- "west_of_180_inds"
                            cmdwest <- paste0(cmdwest, collapse="")
                            cmd <- paste0("datas[[", i, "]][[", var_with_londim_ind, "]] <- ",
                                          "abind(datas[[", i, "]][[", var_with_londim_ind, "]][", cmdeast, "], ",
                                          "datas[[", i, "]][[", var_with_londim_ind, "]][", cmdwest, "], ",
                                          "along=", londimind, ")")
                            message("run ", cmd, " ...")
                            eval(parse(text=cmd))
                            # restore attributes removed by subsetting
                            dimnames(datas[[i]][[var_with_londim_ind]]) <- NULL
                            attributes(datas[[i]][[var_with_londim_ind]]) <- list(dim=dim(datas[[i]][[var_with_londim_ind]]), 
                                                                                  dims=dims_of_var)
                        } # how many dims has the variable of whose dims one dim is "lon" 
                    } # for vi vars per file with lon dim
                } # if any vars with lon dim
            } # if (any(lons[[i]] < 180) && any(lons[[i]] >= 180))
        } else {
            message("`reorder_lon_from_0360_to_180180` = F --> do not change longitudes from 0,...,359 to -180,...,0,180 degree")
        } # if reorder_lon_from_0360_to_180180
    } # if this file has lon dim

    # flip data with dimension "lat" if necessary (needs to be increasing for plot)
    if (any(names(dims[[i]]) == "lat")) {
        if (any(diff(dims[[i]]$lat) < 0)) {
            message("\n", "detected lat dimension and lats are decreasing -> flip data along lat dim ...") 
            dims[[i]]$lat_orig <- dims[[i]]$lat
            dims[[i]]$lat <- rev(dims[[i]]$lat)
            # check for variables that have lat dim
            vars_with_latdim_inds <- lapply(dims_per_setting, function(x) regexpr("lat", x) != -1)
            vars_with_latdim_inds <- which(sapply(vars_with_latdim_inds, any))
            if (length(vars_with_latdim_inds) > 0) {
                for (vi in 1:length(vars_with_latdim_inds)) {
                    var_with_latdim_ind <- vars_with_latdim_inds[vi]
                    dims_of_var <- attributes(datas[[i]][[var_with_latdim_ind]])$dims # e.g. "lon", "lat"
                    latdimind <- which(dims_of_var == "lat")
                    cmdlat <- rep(",", t=length(dims_of_var)) 
                    cmdlat[latdimind] <- paste0("length(dims[[", i, "]]$lat):1")
                    cmdlat <- paste0(cmdlat, collapse="")
                    cmd <- paste0("datas[[", i, "]][[", var_with_latdim_ind, "]] <- ",
                                  "datas[[", i, "]][[", var_with_latdim_ind, "]][", cmdlat, "]")
                    message("run ", cmd, " ...")
                    eval(parse(text=cmd))
                    
                    # restore attributes removed by subsetting
                    attributes(datas[[i]][[var_with_latdim_ind]]) <- list(dim=dim(datas[[i]][[var_with_latdim_ind]]), 
                                                                          dims=dims_of_var)
                } # for vi
            } # if any vars with lat dim
        } # if lats are not increasing
    } # if this file has lat dim
    
    # flip data with dimension "depth" if necessary (needs to be increasing for plot)
    if (any(names(dims[[i]]) == "depth")) {
        if (any(diff(dims[[i]]$depth) < 0)) {
            message("\n", "detected depth dimension and depths are decreasing -> flip data along depth dim ...") 
            dims[[i]]$depth_orig <- dims[[i]]$depth
            dims[[i]]$depth <- rev(dims[[i]]$depth)
            # check for variables that have depth dim
            vars_with_depthdim_inds <- lapply(dims_per_setting, function(x) regexpr("depth", x) != -1)
            vars_with_depthdim_inds <- which(sapply(vars_with_depthdim_inds, any))
            if (length(vars_with_depthdim_inds) > 0) {
                for (vi in 1:length(vars_with_depthdim_inds)) {
                    var_with_depthdim_ind <- vars_with_depthdim_inds[vi]
                    dims_of_var <- attributes(datas[[i]][[var_with_depthdim_ind]])$dims # e.g. "lat", "depth", "time"
                    depthdimind <- which(dims_of_var == "depth")
                    cmddepth <- rep(",", t=length(dims_of_var)) 
                    cmddepth[depthdimind] <- paste0("length(dims[[", i, "]]$depth):1")
                    cmddepth <- paste0(cmddepth, collapse="")
                    cmd <- paste0("datas[[", i, "]][[", var_with_depthdim_ind, "]] <- ",
                                  "datas[[", i, "]][[", var_with_depthdim_ind, "]][", cmddepth, "]")
                    message("run ", cmd, " ...")
                    eval(parse(text=cmd))
                    
                    # restore attributes removed by subsetting
                    #dimnames(datas[[i]][[var_with_depthdim_ind]]) <- NULL
                    attributes(datas[[i]][[var_with_depthdim_ind]]) <- list(dim=dim(datas[[i]][[var_with_depthdim_ind]]), 
                                                                            dims=dims_of_var)
                } # for vi
            } # if any vars with depth dim
        } # if depth values are not increasing
    } # if this file has depth dim

    # if two dimensions and one is time, make it x-dim
    if (any(names(dims[[i]]) == "time") && any(names(dims[[i]]) == "lat")) {
        message("\n", "detected time and lat dims; check if permutation from (lat x time) to (time x lat is necessary) ...") 
        vars_with_timedim_inds <- lapply(dims_per_setting, function(x) grep("time", x) != -1)
        vars_with_timedim_inds <- which(sapply(vars_with_timedim_inds, any))
        vars_with_latdim_inds <- lapply(dims_per_setting, function(x) regexpr("lat", x) != -1)
        vars_with_latdim_inds <- which(sapply(vars_with_latdim_inds, any))
        vars_with_timedim_and_latdim_inds <- intersect(vars_with_timedim_inds, vars_with_latdim_inds)
        if (length(vars_with_timedim_and_latdim_inds) > 0) {
            for (vi in 1:length(vars_with_timedim_and_latdim_inds)) {
                var_with_timedim_and_latdim <- vars_with_timedim_and_latdim_inds[vi]
                if (length(attributes(datas[[i]][[vi]])$dims) == 2) {
                    if (attributes(datas[[i]][[vi]])$dims[1] == "lat" &&
                        attributes(datas[[i]][[vi]])$dims[2] == "time") {
                        message("aperm(datas[[", i, "]][[", var_with_timedim_and_latdim, "]], c(2, 1)) ...")
                        datas[[i]][[var_with_timedim_and_latdim]] <- aperm(datas[[i]][[var_with_timedim_and_latdim]], c(2, 1)) # permutate
                        attributes(datas[[i]][[var_with_timedim_and_latdim]]) <- list(dim=dim(datas[[i]][[var_with_timedim_and_latdim]]),
                                                             dims=dims_of_var[c(2 ,1)])
                    } else {
                        # dims are already time x lat; nothing to do
                    }
                } else {
                    stop("not implemented yet ...")
                }
            }
        }

    } # if any dim is time and lat

} # for i nsettings
message("\n", "****************** reading data finished ***************************")

varnames_unique <- unique(as.vector(unlist(sapply(datas, names))))

# save data before applying offset, multiplication factors, etc. for later
message("\n", "save original data without multiplication factors or offsets or removal of a temporal mean etc. ...")
for (vi in 1:length(varnames_unique)) {
    cmd <- paste0(varnames_unique[vi], "_datas_orig <- vector(\"list\", l=nsettings)")
    eval(parse(text=cmd))
    cmd <- paste0("names(", varnames_unique[vi], "_datas_orig) <- names(datas)")
    eval(parse(text=cmd))
    for (i in 1:nsettings) {
        if (varnames_unique[vi] %in% names(datas[[i]])) { # if variables is present in setting
            cmd <- paste0(varnames_unique[vi], "_datas_orig[[", i, "]] <- datas[[", i, "]][[", vi, "]]")
            if (vi == 1 && i == 1) message("run `", cmd, "` ...")
            eval(parse(text=cmd))
        } else { # if variable is not preset in setting
            # nothing
        }
    }
} 


if (F) { # for testing
    message("special")
    datas[[1]][[2]] <- datas[[1]][[1]] + 10
    names(datas[[1]])[2] <- names(datas[[1]])[1]
}


# set variable specific things
message("\n", "set variable specific things ...")
for (i in 1:nsettings) {
    for (vi in 1:length(datas[[i]])) {

        varname <- names(datas[[i]])[vi]

        # default variable axis label
        label <- names(datas[[i]])[vi]
        if (!is.null(data_infos[[i]][[vi]]$longname)) {
            label <- data_infos[[i]][[vi]]$longname
        }
        if (!is.null(data_infos[[i]][[vi]]$long_name)) {
            label <- data_infos[[i]][[vi]]$long_name
        }
        if (!is.na(remove_mean_froms[i])) {
            label <- paste0(label, "\nanomaly wrt ", 
                            paste(unique(remove_mean_froms[i], remove_mean_tos[i]), collapse="-"))
        }
        if (!is.na(remove_setting)) {
            label <- paste0(label, "\nanomaly wrt ", remove_setting)
        }
        if (!is.null(data_infos[[i]][[vi]]$units)) {
            label <- paste0(label, " [", data_infos[[i]][[vi]]$units, "]")
        }
        data_infos[[i]][[vi]]$label <- label

        # add specific things
        if (varname == "temp2") {
            data_infos[[i]][[vi]]$label <- "2m temperature [°C]"
            if (!is.na(remove_mean_froms[i])) {
                data_infos[[i]][[vi]]$label <- "2m temperature anomaly [°C]"
            }
            if (!is.na(remove_setting)) {
                data_infos[[i]][[vi]]$label <- paste0("2m temperature\nanomaly wrt ", remove_setting)
            }
            if (F) { # anomaly:
                message("*** special label ***")
                data_infos[[i]][[vi]]$label <- "2m temperature anomaly [°C]"
            } else {
                if (grepl("C", data_infos[[i]][[vi]]$units)) {
                    message("detected a \"C\" in the `units` attribute of ", varname, 
                            " --> assume that data is already in deg C")
                } else {
                    message("did not detect a \"C\" in the `units` attribute of ", varname, 
                            " --> assume that data is in K")
                    data_infos[[i]][[vi]]$offset$operator <- "-"
                    data_infos[[i]][[vi]]$offset$value <- 273.15
                }
            }
        } else if (varname == "toa_imbalance") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("TOA imbalance [W m"^paste(-2), "]"))))
        } else if (varname == "tau_aero_550") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " ", tau, " dV [m"^3, "]"))))
        } else if (grepl("moc", varname)) {
            data_infos[[i]][[vi]]$label <- "MOC [Sv]"
            if (grepl("moc_max_depths", varname)) {
                data_infos[[i]][[vi]]$label <- "Depth of MOC max [m]"
            }
        } else if (grepl("siarea", varname)) {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("NH sea ice extent [km"^2, 
                                                                            " " %*% " ", 10^6, "]"))))
        } else if (varname == "tos") {
            data_infos[[i]][[vi]]$label <- "SST [°C]"
            if (T) {
                message("special label")
                data_infos[[i]][[vi]]$label <- "SST anomaly [°C]"
            }
                
        } else if (varname == "potdens") {
            #data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(sigma[theta], " [kg m"^"-3","]"))))
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Potential density ", 
                                                                            sigma[theta], " [kg m"^"-3","]"))))
            data_infos[[i]][[vi]]$offset$operator <- "-"
            data_infos[[i]][[vi]]$offset$value <- 1000
    
        } else if (varname == "wisoaprt_d") {
            message("update")
            data_infos[[i]][[vi]]$label <- "wisoaprt_d180"

        } else if (varname == "lm_temp2_as_time_slope") {
            message("special label")
            data_infos[[i]][[vi]]$label <- "2m temperature trend [K/7k years]"
        } # finished define variable specific things
    
    } # for vi varnames per setting
} # for i nsettings
# finished setting variable specific things


# save data infos for later
message("\n", "save data infos ...")
for (vi in 1:length(varnames_unique)) {
    cmd <- paste0(varnames_unique[vi], "_infos <- vector(\"list\", l=nsettings)")
    eval(parse(text=cmd))
    cmd <- paste0("names(", varnames_unique[vi], "_infos) <- names(datas)")
    eval(parse(text=cmd))
    for (i in 1:nsettings) {
        cmd <- paste0(varnames_unique[vi], "_infos[[", i, "]] <- data_infos[[", i, "]][[", vi, "]]")
        if (vi == 1 && i == 1) message("run `", cmd, "` ...")
        eval(parse(text=cmd))
    }
} 


# apply offset or mult_fac
message("\n", "apply variable specific things ...")
for (i in 1:nsettings) {
    for (vi in 1:length(datas[[i]])) {
        if (!is.null(data_infos[[i]][[vi]]$offset)) {
            cmd <- paste0("datas[[", i, "]][[", vi, "]] <- datas[[", i, "]][[", vi, "]] ", 
                          data_infos[[i]][[vi]]$offset$operator, " ", 
                          data_infos[[i]][[vi]]$offset$value)
            message("eval ", cmd, " ...")
            eval(parse(text=cmd))
        }
    }
} # for i nsettings


# remove setting mean
if (!is.na(remove_setting)) {

    remove_setting_ind <- which(names_short == remove_setting)
    if (length(remove_setting_ind) == 1) {
        message("\n", "remove setting \"", remove_setting, "\" mean from all other settings ...")
        settinginds <- 1:nsettings
        settinginds <- settinginds[-remove_setting_ind]
        for (i in settinginds) {
            for (vi in 1:length(datas[[i]])) {
                datas[[i]][[vi]] <- datas[[i]][[vi]] - datas[[remove_setting_ind]][[vi]]
            }
        }
    } else {
        stop("`remove_setting` = \"", remove_setting, 
             "\" but cannot find this setting in `names_short` = \"",
             paste(names_short, collapse="\",\""), ".")
    }

    # finally: throw out the data which was substracted from all others
    stop("need to update all here... maybe this is not a good idea. use cdo instead")
    dims[[remove_setting_ind]] <- NULL
    datas[[remove_setting_ind]] <- NULL
    nsettings <- length(datas)
    names_short <- names_short[-remove_setting_ind]
    names_legend <- names_legend[-remove_setting_ind]

    # update varnames_unique
    varnames_unique <- unique(as.vector(unlist(sapply(datas, names))))

} else {
    message("\n`remove_setting` = NA --> do not remove a setting mean")
} # if !is.na(remove_setting)
# finished removing setting mean from all settings


# remove some temporal mean if defined
if (any(!is.na(remove_mean_froms))) {
    message("\n", "remove temporal means between")
    for (i in 1:nsettings) {
        message(i, "/", nsettings, ": ", names_short[i], " ...")
        if (!is.na(remove_mean_froms[i])) {
            message("   `remove_mean_froms[", i, "]` = ", remove_mean_froms[i], "\n",
                    "   `remove_mean_tos[", i, "]` = ", remove_mean_tos[i])
            if (any(names(dims[[i]]) == "time")) {
                if (remove_mean_froms[i] < 0) {
                    remove_fromslt <- as.POSIXlt(paste0("0-01-01 00:00:00"), tz="UTC")
                    remove_fromslt <- seq.POSIXt(remove_fromslt, by="-1 year", l=abs(remove_mean_froms[i]) + 1)
                    remove_fromslt <- remove_fromslt[length(remove_fromslt)]
                } else {
                    remove_fromslt <- as.POSIXlt(paste0(remove_mean_froms[i], "-01-01 00:00:00"), tz="UTC")
                }
                if (remove_mean_tos[i] < 0) {
                    remove_toslt <- as.POSIXlt(paste0("0-12-31 00:00:00"), tz="UTC")
                    remove_toslt <- seq.POSIXt(remove_toslt, by="-1 year", l=abs(remove_mean_tos[i]) + 1)
                    remove_toslt <- remove_toslt[length(remove_toslt)]
                } else {
                    remove_toslt <- as.POSIXlt(paste0(remove_mean_tos[i], "-12-31 23:59:59"), tz="UTC")
                }
                time_inds <- which(dims[[i]]$time >= remove_fromslt & dims[[i]]$time <= remove_toslt)
                if (length(time_inds) == 0) {
                    stop("no data found between these given dates.")
                } else {
                    for (vi in 1:length(datas[[i]])) {
                        message("   var ", vi, "/", length(datas[[i]]), ": ", names(datas[[i]])[vi]) 
                        # check if variable has time dimension 
                        dims_of_var <- attributes(datas[[i]][[vi]])$dims # e.g. "time", "lon", "lat"
                        timedimind <- which(dims_of_var == "time")
                        if (length(timedimind) == 1) {
                            message("      found ", length(time_inds), " time points between ", 
                                    min(dims[[i]]$time[time_inds]), " and ", max(dims[[i]]$time[time_inds]))
                            apply_dims <- 1:length(dims_of_var)
                            mu <- rep(",", t=length(dims_of_var))
                            mu[timedimind] <- paste0("time_inds")
                            if (length(apply_dims) == 1) { # data is vector
                                mu <- paste0("mu <- mean(datas[[", i, "]][[", vi, "]][", 
                                             paste0(mu, collapse=""), "], na.rm=T)")
                                message("      ", mu)
                                eval(parse(text=mu))
                                message("      mu = ", mu, " ", data_infos[[i]][[vi]]$units)
                                datas[[i]][[vi]] <- datas[[i]][[vi]] - mu 
                            } else { # data is array
                                apply_dims <- apply_dims[-timedimind]
                                mu <- paste0("mu <- apply(datas[[", i, "]][[", vi, "]][", paste0(mu, collapse=""), 
                                             "], c(", paste(apply_dims, collapse=","), "), mean, na.rm=T)")
                                message("      ", mu)
                                eval(parse(text=mu))
                                message("      min/max mu = ", min(mu), " / ", max(mu), " ", data_infos[[i]][[vi]]$units)
                                if (length(dims_of_var) == 2) {
                                    datas[[i]][[vi]] <- datas[[i]][[vi]] - t(array(mu, dim=dim(t(datas[[i]][[vi]]))))
                                } else {
                                    stop("note defineeddd yettt")
                                }
                            } # if data is vector or array
                        } else {
                            message("   variable ", vi, " \"", names(datas[[i]])[vi], "\" has no time dim.")
                        } # remove temporal mean if variable has time dim
                    } # for vi all variables per setting
                } # if temporal mean can be removed
            } else { # if any variable has time dimension or not
                stop("`remove_mean_froms[", i, "]` = \"", remove_mean_froms[i], 
                     "\" but data has not \"time\" dim.")
            } # if any variable has time dimension or not
        } # if remove_mean_froms[i] is not NA
    } # for i nsettings
} else {
    message("\n", "`remove_mean_froms` all NA --> do not remove temporal means ...")
} # finished removing a temporal mean


# save data after applying offset, multiplication factors, temporal mean or setting mean removal
message("\n", "save data after application of multiplication factors or offsets of temporal mean or setting mean removal etc. ...")
for (vi in 1:length(varnames_unique)) {
    cmd <- paste0(varnames_unique[vi], "_datas <- vector(\"list\", l=nsettings)")
    eval(parse(text=cmd))
    cmd <- paste0("names(", varnames_unique[vi], "_datas) <- names(datas)")
    eval(parse(text=cmd))
    for (i in 1:nsettings) {
        cmd <- paste0(varnames_unique[vi], "_datas[[", i, "]] <- datas[[", i, "]][[", vi, "]]")
        if (vi == 1 && i == 1) message("run `", cmd, "` ...")
        eval(parse(text=cmd))
    }
}


# apply moving average --> datasma
if (add_smoothed && 
    any(sapply(lapply(lapply(dims, names), "==", "time"), any)) &&
    #any(seasonsp == "Jan-Dec") && 
    !all(n_mas == 1)) {
    message("\n", "`add_smoothed` = T --> apply moving averages ...")
    datasma <- datas
    for (i in 1:nsettings) {
        message(i, "/", nsettings, ": ", names_short[i], " ...")
        #if (seasonsp[i] == "Jan-Dec" && n_mas[i] != 1) { # applying moving average
        if (n_mas[i] != 1) {
            for (vi in 1:length(datas[[i]])) { 
                dims_of_var <- attributes(datas[[i]][[vi]])$dims # e.g. "time", "lon", "lat"
                timedimind <- which(dims_of_var == "time")
                if (length(timedimind) == 1) {
                    npy <- unclass(dims[[i]]$timelt)
                    npy <- length(npy$year[which(npy$year == npy$year[1])])
                    apply_dims <- 1:length(dim(datas[[i]][[vi]]))
                    message("   var ", vi, "/", length(datas[[i]]), ": ", names(datas[[i]])[vi], 
                            ": n_mas[", i, "]: ", n_mas[i], " (ntime = ", length(dims[[i]]$time), 
                            ", npy = ", npy, " --> ", n_mas[i], "/", npy, " = ", n_mas[i]/npy, 
                            " year running mean)") 
                    if (length(dims_of_var) == 1) { # variable has only 1 dim and its time
                        datasma[[i]][[vi]] <- stats::filter(datas[[i]][[vi]], filter=rep(1/n_mas[i], t=n_mas[i]))
                    } else { # variable has more than 1 dims
                        apply_dims <- apply_dims[-timedimind]
                        cmd <- paste0("datasma[[", i, "]][[", vi, "]] <- ",
                                      "apply(datas[[", i, "]][[", vi, "]], ",
                                      "c(", paste(apply_dims, collapse=","), "), ",
                                      "function(x) filter(x, rep(1/", n_mas[i], ", t=", n_mas[i], ")))")
                        message("   ", cmd)
                        eval(parse(text=cmd))
                    }
                } else { # variable has no time dim
                    message("variable \"", names(datas[[i]])[vi], "\" has not time dim ...")
                    datasma[[i]][[vi]] <- array(NA, dim=dim(datas[[i]][[vi]]))
                } # if variable has time dim or not
                
                attributes(datasma[[i]][[vi]]) <- attributes(datas[[i]][[vi]])
            
            } # for vi nvars
        } # if n_mas != 1    
    } # for i nsettings
} # if add_smoothed && any(attributes(datas[[i]][[vi]])$dims == "time") && !all(n_mas == 1)
if (!exists("datasma")) {
    message("\n", "`datasma` does not exist. --> set `add_smoothed` = T ",
            "and/or `n_mas` not equal 1\n",
            "in order to apply moving averages ...")
    add_smoothed <- F
}
# finished applying moving average


# calculate monthly and annual means
if (any(sapply(lapply(lapply(dims, names), "==", "time"), any)) 
    && any(seasonsp == "Jan-Dec") 
    && any(sapply(dims, "[", "time_frequency") == "monthly")) {

    datasmon <- datasan <- datas
    monlim <- anlim <- NA
    
    for (i in 1:nsettings) {
        if (i == 1) {
            message("\n", "`dims[[", i, "]]$time_frequency == \"monthly\" ",
                    "--> calc monthly climatology and annual means of setting")
        }
        message(i, "/", nsettings, ": ", names_short[i], " ...")
        if (dims[[i]]$time_frequency == "monthly" && seasonsp[i] == "Jan-Dec") {    
            for (vi in 1:length(datas[[i]])) { # for all vars per setting
                message("   var ", vi, "/", length(datas[[i]]), ": ", names(datas[[i]])[vi], " ...")
                if (any(attributes(datas[[i]][[vi]])$dims == "time")) { # if var has time dim
                    
                    # monthly climatology 
                    months <- unclass(dims[[i]]$timelt)$mon + 1
                    months_unique <- unique(months)
                    dims_of_var <- attributes(datas[[i]][[vi]])$dims # e.g. "time", "lon", "lat"
                    timedimind <- which(dims_of_var == "time")
                    datasmon_dims <- dim(datas[[i]][[vi]])
                    datasmon_dims[timedimind] <- length(months_unique)
                    datasmon_dims_ntime1 <- datasmon_dims
                    datasmon_dims_ntime1[timedimind] <- 1
                    tmp <- array(NA, dim=datasmon_dims)
                    apply_dims <- 1:length(datasmon_dims)
                    if (length(apply_dims) == 1) {
                        apply_dims <- 1
                    } else {
                        apply_dims <- apply_dims[-timedimind]
                    }
                    for (mi in 1:length(months_unique)) {
                        message(months_unique[mi], " ", appendLF=F)
                        if (mi == length(months_unique)) message()
                        time_inds <- which(months == months_unique[mi])
                        indslhs <- indsrhs <- rep(",", t=length(dims_of_var))
                        indslhs[timedimind] <- mi
                        indsrhs[timedimind] <- paste0("time_inds")
                        if (length(datasmon_dims) == 1) { # var has only time dim
                            cmd1 <- paste0("tmp2 <- mean(datas[[", i, "]][[", vi, "]][",
                                           paste(indsrhs, collapse=""), "], na.rm=T)")
                        } else {
                            cmd1 <- paste0("tmp2 <- apply(datas[[", i, "]][[", vi, "]][", 
                                          paste(indsrhs, collapse=""), "]",
                                          ", c(", paste(apply_dims, collapse=","), ")", 
                                          ", mean, na.rm=T)")
                        }
                        #message(cmd1)
                        eval(parse(text=cmd1))
                        dim(tmp2) <- datasmon_dims_ntime1
                        cmd2 <- paste0("tmp[", paste(indslhs, collapse=""), "] <- tmp2")
                        #message(cmd2)
                        eval(parse(text=cmd2))
                    } # for all unique months
                    datasmon[[i]][[vi]] <- tmp
                    attributes(datasmon[[i]][[vi]])$dim <- datasmon_dims
                    attributes(datasmon[[i]][[vi]])$dims <- attributes(datas[[i]][[vi]])$dims
                    attributes(datasmon[[i]][[vi]])$dims[timedimind] <- "month"

                    # annual means
                    years <- unclass(dims[[i]]$timelt)$year + 1900
                    years_unique <- unique(years)
                    datasan_dims <- dim(datas[[i]][[vi]])
                    datasan_dims[timedimind] <- length(years_unique)
                    datasan_dims_ntime1 <- datasan_dims
                    datasan_dims_ntime1[timedimind] <- 1
                    tmp <- array(NA, dim=datasan_dims)
                    apply_dims <- 1:length(datasan_dims)
                    if (length(apply_dims) == 1) {
                        apply_dims <- 1
                    } else {
                        apply_dims <- apply_dims[-timedimind]
                    }
                    for (yi in 1:length(years_unique)) {
                        if (yi < 14 ||
                            yi >= (length(years_unique) - 14)) {
                            message(years_unique[yi], " ", appendLF=F)
                        }
                        if (yi == length(years_unique)) message()
                        time_inds <- which(years == years_unique[yi])
                        indslhs <- indsrhs <- rep(",", t=length(dims_of_var))
                        indslhs[timedimind] <- yi
                        indsrhs[timedimind] <- paste0("time_inds")
                        if (length(datasan_dims) == 1) { # var has only time dim
                            cmd1 <- paste0("tmp2 <- mean(datas[[", i, "]][[", vi, "]][",
                                           paste(indsrhs, collapse=""), "], na.rm=T)")
                        } else {
                            cmd1 <- paste0("tmp2 <- apply(datas[[", i, "]][[", vi, "]][", 
                                          paste(indsrhs, collapse=""), "]",
                                          ", c(", paste(apply_dims, collapse=","), ")", 
                                          ", mean, na.rm=T)")
                        }
                        #message(cmd1)
                        eval(parse(text=cmd1))
                        dim(tmp2) <- datasan_dims_ntime1
                        cmd2 <- paste0("tmp[", paste(indslhs, collapse=""), "] <- tmp2")
                        #message(cmd2)
                        eval(parse(text=cmd2))
                    } # for all unique months
                    datasan[[i]][[vi]] <- tmp
                    attributes(datasan[[i]][[vi]])$dim <- datasan_dims
                    attributes(datasan[[i]][[vi]])$dims <- attributes(datas[[i]][[vi]])$dims
                    attributes(datasan[[i]][[vi]])$dims[timedimind] <- "year"

                } else { # variable has no time dim
                    #message("variable has no \"time\" dim ...")
                    datasmon[[i]][[vi]] <- NA
                    datasan[[i]][[vi]] <- NA
                } # if variable has time dim or not

            } # for vi nvars
        
            dims[[i]]$month <- months_unique
            dims[[i]]$monmean_range <- paste0(fromsp[i], "-", tosp[i])
            monlim <- range(monlim, dims[[i]]$month, na.rm=T)
            dims[[i]]$year <- years_unique
            dims[[i]]$yearmean_range <- dims[[i]]$monmean_range
            anlim <- range(anlim, dims[[i]]$year, na.rm=T)

        } else {

            message(" --> no monthly output or seasonp[", i, "] is not \"Jan-Dec\"")
        
        } # if input data is monthly 

    } # for i nsettings
    
    # remove NA entries
    for (i in 1:nsettings) {
        navars <- which(is.na(datasmon[[i]]))
        if (length(navars) > 0) {
            datasmon[[i]][navars] <- NULL
        }
        navars <- which(is.na(datasan[[i]]))
        if (length(navars) > 0) {
            datasan[[i]][navars] <- NULL
        }
    }
    monat <- monlim[1]:monlim[2]
    monlab <- substr(month.abb[monat], 1, 1) # Jan -> J
    anat <- pretty(anlim, n=10)
    anlab <- anat

} # if any(seasonsp == "Jan-Dec") && any(attributes(datas[[i]][[vi]])$dims == "time")
# finished calculating monthly means if applicable


## calculate temporal mean (long term mean; ltm)
if (any(sapply(lapply(lapply(dims, names), "==", "time"), any))) {
    datasltm <- datas
    for (i in 1:nsettings) {
        if (i == 1) message("\n", "calc temporal mean of setting")
        message(i, "/", nsettings, ": ", names_short[i], " ...")
        for (vi in 1:length(datas[[i]])) {
            if (any(attributes(datas[[i]][[vi]])$dims == "time")) { # if var has time dim
                message("   var ", vi, "/", length(datas[[i]]), ": ", names(datas[[i]])[vi], " ...")
                dims_of_var <- attributes(datas[[i]][[vi]])$dims # e.g. "time", "lon", "lat"
                timedimind <- which(dims_of_var == "time")
                apply_dims <- 1:length(dim(datas[[i]][[vi]]))
                if (length(apply_dims) == 1) { # var has only time dim
                    datasltm[[i]][[vi]] <- mean(datas[[i]][[vi]], na.rm=T)
                } else {
                    apply_dims <- apply_dims[-timedimind]
                    cmd <- paste0("datasltm[[", i, "]][[", vi, "]] <- ",
                                  "apply(datas[[", i, "]][[", vi, "]], ",
                                  "c(", paste(apply_dims, collapse=","), "), ",
                                  "mean, na.rm=T)")
                    #message(cmd)
                    eval(parse(text=cmd))
                }
                
                attributes(datasltm[[i]][[vi]])$dims <- attributes(datas[[i]][[vi]])$dims[-timedimind]

            } else { # variable has not time dim
                datasltm[[i]][[vi]] <- NA
                
            } # if variable has time dim
        } # vi 
        dims[[i]]$ltm_range <- paste0(dims[[i]]$time[1], " to ", dims[[i]]$time[length(dims[[i]]$time)])
    } # i
    # remove NA entries
    for (i in 1:nsettings) {
        navars <- which(is.na(datasltm[[i]]))
        if (length(navars) > 0) {
            datasltm[[i]][navars] <- NULL
        }
    }
} # if any var has time dim
# finished calculating temoral mean


## interpolate data if any dimension is irregular
if (exists("datasltm")) {
    # todo
} # if exsits("datalam")
# finished interpolate if any dimension is irregular


## get time axis values
ntime_per_setting <- sapply(sapply(dims, "[", "time"), length)
if (any(ntime_per_setting > 1)) {
    message("\n", "find good time axis labels ...")

    # POSIX will be converted to numeric by plot(), so use these numeric values as limits
    tlim <- range(lapply(sapply(dims, "[", "time"), as.numeric)) # seconds by default 
    tlimlt <- as.POSIXlt(tlim, origin="1970-01-01", tz="UTC")
    tlimct <- as.POSIXct(tlimlt)
    tlabcex <- 0.8
    tlabsrt <- 0

    # time labels
    tlablt <- as.POSIXlt(pretty(tlimlt, n=10)) # this does not work with very small negative years, e.g. -800000 (800ka BP) 

    # remove lables which are possibly out of limits due to pretty
    tlab_diff_secs <- as.numeric(diff(range(tlablt)), units="secs") # total time label distance
    if (any(tlablt < tlimlt[1])) {
        # check if the too early autmatic time labels are negligible
        overshoot_diff <- abs(as.numeric(tlablt[tlablt < tlimlt[1]], units="secs")) - abs(tlim[1])
        overshoot_rel <- overshoot_diff/tlab_diff_secs*100
        if (any(overshoot_rel > 1)) { # only change pretty labels if overoot is > 1% of total time label range  
            message("remove some automatic labels < ", tlimlt[1], " ...")
            print(tlablt[which(tlablt < tlimlt[1])[overshoot_rel > 1]])
            tlablt <- tlablt[-which(tlablt < tlimlt[1])[overshoot_rel > 1]]
        }
    }
    if (any(tlablt > tlimlt[2])) {
        # check if the too late automatic time labels are negligible
        overshoot_diff <- abs(as.numeric(tlablt[tlablt > tlimlt[2]], units="secs")) - abs(tlim[2])
        overshoot_rel <- overshoot_diff/tlab_diff_secs*100
        if (any(overshoot_rel > 1)) { # only change pretty labels if overoot is > 1% of total time label range  
            message("remove some automatic labels > ", tlimlt[2], " ...")
            print(tlablt[which(tlablt > tlimlt[2])[overshoot_rel > 1]])
            tlablt <- tlablt[-which(tlablt > tlimlt[2])[overshoot_rel > 1]]
        }
    }
    tatn <- as.numeric(tlablt)

    # modify time axis labels YYYY-MM-DD depending on range covered:
    if (tlab_diff_secs > 365*24*60*60) { # do not show days if range of tlim is above 1 year
        message("time lims is longer than 1 year, modify automatic time labels ...")
        tlablt <- unclass(tlablt)$year + 1900 # -> YYYY; this destroys POSIX object
    } else { # decrease label size due to long labels
        message("change time label angle ...")
        tlabsrt <- 45
    }

    # if all dates < 0, use "abs(dates) BP" instead
    tunit <- "Time"
    if (any(tlablt < 0)) {
        message("some times are < 0 --> use \"abs(times)\" for time labels instead ...")
        neg_inds <- which(tlablt < 0)
        tlablt[neg_inds] <- abs(tlablt[neg_inds])
        if (!is.na(time_ref)) {
            tunit <- paste0("Year before ", time_ref)
        } else {
            tunit <- "Year before `time_ref`"
        }
    }

    message("tlablt = ", paste(tlablt, collapse=", "))
    message("tunit = ", tunit)

} # get time axis labels
# finished getting time axis labels

message("\n", "****************** start plotting of `datas` ***************************")

## start dimension-specific plots for each variable of `datas`
for (vi in 1:length(varnames_unique)) {

    # prepare datas plots
    varname <- varnames_unique[vi]
    z <- vector("list", l=nsettings)
    names(z) <- names_short
    if (exists("datasma")) zma <- z
    for (i in 1:nsettings) {
        varind <- which(names(datas[[i]]) == varname)
        if (length(varind) == 1) {
            z[[i]] <- datas[[i]][[varind]]
            if (exists("datasma")) zma[[i]] <- datasma[[i]][[varind]]
        } else {
            stop("not defined")
        }
    } # for all settings
    vardims <- lapply(z, function(x) attributes(x)$dims)
    vardims_unique <- c()
    for (i in 1:nsettings) {
        vardims_unique <- unique(c(vardims_unique, unique(vardims[[i]])))
    }
    ndims_unique <- length(vardims_unique)
    for (di in 1:ndims_unique) {
        cmd <- paste0(vardims_unique[di], "_dim <- vector(\"list\", l=", nsettings, ")")
        #message(cmd, " ...")
        eval(parse(text=cmd))
        for (i in 1:nsettings) {
            dimind <- which(attributes(z[[i]])$dims == vardims_unique[di])
            if (length(dimind) == 1) {
                cmd <- paste0(vardims_unique[di], "_dim[[", i, "]] <- dims[[i]]$", vardims_unique[di])
                #message(cmd)
                eval(parse(text=cmd))
                cmd <- paste0("names(", vardims_unique[di], "_dim)[", i, "] <- \"", vardims_unique[di], "\"")
                #message(cmd)
                eval(parse(text=cmd))
            } # if dim di is a dim of variable vi of setting i
        } # for i nsetting
    } # for di in vardims_unique
    # finished plot preparation
    message("\n", "var ", vi, "/", length(varnames_unique), ": \"", varname, "\" of `datas` has dims \"", 
            paste(vardims_unique, collapse="\",\""), "\". check if this case is defined ...")

    make_regular_grid <- F
    if (make_regular_grid) {
        message("\n", "`make_regular_grid`=T --> make regular grid for using `useRaster`=T")
        for (di in 1:ndims_unique) {
            for (i in 1:nsettings) {
                dimind <- which(attributes(z[[i]])$dims == vardims_unique[di])
                if (length(dimind) == 1) {
                    #cmd <- paste0(vardims_unique[di], "_dim[[", i, "]] <- dims[[i]]$", vardims_unique[di])
                    #message(cmd)
                    #eval(parse(text=cmd))
                    #z[[i]][[vi]] <- interp.surface.grid(obj=list(x=time_list[[i]],
                    #                                   y=depth_list[[i]],
                    #                                   z=datats_list[[i]][[varinds[j]]]),
                    #                          grid.list=list(x=time_list[[i]],
                    #                                         y=depths_interp_all[[i]]))$z
                }
            }
        }
    } # if make_regular_grid


    ### 2 dims
    ## plot `datas` as lon vs lat
    if (ndims_unique == 2 && all(vardims_unique %in% c("lon", "lat"))) {

        message("\n", varname, " ", mode, " plot lon vs lat ...")

        # colorbar values
        source(paste0(homepath, "/functions/image.plot.pre.r"))
        ip <- image.plot.pre(range(z, na.rm=T), verbose=F)

        # determine number of rows and columns
        source(paste0(homepath, "/functions/image.plot.nxm.r"))
        nm <- image.plot.nxm(x=lon_dim, y=lat_dim, z=z, ip=ip, dry=T)

        plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                           varname, "_", 
                           paste0(names_short, "_", seasonsp, 
                                  "_", froms_plot, "_to_", tos_plot, "_", areas, collapse="_vs_"), 
                           ".", p$plot_type)
        message("plot ", plotname, " ...")
        dir.create(dirname(plotname), recursive=T, showWarnings=F)
        if (p$plot_type == "png") {
            png(plotname, width=nm$ncol*p$map_width, height=nm$nrow*p$map_height,
                res=p$dpi, family=p$family_png)
        } else if (p$plot_type == "pdf") {
            pdf(plotname, width=nm$ncol*p$inch, height=p$inch*((nm$nrow*p$map_height)/(nm$ncol*p$map_width)),
                family=p$family_pdf)
        }

        # map plot
        data_info <- data_infos[[which(sapply(data_infos, names) == varname)[1]]][[varname]]
        add_land <- "world"
        if (mode == "area") { # fesom
            add_land <- F
        }
        image.plot.nxm(x=lon_dim, y=lat_dim, z=z, ip=ip, verbose=T,
                       xlab="Longitude [°]", ylab="Latitude [°]", 
                       zlab=data_info$label, znames=names_legend,
                       add_land=add_land)
        
        message("\n", "save plot ", plotname, " ...")
        dev.off()
        if (p$plot_type == "pdf") {
            if("extrafont" %in% (.packages())){
                extrafont::embed_fonts(plotname, outfile=plotname)
            } else {
                grDevices::embedFonts(plotname, outfile=plotname)
            }
        }
   
        # anomaly lon,lat plot of 2 settings 
        if (nsettings == 2) {

            message("\n`nsettings` = 2 ", appendLF=F)

            if (areas[1] == areas[2]) {

                message("AND areas[1] = \"", areas[1], "\" = areas[2] = \"", areas[2], "\" ", appendLF=F)

                if (length(lon_dim[[1]]) == length(lon_dim[[2]]) &&
                    length(lat_dim[[1]]) == length(lat_dim[[2]])) {
                
                    message("AND both settings have same number of lons and lats\n",
                            "--> plot (lon,lat) anomalies 2 minus 1: ", names_short[2], " minus ", names_short[1], " ...")
                    
                    # colorbar values
                    zanom <- list(z[[2]] - z[[1]])
                    names(zanom) <- paste0(names_legend[2], " minus ", names_legend[1])
                    source(paste0(homepath, "/functions/image.plot.pre.r"))
                    ip <- image.plot.pre(range(zanom, na.rm=T), verbose=F)

                    # determine number of rows and columns
                    source(paste0(homepath, "/functions/image.plot.nxm.r"))
                    nm <- image.plot.nxm(x=list(lon_dim[[1]]), y=list(lat_dim[[1]]), z=zanom, ip=ip, dry=T)

                    plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                                       varname, "_", 
                                       paste0(rev(names_short), "_", rev(seasonsp), 
                                              "_", rev(froms_plot), "_to_", rev(tos_plot), "_", rev(areas), 
                                              collapse="_minus_"), 
                                       ".", p$plot_type)
                    message("plot ", plotname, " ...")
                    dir.create(dirname(plotname), recursive=T, showWarnings=F)
                    if (p$plot_type == "png") {
                        png(plotname, width=nm$ncol*p$map_width, height=nm$nrow*p$map_height,
                            res=p$dpi, family=p$family_png)
                    } else if (p$plot_type == "pdf") {
                        pdf(plotname, width=nm$ncol*p$inch, height=p$inch*((nm$nrow*p$map_height)/(nm$ncol*p$map_width)),
                            family=p$family_pdf)
                    }

                    # map plot
                    data_info <- data_infos[[which(sapply(data_infos, names) == varname)[1]]][[varname]]
                    add_land <- "world"
                    if (mode == "area") { # fesom
                        add_land <- F
                    }
                    image.plot.nxm(x=list(lon_dim[[1]]), y=list(lat_dim[[1]]), z=zanom, ip=ip, verbose=T,
                                   xlab="Longitude [°]", ylab="Latitude [°]", 
                                   zlab=data_info$label,
                                   add_land=add_land)
                    
                    message("\n", "save plot ", plotname, " ...")
                    dev.off()
                    if (p$plot_type == "pdf") {
                        if("extrafont" %in% (.packages())){
                            extrafont::embed_fonts(plotname, outfile=plotname)
                        } else {
                            grDevices::embedFonts(plotname, outfile=plotname)
                        }
                    }

                } else { # if lon_dim and lat_dim of both settings are of same length
                    message("\nbut lon and lat dims of both settings are of different length --> cannot plot anomaly")
                }
            } else { # both areas are not equal
                message("\nbut areas[1] = \"", areas[1], "\" != areas[2] = \"", areas[2], "\" --> cannot plot anomaly")
            }
        } # if nsettings == 2
        # finished anomaly plot of 2 dims (lon,lat)

    } # if (ndims_unique == 2 && all(vardims_unique %in% c("lon", "lat"))) {
    # finished plot `datas` as lon vs lat


    ## plot `datas` as time vs lat
    if (ndims_unique == 2 && all(vardims_unique %in% c("time", "lat"))) {
    
        message("\n", varname, " ", mode, " plot time vs lat ...")
        data_info <- data_infos[[which(sapply(data_infos, names) == varname)[1]]][[varname]]

        if (add_smoothed) {
            message("\n", "`add_smoothed` = T --> replace z with zma ...")
            z <- zma
        }
        
        if (F) {
            message("\nspecial: plot values relative to last time point")
            for (i in 1:nsettings) {
                tmp <- drop(z[[i]][length(dims[[i]]$time),])
                message("min / max (z[[", i, "]][", length(dims[[i]]$time), ",] = ", 
                        min(tmp, na.rm=T), " / ", max(tmp, na.rm=T))
                tmp <- replicate(tmp, n=length(dims[[i]]$time))
                tmp <- aperm(tmp, c(2, 1))
                z[[i]] <- tmp - z[[i]]
            }
            data_info$label <- paste0(data_info$label, "\nw.r.t. present")
            message("\n")
        }

        message("get global zlim ... ", appendLF=F)
        zlim <- range(z, na.rm=T)
        message("min/max = ", zlim[1], " / ", zlim[2])
        nlevels <- 20
        if (varname == "srad0d" && F) {
            message("special zlevels")
            # levels/colors as marcott et al. 2013 Fig. 2 A December
            zlevels <- seq(-34, 10, b=4)
            if (min(zlevels) > zlim[1]) zlevels <- c(zlim[1], zlevels)
            if (max(zlevels) < zlim[2]) zlevels <- c(zlevels, zlim[2])
            pos_cols <- c("#fbe4f3", "#f7b9de", "#f591cb", "#ec1168")
            neg_cols <- c("#5b58b0", "#5b58b0", "#c6b7df", "#edeaf7", "#fafbfb")
        } else if (varname == "srad0d" && F) {
            message("special zlevels")
            # levels/colors as marcott et al. 2013 Fig. 2 B June
            zlevels <- seq(-4, 36, b=4)
            if (min(zlevels) > zlim[1]) zlevels <- c(zlim[1], zlevels)
            if (max(zlevels) < zlim[2]) zlevels <- c(zlevels, zlim[2])
            pos_cols <- c("#fbd3eb", "#f693cc", "#f584c6", "#ef47a8", "#ec0f64")
            neg_cols <- "#5b5cb2"
        } else {
            zlevels <- NULL
            pos_cols <- NULL
            neg_cols <- NULL
            nlevels <- 11
        }
        source(paste0(homepath, "/functions/image.plot.pre.r"))
        ip <- image.plot.pre(zlim, nlevels=nlevels,
                             palname="RdBu", 
                             center_include=T, pos_cols=pos_cols, neg_cols=neg_cols)

        plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                           varname, "_",  
                           paste0(names_short, "_", areas, "_", seasonsp, "_", 
                                  froms_plot, "_to_", tos_plot, 
                                  collapse="_vs_"), 
                           ".", p$plot_type)
        message("plot ", plotname, " ...")
        dir.create(dirname(plotname), recursive=T, showWarnings=F)
        
        # determine number of rows and columns
        source(paste0(homepath, "/functions/image.plot.nxm.r"))
        nm <- image.plot.nxm(x=time_dim, y=lat_dim, z=z, n=1, m=2, ip=ip, dry=T)
        
        if (p$plot_type == "png") {
            png(plotname, width=nm$ncol*p$map_width, height=nm$nrow*p$map_height,
                res=p$dpi, family=p$family_png)
        } else if (p$plot_type == "pdf") {
            pdf(plotname, width=nm$ncol*p$inch, height=p$inch*((nm$nrow*p$map_height)/(nm$ncol*p$map_width)),
                family=p$family_pdf)
        }

        # plot
        image.plot.nxm(x=time_dim, y=lat_dim, z=z, n=1, m=2, ip=ip, verbose=F,
                       add_contour=F,
                       #xlim=tlimct, 
                       x_at=tatn, x_labels=tlablt, 
                       xlab=tunit, ylab="Latitude [°]",
                       zlab=data_info$label, znames=names_legend)
    
        message("\n", "save plot ", plotname, " ...")
        dev.off()
        if (p$plot_type == "pdf") {
            if("extrafont" %in% (.packages())){
                extrafont::embed_fonts(plotname, outfile=plotname)
            } else {
                grDevices::embedFonts(plotname, outfile=plotname)
            }
        }

    } # if (ndims_unique == 2 && all(vardims_unique %in% c("time", "lat"))) {
    # finished plot `datas` as time vs lat
    

    ## plot `datas` as time vs depth
    if (ndims_unique == 2 && all(vardims_unique %in% c("time", "depth"))) {
   
        if (add_smoothed) {
            message("\n", "`add_smoothed` = T --> replace z with zma ...")
            z <- zma
        }

        # use km instead of m as depth unit
        if (T) {
            message("divide depth dimension by 1000 m --> km")
            depth_dim <- lapply(depth_dim, "/", 1000)
            ylab <- "Depth [km]"
        } else {
            ylab <- "Depth [m]"
        }

        if (add_ts_to_time_vs_depth) {
            message("\n", "add time series to datas time vs depth plot ...")

        } # if add_ts_to_time_vs_depth

        # colorbar values
        zlim <- range(z, na.rm=T)
        zlevels <- NULL
        if (F && varname == "thetao") {
            zlevels <- seq(ceiling(zlim[1]), min(10, zlim[2]), b=1)
        }
        source(paste0(homepath, "/functions/image.plot.pre.r"))
        ip <- image.plot.pre(zlim, zlevels=zlevels, verbose=T, axis.addzlims=T)

        # determine number of rows and columns
        source(paste0(homepath, "/functions/image.plot.nxm.r"))
        nm <- image.plot.nxm(x=time_dim, y=depth_dim, z=z, ip=ip, dry=T)

        plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                           varname, "_", 
                           paste0(names_short, "_", areas, "_", seasonsp, "_", 
                                  froms_plot, "_to_", tos_plot, "_", 
                                  depth_fromsp, "-", depth_tosp, "m",
                                  collapse="_vs_"), 
                           ".", p$plot_type)
        message("plot ", plotname, " ...")
        dir.create(dirname(plotname), recursive=T, showWarnings=F)
        if (p$plot_type == "png") {
            png(plotname, width=nm$ncol*p$ts_width, height=nm$nrow*p$ts_height,
                res=p$dpi, family=p$family_png)
        } else if (p$plot_type == "pdf") {
            pdf(plotname, width=nm$ncol*p$inch, height=p$inch*((nm$nrow*p$ts_height)/(nm$ncol*p$ts_width)),
                family=p$family_pdf)
        }

        # plot
        data_info <- data_infos[[which(sapply(data_infos, names) == varname)[1]]][[varname]]
        image.plot.nxm(x=time_dim, y=depth_dim, z=z, ip=ip, verbose=T,
                       #xlim=tlimct, 
                       x_at=tatn, x_labels=tlablt, 
                       xlab="Time", ylab=ylab,
                       zlab=data_info$label, znames=names_legend)
    
        message("\n", "save plot ", plotname, " ...")
        dev.off()
        if (p$plot_type == "pdf") {
            if("extrafont" %in% (.packages())){
                extrafont::embed_fonts(plotname, outfile=plotname)
            } else {
                grDevices::embedFonts(plotname, outfile=plotname)
            }
        }

    } # if (ndims_unique == 2 && all(vardims_unique %in% c("time", "depth"))) {
    # finished plot `datas` as time vs depth


    ### 1 dim
    ## plot `datas` as time 
    if (ndims_unique == 1 && vardims_unique == "time") {

        if (!add_unsmoothed && !add_smoothed) {
            warning("both `add_unsmoothed=F` and `add_smoothed=F`. set `add_unsmoothed=T` to show time series.")
            add_unsmoothed <- T # default
        }

        message("\n", varname, " ", mode, " plot vs time ...")

        # prepare right axis data if necessary
        if (add_data_right_yaxis_ts) {
            message("\n", "prepare data right yaxis ..")
            data_right <- list(data=list())
            if (exists("add_co2_hist") && add_co2_hist) {
                data_right$data$co2_hist <- list(x=co2_hist$time, y=co2_hist$co2_ppm, 
                                                 col=co2_hist$col, lty=co2_hist$lty, 
                                                 lwd=co2_hist$lwd, pch=co2_hist$pch)
                data_right$label <- substitute(paste("CO"[2], " [ppm]"))
                data_right$suffix <- "_with_CO2"
            }
            if (exists("add_co2_1pct") && add_co2_1pct) {
                data_right$data$co2_1pct <- list(x=co2_1pct$time, y=co2_1pct$co2_ppm, 
                                                 col=co2_1pct$col, lty=co2_1pct$lty, 
                                                 lwd=co2_1pct$lwd, pch=co2_1pct$pch)
                data_right$label <- substitute(paste("CO"[2], " [ppm]"))
                data_right$suffix <- "_with_CO2"
            }
            if (exists("add_co2_4co2") && add_co2_4co2) {
                data_right$data$co2_4co2 <- list(x="const", y=co2_4co2$co2_ppm, 
                                                 col=co2_4co2$col, lty=co2_4co2$lty, 
                                                 lwd=co2_4co2$lwd, pch=co2_4co2$pch)
                data_right$label <- substitute(paste("CO"[2], " [ppm]"))
                data_right$suffix <- "_with_CO2"
            }
            if (exists("add_orb_berger_eccentricity") && add_orb_berger_eccentricity) {
                data_right$data$orb_berger_eccentricity <- list(x=orb_berger$time, y=orb_berger$eccentricity, 
                                                                col=orb_berger$col, lty=orb_berger$lty, 
                                                                lwd=orb_berger$lwd, pch=orb_berger$pch)
                data_right$label <- "Eccentricity"
                data_right$suffix <- "_with_orb_berger_ecc"
            }
            if (exists("add_orb_berger_precession") && add_orb_berger_precession) {
                data_right$data$orb_berger_precession <- list(x=orb_berger$time, y=orb_berger$precession, 
                                                              col=orb_berger$col, lty=orb_berger$lty, 
                                                              lwd=orb_berger$lwd, pch=orb_berger$pch)
                data_right$label <- "Precession"
                data_right$suffix <- "_with_orb_berger_prec"
            }
            if (exists("add_orb_berger_obliquity") && add_orb_berger_obliquity) {
                data_right$data$orb_berger_obliquity <- list(x=orb_berger$time, y=orb_berger$obliquity, 
                                                             col=orb_berger$col, lty=orb_berger$lty, 
                                                             lwd=orb_berger$lwd, pch=orb_berger$pch)
                data_right$label <- "Obliquity"
                data_right$suffix <- "_with_orb_berger_obliq"
            }
            if (F) {
                if (F) { # CO2 of hist, 1pct and 4CO2
                } else if (F) { # volcanic aerosols
                    ncin <- nc_open(paste0("/work/ab0246/a270073/post/echam6/volint/tau_aero_550/",
                                           "hist_echam6_echammon_yearmean_awi-esm-1-1-lr_volint_",
                                           "selname_tau_aero_550_global_Jan-Dec_1850-2014.nc"))
                    if (F) {
                        data_right <- list(data=list("tau_hist"=list(x=as.POSIXlt(ncin$dim$time$vals*86400, origin="1538-01-01", tz="UTC"), 
                                                                     y=ncvar_get(ncin, "tau_aero_550"),
                                                                     col="#377EB8", lty=1, lwd=0.5)),
                                           label=eval(substitute(expression(paste(integral(), " ", tau, " dV [m"^3, "]")))),
                                           suffix="_with_volcanic_aerosols")
                    } else if (T) {
                        data_right <- list(data=list("tau_hist"=list(x=as.POSIXlt(ncin$dim$time$vals*86400, origin="1538-01-01", tz="UTC"), 
                                                                     y=scale(ncvar_get(ncin, "tau_aero_550")),
                                                                     text="Aerosol optical thickness", col="#377EB8", lty=1, 
                                                                     lwd=0.5, pch=NA),
                                                     "tsi_hist"=list(x=tsi_hist_annual$time, y=scale(tsi_hist_annual$tsi_hist),
                                                                     text="Total solar irradiance", 
                                                                     col=tsi_hist_annual$col, lty=tsi_hist_annual$lty, 
                                                                     lwd=tsi_hist_annual$lwd, pch=tsi_hist_annual$pch),
                                                     "co2_hist"=list(x=co2_hist$time, y=scale(co2_hist$co2_ppm),
                                                                     text=eval(substitute(expression(paste("CO"[2])))),
                                                                     col=co2_hist$col, lty=co2_hist$lty, 
                                                                     lwd=co2_hist$lwd, pch=co2_hist$pch)),
                                           label="Index",
                                           suffix="_with_volcanic_aerosols_and_TSI_and_CO2")
                    }
                }
            }
            if (varname == "siarean") {
                data_right <- list(data=vector("list", l=nsettings))
                names(data_right$data) <- names_short
                for (i in 1:nsettings) {
                    inpath <- paste0(workpath, "/post/", models[i], "/", mode, "/siareas") 
                    fname <- paste0(prefixes[i], "_", mode, 
                                    codesf[i], "_siareas_antarctic",
                                    "_", seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                                    depthsf[i], 
                                    ".nc") # todo: levs 
                    ncin <- nc_open(paste0(inpath, "/", fname))
                    data_right$data[[i]] <- list(x=time_dim[[i]],
                                                 y=ncvar_get(ncin, "siareas"),
                                                 text=names_legend[i], col=cols[i], 
                                                 lty=3, lwd=1, pch=NA)
                }
                data_right$label <- eval(substitute(expression(paste("SH sea ice extent [km"^2, 
                                                                            " " %*% " ", 10^6, "]"))))
                data_right$suffix <- "_with_siareas"
            } # siarean
            if (varname == "temp2") {
                data_right <- list(data=vector("list", l=nsettings))
                names(data_right$data) <- names_short
                for (i in 1:nsettings) {
                    inpath <- paste0(workpath, "/post/", models[i], "/", mode, "/wisoaprt_d") 
                    fname <- paste0(prefixes[i], "_", mode, 
                                    codesf[i], "_wisoaprt_d_sellevel_2_", areas[i],
                                    "_", seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                                    depthsf[i], 
                                    ".nc") # todo: levs 
                    ncin <- nc_open(paste0(inpath, "/", fname))
                    message("read ", inpath, "/", fname, " ...")
                    data_right$data[[i]] <- list(x=time_dim[[i]],
                                                 y=ncvar_get(ncin, "wisoaprt_d"),
                                                 text="wisoaprt_d_sellevel_2", 
                                                 #col=cols[i], 
                                                 cols="#E41A1C",
                                                 lty=1, lwd=1, pch=NA)
                }
                data_right$label <- eval(substitute(expression(paste(delta, ""^18, "O [\u2030]")))) 
                data_right$suffix <- "_with_wisoaprt_d_sellevel_2"
            } # temp2
            # finished variable specific stuff for add_data_right_yaxis_ts
            
            # check
            if (length(data_right$data) == 0) {
                warning("you provided `add_data_right_yaxis_ts=T` but did not ",
                        "define which data should be plotted on right yaxis.\n",
                        " --> set `add_data_right_yaxis_ts=F` and continue ...")
                add_data_right_yaxis_ts <- F
                data_right <- list(suffix="") # default
            }

        } else { # add_data_right_yaxis_ts=F
            data_right <- list(suffix="") # default
        } # if add_data_right_yaxis_ts

        # after check
        if (add_data_right_yaxis_ts) {
            
            nsettings_right <- length(data_right$data)

            if (add_smoothed) {
                for (i in 1:nsettings_right) {
                    data_right$data[[i]]$yma <- filter(data_right$data[[i]]$y, rep(1/n_mas[i], t=n_mas[i]))
                }
            }

            if (!exists("ylim_right")) { # possibly set by user
                message("use automatic data right yaxis limits ...")
                ylim_right <- vector("list", l=length(data_right$data))
                ylim_right_ma <- ylim_right
                for (i in 1:nsettings_right) {
                    if (length(data_right$data[[i]]$x) == 1 && data_right$data[[i]]$x == "const") {
                        ylim_right[[i]] <- range(data_right$data[[i]]$y, na.rm=T)
                    } else {
                        timeinds <- which(data_right$data[[i]]$x >= tlimlt[1] & data_right$data[[i]]$x <= tlimlt[2])
                        if (length(timeinds) == 0) {
                            message("all data of data_right$data[[", i, "]]: ", names(data_right$data)[1], " are out of tlimlt")
                            ylim_right[[i]] <- NA
                        } else {
                            ylim_right[[i]] <- range(data_right$data[[i]]$y[timeinds], na.rm=T)
                            if (add_smoothed) {
                                ylim_right_ma[[i]] <- range(data_right$data[[i]]$yma[timeinds], na.rm=T)
                            }
                        }
                    }
                } # i in data_right
                if ((add_unsmoothed && add_smoothed) ||
                    (add_unsmoothed && !add_smoothed)) {
                    ylim_right <- range(ylim_right)
                } else if (!add_unsmoothed && add_smoothed) {
                    ylim_right <- range(ylim_right_ma)
                }
            } # if ylim_right does not already exist
            
            # add obs to ylim_right
            if (T && varname == "siarean") {
                message("\n", "add nsidc annual to right ylim ...")
                ylim_right <- range(ylim_right, nsidc_siareas_annual$siareas, na.rm=T)
            } # if add nsidc
            message("\n", "ylim_right=", appendLF=F)
            dput(ylim_right)
            if (!exists("yat_right")) {
                message("use automatic data right yaxis labels ...")
                yat_right <- pretty(ylim_right, n=8)
            }
            ylab_right <- format(yat_right, trim=T)
        } 
        # if add_data_right_yaxis_ts finished prepare right axis data

        # ylims of model data
        if (add_unsmoothed) {
            message("\n", mode, " versus time min / mean / max ", varname, " z:")
            for (i in 1:nsettings) {
                message(names_short[i], ": ", min(z[[i]], na.rm=T), " / ",
                        mean(z[[i]], na.rm=T), " / ", max(z[[i]], na.rm=T))
            }
            ylim <- range(z, na.rm=T)
        }
        if (exists("zma")) {
            message("\n", mode, " versus time min / mean / max ", varname, " zma:")
            for (i in 1:nsettings) {
                message(names_short[i], ": ", min(zma[[i]], na.rm=T), " / ",
                        mean(zma[[i]], na.rm=T), " / ", max(zma[[i]], na.rm=T))
            }
            ylimma <- range(zma, na.rm=T)
        }
        if (add_unsmoothed && add_smoothed) {
            ylim <- range(ylim, ylimma)
        } else if (!add_unsmoothed && add_smoothed) {
            ylim <- range(ylimma)
        }

        # ylim of obs, etc.
        if (F && varname == "temp2") {
            message("\n", "add hadcrut4_sat_anom, gistempv4_sat_anom to ylim ...")
            ylim <- range(ylim,
                          hadcrut4_sat_anom_annual$hadcrut4_sat_anom_lower_uncert,
                          hadcrut4_sat_anom_annual$hadcrut4_sat_anom_upper_uncert,
                          #gistempv4_sat_anom_annual$gistempv4_sat_anom, 
                          na.rm=T)
        } # if add som obs to ylim

        if (F && varname == "moc_max_26.25deg") {
            message("\n", "add rapid$moc_annual to ylim ...")
            ylim <- range(ylim, moc_rapid$moc_annual, na.rm=T)
        } # if add moc ts
        
        if (T && varname == "siarean") {
            message("\n", "add nsidc annual to ylim ...")
            ylim <- range(ylim, nsidc_siarean_annual$siarean, na.rm=T)
        } # if add nsidc

        message("\n", "ylim=", appendLF=F)
        dput(ylim)
        yat <- pretty(ylim, n=8)
        ylab <- format(yat, trim=T)

        # plotname
        plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                           varname, "_",
                           paste0(names_short, "_", seasonsp, 
                                  "_", froms_plot, "_to_", tos_plot, "_", areas, collapse="_vs_"), 
                           data_right$suffix, ts_highlight_seasons$suffix,
                           ".", p$plot_type)
        if (nchar(plotname) > nchar_max_foutname) {
            plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                               varname, "_",
                               paste0(names_short, "_", areas, collapse="_vs_"), 
                               data_right$suffix, ts_highlight_seasons$suffix,
                               ".", p$plot_type)
        }
        dir.create(dirname(plotname), recursive=T, showWarnings=F)
        if (p$plot_type == "png") {
            png(plotname, 
                width=p$ts_width, 
                height=p$ts_height,
                #height=p$ts_width,
                res=p$dpi, family=p$family_png)
        } else if (p$plot_type == "pdf") {
            pdf(plotname, width=p$inch, height=p$inch*p$ts_height/p$ts_width,
                family=p$family_pdf)
        }

        # set plot margins
        mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
        mar[4] <- 1 # decrease right margin
        if (!add_title) mar[3] <- 1 # decrease upper margin
        if (F) {
            if (tlabsrt == 0) mar[1] <- mar[1]/2  # decrease lower margin
        }
        if (add_data_right_yaxis_ts) mar[4] <- mar[2] # same as left  

        # open plot
        par(mar=mar)
        plot(time_dim[[1]], z[[1]], t="n",
             xlim=tlim, ylim=ylim, 
             xaxt="n", yaxt="n",
             xlab=NA, ylab=NA)
        if (tlabsrt == 0) { # add horizontal labels (the default)
            axis(1, at=tatn, labels=tlablt, cex.axis=tlabcex)
        } else { # add non-horizontal labels with angle
            axis(1, at=tatn, labels=NA)
            # character height in user coordinates
            text(x=tatn, y=par("usr")[3] - strheight("1"), labels=tlablt, 
                 xpd=T, srt=tlabsrt, adj=c(1, 1), cex=tlabcex)
        }
        axis(2, at=yat, labels=ylab, las=2)

        # add time label on x-axis
        mtext(side=1, tunit, line=3, cex=0.9)

        # add variable label on y-axis
        first_setting_with_varname <- sapply(lapply(data_infos, names), function(x) which(x == varname))[1]
        data_info <- data_infos[[names(first_setting_with_varname)]][[first_setting_with_varname]]
        mtext(side=2, data_info$label, line=4.5, cex=0.9)
        
        # add title
        if (add_title) {
            title <- paste0(paste(unique(areas), collapse=","), 
                            " ", mode, " ", varname, " ", 
                            paste(unique(seasonsp), collapse=","), " ", 
                            paste(unique(fromsp), collapse=","), " to ", 
                            paste(unique(tosp), collapse=","))
            title(title, cex.main=0.75)
        }

        # add grid
        if (add_xgrid) {
            message("\n", "add xgrid ...")
            abline(v=tatn, col="gray", lwd=0.5)
        }
        if (add_ygrid) {
            message("\n", "add ygrid ...")
            abline(h=yat, col="gray", lwd=0.5)
        }

        # add zero line
        if (add_zeroline) {
            abline(h=0, col="gray", lwd=0.5)
        }

        ## add obs, etc.
        if (F && varname == "temp2") {
            message("\n", "add hadcrut4_sat_anom, gistempv4_sat_anom to plot ...")
            polygon(c(as.POSIXct(hadcrut4_sat_anom_annual$time), 
                      rev(as.POSIXct(hadcrut4_sat_anom_annual$time))),
                    c(hadcrut4_sat_anom_annual$hadcrut4_sat_anom_lower_uncert,
                      rev(hadcrut4_sat_anom_annual$hadcrut4_sat_anom_upper_uncert)),
                    col=hadcrut4_sat_anom_annual$col_rgb, border=NA)
            lines(hadcrut4_sat_anom_annual$time, hadcrut4_sat_anom_annual$hadcrut4_sat_anom,
                  col=hadcrut4_sat_anom_annual$col, lty=hadcrut4_sat_anom_annual$lty,
                  lwd=hadcrut4_sat_anom_annual$lwd)
            #lines(gistempv4_sat_anom_annual$time, gistempv4_sat_anom_annual$gistempv4_sat_anom,
            #      col=cols[2], lwd=2, lty=2)
        }
        if (F && varname == "moc_max_26.25deg") {
            message("\n", "add moc_rapid$moc_annual to plot ...")
            # exclude NA values
            nainds <- which(!is.na(moc_rapid$moc) & !is.na(moc_rapid$moc_error)) 
            #polygon(c(as.POSIXct(moc_rapid$time[nainds]), rev(as.POSIXct(moc_rapid$time[nainds]))),
            #        c(moc_rapid$moc[nainds] - moc_rapid$moc_error[nainds], 
            #          rev(moc_rapid$moc[nainds] + moc_rapid$moc_error[nainds])),
            #        col=moc_rapid$col_rgb, border=NA)
            lines(moc_rapid$time, moc_rapid$moc_annual,
                  col=moc_rapid$col, lty=moc_rapid$lty,
                  lwd=moc_rapid$lwd)
        }
        if (T && varname == "siarean") {
            message("\n", "add nsidc annual to plot ...")
            lines(nsidc_siarean_annual$time, nsidc_siarean_annual$siarean,
                  col=nsidc_siarean_annual$col, lty=nsidc_siarean_annual$lty,
                  lwd=nsidc_siarean_annual$lwd)
        }
        # finished adding obs

        # add data
        # unsmoothed before smoothed data
        #if (!is.null(data[[i]])) {
        if (add_unsmoothed) {
            if (ts_highlight_seasons$bool) {
                message("\n`ts_highlight_seasons$bool`=T ...")
                for (i in 1:nsettings) {
                    season_numbers_all <- dims[[i]]$timelt$mon + 1 # numeric [1,...,12] (i.e. not e.g. "01")
                    for (seasi in 1:length(ts_highlight_seasons$seasons)) {
                        season <- ts_highlight_seasons$season[seasi]
                        season_inds <- regexpr(season, season_check$string)
                        if (any(season_inds != -1)) {
                            season_numbers <- season_check$inds[season_inds:(season_inds+attributes(season_inds)$match.length-1)]
                        } else {
                            stop("implement")
                        }
                        message("   ", seasi, "/", length(ts_highlight_seasons$seasons), ": \"", 
                                season, "\" -> found season numbers = ", paste(season_numbers, collapse=","))
                        season_inds <- match(season_numbers_all, season_numbers)
                        #data.frame(dims[[1]]$timelt[1:100], season_inds[1:100])
                        season_inds <- which(!is.na(season_inds))
                        #data.frame(dims[[1]]$timelt[season_inds[1:100]], season_inds[1:100])
                        if (length(season_inds) == 0) {
                            warning("did not find any of these month numbers in time dimension values of data, skip")
                        } else {
                            message("   --> min / max (dims[[", i, "]]$timelt[season_inds]) = ", 
                                    min(dims[[i]]$timelt[season_inds]), " / ", max(dims[[i]]$timelt[season_inds]))
                            lines(time_dim[[i]][season_inds], z[[i]][season_inds], 
                                  t=ts_highlight_seasons$t,
                                  col=ts_highlight_seasons$cols[seasi],
                                  lty=ts_highlight_seasons$ltys[seasi],
                                  #lty=ltys[i], 
                                  lwd=ts_highlight_seasons$lwds[seasi],
                                  #pch=pchs[i]
                                  pch=ts_highlight_seasons$pchs[seasi], cex=0.2
                                 )
                        } # if wanted month numbers were found in time dimension values of data
                    } # for seasi in ts_highlight_seasons$seasons
                } # for i nsettings
                
                if (add_legend && i == nsettings) {
                    message("\n", "add default stuff to ", mode, " legend ...")
                    le <- list()
                    le$pos <- "bottom" 
                    le$ncol <- length(ts_highlight_seasons$seasons)
                    le$text <- ts_highlight_seasons$seasons
                    le$col <- ts_highlight_seasons$cols
                    le$pch <- ts_highlight_seasons$pchs
                    le$lty <- rep(NA, t=length(ts_highlight_seasons$seasons))
                    le$lwd <- rep(NA, t=length(ts_highlight_seasons$seasons))
                    le$cex <- 0.85
                    # reorder reading direction from R's default "top-to-bottom-and-then-left-to-right" 
                    # to "left-to-right-and-then-top-to-bottom"
                    le <- reorder_legend(le)
                    if (length(le$pos) == 1) {
                        legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                               pch=le$pch, col=le$col, ncol=le$ncol,
                               x.intersp=0.2, cex=le$cex, bty="n")
                    } else if (length(le$pos) == 2) {
                        legend(x=le$pos[1], y=le$pos[2],
                               legend=le$text, lty=le$lty, lwd=le$lwd,
                               pch=le$pch, col=le$col, ncol=le$ncol,
                               x.intersp=0.2, cex=le$cex, bty="n")
                    }
                } # add season legend
                
                # finished if ts_highlight_seasons$bool

            } else { # default 
                for (i in 1:nsettings) {
                    lines(time_dim[[i]], z[[i]], 
                          col=ifelse(add_smoothed, cols_rgb[i], cols[i]), 
                          lty=ltys[i], lwd=lwds[i], pch=pchs[i])
                }
            }
        }

        # smoothed data after unsmoothed data
        if (add_smoothed) {
            for (i in 1:nsettings) {
                lines(time_dim[[i]], zma[[i]], 
                      col=cols[i], lty=ltys[i], lwd=lwds[i], pch=pchs[i])
            }
        }

        # special: add first data point
        if (add_first_data_point) {
            for (i in 1:nsettings) {
                if (i == 1) message("\n", "add first data point")
                points(time_dim[[i]][1], z[[i]][1], 
                       col=cols[i], lty=ltys[i], lwd=lwds[i], 
                       pch=1)
            }
        } # add first data point

        # add linear regression trend if wanted
        if (add_linear_trend) {
            message("add_linear_trend not yet here")
        }
        
        # add legend if wanted
        if (add_legend) {
            message("\n", "add default stuff to ", mode, " legend ...")
            le <- list()
            le$pos <- "topleft" 
            #le$pos <- "bottomleft" 
            #le$pos <- "topright"
            #le$pos <- "bottomright" 
            #le$ncol <- nsettings/2
            le$ncol <- 1
            #le$ncol <- 2 
            le$text <- names_legend
            le$col <- cols
            le$lty <- ltys
            le$lwd <- lwds
            le$pch <- pchs
            le$cex <- 1
            le$cex <- 0.85
            # add stuf to legend here
            if (F && varname == "temp2") {
                message("\n", "add non hadcrut4 to ", mode, " datas legend ...")
                if (varname == "temp2") {
                    le$text <- c(le$text, hadcrut4_sat_anom_annual$text)
                    le$col <- c(le$col, hadcrut4_sat_anom_annual$col)
                    le$lty <- c(le$lty, hadcrut4_sat_anom_annual$lty)
                    le$lwd <- c(le$lwd, hadcrut4_sat_anom_annual$lwd)
                    le$pch <- c(le$pch, hadcrut4_sat_anom_annual$pch)
                }
            }
            # reorder reading direction from R's default top->bottom to left->right
            le <- reorder_legend(le)
            if (length(le$pos) == 1) {
                legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                       pch=le$pch, col=le$col, ncol=le$ncol,
                       x.intersp=0.2, cex=le$cex, bty="n")
            } else if (length(le$pos) == 2) {
                legend(x=le$pos[1], y=le$pos[2],
                       legend=le$text, lty=le$lty, lwd=le$lwd,
                       pch=le$pch, col=le$col, ncol=le$ncol,
                       x.intersp=0.2, cex=le$cex, bty="n")
            }
        } # if add_legend

        # add box before eventual right axis data
        box()

        if (add_data_right_yaxis_ts) {
            message("\n", "`add_data_right_yaxis_ts` = T --> add data right yaxis ...")
            par(new=T)
            plot(data_right$data[[1]]$x, data_right$data[[1]]$y, #log="y", 
                 t="n", xlim=tlim, ylim=ylim_right, 
                 xlab=NA, ylab=NA, axes=F)

            # add right axes in same color as the right data if 
            # all colors of the right data are the same
            if (length(unique(sapply(data_right$data, "[", "col"))) == 1) {
                right_axis_col <- data_right$data[[1]]$col
            } else {
                right_axis_col <- "black" # default
            }
            axis(4, at=yat_right, labels=ylab_right, las=2, 
                 col=right_axis_col, col.axis=right_axis_col, col.ticks=right_axis_col)
            mtext(side=4, data_right$label, line=4.5, cex=0.9, col=right_axis_col)

            # add obs before model data
            if (T && varname == "siarean") {
                message("\n", "add nsidc annual to right plot ...")
                lines(nsidc_siareas_annual$time, nsidc_siareas_annual$siareas,
                      col=nsidc_siareas_annual$col, lty=nsidc_siareas_annual$lty,
                      lwd=nsidc_siareas_annual$lwd)
            }

            # add unsmoothed right data before smoothed
            if (add_unsmoothed) {
                for (i in 1:length(data_right$data)) {
                    message(i, "/", length(data_right$data), ": ", names(data_right$data)[i], " unsmoothed ...")
                    if (length(data_right$data[[i]]$x) == 1 && data_right$data[[i]]$x == "const") {
                        abline(h=data_right$data[[i]]$y, 
                               col=data_right$data[[i]]$col, lty=data_right$data[[i]]$lty,
                               lwd=data_right$data[[i]]$lwd)
                    } else {
                        lines(data_right$data[[i]]$x, data_right$data[[i]]$y, 
                              col=data_right$data[[i]]$col, lty=data_right$data[[i]]$lty,
                              lwd=data_right$data[[i]]$lwd, pch=data_right$data[[i]]$pch)
                    }
                }
            }

            # add smoothed data after unsmoothed
            if (add_smoothed) {
                for (i in 1:length(data_right$data)) {
                    message(i, "/", length(data_right$data), ": ", names(data_right$data)[i], " smoothed ...")
                    if (length(data_right$data[[i]]$x) == 1 && data_right$data[[i]]$x == "const") {
                        abline(h=data_right$data[[i]]$yma, 
                               col=data_right$data[[i]]$col, lty=data_right$data[[i]]$lty,
                               lwd=data_right$data[[i]]$lwd)
                    } else {
                        lines(data_right$data[[i]]$x, data_right$data[[i]]$yma, 
                              col=data_right$data[[i]]$col, lty=data_right$data[[i]]$lty,
                              lwd=data_right$data[[i]]$lwd, pch=data_right$data[[i]]$pch)
                    }
                }
            }

            if (add_legend_right_yaxis) {
                message("\n", "add default stuff to ", mode, " right_data legend ...")
                le <- list()
                le$pos <- "topright" 
                le$ncol <- 1
                le$text <- sapply(data_right$data, "[[", "text")
                le$col <- sapply(data_right$data, "[[", "col")
                le$lty <- sapply(data_right$data, "[[", "lty")
                le$lwds <- sapply(data_right$data, "[[", "lwd")
                le$pchs <- sapply(data_right$data, "[[", "pch")
                le$cex <- 1
                le$cex <- 0.85
                # add stuf to legend here
                if (F) {
                    message("\n", "add non default stuff to ", mode, " legend ...")
                }
                # reorder reading direction from R's default top->bottom to left->right
                le <- reorder_legend(le)
                if (length(le$pos) == 1) {
                    legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                           pch=le$pch, col=le$col, ncol=le$ncol,
                           x.intersp=0.2, cex=le$cex, bty="n")
                } else if (length(le$pos) == 2) {
                    legend(x=le$pos[1], y=le$pos[2],
                           legend=le$text, lty=le$lty, lwd=le$lwd,
                           pch=le$pch, col=le$col, ncol=le$ncol,
                           x.intersp=0.2, cex=le$cex, bty="n")
                }
            } # if add_legend

            if (add_cor_data_left_and_right_ts) {

                cor <- cor.test(datas[[i]][[1]], data_right$data[[i]]$y)
                # plusminus: %+-%
                subtitle <- substitute(paste("cor(", x, ",", y, ") = ", rfrom-rto),
                                       list(x=names(datas[[i]])[1], y=data_right$data[[i]]$text,
                                            rfrom=round(cor$conf.int[1], 3), rto=round(cor$conf.int[2], 3)))
                mtext(subtitle, cex=0.7)

            } # if add_cor_data_left_and_right_ts

        } # if add_data_right_yaxis_ts

        message("\n", "save plot ", plotname, " ...")
        dev.off()
        if (p$plot_type == "pdf") {
            if("extrafont" %in% (.packages())){
                extrafont::embed_fonts(plotname, outfile=plotname)
            } else {
                grDevices::embedFonts(plotname, outfile=plotname)
            }
        }
        
        if (ts_plot_each_setting_in_subplot) {

            message("\n`ts_plot_each_setting_in_subplot`=T ....")

            y <- z
            if (add_smoothed) y <- zma
            x_at <- x_lab <- x_labels <- y_lab <- vector("list", l=nsettings)
            for (i in 1:nsettings) {
                x_at[[i]] <- tatn
                x_labels[[i]] <- tlablt
                x_lab[[i]] <- tunit
                y_lab[[i]] <- data_info$label
            }
            
            # plotname
            plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                               varname, "_",
                               paste0(names_short, "_", seasonsp, 
                                      "_", froms_plot, "_to_", tos_plot, "_", areas, collapse="_vs_"), 
                               "_subplots", data_right$suffix, ts_highlight_seasons$suffix,
                               ".", p$plot_type)
            if (nchar(plotname) > nchar_max_foutname) {
                plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                                   varname, "_",
                                   paste0(names_short, "_", areas, collapse="_vs_"), 
                                   "_subplots", data_right$suffix, ts_highlight_seasons$suffix,
                                   ".", p$plot_type)
            }
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            
            source(paste0(homepath, "/functions/plot.nxm.r"))
            nm <- plot.nxm(time_dim, datas, dry=T) 
            
            message("plot ", plotname, " ...")
            if (p$plot_type == "png") { 
                png(plotname, 
                    width=nm$ncol*p$ts_width, 
                    height=nm$nrow*p$ts_height,
                    res=p$dpi, family=p$family_png)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=nm$ncol*p$inch, height=p$inch*((nm$nrow*p$ts_height)/(nm$ncol*p$ts_width)),
                    family=p$family_pdf)
            }
            plot.nxm(time_dim, y, dry=F, 
                     cols=cols, ltys=ltys, lwds=lwds, pchs=pchs, 
                     x_at=x_at, x_labels=x_labels, x_lab=x_lab, 
                     y_lab=y_lab,
                     ynames=names_legend,
                     verbose=T)
            dev.off()

        } # if ts_plot_each_setting_in_subplot

    } # if (ndims_unique == 1 && vardims_unique == "time") {
    # finished plot `datas` as time 
    
    
    ## plot `datas` as lat vs depth vs time
    if (ndims_unique == 3 && all(vardims_unique %in% c("lat", "depth", "time"))) { # e.g. moc

        # extract time series of lat,depth,time MOC data
        if (any(varname == c("MOCw"))) { # add further moc variables here
            if (!exists("moc_lats")) {
                stop("`moc_lats` is not defined. dont know where to extract ", varname, " time series.")
            }
            moc_max_ts <- vector("list", l=nsettings)
            names(moc_max_ts) <- names_short
            for (li in 1:length(moc_lats)) {
                message("extract \"", varname, "\" time series at latitude ", li, "/", 
                        length(moc_lats), ": ", moc_lats[li], " degrees ...")
                for (i in 1:nsettings) {
                    tmp_list <- vector("list")
                    if (any(attributes(z[[i]])$dims == "lat")
                        && any(attributes(z[[i]])$dims == "depth")
                        && any(attributes(z[[i]])$dims == "time")) {
                        latdim_ind <- which(attributes(z[[i]])$dims == "lat")
                        depthdim_ind <- which(attributes(z[[i]])$dims == "depth")
                        timedim_ind <- which(attributes(z[[i]])$dims == "time")
                        if (length(latdim_ind) == 1 && length(depthdim_ind) == 1 && length(timedim_ind) == 1) { 
                            # this setting's variable has one lat and one depth and one time dim
                            lat_ind <- which.min(abs(lat_dim[[i]] - moc_lats[li]))[1]
                            # overwrite wanted `moc_lats` with closest found lat
                            moc_lats[li] <- lat_dim[[i]][lat_ind]
                            message("   setting ", i, "/", nsettings, ": ", names_short[i], ": ", 
                                    "take ", moc_lats[li], " degree latitude as closest lat of the data.")
                            cmdrhs <- rep(",", t=length(dim(z[[i]])))
                            cmdrhs[latdim_ind] <- lat_ind
                            cmd <- paste0("tmp <- drop(z[[i]][", paste(cmdrhs, collapse=""), "])")
                            #message(cmd)
                            eval(parse(text=cmd))
                            timedim_ind <- which(attributes(z[[i]])$dims[-latdim_ind] == "time")
                            cmd <- paste0("tmp_list$moc_max <- apply(tmp, ", timedim_ind, ", max, na.rm=T)")
                            #message(cmd)
                            eval(parse(text=cmd))
                            message("      min/max(moc_max) = ", min(tmp_list$moc_max), "/", 
                                    max(tmp_list$moc_max), " Sv")
                            dim(tmp_list$moc_max) <- length(time_dim[[i]])
                            cmd <- paste0("depths_of_moc_max <- apply(tmp, ", timedim_ind, ", which.max)")
                            #message(cmd)
                            eval(parse(text=cmd))
                            tmp_list$moc_max_depths <- depth_dim[[i]][depths_of_moc_max]
                            message("      min/max(depths_of_moc_max) = ", min(tmp_list$moc_max_depths), "/", 
                                    max(tmp_list$moc_max_depths), " m")
                            moc_max_ts[[i]][[li]] <- tmp_list
                            names(moc_max_ts[[i]])[li] <- paste0(moc_lats[li], "deg")
                        
                        } else { # variable has more than 1 lat or depth or time dims. this should not happen
                            stop("found ", length(latdim_ind), "/", length(depthdim_ind), "/", length(timedim_ind), 
                                 " instead of 1 \"lat\",\"depth\",\"time\" dims of z[[", i, "]].")
                        }
                    } else {
                        # the variable has no lat dim in setting i
                    }
                } # for i nsetitngs
            } # for li moc_lats
                            
            # save moc ts as nc
            for (i in 1:nsettings) {
                outpath <- paste0(postpaths[i], "/", models[i], "/moc_ts/", fvarnames_in[i])
                dir.create(outpath, recursive=T, showWarnings=F)
                outname <- paste0(outpath, "/", 
                                  prefixes[i], "_moc_ts", 
                                  codesf[i], "_", varnames_in[i], 
                                  "_", areas[i],
                                  "_", seasonsp[i], "_", fromsp[i], "-", tosp[i], 
                                  depthsf[i], 
                                  ".nc")
                if (file.exists(outname)) {
                    message("overwrite ", outname, " ...")
                } else {
                    message("save ", outname, " ...")
                }
                time_dim_outnc <- ncdim_def(name="time", units=dims[[i]]$timeunits, vals=time_dim[[i]])
                moc_max_ts_var <- vector("list", l=length(moc_max_ts[[i]]))
                moc_max_depths_var <- moc_max_ts_var
                for (li in 1:length(moc_max_ts[[i]])) { 
                    moc_max_ts_var[[li]] <- ncvar_def(name=paste0("moc_max_", names(moc_max_ts[[i]])[li]),
                                                      units=data_infos[[i]][[varname]]$units,
                                                      dim=time_dim_outnc)
                    moc_max_depths_var[[li]] <- ncvar_def(name=paste0("moc_max_depths_", names(moc_max_ts[[i]])[li]),
                                                          units="m",
                                                          dim=time_dim_outnc)

                }
                outnc <- nc_create(filename=outname,
                                   vars=c(moc_max_ts_var, moc_max_depths_var),
                                   force_v4=T)
                for (li in 1:length(moc_max_ts[[i]])) {
                    ncvar_put(nc=outnc, varid=moc_max_ts_var[[li]], vals=moc_max_ts[[i]][[li]]$moc_max)
                    ncvar_put(nc=outnc, varid=moc_max_depths_var[[li]], vals=moc_max_ts[[i]][[li]]$moc_max_depths)
                }
                nc_close(outnc)
            } # for i in settings
            # finished save moc ts as nc

        } # if varname any MOC

    } # if (ndims_unique == 3 && all(vardims_unique %in% c("lat", "depth", "time"))) {
    # finished plot `datas` as lat vs depth vs time

} # for vi in varnames_unique
# finised dimension-specific plots for each variable of `datas`


## start dimension-specific plots for each variable of `datasmon`
if (exists("datasmon")) {
    
    message("\n", "****************** start plotting of `datasmon` ***************************")
    varnames_unique_mon <- unique(as.vector(unlist(sapply(datasmon, names))))
    
    for (vi in 1:length(varnames_unique_mon)) {

        # prepare datasmon plots
        varname <- varnames_unique_mon[vi]
        zmon <- vector("list", l=nsettings)
        names(zmon) <- names_short
        for (i in 1:nsettings) {
            varind <- which(names(datasmon[[i]]) == varname)
            if (length(varind) == 1) {
                zmon[[i]] <- datasmon[[i]][[varind]]
            }
        }
        vardims_mon <- lapply(zmon, function(x) attributes(x)$dims)
        vardims_unique_mon <- c()
        for (i in 1:nsettings) {
            vardims_unique_mon <- unique(c(vardims_unique_mon, unique(vardims_mon[[i]])))
        }
        ndims_unique_mon <- length(vardims_unique_mon)
        for (di in 1:ndims_unique_mon) {
            cmd <- paste0(vardims_unique_mon[di], "_dim <- vector(\"list\", l=", nsettings, ")")
            #message(cmd, " ...")
            eval(parse(text=cmd))
            for (i in 1:nsettings) {
                dimind <- which(attributes(zmon[[i]])$dims == vardims_unique_mon[di])
                if (length(dimind) == 1) {
                    cmd <- paste0(vardims_unique_mon[di], "_dim[[", i, "]] <- dims[[i]]$", vardims_unique_mon[di])
                    #message(cmd)
                    eval(parse(text=cmd))
                    cmd <- paste0("names(", vardims_unique_mon[di], "_dim)[", i, "] <- \"", vardims_unique_mon[di], "\"")
                    #message(cmd)
                    eval(parse(text=cmd))
                } # if dim di is a dim of variable vi of setting i
            } # for i nsetting
        } # for di in vardims_unique
        # finished plot preparation 
        message("\n", "var ", vi, "/", length(varnames_unique_mon), ": \"", varname, "\" of `datasmon` has dims \"", 
                paste(vardims_unique_mon, collapse="\",\""), "\". check if this case is defined ...")


        ## plot `datasmon` as time 
        if (ndims_unique_mon == 1 && vardims_unique_mon == "month") {

            message("\n", varname, " ", mode, " plot vs months ...")
            
            # ylims for fldmean versus months plot
            message("\n", mode, " versus months min / mean / max ", varname, " zmon:")
            for (i in 1:nsettings) {
                message(names_short[i], ": ", min(zmon[[i]], na.rm=T), " / ",
                        mean(zmon[[i]], na.rm=T), " / ", max(zmon[[i]], na.rm=T))
            }
            ylim_mon <- range(zmon, na.rm=T)
            message("\n", "ylim_mon=", appendLF=F)
            dput(ylim_mon)
            yat_mon <- pretty(ylim_mon, n=10)
            ylab_mon <- format(yat_mon, trim=T)

            # prepare right axis data if necessary
            if (!add_data_right_yaxis_ts_mon) {
                data_right_mon <- list(suffix="") # default
            } else {
                if (varname == "temp2") {
                    data_right_mon <- list(data=vector("list", l=nsettings))
                    names(data_right_mon$data) <- names_short
                    for (i in 1:nsettings) {
                        inpath <- paste0(workpath, "/post/", models[i], "/", mode, "/wisoaprt_d") 
                        fname <- paste0(prefixes[i], "_", mode, 
                                        codesf[i], "_wisoaprt_d_sellevel_2_", areas[i],
                                        "_ymonmean_", fromsf[i], "-", tosf[i], 
                                        depthsf[i], 
                                        ".nc") # todo: levs 
                        ncin <- nc_open(paste0(inpath, "/", fname))
                        message("read ", inpath, "/", fname, " ...")
                        data_right_mon$data[[i]] <- list(x=dims[[i]]$month,
                                                         y=ncvar_get(ncin, "wisoaprt_d"),
                                                         text="wisoaprt_d_sellevel_2", 
                                                         #col=cols[i], 
                                                         cols="#E41A1C",
                                                         lty=1, lwd=1, pch=NA)
                    }
                    data_right_mon$label <- eval(substitute(expression(paste(delta, ""^18, "O [\u2030]")))) 
                    data_right_mon$suffix <- "_with_wisoaprt_d_sellevel_2"
                } # temp2
            
                # check
                if (length(data_right_mon$data) == 0) {
                    warning("you provided `add_data_right_yaxis_ts_mon=T` but did not ",
                            "define which data should be plotted on right yaxis.\n",
                            " --> set `add_data_right_yaxis_ts_mon=F` and continue ...")
                    add_data_right_yaxis_ts_mon <- F
                    data_right_mon <- list(suffix="") # default
                }

            } # if add_data_right_yaxis_ts_mon before check

            # if add_data_right_yaxis_ts after check
            if (add_data_right_yaxis_ts_mon) {
                
                nsettings_right_mon <- length(data_right_mon$data)

                if (!exists("ylim_right_mon")) { # possibly set by user
                    ylim_right_mon <- vector("list", l=length(data_right$data))
                    for (i in seq_len(nsettings_right_mon)) {
                        ylim_right_mon[[i]] <- range(data_right_mon$data[[i]]$y, na.rm=T)
                    }
                    ylim_right_mon <- range(ylim_right_mon)
                } # if ylim_right_mon does not already exist
                
                message("\n", "ylim_right_mon=", appendLF=F)
                dput(ylim_right_mon)
                if (!exists("yat_right_mon")) {
                    message("use automatic data right yaxis labels ...")
                    yat_right_mon <- pretty(ylim_right_mon, n=10)
                }
                ylab_right_mon <- format(yat_right_mon, trim=T)
            } # if add_data_right_yaxis_ts_mon finished prepare right axis data

            # plotname
            plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                               varname, "_",
                               paste0(names_short, "_", seasonsp, 
                                      "_", froms_plot, "_to_", tos_plot, "_", areas, collapse="_vs_"), 
                               "_months",
                               data_right_mon$suffix,
                               ".", p$plot_type)
            if (nchar(plotname) > nchar_max_foutname) {
                plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                                   varname, "_",
                                   paste0(names_short, "_", areas, collapse="_vs_"), 
                                   "_months",
                                   data_right$suffix, 
                                   ".", p$plot_type)
            }
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            if (p$plot_type == "png") {
                png(plotname, width=p$ts_width_m, height=p$ts_height_m,
                    res=p$dpi, family=p$family_png)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=p$inch, height=p$inch*p$ts_height_m/p$ts_width_m,
                    family=p$family_pdf)
            }

            # set plot margins
            mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
            mar[4] <- 1 # decrease right margin
            if (!add_title) mar[3] <- 1 # decrease upper margin
            if (tlabsrt == 0) mar[1] <- mar[1]/2  # decrease lower margin
            if (add_data_right_yaxis_ts_mon) mar[4] <- mar[2] # same as left  

            # open plot
            par(mar=mar)
            plot(month_dim[[1]], zmon[[1]], t="n",
                 xlim=monlim, ylim=ylim_mon, 
                 xaxt="n", yaxt="n",
                 xlab=NA, ylab=NA)
            axis(1, at=monat, labels=monlab, cex.axis=tlabcex)
            axis(2, at=yat_mon, labels=ylab_mon, las=2)

            # add title
            if (add_title) {
                title <- paste0(paste(unique(areas), collapse=","), 
                                " ", mode, " ", varname, " ", 
                                paste(unique(seasonsp), collapse=","), " ", 
                                paste(unique(fromsp), collapse=","), " to ", 
                                paste(unique(tosp), collapse=","))
                title(title, cex.main=0.75)
            }

            # add variable label
            first_setting_with_varname <- sapply(lapply(data_infos, names), function(x) which(x == varname))[1]
            data_info <- data_infos[[names(first_setting_with_varname)]][[first_setting_with_varname]]
            mtext(side=2, data_info$label, line=3.4, cex=0.9)

            # add grid
            if (add_xgrid) {
                message("\n", "add xgrid ...")
                abline(v=tatn, col="gray", lwd=0.5)
            }
            if (add_ygrid) {
                message("\n", "add ygrid ...")
                abline(h=yat, col="gray", lwd=0.5)
            }

            # add zero line
            if (add_zeroline) {
                abline(h=0, col="gray", lwd=0.5)
            }

            ## add data
            for (i in 1:nsettings) {
                lines(month_dim[[i]], zmon[[i]], 
                      col=cols[i], lty=ltys[i], lwd=lwds[i], pch=pchs[i])
            }

            # add legend if wanted
            if (add_legend) {
                message("\n", "add default stuff to ", mode, " mon legend ...")
                le <- list()
                le$pos <- "topleft" 
                #le$pos <- "top"
                #le$pos <- "bottom"
                #le$pos <- "bottomleft" 
                #le$pos <- "bottomright" 
                #le$ncol <- nsettings/2
                le$ncol <- 1
                #le$ncol <- 2 
                le$text <- names_legend
                le$col <- cols
                le$lty <- ltys
                le$lwds <- lwds
                le$pchs <- pchs
                le$cex <- 1
                le$cex <- 0.85
                # add stuf to legend here
                if (F) {
                    message("\n", "add non default stuff to ", mode, " mon legend ...")

                }
                # reorder reading direction from R's default top->bottom to left->right
                if (T) {
                    le <- reorder_legend(le)
                }
                if (length(le$pos) == 1) {
                    legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                           pch=le$pch, col=le$col, ncol=le$ncol,
                           x.intersp=0.2, cex=le$cex, bty="n")
                } else if (length(le$pos) == 2) {
                    legend(x=le$pos[1], y=le$pos[2],
                           legend=le$text, lty=le$lty, lwd=le$lwd,
                           pch=le$pch, col=le$col, ncol=le$ncol,
                           x.intersp=0.2, cex=le$cex, bty="n")
                }
            } # if add_legend

            if (add_data_right_yaxis_ts_mon) {
                message("\n", "`add_data_right_yaxis_ts_mon` = T --> add data right yaxis mon ...")
                par(new=T)
                plot(data_right_mon$data[[1]]$x, data_right_mon$data[[1]]$y, #log="y", 
                     t="n", xlim=monlim, ylim=ylim_right_mon, 
                     xlab=NA, ylab=NA, axes=F)
                
                # add right axes in same color as the right data if 
                # all colors of the right data are the same
                if (length(unique(sapply(data_right$data, "[", "col"))) == 1) {
                    right_axis_col <- data_right$data[[1]]$col
                } else {
                    right_axis_col <- "black" # default
                }
                axis(4, at=yat_right_mon, labels=ylab_right_mon, las=2, 
                     col=right_axis_col, col.axis=right_axis_col, col.ticks=right_axis_col)
                mtext(side=4, data_right_mon$label, line=4.5, cex=0.9, col=right_axis_col)
                
                # add right data
                for (i in seq_len(nsettings_right_mon)) {
                    if (length(data_right_mon$data[[i]]$x) == 1 && data_right_mon$data[[i]]$x == "const") {
                        abline(h=data_right_mon$data[[i]]$y, 
                               col=data_right_mon$data[[i]]$col, lty=data_right_mon$data[[i]]$lty,
                               lwd=data_right_mon$data[[i]]$lwd)
                    } else {
                        lines(data_right_mon$data[[i]]$x, data_right_mon$data[[i]]$y, 
                              col=data_right_mon$data[[i]]$col, lty=data_right_mon$data[[i]]$lty,
                              lwd=data_right_mon$data[[i]]$lwd)
                    }
                }

                if (add_legend_right_yaxis) {
                    message("\n", "add default stuff to ", mode, " right_data mon legend ...")
                    le <- list()
                    le$pos <- "top" 
                    le$ncol <- 1
                    le$text <- names_legend
                    le$col <- cols
                    le$lty <- ltys
                    le$lwds <- lwds
                    le$pchs <- pchs
                    le$cex <- 1
                    le$cex <- 0.85
                    # add stuf to legend here
                    if (T && varname == "temp2") {
                        message("\n", "add non default stuff to ", mode, " legend ...")
                        if (varname == "temp2") {
                            le$text <- c(le$text, hadcrut4_sat_anom_annual$text)
                            le$col <- c(le$col, hadcrut4_sat_anom_annual$col)
                            le$lty <- c(le$lty, hadcrut4_sat_anom_annual$lty)
                            le$lwd <- c(le$lwd, hadcrut4_sat_anom_annual$lwd)
                            le$pch <- c(le$pch, hadcrut4_sat_anom_annual$pch)
                        }
                    }
                    # reorder reading direction from R's default top->bottom to left->right
                    le <- reorder_legend(le)
                    if (length(le$pos) == 1) {
                        legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                               pch=le$pch, col=le$col, ncol=le$ncol,
                               x.intersp=0.2, cex=le$cex, bty="n")
                    } else if (length(le$pos) == 2) {
                        legend(x=le$pos[1], y=le$pos[2],
                               legend=le$text, lty=le$lty, lwd=le$lwd,
                               pch=le$pch, col=le$col, ncol=le$ncol,
                               x.intersp=0.2, cex=le$cex, bty="n")
                    }
                } # if add_legend

            } # if add_data_right_yaxis_ts_mon

            box()
            message("\n", "save plot ", plotname, " ...")
            dev.off()
            if (p$plot_type == "pdf") {
                if("extrafont" %in% (.packages())){
                    extrafont::embed_fonts(plotname, outfile=plotname)
                } else {
                    grDevices::embedFonts(plotname, outfile=plotname)
                }
            }
        
        } # if (ndims_unique_mon == 1 && vardims_unique_mon == "month") {
        # finished plot `datasmon` vs months

    } # for vi in varnames_unique_mon
} # if exists("datasmon")
# finised dimension-specific plots for each variable of `datasmon`


## start dimension-specific plots for each variable of `datasan`
if (exists("datasan")) {
    
    message("\n", "****************** start plotting of `datasan` ***************************")
    varnames_unique_an <- unique(as.vector(unlist(sapply(datasan, names))))
    
    for (vi in 1:length(varnames_unique_an)) {

        # prepare datasan plots
        varname <- varnames_unique_an[vi]
        zan <- vector("list", l=nsettings)
        names(zan) <- names_short
        for (i in 1:nsettings) {
            varind <- which(names(datasan[[i]]) == varname)
            if (length(varind) == 1) {
                zan[[i]] <- datasan[[i]][[varind]]
            }
        }
        vardims_an <- lapply(zan, function(x) attributes(x)$dims)
        vardims_unique_an <- c()
        for (i in 1:nsettings) {
            vardims_unique_an <- unique(c(vardims_unique_an, unique(vardims_an[[i]])))
        }
        ndims_unique_an <- length(vardims_unique_an)
        for (di in 1:ndims_unique_an) {
            cmd <- paste0(vardims_unique_an[di], "_dim <- vector(\"list\", l=", nsettings, ")")
            #message(cmd, " ...")
            eval(parse(text=cmd))
            for (i in 1:nsettings) {
                dimind <- which(attributes(zan[[i]])$dims == vardims_unique_an[di])
                if (length(dimind) == 1) {
                    cmd <- paste0(vardims_unique_an[di], "_dim[[", i, "]] <- dims[[i]]$", vardims_unique_an[di])
                    #message(cmd)
                    eval(parse(text=cmd))
                    cmd <- paste0("names(", vardims_unique_an[di], "_dim)[", i, "] <- \"", vardims_unique_an[di], "\"")
                    #message(cmd)
                    eval(parse(text=cmd))
                } # if dim di is a dim of variable vi of setting i
            } # for i nsetting
        } # for di in vardims_unique
        # finished plot preparation 
        message("\n", "var ", vi, "/", length(varnames_unique_an), ": \"", varname, "\" of `datasan` has dims \"", 
                paste(vardims_unique_an, collapse="\",\""), "\". check if this case is defined ...")


        ## plot `datasan` as time 
        if (ndims_unique_an == 1 && vardims_unique_an == "year") {

            message("\n", varname, " ", mode, " plot vs years ...")
            
            # ylims for fldmean versus years plot
            message("\n", mode, " versus years min / mean / max ", varname, " zan:")
            for (i in 1:nsettings) {
                message(names_short[i], ": ", min(zan[[i]], na.rm=T), " / ",
                        mean(zan[[i]], na.rm=T), " / ", max(zan[[i]], na.rm=T))
            }
            ylim_an <- range(zan, na.rm=T)
            message("\n", "ylim_an=", appendLF=F)
            dput(ylim_an)
            yat_an <- pretty(ylim_an, n=10)
            ylab_an <- format(yat_an, trim=T)

            # prepare right axis data if necessary
            if (!add_data_right_yaxis_ts_an) {
                data_right_an <- list(suffix="") # default
            } else {
                message("\n`add_data_right_yaxis_ts_an`=T ...")
                data_right_an <- list(data=vector("list", l=nsettings))
                names(data_right_an$data) <- names_short
                if (F && varname == "temp2") {
                    for (i in 1:nsettings) {
                        inpath <- paste0(workpath, "/post/", models[i], "/", mode, "/wisoaprt_d") 
                        fname <- paste0(prefixes[i], "_", mode, 
                                        codesf[i], "_wisoaprt_d_sellevel_2_", areas[i],
                                        "_annual_", fromsf[i], "-", tosf[i], 
                                        depthsf[i], 
                                        ".nc") # todo: levs 
                        ncin <- nc_open(paste0(inpath, "/", fname))
                        message("read ", inpath, "/", fname, " ...")
                        data_right_an$data[[i]] <- list(x=dims[[i]]$year,
                                                         y=ncvar_get(ncin, "wisoaprt_d"),
                                                         text="wisoaprt_d_sellevel_2", 
                                                         #col=cols[i], 
                                                         cols="#E41A1C",
                                                         lty=1, lwd=1, pch=NA)
                    }
                    data_right_an$label <- eval(substitute(expression(paste(delta, ""^18, "O [\u2030]")))) 
                    data_right_an$suffix <- "_with_wisoaprt_d_sellevel_2"
                } # temp2
            
                # check
                if (all(sapply(data_right_an$data, is.null))) {
                    message("\nyou provided `add_data_right_yaxis_ts_an=T` but did not ",
                            "define which data should be plotted on right yaxis.\n",
                            " --> set `add_data_right_yaxis_ts_an=F` and continue ...")
                    add_data_right_yaxis_ts_an <- F
                    data_right_an <- list(suffix="") # default
                }

            } # if add_data_right_yaxis_ts_an before check

            # if add_data_right_yaxis_ts after check
            if (add_data_right_yaxis_ts_an) {
                
                nsettings_right_an <- length(data_right_an$data)
            
                if (add_smoothed) {
                    for (i in 1:nsettings_right_an) {
                        data_right_an$data[[i]]$yma <- filter(data_right_an$data[[i]]$y, rep(1/(n_mas[i]/12), t=n_mas[i]/12))
                    }
                }

                if (!exists("ylim_right_an")) { # possibly set by user
                    message("use automatic data right yaxis limits ...")
                    ylim_right_an <- vector("list", l=length(data_right_an$data))
                    ylim_right_an_ma <- ylim_right_an
                    for (i in 1:nsettings_right_an) {
                        if (length(data_right_an$data[[i]]$x) == 1 && data_right_an$data[[i]]$x == "const") {
                            ylim_right_an[[i]] <- range(data_right_an$data[[i]]$y, na.rm=T)
                        } else {
                            timeinds <- which(data_right_an$data[[i]]$x >= anlim[1] & data_right_an$data[[i]]$x <= anlim[2])
                            if (length(timeinds) == 0) {
                                message("all data of data_right_an$data[[", i, "]]: ", names(data_right_an$data)[1], " are out of tlimlt")
                                ylim_right_an[[i]] <- NA
                            } else {
                                ylim_right_an[[i]] <- range(data_right_an$data[[i]]$y[timeinds], na.rm=T)
                                if (add_smoothed) {
                                    ylim_right_an_ma[[i]] <- range(data_right_an$data[[i]]$yma[timeinds], na.rm=T)
                                }
                            }
                        }
                    } # i in data_right
                    if ((add_unsmoothed && add_smoothed) ||
                        (add_unsmoothed && !add_smoothed)) {
                        ylim_right_an <- range(ylim_right_an, na.rm=T)
                    } else if (!add_unsmoothed && add_smoothed) {
                        ylim_right_an <- range(ylim_right_an_ma, na.rm=T)
                    }
                } # if ylim_right does not already exist
                
                message("ylim_right_an=", appendLF=F)
                dput(ylim_right_an)
                if (!exists("yat_right_an")) {
                    message("use automatic data right yaxis labels ...")
                    yat_right_an <- pretty(ylim_right_an, n=10)
                }
                ylab_right_an <- format(yat_right_an, trim=T)
            } # if add_data_right_yaxis_ts_an finished prepare right axis data

            # plotname
            plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                               varname, "_",
                               paste0(names_short, "_", seasonsp, 
                                      "_", froms_plot, "_to_", tos_plot, "_", areas, collapse="_vs_"), 
                               "_annual",
                               data_right_an$suffix,
                               ".", p$plot_type)
            if (nchar(plotname) > nchar_max_foutname) {
                plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                                   varname, "_",
                                   paste0(names_short, "_", areas, collapse="_vs_"), 
                                   "_annual",
                                   data_right_an$suffix,
                                   ".", p$plot_type)
            }
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            if (p$plot_type == "png") {
                png(plotname, width=p$ts_width, height=p$ts_height,
                    res=p$dpi, family=p$family_png)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=p$inch, height=p$inch*p$ts_height/p$ts_width,
                    family=p$family_pdf)
            }

            # set plot margins
            mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
            mar[4] <- 1 # decrease right margin
            if (!add_title) mar[3] <- 1 # decrease upper margin
            if (tlabsrt == 0) mar[1] <- mar[1]/2  # decrease lower margin
            if (add_data_right_yaxis_ts_an) mar[4] <- mar[2] # same as left  

            # open plot
            par(mar=mar)
            plot(year_dim[[1]], zan[[1]], t="n",
                 xlim=anlim, ylim=ylim_an, 
                 xaxt="n", yaxt="n",
                 xlab=NA, ylab=NA)
            axis(1, at=anat, labels=anlab, cex.axis=tlabcex)
            axis(2, at=yat_an, labels=ylab_an, las=2)

            # add title
            if (add_title) {
                title <- paste0(paste(unique(areas), collapse=","), 
                                " ", mode, " ", varname, " ", 
                                paste(unique(seasonsp), collapse=","), " ", 
                                paste(unique(fromsp), collapse=","), " to ", 
                                paste(unique(tosp), collapse=","))
                title(title, cex.main=0.75)
            }

            # add variable label
            first_setting_with_varname <- sapply(lapply(data_infos, names), function(x) which(x == varname))[1]
            data_info <- data_infos[[names(first_setting_with_varname)]][[first_setting_with_varname]]
            mtext(side=2, data_info$label, line=3.4, cex=0.9)

            # add grid
            if (add_xgrid) {
                message("\n", "add xgrid ...")
                abline(v=tatn, col="gray", lwd=0.5)
            }
            if (add_ygrid) {
                message("\n", "add ygrid ...")
                abline(h=yat, col="gray", lwd=0.5)
            }

            # add zero line
            if (add_zeroline) {
                abline(h=0, col="gray", lwd=0.5)
            }

            ## add data
            for (i in 1:nsettings) {
                lines(year_dim[[i]], zan[[i]], 
                      col=cols[i], lty=ltys[i], lwd=lwds[i], pch=pchs[i])
            }

            # add legend if wanted
            if (add_legend) {
                message("\n", "add default stuff to ", mode, " an legend ...")
                le <- list()
                le$pos <- "topleft" 
                #le$pos <- "top"
                #le$pos <- "bottom"
                #le$pos <- "bottomleft" 
                #le$pos <- "bottomright" 
                #le$ncol <- nsettings/2
                le$ncol <- 1
                #le$ncol <- 2 
                le$text <- names_legend
                le$col <- cols
                le$lty <- ltys
                le$lwds <- lwds
                le$pchs <- pchs
                le$cex <- 1
                le$cex <- 0.85
                # add stuf to legend here
                if (F) {
                    message("\n", "add non default stuff to ", mode, " an legend ...")

                }
                # reorder reading direction from R's default top->bottom to left->right
                if (T) {
                    le <- reorder_legend(le)
                }
                if (length(le$pos) == 1) {
                    legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                           pch=le$pch, col=le$col, ncol=le$ncol,
                           x.intersp=0.2, cex=le$cex, bty="n")
                } else if (length(le$pos) == 2) {
                    legend(x=le$pos[1], y=le$pos[2],
                           legend=le$text, lty=le$lty, lwd=le$lwd,
                           pch=le$pch, col=le$col, ncol=le$ncol,
                           x.intersp=0.2, cex=le$cex, bty="n")
                }
            } # if add_legend

            if (add_data_right_yaxis_ts_an) {
                message("\n", "`add_data_right_yaxis_ts_an` = T --> add data right yaxis an ...")
                par(new=T)
                plot(data_right_an$data[[1]]$x, data_right_an$data[[1]]$y, #log="y", 
                     t="n", xlim=anlim, ylim=ylim_right_an, 
                     xlab=NA, ylab=NA, axes=F)
                
                # add right axes in same color as the right data if 
                # all colors of the right data are the same
                if (length(unique(sapply(data_right$data, "[", "col"))) == 1) {
                    right_axis_col <- data_right$data[[1]]$col
                } else {
                    right_axis_col <- "black" # default
                }
                axis(4, at=yat_right_an, labels=ylab_right_an, las=2, 
                     col=right_axis_col, col.axis=right_axis_col, col.ticks=right_axis_col)
                mtext(side=4, data_right_an$label, line=4.5, cex=0.9, col=right_axis_col)
                
                # add unsmoothed right data an before smoothed
                if (add_unsmoothed) {
                    for (i in seq_len(nsettings_right_an)) {
                        message(i, "/", length(data_right_an$data), ": ", names(data_right_an$data)[i], " unsmoothed ...")
                        if (length(data_right_an$data[[i]]$x) == 1 && data_right_an$data[[i]]$x == "const") {
                            abline(h=data_right_an$data[[i]]$y, 
                                   col=data_right_an$data[[i]]$col, lty=data_right_an$data[[i]]$lty,
                                   lwd=data_right_an$data[[i]]$lwd)
                        } else {
                            lines(data_right_an$data[[i]]$x, data_right_an$data[[i]]$yma, 
                                  col=data_right_an$data[[i]]$col, lty=data_right_an$data[[i]]$lty,
                                  lwd=data_right_an$data[[i]]$lwd)
                        }
                    }
                }

                # add smoothed right data an after unsmoothed
                if (add_smoothed) {
                    for (i in seq_len(nsettings_right_an)) {
                        if (length(data_right_an$data[[i]]$x) == 1 && data_right_an$data[[i]]$x == "const") {
                            abline(h=data_right_an$data[[i]]$y, 
                                   col=data_right_an$data[[i]]$col, lty=data_right_an$data[[i]]$lty,
                                   lwd=data_right_an$data[[i]]$lwd)
                        } else {
                            lines(data_right_an$data[[i]]$x, data_right_an$data[[i]]$yma, 
                                  col=data_right_an$data[[i]]$col, lty=data_right_an$data[[i]]$lty,
                                  lwd=data_right_an$data[[i]]$lwd)
                        }
                    }
                }

                if (add_legend_right_yaxis) {
                    message("\n", "add default stuff to ", mode, " right_data an legend ...")
                    le <- list()
                    le$pos <- "top" 
                    le$ncol <- 1
                    le$text <- names_legend
                    le$col <- cols
                    le$lty <- ltys
                    le$lwds <- lwds
                    le$pchs <- pchs
                    le$cex <- 1
                    le$cex <- 0.85
                    # add stuf to legend here
                    if (T && varname == "temp2") {
                        message("\n", "add non default stuff to ", mode, " legend ...")
                        if (varname == "temp2") {
                            le$text <- c(le$text, hadcrut4_sat_anom_annual$text)
                            le$col <- c(le$col, hadcrut4_sat_anom_annual$col)
                            le$lty <- c(le$lty, hadcrut4_sat_anom_annual$lty)
                            le$lwd <- c(le$lwd, hadcrut4_sat_anom_annual$lwd)
                            le$pch <- c(le$pch, hadcrut4_sat_anom_annual$pch)
                        }
                    }
                    # reorder reading direction from R's default top->bottom to left->right
                    le <- reorder_legend(le)
                    if (length(le$pos) == 1) {
                        legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                               pch=le$pch, col=le$col, ncol=le$ncol,
                               x.intersp=0.2, cex=le$cex, bty="n")
                    } else if (length(le$pos) == 2) {
                        legend(x=le$pos[1], y=le$pos[2],
                               legend=le$text, lty=le$lty, lwd=le$lwd,
                               pch=le$pch, col=le$col, ncol=le$ncol,
                               x.intersp=0.2, cex=le$cex, bty="n")
                    }
                } # if add_legend

                if (add_cor_data_left_and_right_ts) {

                    cor <- cor.test(datasan[[i]][[1]], data_right_an$data[[i]]$y)
                    # plusminus: %+-%
                    subtitle <- substitute(paste("cor(", x, ",", y, ") = ", rfrom-rto),
                                           list(x=names(datasan[[i]])[1], y=data_right_an$data[[i]]$text,
                                                rfrom=round(cor$conf.int[1], 3), rto=round(cor$conf.int[2], 3)))
                    mtext(subtitle, cex=0.7)

                } # if add_cor_data_left_and_right_ts
            
            } # if add_data_right_yaxis_ts_an

            box()
            message("\n", "save plot ", plotname, " ...")
            dev.off()
            if (p$plot_type == "pdf") {
                if("extrafont" %in% (.packages())){
                    extrafont::embed_fonts(plotname, outfile=plotname)
                } else {
                    grDevices::embedFonts(plotname, outfile=plotname)
                }
            }
        
        } # if (ndims_unique_an == 1 && vardims_unique_an == "year") {
        # finished plot `datasan` vs years

    } # for vi in varnames_unique_an
} # if exists("datasan")
# finised dimension-specific plots for each variable of `datasan`


## start dimension-specific plots for each variable of `datasltm`
if (exists("datasltm")) {
    
    message("\n", "****************** start plotting of `datasltm` ***************************")
    varnames_unique_ltm <- unique(as.vector(unlist(sapply(datasltm, names))))
    
    for (vi in 1:length(varnames_unique_ltm)) {

        # prepare datasltm plots
        varname <- varnames_unique_ltm[vi]
        zltm <- vector("list", l=nsettings)
        names(zltm) <- names_short
        for (i in 1:nsettings) {
            varind <- which(names(datasltm[[i]]) == varname)
            if (length(varind) == 1) {
                zltm[[i]] <- datasltm[[i]][[varind]]
            }
        }
        vardims_ltm <- lapply(zltm, function(x) attributes(x)$dims)
        vardims_unique_ltm <- c()
        for (i in 1:nsettings) {
            vardims_unique_ltm <- unique(c(vardims_unique_ltm, unique(vardims_ltm[[i]])))
        }
        ndims_unique_ltm <- length(vardims_unique_ltm)

        if (ndims_unique_ltm == 0) {
            message("\n", "var ", vi, "/", length(varnames_unique_ltm), ": \"", varname, 
                    "\" of `datasltm` has no dims. skip to next variable ...")

        } else if (ndims_unique_ltm > 0) {
            for (di in 1:ndims_unique_ltm) {
                cmd <- paste0(vardims_unique_ltm[di], "_dim <- vector(\"list\", l=", nsettings, ")")
                #message(cmd, " ...")
                eval(parse(text=cmd))
                for (i in 1:nsettings) {
                    dimind <- which(attributes(zltm[[i]])$dims == vardims_unique_ltm[di])
                    if (length(dimind) == 1) {
                        cmd <- paste0(vardims_unique_ltm[di], "_dim[[", i, "]] <- dims[[i]]$", vardims_unique_ltm[di])
                        #message(cmd)
                        eval(parse(text=cmd))
                        cmd <- paste0("names(", vardims_unique_ltm[di], "_dim)[", i, "] <- \"", vardims_unique_ltm[di], "\"")
                        #message(cmd)
                        eval(parse(text=cmd))
                    } # if dim di is a dim of variable vi of setting i
                } # for i nsetting
            } # for di in vardims_unique_ltm
            # finished plot preparation 
            message("\n", "var ", vi, "/", length(varnames_unique_ltm), ": \"", varname, "\" of `datasltm` has dims \"", 
                    paste(vardims_unique_ltm, collapse="\",\""), "\". check if this case is defined ...")
        
        } # if ndims_unique == 0 or not -> `datasltm` are just a number without dims

        if (ndims_unique_ltm == 0) next # variable of `datasltm`

        ## plot `datasltm` as lat vs depth (e.g. moc)
        if (any("lat" == vardims_unique_ltm) && any("depth" == vardims_unique_ltm)) {

            message("\n", varname, " ", mode, " ltm plot lat vs depth ...")
           
            # use km instead of m as depth unit
            if (T) {
                message("divide depth dimension by 1000 m --> km")
                depth_dim <- lapply(depth_dim, "/", 1000)
                ylab <- "Depth [km]"
            } else {
                ylab <- "Depth [m]"
            }

            # add moc topo 
            if (any(varname == c("MOCw"))) { # add further moc variables here
                if (any(varnames_unique == "moc_topo")) {
                    moc_topo <- vector("list", l=nsettings)
                    for (i in 1:nsettings) {
                        tmp <- vector("list")
                        moc_topo_ind <- which(names(datas[[i]]) == "moc_topo")
                        if (length(moc_topo_ind) == 1) {
                            tmp$data <- datas[[i]][[moc_topo_ind]]
                            tmp$levels <- unique(range(tmp$data, na.rm=T))
                            tmp$cols <- "gray"
                        }
                        moc_topo[[i]] <- tmp
                    }
                } # if varname "moc_topo" is present
            } # add moc topp if varname is any moc

            # colorbar values
            source(paste0(homepath, "/functions/image.plot.pre.r"))
            ip <- image.plot.pre(range(zltm, na.rm=T), verbose=F)

            # determine number of rows and columns
            source(paste0(homepath, "/functions/image.plot.nxm.r"))
            nm <- image.plot.nxm(x=lat_dim, y=depth_dim, z=zltm, ip=ip, dry=T)

            plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                               varname, "_", 
                               paste0(names_short, "_", seasonsp, 
                                      "_", froms_plot, "_to_", tos_plot, "_", areas, collapse="_vs_"), 
                               ".", p$plot_type)
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            message("plot ", plotname, " ...")
            if (p$plot_type == "png") {
                png(plotname, width=nm$ncol*p$map_width, height=nm$nrow*p$map_height,
                    res=p$dpi, family=p$family_png)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=nm$ncol*p$inch, height=p$inch*((nm$nrow*p$map_height)/(nm$ncol*p$map_width)),
                    family=p$family_pdf)
            }

            # plot
            data_info <- data_infos[[which(sapply(data_infos, names) == varname)[1]]][[varname]]
            image.plot.nxm(x=lat_dim, y=depth_dim, z=zltm, ip=ip, verbose=F,
                           xlab="Latitude [°]", ylab=ylab,
                           zlab=data_info$label, znames=names_legend,
                           image_list=moc_topo)
        
            message("\n", "save plot ", plotname, " ...")
            dev.off()
            if (p$plot_type == "pdf") {
                if("extrafont" %in% (.packages())){
                    extrafont::embed_fonts(plotname, outfile=plotname)
                } else {
                    grDevices::embedFonts(plotname, outfile=plotname)
                }
            }

        } # for vi in unique_varnames_ltm

    } # for vi in varnames_unique_ltm

} # if (exists("datasltm")) {
# finised dimension-specific plots for each variable of `datasltm`

## plot var setting1 vs var setting2 of `datas`
if (plot_scatter_setting1_vs_setting2) {

    message("\n", "****************** `plot_scatter_setting1_vs_setting2`=T --> scatterplot var setting1 vs setting2 ***************************")
    
    if (nsettings == 2 && ndims_unique == 1 && vardims_unique == "time") {

        # set here
        varname <- "temp2"
        if (exists(paste0(varname, "_datas"))) {
       
            eval(parse(text=paste0("scatter_set1_vs_set2 <- ", varname, "_datas")))
            eval(parse(text=paste0("var_infos <- ", varname, "_infos")))
            
            if (all(names(scatter_set1_vs_set2) == c("Hol-Tx10", "Hol-T"))) {
                # only plot same time points of accelerated and non-accelerated runs
                inds1 <- which(dims[[1]]$time %in% dims[[2]]$time)
                inds2 <- which(dims[[2]]$time %in% dims[[1]]$time)
                if (length(inds1) != length(inds2)) {
                    stop("sth wrong")
                }
                scatter_set1_vs_set2[[1]] <- scatter_set1_vs_set2[[1]][inds1]
                scatter_set1_vs_set2[[2]] <- scatter_set1_vs_set2[[2]][inds2]
            } # if "Hol-Tx10", "Hol-T"

            if (all(sapply(dims, "[[", "time_frequency") == "monthly")) {
               
                message("special: plot_scatter_setting1_vs_setting2 highlighted by seasons")

                # color data by time or seasons
                if (F) { # by time
                    message("color by time ...")
                    timecols <- colorRampPalette(c("blue", "red"))(length(scatter_set1_vs_set2[[1]]))
                    scatter_suffix <- "_bytime"
                } else if (T) { # by season
                    message("color by season ...")
                    scatterpchs_vstime <- 1:4
                    season_cols <- c(DJF="blue", MAM="darkgreen", JJA="red", SON="brown")
                    timecols <- rep(NA, t=length(scatter_set1_vs_set2[[1]]))
                    season_pchs <- timecols
                    djf_inds <- which(!is.na(match(unclass(dims[[1]]$timelt[inds1])$mon+1, c(1, 2, 12))))
                    mam_inds <- which(!is.na(match(unclass(dims[[1]]$timelt[inds1])$mon+1, c(3, 4, 5))))
                    jja_inds <- which(!is.na(match(unclass(dims[[1]]$timelt[inds1])$mon+1, c(6, 7, 8))))
                    son_inds <- which(!is.na(match(unclass(dims[[1]]$timelt[inds1])$mon+1, c(9, 10, 11))))
                    timecols[djf_inds] <- season_cols["DJF"]
                    timecols[mam_inds] <- season_cols["MAM"]
                    timecols[jja_inds] <- season_cols["JJA"]
                    timecols[son_inds] <- season_cols["SON"]
                    season_pchs[djf_inds] <- scatterpchs_vstime[1]
                    season_pchs[mam_inds] <- scatterpchs_vstime[2]
                    season_pchs[jja_inds] <- scatterpchs_vstime[3]
                    season_pchs[son_inds] <- scatterpchs_vstime[4]
                    scatter_suffix <- "_byseason"
                }
                timecols_rgb <- rgb(t(col2rgb(timecols)/255), alpha=alpha)
                
                xlim <- range(scatter_set1_vs_set2[[1]], na.rm=T)
                ylim <- range(scatter_set1_vs_set2[[2]], na.rm=T)
                xat <- pretty(xlim, n=10)
                xlab <- format(xat, trim=T)
                yat <- pretty(ylim, n=10)
                ylab <- format(yat, trim=T)

                plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                                   varname, "_", 
                                   paste0(names_short[i], "_", seasonsp[i], 
                                          "_", froms_plot[i], "_to_", tos_plot[i], "_", 
                                          areas[i], collapse="_vs_"), 
                                   scatter_suffix,
                                   ".", p$plot_type)
                dir.create(dirname(plotname), recursive=T, showWarnings=F)
                if (p$plot_type == "png") {
                    png(plotname, width=p$scatter_width, height=p$scatter_height,
                        res=p$dpi, family=p$family_png)
                } else if (p$plot_type == "pdf") {
                    pdf(plotname, width=p$inch, height=p$inch,
                        family=p$family_pdf)
                }

                # set plot margins
                mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                mar[4] <- 1 # decrease right margin
                if (!add_title) mar[3] <- 1 # decrease upper margin
                if (add_data_right_yaxis_ts) mar[4] <- mar[2] # same as left  

                # open plot
                par(mar=mar)
                plot(scatter_set1_vs_set2[[1]], scatter_set1_vs_set2[[2]], t="n",
                     xlab=NA, ylab=NA, 
                     xlim=xlim, ylim=ylim, xaxt="n", yaxt="n")
                axis(1, at=xat, labels=xlab)
                axis(2, at=yat, labels=ylab, las=1)

                # add title
                if (add_title) {
                    title <- paste0(names_short[i], " ", 
                                    paste(unique(areas[i]), collapse=","), 
                                    " ", mode, " ", varname, " ", 
                                    paste(unique(seasonsp[i]), collapse=","), " ", 
                                    paste(unique(fromsp[i]), collapse=","), " to ", 
                                    paste(unique(tosp[i]), collapse=","))
                    title(title, cex.main=0.5)
                }

                # add variable label
                mtext(side=1, var_infos[[1]]$label, line=3.4, cex=0.9)
                mtext(side=2, var_infos[[2]]$label, line=3.4, cex=0.9)

                # add zero lines
                if (add_zeroline) {
                    abline(h=0, col="gray", lwd=0.5)
                    abline(v=0, col="gray", lwd=0.5)
                }

                # add 1:1 line
                if (add_1to1_line_scatter) {
                    message("add 1:1 line ...")
                    abline(a=0, b=1, col="gray") # a=intercept, b=slope
                }
                
                # add data to scatter plot colored by time
                message("add data")
                points(scatter_set1_vs_set2[[1]], scatter_set1_vs_set2[[2]], 
                       col=timecols,
                       #col=timecols_rgb,
                       #pch=scatterpchs_vstime[i], 
                       pch=season_pchs,
                       cex=scattercexs[i])

                # add legend if wanted
                if (add_legend) {
                    le <- list()
                    le$pos <- "topleft"
                    #le$pos <- "topright"
                    le$ncol <- 1
                    #le$ncol <- 2 
                    le$text <- names(season_cols) #names_legend[i]
                    le$col <- season_cols #"black"
                    le$lty <- NA
                    le$lwds <- NA
                    #le$pchs <- scatterpchs_vstime[i]
                    le$pchs <- scatterpchs_vstime
                    le$cex <- 1
                    le$cex <- 0.85
                    if (T) {
                        le <- reorder_legend(le)
                    }
                    if (length(le$pos) == 1) {
                        legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                               pch=le$pch, col=le$col, ncol=le$ncol,
                               x.intersp=0.2, cex=le$cex, bty="n")
                    } else if (length(le$pos) == 2) {
                        legend(x=le$pos[1], y=le$pos[2],
                               legend=le$text, lty=le$lty, lwd=le$lwd,
                               pch=le$pch, col=le$col, ncol=le$ncol,
                               x.intersp=0.2, cex=le$cex, bty="n")
                    }
                } # if add_legend

                box()
                message("\n", "save plot ", plotname, " ...")
                dev.off()
                if (p$plot_type == "pdf") {
                    if("extrafont" %in% (.packages())){
                        extrafont::embed_fonts(plotname, outfile=plotname)
                    } else {
                        grDevices::embedFonts(plotname, outfile=plotname)
                    }
                }

            } # if dims[[i]]$time_frequency == "monthly"

        } else { # if `temp2_datas` does not exist
            message("but `", varname, "_datas` does not exist.")
        } # if temp2_datas exists or not
            

    } else {
        message("but nsettings = ", nsettings, ", ndims_unique = ", ndims_unique, 
                ", vardims_unique = ", paste(vardims_unique, collapse=", "), "\n",
                " --> =! 1 & 1 & \"time\"") 
    }
} # if plot_scatter_setting1_vs_setting2 

## plot var1 vs var2 of `datas`
if (plot_scatter_var1_vs_var2) {

    message("\n", "****************** `plot_scatter_var1_vs_var2`=T --> scatterplot varx vs vary ***************************")
    
    if (ndims_unique == 1 && vardims_unique == "time") {

        # set here
        if (F) { # TOA imbalance gregory et al. 2004 stuff 
            varnamex <- "temp2"
            varnamey <- "toa_imbalance"
        } else if (T) { # temp2 vs precipitation weighted temp2
            varnamex <- "temp2"
            varnamey <- "ptemp"
        } else if (F) {
            varnamex <- "temp2"
            varnamey <- "wisoaprt_d"
        }

        if (exists(paste0(varnamex, "_datas")) 
            && exists(paste0(varnamey, "_datas"))) {

            varname <- paste0(varnamex, "_vs_", varnamey)
            message("\nvarnamex = \"", varnamex, "\"\n",
                     "varnamey = \"", varnamey, "\"\n",
                     "varname = \"", varname, "\"")
            eval(parse(text=paste0("varx <- ", varnamex, "_datas")))
            eval(parse(text=paste0("vary <- ", varnamey, "_datas")))
            eval(parse(text=paste0("varx_infos <- ", varnamex, "_infos")))
            eval(parse(text=paste0("vary_infos <- ", varnamey, "_infos")))

            if (varname == "temp2_vs_toa_imbalance" && T) {
                message("\n", "substract last PI value from experiments ...")
                for (i in 2:nsettings) {
                    varx[[i]] <- varx[[i]] - rep(varx[[1]][length(varx[[1]])], t=length(length(varx[[1]])))
                    varx_infos[[i]]$label <- "2m temperature increase [K]"
                }
                # atlast: pi itself
                varx[[1]] <- varx[[1]] - rep(varx[[1]][length(varx[[1]])], t=length(length(varx[[1]])))
                varx_infos[[1]]$label <- "2m temperature increase [K]"
            }

            xlim <- range(varx, na.rm=T)
            ylim <- range(vary, na.rm=T)
            xat <- pretty(xlim, n=10)
            xlab <- format(xat, trim=T)
            yat <- pretty(ylim, n=10)
            ylab <- format(yat, trim=T)

            plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                               varname, "_", 
                               paste0(names_short, "_", seasonsp, 
                                      "_", froms_plot, "_to_", tos_plot, "_", areas, collapse="_vs_"), 
                               ".", p$plot_type)
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            if (p$plot_type == "png") {
                png(plotname, width=p$scatter_width, height=p$scatter_height,
                    res=p$dpi, family=p$family_png)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=p$inch, height=p$inch,
                    family=p$family_pdf)
            }
            
            message("xlim = ", min(xlim), " / ", max(xlim))
            message("ylim = ", min(ylim), " / ", max(ylim))

            # set plot margins
            mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
            mar[4] <- 1 # decrease right margin
            if (!add_title) mar[3] <- 1 # decrease upper margin
            if (add_data_right_yaxis_ts) mar[4] <- mar[2] # same as left  

            # open plot
            par(mar=mar)
            plot(varx[[1]], vary[[1]], t="n",
                 xlab=NA, ylab=NA, 
                 xlim=xlim, ylim=ylim, xaxt="n", yaxt="n")
            axis(1, at=xat, labels=xlab, cex.axis=1)
            axis(2, at=yat, labels=ylab, las=1, cex.axis=1)

            # add title
            if (add_title) {
                title <- paste0(paste(unique(areas), collapse=","), 
                                " ", mode, " ", varname, " ", 
                                paste(unique(seasonsp), collapse=","), " ", 
                                paste(unique(fromsp), collapse=","), " to ", 
                                paste(unique(tosp), collapse=","))
                title(title, cex.main=0.5)
            }

            # add variable label
            mtext(side=1, varx_infos[[1]]$label, line=3.4)
            mtext(side=2, vary_infos[[1]]$label, line=3.4)

            # add zero lines
            if (add_zeroline) {
                abline(h=0, col="gray", lwd=0.5)
                abline(v=0, col="gray", lwd=0.5)
            }

            # if add 1:1 line to scatter plot
            if (add_1to1_line_scatter) {
                message("add 1:1 line ...")
                abline(a=0, b=1, col="gray") # a=intercept, b=slope
            }

            # add data to scatter plot
            message("add data ...")
            plotorder <- 1:nsettings
            
            # special: change plot order
            if (varname == "temp2_vs_toa_imbalance" && any(names_short == "piControl")) {
                message("change plot order from ", paste0(plotorder, collapse=","), " to ", appendLF=F)
                plotorder <- c(plotorder[-which(names_short == "piControl")], which(names_short == "piControl"))
                message(paste(plotorder, collapse=","), " ...")
            }

            # add gray dots of data first
            if (varname == "temp2_vs_toa_imbalance") {
                for (i in plotorder) {
                    if (names_short[i] == "piControl") {
                        # nothing
                    } else {
                        points(varx[[i]], vary[[i]], 
                               #col=cols_rgb[i], 
                               col=rgb(t(col2rgb("gray")/255), alpha=0.2),
                               pch=scatterpchs[i], cex=scattercexs[i])
                    }
                }
            } # if add gray dots of data first
            
            # now add real data 
            for (i in plotorder) {
                if (varname == "temp2_vs_toa_imbalance" && names_short[i] == "piControl") {
                    message("special: plot only time mean")
                    points(mean(varx[[i]]), mean(vary[[i]]), 
                           #col=cols_rgb[i], 
                           col=cols[i],
                           pch=scatterpchs[i], cex=scattercexs[i])
                } else if (varname == "temp2_vs_toa_imbalance" && T) { 
                    message("special: use year as symbols")
                    #years_of_setting_to_show <- c(1:10, seq(25, 250, b=25))
                    years_of_setting_to_show <- c(1:10, 15, seq(20, 100, b=10), seq(125, 250, b=25))
                    tmpx <- varx[[i]]
                    tmpy <- vary[[i]]
                    tmpx[years_of_setting_to_show] <- NA
                    tmpy[years_of_setting_to_show] <- NA
                    #points(tmpx, tmpy, 
                    #       col=cols_rgb[i], 
                    #       #col=cols[i],
                    #       pch=scatterpchs[i], cex=scattercexs[i])
                    # add wanted years as text
                    text(varx[[i]][years_of_setting_to_show], 
                         vary[[i]][years_of_setting_to_show], 
                         labels=years_of_setting_to_show,
                         #col=cols_rgb[i], 
                         col=cols[i], 
                         cex=scattercexs[i])
                } else {
                    points(varx[[i]], vary[[i]], 
                           col=cols_rgb[i], 
                           #col=cols[i],
                           pch=scatterpchs[i], cex=scattercexs[i])
                } # special plots depending on setting
            } # finished add data to scatter plot 

            # add linear trend
            if (add_linear_trend) {
                message("\n", "add linear trend ...")
                lms_lin <- vector("list", l=nsettings)
                lm_text <- c()
                for (i in 1:nsettings) {
                    message("setting ", i, "/", nsettings, ": ", names_short[i])
                    lms_lin[[i]] <- lm(vary[[i]] ~ varx[[i]])
                    lm_summary <- summary(lms_lin[[i]])
                    print(lm_summary)
                    # linear regression results
                    intercept <- as.vector(lm_summary$coefficients[1,1])
                    intercept_error <- as.vector(lm_summary$coefficients[1,2])
                    intercept_pval <- paste0("=", lm_summary$coefficients[1,4])
                    slope <- as.vector(lm_summary$coefficients[2,1])
                    slope_error <- as.vector(lm_summary$coefficients[2,2])
                    slope_pval <- lm_summary$coefficients[2,4]
                    if (slope_pval < 1e-15) { 
                        slope_pval <- "<1e-15"
                    } else {
                        slope_pval <- paste0("=", format(slope_pval, trim=T))
                    }
                    # plot regression line within data limits only
                    if (F) {
                        message("draw linear regression line withinregression limits only ...")
                        lines(varx[[i]], lms_lin[[i]]$fitted.values, 
                              col=cols[i], lwd=lwds[i], lty=ltys[i])
                    # or plot line through whole plot with regression coefficients
                    } else if (T) {
                        message("draw linear regression line through whole plot ...")
                        abline(a=lms_lin[[i]]$coefficients[1], b=lms_lin[[i]]$coefficients[2],
                               col=cols[i], lwd=lwds[i], lty=ltys[i])
                    }
                    # add linear regression coefficients to legend
                    if (F) {
                        message("add linear regression coeficients to legend ...")
                        first_part <- names_legend[i]
                        last_part <- "" # default
                        last_part <- eval(substitute(expression(paste("(", alpha, "=", slope, ", p", p, ", r=", r, ")")),
                                                     list(slope=round(slope, 2), p=slope_pval, 
                                                          r=round(sqrt(lm_summary$r.squared), 2))))
                        if (is.expression(last_part)) {
                            new <- bquote(.(do.call(substitute, as.list(first_part))) ~ 
                                          .(do.call(substitute, as.list(last_part))))
                            names_legend[i] <- eval(substitute(expression(new), list(new=new)))
                        }
                    }
                    # special stuff 
                    if (varname == "temp2_vs_toa_imbalance" && names_short[i] == "1pctCO2") {

                        ## winton et al. 2014: transient climate respomse: TCR
                        # use global warming as modeled in the 1pct experiment when the pi CO2 value doubled
                        co2_hist_1850_ind <- which.min(abs(co2_hist$time - as.POSIXlt("1850-01-01", tz="UTC")))
                        co2_hist_1850 <- drop(co2_hist$co2_ppm[co2_hist_1850_ind])
                        co2_1pct_1850_doubled_ind <- which.min(abs(co2_1pct$co2_ppm - 2*co2_hist_1850))
                        co2_1pct_1850_doubled <- drop(co2_1pct$co2_ppm[co2_1pct_1850_doubled_ind])
                        deltaT_1pct_1850_co2_doubled <- varx[[i]][co2_1pct_1850_doubled_ind]
                        message("CO2 of 1pctCO2 experiment = 2 x ", co2_hist_1850, " ppm (piControl CO2 of ", 
                                co2_hist$time[co2_hist_1850_ind], ") at year ", co2_1pct_1850_doubled_ind, " = ", 
                                co2_1pct$time[co2_1pct_1850_doubled_ind], " = ", co2_1pct_1850_doubled, " ppm", "\n",
                                " --> deltaT of 1pctCO2 experiment at this year = ", deltaT_1pct_1850_co2_doubled, " K = TCR")
                        # or use model year 61-80 --> 1911-1930 (20 year) mean global warming as in winton et al. 2014
                        year_inds <- which(time_dim[[i]] >= as.POSIXlt("1911-01-01", tz="UTC") &
                                           time_dim[[i]] <= as.POSIXlt("1931-01-01", tz="UC"))
                        average_deltaT_1pct_1850_co2_doubled <- mean(varx[[i]][year_inds])
                        message("average deltaT of 1pctCO2 experiment of years ", min(time_dim[[i]][year_inds]), " to ", 
                                max(time_dim[[i]][year_inds]), " = ", average_deltaT_1pct_1850_co2_doubled, " K")
                        lm_text <- c(lm_text,
                                     eval(substitute(expression(paste("TCR = ", Delta, "T"["1%"], "(CO"[2], "=2" %*% "CO"[paste("2,PI")], 
                                                                      " = ", co2_1pct_1850_doubled, " ppm)")),
                                                     list(co2_1pct_1850_doubled=round(co2_1pct_1850_doubled)))),
                                     eval(substitute(expression(paste("    = ", bar(paste(Delta, "T"))["1%"]^"years 61-80", 
                                                                      " = ", average_deltaT_1pct_1850_co2_doubled, " K")),
                                                     list(average_deltaT_1pct_1850_co2_doubled=round(average_deltaT_1pct_1850_co2_doubled, 2)))))

                    } else if (varname == "temp2_vs_toa_imbalance" && names_short[i] == "abrupt-4xCO2") {
                        
                        # gregory et al. 2004: equilibrium climate sensitivity (ECS):
                        alpha <- slope
                        alpha_error <- slope_error
                        radiative_forcing_F <- intercept
                        radiative_forcing_F_error <- intercept_error
                        deltaT_eq_4x <- sort(c((radiative_forcing_F - radiative_forcing_F_error)/(abs(alpha) - alpha_error),
                                               (radiative_forcing_F + radiative_forcing_F_error)/(abs(alpha) + alpha_error)))
                        deltaT_eq_2x <- deltaT_eq_4x/2
                        message("deltaT_eq_4x for setting ", names_short[i], " = (", round(min(deltaT_eq_4x), 2), ",", 
                                round(max(deltaT_eq_4x), 2), ") K (gregory et al. 2004)\n",
                                " --> deltaT_eq_4x/2 = deltaT_eq_2x = ECS = equilibrium climate sensitivity = ", 
                                round(min(deltaT_eq_2x), 2), ",", round(max(deltaT_eq_2x), 2), " K")
                        Forcing <- abs(alpha)*as.vector(varx[[i]]) # = alpha*dT
                        lm_text <- c(lm_text,
                                     eval(substitute(expression(paste("F"[paste("4" %*% "")], " = ", radiative_forcing_F, "" %+-% "",
                                                                      radiative_forcing_F_error, " W m"^paste(-2), " (intercept)")),
                                                     list(radiative_forcing_F=round(radiative_forcing_F, 2),
                                                          radiative_forcing_F_error=round(radiative_forcing_F_error, 2)))),
                                     eval(substitute(expression(paste(alpha, ""[paste("4" %*% "")], " = ", alph, "" %+-% "", alpha_error, 
                                                                      " W m"^paste(-2), " K"^paste(-1), " (slope)")), 
                                                     list(alph=round(alpha, 2), alpha_error=round(alpha_error, 2)))),
                                     eval(substitute(expression(paste("F"[paste("4" %*% "")], "/|", alpha, ""[paste("4" %*% "")], 
                                                                      "| = ", Delta, "T"[paste("eq,4" %*% "")],
                                                                      " = ", deltaT_eq_4x_lower, "-", deltaT_eq_4x_upper, " K")),
                                                     list(deltaT_eq_4x_lower=round(min(deltaT_eq_4x), 2), deltaT_eq_4x_upper=round(max(T_eq_4x), 2)))),
                                     eval(substitute(expression(paste("ECS = ", Delta, "T"[paste("eq,2" %*% "")], " = 1/2 ", Delta, "T"[paste("eq,4" %*% "")], 
                                                                      " = ", deltaT_eq_2x_lower, "-", deltaT_eq_2x_upper, " K")),
                                                     list(deltaT_eq_2x_lower=round(min(deltaT_eq_2x), 2), deltaT_eq_2x_upper=round(max(deltaT_eq_2x), 2)))))
                    } # if special setting
                } # for i nsettings
                if (exists("deltaT_eq_2x") && exists("average_deltaT_1pct_1850_co2_doubled")) {
                    CER <- average_deltaT_1pct_1850_co2_doubled/deltaT_eq_2x # climate equilibrium ratio
                    message("TCR/ECS = ", min(CER), ",", max(CER))
                }
                if (!is.null(lm_text)) {
                    message("add special linear regression infos to plot")
                    legend("topright", 
                           lm_text, col="black", #text.col=text_cols[i], 
                           lty=NA, pch=NA, lwd=NA, bty="n", 
                           cex=0.9, y.intersp=1.1)
                }
            } # add_linear_trend

            # add non-linear trend
            if (add_nonlinear_trend) {
                message("\n", "add non-linear trend ...")
                lms_exp <- vector("list", l=nsettings)
                library(forecast)
                for (i in 1:nsettings) {
                    if (any(i == c(2, 3))) {
                        message("setting ", i, "/", nsettings, ": ", names_short[i])
                        lms_exp[[i]] <- tslm(ts(vary[[i]]) ~ trend, lambda = 0)
                        print(summary(lms_exp[[i]]))
                        
                        # plot regression line within data limits only
                        if (F) {
                            lines(varx[[i]], lms_lin[[i]]$fitted.values, 
                                  col=cols[i], lwd=lwds[i], lty=ltys[i])
                            
                        # or plot line through whole plot with regression coefficients
                        } else if (T) {
                            abline(a=lms_lin[[i]]$coefficients[1], b=lms_lin[[i]]$coefficients[2],
                                   col=cols[i], lwd=lwds[i], lty=ltys[i])
                        }
                    }
                }
            }

            # add legend if wanted
            if (add_legend) {
                message("\n", "add default stuff to plot_scatter_var1_vs_var2 legend ...")
                le <- list()
                #le$pos <- "topright"
                le$pos <- "bottomright"
                le$ncol <- 1
                #le$ncol <- 2 
                le$text <- names_legend
                #le$col <- cols_rgb
                le$col <- cols
                le$text_cols <- text_cols
                le$lty <- NA
                le$lwds <- NA
                #le$pchs <- scatterpchs
                le$pchs <- pchs
                le$cex <- 1
                # add stuf to legend here
                if (F) {
                    message("\n", "add non default stuff to plot_scatter_var1_vs_var2 legend ...")

                }
                # reorder reading direction from R's default top->bottom to left->right
                if (T) {
                    le <- reorder_legend(le)
                }
                if (length(le$pos) == 1) {
                    legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                           pch=le$pch, col=le$col, text.col=le$text_cols, ncol=le$ncol,
                           x.intersp=0.2, cex=le$cex, bty="n")
                } else if (length(le$pos) == 2) {
                    legend(x=le$pos[1], y=le$pos[2],
                           legend=le$text, lty=le$lty, lwd=le$lwd,
                           pch=le$pch, col=le$col, text.col=le$text_cols, ncol=le$ncol,
                           x.intersp=0.2, cex=le$cex, bty="n")
                }
            } # if add_legend

            box()
            message("\n", "save plot ", plotname, " ...")
            dev.off()
            if (p$plot_type == "pdf") {
                if("extrafont" %in% (.packages())){
                    extrafont::embed_fonts(plotname, outfile=plotname)
                } else {
                    grDevices::embedFonts(plotname, outfile=plotname)
                }
            }

            ## scatter plot for each setting colored by time or season
            if (T) {
                message("\nspecial: scatter vs seasons ...")

                for (i in 1:nsettings) {

                    if (dims[[i]]$time_frequency == "monthly") {
                        
                        # color data by time or seasons
                        if (F) { # by time
                            message("color by time ...")
                            timecols <- colorRampPalette(c("blue", "red"))(length(varx[[i]]))
                            scatter_suffix <- "_bytime"
                        } else if (T) { # by season
                            message("color by season ...")
                            if (i == 1) scatterpchs_vstime <- 1:4
                            season_cols <- c(DJF="blue", MAM="darkgreen", JJA="red", SON="brown")
                            timecols <- rep(NA, t=length(varx[[i]]))
                            season_pchs <- timecols
                            djf_inds <- which(!is.na(match(unclass(dims[[i]]$timelt)$mon+1, c(1, 2, 12))))
                            mam_inds <- which(!is.na(match(unclass(dims[[i]]$timelt)$mon+1, c(3, 4, 5))))
                            jja_inds <- which(!is.na(match(unclass(dims[[i]]$timelt)$mon+1, c(6, 7, 8))))
                            son_inds <- which(!is.na(match(unclass(dims[[i]]$timelt)$mon+1, c(9, 10, 11))))
                            timecols[djf_inds] <- season_cols["DJF"]
                            timecols[mam_inds] <- season_cols["MAM"]
                            timecols[jja_inds] <- season_cols["JJA"]
                            timecols[son_inds] <- season_cols["SON"]
                            season_pchs[djf_inds] <- scatterpchs_vstime[1]
                            season_pchs[mam_inds] <- scatterpchs_vstime[2]
                            season_pchs[jja_inds] <- scatterpchs_vstime[3]
                            season_pchs[son_inds] <- scatterpchs_vstime[4]
                            scatter_suffix <- "_byseason"
                        }
                        timecols_rgb <- rgb(t(col2rgb(timecols)/255), alpha=alpha)
                        
                        xlim <- range(varx[[i]], na.rm=T)
                        ylim <- range(vary[[i]], na.rm=T)
                        xat <- pretty(xlim, n=10)
                        xlab <- format(xat, trim=T)
                        yat <- pretty(ylim, n=10)
                        ylab <- format(yat, trim=T)

                        plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                                           varname, "_", 
                                           paste0(names_short[i], "_", seasonsp[i], 
                                                  "_", froms_plot[i], "_to_", tos_plot[i], "_", 
                                                  areas[i], collapse="_vs_"), 
                                           scatter_suffix,
                                           ".", p$plot_type)
                        dir.create(dirname(plotname), recursive=T, showWarnings=F)
                        if (p$plot_type == "png") {
                            png(plotname, width=p$scatter_width, height=p$scatter_height,
                                res=p$dpi, family=p$family_png)
                        } else if (p$plot_type == "pdf") {
                            pdf(plotname, width=p$inch, height=p$inch,
                                family=p$family_pdf)
                        }

                        # set plot margins
                        mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                        mar[4] <- 1 # decrease right margin
                        if (!add_title) mar[3] <- 1 # decrease upper margin
                        if (add_data_right_yaxis_ts) mar[4] <- mar[2] # same as left  

                        # open plot
                        par(mar=mar)
                        plot(varx[[1]], vary[[1]], t="n",
                             xlab=NA, ylab=NA, 
                             xlim=xlim, ylim=ylim, xaxt="n", yaxt="n")
                        axis(1, at=xat, labels=xlab)
                        axis(2, at=yat, labels=ylab, las=1)

                        # add title
                        if (add_title) {
                            title <- paste0(names_short[i], " ", 
                                            paste(unique(areas[i]), collapse=","), 
                                            " ", mode, " ", varname, " ", 
                                            paste(unique(seasonsp[i]), collapse=","), " ", 
                                            paste(unique(fromsp[i]), collapse=","), " to ", 
                                            paste(unique(tosp[i]), collapse=","))
                            title(title, cex.main=0.5)
                        }

                        # add variable label
                        mtext(side=1, varx_infos[[i]]$label, line=3.4, cex=0.9)
                        mtext(side=2, vary_infos[[i]]$label, line=3.4, cex=0.9)

                        # add zero lines
                        if (add_zeroline) {
                            abline(h=0, col="gray", lwd=0.5)
                            abline(v=0, col="gray", lwd=0.5)
                        }

                        # add 1:1 line
                        if (add_1to1_line_scatter) {
                            message("add 1:1 line ...")
                            abline(a=0, b=1, col="gray") # a=intercept, b=slope
                        }
                        
                        # add data to scatter plot colored by time
                        message("add data")
                        points(varx[[i]], vary[[i]], 
                               col=timecols,
                               #col=timecols_rgb,
                               #pch=scatterpchs_vstime[i], 
                               pch=season_pchs,
                               cex=scattercexs[i])

                        # add legend if wanted
                        if (add_legend) {
                            le <- list()
                            le$pos <- "topleft"
                            #le$pos <- "topright"
                            le$ncol <- 1
                            #le$ncol <- 2 
                            le$text <- names(season_cols) #names_legend[i]
                            le$col <- season_cols #"black"
                            le$lty <- NA
                            le$lwds <- NA
                            #le$pchs <- scatterpchs_vstime[i]
                            le$pchs <- scatterpchs_vstime
                            le$cex <- 1
                            le$cex <- 0.85
                            if (T) {
                                le <- reorder_legend(le)
                            }
                            if (length(le$pos) == 1) {
                                legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                                       pch=le$pch, col=le$col, ncol=le$ncol,
                                       x.intersp=0.2, cex=le$cex, bty="n")
                            } else if (length(le$pos) == 2) {
                                legend(x=le$pos[1], y=le$pos[2],
                                       legend=le$text, lty=le$lty, lwd=le$lwd,
                                       pch=le$pch, col=le$col, ncol=le$ncol,
                                       x.intersp=0.2, cex=le$cex, bty="n")
                            }
                        } # if add_legend

                        box()
                        message("\n", "save plot ", plotname, " ...")
                        dev.off()
                        if (p$plot_type == "pdf") {
                            if("extrafont" %in% (.packages())){
                                extrafont::embed_fonts(plotname, outfile=plotname)
                            } else {
                                grDevices::embedFonts(plotname, outfile=plotname)
                            }
                        }

                    } # if dims[[i]]$time_frequency == "monthly"

                } # for i nsettings

            } # scatter plot for each setting colored by time

        } else { # if exists("varx") && exists("vary")
            message()
            if (!exists(paste0(varnamex, "_datas"))) {
                message("`plot_scatter_var1_vs_var2` = T but could not find ",
                        "`", varnamex, "_datas`")
            }
            if (!exists(paste0(varnamey, "_datas"))) {
                message("`plot_scatter_var1_vs_var2` = T but could not find ",
                        "`", varnamey, "_datas`")
            }
        } # if exists("varx") && exists("vary")
    
    } else { # if (ndims_unique == 1 && vardims_unique == "time")

        message("but ndims_unique = ", ndims_unique, ", vardims_unique = ", paste(vardims_unique, collapse=", "), "\n",
                " --> =! 1 & \"time\"") 

    } # if (ndims_unique == 1 && vardims_unique == "time") {

} # if plot_scatter_var1_vs_var2 of `datas`

message("\n", "finish", "\n")


