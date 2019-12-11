## R

#options(warn = 2) # stop on warnings
if (F) {
    rm(list=ls())
    fctbackup <- `[`; `[` <- function(...) { fctbackup(..., drop=F) }
    # `[` <- fctbackup 
}
graphics.off()

# user input
fnml <- "namelist.plot.r"
message("\n", "Read ", fnml, " ...")
source(fnml)

# load special data
alpha <- 0.3 # transparent: 0,1 (0 fully transparent)
if (machine_tag == "mistral") {
    # cmip6 co2 hist
    co2_hist_ncin <- nc_open("/pool/data//ECHAM6/input/r0007/greenhouse_historical.nc") 
    message("\n", "read hist CO2 from ", co2_hist_ncin$filename, " ...")
    time <- co2_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*365.25*86400, origin="0000-01-01", tz="UTC", format="%Y")
    co2_hist <- list(co2_ppm=ncvar_get(co2_hist_ncin, "CO2"), time=time, timelt=timelt,
                     col="black", lty=2, lwd=0.5, pch=NA)

    # cmip6 co2 1pct
    co2_1pct_ncin <- nc_open("/pool/data//ECHAM6/input/r0008/greenhouse_1pctCO2.nc")
    message("\n", "read 1pct CO2 from ", co2_1pct_ncin$filename, " ...")
    time <- co2_1pct_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*365.25*86400, origin="0000-01-01", tz="UTC", format="%Y")
    co2_1pct <- list(co2_ppm=ncvar_get(co2_1pct_ncin, "CO2"), time=time, timelt=timelt)

    # cmip6 historical monthly total solar irradiance 
    tsi_hist_ncin <- nc_open("/work/ba0941/a270073/data/cmip6/solar_irradiance/swflux_14band_monthly_1850-2014.nc")
    message("\n", "read historical monthly total solar irradiance from ", tsi_hist_ncin$filename, " ...")
    time <- tsi_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*86400, origin="1850-01-01", tz="UTC")
    tsi_hist_monthly <- list(tsi_hist=ncvar_get(tsi_hist_ncin, "TSI"), time=time, timelt=timelt,
                             col="orange2",
                             col_rgb=rgb(t(col2rgb("orange2")/255), alpha=0.5),
                             text="TSI", lty=1, lwd=0.5, pch=NA) 

    # cmip6 historical annual total solar irradiance 
    tsi_hist_ncin <- nc_open("/work/ba0941/a270073/data/cmip6/solar_irradiance/swflux_14band_annual_1850-2014.nc")
    message("\n", "read historical monthly total solar irradiance from ", tsi_hist_ncin$filename, " ...")
    time <- tsi_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*86400, origin="1850-01-01", tz="UTC")
    tsi_hist_annual <- list(tsi_hist=ncvar_get(tsi_hist_ncin, "TSI"), time=time, timelt=timelt,
                            col="orange2",
                            col_rgb=rgb(t(col2rgb("orange2")/255), alpha=0.5),
                            text="TSI", lty=1, lwd=0.5, pch=NA) 
    
    # hadcrut4 global monthly temperature anomaly wrt 1961-1990
    hadcrut4_ncin <- nc_open("/work/ba0941/a270073/data/HadCRUT4/HadCRUT_global_SAT_anomaly_wrt_1961-1990_HadCRUT.4.6.0.0.monthly_ns_avg.txt.nc")
    message("\n", "read hadcrut4 global monthly SAT anomalies wrt to 1961-1990 from ", hadcrut4_ncin$filename, " ...")
    time <- hadcrut4_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    hadcrut4_sat_anom_monthly <- list(hadcrut4_sat_anom=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990"),
                                      hadcrut4_sat_anom_lower_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_lower_uncertainty"),
                                      hadcrut4_sat_anom_upper_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_upper_uncertainty"),
                                      time=time, timelt=timelt)

    # hadcrut4 global annual temperature anomaly wrt 1961-1990
    hadcrut4_ncin <- nc_open("/work/ba0941/a270073/data/HadCRUT4/HadCRUT_global_SAT_anomaly_wrt_1961-1990_HadCRUT.4.6.0.0.annual_ns_avg.txt.nc")
    message("\n", "read hadcrut4 global annual SAT anomalies wrt to 1961-1990 from ", hadcrut4_ncin$filename, " ...")
    time <- hadcrut4_ncin$dim$time$vals
    timelt <- as.POSIXlt(time, origin="1970-01-01", tz="UTC")
    hadcrut4_sat_anom_annual <- list(hadcrut4_sat_anom=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990"),
                                     hadcrut4_sat_anom_lower_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_lower_uncertainty"),
                                     hadcrut4_sat_anom_upper_uncert=ncvar_get(hadcrut4_ncin, "global_SAT_anomaly_wrt_1961-1990_upper_uncertainty"),
                                     time=time, timelt=timelt, col="black",
                                     col_rgb=rgb(t(col2rgb("black")/255), alpha=0.1),
                                     text="HadCRUT4", lty=1, lwd=1, pch=NA)

    # gistempv4 global annual temperature anomaly wrt 1951-1980
    gistempv4_ncin <- nc_open("/work/ba0941/a270073/data/GISTEMPv4/GISTEMPv4_global_SAT_anomaly_wrt_1951-1980_GLB.Ts+dSST.csv.nc")
    message("\n", "read gistempv4 global annual SAT anomalies wrt to 1951-1980 from ", gistempv4_ncin$filename, " ...")
    time <- gistempv4_ncin$dim$time$vals # YYYY
    timelt <- as.POSIXlt(as.Date(paste0(time, "-06-30")), tz="UTC") # use mid-year
    gistempv4_sat_anom_annual <- list(gistempv4_sat_anom=ncvar_get(gistempv4_ncin, "global_SAT_anomaly_wrt_1951-1980"),
                                      time=time, timelt=timelt)

    rm(time, timelt)
} # finished reading extra datasets
co2_4co2 <- list(co2_ppm=1137.2679)
message("\n", "set 4CO2 to ", co2_4co2$co2_ppm, " ppm") 

# check user input and defaults
nsettings <- length(postpaths)
if (!exists("codes")) codes <- rep("", t=nsettings)
if (!exists("levs")) levs <- rep("", t=nsettings)
codesf <- codes
codesf[codes != ""] <- paste0("_selcode_", codesf[codes != ""])
levsf <- levs
levsf[levs != ""] <- paste0("_", levsf[levs != ""], "m")
if (!exists("areas")) areas <- rep("global", t=nsettings)
if (!exists("fromsp")) fromsp <- fromsf
if (!exists("tosp")) tosp <- tosf
if (!exists("froms_shift")) froms_shift <- rep(NA, t=nsettings)
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
if (!exists("remove_mean_froms")) remove_mean_froms <- rep(NA, t=nsettings) 
if (!exists("remove_mean_tos")) remove_mean_tos <- rep(NA, t=nsettings) 
if (!exists("ltys")) ltys <- rep(1, t=nsettings)
if (!exists("lwds")) lwds <- rep(1, t=nsettings)
if (!exists("pchs")) pchs <- rep(NA, t=nsettings)
if (!exists("scatterpchs")) scatterpchs <- rep(16, t=nsettings)
if (!exists("scatterpchs_vstime")) scatterpchs_vstime <- 1:nsettings
if (!exists("scattercexs")) scattercexs <- rep(0.5, t=nsettings)
if (!exists("cols")) {
    # default: black, red, blue
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

base <- 10
power <- 0 # default: 0 --> 10^0 = 1e0 = 1 --> nothing happens
cols_rgb <- rgb(t(col2rgb(cols)/255), alpha=alpha)


# allocate
datas <- vector("list", l=nsettings)
names(datas) <- names_short
data_infos <- dims <- times <- times2 <- times3 <- timesp <- timeslt <- datas

# read data
message("\n", "Read data ...")
for (i in 1:nsettings) {

    message("\n", "*********************************************")
    message("setting ", i, "/", nsettings, ": ", names_short[i], " ...")
    inpath <- paste0(postpaths[i], "/", mode, "/", varnames_in[i])
    fname <- paste0(prefixes[i], "_", mode, 
                    codesf[i], "_selname_", varnames_in[i], 
                    "_", areas[i],
                    "_", seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                    ".nc") 

    message("\n", "open ", inpath, "/", fname, " ...")
    ncin <- nc_open(paste0(inpath, "/", fname))

    # get dims of file
    message("\n", "get dims ...")
    dims_per_file <- names(ncin$dim)
    dimtmp <- vector("list", l=ncin$ndims)
    names(dimtmp) <- dims_per_file
    for (di in 1:length(dimtmp)) {
        message(dims_per_file[di], " ", appendLF=F)
        dimtmp[[di]] <- ncin$dim[[di]]$vals
        if (di == length(dimtmp)) message()
    }
    dims[[i]] <- dimtmp
    rm(dimtmp)

    # time dim as posix object
    if (any(names(dims[[i]]) == "time")) {

        timein_units <- ncin$dim$time$units
        message("\n", "detected \"time\" dim -> make POSIXlt from timein_units:")
        print(timein_units)
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

        # shift times due to e.g. senseless spinup years
        # as.POSIXlt's 'year' starts at 1900
        if (!is.na(froms_shift[i])) {
            # from year in  = min(timein_lt$year) + 1900
            message("\n", "shift from fromsf[", i, "] = ", fromsf[i], 
                    " to froms_shift[", i, "] = ", froms_shift[i], " ...")
            message("range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))
            shift_by <- -(min(timein_lt$year) + 1900 - froms_shift[i]) 
            message("shift_by = ", shift_by, " years") 
            timein_lt$year <- timein_lt$year + shift_by - 1
            message("range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))
            message("update fromsp[", i, "] = ", fromsp[i], " to ", appendLF=F)
            fromsp[i] <- min(timein_lt$year) + 1900
            message(fromsp[i])
            message("update tosp[", i, "] = ", tosp[i], " to ", appendLF=F)
            tosp[i] <- max(timein_lt$year) + 1900
            message(tosp[i])
        } # if !is.na(froms_shift[i])

        # find temporal subset based on given fromsp and tosp
        fromsplt <- as.POSIXlt(paste0(fromsp[i], "-01-01 00:00:00"), tz="UTC")
        tosplt <- as.POSIXlt(paste0(tosp[i], "-12-31 23:59:59"), tz="UTC")
        time_inds <- which(timein_lt >= fromsplt & timein_lt <= tosplt)
        # take subset only if necessary
        if (length(time_inds) > 0 && length(time_inds) != length(timein_lt)) { 
            message("\n", "found ", length(time_inds), " temporal subset time_inds based on fromsp[", 
                    i, "]=", fromsp[i], " to tosp[", i, "]=", tosp[i], " ...")
            timein_lt <- timein_lt[time_inds]
            message("range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))
        } # found temporal subset time_inds
        # update time dim

        # subset seasons from data if wanted (=seasonsp)
        # check which seasonsf and seasonp differ
        if (seasonsp[i] != seasonsf[i]) {
            message("\n", "cut season from `seasonf[", i, "]` = \"", seasonsf[i], 
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
            timein_lt <- timein_lt[month_inds]
            time_inds <- time_inds[month_inds]
            message("range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))
        } # cut season
        # finished time stuff
        dims[[i]]$time_inds <- time_inds
        dims[[i]]$timelt <- timein_lt

        # POSIXlt as numeric
        #dims[[i]]$timen <- lapply(dims[[i]]$timelt, as.numeric)

    } # if any of file dims is "time"

    # get vars of file
    message("\n", "get variables ...")
    vars_per_file <- names(ncin$var)
    vars <- vector("list", l=ncin$nvars)
    var_infos <- vars
    for (vi in 1:length(vars)) {
        message(vi, "/", length(vars), ": ", vars_per_file[vi])
        if (vars_per_file[vi] == paste0("var", codes[i])) {
            message("variable name of nc file \"", vars_per_file[i], 
                    "\" equals \"var`codes[", i, "]` = \"var", codes[i], 
                    "\". use `varnames_in[", i, "]` = \"", varnames_in[i], "\" from now on ...")
            names(vars)[i] <- varnames_in[i]
        } else {
            names(vars)[vi] <- vars_per_file[vi]
        }
        vars[[vi]] <- ncvar_get(ncin, vars_per_file[vi], collapse_degen=squeeze_data) 

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
                message("drop \"", paste0(names(len1_dim_inds), collapse="\",\""), 
                        "\" dim", ifelse(length(len1_dim_inds) > 1, "s", ""), " of length 1 ...")
                dimids <- dimids[-len1_dim_inds]
            } # if var has dims of length 1 
        } else {
            stop("not implemented")
        } # if squeeze_data
        attributes(vars[[vi]]) <- list(dim=dim(vars[[vi]]), dims=dims_per_file[dimids])
        #cmd <- paste0("tmp <- list(", paste0(dims_per_file[dimids], "=ncin$dim[[", dimids, "]]$vals", collapse=", "), ")")
    } # for vi nvars per setting
    datas[[i]] <- vars
    data_infos[[i]] <- var_infos
    rm(vars, var_infos)

    # all dimensions per setting
    dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")

    # cut temporal subset
    if (!is.null(dims[[i]]$time_inds)) {
        # check for variables that have time dim
        vars_with_timedim <- which(lapply(dims_per_setting, function(x) grep("time", x)) == 1)
        if (length(vars_with_timedim) > 0) {
            message("\n", "cut subset from time dim ...")
            for (vi in 1:length(vars_with_timedim)) {
                var_with_timedim_ind <- vars_with_timedim[vi]
                dims_of_var <- attributes(datas[[i]][[var_with_timedim_ind]])$dims # e.g. "time", "lon", "lat"
                timedimind <- which(dims_of_var == "time")
                cmd <- rep(",", t=length(dims_of_var))
                cmd[timedimind] <- paste0("dims[[", i, "]]$time_inds")
                cmd <- paste0("datas[[", i, "]][[", var_with_timedim_ind, "]] <- ",
                              "datas[[", i, "]][[", var_with_timedim_ind, "]][", paste0(cmd, collapse=""), "]")
                message(cmd)
                eval(parse(text=cmd))
                # subsetting removed attributes, apply again
                attributes(datas[[i]][[var_with_timedim_ind]]) <- list(dim=dim(datas[[i]][[var_with_timedim_ind]]), 
                                                                       dims=dims_of_var)
                # cut from time dimension
                dims[[i]]$time <- dims[[i]]$time[dims[[i]]$time_inds]

            } # vi vars per file with time dim
        } # if there are varbels with time dimension
    } # cut temporal subset

    # reorder lons to (-180,...,180) if wanted and necessary
    if (any(names(dims[[i]]) == "lon")) {
        if (reorder_lon_from_0360_to_180180) {
            if (any(dims[[i]]$lon < 180) && any(dims[[i]]$lon >= 180)) {
                message("\n", "detected lon dimension AND", "\n", "`reorder_lon_from_0360_to_180180 <- T` AND", "\n",
                        "any(lon < 180) && any(lon >= 180)", "\n", "--> reorder longitudes from (0,...,360) to (-180,...,180) degree ...")
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
                            # restore attributes remoed by abind() call
                            dimnames(datas[[i]][[var_with_londim_ind]]) <- NULL
                            attributes(datas[[i]][[var_with_londim_ind]]) <- list(dim=dim(datas[[i]][[var_with_londim_ind]]), 
                                                                                  dims=dims_of_var)
                        } # how many dims has the variable of whose dims one dim is "lon" 
                    } # for vi vars per file with lon dim
                } # if any vars with lon dim
            } # if (any(lons[[i]] < 180) && any(lons[[i]] >= 180))
        } # if reorder_lon_from_0360_to_180180
    } # if this file has lon dim

    # flip latitudes if necessary (needs to be increasing)
    if (any(names(dims[[i]]) == "lat")) {
        if (any(diff(dims[[i]]$lat) < 0)) {
            message("\n", "detected lat dimension and lats are decreasing -> flip latitudes ...") 
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
                } 
            } # if any vars with lat dim
        } # 
    } # if this file has lat dim

} # for i nsettings
message("\n", "****************** reading data finished ***************************")

varnames_unique <- unique(as.vector(unlist(sapply(datas, names))))


# save data before applying offset, multiplication factors, etc. for later
message("\n", "save original data without multiplication factors etc. for later ...")
for (vi in 1:length(varnames_unique)) {
    cmd <- paste0(varnames_unique[vi], "_datas_orig <- vector(\"list\", l=nsettings)")
    message("run `", cmd, "` ...")
    eval(parse(text=cmd))
    cmd <- paste0("names(", varnames_unique[vi], "_datas_orig) <- names(datas)")
    eval(parse(text=cmd))
    for (i in 1:nsettings) {
        cmd <- paste0(varnames_unique[vi], "_datas_orig[[", i, "]] <- datas[[", i, "]][[", vi, "]]")
        eval(parse(text=cmd))
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

        vari <- names(datas[[i]])[vi]

        # default variable axis label
        label <- names(datas[[i]])[vi]
        if (!is.null(data_infos[[i]][[vi]]$longname)) {
            label <- data_infos[[i]][[vi]]$longname
        }
        if (!is.null(data_infos[[i]][[vi]]$long_name)) {
            label <- data_infos[[i]][[vi]]$long_name
        }
        if (!is.na(remove_mean_froms[i])) {
            label <- paste0(label, " anomaly wrt ", remove_mean_froms[i], "-", remove_mean_tos[i])
        }
        if (!is.null(data_infos[[i]][[vi]]$units)) {
            label <- paste0(label, " [", data_infos[[i]][[vi]]$units, "]")
        }
        data_infos[[i]][[vi]]$label <- label

        # add specific things
        if (vari == "temp2") {
            data_infos[[i]][[vi]]$label <- "2m temperature [°C]"
            if (!is.na(remove_mean_froms[i])) {
                data_infos[[i]][[vi]]$label <- "2m temperature anomaly [°C]"
            }
            data_infos[[i]][[vi]]$offset$operator <- "-"
            data_infos[[i]][[vi]]$offset$value <- 273.15
        } else if (vari == "toa_imbalance") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("TOA imbalance [W m"^paste(-2), "]"))))

        } else if (vari == "tau_aero_550") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " ", tau, " dV [m"^3, "]"))))

        } # finished define variable specific things
    }
}
# finished setting variable specific things


# save data infos for later
message("\n", "save data infos for later ...")
for (vi in 1:length(varnames_unique)) {
    cmd <- paste0(varnames_unique[vi], "_infos <- vector(\"list\", l=nsettings)")
    message(cmd, " ...")
    eval(parse(text=cmd))
    cmd <- paste0("names(", varnames_unique[vi], "_infos) <- names(datas)")
    eval(parse(text=cmd))
    for (i in 1:nsettings) {
        cmd <- paste0(varnames_unique[vi], "_infos[[", i, "]] <- data_infos[[", i, "]][[", vi, "]]")
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


# remove some temporal mean if defined
if (any(!is.na(remove_mean_froms))) {
    message("\n", "remove temporal means if applicable ...")
    for (i in 1:nsettings) {
        if (!is.na(remove_mean_froms[i])) {
            if (any(names(dims[[i]]) == "time")) {
                remove_fromslt <- as.POSIXlt(paste0(remove_mean_froms[i], "-01-01 00:00:00"), tz="UTC")
                remove_toslt <- as.POSIXlt(paste0(remove_mean_tos[i], "-12-31 23:59:59"), tz="UTC")
                time_inds <- which(dims[[i]]$timelt >= remove_fromslt & dims[[i]]$timelt <= remove_toslt)
                if (length(time_inds) == 0) {
                    stop("no data found between `remove_mean_froms[", i, "]` = \"", remove_mean_froms[i], 
                         "\" and `remove_mean_tos[", i, "]` = \"", remove_mean_tos[i], 
                         "\". cannot remove this temporal mean.")
                } else {
                    for (vi in 1:length(datas[[i]])) {
                        # check if variable has time dimension 
                        dims_of_var <- attributes(datas[[i]][[vi]])$dims # e.g. "time", "lon", "lat"
                        timedimind <- which(dims_of_var == "time")
                        if (length(timedimind) == 1) {
                            mu <- rep(",", t=length(dims_of_var))
                            mu[timedimind] <- paste0("time_inds")
                            mu <- paste0("mu <- mean(datas[[", i, "]][[", vi, "]][", paste0(mu, collapse=""), "])")
                            message(mu)
                            eval(parse(text=mu))
                            message("remove temporal mean between ", remove_mean_froms[i], "-", 
                                    remove_mean_tos[i], " (`remove_mean_froms[", i, "]` to `remove_mean_tos[", i, "]`) = ",
                                    mu, " ", data_infos[[i]][[vi]]$units, " ...")
                            datas[[i]][[vi]] <- datas[[i]][[vi]] - mu 
                        } # remove temporal mean if variable has time dim
                    } # for vi all variables per setting
                } # if temporal mean can be removed
            } else { # if any variable has time dimension or not
                stop("`remove_mean_froms[", i, "]` = \"", remove_mean_froms[i], 
                     "\" but data has not \"time\" dim.")
            } # if any variable has time dimension or not
        } # if remove_mean_froms[i] is not NA
    } # for i nsettings
} # finished removing a temporal mean


# save data after applying offset, multiplication factors, etc. for later
message("\n", "save original data without multiplication factors etc. for later ...")
for (vi in 1:length(varnames_unique)) {
    cmd <- paste0(varnames_unique[vi], "_datas <- vector(\"list\", l=nsettings)")
    message(cmd, " ...")
    eval(parse(text=cmd))
    cmd <- paste0("names(", varnames_unique[vi], "_datas) <- names(datas)")
    eval(parse(text=cmd))
    for (i in 1:nsettings) {
        cmd <- paste0(varnames_unique[vi], "_datas[[", i, "]] <- datas[[", i, "]][[", vi, "]]")
        eval(parse(text=cmd))
    }
}


# get temporal output interval
time_frequencies <- rep(NA, t=nsettings)
for (i in 1:nsettings) {
    if (any(names(dims[[i]]) == "time")) {
        timediff <- round(lubridate::as.duration(diff(dims[[i]]$timelt)))
        # monthly (~4 weeks)
        if (all(timediff %in% c(lubridate::dseconds(2419200), # ~4 weeks
                                lubridate::dseconds(2505600), # ~4.14 weeks
                                lubridate::dseconds(2592000), # ~4.29 weeks
                                lubridate::dseconds(2678400), # ~4.43 weeks
                                lubridate::dseconds(2764800)))) { # 4.57 weeks
            dims[[i]]$time_frequency <- "monthly"
            # annual (~52 weeks)
        } else if (all(timediff %in% c(lubridate::dseconds(31536000), 
                                       lubridate::dseconds(31622400)))) {
            dims[[i]]$time_frequency <- "annual"
        } else {
            stop("asdasds")
        }
        time_frequencies[i] <- dims[[i]]$time_frequency
    }
} # finished getting temporal output interval


# calculate monthly means
if (any(seasonsp == "Jan-Dec") 
    && any(attributes(datas[[i]][[vi]])$dims == "time") 
    && any(sapply(dims, "[", "time_frequency") == "monthly")) {
    datasmon <- datas
    monlim <- NA
    for (i in 1:nsettings) {
        if (i == 1) message("\n", "calc monthly means of setting")
        if (dims[[i]]$time_frequency == "monthly" && seasonsp[i] == "Jan-Dec") {    
            message(i, ": ", names_short[i], " ...")
            for (vi in 1:length(datas[[i]])) { 
                if (length(dim(datas[[i]][[vi]])) == 1 && # var has only 1 dim 
                    attributes(datas[[i]][[vi]])$dims == "time") { # and its "time"
                    months <- unclass(dims[[i]]$timelt)$mon + 1
                    months_unique <- unique(months)
                    tmp <- rep(NA, t=length(months_unique))
                    for (mi in 1:length(months_unique)) {
                        tmp[mi] <- mean(datas[[i]][[vi]][months == months_unique[mi]], na.rm=T)
                    } 
                    datasmon[[i]][[vi]] <- tmp

                    # variable has more than 1 dims
                } else { 
                    message("monthly means with dims \"", 
                            paste0(attributes(datas[[i]][[vi]])$dims, collapse="\",\""), 
                            "\". not implemented.")
                    datasmon[[i]][[vi]] <- NA
                }

                attributes(datasmon[[i]][[vi]]) <- list(dim=length(tmp), dims="months")
                dims[[i]]$monmean_months <- months_unique
                dims[[i]]$monmean_range <- paste0(fromsp[i], "-", tosp[i])
                monlim <- range(monlim, dims[[i]]$monmean_months, na.rm=T)
            } # for vi nvars
        } else {
            message(i, ": ", names_short[i], " --> no monthly output or seasonp[", i, 
                    "] is not \"Jan-Dec\"")
        } # if input data is monthly 
    } # for i nsettings
    monat <- monlim[1]:monlim[2]
    monlab <- substr(month.abb[monat], 1, 1) # Jan -> J
} # if any(seasonsp == "Jan-Dec") && any(attributes(datas[[i]][[vi]])$dims == "time")
# finished calculating monthly means if applicable


# apply moving average
if (add_smoothed && any(attributes(datas[[i]][[vi]])$dims == "time") && 
    any(seasonsp == "Jan-Dec") && !all(n_mas == 1)) {
    message("\n", "apply moving averages ...")
    datasma <- datas
    for (i in 1:nsettings) {
        if (seasonsp[i] == "Jan-Dec" && n_mas[i] != 1) { # applying moving average
            for (vi in 1:length(datas[[i]])) { 
                if (length(dim(datas[[i]][[vi]])) == 1 && # var has only 1 dim 
                    attributes(datas[[i]][[vi]])$dims == "time") { # and its "time"
                    npy <- unclass(dims[[i]]$timelt)
                    npy <- length(npy$year[which(npy$year == npy$year[1])])
                    message("n_mas[", i, "]: ", n_mas[i], " (n = ", length(datas[[i]][[vi]]), 
                            ", npy = ", npy, " --> ", n_mas[i], "/", npy, " = ", n_mas[i]/npy, 
                            " year running mean)") 
                    datasma[[i]][[vi]] <- filter(datas[[i]][[vi]], filter=rep(1/n_mas[i], t=n_mas[i]))

                    # variable has more than 1 dims
                } else { 
                    message("moving average with dims \"", 
                            paste0(attributes(datas[[i]][[vi]])$dims, collapse="\",\""), 
                            "\". not implemented.")
                    datasma[[i]][[vi]] <- array(NA, dim=dim(datas[[i]][[vi]]))
                }

                attributes(datasma[[i]][[vi]]) <- attributes(datas[[i]][[vi]])
            } # for vi nvars
        } # if n_mas != 1    
    } # for i nsettings
} # if add_smoothed && any(attributes(datas[[i]][[vi]])$dims == "time") && !all(n_mas == 1)
if (!exists("datasma")) {
    add_smoothed <- F
}
# finished applying moving average


# get proper time axis values if necessary
ntime_per_setting <- sapply(sapply(dims, "[", "time"), length)
if (any(ntime_per_setting > 1)) {
    message("\n", "find good time axis labels ...")

    # POSIX will be converted to numeric by plot(), so use these numeric values as limits
    tlim <- range(lapply(sapply(dims, "[", "timelt"), as.numeric)) # seconds by default 
    tlimlt <- as.POSIXlt(tlim, origin="1970-01-01", tz="UTC")
    tlabcex <- 0.8
    tlabsrt <- 0

    # time labels
    tlablt <- as.POSIXlt(pretty(tlimlt, n=10))

    # remove lables which are possibly out of limits due to pretty
    tlab_diff_secs <- as.numeric(diff(range(tlablt)), units="secs") # total time label distance
    if (any(tlablt < tlimlt[1])) {
        # check if the too early autmatic time labels are negligible
        overshoot_diff <- abs(as.numeric(tlablt[tlablt < tlimlt[1]], units="secs")) - abs(tlim[1])
        overshoot_rel <- overshoot_diff/tlab_diff_secs*100
        if (any(overshoot_rel > 1)) { # only change pretty labels if overoot is > 1% of total time label range  
            message("remove automatic labels")
            print(tlablt[which(tlablt < tlimlt[1])[overshoot_rel > 1]])
            tlablt <- tlablt[-which(tlablt < tlimlt[1])[overshoot_rel > 1]]
        }
    }
    if (any(tlablt > tlimlt[2])) {
        # check if the too late automatic time labels are negligible
        overshoot_diff <- abs(as.numeric(tlablt[tlablt > tlimlt[2]], units="secs")) - abs(tlim[2])
        overshoot_rel <- overshoot_diff/tlab_diff_secs*100
        if (any(overshoot_rel > 1)) { # only change pretty labels if overoot is > 1% of total time label range  
            message("remove automatic labels")
            print(tlablt[which(tlablt > tlimlt[2])[overshoot_rel > 1]])
            tlablt <- tlablt[-which(tlablt > tlimlt[2])[overshoot_rel > 1]]
        }
    }
    tatn <- as.numeric(tlablt)

    # modify time axis labels YYYY-MM-DD depending on range covered:
    if (tlab_diff_secs > 365*24*60*60) { # do not show days if range of tlim is above 1 year
        message("time lims is longer than 1 year, modify time labels ...")
        tlablt <- substr(tlablt, 1, 4) # -> YYYY; this destroys POSIX object
    } else { # decrease label size due to long labels
        message("change time label angle ...")
        tlabsrt <- 45
    } 
} # get time axis labels


## start mode specific plots
if (any(mode == c("timmean"))) {

    message("\n", "timmean plot ...")
    for (vi in 1:length(varnames_unique)) {

        varname <- varnames_unique[vi]
        message(varname, " ...")
        z <- vector("list", l=nsettings)
        names(z) <- names_legend
        x <- y <- z
        for (i in 1:nsettings) {
            varind <- which(names(datas[[i]]) == varname)
            if (length(varind) != 1) {
                warning("could not find varname \"", varname, " in setting ", i, ": ", names_short[i], ". skip to next")
            } else {
                tmp <- datas[[i]][[varind]]
                if (length(dim(tmp)) != 2) {
                    warning("length(dim(z[[", i, "]])) = ", length(dim(tmp)), 
                            ". cannot make ", varname, " timmean plot of setting ", i, ": ", names_short[i])
                    next # setting
                }
                if (any(attributes(tmp)$dims != c("lon", "lat"))) {
                    warning("attributes(z[[", i, "]]))$dims = ", paste0(attributes(tmp)$dims, collapse=", "),
                                                                        ". need lon, lat. cannot make ", varname, " timmean plot of setting ", i, ": ", names_short[i])
                    next # setting
                }
                z[[i]] <- tmp
                x[[i]] <- dims[[i]]$lon
                y[[i]] <- dims[[i]]$lat
            } # if varname was found
        } # for i nsettings

        # only make timmean plot if checks above are survived
        if (!all(sapply(z, is.null))) {

            # colorbar values
            source(paste0(homepath, "/functions/image.plot.pre.r"))
            ip <- image.plot.pre(range(z, na.rm=T), verbose=F)

            # determine number of rows and columns
            source(paste0(homepath, "/functions/image.plot.nxm.r"))
            nm <- image.plot.nxm(x=x, y=y, z=z, ip=ip, dry=T)

            plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                               varname, "_", 
                               paste0(names_short, "_", seasonsp, 
                                      "_", fromsp, "-", tosp, "_", areas, collapse="_vs_"), 
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
            image.plot.nxm(x=x, y=y, z=z, ip=ip, verbose=F,
                           xlab="Longitude [°]", ylab="Latitude [°]", 
                           zlab=data_info$label, znames=names_legend)

            dev.off()
            if (p$plot_type == "pdf") {
                embed_fonts(plotname, outfile=plotname)
            }

        } # if not all z are NULL

    } # for vi in unique_varnames 

    # timmean end
} else if (any(mode == c("fldmean", "volint"))) {

    if (!add_unsmoothed && !add_smoothed) {
        warning("both `add_unsmoothed=F` and `add_smoothed=F`. set `add_unsmoothed=T` to show time series.")
        add_unsmoothed <- T # default
    }

    message("\n", mode, " plot versus time ...")
    for (vi in 1:length(varnames_unique)) {

        varname <- varnames_unique[vi]
        message(varname, " ...")
        data <- vector("list", l=nsettings)
        names(data) <- names_short
        if (exists("datasma")) datama <- data
        for (i in 1:nsettings) {
            data[[i]] <- NA # default
            if (exists("datasma")) datama[[i]] <- NA # default
            var_ind <- which(names(datas[[i]]) == varname)
            # check if data is really time series
            if (length(dim(datas[[i]][[var_ind]])) != 1) {
                message("datas[[", i, "]][[", var_ind, 
                        "]] does not have 1 dim. cannot plot ", mode, " ...")
                next # setting
            }
            data[[i]] <- datas[[i]][[var_ind]]
            attributes(data[[i]]) <- c(attributes(data[[i]]), list(name=varname))
            if (exists("datasma")) {
                datama[[i]] <- datasma[[i]][[var_ind]]
                attributes(datama[[i]]) <- c(attributes(datama[[i]]), list(name=varname))
            }
        } # for i nsettings

        if (any(sapply(data, function(x) any(is.na(x))))) {
            if (all(sapply(data, is.na))) {
                next # variable
            }
        }

        # prepare right axis data if necessary
        if (!add_data_right_yaxis_ts) {
            data_right <- list(suffix="") # default
        } else {
            message("\n", "prepare data right yaxis ..")
            if (varname == "temp2") {
                if (F) { # CO2 of hist, 1pct and 4CO2
                    data_right <- list(data=list("co2_hist"=list(x=co2_hist$timelt, y=co2_hist$co2_ppm, 
                                                                 col=cols[1], lty=1, lwd=1),
                                                 "co2_1pct"=list(x=co2_1pct$timelt, y=co2_1pct$co2_ppm, 
                                                                 col=cols[2], lty=1, lwd=1),
                                                 "co2_4co2"=list(x="const", y=co2_4co2$co2_ppm, 
                                                                 col=cols[3], lty=1, lwd=1)),
                                       label=substitute(paste("CO"[2], " [ppm]")),
                                       suffix="_with_CO2")
                } else if (T) { # volcanic aerosols
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
                                                     "tsi_hist"=list(x=tsi_hist_annual$timelt, y=scale(tsi_hist_annual$tsi_hist),
                                                                     text="Total solar irradiance", 
                                                                     col=tsi_hist_annual$col, lty=tsi_hist_annual$lty, 
                                                                     lwd=tsi_hist_annual$lwd, pch=tsi_hist_annual$pch),
                                                     "co2_hist"=list(x=co2_hist$timelt, y=scale(co2_hist$co2_ppm),
                                                                     text=eval(substitute(expression(paste("CO"[2])))),
                                                                     col=co2_hist$col, lty=co2_hist$lty, 
                                                                     lwd=co2_hist$lwd, pch=co2_hist$pch)),
                                           label="Index",
                                           suffix="_with_volcanic_aerosols_and_TSI_and_CO2")
                    }
                }
            } else {
                stop("variable \"", varname, "\" not defined here yet.")
            } # finished variable specific stuff for add_data_right_yaxis_ts
            #ylim_at <- c(range(lapply(data_right, "[", "y"))[1],
            #             seq(300, 500, 

            if (!exists("ylim_right")) {
                message("use automatic data right yaxis limits ...")
                ylim_right <- vector("list", l=length(data_right$data))
                for (i in 1:length(data_right$data)) {
                    if (length(data_right$data[[i]]$x) == 1 && data_right$data[[i]]$x == "const") {
                        ylim_right[[i]] <- range(data_right$data[[i]]$y, na.rm=T)
                    } else {
                        timeinds <- which(data_right$data[[i]]$x >= tlimlt[1] & data_right$data[[i]]$x <= tlimlt[2])
                        if (length(timeinds) == 0) {
                            message("all data of data_right$data[[", i, "]]: ", names(data_right$data)[1], " are out of tlimlt")
                            ylim_right[[i]] <- NA
                        } else {
                            ylim_right[[i]] <- range(data_right$data[[i]]$y[timeinds], na.rm=T)
                        }
                    }
                } # i in data_right
                ylim_right <- range(ylim_right)
            } # if ylim_right does not already exist
            if (!exists("ylim_at")) {
                message("use automatic data right yaxis labels ...")
                yat_right <- pretty(ylim_right, n=10)
            }
            ylab_right <- format(yat_right, trim=T)
        } # if add_data_right_yaxis_ts finished prepare right axis data

        # ylims of model data
        if (add_unsmoothed) {
            message("\n", mode, " versus time min / mean / max ", varname, " data:")
            for (i in 1:nsettings) {
                message(names_short[i], ": ", min(data[[i]], na.rm=T), " / ",
                        mean(data[[i]], na.rm=T), " / ", max(data[[i]], na.rm=T))
            }
            ylim <- range(data, na.rm=T)
        }
        if (exists("datasma")) {
            message("\n", mode, " versus time min / mean / max ", varname, " datama:")
            for (i in 1:nsettings) {
                message(names_short[i], ": ", min(datama[[i]], na.rm=T), " / ",
                        mean(datama[[i]], na.rm=T), " / ", max(datama[[i]], na.rm=T))
            }
            ylimma <- range(datama, na.rm=T)
        }
        if (add_unsmoothed && add_smoothed) {
            ylim <- range(ylim, ylimma)
        } else if (!add_unsmoothed && add_smoothed) {
            ylim <- range(ylimma)
        }

        # ylim of obs, etc.
        if (T && varname == "temp2") {
            message("\n", "add hadcrut4_sat_anom, gistempv4_sat_anom to ylim ...")
            ylim <- range(ylim, 
                          hadcrut4_sat_anom_annual$hadcrut4_sat_anom_lower_uncert,
                          hadcrut4_sat_anom_annual$hadcrut4_sat_anom_upper_uncert,
                          #gistempv4_sat_anom_annual$gistempv4_sat_anom, 
                          na.rm=T)
        } # if add som obs to ylim

        message("\n", "ylim=", appendLF=F)
        dput(ylim)
        yat <- pretty(ylim, n=10)
        ylab <- format(yat, trim=T)

        # plotname
        plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                           varname, "_",
                           paste0(names_short, "_", time_frequencies, "_", seasonsp, 
                                  "_", fromsp, "-", tosp, "_", areas, collapse="_vs_"), 
                           data_right$suffix,
                           ".", p$plot_type)
        dir.create(dirname(plotname), recursive=T, showWarnings=F)
        if (p$plot_type == "png") {
            png(plotname, 
                width=p$ts_width, 
                #height=p$ts_height,
                height=p$ts_width,
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
        if (add_data_right_yaxis_ts) mar[4] <- mar[2] # same as left  

        # open plot
        par(mar=mar)
        plot(dims[[1]]$timelt, data[[1]], t="n",
             xlim=tlim, ylim=ylim, 
             xaxt="n", yaxt="n",
             xlab=NA, ylab=NA)
        if (tlabsrt == 0) { # add horizontal labels
            axis(1, at=tatn, labels=tlablt, cex.axis=tlabcex)
        } else { # add non-horizontal labels with angle
            axis(1, at=tatn, labels=NA)
            # character height in user coordinates
            text(x=tatn, y=par("usr")[3] - strheight("1"), labels=tlablt, 
                 xpd=T, srt=tlabsrt, adj=c(1, 1), cex=tlabcex)
        }
        axis(2, at=yat, labels=ylab, las=2)

        # add title
        if (add_title) {
            title <- paste0(paste(unique(areas), collapse=","), 
                            " ", mode, " ", varname, " ", 
                            paste(unique(seasonsp), collapse=","), " ", 
                            paste(unique(fromsp), collapse=","), "-", 
                            paste(unique(tosp), collapse=","))
            title(title, cex.main=0.75)
        }

        # add variable label
        first_setting_with_varname <- sapply(lapply(data_infos, names), function(x) which(x == varname))[1]
        data_info <- data_infos[[names(first_setting_with_varname)]][[first_setting_with_varname]]
        mtext(side=2, data_info$label, line=4.5, cex=0.9)

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

        # add data
        # unsmoothed before smoothed data
        #if (!is.null(data[[i]])) {
        if (add_unsmoothed) {
            for (i in 1:nsettings) {
                lines(dims[[i]]$timelt, data[[i]], 
                      col=ifelse(add_smoothed, cols_rgb[i], cols[i]), 
                      lty=ltys[i], lwd=lwds[i], pch=pchs[i])
            }
        }

        # smoothed data after unsmoothed data
        if (add_smoothed) {
            for (i in 1:nsettings) {
                lines(dims[[i]]$timelt, datama[[i]], 
                      col=cols[i], lty=ltys[i], lwd=lwds[i], pch=pchs[i])
            }
        }

        # special: add first data point
        if (add_first_data_point) {
            for (i in 1:nsettings) {
                if (i == 1) message("\n", "add first data point")
                points(dims[[i]]$timelt[1], data[[i]][1], 
                       col=cols[i], lty=ltys[i], lwd=lwds[i], 
                       pch=1)
            }
        } # add first data point

        # add linear regression trend if wanted
        if (add_linear_trend) {
            message("not yet")
        }

        # add obs, etc.
        if (T && varname == "temp2") {
            message("\n", "add hadcrut4_sat_anom, gistempv4_sat_anom to plot ...")
            polygon(c(as.POSIXct(hadcrut4_sat_anom_annual$timelt), 
                      rev(as.POSIXct(hadcrut4_sat_anom_annual$timelt))),
                    c(hadcrut4_sat_anom_annual$hadcrut4_sat_anom_lower_uncert,
                      rev(hadcrut4_sat_anom_annual$hadcrut4_sat_anom_upper_uncert)),
                    col=hadcrut4_sat_anom_annual$col_rgb, border=NA)
            lines(hadcrut4_sat_anom_annual$timelt, hadcrut4_sat_anom_annual$hadcrut4_sat_anom,
                  col=hadcrut4_sat_anom_annual$col, lty=hadcrut4_sat_anom_annual$lty,
                  lwd=hadcrut4_sat_anom_annual$lwd)
            #lines(gistempv4_sat_anom_annual$timelt, gistempv4_sat_anom_annual$gistempv4_sat_anom,
            #      col=cols[2], lwd=2, lty=2)
        } # if add some obs

        # add legend if wanted
        if (add_legend) {
            message("\n", "add default stuff to ", mode, " legend ...")
            le <- list()
            le$pos <- "topleft" 
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

        # add box before potential right axis data
        box()

        if (add_data_right_yaxis_ts) {
            message("\n", "add data right yaxis ...")
            par(new=T)
            plot(data_right$data[[1]]$x, data_right$data[[1]]$y, #log="y", 
                 t="n", xlim=tlim, ylim=ylim_right, 
                 xlab=NA, ylab=NA, axes=F)

            # add right axes in same color as the right data if all colors of
            # the right data are the same
            if (length(unique(sapply(data_right$data, "[", "col"))) == 1) {
                right_axis_col <- data_right$data[[1]]$col
            } else {
                right_axis_col <- "black" # default
            }
            axis(4, at=yat_right, labels=ylab_right, las=2, 
                 col=right_axis_col, col.axis=right_axis_col, col.ticks=right_axis_col)
            mtext(side=4, data_right$label, line=4.5, cex=0.9, col=right_axis_col)

            # add right data
            for (i in 1:length(data_right$data)) {
                message(i, "/", length(data_right$data), ": ", names(data_right$data)[i])
                if (length(data_right$data[[i]]$x) == 1 && data_right$data[[i]]$x == "const") {
                    abline(h=data_right$data[[i]]$y, 
                           col=data_right$data[[i]]$col, lty=data_right$data[[i]]$lty,
                           lwd=data_right$data[[i]]$lwd, pch=data_right$data[[i]]$pch)
                } else {
                    lines(data_right$data[[i]]$x, data_right$data[[i]]$y, 
                          col=data_right$data[[i]]$col, lty=data_right$data[[i]]$lty,
                          lwd=data_right$data[[i]]$lwd, pch=data_right$data[[i]]$pch)
                }
            }

            if (add_legend) {
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

        } # if add_data_right_yaxis_ts

        message("\n", "save ", plotname, " ...")
        dev.off()
        if (p$plot_type == "pdf") {
            embed_fonts(plotname, outfile=plotname)
        }

    } # for vi max(nvars_per_setting)
    # finished ts fldmean plots for all vars

    # time series plot versus months
    if (exists("datasmon")) {
        message("\n", mode, " plot versus months ...")

        # compare the same variable across models
        for (vi in 1:length(varnames_unique)) {

            varname <- varnames_unique[vi]
            message(varname, " ...")
            datamon <- vector("list", l=nsettings)
            names(datamon) <- names_short
            for (i in 1:nsettings) {
                datamon[[i]] <- NA # default
                var_ind <- which(names(datasmon[[i]]) == varname)
                datamon[[i]] <- datasmon[[i]][[var_ind]]
                attributes(datamon[[i]]) <- c(attributes(datamon[[i]]), list(name=varname))
            } # for i nsettings

            # prepare right axis data if necessary
            if (!add_data_right_yaxis_ts_mon) {
                data_right_mon <- list(suffix="") # default
            } else {
                stop("not yet")
            } # if add_data_right_yaxis_ts_mon finished prepare right axis data

            # ylims for fldmean versus months plot
            message("\n", mode, " versus months min / mean / max ", varname, " data:")
            for (i in 1:nsettings) {
                message(names_short[i], ": ", min(datamon[[i]], na.rm=T), " / ",
                        mean(datamon[[i]], na.rm=T), " / ", max(datamon[[i]], na.rm=T))
            }
            ylim <- range(ylim)
            message("\n", "ylim=", appendLF=F)
            dput(ylim)
            yat <- pretty(ylim, n=10)
            ylab <- format(yat, trim=T)

            # plotname
            plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                               varname, "_",
                               paste0(names_short, "_", seasonsp, 
                                      "_", fromsp, "-", tosp, "_", areas, collapse="_vs_"), 
                               "_months",
                               data_right_mon$suffix,
                               ".", p$plot_type)
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
            plot(dims[[1]]$monmean_months, datamon[[1]], t="n",
                 xlim=monlim, ylim=ylim, 
                 xaxt="n", yaxt="n",
                 xlab=NA, ylab=NA)
            axis(1, at=monat, labels=monlab, cex.axis=tlabcex)
            axis(2, at=yat, labels=ylab, las=2)

            # add title
            if (add_title) {
                title <- paste0(paste(unique(areas), collapse=","), 
                                " ", mode, " ", varname, " ", 
                                paste(unique(seasonsp), collapse=","), " ", 
                                paste(unique(fromsp), collapse=","), "-", 
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
            # unsmoothed before smoothed data
            for (i in 1:nsettings) {
                lines(dims[[i]]$monmean_months, datamon[[i]], 
                      col=cols[i], lty=ltys[i], lwd=lwds[i], pch=pchs[i])
            }

            # add legend if wanted
            if (add_legend) {
                message("\n", "add default stuff to ", mode, " mon legend ...")
                le <- list()
                le$pos <- "top"
                #le$pos <- "bottom"
                #le$pos <- "topleft" 
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
                message("\n", "add data right yaxis ...")
                par(new=T)
                plot(data_right_mon[[1]]$x, data_right_mon[[1]]$y, #log="y", 
                     t="n", xlim=monlim, ylim=ylim_right, 
                     xlab=NA, ylab=NA, axes=F)
                axis(4, at=yat_right, labels=ylab_right, las=2)
                mtext(side=4, data_right_mon_label, line=3.4, cex=0.9)

                # add right data
                for (i in 1:length(data_right_mon)) {
                    if (length(data_right_mon[[i]]$x) == 1 && data_right_mon[[i]]$x == "const") {
                        abline(h=data_right_mon[[i]]$y, 
                               col=data_right_mon[[i]]$col, lty=data_right_mon$lty,
                               lwd=data_right_mon[[i]]$lwd)
                    } else {
                        lines(data_right_mon[[i]]$x, data_right_mon[[i]]$y, 
                              col=data_right_mon[[i]]$col, lty=data_right_mon$lty,
                              lwd=data_right_mon[[i]]$lwd)
                    }
                }

                if (add_legend) {
                    stop("asd")
                    message("\n", "add default stuff to ", mode, " right_data legend ...")
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
            message("\n", "save ", plotname, " ...")
            dev.off()
            if (p$plot_type == "pdf") {
                embed_fonts(plotname, outfile=plotname)
            }

        } # for vi max(nvars_per_setting)

    } # if exists("datasmon")
    # finished ts versus months plot

    if (plot_scatter_var1_vs_var2) {

        # set here
        varnamex <- "temp2"
        varnamey <- "toa_imbalance"

        if (exists(paste0(varnamex, "_datas")) 
            && exists(paste0(varnamey, "_datas"))) {

            varname <- paste0(varnamex, "_vs_", varnamey)
            eval(parse(text=paste0("varx <- ", varnamex, "_datas")))
            eval(parse(text=paste0("vary <- ", varnamey, "_datas")))
            eval(parse(text=paste0("varx_infos <- ", varnamex, "_infos")))
            eval(parse(text=paste0("vary_infos <- ", varnamey, "_infos")))

            if (T) {
                message("\n", "substract last PI value from experiments ...")
                for (i in 2:nsettings) {
                    varx[[i]] <- varx[[i]] - rep(varx[[1]][length(varx[[1]])], t=length(length(varx[[1]])))
                    varx_infos[[i]]$label <- "2m temperature increase [K]"
                }
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
                               paste0(names_short, "_", time_frequencies, "_",seasonsp, 
                                      "_", fromsp, "-", tosp, "_", areas, collapse="_vs_"), 
                               ".", p$plot_type)
            message("\n", "`plot_scatter_var1_vs_var2` = T --> plot ", plotname, " ...")
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
            axis(1, at=xat, labels=xlab, cex.axis=0.85)
            axis(2, at=yat, labels=ylab, las=1, cex.axis=0.85)

            # add title
            if (add_title) {
                title <- paste0(paste(unique(areas), collapse=","), 
                                " ", mode, " ", varname, " ", 
                                paste(unique(seasonsp), collapse=","), " ", 
                                paste(unique(fromsp), collapse=","), "-", 
                                paste(unique(tosp), collapse=","))
                title(title, cex.main=0.5)
            }

            # add variable label
            mtext(side=1, varx_infos[[1]]$label, line=3.4, cex=0.9)
            mtext(side=2, vary_infos[[1]]$label, line=3.4, cex=0.9)

            # add zero lines
            if (add_zeroline) {
                abline(h=0, col="gray", lwd=0.5)
                abline(v=0, col="gray", lwd=0.5)
            }

            # add data to scatter plot 
            for (i in 1:nsettings) {
                if (names_short[i] == "piControl") {
                    message("special: plot only time mean")
                    points(mean(varx[[i]]), mean(vary[[i]]), 
                           #col=cols_rgb[i], 
                           col=cols[i],
                           pch=scatterpchs[i], cex=scattercexs[i])
                } else {
                    if (T) { # special: plot years as symbol
                        message("special: use year as symbols")
                        #years_of_setting_to_show <- c(1:10, seq(25, 250, b=25))
                        years_of_setting_to_show <- c(1:10, seq(20, 100, b=10), seq(125, 250, b=25))
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
                             cex=0.5)
                    } else {
                        points(varx[[i]], vary[[i]], 
                               #col=cols_rgb[i], 
                               col=cols[i],
                               pch=scatterpchs[i], cex=scattercexs[i])
                    }
                } # special plots depending on setting
            } # finished add data to scatter plot 

            # add linear trend
            if (add_linear_trend) {
                message("\n", "add linear trend ...")
                lms_lin <- vector("list", l=nsettings)
                for (i in 1:nsettings) {
                    if (any(i == c(4))) {
                        message("setting ", i, "/", nsettings, ": ", names_short[i])
                        lms_lin[[i]] <- lm(vary[[i]] ~ varx[[i]])
                        lm_summary <- summary(lms_lin[[i]])
                        print(lm_summary)
                        # plot regression line within data limits only
                        if (F) {
                            lines(varx[[i]], lms_lin[[i]]$fitted.values, 
                                  col=cols[i], lwd=lwds[i], lty=ltys[i])
                            # or plot line through whole plot with regression coefficients
                        } else if (T) {
                            abline(a=lms_lin[[i]]$coefficients[1], b=lms_lin[[i]]$coefficients[2],
                                   col=cols[i], lwd=lwds[i], lty=ltys[i])
                        }
                        # linear regression results
                        intercept <- as.vector(lm_summary$coefficients[1,1])
                        intercept_error <- as.vector(lm_summary$coefficients[1,2])
                        intercept_pval <- paste0("=", lm_summary$coefficients[1,4])
                        slope <- abs(as.vector(lm_summary$coefficients[2,1]))
                        slope_error <- as.vector(lm_summary$coefficients[2,2])
                        slope_pval <- lm_summary$coefficients[2,4]
                        if (slope_pval < 1e-15) { 
                            slope_pval <- "<1e-15"
                        } else {
                            slope_pval <- paste0("=", format(slope_pval, trim=T))
                        }
                        if (varnamex == "temp2") {
                            # gregory et al. 2004:
                            alpha <- slope
                            alpha_error <- slope_error
                            Teq <- sort(c((intercept - intercept_error)/(alpha - alpha_error),
                                          (intercept + intercept_error)/(alpha + alpha_error)))
                            message("Teq for setting ", names_short[i], " = (", round(min(Teq), 2), ",", 
                                    round(max(Teq), 2), ") K (gregory et al. 2004)\n",
                                    " --> Teq/2 = ", round(min(Teq)/2, 2), ",", round(max(Teq)/2, 2), " K")
                            Forcing <- alpha*as.vector(varx[[i]]) # = alpha*dT
                            effective_climate_response <- Forcing/alpha # = F/alpha
                        }
                        # update legend
                        first_part <- names_legend[i]
                        last_part <- eval(substitute(expression(paste("(", alpha, "=", slope, ", p", p, ", r=", r, ")")),
                                                     list(slope=round(slope, 2), p=slope_pval, 
                                                          r=round(sqrt(lm_summary$r.squared), 2))))
                        new <- bquote(.(do.call(substitute, as.list(first_part))) ~ 
                                      .(do.call(substitute, as.list(last_part))))
                        names_legend[i] <- eval(substitute(expression(new), list(new=new)))
                        #names_legend[i] <- eval(substitute(expression(paste(p1, p2)), list(p1=new[2], p2=new[3])))
                        #names_legend[i] <- eval(parse(text=paste0(dput(names_legend[i]), " (test)")))
                        #names_legend[i] <- paste0(dput(names_legend[i]), " (test)")
                    }
                }
            }

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
                le$pos <- "topright"
                le$ncol <- 1
                #le$ncol <- 2 
                le$text <- names_legend
                #le$col <- cols_rgb
                le$col <- cols
                le$lty <- NA
                le$lwds <- NA
                le$pchs <- scatterpchs
                le$cex <- 1
                le$cex <- 0.66
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
            dev.off()

            ## scatter plot for each setting colored by time or season
            if (T) {

                for (i in 1:nsettings) {

                    if (dims[[i]]$time_frequency == "monthly") {
                        xlim <- range(varx[[i]], na.rm=T)
                        ylim <- range(vary[[i]], na.rm=T)
                        xat <- pretty(xlim, n=10)
                        xlab <- format(xat, trim=T)
                        yat <- pretty(ylim, n=10)
                        ylab <- format(yat, trim=T)

                        plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                                           varname, "_", 
                                           paste0(names_short[i], "_", time_frequencies, "_", seasonsp[i], 
                                                  "_", fromsp[i], "-", tosp[i], "_", areas[i], collapse="_vs_"), 
                                           ".", p$plot_type)
                        message("\n", "plot ", names_short[i], " ", plotname, " ...")
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
                                            paste(unique(fromsp[i]), collapse=","), "-", 
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

                        # add data to scatter plot colored by time
                        if (F) { # by time
                            timecols <- colorRampPalette(c("blue", "red"))(length(varx[[i]]))
                        } else if (T) { # by season
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
                        }
                        timecols_rgb <- rgb(t(col2rgb(timecols)/255), alpha=alpha)
                        points(varx[[i]], vary[[i]], 
                               col=timecols,
                               #col=timecols_rgb,
                               #pch=scatterpchs_vstime[i], 
                               pch=season_pchs,
                               cex=scattercexs[i])

                        # add legend if wanted
                        if (add_legend) {
                            message("\n", "add default stuff to plot_scatter_var1_vs_var2 legend ...")
                            le <- list()
                            le$pos <- "topright"
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
                        dev.off()
                    } # if dims[[i]]$time_frequency == "monthly"

                } # for i nsettings

            } # scatter plot for each setting colored by time

        } else { # if exists("varx") && exists("vary")
            if (!exists(paste0(varnamex, "_datas"))) {
                message("\n", "`plot_scatter_var1_vs_var2` = T but could not find ",
                        "`", varnamex, "_datas`")
            }
            if (!exists(paste0(varnamey, "_datas"))) {
                message("\n", "`plot_scatter_var1_vs_var2` = T but could not find ",
                        "`", varnamey, "_datas`")
            }
        } # if exists("varx") && exists("vary")

    } # if plot_scatter_var1_vs_var2

} # plots of which mode

message("\n", "finish", "\n")

