## R

rm(list=ls()); graphics.off()
fctbackup <- `[`; `[` <- function(...) { fctbackup(..., drop=F) }

# user input
fnml <- "namelist.plot.r"
message("\n", "Read ", fnml, " ...")
source(fnml)

# load special data
if (machine_tag == "mistral") {
    co2_hist_ncin <- nc_open("/pool/data//ECHAM6/input/r0007/greenhouse_historical.nc") 
    message("\n", "read hist CO2 from ", co2_hist_ncin$filename, " ...")
    time <- co2_hist_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*365.25*86400, origin="0000-01-01", tz="UTC", format="%Y")
    co2_hist <- list(co2_ppm=ncvar_get(co2_hist_ncin, "CO2"), time=time, timelt=timelt)
    co2_1pct_ncin <- nc_open("/pool/data//ECHAM6/input/r0008/greenhouse_1pctCO2.nc")
    message("\n", "read 1pct CO2 from ", co2_1pct_ncin$filename, " ...")
    time <- co2_1pct_ncin$dim$time$vals
    timelt <- as.POSIXlt(time*365.25*86400, origin="0000-01-01", tz="UTC", format="%Y")
    co2_1pct <- list(co2_ppm=ncvar_get(co2_1pct_ncin, "CO2"), time=time, timelt=timelt)
}
co2_4co2 <- list(co2_ppm=1137.2679)
message("\n", "set 4CO2 to ", co2_4co2$co2_ppm, " ppm") 

# check user input and defaults
nsettings <- length(postpaths)
if (!exists("codes")) codes <- rep("", t=nsettings)
if (!exists("levs")) levs <- rep("", t=nsettings)
if (!exists("ltys")) ltys <- rep(1, t=nsettings)
if (!exists("lwds")) lwds <- rep(1, t=nsettings)
if (!exists("pchs")) pchs <- rep(NA, t=nsettings)
if (!exists("cols")) {
    cols <- 1:nsettings
    # dont use default green as 3rd color
    if (length(cols) >= 3) {
        cols[3] <- "blue"
    }
    if (length(cols) >= 4) {
        cols[4] <- "darkgreen"
    }
}
if (!exists("n_mas")) n_mas <- rep(1, t=nsettings) # 1 = no moving average effect
if (!exists("fromsp")) fromsp <- fromsf
if (!exists("tosp")) tosp <- tosf
if (!exists("froms_shift")) froms_shift <- rep(NA, t=nsettings)
if (!exists("seasonsf")) seasonsf <- rep("Jan-Dec", t=nsettings)
if (!exists("seasonsp")) seasonsp <- seasonsf
if (!exists("areas")) areas <- rep("global", t=nsettings)
codesf <- codes
codesf[codes != ""] <- paste0("_selcode_", codesf[codes != ""])
levsf <- levs
levsf[levs != ""] <- paste0("_", levsf[levs != ""], "m")
tos_shift <- froms_shift

base <- 10
power <- 0 # default: 0 --> 10^0 = 1e0 = 1 --> nothing happens
alpha <- 0.15 # transparent: 0,1 (1 fully transparent)
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
                    "_selname_", varnames_in[i], # codes
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
        
        # find temporal subset based on given fromsp and tosp
        fromsplt <- as.POSIXlt(paste0(fromsp[i], "-01-01 00:00:00"), tz="UTC")
        tosplt <- as.POSIXlt(paste0(tosp[i], "-12-31 23:59:59"), tz="UTC")
        time_inds <- which(timein_lt >= fromsplt & timein_lt <= tosplt)
        # take subset only if necessary
        if (length(time_inds) > 0 && length(time_inds) != length(timein_lt)) { 
            message("\n", "found ", length(time_inds), " temporal subset time_inds based on fromsp[", 
                    i, "]=", fromsp[i], " to tosp[", i, "]=", tosp[i], " ...")
            
            # subset seasons from data if wanted (=seasonsp)
            # check which seasonsf and seasonp differ
            if (seasonsp[i] != seasonsf[i]) {
                stop("todo")
            }

            timein_lt <- timein_lt[time_inds]
            message("range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))
        
        } # found temporal subset time_inds
        
        # shift times due to e.g. senseless spinup years
        # as.POSIXlt's 'year' starts at 1900
        if (!is.na(froms_shift[i])) {
            stop("update")
            # from year in  = min(timeslt[[i]]$year) + 1900
            message("range(timeslt[[", i, "]]) = ", appendLF=F)
            print(range(timeslt[[i]]))
            shift_by <- -(min(timeslt[[i]]$year) + 1900 - froms_shift[i]) 
            message("shift fromsf[", i, "]=", fromsf[i], 
                    " to froms_shift[", i, "]=", froms_shift[i], " by ", shift_by, " years ...")
            timeslt[[i]]$year <- timeslt[[i]]$year + shift_by
            tos_shift[i] <- max(timeslt[[i]]$year) + 1900
        } # if !is.na(froms_shift[i])
        
        # update time dim
        dims[[i]]$time_inds <- time_inds
        dims[[i]]$timelt <- timein_lt

        # POSIXlt as numeric
        #dims[[i]]$timen <- lapply(dims[[i]]$timelt, as.numeric)

    } # if any of file dims is "time"
    
    # get vars of file
    message("\n", "get variables ...")
    vars_per_file <- names(ncin$var)
    vars <- vector("list", l=ncin$nvars)
    names(vars) <- vars_per_file
    var_infos <- vars
    for (vi in 1:length(vars)) {
        message(vi, "/", length(vars), ": ", vars_per_file[vi])
        vars[[vi]] <- ncvar_get(ncin, names(vars)[vi], collapse_degen=squeeze_data) 
        
        # get infos of variable
        var_infos[[vi]]$name <- ncin$var[[vars_per_file[vi]]]$name
        var_infos[[vi]]$longname <- ncin$var[[vars_per_file[vi]]]$longname
        var_infos[[vi]]$units <- ncin$var[[vars_per_file[vi]]]$units
        var_infos[[vi]]$missval <- ncin$var[[vars_per_file[vi]]]$missval
        var_infos[[vi]]$hasAddOffset <- ncin$var[[vars_per_file[vi]]]$hasAddOffset
        var_infos[[vi]]$hasScaleFact <- ncin$var[[vars_per_file[vi]]]$hasScaleFact

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

# save data before applying offset, multiplication factors, running mean, etc. for later
message("\n", "save original data for later ...")
varnames_unique <- apply(unlist(sapply(datas, names)), 1, unique)
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

if (F) { # for testing
    message("special")
    datas[[1]][[2]] <- datas[[1]][[1]] + 10
    names(datas[[1]])[2] <- names(datas[[1]])[1]
}

# set variable specific things
message("\n", "set variable specific things ...")
for (i in 1:nsettings) {
    for (vi in 1:length(datas[[i]])) {
        # add specific things
        if (names(datas[[i]])[vi] == "temp2") {
            data_infos[[i]][[vi]]$label <- "2m Temperature [°C]"
            data_infos[[i]][[vi]]$offset$operator <- "-"
            data_infos[[i]][[vi]]$offset$value <- 273.15
        } # finished define variable specific things
    }
}
# finished setting variable specific things

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

# calculate monthly means
if (any(seasonsp == "Jan-Dec") && any(attributes(datas[[i]][[vi]])$dims == "time")) {
    message("\n", "calc monthly means ...")
    datasmon <- datas
    for (i in 1:nsettings) {
        if (seasonsp[i] == "Jan-Dec") {
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
                    message("moving average with dims \"", 
                            paste0(attributes(datas[[i]][[vi]])$dims, collapse="\",\""), 
                            "\". not implemented.")
                    datasmon[[i]][[vi]] <- NA
                }

                attributes(datasmon[[i]][[vi]]) <- list(dim=length(tmp), dims="months")
                dims[[i]]$months <- months_unique
            } # for vi nvars
        } # if n_mas != 1    
    } # for i nsettings
    monlim <- range(sapply(dims, "[", "months"))
    monat <- monlim[1]:monlim[2]
    monlab <- substr(month.abb[monat], 1, 1) # Jan -> J
} # if any(seasonsp == "Jan-Dec") && any(attributes(datas[[i]][[vi]])$dims == "time")

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
                    datasma[[i]][[vi]] <- NA
                }

                attributes(datasma[[i]][[vi]]) <- attributes(datas[[i]][[vi]])
            } # for vi nvars
        } # if n_mas != 1    
    } # for i nsettings
} # if add_smoothed && any(attributes(datas[[i]][[vi]])$dims == "time") && !all(n_mas == 1)

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

## start mode specific things
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
} else if (any(mode == c("fldmean"))) {

    if (!add_unsmoothed && !add_smoothed) {
        warning("both `add_unsmoothed=F` and `add_smoothed=F`. set `add_unsmoothed=T` to show time series.")
        add_unsmoothed <- T # default
    }

    message("\n", "fldmean plot versus time ...")
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
            data[[i]] <- datas[[i]][[var_ind]]
            attributes(data[[i]]) <- c(attributes(data[[i]]), list(name=varname))
            if (exists("datasma")) {
                datama[[i]] <- datasma[[i]][[var_ind]]
                attributes(datama[[i]]) <- c(attributes(datama[[i]]), list(name=varname))
            }
        } # for i nsettings

        # prepare right axis data if necessary
        if (!add_data_right_yaxis_ts) {
            data_right_yaxis_suffix_ts <- "" # default
        } else {
            message("\n", "prepare data right yaxis ..")
            if (varname == "temp2") {
                # hist, 1pct and 4CO2
                data_right_label <- substitute(paste("CO"[2], " [ppm]"))
                data_right <- list("co2_hist"=list(x=co2_hist$timelt, y=co2_hist$co2_ppm), 
                               "co2_1pct"=list(x=co2_1pct$timelt, y=co2_1pct$co2_ppm), 
                               "co2_4co2"=list(x="const", y=co2_4co2$co2_ppm))
            data_right_yaxis_suffix_ts <- "_with_CO2"
            #ylim_at <- c(range(lapply(data_right, "[", "y"))[1],
            #             seq(300, 500, 
        } # finished variable specific stuff for add_data_right_yaxis_ts
        
        if (!exists("ylim_right")) {
            message("use automatic data right yaxis labels ...")
            ylim_right <- vector("list", l=length(data_right))
            for (i in 1:length(data_right)) {
                if (length(data_right[[i]]$x) == 1 && data_right[[i]]$x == "const") {
                    ylim_right[[i]] <- range(data_right[[i]]$y, na.rm=T)
                } else {
                    timeinds <- which(data_right[[i]]$x >= tlimlt[1] & data_right[[i]]$x <= tlimlt[2])
                    if (length(timeinds) == 0) {
                        message("all data of data_right[[", i, "]]: ", names(data_right)[1], " are out of tlimlt")
                        ylim_right[[i]] <- NA
                    } else {
                        ylim_right[[i]] <- range(data_right[[i]]$y[timeinds], na.rm=T)
                    }
                }
            } # i in data_right
            ylim_right <- range(ylim_right)
        } # if ylim_right does not already exist
        if (!exists("ylim_at")) {
            message("use automatic data right yaxis labels ...")
            yat_right <- pretty(ylim_right, n=10)
        }
        ylab_right <- format(yat_right)
    } # if add_data_right_yaxis_ts finished prepare right axis data

    # ylims for fldmean plot
    if (add_unsmoothed) {
        message("\n", "fldmean versus time min / mean / max ", varname, " data:")
        for (i in 1:nsettings) {
            message(names_short[i], ": ", min(data[[i]], na.rm=T), " / ",
                    mean(data[[i]], na.rm=T), " / ", max(data[[i]], na.rm=T))
        }
        ylim <- range(data, na.rm=T)
    }
    if (exists("datasma")) {
        message("\n", "fldmean versus time min / mean / max ", varname, " datama:")
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
    message("\n", "ylim=", appendLF=F)
    dput(ylim)
    yat <- pretty(ylim, n=10)
    ylab <- format(yat)

    # plotname
    plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                       varname, "_",
                       paste0(names_short, "_", seasonsp, 
                              "_", fromsp, "-", tosp, "_", areas, collapse="_vs_"), 
                       data_right_yaxis_suffix_ts,
                       ".", p$plot_type)
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
        message("\n", "add title ...")
        if (length(unique(fromsp) == 1 && length(unique(tosp)) == 1) &&
            length(unique(seasonsp) == 1) && length(unique(areas) == 1)) {
            title <- paste0(unique(areas), " ", mode, " ", varname, " ", 
                            unique(seasonsp), " ", unique(fromsp), "-", unique(tosp))
        } else {
            title <- "define title"
        }
        title(title, cex.main=0.75)
    }

    # add variable label
    data_info <- data_infos[[which(sapply(data_infos, names) == varname)[1]]]
    mtext(side=2, data_info[[1]]$label, line=3.4, cex=0.9)

    # add grid
    if (add_xgrid) {
        message("\n", "add xgrid ...")
        abline(v=tatn, col="gray", lwd=0.5)
    }
    if (add_ygrid) {
        message("\n", "add ygrid ...")
        abline(h=yat, col="gray", lwd=0.5)
    }

    ## add data
    # unsmoothed before smoothed data
    if (!is.null(data[[i]])) {
        for (i in 1:nsettings) {
            lines(dims[[i]]$timelt, data[[i]], 
                  col=ifelse(!is.null(datama[[i]]), cols_rgb[i], cols[i]), 
                  lty=ltys[i], lwd=lwds[i], pch=pchs[i])
        }
    }
    
    # smoothed data after unsmoothed data
    if (!is.null(datama[[i]])) {
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
    if (add_trend) {
        stop("not yet")
    }

    # add legend if wanted
    if (add_legend) {
        message("\n", "add default stuff to fldmean legend ...")
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
        if (F) {
            message("\n", "add non default stuff to fldmean legend ...")

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

    if (add_data_right_yaxis_ts) {
        message("\n", "add data right yaxis ...")
        par(new=T)
        plot(data_right[[1]]$x, data_right[[1]]$y, #log="y", 
             t="n", xlim=tlim, ylim=ylim_right, 
             xlab=NA, ylab=NA, axes=F)
        axis(4, at=yat_right, labels=ylab_right, las=2)
        mtext(side=4, data_right_label, line=3.4, cex=0.9)

        # add right data
        for (i in 1:length(data_right)) {
            if (length(data_right[[i]]$x) == 1 && data_right[[i]]$x == "const") {
                abline(h=data_right[[i]]$y, col=cols[i], lty=2)
            } else {
                lines(data_right[[i]]$x, data_right[[i]]$y, col=cols[i], lty=2)
            }
        }
    } # if add_data_right_yaxis_ts

    box()
    message("\n", "save ", plotname, " ...")
    dev.off()
    if (p$plot_type == "pdf") {
        embed_fonts(plotname, outfile=plotname)
    }

} # for vi max(nvars_per_setting)

# time series plot versus months
if (exists("datasmon")) {
    message("\n", "fldmean plot versus months ...")

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
            data_right_yaxis_suffix_ts_mon <- "" # default
        } else {
            message("\n", "prepare data right yaxis ..")
        } # if add_data_right_yaxis_ts_mon finished prepare right axis data

        # ylims for fldmean versus months plot
        message("\n", "fldmean versus months min / mean / max ", varname, " data:")
        for (i in 1:nsettings) {
            message(names_short[i], ": ", min(datamon[[i]], na.rm=T), " / ",
                    mean(datamon[[i]], na.rm=T), " / ", max(datamon[[i]], na.rm=T))
        }
        ylim <- range(ylim)
        message("\n", "ylim=", appendLF=F)
        dput(ylim)
        yat <- pretty(ylim, n=10)
        ylab <- format(yat)

        # plotname
        plotname <- paste0(plotpath, "/", mode, "/", varname, "/",
                           varname, "_",
                           paste0(names_short, "_", seasonsp, 
                                  "_", fromsp, "-", tosp, "_", areas, collapse="_vs_"), 
                           "_months",
                           data_right_yaxis_suffix_ts_mon,
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
        plot(dims[[1]]$months, datamon[[1]], t="n",
             xlim=monlim, ylim=ylim, 
             xaxt="n", yaxt="n",
             xlab=NA, ylab=NA)
        axis(1, at=monat, labels=monlab, cex.axis=tlabcex)
        axis(2, at=yat, labels=ylab, las=2)

        # add title
        if (add_title) {
            message("\n", "add title ...")
            if (length(unique(fromsp) == 1 && length(unique(tosp)) == 1) &&
                length(unique(areas) == 1)) {
                title <- paste0(unique(areas), " ", mode, " ", varname, " ", 
                                unique(fromsp), "-", unique(tosp))
            } else {
                title <- "define title"
            }
            title(title, cex.main=0.75)
        }

        # add variable label
        data_info <- data_infos[[which(sapply(data_infos, names) == varname)[1]]]
        mtext(side=2, data_info[[1]]$label, line=3.4, cex=0.9)

        # add grid
        if (add_xgrid) {
            message("\n", "add xgrid ...")
            abline(v=tatn, col="gray", lwd=0.5)
        }
        if (add_ygrid) {
            message("\n", "add ygrid ...")
            abline(h=yat, col="gray", lwd=0.5)
        }

        ## add data
        # unsmoothed before smoothed data
        for (i in 1:nsettings) {
            lines(dims[[i]]$months, datamon[[i]], 
                  col=cols[i], lty=ltys[i], lwd=lwds[i], pch=pchs[i])
        }
        
        # add legend if wanted
        if (add_legend) {
            message("\n", "add default stuff to fldmean legend ...")
            le <- list()
            le$pos <- "bottom"
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
                message("\n", "add non default stuff to fldmean legend ...")

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
                    abline(h=data_right_mon[[i]]$y, col=cols[i], lty=2)
                } else {
                    lines(data_right_mon[[i]]$x, data_right_mon[[i]]$y, col=cols[i], lty=2)
                }
            }
        } # if add_data_right_yaxis_ts_mon

        box()
        message("\n", "save ", plotname, " ...")
        dev.off()
        if (p$plot_type == "pdf") {
            embed_fonts(plotname, outfile=plotname)
        }

    } # for vi max(nvars_per_setting)

} # if exists("datasmon")

# fldmean end
} # which mode

message("\n", "finish", "\n")
