## R

rm(list=ls()); graphics.off()

# Host options
homepath <- "~/scripts/r"
machine <- system("hostname -f", intern=T)
message("Run on ", machine, ":")
if (regexpr("ollie", machine) != -1 ||
    regexpr("prod-", machine) != -1 ||
    regexpr("fat-", machine) != -1) {
    machine_tag <- "ollie"
    plotpath <- "/work/ollie/cdanek/plots"
} else if (regexpr("hpc.dkrz", machine) != -1) {
    machine <- substr(machine, 1, regexpr(".hpc.dkrz", machine) - 1)
    machine_tag <- "mistral"
    plotpath <- "/work/ab0246/a270073/plots"
} else {
    stop("machine ", machine, " is unknown. stop")
}
message("homepath = ", homepath)
message("plotpath = ", plotpath)
# =====================================

fctbackup <- `[`; `[` <- function(...) { fctbackup(..., drop=F) }

# =====================================
# 3 settings
if (T) {
    postpaths <- rep("/work/ab0246/a270073/post/echam6", t=3)
    prefixs <- c("hist_echam6_echammon_dynveg",
                 "1percCO2_echam6_echammon_dynveg",
                 "4CO2_echam6_echammon_dynveg")
    models <- rep("echam6", t=3)
    names_short <- c("hist", "1pctCO2", "abrupt-4xCO2") 
    names_legend <- c("historical", "1pct", "abrupt-4×CO2")
    fromsf <- rep(1850, t=3)
    #fromsf <- rep(1985, t=3)
    #tosf <- rep(2014, t=3)
    tosf <- c(2014, 2099, 2099)
    fromsp <- fromsf
    tosp <- tosf
    seasonsf <- rep("Jan-Dec", t=3)
    seasonsp <- seasonsf
    areas <- rep("global", t=3)
    n_mas <- c(60, 60, 60)
}

# ==================================================

# which variable
if (exists("varnames_in")) varname_out <- unique(varnames_in) # default
varname_out <- "temp2"

# options across settings
mode <- "fldmean" # "timmean" "fldmean"
add_title <- F
add_land <- T
reorder_lon_from_0360_to_180180 <- T
add_grid <- F
add_legend <- T
plot_type <- "png" # "png" "pdf"
p <- setDefaultPlotOptions(plot_type=plot_type)

# time series options
# woa13 seasons: "JFM" "AMJ" "JAS" "OND"
# other seasons: "Jan-Dec" "DJF" "MAM" "JJA" "SON" "JJ"
# months: "Feb" "Jul" "Jan"  "Jul"
add_unsmoothed <- T
add_sd <- F
add_trend <- F
scale_ts <- F
plot_redfit <- F
plot_var_vs_months <- T

# map options
proj <- "rectangular" #"rectangular"

# defaults
nsettings <- length(postpaths)
if (!exists("varnames_in")) varnames_in <- rep(varname_out, t=nsettings)
if (!exists("codes")) codes <- rep("", t=nsettings)
if (!exists("levs")) levs <- rep("", t=nsettings)
if (!exists("cols")) cols <- 1:nsettings
if (!exists("ltys")) ltys <- rep(1, t=nsettings)
if (!exists("lwds")) lwds <- rep(1, t=nsettings)
if (!exists("pchs")) pchs <- rep(NA, t=nsettings)
if (!exists("n_mas")) n_mas <- rep(1, t=nsettings) # 1 = no moving average effect
if (!exists("fromsp")) fromsp <- fromsf
if (!exists("tosp")) tosp <- tosf
if (!exists("froms_shift")) froms_shift <- rep(NA, t=nsettings)
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
dims <- times <- times2 <- times3 <- timesp <- timeslt <- lons <- lats <- datas

# read data
message("\n", "Read data ...")
for (i in 1:nsettings) {
  
    message("\n", "*********************************************")
    message("setting ", i, "/", nsettings, ": ", names_short[i], " ...")
    inpath <- paste0(postpaths[i], "/", mode, "/", varnames_in[i])
    fname <- paste0(prefixs[i], "_", mode, 
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
        message("\n", "detected \"time\" dim -> make POSIXlt from timein_units")
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
        message("range(timein_lt[[", i, "]]) = ", appendLF=F)
        print(range(timein_lt))
        
        # find temporal subset based on given fromsp and tosp
        fromsplt <- as.POSIXlt(paste0(fromsp[i], "-01-01 00:00:00"), tz="UTC")
        tosplt <- as.POSIXlt(paste0(tosp[i], "-12-31 23:59:59"), tz="UTC")
        inds <- which(timein_lt >= fromsplt & timein_lt <= tosplt)
        # take subset only if necessary
        if (length(inds) > 0 && length(inds) != length(timein_lt)) { 
            message("\n", "found ", lebngth(inds), " temporal subset inds based on fromsp[", 
                    i, "]=", fromsp[i], " to tosp[", i, "]=", tosp[i], " ...")
            
            # subset seasons from data if wanted (=seasonsp)
            # check which seasonsf and seasonp differ
            if (seasonsp[i] != seasonsf[i]) {
                stop("todo")
            }

            timein_lt <- timein_lt[inds]
            print(range(timeslt[[i]]))
        
        } # found temporal subset inds
        message("new range(timein_lt[[", i, "]]) = ", appendLF=F)
        print(range(timein_lt))
        
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
        dims[[i]]$time_inds <- inds
        dims[[i]]$timelt <- timein_lt

        # POSIXlt as numeric
        #dims[[i]]$timen <- lapply(dims[[i]]$timelt, as.numeric)

    } # if any of file dims is "time"
    
    # flip latitudes if necessary (needs to be increasing)
    if (any(names(dims[[i]]) == "lat")) {
        if (any(diff(dims[[i]]$lat) < 0)) {
            message("\n", "detected lat dimension and lats are decreasing -> flip latitudes ...") 
            dims[[i]]$lat_orig <- dims[[i]]$lat
            dims[[i]]$lat <- rev(dims[[i]]$lat)
        }
    }

    # get vars of file
    message("\n", "get variables ...")
    vars_per_file <- names(ncin$var)
    vars <- vector("list", l=ncin$nvars)
    names(vars) <- vars_per_file
    for (vi in 1:length(vars)) {
        message("  ", vars_per_file[vi])
        # read data: squeeze() is automatically applied
        vars[[vi]] <- ncvar_get(ncin, names(vars)[vi])
        # get dims of data
        stop("asd")
        dimids <- ncin$var[[vars_per_file[vi]]]$dimids
        dimids <- dimids + 1 # nc dim ids start counting from zero
        attributes(vars[[vi]]) <- list(dim=dim(vars[[vi]]), dims=dims_per_file[dimids])
        #cmd <- paste0("tmp <- list(", paste0(dims_per_file[dimids], "=ncin$dim[[", dimids, "]]$vals", collapse=", "), ")")
    }
    datas[[i]] <- vars
    rm(vars)
               
    # cut temporal subset
    dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")
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
                vars_with_londim <- which(lapply(dims_per_setting, function(x) grep("lon", x)) == 1)
                if (length(vars_with_londim) > 0) {
                    for (vi in 1:length(vars_with_londim)) {
                        var_with_londim_ind <- vars_with_londim[vi]
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
    
} # for i nsettings
message("\n", "*********************************************")

# save data before applying offset, multiplication factors, running mean, etc. for later
cmd <- paste0(varname_out, "_datas <- datas")
message("\n", "for later save ", cmd, " ...")
eval(parse(text=cmd))

if (add_unsmoothed && !all(n_mas == 1)) {

    # apply ma
    message("\n", "apply moving averages ...")
    datasma <- datas
    for (i in 1:nsettings) {
        for (vi in 1:length(datasma[[i]])) { # for all vars
            message("n_mas[", i, "]: ", n_mas[i], 
                    " (n = ", length(datas[[i]][[vi]]), 
                    " --> ", length(datas[[i]][[vi]]), "/", n_mas[i], " = ", 
                    length(datas[[i]][[vi]])/n_mas[i], ")")
            datasma[[i]][[vi]] <- filter(datas[[i]][[vi]], filter=rep(1/n_mas[i], t=n_mas[i]))
        }
    }
    stop("asd")

    tlimn <- range(timesn)
    tlimlt <- as.POSIXlt(tlimn, origin="1970-01-01", tz="UTC")
    tlabcex <- 1
    tlabsrt <- 0
    tlablt <- as.POSIXlt(pretty(tlimlt, n=10))
    # remove lables which are possibly out of limits due to pretty
    if (any(tlablt < tlimlt[1])) {
        tlablt <- tlablt[-which(tlablt < tlimlt[1])]
    }
    if (any(tlablt > tlimlt[2])) {
        tlablt <- tlablt[-which(tlablt > tlimlt[2])]
    }
    tatn <- as.numeric(tlablt)
    tlablt_diff <- unclass(diff(range(tlablt)))
    # do not show days if range of tlim is above 1 year
    if (attr(tlablt_diff, "units") == "days") {
        if (tlablt_diff > 365) { # remove days --> shortens labels
            message("time lims is longer than 1 year, modify time labels ...")
            tlablt <- substr(tlablt, 1, 7) # this destroys the POSIX
        } else { # decrease label size due to long labels
            message("change time label angle ...")
            tlabsrt <- 45
        } 
    }

} # check times related stuff

if (F) { # for testing
    message("special")
    datas[[1]][[2]] <- datas[[1]][[1]] + 10
    names(datas[[1]])[2] <- names(datas[[1]])[1]
}

# set variable specific things
message("\n", "set variable specific things ...")
nvars_per_runid <- sapply(datas, length)
v <- datas
for (i in 1:nsettings) {
    for (vi in 1:nvars_per_runid[i]) {
        # default options: default name only
        v2 <- list(label=names(datas[[i]])[vi])
        if (names(datas[[i]])[vi] == "temp2") {
            v2$label <- "2m Temperature [°C]"
            v2$offset <- c("-", 273.15)
        }
        v[[i]][[vi]] <- v2
        rm(v2)
    }
}
# finished setting variable specific things

# apply offset or mult_fac
message("\n", "apply variable specific things ...")
for (i in 1:nsettings) {
    for (vi in 1:nvars_per_runid[i]) {
        if (!is.null(v[[i]][[vi]]$offset) &&
            !is.null(v[[i]][[vi]]$mult_fac)) {
            stop("both \"offset\" and \"mult_fac\" are defined for runid ", 
                 names_short[i], " variable ", names(v[[i]])[vi], ". dont know which to take")
        }
        if (!is.null(v[[i]][[vi]]$offset)) {
            cmd <- paste0("datas[[", i, "]][[", vi, "]] <- datas[[", i, "]][[", vi, "]] ", 
                          v[[i]][[vi]]$offset[1], " ", v[[i]][[vi]]$offset[2])
            message("eval ", cmd, " ...")
            eval(parse(text=cmd))
        }
    }
} # for i nsettings
# start mode specific things
if (any(mode == c("timmean"))) {

    message("\n", "timmean plot ...")

    z <- vector("list", l=nsettings)
    names(z) <- names_legend
    x <- y <- z
    for (i in 1:nsettings) {
        varind <- which(names(datas[[i]]) == varname_out)
        if (length(varind) != 1) {
            warning("could not find varname \"", varname_out, " in setting ", i, ": ", names_short[i], ". skip to next")
        } else {
            z[[i]] <- datas[[i]][[varind]]
            if (length(dim(z[[i]])) != 2) {
                warning("length(dim(z[[", i, "]])) = ", length(dim(z[[i]])), 
                        ". cannot make timmean plot of setting ", i, ": ", names_short[i])
                next # setting
            }
            if (any(attributes(z[[i]])$dims != c("lon", "lat"))) {
                warning("attributes(z[[", i, "]]))$dims = ", paste0(attributes(z[[i]])$dims, collapse=", "),
                        ". need lon, lat. cannot make timmean plot of setting ", i, ": ", names_short[i])
                next # setting
            }
            x[[i]] <- dims[[i]]$lon
            y[[i]] <- dims[[i]]$lat
        } # if varname was found
    } # for i nsettings
    
    plotname <- paste0(plotpath, "/", mode, "/", varname_out, "/",
                       varname_out, "_", 
                       paste0(names_short, "_", seasonsp, 
                              "_", fromsp, "-", tosp, "_", areas, collapse="_vs_"), 
                       ".", p$plot_type)
    message("plot ", plotname, " ...")
    dir.create(dirname(plotname), recursive=T, showWarnings=F)
    if (p$plot_type == "png") {
        png(plotname, width=p$map_width, height=p$map_height,
            res=p$dpi, family=p$family)
    } else if (p$plot_type == "pdf") {
        pdf(plotname, width=p$inch, height=p$inch*p$ts_height/p$ts_width,
            family=p$family)
    }

    source(paste0(homepath, "/functions/image.plot.pre.r"))
    ip <- image.plot.pre(range(z, na.rm=T))

    source(paste0(homepath, "/functions/image.plot.nxm.r"))
    image.plot.nxm(x=x, y=y, z=z, ip=ip, verbose=T)

    dev.off()

} else if (any(mode == c("fldmean"))) {

    message("\n", "fldmean plot ...")
    
    # compare the same variable across models
    for (vi in 1:max(nvars_per_runid)) {

        data <- lapply(datas, "[", vi)
        if (any(n_mas != 1)) datama <- lapply(datasma, "[", vi)

        varsf <- unique(sapply(data, names))
        if (any(is.na(varsf))) varsf <- varsf[-which(is.na(varsf))]
        varsf <- paste0(varsf, collapse="_")
        if (varsf == "") stop("varsf = ", varsf)
        message("\n", varsf, " ...")

        # ylim default: moving average only if available
        if (any(n_mas != 1)) {
            message("\n", "temporal min/mean/max datama:")
            for (i in 1:nsettings) {
                message(names_short[i], " = ", min(datama[[i]][[1]], na.rm=T), "/",
                        mean(datama[[i]][[1]], na.rm=T), "/", max(datama[[i]][[1]], na.rm=T))
            }
            ylim <- range(datama, na.rm=T)
            if (add_unsmoothed) {
                message("\n", "temporal min/mean/max data:")
                for (i in 1:nsettings) {
                    message(names_short[i], " = ", min(data[[i]][[1]], na.rm=T), "/",
                            mean(data[[i]][[1]], na.rm=T), "/", max(data[[i]][[1]], na.rm=T))
                }
                ylim <- range(ylim, data, na.rm=T)
            }
        } else {
            message("\n", "temporal min/mean/max data:")
            for (i in 1:nsettings) {
                message(names_short[i], " = ", min(data[[i]][[1]], na.rm=T), "/",
                        mean(data[[i]][[1]], na.rm=T), "/", max(data[[i]][[1]], na.rm=T))
            }
            ylim <- range(data, na.rm=T)
        }
        message("\n", "ylim=", appendLF=F)
        dput(ylim)

        yat <- pretty(ylim)

        # plotname
        plotname <- paste0(plotpath, "/", mode, "/", varsf, "/",
                           paste0(unique(paste0(models, "_", names_short)), collapse="_vs_"), "_",
                           paste0(unique(areas), collapse="_vs_"), "_",
                           ifelse(any(levsf != ""), 
                                  paste0(unique(levsf), collapse="_vs_"), ""), "_",
                           paste0(unique(streams), collapse="_vs_"), "_",
                           paste0(unique(seasonsp), collapse="_vs_"), "_",
                           gsub(" ", "_", paste0(tlimlt, collapse="-")),
                           ".", p$plot_type)
        dir.create(dirname(plotname), recursive=T, showWarnings=F)
        if (p$plot_type == "png") {
            png(plotname, width=p$ts_width, height=p$ts_height,
                res=p$dpi, family=p$family)
        } else if (p$plot_type == "pdf") {
            pdf(plotname, width=p$inch, height=p$inch*p$ts_height/p$ts_width,
                family=p$family)
        }
                           
        # open plot 
        mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
        message("decrease right margin")
        mar[4] <- 1
        if (!add_title) { 
            message("decrease upper margin")
            mar[3] <- 1
        }
        if (tlabsrt == 0) {
            message("decrease lower margin")
            mar[1] <- mar[1]/2
        }
        #par(mar=c(5.1, 6.1, 4.1, 4.1))
        par(mar=mar)
        plot(timeslt[[1]], data[[1]][[1]], t="n",
             xlim=tlimn, ylim=ylim, 
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
        axis(2, at=yat, las=2)

        # add title
        if (add_title) {
            message("\n", "add title ...")
        }

        # add variable label
        mtext(side=2, v[[1]][[vi]]$label, line=3.4, cex=0.9)

        # add grid
        if (add_grid) {
            message("\n", "add grid ...")
            abline(v=tatn, h=yat, col="gray", lwd=0.5)
        }

        # add data
        for (i in 1:nsettings) {

            if (!is.null(data[[i]][[1]])) {

                # add moving average data if wanted
                if (any(n_mas != 1)) {
                    # unsmoothed first if wanted
                    if (add_unsmoothed) {
                        lines(timeslt[[i]], data[[i]][[1]], 
                              col=cols_rgb[i], lty=ltys[i], lwd=lwds[i], pch=pchs[i])
                    }
                    # smoothed second 
                    lines(timeslt[[i]], datama[[i]][[1]], 
                          col=cols[i], lty=ltys[i], lwd=lwds[i], pch=pchs[i])

                # add unsmoothed data only
                } else {
                    # if length is only 1 due to subset
                    if (length(timeslt[[i]]) == 1) { 
                        points(timeslt[[i]], data[[i]][[1]], col=cols[i], pch=1)
                    } else {
                        lines(timeslt[[i]], data[[i]][[1]], 
                              col=cols[i], lty=ltys[i], lwd=lwds[i], pch=pchs[i])
                    }
                }
                if (length(timeslt[[i]]) == 1) abline(v=timeslt[[i]])

                # special: add first data point
                if (T) {
                    if (i == 1) message("\n", "add first data point")
                    points(timeslt[[i]][1], data[[i]][[1]][1], 
                          col=cols[i], lty=ltys[i], lwd=lwds[i], 
                          pch=1)
                } # add first data point

                # add linear regression trend if wanted
                if (add_trend) {

                }

            } # if (!is.null(data[[i]][[1]]))

        } # for i nsettings add data 

        # add legend if wanted
        if (add_legend) {
            le <- list()
            #le$pos <- "topleft" 
            le$pos <- "bottomleft" 
            #le$pos <- "bottomright" 
            #le$ncol <- nsettings/2
            le$ncol <- 1
            #le$ncol <- 2 
            le$text <- names_short
            le$col <- cols
            le$lty <- ltys
            le$lwds <- lwds
            le$pchs <- pchs
            le$cex <- 1
            le$cex <- 0.85
            # add stuf to legend here
            if (F) {

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
        message("\n", "save ", plotname, " ...")
        dev.off()

    } # for vi max(nvars_per_runid)

} # if mode = fldmean

message("\n", "finish", "\n")
