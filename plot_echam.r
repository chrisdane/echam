## r

#options(warn=2) # stop on warnings
#options(warn=0) # back to default

if (T) {
    message("\nrm(list=ls())")
    rm(list=ls())
    # make squeeze default:
    fctbackup <- `[`
    `[` <- function(...) { fctbackup(..., drop=F) } # set back to default: `[` <- fctbackup 
} else {
    message("\ndo not clear work space ...")
}
message("graphics.off()")
graphics.off()

# load libraries necessary for plot_echam.r
requirements <- readLines("requirements_plot.txt")
for (r in requirements) if (substr(r, 1, 1) != "#") library(r, character.only=T)

# helper functions
message("\nload helper_functions.r ...")
source("helper_functions.r")

# get host options
host <- get_host()

# todo: how to load functions from another repo without the subrepo hassle?
if (file.exists(paste0(host$homepath, "/functions/myfunctions.r"))) {
    # dependencies from myfuncions.r: 
    # setDefaultPlotOptions(), reorder_legend(), mycols(), get_pval(), make_posixlt_origin()
    message("\nload ", host$homepath, "/functions/myfunctions.r ...")
    source(paste0(host$homepath, "/functions/myfunctions.r"))
} else {
    stop("\ncould not load ", host$homepath, "/functions/myfunctions.r")
}

## check user input via namelist.plot.r
message("\nload and check namelist.plot.r ...")
source("namelist.plot.r")

# must-have objects set by user
objs <- c("prefixes", "models", "names_short", "fromsf", "tosf", "varnames_in", "modes")
for (obj in objs) if (!exists(obj)) stop("provide `", obj, "` in namelist.plot.r")
nsettings <- length(prefixes)
if (exists("varnames_uv")) {
    if (!all(sapply(varnames_uv, length) == 2)) {
        stop("provided `varnames_uv` must have 2 entries (name of u- and v-components) of given variable")
    }
}
if (center_ts && scale_ts) {
    stop("both `center_ts` and `scale_ts` are true. choose one: center = x-mu; scale = (x-mu)/sd.")
}

## possible objects set by user: set defaults if not set by user 
# defaults: general stuff
if (!exists("postpaths")) { # default from post_echam.r
    postpaths <- rep(paste0(host$workpath, "/post"), t=nsettings)
}
if (!exists("plotpath")) { # default from post_echam.r
    plotpath <- paste0(host$workpath, "/plots/", paste(unique(models), collapse="_vs_"))
}
if (!exists("names_legend")) names_legend <- rep("", t=nsettings)

# defaults: code stuff
if (!exists("codes")) codes <- rep("", t=nsettings)
codesf <- codes
codesf[codes != ""] <- paste0("_selcode_", codesf[codes != ""])

# defaults: time stuff
if (!exists("seasonsf")) seasonsf <- rep("Jan-Dec", t=nsettings)
if (any(is.na(seasonsf))) seasonsf[is.na(seasonsf)] <- "Jan-Dec"
if (!exists("fromsp")) fromsp <- rep(NA, t=nsettings)
if (!exists("tosp")) tosp <- rep(NA, t=nsettings)
froms_plot <- tos_plot <- rep(NA, t=nsettings)
if (!exists("seasonsp")) seasonsp <- seasonsf
if (any(is.na(seasonsp))) seasonsp[is.na(seasonsp)] <- seasonsf[is.na(seasonsp)]
if (!exists("n_mas")) n_mas <- rep(1, t=nsettings) # 1 = no moving average effect
if (all(n_mas == 1)) {
    if (add_unsmoothed == F) {
        message("\nall `n_mas` = 1, change `add_unsmoothed` from F to T ...")
        add_unsmoothed <- T
    }
    if (add_smoothed == T) {
        message("\nall `n_mas` = 1, change `add_smoothed` from T to F ...")
        add_smoothed <- F
    }
}
if (!exists("new_origins")) new_origins <- rep(NA, t=nsettings)
if (!exists("time_ref")) time_ref <- NA # only one
if (!exists("remove_setting")) remove_setting <- NA
if (!exists("remove_mean_froms")) remove_mean_froms <- rep(NA, t=nsettings) 
if (!exists("remove_mean_tos")) remove_mean_tos <- rep(NA, t=nsettings) 
if (!is.na(remove_setting) && 
    (any(!is.na(remove_mean_froms)) || any(!is.na(remove_mean_tos)))) {
    stop("both `remove_setting` and `remove_mean_froms` have non-NA values. ",
         "choose either `remove_setting` OR `remove_mean_froms` (and `remove_mean_tos`)")
}

# defaults: level stuff
if (!exists("levs") && !exists("levsf")) {
    levs <- levsf <- rep("", t=nsettings)
} else if (exists("levs") && !exists("levsf")) {
    levsf <- levs
    levsf[levs != ""] <- paste0("_sellevel_", levs[levs != ""])
} else if (!exists("levs") && exists("levsf")) {
    levs <- levsf
    levs[levsf != ""] <- gsub("_", "", levsf[levsf != ""])
}

# defaults: depth stuff
if (!exists("depths") && !exists("depthsf")) {
    depths <- depthsf <- rep("", t=nsettings)
} else if (exists("depths") && !exists("depthsf")) {
    depthsf <- depths
    depthsf[depths != ""] <- paste0("_", depths[depths != ""], "m")
} else if (!exists("depths") && exists("depthsf")) {
    depths <- depthsf
    depths[depthsf != ""] <- gsub("_", "", depthsf[depthsf != ""])
}
if (!exists("depth_fromsf")) depth_fromsf <- rep(NA, t=nsettings)
if (!exists("depth_tosf")) depth_tosf <- rep(NA, t=nsettings)
if (!exists("depth_fromsp")) depth_fromsp <- depth_fromsf
if (!exists("depth_tosp")) depth_tosp <- depth_tosf

# defaults: area stuff
if (!exists("areas")) areas <- rep("global", t=nsettings)
if (!exists("regboxes")) {
    regboxes <- vector("list", l=nsettings)
    regboxes <- lapply(regboxes, append, list(regbox=NA))
}
names(regboxes) <- names_short
areas_out <- areas
if (any(!is.na(sapply(regboxes, "[[", "regbox")))) { # run namelist.area.r to get lon/lat of regional boxes
    message("\nsome regboxes$regbox are not NA -> load and check namelist.area.r ...")
    source("namelist.area.r")
    areas_out[which(!is.na(sapply(regboxes, "[[", "regbox")))] <- sapply(regboxes, "[[", "regbox")
}

# defaults: regular interp stuff
if (!exists("reg_dxs")) reg_dxs <- rep("", t=nsettings)
if (!exists("reg_dys")) reg_dys <- rep("", t=nsettings)
reg_dxsf <- reg_dxs
reg_dxsf[reg_dxs != ""] <- paste0("_regular_dx", reg_dxs[reg_dxs != ""])
reg_dysf <- reg_dys
reg_dysf[reg_dys != ""] <- paste0("_dy", reg_dys[reg_dys != ""])

# defaults: plot stuff
if (!exists("types")) types <- rep("l", t=nsettings) # default: lines plots and not points
if (!exists("ltys")) ltys <- rep(1, t=nsettings)
if (nsettings == 1) { # mycols my colors nicer than R defaults
    cols_vec <- "black"
} else if (nsettings == 2) {
    cols_vec <- c("black", "#E41A1C") # black, myred
} else if (nsettings >= 3) {
    cols_vec <- c("black", "#E41A1C", "#377EB8") # my default: (black, myred, myblue) instead of R default (black, red, blue)
    if (nsettings > 3) {
        if (F) {
            cols_vec <- c(cols_vec, 4:nsettings)
        } else if (T) {
            library(RColorBrewer) # https://www.r-bloggers.com/palettes-in-r/
            cols_vec <- c(cols_vec, brewer.pal(max(3, nsettings), "Dark2")[1:(nsettings-3)])
        }
    }
}
if (!exists("cols")) {
    cols <- cols_vec
} else if (exists("cols")) {
    #message("provided `cols`: ", paste(cols, collapse=", "))
    if (is.numeric(cols)) {
        cols <- as.integer(cols)
        cols <- cols_vec[cols]
    }
}
cols_rgb <- rgb(t(col2rgb(cols)/255), alpha=alpha_rgb)
if (F) {
    message("\nuse transparent cols ...")
    cols_save <- cols
    cols <- cols_rgb
} 
if (!exists("lwds")) lwds <- rep(1, t=nsettings)
if (!exists("pchs")) pchs <- rep(pchs_filled_wout_border[1], t=nsettings)
if (!exists("scatterpchs")) scatterpchs <- rep(pchs_filled_wout_border[1], t=nsettings)
if (!exists("text_cols")) text_cols <- rep("black", t=nsettings)
if (exists("cols_samedims")) {
    if (class(cols_samedims) == "integer") { # user provided color numbers
        cols_samedims <- cols_vec[cols_samedims]
    }
}

# check suffixes
plotname_suffix <- "" # default: nothing
if (center_ts) plotname_suffix <- "_center_ts"
if (scale_ts) plotname_suffix <- "_scale_ts"
if (!ts_highlight_seasons$bool) ts_highlight_seasons$suffix <- ""

if (length(add_linear_trend) != nsettings) {
    if (length(add_linear_trend) == 1) {
        message("\ngiven `add_linear_trend` but of length ", length(add_linear_trend), 
                " and nsettings = ", nsettings, " --> repeat ", nsettings, " times ...") 
        add_linear_trend <- rep(add_linear_trend, t=nsettings)
    } else {
        if (length(add_linear_trend) != nsettings) {
            stop("\ngiven `add_linear_trend` but of length ", length(add_linear_trend), 
                " and nsettings = ", nsettings, " --> dont know how to interpret this")
        }
    }
}
if (length(add_scatter_density) != nsettings) {
    if (length(add_scatter_density) == 1) {
        message("\ngiven `add_scatter_density` but of length ", length(add_scatter_density), 
                " and nsettings = ", nsettings, " --> repeat ", nsettings, " times ...") 
        add_scatter_density <- rep(add_scatter_density, t=nsettings)
    } else {
        if (length(add_scatter_density) != nsettings) {
            stop("\ngiven `add_scatter_density` but of length ", length(add_scatter_density), 
                " and nsettings = ", nsettings, " --> dont know how to interpret this")
        }
    }
}


## allocate
datas <- vector("list", l=nsettings)
names(datas) <- names_short
data_infos <- dims <- dims_per_setting_in <- ll_data <- poly_data <- datas


## load pangaea data if defined 
if (F) {
    message("\nload pangaea data via load_pangaea_data.r ...")
    source("load_pangaea_data.r")
}

## load special data if defined
message("\nload special data via load_special_data.r ...")
source("load_special_data.r")

## plot special data if defined
message("\nplot special data via load_special_data.r ...")
source("plot_special_data.r")


# read data
message("\n===================================\nread model data ...")
for (i in seq_len(nsettings)) {

    message("\n*********************************************")
    message("setting ", i, "/", nsettings, ": ", names_short[i], " ...")
    inpath <- paste0(postpaths[i], "/", models[i], "/", modes[i], "/", varnames_in[i])
    
    fname <- paste0(prefixes[i], "_", models[i], "_", modes[i], 
                    codesf[i], "_", varnames_in[i], 
                    levsf[i], depthsf[i], "_",
                    areas[i], "_", 
                    seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                    reg_dxsf[i], reg_dysf[i],
                    ".nc") 

    message("\nopen ", inpath, "/", fname, " ...")
    ncin <- nc_open(paste0(inpath, "/", fname))

    # get dims of file
    message("\nget dims ...")
    dims_per_setting_in[[i]] <- names(ncin$dim)
    dimtmp <- vector("list", l=ncin$ndims)
    names(dimtmp) <- dims_per_setting_in[[i]]
    for (di in seq_along(dimtmp)) {
        message(di, ": \"", dims_per_setting_in[[i]][di], "\", n=", length(ncin$dim[[di]]$vals))
        dimtmp[[di]] <- ncin$dim[[di]]$vals
    }
    dims[[i]] <- dimtmp
    rm(dimtmp)

    # drop dims of length 1
    dropinds <- c()
    for (di in seq_along(dims[[i]])) {
        if (length(dims[[i]][[di]]) == 1) {
            if (length(dropinds) == 0) message("\n", appendLF=F)
            message("length of detected dim \"", names(dims[[i]])[di], 
                    "\" is 1 and its value is ", dims[[i]][[di]])
            dropinds <- c(dropinds, di)
        }
    } # for di
    if (length(dropinds) > 0) {
        message("--> drop these dims of length 1 ...")
        dims[[i]][dropinds] <- NULL
    }


    ## time dim stuff
    # time dim as posix object
    if (any(names(dims[[i]]) == "time")) {

        timein_units <- ncin$dim$time$units
        message("\ndetected time dim of length ", length(dims[[i]]$time), "; dims[[", i, "]]$time:")
        ht(dims[[i]]$time)
        
        if (prefixes[i] == "Hol-T_stschuett_echam5_wiso") {
            message("\nspecial: rev steffens time ...")
            dims[[i]]$time <- rev(dims[[i]]$time)
            ht(dims[[i]]$time, n=20)
        }
        
        # convert any unit to seconds for POSIX
        message("\n--> POSIXlt object from timein_units: \"", timein_units, "\" ...")

        # 2 different types so far:
        #   "days since 1538-1-1 00:00:00"
        #   "day as %Y%m%d.%f"
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
            } else if (timein_format == "%Y.%f") { # e.g. 7000
                # only years given -> take mid-year for months & days
                months <- rep(6, t=length(dims[[i]]$time))
                days <- rep(15, t=length(dims[[i]]$time))
                timein_lt <- as.POSIXlt(paste0(substr(dims[[i]]$time, 1, 4), "-", 
                                               months, "-", days), tz="UTC")
            } else {
                stop("timein_format = \"", timein_format, "\" not defined")
            }
        } # which timein_units "days since", "day as", etc.
        message("timein_lt:")
        ht(timein_lt, n=20)
        message("range(timein_lt) = ", appendLF=F)
        print(range(timein_lt))
        message("range(timein_lt$mon+1) = ", appendLF=F)
        print(range(timein_lt$mon+1))

        # shift times due to e.g. senseless spinup years
        # as.POSIXlt's 'year' starts at 1900
        if (!is.na(new_origins[i])) {
            # from year in  = min(timein_lt$year) + 1900
            message("\nprovided `new_origins[", i, "]` = ", new_origins[i])
            message("--> shift range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))
            #shift_by <- -(timein_lt$year[1] + 1900 - new_origins[i]) 
            shift_by <- new_origins[i] - (timein_lt$year[1] + 1900) #- 1
            message("by `shift_by` = ", 
                    new_origins[i], " - ", timein_lt$year[1] + 1900, #" - 1 ", 
                    " = ", shift_by, " years") 
            timein_lt$year <- timein_lt$year + shift_by
            message("new timein_lt:")
            ht(timein_lt, n=20)
            message("--> new range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))
            dims[[i]]$time_shift_by <- shift_by
        } # if !is.na(new_origins[i])
        # finished set new time origin

        # find time inds if wanted (`fromsp` is defined and different than `fromsf`)
        if (!is.na(fromsp[i]) || !is.na(tosp[i])) {
            fromind <- 1 # default
            toind <- length(timein_lt)
            message("\n", appendLF=F)
            if (!is.na(fromsp[i])) {
                message("fromsp is given --> fromsp[", i, "] = ", fromsp[i])
                fromind <- which(timein_lt$year + 1900 == fromsp[i])[1]
                if (length(fromind) == 0) {
                    stop("could not find this year in input time")
                }
            }
            if (!is.na(tosp[i])) {
                message("tosp is given --> tosp[", i, "] = ", tosp[i])
                toind <- which(timein_lt$year + 1900 == tosp[i])
                toind <- toind[length(toind)]
                if (length(toind) == 0) {
                    stop("could not find this year in input time")
                }
            }
            message("--> found temporal subset between inds ", fromind, " to ", 
                    toind, " (", timein_lt[fromind], " to ", timein_lt[toind], ")")
            if (fromind > toind) {
                stop("--> `fromind` = ", fromind, " > `toind` = ", toind, " --> that does not make sense")
            }

            time_inds <- fromind:toind
            if (length(time_inds) == 0) {
                stop("temporal subset is of length 0")
            } else {
                if (length(time_inds) != length(timein_lt)) { 
                    message("found temporal subset of length ", length(time_inds), " out of ", 
                            length(dims[[i]]$time), " total time points:")
                    ht(time_inds)
                    message("before range(timein_lt) = ", appendLF=F)
                    print(range(timein_lt))
                    timein_lt <- timein_lt[time_inds]
                    message("after range(timein_lt) = ", appendLF=F)
                    print(range(timein_lt))
                    dims[[i]]$time <- dims[[i]]$time[time_inds]
                } else {
                    message("--> use complete time dim of ", 
                            length(dims[[i]]$time), " time points ...")
                }
            }

        } else { # `fromsp` is not defined
            fromsp[i] <- timein_lt$year[1] + 1900
            tosp[i] <- timein_lt$year[length(timein_lt)] + 1900
            time_inds <- NULL # default
            
        } # if exists("fromsp") || exists("tosp")
        # finished find time inds if wanted

        # find seasons inds if wanted (`seasonsp` is defined and different from `seasonsf`)
        if (seasonsp[i] != seasonsf[i]) {
            
            if (seasonsp[i] == "" || is.na(seasonsp[i])) {
                stop("seasonsp must not equal \"\" or NA")
            }

            # special case:  
            if (seasonsp[i] == "annual" && seasonsf[i] == "Jan-Dec") {
                # nothing do to

            # all other season cases:
            } else { # seasonsp != "annual" && seasonsf != "Jan-Dec"
                if (is.character(seasonsp[i])) { # "DJF" or "Jul"
                    message("\nprovided `seasonsp[", i, "]` = \"", seasonsp[i], "\" != `seasonsf[", i, 
                            "]` = \"", seasonsf[i], "\"")
                    # check if substring is in DJFMAM ...
                    season_inds <- regexpr(seasonsp[i], season_check$string)
                    if (any(season_inds != -1)) { # if season like "DJF", "JJA"
                        season_inds <- season_check$inds[season_inds:(season_inds+attributes(season_inds)$match.length-1)]
                    } else { # else if season like "Jan", "Jul"
                        season_inds <- regexpr(seasonsp[i], season_check$names)
                        if (length(which(season_inds != -1)) == 1) {
                            season_inds <- which(season_inds != -1)
                        } else {
                            stop("`seasonsp[", i, "]` = \"", seasonsp[i], "\" not defined")
                        }
                    }
                } else if (is.numeric(seasonsp[i])) {
                    message("\nprovided `seasonsp[", i, "] = ", seasonsp[i], " != `seasonsf[", i, 
                            "]` = \"", seasonsf[i], "\"\n--> find season indices ...")
                    stop("not yet")
                }
                dims[[i]]$season_inds <- season_inds
                message("--> found ", length(season_inds), " season_inds: ", paste(season_inds, collapse=", "))
                months_in <- unclass(timein_lt)$mon + 1
                month_inds <- months_in %in% season_inds
                month_inds <- which(month_inds)
                message("--> found ", length(month_inds), " month_inds:")
                ht(month_inds)
                timein_lt <- timein_lt[month_inds]
                if (!is.null(time_inds)) {
                    time_inds <- time_inds[month_inds]
                } else {
                    time_inds <- month_inds
                }
            } # if (seasonsp[i] == "annual" && seasonsf[i] == "Jan-Dec") 
        } # if seasonsp[i] != seasonsf[i]
        # finished find season inds if wanted

        # finish time stuff
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

        message("\nfinished time dim stuff:")
        message("range(dims[[", i, "]]$timelt) = ", appendLF=F)
        print(range(dims[[i]]$timelt))
        message("range(dims[[", i, "]]$timelt$mon+1) = ", appendLF=F)
        print(range(dims[[i]]$timelt$mon+1))

    } # if nc file has time dim
    froms_plot[i] <- fromsf[i] # default
    tos_plot[i] <- tosf[i]
    if (!is.na(fromsp[i])) froms_plot[i] <- fromsp[i]
    if (!is.na(tosp[i])) tos_plot[i] <- tosp[i]
    # finfished time dim stuff
    #stop("asd")


    ## lon and lat dim stuff
    # load additional modeled lon,lat data as matrix if wanted
    if (any(names(dims[[i]]) == "lon") && any(names(dims[[i]]) == "lat")) {
        
        ll_fnames <- ll_vars <- "" # default: do not load additional lon,lat model data if wanted
        if (F && grepl("cosmos-aso-wiso_Hol-T", prefixes[i]) && varnames_in[i] == "lm_tsurf_as_time_slope") {
            ll_vars <- "lm_SICOMO_as_time_slope" # multiple possible
            ll_fnames <- paste0(postpaths[i], "/mpiom1/select/", ll_vars, "/",
                               "cosmos-aso-wiso_Hol-T_remapcon2_r3600x1800_grb_mpiom1_select_selcode_15_", 
                               ll_vars, "_global_")
            if (seasonsp[i] == "MAM") {
                ll_fnames <- paste0(ll_fnames, "Mar")
            } else if (seasonsp[i] == "SON") {
                ll_fnames <- paste0(ll_fnames, "Sep")
            } else if (seasonsp[i] == "annual") {
                ll_fnames <- paste0(ll_fnames, "annual")
            } 
            ll_fnames <- paste0(ll_fnames, "_", fromsf[i], "-", tosf[i], ".nc")
        } # which setting and variable

        ll_ncs <- ll_data_per_setting <- list()
        ll_cnt <- 0
        for (li in seq_along(ll_fnames)) {
            if (file.exists(ll_fnames[li]) && ll_vars[li] != "") {
                message("\ndata has lon and lat dims and `ll_fnames` with additional modeled lon,lat data matrix exists. open\n",
                        "   `ll_fnames[", li, "]` = \"", ll_fnames[li], "\" ...")
                ll_nc <- nc_open(ll_fnames[li])
                if (!any(names(ll_nc$var) == ll_vars[li])) {
                    warning("variable `ll_vars[", li, "]` = \"", ll_vars[li], "\" is not in `ll_fnames[", li, 
                            "]` = \"", ll_fnames[li], "\".")
                } else { # wanted var is in file
                    ll_dims_of_var <- sapply(ll_nc$var[[ll_vars[li]]]$dim, "[", "name")
                    if (!any(unlist(lapply(ll_dims_of_var, function(x) any(x == "lon")))) &&
                        !any(unlist(lapply(ll_dims_of_var, function(x) any(x == "lat"))))) {
                        warning("variable `ll_vars[", li, "]` = \"", ll_vars[li], "\" from `ll_fnames[", li, 
                                "]` = \"", ll_fnames[li], "\" does not have lon and lat dims.")
                    } else { # wanted var has lon,lat dims
                        ll_cnt <- ll_cnt + 1
                        ll_ncs[[ll_cnt]] <- ll_nc
                        ll_data_per_setting[[ll_cnt]] <- list(file=ll_fnames[li],
                                                              lon=ll_nc$dim$lon$vals, lat=ll_nc$dim$lat$vals)
                        names(ll_data_per_setting)[ll_cnt] <- ll_vars[li]
                    } # if ll_vars has lon and lat dims
                } # if ll_vars is variable in ll_nc
            
            } else { # if ll_fnames and/or ll_vars are missing
                message("\ndata has lon and lat dims but ", appendLF=F)
                if (!file.exists(ll_fnames[li])) {
                    message("no proper `ll_file` with additional modeled lon,lat data", appendLF=F)
                }
                if (!file.exists(ll_fnames[li]) && ll_vars[li] == "") message(" and ", appendLF=F)
                if (ll_vars[li] == "") {
                    message("no proper `ll_vars` variable name", appendLF=F)
                }
                message(" was defined. do not load further modeled lon,lat data for this setting.")
            } # if file.exists(fname)
        } # for li in ll_fnames

        if (!all(sapply(ll_data_per_setting, is.null))) {
            ll_data[[i]] <- ll_data_per_setting
        }

    } # if nc file has lon and lat dims
    # finished loading additional modeled lon,lat data matrix if wanted

    # load additional modeled lon,lat data as polyon if wanted
    if (any(names(dims[[i]]) == "lon") && any(names(dims[[i]]) == "lat")) {
        
        poly_fnames <- poly_vars <- "" # default: do not load additional lon,lat model data if wanted
        if (F && grepl("cosmos-aso-wiso_Hol-T", prefixes[i]) && 
            varnames_in[i] == "lm_tsurf_as_time_slope") {
            poly_vars <- list(c(lon="grid_corner_lon", lat="grid_corner_lat",
                                data="lm_THO_as_time_slope")) # multiple lon,lat,data-vectors possible
            poly_fnames <- list(c(lon=paste0(host$repopath, "/mpiom/GR30s.nc"),
                                  lat=paste0(host$repopath, "/mpiom/GR30s.nc"),
                                  data=paste0(postpaths[i], "/mpiom1/", modes[i], "/", poly_vars[[1]]["data"], "/",
                                              "cosmos-aso-wiso_Hol-T_grb_mpiom1_", modes[i], "_selcode_2_", 
                                              poly_vars[[1]]["data"], "_sellevel_6_global_", seasonsp[i], "_", 
                                              fromsf[i], "-", tosf[i], ".nc")))
        } # which setting and variable
        if (length(poly_vars) != length(poly_fnames)) stop("`poly_vars` and `poly_fnames` must have the same length")

        poly_ncs <- vector("list", l=length(poly_vars))
        names(poly_ncs) <- sapply(poly_vars, "[", "data")
        poly_data_per_setting <- poly_ncs
        for (polyi in seq_along(poly_fnames)) { # for all datasets to add to plot per setting
            tmp <- list()
            for (vi in seq_along(poly_fnames[[polyi]])) { # for lon, lat, and data variables of datasets to add to plot per setting
                if (file.exists(poly_fnames[[polyi]][vi]) && poly_vars[[polyi]][vi] != "") {
                    message("\ndata has lon and lat dims and a `poly_fnames` with additional ",
                            "modeled lon,lat data polygon exists. open\n",
                            "   `poly_fnames[[", polyi, "]][", vi, "]` = \"", poly_fnames[[polyi]][vi], "\" ...")
                    poly_nc <- nc_open(poly_fnames[[polyi]][vi])
                    if (!any(names(poly_nc$var) == poly_vars[[polyi]][vi])) {
                        stop("variable `poly_vars[[", polyi, "]][", vi, "]` = \"", poly_vars[[polyi]][vi], 
                             "\" is not in `poly_fnames[[", polyi, "]][", vi, "]` = \"", poly_fnames[[polyi]][vi], "\".")
                    } else { # wanted variable is in file
                        #poly_dims_of_var_tmp <- sapply(poly_nc$var[[poly_vars[[polyi]][vi]]]$dim, "[", "name")
                        #poly_dims_of_var[[poly_cnt]] <- poly_dims_of_var_tmp
                        tmp <- c(tmp, unname(poly_fnames[[polyi]][vi]))
                        names(tmp) <- c(names(tmp)[1:(length(tmp)-1)], paste0(names(poly_vars[[polyi]])[vi], "_file"))
                        if (any(names(poly_vars[[polyi]])[vi] == c("lon", "lat"))) {
                            tmp <- c(tmp, list(ncvar_get(poly_nc, poly_vars[[polyi]][vi])))
                            names(tmp) <- c(names(tmp)[1:(length(tmp)-1)], names(poly_vars[[polyi]])[vi])
                        }
                        if (names(poly_vars[[polyi]])[vi] == "data") { # save open nc connection for loading actual data later 
                            poly_ncs[[polyi]] <- poly_nc
                        }
                    } # if poly_vars is variable in poly_nc
                
                } else { # if poly_fnames and/or poly_vars are missing
                    message("\ndata has lon and lat dims but ", appendLF=F)
                    if (!file.exists(poly_fnames[[polyi]][vi])) {
                        message("`poly_file[[", polyi, "]][", vi, "]`=", poly_fnames[[polyi]][vi], " does not exist", appendLF=F)
                    }
                    if (!file.exists(poly_fnames[[polyi]][vi]) && poly_vars[[polyi]][vi] == "") message(" and ", appendLF=F)
                    if (poly_vars[[polyi]][vi] == "") {
                        message("`poly_vars[[", polyi, "]][", vi, "]` is no proper variable name was defined", appendLF=F)
                    }
                    message(" --> do not load further modeled lon,lat data for this setting.")
                } # if file.exists(fname)
            } # for vi in poly_fnames[[polyi]]

            if (length(tmp) == 0) {
                poly_data_per_setting[[polyi]] <- NULL
            } else {
                poly_data_per_setting[[polyi]] <- tmp
            }

        } # for polyi in poly_fnames

        if (!all(sapply(poly_data_per_setting, is.null))) {
            poly_data[[i]] <- poly_data_per_setting
        }
    
    } # if nc file has lon and lat dims
    # finished loading additional modeled lon,lat data polygon if wanted

    # reorder lon dim values to (-180,...,180) if wanted and necessary
    if (any(names(dims[[i]]) == "lon")) {
        if (reorder_lon_from_0360_to_180180) {
            if (any(dims[[i]]$lon < 180) && any(dims[[i]]$lon >= 180)) {
                message("\ndetected lon dim of length ", length(dims[[i]]$lon), " with min/max = ", 
                        min(dims[[i]]$lon), "/", max(dims[[i]]$lon), 
                        " degree lon\n`reorder_lon_from_0360_to_180180` = T AND any(lon < 180) && any(lon >= 180)")
                dims[[i]]$lon_orig <- dims[[i]]$lon
                west_of_180_inds <- which(dims[[i]]$lon < 180)
                east_of_180_inds <- which(dims[[i]]$lon >= 180)
                dims[[i]]$lon <- dims[[i]]$lon_orig - 180
                message("--> reorder lon dim value indices from\n",
                        "   ", west_of_180_inds[1],
                        ifelse(length(west_of_180_inds) > 1, paste0(",", west_of_180_inds[2]), ""), 
                        ifelse(length(west_of_180_inds) > 3, ",...", ""),
                        ifelse(length(west_of_180_inds) > 2, paste0(",", west_of_180_inds[length(west_of_180_inds)]), ""), 
                        ",", east_of_180_inds[1],
                        ifelse(length(east_of_180_inds) > 1, paste0(",", east_of_180_inds[2]), ""), 
                        ifelse(length(east_of_180_inds) > 3, ",...", ""),
                        ifelse(length(east_of_180_inds) > 2, paste0(",", east_of_180_inds[length(east_of_180_inds)]), ""), 
                        " (", dims[[i]]$lon_orig[west_of_180_inds[1]], 
                        ifelse(length(west_of_180_inds) > 1, paste0(",", dims[[i]]$lon_orig[west_of_180_inds[2]]), ""), 
                        ifelse(length(west_of_180_inds) > 3, ",...", ""),
                        ifelse(length(west_of_180_inds) > 2, paste0(",", dims[[i]]$lon_orig[west_of_180_inds[length(west_of_180_inds)]]), ""), 
                        ",", dims[[i]]$lon_orig[east_of_180_inds[1]],
                        ifelse(length(east_of_180_inds) > 1, paste0(",", dims[[i]]$lon_orig[east_of_180_inds[2]]), ""), 
                        ifelse(length(east_of_180_inds) > 3, ",...", ""),
                        ifelse(length(east_of_180_inds) > 2, paste0(",", dims[[i]]$lon_orig[east_of_180_inds[length(east_of_180_inds)]]), ""), 
                        " deg lon)\nto\n   ", east_of_180_inds[1],
                        ifelse(length(east_of_180_inds) > 1, paste0(",", east_of_180_inds[2]), ""), 
                        ifelse(length(east_of_180_inds) > 3, ",...", ""),
                        ifelse(length(east_of_180_inds) > 2, paste0(",", east_of_180_inds[length(east_of_180_inds)]), ""), 
                        ",", west_of_180_inds[1],
                        ifelse(length(west_of_180_inds) > 1, paste0(",", west_of_180_inds[2]), ""), 
                        ifelse(length(west_of_180_inds) > 3, ",...", ""),
                        ifelse(length(west_of_180_inds) > 2, paste0(",", west_of_180_inds[length(west_of_180_inds)]), ""), 
                        " (", dims[[i]]$lon_orig[east_of_180_inds[1]], 
                        ifelse(length(east_of_180_inds) > 1, paste0(",", dims[[i]]$lon_orig[east_of_180_inds[2]]), ""), 
                        ifelse(length(east_of_180_inds) > 3, ",...", ""),
                        ifelse(length(east_of_180_inds) > 2, paste0(",", dims[[i]]$lon_orig[east_of_180_inds[length(east_of_180_inds)]]), ""), 
                        ",", dims[[i]]$lon_orig[west_of_180_inds[1]],
                        ifelse(length(west_of_180_inds) > 1, paste0(",", dims[[i]]$lon_orig[west_of_180_inds[2]]), ""), 
                        ifelse(length(west_of_180_inds) > 3, ",...", ""),
                        ifelse(length(west_of_180_inds) > 2, paste0(",", dims[[i]]$lon_orig[west_of_180_inds[length(west_of_180_inds)]]), ""), 
                        " deg lon)\n--> are these numbers correct?!")
                dims[[i]]$west_of_180_inds <- west_of_180_inds
                dims[[i]]$east_of_180_inds <- east_of_180_inds
            } # if reordering is necessary
        } # if reorder_lon_from_0360_to_180180
    } # if nc file has lon dim

    # repeat reorder lons for ll_data if necessary
    if (!is.null(ll_data[[i]][[1]]$lon)) {
        if (reorder_lon_from_0360_to_180180) {
            for (li in seq_along(ll_data[[i]])) {
                if (any(ll_data[[i]][[li]]$lon < 180) && any(ll_data[[i]][[li]]$lon >= 180)) {
                    message("\nadditional lon,lat matrix data: detected lon dim of length ", 
                            length(ll_data[[i]][[li]]$lon), " with min/max = ", 
                            min(ll_data[[i]][[li]]$lon), "/", max(ll_data[[i]][[li]]$lon), 
                            " degree lon\n`reorder_lon_from_0360_to_180180` = T AND any(lon < 180) && any(lon >= 180)")
                    ll_data[[i]][[li]]$lon_orig <- ll_data[[i]][[li]]$lon
                    west_of_180_inds <- which(ll_data[[i]][[li]]$lon < 180)
                    east_of_180_inds <- which(ll_data[[i]][[li]]$lon >= 180)
                    ll_data[[i]][[li]]$lon <- ll_data[[i]][[li]]$lon_orig - 180
                    message("--> reorder lon dim value indices from\n",
                            "   ", west_of_180_inds[1],
                            ifelse(length(west_of_180_inds) > 1, paste0(",", west_of_180_inds[2]), ""), 
                            ifelse(length(west_of_180_inds) > 3, ",...", ""),
                            ifelse(length(west_of_180_inds) > 2, paste0(",", west_of_180_inds[length(west_of_180_inds)]), ""), 
                            ",", east_of_180_inds[1],
                            ifelse(length(east_of_180_inds) > 1, paste0(",", east_of_180_inds[2]), ""), 
                            ifelse(length(east_of_180_inds) > 3, ",...", ""),
                            ifelse(length(east_of_180_inds) > 2, paste0(",", east_of_180_inds[length(east_of_180_inds)]), ""), 
                            " (", ll_data[[i]][[li]]$lon_orig[west_of_180_inds[1]], 
                            ifelse(length(west_of_180_inds) > 1, paste0(",", ll_data[[i]][[li]]$lon_orig[west_of_180_inds[2]]), ""), 
                            ifelse(length(west_of_180_inds) > 3, ",...", ""),
                            ifelse(length(west_of_180_inds) > 2, paste0(",", ll_data[[i]][[li]]$lon_orig[west_of_180_inds[length(west_of_180_inds)]]), ""), 
                            ",", ll_data[[i]][[li]]$lon_orig[east_of_180_inds[1]],
                            ifelse(length(east_of_180_inds) > 1, paste0(",", ll_data[[i]][[li]]$lon_orig[east_of_180_inds[2]]), ""), 
                            ifelse(length(east_of_180_inds) > 3, ",...", ""),
                            ifelse(length(east_of_180_inds) > 2, paste0(",", ll_data[[i]][[li]]$lon_orig[east_of_180_inds[length(east_of_180_inds)]]), ""), 
                            " deg lon)\nto\n   ", east_of_180_inds[1],
                            ifelse(length(east_of_180_inds) > 1, paste0(",", east_of_180_inds[2]), ""), 
                            ifelse(length(east_of_180_inds) > 3, ",...", ""),
                            ifelse(length(east_of_180_inds) > 2, paste0(",", east_of_180_inds[length(east_of_180_inds)]), ""), 
                            ",", west_of_180_inds[1],
                            ifelse(length(west_of_180_inds) > 1, paste0(",", west_of_180_inds[2]), ""), 
                            ifelse(length(west_of_180_inds) > 3, ",...", ""),
                            ifelse(length(west_of_180_inds) > 2, paste0(",", west_of_180_inds[length(west_of_180_inds)]), ""), 
                            " (", ll_data[[i]][[li]]$lon_orig[east_of_180_inds[1]], 
                            ifelse(length(east_of_180_inds) > 1, paste0(",", ll_data[[i]][[li]]$lon_orig[east_of_180_inds[2]]), ""), 
                            ifelse(length(east_of_180_inds) > 3, ",...", ""),
                            ifelse(length(east_of_180_inds) > 2, paste0(",", ll_data[[i]][[li]]$lon_orig[east_of_180_inds[length(east_of_180_inds)]]), ""), 
                            ",", ll_data[[i]][[li]]$lon_orig[west_of_180_inds[1]],
                            ifelse(length(west_of_180_inds) > 1, paste0(",", ll_data[[i]][[li]]$lon_orig[west_of_180_inds[2]]), ""), 
                            ifelse(length(west_of_180_inds) > 3, ",...", ""),
                            ifelse(length(west_of_180_inds) > 2, paste0(",", ll_data[[i]][[li]]$lon_orig[west_of_180_inds[length(west_of_180_inds)]]), ""), 
                            " deg lon)\n--> are these numbers correct?!")
                    ll_data[[i]][[li]]$west_of_180_inds <- west_of_180_inds
                    ll_data[[i]][[li]]$east_of_180_inds <- east_of_180_inds
                } # if reordering is necessary
            } # for li all ll vars per setting
        } # if reorder_lon_from_0360_to_180180
    } # if nc file of ll_data has lon dim
   
    # repeat reorder lons for poly_data if necessary
    if (!is.null(poly_data[[i]][[1]]$lon)) {
        if (reorder_lon_from_0360_to_180180) {
            for (li in seq_along(poly_data[[i]])) {
                if (any(poly_data[[i]][[li]]$lon < 180) && any(poly_data[[i]][[li]]$lon >= 180)) {
                    message("\nadditional lon,lat poly data: detected lon dim of length ", length(poly_data[[i]][[li]]$lon), 
                            " with min/max = ", min(poly_data[[i]][[li]]$lon), "/", max(poly_data[[i]][[li]]$lon), 
                            " degree lon\n`reorder_lon_from_0360_to_180180` = T AND any(lon < 180) && any(lon >= 180)")
                    poly_data[[i]][[li]]$lon_orig <- poly_data[[i]][[li]]$lon
                    poly_data[[i]][[li]]$lon[poly_data[[i]][[li]]$lon > 180] <- 
                        poly_data[[i]][[li]]$lon[poly_data[[i]][[li]]$lon > 180] - 360
                } # if reordering is necessary
            } # for li all ll vars per setting
        } # if reorder_lon_from_0360_to_180180
    } # if nc file of poly_data has lon dim
    
    # flip lats (needs to be increasing for plot)
    if (any(names(dims[[i]]) == "lat")) {
        if (any(diff(dims[[i]]$lat) < 0)) {
            message("\ndetected lat dim of length ", length(dims[[i]]$lat), " with min/max = ", 
                    min(dims[[i]]$lat), "/", max(dims[[i]]$lat), 
                    " degree lat\nlats are decreasing -> flip to get increasing values ...") 
            dims[[i]]$lat_orig <- dims[[i]]$lat
            dims[[i]]$lat <- rev(dims[[i]]$lat)
        }
    } # if nc file has lat dim
    
    # repeat flip lats for ll_data if necessary
    if (!is.null(ll_data[[i]][[1]]$lat)) {
        for (li in seq_along(ll_data[[i]])) {
            if (any(diff(ll_data[[i]][[li]]$lat) < 0)) {
                message("\nadditional lon,lat data: detected lat dim of length ", length(ll_data[[i]][[li]]$lat), " with min/max = ", 
                        min(ll_data[[i]][[li]]$lat), "/", max(ll_data[[i]][[li]]$lat), 
                        " degree lat\nlats are decreasing -> flip to get increasing values ...") 
                ll_data[[i]][[li]]$lat_orig <- ll_data[[i]][[li]]$lat
                ll_data[[i]][[li]]$lat <- rev(ll_data[[i]][[li]]$lat)
            }
        } # for li in ll_data[[i]] 
    } # if !is.null(ll_data[[i]][[1]]$lat)

    # get lon and/or lat inds
    if (!is.na(regboxes[[i]]$regbox)) {
        message("\n`regboxes[[", i, "]]$regbox` = \"", regboxes[[i]]$regbox, "\" is not NA")
        # 2 cases: case 1: rectangular box
        #          case 2: list of x,y pairs indicating contour of arbitrary polygon per setting
        # case 1: lons
        if (!is.null(regboxes[[i]]$lons) && length(regboxes[[i]]$lons) == 2 && any(names(dims[[i]]) == "lon")) {
            message("   `regboxes[[", i, "]]$lons` of length 2 are given -> find lon inds between min/max(regboxes[[", i, "]]$lons) = ", 
                    min(regboxes[[i]]$lon), "/", max(regboxes[[i]]$lon), " deg lon ...")
            lon_inds <- which(dims[[i]]$lon >= regboxes[[i]]$lons[1] & dims[[i]]$lon <= regboxes[[i]]$lons[2])
            if (length(lon_inds) > 0) {
                if (length(lon_inds) != length(dims[[i]]$lon)) { 
                    message("      found lon subset of length ", length(lon_inds), " out of ", 
                            length(dims[[i]]$lon), " total lon points ...")
                    message("      before range(dims[[i]]$lon) = ", appendLF=F)
                    print(range(dims[[i]]$lon))
                    dims[[i]]$lon <- dims[[i]]$lon[lon_inds]
                    message("      after range(dims[[i]]$lon) = ", appendLF=F)
                    print(range(dims[[i]]$lon))
                    dims[[i]]$lon_inds <- lon_inds
                } else {
                    message("      use all ", length(dims[[i]]$lon), " lon points ...")
                }
            } else {
                if (length(lon_inds) == 0) {
                    stop("lon subset is of length 0")
                }
            }
        } # if lon lims are given as rectangular box and lon dim is available
        
        # case 1: lats
        if (!is.null(regboxes[[i]]$lats) && length(regboxes[[i]]$lats) == 2 && any(names(dims[[i]]) == "lat")) {
            message("   `regboxes[[", i, "]]$lats` of length 2 are given -> find lat inds between min/max(regboxes[[", i, "]]$lats) = ", 
                    min(regboxes[[i]]$lat), "/", max(regboxes[[i]]$lat), " deg lat ...")
            lat_inds <- which(dims[[i]]$lat >= regboxes[[i]]$lats[1] & dims[[i]]$lat <= regboxes[[i]]$lats[2])
            if (length(lat_inds) > 0) {
                if (length(lat_inds) != length(dims[[i]]$lat)) { 
                    message("      found lat subset of length ", length(lat_inds), " out of ", 
                            length(dims[[i]]$lat), " total lat points ...")
                    message("      before range(dims[[i]]$lat) = ", appendLF=F)
                    print(range(dims[[i]]$lat))
                    dims[[i]]$lat <- dims[[i]]$lat[lat_inds]
                    message("      after range(dims[[i]]$lat) = ", appendLF=F)
                    print(range(dims[[i]]$lat))
                    dims[[i]]$lat_inds <- lat_inds
                } else {
                    message("      use all ", length(dims[[i]]$lat), " lat points ...")
                }
            } else {
                if (length(lat_inds) == 0) {
                    stop("lat subset is of length 0")
                }
            }
        } # if case1: if lat lims are given as rectangular box and lat dim is available

        # case 2
        if (!is.null(regboxes[[i]]$lons) && !is.null(regboxes[[i]]$lats) && 
            length(regboxes[[i]]$lons) != 2 && length(regboxes[[i]]$lats) != 2 &&
            any(names(dims[[i]]) == "lon") && any(names(dims[[i]]) == "lat")) {
            if (length(regboxes[[i]]$lons) != length(regboxes[[i]]$lats)) {
                stop("given regboxes[[", i, "]]$lons (n=", length(regboxes[[i]]$lons), 
                     ") and regboxes[[", i, "]]$lats (n=", length(regboxes[[i]]$lats), 
                     ") are of different length")
            }
            message("   `regboxes[[", i, "]]$lons` and `regboxes[[", i, "]]$lats` both of length ", length(regboxes[[i]]$lons), 
                    " are given -> find lon/lat inds between min/max(regboxes[[", i, "]]$lons) = ", 
                    min(regboxes[[i]]$lon), "/", max(regboxes[[i]]$lon), " deg lon and min/max(regboxes[[", i, "]]$lats) = ", 
                    min(regboxes[[i]]$lat), "/", max(regboxes[[i]]$lat), " deg lat ...")
            stop("todo")
        } # if case 2

        # repeat for ll_data if necessary
        if (!is.null(ll_data[[i]][[1]]$lon) || !is.null(ll_data[[i]][[1]]$lat)) {
            message("\nadditional lon,lat data: repeat regboxes ...")

            # case 1: lons
            for (li in seq_along(ll_data[[i]])) {
                if (!is.null(regboxes[[i]]$lons) && length(regboxes[[i]]$lons) == 2 && any(names(ll_data[[i]][[li]]) == "lon")) {
                    message("   `regboxes[[", i, "]]$lons` of length 2 are given -> find lon inds between min/max(regboxes[[", i, "]]$lons) = ", 
                            min(regboxes[[i]]$lon), "/", max(regboxes[[i]]$lon), " deg lon ...")
                    lon_inds <- which(ll_data[[i]][[li]]$lon >= regboxes[[i]]$lons[1] & ll_data[[i]][[li]]$lon <= regboxes[[i]]$lons[2])
                    if (length(lon_inds) > 0) {
                        if (length(lon_inds) != length(ll_data[[i]][[li]]$lon)) { 
                            message("      found lon subset of length ", length(lon_inds), " out of ", 
                                    length(ll_data[[i]][[li]]$lon), " total lon points ...")
                            message("      before range(ll_data[[i]][[li]]$lon) = ", appendLF=F)
                            print(range(ll_data[[i]][[li]]$lon))
                            ll_data[[i]][[li]]$lon <- ll_data[[i]][[li]]$lon[lon_inds]
                            message("      after range(ll_data[[i]][[li]]$lon) = ", appendLF=F)
                            print(range(ll_data[[i]][[li]]$lon))
                            ll_data[[i]][[li]]$lon_inds <- lon_inds
                        } else {
                            message("      use all ", length(ll_data[[i]][[li]]$lon), " lon points ...")
                        }
                    } else {
                        if (length(lon_inds) == 0) {
                            stop("lon subset is of length 0")
                        }
                    }
                } # if lon lims are given as rectangular box and lon dim is available

                # case 1: lats
                if (!is.null(regboxes[[i]]$lats) && length(regboxes[[i]]$lats) == 2 && any(names(ll_data[[i]][[li]]) == "lat")) {
                    message("   `regboxes[[", i, "]]$lats` of length 2 are given -> find lat inds between min/max(regboxes[[", i, "]]$lats) = ", 
                            min(regboxes[[i]]$lat), "/", max(regboxes[[i]]$lat), " deg lat ...")
                    lat_inds <- which(ll_data[[i]][[li]]$lat >= regboxes[[i]]$lats[1] & ll_data[[i]][[li]]$lat <= regboxes[[i]]$lats[2])
                    if (length(lat_inds) > 0) {
                        if (length(lat_inds) != length(ll_data[[i]][[li]]$lat)) { 
                            message("      found lat subset of length ", length(lat_inds), " out of ", 
                                    length(ll_data[[i]][[li]]$lat), " total lat points ...")
                            message("      before range(ll_data[[i]][[li]]$lat) = ", appendLF=F)
                            print(range(ll_data[[i]][[li]]$lat))
                            ll_data[[i]][[li]]$lat <- ll_data[[i]][[li]]$lat[lat_inds]
                            message("      after range(ll_data[[i]][[li]]$lat) = ", appendLF=F)
                            print(range(ll_data[[i]][[li]]$lat))
                            ll_data[[i]][[li]]$lat_inds <- lat_inds
                        } else {
                            message("      use all ", length(ll_data[[i]][[li]]$lat), " lat points ...")
                        }
                    } else {
                        if (length(lat_inds) == 0) {
                            stop("lat subset is of length 0")
                        }
                    }
                } # if case1: if lat lims are given as rectangular box and lat dim is available

                # case 2
                if (!is.null(regboxes[[i]]$lons) && !is.null(regboxes[[i]]$lats) && 
                    length(regboxes[[i]]$lons) != 2 && length(regboxes[[i]]$lats) != 2 &&
                    any(names(ll_data[[i]][[li]]) == "lon") && any(names(ll_data[[i]][[li]]) == "lat")) {
                    if (length(regboxes[[i]]$lons) != length(regboxes[[i]]$lats)) {
                        stop("given regboxes[[", i, "]]$lons (n=", length(regboxes[[i]]$lons), 
                             ") and regboxes[[", i, "]]$lats (n=", length(regboxes[[i]]$lats), 
                             ") are of different length")
                    }
                    message("   `regboxes[[", i, "]]$lons` and `regboxes[[", i, "]]$lats` both of length ", length(regboxes[[i]]$lons), 
                            " are given -> find lon/lat inds between min/max(regboxes[[", i, "]]$lons) = ", 
                            min(regboxes[[i]]$lon), "/", max(regboxes[[i]]$lon), " deg lon and min/max(regboxes[[", i, "]]$lats) = ", 
                            min(regboxes[[i]]$lat), "/", max(regboxes[[i]]$lat), " deg lat ...")
                    stop("todo")
                } # if case 2
            } # for li in seq_along(ll_data[[i]])
        } # if !is.null(ll_data[[i]][[1]]$lon) && !is.null(ll_data[[i]][[1]]$lat) 
    } # if regbox is not NA
    # finished getting lon/lat inds

    # get depth inds
    if (any(names(dims[[i]]) == "depth")) {
        depth_fromsf[i] <- min(dims[[i]]$depth)
        depth_tosf[i] <- max(dims[[i]]$depth)
        if (is.na(depth_fromsp[i])) depth_fromsp[i] <- depth_fromsf[i]
        if (is.na(depth_tosp[i])) depth_tosp[i] <- depth_tosf[i]
        message("\nfind depth subsets from depth_fromsp[", i, "]=", 
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
    message("\nget variables ...")
    vars_per_file <- names(ncin$var)
    vars <- vector("list", l=ncin$nvars)
    var_infos <- vars
    for (vi in 1:length(vars)) {
        message(vi, "/", length(vars), ": \"", vars_per_file[vi], "\"", appendLF=F)
        
        # ignore variable
        if (any(ignore_vars == vars_per_file[vi])) {
            message(" --> this variable is included in `ignore_vars` --> ignore this variable ...")
            next # variable of this setting
        } else {
            message()
        }
        
        if (vars_per_file[vi] == paste0("var", codes[i])) {
            message("variable name of variable \"", vars_per_file[vi], 
                    "\" equals \"var`codes[", i, "]`\" = \"var", codes[i], 
                    "\". use `varnames_in[", i, "]` = \"", varnames_in[i], "\" from now on ...")
            names(vars)[vi] <- varnames_in[i]
        } else {
            names(vars)[vi] <- vars_per_file[vi]
        }
        vars[[vi]] <- ncvar_get(ncin, vars_per_file[vi], collapse_degen=squeeze) 

        # get infos of variable
        names(var_infos)[vi] <- names(vars)[vi]
        var_infos[[vi]] <- ncatt_get(ncin, vars_per_file[vi])

        # special: if evap, take abs values
        if (grepl("echam", models[i]) && vars_per_file[vi] == "evap") {
            message("special: use -1*evap instead of evap")
            vars[[vi]] <- -1*vars[[vi]]
            var_infos[[vi]]$long_name <- "evaporation*-1" 
        }

        # get dims of variable
        dimids <- ncin$var[[vars_per_file[vi]]]$dimids # get dims of data
        dimids <- dimids + 1 # nc dim ids start counting from zero
        if (squeeze) { # drop dims with len=1
            dim_lengths <- sapply(ncin$var[[vars_per_file[vi]]]$dim, "[", "len")
            names(dim_lengths) <- sapply(ncin$var[[vars_per_file[vi]]]$dim, "[", "name")
            if (any(dim_lengths == 1)) {
                len1_dim_inds <- which(dim_lengths == 1)
                message("`\nsqueeze=T` --> drop dims of length 1: \"", 
                        paste(names(len1_dim_inds), collapse="\", \""), "\" ...")
                dimids <- dimids[-len1_dim_inds]
                for (di in seq_along(len1_dim_inds)) {
                    dims[[i]][names(len1_dim_inds)[di]] <- NULL
                }
            } # if var has dims of length 1 
        } else {
            stop("not implemented")
        } # if squeeze
        attributes(vars[[vi]]) <- list(dim=dim(vars[[vi]]), dims=dims_per_setting_in[[i]][dimids])
        #cmd <- paste0("tmp <- list(", paste0(dims_per_setting_in[[i]][dimids], "=ncin$dim[[", dimids, "]]$vals", collapse=", "), ")")
    } # for vi nvars per setting
    
    # remove empty entries which were ignored during ncvar_get() above
    if (any(sapply(vars, is.null))) {
        if (all(sapply(vars, is.null))) {
            stop("removed all variables. is this really what you want?")
        }
        ignore_inds <- which(sapply(vars, is.null))
        vars <- vars[-ignore_inds]
        var_infos <- var_infos[-ignore_inds]
    }
    datas[[i]] <- vars
    data_infos[[i]] <- var_infos
    rm(vars, var_infos)

    # update dims per setting
    dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")

    # load additional lon,lat model matrix data if wanted
    if (!is.null(ll_data[[i]][[1]]$lon) && !is.null(ll_data[[i]][[1]]$lat)) {
        for (li in seq_along(ll_data[[i]])) {
            message("\nload `ll_vars[", li, "]` = \"", ll_vars[li], "\" ...")
            ll_data[[i]][[li]][[length(ll_data[[i]][[li]])+1]] <- ncvar_get(ll_ncs[[li]], ll_vars[li], 
                                                                            collapse_degen=squeeze)
            names(ll_data[[i]][[li]])[length(ll_data[[i]][[li]])] <- ll_vars[li]
    
            # get dims of variable
            ll_dims_per_setting_in <- names(ll_ncs[[li]]$dim)
            dimids <- ll_ncs[[li]]$var[[ll_vars[li]]]$dimids # get dims of data
            dimids <- dimids + 1 # nc dim ids start counting from zero
            if (squeeze) { # drop dims with len=1
                dim_lengths <- sapply(ll_ncs[[li]]$var[[ll_vars[li]]]$dim, "[", "len")
                names(dim_lengths) <- sapply(ll_ncs[[li]]$var[[ll_vars[li]]]$dim, "[", "name")
                if (any(dim_lengths == 1)) {
                    len1_dim_inds <- which(dim_lengths == 1)
                    message("`squeeze=T` --> drop dims of length 1: \"", 
                            paste(names(len1_dim_inds), collapse="\", \""), "\" ...")
                    dimids <- dimids[-len1_dim_inds]
                    for (di in seq_along(len1_dim_inds)) {
                        ll_data[[i]][[li]][names(len1_dim_inds)[di]] <- NULL
                    }
                } # if var has dims of length 1 
            } else {
                stop("not implemented")
            } # if squeeze
            attributes(ll_data[[i]][[li]][[ll_vars[li]]]) <- list(dim=dim(ll_data[[i]][[li]][[ll_vars[li]]]),
                                                                  dims=ll_dims_per_setting_in[dimids])
        } # for li in seq_along(ll_data[[i]])
    } # if !is.null(ll_data[[i]][[1]]$lon) && !is.null(ll_data[[i]][[1]]$lat)

    # load additional lon,lat model matrix data if wanted
    if (!is.null(poly_data[[i]][[1]]$lon) && !is.null(poly_data[[i]][[1]]$lat)) {
        for (li in seq_along(poly_data[[i]])) {
            message("\nload `poly_vars[[", li, "]][\"data\"]` = \"", poly_vars[[li]]["data"], "\" ...")
            poly_data[[i]][[li]]$data <- ncvar_get(poly_ncs[[li]], names(poly_ncs)[li])
        }
    }

    # special
    if (any(dims_per_setting == "time")) {
        vars_with_timedim_inds <- lapply(dims_per_setting, function(x) grep("time", x) != -1)
        vars_with_timedim_inds <- which(sapply(vars_with_timedim_inds, any))
        for (vi in seq_along(vars_with_timedim_inds)) {
            if (grepl("Hol-Tx10", prefixes[i])) {
                # special missing files                    raw         links               calendar
                # cosmos-aso-wiso_echam5_Hol-Tx10_main_mm: 334812      447 (x10->4471)     Dec 2530 BP
                # cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm: 334811      447 (x10->4471)     Nov 2530 BP
                #                                          334812      447 (x10->4471)     Dec 2530 BP
                # cosmos-aso-wiso_echam5_Hol-T_wiso_mm:    254906      5550                Jun 1450 BP
                if (grepl("main_mm", prefixes[i])) {
                    if (any(dims[[i]]$timelt$year + 1900 == -2530)) {
                        nainds <- which(dims[[i]]$timelt$year + 1900 == -2530)
                        message("\nspecial: set ", length(nainds), " time points from ", min(nainds), 
                                ":", max(nainds), " of incomplete year from ", 
                                dims[[i]]$timelt[min(nainds)], " to ", dims[[i]]$timelt[max(nainds)], 
                                " to NA...")
                        if (length(dim(datas[[i]][[vi]])) == 2) {
                            datas[[i]][[vi]][,nainds] <- NA
                        } else {
                            message("no dont do it")
                        }
                    }
                } else if (grepl("wiso_mm", prefixes[i])) {
                    if (any(dims[[i]]$timelt$year + 1900 == -2530)) {
                        nainds <- which(dims[[i]]$timelt$year + 1900 == -2530)
                        message("\nspecial: set ", length(nainds), " time points from ", min(nainds), 
                                ":", max(nainds), " of incomplete year from ", 
                                dims[[i]]$timelt[min(nainds)], " to ", dims[[i]]$timelt[max(nainds)], 
                                " to NA...")
                        if (length(dim(datas[[i]][[vi]])) == 1) {
                            message("no dont do it")
                            #datas[[i]][[vi]][nainds] <- NA
                        } else {
                            message("no dont do it")
                        }
                    }
                }
            } else if (grepl("Hol-T", prefixes[i])) {
                if (grepl("wiso_mm", prefixes[i])) {
                    if (any(dims[[i]]$timelt$year + 1900 == -1450)) {
                        nainds <- which(dims[[i]]$timelt$year + 1900 == -1450)
                        message("\nspecial: set ", length(nainds), " time points from ", min(nainds), 
                                ":", max(nainds), " of incomplete year from ", 
                                dims[[i]]$timelt[min(nainds)], " to ", dims[[i]]$timelt[max(nainds)], 
                                " to NA...")
                        if (length(dim(datas[[i]][[vi]])) == 1) {
                            message("no dont do it")
                            #datas[[i]][[vi]][nainds] <- NA
                        } else {
                            message("no dont do it")
                        }
                    }
                }
            }
        }
    } # special: set values to NA if files are missing

    # cut temporal subset from data if wanted
    if (!is.null(dims[[i]]$time_inds)) {
        message("\ncut subset from time dim (or months or seasons) ...")
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
                    cmd <- rep(",", t=length(dims_of_var))
                    cmd[timedimind] <- paste0("dims[[", i, "]]$time_inds")
                    cmd <- paste0("datas[[", i, "]][[", var_with_timedim_ind, "]] <- ",
                                  "datas[[", i, "]][[", var_with_timedim_ind, "]][", paste0(cmd, collapse=""))
                    #if (squeeze) {
                    #    cmd <- paste0(cmd, ",drop=T]")
                    #} else {
                    #    cmd <- paste0(cmd, ",drop=F]")
                    #}
                    cmd <- paste0(cmd, "]")
                    message("   run `", cmd, "` ...")
                    eval(parse(text=cmd))
                    # subsetting removed attributes, apply again
                    #if (squeeze && dim(datas[[i]][[var_with_timedim_ind]])[timedimind] == 1) {
                    #    attributes(datas[[i]][[var_with_timedim_ind]]) <- list(dim=dim(datas[[i]][[var_with_timedim_ind]]), 
                    #                                                           dims=dims_of_var[-timedimind])
                    #} else {
                        attributes(datas[[i]][[var_with_timedim_ind]]) <- list(dim=dim(datas[[i]][[var_with_timedim_ind]]), 
                                                                               dims=dims_of_var)
                    #}
                } # if time subset inds are of different length then the given time dim of the data 
            } # vi vars per file with time dim
            # update dims per setting
            #dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")
        } # if there are varbels with time dim
    } # finished cut temporal subset if wanted

    # cut depth subset from data if wanted
    if (!is.null(dims[[i]]$depth_inds)) {
        message("\ncut subset from depth dim ...")
        # check for variables that have depth dim
        vars_with_depthdim_inds <- lapply(dims_per_setting, function(x) grep("depth", x) != -1)
        vars_with_depthdim_inds <- which(sapply(vars_with_depthdim_inds, any))
        if (length(vars_with_depthdim_inds) > 0) {
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
            # update dims per setting
            #dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")
        } # if there are varbels with depth dim
    } # finished cut depth subset if wanted
    
    # reorder lons of data to (-180,...,180) if wanted and necessary
    if (any(names(dims[[i]]) == "lon_orig")) {
        # check for variables that have lon dim
        vars_with_londim_inds <- which(lapply(dims_per_setting, function(x) grep("lon", x)) == 1)
        if (length(vars_with_londim_inds) > 0) {
            message("\nreorder lon dim of data ...")
            if (!any(search() == "package:abind")) library(abind)
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
                    cmdeast[londimind] <- paste0("dims[[", i, "]]$east_of_180_inds")
                    cmdeast <- paste0(cmdeast, collapse="")
                    cmdwest <- rep(",", t=length(dims_of_var)) 
                    cmdwest[londimind] <- paste0("dims[[", i, "]]$west_of_180_inds")
                    cmdwest <- paste0(cmdwest, collapse="")
                    cmd <- paste0("datas[[", i, "]][[", var_with_londim_ind, "]] <- ",
                                  "abind(datas[[", i, "]][[", var_with_londim_ind, "]][", cmdeast, "], ",
                                        "datas[[", i, "]][[", var_with_londim_ind, "]][", cmdwest, "], ",
                                        "along=", londimind, ")")
                    message("  run `", cmd, "` ...")
                    eval(parse(text=cmd))
                    # restore attributes removed by subsetting
                    dimnames(datas[[i]][[var_with_londim_ind]]) <- NULL
                    attributes(datas[[i]][[var_with_londim_ind]]) <- list(dim=dim(datas[[i]][[var_with_londim_ind]]), 
                                                                          dims=dims_of_var)
                } # how many dims has the variable of whose dims one dim is "lon" 
            } # for vi vars per file with lon dim
        } # if any vars with lon dim
    } # if any(names(dims[[i]]) == "lon_orig") 
    # finished reorder lons of data to (-180,...,180) if wanted and necessary
    
    # reorder lons of ll_data to (-180,...,180) if wanted and necessary
    if (!is.null(ll_data[[i]][[1]]$lon)) {
        if (any(names(ll_data[[i]][[1]]) == "lon_orig")) {
            message("\nreorder lon dim of ll_data ...")
            if (!any(search() == "package:abind")) library(abind)
            for (li in seq_along(ll_data[[i]])) {
                ll_dims_of_var <- attributes(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]])$dims # e.g. "lon", "lat"
                londimind <- which(ll_dims_of_var == "lon")
                cmdeast <- rep(",", t=length(ll_dims_of_var)) 
                cmdeast[londimind] <- paste0("ll_data[[", i, "]][[", li, "]]$east_of_180_inds")
                cmdeast <- paste0(cmdeast, collapse="")
                cmdwest <- rep(",", t=length(ll_dims_of_var)) 
                cmdwest[londimind] <- paste0("ll_data[[", i, "]][[", li, "]]$west_of_180_inds")
                cmdwest <- paste0(cmdwest, collapse="")
                cmd <- paste0("ll_data[[", i, "]][[", li, "]]$", names(ll_data[[i]])[li], "  <- ",
                              "abind(ll_data[[", i, "]][[", li, "]]$", names(ll_data[[i]])[li], "[", cmdeast, "], ",
                                    "ll_data[[", i, "]][[", li, "]]$", names(ll_data[[i]])[li], "[", cmdwest, "], ",
                                    "along=", londimind, ")")
                message("  run `", cmd, "` ...")
                eval(parse(text=cmd))
                # restore attributes removed by subsetting
                dimnames(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]]) <- NULL
                attributes(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]]) <- list(dim=dim(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]]), 
                                                                                  dims=ll_dims_of_var)
            } # for li in ll_data[[i]]
        } # if any(names(ll_data[[i]][[1]]) == "lon_orig")
    } # if !is.null(ll_data[[i]][[1]]$lon)
    # finished reorder lons of ll_data to (-180,...,180) if wanted and necessary
    
    # flip lat dim of data necessary (needs to be increasing for plot)
    if (any(names(dims[[i]]) == "lat_orig")) {
        # check for variables that have lat dim
        vars_with_latdim_inds <- lapply(dims_per_setting, function(x) regexpr("lat", x) != -1)
        vars_with_latdim_inds <- which(sapply(vars_with_latdim_inds, any))
        if (length(vars_with_latdim_inds) > 0) {
            message("\nflip lat dim of data ...") 
            for (vi in 1:length(vars_with_latdim_inds)) {
                var_with_latdim_ind <- vars_with_latdim_inds[vi]
                dims_of_var <- attributes(datas[[i]][[var_with_latdim_ind]])$dims # e.g. "lon", "lat"
                latdimind <- which(dims_of_var == "lat")
                nlat_of_var <- dim(datas[[i]][[var_with_latdim_ind]])[latdimind]
                cmdlat <- rep(",", t=length(dims_of_var)) 
                cmdlat[latdimind] <- paste0(nlat_of_var, ":1")
                cmdlat <- paste0(cmdlat, collapse="")
                cmd <- paste0("datas[[", i, "]][[", var_with_latdim_ind, "]] <- ",
                              "datas[[", i, "]][[", var_with_latdim_ind, "]][", cmdlat, "]")
                message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
                
                # restore attributes removed by subsetting
                attributes(datas[[i]][[var_with_latdim_ind]]) <- list(dim=dim(datas[[i]][[var_with_latdim_ind]]), 
                                                                      dims=dims_of_var)
            } # for vi
        } # if any vars with lat dim
    } # finished flip data with lat dim if necessary (needs to be increasing for plot)
    
    # flip lat dim of ll_data necessary (needs to be increasing for plot)
    if (!is.null(ll_data[[i]][[1]]$lat)) {
        if (any(names(ll_data[[i]][[1]]) == "lat_orig")) {
            message("\nflip lat dim of ll_data ...") 
            for (li in seq_along(ll_data[[i]])) {
                ll_dims_of_var <- attributes(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]])$dims # e.g. "lon", "lat"
                latdimind <- which(ll_dims_of_var == "lat")
                nlat_of_var <- dim(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]])[latdimind]
                cmdlat <- rep(",", t=length(ll_dims_of_var)) 
                cmdlat[latdimind] <- paste0(nlat_of_var, ":1")
                cmdlat <- paste0(cmdlat, collapse="")
                cmd <- paste0("ll_data[[", i, "]][[", li, "]]$", names(ll_data[[i]])[li], " <- ",
                              "ll_data[[", i, "]][[", li, "]]$", names(ll_data[[i]])[li], "[", cmdlat, "]")
                message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
                
                # restore attributes removed by subsetting
                attributes(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]]) <- list(dim=dim(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]]), 
                                                                                  dims=ll_dims_of_var)
            } # for li in ll_data[[i]]
        } # if any(names(ll_data[[i]][[1]]) == "lat_orig")
    } # if !is.null(ll_data[[i]][[1]]$lon)
    # finished flip ll_data with lat dim if necessary (needs to be increasing for plot)
    
    # cut rectangular lon subset from data if wanted
    if (!is.null(dims[[i]]$lon_inds)) {
        # check for variables that have lon dim
        vars_with_londim_inds <- lapply(dims_per_setting, function(x) grep("lon", x) != -1)
        vars_with_londim_inds <- which(sapply(vars_with_londim_inds, any))
        if (length(vars_with_londim_inds) > 0) {
            message("\ncut data subset along lon dim ...")
            for (vi in 1:length(vars_with_londim_inds)) {
                var_with_londim_ind <- vars_with_londim_inds[vi]
                dims_of_var <- attributes(datas[[i]][[var_with_londim_ind]])$dims # e.g. "time", "lon"
                londimind <- which(dims_of_var == "lon")
                cmd <- rep(",", t=length(dims_of_var))
                cmd[londimind] <- paste0("dims[[", i, "]]$lon_inds")
                cmd <- paste0("datas[[", i, "]][[", var_with_londim_ind, "]] <- ",
                              "datas[[", i, "]][[", var_with_londim_ind, "]][", paste0(cmd, collapse=""), "]")
                message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
                # subsetting removed attributes, apply again
                attributes(datas[[i]][[var_with_londim_ind]]) <- list(dim=dim(datas[[i]][[var_with_londim_ind]]), 
                                                                      dims=dims_of_var)

            } # vi vars per file with lon dim
            # update dims per setting
            #dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")
        } # if there are variables with lon dim
    } # finished cut rectangular lon subset from data if wanted
    
    # cut rectangular lon subset from ll_data if wanted
    if (!is.null(ll_data[[i]][[1]]$lon)) {
        if (any(names(ll_data[[i]][[1]]) == "lon_inds")) {
            message("\ncut ll_data subset along lon dim ...")
            for (li in seq_along(ll_data[[i]])) {
                ll_dims_of_var <- attributes(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]])$dims # e.g. "lon", "lat"
                londimind <- which(ll_dims_of_var == "lon")
                cmd <- rep(",", t=length(ll_dims_of_var))
                cmd[londimind] <- paste0("ll_data[[", i, "]][[", li, "]]$lon_inds")
                cmd <- paste0("ll_data[[", i, "]][[", li, "]]$", names(ll_data[[i]])[li], " <- ",
                              "ll_data[[", i, "]][[", li, "]]$", names(ll_data[[i]])[li], "[", paste0(cmd, collapse=""), "]")
                message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
                # subsetting removed attributes, apply again
                attributes(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]]) <- list(dim=dim(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]]), 
                                                                                  dims=ll_dims_of_var)
            } # for li in ll_data[[i]]
        } # if ll_data has lon_inds
    } # if ll_data has lon
    # finished cut rectangular lon subset from ll_data if wanted
    
    # cut rectangular lat subset from data if wanted
    if (!is.null(dims[[i]]$lat_inds)) {
        # check for variables that have lat dim
        vars_with_latdim_inds <- lapply(dims_per_setting, function(x) grep("lat", x) != -1)
        vars_with_latdim_inds <- which(sapply(vars_with_latdim_inds, any))
        if (length(vars_with_latdim_inds) > 0) {
            message("\ncut data subset along lat dim ...")
            for (vi in 1:length(vars_with_latdim_inds)) {
                var_with_latdim_ind <- vars_with_latdim_inds[vi]
                dims_of_var <- attributes(datas[[i]][[var_with_latdim_ind]])$dims # e.g. "time", "lat"
                latdimind <- which(dims_of_var == "lat")
                cmd <- rep(",", t=length(dims_of_var))
                cmd[latdimind] <- paste0("dims[[", i, "]]$lat_inds")
                cmd <- paste0("datas[[", i, "]][[", var_with_latdim_ind, "]] <- ",
                              "datas[[", i, "]][[", var_with_latdim_ind, "]][", paste0(cmd, collapse=""), "]")
                message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
                # subsetting removed attributes, apply again
                attributes(datas[[i]][[var_with_latdim_ind]]) <- list(dim=dim(datas[[i]][[var_with_latdim_ind]]), 
                                                                      dims=dims_of_var)

            } # vi vars per file with lat dim
            # update dims per setting
            #dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")
        } # if there are variables with lat dim
    } # finished cut rectangular lat subset from data if wanted
    
    # cut rectangular lat subset from ll_data if wanted
    if (!is.null(ll_data[[i]][[1]]$lat)) {
        if (any(names(ll_data[[i]][[1]]) == "lat_inds")) {
            message("\ncut ll_data subset along lat dim ...")
            for (li in seq_along(ll_data[[i]])) {
                ll_dims_of_var <- attributes(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]])$dims # e.g. "lon", "lat"
                latdimind <- which(ll_dims_of_var == "lat")
                cmd <- rep(",", t=length(ll_dims_of_var))
                cmd[latdimind] <- paste0("ll_data[[", i, "]][[", li, "]]$lat_inds")
                cmd <- paste0("ll_data[[", i, "]][[", li, "]]$", names(ll_data[[i]])[li], " <- ",
                              "ll_data[[", i, "]][[", li, "]]$", names(ll_data[[i]])[li], "[", paste0(cmd, collapse=""), "]")
                message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
                # subsetting removed attributes, apply again
                attributes(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]]) <- list(dim=dim(ll_data[[i]][[li]][[names(ll_data[[i]])[li]]]), 
                                                                                  dims=ll_dims_of_var)
            } # for li in ll_data[[i]]
        } # if ll_data has lat_inds
    } # if ll_data has lat
    # finished cut rectangular lat subset from ll_data if wanted
    
    # flip data with depth dim if necessary (needs to be increasing for plot)
    if (any(names(dims[[i]]) == "depth")) {
        if (any(diff(dims[[i]]$depth) < 0)) {
            dims[[i]]$depth_orig <- dims[[i]]$depth
            dims[[i]]$depth <- rev(dims[[i]]$depth)
            # check for variables that have depth dim
            vars_with_depthdim_inds <- lapply(dims_per_setting, function(x) regexpr("depth", x) != -1)
            vars_with_depthdim_inds <- which(sapply(vars_with_depthdim_inds, any))
            if (length(vars_with_depthdim_inds) > 0) {
                message("\nvalues of depth dim are decreasing -> flip data along depth dim ...") 
                for (vi in 1:length(vars_with_depthdim_inds)) {
                    var_with_depthdim_ind <- vars_with_depthdim_inds[vi]
                    dims_of_var <- attributes(datas[[i]][[var_with_depthdim_ind]])$dims # e.g. "lat", "depth", "time"
                    depthdimind <- which(dims_of_var == "depth")
                    cmddepth <- rep(",", t=length(dims_of_var)) 
                    cmddepth[depthdimind] <- paste0("length(dims[[", i, "]]$depth):1")
                    cmddepth <- paste0(cmddepth, collapse="")
                    cmd <- paste0("datas[[", i, "]][[", var_with_depthdim_ind, "]] <- ",
                                  "datas[[", i, "]][[", var_with_depthdim_ind, "]][", cmddepth, "]")
                    message("   run `", cmd, "` ...")
                    eval(parse(text=cmd))
                    
                    # restore attributes removed by subsetting
                    #dimnames(datas[[i]][[var_with_depthdim_ind]]) <- NULL
                    attributes(datas[[i]][[var_with_depthdim_ind]]) <- list(dim=dim(datas[[i]][[var_with_depthdim_ind]]), 
                                                                            dims=dims_of_var)
                } # for vi
            } # if any vars with depth dim
        } # if depth values are not increasing
    } # finished flip data with depth dim if necessary (needs to be increasing for plot)

    # if two dims and one is time, make it x-dim
    if (any(names(dims[[i]]) == "time") && any(names(dims[[i]]) == "lat")) {
        vars_with_timedim_inds <- lapply(dims_per_setting, function(x) grep("time", x) != -1)
        vars_with_timedim_inds <- which(sapply(vars_with_timedim_inds, any))
        vars_with_latdim_inds <- lapply(dims_per_setting, function(x) regexpr("lat", x) != -1)
        vars_with_latdim_inds <- which(sapply(vars_with_latdim_inds, any))
        vars_with_timedim_and_latdim_inds <- intersect(vars_with_timedim_inds, vars_with_latdim_inds)
        if (length(vars_with_timedim_and_latdim_inds) > 0) {
            message("\ndetected variables with time and lat dims. check if ndims=2 and permute (lat x time) to (time x lat) if necessary ...") 
            for (vi in seq_along(vars_with_timedim_and_latdim_inds)) {
                var_with_timedim_and_latdim <- vars_with_timedim_and_latdim_inds[vi]
                dims_of_var <- attributes(datas[[i]][[var_with_timedim_and_latdim]])$dims # e.g. "lat", "depth", "time"
                if (length(dims_of_var) == 2) {
                    if (dims_of_var[1] == "lat" && dims_of_var[2] == "time") {
                        message("   aperm(datas[[", i, "]][[", var_with_timedim_and_latdim, "]], c(2, 1)) ...")
                        datas[[i]][[var_with_timedim_and_latdim]] <- aperm(datas[[i]][[var_with_timedim_and_latdim]], c(2, 1)) # permutate
                        attributes(datas[[i]][[var_with_timedim_and_latdim]]) <- list(dim=dim(datas[[i]][[var_with_timedim_and_latdim]]),
                                                             dims=dims_of_var[c(2, 1)])
                    } else {
                        # ndims=2 AND dims are already time x lat; nothing to do
                    }
                } else {
                    message("ndim(datas[[", i, "]][[", vi, "]]) = ", length(attributes(datas[[i]][[vi]])$dims), " != 2. skip.")
                }
            }
            # update dims per setting
            #dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")
        } # if variables with
    } # finished if two dims and one is time, make it x-dim

} # for i nsettings
message("\n****************** reading model data finished ***************************")

varnames_unique <- unique(as.vector(unlist(sapply(datas, names))))
if (exists("varnames_uv")) {
    varnames_uv_unique <- unique(as.vector(sapply(varnames_uv, unique)))
}

# save data before applying offset, multiplication factors, etc. for later
message("\nsave original data without multiplication factors or offsets or removal of a temporal mean etc. ...")
for (vi in 1:length(varnames_unique)) {
    cmd <- paste0(varnames_unique[vi], "_datas_orig <- list()")
    eval(parse(text=cmd))
    cnt <- 0
    for (i in 1:nsettings) {
        if (varnames_unique[vi] %in% names(datas[[i]])) { # if variables is present in setting
            cnt <- cnt + 1
            varind <- which(names(datas[[i]]) == varnames_unique[vi])
            cmd <- paste0(varnames_unique[vi], "_datas_orig[[", cnt, "]] <- datas[[", i, "]][[", varind, "]]")
            message("   run `", cmd, "` ...")
            eval(parse(text=cmd))
            cmd <- paste0("names(", varnames_unique[vi], "_datas_orig)[", cnt, "] <- names_short[", i, "]")
            #message("   run `", cmd, "` ...")
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
message("\nset variable specific things (define axis labels for specific variables here) ...")
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
        if (scale_ts) data_infos[[i]][[vi]]$label <- paste0(data_infos[[i]][[vi]]$label, " (Index)")

        # add variable-specific things
        data_infos[[i]][[vi]]$units_old <- data_infos[[i]][[vi]]$units
        
        if (any(varname == c("temp2", "tas", "t"))) {
            if (grepl("C", data_infos[[i]][[vi]]$units)) {
                message("detected a \"C\" in the `units` attribute of ", varname, 
                        " --> assume that data is already in deg C")
                data_infos[[i]][[vi]]$units <- "°C"
            } else {
                message("did not detect a \"C\" in the `units` attribute of ", varname, 
                        " --> assume that data is in K")
                data_infos[[i]][[vi]]$units <- "°C"
                data_infos[[i]][[vi]]$offset$operator <- "-"
                data_infos[[i]][[vi]]$offset$value <- 273.15
            }
        
            if (any(varname == c("t"))) {
                data_infos[[i]][[vi]]$label <- expression(paste("T [°C]"))
                if (scale_ts) data_infos[[i]][[vi]]$label <- expression(paste("T (Index)"))
                data_infos[[i]][[vi]]$label <- expression(paste("st [°C]"))
                if (scale_ts) data_infos[[i]][[vi]]$label <- expression(paste("st (Index)"))
            
            } else if (any(varname == c("temp2", "tas"))) {
                data_infos[[i]][[vi]]$label <- expression(paste("T"["2m"], " [°C]"))
                if (T && varname == "temp2") {
                    data_infos[[i]][[vi]]$label <- expression(paste("temp2 [°C]"))
                    if (scale_ts) data_infos[[i]][[vi]]$label <- expression(paste("temp2 (Index)"))
                }
                if (T && varname == "tas") {
                    data_infos[[i]][[vi]]$label <- expression(paste("tas [°C]"))
                    if (scale_ts) data_infos[[i]][[vi]]$label <- expression(paste("tas (Index)"))
                }
                if (!is.na(remove_mean_froms[i])) {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("T"["2m"], " anomaly wrt ", range, " [°C]")),
                                                list(range=paste(unique(c(remove_mean_froms[i], 
                                                                          remove_mean_tos[i])), collapse="-"))))
                }
                if (!is.na(remove_setting)) {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("T"["2m"], " anomaly wrt ", set, " [°C]")),
                                                list(set=remove_setting)))
                }
                if (F) { # anomaly:
                    message("*** special label ***")
                    data_infos[[i]][[vi]]$label <- expression(paste("T"["2m"], " anomaly [°C]"))
                }
            }
        
        } else if (varname == "tsurf") {
            data_infos[[i]][[vi]]$label <- expression(paste("T"["surf"], " [°C]"))
        
        } else if (varname == "aprt") {
            data_infos[[i]][[vi]]$label <- expression(paste("P"["total"], " [mm/month]"))
        
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
            #data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O precip [‰]"))
            data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O"["p,SMOW"], " [\u2030]"))
            #encoding <- get_encoding("‰") # does not work yet
            if (p$plot_type == "pdf") encoding <- "CP1250"

            if (scale_ts) {
                if (T) {
                    message("special diatom label")
                    data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O diatom/precip (Index)"))
                } else {
                    data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O precip (Index)"))
                }
            } else {
                if (!is.na(remove_mean_froms[i])) {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(delta^{18}, "O anomaly wrt ", fromto, " [‰]")),
                                                                   list(fromto=paste(unique(remove_mean_froms[i], remove_mean_tos[i]),
                                                                                     collapse="-"))))
                }
            }
        
        } else if (varname == "lm_wisoaprt_d_post_as_time_slope") {
            #data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(delta^{18}, "O"[p,SMOW], " trend [‰/7k years]"))))
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(delta^{18}, "O"["p,SMOW"], " trend [\u2030/7k years]"))))
            data_infos[[i]][[vi]]$units <- "o/oo/7k years"
            if (p$plot_type == "pdf") encoding <- "CP1250"
            if (T) {
                message("special unit")
                data_infos[[i]][[vi]]$offset$operator <- "*"
                data_infos[[i]][[vi]]$offset$value <- "6/7" # permil/7k years --> permil/6k years
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(delta^{18}, "O"["p,SMOW"], " trend [\u2030/6k years]"))))
                data_infos[[i]][[vi]]$units <- "o/oo/6k years"
            }

        } else if (varname == "wisoevap_d") {
            data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O evaporation (‰)"))
            if (scale_ts) {
                data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O evaporation (Index)"))
            }
            if (p$plot_type == "pdf") encoding <- "CP1250"

        } else if (varname == "wisope_d") {
            data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O (P-E) (‰)"))
            if (scale_ts) {
                data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O (P-E) (Index)"))
            }
            if (p$plot_type == "pdf") encoding <- "CP1250"
        
        } else if (varname == "lm_temp2_as_time_slope") {
            data_infos[[i]][[vi]]$label <- expression(paste("T"["2m"], " trend [°C/7k years]"))
            data_infos[[i]][[vi]]$units <- "°C/7k years"
            if (F) {
                message("special: *-1")
                data_infos[[i]][[vi]]$offset$operator <- "*"
                data_infos[[i]][[vi]]$offset$value <- -1
                data_infos[[i]][[vi]]$label <- expression(paste("T"["2m"], " trend 7ka - PI [°C]"))
                data_infos[[i]][[vi]]$units <- "°C"
            }
            if (T) {
                message("special unit")
                data_infos[[i]][[vi]]$offset$operator <- "*"
                data_infos[[i]][[vi]]$offset$value <- "6/7" # °C/7k years --> °C/6k years
                data_infos[[i]][[vi]]$label <- expression(paste("T"["2m"], " trend [°C/6k years]"))
                data_infos[[i]][[vi]]$units <- "°C/6k years"
            }
        
        } else if (varname == "lm_tsurf_as_time_slope") {
            data_infos[[i]][[vi]]$label <- expression(paste("T"["surf"], " trend [°C/7k years]"))
            data_infos[[i]][[vi]]$units <- "°C/7k years"
            if (T) {
                message("special unit")
                data_infos[[i]][[vi]]$offset$operator <- "*"
                data_infos[[i]][[vi]]$offset$value <- "6/7" # °C/7k years --> °C/6k years
                data_infos[[i]][[vi]]$label <- expression(paste("T"["surf"], " trend [°C/6k years]"))
                data_infos[[i]][[vi]]$units <- "°C/6k years"
            }
        
        } else if (varname == "lm_THO_as_time_slope") {
            if (all(levs == 6)) {
                data_infos[[i]][[vi]]$label <- "SST trend [°C/7k years]"
            } else {
                data_infos[[i]][[vi]]$label <- "potential temperature trend [°C/7k years]"
            }
        
        } else if (varname == "lm_SICOMO_as_time_slope") {
            data_infos[[i]][[vi]]$label <- "sea ice fraction trend [fraction/7k years]"

        } else if (varname == "lm_aprt_as_time_slope") {
            data_infos[[i]][[vi]]$label <- expression(paste("P"["total"], " trend [mm/month/7k years]"))
            data_infos[[i]][[vi]]$units <- "mm/month 7k years"
            if (T) {
                message("special unit")
                data_infos[[i]][[vi]]$offset$operator <- c("*", "*")
                data_infos[[i]][[vi]]$offset$value <- c(12, "6/7") # mm/month/7k years -> mm/year/6k years
                data_infos[[i]][[vi]]$label <- expression(paste("P"["total"], " trend [mm/year/6k years]"))
                data_infos[[i]][[vi]]$units <- "mm/month 6k years"
            }

        } else if (varname == "lm_act_fpc_as_time_slope") {
            if (all(levs == "sum1-4lev")) {
                data_infos[[i]][[vi]]$label <- "Forest trend [fraction/7k years]"
            } else {
                data_infos[[i]][[vi]]$label <- "fractional plant cover trend [fraction/7k years]"
            }
        
        } else if (varname == "lm_albedo_as_time_slope") {
            data_infos[[i]][[vi]]$label <- "Albedo trend [fraction/7k years]"
        
        } else if (varname == "SICOMO") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 6+6 # 1x10^6: m2 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIA [km"^2, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))

        } else if (varname == "c204_ICEARE_GLO") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 6+6 # 1x10^6: m2 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIA global [km"^2, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))

        } else if (varname == "c205_ICEVOL_GLO") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 9+4 # 1x10^9: m3 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIV global [km"^3, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))
   
        } else if (varname == "c64_ICEARE_ARC") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 6+6 # 1x10^6: m2 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIA Arctic [km"^2, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))

        } else if (varname == "c65_ICEVOL_ARC") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 9+4 # 1x10^9: m3 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIV Arctic [km"^3, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))
       
        } else if (any(varname == c("c46_HFL_GIN", "c86_HFL_LAB"))) {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 13
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Heat flux into ocean [W " %*% " ", 
                                                                            10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))
        
        } else if (any(varname == c("c47_WFL_GIN", "c87_WFL_LAB"))) {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 3
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Freshwater flux into ocean [m"^3, 
                                                                            " s"^paste(-1), " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))

        } else if (any(varname == c("c208_SST_GLO", "c210_T200_GLO", "c212_T700_GLO", "c214_T2200_GLO",
                                    "c128_SST_ATL", "c130_T200_ATL", "c132_T700_ATL", "c134_T2200_ATL",
                                    "cSST_GIN", "c50_T200_GIN", "c52_T700_GIN", "c54_T2200_GIN",
                                    "c88_SST_LAB", "c90_T200_LAB", "c92_T700_LAB", "c94_T2200_LAB"))) {
            data_infos[[i]][[vi]]$label <- "Potential temperature [°C]"

        } else if (any(varname == c("c209_SSS_GLO", "c211_S200_GLO", "c213_S700_GLO", "c215_S2200_GLO",
                                    "c129_SSS_ATL", "c131_S200_ATL", "c133_S700_ATL", "c135_S2200_ATL",
                                    "c49_SSS_GIN", "c51_S200_GIN", "c53_S700_GIN", "c55_S2200_GIN",
                                    "c89_SSS_LAB", "c91_S200_LAB", "c93_S700_LAB", "c95_S2200_LAB"))) {
            data_infos[[i]][[vi]]$label <- "Salinity [psu]"
        
        } else if (varname == "c44_ICEARE_GIN") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 6+5 # 1x10^6: m2 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIA GIN Sea [km"^2, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))

        } else if (varname == "c45_ICEVOL_GIN") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 9+2 # 1x10^9: m3 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIV GIN Sea [km"^3, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))
        
        } else if (varname == "c84_ICEARE_LAB") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 6+5 # 1x10^6: m2 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIA LSea [km"^2, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))

        } else if (varname == "c85_ICEVOL_LAB") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 9+2 # 1x10^9: m3 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIV LSea [km"^3, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))
  
        } else if (varname == "c144_ICEARE_SO") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 6+6 # 1x10^6: m2 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIA Southern Ocean [km"^2, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))

        } else if (varname == "c145_ICEVOL_SO") {
            data_infos[[i]][[vi]]$offset$operator <- "/"
            data_infos[[i]][[vi]]$offset$power <- 9+3 # 1x10^9: m3 --> km2
            data_infos[[i]][[vi]]$offset$value <- 10^data_infos[[i]][[vi]]$offset$power
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("SIV Southern Ocean [km"^3, 
                                                                            " " %*% " ", 10^power, "]")),
                                                           list(power=data_infos[[i]][[vi]]$offset$power)))
        
        } else if (any(varname == c("qu", "qv", "quv"))) {
            if (grepl("int", levs[i])) { # vertical integral
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(g^-1, integral(), bold("u")[h], 
                                                                                "q dp [kg ", m^-1, " ", s^-1, "]")), 
                                                               list(g="g", m="m", s="s")))
            } else {
                stop("update")
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(bold("u")[h], 
                                                                                "q [kg ", unit1^-1, " ", unit2^-1, "]")), 
                                                               list(unit1="m", unit2="s")))
            }

        } else if (varname == "quv_direction") {
            data_infos[[i]][[vi]]$label <- "direction of water vapor transport [°]"

        } # finished define variable specific things
    
    } # for vi varnames per setting
} # for i nsettings
# finished setting variable specific things


# save data infos for later
message("\nsave data infos ...")
for (vi in 1:length(varnames_unique)) {
    cmd <- paste0(varnames_unique[vi], "_infos <- list()")
    eval(parse(text=cmd))
    cnt <- 0
    for (i in 1:nsettings) {
        if (varnames_unique[vi] %in% names(datas[[i]])) { # if variables is present in setting
            cnt <- cnt + 1
            varind <- which(names(datas[[i]]) == varnames_unique[vi])
            cmd <- paste0(varnames_unique[vi], "_infos[[", cnt, "]] <- data_infos[[", i, "]][[", varind, "]]")
            message("   run `", cmd, "` ...")
            eval(parse(text=cmd))
            cmd <- paste0("names(", varnames_unique[vi], "_infos)[", cnt, "] <- names_short[", i, "]")
            #message("   run `", cmd, "` ...")
            eval(parse(text=cmd))
        } 
    }
} 


# apply offset or mult_fac
message("\napply variable specific things ...")
for (i in 1:nsettings) {
    for (vi in 1:length(datas[[i]])) {
        if (!is.null(data_infos[[i]][[vi]]$offset)) {
            for (oi in seq_along(data_infos[[i]][[vi]]$offset$operator)) {
                cmd <- paste0("datas[[", i, "]][[", vi, "]] <- datas[[", i, "]][[", vi, "]] ", 
                              data_infos[[i]][[vi]]$offset$operator[oi], " ", 
                              data_infos[[i]][[vi]]$offset$value[oi])
                message("eval ", cmd, " ...")
                eval(parse(text=cmd))
            }
        }
    }
} # for i nsettings


# remove setting mean
if (!is.na(remove_setting)) {

    remove_setting_ind <- which(names_short == remove_setting)
    if (length(remove_setting_ind) == 1) {
        message("\nremove setting \"", remove_setting, "\" mean from all other settings ...")
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
if (any(sapply(lapply(lapply(dims, names), "==", "time"), any))) {
    if (any(!is.na(remove_mean_froms)) || any(!is.na(remove_mean_tos))) {
        message("\nremove temporal means between")
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
                            # check if variable has time dim
                            dims_of_var <- attributes(datas[[i]][[vi]])$dims # e.g. "time", "lon", "lat"
                            timedimind <- which(dims_of_var == "time")
                            if (length(timedimind) == 1) {
                                message("      found ", length(time_inds), " time points from ", 
                                        min(dims[[i]]$time[time_inds]), " to ", max(dims[[i]]$time[time_inds]))
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
                                    message("      min/max mu = ", min(mu, na.rm=T), " / ", max(mu, na.rm=T))
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
                } else { # if any variable has time dim or not
                    stop("`remove_mean_froms[", i, "]` = \"", remove_mean_froms[i], 
                         "\" but data has not \"time\" dim.")
                } # if any variable has time dim or not
            } # if remove_mean_froms[i] is not NA
        } # for i nsettings
    } else {
        message("\n`remove_mean_froms` all NA --> do not remove temporal means ...")
    } # finished removing a temporal mean
} # if any data has time dim

# apply moving average in time --> datasma
n_mas_fname <- rep("", t=nsettings) # default
if (add_smoothed && 
    any(sapply(lapply(lapply(dims, names), "==", "time"), any)) &&
    #any(seasonsp == "Jan-Dec") && 
    !all(n_mas == 1)) {
    message("\nsome settings have \"time\" dim AND `add_smoothed` = T AND some `n_mas` != 1 --> apply moving averages ...")
    datasma <- datas
    for (i in seq_len(nsettings)) {
        message(i, "/", nsettings, ": ", names_short[i], " ...")
        #if (seasonsp[i] == "Jan-Dec" && n_mas[i] != 1) { # applying moving average
        if (n_mas[i] != 1) {
            for (vi in seq_along(datas[[i]])) { 
                dims_of_var <- attributes(datas[[i]][[vi]])$dims # e.g. "time", "lon", "lat"
                timedimind <- which(dims_of_var == "time")
                if (length(timedimind) == 1) {
                    # use number of time points of middle year (avoid possible incomplete start/ending of time series)
                    npy <- length(dims[[i]]$timelt$year[which(dims[[i]]$timelt$year == dims[[i]]$timelt$year[length(dims[[i]]$time)/2])])
                    n_mas_fname[i] <- paste0("_ma", round(n_mas[i]/npy), "yr") # years
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
                    message("variable \"", names(datas[[i]])[vi], "\" has no time dim ...")
                    datasma[[i]][[vi]] <- array(NA, dim=dim(datas[[i]][[vi]]))
                } # if variable has time dim or not
                
                attributes(datasma[[i]][[vi]]) <- attributes(datas[[i]][[vi]])
            
            } # for vi nvars
        } # if n_mas != 1    
    } # for i nsettings
} # if add_smoothed && any(attributes(datas[[i]][[vi]])$dims == "time") && !all(n_mas == 1)
if (!exists("datasma")) {
    message("\n`datasma` does not exist\n--> set `add_smoothed` = T ",
            "and/or `n_mas` not equal 1 in order to apply moving averages ...")
    add_smoothed <- F
}
# finished applying moving average


# calculate monthly and annual means
if (T && any(sapply(lapply(lapply(dims, names), "==", "time"), any))) { 

    message("\nsome settings have \"time\" dim --> calc monthly climatology and annual means ...")
    datasmon <- datasan <- datas # lazy declaration
    
    for (i in 1:nsettings) {
        message(i, "/", nsettings, ": ", names_short[i], " ...")
        for (vi in 1:length(datas[[i]])) { # for all vars per setting
            message("   var ", vi, "/", length(datas[[i]]), ": ", names(datas[[i]])[vi], " ...")
            if (any(attributes(datas[[i]][[vi]])$dims == "time")) { # if var has time dim
                
                # monthly climatology 
                months <- dims[[i]]$timelt$mon + 1
                months_unique <- sort(unique(months)) # sort here for plot later
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
                if (length(months_unique) == 1 || # whole time series has only one month --> var vs months has no sense
                    length(months_unique) == length(months)) { # or every months occurs only once --> `datasmon` would yield the same as `datas`
                    if (length(months_unique) == 1) {
                        message("   whole time series consists of only 1 month: ", months_unique, " --> do not calc monthly climatology")
                    } else {
                        message("   every month of the time series occurs only once --> do not calc monthly climatology")
                    }
                    datasmon[[i]][[vi]] <- NA
                } else {
                    message("mon ", appendLF=F)
                    for (mi in 1:length(months_unique)) {
                        time_inds <- which(months == months_unique[mi])
                        message(months_unique[mi], " (n=", length(time_inds), ") ", appendLF=F)
                        if (mi == length(months_unique)) message()
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
                } # if time series has only one month or more

                # annual means
                years <- dims[[i]]$timelt$year + 1900
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
                if (length(years_unique) == 1 || # whole time series has only one year --> var vs years has no sense
                    length(years_unique) == length(years)) {  # or every year occurs only once --> `datasan` would yield the same as `datas`
                    if (length(years_unique) == 1) {
                        message("   whole time series consists of only 1 year: ", years_unique, " --> do not calc annual means")
                    } else {
                        message("   every year of the time series occurs only once --> do not calc annual means")
                    }
                    datasan[[i]][[vi]] <- NA
                } else {
                    message("year ", appendLF=F)
                    for (yi in 1:length(years_unique)) {
                        time_inds <- which(years == years_unique[yi])
                        if (yi < 14 ||
                            yi >= (length(years_unique) - 14)) {
                            message(years_unique[yi], " (n=", length(time_inds), ") ", appendLF=F)
                            if (yi == 13) message("... ", appendLF=F)
                            if (yi == length(years_unique)) message()
                        }
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
                } # if time series has only 1 year or more

            } else { # variable has no time dim
                #message("variable has no \"time\" dim ...")
                datasmon[[i]][[vi]] <- NA
                datasan[[i]][[vi]] <- NA
            } # if variable has time dim or not

        } # for vi nvars
   
        # get monthly and annual time lims
        if (all(is.na(datasmon[[i]]))) {
            datasmon[[i]] <- NA
        } else {
            # remove NA entries
            navars <- which(is.na(datasmon[[i]]))
            if (length(navars) > 0) {
                datasmon[[i]][navars] <- NULL
            }
            dims[[i]]$month <- months_unique
            dims[[i]]$monmean_range <- paste0(fromsp[i], " to ", tosp[i])
        }
        if (all(is.na(datasan[[i]]))) {
            datasan[[i]] <- NA
        } else {
            # remove NA entries
            navars <- which(is.na(datasan[[i]]))
            if (length(navars) > 0) {
                datasan[[i]][navars] <- NULL
            }
            dims[[i]]$year <- years_unique
            dims[[i]]$yearmean_range <- dims[[i]]$monmean_range
        }

    } # for i nsettings
 
    if (all(is.na(datasmon))) {
        rm(datasmon)
    }
    if (all(is.na(datasan))) {
        rm(datasan)
    }

} else { # if any setting has time dim
    message("\ndo not calculate monthly and annual means ...")
}
# finished calculating monthly means if applicable

## calculate temporal mean (long term mean; ltm)
if (any(sapply(lapply(lapply(dims, names), "==", "time"), any))) {
    datasltm <- datas
    for (i in seq_len(nsettings)) {
        if (i == 1) message("\nsome settings have \"time\" dim --> calc long time means ...")
        ltm_range <- paste0(dims[[i]]$time[1], " to ", dims[[i]]$time[length(dims[[i]]$time)])
        message(i, "/", nsettings, ": ", names_short[i], " (", ltm_range, ") ...")
        for (vi in 1:length(datas[[i]])) {
            if (any(attributes(datas[[i]][[vi]])$dims == "time")) { # if var has time dim
                message("   var ", vi, "/", length(datas[[i]]), ": ", names(datas[[i]])[vi], " ...")
                dims_of_var <- attributes(datas[[i]][[vi]])$dims # e.g. "time", "lon", "lat"
                timedimind <- which(dims_of_var == "time")
                if (dim(datas[[i]][[vi]])[timedimind] > 1) {
                    apply_dims <- 1:length(dim(datas[[i]][[vi]]))
                    if (length(apply_dims) == 1) { # var has only time dim
                        datasltm[[i]][[vi]] <- mean(datas[[i]][[vi]], na.rm=T)
                        # make 1D array if only vector
                        if (is.null(dim(datasltm[[i]][[vi]]))) {
                            datasltm[[i]][[vi]] <- array(datasltm[[i]][[vi]])
                        }
                        attributes(datasltm[[i]][[vi]]) <- list(dim=1, dims="ltm_range")
                    } else {
                        apply_dims <- apply_dims[-timedimind]
                        cmd <- paste0("datasltm[[", i, "]][[", vi, "]] <- ",
                                      "apply(datas[[", i, "]][[", vi, "]], ",
                                      "c(", paste(apply_dims, collapse=","), "), ",
                                      "mean, na.rm=T)")
                        #message(cmd)
                        eval(parse(text=cmd))
                        # make 1D array if only vector
                        if (is.null(dim(datasltm[[i]][[vi]]))) {
                            datasltm[[i]][[vi]] <- array(datasltm[[i]][[vi]])
                        }
                        attributes(datasltm[[i]][[vi]])$dims <- attributes(datas[[i]][[vi]])$dims[-timedimind]
                    }
                } else { # time dim is of length 1 
                    datasltm[[i]][[vi]] <- NA
                }
            } else { # variable has not time dim
                datasltm[[i]][[vi]] <- NA
            } # if variable has time dim
        } # vi 
        dims[[i]]$ltm_range <- ltm_range
    } # i nsettings
    # remove NA entries
    for (i in seq_len(nsettings)) {
        navars <- which(is.na(datasltm[[i]]))
        if (length(navars) > 0) {
            datasltm[[i]][navars] <- NULL
        }
    }
    # if no ltm was calculated at all
    if (all(sapply(datasltm, length)) == 0) {
        message("--> temporal mean calculation was not necessary")
        rm(datasltm)
    }
} # if any var has time dim
# finished calculating temoral mean


## interpolate data if any dims is irregular
if (exists("datasltm")) {
    # todo
} # if exsits("datalam")
# finished interpolate if any dims is irregular


# save data after applying offset, multiplication factors, temporal mean or setting mean removal
message("\nsave data after application of multiplication factors or offsets of temporal mean or setting mean removal etc. ...")
for (vi in 1:length(varnames_unique)) {
    cmd <- paste0(varnames_unique[vi], "_dims <- list()")
    eval(parse(text=cmd))
    cmd <- paste0(varnames_unique[vi], "_datas <- list()")
    eval(parse(text=cmd))
    if (exists("datasmon")) {
        cmd <- paste0(varnames_unique[vi], "_datasmon <- list()")
        eval(parse(text=cmd))
    }
    if (exists("datasan")) {
        cmd <- paste0(varnames_unique[vi], "_datasan <- list()")
        eval(parse(text=cmd))
    }
    cnt <- 0
    for (i in 1:nsettings) {
        # save dims for later
        cmd <- paste0(varnames_unique[vi], "_dims[[", i, "]] <- dims[[", i, "]]")
        message("   run `", cmd, "` ...")
        eval(parse(text=cmd))
        cmd <- paste0("names(", varnames_unique[vi], "_dims)[", i, "] <- names_short[", i, "]")
        eval(parse(text=cmd))
        # save datas for later
        if (varnames_unique[vi] %in% names(datas[[i]])) { # if variables is present in setting
            cnt <- cnt + 1
            varind <- which(names(datas[[i]]) == varnames_unique[vi])
            cmd <- paste0(varnames_unique[vi], "_datas[[", cnt, "]] <- datas[[", i, "]][[", varind, "]]")
            message("   run `", cmd, "` ...")
            eval(parse(text=cmd))
            cmd <- paste0("names(", varnames_unique[vi], "_datas)[", cnt, "] <- names_short[", i, "]")
            #message("   run `", cmd, "` ...")
            eval(parse(text=cmd))
            if (exists("datasmon")) {
                cmd <- paste0(varnames_unique[vi], "_datasmon[[", cnt, "]] <- datasmon[[", i, "]][[", varind, "]]")
                message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
                cmd <- paste0("names(", varnames_unique[vi], "_datasmon)[", cnt, "] <- names_short[", i, "]")
                #message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
            }
            if (exists("datasan")) {
                cmd <- paste0(varnames_unique[vi], "_datasan[[", cnt, "]] <- datasan[[", i, "]][[", varind, "]]")
                message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
                cmd <- paste0("names(", varnames_unique[vi], "_datasan)[", cnt, "] <- names_short[", i, "]")
                #message("   run `", cmd, "` ...")
                eval(parse(text=cmd))
            }
        }  # if vari is present in settingi
    } # for i nsettings
} # for vi varnames_unique
# finished saving data for later


## plotting
# 1st: plot all vars with same name together (datas, datasma, datasmon, datasltm)
# 2nd: plot all vars with same dims together (datas, datasma, datasmon, datasltm)
# 3rd: <define>
plot_groups <- c("samevars", "samedims")
message("\n********************* prepare plot groups *********************")

if (F) { # test
    datas_save <- datas
    #datas <- datas_save
    datas[[1]]$myvar <- array(1:6, c(3,2))
    attributes(datas[[1]]$myvar) <- list(dim=c(3,2), dims=c("lon","lat"))
    dims[[1]]$lon=1:3
    dims[[1]]$lat=4:6
    datas[[1]]$myvar2 <- array(1:6, c(3,2))
    attributes(datas[[1]]$myvar2) <- list(dim=c(3,2), dims=c("lon","lit"))
    dims[[1]]$lit=7:9
    datas[[2]]$myvar3 <- array(1:12, c(2,3,4))
    attributes(datas[[2]]$myvar3) <- list(dim=c(2,3,4), dims=c("lon","lat","lit"))
    dims[[2]]$lon=0
    dims[[2]]$lat=1
    dims[[2]]$lit=2
    varnames_unique_save <- varnames_unique
    varnames_unique <- c(varnames_unique, "myvar", "myvar2", "myvar3")
}
    
# prepare temporary data matrices for plot (z, zuv, zma, zmon, zlm)
nplot_groups <- length(plot_groups)
for (plot_groupi in seq_len(nplot_groups)) {

    message("\nprepare \"", plot_groups[plot_groupi], "\" plots ...")

    # plot same vars together (datas, datasma, datasmon, datasan, datasltm)
    if (plot_groups[plot_groupi] == "samevars") { 

        # prepare datas plots same vars
        z_samevars <- vector("list", l=length(varnames_unique))
        names(z_samevars) <- varnames_unique
        if (exists("varnames_uv")) {
            zuv_samevars <- vector("list", l=length(varnames_uv_unique))
            names(zuv_samevars) <- varnames_uv_unique
            cnt_uv <- 0
        }
        if (exists("datasma")) zma_samevars <- z_samevars
        dinds_samevars <- vinds_samevars <- z_samevars
        for (vi in seq_along(varnames_unique)) {
            varname <- varnames_unique[vi]
            z <- dinds <- vinds <- list()
            if (exists("varnames_uv")) zuv <- z
            if (exists("datasma")) zma <- z
            cnt <- 0
            tmp <- list()
            for (i in seq_len(nsettings)) {
                varind <- which(names(datas[[i]]) == varname)
                if (length(varind) == 1) {
                    cnt <- cnt + 1
                    z[[cnt]] <- datas[[i]][[varind]]
                    names(z)[cnt] <- names_short[i]
                    dinds[[cnt]] <- i
                    vinds[[cnt]] <- varind
                    if (exists("varnames_uv") && varname %in% varnames_uv[[i]]) {
                        message("i ", i, ", vi ", vi, ", varind ", varind, ", varname ", varname)
                        zuv[[length(zuv)+1]] <- datas[[i]][[varind]]
                        names(zuv)[length(zuv)] <- names_short[i]
                    }
                    if (exists("datasma")) {
                        zma[[cnt]] <- datasma[[i]][[varind]]
                        names(zma)[cnt] <- names_short[i]
                    }
                } else {
                    #message("setting ", i, " \"", names_short[i], "\" does not have variable \"", varname, "\"")
                }
            } # for i nsettings
            z_samevars[[vi]] <- z
            dinds_samevars[[vi]] <- dinds
            vinds_samevars[[vi]] <- vinds
            if (exists("varnames_uv") && length(zuv) != 0) {
                cnt_uv <- cnt_uv + 1
                message("vi ", vi, ", cnt_uv ", cnt_uv)
                zuv_samevars[[cnt_uv]] <- zuv
            }
            if (exists("datasma")) zma_samevars[[vi]] <- zma
        } # vi in varnames_unique
        # dimensinons in same order as data 
        d_samevars <- vector("list", l=length(z_samevars))
        for (vi in seq_along(z_samevars)) {
            dimnames_of_settings <- lapply(lapply(z_samevars[[vi]], attributes), "[[", "dims")
            d <- list()
            for (si in seq_along(dimnames_of_settings)) {
                for (di in seq_along(dimnames_of_settings[[si]])) {
                    d[[dimnames_of_settings[[si]][di]]][[si]] <- dims[[dinds_samevars[[vi]][[si]]]][[dimnames_of_settings[[si]][di]]]
                }
            }
            d_samevars[[vi]] <- d
        }
        # finished datas plot preparation

        # prepare datasmon plots same vars
        if (exists("datasmon")) {
            varnames_unique_mon <- unique(as.vector(unlist(sapply(datasmon, names))))
            zmon_samevars <- vector("list", l=length(varnames_unique_mon))
            names(zmon_samevars) <- varnames_unique_mon
            dmoninds_samevars <- vmoninds_samevars <- zmon_samevars
            for (vi in seq_along(varnames_unique_mon)) {
                varname <- varnames_unique_mon[vi]
                zmon <- dinds <- vinds <- list()
                cnt <- 0
                for (i in seq_len(nsettings)) {
                    varind <- which(names(datasmon[[i]]) == varname)
                    if (length(varind) == 1) {
                        cnt <- cnt + 1
                        zmon[[cnt]] <- datasmon[[i]][[varind]]
                        names(zmon)[cnt] <- names_short[i]
                        dinds[[cnt]] <- i
                        vinds[[cnt]] <- varind
                    } else {
                        #message("setting ", names_short[i], " does not have variable \"", varname, "\"")
                    }
                }
                zmon_samevars[[vi]] <- zmon
                dmoninds_samevars[[vi]] <- dinds
                vmoninds_samevars[[vi]] <- vinds
            } # vi in varnames_unique_mon
            # dimensinons in same order as data 
            dmon_samevars <- vector("list", l=length(zmon_samevars))
            for (vi in seq_along(zmon_samevars)) {
                dimnames_of_settings <- lapply(lapply(zmon_samevars[[vi]], attributes), "[[", "dims")
                d <- list()
                for (si in seq_along(dimnames_of_settings)) {
                    for (di in seq_along(dimnames_of_settings[[si]])) {
                        d[[dimnames_of_settings[[si]][di]]][[si]] <- dims[[dmoninds_samevars[[vi]][[si]]]][[dimnames_of_settings[[si]][di]]]
                    }
                }
                dmon_samevars[[vi]] <- d
            }
        } # if exists("datasmon")
        # finished datasmon plot preparation 
        
        # prepare datasan plots same vars
        if (exists("datasan")) {
            varnames_unique_an <- unique(as.vector(unlist(sapply(datasan, names))))
            zan_samevars <- vector("list", l=length(varnames_unique_an))
            names(zan_samevars) <- varnames_unique_an
            daninds_samevars <- vaninds_samevars <- zan_samevars
            for (vi in seq_along(varnames_unique_an)) {
                varname <- varnames_unique_an[vi]
                zan <- dinds <- vinds <- list()
                cnt <- 0
                for (i in seq_len(nsettings)) {
                    varind <- which(names(datasan[[i]]) == varname)
                    if (length(varind) == 1) {
                        cnt <- cnt + 1
                        zan[[cnt]] <- datasan[[i]][[varind]]
                        names(zan)[cnt] <- names_short[i]
                        dinds[[cnt]] <- i
                        vinds[[cnt]] <- varind
                    } else {
                        #message("setting ", names_short[i], " does not have variable \"", varname, "\"")
                    }
                }
                zan_samevars[[vi]] <- zan
                daninds_samevars[[vi]] <- dinds
                vaninds_samevars[[vi]] <- vinds
            } # vi in varnames_unique_an
            # dimensinons in same order as data 
            dan_samevars <- vector("list", l=length(zan_samevars))
            for (vi in seq_along(zan_samevars)) {
                dimnames_of_settings <- lapply(lapply(zan_samevars[[vi]], attributes), "[[", "dims")
                d <- list()
                for (si in seq_along(dimnames_of_settings)) {
                    for (di in seq_along(dimnames_of_settings[[si]])) {
                        d[[dimnames_of_settings[[si]][di]]][[si]] <- dims[[daninds_samevars[[vi]][[si]]]][[dimnames_of_settings[[si]][di]]]
                    }
                }
                dan_samevars[[vi]] <- d
            }
        } # if exists("datasan")
        # finished datasan plot preparation 
        
        # prepare datasltm plots same vars
        if (exists("datasltm")) {
            varnames_unique_ltm <- unique(as.vector(unlist(sapply(datasltm, names))))
            zltm_samevars <- vector("list", l=length(varnames_unique_ltm))
            names(zltm_samevars) <- varnames_unique_ltm
            dltminds_samevars <- vltminds_samevars <- zltm_samevars
            for (vi in seq_along(varnames_unique_ltm)) {
                varname <- varnames_unique_ltm[vi]
                zltm <- dinds <- vinds <- list()
                cnt <- 0
                for (i in seq_len(nsettings)) {
                    varind <- which(names(datasltm[[i]]) == varname)
                    if (length(varind) == 1) {
                        cnt <- cnt + 1
                        zltm[[cnt]] <- datasltm[[i]][[varind]]
                        names(zltm)[cnt] <- names_short[i]
                        dinds[[cnt]] <- i
                        vinds[[cnt]] <- varind
                    } else {
                        #message("setting ", names_short[i], " does not have variable \"", varname, "\"")
                    }
                }
                zltm_samevars[[vi]] <- zltm
                dltminds_samevars[[vi]] <- dinds
                vltminds_samevars[[vi]] <- vinds
            } # vi in varnames_unique_ltm
            # dimensinons in same order as data 
            dltm_samevars <- vector("list", l=length(zltm_samevars))
            for (vi in seq_along(zltm_samevars)) {
                dimnames_of_settings <- lapply(lapply(zltm_samevars[[vi]], attributes), "[[", "dims")
                d <- list()
                for (si in seq_along(dimnames_of_settings)) {
                    for (di in seq_along(dimnames_of_settings[[si]])) {
                        d[[dimnames_of_settings[[si]][di]]][[si]] <- dims[[dltminds_samevars[[vi]][[si]]]][[dimnames_of_settings[[si]][di]]]
                    }
                }
                dltm_samevars[[vi]] <- d
            }
        } # if exists("datasltm")
        # finished datasltm plot preparation 

    # plot all vars with same dims together (datas, datasma, datasmon, datasan, datasltm)
    } else if (plot_groups[plot_groupi] == "samedims") { 
        
        # prepare `datas` samedims
        dnames <- list()
        cnt <- 0
        for (i in seq_along(datas)) { # for each setting
            for (vi in seq_along(datas[[i]])) { # for each variable of setting
                cnt <- cnt + 1
                dnames[[cnt]] <- attributes(datas[[i]][[vi]])$dims
            }
        }
        ndims <- sapply(dnames, length)
        ndims_unique <- unique(ndims)

        dnames_unique <- vector("list", l=length(ndims_unique))
        for (dimi in seq_along(ndims_unique)) {
            diminds <- which(ndims == ndims_unique[dimi]) # all variables with same number of dims
            dnames_array <- array(NA, dim=c(length(diminds), ndims_unique[dimi]))
            for (dimnamei in seq_along(diminds)) {
                dnames_array[dimnamei,] <- dnames[[diminds[dimnamei]]]
            }
            dnames_unique[[dimi]] <- unique(dnames_array)
        }

        z_samedims <- list()
        dinds_samedims <- vinds_samedims <- z_samedims
        if (exists("datasma")) zma_samedims <- z_samedims
        cnt_all <- 0
        for (dimleni in seq_along(dnames_unique)) {
            for (dimi in seq_along(dnames_unique[[dimleni]][,1])) {
                cnt_all <- cnt_all + 1
                message("\nprepare datas dimlength-dimname-combination ", cnt_all, ":\n",
                        "   number of dims = ", ndims_unique[dimleni], "\n",
                        "   name(s) of dim(s) = \"", 
                        paste(dnames_unique[[dimleni]][dimi,], collapse="\", \""), "\" ...")
                z <- d <- dinds <- vinds <- list()
                #if (exists("datasma")) zma <- z
                cnt <- 0
                for (i in seq_along(datas)) {
                    for (vi in seq_along(datas[[i]])) {
                        if (# variable belongs to current dimlengh-dimname combination
                            length(dim(datas[[i]][[vi]])) == ndims_unique[dimleni] &&
                            all(attributes(datas[[i]][[vi]])$dims == as.vector(dnames_unique[[dimleni]][dimi,]))
                            ) {
                            cnt <- cnt + 1
                            z[[cnt]] <- datas[[i]][[vi]]
                            names(z)[cnt] <- paste0(names(datas)[i], "_", names(datas[[i]])[vi]) 
                            # --> <names_short>_vs_<varnames_in> --> these names are used for the varnames_uv check in lon vs lat plots
                            dinds[[cnt]] <- i
                            vinds[[cnt]] <- vi
                            if (exists("datasma")) {
                                zma[[cnt]] <- datasma[[i]][[vi]]
                                names(zma)[cnt] <- paste0(names(datasma)[i], "_", names(datasma[[i]])[vi])
                            }
                        } # if variable belongs to current dimlengh-dimname combination
                    } # vi vars of setting
                } # i nsettings
                z_samedims[[cnt_all]] <- z
                names(z_samedims)[cnt_all] <- paste(dnames_unique[[dimleni]][dimi,], collapse="_vs_")
                dinds_samedims[[cnt_all]] <- dinds
                vinds_samedims[[cnt_all]] <- vinds
                if (exists("datasma")) zma_samedims[[cnt_all]] <- zma
            } # for dimi: all variables with same number of dims and whose dim(s) have the same name(s); e.g. all "time" or "lon","lat", etc.
        } # for dimleni: all variables with same number of dims
        d_samedims <- vector("list", l=length(z_samedims))
        names(d_samedims) <- names(z_samedims)
        for (ci in seq_along(z_samedims)) { # dimlength-dimname-combination
            dimnames_of_settings <- lapply(lapply(z_samedims[[ci]], attributes), "[[", "dims")
            d <- list()
            for (si in seq_along(dimnames_of_settings)) {
                for (di in seq_along(dimnames_of_settings[[si]])) {
                    d[[dimnames_of_settings[[si]][di]]][[si]] <- dims[[dinds_samedims[[ci]][[si]]]][[dimnames_of_settings[[si]][di]]]
                }
            }
            d_samedims[[ci]] <- d
        } # for ci dimlength-dimname-combination

        # prepare `datasmon` samedims
        if (exists("datasmon")) {
            dnames <- list()
            cnt <- 0
            for (i in seq_along(datasmon)) { # for each setting
                for (vi in seq_along(datasmon[[i]])) { # for each variable of setting
                    cnt <- cnt + 1
                    dnames[[cnt]] <- attributes(datasmon[[i]][[vi]])$dims
                }
            }
            ndims <- sapply(dnames, length)
            ndims_unique <- unique(ndims)

            dmonnames_unique <- vector("list", l=length(ndims_unique))
            for (dimi in seq_along(ndims_unique)) {
                diminds <- which(ndims == ndims_unique[dimi]) # all variables with same number of dims
                dnames_array <- array(NA, dim=c(length(diminds), ndims_unique[dimi]))
                for (dimnamei in seq_along(diminds)) {
                    dnames_array[dimnamei,] <- dnames[[diminds[dimnamei]]]
                }
                dmonnames_unique[[dimi]] <- unique(dnames_array)
            }

            zmon_samedims <- list()
            dmoninds_samedims <- vmoninds_samedims <- zmon_samedims
            cnt_all <- 0
            for (dimleni in seq_along(dmonnames_unique)) {
                for (dimi in seq_along(dmonnames_unique[[dimleni]][,1])) {
                    cnt_all <- cnt_all + 1
                    message("\nprepare datasmon dimlength-dimname-combination ", cnt_all, ":\n",
                            "   number of dims = ", ndims_unique[dimleni], "\n",
                            "   name(s) of dim(s) = \"", 
                            paste(dmonnames_unique[[dimleni]][dimi,], collapse="\", \""), "\" ...")
                    z <- d <- dinds <- vinds <- list()
                    cnt <- 0
                    for (i in seq_along(datasmon)) {
                        for (vi in seq_along(datasmon[[i]])) {
                            if (# variable belongs to current dimlengh-dimname combination
                                length(dim(datasmon[[i]][[vi]])) == ndims_unique[dimleni] &&
                                all(attributes(datasmon[[i]][[vi]])$dims == as.vector(dmonnames_unique[[dimleni]][dimi,]))
                                ) {
                                cnt <- cnt + 1
                                z[[cnt]] <- datasmon[[i]][[vi]]
                                names(z)[cnt] <- paste0(names(datasmon)[i], "_", names(datasmon[[i]])[vi])
                                vardims <- attributes(z[[cnt]])$dims
                                tmp <- list()
                                for (di in seq_along(vardims)) {
                                    tmp[[di]] <- dims[[i]][[vardims[di]]]
                                }
                                d[[cnt]] <- tmp
                                names(d)[cnt] <- names_short[i]
                                dinds[[cnt]] <- i
                                vinds[[cnt]] <- vi
                            } # if variable belongs to current dimlengh-dimname combination
                        } # vi vars of setting
                    } # i nsettings
                    zmon_samedims[[cnt_all]] <- z
                    names(zmon_samedims)[cnt_all] <- paste(dmonnames_unique[[dimleni]][dimi,], collapse="_")
                    dmoninds_samedims[[cnt_all]] <- dinds
                    vmoninds_samedims[[cnt_all]] <- vinds
                } # for dimi: all variables with same number of dims and whose dim(s) have the same name(s); e.g. all "time" or "lon","lat", etc.
            } # for dimleni: all variables with same number of dims
            dmon_samedims <- vector("list", l=length(zmon_samedims))
            names(dmon_samedims) <- names(zmon_samedims)
            for (ci in seq_along(zmon_samedims)) { # dimlength-dimname-combination
                dimnames_of_settings <- lapply(lapply(zmon_samedims[[ci]], attributes), "[[", "dims")
                d <- list()
                for (si in seq_along(dimnames_of_settings)) {
                    for (di in seq_along(dimnames_of_settings[[si]])) {
                        d[[dimnames_of_settings[[si]][di]]][[si]] <- dims[[dmoninds_samedims[[ci]][[si]]]][[dimnames_of_settings[[si]][di]]]
                    }
                }
                dmon_samedims[[ci]] <- d
            } # for ci dimlength-dimname-combination
        } # if (exists("datasmon"))
        
        # prepare `datasan` samedims
        if (exists("datasan")) {
            dnames <- list()
            cnt <- 0
            for (i in seq_along(datasan)) { # for each setting
                for (vi in seq_along(datasan[[i]])) { # for each variable of setting
                    cnt <- cnt + 1
                    dnames[[cnt]] <- attributes(datasan[[i]][[vi]])$dims
                }
            }
            ndims <- sapply(dnames, length)
            ndims_unique <- unique(ndims)

            dannames_unique <- vector("list", l=length(ndims_unique))
            for (dimi in seq_along(ndims_unique)) {
                diminds <- which(ndims == ndims_unique[dimi]) # all variables with same number of dims
                dnames_array <- array(NA, dim=c(length(diminds), ndims_unique[dimi]))
                for (dimnamei in seq_along(diminds)) {
                    dnames_array[dimnamei,] <- dnames[[diminds[dimnamei]]]
                }
                dannames_unique[[dimi]] <- unique(dnames_array)
            }

            zan_samedims <- list()
            daninds_samedims <- vaninds_samedims <- zan_samedims
            cnt_all <- 0
            for (dimleni in seq_along(dannames_unique)) {
                for (dimi in seq_along(dannames_unique[[dimleni]][,1])) {
                    cnt_all <- cnt_all + 1
                    message("\nprepare datasan dimlength-dimname-combination ", cnt_all, ":\n",
                            "   number of dims = ", ndims_unique[dimleni], "\n",
                            "   name(s) of dim(s) = \"", 
                            paste(dannames_unique[[dimleni]][dimi,], collapse="\", \""), "\" ...")
                    z <- d <- dinds <- vinds <- list()
                    cnt <- 0
                    for (i in seq_along(datasan)) {
                        for (vi in seq_along(datasan[[i]])) {
                            if (# variable belongs to current dimlengh-dimname combination
                                length(dim(datasan[[i]][[vi]])) == ndims_unique[dimleni] &&
                                all(attributes(datasan[[i]][[vi]])$dims == as.vector(dannames_unique[[dimleni]][dimi,]))
                                ) {
                                cnt <- cnt + 1
                                z[[cnt]] <- datasan[[i]][[vi]]
                                names(z)[cnt] <- paste0(names(datasan)[i], "_", names(datasan[[i]])[vi])
                                vardims <- attributes(z[[cnt]])$dims
                                tmp <- list()
                                for (di in seq_along(vardims)) {
                                    tmp[[di]] <- dims[[i]][[vardims[di]]]
                                }
                                d[[cnt]] <- tmp
                                names(d)[cnt] <- names_short[i]
                                dinds[[cnt]] <- i
                                vinds[[cnt]] <- vi
                            } # if variable belongs to current dimlengh-dimname combination
                        } # vi vars of setting
                    } # i nsettings
                    zan_samedims[[cnt_all]] <- z
                    names(zan_samedims)[cnt_all] <- paste(dannames_unique[[dimleni]][dimi,], collapse="_")
                    daninds_samedims[[cnt_all]] <- dinds
                    vaninds_samedims[[cnt_all]] <- vinds
                } # for dimi: all variables with same number of dims and whose dim(s) have the same name(s); e.g. all "time" or "lon","lat", etc.
            } # for dimleni: all variables with same number of dims        
            dan_samedims <- vector("list", l=length(zan_samedims))
            names(dan_samedims) <- names(zan_samedims)
            for (ci in seq_along(zan_samedims)) { # dimlength-dimname-combination
                dimnames_of_settings <- lapply(lapply(zan_samedims[[ci]], attributes), "[[", "dims")
                d <- list()
                for (si in seq_along(dimnames_of_settings)) {
                    for (di in seq_along(dimnames_of_settings[[si]])) {
                        d[[dimnames_of_settings[[si]][di]]][[si]] <- dims[[daninds_samedims[[ci]][[si]]]][[dimnames_of_settings[[si]][di]]]
                    }
                }
                dan_samedims[[ci]] <- d
            } # for ci dimlength-dimname-combination     
        } # if (exists("datasan")) 

        # prepare `datasltm` samedims
        if (exists("datasltm")) {   
            dnames <- list()
            cnt <- 0
            for (i in seq_along(datasltm)) { # for each setting
                for (vi in seq_along(datasltm[[i]])) { # for each variable of setting
                    cnt <- cnt + 1
                    dnames[[cnt]] <- attributes(datasltm[[i]][[vi]])$dims
                }
            }
            ndims <- sapply(dnames, length)
            ndims_unique <- unique(ndims)

            dltmnames_unique <- vector("list", l=length(ndims_unique))
            for (dimi in seq_along(ndims_unique)) {
                diminds <- which(ndims == ndims_unique[dimi]) # all variables with same number of dims
                dnames_array <- array(NA, dim=c(length(diminds), ndims_unique[dimi]))
                for (dimnamei in seq_along(diminds)) {
                    dnames_array[dimnamei,] <- dnames[[diminds[dimnamei]]]
                }
                dltmnames_unique[[dimi]] <- unique(dnames_array)
            }

            zltm_samedims <- list()
            dltminds_samedims <- vltminds_samedims <- zltm_samedims
            cnt_all <- 0
            for (dimleni in seq_along(dltmnames_unique)) {
                for (dimi in seq_along(dltmnames_unique[[dimleni]][,1])) {
                    cnt_all <- cnt_all + 1
                    message("\nprepare datasltm dimlength-dimname-combination ", cnt_all, ":\n",
                            "   number of dims = ", ndims_unique[dimleni], "\n",
                            "   name(s) of dim(s) = \"", 
                            paste(dltmnames_unique[[dimleni]][dimi,], collapse="\", \""), "\" ...")
                    z <- d <- dinds <- vinds <- list()
                    cnt <- 0
                    for (i in seq_along(datasltm)) {
                        for (vi in seq_along(datasltm[[i]])) {
                            if (# variable belongs to current dimlengh-dimname combination
                                length(dim(datasltm[[i]][[vi]])) == ndims_unique[dimleni] &&
                                all(attributes(datasltm[[i]][[vi]])$dims == as.vector(dltmnames_unique[[dimleni]][dimi,]))
                                ) {
                                cnt <- cnt + 1
                                z[[cnt]] <- datasltm[[i]][[vi]]
                                names(z)[cnt] <- paste0(names(datasltm)[i], "_", names(datasltm[[i]])[vi])
                                vardims <- attributes(z[[cnt]])$dims
                                tmp <- list()
                                for (di in seq_along(vardims)) {
                                    tmp[[di]] <- dims[[i]][[vardims[di]]]
                                }
                                d[[cnt]] <- tmp
                                names(d)[cnt] <- names_short[i]
                                dinds[[cnt]] <- i
                                vinds[[cnt]] <- vi
                            } # if variable belongs to current dimlengh-dimname combination
                        } # vi vars of setting
                    } # i nsettings
                    if (length(z) == 0) {
                        stop("sth strange happened with the dims of datasltm")
                    }
                    zltm_samedims[[cnt_all]] <- z
                    names(zltm_samedims)[cnt_all] <- paste(dltmnames_unique[[dimleni]][dimi,], collapse="_")
                    dltminds_samedims[[cnt_all]] <- dinds
                    vltminds_samedims[[cnt_all]] <- vinds
                } # for dimi: all variables with same number of dims and whose dim(s) have the same name(s); e.g. all "time" or "lon","lat", etc.
            } # for dimleni: all variables with same number of dims        
            dltm_samedims <- vector("list", l=length(zltm_samedims))
            names(dltm_samedims) <- names(zltm_samedims)
            for (ci in seq_along(zltm_samedims)) { # dimlength-dimname-combination
                dimnames_of_settings <- lapply(lapply(zltm_samedims[[ci]], attributes), "[[", "dims")
                d <- list()
                for (si in seq_along(dimnames_of_settings)) {
                    for (di in seq_along(dimnames_of_settings[[si]])) {
                        d[[dimnames_of_settings[[si]][di]]][[si]] <- dims[[dltminds_samedims[[ci]][[si]]]][[dimnames_of_settings[[si]][di]]]
                    }
                }
                dltm_samedims[[ci]] <- d
            } # for ci dimlength-dimname-combination     
        } # if (exists("datasltm"))

    } else { # every other plot_groupi
        
        stop("not defined")

    } # if plot_groupi == 1,2,...

} # for plot_groupi in nplot_groups:
# 1st: plot same vars together (datas, datasma, datasmon, datasan, datasltm)
# 2nd: plot all vars with same dims together (datas, datasma, datasmon, datasan, datasltm)

# test if z_samevars == z_samedims
if ("samevars" %in% plot_groups && "samedims" %in% plot_groups) {
    message("\ncheck if z_samevars == z_samedims ...")
    check_vec <- rep(F, t=length(z_samevars)) # default
    if (length(z_samevars) == length(z_samedims)) {
        for (checki in seq_along(z_samevars)) {
            if (length(z_samevars[[checki]]) == length(z_samedims[[checki]])) {
                check_vec[checki] <- T
                for (checkj in seq_along(z_samevars[[checki]])) {
                    if (!identical(z_samevars[[checki]][[checkj]], 
                                   z_samedims[[checki]][[checkj]])) {
                        check_vec[checki] <- F
                        next # entry to check
                    }
                }
            }
        }
    }

    # check data preparation for all groups and adjust names_legend/cols/ltys/etc. ...
    if (all(check_vec)) { # if z_samevars == z_samedims 

        message("--> z_samevars == z_samedims --> drop redundant plot_group \"samedims\" ...")
        plot_groups <- plot_groups[-which(plot_groups == "samedims")] # update plot_goups: drop redundant "samedims"

    } else { # z_samevars != z_samedims
        
        # check
        message("--> z_samevars != z_samedims --> keep plot_group \"samedims\" ...")
        if (!exists("varnames_out_samedims")) { # `varnames_out_samedims` not provided
            varnames_out_samedims <- rep(NA, t=length(z_samedims))
            for (combi in seq_along(varnames_out_samedims)) {
                varnames_out_samedims[combi] <- paste0("varname_for_", length(dim(z_samedims[[1]][[1]])), "d_variable_vs_",
                                                       paste(attributes(z_samedims[[1]][[1]])$dims, collapse="_"))
            }
            stop("provide `varnames_out_samedims` = ", 
                 ifelse(length(varnames_out_samedims) > 1, "c(", ""), 
                 "\"", paste(varnames_out_samedims, collapse="\", \""), "\"", 
                 ifelse(length(varnames_out_samedims) > 1, ")", ""), 
                 "` in namelist.plot.r for saving plots with vars of same dimlength-dimname-combinations")
        
        } else { # `varnames_out_samedims` is provided
            if (length(varnames_out_samedims) != length(z_samedims)) {
                stop("provided `varnames_out_samedims` = ",
                     ifelse(length(varnames_out_samedims) > 1, "c(", ""), 
                     "\"", paste(varnames_out_samedims, collapse="\", \""), "\"", 
                     ifelse(length(varnames_out_samedims) > 1, ")", ""), 
                     "` in namelist.plot.r for saving plots with vars of same dimlength-dimname-combinations is of different length than `z_samedims`: ", 
                     length(varnames_out_samedims), " != ", length(z_samedims))
            }
        } # if varnames_out_samedims is missing or not

        if (!exists("names_legend_samedims")) {
            names_legend_samedims <- seq_along(z_samedims)
        }

    } # if z_samevars == z_samedims or not
} # if z_samevars AND z_samedims are defined initially

nplot_groups <- length(plot_groups)
message("--> plot ", nplot_groups, " group", ifelse(nplot_groups > 1, "s", ""), ": \"",
        paste(plot_groups, collapse="\", \""), "\" ...")

message("\n********************* prepare plots *********************")
for (plot_groupi in seq_len(nplot_groups)) {
        
    if (plot_groups[plot_groupi] == "samevars") { # --> plot same vars together (datas, datasma, datasmon, datasan, datasltm)
        nplots <- length(z_samevars)
    } else if (plot_groups[plot_groupi] == "samedims") {
        nplots <- length(z_samedims)
    }

    for (ploti in seq_len(nplots)) { # either for all vars with same names or for all vars with same dims or ...
            
        message("\n========================================\nploti ", ploti, "/", nplots, 
                " in plot_groupi ", plot_groupi, "/", nplot_groups, 
                ": \"", plot_groups[plot_groupi], "\" ...\n",
                "========================================")

        # plot same vars together (datas, datasma, datasmon, datasan, datasltm)
        if (plot_groups[plot_groupi] == "samevars") { 
            varname <- varnames_unique[ploti]
            zname <- names(z_samevars)[ploti]
            z <- z_samevars[[ploti]]
            d <- d_samevars[[ploti]]
            dinds <- dinds_samevars[[ploti]]
            vinds <- vinds_samevars[[ploti]]
            # take varinfo of first found occurence of current variable
            data_info <- data_infos[[dinds_samevars[[ploti]][[1]]]][[vinds_samevars[[ploti]][[1]]]]
            names_legend_p <- names_legend[sapply(dinds, "[")]
            text_cols_p <- text_cols[sapply(dinds, "[")]
            cols_p <- cols[sapply(dinds, "[")]
            ltys_p <- ltys[sapply(dinds, "[")]
            if (exists("datasma")) zma <- zma_samevars[[ploti]]
            if (exists("datasmon")) {
                zname_mon <- names(zmon_samevars)[ploti]
                zmon <- zmon_samevars[[ploti]]
                dmon <- dmon_samevars[[ploti]]
                dmoninds <- dmoninds_samevars[[ploti]]
                vmoninds <- vmoninds_samevars[[ploti]]
                names_legend_pmon <- names_legend[sapply(dmoninds , "[")]
                text_cols_pmon <- text_cols[sapply(dmoninds, "[")]
            }
            if (exists("datasan")) {
                zname_an <- names(zan_samevars)[ploti]
                zan <- zan_samevars[[ploti]]
                dan <- dan_samevars[[ploti]]
                daninds <- daninds_samevars[[ploti]]
                vaninds <- vaninds_samevars[[ploti]]
                names_legend_pan <- names_legend[sapply(daninds , "[")]
                text_cols_pan <- text_cols[sapply(daninds, "[")]
            }
            if (exists("datasltm")) {
                zname_ltm <- names(zltm_samevars)[ploti]
                zltm <- zltm_samevars[[ploti]]
                dltm <- dltm_samevars[[ploti]]
                dltminds <- dltminds_samevars[[ploti]]
                vltminds <- vltminds_samevars[[ploti]]
                names_legend_pltm <- names_legend[sapply(dltminds , "[")]
                text_cols_pltm <- text_cols[sapply(dltminds, "[")]
            }
            
        # plot all vars with same dims together (datas, datasma, datasmon, datasan, datasltm)
        } else if (plot_groups[plot_groupi] == "samedims") { 
            varname <- varnames_out_samedims[ploti]
            zname <- names(z_samedims)[ploti]
            z <- z_samedims[[ploti]]
            d <- d_samedims[[ploti]]
            dinds <- dinds_samedims[[ploti]]
            vinds <- vinds_samedims[[ploti]]
            data_info <- data_infos[[dinds_samedims[[ploti]][[1]]]][[vinds_samedims[[ploti]][[1]]]]
            names_legend_p <- names_legend_samedims[sapply(dinds, "[")]
            if (!exists("cols_samedims")) {
                cols_p <- cols[sapply(dinds, "[")]
            } else {
                cols_p <- cols_samedims[sapply(dinds, "[")]
            }
            if (!exists("ltys_samedims")) {
                ltys_p <- ltys[sapply(dinds, "[")]
            } else {
                ltys_p <- ltys_samedims[sapply(dinds, "[")]
            }
            if (exists("datasma")) zma <- zma_samedims[[ploti]]
            if (exists("datasmon")) {
                zname_mon <- names(zmon_samedims)[ploti]
                zmon <- zmon_samedims[[ploti]]
                dmon <- dmon_samedims[[ploti]]
                dmoninds <- dmoninds_samedims[[ploti]]
                vmoninds <- vmoninds_samedims[[ploti]]
                names_legend_pmon <- names_legend_samedims[sapply(dmoninds , "[")]
            }
            if (exists("datasan")) {
                zname_an <- names(zan_samedims)[ploti]
                zan <- zan_samedims[[ploti]]
                dan <- dan_samedims[[ploti]]
                daninds <- daninds_samedims[[ploti]]
                vaninds <- vaninds_samedims[[ploti]]
                names_legend_pan <- names_legend_samedims[sapply(daninds , "[")]
            }
            if (exists("datasltm")) {
                zname_ltm <- names(zltm_samedims)[ploti]
                zltm <- zltm_samedims[[ploti]]
                dltm <- dltm_samedims[[ploti]]
                dltminds <- dltminds_samedims[[ploti]]
                vltminds <- vltminds_samedims[[ploti]]
                names_legend_pltm <- names_legend_samedims[sapply(dltminds , "[")]
            }

        } # which plot_group "samevars" "samedims"
        
        # temporary plot specs
        mode_p <- paste(sort(unique(modes[sapply(dinds, "[")])), collapse="_vs_")
        varnames_in_p <- gsub("_", "", varnames_in[sapply(dinds, "[")])
        names_short_p <- names_short[sapply(dinds, "[")]
        areas_p <- areas_out[sapply(dinds, "[")]
        seasonsp_p <- seasonsp[sapply(dinds, "[")]
        froms_plot_p <- froms_plot[sapply(dinds, "[")]
        tos_plot_p <- tos_plot[sapply(dinds, "[")]
        types_p <- types[sapply(dinds, "[")]
        cols_rgb_p <- rgb(t(col2rgb(cols_p)/255), alpha=alpha_rgb)
        lwds_p <- lwds[sapply(dinds, "[")]
        pchs_p <- pchs[sapply(dinds, "[")]
        n_mas_fname_p <- n_mas_fname[sapply(dinds, "[")]
        if (exists("datasmon")) {
            varnames_in_pmon <- gsub("_", "", varnames_in[sapply(dmoninds, "[")])
            names_short_pmon <- names_short[sapply(dmoninds , "[")]
            areas_pmon <- areas_out[sapply(dmoninds , "[")]
            seasonsp_pmon <- seasonsp[sapply(dmoninds , "[")]
            froms_plot_pmon <- froms_plot[sapply(dmoninds , "[")]
            tos_plot_pmon <- tos_plot[sapply(dmoninds , "[")]
            types_pmon <- types[sapply(dmoninds , "[")]
            cols_pmon <- cols[sapply(dmoninds , "[")]
            cols_rgb_pmon <- cols_rgb[sapply(dmoninds , "[")]
            ltys_pmon <- ltys[sapply(dmoninds , "[")]
            lwds_pmon <- lwds[sapply(dmoninds , "[")]
            pchs_pmon <- pchs[sapply(dmoninds , "[")]
        }
        if (exists("datasan")) {
            varnames_in_pan <- gsub("_", "", varnames_in[sapply(daninds, "[")])
            names_short_pan <- names_short[sapply(daninds , "[")]
            areas_pan <- areas_out[sapply(daninds , "[")]
            seasonsp_pan <- seasonsp[sapply(daninds , "[")]
            froms_plot_pan <- froms_plot[sapply(daninds , "[")]
            tos_plot_pan <- tos_plot[sapply(daninds , "[")]
            types_pan <- types[sapply(daninds , "[")]
            cols_pan <- cols[sapply(daninds , "[")]
            cols_rgb_pan <- cols_rgb[sapply(daninds , "[")]
            ltys_pan <- ltys[sapply(daninds , "[")]
            lwds_pan <- lwds[sapply(daninds , "[")]
            pchs_pan <- pchs[sapply(daninds , "[")]
        }
        if (exists("datasltm")) {
            varnames_in_pltm <- gsub("_", "", varnames_in[sapply(dltminds, "[")])
            names_short_pltm <- names_short[sapply(dltminds , "[")]
            areas_pltm <- areas_out[sapply(dltminds , "[")]
            seasonsp_pltm <- seasonsp[sapply(dltminds , "[")]
            froms_plot_pltm <- froms_plot[sapply(dltminds , "[")]
            tos_plot_pltm <- tos_plot[sapply(dltminds , "[")]
            types_pltm <- types[sapply(dltminds , "[")]
            cols_pltm <- cols[sapply(dltminds , "[")]
            cols_rgb_pltm <- cols_rgb[sapply(dltminds , "[")]
            ltys_pltm <- ltys[sapply(dltminds , "[")]
            lwds_pltm <- lwds[sapply(dltminds , "[")]
            pchs_pltm <- pchs[sapply(dltminds , "[")]
        }
       
        # dims and dimnames of current plot in current plot_group
        ndims <- length(dim(z[[1]]))
        dim_names <- attributes(z[[1]])$dims
        message("\nz_", plot_groups[plot_groupi], "[[", ploti, "/", nplots, "]]: \"", zname, 
                "\" has ", ndims, " dim", ifelse(ndims > 1, "s", ""), ": \"", 
                paste(dim_names, collapse="\", \""), "\". check if this case is defined ...")
       
        # append obs to `z_samevars` depending on available varnames/areas/etc.
        if (plot_groups[plot_groupi] == "samevars") {
            
            message("\ncheck for obs data that fit to specific dim-varname-area-etc. model data and append to z_samevars ...")
            if (ndims == 1 && dim_names == "time") {

                if (F && exists("era5_ts") && length(unique(areas)) == 1) {
                    message("\nload varname-and-area-specific era5 ts data. disable here if you do not want that.")
                    era5var <- NULL # default
                    if (zname == "quv") era5var <- "viwv"
                    if (zname == "quv_direction") era5var <- "viwv_direction"
                    if (!is.null(era5var)) {
                        era5ind <- which(grepl(paste0(era5var, "_", areas[1]), era5_ts$fs))
                    }
                    if (length(era5ind) == 1) {
                        message("load era5 ts data from \"", era5_ts$fs[era5ind], "\" ...")
                        era5_ncin <- nc_open(era5_ts$fs[era5ind])
                        tmp <- list()
                        tmp$f <- era5_ts$fs[era5ind]
                        tmp[[era5var]] <- ncvar_get(era5_ncin, era5var)
                        time <- era5_ncin$dim$time$vals
                        tmp$timelt <- as.POSIXlt(as.Date(time/24, origin="1990-1-1"), tz="UTC")
                        tmp$time <- as.POSIXct(tmp$timelt)
                        # apply season inds if necessary/possible
                        if (any(seasonsp != "Jan-Dec")) {
                            # get season inds of all settings
                            season_inds_unique <- lapply(dims, "[[", "season_inds")
                            season_inds_unique <- unique(unlist(season_inds_unique))
                            message("get season_inds ", paste(season_inds_unique, collapse=", "), " of era5 ts data ...")
                            season_inds <- tmp$timelt$mon + 1
                            if (any(season_inds %in% season_inds_unique)) {
                                season_inds <- which(season_inds %in% season_inds_unique)
                                tmp$time <- tmp$time[season_inds]
                                tmp$timelt <- tmp$timelt[season_inds]
                                tmp[[era5var]] <- tmp[[era5var]][season_inds]
                            } else {
                                message("-> none of these season_inds are included in era5_ts time -> cannot take seasonal subset.")
                            }
                        } # if any(seasnosp != "Jan-Dec")
                        # temporal smooth
                        if (exists("datasma")) {
                           tmp[[paste0(era5var, "_ma")]] <- filter(tmp[[era5var]], rep(1/era5_ts$n_ma, t=era5_ts$n_ma)) 
                        }
                        # annual means
                        tmp$year <- unique(tmp$timelt$year+1900)
                        tmp_an <- nmonths_an <- rep(NA, t=length(tmp$year))
                        for (yi in seq_along(tmp$year)) {
                            yinds <- which(tmp$timelt$year+1900 == tmp$year[yi])
                            tmp_an[yi] <- mean(tmp[[era5var]][yinds], na.rm=T)
                            nmonths_an[yi] <- length(yinds)
                        }
                        tmp[[paste0(era5var, "_an")]] <- tmp_an
                        tmp$nmonths_an <- nmonths_an
                        # monthly climatologies
                        tmp$month <- sort(unique(tmp$timelt$mon+1))
                        tmp_mon <- nyears_mon <- rep(NA, t=length(tmp$month))
                        for (mi in seq_along(tmp$month)) {
                            minds <- which(tmp$timelt$mon+1 == tmp$month[mi])
                            if (length(minds) > 0) {
                                tmp_mon[mi] <- mean(tmp[[paste0(era5var, "_an")]][minds], na.rm=T)
                                nyears_mon[mi] <- length(minds)
                            }
                        }
                        tmp[[paste0(era5var, "_mon")]] <- tmp_mon
                        tmp$nyears_mon <- nyears_mon
                        # temporal mean
                        tmp[[paste0(era5var, "_ltm")]] <- mean(tmp[[era5var]], na.rm=T)
                        tmp$ltm_range <- paste0(tmp$time[1], " to ", tmp$time[length(tmp$time)])
                        era5_ts$data[[era5var]] <- tmp
                        cat(capture.output(str(era5_ts$data)), sep="\n")
                    
                        # append era5 ts to data
                        z[[era5_ts$name]] <- era5_ts$data[[era5var]][[era5var]]
                        attributes(z[[era5_ts$name]])$dims <- "time"
                        d$time[[length(d$time)+1]] <- era5_ts$data[[era5var]]$time
                        names_legend_p <- c(names_legend_p, era5_ts$name)
                        text_cols_p <- c(text_cols_p, era5_ts$col)
                        cols_p <- c(cols_p, era5_ts$col)
                        ltys_p <- c(ltys_p, era5_ts$lty)
                        varnames_in_p <- c(varnames_in_p, era5var)
                        names_short_p <- c(names_short_p, era5_ts$name)
                        areas_p <- c(areas_p, areas_p[1])
                        seasonsp_p <- c(seasonsp_p, seasonsp_p[1])
                        froms_plot_p <- c(froms_plot_p, era5_ts$data[[era5var]]$timelt$year[1]+1900)
                        tos_plot_p <- c(tos_plot_p, era5_ts$data[[era5var]]$timelt$year[length(era5_ts$data[[era5var]]$timelt)]+1900)
                        types_p <- c(types_p, types_p[1])
                        cols_rgb_p <- rgb(t(col2rgb(cols_p)/255), alpha=alpha_rgb)
                        lwds_p <- c(lwds_p, era5_ts$lwd)
                        pchs_p <- c(pchs_p, era5_ts$pch)
                        if (exists("datasma")) {
                            zma[[era5_ts$name]] <- era5_ts$data[[era5var]][[paste0(era5var, "_ma")]]
                        }
                        if (exists("datasmon")) {
                            zmon[[era5_ts$name]] <- era5_ts$data[[era5var]][[paste0(era5var, "_mon")]]
                            attributes(zmon[[era5_ts$name]])$dims <- "month"
                            dmon$month[[length(dmon$month)+1]] <- era5_ts$data[[era5var]]$month
                            names_legend_pmon <- names_legend_p
                            text_cols_pmon <- text_cols_p
                            varnames_in_pmon <- varnames_in_p
                            names_short_pmon <- names_short_p
                            areas_pmon <- areas_p
                            seasonsp_pmon <- seasonsp_p
                            froms_plot_pmon <- froms_plot_p
                            tos_plot_pmon <- tos_plot_p
                            types_pmon <- types_p
                            cols_pmon <- cols_p
                            cols_rgb_pmon <- cols_rgb_p
                            ltys_pmon <- ltys_p
                            lwds_pmon <- lwds_p
                            pchs_pmon <- pchs_p
                        }
                        if (exists("datasan")) {
                            zan[[era5_ts$name]] <- era5_ts$data[[era5var]][[paste0(era5var, "_an")]]
                            attributes(zan[[era5_ts$name]])$dims <- "year"
                            dan$year[[length(dan$year)+1]] <- era5_ts$data[[era5var]]$year
                            names_legend_pan <- names_legend_p
                            text_cols_pan <- text_cols_p
                            varnames_in_pan <- varnames_in_p
                            names_short_pan <- names_short_p
                            areas_pan <- areas_p
                            seasonsp_pan <- seasonsp_p
                            froms_plot_pan <- froms_plot_p
                            tos_plot_pan <- tos_plot_p
                            types_pan <- types_p
                            cols_pan <- cols_p
                            cols_rgb_pan <- cols_rgb_p
                            ltys_pan <- ltys_p
                            lwds_pan <- lwds_p
                            pchs_pan <- pchs_p
                        }
                        if (exists("datasltm")) {
                            #todo
                            #zltm[[era5_ts$name]] <- era5_ts$data[[era5var]][[paste0(era5var, "_ltm")]]
                            #attributes(zltm[[era5_ts$name]])$dims <- "ltm_range"
                            #dltm$ltm_range[[length(dltm$year_range)+1]] <- era5_ts$data[[era5var]]$ltm_range
                        }

                        # append era5 ts to `datas`
                        cmd <- paste0(zname, "_datas[[length(", zname, "_datas)+1]] <- era5_ts$data[[\"", era5var, "\"]][[\"", era5var, "\"]]")
                        message("run `", cmd, "` ...")
                        eval(parse(text=cmd))
                        cmd <- paste0("names(", zname, "_datas)[length(", zname, "_datas)] <- \"", era5_ts$name, "\"")
                        message("run `", cmd, "` ...")
                        eval(parse(text=cmd))
                        cmd <- paste0("attributes(", zname, "_datas[[\"", era5_ts$name, "\"]])$dims <- \"time\"")
                        message("run `", cmd, "` ...")
                        eval(parse(text=cmd))

                    } else {
                        message("cannot add era5_ts data: must find 1 file of correct varname/area combination. check `era5_ts$fs`")
                    } # if era5_ts data fits to varname/area selection
                } # if exists("era5_ts")
            
            } else if (ndims == 2 && all(dim_names == c("lon", "lat"))) {
           
                # nothing yet

            } # which ndims-and-dim_names combination
            message("finished check for obs data that fit to specific dim-varname-area-etc. model data and append to z_samevars")
            
            # update dims and dimnames of current plot in current plot_group after adding obs
            ndims <- length(dim(z[[1]]))
            dim_names <- attributes(z[[1]])$dims
            message("\npotentially updated z_", plot_groups[plot_groupi], "[[", ploti, "/", nplots, "]]: \"", zname, 
                    "\" has ", ndims, " dim", ifelse(ndims > 1, "s", ""), ": \"", 
                    paste(dim_names, collapse="\", \""), "\". check if this case is defined ...")
        
        } # if plot_group == "samevars"  
        # finished appending obs to z_samevars
        
        # append obs to `z_samedims` depending on available varnames/areas/etc.
        if (plot_groups[plot_groupi] == "samedims") {
            
            message("\ncheck for obs data that fit to specific dim-varname-area-etc. model data and append to z_samedims ...")
            # nothing yet
        
            message("finished check for obs data that fit to specific dim-varname-area-etc. model data and append to z_samedims")
        
            # update dims and dimnames of current plot in current plot_group after adding obs
            #ndims <- length(dim(z[[1]]))
            #dim_names <- attributes(z[[1]])$dims
            #message("\npotentially updated z_", plot_groups[plot_groupi], "[[", ploti, "/", nplots, "]]: \"", zname, 
            #        "\" has ", ndims, " dim", ifelse(ndims > 1, "s", ""), ": \"", 
            #        paste(dim_names, collapse="\", \""), "\". check if this case is defined ...")
        
        } # if plot_group == "samedims"  
        # finished appending obs to z_samedims
        
        # get time axis values based on final d* lists
        if (any(sapply(lapply(z, attributes), "[[", "dims") == "time")) {
            ntime_per_setting <- sapply(d$time, length)
            if (any(ntime_per_setting > 1)) {
                message("\nsome settings have \"time\" dims and some are longer than 1 --> find pretty time axes labels ...")

                # time limits
                # -> POSIX will be converted to numeric by plot(), so use these numeric values as limits
                tlim <- range(lapply(d$time, as.numeric)) # seconds by default 
                tlimlt <- as.POSIXlt(tlim, origin="1970-1-1", tz="UTC")
                tlimct <- as.POSIXct(tlimlt)
                tlabcex <- 0.8
                tlabsrt <- 0
                monlim <- range(tlimlt$mon+1)
                anlim <- range(tlimlt$year+1900)

                # time labels
                tlablt <- as.POSIXlt(pretty(tlimlt, n=10)) # this does not work with large negative years, e.g. -800000 (800ka) 
                if (F) {
                    message("my special tlablt")
                    tlablt <- make_posixlt_origin(seq(-7000, 0, b=1000)) 
                }
                monat <- monlim[1]:monlim[2]
                monlab <- substr(month.abb[monat], 1, 1) # Jan -> J

                # remove lables which are possibly out of limits due to pretty
                tlab_diff_secs <- as.numeric(diff(range(tlablt)), units="secs") # duration of time labels
                if (any(tlablt < tlimlt[1])) {
                    if (F) { # remove pretty labels with overshoot > 1% of total time label range  
                        overshoot_diff <- abs(as.numeric(tlablt[tlablt < tlimlt[1]], units="secs") - tlim[1])
                        overshoot_rel <- overshoot_diff/tlab_diff_secs*100
                        if (any(overshoot_rel > 1)) { # todo: only change pretty labels if overshoot is > 1% of total time label range  
                            message("remove some automatic labels < ", tlimlt[1], " ...")
                            print(tlablt[which(tlablt < tlimlt[1])[overshoot_rel > 1]])
                            tlablt <- tlablt[-which(tlablt < tlimlt[1])[overshoot_rel > 1]]
                        }
                    } else if (F) { # remove pretty labels < total time label range
                        inds <- which(tlablt < tlimlt[1])
                        if (length(inds) == length(tlablt)) {
                            stop("removing pretty labels < tlimlt[1] would remove all labels")
                        } else {
                            tlablt <- tlablt[-inds]
                        }
                    } else if (T) { # remove pretty labels whose day & mon & year are != total time label range
                        inds <- which(tlablt < tlimlt[1])
                        inds <- inds[which(tlablt$mday[inds] != tlimlt[1]$mday & 
                                           tlablt$mon[inds] != tlimlt[1]$mon & 
                                           tlablt$year[inds] != tlimlt[1]$year)]
                        if (length(inds) > 0) {
                            if (length(inds) == length(tlablt)) {
                                stop("removing pretty labels < tlimlt[1] would remove all labels")
                            } else {
                                tlablt <- tlablt[-inds]
                            }
                        }
                    }
                }
                if (any(tlablt > tlimlt[2])) {
                    if (F) { # remove pretty labels with overshoot > 1% of total time label range  
                        overshoot_diff <- abs(as.numeric(tlablt[tlablt > tlimlt[2]], units="secs") - tlim[2])
                        overshoot_rel <- overshoot_diff/tlab_diff_secs*100
                        if (any(overshoot_rel > 1)) { 
                            message("remove some automatic labels > ", tlimlt[2], " ...")
                            print(tlablt[which(tlablt > tlimlt[2])[overshoot_rel > 1]])
                            tlablt <- tlablt[-which(tlablt > tlimlt[2])[overshoot_rel > 1]]
                        }
                    } else if (F) { # remove pretty labels > total time label range
                        inds <- which(tlablt > tlimlt[2])
                        if (length(inds) == length(tlablt)) {
                            stop("removing pretty labels > tlimlt[2] would remove all labels")
                        } else {
                            tlablt <- tlablt[-inds]
                        }
                    } else if (T) { # remove pretty labels whose day & mon & year are != total time label range
                        inds <- which(tlablt > tlimlt[2])
                        inds <- inds[which(tlablt$mday[inds] != tlimlt[2]$mday & 
                                           tlablt$mon[inds] != tlimlt[2]$mon & 
                                           tlablt$year[inds] != tlimlt[2]$year)]
                        if (length(inds) > 0) {
                            if (length(inds) == length(tlablt)) {
                                stop("removing pretty labels < tlimlt[1] would remove all labels")
                            } else {
                                tlablt <- tlablt[-inds]
                            }
                        }
                    }
                }
                if (length(tlablt) == 0) stop("removed all tlablt")
                tatn <- as.numeric(tlablt)
                
                # modify time axis labels YYYY-MM-DD depending on range covered:
                tunit <- "time"
                if (length(tlablt) > 1) {
                    tlab_dt_secs <- as.numeric(diff(range(tlablt[1:2])), units="secs") # duration of time labels
                    if (tlab_dt_secs >= 30*24*60*60) { # do not show days if dt >= 1 mon
                        tlablt <- paste0(tlablt$year+1900, "-", tlablt$mon+1) # YYYY; this destroys POSIX object
                        if (tlab_dt_secs >= 365*24*60*60) { # do not show months if dt >= 1 yr
                            tlablt <- tlablt$year+1900 # YYYY; this destroys POSIX object
                        }
                    } else { # decrease label size due to long labels
                        message("duration of time dim is shorter than 1 year --> ",
                                "change time label angle ...")
                        tlablt <- paste0(tlablt$year+1900, "-", tlablt$mon+1, "-", tlablt$mday) # YYYY; this destroys POSIX object
                        tlabsrt <- 45
                    }
                }
                message("final tlablt = ", paste(tlablt, collapse=", "))

                # if all dates < 0 AD, use "abs(dates) BP" instead
                if (all(anlim <= 0)) {
                    message("all times are <= 0 AD --> use `abs(times)` for time labels instead ...")
                    neg_inds <- which(tlablt < 0)
                    tlablt[neg_inds] <- abs(tlablt[neg_inds])
                    if (!is.na(time_ref)) {
                        message("`time_ref` = \"", time_ref, "\" is not NA --> modify `tunit` = \"", tunit, "\" ...")
                        tunit <- paste0("year before ", time_ref)
                    } else {
                        tunit <- "year before `time_ref`"
                    }
                }
                message("final tunit = \"", tunit, "\"")
                
                # use years from time for plots versus years 
                #anlab <- tlablt$year+1900
                #anat <- as.numeric(as.POSIXct(paste0(anlab, "-1-1"), o="1970-1-1", tz="UTC"))
                anlab <- tlablt
                anat <- tatn

            } # if any(ntime_per_setting > 1)

        } # if any data has time axis
        # finished getting time axis labels
     
        # todo: find common axes values for all dims 

        # verbose
        message("\nz:")
        cat(capture.output(str(z)), sep="\n")
        message("d:")
        cat(capture.output(str(d)), sep="\n")

        ## plot `datas` (`datas` always exists; no exists() check necessary)
        message("\n****************** plot datas z_* ***************************")
        #stop("asd")

        ### 2 dims
        ## plot `datas` as lon vs lat
        if (ndims == 2 && all(dim_names == c("lon", "lat"))) {
            
            message("\n", zname, " ", mode_p, " plot lon vs lat ...")

            # compare lon,lat model data against point data in a scatter plot
            # -> find locations through interpolation (choose one):
            # a)     linear methods from function   interp2(): "linear"; "nearest"; "constant"
            # b) non-linear method  from function barylag2d(): "barylag2d"
            # --> do this before lon,lat plot so that the finally used locations 
            #     can be plotted on the lon,lat map
            interp_method <- "linear"
            point_data <- point_datap <- point_data_fname <- 
                point_data_varname <- point_data_label <- point_data_legend <- NULL
            message("\ncheck for point_data ...")
            if (T && exists("gnip_list") && 
                any(zname == c("temp2", "tsurf", "aprt", "wisoaprt_d"))) {
                point_data_fname <- point_data_legend <- "GNIP"
                if (any(zname == c("temp2", "tsurf"))) {
                    point_data_varname <- "tair"
                    point_data_label <- "GNIP air temperature [°C]" 
                } else if (zname == "aprt") {
                    point_data_varname <- "precip"
                    point_data_label <- "GNIP precipitation [mm/month]" 
                } else if (zname == "wisoaprt_d") {
                    point_data_varname <- "O18"
                    #point_data_label <- expression(paste("GNIP ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                    point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                }
                tmp <- list()
                cnt <- 0
                for (i in seq_along(gnip_list)) { 
                    if (!is.null(gnip_list[[i]]$data[[point_data_varname]])) {
                        tmp2 <- gnip_list[[i]]
                        tmp2[[point_data_varname]] <- gnip_list[[i]]$data[[point_data_varname]]
                        tmp2$time <- gnip_list[[i]]$data[[paste0(point_data_varname, "_time")]]
                        tmp2$colno <- 1
                        tmp2$pchno <- 1
                        tmp2$varname <- point_data_varname
                        tmp2$fname <- point_data_fname
                        tmp2$label <- point_data_label
                        tmp2$legend <- point_data_legend
                        cnt <- cnt + 1
                        tmp[[cnt]] <- tmp2
                    }
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add gnip data to point_data
                rm(tmp)
            } # if gnip_list
            if (T && exists("bartlein_etal_2011") && 
                any(zname == c("lm_temp2_as_time_slope", "lm_tsurf_as_time_slope",
                               "lm_aprt_as_time_slope"))) {
                point_data_fname <- "bartlein_etal_2011"
                if (any(zname == c("lm_temp2_as_time_slope", "lm_tsurf_as_time_slope"))) {
                    point_data_varname <- "mat_trend"
                    #point_data_label <- "B11 MAT trend [°C/6k years]" 
                    point_data_label <- "Proxy temperature trend [°C/6k years]"
                    point_data_legend <- "B11"
                    tmp <- bartlein_etal_2011$mat_MH_minus_PI
                    tmp$data <- tmp$data*-1 # convert anomaly to trend
                } else if (zname == "lm_aprt_as_time_slope") {
                    point_data_varname <- "map_trend"
                    #point_data_label <- "B11 MAP trend [mm/year/6k years]" 
                    point_data_label <- "Proxy precipitation trend [mm/year/6k years]" 
                    point_data_legend <- "B11"
                    tmp <- bartlein_etal_2011$map_MH_minus_PI
                    tmp$data <- tmp$data*-1 # convert anomaly to trend
                }
                # convert bartlein et al 2011 data from 2d-lon,lat-data to point-data for scatter plot
                tmp2 <- tmp
                tmp2$data <- as.vector(tmp2$data)
                tmp2$lonlat <- expand.grid(tmp2$lon, tmp2$lat, KEEP.OUT.ATTRS=F)
                inds <- which(!is.na(tmp2$data))
                if (length(inds) > 0) {
                    tmp2$data <- tmp2$data[inds]
                    tmp2$lonlat <- tmp2$lonlat[inds,]
                }
                tmp3 <- vector("list", l=length(tmp2$data))
                stop("update for data")
                for (i in seq_along(tmp3)) {
                    tmp3[[i]] <- list(lon=tmp2$lonlat$Var1[i], lat=tmp2$lonlat$Var2[i], 
                                      time=make_posixlt_origin(-6000), # just placeholder
                                      data=tmp2$data[i], 
                                      varname=point_data_varname, fname=point_data_fname,
                                      label=point_data_label, legend=point_data_legend)
                }
                message("add ", length(tmp3), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp3) # add bartlein et al. 2011 to point_data
                rm(tmp, tmp2, tmp3)
            } # if bartlein_etal_2011
            if (T && exists("kaufman_etal_2020_temp12k") && 
                any(zname == c("lm_temp2_as_time_slope", "lm_tsurf_as_time_slope"))) {
                point_data_fname <- "kaufman_etal_2020_temp12k"
                point_data_varname <- "temperature_trend"
                #point_data_label <- "K20 temperature trend [°C/6k years]" # mean annual precip
                point_data_label <- "Proxy temperature trend [°C/6k years]" # mean annual precip
                point_data_legend <- "K20"
                tmp <- kaufman_etal_2020_temp12k
                stop("update for data")
                for (i in seq_along(tmp)) { # add the linear trend as data point
                    tmp[[i]]$time <- make_posixlt_origin(-6000) # just placeholder
                    tmp[[i]]$data <- tmp[[i]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                    tmp[[i]]$varname <- point_data_varname 
                    tmp[[i]]$fname <- point_data_fname
                    tmp[[i]]$label <- point_data_label
                    tmp[[i]]$legend <- point_data_legend
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add kaufman et al. 2020 temp12k to point_data
                rm(tmp)
            } # if kaufman_etal_2020_temp12k
            if (T && exists("global_holocene_lipd_precip") && 
                zname == "lm_aprt_as_time_slope") {
                point_data_fname <- "global_holocene_lipd_precip"
                point_data_varname <- "precipitation_trend"
                point_data_label <- "Proxy precipitation trend [mm/year/6k years]" # mean annual temperature
                point_data_legend <- "LiPD"
                tmp <- global_holocene_lipd_precip
                stop("update for data")
                for (i in seq_along(tmp)) { # add the linear trend as data point
                    tmp[[i]]$time <- make_posixlt_origin(-6000) # just placeholder
                    tmp[[i]]$data <- tmp[[i]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                    tmp[[i]]$varname <- point_data_varname 
                    tmp[[i]]$fname <- point_data_fname
                    tmp[[i]]$label <- point_data_label
                    tmp[[i]]$legend <- point_data_legend
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add global_holocene_lipd_precip to point_data
                rm(tmp)
            } # if global_holocene_lipd_precip
            if (T && exists("konecky_etal_2020_iso2k_d18o_precip") && 
                any(zname == c("wisoaprt_d", "lm_wisoaprt_d_post_as_time_slope"))) {
                point_data_fname <- "iso2k-precip"
                point_data_legend <- "Iso2k 1.0.0"
                if (zname == "wisoaprt_d") {
                    point_data_varname <- "d18Op"
                } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                    point_data_varname <- "d18Op_trend"
                }
                #point_data_label <- expression(paste("Iso2k ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/nonp,SMOW"], " [‰]"))
                point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                tmp <- konecky_etal_2020_iso2k_d18o_precip
                for (i in seq_along(tmp)) { # add the linear trend as data point
                    if (zname == "wisoaprt_d") {
                        # nothing to do
                    } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                        tmp[[i]]$time <- make_posixlt_origin(-6000) # placeholder for trend
                        tmp[[i]][[point_data_varname]] <- tmp[[i]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                    }
                    tmp[[i]]$colno <- 2
                    tmp[[i]]$pchno <- 2
                    tmp[[i]]$varname <- point_data_varname 
                    tmp[[i]]$fname <- point_data_fname
                    tmp[[i]]$label <- point_data_label
                    tmp[[i]]$legend <- point_data_legend
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add data to point_data
                rm(tmp)
            } # if konecky_etal_2020_iso2k
            if (F && exists("konecky_etal_2020_iso2k_d18o_nonprecip") && 
                any(zname == c("wisoaprt_d", "lm_wisoaprt_d_post_as_time_slope"))) {
                point_data_fname <- point_data_legend <- "Iso2k-nonprecip"
                point_data_varname <- "d18Ononp_scaled"
                #point_data_label <- expression(paste("Iso2k ", delta^{18}, "O"["nonp,SMOW"], " [‰]"))
                point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/nonp,SMOW"], " [‰]"))
                tmp <- konecky_etal_2020_iso2k_d18o_nonprecip
                stop("update for data")
                for (i in seq_along(tmp)) { # add the linear trend as data point
                    if (zname == "wisoaprt_d") {
                        tmp[[i]]$time <- mean(tmp[[i]]$time, na.rm=T)
                        tmp[[i]]$data <- mean(tmp[[i]], na.rm=T)
                    } else  if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                        stop("todo")
                        #tmp[[i]]$time <- make_posixlt_origin(-6000) # just placeholder
                        #tmp[[i]]$data <- tmp[[i]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                    }
                    tmp[[i]]$varname <- point_data_varname 
                    tmp[[i]]$fname <- point_data_fname
                    tmp[[i]]$label <- point_data_label
                    tmp[[i]]$legend <- point_data_legend
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add data to point_data
                rm(tmp)
            } # if konecky_etal_2020_iso2k
            if (T && exists("comas_bru_etal_2020_sisal_d18o_precip") && 
                any(zname == c("wisoaprt_d", "lm_wisoaprt_d_post_as_time_slope"))) {
                point_data_fname <- "sisal"
                #point_data_legend <- "Speleothems (SISAL 2.0)"
                point_data_legend <- "SISAL 2.0"
                point_data_varname <- "d18O_w_smow"
                #point_data_label <- expression(paste("SISAL ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                tmp <- comas_bru_etal_2020_sisal_d18o_precip
                for (i in seq_along(comas_bru_etal_2020_sisal_d18o_precip)) {
                    timeinds <- seq_along(tmp[[i]][[point_data_varname]]) # default: all
                    if (F) { # PI mean: cauquoin et al. 2019: "defined as the interval 1850–1990 CE" 
                             #--> from -100 before 1950 CE to 40 from 1950 CE
                        if (attributes(tmp[[i]]$time)$origin == 1950) {
                            timeinds <- which(tmp[[i]]$time$year+1900 >= -100 &
                                              tmp[[i]]$time$year+1900 <= 40)
                        } else {
                            stop("not yet")
                        }
                    }
                    if (length(timeinds) > 0) {
                        if (zname == "wisoaprt_d") {
                            # nothing to do
                        } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                            # add linear trend as data point
                            tmp[[i]]$time <- make_posixlt_origin(-6000) # placeholder for trend
                            tmp[[i]][[point_data_varname]] <- 
                                tmp[[i]][[paste0("lm_", point_data_varname)]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                        }
                        tmp[[i]]$colno <- 3
                        tmp[[i]]$pchno <- 3
                        tmp[[i]]$varname <- point_data_varname
                        tmp[[i]]$fname <- point_data_fname
                        tmp[[i]]$label <- point_data_label
                        tmp[[i]]$legend <- point_data_legend
                    } else {
                        tmp[[i]] <- NA
                    } # if timeinds
                } # for i
                nainds <- which(is.na(tmp))
                if (length(nainds) > 0) tmp <- tmp[-nainds]
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add ata to point_data
                rm(tmp)
            } # if comas_bru_etal_2020_sisal_d18o_precip
            if (F && exists("meyer_etal") && 
                any(zname == c("wisoaprt_d", "lm_wisoaprt_d_post_as_time_slope"))) {
                point_data_fname <- point_data_legend <- "PLOT"
                point_data_varname <- "d18Odiatom_scaled"
                #point_data_label <- expression(paste("PLOT ", delta^{18}, "O"["diatom,SMOW"], " [‰]"))
                point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/nonp,SMOW"], " [‰]"))
                # calc lm of scaled PLOT time series
                tmp <- vector("list", l=length(meyer_etal$data))
                for (i in seq_along(tmp)) {
                    x <- meyer_etal$data[[i]]$data$time
                    y <- scale(meyer_etal$data[[i]]$data$d18o_corr_perm)
                    lm <- lm(y ~ x)
                    dt <- as.numeric(difftime(max(x), min(x), units="day"))
                    nyears <- dt/365
                    slope_per_year <- diff(range(lm$fitted.values))/nyears
                    if (lm$coefficients[2] < 0) slope_per_year <- -1*slope_per_year
                    stop("update for data")
                    tmp[[i]] <- list(PLOT_lake=names(meyer_etal$data)[i],
                                     lon=meyer_etal$data[[i]]$lon,
                                     lat=meyer_etal$data[[i]]$lat,
                                     time=make_posixlt_origin(-6000), # just placeholder
                                     data=slope_per_year*6000, # trend/yr --> trend/6k yrs
                                     varname=point_data_varname,
                                     fname=point_data_fname,
                                     label=point_data_label,
                                     legend=point_data_legend)
                } # for i
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add data to point_data
                rm(tmp)
            } # if meyer_etal
            if (T && exists("pg")) {
                if (zname == "wisoaprt_d" && any(names(pg) == "d18o_w_smow")) {
                    point_data_varname <- "d18o_w_smow"
                    point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                    # find time series of wanted time
                    for (i in seq_along(pg$d18o_w_smow)) { # dois
                        point_data_fname <- names(pg$d18o_w_smow)[i]
                        point_data_legend <- "Pangaea"
                        tmp <- list()
                        cnt <- 0
                        for (j in seq_along(pg$d18o_w_smow[[i]])) { # events
                            if (length(pg$d18o_w_smow[[i]][[j]]$dims) == 1 && 
                                names(pg$d18o_w_smow[[i]][[j]]$dims) == "time") { 
                                time <- pg$d18o_w_smow[[i]][[j]]$dims$time
                                timeinds <- seq_along(time) # default: all
                                if (T) { 
                                    # PI mean: cauquoin et al. 2019: "last 200 years" 
                                    # --> from -150 before 1950 CE
                                    if (cnt == 0) message("select PI only ...")
                                    if (attributes(time)$origin == 1950) {
                                        timeinds <- which(time$year+1900 >= -150)
                                    } else {
                                        stop("not yet")
                                    }
                                }
                                if (length(timeinds) > 0) {
                                    cnt <- cnt + 1
                                    tmp[[cnt]] <- list(time=time[timeinds])
                                    tmp[[cnt]][[point_data_varname]] <- pg$d18o_w_smow[[i]][[j]]$data[timeinds]
                                    tmp[[cnt]]$doi <- pg$d18o_w_smow[[i]][[j]]$doi
                                    tmp[[cnt]]$loc <- pg$d18o_w_smow[[i]][[j]]$loc
                                    tmp[[cnt]]$lon <- pg$d18o_w_smow[[i]][[j]]$lon
                                    tmp[[cnt]]$lat <- pg$d18o_w_smow[[i]][[j]]$lat
                                    tmp[[cnt]]$colno <- 4
                                    tmp[[cnt]]$pchno <- 4
                                    tmp[[cnt]]$varname <- point_data_varname
                                    tmp[[cnt]]$fname <- point_data_fname
                                    tmp[[cnt]]$label <- point_data_label
                                    tmp[[cnt]]$legend <- point_data_legend
                                }
                            } # if ndim=1 and dim=time 
                        } # for j in seq_along(pg$d18o_w_smow[[i]]))
                        if (length(tmp) > 0) {
                            message("add ", length(tmp), " ", point_data_varname, " from ", 
                                    point_data_fname, " to point_data ...")
                            point_data <- c(point_data, tmp) # add data to point_data
                            rm(tmp)
                        }
                    } # for i seq_along(pg$d18o_w_smow)
                } # if zname == "wisoaprt_d" && any(names(pg) == "d18o_w_smow")
            } # if exists("pg")


            # from here: `point_data` must be list of n lists each 
            #            having (at least) the 8 entries
            #               "lon", "lat", "time", "colno", "pchno", "varname", "fname", "label"
            #            AND 1 entry with the same name as `varname`
            if (!is.null(point_data)) {
               
                # update for all attached point_data 
                point_data_fname <- sapply(point_data, "[[", "fname")
                point_data_fname_unique <- unique(point_data_fname)
                point_data_varname <- sapply(point_data, "[[", "varname")
                point_data_varname_unique <- unique(point_data_varname)
                point_data_label <- sapply(point_data, "[[", "label")
                point_data_label_types <- sapply(point_data_label, typeof)
                if (any(point_data_label_types == "language")) {
                    point_data_labelp <- lapply(point_data_label, deparse)
                    point_data_labelp <- sapply(point_data_labelp, paste, collapse="")
                    point_data_label_unique <- unique(point_data_labelp)
                    point_data_label_unique <- sapply(point_data_label_unique, function(x) parse(text=x))
                } else {
                    point_data_label_unique <- unique(point_data_label)
                }
                if (length(point_data_label_unique) != 1) {
                    stop("there are ", length(point_data_label_unique), 
                         " different `point_data_label_unique`: \"",
                         paste(point_data_label_unique, collapse="\", \""), "\"")
                }

                # find model data points from z_points locations
                message("--> ", length(point_data), " point_data defined for zname = \"", 
                        zname, "\": \"", 
                        paste(unique(sapply(point_data, "[[", "fname")), collapse="\", \""), "\"") 
                xp <- yp <- z
                for (i in seq_along(z)) { # for all settings
                    xp[[i]] <- yp[[i]] <- NA # default
                    # reduce data: use only locations within lon,lat lims
                    z_lonlim <- range(d$lon[[i]]); z_latlim <- range(d$lat[[i]])
                    message("******\nsetting ", i, " ", names_short[i], 
                            " lon,lat-data within area ",
                            areas_p[i], ": ",
                            z_lonlim[1], " to ", z_lonlim[2], "° lon ", 
                            z_latlim[1], " to ", z_latlim[2], "° lat ...")
                    point_data_lons <- sapply(point_data, "[[", "lon")
                    point_data_lats <- sapply(point_data, "[[", "lat")
                    message("check ", length(point_data), 
                            " point_datap locations with lon,lat-coords ", 
                            min(point_data_lons), " to ", 
                            max(point_data_lons), "° lon and ", 
                            min(point_data_lats), " to ", 
                            max(point_data_lats), "° lat ...")
                    lonlatinds <- which(point_data_lons >= z_lonlim[1] & 
                                        point_data_lons <= z_lonlim[2] &
                                        point_data_lats >= z_latlim[1] & 
                                        point_data_lats <= z_latlim[2])
                    message("found ", length(lonlatinds), "/", length(point_data), 
                            " point_datap locations within this model data area")
                        
                    # some point_data locations are within model data area
                    if (length(lonlatinds) > 0) { 
                        
                        # update all attached point_datap 
                        point_datap <- point_data[lonlatinds]
                        point_data_fname <- sapply(point_datap, "[[", "fname")
                        point_data_varname <- sapply(point_datap, "[[", "varname")
                        point_data_label <- point_data_label[lonlatinds]
                        point_data_label_types <- sapply(point_data_label, typeof)
                        if (any(point_data_label_types == "language")) {
                            point_data_labelp <- lapply(point_data_label, deparse)
                            point_data_labelp <- sapply(point_data_labelp, paste, collapse="")
                            point_data_label_unique <- unique(point_data_labelp)
                            point_data_label_unique <- sapply(point_data_label_unique, function(x) parse(text=x))
                        } else {
                            point_data_label_unique <- unique(point_data_label)
                        }
                        
                        # check for duplicate lon,lat pairs in point_datap
                        message("\ncheck for duplicate point_data locations ...")
                        lons <- sapply(point_datap, "[[", "lon")
                        lats <- sapply(point_datap, "[[", "lat")
                        cnt <- 0
                        for (j in seq_along(lons)) { # for all point_datap locations
                            inds <- which(lons == lons[j] & lats == lats[j])
                            if (length(inds) > 1) {
                                fnames <- sapply(point_datap[inds], "[[", "fname")
                                varnames <- sapply(point_datap[inds], "[[", "varname")
                                plotname <- paste0("tmp/lon=", lons[j], "_lat=", lats[j], "_", 
                                                   paste(inds, collapse="_"), "_",
                                                   paste(unique(fnames), collapse="_"), "_",
                                                   paste(unique(varnames), collapse="_"), ".pdf")
                                if (!file.exists(plotname)) { # not already covered before
                                    message("****************************************************",
                                            "\nlon,lat-combination ", j, "/", length(point_datap), 
                                            " = ", lons[j], "°lon and ", lats[j], "°lat occurs ", 
                                            length(inds), " times in point_datap: inds = c(", 
                                            paste(inds, collapse=", "), ")")
                                    x <- lapply(point_datap[inds], "[[", "time")
                                    y <- list()
                                    for (k in seq_along(inds)) {
                                        #cat(capture.output(str(point_datap[[inds[k]]], max.level=1)), sep="\n")
                                        x[[k]] <- as.POSIXct(x[[k]], o="1970-1-1", tz="UTC")
                                        y[[k]] <- as.vector(sapply(point_datap[inds[k]], "[[", varnames[k]))
                                    }
                                    message("plot ", plotname, " ...")
                                    pdf(plotname, family="sans", encoding=encoding)
                                    plot(0, t="n", xlim=range(x, na.rm=T), ylim=range(y, na.rm=T),
                                         xaxt="n", yaxt="n",
                                         xlab="year from 1950", ylab=paste(unique(varnames), collapse=","))
                                    axis.POSIXct(1, at=as.POSIXct(pretty(range(x, na.rm=T), n=15), o="1970-1-1", tz="UTC"))
                                    axis(2, at=pretty(range(y, na.rm=T), n=10), las=2)
                                    for (k in seq_along(inds)) {
                                        lines(x[[k]], y[[k]], col=k, lty=k)
                                    }
                                    legend <- paste0("i=", inds, "_", fnames, "_", varnames)
                                    legend("topleft", legend=legend, col=seq_along(inds), lty=seq_along(inds))
                                    box()
                                    dev.off()
                                    cnt <- cnt + 1
                                    #if (cnt == 9) stop("asd")
                                } # if plot not already existing
                            } # if duplicated lon,lat-combi
                        } # for j point_datap locations
                        #stop("asd")

                        # if point data has time dim, calc temporal means of point data (since 
                        # here is the lon,lat section and not lon,lat,time)
                        if (any(seasonsp_p[i] == names(season_check$known_seasons))) { # seas check 1/3
                            season_inds <- season_check$known_seasons[[
                                            which(seasonsp_p[i] == names(season_check$known_seasons))
                                                                     ]]$inds
                        } else {
                            season_inds <- regexpr(seasonsp_p[i], season_check$string)
                            if (any(season_inds != -1)) { # seas check 2/3: "DJF", "JJA"
                                season_inds <- season_check$inds[
                                            season_inds:(season_inds+attributes(season_inds)$match.length-1)
                                                                ]
                            } else { # seas check 3/3: "Jan", "Jul"
                                season_inds <- regexpr(seasonsp_p[i], season_check$names)
                                if (length(which(season_inds != -1)) == 1) {
                                    season_inds <- which(season_inds != -1)
                                } else {
                                    stop("`seasonsp_p[", i, "]` = \"", seasonsp_p[i], "\" not defined")
                                }
                            }
                        }
                        # apply seasonal subset and apply temporal mean to all point data coords within lon,lat-lims
                        message("\ncalc seasonal mean seasonsp_p[", i, "] = ", seasonsp_p[i], " of ",
                                length(point_datap), " point_datap locations (use months ", 
                                paste(season_inds, collapse=","), ") ...") 
                        for (j in seq_along(point_datap)) { # for every point data location
                            mu <- n_mu <- range_mu <- NA # default mean
                            if (!is.null(point_datap[[j]][[point_data_varname[j]]]) && # if current location has wanted varname
                                !all(is.na(point_datap[[j]][[point_data_varname[j]]]))) { # and not all are NA 
                                minds <- c()
                                for (si in seq_along(season_inds)) {
                                    minds <- c(minds, which(point_datap[[j]]$time$mon+1 == season_inds[si]))
                                }
                                if (length(minds) == 0) { # season was not found
                                    if (T) message("point_datap[[", j, "]] has no ", seasonsp_p[i], " values")
                                    #stop("asd")
                                } else { # season was found
                                    minds <- sort(minds)
                                    # use only non-NA data
                                    if (any(is.na(point_datap[[j]][[point_data_varname[j]]][minds]))) { 
                                        minds <- minds[-which(is.na(point_datap[[j]][[point_data_varname[j]]][minds]))]
                                    }
                                    if (length(minds) == 0) { 
                                        if (T) message("point_datap[[", j, "]] has no non-NA ", seasonsp_p[i], " values")
                                        #stop("asd")
                                    } else { # any data left after varname/season/NA-filters
                                        mu <- mean(point_datap[[j]][[point_data_varname[j]]][minds], na.rm=T)
                                        n_mu <- length(minds)
                                        range_mu <- as.POSIXct(c(min(point_datap[[j]]$time[minds]),
                                                                 max(point_datap[[j]]$time[minds])),
                                                               tz="UTC")
                                    }
                                }
                            } else {
                                stop("`point_datap[[j]]` has no point_data_varname[j] = ", 
                                     point_data_varname[j], " values")
                            }
                            point_datap[[j]][["data_mean"]] <- mu
                            point_datap[[j]][["data_mean_ntime"]] <- n_mu
                            point_datap[[j]][["data_mean_rangetime"]] <- range_mu
                        } # for j in point_datap
                        
                        # throw out locations that did not survive the varname/season/NA-filters
                        if (any(is.na(sapply(point_datap, "[[", "data_mean")))) {
                            nainds <- which(is.na(sapply(point_datap, "[[", "data_mean")))
                            if (T) message("remove ", length(nainds), "/", length(point_datap), 
                                           " point_datap with `mean_data`=NA:\n",
                                           paste(paste0("   point_datap[", nainds, "]: ", 
                                                        sapply(point_datap[nainds], "[[", "varname"), " ",
                                                        sapply(point_datap[nainds], "[[", "fname")
                                                        ), collapse="\n"))
                            stop("asd")
                            if (length(nainds) == length(point_datap)) {
                                point_datap <- NULL
                                message("none of the point_data survived the varname/season/NA-filters")
                            } else {
                                point_datap <- point_datap[-nainds]
                                point_data_varname <- point_data_varname[-nainds]
                                point_data_label <- point_data_label[-nainds]
                                point_data_label_types <- sapply(point_data_label, typeof)
                                if (any(point_data_label_types == "language")) {
                                    point_data_labelp <- lapply(point_data_label, deparse)
                                    point_data_labelp <- sapply(point_data_labelp, paste, collapse="")
                                    point_data_label_unique <- unique(point_data_labelp)
                                    point_data_label_unique <- sapply(point_data_label_unique, function(x) parse(text=x))
                                } else {
                                    point_data_label_unique <- unique(point_data_label)
                                }
                            }
                        }
                            
                        # interp closest model locations to remaining point data locations
                        if (!is.null(point_datap)) {
                            rangetot <- range(lapply(point_datap, "[[", "data_mean_rangetime"))
                            message("interp model data to ", length(point_datap), " non-NA ", 
                                    seasonsp_p[i], " point_datap locations from\n   ", 
                                    as.POSIXlt(rangetot, o="1970-1-1")[1], " to ", 
                                    as.POSIXlt(rangetot, o="1970-1-1")[2], "\nwith interp_method = ", 
                                    interp_method, " ...")
                            model_data <- rep(NA, t=length(point_datap))
                            for (j in seq_along(point_datap)) {
                                if (!any(search() == "package:pracma")) suppressMessages(library(pracma))
                                if (interp_method == "barylag2d") {
                                    stop("not yet")
                                } else {
                                    # pracma::interp2 uses x,y in oppsite order
                                    model_data[j] <- pracma::interp2(x=d$lat[[i]], y=d$lon[[i]], Z=z[[i]],
                                                                     xp=point_datap[[j]]$lat, 
                                                                     yp=point_datap[[j]]$lon, 
                                                                     method=interp_method)
                                }
                            } # for j in point data lon,lat-subset data
                            
                            # save for scatter plot
                            xp[[i]] <- sapply(point_datap, "[[", "data_mean")
                            attributes(xp[[i]])$lon <- sapply(point_datap, "[[", "lon")
                            attributes(xp[[i]])$lat <- sapply(point_datap, "[[", "lat")
                            attributes(xp[[i]])$colno <- sapply(point_datap, "[[", "colno")
                            attributes(xp[[i]])$pchno <- sapply(point_datap, "[[", "pchno")
                            attributes(xp[[i]])$fname <- sapply(point_datap, "[[", "fname")
                            attributes(xp[[i]])$varname <- sapply(point_datap, "[[", "varname")
                            attributes(xp[[i]])$legend <- sapply(point_datap, "[[", "legend")
                            attributes(xp[[i]])$ntimes <- sapply(point_datap, "[[", "data_mean_ntime")
                            attributes(xp[[i]])$ntot <- sum(attributes(xp[[i]])$ntimes)
                            attributes(xp[[i]])$ranges <- sapply(point_datap, "[[", "data_mean_rangetime")
                            attributes(xp[[i]])$rangetot <- rangetot
                            attributes(xp[[i]])$from <- apply(sapply(point_datap, "[[", "data_mean_rangetime"), 2, min)
                            attributes(xp[[i]])$to <- apply(sapply(point_datap, "[[", "data_mean_rangetime"), 2, max) 
                            attributes(xp[[i]])$origin <- sapply(lapply(sapply(point_datap, "[[", "time"), attributes), "[[", "origin")
                            yp[[i]] <- model_data
                        
                            # make attributes sticky (not removed by subsetting)
                            if (!any(search() == "package:sticky")) suppressPackageStartupMessages(library(sticky))
                            xp[[i]] <- sticky::sticky(xp[[i]])
                            yp[[i]] <- sticky::sticky(yp[[i]])

                            # remove NA
                            if (any(is.na(xp[[i]]))) {
                                inds <- which(is.na(xp[[i]]))
                                xp[[i]] <- xp[[i]][-inds]
                                yp[[i]] <- yp[[i]][-inds]
                            }
                            if (any(is.na(yp[[i]]))) {
                                inds <- which(is.na(yp[[i]]))
                                xp[[i]] <- xp[[i]][-inds]
                                yp[[i]] <- yp[[i]][-inds]
                            }

                            # save obs as netcdf
                            if (F && length(xp[[i]]) > 0) {
                                outname <- paste0(paste(unique(attributes(xp[[i]])$fname), collapse="_vs_"), 
                                                  "_", areas_p[i], "_", seasonsp_p[i], "_", 
                                                  paste(unique(as.POSIXlt(attributes(xp[[i]])$rangetot, 
                                                                          o="1970-1-1", tz="UTC")$year+1900), 
                                                        collapse="_to_"), ".nc")
                                message("save temporally averaged point data to ", outname, " ...")
                                location_dim <- ncdim_def(name="location", units="#", vals=seq_along(xp[[i]]))
                                point_datap_lon_var <- ncvar_def(name="lon", units="degrees_east",
                                                                 dim=location_dim, prec="double")
                                point_datap_lat_var <- ncvar_def(name="lat", units="degrees_north",
                                                                 dim=location_dim, prec="double")
                                point_datap_ntimes_var <- ncvar_def(name="ntime", units="#",
                                                                    dim=location_dim, prec="integer")
                                point_datap_from_var <- ncvar_def(name="from", units="#",
                                                                  dim=location_dim, prec="char")
                                point_datap_to_var <- ncvar_def(name="to", units="#",
                                                                   dim=location_dim, prec="char")
                                point_datap_fname_var <- ncvar_def(name="fname", units="#",
                                                                   dim=location_dim, prec="char")
                                point_datap_varname_var <- ncvar_def(name="varname", units="#",
                                                                     dim=location_dim, prec="char")
                                point_datap_var <- ncvar_def(name="data", units=data_info$units,
                                                             dim=location_dim,
                                                             missval=NA, prec="double")
                                outnc <- nc_create(filename=outname, force_v4=T, 
                                                   vars=list(point_datap_lon_var, point_datap_lat_var, 
                                                             point_datap_ntimes_var, 
                                                             point_datap_from_var, point_datap_to_var,
                                                             point_datap_fname_var, point_datap_varname_var, 
                                                             point_datap_var))
                                ncvar_put(outnc, point_datap_lon_var, attributes(xp[[i]])$lon)
                                ncvar_put(outnc, point_datap_lat_var, attributes(xp[[i]])$lat)
                                ncvar_put(outnc, point_datap_ntimes_var, attributes(xp[[i]])$ntimes)
                                ncvar_put(outnc, point_datap_from_var, attributes(xp[[i]])$from)
                                ncvar_put(outnc, point_datap_to_var, attributes(xp[[i]])$to)
                                ncvar_put(outnc, point_datap_fname_var, attributes(xp[[i]])$fname)
                                ncvar_put(outnc, point_datap_varname_var, attributes(xp[[i]])$varname)
                                ncvar_put(outnc, point_datap_var, xp[[i]])
                                #ncatt_put(outnc, 0, "datapath", paste(datainpaths, collapse=", "))
                                nc_close(outnc)
                            } # save temporally averaged point data as netcdf or not 
                        } # if any point_data points remain after seasonal subset
                    } # if any point_data lon,lat coords are within model data area
                } # for i in z

                ## different model setups (yp) vs respective obs (xp) in one scatter plot
                if (!all(sapply(lapply(xp, is.na), all))) {

                    # survived point_datap
                    xp_colno <- unlist(lapply(lapply(xp, attributes), "[[", "colno"))
                    xp_pchno <- unlist(lapply(lapply(xp, attributes), "[[", "pchno"))
                    xp_fname <- unlist(lapply(lapply(xp, attributes), "[[", "fname"))
                    xp_fname_unique <- unique(xp_fname)
                    xp_varname <- unlist(lapply(lapply(xp, attributes), "[[", "varname"))
                    xp_varname_unique <- unique(xp_varname)
                    xp_legend <- unlist(lapply(lapply(xp, attributes), "[[", "legend"))
                    xp_legend_unique <- unique(xp_legend)
                   
                    # special: save survived pangaea dois as latex table
                    # one multi-row per unique pangaea bibtex entry (and not DOI) because:
                    # different pangaea DOIs may have the same bibtex entry! 
                    if (any(xp_legend_unique == "Pangaea")) {
                        message("special: save survived pangaea dois as latex table")
                        inds <- which(xp_legend == "Pangaea")
                        pdois <- sapply(point_datap[inds], "[[", "doi")
                        pdois_unique <- unique(pdois)
                        bibtex <- vector("list", l=length(pdois_unique))
                        if (!any(search() == "package:RCurl")) library(RCurl)
                        for (i in seq_along(pdois_unique)) {
                            #message("run `RCurl::getURL(", paste0(pdois_unique[i], "?format=citation_bibtex"), ")` ...")
                            bibtex[[i]] <- RCurl::getURL(paste0(pdois_unique[i], "?format=citation_bibtex"))
                            bibtex[[i]] <- strsplit(bibtex[[i]], "\n ")[[1]]
                        } 
                        # first rows of bibtex-entries
                        prefs <- sapply(bibtex, "[", 1)
                        prefs <- substr(prefs,
                                        sapply(gregexpr("\\{", prefs), "[[", 1) + 1,
                                        sapply(gregexpr(",", prefs), "[[", 1) - 1)
                        names(bibtex) <- prefs
                        if (zname == "wisoaprt_d") {
                            latex_varname <- "$\\delta^{18}$O$_\\text{p,SMOW}$ [\\textperthousand]"
                        } else {
                            stop("not defined")
                        }
                        latex_caption <- paste0("References and temporal average periods of ", latex_varname, " data from Pangaea shown in Fig. \\ref{fig:timmean_d18o_pi_gnip_iso2k_sisal_pangaea} and \\ref{fig:scatter_d18o_pi_linear_gnip_iso2k_sisal_pangaea}.")
                        lines <- c("\\begin{table}",
                                   paste0("\\appendcaption{A1}{", latex_caption, "}"),
                                   "\\begin{center}",
                                   "\\begin{footnotesize}",
                                   "\\bgroup",
                                   "\\def\\arraystretch{0.8} % change rowsize; 1=default",
                                   "\\begin{tabular}{rlrrrrrrr}",
                                   "\\toprule",
                                   paste0("\\thead{No}",
                                          " & \\thead{Pangaea reference}",
                                          " & \\thead{lon [$^{\\circ}$]}",
                                          " & \\thead{lat [$^{\\circ}$]}",
                                          " & \\thead{Start from \\\\ 1950 CE}",
                                          " & \\thead{End from \\\\ 1950 CE}",
                                          " & \\thead{Start from \\\\ 0 CE}",
                                          " & \\thead{End from \\\\ 0 CE}",
                                          " & \\thead{", latex_varname, "}",
                                          #" & \\thead{Pangaea DOI}",
                                          "\\\\"),
                                   "\\midrule")
                        cnt <- 0
                        for (i in seq_along(pdois_unique)) {
                            inds2 <- inds[which(pdois == pdois_unique[i])]
                            if (length(unique(sapply(point_datap[inds2], "[[", "fname"))) != 1) {
                                stop("there should only be one ref for ", 
                                     length(inds2), " identical pangaea dois")
                            }
                            line <- c()
                            for (j in seq_along(inds2)) {
                                cnt <- cnt + 1
                                line[j] <- paste0(cnt, " &")
                                if (j == 1) {
                                    line[j] <- paste0(line[j], " \\citet{", names(bibtex)[i], "}")
                                }
                                line[j] <- paste0(line[j], " & ", round(point_datap[[inds2[j]]]$lon, 3))
                                line[j] <- paste0(line[j], " & ", round(point_datap[[inds2[j]]]$lat, 3))
                                o <- attributes(point_datap[[inds2[j]]]$time)$origin
                                if (o == 1950) {
                                    fromto_1950 <- as.POSIXlt(point_datap[[inds2[j]]]$data_mean_rangetime, o="1970-1-1", tz="UTC")
                                    fromto_0 <- fromto_1950
                                    #fromto_1950 <- as.Date(fromto_1950)
                                    fromto_1950 <- fromto_1950$year + 1900
                                    fromto_0$year <- fromto_0$year + 1950
                                    #fromto_0 <- as.Date(fromto_0)
                                    fromto_0 <- fromto_0$year + 1900
                                } else if (o == 0) {
                                    fromto_0 <- as.POSIXlt(point_datap[[inds2[j]]]$data_mean_rangetime, o="1970-1-1", tz="UTC")
                                    fromto_1950 <- fromto_0
                                    fromto_0 <- as.Date(fromto_0)
                                    stop("continue")
                                    fromto_1950$year <- fromto_1950$year + 1950
                                    fromto_1950 <- as.Date(fromto_1950)
                                } else {
                                    stop("not defined")
                                }
                                line[j] <- paste0(line[j], " & ", fromto_1950[1])
                                line[j] <- paste0(line[j], " & ", fromto_1950[2])
                                line[j] <- paste0(line[j], " & ", fromto_0[1])
                                line[j] <- paste0(line[j], " & ", fromto_0[2])
                                line[j] <- paste0(line[j], " & ", round(point_datap[[inds2[j]]]$data_mean, 2))
                                if (F) { # add pangaea DOI
                                    if (j == 1) {
                                        line[j] <- paste0(line[j], " & ")
                                        if (F) { # complete url
                                            line[j] <- paste0(line[j], 
                                                              "\\href{", pdois_unique[i], "}{", 
                                                              basename(dirname(pdois_unique[i])), "/", basename(pdois_unique[i]), "}")
                                        } else if (T) { # just doi
                                            line[j] <- paste0(line[j], basename(dirname(pdois_unique[i])), "/", basename(pdois_unique[i]))
                                        }
                                    }
                                }
                                line[j] <- paste0(line[j], "\\\\")
                            } # for j inds2
                            lines <- c(lines, line)
                        } # for i pdois_unique
                        lines <- c(lines, 
                                   "\\bottomrule",
                                   "\\label{tab:pangaea}",
                                   "\\end{tabular}",
                                   "\\egroup",
                                   "\\end{footnotesize}",
                                   "\\end{center}",
                                   "\\end{table}", 
                                   "")
                        for (i in seq_along(bibtex)) lines <- c(lines, bibtex[[i]])
                        fout <- paste0("pangaea_table_", paste(prefs, collapse="_"), ".txt")
                        message("save ", fout, " ...")
                        writeLines(lines, con=fout)
                    } # if any(xp_legend_unique == "Pangaea")
                    # finished special: save survived pangaea dois as latex table 


                    # group point_data by?
                    scatterobscols <- scatterobspchs <- vector("list", l=length(xp))
                    for (i in seq_along(xp)) {

                        ## col
                        # color by season
                        if (F && any(names(season_check$known_seasons) == seasonsp_p[i])) { 
                            if (i == 1) message("color scatter by season ", seasonsp_p[i], " col = ",
                    season_check$known_seasons[[which(names(season_check$known_seasons) == seasonsp_p[i])]]$col) 
                            scatterobscols[[i]] <- rep(
                    season_check$known_seasons[[which(names(season_check$known_seasons) == seasonsp_p[i])]]$col,
                                                       t=length(xp[[i]]))
                        # else color by xp_fname_unique
                        } else if (T) { 
                            if (i == 1) message("color scatter by xp_fname_unique ...")
                            scatterobscols[[i]] <- mycols(max(xp_colno))[xp_colno]
                        # else color by model setting i
                        } else if (F) {
                            if (i == 1) message("color scatter by model setting cols ...")
                            scatterobscols[[i]] <- rep(cols_p[i], t=length(xp[[i]]))
                        }
                        # use transparent colors
                        if (F) {
                            if (i == 1) message("special: use transparent colors")
                            scatterobscols[[i]] <- col2rgba(scatterobscols[[i]], alpha=alpha_rgb)
                        }

                        ## pch
                        if (T) {
                            if (i == 1) message("pch scatter by xp_pchno ...")
                            xp_pchno_unique <- unique(xp_pchno)
                            if (T) { # hollow symbols
                                pchs <- c(pchs_hollow, seq_len(255)[-pchs_hollow])
                            } else if (F) { # filled symbols 
                                pchs <- c(pchs_filled_wout_border, 
                                          (15:20)[!match(15:20, pchs_filled_wout_border, nomatch=F)])
                            }
                            tmp <- rep(NA, t=length(xp[[i]]))
                            for (j in seq_along(xp_pchno_unique)) {
                                inds <- which(xp_pchno == xp_pchno_unique[j]) 
                                tmp[inds] <- pchs[xp_pchno_unique[j]]
                            }
                            scatterobspchs[[i]] <- tmp
                        }
                        
                    } # for i in xp
                    scatterobsle <- list()
                    xp_n_unique <- rep(NA, t=length(xp_legend_unique))
                    for (i in seq_along(xp_legend_unique)) {
                        if (T) {
                            if (i == 1) message("group point_data by `legend`")
                            inds <- which(xp_legend == xp_legend_unique[i])
                            scatterobsle$text[i] <- paste0(xp_legend_unique[i], " (n=", length(inds), ")")
                        } else if (F) {
                            if (i == 1) message("group point_data by `fname`")
                            inds <- which(xp_fname == xp_fname_unique[i])
                            scatterobsle$text[i] <- paste0(xp_fname_unique[i], " (n=", length(inds), ")")
                        }
                        xp_n_unique[i] <- length(inds)
                        tmp <- unlist(scatterobscols)[inds]
                        if (length(unique(tmp)) != 1) stop("should be 1")
                        scatterobsle$col[i] <- unique(tmp)
                        tmp <- unlist(scatterobspchs)[inds]
                        if (length(unique(tmp)) != 1) stop("should be 1")
                        scatterobsle$pch[i] <- unique(tmp)
                    } # for i in xp_legend_unique
                    scatterobsle$lty <- rep(NA, t=length(scatterobsle$pch))
                    scatterobsle$lwd <- rep(NA, t=length(scatterobsle$pch))
                    
                    point_datap_suffix <- paste(paste0(xp_n_unique, "_", 
                                                       gsub(" ", "", xp_legend_unique)),
                                                collapse="_vs_")

                    tlim_scatter <- as.POSIXlt(range(lapply(lapply(xp, attributes), 
                                                            "[[", "rangetot")), o="1970-1-1")
                    message("\ncompare ", length(yp), " model data and\n", 
                            point_datap_suffix, "\ndata from ", 
                            tlim_scatter[1], " to ", tlim_scatter[2], 
                            " in one scatter plot ...")
                    
                    # xlim 
                    xlim <- range(xp, na.rm=T)
                    if (all(!is.na(match(xp_varname_unique, c("map_trend", "precipitation_trend"))))) { 
                        message("special xlim ...")
                        xlim <- c(-1000, 650)
                    }
                    message("xlim (", paste(xp_varname_unique, collapse=", "), 
                            " of point_data) = ", appendLF=F)
                    dput(xlim)
                    xat <- pretty(xlim, n=10)
                    xlab <- format(xat, trim=T)

                    # ylim
                    ylim <- range(yp, na.rm=T)
                    message("ylim (", zname, " of models) = ", appendLF=F)
                    dput(ylim)
                    yat <- pretty(ylim, n=10)
                    ylab <- format(yat, trim=T)
                
                    width_in <- p$inch #a4_width_in # maximum a4 width as threshold (8.26 in)
                    message("width_in = ", width_in, " ", appendLF=F)
                    asp_width_over_height <- p$scatter_width/p$scatter_height 
                    message("--> aspect ratio = ", round(asp_width_over_height, 3))
                    height_in <- width_in/asp_width_over_height
                    message("height_in = width_in/aspect_ratio = ", 
                            width_in, "/", round(asp_width_over_height, 3), " = ", 
                            round(width_in/asp_width_over_height, 4), " = ",
                            round(height_in, 4), " ", appendLF=F)
                    if (height_in > p$a4_height_in) {
                        height_in <- p$a4_height_in # take a4 maximum height as threshold (11.58 in)
                        message("> ", p$a4_height_in, " --> height_in = ", height_in, 
                                " --> aspect ratio = ", round(width_in/height_in, 3))
                    } else {
                        message()
                    }
                    pointsize <- p$pointsize*width_in/p$inch # multiple of default
                    if (T) {
                        message("special: increase pointsize ...")
                        pointsize <- 1.25*pointsize
                    }

                    plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                       zname, "_", 
                                       paste0(names_short_p, "_", seasonsp_p, 
                                              "_", froms_plot_p, "_to_", tos_plot_p, "_", 
                                              areas_p, collapse="_vs_"), 
                                       plotname_suffix, 
                                       "_scatter_", interp_method, "_vs_", 
                                       point_datap_suffix,
                                       ".", p$plot_type)
                    if (nchar(plotname) > nchar_max_foutname) {
                        plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                           zname, "_", 
                                           paste(unique(names_short_p), collapse="_"), "_",
                                           paste(unique(seasonsp_p), collapse="_"), "_", 
                                           paste(unique(froms_plot_p), collapse="_"), "-", 
                                           paste(unique(tos_plot_p), collapse="_"), "_", 
                                           paste(unique(areas_p), collapse="_"), 
                                           plotname_suffix, 
                                           "_scatter_", interp_method, "_vs_", 
                                           point_datap_suffix,
                                           ".", p$plot_type)
                    }
                    message("open plot ", plotname, " ...")
                    dir.create(dirname(plotname), recursive=T, showWarnings=F)
                    if (p$plot_type == "png") {
                        width <- width_in*p$ppi; height <- height_in*p$ppi
                        png(plotname, width=width, height=height, res=p$ppi, 
                            family=p$family_png, pointsize=pointsize)
                    } else if (p$plot_type == "pdf") {
                        pdf(plotname, width=width_in, height=height_in, family=p$family_pdf, 
                            pointsize=pointsize, encoding=encoding)
                    }
                     
                    # set plot margins
                    mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                    mar[4] <- 1 # decrease right margin
                    if (!add_title) mar[3] <- 1 # decrease upper margin

                    # open plot
                    par(mar=mar)
                    plot(0, t="n", xlab=NA, ylab=NA, 
                         xlim=xlim, ylim=ylim, xaxt="n", yaxt="n")
                    axis(1, at=xat, labels=xlab, cex.axis=0.9)
                    axis(2, at=yat, labels=ylab, las=1)

                    # add variable label
                    if (any(zname == c("temp2", "tsurf"))) {
                        lab <- expression(paste("Model T"["2m"], " [°C]"))
                    } else if (zname == "tsurf") {
                        lab <- expression(paste("Model T"["surf"], " [°C]"))
                    } else if (zname == "aprt") {
                        lab <- expression(paste("Model P"["total"], " [mm/month]"))
                    } else if (zname == "wisoaprt_d") {
                        lab <- expression(paste("Model ", delta^{18}, "O"["p,SMOW"], " [\u2030]"))
                    } else if (zname == "lm_temp2_as_time_slope") {
                        lab <- expression(paste("Model T"["2m"], " trend [°C/6k years]"))
                    } else if (zname == "lm_tsurf_as_time_slope") {
                        lab <- expression(paste("Model T"["surf"], " trend [°C/6k years]"))
                    } else if (zname == "lm_aprt_as_time_slope") {
                        lab <- expression(paste("Model P"["total"], " trend [mm/year/6k years]"))
                    } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                        lab <- eval(substitute(expression(paste("Model ", delta^{18}, "O"["p,SMOW"], " trend [\u2030/6k years]"))))
                    } else {
                        lab <- paste0("Model ", data_info$label)
                    }
                    mtext(side=1, point_data_label_unique, line=2.75)
                    mtext(side=2, lab, line=3.4)

                    # add zero lines
                    if (add_zeroline) {
                        #abline(h=0, col="gray", lwd=0.75, lty=2)
                        #abline(v=0, col="gray", lwd=0.75, lty=2)
                        abline(h=0, lty=2)
                        abline(v=0, lty=2)
                    }

                    # add 1:1 line
                    if (add_scatter_1to1_line) {
                        message("add 1:1 line ...")
                        #abline(a=0, b=1, col="gray", lwd=1, lty=2) # a=intercept, b=slope
                        abline(a=0, b=1, lty=2)
                    }
                    
                    # add data to scatter plot colored by time
                    for (i in seq_along(yp)) { # for all models
                        if (!all(is.na(xp[[i]]))) { 
                            points(xp[[i]], yp[[i]],
                                   col=scatterobscols[[i]], pch=scatterobspchs[[i]])
                        } else {
                            message("--> cannot add. all xp[[", i, "]] is NA")
                        }
                    } # for i in yp

                    # add scatter density to show where the most points are
                    if (any(add_scatter_density)) {
                        for (i in seq_along(yp)) {
                            if (add_scatter_density[i]) {
                                if (!any(search() == "package:KernSmooth")) library(KernSmooth)
                                if (!all(is.na(xp[[i]]))) { 
                                    # the smaller `bandwidth` (must > 0), the finer the density contour
                                    if (any(zname == c("lm_temp2_as_time_slope", "lm_tsurf_as_time_slope"))) {
                                       bandwidth <- c(0.5, 0.5) 
                                       gridsize <- c(100, 100)
                                    } else if (zname == "lm_aprt_as_time_slope") {
                                       bandwidth <- c(50, 50) 
                                       gridsize <- c(500, 500)
                                    } else {
                                        stop("not defined")
                                    }
                                    dens <- KernSmooth::bkde2D(cbind(xp[[i]], yp[[i]]), 
                                                               bandwidth=bandwidth, gridsize=gridsize)
                                                               #range.x, truncate = TRUE)
                                    contour(dens$x1, dens$x2, dens$fhat, add=T, 
                                            drawlabels=F, col=cols_p[i])
                                } # if obs is not all NA
                            } # if add_scatter_density
                        } # for i in yp (model data)
                    } else {
                        message("all `add_scatter_density`=F")
                    } # if any add_scatter_density

                    # add linear trend
                    if (any(add_linear_trend)) {
                        lm_labels <- rep(NA, t=length(yp))
                        for (i in seq_along(yp)) {
                            if (add_linear_trend[i]) {
                                if (!all(is.na(xp[[i]]))) {
                                    lm <- lm(yp[[i]] ~ xp[[i]]) 
                                    lm_summary <- summary(lm)
                                    #print(lm_summary)
                                    lm_labels[i] <- paste0("r=", round(sqrt(lm_summary$r.squared), 2), ", p")
                                    pval <- get_pval(lm)
                                    if (pval < 1e-5) {
                                        lm_labels[i] <- paste0(lm_labels[i], "<1e-5")
                                    } else {
                                        lm_labels[i] <- paste0(lm_labels[i], "=", round(pval, 4)) 
                                    }
                                    lm_labels[i] <- paste0(lm_labels[i], ", df=", lm_summary$fstatistic["dendf"])
                                    message("--> ", lm_labels[i])
                                    # plot regression line within data limits only
                                    if (F) {
                                        message("draw linear regression line within regression limits only ...")
                                        lines(xp[[i]], lm$fitted.values, 
                                              col=scatterobscols[i], lwd=lwds_p[i], lty=ltys_p[i])
                                    # or plot line through whole plot with regression coefficients
                                    } else if (T) {
                                        message("draw linear regression line through whole plot ...")
                                        abline(a=lm$coefficients[1], b=lm$coefficients[2],
                                               col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i])
                                    }
                                } # if obs not all NA
                            } # if add_linear_trend
                        } # for i in yp
                    } else {
                        message("all `add_linear_trend`=F")
                    } # if any add_linear_trend

                    # add legend if wanted
                    if (add_legend) {
                        message("add legend to scatter plot ...")
                        le <- list()
                        if (F && suppressPackageStartupMessages(require(Hmisc))) {
                            tmp <- Hmisc::largest.empty(x=unlist(xp), y=unlist(yp), method="area")
                            #rect(tmp$rect$x[1], tmp$rect$y[1], tmp$rect$x[2], tmp$rect$y[3])
                            le$pos <- c(x=min(tmp$rect$x), y=max(tmp$rect$y)) # topleft corner if x- and y-coords are both increasing (default)
                            message("automatically derived Hmisc::largest.empty legend position: ", le$pos[1], ", ", le$pos[2])
                        } else if (F && suppressPackageStartupMessages(require(adagio))) {
                            x <- unlist(xp); y <- unlist(yp)
                            tmp <- adagio::maxempty(x=x, y=y, ax=par("usr")[1:2], ay=par("usr")[3:4])
                            #rect(tmp$rect[1], tmp$rect[2], tmp$rect[3], tmp$rect[4])
                            le$pos <- c(x=tmp$rect[1], y=tmp$rect[4]) # topleft corner if x- and y-coords are both increasing (default)
                            message("automatically derived adagio::maxempty legend position: ", le$pos[1], ", ", le$pos[2])
                        } else {
                            le$pos <- "topleft" 
                            #le$pos <- "topright" 
                            #le$pos <- "bottomright" 
                        }
                        le$ncol <- 1
                        #le$ncol <- 2 
                        le$cex <- 1
                        #le$cex <- 0.85
                        #le$text <- paste0(names_legend, " (n=", sapply(lapply(xp, attributes), "[[", "ntot"), ")")
                        le$text <- names_legend
                        le$col <- cols_p #"black"
                        le$lty <- rep(NA, t=length(yp))
                        le$lwd <- rep(NA, t=length(yp))
                        #le$pch <- scatterpchs
                        le$pch <- rep(15, t=length(yp)) # filled square for just showing the color
                        le$pt.cex <- rep(1.5, t=length(yp))
                        if (T && length(scatterobsle) > 0) {
                            if (T) {
                                message("specialllll legendddddd")
                                le$text <- scatterobsle$text
                                le$col <- scatterobsle$col
                                le$lty <- scatterobsle$lty
                                le$lwd <- scatterobsle$lwd
                                le$pch <- scatterobsle$pch
                            } else {
                                message("add point_data legend to model legend ...")
                                le$text <- c(le$text, scatterobsle$text)
                                le$col <- c(le$col, scatterobsle$col)
                                le$lty <- c(le$lty, scatterobsle$lty)
                                le$lwd <- c(le$lwd, scatterobsle$lwd)
                                le$pch <- c(le$pch, scatterobsle$pch)
                                le$pt.cex <- c(le$pt.cex, rep(1, t=length(scatterobsle$text)))
                            }
                        }
                        if (T) le <- reorder_legend(le)
                        if (length(le$pos) == 1) {
                            legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                                   pch=le$pch, col=le$col, ncol=le$ncol, pt.cex=le$pt.cex,
                                   x.intersp=-0.2, cex=le$cex, bty="n")
                        } else if (length(le$pos) == 2) {
                            legend(x=le$pos[1], y=le$pos[2],
                                   legend=le$text, lty=le$lty, lwd=le$lwd,
                                   pch=le$pch, col=le$col, ncol=le$ncol, pt.cex=le$pt.cex,
                                   x.intersp=-0.2, cex=le$cex, bty="n")
                        }
                        # add 2nd legend for different point_data 
                        if (F && !is.null(scatter_labels)) {
                            message("make extra legend for point_data ...")
                            #le$pos <- "topright" 
                            #le$pos <- "bottomleft" 
                            le$pos <- "bottomright" 
                            le$ncol <- 1
                            #le$ncol <- 2 
                            le$cex <- 1
                            le$text <- scatter_labels
                            le$col <- "black" #scatterobscols #"black"
                            le$lty <- NA
                            le$lwd <- NA
                            le$pch <- unique(unlist(scatterobspchs)) # todo: is this always the correct order?
                            if (T) le <- reorder_legend(le)
                            if (length(le$pos) == 1) {
                                legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                                       pch=le$pch, col=le$col, ncol=le$ncol,
                                       x.intersp=-0.2, cex=le$cex, bty="n")
                            } else if (length(le$pos) == 2) {
                                legend(x=le$pos[1], y=le$pos[2],
                                       legend=le$text, lty=le$lty, lwd=le$lwd,
                                       pch=le$pch, col=le$col, ncol=le$ncol,
                                       x.intersp=-0.2, cex=le$cex, bty="n")
                            }
                        } # if !is.null(scatter_labels)
                    } # if add_legend
                    box()
                    message("save plot ", plotname, " ...")
                    dev.off()
                    if (p$plot_type == "pdf") {
                        if (T) {
                            if (F && "extrafont" %in% (.packages())){
                                message("run `extrafont::embed_fonts()` ...")
                                extrafont::embed_fonts(plotname, outfile=plotname)
                            } else {
                                message("run `grDevices::embedFonts()` ...")
                                grDevices::embedFonts(plotname, outfile=plotname)
                            }
                        } else {
                            message("todo: sometimes pdf font embedding blurrs colors why?")
                        }
                    }
                    message()
                    # finished plot point data vs model data for all settings in one scatter plot
                    

                    # plot model point data vs model data in n scatter plots for nsettings separately
                    
                    # finished plot model point data vs model data in n scatter plots for nsettings separately
                
                } else {
                    message("\no point_data found in lonlatlims ...\n")
                } # if xp is NA or not

            } else {
                message("\nno point_data for zname = ", zname, " ...\n")
            } # if point_data was properly defined or not for current zname


            # make lat regular for `image(..., useRaster=T)` usage
            if (!all(sapply(lapply(lapply(d$lat, diff), unique), length) == 1)) {
                message("make constant dlat for `image(..., useRaster=T)` usage ...")
                for (i in seq_along(d$lat)) {
                    d$lat[[i]] <- array(seq(min(d$lat[[i]]), max(d$lat[[i]]), l=length(d$lat[[i]])))
                }
            }
           
            # load further data to add to lon,lat plot 
            polygon_list <- contour_list <- quiver_list <- NULL # default: add nothing to lon,lat plot

            if (exists("varnames_uv")) {
                if (plot_groups[plot_groupi] == "samevars") {
                    message("zuv_samevars:")
                    cat(capture.output(str(zuv_samevars)), sep="\n")
                    if (zname %in% names(varnames_uv)) { # if current variable is defined by u,v-components
                        znameu <- varnames_uv[[zname]]["u"]
                        znamev <- varnames_uv[[zname]]["v"]
                        message("prepare u,v-components for quivers: take variables \"", znameu, 
                                "\" and \"", znamev, "\" as u and v components ")  
                        quiver_list <- list(u=zuv_samevars[[znameu]], v=zuv_samevars[[znamev]])
                        quiver_list$nx_fac <- rep(0.5, t=length(quiver_list$u))
                        quiver_list$ny_fac <- rep(0.75, t=length(quiver_list$u))
                        quiver_list$const <- rep(T, t=length(quiver_list$u))
                    }
                }
            }
            # finished prepare u,v components if needed
            
            # add special data to z
            if (any(zname == c("quv", "quv_direction"))) {
                if (exists("era5_spatial")) {
                    message("\ntodo: transfer this block to the appending z_samevars/z_samedims section")
                    # find temporal/spatial subset as in models
                    if (zname == "quv") {
                        era5fname <- "era5_viwv_yseasmean_1990-2010.nc"
                        era5zname <- "viwv"
                        era5znameu <- "viwve"
                        era5znamev <- "viwvn"
                    }
                    if (zname == "quv_direction") {
                        era5fname <- "era5_viwv_direction_yseasmean_1990-2010.nc"
                        era5zname <- "viwv_direction"
                    }
                    if (length(unique(sapply(dims, "[[", "time_inds"))) != 1) {
                        warning("there are different `time_inds` available in `dims`. dont know which to use for era5 data \"", 
                                era5zname, "\".")
                    } else {
                        era5timeind <- dims[[1]]$time_inds[1]
                        era5lon <- era5_spatial[[era5fname]]$dims$lon
                        if (any(era5lon >= sapply(d$lon, min)) && any(era5lon <= sapply(d$lon, max))) {
                            era5loninds <- which(era5lon >= sapply(d$lon, min) & era5lon <= sapply(d$lon, max)) 
                        }
                        era5lat <- era5_spatial[[era5fname]]$dims$lat
                        if (any(era5lat >= sapply(d$lat, min)) && any(era5lat <= sapply(d$lat, max))) {
                            era5latinds <- which(era5lat >= sapply(d$lat, min) & era5lat <= sapply(d$lat, max)) 
                        }
                        if (length(era5loninds) <= 1 || length(era5latinds) <= 1) {
                            warning("cannot add era5 data \"", era5zname, 
                                    "\" to plot. had problems selecting spatial subset.")
                        } else {
                            era5lon <- era5lon[era5loninds]
                            era5lat <- era5lat[era5latinds]
                            era5 <- era5_spatial[[era5fname]]$data[[era5zname]][era5loninds,era5latinds,era5timeind]
                            message("special: add era5 (lon,lat,time) subset:")
                            message("-> lon = ", era5lon[1], " to ", era5lon[length(era5lon)], 
                                    "°, lat = ", era5lat[1], " to ", era5lat[length(era5lat)], "°, time = ", 
                                    era5_spatial[[era5fname]]$dims$time[era5timeind], ", min, max = ", appendLF=F)
                            dput(range(era5, na.rm=T))
                            message("to z ...")
                            d$lon <- c(d$lon, list(era5lon))
                            d$lat <- c(d$lat, list(era5lat))
                            z <- c(z, list(era5))
                            names(z)[length(z)] <- paste0("ERA5 ", era5zname)
                            plotname_suffix <- paste0(plotname_suffix, "_ERA")
                            names_legend_p <- c(names_legend_p, paste0("ERA5 ", era5zname))
                            if (!is.null(quiver_list)) { 
                                era5u <- era5_spatial[[era5fname]]$data[[era5znameu]][era5loninds,era5latinds,era5timeind]
                                era5v <- era5_spatial[[era5fname]]$data[[era5znamev]][era5loninds,era5latinds,era5timeind]
                                quiver_list$u <- c(quiver_list$u, list(era5u))
                                quiver_list$v <- c(quiver_list$v, list(era5v))
                                names(quiver_list$u)[length(quiver_list$u)] <- paste0("ERA5 ", era5znameu)
                                names(quiver_list$v)[length(quiver_list$v)] <- paste0("ERA5 ", era5znamev)
                                quiver_list$nx_fac <- c(quiver_list$nx_fac, 0.03)
                                quiver_list$ny_fac <- c(quiver_list$ny_fac, 0.075)
                                quiver_list$const <- c(quiver_list$const, T)
                            }
                        } # if era5 subset could be selected
                    } # if time_ind for era5 can be determined
                } # if exists era5
            } # if quv quv_direction
            # finished add special data to z
            
            # colorbar values
            message("define color levels here if wanted ...")
            message("get global min/max zlim ... ", appendLF=F)
            zlim <- range(z, na.rm=T)
            op <- options()$digits; options(digits=15); cat("=", zlim, "\n"); options(digits=op)

            nlevels <- zlevels <- y_at <- palname <- anom_colorbar <- NULL
            if (zname == "quv") {
                message("special zlim")
                nlevels <- 200
                #zlim <- c(5.03, 324.37) # feb; era5: 0.0244510751217604, 294.239959716797 
                #zlim <- c(6.05, 328.23) # may; era5: 0.00584759470075369, 297.601440429688
                #zlim <- c(12.1, 474.4) # aug; era5: 0.077091708779335, 531.362243652344
                #zlim <- c(8.33, 422.71) # nov; era5: 0.108525462448597, 300.630645751953
                zlim <- c(0.00584759470075369, 531.362243652344)
            } else if (zname == "lm_temp2_as_time_slope") {
                #message("special zlim")
                # Hol-T annual: c(-2.77528234243418, 2.26845881332211)
                #zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=0.25), zlim[2]) # deg C / 7k yrs
                #zlim <- c(-4.3802347784168293998, 2.4162697392007639330)
                #zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=0.5), zlim[2]) # deg C / 7k yrs
                palname <- "colormaps_jaisnd"
            } else if (zname == "lm_THO_as_time_slope") {
                # lm_tho: 360x180: 5 season: c(-4.00305379614524, 3.54971842687043)
                # lm_tho: 3600x1800: 5 season: c(-4.03669006075327, 3.55691367873545)
            } else if (zname == "lm_tsurf_as_time_slope") {
                message("special zlim")
                # Hol-T annual: c(-4.3802347784168293998, 2.4162697392007639330)
                # Hol-T: 5 season: -9.440916  6.903386
                # Hol-T: 3600x1800: 5 season: c(-9.46404887704723, 6.91053024191786)
                zlim <- c(-4.3802347784168293998, 2.4162697392007639330)
                #zlim <- c(-4.03669006075327, 3.55691367873545)
                #zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=0.25), zlim[2]) # deg C / 7k yrs
                zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=0.5), zlim[2]) # deg C / 7k yrs
                palname <- "colormaps_jaisnd"
            } else if (T && zname == "lm_aprt_as_time_slope") {
                message("special zlim")
                # Hol-T and Hol-Tx10 annual: -404.457947500465 233.716296217084
                #zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=25), zlim[2]) # mm/month / 7k yrs
                zlevels <- c(-500, -200, -100, -50, -20, 0, 20, 50, 100, 200, 500) # bartlein et al. 2011 Fig. 6
                palname <- "BrBG"
            } else if (zname == "wisoaprt_d") {
                palname <- "RdYlBu"
                anom_colorbar <- F
            }
            source(paste0(host$homepath, "/functions/image.plot.pre.r"))
            ip <- image.plot.pre(zlim=zlim, nlevels=nlevels, zlevels=zlevels, 
                                 palname=palname, anom_colorbar=anom_colorbar, 
                                 verbose=F)
            if (any(zlim[1] < min(ip$levels))) warning("zlim[1] < min(ip$levels) in lon vs lat plot. do you want that?")
            if (any(zlim[2] > max(ip$levels))) warning("zlim[2] > max(ip$levels) in lon vs lat plot. do you want that?")

            # default names
            if (any(names_legend_p != "")) {
                names_legend_p[which(names_legend_p != "")] <- paste0(letters[which(names_legend_p != "")], ") ", 
                                                                      names_legend_p[which(names_legend_p != "")])
            }


            ## prepare adding further stuff to every subplot
            polygon_list <- segment_list <- point_list <- text_list <- cmd_list <- NULL # default
           

            if (T && zname == "lm_tsurf_as_time_slope" && !all(sapply(ll_data, is.null))) { # add sea ice trend
                message("add SICOMO contours")
                contour_list <- list(x=NULL, y=NULL, z=NULL, levels=NULL)
                for (i in seq_along(ll_data)) {
                    contour_list$x[[i]] <- ll_data[[i]]$lm_SICOMO_as_time_slope$lon
                    contour_list$y[[i]] <- ll_data[[i]]$lm_SICOMO_as_time_slope$lat
                    contour_list$z[[i]] <- ll_data[[i]]$lm_SICOMO_as_time_slope$lm_SICOMO_as_time_slope
                }
                contour_list$levels <- pretty(range(contour_list$z, na.rm=T), n=10)
                plotname_suffix <- paste0(plotname_suffix, "_contour_SICOMO")
            }
            if (T && zname == "lm_tsurf_as_time_slope" && !all(sapply(poly_data, is.null))) { # add THO trend
                message("add THO polys")
                polygon_list <- list(x=NULL, y=NULL, z=NULL, levels=NULL)
                for (i in seq_along(poly_data)) {
                    polygon_list$x[[i]] <- rbind(t(apply(poly_data[[i]]$lm_THO_as_time_slope$lon, 1, as.vector)), NA)
                    polygon_list$y[[i]] <- rbind(t(apply(poly_data[[i]]$lm_THO_as_time_slope$lat, 1, as.vector)), NA)
                    polygon_list$z[[i]] <- as.vector(poly_data[[i]]$lm_THO_as_time_slope$data)
                    # remove cyclic elements
                    inds <- abs(apply(polygon_list$x[[i]], 2, diff))
                    inds <- apply(inds, 2, max, na.rm=T)
                    inds <- which(inds > 100*median(inds, na.rm=T))
                    if (length(inds) > 0) {
                        message("remove ", length(inds), " cyclic polygons ...")
                        polygon_list$x[[i]] <- polygon_list$x[[i]][,-inds]
                        polygon_list$y[[i]] <- polygon_list$y[[i]][,-inds]
                        polygon_list$z[[i]] <- polygon_list$z[[i]][-inds]
                    }
                }
                #polygon_list$levels <- pretty(range(polygon_list$z, na.rm=T), n=10)
                polygon_list$levels <- zlevels
                plotname_suffix <- paste0(plotname_suffix, "_poly_THO")
            }

            # add point data to lon,lat plot
            # --> `point_list` must have at least the 4 entries lon,lat,col,pch
            if (T && !all(sapply(lapply(xp, is.na), all))) {
                message("not all `xp` are NA: add ", 
                        point_datap_suffix, " xp to plot ...")
                point_list <- text_list <- vector("list", l=length(z))
                for (i in seq_along(z)) {
                    # default xp symbols: black cross
                    point_list[[i]] <- list(x=attributes(xp[[i]])$lon,
                                            y=attributes(xp[[i]])$lat)
                    point_list[[i]]$col <- rep("black", t=length(point_list[[i]]$x))
                    point_list[[i]]$pch <- rep(4, t=length(point_list[[i]]$x))
                    text_list[[i]] <- list(x=rep(NA, t=length(xp[[i]])),
                                           y=rep(NA, t=length(xp[[i]])),
                                           labels=rep(NA, t=length(xp[[i]])),
                                           col=rep(NA, t=length(xp[[i]])))
                    # if special color/symbol for xp 
                    if (T) { 
                        
                        ## cols
                        if (F) { # constant color
                            message("   --> color xp by its sign ...")
                            cols <- rep("brown", t=length(xp[[i]])) # color for dry trend
                            if (any(xp[[i]] > 0)) cols[which(xp[[i]] > 0)] <- "blue" # color for wet trend
                        } else if (T) { # same colors as z
                            message("   --> color xp by its value ...")
                            cols <- findInterval(x=xp[[i]], vec=ip$levels, all.inside=T)
                            cols <- ip$cols[cols]
                            if (T && any(xp_varname_unique == "d18Ononp_scaled")) {
                                message("   --> special d18Ononp_scaled: show red/blue plus/minus")
                                inds <- which(xp_varname == "d18Ononp_scaled")
                                cols[inds] <- NA
                                posinds <- which(xp[[i]][inds] >= 0)
                                neginds <- which(xp[[i]][inds] < 0)
                                if (length(posinds) > 0) text_list[[i]]$col[inds][posinds] <- "red"
                                if (length(neginds) > 0) text_list[[i]]$col[inds][neginds] <- "blue"
                            } else if (T && any(xp_varname_unique == "d18Odiatom_scaled")) {
                                message("   --> special d18Odiatom_scaled: show blue/red PLOT letters")
                                inds <- which(xp_varname == "d18Odiatom_scaled")
                                cols[inds] <- NA
                            }
                        }
                        point_list[[i]]$bg <- cols
                        
                        ## pchs:
                        # default: same symbol for all xp
                        point_list[[i]]$pch <- pchs_filled_w_border[1] 
                        if (T) { # distinguish xp by pch
                            message("   distinguish xp symbols ...")
                            pchs <- c(pchs_filled_w_border, (21:25)[!match(21:25, pchs_filled_w_border, nomatch=F)])
                            tmp <- rep(NA, t=length(xp_pchno))
                            for (j in seq_along(xp_pchno_unique)) {
                                tmp[which(xp_pchno == xp_pchno_unique[j])] <- pchs[xp_pchno_unique[j]]
                            }
                            if (T && any(xp_varname_unique == "d18Ononp_scaled")) {
                                message("   --> special d18Ononp_scaled: show red/blue plus/minus")
                                inds <- which(xp_varname == "d18Ononp_scaled")
                                tmp[inds] <- NA
                                text_list[[i]]$x[inds] <- attributes(xp[[i]])$lon[inds]
                                text_list[[i]]$y[inds] <- attributes(xp[[i]])$lat[inds]
                                posinds <- which(xp[[i]][inds] >= 0)
                                neginds <- which(xp[[i]][inds] < 0)
                                if (length(posinds) > 0) text_list[[i]]$labels[inds][posinds] <- "+"
                                if (length(neginds) > 0) text_list[[i]]$labels[inds][neginds] <- "-"
                            } else if (T && any(xp_varname_unique == "d18Odiatom_scaled")) {
                                message("   --> special d18Odiatom_scaled: show blue/red PLOT letters")
                                inds <- which(xp_varname == "d18Odiatom_scaled")
                                tmp[inds] <- NA
                            }
                            point_list[[i]]$pch <- tmp
                        }
                    }
                } # for i in z
                plotname_suffix <- paste0("_vs_xp_", point_datap_suffix) 
            } # if xp is defined

            addland_list <- list(data="world", xlim="xlim", ylim="ylim") # default yes since lon,lat plot
            if (mode_p == "area") addland_list <- NULL # fesom
            if (add_mpiom_GR30_lsm_seg) {
                message("special: add mpiom land sea mask segments to plot ...")
                segment_list <- mpiom_GR30_lsm_seg
                addland_list <- NULL
            }

            # show PLOT coords
            if (exists("PLOT_coords_cmd_list")) {
                message("special: add PLOT coords to plot ...")
                cmd_list <- c(cmd_list, PLOT_coords_cmd_list)
            }

            # determine number of rows and columns
            source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
            if (T && all(areas_p == "N30-90") || all(areas_p == "NAsiberia")) {
                message("special nrow ncol")
                n <- length(z); m <- 1
                nm <- image.plot.nxm(x=d$lon, y=d$lat, z=z, n=n, m=m, ip=ip, dry=T)
                y_at <- pretty(d$lat[[1]], n=5)
            }  else {
                nm <- image.plot.nxm(x=d$lon, y=d$lat, z=z, ip=ip, dry=T)
            }

            # this is the lon vs lat plot: respecting aspect ratio based on dlon and dlat make sense here 
            width_in <- p$a4_width_in # maximum a4 width as threshold (8.26 in)
            message("width_in = ", width_in, " ", appendLF=F)
            if (respect_asp) {
                message("--> `respect_asp`=T ", appendLF=F)
                asp_width_over_height <- sapply(lapply(d$lon, range), diff)/sapply(lapply(d$lat, range), diff) # per subplot
                asp_width_over_height <- max(asp_width_over_height) # makes sense?
                if (asp_width_over_height > aspect_ratio_thr) {
                    message("--> dlon/dlat = ", round(asp_width_over_height, 3), 
                            " > `aspect_ratio_thr` = ", aspect_ratio_thr, " ", appendLF=F)
                    asp_width_over_height <- aspect_ratio_thr
                }
            } else { # use own default aspect ratio 
                asp_width_over_height <- p$map_width/p$map_height 
            }
            message("--> aspect ratio = ", round(asp_width_over_height, 3))
            height_in <- nm$nrow*width_in/asp_width_over_height
            message("height_in = nrow*width_in/aspect_ratio = ", 
                    nm$nrow, "*", width_in, "/", round(asp_width_over_height, 3), " = ", 
                    nm$nrow, "*", round(width_in/asp_width_over_height, 4), " = ",
                    round(height_in, 4), " ", appendLF=F)
            if (height_in > p$a4_height_in) {
                height_in <- p$a4_height_in # take a4 maximum height as threshold (11.58 in)
                message("> ", p$a4_height_in, " --> height_in = ", height_in, 
                        " --> aspect ratio = ", round(width_in/height_in, 3))
            } else {
                message()
            }
            pointsize <- p$pointsize*width_in/p$inch # multiple of default
            
            # open plot device 
            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                               zname, "_", 
                               paste0(names_short_p, "_", seasonsp_p, 
                                      "_", froms_plot_p, "_to_", tos_plot_p, "_", 
                                      areas_p, collapse="_vs_"), 
                               plotname_suffix, ".", p$plot_type)
            if (nchar(plotname) > nchar_max_foutname) {
                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   zname, "_", 
                                   paste(unique(names_short_p), collapse="_"), "_",
                                   paste(unique(seasonsp_p), collapse="_"), "_", 
                                   paste(unique(froms_plot_p), collapse="_"), "-", 
                                   paste(unique(tos_plot_p), collapse="_"), "_", 
                                   paste(unique(areas_p), collapse="_"), 
                                   plotname_suffix, ".", p$plot_type)
            }
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            message("open plot ", plotname, " ...")
            if (p$plot_type == "png") {
                width <- width_in*p$ppi; height <- height_in*p$ppi
                png(plotname, width=width, height=height, res=p$ppi, family=p$family_png, pointsize=pointsize)
            } else if (p$plot_type == "pdf") {
                width <- width_in; height <- height_in
                pdf(plotname, width=width_in, height=height_in, family=p$family_pdf, encoding=encoding, pointsize=pointsize
                    #, paper="a4" # pdf page will alyways have sizes of a4 in inch, no matter what `width` and `height` are
                    )
                #library(showtext)
                #showtext_auto() 
                #def <- get(".PDF.Options.default", envir = grDevices:::.PSenv)
            }
            message("final ", p$plot_type, ifelse(p$plot_type == "png", paste0(" (ppi=", p$ppi, ")"), ""), 
                    " plot width/height = ", width, "/", height, " = ", round(width/height, 3), " ...") 

            # map plot
            source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
            image.plot.nxm(x=d$lon, y=d$lat, z=z, ip=ip, 
                           n=nm$nrow, m=nm$ncol, verbose=T,
                           y_at=y_at,
                           xlab="Longitude [°]", ylab="Latitude [°]", 
                           zlab=data_info$label, znames=names_legend_p, 
                           add_contour=F,
                           polygon_list=polygon_list,
                           quiver_list=quiver_list,
                           contour_list=contour_list, 
                           contour_posneg_redblue=T, 
                           #contour_smooth=T, contour_smooth_n_pixel_thr=50, contour_spar=1,
                           addland_list=addland_list,
                           point_list=point_list,
                           segment_list=segment_list,
                           text_list=text_list,
                           cmd_list=cmd_list)
            
            message("save plot ", plotname, " ...")
            dev.off()
            if (p$plot_type == "pdf") {
                if (T) {
                    if (F && "extrafont" %in% (.packages())){
                        message("run `extrafont::embed_fonts()` ...")
                        extrafont::embed_fonts(plotname, outfile=plotname)
                    } else {
                        message("run `grDevices::embedFonts()` ...")
                        grDevices::embedFonts(plotname, outfile=plotname)
                    }
                } else {
                    message("todo: sometimes pdf font embedding blurrs colors why?")
                }
            }
            #stop("asd")

            # anomaly lon,lat plot of 2 settings 
            if (length(z) == 2) {

                message("\n`length(z)` = 2 ", appendLF=F)

                if (length(d$lon[[1]]) == length(d$lon[[2]]) &&
                    length(d$lat[[1]]) == length(d$lat[[2]])) {
                
                    message("AND both settings have same number of lons and lats\n",
                            "--> plot anomalies 2 minus 1: ", names_short_p[2], " minus ", names_short_p[1], 
                            " as lon vs lat ...")
                    
                    # colorbar values
                    zanom <- list(z[[2]] - z[[1]])
                    names(zanom) <- paste0(names_short_p[2], " minus ", names_short_p[1])
                    source(paste0(host$homepath, "/functions/image.plot.pre.r"))
                    ip <- image.plot.pre(range(zanom, na.rm=T), verbose=F)

                    # determine number of rows and columns
                    source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
                    nm <- image.plot.nxm(x=d$lon[1], y=d$lat[1], z=zanom, ip=ip, dry=T)

                    plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                       varname, "_", 
                                       paste0(rev(names_short_p), "_", rev(seasonsp_p), 
                                              "_", rev(froms_plot_p), "_to_", rev(tos_plot_p), "_", 
                                              rev(areas_p), collapse="_minus_"), 
                                       ".", p$plot_type)
                    message("plot ", plotname, " ...")
                    dir.create(dirname(plotname), recursive=T, showWarnings=F)
                    if (p$plot_type == "png") {
                        png(plotname, width=nm$ncol*p$map_width, height=nm$nrow*p$map_height,
                            res=p$ppi, family=p$family_png)
                    } else if (p$plot_type == "pdf") {
                        pdf(plotname, width=nm$ncol*p$inch, 
                            height=p$inch*((nm$nrow*p$map_height)/(nm$ncol*p$map_width)),
                            family=p$family_pdf, encoding=encoding)
                    }

                    # map plot
                    data_info <- data_infos[[which(sapply(data_infos, names) == varname)[1]]][[varname]]
                    addland_list <- list(data="world", xlim="xlim", ylim="ylim")
                    if (mode_p == "area") addland <- NULL # fesom
                    image.plot.nxm(x=d$lon[1], y=d$lat[1], z=zanom, ip=ip, verbose=T,
                                   xlab="Longitude [°]", ylab="Latitude [°]", 
                                   zlab=data_info$label, 
                                   znames=paste0(names_short_p[2], " minus ", names_short_p[1]),
                                   addland_list=addland_list)
                    
                    message("\nsave plot ", plotname, " ...")
                    dev.off()
                    if (p$plot_type == "pdf") {
                        if (T) {
                            if (F && "extrafont" %in% (.packages())){
                                message("run `extrafont::embed_fonts()` ...")
                                extrafont::embed_fonts(plotname, outfile=plotname)
                            } else {
                                message("run `grDevices::embedFonts()` ...")
                                grDevices::embedFonts(plotname, outfile=plotname)
                            }
                        } else {
                            message("todo: sometimes pdf font embedding blurrs colors why?")
                        }
                    }

                } else { # if lon_dim and lat_dim of both settings are of same length
                    message("but lon and lat dims of both settings are of different length --> cannot plot anomaly as lon vs lat")
                }
            } else { # if length(z) != 2
                message("\nlength(z) != 2 --> cannot plot anomaly as lon vs lat")
            } # finished anomaly plot of 2 dims (lon,lat)



        } # if ndims == 2 and lon,lat
        # finished plot `datas` as lon vs lat


        ## plot `datas` as time vs lat
        if (ndims == 2 && all(dim_names == c("time", "lat"))) {
        
            message("\n", zname, " ", mode_p, " plot time vs lat ...")

            if (add_smoothed) {
                message("\n`add_smoothed` = T --> use zma and not z ...")
                z <- zma
            }
            
            if (F) {
                message("\nspecial: plot values relative to last time point")
                for (i in seq_along(z)) {
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

            # load additional data sets
            addland_list <- NULL

            message("get global zlim ... ", appendLF=F)
            zlim <- range(z, na.rm=T)
            message("min, max = ", zlim[1], ", ", zlim[2])
            zlevels <- NULL
            pos_cols <- NULL
            neg_cols <- NULL
            nlevels <- 11
            if (F && varname == "srad0d") {
                message("special zlevels")
                if (T) { # levels/colors as marcott et al. 2013 Fig. 2 A December
                    zlevels <- seq(-34, 10, b=4)
                    if (min(zlevels) > zlim[1]) zlevels <- c(zlim[1], zlevels)
                    if (max(zlevels) < zlim[2]) zlevels <- c(zlevels, zlim[2])
                    pos_cols <- c("#fbe4f3", "#f7b9de", "#f591cb", "#ec1168")
                    neg_cols <- c("#5b58b0", "#5b58b0", "#c6b7df", "#edeaf7", "#fafbfb")
                    nlevels <- 20
                } else if (T) { # levels/colors as marcott et al. 2013 Fig. 2 B June
                    zlevels <- seq(-4, 36, b=4)
                    if (min(zlevels) > zlim[1]) zlevels <- c(zlim[1], zlevels)
                    if (max(zlevels) < zlim[2]) zlevels <- c(zlevels, zlim[2])
                    pos_cols <- c("#fbd3eb", "#f693cc", "#f584c6", "#ef47a8", "#ec0f64")
                    neg_cols <- "#5b5cb2"
                    nlevels <- 20
                }
            } else if (F && any(varname == c("temp2", "tslm1", "THO"))) {
                message("special zlim")
                #zlim <- c(-0.566165227890015, 2.67736038208008) # annual
                #zlim <- c(-2.46028671264648, 2.82926086425781) # jun
                #zlim <- c(-0.738620281219482, 6.65275375366211) # dec
                message("min, max = ", zlim[1], ", ", zlim[2])
                addland_list <- list(data="world", ylim="ylim")
            }
            if (add_mpiom_GR30_lsm_seg) {
                message("special: add mpiom land sea mask segments to plot ...")
                addland_list <- list(data=mpiom_GR30_lsm_seg, ylim="ylim")
            }
            
            source(paste0(host$homepath, "/functions/image.plot.pre.r"))
            ip <- image.plot.pre(zlim, nlevels=nlevels,
                                 palname="RdBu", 
                                 center_include=T, pos_cols=pos_cols, neg_cols=neg_cols)

            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                               varname, "_",  
                               paste0(names_short_p, "_", areas_p, "_", seasonsp_p, "_", 
                                      froms_plot_p, "_to_", tos_plot_p,
                                      collapse="_vs_"),
                               ifelse(add_smoothed, paste0("_ma", paste(unique(n_mas), collapse="_")), ""),
                               ".", p$plot_type)
            message("plot ", plotname, " ...")
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            
            # determine number of rows and columns
            source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
            nm <- image.plot.nxm(x=d$time, y=d$lat, z=z, verbose=F,
                                 #, n=1, m=2 # special
                                 #, n=1, m=3 # special
                                 , n=2, m=2 # special
                                 , ip=ip, dry=T
                                 )
            
            if (p$plot_type == "png") {
                png(plotname, width=nm$ncol*p$map_width, height=nm$nrow*p$map_height,
                    res=p$ppi, family=p$family_png)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=p$inch, height=p$inch/(p$map_width/p$map_height),
                    family=p$family_pdf, encoding=encoding)
            }

            # plot
            nm <- image.plot.nxm(x=d$time, y=d$lat, z=z
                           #, n=1, m=2 # special
                           #, n=1, m=3 # special
                           , n=2, m=2 # special
                           , ip=ip, verbose=T,
                           individual_zlim=T,
                           contour_only=T, contour_posneg_redblue=T,
                           #add_contour=T,
                           addland_list=addland_list,
                           #xlim=tlimct, 
                           , x_at=tatn, x_labels=tlablt, xlab=tunit, 
                           ylab="Latitude [°]",
                           zlab=data_info$label, znames=names_legend_p)
        
            message("\nsave plot ", plotname, " ...")
            dev.off()
            if (p$plot_type == "pdf") {
                if (T) {
                    if (F && "extrafont" %in% (.packages())){
                        message("run `extrafont::embed_fonts()` ...")
                        extrafont::embed_fonts(plotname, outfile=plotname)
                    } else {
                        message("run `grDevices::embedFonts()` ...")
                        grDevices::embedFonts(plotname, outfile=plotname)
                    }
                } else {
                    message("todo: sometimes pdf font embedding blurrs colors why?")
                }
            }

        } # if ndims == 2 and time, lat
        # finished plot `datas` as time vs lat
        

        ## plot `datas` as time vs depth
        if (ndims == 2 && all(dim_names == c("time", "depth"))) {
       
            if (add_smoothed) {
                message("\n`add_smoothed` = T --> replace z with zma ...")
                z <- zma
            }

            # use km instead of m as depth unit
            if (T) {
                message("divide depth dim by 1000 m --> km")
                depth_dim <- lapply(depth_dim, "/", 1000)
                ylab <- "Depth [km]"
            } else {
                ylab <- "Depth [m]"
            }

            if (add_ts_to_time_vs_depth) {
                message("\nadd time series to datas time vs depth plot ...")

            } # if add_ts_to_time_vs_depth

            # colorbar values
            zlim <- range(z, na.rm=T)
            zlevels <- NULL
            if (F && varname == "thetao") {
                zlevels <- seq(ceiling(zlim[1]), min(10, zlim[2]), b=1)
            }
            source(paste0(host$homepath, "/functions/image.plot.pre.r"))
            ip <- image.plot.pre(zlim, zlevels=zlevels, verbose=T)

            # determine number of rows and columns
            source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
            nm <- image.plot.nxm(x=d$time, y=d$depth, z=z, ip=ip, dry=T)

            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                               varname, "_", 
                               paste0(names_short_p, "_", areas_p, "_", seasonsp_p, "_", 
                                      froms_plot_p, "_to_", tos_plot_p, "_", 
                                      depth_fromsp_p, "-", depth_tosp_p, "m",
                                      collapse="_vs_"), 
                               ".", p$plot_type)
            message("plot ", plotname, " ...")
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            if (p$plot_type == "png") {
                png(plotname, width=nm$ncol*p$ts_width, height=nm$nrow*p$ts_height,
                    res=p$ppi, family=p$family_png)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=nm$ncol*p$inch, 
                    height=p$inch*((nm$nrow*p$ts_height)/(nm$ncol*p$ts_width)),
                    family=p$family_pdf, encoding=encoding)
            }

            # plot
            image.plot.nxm(x=d$time, y=d$depth, z=z, ip=ip, verbose=T,
                           #xlim=tlimct, 
                           x_at=tatn, x_labels=tlablt, 
                           xlab="Time", ylab=ylab,
                           zlab=data_info$label, znames=names_legend_p)
        
            message("\nsave plot ", plotname, " ...")
            dev.off()
            if (p$plot_type == "pdf") {
                if (T) {
                    if (F && "extrafont" %in% (.packages())){
                        message("run `extrafont::embed_fonts()` ...")
                        extrafont::embed_fonts(plotname, outfile=plotname)
                    } else {
                        message("run `grDevices::embedFonts()` ...")
                        grDevices::embedFonts(plotname, outfile=plotname)
                    }
                } else {
                    message("todo: sometimes pdf font embedding blurrs colors why?")
                }
            }

        } # if ndims == 2 and time, depth
        # finished plot `datas` as time vs depth


        ### 1 dim
        ## plot `datas` as time 
        if (ndims == 1 && dim_names == "time") {

            if (!add_unsmoothed && !add_smoothed) {
                warning("both `add_unsmoothed=F` and `add_smoothed=F`. set `add_unsmoothed=T` to show time series.")
                add_unsmoothed <- T # default
            }

            message("\n", varname, " ", mode_p, " plot vs time ...")

            # prepare right axis data if necessary
            if (add_data_right_yaxis_ts) {
                message("\nprepare data right yaxis ..")
                message("update data_right datas for plot groups: samevars and samedims")
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
                if (exists("koehler_etal_2017_paul") && add_koehler_etal_2017_paul) {
                    data_right$data$koehler_etal_2017 <- list(x=koehler_etal_2017_paul$time, 
                                                              y=koehler_etal_2017_paul$ghg_wm2,
                                                              #col=koehler_etal_2017_paul$col, 
                                                              col=1,
                                                              #lty=koehler_etal_2017_paul$lty, 
                                                              lty=2,
                                                              lwd=koehler_etal_2017_paul$lwd, pch=koehler_etal_2017_paul$pch,
                                                              text=koehler_etal_2017_paul$text)
                    data_right$label <- eval(substitute(expression(paste("GHG forcing anomaly [W m"^-2, "]"))))
                    data_right$suffix <- "_with_ghg_koehler_etal_2017"
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
                    data_right <- list(data=vector("list", l=length(z)))
                    names(data_right$data) <- names_short
                    for (i in seq_along(data_right$data)) {
                        inpath <- paste0(host$workpath, "/post/", models[i], "/", mode_p, "/siareas") 
                        fname <- paste0(prefixes[i], "_", mode_p, 
                                        codesf[i], "_siareas_antarctic",
                                        "_", seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                                        depthsf[i], 
                                        ".nc") # todo: levs 
                        ncin <- nc_open(paste0(inpath, "/", fname))
                        data_right$data[[i]] <- list(x=d$time[[i]],
                                                     y=ncvar_get(ncin, "siareas"),
                                                     text=names_legend[i], col=cols[i], 
                                                     lty=3, lwd=1, pch=NA)
                    }
                    data_right$label <- eval(substitute(expression(paste("SH sea ice extent [km"^2, 
                                                                                " " %*% " ", 10^6, "]"))))
                    data_right$suffix <- "_with_siareas"
                } # siarean
                if (F && any(zname == c("temp2", "temp2aprt", "tsurf", "tsurfaprt", "ptemp"))) {
                    message("add d18o to right yaxis")
                    data_right <- list(data=vector("list", l=length(z)))
                    names(data_right$data) <- names_short
                    for (i in seq_along(data_right$data)) {
                        inpath <- paste0(host$workpath, "/post/", models[i], "/",
                                         #mode_p,
                                         "yearsum",
                                         #"seassum",
                                         "/wisoaprt_d_post") 
                        fname <- paste0(prefixes[i], "_",
                                        #mode_p,
                                        "yearsum", 
                                        #"seassum",
                                        codesf[i], "_wisoaprt_d_post_sellevel_2_", areas[i], "_", 
                                        #seasonsf[i], 
                                        "yearsum",
                                        #"seassum",
                                        "_", fromsf[i], "-", tosf[i], 
                                        depthsf[i], 
                                        ".nc") # todo: levs 
                        ncin <- nc_open(paste0(inpath, "/", fname))
                        message("read ", inpath, "/", fname, " ...")
                        data_right$data[[i]] <- list(x=ncin$dim$time$vals,
                                                     y=ncvar_get(ncin, "wisoaprt_d"),
                                                     text="wisoaprt_d_sellevel_2", 
                                                     col=cols[i], 
                                                     #cols="#E41A1C",
                                                     lty=ltys[i]+1, lwd=1, pch=NA)
                        if (substr(ncin$dim$time$units, 1, 11) == "days since ") {
                            data_right$data[[i]]$timelt <- as.POSIXlt(data_right$data[[i]]$x*86400, 
                                                                      origin=substr(ncin$dim$time$units, 
                                                                                    12, 
                                                                                    nchar(ncin$dim$time$units)),
                                                                      tz="UTC")
                        } else {
                            stop("not defined")
                        }
                    } # for i 
                    data_right$label <- eval(substitute(expression(paste(delta, ""^18, "O [\u2030]")))) 
                    data_right$suffix <- "_with_wisoaprt_d_sellevel_2"
                } # load data_right based on variable

                for (i in seq_along(data_right$data)) {
                    
                    if (!is.null(data_right$data[[i]]$timelt)) {

                        # shift data_right time as main data if necessary
                        if (!is.null(dims[[i]]$time_shift_by)) {
                            message("shift data_right years by ", dims[[i]]$time_shift_by, " years ...")
                            data_right$data[[i]]$timelt$year <- data_right$data[[i]]$timelt$year + dims[[i]]$time_shift_by
                        } # shift_by

                        # cut data_right time as main data if necessary
                        if (!is.null(dims[[i]]$time_inds)) {
                            if (!is.null(dims[[i]]$season_inds)) {
                                months_in_right <- unclass(data_right$data[[i]]$timelt)$mon + 1
                                month_inds_right <- months_in_right %in% season_inds
                                time_inds_right <- which(month_inds_right)
                            } else {
                                #time_inds_right <- 
                                stop("not implemented yet")
                            }
                            message("cut right data of length ", length(data_right$data[[i]]$y), " by ", 
                                    length(time_inds_right), " time_inds_right:")
                            ht(time_inds_right)
                            data_right$data[[i]]$timelt <- data_right$data[[i]]$timelt[time_inds_right]
                            data_right$data[[i]]$y <- data_right$data[[i]]$y[time_inds_right]
                        } # if time_inds
                        
                        data_right$data[[i]]$x <- as.POSIXct(data_right$data[[i]]$timelt)

                    } # if !is.null(data_right$data[[i]]$timelt)

                } # for i data_right
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
                    for (i in seq_len(nsettings_right)) {
                        if (F) { # todo: how to do this check autmatically?
                            message("apply run mean with n_mas[", i, "] = ", n_mas[i], " to data_right ...")
                            data_right$data[[i]]$yma <- filter(data_right$data[[i]]$y, rep(1/n_mas[i], t=n_mas[i]))
                        } else {
                            data_right$data[[i]]$yma <- data_right$data[[i]]$y
                        }
                    }
                }

                if (!exists("ylim_right")) { # possibly set by user
                    message("use automatic data right yaxis limits ...")
                    ylim_right <- vector("list", l=length(data_right$data))
                    ylim_right_ma <- ylim_right
                    for (i in seq_len(nsettings_right)) {
                        if (length(data_right$data[[i]]$x) == 1 && data_right$data[[i]]$x == "const") {
                            ylim_right[[i]] <- range(data_right$data[[i]]$y, na.rm=T)
                        } else {
                            timeinds <- which(data_right$data[[i]]$x >= tlimlt[1] & data_right$data[[i]]$x <= tlimlt[2])
                            if (length(timeinds) == 0) {
                                message("all data of data_right$data[[", i, "]]: ", names(data_right$data)[i], 
                                        " are out of tlimlt", appendLF=F)
                                print(range(tlimlt))
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
                    message("add nsidc annual to right ylim ...")
                    ylim_right <- range(ylim_right, nsidc_siareas_annual$siareas, na.rm=T)
                } # if add nsidc
                message("ylim_right=", appendLF=F)
                dput(ylim_right)
                ylim_right[is.infinite(ylim_right)] <- 0
                if (!exists("yat_right")) {
                    message("use automatic data right yaxis labels ...")
                    yat_right <- pretty(ylim_right, n=8)
                }
                ylab_right <- format(yat_right, trim=T)
            } 
            # if add_data_right_yaxis_ts finished prepare right axis data

            if (add_data_upper_xaxis_ts) {
                message("\nprepare data upper xaxis ..")
                message("update data_upper datas for plot groups: samevars and samedims")
                data_upper <- list(data=list())
                tlimlt_upper <- vector("list", l=length(data_upper))
       
                if (varname == "amoc" && any(names_short == "Hol-Tx10")) {
                    message("\nadd Hol-Tx10 ts in model years wout stretching on upper xaxis")
                    data_upper <- list(data=vector("list", l=1))
                    names(data_upper$data) <- "Hol-Tx10"
                    ind <- which(names_short == "Hol-Tx10")
                    x <- dims[[ind]]$timelt
                    yrs_unique <- unique(x$year + 1900)
                    yrs_new <- rep(NA, t=length(x))
                    for (i in seq_along(yrs_unique)) { # model years from 1 to n
                        inds <- which(x$year + 1900 == yrs_unique[i])
                        yrs_new[inds] <- rep(i, t=length(inds))
                    }
                    x$year <- yrs_new - 1900
                    data_upper$data[[1]] <- list(x=x,
                                                 y=z[[ind]],
                                                 ind=ind,
                                                 text=paste0(names_legend[ind], "(model years)"), 
                                                 #col=cols[ind], 
                                                 col="#377EB8",
                                                 lty=2, lwd=1, pch=NA)
                    data_upper$label <- "Model year of accelerated run"
                    data_upper$suffix <- "_with_upper_xaxis"
                    tlimlt_upper[[1]] <- range(data_upper$data[[1]]$x)
                } # if amoc and Hol-Tx10 
                
                # check
                if (length(data_upper$data) == 0) {
                    warning("you provided `add_data_upper_xaxis_ts=T` but did not ",
                            "define which data should be plotted on upper xaxis.\n",
                            " --> set `add_data_upper_xaxis_ts=F` and continue ...")
                    add_data_upper_xaxis_ts <- F
                    data_upper <- list(suffix="") # default
                
                } else {

                    tlim_upper <- range(lapply(tlimlt_upper, as.numeric)) # seconds by default 
                    tlimlt_upper <- as.POSIXlt(tlim_upper, origin="1970-01-01", tz="UTC")
                    tlimlt_upper <- range(tlimlt_upper)
                    
                    # upper time labels
                    tlablt_upper <- as.POSIXlt(pretty(tlimlt_upper, n=10)) # this does not work with large negative years, e.g. -800000 (800ka) 

                    # remove lables which are possibly out of limits due to pretty
                    tlab_diff_secs <- as.numeric(diff(range(tlablt_upper)), units="secs") # duration of time labels
                    if (any(tlablt_upper < tlimlt_upper[1])) {
                        # check if the too early autmatic time labels are negligible
                        overshoot_diff <- abs(as.numeric(tlablt_upper[tlablt_upper < tlimlt_upper[1]], units="secs") - tlim_upper[1])
                        overshoot_rel <- overshoot_diff/tlab_diff_secs*100
                        if (any(overshoot_rel > 1)) { # todo: only change pretty labels if overshoot is > 1% of total time label range  
                            message("remove some automatic labels < ", tlimlt[1], " ...")
                            print(tlablt_upper[which(tlablt_upper < tlimlt_upper[1])[overshoot_rel > 1]])
                            tlablt_upper <- tlablt_upper[-which(tlablt_upper < tlimlt_upper[1])[overshoot_rel > 1]]
                        }
                    }
                    if (any(tlablt_upper > tlimlt_upper[2])) {
                        # check if the too late automatic time labels are negligible
                        overshoot_diff <- abs(as.numeric(tlablt_upper[tlablt_upper > tlimlt_upper[2]], units="secs") - tlim_upper[2])
                        overshoot_rel <- overshoot_diff/tlab_diff_secs*100
                        if (any(overshoot_rel > 1)) { # todo: only change pretty labels if overshoot is > 1% of total time label range  
                            message("remove some automatic labels > ", tlimlt_upper[2], " ...")
                            print(tlablt_upper[which(tlablt_upper > tlimlt_upper[2])[overshoot_rel > 1]])
                            tlablt_upper <- tlablt_upper[-which(tlablt_upper > tlimlt_upper[2])[overshoot_rel > 1]]
                        }
                    }
                    tatn_upper <- as.numeric(tlablt_upper)
                    
                    # modify time axis labels YYYY-MM-DD depending on range covered:
                    if (tlab_diff_secs >= 360*24*60*60) { # do not show days if range of tlim is above 1 year
                        message("duration of time dim is longer than 1 year --> ",
                                "use year only as time labels and set `tunit` from \"time\" to \"year\" ...")
                        tlablt_upper <- tlablt_upper$year + 1900 # YYYY; this destroys POSIX object
                    } else { # decrease label size due to long labels
                        message("duration of time dim is shorter than 1 year --> ",
                                "change time label angle ...")
                        tlabsrt_upper <- 45
                    }
                    message("tlablt_upper= ", paste(tlablt_upper, collapse=", "))

                } # if length(data_upper$data) == 0

            } else { # add_data_upper_xaxis_ts=F
                data_upper <- list(suffix="") # default
            } # if add_data_upper_xaxis_ts

            # after check
            if (add_data_upper_xaxis_ts) {
                
                nsettings_upper <- length(data_upper$data)

                if (add_smoothed) {
                    for (i in seq_len(nsettings_upper)) {
                        data_upper$data[[i]]$yma <- filter(data_upper$data[[i]]$y, 
                                                           rep(1/n_mas[data_upper$data[[i]]$ind], 
                                                               t=n_mas[data_upper$data[[i]]$ind]))
                    }
                }

                if (!exists("ylim_upper")) { # possibly set by user
                    message("use automatic data upper xaxis limits ...")
                    ylim_upper <- vector("list", l=length(data_upper$data))
                    ylim_upper_ma <- ylim_upper
                    for (i in seq_len(nsettings_upper)) {
                        ylim_upper[[i]] <- range(data_upper$data[[i]]$y, na.rm=T)
                        if (add_smoothed) {
                            ylim_upper_ma[[i]] <- range(data_upper$data[[i]]$yma, na.rm=T)
                        }
                    } # i in data_upper
                    if ((add_unsmoothed && add_smoothed) ||
                        (add_unsmoothed && !add_smoothed)) {
                        ylim_upper <- range(ylim_upper)
                    } else if (!add_unsmoothed && add_smoothed) {
                        ylim_upper <- range(ylim_upper_ma)
                    }
                } # if ylim_upper does not already exist
                
                message("ylim_upper=", appendLF=F)
                dput(ylim_upper)
                ylim_upper[is.infinite(ylim_upper)] <- 0
            } 
            # if add_data_upper_xaxis_ts finished prepare upper axis data
            
            if (center_ts || scale_ts) {
                if (center_ts) {
                    message("\n`center_ts` = T --> center ts before plot ...")
                } else if (scale_ts) {
                    message("\n`scale_ts` = T --> scale ts before plot ...")
                }
                for (i in seq_along(z)) {
                    if (center_ts) {
                        z[[i]] <- scale(z[[i]], scale=F)
                        if (exists("zma")) zma[[i]] <- scale(zma[[i]], scale=F)
                    } else if (scale_ts) {
                        z[[i]] <- scale(z[[i]])
                        if (exists("zma")) zma[[i]] <- scale(zma[[i]])
                    }
                }
            } # if center_ts or scale_ts

            # ylims of model data
            if (add_unsmoothed) {
                message("\n", mode_p, " versus time min / mean / max ", varname, " z:")
                for (i in seq_along(z)) {
                    message(names_short_p[i], ": ", min(z[[i]], na.rm=T), " / ",
                            mean(z[[i]], na.rm=T), " / ", max(z[[i]], na.rm=T))
                }
                ylim <- range(z, na.rm=T)
            }
            if (exists("zma")) {
                message("\n", mode_p, " versus time min / mean / max ", varname, " zma:")
                for (i in seq_along(z)) {
                    message(names_short_p[i], ": ", min(zma[[i]], na.rm=T), " / ",
                            mean(zma[[i]], na.rm=T), " / ", max(zma[[i]], na.rm=T))
                }
                ylimma <- range(zma, na.rm=T)
            }
            if (add_unsmoothed && add_smoothed) {
                ylim <- range(ylim, ylimma)
            } else if (!add_unsmoothed && add_smoothed) {
                ylim <- range(ylimma)
            }
            ylim[is.infinite(ylim)] <- 0

            if (add_data_upper_xaxis_ts) {
                ylim <- range(ylim, ylim_upper)
            } # change ylim according to ylim_upper

            # ylim of obs, etc.
            if (F && varname == "temp2") {
                message("\nadd hadcrut4_sat_anom, gistempv4_sat_anom to ylim ...")
                ylim <- range(ylim,
                              hadcrut4_sat_anom_annual$hadcrut4_sat_anom_lower_uncert,
                              hadcrut4_sat_anom_annual$hadcrut4_sat_anom_upper_uncert,
                              #gistempv4_sat_anom_annual$gistempv4_sat_anom, 
                              na.rm=T)
            } # if add som obs to ylim

            if (F && varname == "moc_max_26.25deg") {
                message("\nadd rapid$moc_annual to ylim ...")
                ylim <- range(ylim, moc_rapid$moc_annual, na.rm=T)
            } # if add moc ts
            
            if (T && varname == "siarean") {
                message("\nadd nsidc annual to ylim ...")
                ylim <- range(ylim, nsidc_siarean_annual$siarean, na.rm=T)
            } # if add nsidc

            if (T && any(varname == c("wisoaprt_d", "wisoaprt_d", "wisoevap_d", "wisope_d")) && 
                exists("kostrova_etal_2019") &&
                all(grepl("ladoga", areas))) {
                message("\nadd kostrova et al. 2019 d18o data ...")
                message("ylim before: ", ylim[1], ", ", ylim[2])
                if (center_ts) kostrova_etal_2019$d18o <- scale(kostrova_etal_2019$d18o, scale=F)
                if (scale_ts) kostrova_etal_2019$d18o <- scale(kostrova_etal_2019$d18o)
                ylim <- range(ylim, kostrova_etal_2019$d18o)
                message("ylim after: ", ylim[1], ", ", ylim[2])
            }

            if (#any(varname == c("wisoaprt_d", "wisoaprt_d", "wisoevap_d", "wisope_d")) && 
                exists("meyer_etal")) {
                add_meyer_etal_xlsx <- T
                message("\nadd meyer et al. xlsx d18o data ...")
                if (all(grepl("ladoga", areas))) {
                    meyer_etal_tmp <- meyer_etal$data$"ladoga"
                } else if (all(grepl("shuchye", areas))) {
                    meyer_etal_tmp <- meyer_etal$data$"shuchye"
                } else if (all(grepl("emanda", areas))) {
                    meyer_etal_tmp <- meyer_etal$data$"emanda"
                } else if (all(grepl("elgygytgyn", areas))) {
                    meyer_etal_tmp <- meyer_etal$data$"elgygytgyn"
                } else if (all(grepl("two-jurts", areas))) {
                    meyer_etal_tmp <- meyer_etal$data$"two-jurts"
                } else if (all(grepl("kotokel", areas))) {
                    meyer_etal_tmp <- meyer_etal$data$"kotokel"
                } else {
                    message("no meyer et al. xlsx data available for some of the areas\n",
                            paste(areas, collapse=", "))
                    add_meyer_etal_xlsx <- F
                }
                if (add_meyer_etal_xlsx) {
                    if (center_ts) meyer_etal_tmp$data$d18o_corr_perm <- scale(meyer_etal_tmp$data$d18o_corr_perm, scale=F)
                    if (scale_ts) meyer_etal_tmp$data$d18o_corr_perm <- scale(meyer_etal_tmp$data$d18o_corr_perm)
                    message("ylim before: c(", ylim[1], ", ", ylim[2], ")")
                    message("ylim meyer_etal: c(", range(meyer_etal_tmp$data$d18o_corr_perm)[1], 
                            ", ", range(meyer_etal_tmp$data$d18o_corr_perm)[2], ")")
                    ylim <- range(ylim, meyer_etal_tmp$data$d18o_corr_perm, na.rm=T)
                    # ylims Hol-T* and meyer et al. xlsx d18o lakes
                    if (center_ts) {
                        # ladoga: c(-3.30543235329369, 1.8024062688112) 
                        # shuchye: c(-3.31588729672117, 5.07708695323018)
                        # emanda: c(-1.69386684350292, 0.902706467116598)
                        # elgygytgyn: c(-0.859484244878168, 0.837003798075255)
                        # two-jurts: c(-2.05770537576561, 1.94485402526289)
                        # kotokel: c(-4.32039527896077, 2.06724117367673)
                        message("special ylim")
                        ylim <- c(-4.32039527896077, 5.07708695323018)
                    }
                    if (scale_ts) {
                        ## meyer et al. 10k BP 
                        # ladoga: c(-2.71845068304792, 1.12244344859212)
                        # shuchye: c(-2.06540423594796, 2.95748401109693)
                        # emanda: c(-1.98885622964664, 2.40795779371568)
                        # kotokel: c(-3.08014665995613, 1.35190103264372)
                        # elgygytgyn: c(-1.47405790466153, 1.61041540752816)
                        # two-jurts: c(-2.1518519172735, 2.0338372598759)
                        # hol-t, hol-tx10; n_mas 250, 50
                        # ladoga: c(-2.85126486366215, 2.32484231975009)
                        # shuchye: c(-2.85992909997141, 2.44526416112507)
                        # emanda: c(-2.06748945256795, 2.55525657817665)
                        # kotokel: c(-2.60277676884169, 2.96409565621687)
                        # elgygytgyn: c(-2.37444352133196, 3.04432535587108)
                        # two-jurts: c(-2.70199788713988, 3.21519386227393) 
                        message("special ylim")
                        ylim <- c(-3.08014665995613,3.215193862273933)
                    }
                    message("ylim after: c(", ylim[1], ", ", ylim[2], ")")
                } # if add_meyer_etal_xlsx
            } else {  
                add_meyer_etal_xlsx <- F
            } # if add meyer et al xlsx

            if (T && exists("noaa_ghcdn")) {
                if (any(varname == c("temp2", "tsurf", "aprt"))) {
                    message("\nadd noadd ghcdn monthly data\n", 
                            "check https://github.com/chrisdane/PLOT/blob/master/lakes/lake_coords_closest_GHCDN_stations.txt ...")
                    message("ylim before: ", ylim[1], ", ", ylim[2])
                    if (all(grepl("ladoga", areas))) {
                        noaa_ghcdn_tmp <- noaa_ghcdn$RSM00022802_SORTAVALA_RS
                    } else if (all(grepl("shuchye", areas))) {
                        noaa_ghcdn_tmp <- noaa_ghcdn$RSM00023226_VORKUTA_RS
                    } else if (all(grepl("levinson-lessing", areas)) || all(grepl("taymyr", areas))) {
                        noaa_ghcdn_tmp <- noaa_ghcdn$RSM00021802_SASKYLAH_RS
                    } else if (all(grepl("emanda", areas))) {
                        noaa_ghcdn_tmp <- noaa_ghcdn$RSM00024671_TOMPO_RS
                    } else if (all(grepl("elgygytgyn", areas))) {
                        noaa_ghcdn_tmp <- noaa_ghcdn$RSM00025248_ILIRNEJ_RS
                    } else if (all(grepl("two-jurts", areas))) {
                        noaa_ghcdn_tmp <- noaa_ghcdn$RSM00032389_KLJUCHI_RS
                    } else if (all(grepl("kotokel", areas))) {
                        noaa_ghcdn_tmp <- noaa_ghcdn$RSM00030731_GORJACINSK_RS
                    } else {
                        stop("noaa ghcdn data not defined for areas\n",
                             paste(areas, collapse=", "))
                    }
                    noaax <- noaa_ghcdn_tmp$ts$time
                    if (any(varname == c("temp2", "tsurf"))) {
                        noaay <- noaa_ghcdn_tmp$ts$Tavg
                    } else if (varname == "aprt") {
                        noaay <- noaa_ghcdn_tmp$ts$precip
                    }
                    if (center_ts) noaay <- scale(noaay, scale=F)
                    if (scale_ts) noaay <- scale(noaay)
                    if (exists("zma")) {
                        if (length(unique(n_mas)) != 1) stop("different n_ma present. dont know which to use for noaa ghcdn station data")
                        message("filter(noaay) by n_mas[1] = ", n_mas[1])
                        noaay <- filter(noaay, rep(1/n_mas[1], t=n_mas[1]))
                    }
                    ylim <- range(ylim, noaay, na.rm=T)
                    message("ylim after: ", ylim[1], ", ", ylim[2])
                } # if temp2, tsurf, aprt
            } # if exists("noaa_ghcdn")

            # increase ylim for legend if many settings
            if (F && length(z) > 6) {
                message("\ninrease ylim for ts legend ...")
                ylim[2] <- ylim[2] + 0*diff(ylim)
            }
            
            message("\nfinal ylim=", appendLF=F)
            dput(ylim)
            yat <- pretty(ylim, n=8)
            ylab <- format(yat, trim=T)

            # plotname
            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                               varname, "_",
                               paste0(names_short_p, "_", seasonsp_p, "_",
                                      froms_plot_p, "_to_", tos_plot_p, 
                                      n_mas_fname_p, "_", 
                                      areas_p, collapse="_vs_"), 
                               data_right$suffix, data_upper$suffix, 
                               ts_highlight_seasons$suffix,
                               plotname_suffix,
                               ".", p$plot_type)
            if (nchar(plotname) > nchar_max_foutname) {
                if (plot_groups[plot_groupi] == "samevars") {
                    plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                       varname, "_",
                                       paste0(names_short_p, n_mas_fname_p, "_", areas_p, collapse="_vs_"), 
                                       data_right$suffix, data_upper$suffix, ts_highlight_seasons$suffix,
                                       plotname_suffix,
                                       ".", p$plot_type)
                } else if (plot_groups[plot_groupi] == "samedims") {
                    plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                       varname, "_",
                                       paste0(names_short_p, "_", varnames_in_p, collapse="_vs_"), 
                                       data_right$suffix, data_upper$suffix, ts_highlight_seasons$suffix,
                                       plotname_suffix,
                                       ".", p$plot_type)
                }
            }
            if (nchar(plotname) > nchar_max_foutname) {
                warning("plotname = ", plotname, "\nnchar(plotname) = ", nchar(plotname), 
                        " > nchar_max_foutname = ", nchar_max_foutname, " -> cut too long plotname ...")
                plotname <- substr(plotname, 1, nchar_max_foutname)
            }
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            if (p$plot_type == "png") {
                png(plotname, 
                    width=p$ts_width, 
                    height=p$ts_height,
                    #height=p$ts_width,
                    #width=p$map_width, 
                    #height=p$map_height,
                    res=p$ppi, family=p$family_png)
            } else if (p$plot_type == "pdf") {
                #message("special plot size")
                pdf(plotname, width=p$inch, 
                    height=p$inch*p$ts_height/p$ts_width,
                    #height=p$inch*p$map_height/p$map_width,
                    family=p$family_pdf, encoding=encoding)
            }

            # set plot margins
            mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
            mar[4] <- 1 # decrease right margin
            if (!add_title && !add_data_upper_xaxis_ts) mar[3] <- 1 # decrease upper margin
            if (F) {
                if (tlabsrt == 0) mar[1] <- mar[1]/2  # decrease lower margin
            }
            if (add_data_right_yaxis_ts) mar[4] <- mar[2] # right same as left  

            # open plot
            par(mar=mar)
            plot(d$time[[1]], z[[1]], t="n",
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
            mtext(side=1, tunit, line=2, cex=0.9)

            # add variable label on y-axis
            mtext(side=2, data_info$label, line=4.5, cex=0.9)
            
            # add title
            if (add_title) {
                title <- paste0(paste(unique(areas_p), collapse=","), 
                                " ", mode_p, " ", varname, " ", 
                                paste(unique(seasonsp_p), collapse=","), " ", 
                                paste(unique(froms_plot_p), collapse=","), " to ", 
                                paste(unique(tos_plot_p), collapse=","))
                title(title, cex.main=0.75)
            }

            # add grid
            if (add_xgrid) {
                message("\nadd xgrid ...")
                abline(v=tatn, col="gray", lwd=0.5)
            }
            if (add_ygrid) {
                message("\nadd ygrid ...")
                abline(h=yat, col="gray", lwd=0.5)
            }

            # add zero line
            if (add_zeroline) {
                abline(h=0, col="gray", lwd=0.5)
            }

            # add model data
            # unsmoothed before smoothed data
            #if (!is.null(data[[i]])) {
            if (add_unsmoothed) {

                # use different col/lty/pch for different seasons 
                if (ts_highlight_seasons$bool) {
                    message("\n`ts_highlight_seasons$bool`=T ...")
                    for (i in seq_along(z)) {
                        stop("update with dinds/vinds")
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
                                warning("did not find any of these month numbers in time dim values of data, skip")
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
                            } # if wanted month numbers were found in time dim values of data
                        } # for seasi in ts_highlight_seasons$seasons
                    } # for i seq_along(z)
                    
                    if (add_legend && i == length(z)) {
                        message("\nadd default stuff to ", mode_p, " legend ...")
                        le <- list()
                        if (suppressPackageStartupMessages(require(Hmisc))) {
                            tmp <- Hmisc::largest.empty(x=unlist(d$time), y=unlist(z), method="area")
                            le$pos <- c(x=min(tmp$rect$x), y=max(tmp$rect$y))
                        } else {
                            le$pos <- "bottom" 
                        }
                        le$ncol <- length(ts_highlight_seasons$seasons)
                        le$cex <- 0.85
                        le$text <- ts_highlight_seasons$seasons
                        le$col <- ts_highlight_seasons$cols
                        le$pch <- ts_highlight_seasons$pchs
                        le$lty <- rep(NA, t=length(ts_highlight_seasons$seasons))
                        le$lwd <- rep(NA, t=length(ts_highlight_seasons$seasons))
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
                    
                # default unsmoothed data 
                } else { # if not (ts_highlight_seasons$bool)
                    for (i in seq_along(z)) {
                        points(d$time[[i]], z[[i]], t=types_p[i],
                               col=ifelse(add_smoothed, cols_rgb_p[i], cols_p[i]), 
                               lty=ltys_p[i], lwd=lwds_p[i], pch=pchs_p[i])
                    }
                } # if (ts_highlight_seasons$bool)
            } # if add_unsmoothed

            # add smoothed data after unsmoothed data
            if (add_smoothed) {
                for (i in seq_along(zma)) {
                    lines(d$time[[i]], zma[[i]], 
                          col=cols_p[i], lty=ltys_p[i], lwd=lwds_p[i], pch=pchs_p[i])
                }
            }

            # special: add first data point
            if (show_first_data_point) {
                for (i in seq_along(z)) {
                    if (i == 1) message("\n`show_first_data_point`=T --> show first data point")
                    points(d$time[[i]][1], z[[i]][1], 
                           col=cols_p[i], lty=ltys_p[i], lwd=lwds_p[i], 
                           pch=1)
                }
            } # add first data point

            # add linear regression trend if wanted
            if (any(add_linear_trend)) {
                message("\ncalc and add linear trend against time ...")
                lm_labels <- rep(NA, t=length(z))
                names(lm_labels) <- names_short_p
                for (i in seq_along(z)) {
                    if (add_linear_trend[i]) {
                        message("\nsetting ", i, " ", names_short_p[i], ":")
                        lm <- lm(z[[i]] ~ d$time[[i]])
                        lm_summary <- summary(lm)
                        print(lm_summary)
                        lm_labels[i] <- ifelse(diff(range(lm$fitted.values)) > 0, "+", "-")
                        lm_labels[i] <- paste0(lm_labels[i], 
                                               #round(diff(range(lm$fitted.values)), 4)
                                               round(diff(range(lm$fitted.values)), 2)
                                               , " ", data_info$units
                                               )
                        message("--> ", lm_labels[i], " from ", 
                                min(d$time[[i]]), " to ", max(d$time[[i]]), " (", 
                                length(unique(dims[[i]]$timelt$year+1900)), " years)")
                        # plot regression line within data limits only
                        if (T) {
                            message("draw linear regression line within regression limits only ...")
                            lines(d$time[[i]], lm$fitted.values, 
                                  col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i])
                        # or plot line through whole plot with regression coefficients
                        } else if (F) {
                            message("draw linear regression line through whole plot ...")
                            abline(a=lm$coefficients[1], b=lm$coefficients[2],
                                   col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i])
                        }
                    }
                }
            } # add_linear_trend
            
            ## add obs, etc.
            if (F && varname == "temp2") {
                message("\nadd hadcrut4_sat_anom, gistempv4_sat_anom to datas plot ...")
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
                message("\nadd moc_rapid$moc_annual to datas plot ...")
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
                message("\nadd nsidc annual to datas plot ...")
                lines(nsidc_siarean_annual$time, nsidc_siarean_annual$siarean,
                      col=nsidc_siarean_annual$col, lty=nsidc_siarean_annual$lty,
                      lwd=nsidc_siarean_annual$lwd)
            }
            
            if (T && any(varname == c("wisoaprt_d", "wisoaprt_d", "wisoevap_d", "wisope_d")) && 
                exists("kostrova_etal_2019") &&
                all(grepl("ladoga", areas))) {
                message("\nadd kostrova et al. 2019 to datas plot ...")
                points(kostrova_etal_2019$time, kostrova_etal_2019$d18o,
                       t=kostrova_etal_2019$type, col=kostrova_etal_2019$col, 
                       lty=kostrova_etal_2019$lty, lwd=kostrova_etal_2019$lwd, 
                       pch=kostrova_etal_2019$pch, cex=kostrova_etal_2019$cex)
            }
            
            if (add_meyer_etal_xlsx) {
                message("\nadd meyer et al. xlsx to datas plot ...")
                points(meyer_etal_tmp$data$time, meyer_etal_tmp$data$d18o_corr_perm,
                       t=meyer_etal$type, col=meyer_etal$col, 
                       lty=meyer_etal$lty, lwd=meyer_etal$lwd, 
                       pch=meyer_etal$pch, cex=meyer_etal$cex)
            }
            
            if (T && exists("noaa_ghcdn")) {
                if (any(varname == c("temp2", "tsurf", "aprt"))) {
                    message("\nadd noadd ghcdn monthly data to datas plot ...")
                    points(noaax, noaay,
                           t=noaa_ghcdn_tmp$type, 
                           col=noaa_ghcdn_tmp$col,
                           lty=noaa_ghcdn$lty, 
                           lwd=noaa_ghcdn$lwd,
                           pch=noaa_ghcdn$pch, 
                           cex=noaa_ghcdn$cex)
                } # if temp2, tsurf, aprt
            } # if exists("noaa_ghcdn")
            # finished adding obs

            # add legend if wanted
            if (add_legend) {
                message("\nadd default stuff to datas legend here1 ...")
                le <- list()
                if (F && suppressPackageStartupMessages(require(Hmisc))) {
                    tmp <- Hmisc::largest.empty(x=unlist(d$time), y=unlist(z), method="area")
                    le$pos <- c(x=min(tmp$rect$x), y=max(tmp$rect$y)) # topleft corner
                    message("automatically derived legend position: ", le$pos[1], ", ", le$pos[2])
                } else if (T && suppressPackageStartupMessages(require(adagio))) {
                    tmp <- adagio::maxempty(x=unlist(d$time), y=unlist(z), ax=par("usr")[1:2], ay=par("usr")[3:4])
                    #rect(tmp$rect[1], tmp$rect[2], tmp$rect[3], tmp$rect[4])
                    le$pos <- c(x=tmp$rect[1], y=tmp$rect[4]) # topleft corner if x- and y-coords are both increasing (default)
                    message("automatically derived adagio::maxempty legend position: ", le$pos[1], ", ", le$pos[2])
                } else {
                    le$pos <- "bottom" 
                    #le$pos <- "topleft" 
                    #le$pos <- "left"
                    #le$pos <- "bottomleft"
                    #le$pos <- "topright"
                    #le$pos <- "bottomright" 
                    #le$pos <- c(tatn[1], yat[length(yat)-1])
                    #le$pos <- c(as.POSIXct("2650-1-1", tz="UTC"), 13.45)
                    #le$pos <- c(as.POSIXct("2650-1-1", tz="UTC"), yat[length(yat)])
                    message("manually set legend position: ", appendLF=F)
                    if (length(le$pos) == 1) {
                        message(le$pos) 
                    } else {
                        message(le$pos[1], ", ", le$pos[2])
                    }
                }
                #le$ncol <- ceiling(length(z)/4) 
                #le$ncol <- length(z)
                #le$ncol <- length(z)/2
                le$ncol <- 1
                #le$ncol <- 2
                #le$ncol <- length(z)
                if (le$ncol > length(z)) stop("defined more legend columns than data") 
                le$cex <- 1
                #le$cex <- 0.85
                #le$cex <- 0.7
                #le$cex <- 0.75
                #le$cex <- 0.66
                #le$cex <- 0.5
                names_legend_p_w_lm <- names_legend_p
                if (any(add_linear_trend)) {
                    names_legend_p_w_lm[which(!is.na(lm_labels))] <- 
                        paste0(names_legend_p_w_lm[which(!is.na(lm_labels))], " ", 
                               lm_labels[which(!is.na(lm_labels))])
                }
                le$text <- names_legend_p_w_lm
                le$col <- cols_p
                le$lty <- ltys_p
                le$lwd <- lwds_p
                le$pch <- pchs_p
                for (i in seq_along(z)) {
                    if (types_p[i] == "p") {
                        le$lty[i] <- NA
                    } else if (types_p[i] == "l") {
                        le$pch[i] <- NA
                    }
                }
                # add stuf to legend here
                if (F && varname == "temp2") {
                    message("\nadd non hadcrut4 to ", mode_p, " datas legend ...")
                    if (varname == "temp2") {
                        le$text <- c(le$text, hadcrut4_sat_anom_annual$text)
                        le$col <- c(le$col, hadcrut4_sat_anom_annual$col)
                        le$lty <- c(le$lty, hadcrut4_sat_anom_annual$lty)
                        le$lwd <- c(le$lwd, hadcrut4_sat_anom_annual$lwd)
                        le$pch <- c(le$pch, hadcrut4_sat_anom_annual$pch)
                    }
                }
                if (T && any(varname == c("wisoaprt_d", "wisoaprt_d", "wisoevap_d", "wisope_d")) &&
                    exists("kostrova_etal_2019") &&
                    all(grepl("ladoga", areas))) {
                    message("\nadd kostrova et al. 2019 to datas legend ...")
                    le$pos <- "bottom"
                    le$text <- c(le$text, kostrova_etal_2019$text)
                    le$col <- c(le$col, kostrova_etal_2019$col)
                    le$lty <- c(le$lty, kostrova_etal_2019$lty)
                    le$lwd <- c(le$lwd, kostrova_etal_2019$lwd)
                    le$pch <- c(le$pch, kostrova_etal_2019$pch)
                }
                if (add_meyer_etal_xlsx) {
                    message("\nadd meyer et al. xlsx to datas legend ...")
                    if (all(grepl("ladoga", areas))) le$pos <- "top"
                    if (all(grepl("shuchye", areas))) le$pos <- "top"
                    if (all(grepl("emanda", areas))) le$pos <- "topright"
                    if (all(grepl("kotokel", areas))) le$pos <- "top"
                    if (all(grepl("elgygytgyn", areas))) le$pos <- "top"
                    if (all(grepl("two-jurts", areas))) le$pos <- "top"
                    if (all(grepl("kotokel", areas))) le$pos <- "bottom"
                    le$cex <- 0.7
                    le$text <- c(le$text, meyer_etal_tmp$text)
                    le$col <- c(le$col, meyer_etal$col)
                    le$lty <- c(le$lty, meyer_etal$lty)
                    le$lwd <- c(le$lwd, meyer_etal$lwd)
                    le$pch <- c(le$pch, meyer_etal$pch)
                }
                if (T && exists("noaa_ghcdn")) {
                    if (any(varname == c("temp2", "tsurf", "aprt"))) {
                        message("\nadd noadd ghcdn monthly data to legend ...")
                        le$text <- c(le$text, noaa_ghcdn_tmp$text)
                        le$col <- c(le$col, noaa_ghcdn_tmp$col)
                        le$lty <- c(le$lty, noaa_ghcdn_tmp$lty)
                        le$lwd <- c(le$lwd, noaa_ghcdn_tmp$lwd)
                        le$pch <- c(le$pch, noaa_ghcdn_tmp$pch)
                    } # temp2, tsurf, aprt
                } # noadd_ghcdn

                # reorder reading direction from R's default top->bottom to left->right
                if (T) {
                    message("\nreorder legend from top->bottom to left->right")
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

            # add box before eventual right axis data
            box()

            if (add_data_right_yaxis_ts) {

                message("\n`add_data_right_yaxis_ts` = T --> add data right yaxis ...")
                par(new=T)
                plot(data_right$data[[1]]$x, data_right$data[[1]]$y, #log="y", 
                     t="n", xlim=tlim, ylim=ylim_right, 
                     xlab=NA, ylab=NA, axes=F)

                # add right axes in same color as the right data if 
                # all colors of the right data are the same
                if (F && length(unique(sapply(data_right$data, "[", "col"))) == 1) {
                    message("length of data_right = 1 --> use color \"", data_right$data[[1]]$col, "\" to distinguis from left xaxis")
                    right_axis_col <- data_right$data[[1]]$col
                } else {
                    message("length of data_right != 1 --> use color \"black\" for right xaxis")
                    right_axis_col <- "black" # default
                }
                axis(4, at=yat_right, labels=ylab_right, las=2, 
                     col=right_axis_col, col.axis=right_axis_col, col.ticks=right_axis_col)
                mtext(side=4, data_right$label, line=4.5, cex=0.9, col=right_axis_col)

                # add obs before model data
                if (T && varname == "siarean") {
                    message("\nadd nsidc annual to right plot ...")
                    lines(nsidc_siareas_annual$time, nsidc_siareas_annual$siareas,
                          col=nsidc_siareas_annual$col, lty=nsidc_siareas_annual$lty,
                          lwd=nsidc_siareas_annual$lwd)
                }

                # add unsmoothed right data before smoothed
                if (add_unsmoothed) {
                    for (i in seq_along(data_right$data)) {
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
                    for (i in seq_along(data_right$data)) {
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
                    message("\nadd default stuff to right_data legend here1 ...")
                    le <- list()
                    #le$pos <- "topright" 
                    le$pos <- "bottomright" 
                    le$ncol <- 1
                    le$cex <- 1
                    le$cex <- 0.85
                    le$text <- sapply(data_right$data, "[[", "text")
                    le$col <- sapply(data_right$data, "[[", "col")
                    le$lty <- sapply(data_right$data, "[[", "lty")
                    le$lwd <- sapply(data_right$data, "[[", "lwd")
                    le$pch <- sapply(data_right$data, "[[", "pch")
                    for (i in seq_along(data_right$data)) {
                        if (types[i] == "p") {
                            le$lty[i] <- NA
                        } else if (types[i] == "l") {
                            le$pch[i] <- NA
                        }
                    }
                    # add stuf to legend here
                    if (F) {
                        message("\nadd non default stuff to ", mode_p, " legend ...")
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
                    
                    message("`add_cor_data_left_and_right_ts`=T ...")

                    for (i in seq_along(z)) {
                        cor <- cor.test(z[[i]], data_right$data[[i]]$y)
                        # plusminus: %+-%
                        subtitle <- substitute(paste(setting, ": cor(", x, ",", y, ") = ", r, " " %+-% "", uncert, "; p ", p),
                                               list(setting=names_legend_p[i],
                                                    x=varname, y=data_right$data[[i]]$text,
                                                    r=round(cor$estimate, 2), 
                                                    uncert=round(cor$estimate - cor$conf.int[1], 3),
                                                    p=ifelse(cor$p.value < 1e-3, 
                                                             paste0("< 1e-3"), round(cor$p.value, 3))))
                        message(subtitle)
                        mtext(subtitle, line=i-1, cex=0.7)
                    }

                } # if add_cor_data_left_and_right_ts

            } # if add_data_right_yaxis_ts

            if (add_data_upper_xaxis_ts) {

                message("\n`add_data_upper_xaxis_ts` = T --> add data upper xaxis ...")
                par(new=T)
                plot(data_upper$data[[1]]$x, data_upper$data[[1]]$y, #log="y", 
                     t="n", xlim=tlim_upper, ylim=ylim, 
                     xlab=NA, ylab=NA, axes=F)
                if (tlabsrt == 0) { # add horizontal labels (the default)
                    axis(3, at=tatn_upper, labels=tlablt_upper, cex.axis=tlabcex)
                } else { # add non-horizontal labels with angle
                    axis(3, at=tatn_upper, labels=NA)
                    # character height in user coordinates
                    text(x=tatn_upper, y=par("usr")[4] + strheight("1"), labels=tlablt_upper, 
                         xpd=T, srt=tlabsrt_upper, adj=c(1, 1), cex=tlabcex)
                }

                # add time label on upper x-axis
                mtext(side=3, data_upper$label, line=2, cex=0.9)

                # add unsmoothed upper data before smoothed
                if (add_unsmoothed) {
                    for (i in seq_along(data_upper$data)) {
                        message(i, "/", length(data_upper$data), ": ", names(data_upper$data)[i], " unsmoothed ...")
                        if (length(data_upper$data[[i]]$x) == 1 && data_upper$data[[i]]$x == "const") {
                            abline(h=data_upper$data[[i]]$y, 
                                   col=data_upper$data[[i]]$col, lty=data_upper$data[[i]]$lty,
                                   lwd=data_upper$data[[i]]$lwd)
                        } else {
                            lines(data_upper$data[[i]]$x, data_upper$data[[i]]$y, 
                                  col=data_upper$data[[i]]$col, lty=data_upper$data[[i]]$lty,
                                  lwd=data_upper$data[[i]]$lwd, pch=data_upper$data[[i]]$pch)
                        }
                    }
                }

                # add smoothed data after unsmoothed
                if (add_smoothed) {
                    for (i in seq_along(data_upper$data)) {
                        message(i, "/", length(data_upper$data), ": ", names(data_upper$data)[i], " smoothed ...")
                        if (length(data_upper$data[[i]]$x) == 1 && data_upper$data[[i]]$x == "const") {
                            abline(h=data_upper$data[[i]]$yma, 
                                   col=data_upper$data[[i]]$col, lty=data_upper$data[[i]]$lty,
                                   lwd=data_upper$data[[i]]$lwd)
                        } else {
                            lines(data_upper$data[[i]]$x, data_upper$data[[i]]$yma, 
                                  col=data_upper$data[[i]]$col, lty=data_upper$data[[i]]$lty,
                                  lwd=data_upper$data[[i]]$lwd, pch=data_upper$data[[i]]$pch)
                        }
                    }
                }

                if (add_legend_upper_xaxis) {
                    message("\nadd default stuff to ", mode_p, " upper xaxis data legend ...")
                    le <- list()
                    le$pos <- "topleft" 
                    le$ncol <- 1
                    le$cex <- 1
                    le$cex <- 0.85
                    le$text <- sapply(data_upper$data, "[[", "text")
                    le$col <- sapply(data_upper$data, "[[", "col")
                    le$lty <- sapply(data_upper$data, "[[", "lty")
                    le$lwd <- sapply(data_upper$data, "[[", "lwd")
                    le$pch <- sapply(data_upper$data, "[[", "pch")
                    for (i in seq_along(data_upper$data)) {
                        if (types[i] == "p") {
                            le$lty[i] <- NA
                        } else if (types[i] == "l") {
                            le$pch[i] <- NA
                        }
                    }
                    # add stuf to legend here
                    if (F) {
                        message("\nadd non default stuff to ", mode_p, " legend ...")
                    }
                    # reorder reading direction from R's default top->bottom to left->upper
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

            } # if add_data_upper_xaxis_ts

            message("\nsave plot ", plotname, " ...")
            dev.off()
            if (p$plot_type == "pdf") {
                if (T) {
                    if (F && "extrafont" %in% (.packages())){
                        message("run `extrafont::embed_fonts()` ...")
                        extrafont::embed_fonts(plotname, outfile=plotname)
                    } else {
                        message("run `grDevices::embedFonts()` ...")
                        grDevices::embedFonts(plotname, outfile=plotname)
                    }
                } else {
                    message("todo: sometimes pdf font embedding blurrs colors why?")
                }
            }
            
            if (ts_plot_each_setting_in_subplot) {

                message("\n`ts_plot_each_setting_in_subplot`=T ....")

                y <- z
                if (add_smoothed) y <- zma
                x_at <- x_lab <- x_labels <- y_lab <- vector("list", l=length(z))
                for (i in seq_along(z)) {
                    x_at[[i]] <- tatn
                    x_labels[[i]] <- tlablt
                    x_lab[[i]] <- tunit
                    y_lab[[i]] <- data_info$label
                }
                
                # plotname
                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   varname, "_",
                                   paste0(names_short_p, "_", seasonsp_p, 
                                          "_", froms_plot_p, "_to_", tos_plot_p, "_", 
                                          areas_p, collapse="_vs_"), 
                                   "_subplots", data_right$suffix, ts_highlight_seasons$suffix,
                                   ".", p$plot_type)
                if (nchar(plotname) > nchar_max_foutname) {
                    plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                       varname, "_",
                                       paste0(names_short_p, "_", areas_p, collapse="_vs_"), 
                                       "_subplots", data_right$suffix, ts_highlight_seasons$suffix,
                                       ".", p$plot_type)
                }
                dir.create(dirname(plotname), recursive=T, showWarnings=F)
                
                source(paste0(host$homepath, "/functions/plot.nxm.r"))
                nm <- plot.nxm(d$time, y, dry=T) 
                
                message("plot ", plotname, " ...")
                if (p$plot_type == "png") { 
                    png(plotname, 
                        width=nm$ncol*p$ts_width, 
                        height=nm$nrow*p$ts_height,
                        res=p$ppi, family=p$family_png)
                } else if (p$plot_type == "pdf") {
                    pdf(plotname, width=nm$ncol*p$inch, 
                        height=p$inch*((nm$nrow*p$ts_height)/(nm$ncol*p$ts_width)),
                        family=p$family_pdf, encoding=encoding)
                }
                plot.nxm(d$time, y, dry=F, 
                         cols=cols_p, ltys=ltys_p, lwds=lwds_p, pchs=pchs_p, 
                         x_at=x_at, x_labels=x_labels, x_lab=x_lab, 
                         y_lab=y_lab, ynames=names_legend_p,
                         verbose=T)
                dev.off()

            } # if ts_plot_each_setting_in_subplot

        } # if (ndims_unique == 1 && dim_names == "time")
        # finished plot `datas` as time 
        
        
        ## plot `datas` as lat vs depth vs time
        if (ndims == 3 && all(dim_names %in% c("lat", "depth", "time"))) { # e.g. moc

            # extract time series of lat,depth,time MOC data
            if (any(varname == c("MOCw"))) { # add further moc variables here
                if (!exists("moc_lats")) {
                    stop("`moc_lats` is not defined. dont know where to extract ", varname, " time series.")
                }
                moc_max_ts <- vector("list", l=length(z))
                names(moc_max_ts) <- names_short_p
                for (li in 1:length(moc_lats)) {
                    message("extract \"", varname, "\" time series at latitude ", li, "/", 
                            length(moc_lats), ": ", moc_lats[li], " degrees ...")
                    for (i in seq_along(z)) {
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
                                message("   setting ", i, "/", length(z), ": ", names_short_p[i], ": ", 
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
                for (i in seq_along(z)) {
                    outpath <- paste0(postpaths[sapply(dinds, "[")][i], "/", 
                                      models[sapply(dinds, "[")][i], "/moc_ts/", 
                                      varnames_in[sapply(dinds, "[")][i])
                    dir.create(outpath, recursive=T, showWarnings=F)
                    outname <- paste0(outpath, "/", 
                                      prefixes[sapply(dinds, "[")][i], "_moc_ts", 
                                      codesf[sapply(dinds, "[")][i], "_", 
                                      varnames_in[sapply(dinds, "[")][i], "_", 
                                      areas_p[i], "_", seasonsp_p[i], "_", 
                                      froms_plot_p[i], "-", tos_plot_p[i],
                                      depthsf_p[i], ".", p$plot_type)
                    if (file.exists(outname)) {
                        message("overwrite ", outname, " ...")
                    } else {
                        message("save ", outname, " ...")
                    }
                    stop("update dinds/vinds")
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

        } # if (ndims == 3 && all(dim_names %in% c("lat", "depth", "time"))) {
        # finished plot `datas` as lat vs depth vs time


        ## start dim-specific plots for each variable of `datasmon`
        if (exists("datasmon")) {
            
            message("\n****************** plot datasmon zmon_* ***************************")

            ndims_mon <- length(dim(zmon[[1]]))
            dim_names_mon <- attributes(zmon[[1]])$dims
            message("\nzmon_", plot_groups[plot_groupi], "[[", ploti, "/", nplots, "]]: \"", zname_mon, 
                    "\" has ", ndims_mon, " dim", ifelse(ndims_mon > 1, "s", ""), ": \"", 
                    paste(dim_names_mon, collapse="\", \""), "\". check if this case is defined ...")

            ## plot `datasmon` as time 
            if (ndims_mon == 1 && dim_names_mon == "month") {

                message("\n", varname, " ", mode_p, " plot vs months ...")
                
                # ylims for fldmean versus months plot
                message("\n", mode_p, " versus months min / mean / max ", varname, " zmon:")
                for (i in seq_along(zmon)) {
                    message(names_short_pmon[i], ": ", min(zmon[[i]], na.rm=T), " / ",
                            mean(zmon[[i]], na.rm=T), " / ", max(zmon[[i]], na.rm=T))
                }
                ylim_mon <- range(zmon, na.rm=T)
                message("\nylim_mon=", appendLF=F)
                dput(ylim_mon)
                ylim_mon[is.infinite(ylim_mon)] <- 0
                
                # add obs
                if (T && exists("noaa_ghcdn")) {
                    if (any(varname == c("temp2", "tsurf", "aprt"))) {
                        message("\nadd noaa ghcdn monthly climatology data\n", 
                                "check https://github.com/chrisdane/PLOT/blob/master/lakes/lake_coords_closest_GHCDN_stations.txt ...")
                        message("ylim_mon before: ", ylim_mon[1], ", ", ylim_mon[2])
                        noaax <- noaa_ghcdn_tmp$ts$months
                        if (any(varname == c("temp2", "tsurf"))) {
                            noaay <- noaa_ghcdn_tmp$ts$Tavg_mon
                        } else if (varname == "aprt") {
                            noaay <- noaa_ghcdn_tmp$ts$precip_mon
                        }
                        ylim_mon <- range(ylim_mon, noaay, na.rm=T)
                        message("ylim_mon after: ", ylim_mon[1], ", ", ylim_mon[2])
                    } # if temp2, tsurf, aprt
                } # if exists("noaa_ghcdn")
                # finished adding obs
                
                yat_mon <- pretty(ylim_mon, n=10)
                ylab_mon <- format(yat_mon, trim=T)

                # prepare right axis data if necessary
                if (!add_data_right_yaxis_ts_mon) {
                    data_right_mon <- list(suffix="") # default
                } else {
                    message("update zmon data_right")
                    data_right_mon <- list(data=list())
                    if (varname == "temp2") {
                        data_right_mon <- list(data=vector("list", l=length(zmon)))
                        names(data_right_mon$data) <- names_short
                        for (i in seq_along(data_right_mon$data)) {
                            inpath <- paste0(host$workpath, "/post/", models[i], "/", mode_p, "/wisoaprt_d") 
                            fname <- paste0(prefixes[i], "_", mode_p, 
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
                    
                    message("\nylim_right_mon=", appendLF=F)
                    dput(ylim_right_mon)
                    ylim_right_mon[is.infinite(ylim_right_mon)] <- 0

                    if (!exists("yat_right_mon")) {
                        message("use automatic data right yaxis labels ...")
                        yat_right_mon <- pretty(ylim_right_mon, n=10)
                    }
                    ylab_right_mon <- format(yat_right_mon, trim=T)
                } # if add_data_right_yaxis_ts_mon finished prepare right axis data

                # plotname
                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   varname, "_",
                                   paste0(names_short_pmon, "_", seasonsp_pmon, "_",
                                          froms_plot_pmon, "_to_", tos_plot_pmon, "_", 
                                          areas_pmon, collapse="_vs_"), 
                                   "_months",
                                   data_right_mon$suffix,
                                   ".", p$plot_type)
                if (nchar(plotname) > nchar_max_foutname) {
                    if (plot_groups[plot_groupi] == "samevars") {
                        plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                           varname, "_",
                                           paste0(names_short_pmon, "_", areas_pmon, collapse="_vs_"), 
                                           "_months",
                                           data_right$suffix, 
                                           ".", p$plot_type)
                    } else if (plot_groups[plot_groupi] == "samedims") {
                        plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                           varname, "_",
                                           paste0(names_short_pmon, "_", varnames_in_pmon, collapse="_vs_"), 
                                           "_months",
                                           data_right$suffix, 
                                           ".", p$plot_type)
                    }
                }
                dir.create(dirname(plotname), recursive=T, showWarnings=F)
                if (p$plot_type == "png") {
                    png(plotname, width=p$ts_width_m, height=p$ts_height_m,
                        res=p$ppi, family=p$family_png)
                } else if (p$plot_type == "pdf") {
                    pdf(plotname, width=p$inch, height=p$inch*p$ts_height_m/p$ts_width_m,
                        family=p$family_pdf, encoding=encoding)
                }

                # set plot margins
                mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                mar[4] <- 1 # decrease right margin
                if (!add_title) mar[3] <- 1 # decrease upper margin
                if (tlabsrt == 0) mar[1] <- mar[1]/2  # decrease lower margin
                if (add_data_right_yaxis_ts_mon) mar[4] <- mar[2] # same as left  

                # open plot
                par(mar=mar)
                plot(dmon$month[[1]], zmon[[1]], t="n",
                     xlim=monlim, ylim=ylim_mon, 
                     xaxt="n", yaxt="n",
                     xlab=NA, ylab=NA)
                axis(1, at=monat, labels=monlab, cex.axis=tlabcex)
                axis(2, at=yat_mon, labels=ylab_mon, las=2)

                # add title
                if (add_title) {
                    title <- paste0(paste(unique(areas_pmon), collapse=","), 
                                    " ", mode_p, " ", varname, " ", 
                                    paste(unique(seasonsp_pmon), collapse=","), " ", 
                                    paste(unique(froms_plot_pmon), collapse=","), " to ", 
                                    paste(unique(tos_plot_pmon), collapse=","))
                    title(title, cex.main=0.75)
                }

                # add variable label
                mtext(side=2, data_info$label, line=3.4, cex=0.9)

                # add grid
                if (add_xgrid) {
                    message("\nadd xgrid ...")
                    abline(v=tatn, col="gray", lwd=0.5)
                }
                if (add_ygrid) {
                    message("\nadd ygrid ...")
                    abline(h=yat, col="gray", lwd=0.5)
                }

                # add zero line
                if (add_zeroline) {
                    abline(h=0, col="gray", lwd=0.5)
                }

                ## add data
                for (i in seq_along(zmon)) {
                    lines(dmon$month[[i]], zmon[[i]], 
                          col=cols_pmon[i], lty=ltys_pmon[i], lwd=lwds_pmon[i], pch=pchs_pmon[i])
                }

                # add obs
                if (T && exists("noaa_ghcdn")) {
                    if (any(varname == c("temp2", "tsurf", "aprt"))) {
                        message("\nadd noadd ghcdn monthly data to monthly plot ...")
                        points(noaax, noaay,
                               t=noaa_ghcdn_tmp$type, 
                               col=noaa_ghcdn_tmp$col,
                               lty=noaa_ghcdn$lty, 
                               lwd=noaa_ghcdn$lwd,
                               pch=noaa_ghcdn$pch, 
                               cex=noaa_ghcdn$cex)
                    } # if temp2, tsurf, aprt
                } # if exists("noaa_ghcdn")
                # finished adding obs
                
                # add legend if wanted
                if (add_legend) {
                    message("\nadd default stuff to ", mode_p, " mon legend ...")
                    le <- list()
                    le$pos <- "topleft" 
                    #le$pos <- "top"
                    #le$pos <- "bottom"
                    #le$pos <- "bottomleft" 
                    #le$pos <- "bottomright" 
                    #le$ncol <- nsettings/2
                    le$ncol <- 1
                    #le$ncol <- 2 
                    le$cex <- 1
                    le$cex <- 0.85
                    le$text <- names_legend_pmon
                    le$col <- cols_pmon
                    le$lty <- ltys_pmon
                    le$lwd <- lwds_pmon
                    le$pch <- pchs_pmon
                    for (i in seq_along(zmon)) {
                        if (types_pmon[i] == "p") {
                            le$lty[i] <- NA
                        } else if (types_pmon[i] == "l") {
                            le$pch[i] <- NA
                        }
                    }
                    # add stuf to legend here
                    if (F) {
                        message("\nadd non default stuff to ", mode_p, " mon legend ...")

                    }
                    if (T && exists("noaa_ghcdn")) {
                        if (any(varname == c("temp2", "tsurf", "aprt"))) {
                            message("\nadd noadd ghcdn monthly data to monthly legend ...")
                            le$text <- c(le$text, noaa_ghcdn_tmp$text)
                            le$col <- c(le$col, noaa_ghcdn_tmp$col)
                            le$lty <- c(le$lty, noaa_ghcdn_tmp$lty)
                            le$lwd <- c(le$lwd, noaa_ghcdn_tmp$lwd)
                            le$pch <- c(le$pch, noaa_ghcdn_tmp$pch)
                        }
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
                    message("\n`add_data_right_yaxis_ts_mon` = T --> add data right yaxis mon ...")
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
                        message("\nadd default stuff to ", mode_p, " right_data mon legend ...")
                        le <- list()
                        le$pos <- "top" 
                        le$ncol <- 1
                        le$cex <- 1
                        le$cex <- 0.85
                        le$text <- names_legend_pmon
                        le$col <- cols_pmon
                        le$lty <- ltys_pmon
                        le$lwd <- lwds_pmon
                        le$pch <- pchs_pmon
                        for (i in seq_len(nsettings_right_mon)) {
                            if (types_pmon[i] == "p") {
                                le$lty[i] <- NA
                            } else if (types_pmon[i] == "l") {
                                le$pch[i] <- NA
                            }
                        }
                        # add stuf to legend here
                        if (T && varname == "temp2") {
                            message("\nadd non default stuff to ", mode_p, " legend ...")
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
                message("\nsave plot ", plotname, " ...")
                dev.off()
                if (p$plot_type == "pdf") {
                    if (T) {
                        if (F && "extrafont" %in% (.packages())){
                            message("run `extrafont::embed_fonts()` ...")
                            extrafont::embed_fonts(plotname, outfile=plotname)
                        } else {
                            message("run `grDevices::embedFonts()` ...")
                            grDevices::embedFonts(plotname, outfile=plotname)
                        }
                    } else {
                        message("todo: sometimes pdf font embedding blurrs colors why?")
                    }
                }
            
            } # if (ndims_unique_mon == 1 && vardims_unique_mon == "month") {
            # finished plot `datasmon` vs months

        } # if exists("datasmon")
        # finised dim-specific plots for each variable of `datasmon`


        ## start dim-specific plots for each variable of `datasan`
        if (exists("datasan")) {
            
            message("\n****************** plot datasan zan_* ***************************")

            ndims_an <- length(dim(zan[[1]]))
            dim_names_an <- attributes(zan[[1]])$dims
            message("\nzan_", plot_groups[plot_groupi], "[[", ploti, "/", nplots, "]]: \"", zname_an, 
                    "\" has ", ndims_an, " dim", ifelse(ndims_an > 1, "s", ""), ": \"", 
                    paste(dim_names_an, collapse="\", \""), "\". check if this case is defined ...")

            ## plot `datasan` as time 
            if (ndims_an == 1 && dim_names_an == "year") {

                message("\n", varname, " ", mode_p, " plot vs years ...")
                
                # ylims for fldmean versus years plot
                message("\n", mode_p, " versus years min / mean / max ", varname, " zan:")
                for (i in seq_along(zan)) {
                    message(names_short_pan[i], ": ", min(zan[[i]], na.rm=T), " / ",
                            mean(zan[[i]], na.rm=T), " / ", max(zan[[i]], na.rm=T))
                }
                ylim_an <- range(zan, na.rm=T)
                message("\nylim_an=", appendLF=F)
                dput(ylim_an)
                ylim_an[is.infinite(ylim_an)] <- 0
            
                # add obs to ylim
                if (T && exists("noaa_ghcdn")) {
                    if (any(varname == c("temp2", "tsurf", "aprt"))) {
                        message("\nadd noaa ghcdn annual data\n", 
                                "check https://github.com/chrisdane/PLOT/blob/master/lakes/lake_coords_closest_GHCDN_stations.txt ...")
                        message("ylim_an before: ", ylim_an[1], ", ", ylim_an[2])
                        noaax <- noaa_ghcdn_tmp$ts$years
                        if (any(varname == c("temp2", "tsurf"))) {
                            noaay <- noaa_ghcdn_tmp$ts$Tavg_an
                        } else if (varname == "aprt") {
                            noaay <- noaa_ghcdn_tmp$ts$precip_an
                        }
                        ylim_an <- range(ylim_an, noaay, na.rm=T)
                        message("ylim_an after: ", ylim_an[1], ", ", ylim_an[2])
                    } # if temp2, tsurf, aprt
                } # if exists("noaa_ghcdn")
                
                # finished ylim
                yat_an <- pretty(ylim_an, n=10)
                ylab_an <- format(yat_an, trim=T)

                # prepare right axis data if necessary
                if (!add_data_right_yaxis_ts_an) {
                    data_right_an <- list(suffix="") # default
                } else {
                    message("\n`add_data_right_yaxis_ts_an`=T ...")
                    message("update zan data_right")
                    data_right_an <- list(data=vector("list", l=length(zan)))
                    names(data_right_an$data) <- names_short_p
                    if (T && any(varname == c("temp2", "tsurf", "tsurfaprt", "ptemp"))) {
                        for (i in seq_along(data_right_an$data)) {
                            inpath <- paste0(host$workpath, "/post/", models[i], "/",
                                             "yearsum",
                                             "/wisoaprt_d_post") 
                            fname <- paste0(prefixes[i], "_", 
                                            "yearsum", 
                                            codesf[i], "_wisoaprt_d_post_sellevel_2_", areas[i], "_",
                                            "yearsum", 
                                            "_", fromsf[i], "-", tosf[i], 
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
                        for (i in seq_len(nsettings_right_an)) {
                            data_right_an$data[[i]]$yma <- filter(data_right_an$data[[i]]$y, rep(1/(n_mas[i]/12), t=n_mas[i]/12))
                        }
                    }

                    if (!exists("ylim_right_an")) { # possibly set by user
                        message("use automatic data right yaxis limits ...")
                        ylim_right_an <- vector("list", l=length(data_right_an$data))
                        ylim_right_an_ma <- ylim_right_an
                        for (i in seq_len(nsettings_right_an)) {
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
                    ylim_right_an[is.infinite(ylim_right_an)] <- 0
                    if (!exists("yat_right_an")) {
                        message("use automatic data right yaxis labels ...")
                        yat_right_an <- pretty(ylim_right_an, n=10)
                    }
                    ylab_right_an <- format(yat_right_an, trim=T)
                } # if add_data_right_yaxis_ts_an finished prepare right axis data

                # plotname
                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   varname, "_",
                                   paste0(names_short_pan, "_", seasonsp_pan, "_", 
                                          froms_plot_pan, "_to_", tos_plot_pan, "_", 
                                          areas_pan, collapse="_vs_"), 
                                   "_annual",
                                   data_right_an$suffix,
                                   ".", p$plot_type)
                if (nchar(plotname) > nchar_max_foutname) {
                    if (plot_groups[plot_groupi] == "samevars") {
                        plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                           varname, "_",
                                           paste0(names_short_pan, "_", areas_pan, collapse="_vs_"), 
                                           "_annual",
                                           data_right_an$suffix,
                                           ".", p$plot_type)
                    } else if (plot_groups[plot_groupi] == "samedims") {
                        plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                           varname, "_",
                                           paste0(names_short_pan, "_", varnames_in_pan, collapse="_vs_"), 
                                           "_annual",
                                           data_right_an$suffix,
                                           ".", p$plot_type)
                    }
                }
                dir.create(dirname(plotname), recursive=T, showWarnings=F)
                if (p$plot_type == "png") {
                    png(plotname, width=p$ts_width, height=p$ts_height,
                        res=p$ppi, family=p$family_png)
                } else if (p$plot_type == "pdf") {
                    pdf(plotname, width=p$inch, height=p$inch*p$ts_height/p$ts_width,
                        family=p$family_pdf, encoding=encoding)
                }

                # set plot margins
                mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                mar[4] <- 1 # decrease right margin
                if (!add_title) mar[3] <- 1 # decrease upper margin
                if (tlabsrt == 0) mar[1] <- mar[1]/2  # decrease lower margin
                if (add_data_right_yaxis_ts_an) mar[4] <- mar[2] # same as left  

                # open plot
                par(mar=mar)
                plot(dan$year[[1]], zan[[1]], t="n",
                     xlim=anlim, ylim=ylim_an, 
                     xaxt="n", yaxt="n",
                     xlab=NA, ylab=NA)
                axis(1, at=anat, labels=anlab, cex.axis=tlabcex)
                axis(2, at=yat_an, labels=ylab_an, las=2)

                # add title
                if (add_title) {
                    title <- paste0(paste(unique(areas_pan), collapse=","), 
                                    " ", mode_p, " ", varname, " ", 
                                    paste(unique(seasonsp_pan), collapse=","), " ", 
                                    paste(unique(froms_plot_pan), collapse=","), " to ", 
                                    paste(unique(tos_plot_pan), collapse=","))
                    title(title, cex.main=0.75)
                }

                # add variable label
                mtext(side=2, data_info$label, line=3.4, cex=0.9)

                # add grid
                if (add_xgrid) {
                    message("\nadd xgrid ...")
                    abline(v=tatn, col="gray", lwd=0.5)
                }
                if (add_ygrid) {
                    message("\nadd ygrid ...")
                    abline(h=yat, col="gray", lwd=0.5)
                }

                # add zero line
                if (add_zeroline) {
                    abline(h=0, col="gray", lwd=0.5)
                }

                # add data
                for (i in seq_along(zan)) {
                    lines(dan$year[[i]], zan[[i]], 
                          col=cols_pan[i], lty=ltys_pan[i], lwd=lwds_pan[i], pch=pchs_pan[i])
                }

                # add obs 
                if (T && any(varname == c("wisoaprt_d", "wisoaprt_d", "wisoevap_d", "wisope_d")) &&
                    exists("kostrova_etal_2019") &&
                    all(grepl("ladoga", areas))) {
                    message("\nadd kostroval et al. 2019 to annual plot ...")
                    points(kostrova_etal_2019$time, kostrova_etal_2019$d18o,
                           t=kostrova_etal_2019$type, col=kostrova_etal_2019$col, 
                           lty=kostrova_etal_2019$lty, lwd=kostrova_etal_2019$lwd, 
                           pch=kostrova_etal_2019$pch, cex=kostrova_etal_2019$cex)
                }
                
                if (T && exists("noaa_ghcdn")) {
                    if (any(varname == c("temp2", "tsurf", "aprt"))) {
                        message("\nadd noadd ghcdn monthly data to annual plot ...")
                        points(noaax, noaay,
                               t=noaa_ghcdn_tmp$type, 
                               col=noaa_ghcdn_tmp$col,
                               lty=noaa_ghcdn$lty, 
                               lwd=noaa_ghcdn$lwd,
                               pch=noaa_ghcdn$pch, 
                               cex=noaa_ghcdn$cex)
                    } # if temp2, tsurf, aprt
                } # if exists("noaa_ghcdn")
                # finished adding obs
                
                # add legend if wanted
                if (add_legend) {
                    message("\nadd default stuff to ", mode_p, " an legend ...")
                    le <- list()
                    le$pos <- "topleft" 
                    #le$pos <- "top"
                    #le$pos <- "bottom"
                    #le$pos <- "bottomleft" 
                    #le$pos <- "bottomright" 
                    #le$ncol <- nsettings/2
                    le$ncol <- 1
                    #le$ncol <- 2 
                    le$text <- names_legend_pan
                    le$cex <- 1
                    le$cex <- 0.85
                    le$col <- cols_pan
                    le$lty <- ltys_pan
                    le$lwd <- lwds_pan
                    le$pch <- pchs_pan
                    for (i in seq_along(zan)) {
                        if (types_pan[i] == "p") {
                            le$lty[i] <- NA
                        } else if (types_pan[i] == "l") {
                            le$pch[i] <- NA
                        }
                    }
                    # add stuf to legend here
                    if (F) {
                        message("\nadd non default stuff to ", mode_p, " an legend ...")

                    }
                    if (T && exists("noaa_ghcdn")) {
                        if (any(varname == c("temp2", "tsurf", "aprt"))) {
                            message("\nadd noadd ghcdn monthly data to annual legend ...")
                            le$text <- c(le$text, noaa_ghcdn_tmp$text)
                            le$col <- c(le$col, noaa_ghcdn_tmp$col)
                            le$lty <- c(le$lty, noaa_ghcdn_tmp$lty)
                            le$lwd <- c(le$lwd, noaa_ghcdn_tmp$lwd)
                            le$pch <- c(le$pch, noaa_ghcdn_tmp$pch)
                        }
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
                    message("\n`add_data_right_yaxis_ts_an` = T --> add data right yaxis an ...")
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
                        message("\nadd default stuff to ", mode_p, " right_data an legend ...")
                        le <- list()
                        le$pos <- "top" 
                        le$ncol <- 1
                        le$cex <- 1
                        le$cex <- 0.85
                        le$text <- names_legend_pan
                        le$col <- cols_pan
                        le$lty <- ltys_pan
                        le$lwd <- lwds_pan
                        le$pch <- pchs_pan
                        for (i in seq_len(nsettings_right_an)) {
                            if (types_pan[i] == "p") {
                                le$lty[i] <- NA
                            } else if (types_pan[i] == "l") {
                                le$pch[i] <- NA
                            }
                        }
                        # add stuf to legend here
                        if (T && varname == "temp2") {
                            message("\nadd non default stuff to ", mode_p, " legend ...")
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

                    if (add_cor_data_left_and_right_ts_an) {

                        message("`add_cor_data_left_and_right_ts_an`=T ...")
                        for (i in seq_along(zan)) {
                            cor <- cor.test(zan[[i]], data_right_an$data[[i]]$y)
                            # plusminus: %+-%
                            subtitle <- substitute(paste(setting, ": cor(", x, ",", y, ") = ", r, " " %+-% "", uncert, "; p ", p),
                                                   list(setting=names_legend_pan[i],
                                                        x=varname, y=data_right_an$data[[i]]$text,
                                                        r=round(cor$estimate, 2), 
                                                        uncert=round(cor$estimate - cor$conf.int[1], 3),
                                                        p=ifelse(cor$p.value < 1e-3, 
                                                                 paste0("< 1e-3"), round(cor$p.value, 3))))
                            message(subtitle)
                            mtext(subtitle, line=i-1, cex=0.7)
                        }

                    } # if add_cor_data_left_and_right_ts_an
                
                } # if add_data_right_yaxis_ts_an

                box()
                message("\nsave plot ", plotname, " ...")
                dev.off()
                if (p$plot_type == "pdf") {
                    if (T) {
                        if (F && "extrafont" %in% (.packages())){
                            message("run `extrafont::embed_fonts()` ...")
                            extrafont::embed_fonts(plotname, outfile=plotname)
                        } else {
                            message("run `grDevices::embedFonts()` ...")
                            grDevices::embedFonts(plotname, outfile=plotname)
                        }
                    } else {
                        message("todo: sometimes pdf font embedding blurrs colors why?")
                    }
                }
            
            } # if (ndims_unique_an == 1 && vardims_unique_an == "year") {
            # finished plot `datasan` vs years

        } # if exists("datasan")
        # finised dim-specific plots for each variable of `datasan`


        ## start dim-specific plots for each variable of `datasltm`
        if (exists("datasltm")) {
            
            message("\n****************** plot of datasltm zltm_* ***************************")

            ndims_ltm <- length(dim(zltm[[1]]))
            dim_names_ltm <- attributes(zltm[[1]])$dims
            message("\nzltm_", plot_groups[plot_groupi], "[[", ploti, "/", nplots, "]]: \"", zname_ltm, 
                    "\" has ", ndims_ltm, " dim", ifelse(ndims_ltm > 1, "s", ""), ": \"", 
                    paste(dim_names_ltm, collapse="\", \""), "\". check if this case is defined ...")

            if (ndims_ltm == 0) {
                message("--> show only ltm numbers:")
                for (i in seq_along(zltm)) {
                    message(i, "/", length(zltm), " \"", names_short_pltm[i], "\" : zltm[[", i, "]] = ", zltm[[i]])
                }
            }

            ## plot `datasltm` as lat vs depth (e.g. moc)
            if (ndims_ltm == 2 && all(dim_names_ltm == c("lat", "depth"))) {

                message("\n", varname, " ", mode_p, " ltm plot lat vs depth ...")
               
                # add moc topo 
                if (any(varname == c("MOCw"))) { # add further moc variables here
                    if (any(varnames_unique == "moc_topo")) {
                        moc_topo <- vector("list", l=length(zltm))
                        for (i in seq_along(z)) {
                            tmp <- vector("list")
                            moc_topo_ind <- which(names(z[[i]]) == "moc_topo")
                            if (length(moc_topo_ind) == 1) {
                                tmp$data <- z[[i]][[moc_topo_ind]]
                                tmp$levels <- unique(range(tmp$data, na.rm=T))
                                tmp$cols <- "gray"
                            }
                            moc_topo[[i]] <- tmp
                        }
                    } # if varname "moc_topo" is present
                } # add moc topp if varname is any moc

                # colorbar values
                source(paste0(host$homepath, "/functions/image.plot.pre.r"))
                ip <- image.plot.pre(range(zltm, na.rm=T), verbose=F)

                # determine number of rows and columns
                source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
                nm <- image.plot.nxm(dltm$lat, dltm$depth, z=zltm, ip=ip, dry=T)

                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   varname, "_", 
                                   paste0(names_short_pltm, "_", seasonsp_pltm, "_",
                                          froms_plot_pltm, "_to_", tos_plot_pltm, "_", 
                                          areas_pltm, collapse="_vs_"), 
                                   ".", p$plot_type)
                dir.create(dirname(plotname), recursive=T, showWarnings=F)
                message("plot ", plotname, " ...")
                if (p$plot_type == "png") {
                    png(plotname, width=nm$ncol*p$map_width, height=nm$nrow*p$map_height,
                        res=p$ppi, family=p$family_png)
                } else if (p$plot_type == "pdf") {
                    pdf(plotname, width=nm$ncol*p$inch, 
                        height=p$inch*((nm$nrow*p$map_height)/(nm$ncol*p$map_width)),
                        family=p$family_pdf, encoding=encoding)
                }

                # use km instead of m as depth unit
                if (T) {
                    message("divide depth dim by 1000 m --> km")
                    depth_dim <- lapply(depth_dim, "/", 1000)
                    ylab <- "Depth [km]"
                } else {
                    ylab <- "Depth [m]"
                }

                # plot
                image.plot.nxm(dltm$lat, dltm$depth, z=zltm, ip=ip, verbose=F,
                               xlab="Latitude [°]", ylab=ylab,
                               zlab=data_info$label, znames=names_legend_pltm,
                               image_list=moc_topo)
            
                message("\nsave plot ", plotname, " ...")
                dev.off()
                if (p$plot_type == "pdf") {
                    if (T) {
                        if (F && "extrafont" %in% (.packages())){
                            message("run `extrafont::embed_fonts()` ...")
                            extrafont::embed_fonts(plotname, outfile=plotname)
                        } else {
                            message("run `grDevices::embedFonts()` ...")
                            grDevices::embedFonts(plotname, outfile=plotname)
                        }
                    } else {
                        message("todo: sometimes pdf font embedding blurrs colors why?")
                    }
                }

            } # if ndims == 2: lon lat

        } # if (exists("datasltm")) {
        # finised dim-specific plots for each variable of `datasltm`


        ## plot var setting1 vs var setting2 of `datas`
        if (plot_scatter_s1_vs_s2) {

            message("\n********** `plot_scatter_s1_vs_s2`=T --> scatterplot var setting1 vs setting2 ************\n",
                    "`scatter_s1_vs_s1_varname` = \"", scatter_s1_vs_s1_varname, "\"")

            if (exists(paste0(scatter_s1_vs_s1_varname, "_datas"))) { # set in namelist.plot
                message("`", scatter_s1_vs_s1_varname, "_datas` exists ", appendLF=F)
                eval(parse(text=paste0("scatter_set1_vs_set2 <- ", varname, "_datas")))
                ndims_set1_vs_set2 <- unique(sapply(lapply(lapply(scatter_set1_vs_set2, attributes), "[", "dims"), length))
                
                if (length(scatter_set1_vs_set2) == 2 && ndims_set1_vs_set2 == 1) {
                    message("and `ndims_set1_vs_set2` = 1 ", appendLF=F) 
                    dim_names_set1_vs_set2 <- sapply(lapply(scatter_set1_vs_set2, attributes), "[", "dims")
                    
                    if (all(dim_names_set1_vs_set2 == "time")) {
                        message("and `dim_names_set1_vs_set2` == \"time\" ...")

                        message("\find same temporal indices ...")
                        inds1 <- which(dims[[1]]$time %in% dims[[2]]$time)
                        inds2 <- which(dims[[2]]$time %in% dims[[1]]$time)
                        if (length(inds1) != length(inds2)) {
                            stop("sth wrong")
                        }
                        scatter_set1_vs_set2[[1]] <- scatter_set1_vs_set2[[1]][inds1]
                        scatter_set1_vs_set2[[2]] <- scatter_set1_vs_set2[[2]][inds2]

                        scattercexs <- rep(1, t=length(varx))
                        scatterpchs <- rep(16, t=length(varx))
                        scatterpchs_vstime <- seq_along(varx)

                        if (T) { # monthly
                           
                            # color data by time or seasons
                            if (F) { # by time
                                message("color by time ...")
                                timecols <- colorRampPalette(c("blue", "red"))(length(scatter_set1_vs_set2[[1]]))
                                scatter_suffix <- "_bytime"
                            } else if (T) { # by season
                                message("special: plot_scatter_s1_vs_s2 highlighted by seasons")
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
                            timecols_rgb <- rgb(t(col2rgb(timecols)/255), alpha=alpha_rgb)
                            
                            xlim <- range(scatter_set1_vs_set2[[1]], na.rm=T)
                            ylim <- range(scatter_set1_vs_set2[[2]], na.rm=T)
                            xat <- pretty(xlim, n=10)
                            xlab <- format(xat, trim=T)
                            yat <- pretty(ylim, n=10)
                            ylab <- format(yat, trim=T)

                            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                               varname, "_", 
                                               paste0(names_short[i], "_", seasonsp[i], 
                                                      "_", froms_plot[i], "_to_", tos_plot[i], "_", 
                                                      areas[i], collapse="_vs_"), 
                                               scatter_suffix,
                                               ".", p$plot_type)
                            dir.create(dirname(plotname), recursive=T, showWarnings=F)
                            if (p$plot_type == "png") {
                                png(plotname, width=p$scatter_width, height=p$scatter_height,
                                    res=p$ppi, family=p$family_png)
                            } else if (p$plot_type == "pdf") {
                                pdf(plotname, width=p$inch, height=p$inch,
                                    family=p$family_pdf, encoding=encoding)
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
                                                " ", mode_p, " ", varname, " ", 
                                                paste(unique(seasonsp[i]), collapse=","), " ", 
                                                paste(unique(fromsp[i]), collapse=","), " to ", 
                                                paste(unique(tosp[i]), collapse=","))
                                title(title, cex.main=0.5)
                            }

                            # add variable label
                            eval(parse(text=paste0("var_infos <- ", varname, "_infos")))
                            mtext(side=1, var_infos[[1]]$label, line=3.4, cex=0.9)
                            mtext(side=2, var_infos[[2]]$label, line=3.4, cex=0.9)

                            # add zero lines
                            if (add_zeroline) {
                                abline(h=0, col="gray", lwd=0.5)
                                abline(v=0, col="gray", lwd=0.5)
                            }

                            # add 1:1 line
                            if (add_scatter_1to1_line) {
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
                                le$cex <- 1
                                le$cex <- 0.85
                                le$text <- names(season_cols) #names_legend[i]
                                le$col <- season_cols #"black"
                                le$lty <- NA
                                le$lwd <- NA
                                #le$pchs <- scatterpchs_vstime[i]
                                le$pch <- scatterpchs_vstime
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
                            message("\nsave plot ", plotname, " ...")
                            dev.off()
                            if (p$plot_type == "pdf") {
                                if (T) {
                                    if (F && "extrafont" %in% (.packages())){
                                        message("run `extrafont::embed_fonts()` ...")
                                        extrafont::embed_fonts(plotname, outfile=plotname)
                                    } else {
                                        message("run `grDevices::embedFonts()` ...")
                                        grDevices::embedFonts(plotname, outfile=plotname)
                                    }
                                } else {
                                    message("todo: sometimes pdf font embedding blurrs colors why?")
                                }
                            }

                        } # if T monthly

                    } else {
                        message(" but `dim_names_set1_vs_set2` = \"", 
                                paste(dim_names_set1_vs_set2, collapse="\", \""), "\" != \"time\"")
                    }

                } else { # if `temp2_datas` does not exist
                    message(" but `ndims_set1_vs_set2` = ", ndims_set1_vs_set2, " != 1") 
                } # if temp2_datas exists or not
            
            } else {            
                message("but `", scatter_s1_vs_s1_varname, "_datas` does not exist.")
            }

        } # if plot_scatter_s1_vs_s2 

        ## plot var1 vs var2 of `datas`
        if (plot_scatter_v1_vs_v2) {

            message("\n****************** `plot_scatter_v1_vs_v2`=T --> scatterplot varx vs vary *******************\n",
                    "`varnamex = \"", varnamex, "\", varnamey = \"", varnamey, "\"")

            if (exists(varnamex) && exists(varnamey)) {

                eval(parse(text=paste0("varx <- ", varnamex)))
                eval(parse(text=paste0("vary <- ", varnamey)))
                if (length(varx) != length(vary)) {
                    stop("varx and vary are of different lengths")
                }

                varx_dims <- lapply(lapply(varx, attributes), "[[", "dims")
                vary_dims <- lapply(lapply(vary, attributes), "[[", "dims")

                # convert potential n-dim data --> to vector
                for (i in seq_along(varx)) {
                    varx[[i]] <- as.vector(varx[[i]])
                    vary[[i]] <- as.vector(vary[[i]])
                }

                message("plot v1 vs v2 of all settings along time dims ...")
                scattercexs <- rep(1, t=length(varx))
                scatterpchs <- rep(16, t=length(varx))

                # "temp2_datas" or "temp2_datasmon" to "temp2"
                varnamexp <- regexpr("_datas", varnamex)
                varnamexp <- substr(varnamex, 1, varnamexp-1)
                varnameyp <- regexpr("_datas", varnamey)
                varnameyp <- substr(varnamey, 1, varnameyp-1)
                varname <- paste0(varnamexp, "_vs_", varnameyp)
                message("varnamex = \"", varnamex, "\"\n",
                        "varnamey = \"", varnamey, "\"\n",
                        "varnamexp = \"", varnamexp, "\"\n",
                        "varnameyp = \"", varnameyp, "\"\n",
                        "varname = \"", varname, "\"")
                eval(parse(text=paste0("varx_infos <- ", varnamexp, "_infos")))
                eval(parse(text=paste0("vary_infos <- ", varnameyp, "_infos")))

                # special ECS/TCR stuff
                if (T && varname == "temp2_vs_toa_imbalance" && any(grepl("piControl", names_short))) {
                    message("\nspecial: calc equilibrium climate sensitivity (ECS): https://github.com/ESMValGroup/ESMValTool/issues/1814")
                    inds <- seq_along(z)
                    if (length(which(grepl("piControl", names_short))) == 1) {
                        piind <- which(grepl("piControl", names_short))
                        inds <- inds[-piind]
                    } else {
                        stop("not defined")
                    }
                    if (F) { # subtract last PI value
                        message("case 1: subtract last PI value from experiments ...")
                        for (i in inds) {
                            varx[[i]] <- varx[[i]] - rep(varx[[piind]][length(varx[[piind]])], 
                                                         t=length(length(varx[[piind]])))
                            varx_infos[[i]]$label <- "2m temperature increase [K]"
                        }
                        # last: pi itself
                        varx[[piind]] <- varx[[piind]] - rep(varx[[piind]][length(varx[[piind]])], 
                                                             t=length(length(varx[[piind]])))
                        varx_infos[[piind]]$label <- "2m temperature increase [K]"
                    
                    } else if (T) { # subtract PI values year by year
                        message("case 2: calc `delta data(year_i) = data_experiment(year_i) minus data_piControl(year_i)` ...")
                        for (i in inds) {
                            if (length(varx[[i]]) != length(varx[[piind]])) {
                                stop("varx[[", i, "]] and varx[[", piind, "]] are of different length")
                            }
                            if (length(vary[[i]]) != length(vary[[piind]])) {
                                stop("vary[[", i, "]] and vary[[", piind, "]] are of different length")
                            }
                            if (!all(dims[[i]]$timelt$year == dims[[piind]]$timelt$year)) {
                                stop("years of setting ", i, " and ", piind, " differ")
                            }
                            varx[[i]] <- varx[[i]] - varx[[piind]]
                            varx_infos[[i]]$label <- "2m temperature increase [K]"
                            vary[[i]] <- vary[[i]] - vary[[piind]]
                        }
                        # last: pi itself
                        varx[[piind]] <- varx[[piind]] - varx[[piind]] # = all zero
                        varx_infos[[piind]]$label <- "2m temperature increase [K]"
                        if (F) vary[[piind]] <- vary[[piind]] - vary[[piind]] # = all zero
                    }
                } # if varname == "temp2_vs_toa_imbalance" do ECS/TCR stuff

                xlim <- range(varx, na.rm=T)
                ylim <- range(vary, na.rm=T)
                xat <- pretty(xlim, n=10)
                xlab <- format(xat, trim=T)
                yat <- pretty(ylim, n=10)
                ylab <- format(yat, trim=T)
                message("xlim = ", min(xlim), " / ", max(xlim))
                message("ylim = ", min(ylim), " / ", max(ylim))

                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   varname, "_", 
                                   paste0(names_short, "_", seasonsp, 
                                          "_", froms_plot, "_to_", tos_plot, "_", areas, collapse="_vs_"), 
                                   ".", p$plot_type)
                dir.create(dirname(plotname), recursive=T, showWarnings=F)
                if (p$plot_type == "png") {
                    png(plotname, width=p$scatter_width, height=p$scatter_height,
                        res=p$ppi, family=p$family_png)
                } else if (p$plot_type == "pdf") {
                    pdf(plotname, width=p$inch, height=p$inch,
                        family=p$family_pdf, encoding=encoding)
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
                axis(1, at=xat, labels=xlab, cex.axis=1)
                axis(2, at=yat, labels=ylab, las=1, cex.axis=1)

                # add title
                if (add_title) {
                    title <- paste0(paste(unique(areas), collapse=","), 
                                    " ", mode_p, " ", varname, " ", 
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
                if (add_scatter_1to1_line) {
                    message("add 1:1 line ...")
                    abline(a=0, b=1, col="gray") # a=intercept, b=slope
                }

                # add data to scatter plot
                message("add data ...")
                plotorder <- seq_along(varx)
                
                # special: change plot order
                if (T && varname == "temp2_vs_toa_imbalance" && any(grepl("piControl", names_short))) {
                    # plot PI last
                    message("special: change plot order from ", 
                            paste(plotorder, collapse=","), " to ", appendLF=F)
                    plotorder <- c(plotorder[-which(grepl("piControl", names_short))],
                                   which(grepl("piControl", names_short)))
                    if (length(plotorder) == 0) stop("length(plotorder) new is 0")
                    message(paste(plotorder, collapse=","), " ...")
                }

                # special: add gray dots of data first
                if (T && varname == "temp2_vs_toa_imbalance") {
                    message("special: add individual years by gray points")
                    for (i in plotorder) {
                        if (F && names_short[i] == "piControl") {
                            message("special: dont plot individual years if piControl")
                            # nothing
                        } else {
                            points(varx[[i]], vary[[i]], 
                                   #col=cols_rgb[i], 
                                   col=rgb(t(col2rgb("gray")/255), alpha=0.3),
                                   pch=scatterpchs[i], cex=scattercexs[i])
                        }
                    }
                } # if add gray dots of data first
                
                # add data 
                for (i in plotorder) {
                    if (T && varname == "temp2_vs_toa_imbalance") {
                        if (T && grepl("piControl", names_short[i])) {
                            message("special: plot only time mean for piControl")
                            points(mean(varx[[i]]), mean(vary[[i]]), 
                                   #col=cols_rgb[i], 
                                   col=cols[i],
                                   pch=scatterpchs[i], cex=scattercexs[i])
                        } else { # non-PI
                            message("special: use year as symbols")
                            #years_of_setting_to_show <- c(1:10, seq(25, 250, b=25))
                            #years_of_setting_to_show <- c(1:10, 15, seq(20, 100, b=10), seq(125, 250, b=25))
                            years_of_setting_to_show <- c(1:10, 15, seq(20, 150, b=10), seq(175, 250, b=25))
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
                        }
                        
                    # else default plotting
                    } else { 
                        points(varx[[i]], vary[[i]], 
                               #col=cols_rgb[i], 
                               col=rgb(t(col2rgb("gray")/255), alpha=0.2),
                               pch=scatterpchs[i], cex=scattercexs[i])
                    } # special plots depending on setting
                } # for i in plotorder 
                # finished add data to scatter plot 

                # add linear trend
                names_legend_p_w_lm <- names_legend_p
                if (any(add_linear_trend)) {
                    message("\nadd linear trend in scatterplot varx vs vary ...")
                    lms_lin <- vector("list", l=length(varx))
                    lm_text <- c()
                    for (i in seq_along(varx)) {
                        if (add_linear_trend[i]) {
                            message("\nsetting ", i, "/", length(varx), ": ", names_short_p[i])
                            lms_lin[[i]] <- lm(vary[[i]] ~ varx[[i]])
                            lm_summary <- summary(lms_lin[[i]])
                            print(lm_summary)
                            # linear regression results
                            intercept <- as.vector(lm_summary$coefficients[1,1])
                            intercept_error <- as.vector(lm_summary$coefficients[1,2])
                            intercept_pval <- paste0("=", lm_summary$coefficients[1,4])
                            if (dim(lm_summary$coefficients)[1] == 1) { # all input NA or lm not succcessfull
                                slope <- NA
                                slope_pval <- NA
                            } else {
                                slope <- as.vector(lm_summary$coefficients[2,1])
                                slope_error <- as.vector(lm_summary$coefficients[2,2])
                                slope_pval <- lm_summary$coefficients[2,4]
                                if (slope_pval < 1e-5) { 
                                    slope_pval <- "<1e-5"
                                } else {
                                    slope_pval <- paste0("=", format(slope_pval, trim=T))
                                }
                                # plot regression line within data limits only
                                if (F) {
                                    message("draw linear regression line within regression limits only ...")
                                    lines(varx[[i]], lms_lin[[i]]$fitted.values, 
                                          col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i])
                                # or plot line through whole plot with regression coefficients
                                } else if (T || 
                                           (varname == "temp2_vs_toa_imbalance" && grepl("abrupt-4xCO2", names_short[i]))) {
                                    message("draw linear regression line through whole plot ...")
                                    abline(a=lms_lin[[i]]$coefficients[1], b=lms_lin[[i]]$coefficients[2],
                                           col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i])
                                }
                                if (T) {
                                    message("add linear regression coefficients to legend ...")
                                    first_part <- names_legend_p[i]
                                    last_part <- "" # default
                                    last_part <- eval(substitute(expression(paste(
                                        "(", alpha, "=", slope, ", p", p, ", r=", r, ")")),
                                                                 list(slope=round(slope, 2), p=slope_pval, 
                                                                      r=round(sqrt(lm_summary$r.squared), 2))))
                                    if (is.expression(last_part)) {
                                        new <- bquote(.(do.call(substitute, as.list(first_part))) ~ 
                                                      .(do.call(substitute, as.list(last_part))))
                                        names_legend_p_w_lm[i] <- eval(substitute(expression(new), list(new=new)))
                                    }
                                }
                            } # if lm sucessfull or not
                            
                            # special stuff 
                            if (T && varname == "temp2_vs_toa_imbalance") {
                                # transient climate response: TCR after winton et al. 2014
                                # --> global warming as modeled in the 1pct experiment when the pi CO2 value doubled
                                if (grepl("1pctCO2", names_short[i]) && exists("co2_hist") && exists("co2_1pct")) {
                                    message("\nspecial: transient climate response TCR based on `co2_hist` and `co2_1pct` after\n",
                                            "Winton et al. 2014 (https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1002/2014GL061523):\n",
                                            "   \"The transient sensitivity is quantified with the transient climate\n",
                                            "   response (TCR), the global surface warming at CO2 doubling in a 1%/year\n",
                                            "   CO2 increase experiment.\"\n",
                                            "and later:\n",
                                            "   \"The values for the transient climate response (TCR)—the year 61–80 \n",
                                            "   average global warming—are ...\"\n",
                                            "--> length(61:80) = ", length(61:80), " years\n",
                                            "see https://github.com/ESMValGroup/ESMValTool/issues/1901 ...")
                                    co2_hist_1850_ind <- which(co2_hist$time$year+1900 == 1850)
                                    if (length(co2_hist_1850_ind) != 1) stop("not defined for current `co2_hist` data")
                                    co2_hist_1850 <- drop(co2_hist$co2_ppm[co2_hist_1850_ind])
                                    co2_1pct_doubled_1850_ind <- which.min(abs(co2_1pct$co2_ppm - 2*co2_hist_1850))[1]
                                    co2_1pct_doubled <- drop(co2_1pct$co2_ppm[co2_1pct_doubled_1850_ind])
                                    co2_1pct_doubled_time <- co2_1pct$time[co2_1pct_doubled_1850_ind]
                                    co2_1pct_doubled_1850_model_ind <- difftime(d$time[[i]], 
                                                                                rep(co2_1pct_doubled_time, t=length(d$time[[i]])))
                                    co2_1pct_doubled_1850_model_ind <- which.min(abs(co2_1pct_doubled_1850_model_ind))[1]
                                    deltaT_1pct_co2_doubled <- varx[[i]][co2_1pct_doubled_1850_model_ind]
                                    message("TCR_{1year} = dT_{modelexp-1pctCO2}[CO2_{1pctCO2}=2xCO2_{piControl}]\n",
                                            "with CO2_{piControl} from e.g. ", co2_hist$file, "\n",
                                            "     CO2_{1pctCO2} from e.g. ", co2_1pct$file, "\n",
                                            "--> CO2_{piControl} = CO2_{historical,year=1850} = ",
                                            "CO2_{1pctCO2,year=1850} = ", co2_hist_1850, " ppm\n",
                                            "--> 2 x ", co2_hist_1850, " ppm = ", 2*co2_hist_1850, " ppm\n",
                                            "--> this CO2 value is from ", co2_hist$time[co2_hist_1850_ind], 
                                            " (time from ", co2_hist$file, ")\n",
                                            "--> closest CO2_{1pctCO2} = ", co2_1pct_doubled, 
                                            " ppm from ", co2_1pct$time[co2_1pct_doubled_1850_ind], 
                                            " (time from ", co2_1pct$file, ")\n",
                                            "--> counting from year 1850 (=year number 1), this year ",
                                            co2_1pct$time[co2_1pct_doubled_1850_ind]$year+1900, 
                                            " is year number ",
                                            co2_1pct$time[co2_1pct_doubled_1850_ind]$year+1900-1850+1, " (=",
                                            co2_1pct$time[co2_1pct_doubled_1850_ind]$year+1900, "-1850+1)\n",
                                            "--> the closest 1pctCO2 model-date to this 1pctCO2-date is ",
                                            "model-date number ", co2_1pct_doubled_1850_model_ind, ": ", 
                                            d$time[[i]][co2_1pct_doubled_1850_model_ind], "\n",
                                            "--> dT_{modelexp-1pctCO2,model-year[", co2_1pct_doubled_1850_model_ind, "]=", 
                                            dims[[i]]$timelt[co2_1pct_doubled_1850_model_ind]$year+1900, "} = ", 
                                            deltaT_1pct_co2_doubled, " K = ",
                                            round(deltaT_1pct_co2_doubled, 2), " K")
                                    
                                    # or use model year 61-80 starting from 1850
                                    # --> 1911-1930 (20 year) mean global warming as in winton et al. 2014
                                    #TCR_from_to <- c(as.POSIXct("1911-01-01", tz="UTC"),
                                    #                 as.POSIXct("1931-21-31", tz="UTC"))
                                    TCR_from_to <- c(as.POSIXct("1910-01-01", tz="UTC"),
                                                     as.POSIXct("1930-12-31", tz="UTC"))
                                    year_inds <- which(d$time[[i]] >= TCR_from_to[1] &
                                                       d$time[[i]] <= TCR_from_to[2])
                                    if (length(year_inds) == 0) {
                                        stop("cannot calc TCR between years ", TCR_from_to[1], " and ", TCR_from_to[2], 
                                                ": out or range of d$time[[", i, "]]")
                                    }
                                    average_deltaT_1pct_co2_doubled <- mean(varx[[i]][year_inds])
                                    message("TCR_{mean} = dT_{modelexp-1pctCO2} mean over model-dates ", 
                                            min(year_inds), " to ", max(year_inds), ":\n",
                                            "            ", min(d$time[[i]][year_inds]), " to ",
                                            max(d$time[[i]][year_inds]), " (n=", 
                                            length(year_inds), "; n_unique_years=", 
                                            length(unique(dims[[i]]$timelt[year_inds]$year+1900)), ")\n",
                                            "--> ", average_deltaT_1pct_co2_doubled, " K = ", 
                                            round(average_deltaT_1pct_co2_doubled, 2), " K")
                                    lm_text <- c(lm_text, 
                                                 eval(substitute(expression(paste(
            "TCR = ", Delta, "T"["1%"], "(CO"[2], "=2" %*% "CO"[paste("2,PI")], " = ", co2_1pct_doubled, " ppm)")),
                                                                 list(co2_1pct_doubled=round(co2_1pct_doubled)))),
                                                 eval(substitute(expression(paste(
            "    = ", bar(paste(Delta, "T"))["1%"]^"years 61-80", " = ", average_deltaT_1pct_co2_doubled, " K")),
                                                                 list(average_deltaT_1pct_co2_doubled=round(average_deltaT_1pct_co2_doubled, 2)))))
                                } # if 1pctCO2 and exists("co2_hist")
                                
                                if (grepl("abrupt-4xCO2", names_short[i]) && !is.na(slope)) {
                                    message("\nspecial: ECS for abrupt-4xCO2 and non-NA regression slope\n",
                                            "see https://github.com/ESMValGroup/ESMValTool/issues/1814 ...")
                                    # gregory et al. 2004: equilibrium climate sensitivity (ECS):
                                    alpha <- slope
                                    alpha_error <- slope_error
                                    radiative_forcing_F <- intercept
                                    radiative_forcing_F_error <- intercept_error
                                    deltaT_eq_4x <- radiative_forcing_F/abs(alpha)
                                    deltaT_eq_4x_error <- sort(c((radiative_forcing_F - radiative_forcing_F_error)/(abs(alpha) - alpha_error),
                                                                 (radiative_forcing_F + radiative_forcing_F_error)/(abs(alpha) + alpha_error)))
                                    deltaT_eq_2x <- deltaT_eq_4x/2
                                    deltaT_eq_2x_error <- deltaT_eq_4x_error/2
                                    message("deltaT_eq_4x for setting ", names_short[i], " = ", round(deltaT_eq_4x, 2), " (", 
                                            round(min(deltaT_eq_4x_error), 2), "-", round(max(deltaT_eq_4x_error), 2), 
                                            ") K (gregory et al. 2004)\n",
                                            " --> deltaT_eq_4x/2 = deltaT_eq_2x = ECS = equilibrium climate sensitivity = ", 
                                            round(deltaT_eq_2x, 2), " (", round(min(deltaT_eq_2x_error), 2), "-", 
                                            round(max(deltaT_eq_2x_error), 2), ") K")
                                    Forcing <- abs(alpha)*as.vector(varx[[i]]) # = alpha*dT
                                    lm_text <- c(lm_text,
                                                 eval(substitute(expression(paste(
"F"[paste("4" %*% "")], " = ", radiative_forcing_F, "" %+-% "", radiative_forcing_F_error, " W m"^paste(-2), " (intercept)")),
                                                                 list(radiative_forcing_F=round(radiative_forcing_F, 2),
                                                                      radiative_forcing_F_error=round(radiative_forcing_F_error, 2)))),
                                                 eval(substitute(expression(paste(
alpha, ""[paste("4" %*% "")], " = ", alph, "" %+-% "", alpha_error, " W m"^paste(-2), " K"^paste(-1), " (slope)")), 
                                                                 list(alph=round(alpha, 2), alpha_error=round(alpha_error, 2)))),
                                                 eval(substitute(expression(paste(
"F"[paste("4" %*% "")], "/|", alpha, ""[paste("4" %*% "")], "| = ", Delta, "T"[paste("eq,4" %*% "")], " = ", deltaT_eq_4x, " (", deltaT_eq_4x_lower, "-", deltaT_eq_4x_upper, ") K")),
                                                                 list(deltaT_eq_4x=round(deltaT_eq_4x, 2),
                                                                      deltaT_eq_4x_lower=round(min(deltaT_eq_4x_error), 2), 
                                                                      deltaT_eq_4x_upper=round(max(deltaT_eq_4x_error), 2)))),
                                                 eval(substitute(expression(paste(
"ECS = ", Delta, "T"[paste("eq,2" %*% "")], " = 1/2 ", Delta, "T"[paste("eq,4" %*% "")], " = ", deltaT_eq_2x, " (", deltaT_eq_2x_lower, "-", deltaT_eq_2x_upper, ") K")),
                                                                 list(deltaT_eq_2x=round(deltaT_eq_2x, 2),
                                                                      deltaT_eq_2x_lower=round(min(deltaT_eq_2x_error), 2), 
                                                                      deltaT_eq_2x_upper=round(max(deltaT_eq_2x_error), 2)))))
                                } # if abrupt-4xCO2
                            } # if temp2_vs_toa_imbalance
                        } # if add_linear_trend[i]
                    } # for i in seq_along(varx)
                    if (exists("deltaT_eq_2x") && exists("average_deltaT_1pct_co2_doubled")) {
                        CER <- average_deltaT_1pct_co2_doubled/deltaT_eq_2x # climate equilibrium ratio
                        message("min/max of climate equilibirum ratio CER = TCR/ECS = ", min(CER), "/", max(CER))
                    }
                    if (!is.null(lm_text)) {
                        message("add special linear regression infos to plot")
                        legend("topright", 
                               lm_text, col="black", #text.col=text_cols[i], 
                               lty=NA, pch=NA, lwd=NA, bty="n", 
                               cex=0.9, y.intersp=1.1)
                    }
                } # if any add_linear_trend

                # add non-linear trend
                if (add_nonlinear_trend) {
                    message("\nadd non-linear trend ...")
                    lms_exp <- vector("list", l=length(varx))
                    library(forecast)
                    for (i in seq_along(varx)) {
                        if (any(i == c(2, 3))) {
                            message("setting ", i, "/", length(varx), ": ", names_short_p[i])
                            lms_exp[[i]] <- tslm(ts(vary[[i]]) ~ trend, lambda = 0)
                            print(summary(lms_exp[[i]]))
                            
                            # plot regression line within data limits only
                            if (F) {
                                lines(varx[[i]], lms_lin[[i]]$fitted.values, 
                                      col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i])
                                
                            # or plot line through whole plot with regression coefficients
                            } else if (T) {
                                abline(a=lms_lin[[i]]$coefficients[1], b=lms_lin[[i]]$coefficients[2],
                                       col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i])
                            }
                        }
                    }
                }

                # add legend if wanted
                if (add_legend) {
                    message("\nadd default stuff to plot_scatter_v1_vs_v2 legend ...")
                    le <- list()
                    #le$pos <- "topright"
                    le$pos <- "bottomright"
                    le$ncol <- 1
                    #le$ncol <- 2 
                    le$cex <- 1
                    le$text <- names_legend_p_w_lm # = names_legend_p if no lm
                    le$col <- cols_p
                    #le$col <- cols_rgb
                    #le$col <- cols
                    le$text_cols <- text_cols_p
                    le$lty <- NA
                    le$lwds <- NA
                    le$pchs <- scatterpchs
                    #le$pchs <- pchs_p
                    if (T && varname == "temp2_vs_toa_imbalance") {
                        message("special legend pchs")
                        le$pchs <- rep(NA, t=length(varx))
                    }
                    # add stuf to legend here
                    if (F) {
                        message("add non default stuff to plot_scatter_v1_vs_v2 legend ...")

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
                message("save v1 vs v2 plot\n   \"", plotname, "\"\n...")
                dev.off()
                if (p$plot_type == "pdf") {
                    if (T) {
                        if (F && "extrafont" %in% (.packages())){
                            message("run `extrafont::embed_fonts()` ...")
                            extrafont::embed_fonts(plotname, outfile=plotname)
                        } else {
                            message("run `grDevices::embedFonts()` ...")
                            grDevices::embedFonts(plotname, outfile=plotname)
                        }
                    } else {
                        message("todo: sometimes pdf font embedding blurrs colors why?")
                    }
                }

                ## scatter plot for each setting colored by time or season
                if (T
                    && sapply(lapply(varx_dims, "==", c("time")), all)
                    && sapply(lapply(vary_dims, "==", c("time")), all)) {
                    message("\nspecial: v1 vs v2 colored by time or seasons for every setting ...")
                    message("update dinds/vinds")

                    xlim <- range(varx, na.rm=T)
                    ylim <- range(vary, na.rm=T)
                    xat <- pretty(xlim, n=10)
                    xlab <- format(xat, trim=T)
                    yat <- pretty(ylim, n=10)
                    ylab <- format(yat, trim=T)
                    message("xlim = ", min(xlim), " / ", max(xlim))
                    message("ylim = ", min(ylim), " / ", max(ylim))

                    for (i in seq_along(varx)) {

                        message("varx[[", i, "]] vs vary[[", i, "]] ...")

                        if (T) { # monthly
                            
                            # color data by time or seasons
                            if (F) { # by time
                                message("color v1 vs v2 by time ...")
                                timecols <- colorRampPalette(c("blue", "red"))(length(varx[[i]]))
                                scatter_suffix <- "_bytime"
                            } else if (T) { # by season
                                message("color v1 vs v2 by season ...")
                                if (i == 1) scatterpchs_vstime <- 1:4
                                season_cols <- c(DJF="blue", MAM="darkgreen", JJA="red", SON="brown")
                                timecols <- rep(NA, t=length(varx[[i]]))
                                season_pchs <- timecols
                                timelt <- as.POSIXlt(d$time[[i]])
                                djf_inds <- which(!is.na(match(timelt$mon+1, c(1, 2, 12))))
                                mam_inds <- which(!is.na(match(timelt$mon+1, c(3, 4, 5))))
                                jja_inds <- which(!is.na(match(timelt$mon+1, c(6, 7, 8))))
                                son_inds <- which(!is.na(match(timelt$mon+1, c(9, 10, 11))))
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
                            timecols_rgb <- rgb(t(col2rgb(timecols)/255), alpha=alpha_rgb)
                            
                            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                               varname, "_", 
                                               paste0(names_short_p[i], "_", seasonsp_p[i], "_",
                                                      froms_plot_p[i], "_to_", tos_plot_p[i], "_", 
                                                      areas_p[i], collapse="_vs_"), 
                                               scatter_suffix,
                                               ".", p$plot_type)
                            dir.create(dirname(plotname), recursive=T, showWarnings=F)
                            if (p$plot_type == "png") {
                                png(plotname, width=p$scatter_width, height=p$scatter_height,
                                    res=p$ppi, family=p$family_png)
                            } else if (p$plot_type == "pdf") {
                                pdf(plotname, width=p$inch, height=p$inch,
                                    family=p$family_pdf, encoding=encoding)
                            }

                            # set plot margins
                            mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                            mar[4] <- 1 # decrease right margin
                            if (!add_title) mar[3] <- 1 # decrease upper margin
                            if (add_data_right_yaxis_ts) mar[4] <- mar[2] # same as left  

                            # open plot
                            par(mar=mar)
                            plot(varx[[i]], vary[[i]], t="n",
                                 xlab=NA, ylab=NA, 
                                 xlim=xlim, ylim=ylim, xaxt="n", yaxt="n")
                            axis(1, at=xat, labels=xlab)
                            axis(2, at=yat, labels=ylab, las=1)

                            # add title
                            if (add_title) {
                                title <- paste0(names_short_p[i], " ", 
                                                paste(unique(areas_p[i]), collapse=","), 
                                                " ", mode_p, " ", varname, " ", 
                                                paste(unique(seasonsp_p[i]), collapse=","), " ", 
                                                paste(unique(froms_plot_p[i]), collapse=","), " to ", 
                                                paste(unique(tos_plot_p[i]), collapse=","))
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

                            # add 1:1 line
                            if (add_scatter_1to1_line) {
                                message("add 1:1 line ...")
                                abline(a=0, b=1, col="gray") # a=intercept, b=slope
                            }
                            
                            # add data to scatter plot colored by time
                            message("add ", names(varx)[i], " data vs seasons ...")
                            points(varx[[i]], vary[[i]], 
                                   col=timecols,
                                   #col=timecols_rgb,
                                   #pch=scatterpchs_vstime[i], 
                                   pch=season_pchs,
                                   cex=scattercexs[i])

                            # add legend if wanted
                            if (add_legend) {
                                le <- list()
                                #le$pos <- "topleft"
                                le$pos <- "topright"
                                le$ncol <- 1
                                #le$ncol <- 2 
                                le$cex <- 1
                                le$cex <- 0.85
                                le$text <- names(season_cols) #names_legend[i]
                                le$col <- season_cols #"black"
                                le$lty <- NA
                                le$lwds <- NA
                                #le$pchs <- scatterpchs_vstime[i]
                                le$pchs <- scatterpchs_vstime
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
                            message("save v1 vs v2 plot colored by time or season or ...\n   \"", plotname, "\"\n...")
                            dev.off()
                            if (p$plot_type == "pdf") {
                                if (T) {
                                    if (F && "extrafont" %in% (.packages())){
                                        message("run `extrafont::embed_fonts()` ...")
                                        extrafont::embed_fonts(plotname, outfile=plotname)
                                    } else {
                                        message("run `grDevices::embedFonts()` ...")
                                        grDevices::embedFonts(plotname, outfile=plotname)
                                    }
                                } else {
                                    message("todo: sometimes pdf font embedding blurrs colors why?")
                                }
                            }

                        } # T monthly

                    } # for i nsettings

                } # if sapply(lapply(varx_dims, "==", c("time")), all)
                
            } else {
                message("but `", varnamex, "` and/or `", varnamey, "` do not exist")
            }

        } # if plot_scatter_v1_vs_v2 of `datas`
    
    } # ploti nplots: length(z_samevars) or length(z_samedims) or ...

} # plot_groupi

message("\nfinished\n")

