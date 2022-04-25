# r

# plotting post processing results of echam, jsbach, mpiom output

# source order:
# 1. namelist.plot.r
# 2. namelist.general.plot.r
# 3. helper_functions.r
# 4. ~/scripts/r/functions/myfunctions.r
# 5. plot_echam.r (this file)
# 6. namelist.area.r
# 7. load_pangaea_data.r
# 8. load_special_data.r
# 9. plot_special_data.r

graphics.off()
options(show.error.locations=T)
#options(warn=2) # stop on warnings
if (T && options()$warn != 0) options(warn=0) # back to default
fctbackup <- `[`
`[` <- function(...) { fctbackup(..., drop=F) } # set back to default: `[` <- fctbackup 

if (!exists("host")) {
    stop("`host` does not exist. did you accidentally run `source(\"plot.echam.r\")` instead of `source(\"namelist.plot.r\")`?")
}

# load libraries necessary for plot_echam.r
message("\nload packages defined in ", host$repopath, "/requirements_plot.txt ...")
requirements <- readLines(paste0(host$repopath, "/requirements_plot.txt"))
for (r in requirements) {
    r <- trimws(r)
    if (substr(r, 1, 1) != "#") { # current line is not a comment
        if (grepl(" ", r)) { # there is a space
            r <- substr(r, 1, regexpr(" ", r)-1) # everything until first space
        }
        message("   ", r)
        suppressPackageStartupMessages(library(r, character.only=T))
    }
}

## check user input from plot namelist

# check if must-have objects are set by user and are of correct length
objs <- c("prefixes", "models", "names_short", "fromsf", "tosf", "varnames_in", "modes")
for (obj in objs) if (!exists(obj)) stop("provide `", obj, "` in plot namelist")
nsettings <- length(prefixes)
for (i in seq_along(objs)) {
    cmd <- paste0("length(", objs[i], ")")
    length_of_obj <- eval(parse(text=cmd))
    if (length_of_obj != nsettings) {
        stop("variable `", objs[i], "` is of length ", length_of_obj, 
             " but must be of length ", nsettings, " (as `prefixes`)")
    }
}

# replace blanks and special chars in `names_short`
names_short <- gsub("\\s+", "_", names_short)
names_short <- gsub("[[:punct:]]", "_", names_short)

if (exists("varnames_uv")) {
    if (!all(sapply(varnames_uv, length) == 3)) {
        stop("every list in `varnames_uv` must have 3 entries (name of uv-, u- and v-components)")
    }
}

if (center_ts && scale_ts) {
    stop("both `center_ts` and `scale_ts` are true. choose one: center = x-mu; scale = (x-mu)/sd.")
}

# set defaults
if (exists("workpath")) host$workpath <- workpath
if (!exists("postpaths")) { # default from post_echam.r
    postpaths <- rep(paste0(host$workpath, "/post"), t=nsettings)
} else {
    if (!any(dir.exists(postpaths))) {
        for (i in seq_along(postpaths)) {
            if (!dir.exists(postpaths[i])) {
                stop("provided `postpaths[", i, "]` = ", postpaths[i], 
                     " does not exist. cannot load post data from non-existing path")
            }
        }
    }
}
postpaths <- normalizePath(postpaths)
if (!exists("plotpath")) { # default from post_echam.r
    plotpath <- paste0(host$workpath, "/plots")
    if (!exists("plotprefix")) { # default
        plotpath <- paste0(plotpath, "/", paste(unique(models), collapse="_vs_"))
    } else {
        plotpath <- paste0(plotpath, "/", plotprefix)
    }
}
if (!dir.exists(plotpath)) dir.create(plotpath, recursive=T, showWarnings=F)
plotpath <- normalizePath(plotpath)
if (!exists("names_legend")) names_legend <- names_short

if (!exists("codes")) codes <- rep("", t=nsettings)
codesf <- codes
codesf[codes != ""] <- paste0("_selcode_", codesf[codes != ""])

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
        message("all `n_mas` = 1, change `add_unsmoothed` from F to T ...")
        add_unsmoothed <- T
    }
    if (add_smoothed == T) {
        message("all `n_mas` = 1, change `add_smoothed` from T to F ...")
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

if (!exists("levs") && !exists("levsf")) {
    levs <- levsf <- rep("", t=nsettings)
} else if (exists("levs") && !exists("levsf")) {
    levsf <- levs
    levsf[levs != ""] <- paste0("_sellevel_", levs[levs != ""])
} else if (!exists("levs") && exists("levsf")) {
    levs <- levsf
    levs[levsf != ""] <- gsub("_", "", levsf[levsf != ""])
}

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
if (!exists("lev_fromsf")) lev_fromsf <- rep(NA, t=nsettings)
if (!exists("lev_tosf")) lev_tosf <- rep(NA, t=nsettings)
if (!exists("depth_fromsp")) depth_fromsp <- depth_fromsf
if (!exists("depth_tosp")) depth_tosp <- depth_tosf
if (!exists("lev_fromsp")) lev_fromsp <- lev_fromsf
if (!exists("lev_tosp")) lev_tosp <- lev_tosf

if (!exists("areas")) areas <- rep("global", t=nsettings)
if (!exists("regboxes")) {
    regboxes <- vector("list", l=nsettings)
    regboxes <- lapply(regboxes, base::append, list(regbox=NA))
}
names(regboxes) <- names_short
areas_out <- areas
if (any(!is.na(sapply(regboxes, "[[", "regbox")))) { # run namelist.area.r to get lon/lat of regional boxes
    message("some regboxes$regbox are not NA -> load and check coordinates from namelist.area.r ...")
    if (!file.exists("namelist.area.r")) {
        stop("cannot find file namelist.area.r")
    } 
    source("namelist.area.r")
    areas_out[which(!is.na(sapply(regboxes, "[[", "regbox")))] <- sapply(regboxes, "[[", "regbox")
}

if (!exists("reg_dxs")) reg_dxs <- rep("", t=nsettings)
if (!exists("reg_dys")) reg_dys <- rep("", t=nsettings)
reg_dxsf <- reg_dxs
reg_dxsf[reg_dxs != ""] <- paste0("_regular_dx", reg_dxs[reg_dxs != ""]) # for back compatility old fesom post
reg_dysf <- reg_dys
reg_dysf[reg_dys != ""] <- paste0("_dy", reg_dys[reg_dys != ""])

if (!exists("types")) types <- rep("l", t=nsettings) # default: lines plots and not points
if (!exists("ltys")) ltys <- rep(1, t=nsettings)
if (!exists("lwds")) lwds <- rep(1, t=nsettings)
if (!exists("pchs")) {
    pchs <- rep(NA, t=nsettings)
    for (i in seq_along(pchs)) {
        if (types[i] == "l") {
            pchs[i] <- NA
        } else {
            pchs[i] <- pchs_hollow[1]
            #pchs[i] <- pchs_filled_wout_border[1]
        }
    }
}
if (!exists("scatterpchs")) {
    scatterpchs <- rep(pchs_hollow[1], t=nsettings)
    #scatterpchs <- rep(pchs_filled_wout_border[1], t=nsettings)
}
if (!exists("lecex")) {
    lecex <- 1
} else {
    if (!is.finite(lecex)) stop("provided `lecex` = ", lecex, " is not finite")
}

if (!exists("cols")) {
    cols <- mycols(nsettings)
} else if (exists("cols")) {
    if (length(cols) != nsettings) {
        stop(length(cols), " provided `cols`: ", paste(cols, collapse=", "), 
             " must be of length `nsettings` = ", nsettings)
    }
    if (is.numeric(cols)) {
        if (!is.integer(cols)) { 
            msg <- paste0("provided `cols` = ", paste(cols, collapse=", "), 
                          " are \"numeric\" but not \"integer\". convert to integer ... ")
            cols <- as.integer(cols)
            message(msg, paste(cols, collapse=", ")); rm(msg)
        }
        cols <- mycols(max(cols))[cols]
    } else if (is.character(cols)) {
        # check if mixture of color names and inds is provided
        tmp <- suppressWarnings(as.integer(cols))
        if (any(!is.na(tmp))) {
            inds <- which(!is.na(tmp))
            tmp <- mycols(max(as.integer(cols[inds])))
            cols[inds] <- tmp[as.integer(cols[inds])]
            rm(tmp)
        }
    } else {
        stop("provided `cols` = ", paste(cols, collapse=", "), 
             " must be of type \"numeric\", \"integer\" or \"character\"") 
    }
}
cols_rgb <- rgb(t(col2rgb(cols)/255), alpha=alpha_rgb)
if (F) {
    message("\nuse transparent cols ...")
    cols_save <- cols
    cols <- cols_rgb
} 
if (!exists("text_cols")) text_cols <- rep("black", t=nsettings)
if (exists("cols_samedims")) {
    if (is.integer(cols_samedims)) { # provided color numbers; use mycols
        cols_samedims <- mycols(length(cols_samedims))[cols_samedims]
    }
}

plotname_suffix <- "" # default: nothing
if (center_ts) plotname_suffix <- "_center_ts"
if (scale_ts) plotname_suffix <- "_scale_ts"
if (!ts_highlight_seasons$bool) ts_highlight_seasons$suffix <- ""

if (!exists("add_linear_trend_froms")) add_linear_trend_froms <- rep(NA, t=nsettings)
if (!exists("add_linear_trend_tos")) add_linear_trend_tos <- rep(NA, t=nsettings)

# repeat things if necessary
if (length(echam6_global_setNA) != nsettings) {
    if (length(echam6_global_setNA) == 1) {
        message("`echam6_global_setNA` is only of length 1 but nsettings = ", nsettings, 
                " --> repeat echam6_global_setNA[1] = ", echam6_global_setNA, " ...")
        echam6_global_setNA <- rep(echam6_global_setNA, t=nsettings)
    } else {
        stop("`echam6_global_setNA` is of length ", length(echam6_global_setNA), 
             " but nsettings = ", nsettings, ". dont know how to proceed")
    }
}
if (length(add_scatter_density) != nsettings) {
    if (length(add_scatter_density) == 1) {
        message("given `add_scatter_density` but of length ", length(add_scatter_density), 
                " and nsettings = ", nsettings, " --> repeat ", nsettings, " times ...") 
        add_scatter_density <- rep(add_scatter_density, t=nsettings)
    } else {
        if (length(add_scatter_density) != nsettings) {
            stop("given `add_scatter_density` but of length ", length(add_scatter_density), 
                " and nsettings = ", nsettings, " --> dont know how to interpret this")
        }
    }
}
if (length(add_linear_trend) != nsettings) {
    if (length(add_linear_trend) == 1) {
        message("given `add_linear_trend` but of length ", length(add_linear_trend), 
                " and nsettings = ", nsettings, " --> repeat ", nsettings, " times ...") 
        add_linear_trend <- rep(add_linear_trend, t=nsettings)
    } else {
        if (length(add_linear_trend) != nsettings) {
            stop("given `add_linear_trend` but of length ", length(add_linear_trend), 
                " and nsettings = ", nsettings, " --> dont know how to interpret this")
        }
    }
}

# do this here and not in namelist since user could change in namelist from namelist.general
if (p$plot_type == "pdf") {
    minus_symbol_dash <- "-"
} else {
    minus_symbol_dash <- "\u2013"
}

# clear lastfiles_plot file
lastfiles_plot_fname <- paste0("lastfiles_plot_", Sys.getpid(), ".txt") # in same path as this plot_echam.r-call
lastfiles_plot_fname <- paste0(normalizePath(dirname(lastfiles_plot_fname)), "/", lastfiles_plot_fname)
message("\ncreate file for storing plotnames \"", lastfiles_plot_fname, "\" ...")
invisible(file.create(lastfiles_plot_fname)) # error if no success

# allocate
datas <- vector("list", l=nsettings)
names(datas) <- names_short
data_infos <- dims <- dims_of_settings <- ll_data <- poly_data <- datas


# load pangaea data if defined 
if (load_pangaea_data) {
    message("\ndisable here if you do not want to load pangaea data via load_pangaea_data.r ...")
    source("load_pangaea_data.r")
} else {
    message("\nenable here to load pangaea data via load_pangaea_data.r ...")
}

# load special data if defined
if (load_special_data) {
    message("\ndisable here if you do not want to load special data via load_special_data.r ...")
    source("load_special_data.r")
} else {
    message("\nenable here to load special data via load_special_data.r ...")
}

# plot special data if defined
if (plot_special_data) {
    message("\ndisable here if you do not want to plot special data via plot_special_data.r ...")
    source("plot_special_data.r")
} else {
    message("\nenable here to plot special data via plot_special_data.r ...")
}

# read data
inpaths <- fnames <- rep(NA, t=nsettings)
mtimes <- as.POSIXct(Sys.time()) # placeholder
message("\n===================================\nread model data ...")
for (i in seq_len(nsettings)) {

    message("\n*********************************************")
    message("setting ", i, "/", nsettings, ": ", names_short[i], " ...")
    inpath <- paste0(postpaths[i], "/", models[i], "/", modes[i], "/", varnames_in[i])
    fname <- paste0(prefixes[i], "_", models[i], "_", modes[i], 
                    codesf[i], "_", varnames_in[i], 
                    levsf[i], depthsf[i], "_",
                    areas[i])
    if (seasonsf[i] != "") fname <- paste0(fname, "_", seasonsf[i])
    if (fromsf[i] != "") fname <- paste0(fname, "_", fromsf[i])
    if (tosf[i] != "") fname <- paste0(fname, "-", tosf[i])
    fname <- paste0(fname, 
                    reg_dxsf[i], reg_dysf[i],
                    ".nc")
    inpaths[i] <- inpath; fnames[i] <- fname
    mtimes[i] <- file.info(paste0(inpath, "/", fname))$mtime
        
    message("\nopen ", inpath, "/", fname, " ...")
    ncin <- nc_open(paste0(inpath, "/", fname))

    # get dims of file
    message("\nget dims ...")
    dims_of_settings[[i]] <- names(ncin$dim)
    
    # ignore dimnames of file
    if (any(!is.na(match(dims_of_settings[[i]], ignore_dimnames)))) {
        inds <- which(!is.na(match(dims_of_settings[[i]], ignore_dimnames)))
        message("ignore ", length(inds), " dimnames \"", paste(dims_of_settings[[i]][inds], collapse="\", \""), "\"")
        dims_of_settings[[i]][inds] <- NA
    }
    if (any(is.na(dims_of_settings[[i]]))) {
        dims_of_settings[[i]] <- dims_of_settings[[i]][-which(is.na(dims_of_settings[[i]]))]
    }

    # check for unknown dimnames of file
    if (any(is.na(match(dims_of_settings[[i]], unlist(known_dimnames))))) {
        inds <- which(is.na(match(dims_of_settings[[i]], unlist(known_dimnames))))
        stop(length(inds), " dims of file are unknown: \"", 
             paste(dims_of_settings[[i]][inds], collapse="\", \""), "\"\n",
             "add these dimnames to `known_dimnames` in namelist.general.plot.r")
    }

    # map input dimnames to known dimnames to make everything modular 
    for (di in seq_along(known_dimnames)) {
        if (any(!is.na(match(dims_of_settings[[i]], known_dimnames[[di]])))) {
            ind <- which(!is.na(match(dims_of_settings[[i]], known_dimnames[[di]])))
            if (length(ind) != 1) stop("this should not happen")
            message("map input dimname \"", dims_of_settings[[i]][ind], "\" --> \"", names(known_dimnames)[di], "\"")
            # val(obj) --> original dimnames
            # name(obj) --> my dimnames
            names(dims_of_settings[[i]])[ind] <- names(known_dimnames)[di] 
        }
    }
    
    # get dimvals
    dimtmp <- vector("list", l=length(dims_of_settings[[i]]))
    cnt <- 0
    for (di in seq_len(ncin$ndims)) {
        message("input dim ", di, "/", ncin$ndims, ": \"", names(ncin$dim)[di], "\", n=", length(ncin$dim[[di]]$vals))
        if (!any(names(ncin$dim)[di] == dims_of_settings[[i]])) {
            # dim was excluded before due to e.g. `ignore_dimnames`
        } else {
            cnt <- cnt + 1
            dimtmp[[cnt]] <- ncin$dim[[di]]$vals
            ind <- which(dims_of_settings[[i]] == names(ncin$dim)[di])
            if (length(ind) != 1) stop("this should not happen")
            names(dimtmp)[cnt] <- names(dims_of_settings[[i]])[ind]
        }
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
        if (length(dropinds) == length(dims[[i]])) {
            message("dropping ", length(dropinds), 
                    " dims of length 1 would mean to drop all dims of the data")
            stop("rethink")
            if (any(names(dims[[i]]) == "time")) {
                message("--> keep time dim")
                ind <- which(names(dims[[i]]) == "time")
                dropinds <- dropinds[-ind]
            } else {
                stop("not defined")
            }
        }
        dims[[i]][dropinds] <- NULL
    }


    ## time dim stuff
    # time dim as posix object
    if (any(names(dims[[i]]) == "time")) {

        message("\ndetected time dim of length ", length(dims[[i]]$time), "; dims[[", i, "]]$time:")
        cat(capture.output(str(ncin$dim[[dims_of_settings[[i]]["time"]]])), sep="\n")
        ht(dims[[i]]$time)
        
        if (prefixes[i] == "Hol-T_stschuett_echam5_wiso") {
            message("\nspecial: rev steffens time ...")
            dims[[i]]$time <- rev(dims[[i]]$time)
            ht(dims[[i]]$time, n=20)
        }
        
        # convert any unit to seconds for POSIX
        timein_calendar <- timein_leap <- NULL
        if (!is.null(ncin$dim[[dims_of_settings[[i]]["time"]]]$calendar)) {
            timein_calendar <- ncin$dim[[dims_of_settings[[i]]["time"]]]$calendar
            if (any(timein_calendar == c("365_day", "proleptic_gregorian", "gregorian"))) {
                timein_leap <- F
            } else if (any(timein_calendar == c("366_day", "standard"))) {
                timein_leap <- T
            } else {
                stop("time dim `calendar` attribute \"", timein_calendar, "\" unknown")
            }
        }
        timein_units <- ncin$dim[[dims_of_settings[[i]]["time"]]]$units
        if (timein_units == "") { # my phd stuff
            if (is.null(ncin$var[[dims_of_settings[[i]]["time"]]]$units)) stop("not defined")
            timein_units <- ncin$var[[dims_of_settings[[i]]["time"]]]$units
        }
        if (all(dims[[i]]$time == 0)) { # my phd stuff
            message("make my phd special monthly time from `fromsf[i]` = ", fromsf[i], " to `tosf[i]` = ", tosf[i])
            timein_units <- "months"
            if (length(dims[[i]]$time) == 743) { # dt
                dims[[i]]$time <- rep(fromsf[i]:tosf[i], e=12) + rep((0:11)/12, t=length(fromsf[i]:tosf[i]))
                dims[[i]]$time <- dims[[i]]$time[2:744]
            } else if (length(dims[[i]]$time) == 744) {
                dims[[i]]$time <- rep(fromsf[i]:tosf[i], e=12) + rep((0:11)/12, t=length(fromsf[i]:tosf[i]))
            } else {
                stop("time length ", length(dims[[i]]$time), " not defined here")
            }
            if (grepl("Arc22_sub_daily", prefixes[i])) { # every (also leap) year has ntime=365; dt=86400 sec 
                message("days!!!")
                timein_units <- "days"
                dims[[i]]$time <- rep(fromsf[i]:tosf[i], e=365) + rep((0:364)/365, t=length(fromsf[i]:tosf[i]))
            }
        }
        message("\n--> make POSIX object from timein_units = \"", timein_units, "\" ...")

        # 3 different time units so far:
        # case 1: e.g. "days since 1538-1-1 00:00:00"  
        if (regexpr(" since ", timein_units) != -1) {
            timein_unit <- substr(timein_units, 1, regexpr(" since ", timein_units) - 1)
            timein_origin <- substr(timein_units, regexpr(" since ", timein_units) + 7, nchar(timein_units))
            timein_fac <- NULL
            if (any(timein_unit == c("second", "seconds"))) {
                timein_fac <- 1
            } else if (grepl("day", timein_unit)) {
                timein_fac <- 86400
            } else if (grepl("hour", timein_unit)) {
                timein_fac <- 60*60
            } else if (grepl("month", timein_unit)) {
                if (all(diff(dims[[i]]$time) == 1)) {
                    timein_lt <- as.POSIXlt(seq(as.POSIXct(timein_origin, tz="UTC"), 
                                                by=paste0(diff(dims[[i]]$time)[1], " months"),
                                                l=length(dims[[i]]$time)))
                } else {
                    stop("time dim is given in months with uneven dt. who is doing such thing o_O")
                }
            } else {
                stop("timein_unit \"", timein_unit, "\" obtained from time units \"", 
                     timein_units, "\" is unknown")
            }
            if (!is.null(timein_fac)) {
                if (!is.null(timein_leap) && !timein_leap) {
                    origin_year <- as.integer(substr(timein_origin, 1, 4))
                    message("detected relative time unit AND non-leap calendar --> add number of feb29 from ",
                            "`origin year` = ", origin_year, " to `fromsf[", i, "] = ", fromsf[i])
                    nfeb29 <- length(which(is.leap(origin_year:fromsf[i])))
                    message("--> add ", nfeb29, " feb29 in time unit \"", timein_unit, "\" to time vals ...")
                    timein_lt <- as.POSIXlt(dims[[i]]$time*timein_fac+nfeb29*timein_fac, origin=timein_origin, tz="UTC")
                } else {
                    timein_lt <- as.POSIXlt(dims[[i]]$time*timein_fac, origin=timein_origin, tz="UTC")
                }
            }

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

        # case 3: # my phd stuff
        } else if (any(timein_units == c("days", "months"))) {
            message("\nspecial: my old phd stuff")
            timein_lt <- make_posixlt_origin(dims[[i]]$time)
        
        } else {
            stop("`timein_units` = \"", timein_units, "\" not defined")
        
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
            message("by `shift_by` = `new_origins[", i, "]` - `years[1]` = ", 
                    new_origins[i], " - ", timein_lt$year[1] + 1900, #" - 1 ", 
                    " = ", shift_by, " years") 
            if (any(is.na(timein_lt))) stop("some NA in timein_lt before shift_by")
            timein_lt$year <- timein_lt$year + shift_by
            # maybe necessary:
            #timein_lt$year[] <- timein_lt$year[] + shift_by
            if (any(is.na(timein_lt))) stop("some NA in timein_lt after shift_by")
            message("new timein_lt:")
            ht(timein_lt, n=20)
            message("--> new range(timein_lt) = ", appendLF=F)
            print(range(timein_lt))
            dims[[i]]$time_shift_by <- shift_by
        } # if !is.na(new_origins[i])
        # finished set new time origin

        # find time inds if wanted (`fromsp` is defined and different from `fromsf`)
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
                    message("found ", length(time_inds), " temporal subset inds out of ", 
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

        } else { # `fromsp` and `tosp` not provided
            fromsp[i] <- timein_lt$year[1] + 1900
            message("\nfromsp not given --> use first year --> fromsp[", i, "] = ", fromsp[i])
            tosp[i] <- timein_lt$year[length(timein_lt)] + 1900
            message("\ntosp not given --> use last year --> tosp[", i, "] = ", tosp[i])
            time_inds <- NULL # default
            
        } # if exists("fromsp") || exists("tosp")
        # finished find time inds if wanted

        # find seasons inds if wanted (`seasonsp` is defined and different from `seasonsf`)
        if (seasonsp[i] != seasonsf[i]) {
            
            if (!is.character(seasonsp[i])) {
                stop("provided seasonsp[", i, "] is not of type character")
            }

            # special case:  
            if (seasonsp[i] == "annual" && seasonsf[i] == "Jan-Dec") {
                # nothing do to with `datas`; `datasan` will be calculated

            # all other season cases:
            } else { # seasonsp != "annual" && seasonsf != "Jan-Dec"

                message("\nprovided `seasonsp[", i, "]` = \"", seasonsp[i], "\" != `seasonsf[", i, 
                        "]` = \"", seasonsf[i], "\"\n",
                        "--> check `known_seasons` for this string ...")
                season_ind <- which(names(known_seasons) == seasonsp[i])
                if (length(season_ind) == 0) {
                    stop("this season is not in `known_seasons`")
                } else if (length(season_ind) > 1) {
                    stop("found ", length(season_ind), " entries in `known_seasons` with this name")
                }

                season_inds <- known_seasons[[season_ind]]$inds
                dims[[i]]$season_inds <- season_inds
                message("--> found ", length(season_inds), " season_inds: ", paste(season_inds, collapse=", "))
                months_in <- timein_lt$mon + 1L
                month_inds <- months_in %in% season_inds
                month_inds <- which(month_inds)
                message("--> found ", length(month_inds), " month_inds:")
                ht(month_inds)
                timein_lt <- timein_lt[month_inds]
                if (!is.null(time_inds)) { # time_inds already found based on provided `fromsp` and/or `tosp`
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
    
    # find YYYY from and to for plotnames if time dim was dropped
    if (is.na(fromsp[i])) {
        if (is.na(new_origins[i])) {
            fromsp[i] <- fromsf[i] # same as file input
            message("\nfromsp not given --> fromsf[", i, "] = ", fromsf[i], 
                    " --> fromsp[", i, "] = ", fromsp[i])
        } else if (!is.na(new_origins[i])) { # new origin is wanted
            shift_by <- new_origins[i] - as.integer(fromsf[i]) # - 1
            fromsp[i] <- as.integer(fromsf[i]) + shift_by
            message("\nfromsp not given --> fromsf[", i, "] = ", fromsf[i], 
                    ", shift_by = ", shift_by, " --> fromsp[", i, "] = ", fromsp[i])
        }
    }
    if (is.na(tosp[i])) {
        if (is.na(new_origins[i])) {
            tosp[i] <- tosf[i] # same as file input
            message("\ntosp not given --> tosf[", i, "] = ", tosf[i], 
                    " --> tosp[", i, "] = ", tosp[i])
        } else if (!is.na(new_origins[i])) { # new origin is wanted
            shift_by <- new_origins[i] - as.integer(fromsf[i]) # - 1
            tosp[i] <- as.integer(tosf[i]) + shift_by
            message("\ntosp not given --> tosf[", i, "] = ", tosf[i], 
                    ", shift_by = ", shift_by, " --> tosp[", i, "] = ", tosp[i])
        }
    }
    froms_plot[i] <- fromsp[i]
    tos_plot[i] <- tosp[i]

    if (add_linear_trend[i]) {
        if (is.na(add_linear_trend_froms[i])) add_linear_trend_froms[i] <- fromsp[i]
        if (is.na(add_linear_trend_tos[i])) add_linear_trend_tos[i] <- tosp[i]
    }
    # finfished time dim stuff


    # depth dim stuff 
    # use km instead of m as depth unit
    if (any(names(dims[[i]]) == "depth")) {
        dims[[i]]$depthunit <- "Depth [m]"
        if (F) {
            message("\ndivide depth dim by 1000 m --> km")
            dims[[i]]$depth <- dims[[i]]$depth/1000
            dims[[i]]$depthunit <- "Depth [km]"
        }
    }
    # finished depth dim stuff


    # lev dim stuff 
    # use km instead of m as lev unit
    if (any(names(dims[[i]]) == "lev")) {
        dims[[i]]$levunit <- "Level"
    }
    # finished lev dim stuff
    
    
    ## lon and lat dim stuff
    # load additional modeled lon,lat data (ll_data) as matrix if wanted
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
        
        } else if (T && grepl("Low01", prefixes[i])) {
            ll_vars <- c("bathy", "mixlay", "u", "v", "eke", "sic") # multiple possible
            ll_fnames <- paste0(postpaths[i], 
                                c("/fesom/timmean/bathy/Low01_s52_regular_dx0.100_dy0.100_fesom_timmean_bathy_global.nc",
                                  "/fesom/timmean/mixlay/Low01_sub_lsea_s52_regular_dx0.100_dy0.100_fesom_timmean_mixlay_lsea_Mar_1948-2009.nc",
                                  "/fesom/timmean/hvel/Low01_sub_lsea_s52_regular_dx0.100_dy0.100_fesom_timmean_hvel_0m_lsea_Jan-Dec_1948-2009.nc",
                                  "/fesom/timmean/hvel/Low01_sub_lsea_s52_regular_dx0.100_dy0.100_fesom_timmean_hvel_0m_lsea_Jan-Dec_1948-2009.nc",
                                  #"/fesom/timmean/hvel/Low01_sub_lsea_s52_regular_dx0.100_dy0.100_fesom_timmean_hvel_0m_lsea_Mar_1948-2009.nc",
                                  #"/fesom/timmean/hvel/Low01_sub_lsea_s52_regular_dx0.100_dy0.100_fesom_timmean_hvel_0m_lsea_Mar_1948-2009.nc",
                                  "/fesom/timmean/eke/Low01_sub_lsea_s52_regular_dx0.100_dy0.100_fesom_timmean_eke_0m_lsea_Jan-Dec_1948-2009.nc",
                                  #"/fesom/timmean/eke/Low01_sub_lsea_s52_regular_dx0.100_dy0.100_fesom_timmean_eke_0m_lsea_Mar_1948-2009.nc",
                                  #"/fesom/timmean/eke/Low01_sub_lsea_s52_regular_dx0.100_dy0.100_fesom_timmean_eke_int0-3600m_lsea_Mar_1948-2009.nc",
                                  "/fesom/timmean/sic/Low01_sub_lsea_s52_regular_dx0.100_dy0.100_fesom_timmean_sic_lsea_Mar_1948-2009.nc"))
        } else if (T && grepl("LSea5", prefixes[i])) {
            ll_vars <- c("bathy", "mixlay", "u", "v", "eke", "sic") # multiple possible
            ll_fnames <- paste0(postpaths[i], 
                                c("/fesom/timmean/bathy/LSea5_s5_regular_dx0.100_dy0.100_fesom_timmean_bathy_global.nc",
                                  "/fesom/timmean/mixlay/LSea5_sub_lsea_s5_regular_dx0.100_dy0.100_fesom_timmean_mixlay_lsea_Mar_1948-2009.nc",
                                  "/fesom/timmean/hvel/LSea5_sub_lsea_s5_regular_dx0.100_dy0.100_fesom_timmean_hvel_0m_lsea_Jan-Dec_1948-2009.nc",
                                  "/fesom/timmean/hvel/LSea5_sub_lsea_s5_regular_dx0.100_dy0.100_fesom_timmean_hvel_0m_lsea_Jan-Dec_1948-2009.nc",
                                  #"/fesom/timmean/hvel/LSea5_sub_lsea_s5_regular_dx0.100_dy0.100_fesom_timmean_hvel_0m_lsea_Mar_1948-2009.nc",
                                  #"/fesom/timmean/hvel/LSea5_sub_lsea_s5_regular_dx0.100_dy0.100_fesom_timmean_hvel_0m_lsea_Mar_1948-2009.nc",
                                  "/fesom/timmean/eke/LSea5_sub_lsea_s5_regular_dx0.100_dy0.100_fesom_timmean_eke_0m_lsea_Jan-Dec_1948-2009.nc",
                                  #"/fesom/timmean/eke/LSea5_sub_lsea_s5_regular_dx0.100_dy0.100_fesom_timmean_eke_0m_lsea_Mar_1948-2009.nc",
                                  #"/fesom/timmean/eke/LSea5_sub_lsea_s5_regular_dx0.100_dy0.100_fesom_timmean_eke_int0-4150m_lsea_Mar_1948-2009.nc",
                                  "/fesom/timmean/sic/LSea5_sub_lsea_s5_regular_dx0.100_dy0.100_fesom_timmean_sic_lsea_Mar_1948-2009.nc"))
        } # which setting and variable
        if (length(ll_vars) != length(ll_fnames)) stop("`ll_vars` and `ll_fnames` must be of same length")
        if (ll_fnames != "" && ll_vars != "") {
            message("\ndata has lon and lat dims and `ll_fnames` with additional modeled lon,lat data matrix is defined ...")
        }
        ll_ncs <- ll_data_per_setting <- list()
        ll_cnt <- 0
        for (li in seq_along(ll_fnames)) {
            if (file.exists(ll_fnames[li]) && ll_vars[li] != "") {
                message("   open `ll_fnames[", li, "]` = \"", ll_fnames[li], "\" ...")
                ll_nc <- nc_open(ll_fnames[li])
                if (!any(names(ll_nc$var) == ll_vars[li])) {
                    warning("variable `ll_vars[", li, "]` = \"", ll_vars[li], "\" is not in `ll_fnames[", li, 
                            "]` = \"", ll_fnames[li], "\".")
                } else { # wanted var is in file
                    ll_dims_of_var <- sapply(ll_nc$var[[ll_vars[li]]]$dim, "[", "name")
                    lonname <- "lon"; latname <- "lat" # defaults
                    if (any(sapply(ll_dims_of_var, "[") == "nxi")) { # old rfesom
                        lonname <- "nxi"
                        #ind <- which(sapply(ll_dims_of_var, "[") == "nxi")
                    }
                    if (any(sapply(ll_dims_of_var, "[") == "nyi")) { # old rfesom
                        latname <- "nyi"
                        #ind <- which(sapply(ll_dims_of_var, "[") == "nyi")
                        #ll_dims_of_var[[ind]] <- "lat"
                    }
                    if (!any(unlist(lapply(ll_dims_of_var, function(x) any(x == lonname)))) &&
                        !any(unlist(lapply(ll_dims_of_var, function(x) any(x == latname))))) {
                        stop("variable `ll_vars[", li, "]` = \"", ll_vars[li], "\" from `ll_fnames[", li, 
                                "]` = \"", ll_fnames[li], "\" does not have \"", lonname, "\" and \"", latname, "\" dims.")
                    } else { # wanted var has lon,lat dims
                        ll_cnt <- ll_cnt + 1
                        ll_ncs[[ll_cnt]] <- ll_nc
                        ll_data_per_setting[[ll_cnt]] <- list(file=ll_fnames[li],
                                                              lon=ll_nc$dim[[lonname]]$vals, lat=ll_nc$dim[[latname]]$vals)
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

    # get lev inds
    if (any(names(dims[[i]]) == "lev")) {
        lev_fromsf[i] <- min(dims[[i]]$lev)
        lev_tosf[i] <- max(dims[[i]]$lev)
        if (is.na(lev_fromsp[i])) lev_fromsp[i] <- lev_fromsf[i]
        if (is.na(lev_tosp[i])) lev_tosp[i] <- lev_tosf[i]
        message("\nfind lev subsets from lev_fromsp[", i, "]=", 
                lev_fromsp[i], " to lev_tosp[", i, "]=", lev_tosp[i], " ...")
        # find lev subset based on given lev_fromsp lev_tosp
        lev_inds <- which(dims[[i]]$lev >= lev_fromsp[i] & dims[[i]]$lev <= lev_tosp[i])
        # take lev subset
        if (length(lev_inds) > 0 && length(lev_inds) != length(dims[[i]]$lev)) { 
            message("found lev subset of length ", length(lev_inds), " out of ", 
                    length(dims[[i]]$lev), " total lev points ...")
            message("before range(dims[[i]]$lev) = ", appendLF=F)
            print(range(dims[[i]]$lev))
            dims[[i]]$lev <- dims[[i]]$lev[lev_inds]
            message("after range(dims[[i]]$lev) = ", appendLF=F)
            print(range(dims[[i]]$lev))
            dims[[i]]$lev_inds <- lev_inds
        } else {
            if (length(lev_inds) == 0) {
                stop("lev subset is of length 0")
            }
        }
    } # if any of file dims is "lev"
    
    # get vars of file
    message("\nget variables ...")
    vars_per_file <- names(ncin$var)
    vars <- vector("list", l=ncin$nvars)
    var_infos <- vars
    for (vi in seq_along(vars)) {
        message(vi, "/", length(vars), ": \"", vars_per_file[vi], "\"", appendLF=F)
        
        # ignore variable based on `ignore_vars`?
        ignore <- F # default: dont ignore
        for (igni in seq_along(ignore_vars)) { # loop necessary to use wildcards
            if (grepl(glob2rx(ignore_vars[igni]), vars_per_file[vi])) {
                message(" --> this variable is included in `ignore_vars` (\"", 
                        ignore_vars[igni], "\") --> ignore this variable ...")
                ignore <- T
                next # ignore_var to test 
            }
        }
        if (ignore) next # variable of this setting
        message()
        
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

        # get dims of variable
        dimnames <- sapply(ncin$var[[vars_per_file[vi]]]$dim, "[[", "name")
        dimlengths <- ncin$var[[vars_per_file[vi]]]$size
        if (squeeze && any(dimlengths == 1)) { # drop dims with len=1
            len1_dim_inds <- which(dimlengths == 1)
            message("\n`squeeze=T` --> drop ", length(len1_dim_inds), " dims of length 1: \"", 
                    paste(dimnames[len1_dim_inds], collapse="\", \""), "\" ...")
            if (length(len1_dim_inds) == length(dimlengths)) {
                message("dropping ", length(len1_dim_inds), 
                        " dims of length 1 would remove all dims of the data")
                if (any(names(dims_of_settings[[i]]) == "time") && # current setting has a time dim and
                    any(dimnames == dims_of_settings[[i]]["time"])) { # current variable has time dim
                        message("--> keep time dim \"", dims_of_settings[[i]]["time"], "\"")
                        ind <- which(dimnames == dims_of_settings[[i]]["time"])
                        len1_dim_inds <- len1_dim_inds[-ind]
                } else {
                    stop("not defined")
                }
            } # if removing len-1 dims would drop all dims
            dimlengths <- dimlengths[-len1_dim_inds]
            dimnames <- dimnames[-len1_dim_inds]
        } # if squeeze

        # use my dimnames, not original dimnames
        dimnames <- match(dimnames, dims_of_settings[[i]])
        if (any(is.na(dimnames))) stop("this should not happen")
        dimnames <- names(dims_of_settings[[i]][dimnames]) # e.g. "TIME" --> "time"
        
        # set proper attributes to data object
        attributes(vars[[vi]]) <- list(dim=dimlengths, dims=dimnames)
    
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
        message("\ndata has lon and lat dims and  `ll_data` is defined --> load ll_data variables ...")
        for (li in seq_along(ll_data[[i]])) {
            message("   load `ll_vars[", li, "]` = \"", ll_vars[li], "\" ...")
            ll_data[[i]][[li]][[length(ll_data[[i]][[li]])+1]] <- ncdf4::ncvar_get(ll_ncs[[li]], ll_vars[li], 
                                                                                   collapse_degen=squeeze)
            names(ll_data[[i]][[li]])[length(ll_data[[i]][[li]])] <- ll_vars[li]

            # do special stuff
            if (ll_vars[li] == "eke") {
                atts <- ncdf4::ncatt_get(ll_ncs[[li]], ll_vars[[li]])
                message("eke units = ", atts$units)
                if (atts$units == "m2 s-2") {
                    message("ll_data ", li, " eke m2 s-2 --> cm2 -s2")
                    ll_data[[i]][[li]][[length(ll_data[[i]][[li]])]] <- ll_data[[i]][[li]][[length(ll_data[[i]][[li]])]]*1e4
                }
            }
    
            # get dims of variable
            ll_dims_of_settings <- names(ll_ncs[[li]]$dim)
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
                                                                  dims=ll_dims_of_settings[dimids])
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

    ## do stuff along time dim of data
    # first, apply seasonal mean (i.e. reduce data along time dim) based on input `seasonsf` and, if provided, `seasonsp`
    # e.g. seasonsf == Jan-Dec seasonsf input and provided seasonsp == NDJFM
    # and second, cut temporal subset from data if wanted 
    if (!is.null(dims[[i]]$time_inds)) {
        message("\napply seasonal mean or cut subset from time dim ...")
        # check for variables that have time dim
        vars_with_timedim_inds <- lapply(dims_per_setting, function(x) grep("time", x) != -1)
        vars_with_timedim_inds <- which(sapply(vars_with_timedim_inds, any))
        if (length(vars_with_timedim_inds) > 0) {
            for (vi in seq_along(vars_with_timedim_inds)) {
                var_with_timedim_ind <- vars_with_timedim_inds[vi]
                dims_of_var <- attributes(datas[[i]][[var_with_timedim_ind]])$dims # e.g. "time", "lon", "lat"
                timedimind <- which(dims_of_var == "time")
                dim_lengths_of_var <- dim(datas[[i]][[var_with_timedim_ind]])
                time_dim_length <- dim_lengths_of_var[timedimind]
    
                # task 1/2: apply seasonal mean: input seasonsf has not but seasonsp has "mean" in it
                # e.g. seasonsf == Jan-Dec seasonsf input and provided seasonsp == NDJFM
                if (!grepl("mean", seasonsf[i]) && grepl("mean", seasonsp[i])) {

                    season_inds_first_part <- season_inds_second_part <- NULL
                    if (any(diff(dims[[i]]$season_inds) < 0)) { # e.g. DJF: 12, 1, 2 or NDJFM: 11, 12, 1, 2, 3
                        if (length(which(diff(dims[[i]]$season_inds) < 0)) != 1) {
                            stop("season_inds ", paste(dims[[i]]$season_inds, collapse=","), ") have ", 
                                 length(which(diff(dims[[i]]$season_inds) < 0)), 
                                 " positions where their diff is < 0. must be one.")
                        }
                        season_inds_first_part <- dims[[i]]$season_inds[1:(which(diff(dims[[i]]$season_inds) < 0))] # e.g. 1,2 of 12,1,2
                        season_inds_second_part <- dims[[i]]$season_inds[(which(diff(dims[[i]]$season_inds) < 0) + 1):length(dims[[i]]$season_inds)] # e.g. 12 of 12,1,2
                    } else { # no gaps in season inds
                        season_inds_first_part <- dims[[i]]$season_inds # only one part necessary
                    }
                    if (length(season_inds_first_part) == 0) stop("this should not happen")
                    if (!is.null(season_inds_second_part) && length(season_inds_second_part) == 0) stop("this should not happen")
                    
                    years <- unique(dims[[i]]$timelt$year + 1900L)
                    dim_lengths_of_seasonally_averaged_var <- dim_lengths_of_var
                    dim_lengths_of_seasonally_averaged_var[timedimind] <- length(years)
                    data2 <- array(NA, dim=dim_lengths_of_seasonally_averaged_var)
                    time2 <- rep(dims[[i]]$time[1], t=length(years))
                    counts <- rep(NA, t=length(years))
                    
                    message("calc ", length(years), " annual means over ", length(season_inds_first_part), 
                            " months ", appendLF=F)
                    if (is.null(season_inds_second_part)) {
                        message(paste(season_inds_first_part, collapse=","), " of year i", appendLF=F)
                    } else if (!is.null(season_inds_second_part)) {
                        message(paste(season_inds_first_part, collapse=","), " of year i-1 and ", 
                                length(season_inds_second_part), " months ",
                                paste(season_inds_second_part, collapse=","), " of year i", appendLF=F)
                    }
                    if (length(dims_of_var) > 1) {
                        message(" while keeping ", length(dims_of_var) - 1, " ",
                            paste(dims_of_var[-timedimind], collapse=", "), " dims ...")
                    } else {
                        message(" ...")
                    }
                    for (yi in seq_along(years)) {
                        inds_yi <- which(dims[[i]]$timelt$year + 1900L == years[yi])
                        months_yi <- dims[[i]]$timelt$mon[inds_yi] + 1L
                        if (!is.null(season_inds_second_part)) { # if there are gaps in season inds
                            if (yi == 1) { # first year: e.g. months 1,2,3 of 11,12,1,2,3
                                if (all(is.na(match(season_inds_second_part, months_yi)))) {
                                    stop("did not find any month of the wanted ones")
                                }
                                inds <- inds_yi[match(season_inds_second_part, months_yi)]
                            } else { # all other years: months 11,12 of yeari i-1 and months 1,2,3 of year i
                                inds_yi_minus1 <- which(dims[[i]]$timelt$year + 1900L == years[yi]-1)
                                if (length(inds_yi_minus1) == 0) { # found no year i-1 in time values; cannot apply wanted seasonal mean
                                    stop("did not find year i-1 = ", years[yi]-1, " although the current year ", 
                                         years[yi], " is year ", i, " --> cannot use season inds ", 
                                         paste(season_inds_first_part, collapse=","), " of year i-1.")
                                } else {
                                    months_yi_minus1 <- dims[[i]]$timelt$mon[inds_yi_minus1] + 1L
                                    if (all(is.na(match(season_inds_first_part, months_yi_minus1)))) {
                                        stop("did not find any month of the wanted ones")
                                    }
                                    if (all(is.na(match(season_inds_second_part, months_yi)))) {
                                        stop("did not find any month of the wanted ones")
                                    }
                                    inds <- c(inds_yi_minus1[match(season_inds_first_part, months_yi_minus1)],
                                              inds_yi[match(season_inds_second_part, months_yi)])
                                }
                            }
                        } else { # no gaps in season inds
                            if (all(is.na(match(season_inds_first_part, months_yi)))) {
                                stop("did not find any month of the wanted ones")
                            }
                            stop("check if this is correct")
                            inds <- inds_yi[season_inds_first_part]
                        }
                        if (any(is.null(inds)) || any(is.null(inds))) stop("this should not happen")
                        if (F) message("mean over n=", length(inds), ": ", paste(inds, collapse=","))
                        if (F) message("mean over n=", length(inds), ": ", paste(dims[[i]]$timelt[inds], collapse=","))

                        # apply seasonal average
                        if (T) { # `apply` slow
                            cmdl <- rep(",", t=length(dims_of_var))
                            cmdl[timedimind] <- yi
                            cmdl <- paste0("data2[", paste(cmdl, collapse=""), "]")
                            cmdr1 <- rep(",", t=length(dims_of_var))
                            cmdr1[timedimind] <- paste0("c(", paste(inds, collapse=","), ")")
                            cmdr1 <- paste(cmdr1, collapse="")
                            cmdr2 <- seq_along(dims_of_var)
                            cmdr2 <- cmdr2[-timedimind]
                            cmdr <- paste0("apply(datas[[", i, "]][[", var_with_timedim_ind, "]][", cmdr1, "], ",
                                           "c(", paste(cmdr2, collapse=", "), "), mean, na.rm=T)")
                            cmd <- paste0(cmdl, " <- ", cmdr)
                            if (F) message("   run `", cmd, "` ...")
                            eval(parse(text=cmd))
                        
                        # sum over season inds
                        } else if (F) {
                            counts[yi] <- length(inds)
                            stop("continue")
                        }
                        time2[yi] <- mean(dims[[i]]$time[inds]) # new time is average over seasons
                    } # for yi
                    
                    # overwrite original data with seasonally averaged one
                    datas[[i]][[var_with_timedim_ind]] <- data2
                    attributes(datas[[i]][[var_with_timedim_ind]]) <- list(dim=dim(datas[[i]][[var_with_timedim_ind]]), 
                                                                           dims=dims_of_var)
                    dims[[i]]$time <- time2
                    dims[[i]]$timelt <- as.POSIXlt(time2)
                    dims[[i]]$timen <- as.numeric(time2)

                    #todo: update time_inds based on new time if necessary
                    message("\ntodo: update time_inds based on new time if necessary")
                    # temporary solution:
                    dims[[i]]$time_inds <- seq_along(time2)
                    
                    rm(data2, time2)

                    # update length of time dim of now seasonally averaged data
                    dim_lengths_of_var <- dim(datas[[i]][[var_with_timedim_ind]])
                    time_dim_length <- dim_lengths_of_var[timedimind]
                
                } # finished apply seasonal mean on data

                # task 2/2: cut subset based on input `seasonsf` and wanted output `seasonsp`
                if (length(dims[[i]]$time_inds) != time_dim_length) { # if time subset is necessary
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
                
                }
            } # for vi vars per file with time dim
            
            # update dims per setting
            #dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")
        
        } # if there are varbels with time dim in current setting
    } # finished cut temporal subset if wanted

    # cut depth subset from data if wanted
    if (!is.null(dims[[i]]$depth_inds)) {
        message("\ncut subset from depth dim ...")
        # check for variables that have depth dim
        vars_with_depthdim_inds <- lapply(dims_per_setting, function(x) grep("depth", x) != -1)
        vars_with_depthdim_inds <- which(sapply(vars_with_depthdim_inds, any))
        if (length(vars_with_depthdim_inds) > 0) {
            for (vi in seq_along(vars_with_depthdim_inds)) {
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
    
    
    # apply missval to lon,lat data if wanted
    if (models[i] == "echam6" && areas[i] == "global") {
        if (!is.na(echam6_global_setNA[i])) {
            if (!any(echam6_global_setNA[i] == c("land", "ocean"))) {
                stop("`echam6_global_setNA[", i, "]` = \"", echam6_global_setNA[i], 
                     "\" must be either NA, \"land\" or \"ocean\"")
            }
            message("\n`echam6_global_setNA[", i, "]` = \"", echam6_global_setNA[i], "\"", appendLF=F)
            if (any(names(dims[[i]]) == "lon") && any(names(dims[[i]]) == "lat")) {
                message(" and current echam6 setting has global variables with lon and lat dims ",
                        "--> apply missval to lon,lat data if possible")
                echam6_lsm_varname <- "slf" # "slm" or "slf"
                if (!any(echam6_lsm_varname == c("slm", "slf"))) stop("`echam6_lsm_varname` must be one of \"slm\" or \"slf\"")
                # slm = 0 --> ocean; slm  = 1 --> land
                # slf = 0 --> ocean; slf != 0 --> land 
                # --> nlandpixel(slf != 0) > nlandpixel(slm == 1)
                # e.g. panama strait T63: slm = 0      --> ocean
                #                         slf = 0.4125 --> land
                message("`echam6_lsm_varname` = \"", echam6_lsm_varname, "\"")
                if (!exists("echam6_lsm_global")) { # open echam6 land sea mask from nc
                    if (length(dims[[i]]$lon) == 192 && length(dims[[i]]$lat) == 96) {
                        message("--> nlon = 192; nlat = 96")
                        echam6_lsm_file <- paste0(host$repopath, "/echam/T63_", echam6_lsm_varname, ".nc")
                    } else if (length(dims[[i]]$lon) == 384 && length(dims[[i]]$lat) == 192) {
                        message("--> nlon = 384; nlat = 192")
                        echam6_lsm_file <- paste0(host$repopath, "/echam/T127_", echam6_lsm_varname, ".nc")
                    } else {
                        stop("case nlon = ", length(dims[[i]]$lon), " and nlat = ", length(dims[[i]]$lat), " not defined")
                    }
                    if (!file.exists(echam6_lsm_file)) {
                        warning("file ", echam6_lsm_file, " not found")
                        echam6_lsm_global <- NULL
                    } else {
                        message("--> read land sea mask variable \"", echam6_lsm_varname, "\" from ", echam6_lsm_file, " ...")
                        echam6_lsm_nc <- nc_open(echam6_lsm_file)
                        echam6_lsm_global <- ncvar_get(echam6_lsm_nc, echam6_lsm_varname)
                    }
                }
                if (!is.null(echam6_lsm_global)) { # echam6 land sea mask was successfully loaded
                    vars_with_londim_inds <- lapply(dims_per_setting, function(x) grep("lon", x) != -1)
                    vars_with_londim_inds <- which(sapply(vars_with_londim_inds, any))
                    vars_with_latdim_inds <- lapply(dims_per_setting, function(x) grep("lat", x) != -1)
                    vars_with_latdim_inds <- which(sapply(vars_with_latdim_inds, any))
                    vars_with_lonandlatdim_inds <- intersect(vars_with_londim_inds, vars_with_latdim_inds)
                    if (length(vars_with_lonandlatdim_inds) > 0) {
                        for (vi in seq_along(vars_with_lonandlatdim_inds)) {
                            var_with_lonandlatdim_ind <- vars_with_lonandlatdim_inds[vi]
                            dims_of_var <- attributes(datas[[i]][[var_with_lonandlatdim_ind]])$dims # e.g. "lon", "lat"
                            if (length(dims_of_var) == 2) { # only 2 dims: lon and lat
                                message("--> echam6_global_setNA[", i, "] = \"", echam6_global_setNA[i], 
                                        "\" --> set ", echam6_lsm_varname, " ", appendLF=F)
                                if (echam6_global_setNA[i] == "land") {
                                    if (echam6_lsm_varname == "slm") {
                                        message("== 1 to NA")
                                        nainds <- echam6_lsm_global == 1
                                    } else if (echam6_lsm_varname == "slf") {
                                        message(">= 0.4 to NA")
                                        nainds <- echam6_lsm_global >= 0.4 # keep panama strait as land
                                    }
                                } else if (echam6_global_setNA[i] == "ocean") {
                                    if (echam6_lsm_varname == "slm") {
                                        message("== 0 to NA")
                                        nainds <- echam6_lsm_global == 0 
                                    } else if (echam6_lsm_varname == "slf") {
                                        message("< 0.4 to NA")
                                        nainds <- echam6_lsm_global < 0.4 # keep panama strait as land
                                    }
                                } else {
                                    stop("`echam6_global_setNA[", i, "]` = \"", echam6_global_setNA[i], "\" not defined")
                                }
                                datas[[i]][[var_with_lonandlatdim_ind]][nainds] <- NA
                            } else {
                                londimind <- which(dims_of_var == "lon")
                                latdimind <- which(dims_of_var == "lat")
                                stop("not implemented yet")
                            }
                        } # for vi
                    } # if no variable with lon and lat dim
                } # if echam6 land sea mask was successfully loaded
            } else { # no lon and lat dims
                message(" but current echam6 setting has no global variables with lon and lat dims ",
                        "--> do not apply missval to lon,lat data")
            } 
        } else { # echam6_global_setNA[i] is NA
            message("\n`echam6_global_setNA[", i, "]` is NA. set to \"ocean\" or \"land\" to set ocean or land values of global echam6 data to missval (NA)")
        }
    } # apply missval if global echam6 data

    
    # reorder lons of data to (-180,...,180) if wanted and necessary
    if (any(names(dims[[i]]) == "lon_orig")) {
        # check for variables that have lon dim
        vars_with_londim_inds <- which(lapply(dims_per_setting, function(x) grep("lon", x)) == 1)
        if (length(vars_with_londim_inds) > 0) {
            message("\nreorder lon dim of data ...")
            if (!any(search() == "package:abind")) library(abind)
            for (vi in seq_along(vars_with_londim_inds)) {
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
    
    # cut area: rectangular lon subset from data if wanted
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
    if (any(names(dims[[i]]) == "time")) {
        vars_with_timedim_inds <- lapply(dims_per_setting, function(x) grep("time", x) != -1)
        vars_with_timedim_inds <- which(sapply(vars_with_timedim_inds, any))
        if (length(vars_with_timedim_inds) > 0) {
            message("\ndetected variables with \"time\" dimension\n",
                    "--> check if ndims=2 and permute (<otherdim> x time) to (time x <otherdim>) if necessary ...") 
            for (vi in seq_along(vars_with_timedim_inds)) {
                var_with_timedim <- vars_with_timedim_inds[vi]
                dims_of_var <- attributes(datas[[i]][[var_with_timedim]])$dims # e.g. "lat" and "time" or "lev" and "time" or "depth" and "time"
                if (length(dims_of_var) == 2) {
                    if (dims_of_var[1] != "time") { # if first dimension (x-dim) is not time
                        message("   aperm(datas[[", i, "]][[", var_with_timedim, "]], c(2, 1)) ...")
                        datas[[i]][[var_with_timedim]] <- aperm(datas[[i]][[var_with_timedim]], c(2, 1)) # permutate
                        attributes(datas[[i]][[var_with_timedim]]) <- list(dim=dim(datas[[i]][[var_with_timedim]]),
                                                                           dims=dims_of_var[c(2, 1)])
                    } else {
                        # time is already x-dim; nothing to do
                    }
                } else {
                    #message("ndim(datas[[", i, "]][[", vi, "]]) = ", length(attributes(datas[[i]][[vi]])$dims), " != 2. skip.")
                }
            }
            # update dims per setting
            #dims_per_setting <- sapply(lapply(datas[[i]], attributes), "[", "dims")
        } # if variables with
    } # finished if two dims and one is time, make it x-dim

} # for i nsettings
message("\n****************** reading model data finished ***************************")

# add most recent mtime of loaded post files to last files list
write(format(max(mtimes), usetz=T), file=lastfiles_plot_fname, append=T)

varnames_unique <- unique(as.vector(unlist(sapply(datas, names))))

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
message("\ndefine mutliplication factors and text for unit labels for specific variables ...")
for (i in seq_len(nsettings)) {
    message("setting ", i, "/", nsettings, ": ", prefixes[i], " ", names_short[i], " ...")
    
    for (vi in seq_along(datas[[i]])) {

        varname <- names(datas[[i]])[vi]
        message("variable ", vi, "/", length(datas[[i]]), ": ", varname, ":")
        cat(capture.output(str(data_infos[[i]])), sep="\n")
        
        # default: dont apply any factor
        data_infos[[i]][[vi]]$offset$operator <- NULL 
        data_infos[[i]][[vi]]$offset$value <- NULL

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
        
            # special: revoke old rfesom multiplication factors
            if (any(sapply(c(" x 10", " x 1e+"), grepl, data_infos[[i]][[vi]]$units))) {
                if (grepl(" x 10", data_infos[[i]][[vi]]$units)) pat <- " x 10"
                if (grepl(" x 1e+", data_infos[[i]][[vi]]$units)) pat <- " x 1e+"
                mult_fac <- substr(data_infos[[i]][[vi]]$units,
                                   regexpr(pat, data_infos[[i]][[vi]]$units) + 3,
                                   nchar(data_infos[[i]][[vi]]$units))
                message("input unit is \"", data_infos[[i]][[vi]]$units, "\"\n",
                        "--> detected old rfesom multiplication factor applied to nc file \"", mult_fac, "\"")
                warn <- options()$warn
                options(warn=2) # stop on warnings
                mult_fac <- as.numeric(mult_fac) # error if no success
                options(warn=warn) # back to default/user setting
                message("--> ", mult_fac)
                data_infos[[i]][[vi]]$offset$operator <- "/"
                data_infos[[i]][[vi]]$offset$value <- mult_fac
                label <- paste0(label, " [", 
                                substr(data_infos[[i]][[vi]]$units,
                                       1, regexpr(pat, data_infos[[i]][[vi]]$units) - 1),
                                "]")
            }
        }
        data_infos[[i]][[vi]]$label <- label
        if (scale_ts) data_infos[[i]][[vi]]$label <- paste0(data_infos[[i]][[vi]]$label, " (Index)")

        # add my special variable-specific things
        data_infos[[i]][[vi]]$units_old <- data_infos[[i]][[vi]]$units
        
        ## conversion tables
        # carbon variables
        # 1 mole C = 1 mole CO2; 1 mole = 6.02214076 * 1e23 particles
        # 1 mole C = 12.0107 g C
        # --> convert mole C to g C: *12.0107 
        # 1 mole CO2 = 44.0095 g CO2
        # --> 12.0107 g C = 44.0095         g CO2
        # <=>       1 g C = 44.0095/12.0107 g CO2
        # <=>       1 g C = 3.664191        g CO2
        # --> convert g C   to g CO2: *3.664191 (or /0.272912)
        # --> convert g CO2 to g C  : /3.664191 (or *0.272912)
        
        # echam
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
                if (F && varname == "temp2") {
                    message("special temp2 label")
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
            # units can be mm/month or mm/a depending on `modes[i]`
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("P"["total"], " [", unit, "]")),
                                                           list(unit=data_infos[[i]][[vi]]$units))) 
        
        } else if (varname == "evap") {
            message("special: use -1*evap instead of evap")
            data_infos[[i]][[vi]]$offset$operator <- "*"
            data_infos[[i]][[vi]]$offset$value <- -1
            data_infos[[i]][[vi]]$label <- expression(paste("evap*-1 mm/month"))
       
        } else if (varname == "psl") {
            if (grepl("Pa", data_infos[[i]][[vi]]$units)) {
                message("detected a \"Pa\" in the `units` attribute of ", varname, 
                        " --> convert data from Pa to hPa ...")
                data_infos[[i]][[vi]]$units <- "hPa"
                data_infos[[i]][[vi]]$offset$operator <- "/"
                data_infos[[i]][[vi]]$offset$value <- 100
                data_infos[[i]][[vi]]$label <- expression(paste("SLP [hPa]"))
            }

        } else if (varname == "toa_imbalance") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("TOA imbalance [W m"^paste(-2), "]"))))
        
        } else if (varname == "tau_aero_550") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " ", tau, " dV [m"^3, "]"))))
        
        } else if (varname == "lm_albedo_as_time_slope") {
            data_infos[[i]][[vi]]$label <- "Albedo trend [fraction/7k years]"
        
        } else if (varname == "wisoaprt_d") {
            #data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O precip [‰]"))
            data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O"["p,SMOW"], " [\u2030]"))
            #encoding <- get_encoding("‰") # does not work yet
            #if (p$plot_type == "pdf") encoding <- "CP1250"
            if (p$plot_type == "pdf") encoding <- "WinAnsi"

            if (scale_ts) {
                if (T) {
                    message("special diatom label")
                    #data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O diatom/precip (Index)"))
                    data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O"["diatom/p,SMOW"], " [std. dev.]"))
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
            #if (p$plot_type == "pdf") encoding <- "CP1250"
            if (p$plot_type == "pdf") encoding <- "WinAnsi"
            if (T) {
                message("special unit lm_wisoaprt_d_post_as_time_slope")
                data_infos[[i]][[vi]]$offset$operator <- "*"
                if (grepl("cosmos-aso-wiso_Hol-Tx10_", prefixes[i])) {
                    data_infos[[i]][[vi]]$offset$value <- "600/701" # permil/701 accelerated = 7001 not-accelerated years --> permil/6k years
                } else if (grepl("cosmos-aso-wiso_Hol-T_", prefixes[i])) {
                    data_infos[[i]][[vi]]$offset$value <- "6000/6997" # permil/6997 years --> permil/6k years
                } else {
                    stop("setting ", prefixes[i], " not defined")
                }
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(delta^{18}, "O"["p,SMOW"], " trend [\u2030/6k years]"))))
                data_infos[[i]][[vi]]$units <- "o/oo/6k years"
            }

        } else if (varname == "lm_aprt_as_time_slope") {
            if (modes[i] == "select") {
                data_infos[[i]][[vi]]$label <- expression(paste("P"["total"], " trend [mm/month/7k years]"))
                data_infos[[i]][[vi]]$units <- "mm/month/7k years"
                if (T) {
                    message("special unit lm_aprt_as_time_slope")
                    data_infos[[i]][[vi]]$offset$operator <- "*"
                    if (grepl("cosmos-aso-wiso_Hol-Tx10_", prefixes[i])) {
                        data_infos[[i]][[vi]]$offset$value <- "600/701" # mm/month / 701 accelerated = 7001 not-accelerated years --> mm/month / 6k years
                    } else if (grepl("cosmos-aso-wiso_Hol-T_", prefixes[i])) {
                        data_infos[[i]][[vi]]$offset$value <- "6000/6997" # mm/month / 6997 years --> mm/month / 6k years
                    } else {
                        stop("setting ", prefixes[i], " not defined")
                    }
                    data_infos[[i]][[vi]]$label <- expression(paste("P"["total"], " trend [mm/month/6k years]"))
                    data_infos[[i]][[vi]]$units <- "mm/month 6k years"
                }
            } else if (modes[i] == "yearsum") {
                data_infos[[i]][[vi]]$label <- expression(paste("P"["total"], " trend [mm/a/7k years]"))
                data_infos[[i]][[vi]]$units <- "mm/a/7k years"
                if (T) {
                    message("special unit lm_aprt_as_time_slope")
                    data_infos[[i]][[vi]]$offset$operator <- "*"
                    if (grepl("cosmos-aso-wiso_Hol-Tx10_", prefixes[i])) {
                        data_infos[[i]][[vi]]$offset$value <- "600/701" # mm/a / 701 accelerated = 7001 not-accelerated years --> mm/a / 6k years
                    } else if (grepl("cosmos-aso-wiso_Hol-T_", prefixes[i])) {
                        data_infos[[i]][[vi]]$offset$value <- "6000/6997" # mm/a / 6997 years --> mm/a / 6k years
                    } else {
                        stop("setting ", prefixes[i], " not defined")
                    }
                    data_infos[[i]][[vi]]$label <- expression(paste("P"["total"], " trend [mm/a/6k years]"))
                    data_infos[[i]][[vi]]$units <- "mm/a 6k years"
                }
            }

        } else if (varname == "wisoevap_d") {
            data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O evaporation (‰)"))
            if (scale_ts) {
                data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O evaporation (Index)"))
            }
            #if (p$plot_type == "pdf") encoding <- "CP1250"
            if (p$plot_type == "pdf") encoding <- "WinAnsi"

        } else if (varname == "wisope_d") {
            data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O (P-E) (‰)"))
            if (scale_ts) {
                data_infos[[i]][[vi]]$label <- expression(paste(delta^{18}, "O (P-E) (Index)"))
            }
            #if (p$plot_type == "pdf") encoding <- "CP1250"
            if (p$plot_type == "pdf") encoding <- "WinAnsi"
        
        } else if (varname == "lm_temp2_as_time_slope") {
            data_infos[[i]][[vi]]$label <- expression(paste("T"["2m"], " trend [°C/7k years]"))
            data_infos[[i]][[vi]]$units <- "°C/7k years"
            if (F) {
                message("special: lm_temp2_as_time_slope*-1")
                data_infos[[i]][[vi]]$offset$operator <- "*"
                data_infos[[i]][[vi]]$offset$value <- -1
                data_infos[[i]][[vi]]$label <- expression(paste("T"["2m"], " trend 7ka - PI [°C]"))
                data_infos[[i]][[vi]]$units <- "°C"
            }
            if (T) {
                message("special unit lm_temp2_as_time_slope")
                data_infos[[i]][[vi]]$offset$operator <- "*"
                if (grepl("cosmos-aso-wiso_Hol-Tx10_", prefixes[i])) {
                    data_infos[[i]][[vi]]$offset$value <- "600/701" # °C/701 accelerated = 7001 not-accelerated years --> °C/6k years
                } else if (grepl("cosmos-aso-wiso_Hol-T_", prefixes[i])) {
                    data_infos[[i]][[vi]]$offset$value <- "6000/6997" # °C/6997 years --> °C/6k years
                } else {
                    stop("setting ", prefixes[i], " not defined")
                }
                data_infos[[i]][[vi]]$label <- expression(paste("T"["2m"], " trend [°C/6k years]"))
                data_infos[[i]][[vi]]$units <- "°C/6k years"
            }
        
        } else if (varname == "lm_tsurf_as_time_slope") {
            data_infos[[i]][[vi]]$label <- expression(paste("T"["surf"], " trend [°C/7k years]"))
            data_infos[[i]][[vi]]$units <- "°C/7k years"
            if (T) {
                message("special unit lm_tsurf_as_time_slope")
                data_infos[[i]][[vi]]$offset$operator <- "*"
                data_infos[[i]][[vi]]$offset$value <- "6/7" # °C/7k years --> °C/6k years
                data_infos[[i]][[vi]]$label <- expression(paste("T"["surf"], " trend [°C/6k years]"))
                data_infos[[i]][[vi]]$units <- "°C/6k years"
            }
        
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
       
        # carbon echam tracer stream
        } else if (varname == "CO2") { 
            if (data_infos[[i]][[vi]]$units_old == "ppm") {
                if (modes[i] == "fldmean") {
                    data_infos[[i]][[vi]]$label <- expression(paste("global mean atm CO"[2], " [ppm]"))
                } else {
                    data_infos[[i]][[vi]]$label <- expression(paste("atm CO"[2], " [ppm]"))
                }
            } else {
                stop("not defined")
            }
        } else if (any(varname == c("co2_flux", "co2_flx_ocean", "co2_flx_land",
                                    "co2_flx_npp", "co2_flx_resp", "co2_flx_herb", "co2_flx_lcc", "co2_flx_harvest", "co2_flx_fire"))) { 
            # original units kgCO2 m-2 s-1
            if (any(varname == c("co2_flux", "co2_flx_ocean"))) {
                warning("If echam variable `", varname, "` was calculated in awicm1-recom setup with old incorrect code\n",
                        "  `exchange(:) = GloCO2flux` instead of new corrected code `exchange(:) = -GloCO2flux`\n",
                        "  `", varname, "` is incorrect. Here, it is assumed that the correct variable is used.")
            }
            data_infos[[i]][[vi]]$units <- "kgCO2 m-2 s-1"
            if (varname == "co2_flux") { # = co2_flx_ocean + co2_flx_land
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Total CO"[2], " flux [kgCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "co2_flx_ocean") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [kgCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "co2_flx_land") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [kgCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "co2_flx_npp") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("NPP CO"[2], " flux [kgCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "co2_flx_resp") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Soil resp CO"[2], " flux [kgCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "co2_flx_herb") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Herbivory CO"[2], " flux [kgCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "co2_flx_lcc") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("LCC CO"[2], " flux [kgCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "co2_flx_harvest") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Harvest CO"[2], " flux [kgCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "co2_flx_fire") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Fire CO"[2], " flux [kgCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            }
            if (T) { # convert carbon units
                message(">0 into atm -> >0 into land/ocean; s-1 -> yr-1; kgCO2 -> kgC")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*", "*", "/")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1, 365.25*86400, 3.664191) # into atm->into land/ocean, s-1->yr-1, kgCO2->kgC
                data_infos[[i]][[vi]]$units <- "kgC m-2 yr-1"
                if (varname == "co2_flux") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Total CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land/ocean)"))))
                } else if (varname == "co2_flx_ocean") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into ocean)"))))
                } else if (varname == "co2_flx_land") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                } else if (varname == "co2_flx_npp") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("NPP CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                } else if (varname == "co2_flx_resp") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Soil resp CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                } else if (varname == "co2_flx_herb") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Herbivory CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                } else if (varname == "co2_flx_lcc") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("LCC CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                } else if (varname == "co2_flx_harvest") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Harvest CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                } else if (varname == "co2_flx_fire") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Fire CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                }
                if (modes[i] == "fldint") {
                    message("kg -> Pg")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e12) # kg -> Pg
                    data_infos[[i]][[vi]]$units <- "PgC yr-1"
                    if (varname == "co2_flux") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Total CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land/ocean)"))))
                    } else if (varname == "co2_flx_ocean") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into ocean)"))))
                    } else if (varname == "co2_flx_land") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_npp") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("NPP CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_resp") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Soil resp CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_herb") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Herbivory CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_lcc") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("LCC CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_harvest") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Harvest CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_fire") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Fire CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    }
                } else {
                    message("co2_flux: kg --> g")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e3) # kg -> g
                    data_infos[[i]][[vi]]$units <- "gC m-2 yr-1"
                    if (varname == "co2_flux") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Total CO"[2], " flux [gC", " m"^paste(-2), " yr"^paste(-1), "] (>0 into land/ocean)"))))
                    } else if (varname == "co2_flx_ocean") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [gC", " m"^paste(-2), " yr"^paste(-1), "] (>0 into ocean)"))))
                    } else if (varname == "co2_flx_land") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [gC", " m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_npp") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("NPP CO"[2], " flux [gC", " m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_resp") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Soil resp CO"[2], " flux [gC", " m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_herb") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Herbivory CO"[2], " flux [gC", " m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_lcc") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("LCC CO"[2], " flux [gC", " m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_harvest") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Harvest CO"[2], " flux [gC", " m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_fire") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Fire CO"[2], " flux [gC", " m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                    }
                }
            }

        # carbon jsbach jsbach stream; see mo_jsbach_interface.f90:
        } else if (any(varname == c("CO2_flux_net", "CO2_emission_landcover_change", "CO2_flux_dynveg"))) {
            # original units: mol(CO2) m-2(grid box) s-1
            data_infos[[i]][[vi]]$units <- "molCO2 m-2 s-1"
            if (varname == "CO2_flux_net") { 
                # CO2_flux_net (160) = CO2_flux_npp + CO2_flux_soilresp + CO2_flux_herbivory (161) + CO2_emission_landcover_change (162) + CO2_emission_harvest (163) + CO2_flux_dynveg (164)
                # --> CO2_flux_net = nbp = co2_flx_land + co2_flx_lcc + co2_flx_harvest
                # co2_flx_fire                  = 146475      kgCO2 s-1  = 1.261508281 PgC yr-1
                # CO2_flux_dynveg               = 3.32824e+06 molCO2 s-1 = 1.261501204 PgC yr-1
                # co2_flx_lcc                   = 96751.1     kgCO2 s-1  = 0.8332638 PgC yr-1
                # CO2_emission_landcover_change = 2.19546e+06 molCO2 s-1 = 0.8321441 PgC yr-1
                # --> echam:co2:co2_flx_* = jsbach:jsbach_CO2_*
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [molCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "CO2_emission_landcover_change") {
                # CO2 emission from landcover change
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("LCC CO"[2], " flux [molCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            } else if (varname == "CO2_flux_dynveg") {
                # CO2 flux due to (natural) fires (dynveg) 
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Fire CO"[2], " flux [molCO"[2], " m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            }
            if (T) { # convert carbon units
                message("into atm -> into land; molCO2 -> gCO2; gCO2 -> gC; gC -> kgC; s-1 -> yr-1")
                data_infos[[i]][[vi]]$offset$operator <- c("*", "*", "/", "*", "/")
                data_infos[[i]][[vi]]$offset$value <- c(-1, 44.0095, 3.664191, 365.25*86400, 1e3) # into atm->into land; molCO2->gCO2, gCO2->gC, s-1->yr-1, gC->kgC
                data_infos[[i]][[vi]]$units <- "kgC m-2 yr-1"
                if (varname == "CO2_flux_net") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [PgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                } else if (varname == "CO2_emission_landcover_change") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("LCC CO"[2], " flux [PgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                } else if (varname == "CO2_flux_dynveg") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Fire CO"[2], " flux [PgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                }
                if (modes[i] == "fldint") {
                    message("kg -> Pg")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e12) # kg -> Pg
                    data_infos[[i]][[vi]]$units <- "PgC yr-1"
                    if (varname == "CO2_flux_net") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "CO2_emission_landcover_change") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("LCC CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "CO2_flux_dynveg") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Fire CO"[2], " flux to atm [PgC yr"^paste(-1), "] (>0 into land)"))))
                    }
                } else {
                    stop("not yet")
                }
            }

        # cmip6 carbon
        } else if (varname == "co2mass") {
            data_infos[[i]][[vi]]$units <- "kgCO2 m-2"
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Total Atmospheric Mass of CO"[2], " [kgCO2 m"^paste(-2), "]"))))
            if (T) { # convert units
                data_infos[[i]][[vi]]$units <- "ppm"
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Total Atmospheric Mass of CO"[2], " [ppm]"))))
                if (grep("fldint", modes)) {
                    message("kgCO2 -> ppm")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*", "/", "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 0.272912, 1e12, 2.124) # m-2->fldint, kgCO2->kgC; kgC->PgC; PgC->ppm
                } else {
                    message("kgCO2 m-2 -> ppm")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*", "*", "/", "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, Aearth, 0.272912, 1e12, 2.124) # m2->fldint, kgCO2->kgC; kgC->PgC; PgC->ppm
                }
            }

        # cmip6 carbon fluxes away from atm
        } else if (any(varname == c("fgco2", "nbp", "netAtmosLandCO2Flux", "co2_flx_total"))) {
            if (grepl("reccap2", prefixes[i])) {
                data_infos[[i]][[vi]]$units <- "molC m-2 s-1"
                if (models[i] == "CCSM-WHOI") {
                    message("<0: into ocean --> >0: into ocean")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1)
                }
                if (varname == "fgco2") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [molC m"^paste(-2), " s"^paste(-1), "] (>0 into ocean)"))))
                }
                if (T) { # convert carbon units
                    message("s-1 -> yr-1")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 365.25*86400) # s-1->yr-1
                    data_infos[[i]][[vi]]$units <- "molC m-2 yr-1"
                    if (varname == "fgco2") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [molC m"^paste(-2), " yr"^paste(-1), "] (>0 into ocean)"))))
                    }
                    if (grepl("fldint", modes[i])) {
                        message("molC -> Pg")
                        data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*", "/", "/")
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 12.0107, 1e3, 1e12) # molC-->gC, gC-->kg, kg->Pg
                        data_infos[[i]][[vi]]$units <- "PgC yr-1"
                        if (varname == "fgco2") {
                            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into ocean)"))))
                        }
                    } else {
                        stop("not yet")
                    }
                } # convert carbon units

            } else { # default: original units kgC m-2 s-1
                data_infos[[i]][[vi]]$units <- "kgC m-2 s-1"
                if (varname == "fgco2") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [kgC m"^paste(-2), " s"^paste(-1), "] (>0 into ocean)"))))
                } else if (any(varname == c("nbp", "netAtmosLandCO2Flux"))) {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [kgC m"^paste(-2), " s"^paste(-1), "] (>0 into land)"))))
                } else if (varname == "co2_flx_total") { # my own definition = fgco2 + nbp !
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("total CO"[2], " flux [kgC m"^paste(-2), " s"^paste(-1), "] (>0 into ocean/land)"))))
                }
                if (T) { # convert carbon units
                    message("s-1 -> yr-1")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 365.25*86400) # s-1->yr-1
                    data_infos[[i]][[vi]]$units <- "kgC m-2 yr-1"
                    if (varname == "fgco2") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into ocean)"))))
                    } else if (any(varname == c("nbp", "netAtmosLandCO2Flux"))) {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                    } else if (varname == "co2_flx_total") { # my own definition = fgco2 + nbp
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("total CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into ocean/land)"))))
                    }
                    if (grepl("fldint", modes[i])) {
                        message("kg -> Pg")
                        data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e12) # kg->Pg
                        data_infos[[i]][[vi]]$units <- "PgC yr-1"
                        if (varname == "fgco2") {
                            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into ocean)"))))
                        } else if (any(varname == c("nbp", "netAtmosLandCO2Flux"))) {
                            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-land CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                        } else if (varname == "co2_flx_total") { # my own definition = fgco2 + nbp
                            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("total CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into ocean/land)"))))
                        }
                    } else {
                        stop("not yet")
                    }
                } # convert carbon units
            } # which model

        # cmip6 carbon fluxes to atm
        } else if (any(varname == c("fHarvest"))) {
            # original units kgC m-2 s-1
            data_infos[[i]][[vi]]$units <- "kgC m-2 s-1"
            if (varname == "fHarvest") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Crop harvest CO"[2], " flux [kgC m"^paste(-2), " s"^paste(-1), "] (>0 into atm)"))))
            }
            if (T) { # convert carbon units
                message("into atm -> into land; s-1 -> yr-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*", "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1, 365.25*86400) # into atm->into land; s-1->yr-1
                data_infos[[i]][[vi]]$units <- "kgC m-2 yr-1"
                if (varname == "fHarvest") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Crop harvest CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into land)"))))
                }
                if (grepl("fldint", modes[i])) {
                    message("kg -> Pg")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e12) # kg->Pg
                    data_infos[[i]][[vi]]$units <- "PgC yr-1"
                    if (varname == "fHarvest") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Crop harvest CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into land)"))))
                    }
                } else {
                    stop("not yet")
                }
            }

        # cmip6 carbon pools
        } else if (varname == "cSoilSlow") {
            data_infos[[i]][[vi]]$units <- "kgC m-2"
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("C"["soil,slow"], " [kgC m"^paste(-2), "]"))))
            if (modes[i] == "fldint") {
                message("cSoilSlow kgC --> PgC")
                data_infos[[i]][[vi]]$offset$operator <- c("/")
                data_infos[[i]][[vi]]$offset$value <- c(1e12) # kgC->PgC
                data_infos[[i]][[vi]]$units <- "PgC"
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("C"["soil,slow"], " [PgC]"))))
            }
        
        # carbon recom
        } else if (varname == "aCO2") { # recom
            # atm = non-SI international unit of pressure defined as 101.325 kPa = 101.325 Pa
            # --> convert Pa to atm:  Pa/101325     = Pa*9.86923266716013E-06 = atm
            # --> convert Pa to µatm: Pa/101325*1e6 = Pa*9.869233             = µatm
            data_infos[[i]][[vi]]$units <- "µatm"
            data_infos[[i]][[vi]]$label <- expression(paste("global mean atm CO"[2], " [µatm]"))

        } else if (any(varname == c("CO2f", "NPP"))) { 
            # original: mmolC m-2 d-1
            if (varname == "CO2f") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [mmolC m"^paste(-2), " d"^paste(-1), "] (>0 into ocean)"))))
            } else if (varname == "NPP") {
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("NPP [mmolC m"^paste(-2), " d"^paste(-1), "]"))))
            }
            if (T) {
                message("mmolC --> molC; molC -> gC; gc -> kgC; d-1 --> yr-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/", "*", "/", "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e3, 12.0107, 1e3, 365.25) # mmolC -> molC; molC -> gC; gc -> kgC; d-1 -> yr-1; 
                data_infos[[i]][[vi]]$units <- "kgC m-2 yr-1"
                if (varname == "CO2f") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [kgC m"^paste(-2), " yr"^paste(-1), "] (>0 into ocean)"))))
                } else if (varname == "NPP") {
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("NPP [kgC m"^paste(-2), " yr"^paste(-1), "]"))))
                }
                if (modes[i] == "fldint") {
                    message("kgC --> PgC")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e12) # kg -> Pg
                    data_infos[[i]][[vi]]$units <- "PgC yr-1"
                    if (varname == "CO2f") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("air-sea CO"[2], " flux [PgC yr"^paste(-1), "] (>0 into ocean)"))))
                    } else if (varname == "NPP") {
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("NPP [PgC yr"^paste(-1), "]"))))
                    }
                }
            }
        
        ## land

        # jsbach
        } else if (varname == "lm_act_fpc_as_time_slope") {
            if (all(levs == "sum1-4lev")) {
                data_infos[[i]][[vi]]$label <- "Forest trend [fraction/7k years]"
            } else {
                data_infos[[i]][[vi]]$label <- "fractional plant cover trend [fraction/7k years]"
            }
        
        } else if (varname == "pft_fract_box") {
            if (modes[i] == "fldsum") {
                message("pft_fract_box fldsum divide through nland = 6126")
                warning("todo: get nland from nc att")
                data_infos[[i]][[vi]]$offset$operator <- "/"
                data_infos[[i]][[vi]]$offset$value <- 6126
                message("pft_fract_box fraction --> percent")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 100)
                data_infos[[i]][[vi]]$label <- "% of global land cover"
            }

        ## ocean

        # fesom
        } else if (varname == "hvel") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("horizontal velocity [m s"^paste(-1), "]"))))
            if (T) {
                message("hvel m s-1 --> cm s-1")
                data_infos[[i]][[vi]]$units <- "cm s-1"
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("horizontal velocity [cm s"^paste(-1), "]"))))
                data_infos[[i]][[vi]]$offset$operator <- "*"
                data_infos[[i]][[vi]]$offset$value <- 100
            }

        } else if (grepl("moc", varname)) {
            data_infos[[i]][[vi]]$label <- "MOC [Sv]"
            if (grepl("moc_max_depths", varname)) {
                data_infos[[i]][[vi]]$label <- "Depth of MOC max [m]"
            }
        
        } else if (any(varname == c("mlotst", "mixlay", "mixlay_mean"))) { # ML from density threshold
            #data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("MLD"[sigma[theta]], " [m]"))))
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("MLD"[paste(sigma[theta], "=0.125 kg m"^-3)], " [m]"))))
            if (F) {
                message("mixlay m --> km ...")
                #data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("MLD"[paste(sigma[theta], "=0.125 kg m"^-3)], " [km]"))))
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("March MLD"[paste(sigma[theta], "=0.125 kg m"^-3)], " [km]"))))
                data_infos[[i]][[vi]]$offset$operator <- "/"
                data_infos[[i]][[vi]]$offset$value <- 1000
            }
            if (T) {
                message("special mlotst label")
                data_infos[[i]][[vi]]$label <- substitute(paste("MLD"[paste(sigma[theta], "=0.125 kg m"^-3)], " Event/Clim*100 [%]"))
            }

        } else if (varname == "mlotstmax") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("max MLD"[sigma[theta]], " [m]"))))
        
        } else if (varname == "mlotstmin") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("min MLD"[sigma[theta]], " [m]"))))
        
        } else if (varname == "omldamax") { # ML from mixing scheme
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("max MLD"["MS"], " [m]"))))
            if (F) {
                message("omldamax special")
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("max MLD"["MS"], " [m] (>0: AWI deeper than MPI)"))))
            }
            if (T) {
                message("special omldamax label")
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("max MLD"["MS"], " Event/Clim*100 [%]"))))
            }

        } else if (grepl("siarea", varname)) {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("NH sea ice extent [km"^2, 
                                                                            " " %*% " ", 10^6, "]"))))
        
        } else if (varname == "tos") {
            data_infos[[i]][[vi]]$label <- "SST [°C]"
            if (F) {
                message("special label")
                data_infos[[i]][[vi]]$label <- "SST anomaly [°C]"
            }
        
        } else if (any(varname == c("thetao", "thetaoga"))) {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("T"[theta], " [°C]"))))
                
        } else if (varname == "potdens") {
            #data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(sigma[theta], " [kg m"^"-3","]"))))
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("Potential density ", 
                                                                            sigma[theta], " [kg m"^"-3","]"))))
            data_infos[[i]][[vi]]$offset$operator <- "-"
            data_infos[[i]][[vi]]$offset$value <- 1000
        
        } else if (varname == "Ftemp") {
            # Ftemp = Qnet/(rho*cp) in °C m s-1; rho0 = 1027 kg m3; cp = 4000 m2 s-2 K-1
            data_infos[[i]][[vi]]$label <- substitute(paste(#F[T], " to ocean ",
                                                            "(", rho, "c"[p], ")"^-1, 
                                                            " ", Q[net], 
                                                            " [°C ", var1, " ", var2^-1, "]",
                                                            #" " %*% " ", 10^-6
                                                            ),
                                                      list(var1="m", var2="s"
                                                           #, base=base, power_plot=power_plot
                                                           ))
            if (nsettings == 2 && i == 2) {
                message("special Ftemp names_legend")
                names_legend <- c(eval(substitute(expression(paste("(", rho, "c"[p], ")"^-1, " ", Q[net])))),
                                  "")
            }
            if (F) { # in W m-2
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1027*4000)
                data_infos[[i]][[vi]]$label <- substitute(paste(#F[T], " to ocean ",
                                                                Q[net], 
                                                                " [W ", var1^-2, "]",
                                                                ),
                                                          list(var1="m"#, var2="s"
                                                               #, base=base, power_plot=power_plot
                                                               ))
                if (nsettings == 2 && i == 2) {
                    message("special Ftemp names_legend")
                    names_legend <- c(eval(substitute(expression(paste(Q[net])))), "")
                }
            }
            if (modes[i] == "fldint") {
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                if (T) {
                    message("special Ftemp names_legend_samedims")
                    #names_legend_samedims[i] <- eval(substitute(expression(paste(F[T]))))
                    names_legend_samedims[i] <- eval(substitute(expression(paste(F))))
                }
            }

        } else if (any(varname == c("taux", "tauuo"))) {
            data_infos[[i]][[vi]]$label <- substitute(paste("Meridional windstress ", tau[x], " [N m"^"-2","]"))
        
        } else if (any(varname == c("tauy", "tauvo"))) {
            data_infos[[i]][[vi]]$label <- substitute(paste("Zonal windstress ", tau[y], " [N m"^"-2","]"))

        } else if (varname == "tau") {
            data_infos[[i]][[vi]]$label <- substitute(paste("|", bold(tau)[h], "| [N m"^"-2","]"))
            if (any(datas[[i]][[vi]] < 0)) { # very few slightly negative values due to interpolation from irregular to regular
                inds <- datas[[i]][[vi]] < 0
                message("set ", length(which(inds)), " points < 0 to 0")
                datas[[i]][[vi]][inds] <- 0
            }
            if (T) {
                message("special tau label")
                data_infos[[i]][[vi]]$label <- substitute(paste("|", bold(tau)[h], "| Event/Clim*100 [%]"))
            }

        } else if (varname == "curltau") {
            if (T) { # special anom_pcnt
                message("special curltau label")
                data_infos[[i]][[vi]]$label <- substitute(paste(bold(k), "" %.% bold(nabla), " " %*% " ", bold(tau),
                                                            " Event/Clim*100 [%]"))
            } else { # default
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e7)
                data_infos[[i]][[vi]]$label <- substitute(paste(bold(k), "" %.% bold(nabla), " " %*% " ", bold(tau),
                                                                " [N m"^-3, "] " %*% " ", 10^-7))
            }

        } else if (varname == "ekmanP_ms") {
            if (T) { # special anom_pcnt
                message("special ekmanP_ms label")
                data_infos[[i]][[vi]]$label <- substitute(paste(rho[0]^-1, " ", bold(k), "" %.% bold(nabla), " " %*% " (", bold(tau), "/f) ",
                                                                "Event/Clim*100 [%]"))
            } else { # default
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 86400) # m s-1 --> m a-1
                data_infos[[i]][[vi]]$label <- substitute(paste(rho[0]^-1, " ", bold(k), "" %.% bold(nabla), " " %*% " (", bold(tau), "/f) ",
                                                                "[m a"^-1, "] (<0 downwelling)"))
            }

        } else if (any(varname == c("divuvt", "divuvt_meanint"))) {
            minus_symbol <- ""
            if (T) {
                message("convert divergence to convergence --> *-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1)
                minus_symbol <- minus_symbol_dash
            }
            if (modes[i] == "timmean") {
                if (grepl("int", depths[i])) {
                    # my unit: °C m s-1
                    # chanut et al. 2008: W m-2
                    # -> my unit * rho0 * cp = chanuts unit
                    # -> chanut: rho0 = 1027 kg m3; cp = 4000 m2 s-2 K-1
                    if (F) { # in W m-2
                        message("convert °C m s-1 --> W m-2 by multiplying by rho0 * cp = 1027 kg m-3 * 4000 m2 s-2 K = 4108000 kg m-1 s-2 K-1")
                        data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1027*4000)
                        data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol,
                                                                        rho[0], " ", c[p],
                                                                        bold(nabla)[h] %.% bar(bold(u))["h"], bar(T),
                                                                        " dz [", var1, " ", var2^-2,
                                                                        "]"),
                                                                  list(var1="W", var2="m", minus_symbol=minus_symbol))#,
                                                                   #base=base, power_plot=3))
                    } else if (F) { # in °C m s-1 with multiplication factor
                        data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^3)
                        data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol,
                                                                        integral(),
                                                                        bold(nabla)[h] %.% bar(bold(u))["h"], bar(T),
                                                                        " dz [°C ", var1, " ", var2^-1,
                                                                        "] " %*% "", 10^-3),
                                                                  list(var1="m", var2="s", minus_symbol=minus_symbol))#,
                                                                       #base=base, power_plot=3))
                    } else { # in °C m s-1 without multiplication factor
                        data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol,
                                                                        integral(),
                                                                        bold(nabla)[h] %.% bar(bold(u))["h"], bar(T),
                                                                        " dz [°C ", var1, " ", var2^-1,
                                                                        "]"),
                                                                  list(var1="m", var2="s", minus_symbol=minus_symbol))#,
                                                                       #base=base, power_plot=3))
                    }
                    if (nsettings == 2 && i == 2) {
                        message("special divuvt names_legend")
                        names_legend <- c(eval(substitute(expression(paste(minus_symbol,  integral(),
                                                                           bold(nabla)[h] %.% bar(bold(u))["h"], bar(T),
                                                                           " dz")),
                                                          list(minus_symbol=minus_symbol))), "")
                    }
                } else { # not depth integrated
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol,
                                                                    bold(nabla)[h] %.% bar(bold(u))["h"], bar(T),
                                                                    " [", var1, " ", var2^-1,
                                                                     "] " %*% " ", 10^-6),
                                                              list(var1="°C", var2="s", minus_symbol=minus_symbol))#,
                                                                   #base=10, 2))
                }
            } else if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) { # if depth integrated
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    if (T) {
                        message("special divuvt names_legend_samedims")
                        names_legend_samedims[i] <- eval(substitute(expression(paste(minus_symbol, 
                                                                                     bold(nabla)[h] %.% bar(bold(u))[h], bar(T))),
                                                                    list(minus_symbol=minus_symbol))) # mean
                    }
                }
            } # which mode
        
        } else if (any(varname == c("divuvteddy", "divuvteddy_meanint"))) {
            minus_symbol <- ""
            if (T) {
                message("convert divergence to convergence --> *-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1)
                minus_symbol <- minus_symbol_dash
            }
            if (modes[i] == "timmean") {
                if (grepl("int", depths[i])) {
                    # my unit: °C m s-1
                    # chanut et al. 2008: W m-2
                    # -> my unit * rho0 * cp = chanuts unit
                    # -> chanut: rho0 = 1027 kg m3; cp = 4000 m2 s-2 K-1
                    if (F) { # in W m-2
                        message("convert °C m s-1 --> W m-2 by multiplying by rho0 * cp = 1027 kg m-3 * 4000 m2 s-2 K = 4108000 kg m-1 s-2 K-1")
                        data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1027*4000)
                        data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol,
                                                                        rho[0], " ", c[p],
                                                                        integral(),
                                                                        bold(nabla)[h] %.% bar(paste(bold(u)[h], "'T'")),
                                                                        " dz [", var1, " ", var2^-2,
                                                                        "]"),# " %*% " ", 10^-3),
                                                                  list(var1="W", var2="m", minus_symbol=minus_symbol))#,
                                                                       #base=base, power_plot=power_plot))
                    } else if (F) { # in °C m s-1 with multiplication factor
                        data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^3)
                        data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol,
                                                                        integral(),
                                                                        bold(nabla)[h] %.% bar(paste(bold(u)[h], "'T'")),
                                                                        " dz [°C ", var1, " ", var2^-1,
                                                                        "] " %*% " ", 10^-3),
                                                                  list(var1="m", var2="s", minus_symbol=minus_symbol))#,
                                                                       #base=base, power_plot=power_plot))
                    } else { # in °C m s-1 without multiplication factor
                        data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol,
                                                                        integral(),
                                                                        bold(nabla)[h] %.% bar(paste(bold(u)[h], "'T'")),
                                                                        " dz [°C ", var1, " ", var2^-1,
                                                                        "]"),
                                                                  list(var1="m", var2="s", minus_symbol=minus_symbol))#,
                                                                       #base=base, power_plot=power_plot))
                    }
                } else { # not depth integrated
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol,
                                                                    bold(nabla)[h] %.% bar(paste(bold(u)[h], "'T'")),
                                                                    " [", var1, " ", var2^-1,
                                                                    "] " %*% " ", 10^-6),
                                                              list(var1="°C", var2="s", minus_symbol=minus_symbol))#,
                                                                   #base=base, power_plot=power_plot))
                }
                if (nsettings == 2 && i == 2) {
                    message("special divuvteddy names_legend")
                    names_legend <- c(eval(substitute(expression(paste(minus_symbol,  integral(),
                                                                       bold(nabla)[h] %.% bar(paste(bold(u)[h], "'T'")),
                                                                       " dz")),
                                                      list(minus_symbol=minus_symbol))), "")
                }
            } else if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) { # if depth integrated
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    if (T) {
                        message("special divuvteddy names_legend_samedims")
                        names_legend_samedims[i] <- eval(substitute(expression(paste(minus_symbol,
                                                                                     bold(nabla)[h] %.% bar(paste(bold(u)[h], "'T'")))),
                                                                    list(minus_symbol=minus_symbol)))
                    }
                }
            } # which mode

        } else if (any(varname == c("divuvttot", "divuvttot_meanint", 
                                    "divuvttot_plus_divuvsgsttot", "divuvttot_plus_divuvsgsttot_meanint"))) {
            minus_symbol <- ""
            if (T) {
                message("convert divergence to convergence --> *-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1)
                minus_symbol <- minus_symbol_dash
            }
            if (modes[i] == "timmean") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol,
                                                                    integral(),
                                                                    bold(nabla)[h] %.% bar(paste(bold(u)[h], "T")),
                                                                    " dz [°C ", var1, " ", var2^-1,
                                                                    "]"),
                                                              list(var1="m", var2="s", minus_symbol=minus_symbol))#,
                                                                   #base=base, power_plot=power_plot))
                    if (nsettings == 2 && i == 2) {
                        message("special divuvttot names_legend")
                        names_legend <- c(eval(substitute(expression(paste(minus_symbol,  integral(),
                                                                           bold(nabla)[h] %.% bar(paste(bold(u)[h], "T")),
                                                                           " dz")),
                                                          list(minus_symbol=minus_symbol))), "")
                    }
                }
            } else if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    if (T) {
                        message("set special label for divuvt_budget samedims")
                        data_infos[[i]][[vi]]$label <- substitute(paste(integral(),
                                                                        " T-advection",
                                                                        " d", V[MLD], " [°C ", var1^3, " ", var2^-1,
                                                                        "] " %*% " ", 10^6),
                                                                   list(var1="m", var2="s"))
                        names_legend_samedims[i] <- eval(substitute(expression(paste(minus_symbol, bold(nabla)[h] %.% 
                                                                                     bar(paste(bold(u)[h], "T")))), 
                                                                    list(minus_symbol=minus_symbol))) # total
                    }
                }
            }

        } else if (any(varname == c("divuvteddy_plus_divuvsgsttot", "divuvteddy_plus_divuvsgsttot_meanint"))) {
            minus_symbol <- ""
            if (T) {
                message("convert divergence to convergence --> *-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1)
                minus_symbol <- minus_symbol_dash
            }
            if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    if (T) {
                        message("special divuvteddy_plus_divuvsgsttot names_legend_samedims")
                        names_legend_samedims[i] <- eval(substitute(expression(paste(minus_symbol, 
                                                                                     "(", bold(nabla)[h] %.% bar(paste(bold(u)[h], "'T'")),
                                                                                     " + ", bold(nabla)[h] %.% bar(paste(bold(u)["SGS,h"], "T")), 
                                                                                     ")")),
                                                                    list(minus_symbol=minus_symbol))) # eddy + sgs total
                    }
                }
            }

        } else if (any(varname == c("divuvsgsttot", "divuvsgsttot_meanint"))) {
            minus_symbol <- ""
            if (T) {
                message("convert divergence to convergence --> *-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1)
                minus_symbol <- minus_symbol_dash
            }
            if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    if (T) {
                        message("special divuvsgsttot names_legend_samedims")
                        names_legend_samedims[i] <- eval(substitute(expression(paste(minus_symbol, 
                                                                                     bold(nabla)[h] %.% bar(paste(bold(u)["SGS,h"], "T")))),
                                                                    list(minus_symbol=minus_symbol))) # sgs total
                    }
                }
            }
        
        } else if (varname == "divuvttot_divuvsgsttot_Ftemp") {
            if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    if (T) {
                        message("special divuvttot_divuvsgsttot_Ftemp names_legend_samedims")
                        names_legend_samedims[i] <- eval(substitute(expression(paste("RHS"))))
                    }
                }
            }
        
        } else if (varname == "divuvtrest") {
            if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    if (T) {
                        message("special divuvtrest names_legend_samedims")
                        names_legend_samedims[i] <- eval(substitute(expression(paste("Rest"))))
                    }
                }
            }

        } else if (varname == "dttemp") {
            # apply /dt
            #dt_day <- difftime(dims[[i]]$time[2:length(dims[[i]]$time)], dims[[i]]$time[1:(length(dims[[i]]$time)-1)], units="day")
            #if (!any(is.na(match(dt_day, c(28, 29, 30, 31))))) {
            #    dt_fac <- 30.5*86400
            #    message("dttemp apply monthly /dt fac 30.5*86400 = ", dt_fac) 
            #    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
            #    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, dt_fac)
            #}
            if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^6)
                    if (T) {
                        message("special dttemp names_legend_samedims")
                        names_legend_samedims[i] <- eval(substitute(expression(paste("LHS"))))
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(),
                                                                                        " T-budget",
                                                                                        " dV [°C  ", varunit1^3, " ", varunit2^-1,
                                                                                        "] " %*% " ", base^power)),
                                                                       list(varunit1="m", varunit2="s",
                                                                            base=10, power=6)))
                    }
                }
            }

        } else if (varname == "FeKe") {
            if (modes[i] == "timmean") {
                #data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                #data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                #data_infos[[i]][[vi]]$label <- "FeKe 1e6"
                if (nsettings == 2) {
                    message("special FeKe names_legend")
                    #names_legend <- c(eval(substitute(expression(paste(bar(paste(bold(u)[h], "'" %.% "", bold(tau), "'")),
                    #                                                   " ", rho[0], ""^-1)))), "")
                    names_legend <- c(eval(substitute(expression(paste(F[e], "", K[e])))), "")
                }
            } else if (modes[i] == "fldint") {
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(#rho[0], ""^-1, " ",
                                                                                integral(), " ",
                                                                                #bar(paste(bold(u)[h], "'" %.% "", bold(tau), "'")),
                                                                                F[e], "", K[e],
                                                                                " dA [", var1^5, " ", var2^-3,
                                                                                "] " %*% "", 10^6)),
                                                               list(var1="m", var2="s")))
            }

        } else if (varname == "HRS") {
            if (modes[i] == "timmean") {
                if (grepl("int", depths[i])) {
                    #data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    #data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                    #data_infos[[i]][[vi]]$label <- "HRS 1e6"
                    if (nsettings == 2) {
                        message("special HRS names_legend")
                        names_legend <- c(eval(substitute(expression(paste(integral(), " HRS dz")))), "")
                    }
                }
            } else if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    if (F) {
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " HRS ",
                                                                                        " dV [", var1^5, " ", var2^-3,
                                                                                        "] " %*% "", 10^6)),
                                                                       list(var1="m", var2="s")))
                    } else if (T) {
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e4)
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " HRS ",
                                                                                        " dV [", var1^5, " ", var2^-3,
                                                                                        "] " %*% "", 10^4)),
                                                                       list(var1="m", var2="s")))
                    }
                }
            }
        
        } else if (varname == "VRS") {
            if (modes[i] == "timmean") {
            } else if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    if (F) {
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " VRS ",
                                                                                        " dV [", var1^5, " ", var2^-3,
                                                                                        "] " %*% "", 10^6)),
                                                                       list(var1="m", var2="s")))
                    } else if (T) {
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e4)
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " VRS ",
                                                                                        " dV [", var1^5, " ", var2^-3,
                                                                                        "] " %*% "", 10^4)),
                                                                       list(var1="m", var2="s")))
                    }
                }
            }

        } else if (varname == "KmKe") {
            if (modes[i] == "timmean") {
                if (grepl("int", depths[i])) {
                    #data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    #data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                    #data_infos[[i]][[vi]]$label <- "HRS 1e6"
                    if (nsettings == 2) {
                        message("special KmKe names_legend")
                        names_legend <- c(eval(substitute(expression(paste(integral(), " KmKe dz")))), "")
                    }
                }
            } else if (modes[i] == "fldmean") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " ", K[m], "", K[e],
                                                                                    " dz [", var1^3, " ", var2^-3,
                                                                                    "] " %*% "", 10^-6)),
                                                                   list(var1="m", var2="s")))
                }
            } else if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    if (F) {
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " HRS ",
                                                                                        " dV [", var1^5, " ", var2^-3,
                                                                                        "] " %*% "", 10^6)),
                                                                       list(var1="m", var2="s")))
                    } else if (T) {
                        data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e4)
                        data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " HRS ",
                                                                                        " dV [", var1^5, " ", var2^-3,
                                                                                        "] " %*% "", 10^4)),
                                                                       list(var1="m", var2="s")))
                    }
                }
            }
        
        } else if (varname == "wbeddy") {
            if (any(modes[i] == c("timmean", "fldmean"))) {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " ", 
                                                                                    #bar(paste("w'b'")),
                                                                                    P[e], "", K[e],
                                                                                    " dz [", var1^3, " ", var2^-3,
                                                                                    #"]",
                                                                                    "] " %*% "", 10^-6
                                                                                    )),
                                                                   list(var1="m", var2="s")))
                    if (F && nsettings == 2) {
                        message("special wbeddy names_legend")
                        #names_legend <- c(eval(substitute(expression(paste(integral(), " ", bar(paste("w'b'")), " dz")))), "")
                        names_legend <- c(eval(substitute(expression(paste(integral(), " ", P[e], "", K[e], " dz")))), "")
                    }
                }
            } else if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e6)
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " ", 
                                                                                    #bar(paste("w'b'")),
                                                                                    P[e], "", K[e],
                                                                                    " dV [", var1^5, " ", var2^-3,
                                                                                    "] " %*% "", 10^6)),
                                                                   list(var1="m", var2="s")))
                }
            }

        } else if (any(varname == c("divuvb", "divuvb_meanint"))) {
            if (T) {
                message("convert divergence to convergence --> *-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1)
                minus_symbol <- minus_symbol_dash
            } else {
                minus_symbol <- ""
            }
            if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) { # if depth integrated
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^9)
                    if (T) {
                        message("special divuvb names_legend_samedims")
                        names_legend_samedims[i] <- eval(substitute(expression(paste(minus_symbol, 
                                                                                     bold(nabla)[h] %.% bar(bold(u))[h], bar(b))),
                                                                    list(minus_symbol=minus_symbol))) # mean
                    }
                }
            } # which mode
        
        } else if (any(varname == c("divuvbeddy", "divuvbeddy_meanint"))) {
            if (T) {
                message("convert divergence to convergence --> *-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1)
                minus_symbol <- minus_symbol_dash
            } else {
                minus_symbol <- ""
            }
            if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^9)
                    if (T) {
                        message("special divuvbeddy names_legend_samedims")
                        data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol, integral(),
                                                                        bold(nabla)[h] %.% 
                                                                        bar(paste(bold(u)[h], "'b'")),
                                                                        #" b-advection",
                                                                        " dV [", var1^4, " ", var2^-3,
                                                                        "] " %*% " ", 10^9),
                                                                   list(minus_symbol=minus_symbol, var1="m", var2="s"))
                        #names_legend_samedims[i] <- eval(substitute(expression(paste(minus_symbol,  integral(),
                        #                                                             bold(nabla)[h] %.% bar(paste(bold(u)[h], "'b'")))),
                        #                                            list(minus_symbol=minus_symbol))) # eddy
                    }
                }
            }
        } else if (any(varname == c("divuvbtot", "divuvbtot_meanint"))) {
            if (T) {
                message("convert divergence to convergence --> *-1")
                data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, -1)
                minus_symbol <- minus_symbol_dash
            } else {
                minus_symbol <- ""
            }
            if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 10^9)
                    if (T) {
                        message("set special label for divuvb_budget samedims")
                        data_infos[[i]][[vi]]$label <- substitute(paste(minus_symbol, integral(),
                                                                        bold(nabla)[h] %.% 
                                                                        bar(paste(bold(u)[h], "b")), 
                                                                        #" b-advection",
                                                                        " dV [", var1^4, " ", var2^-3,
                                                                        "] " %*% " ", 10^9),
                                                                   list(minus_symbol=minus_symbol, var1="m", var2="s"))
                        #names_legend_samedims[i] <- eval(substitute(expression(paste(minus_symbol, bold(nabla)[h] %.% 
                        #                                                             bar(paste(bold(u)[h], "b")))), 
                        #                                            list(minus_symbol=minus_symbol))) # total
                    }
                }
            }

        } else if (varname == "eke") {
            data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("EKE [m"^2, " s"^-2, "]"))))
            if (modes[i] == "fldint") {
                if (grepl("int", depths[i])) {
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "/")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e11)
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste(integral(), " EKE dV [m"^5, " s"^-2, "] " %*% "", 10^11))))
                }
            } else {
                if (T) {
                    message("eke m2 s-2 --> cm-2 s-2")
                    data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
                    data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 1e4)
                    data_infos[[i]][[vi]]$label <- eval(substitute(expression(paste("EKE [cm"^2, " s"^-2, "]"))))
                }
            }

        } else if (varname == "eke_over_tke") {
            data_infos[[i]][[vi]]$offset$operator <- c(data_infos[[i]][[vi]]$offset$operator, "*")
            data_infos[[i]][[vi]]$offset$value <- c(data_infos[[i]][[vi]]$offset$value, 100)
            data_infos[[i]][[vi]]$label <- "EKE [%]"
        
        # mpiom 
        } else if (varname == "lm_THO_as_time_slope") {
            if (all(levs == 6)) {
                data_infos[[i]][[vi]]$label <- "SST trend [°C/7k years]"
            } else {
                data_infos[[i]][[vi]]$label <- "potential temperature trend [°C/7k years]"
            }
        
        } else if (varname == "lm_SICOMO_as_time_slope") {
            data_infos[[i]][[vi]]$label <- "sea ice fraction trend [fraction/7k years]"

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

        } # finished define variable specific things
    
    } # for vi varnames per setting
} # for i nsettings
# finished setting variable specific things


# save data infos for later
message("\nsave data infos ...")
for (vi in seq_along(varnames_unique)) {
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
for (i in seq_len(nsettings)) {
    for (vi in seq_along(datas[[i]])) {
        if (!is.null(data_infos[[i]][[vi]]$offset)) {
            message("setting ", i, "/", nsettings, ": ", prefixes[i], " ", names_short[i], ":")
            for (oi in seq_along(data_infos[[i]][[vi]]$offset$operator)) {
                if (is.null(data_infos[[i]][[vi]]$offset$operator[oi]) ||
                    is.na(data_infos[[i]][[vi]]$offset$operator[oi])) {
                    stop("data_infos[[", i, "]][[", vi, "]]$offset$operator[", oi, "] not defined")
                }
                if (is.null(data_infos[[i]][[vi]]$offset$value[oi]) ||
                    is.na(data_infos[[i]][[vi]]$offset$value[oi])) {
                    stop("data_infos[[", i, "]][[", vi, "]]$offset$value[", oi, "] not defined")
                }
                cmd <- paste0("datas[[", i, "]][[", vi, "]] <- datas[[", i, "]][[", vi, "]] ", 
                              data_infos[[i]][[vi]]$offset$operator[oi], " ", 
                              data_infos[[i]][[vi]]$offset$value[oi])
                message("   run `", cmd, "` ...")
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
        plotname_suffix <- paste0(plotname_suffix, "_anom")
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
    message("\nsome settings have time dim AND `add_smoothed` = T AND some `n_mas` != 1 --> apply moving averages ...")
    datasma <- datas
    for (i in seq_len(nsettings)) {
        message(i, "/", nsettings, ": ", names_short[i], " ...")
        #if (seasonsp[i] == "Jan-Dec" && n_mas[i] != 1) { # applying moving average
        if (n_mas[i] != 1) {
            for (vi in seq_along(datas[[i]])) { 
                dims_of_var <- attributes(datas[[i]][[vi]])$dims # e.g. "time", "lon", "lat"
                timedimind <- which(dims_of_var == "time")
                if (length(timedimind) == 1) {
                    # how many data points per year?
                    midind <- length(dims[[i]]$time)/2 # middle of time series to avoid incomplete start/end of ts
                    yearinds <- which(dims[[i]]$timelt$year == dims[[i]]$timelt$year[midind]) # all time points equal middle year
                    npy <- length(yearinds) # how many time points eqial middle year
                    n_mas_fname[i] <- paste0("_ma", round(n_mas[i]/npy), "yr") # years
                    apply_dims <- 1:length(dim(datas[[i]][[vi]]))
                    message("   var ", vi, "/", length(datas[[i]]), ": ", names(datas[[i]])[vi], 
                            ": ntime = ", length(dims[[i]]$time), ", npy = ", npy, " --> n_ma/npy = ", 
                            n_mas[i], "/", npy, " = ", n_mas[i]/npy, " year running mean")
                    if (length(dims_of_var) == 1) { # variable has only 1 dim and its time
                        datasma[[i]][[vi]] <- stats::filter(datas[[i]][[vi]], filter=rep(1/n_mas[i], t=n_mas[i]))
                        # `forecast::ma(x, order=n_mas[i], centre=ifelse(n_mas[i] %% 2 == 0, F, T))` yields the same
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
                if (length(timedimind) == 1) {
                    attributes(datasma[[i]][[vi]])$n_ma <- n_mas[i]
                    attributes(datasma[[i]][[vi]])$npy <- npy
                }

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
if (any(sapply(lapply(lapply(dims, names), "==", "time"), any))) {
    
    if (!calc_monthly_and_annual_climatology) {
        message("\nsome settings have time dim but `calc_monthly_and_annual_climatology`=F --> ",
                "do not calc monthly climatology and annual means ...") 
    
    } else if (calc_monthly_and_annual_climatology) { 

        message("\nsome settings have time dim --> calc monthly climatology and annual means ...")
        datasmon <- datasan <- datas # allocate
        
        for (i in seq_len(nsettings)) {
            message(i, "/", nsettings, ": ", names_short[i], " ...")
            for (vi in seq_along(datas[[i]])) { # for all vars per setting
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
                        for (mi in seq_along(months_unique)) {
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
                        for (yi in seq_along(years_unique)) {
                            time_inds <- which(years == years_unique[yi])
                            # verbose
                            if (yi < 14 ||
                                yi >= (length(years_unique) - 14)) {
                                message(years_unique[yi], " (n=", length(time_inds), ") ", appendLF=F)
                                if (yi == 13) message("... ", appendLF=F)
                                if (yi == length(years_unique)) message()
                            }
                            # cmd
                            indslhs <- indsrhs <- rep(",", t=length(dims_of_var))
                            indslhs[timedimind] <- yi
                            indsrhs[timedimind] <- paste0("time_inds")
                            cmd1 <- paste0("tmp2 <- mean(")
                            # use smoothed input time series for annual means if possible 
                            if (F && exists("datasma") && !all(is.na(datasma[[i]]))) {
                                # doesnt make sense
                                cmd1 <- paste0(cmd1, "datasma")
                            } else {
                                cmd1 <- paste0(cmd1, "datas")
                            }
                            cmd1 <- paste0(cmd1, "[[", i, "]][[", vi, "]][")
                            if (length(datasan_dims) == 1) { # var has only time dim
                                cmd1 <- paste0(cmd1, paste(indsrhs, collapse=""), "], na.rm=T)")
                            } else {
                                cmd1 <- paste0(cmd1, paste(indsrhs, collapse=""), "]",
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
                        attr(datasan[[i]][[vi]], "npy") <- 1
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
    } # if calc_monthly_and_annual_climatology

} else { # if any setting has time dim
    #message("\nno time dim detected")
}
# if calc_monthly_and_annual_climatology finished calculating monthly means if applicable

## calculate temporal mean (long term mean; ltm)
if (any(sapply(lapply(lapply(dims, names), "==", "time"), any))) {
    datasltm <- datas
    for (i in seq_len(nsettings)) {
        if (i == 1) message("\nsome settings have time dim --> calc ltm ...")
        ltm_range <- paste0(dims[[i]]$time[1], " to ", dims[[i]]$time[length(dims[[i]]$time)])
        message(i, "/", nsettings, ": ", names_short[i], " (", ltm_range, ") ...")
        for (vi in 1:length(datas[[i]])) {
            if (any(attributes(datas[[i]][[vi]])$dims == "time")) { # if var has time dim
                message("   var ", vi, "/", length(datas[[i]]), ": ", names(datas[[i]])[vi], " ... ", appendLF=F)
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
                    if (length(apply_dims) <= 2) { # paste ltms along dims
                        message(paste(datasltm[[i]][[vi]], collapse=", "), " ", data_infos[[i]][[vi]]$units)
                    } else { # show just mean along all dims
                        message(mean(datasltm[[i]][[vi]]), " ", data_infos[[i]][[vi]]$units)
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


# interpolate data if any dim is irregular
if (exists("datasltm")) {
    # todo
} # if exsits("datalam")
# finished interpolate if any dim is irregular


# bilinear interpolation of data for smoother plot using fields::interp.surface.grid
if (any(sapply(lapply(lapply(dims, names), "==", "lon"), any)) &&
    any(sapply(lapply(lapply(dims, names), "==", "lat"), any)) &&
    bilinear_interp_factor > 1) {
    message("\n`bilinear_interp_factor` = ", bilinear_interp_factor, " > 1 (default) --> ",
            "interp data for smoother plot using fields::interp.surface.grid() ...")
    cnt <- 0 # just for `plot_suffix`
    for (i in seq_len(nsettings)) {
        for (vi in seq_along(datas[[i]])) {
            if (length(dim(datas[[i]][[vi]])) == 2) { # if variable has 2 dims
                dims_of_var <- attributes(datas[[i]][[vi]])$dims
                lengths_of_dims <- attributes(datas[[i]][[vi]])$dim
                lengths_of_dims_interp <- bilinear_interp_factor*lengths_of_dims
                x <- seq(min(dims[[i]][[dims_of_var[1]]], na.rm=T), 
                         max(dims[[i]][[dims_of_var[1]]], na.rm=T), 
                         l=lengths_of_dims_interp[1])
                y <- seq(min(dims[[i]][[dims_of_var[2]]], na.rm=T), 
                         max(dims[[i]][[dims_of_var[2]]], na.rm=T), 
                         l=lengths_of_dims_interp[2])
                message(names(datas)[i], " variable ", names(datas[[i]])[vi], 
                        " from ", dims_of_var[1], " x ", dims_of_var[2], 
                        " (", lengths_of_dims[1], " x ", lengths_of_dims[2], ") --> (", 
                        lengths_of_dims_interp[1], " x ", lengths_of_dims_interp[2], ") ...")
                message("range before = ", paste(range(datas[[i]][[vi]], na.rm=T), collapse=", "))
                datas[[i]][[vi]] <- fields::interp.surface.grid(obj=list(x=dims[[i]][[dims_of_var[1]]], 
                                                                         y=dims[[i]][[dims_of_var[2]]],
                                                                         z=datas[[i]][[vi]]),
                                                                grid.list=list(x=x, y=y))$z
                message("range after = ", paste(range(datas[[i]][[vi]], na.rm=T), collapse=", "))
                dims[[i]][[dims_of_var[1]]] <- as.vector(x)
                dims[[i]][[dims_of_var[2]]] <- as.vector(y)
                attributes(datas[[i]][[vi]]) <- list(dim=lengths_of_dims_interp, dims=dims_of_var)
                if (cnt == 0) {
                    cnt <- 1
                    plotname_suffix <- paste0(plotname_suffix, "_bil_x", bilinear_interp_factor)
                }
            } else {
                message("variable ", names(datas[[i]])[vi], " of setting ", names(datas)[i], 
                        " has ", length(dim(datas[[i]][[vi]])), " dims. must be 2 to run ",
                        "fields::interp.surface.grid()")
            }
        } # for vi
    } # for i
} # if bilinear_interp_factor != 1


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
        if (exists("varnames_uv")) zuv_samevars <- list()
        if (exists("datasma")) zma_samevars <- z_samevars
        dinds_samevars <- vinds_samevars <- z_samevars
        for (vi in seq_along(varnames_unique)) { # wind10, u10, v10
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
                    # check if varnames_uv was defined for setting i
                    if (exists("varnames_uv")) {
                        if (any(sapply(varnames_uv, "[[", "uv") == i)) {
                            uvind <- which(sapply(varnames_uv, "[[", "uv") == i)
                            if (length(uvind) != 1) {
                                stop("varnames_uv with index ", i, " was defined ", 
                                     length(uvind), " times. can only be defined once.")
                            }
                            message("   i ", i, ", vi ", vi, ", varind ", varind, ", varname ", varname, 
                                    ", uvind ", varnames_uv[[uvind]]$uv, ", uind ", varnames_uv[[uvind]]$u,
                                    ", vind ", varnames_uv[[uvind]]$v)
                            zuv[[length(zuv)+1]] <- list(u=datas[[varnames_uv[[uvind]]$u]][[1]],
                                                         v=datas[[varnames_uv[[uvind]]$v]][[1]])
                            names(zuv)[length(zuv)] <- names_short[i]
                        } else { # current varind of current setting ind not defined in varnames_uv
                            zuv[[length(zuv)+1]] <- NA
                        }
                    } # if varnames_uv exists
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
            if (exists("varnames_uv") && !all(is.na(zuv))) {
                zuv_samevars[[length(zuv_samevars)+1]] <- zuv
                names(zuv_samevars)[length(zuv_samevars)] <- varname
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

    if (!all(check_vec) && plot_samedims == F) {
        message("not all `check_vec` are true (i.e. z_samevars != z_samedims) but `plot_samedims` is false\n",
                "--> set all `check_vec` to true and do not plot z_samedims ...")
        check_vec[] <- T
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
            stop("provide `varnames_out_samedims = ", 
                 ifelse(length(varnames_out_samedims) > 1, "c(", ""), 
                 "\"", paste(varnames_out_samedims, collapse="\", \""), "\"", 
                 ifelse(length(varnames_out_samedims) > 1, ")", ""), 
                 "` in plot namelist for saving\nplots with vars of same dimlength-dimname-combinations")
        
        } else { # `varnames_out_samedims` is provided
            if (length(varnames_out_samedims) != length(z_samedims)) {
                stop("provided `varnames_out_samedims` = ",
                     ifelse(length(varnames_out_samedims) > 1, "c(", ""), 
                     "\"", paste(varnames_out_samedims, collapse="\", \""), "\"", 
                     ifelse(length(varnames_out_samedims) > 1, ")", ""), 
                     "` in plot namelist for saving plots with vars of same dimlength-dimname-combinations is of different length than `z_samedims`: ", 
                     length(varnames_out_samedims), " != ", length(z_samedims))
            }
        } # if varnames_out_samedims is missing or not

        if (add_legend && !exists("names_legend_samedims")) {
            stop("provide `names_legend_samedims` of length ", length(z_samedims))
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
            if (exists("varnames_uv")) {
                if (!is.null(zuv_samevars[[zname]])) {
                    zuv <- zuv_samevars[[zname]]
                } else {
                    zuv <- NA
                }
            }
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
            #names_legend_p <- names_legend[sapply(dinds, "[")]
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
            if (!exists("lwds_samedims")) {
                lwds_p <- lwds[sapply(dinds, "[")]
            } else {
                lwds_p <- lwds_samedims[sapply(dinds, "[")]
            }
            if (exists("datasma")) zma <- zma_samedims[[ploti]]
            if (exists("datasmon")) {
                zname_mon <- names(zmon_samedims)[ploti]
                zmon <- zmon_samedims[[ploti]]
                dmon <- dmon_samedims[[ploti]]
                dmoninds <- dmoninds_samedims[[ploti]]
                vmoninds <- vmoninds_samedims[[ploti]]
                names_legend_pmon <- names_legend_samedims[sapply(dmoninds , "[")]
                #names_legend_pmon <- names_legend[sapply(dmoninds, "[")]
            }
            if (exists("datasan")) {
                zname_an <- names(zan_samedims)[ploti]
                zan <- zan_samedims[[ploti]]
                dan <- dan_samedims[[ploti]]
                daninds <- daninds_samedims[[ploti]]
                vaninds <- vaninds_samedims[[ploti]]
                names_legend_pan <- names_legend_samedims[sapply(daninds , "[")]
                #names_legend_pan <- names_legend[sapply(daninds, "[")]
            }
            if (exists("datasltm")) {
                zname_ltm <- names(zltm_samedims)[ploti]
                zltm <- zltm_samedims[[ploti]]
                dltm <- dltm_samedims[[ploti]]
                dltminds <- dltminds_samedims[[ploti]]
                vltminds <- vltminds_samedims[[ploti]]
                names_legend_pltm <- names_legend_samedims[sapply(dltminds , "[")]
                #names_legend_pltm <- names_legend[sapply(dltminds, "[")]
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
        depth_fromsp_p <- depth_fromsp[sapply(dinds, "[")]
        depth_tosp_p <- depth_tosp[sapply(dinds, "[")]
        lev_fromsp_p <- lev_fromsp[sapply(dinds, "[")]
        lev_tosp_p <- lev_tosp[sapply(dinds, "[")]
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
            depth_fromsp_pmon <- depth_fromsp[sapply(dmoninds, "[")]
            depth_tosp_pmon <- depth_tosp[sapply(dmoninds, "[")]
            lev_fromsp_pmon <- lev_fromsp[sapply(dmoninds, "[")]
            lev_tosp_pmon <- lev_tosp[sapply(dmoninds, "[")]
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
            depth_fromsp_pan <- depth_fromsp[sapply(daninds, "[")]
            depth_tosp_pan <- depth_tosp[sapply(daninds, "[")]
            lev_fromsp_pan <- lev_fromsp[sapply(daninds, "[")]
            lev_tosp_pan <- lev_tosp[sapply(daninds, "[")]
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
            depth_fromsp_pltm <- depth_fromsp[sapply(dltminds, "[")]
            depth_tosp_pltm <- depth_tosp[sapply(dltminds, "[")]
            lev_fromsp_pltm <- lev_fromsp[sapply(dltminds, "[")]
            lev_tosp_pltm <- lev_tosp[sapply(dltminds, "[")]
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
            message("\nfinal z_", plot_groups[plot_groupi], "[[", ploti, "/", nplots, "]]: \"", zname, 
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
            #message("\nfinal z_", plot_groups[plot_groupi], "[[", ploti, "/", nplots, "]]: \"", zname, 
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
                tlabcex <- 1
                #tlabcex <- 0.8
                monlim <- range(tlimlt$mon+1)
                anlim <- range(tlimlt$year+1900)

                # time labels
                tlablt <- as.POSIXlt(pretty(tlimlt, n=10)) # todo: this does not work with large negative years, e.g. -800000 (800ka) 
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
                                message("remove ", length(inds), " tlab dates ", paste(tlablt[inds], collapse=", "))
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
                                message("remove ", length(inds), " tlab dates ", paste(tlablt[inds], collapse=", "))
                                tlablt <- tlablt[-inds]
                            }
                        }
                    }
                }
                if (length(tlablt) == 0) stop("removed all tlablt")
                tatn <- as.numeric(tlablt)
                
                # modify time axis labels YYYY-MM-DD depending on range covered:
                if (!exists("tunit")) {
                    tunit <- "time" # default
                    update_tunit <- T
                } else { # tunit was provided by user
                    update_tunit <- F
                }
                tlabsrt <- 0 # default
                if (length(tlablt) > 1) {
                    tlab_dt_secs <- as.numeric(diff(range(tlablt[1:2])), units="secs") # dt between time labels
                } else {
                    tlab_dt_secs <- 1 # sec; placeholder
                }
                # case 1: dt_lab is shorter than 1 month: YYYY-MM-DD
                if (tlab_dt_secs < 30*24*60*60) { 
                    tlablt <- paste0(tlablt$year+1900, "-", tlablt$mon+1, "-", tlablt$mday)
                    tlabsrt <- 45
                # case 2: dt_lab is longer than 1 month and shorter than 0.5 year: YYYY-MM
                } else if (tlab_dt_secs >= 30*24*60*60 && tlab_dt_secs <= 180*24*60*60) { 
                    tlablt <- paste0(tlablt$year+1900, "-", tlablt$mon+1) 
                    if (update_tunit) tunit <- "year"
                # case 3: dt_lab longer than 0.5 year: YYYY
                } else if (tlab_dt_secs > 180*24*60*60) { 
                    tlablt <- tlablt$year+1900
                    if (update_tunit) tunit <- "year"
                }
                # from here, tlablt is not of type POSIX* anymore!
                if (any(duplicated(tlablt))) {
                    inds <- which(duplicated(tlablt))
                    tlablt <- tlablt[-inds]
                    tatn <- tatn[-inds]
                }
                message("final tlablt = ", paste(tlablt, collapse=", "))

                # if all dates < 0 AD, use "abs(dates) BP" instead
                if (all(anlim <= 0)) {
                    message("all times are <= 0 AD --> use `abs(times)` for time labels instead ...")
                    neg_inds <- which(tlablt < 0)
                    tlablt[neg_inds] <- abs(tlablt[neg_inds])
                    if (update_tunit) {
                        message("`time_ref` = \"", time_ref, "\" --> change `tunit` = \"", tunit, "\" to ")
                        if (!is.na(time_ref)) {
                            tunit <- paste0("year before ", time_ref)
                        } else {
                            tunit <- "year before `time_ref`"
                        }
                        message("\"", tunit, "\"")
                    }
                }
                message("final tunit = \"", tunit, "\"")
                
                # use years from time for plots versus years 
                #anlab <- tlablt$year+1900
                #anat <- as.numeric(as.POSIXct(paste0(anlab, "-1-1"), o="1970-1-1", tz="UTC"))
                anlab <- tlablt
                #anat <- tatn # --> annual plots use numeric years as dim, not POSIX
                anat <- tlablt

            } # if any(ntime_per_setting > 1)

        } # if any data has time axis
     
        # todo: find common axes values for all dims 

        if (F) {
            message("\nspecial tlim, tatn, tablt for phd stuff ...")
            tlim <- c(-694310400, 1259625600)
            tatn <- c(-788918400, -631152000, -473385600, -315619200, -157766400, 
                      0, 157766400, 315532800, 473385600, 631152000, 788918400, 946684800, 
                      1104537600, 1262304000)
            tlablt <- c(1945, 1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990, 
                        1995, 2000, 2005, 2010)
        }
        # finished getting time axis labels
        
        # set depth axis label
        if (any(sapply(lapply(z, attributes), "[[", "dims") == "depth")) {
            depthunit <- sapply(dims, "[[", "depthunit")
            depthunit <- unique(depthunit)
            if (length(depthunit) != 1) {
                stop("detected different depth units, implement this")
            }
        }
        # finished getting depth axis labels

        # set lev axis label
        if (any(sapply(lapply(z, attributes), "[[", "dims") == "lev")) {
            levunit <- sapply(dims, "[[", "levunit")
            levunit <- unique(levunit)
            if (length(levunit) != 1) {
                stop("detected different lev units, implement this")
            }
        }
        # finished getting lev axis labels
        
        # verbose
        message("\nz:")
        cat(capture.output(str(z)), sep="\n")
        message("d:")
        cat(capture.output(str(d)), sep="\n")

        # special: reccp2 time series output in one nc file
        if (plot_groups[plot_groupi] == "samevars" && all(grepl("reccap2", prefixes)) && 
            length(unique(modes)) == 1 && length(unique(areas)) == 1) {
            fout <- paste0(host$workpath, "/data/reccap2-ocean/reccap2-ocean_", length(models), "_models_", 
                           modes[1], "_", zname, "_", areas[1], "_Jan-Dec_", paste(format(tlimct, "%Y"), collapse="-"), ".nc")
            if (!file.exists(fout)) {
                message("\nspecial: save reccap2 post")
                modeldim <- ncdf4::ncdim_def(name="model", units="", vals=seq_along(z))
                fromto <- min(fromsf):max(tosf)
                tvals <- paste0(rep(fromto, e=12), "-", rep(1:12, t=length(fromto)), "-", rep(15, t=length(fromto)*12)) # all monthly data at same time points
                tvals <- as.POSIXct(tvals, tz="UTC")
                tdim <- ncdf4::ncdim_def(name="time", units="seconds since 1970-1-1", vals=as.numeric(tvals))
                arr <- array(NA, dim=c(nsettings, length(tvals)))
                for (i in seq_along(z)) {
                    for (ti in seq_along(z[[i]])) {
                        ind <- which(format(tvals, "%Y-%m") == format(d$time[[i]][ti], "%Y-%m"))
                        if (length(ind) != 1) stop("this should not happen")
                        arr[i,ind] <- z[[i]][ti]
                    }
                }
                arr_min <- apply(arr, 2, min, na.rm=T)
                arr_max <- apply(arr, 2, max, na.rm=T)
                arr_mean <- apply(arr, 2, mean, na.rm=T)
                arr_median <- apply(arr, 2, median, na.rm=T)
                ncvar <- ncdf4::ncvar_def(name="fgco2_mon", units="PgC yr-1", dim=list(modeldim, tdim), missval=NA)
                ncvar_min <- ncdf4::ncvar_def(name="fgco2_mon_min", units="PgC yr-1", dim=tdim, missval=NA)
                ncvar_max <- ncdf4::ncvar_def(name="fgco2_mon_max", units="PgC yr-1", dim=tdim, missval=NA)
                ncvar_mean <- ncdf4::ncvar_def(name="fgco2_mon_mean", units="PgC yr-1", dim=tdim, missval=NA)
                ncvar_median <- ncdf4::ncvar_def(name="fgco2_mon_median", units="PgC yr-1", dim=tdim, missval=NA)
                anvals <- as.POSIXct(paste0(fromto, "-1-1"), tz="UTC")
                andim <- ncdf4::ncdim_def(name="years", units="seconds since 1970-1-1", vals=as.numeric(anvals))
                arr_an <- array(NA, dim=c(nsettings, length(anvals)))
                for (i in seq_along(zan)) {
                    for (ti in seq_along(zan[[i]])) {
                        ind <- which(format(anvals, "%Y") == dan$year[[i]][ti])
                        if (length(ind) != 1) stop("this should not happen")
                        arr_an[i,ind] <- zan[[i]][ti]
                    }
                }
                arr_an_min <- apply(arr_an, 2, min, na.rm=T)
                arr_an_max <- apply(arr_an, 2, max, na.rm=T)
                arr_an_mean <- apply(arr_an, 2, mean, na.rm=T)
                arr_an_median <- apply(arr_an, 2, median, na.rm=T)
                ncvar_an <- ncdf4::ncvar_def(name="fgco2_an", units="PgC yr-1", dim=list(modeldim, andim), missval=NA)
                ncvar_an_min <- ncdf4::ncvar_def(name="fgco2_an_min", units="PgC yr-1", dim=andim, missval=NA)
                ncvar_an_max <- ncdf4::ncvar_def(name="fgco2_an_max", units="PgC yr-1", dim=andim, missval=NA)
                ncvar_an_mean <- ncdf4::ncvar_def(name="fgco2_an_mean", units="PgC yr-1", dim=andim, missval=NA)
                ncvar_an_median <- ncdf4::ncvar_def(name="fgco2_an_median", units="PgC yr-1", dim=andim, missval=NA)
                outnc <- ncdf4::nc_create(fout, vars=list(ncvar, ncvar_min, ncvar_max, ncvar_mean, ncvar_median,
                                                          ncvar_an, ncvar_an_min, ncvar_an_max, ncvar_an_mean, ncvar_an_median), force_v4=T)
                ncdf4::ncvar_put(outnc, ncvar, arr)
                ncdf4::ncvar_put(outnc, ncvar_min, arr_min)
                ncdf4::ncvar_put(outnc, ncvar_max, arr_max)
                ncdf4::ncvar_put(outnc, ncvar_mean, arr_mean)
                ncdf4::ncvar_put(outnc, ncvar_median, arr_median)
                ncdf4::ncvar_put(outnc, ncvar_an, arr_an)
                ncdf4::ncvar_put(outnc, ncvar_an_min, arr_an_min)
                ncdf4::ncvar_put(outnc, ncvar_an_max, arr_an_max)
                ncdf4::ncvar_put(outnc, ncvar_an_mean, arr_an_mean)
                ncdf4::ncvar_put(outnc, ncvar_an_median, arr_an_median)
                for (i in seq_along(z)) {
                    ncdf4::ncatt_put(outnc, "model", i, models[i])
                }
                ncdf4::nc_close(outnc)
            } # if fout does not exist
        } # special reccap2


        ## plot `datas` (`datas` always exists; no exists() check necessary)
        message("\n****************** plot datas z_* ***************************")
        #stop("asd")
        
        message("\nz_", plot_groups[plot_groupi], "[[", ploti, "/", nplots, "]]: \"", zname, 
                "\" has ", ndims, " dim", ifelse(ndims > 1, "s", ""), ": \"", 
                paste(dim_names, collapse="\", \""), "\". check if this case is defined ...")

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
                    #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                    point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/w,SMOW"], " [‰]"))
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
                any(zname == c("lm_temp2_as_time_slope", "lm_tsurf_as_time_slope", "lm_aprt_as_time_slope"))) {
                point_data_fname <- "bartlein_etal_2011"
                if (any(zname == c("lm_temp2_as_time_slope", "lm_tsurf_as_time_slope"))) {
                    point_data_varname <- "mat_trend"
                    #point_data_label <- "B11 MAT trend [°C/6k years]" 
                    point_data_label <- "Proxy temperature trend [°C/6k years]"
                    point_data_legend <- "B11"
                    tmp <- bartlein_etal_2011$mat_MH_minus_PI
                } else if (zname == "lm_aprt_as_time_slope") {
                    point_data_varname <- "map_trend"
                    #point_data_label <- "B11 MAP trend [mm/a/6k years]" 
                    point_data_label <- "Proxy precipitation trend [mm/a/6k years]" 
                    point_data_legend <- "B11"
                    tmp <- bartlein_etal_2011$map_MH_minus_PI
                }
                # convert anomaly (MH minus PI) to trend; e.g. anom = 2K --> trend = -2K/6k years
                tmp$data <- tmp$data*-1 
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
                for (i in seq_along(tmp3)) {
                    tmp4 <- list(lon=tmp2$lonlat$Var1[i], lat=tmp2$lonlat$Var2[i], 
                                 time=make_posixlt_origin(-6000), # just placeholder
                                 colno=1, pchno=1,
                                 varname=point_data_varname, fname=point_data_fname,
                                 label=point_data_label, legend=point_data_legend)
                    tmp4[[point_data_varname]] <- tmp2$data[i]
                    tmp3[[i]] <- tmp4
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
                for (i in seq_along(tmp)) { # add the linear trend as data point
                    tmp[[i]]$time_ts <- tmp[[i]]$time # save original time series time
                    tmp[[i]]$time <- make_posixlt_origin(-6000) # just placeholder
                    tmp[[i]][[point_data_varname]] <- tmp[[i]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                    tmp[[i]]$varname <- point_data_varname 
                    tmp[[i]]$fname <- point_data_fname
                    tmp[[i]]$label <- point_data_label
                    tmp[[i]]$legend <- point_data_legend
                    tmp[[i]]$colno <- 1 # 1 2
                    tmp[[i]]$pchno <- 2
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add kaufman et al. 2020 temp12k to point_data
                rm(tmp)
            } # if kaufman_etal_2020_temp12k
            
            if (T && exists("global_holocene_lipd_precip") && zname == "lm_aprt_as_time_slope") {
                point_data_fname <- "global_holocene_lipd_precip"
                point_data_varname <- "precipitation_trend"
                point_data_label <- "Proxy precipitation trend [mm/a/6k years]" # mean annual temperature
                point_data_legend <- "LiPD"
                tmp <- global_holocene_lipd_precip
                for (i in seq_along(tmp)) { # add the linear trend as data point
                    tmp[[i]]$time_ts <- tmp[[i]]$time # save original time series time
                    tmp[[i]]$time <- make_posixlt_origin(-6000) # just placeholder for point data
                    tmp[[i]][[point_data_varname]] <- tmp[[i]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                    tmp[[i]]$varname <- point_data_varname 
                    tmp[[i]]$fname <- point_data_fname
                    tmp[[i]]$label <- point_data_label
                    tmp[[i]]$legend <- point_data_legend
                    tmp[[i]]$colno <- 1 # 1 2
                    tmp[[i]]$pchno <- 2
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add global_holocene_lipd_precip to point_data
                rm(tmp)
            } # if global_holocene_lipd_precip
            
            if (T && exists("kaufman_etal_2020_temp12k_d18o") && 
                any(zname == c("lm_wisoaprt_d_post_as_time_slope"))) {
                point_data_fname <- "kaufman_etal_2020_temp12k_d18o"
                point_data_legend <- "Temp12k 1.0.0"
                point_data_varname <- "d18O_trend"
                #point_data_label <- expression(paste("Iso2k ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/nonp,SMOW"], " [‰]"))
                point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/w,SMOW"], " [‰]"))
                #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                tmp <- kaufman_etal_2020_temp12k_d18o
                for (i in seq_along(tmp)) { # add the linear trend as data point
                    tmp[[i]]$time_ts <- tmp[[i]]$time # save original time series time
                    tmp[[i]]$time <- make_posixlt_origin(-6000) # placeholder for trend
                    tmp[[i]][[point_data_varname]] <- tmp[[i]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                    tmp[[i]]$varname <- point_data_varname 
                    tmp[[i]]$fname <- point_data_fname
                    tmp[[i]]$label <- point_data_label
                    tmp[[i]]$legend <- point_data_legend
                    tmp[[i]]$colno <- 1
                    tmp[[i]]$pchno <- 1
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add data to point_data
                rm(tmp)
            } # if konecky_etal_2020_iso2k_d18o_precip
            
            if (T && exists("konecky_etal_2020_iso2k_d18o_precip") && 
                any(zname == c("wisoaprt_d" 
                               #, "lm_wisoaprt_d_post_as_time_slope" # d18o trends are already included in temp12k
                               ))) {
                point_data_fname <- "iso2k-precip"
                point_data_legend <- "Iso2k 1.0.0"
                if (zname == "wisoaprt_d") {
                    point_data_varname <- "d18Op"
                } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                    point_data_varname <- "d18Op_trend"
                }
                #point_data_label <- expression(paste("Iso2k ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/nonp,SMOW"], " [‰]"))
                point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/w,SMOW"], " [‰]"))
                #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                tmp <- konecky_etal_2020_iso2k_d18o_precip
                for (i in seq_along(tmp)) { # add the linear trend as data point
                    tmp[[i]]$time_ts <- tmp[[i]]$time # save original time series time
                    if (zname == "wisoaprt_d") {
                        # nothing to do
                    } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                        tmp[[i]]$time <- make_posixlt_origin(-6000) # placeholder for trend
                        tmp[[i]][[point_data_varname]] <- tmp[[i]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                    }
                    tmp[[i]]$varname <- point_data_varname 
                    tmp[[i]]$fname <- point_data_fname
                    tmp[[i]]$label <- point_data_label
                    tmp[[i]]$legend <- point_data_legend
                    tmp[[i]]$colno <- 2 #2 4
                    tmp[[i]]$pchno <- 2 #2 4
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add data to point_data
                rm(tmp)
            } # if konecky_etal_2020_iso2k_d18o_precip
            
            if (T && exists("konecky_etal_2020_iso2k_d18o_nonprecip") && 
                any(zname == c(#"wisoaprt_d"
                               "lm_wisoaprt_d_post_as_time_slope"
                               ))) {
                point_data_fname <- "iso2k-nonprecip"
                point_data_legend <- "Iso2k-nonprecip"
                if (zname == "wisoaprt_d") {
                    point_data_varname <- "d18Ononp"
                    #point_data_varname <- "d18Ononp_scaled"
                } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                    point_data_varname <- "d18Ononp_trend"
                    #point_data_varname <- "d18Ononp_scaled_trend"
                }
                #point_data_label <- expression(paste("Iso2k ", delta^{18}, "O"["nonp,SMOW"], " [‰]"))
                #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/nonp,SMOW"], " [‰]"))
                point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/w,SMOW"], " [‰]"))
                tmp <- konecky_etal_2020_iso2k_d18o_nonprecip
                for (i in seq_along(tmp)) { # add the linear trend as data point
                    tmp[[i]]$time_ts <- tmp[[i]]$time # save original time series time
                    if (zname == "wisoaprt_d") {
                        # nothing to do
                        if (F) {
                            if (i == 1) message("use timmean of konecky_etal_2020_iso2k_d18o_nonprecip ...")
                            tmp[[i]]$time <- mean(tmp[[i]]$time, na.rm=T)
                            tmp[[i]][[point_data_varname]] <- mean(tmp[[i]], na.rm=T)
                        }
                    } else  if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                        tmp[[i]]$time <- make_posixlt_origin(-6000) # placeholder for trend
                        tmp[[i]][[point_data_varname]] <- tmp[[i]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                    }
                    tmp[[i]]$varname <- point_data_varname 
                    tmp[[i]]$fname <- point_data_fname
                    tmp[[i]]$label <- point_data_label
                    tmp[[i]]$legend <- point_data_legend
                    tmp[[i]]$colno <- 3 #2 3
                    tmp[[i]]$pchno <- 3 #2 3
                }
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add data to point_data
                rm(tmp)
            } # if konecky_etal_2020_iso2k_d18o_nonprecip
            
            if (T && exists("comas_bru_etal_2020_sisal_d18o_precip") && 
                any(zname == c("wisoaprt_d", "lm_wisoaprt_d_post_as_time_slope"))) {
                point_data_fname <- "sisal"
                #point_data_legend <- "Speleothems (SISAL 2.0)"
                point_data_legend <- "SISAL 2.0"
                if (zname == "wisoaprt_d") {
                    point_data_varname <- "d18O_w_smow"
                } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                    point_data_varname <- "d18O_w_smow_trend"
                }
                #point_data_label <- expression(paste("SISAL ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/nonp,SMOW"], " [‰]"))
                point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/w,SMOW"], " [‰]"))
                tmp <- comas_bru_etal_2020_sisal_d18o_precip
                for (i in seq_along(comas_bru_etal_2020_sisal_d18o_precip)) {
                    tmp[[i]]$time_ts <- tmp[[i]]$time # save original time series time
                    timeinds <- seq_along(tmp[[i]]$time) # default: all
                    if (F) { 
                        if (i == 1) message("use only PI from comas_bru_etal_2020_sisal_d18o_precip")
                        # PI mean: cauquoin et al. 2019: "defined as the interval 1850–1990 CE" 
                        #--> from 100 before 1950 CE to 40 from 1950 CE
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
                            tmp[[i]]$from <- min(tmp[[i]]$time$year + 1900)
                            tmp[[i]]$to <- max(tmp[[i]]$time$year + 1900)
                            tmp[[i]][["lm_label"]] <- tmp[[i]][[point_data_varname]]$lm_label
                            # next lines overwrite input data
                            tmp[[i]]$time <- make_posixlt_origin(-6000) # placeholder for trend
                            tmp[[i]][[point_data_varname]] <- 
                                tmp[[i]][[point_data_varname]]$lm_slope_per_year*6000 # trend/yr --> trend/6k yrs
                        }
                        tmp[[i]]$varname <- point_data_varname
                        tmp[[i]]$fname <- point_data_fname
                        tmp[[i]]$label <- point_data_label
                        tmp[[i]]$legend <- point_data_legend
                        tmp[[i]]$colno <- 2 #2 3
                        tmp[[i]]$pchno <- 2 #2 3
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
            
            if (T && exists("meyer_etal") && 
                any(zname == c("wisoaprt_d", "lm_wisoaprt_d_post_as_time_slope"))) {
                point_data_fname <- point_data_legend <- "PLOT"
                if (zname == "wisoaprt_d") {
                    point_data_varname <- "d18Odiatom_scaled"
                } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                    point_data_varname <- "d18Odiatom_scaled_trend"
                }
                #point_data_label <- expression(paste("PLOT ", delta^{18}, "O"["diatom,SMOW"], " [‰]"))
                #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/nonp,SMOW"], " [‰]"))
                point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/w,SMOW"], " [‰]"))
                if (T && any(names(meyer_etal$data) == "emanda") && exists("kostrova_etal_2021")) {
                    message("replace meyer et al. xlsx emanda with kostrova et al. 2021 emanda ...")
                    emandaind <- which(names(meyer_etal$data) == "emanda")
                    inds <- seq_along(kostrova_etal_2021$data$time)
                    if (F) {
                        message("  --> use only younger than 10k BP ...")
                        inds <- which(kostrova_etal_2021$data$time$year + 1900 > -10000)
                    } else if (T) {
                        message("  --> use only younger than 7k BP ...")
                        inds <- which(kostrova_etal_2021$data$time$year + 1900 > -7000)
                    }
                    meyer_etal$data[[emandaind]]$data <- list(d18o_corr_perm=kostrova_etal_2021$data$d18o_corr_perm[inds],
                                                              timelt=kostrova_etal_2021$data$time[inds])
                    meyer_etal$data[[emandaind]]$data$time <- as.POSIXct(meyer_etal$data[[emandaind]]$data$timelt)
                    meyer_etal$data[[emandaind]]$loc <- kostrova_etal_2021$loc
                    meyer_etal$data[[emandaind]]$ref <- kostrova_etal_2021$ref
                    meyer_etal$data[[emandaind]]$text <- kostrova_etal_2021$text
                }
                # add swann et al. 2010 from pangaea to meyer et al xlsx at elgygytgyn
                if (exists("pg")) {
                    varind <- which(names(pg) == "d18o_w_smow")
                    if (T && any(names(pg[[varind]]) == "swann_etal_2010")) {
                        doiind <- which(names(pg[[varind]]) == "swann_etal_2010")
                        eventind <- 1
                        message("add d18o_smow pangaea event \"", names(pg[[varind]][[doiind]])[eventind], 
                                "\" from swann_etal_2010 to meyer et al xlsx ...")
                        inds <- seq_along(pg[[varind]][[doiind]][[eventind]]$dims$time)
                        if (F) {
                            message("  --> only use younger than 10k BP ...")
                            inds <- which(pg[[varind]][[doiind]][[eventind]]$dims$time$year + 1900 > -10000)
                        } else if (T) {
                            message("  --> only use younger than 7k BP ...")
                            inds <- which(pg[[varind]][[doiind]][[eventind]]$dims$time$year + 1900 > -7000)
                        }
                        meyer_etal$data[[length(meyer_etal$data) + 1]] <- list(data=list(timelt=pg[[varind]][[doiind]][[eventind]]$dims$time[inds],
                                                                                         d18o_corr_perm=pg[[varind]][[doiind]][[eventind]]$data[inds]),
                                                                               ref="Swann et al. 2010",
                                                                               loc="El’gygytgyn (F)",
                                                                               lon=pg[[varind]][[doiind]][[eventind]]$lon,
                                                                               lat=pg[[varind]][[doiind]][[eventind]]$lat,
                                                                               text="F: El’gygytgyn (Swann et al. 2010)")
                        meyer_etal$data[[length(meyer_etal$data)]]$data$time <- as.POSIXct(meyer_etal$data[[length(meyer_etal$data)]]$data$timelt)
                        names(meyer_etal$data)[length(meyer_etal$data)] <- "elgygytgyn_swann_etal_2010"
                    }
                }
                # calc lm of scaled PLOT time series
                tmp <- vector("list", l=length(meyer_etal$data))
                for (i in seq_along(tmp)) { # loop through lakes
                    tmp[[i]] <- list(PLOT_lake=names(meyer_etal$data)[i],
                                     ref=meyer_etal$data[[i]]$ref,
                                     loc=meyer_etal$data[[i]]$loc,
                                     lon=meyer_etal$data[[i]]$lon,
                                     lat=meyer_etal$data[[i]]$lat,
                                     varname=point_data_varname,
                                     fname=point_data_fname,
                                     label=point_data_label,
                                     legend=point_data_legend,
                                     colno=4, pchno=4)
                    tmp[[i]]$time_ts <- meyer_etal$data[[i]]$data$timelt # save original time series time
                    tmp[[i]]$from <- min(meyer_etal$data[[i]]$data$timelt$year + 1900)
                    tmp[[i]]$to <- max(meyer_etal$data[[i]]$data$timelt$year + 1900)
                    
                    # calc lm
                    x <- meyer_etal$data[[i]]$data$time
                    y <- base::scale(meyer_etal$data[[i]]$data$d18o_corr_perm)
                    message("calc lm of lake ", tmp[[i]]$PLOT_lake, " of `meyer_etal` data from ",
                            min(x), " to ", max(x), " for `point_data` ...")
                    if (zname == "wisoaprt_d") {
                        tmp[[i]]$time <- x
                        tmp[[i]][[point_data_varname]] <- y
                    } else if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                        lm <- lm(y ~ x)
                        lm_summary <- summary(lm)
                        pval <- lm_summary$coefficients[2,4] # get_pval(lm)
                        message("--> trend pval = ", pval, " ", appendLF=F)
                        if (pval > 0.01) {
                            message("> 0.01 --> ignore this lake") 
                            tmp[[i]] <- NA
                        } else {
                            nyears <- diff(range(as.POSIXlt(x)$year)) # --> zero if all data points from same year
                            slope_per_year <- diff(range(lm$fitted.values))/nyears
                            if (lm$coefficients[2] < 0) slope_per_year <- -1*slope_per_year
                            tmp[[i]]$time <- make_posixlt_origin(-6000) # just placeholder
                            tmp[[i]][[point_data_varname]] <- slope_per_year*6000 # trend/yr --> trend/6k yrs
                            lm_label <- paste0("r=", round(sqrt(lm_summary$r.squared), 2), ", p")
                            if (pval < 1e-4) {
                                lm_label <- paste0(lm_label, "<1e-4")
                            } else {
                                lm_label <- paste0(lm_label, "=", round(pval, 4)) 
                            }
                            lm_label <- paste0(lm_label, ", df=", lm_summary$fstatistic["dendf"])
                            message("--> slope_per_year = ", slope_per_year, " --> ", lm_label)
                            tmp[[i]]$lm_label <- lm_label
                        }
                    }
                } # for i
                nainds <- which(is.na(tmp))
                if (length(nainds) > 0) tmp <- tmp[-nainds]
                message("add ", length(tmp), " ", point_data_varname, " from ", 
                        point_data_fname, " to point_data ...")
                point_data <- c(point_data, tmp) # add data to point_data
                rm(tmp)
            } # if meyer_etal
            
            if (T && exists("pg")) {
                if (zname == "wisoaprt_d" && any(names(pg) == "d18o_w_smow")) {
                    point_data_varname <- "d18o_w_smow"
                    #point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p,SMOW"], " [‰]"))
                    point_data_label <- expression(paste("Proxy ", delta^{18}, "O"["p/w,SMOW"], " [‰]"))
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
                    #options(width=200); data.frame(point_data_fname, point_data_labelp)
                }
                message("--> ", length(point_data), " point_data defined for zname = \"", 
                        zname, "\": \"", 
                        paste(unique(sapply(point_data, "[[", "fname")), collapse="\", \""), "\"") 

                # find obs and model data points from same area and interpolate
                message("\nfind obs and model data points from same area ...")
                xp <- yp <- z
                for (i in seq_along(z)) { # for all settings
                    xp[[i]] <- yp[[i]] <- NA # default
                    # reduce data: use only locations within lon,lat lims
                    z_lonlim <- range(d$lon[[i]]); z_latlim <- range(d$lat[[i]])
                    message("\n****************************************************\n",
                            "setting ", i, "/", length(z), ": ", names_short[i], 
                            " lon,lat-data within area ", areas_p[i], ": ",
                            z_lonlim[1], " to ", z_lonlim[2], "° lon ", 
                            z_latlim[1], " to ", z_latlim[2], "° lat ...")
                    point_data_lons <- sapply(point_data, "[[", "lon")
                    point_data_lats <- sapply(point_data, "[[", "lat")
                    message("--> check ", length(point_data), 
                            " point_datap locations with lon,lat-coords ", 
                            min(point_data_lons), " to ", 
                            max(point_data_lons), "° lon and ", 
                            min(point_data_lats), " to ", 
                            max(point_data_lats), "° lat ...")
                    lonlatinds <- which(point_data_lons >= z_lonlim[1] & 
                                        point_data_lons <= z_lonlim[2] &
                                        point_data_lats >= z_latlim[1] & 
                                        point_data_lats <= z_latlim[2])
                    message("--> found ", length(lonlatinds), "/", length(point_data), 
                            " point_datap locations within model data area")
                        
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
                        message("--> check for duplicate point_data locations ...")
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
                                                   paste(unique(paste0(fnames, "_", varnames)), collapse="_"),
                                                   ".pdf")
                                if (!file.exists(plotname)) { # not already covered before
                                    message("lon,lat-combination ", j, "/", length(point_datap), 
                                            " = ", lons[j], "° lon and ", lats[j], "° lat occurs ", 
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
                                        if (length(x[[k]]) == 1) {
                                            points(x[[k]], y[[k]], col=k)
                                        } else {
                                            lines(x[[k]], y[[k]], col=k, lty=k)
                                        }
                                    }
                                    legend <- paste0("i=", inds, "_", fnames, "_", varnames)
                                    legend("topleft", legend=legend, col=seq_along(inds), lty=seq_along(inds))
                                    box()
                                    dev.off()
                                    write(plotname, file=lastfiles_plot_fname, append=T)
                                    cnt <- cnt + 1
                                    #if (cnt == 9) stop("asd")
                                } # if plot not already existing
                            } # if duplicated lon,lat-combi
                        } # for j point_datap locations
                        #stop("asd")

                        # if point data has time dim, calc temporal means of point data (since 
                        # here is the lon,lat section and not lon,lat,time)
                        if (any(seasonsp_p[i] == names(known_seasons))) { # seas check 1/3
                            season_inds <- known_seasons[[
                                            which(seasonsp_p[i] == names(known_seasons))
                                                        ]]$inds
                        } else {
                            stop("this should not happen here")
                        }
                        
                        # apply seasonal subset and apply temporal mean to all point data coords within lon,lat-lims
                        message("--> calc seasonal mean seasonsp_p[", i, "] = ", seasonsp_p[i], " of ",
                                length(point_datap), " point_datap locations ...")
                        message("--> use months ", paste(season_inds, collapse=","))
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
                            message("--> interp model data to ", length(point_datap), " non-NA ", 
                                    seasonsp_p[i], " point_datap locations from ", 
                                    as.POSIXlt(rangetot, o="1970-1-1")[1], " to ", 
                                    as.POSIXlt(rangetot, o="1970-1-1")[2], " ...")
                            message("--> interp_method = ", interp_method, " ...")
                            model_data <- rep(NA, t=length(point_datap))
                            for (j in seq_along(point_datap)) {
                                if (!any(search() == "package:pracma")) suppressMessages(library(pracma))
                                if (interp_method == "barylag2d") {
                                    stop("not yet")
                                } else {
                                    # pracma::interp2 uses x,y in flipped order
                                    model_data[j] <- pracma::interp2(x=d$lat[[i]], y=d$lon[[i]], Z=z[[i]],
                                                                     xp=point_datap[[j]]$lat, 
                                                                     yp=point_datap[[j]]$lon, 
                                                                     method=interp_method) # result is NaN if model input is NA
                                }
                            } # for j in point data lon,lat-subset data
                            
                            # save in xp and yp for plot
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
                            #attributes(xp[[i]])$origin <- sapply(lapply(sapply(point_datap, "[[", "time"), attributes), "[[", "origin")
                            yp[[i]] <- model_data # possibly NA data

                            if (length(xp[[i]]) != length(yp[[i]])) stop("this should not happen")
                        
                            # make attributes sticky (not removed by subsetting)
                            if (!any(search() == "package:sticky")) suppressPackageStartupMessages(library(sticky))
                            xp[[i]] <- sticky::sticky(xp[[i]])
                            yp[[i]] <- sticky::sticky(yp[[i]])

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
                } # for i nsettings


                ## scatter plot: different model setups (yp) vs respective obs (xp)
                if (!all(sapply(lapply(yp, is.na), all))) {

                    # survived point_datap
                    xp_colno <- lapply(lapply(xp, attributes), "[[", "colno") # list of length nsettings
                    xp_pchno <- lapply(lapply(xp, attributes), "[[", "pchno")
                    xp_fname <- lapply(lapply(xp, attributes), "[[", "fname")
                    xp_fname_unique <- lapply(xp_fname, unique)
                    xp_varname <- lapply(lapply(xp, attributes), "[[", "varname")
                    xp_varname_unique <- lapply(xp_varname, unique)
                    xp_legend <- lapply(lapply(xp, attributes), "[[", "legend")
                    xp_legend_unique <- lapply(xp_legend, unique)
                    xp_tlims <- lapply(lapply(xp, attributes), "[[", "ranges") # list of length nsettings, each having dims = (2,nxp) 
                  
                    # special: save survived point_datap as latex table
                    if (T && length(xp) == 1) {
                        message("\nspecial: save survived point_datap as latex table")
                        
                        # table a1 jqs paper
                        if (T && any(xp_legend_unique[[1]] == "Pangaea")) {
                            # save survived pangaea dois as latex table
                            # one multi-row per unique pangaea bibtex entry (and not DOI) because:
                            # different pangaea DOIs may have the same bibtex entry! 
                            message("\nspecial: save survived pangaea dois as latex table")
                            inds <- which(xp_legend[[1]] == "Pangaea")
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
                                latex_varname <- "$\\delta^{18}$O$_\\text{p,SMOW}$"
                                latex_unit <- "\\textperthousand"
                            } else {
                                stop("not defined")
                            }
                            latex_caption <- paste0("Pangaea data sets used for temporal mean calculation of ", 
                                                    latex_varname, " (in ", latex_unit, ") shown in Fig. ",
                                                    "\\ref{fig:timmean_d18o_pi_gnip_iso2k_sisal_pangaea} and ",
                                                    "\\ref{fig:scatter_d18o_pi_linear_gnip_iso2k_sisal_pangaea}. ",
                                                    "Start and End columns provide years from 0 CE.")
                            columns <- paste0("\\mythead{No}",
                                              " & \\mytheadl{Pangaea reference}",
                                              " & \\mythead{lon [$^{\\circ}$]}",
                                              " & \\mythead{lat [$^{\\circ}$]}",
                                              " & \\mythead{Start}",
                                              " & \\mythead{End}",
                                              paste0(" & \\mythead{", latex_varname, "}"),
                                              "\\\\")
                            lines <- c("\\fontsize{9}{6}\\selectfont",
                                       "\\begin{longtable}{@{}rlrrrrrr@{}}",
                                       paste0("\\caption{", latex_caption, "}\\label{tab:appendix_pangaea}\\\\"),
                                       "\\toprule",
                                       columns,
                                       "\\endfirsthead",
                                       "\\toprule",
                                       columns,
                                       "\\midrule",
                                       "\\endhead",
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
                                    if (j == 1) { # add authors only for first record
                                        line[j] <- paste0(line[j], " \\citet{", names(bibtex)[i], "}")
                                    } else {
                                        line[j] <- paste0(line[j], " \\dittotikz")
                                    }
                                    line[j] <- paste0(line[j], " & ", round(point_datap[[inds2[j]]]$lon, 3))
                                    line[j] <- paste0(line[j], " & ", round(point_datap[[inds2[j]]]$lat, 3))
                                    o <- attributes(point_datap[[inds2[j]]]$time)$origin
                                    if (is.null(o)) stop("posixlt object does not yet have `origin` attribute")
                                    if (o == 1950) {
                                        fromto_1950 <- as.POSIXlt(point_datap[[inds2[j]]]$data_mean_rangetime, o="1970-1-1", tz="UTC")
                                        fromto_1950 <- fromto_1950$year + 1900
                                        fromto_0 <- fromto_1950 + 1950
                                    } else if (o == 0) {
                                        stop("not defined")
                                    } else {
                                        stop("not defined")
                                    }
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
                                       "\\end{longtable}", 
                                       "\\normalsize",
                                       "")
                            for (i in seq_along(bibtex)) lines <- c(lines, bibtex[[i]])
                            fout <- paste0("pangaea_table_", paste(prefs, collapse="_"), ".txt")
                            message("save ", fout, " ...")
                            writeLines(lines, con=fout)
                        
                        } # if table a1
                        
                        # table a2 jqs paper
                        if (F && any(xp_legend_unique[[1]] == "LiPD")) {
                            inds <- which(xp_legend[[1]] == "LiPD")
                            if (zname == "lm_aprt_as_time_slope") {
                                latex_varname <- "$P$"
                                latex_varname_thead <- "$P$\\\\trend"
                                latex_unit <- "mm/year/6k years"
                                latex_caption <- paste0("Linked Paleo Data (LiPD) records used for linear trends of ",
                                                        "annual precipitation ", latex_varname, " (in ", latex_unit, ") shown in Fig. ",
                                                        "\\ref{fig:trend_tsurf_aprt_annual_bartlein_kaufman_lipd} ",
                                                        "and \\ref{fig:scatter_linear_midHolocene_bartlein_kaufman_lipd}. ",
                                                        "Only those proxy records were included whose database fields ",
                                                        "\\textit{variableName} = \"precipitation\", \\textit{units} = \"mm\" ",
                                                        "or \"mm/yr\", \\textit{seasonality} = \"annual\", ",
                                                        "which cover a period of at least 2000 years and where the p-value of ",
                                                        "the least square trend from 7k to 0 BP is smaller than 0.01. ",
                                                        "The LiPD reference denotes the suffix to the URL ",
                                                        "\\href{https://lipdverse.org/globalHolocene/1_0_0/}",
                                                        "{https://lipdverse.org/globalHolocene/1\\_0\\_0/$<$LiPD reference$>$.html}, ",
                                                        "maintained by Nicholas McKay. Duplicate LiPD references denote different samples per site. ",
                                                        "Start and End columns provide years before 0 BP.")
                            } else {
                                stop("not defined")
                            }
                            columns <- paste0("\\thead{No}",
                                              " & \\theadl{LiPD reference}",
                                              " & \\thead{lon [$^{\\circ}$]}",
                                              " & \\thead{lat [$^{\\circ}$]}",
                                              " & \\thead{Start}",
                                              " & \\thead{End}",
                                              " & \\thead{", latex_varname_thead, "}",
                                              " & \\thead{Trend\\\\summary}",
                                              "\\\\")
                            lines <- c("\\fontsize{9}{6}\\selectfont",
                                       "\\begin{longtable}{@{}rlrrrrrr@{}}",
                                       paste0("\\caption{", latex_caption, "}\\label{tab:appendix_lipd}\\\\"),
                                       "\\toprule",
                                       columns,
                                       "\\endfirsthead",
                                       "\\toprule",
                                       columns,
                                       "\\midrule",
                                       "\\endhead",
                                       "\\midrule")
                            for (i in seq_along(inds)) {
                                line <- paste0(i, " &")
                                lipd_ref <- tools::file_path_sans_ext(basename(point_datap[[inds[i]]]$lipd$lipdverseLink))
                                #if (grepl("yCorMontana", lipd_ref)) stop("asdasd")
                                if (grepl("_", lipd_ref)) lipd_ref <- gsub("_", "\\\\_", lipd_ref)
                                line <- paste0(line, " \\href{", point_datap[[inds[i]]]$lipd$lipdverseLink, 
                                               "}{", lipd_ref, "}")
                                line <- paste0(line, " & ", round(point_datap[[inds[i]]]$lon, 3))
                                line <- paste0(line, " & ", round(point_datap[[inds[i]]]$lat, 3))
                                o <- attributes(point_datap[[inds[i]]]$time_ts)$origin
                                if (is.null(o)) stop("posixlt object does not yet have `origin` attribute")
                                if (o == 1950) {
                                    fromto_1950 <- abs(range(point_datap[[inds[i]]]$time_ts$year + 1900)) # years before 1950
                                    #fromto_1950 <- as.Date(fromto_1950)
                                } else if (o == 0) {
                                    stop("not defined")
                                } else {
                                    stop("not defined")
                                }
                                line <- paste0(line, " & ", fromto_1950[1])
                                line <- paste0(line, " & ", fromto_1950[2])
                                line <- paste0(line, " & ", round(point_datap[[inds[i]]]$data_mean))
                                lm_summary <- point_datap[[inds[i]]]$lm_label
                                if (grepl("<", lm_summary)) {
                                    lm_summary <- sub("<", "$<$", lm_summary)
                                }
                                line <- paste0(line, " & ", lm_summary)
                                line <- paste0(line, "\\\\")
                                lines <- c(lines, line)
                            } # for i inds
                            lines <- c(lines, 
                                       "\\bottomrule",
                                       "\\end{longtable}", 
                                       "\\normalsize")
                            fout <- paste0("LiPD_", zname, "_table.txt")
                            message("save ", fout, " ...")
                            writeLines(lines, con=fout)
                        
                        } # if table a2 
                        
                        # table a3 jqs paper
                        if (T && all(grepl("Temp12k|Iso2k|SISAL|PLOT", xp_legend_unique[[1]]))) {
                            if (zname == "lm_wisoaprt_d_post_as_time_slope") {
                                latex_varname_thead <- "$\\delta^{18}$O\\\\trend"
                                latex_caption <- paste0("Temp12k 1.0.0 \\citep{kaufman_etal_2020}, Iso2k 1.0.0 \\citep{konecky_etal_2020}, ",
                                                        "SISAL 2.0 \\citep{comas-bru_etal_2020} and Eurasian lake (index time series) records used for ",
                                                        "linear trends of annual mean $\\delta^{18}$O$_\\text{p/w/diatom,SMOW}$ ",
                                                        "(in \\textperthousand/6k years) shown in Fig. ",
                                                        "\\ref{fig:trend_wisoaprt_d_post_annual_kaufman_konecky_comas-bru_plot}. ",
                                                        "Only those proxy records were included which cover a period of at least 2000 years and ",
                                                        "where the p-value of the least square regression trend from 7k to 0 BP is smaller than 0.01 ",
                                                        "(see methods). Start and End columns provide years before 0 BP.")
                                columns <- paste0("\\mythead{No}",
                                                  " & \\mytheadl{Data set}",
                                                  #" & \\mytheadl{Reference}",
                                                  " & \\mytheadl{Location}", 
                                                  " & \\mythead{lon [$^{\\circ}$]}",
                                                  " & \\mythead{lat [$^{\\circ}$]}",
                                                  " & \\mythead{Start}",
                                                  " & \\mythead{End}",
                                                  " & \\mythead{", latex_varname_thead, "}",
                                                  " & \\mythead{Trend\\\\summary}",
                                                  "\\\\")
                                lines <- c("\\fontsize{9}{6}\\selectfont", # % fontsize linespacing
                                           #"\\begin{longtable}{@{}rlrrrrrrrr@{}}", # with ref and loc
                                           "\\begin{longtable}{@{}rllrrrrrr@{}}", # with ref or loc
                                           #"\\begin{longtable}{@{}rlrrrrrr@{}}", # without ref and loc
                                           paste0("\\caption{", latex_caption, "}\\label{tab:appendix_d18O_trend}\\\\"),
                                           "\\toprule",
                                           columns,
                                           "\\endfirsthead",
                                           "\\toprule",
                                           columns,
                                           "\\midrule",
                                           "\\endhead",
                                           "\\midrule")
                                inds <- seq_along(point_datap) # use all
                                for (i in seq_along(inds)) {
                                    dataset <- point_datap[[inds[i]]]$legend
                                    if (dataset == "Iso2k-nonprecip") dataset <- "Iso2k 1.0.0"
                                    if (dataset == "PLOT") dataset <- "Lake diatoms"
                                    ref <- NA
                                    if (any(point_datap[[inds[i]]]$fname == c("kaufman_etal_2020_temp12k_d18o", 
                                                                              "iso2k-nonprecip",
                                                                              "iso2k-precip"))) {
                                        ref <- tools::file_path_sans_ext(basename(point_datap[[inds[i]]]$lipd$lipdverseLink))
                                        ref <- paste0("\\href{", point_datap[[inds[i]]]$lipd$lipdverseLink, "}{", ref, "}")
                                    } else if (point_datap[[inds[i]]]$fname == "sisal") {
                                        ref <- "todo"
                                    } else if (point_datap[[inds[i]]]$fname == "PLOT") {
                                        ref <- point_datap[[inds[i]]]$ref
                                    }
                                    if (is.na(ref)) stop("implement")
                                    if (grepl("_", ref)) ref <- gsub("_", "\\\\_", ref)
                                    loc <- NA
                                    if (any(point_datap[[inds[i]]]$fname == c("kaufman_etal_2020_temp12k_d18o", 
                                                                              "iso2k-nonprecip",
                                                                              "iso2k-precip"))) {
                                        loc <- point_datap[[inds[i]]]$lipd$geo_siteName
                                    } else if (any(point_datap[[inds[i]]]$fname == c("sisal", "PLOT"))) {
                                        loc <- point_datap[[inds[i]]]$loc
                                    }
                                    if (is.na(loc)) stop("implement")
                                    if (grepl("_", loc)) loc <- gsub("_", "\\\\_", loc)
                                    lon <- round(point_datap[[inds[i]]]$lon, 3)
                                    lat <- round(point_datap[[inds[i]]]$lat, 3)
                                    o <- attributes(point_datap[[inds[i]]]$time_ts)$origin
                                    if (is.null(o)) stop("posixlt object does not yet have `origin` attribute")
                                    if (o == 1950) {
                                        fromto_1950 <- abs(range(point_datap[[inds[i]]]$time_ts$year + 1900)) # years before 1950
                                    } else if (o == 0) {
                                        stop("not defined")
                                    } else {
                                        stop("not defined")
                                    }
                                    trend_val <- round(point_datap[[inds[i]]]$data_mean, 2)
                                    lm_summary <- point_datap[[inds[i]]]$lm_label
                                    if (grepl("<", lm_summary)) lm_summary <- sub("<", "$<$", lm_summary)

                                    # add entries to line
                                    line <- i # no
                                    line <- paste0(line, " & ", dataset)
                                    #line <- paste0(line, " & ", ref)
                                    line <- paste0(line, " & ", loc)
                                    line <- paste0(line, " & ", lon)
                                    line <- paste0(line, " & ", lat)
                                    line <- paste0(line, " & ", fromto_1950[1])
                                    line <- paste0(line, " & ", fromto_1950[2])
                                    line <- paste0(line, " & ", trend_val)
                                    line <- paste0(line, " & ", lm_summary)
                                    line <- paste0(line, "\\\\")
                                    lines <- c(lines, line)
                                } # for i inds
                                lines <- c(lines, 
                                           "\\bottomrule",
                                           "\\end{longtable}", 
                                           "\\normalsize")
                                fout <- paste0("temp12k_iso2k_sisal_plot_", zname, "_table.txt")
                                message("save ", fout, " ...")
                                writeLines(lines, con=fout)

                            } else {
                                stop("not defined")
                            }

                        } # which latex table

                    } else {
                        message("\nenable here to export latex tables ...")
                    } # if saved survived point_datap as latex table
                    # finished special: save survived point_data as latex table 

                    # group scatter point_data by?
                    message("\ngroup color and pch of point_data xp and yp by what?")
                    scatterobscols <- scatterobspchs <- vector("list", l=length(xp))
                    for (i in seq_along(xp)) {

                        ## col
                        # color by season
                        if (F && any(names(known_seasons) == seasonsp_p[i])) { 
                            if (i == 1) message("color scatter by season ", seasonsp_p[i], " col = ",
                    known_seasons[[which(names(known_seasons) == seasonsp_p[i])]]$col) 
                            scatterobscols[[i]] <- rep(
                    known_seasons[[which(names(known_seasons) == seasonsp_p[i])]]$col,
                                                       t=length(xp[[i]]))
                        # color by xp_fname_unique
                        } else if (T) { 
                            if (i == 1) message("color scatter by xp_fname_unique ...")
                            scatterobscols[[i]] <- mycols(max(xp_colno[[i]]))[xp_colno[[i]]]
                        
                        # color by model setting i
                        } else if (T) {
                            if (i == 1) message("color scatter by model setting cols ...")
                            scatterobscols[[i]] <- rep(cols_p[i], t=length(xp[[i]]))
                        }

                        # use transparent colors
                        if (F) {
                            if (i == 1) message("use transparent colors for scatter")
                            scatterobscols[[i]] <- col2rgba(scatterobscols[[i]], alpha=alpha_rgb)
                        }

                        ## pch
                        if (T) {
                            if (T) { # hollow symbols
                                if (i == 1) message("pch scatter hollow by xp_pchno ...")
                                pchs <- c(pchs_hollow, seq_len(255)[-pchs_hollow]) # declare
                            } else if (F) { # filled symbols 
                                if (i == 1) message("pch scatter filled wout border by xp_pchno ...")
                                pchs <- c(pchs_filled_wout_border, 
                                          (15:20)[!match(15:20, pchs_filled_wout_border, nomatch=F)]) # declare
                            }
                            xp_pchno_unique <- unique(xp_pchno[[i]])
                            tmp <- rep(NA, t=length(xp[[i]]))
                            for (j in seq_along(xp_pchno_unique)) {
                                inds <- which(xp_pchno[[i]] == xp_pchno_unique[j]) 
                                tmp[inds] <- pchs[xp_pchno_unique[j]]
                            }
                            scatterobspchs[[i]] <- tmp
                        }
                        
                    } # for i in xp
                    
                    # scatter plot point_datap vs model legend
                    scatterobsle <- list(text=unique(unlist(xp_legend)))
                    if (T) { # add number of individual datasets to scatter legend text
                        message("add numbers of individual datasets of setting 1 to scatter legend text")
                        tmp <- rep(NA, t=length(scatterobsle$text))
                        for (i in seq_along(tmp)) {
                            tmp[i] <- length(which(xp_legend[[1]] == scatterobsle$text[i]))
                        }
                        scatterobsle$text <- paste0(scatterobsle$text, " (n=", tmp, ")")
                    }
                    if (T) {
                        message("scatter legend cols by unique cols of setting 1 = ", appendLF=F)
                        scatterobsle$col <- unique(scatterobscols[[1]])
                    } else if (F) {
                        message("scatter legend cols by first obs col = ", appendLF=F)
                        scatterobsle$col <- rep(unique(unlist(scatterobscols))[1], t=length(xp))
                    }
                    message(paste(scatterobsle$col, collapse=", "))
                    if (T) {
                        message("scatter legend pchs by first ", length(xp), " pchs = ", appendLF=F)
                        scatterobsle$pch <- unique(scatterobspchs[[1]])
                    } 
                    message(paste(scatterobsle$pch, collapse=", "))
                    scatterobsle$lty <- rep(NA, t=length(scatterobsle$pch))
                    scatterobsle$lwd <- rep(NA, t=length(scatterobsle$pch))
                   
                    #tlim_scatter <- as.POSIXlt(range(lapply(lapply(xp, attributes), 
                    #                                        "[[", "rangetot")), o="1970-1-1", tz="UTC")
                    
                    message("\nscatterplot point_datap versus ", length(yp), " model data:")
                    point_datap_suffix <- rep("", t=length(xp))
                    point_datap_n <- rep(0, t=length(xp))
                    for (modeli in seq_along(xp)) {
                        message(names_short[modeli], " versus:")
                        for (i in seq_along(xp_fname_unique[[modeli]])) {
                            inds <- which(xp_fname[[modeli]] == xp_fname_unique[[modeli]][i])
                            point_datap_n[modeli] <- point_datap_n[modeli] + length(inds)
                            point_datap_suffix[modeli] <- paste0(point_datap_suffix[modeli], 
                                                                 "_", length(inds), "_", xp_fname_unique[[modeli]][i])
                            tlim <- range(xp_tlims[[modeli]][,inds])
                            tlim <- as.POSIXlt(tlim, o="1970-1-1", tz="UTC")
                            message("   ", length(inds), " ", xp_fname_unique[[modeli]][i], 
                                    " ", xp_varname_unique[[modeli]][i], 
                                    " from ", tlim[1], " to ", tlim[2])
                        }
                        message("   --> ", point_datap_n[modeli], " point_datap")
                    }
                    point_datap_suffix <- paste(unique(point_datap_suffix), collapse="_and_")
                    
                    # xlim and ylim at inds where both obs and model are not NA
                    xlim <- ylim <- vector("list", l=length(xp))
                    for (i in seq_along(xlim)) {
                        okinds <- which(!is.na(xp[[i]]) & !is.na(yp[[i]]))
                        if (length(okinds) > 0) {
                            xlim[[i]] <- range(xp[[i]][okinds])
                            ylim[[i]] <- range(yp[[i]][okinds])
                        } else {
                            xlim[[i]] <- c(0, 0) # so that plot works without error in case no common non-NA locations
                            ylim[[i]] <- c(0, 0)
                        }
                    }
                    xlim <- range(xlim)
                    ylim <- range(ylim)
                    
                    # special
                    if (T && all(!is.na(match(unique(unlist(xp_varname_unique)), c("map_trend", "precipitation_trend"))))) { 
                        message("special map_trend and/or precipitation_trend xlim ...")
                        xlim[1] <- -900 # do not show one very large Bartlein et al. 2011 negative precip trend -3150 mm/a/6k years
                    }
                   
                    # xlim ylim summary
                    message("xlim (", paste(unique(unlist(xp_varname_unique)), collapse=", "), 
                            " of point_data) = ", appendLF=F)
                    dput(xlim)
                    xat <- pretty(xlim, n=10)
                    xlab <- format(xat, trim=T)

                    message("ylim (", zname, " of models) = ", appendLF=F)
                    dput(ylim)
                    yat <- pretty(ylim, n=10)
                    ylab <- format(yat, trim=T)
                    
                    plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                       zname, "_", 
                                       paste0(names_short_p, "_", seasonsp_p, 
                                              "_", froms_plot_p, "_to_", tos_plot_p, "_", 
                                              areas_p, collapse="_vs_"), 
                                       plotname_suffix, 
                                       "_scatter_", interp_method, "_vs", 
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
                                           "_scatter_", interp_method, "_vs", 
                                           point_datap_suffix,
                                           ".", p$plot_type)
                    }
                    message("open plot ", plotname, " ...")
                    dir.create(dirname(plotname), recursive=T, showWarnings=F)
                    source("~/scripts/r/functions/myfunctions.r") 
                    pp <- plot_sizes(width_in=p$scatter_width_in, asp=p$scatter_asp, verbose=T)
                    if (T) {
                        message("increase pointsize")
                        pp$png_pointsize <- 14
                        pp$pdf_pointsize <- 14
                    }
                    if (p$plot_type == "png") {
                        png(plotname, width=pp$png_width_px, height=pp$png_height_px,
                            pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
                    } else if (p$plot_type == "pdf") {
                        pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
                            family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
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
                        lab <- expression(paste("Model P"["total"], " trend [mm/a/6k years]"))
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
                    if (add_1to1_line) {
                        message("add 1:1 line ...")
                        #abline(a=0, b=1, col="gray", lty=2) # a=intercept, b=slope
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
                            message("get legend position automatically with Hmisc::largest.empty() ... ", appendLF=F) 
                            tmp <- Hmisc::largest.empty(x=unlist(xp), y=unlist(yp), method="area")
                            #rect(tmp$rect$x[1], tmp$rect$y[1], tmp$rect$x[2], tmp$rect$y[3])
                            le$pos <- c(x=min(tmp$rect$x), y=max(tmp$rect$y)) # topleft corner if x- and y-coords are both increasing (default)
                        } else if (F && suppressPackageStartupMessages(require(adagio))) {
                            message("get legend position automatically with adagio::maxempty() ... ", appendLF=F) 
                            x <- unlist(xp); y <- unlist(yp)
                            tmp <- adagio::maxempty(x=x, y=y, ax=par("usr")[1:2], ay=par("usr")[3:4])
                            #rect(tmp$rect[1], tmp$rect[2], tmp$rect[3], tmp$rect[4])
                            le$pos <- c(x=tmp$rect[1], y=tmp$rect[4]) # topleft corner if x- and y-coords are both increasing (default)
                        } else {
                            le$pos <- "topleft" 
                            #le$pos <- "topright" 
                            #le$pos <- "bottomright" 
                        }
                        message(paste(le$pos, collapse=", "))
                        le$ncol <- 1
                        #le$ncol <- 2 
                        le$cex <- lecex
                        #le$cex <- 1.25
                        #le$cex <- 1.5
                        #le$cex <- 0.85
                        le$text <- names_legend
                        if (F) {
                            message("special legend names")
                            le$text <- paste0(names_legend, " (n=", sapply(lapply(xp, attributes), "[[", "ntot"), ")")
                        }
                        le$col <- cols_p #"black"
                        le$lty <- rep(NA, t=length(yp))
                        le$lwd <- rep(NA, t=length(yp))
                        #le$pch <- scatterpchs
                        le$pch <- rep(15, t=length(yp)) # filled square for just showing the color
                        le$pt.cex <- rep(1.5, t=length(yp))
                        if (T && length(scatterobsle) > 0) {
                            if (T) {
                                message("replace model legend by scatterobsle")
                                le$text <- scatterobsle$text
                                le$col <- scatterobsle$col
                                le$lty <- scatterobsle$lty
                                le$lwd <- scatterobsle$lwd
                                le$pch <- scatterobsle$pch
                            }
                            if (F) {
                                message("add scatterobsle to model legend ...")
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
                            le$cex <- lecex
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
                    write(plotname, file=lastfiles_plot_fname, append=T)
                    if (p$plot_type == "pdf") {
                        if (T) {
                            message("run `", p$pdf_embed_fun, "()` ...")
                            if (p$pdf_embed_fun == "grDevices::embedFonts") {
                                grDevices::embedFonts(plotname, outfile=plotname)
                            } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                                extrafont::embed_fonts(plotname, outfile=plotname)
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
                    message("\n--> model point data yp[[", i, "]] all NA in lonlatlims ...\n")
                } # if yp is NA or not

            } else {
                message("--> no point_data defined for zname = ", zname, " ...\n")
            } # if point_data was properly defined or not for current zname


            ## continue with lon-lat stuff
            # make lat regular for `image(..., useRaster=T)` usage
            if (p$plot_type == "png" && !all(sapply(lapply(lapply(d$lat, diff), unique), length) == 1)) {
                message("make constant dlat for `image(..., useRaster=T)` usage in png plot ...")
                for (i in seq_along(d$lat)) {
                    d$lat[[i]] <- array(seq(min(d$lat[[i]]), max(d$lat[[i]]), l=length(d$lat[[i]])))
                }
            }
           
            ## add stuff lon,lat plot before zlim
            addland_list <- contour_list <- polygon_list <- quiver_list <- segment_list <- 
                point_list <- text_list <- cmd_list <- NULL # default
           
            # add special quiver data to lon,lat plot
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
                                stop("update")
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
            message("define lon vs lat color levels here if wanted ...")
            message("get global min/max zlim ... ", appendLF=F)
            zlim <- range(z, na.rm=T)
            op <- options()$digits; options(digits=15); cat("=", zlim, "\n"); options(digits=op)

            # placeholder defaults for image.plot.pre.r:
            nlevels <- zlevels <- method <- power_lims <- power_min <- 
                axis.labels <- axis.addzlims <- 
                y_at <- palname <- anom_colorbar <- 
                center_around <- center_include <- NULL
            # placeholder defaults for image.plot.nxm.r:
            znames_method <- znames_pos <- znames_cex <- legend.line <- colorbar.cex <- NULL
            contour_only <- F
            if (any(zname == c("tos", "thetao"))) {
                #anom_colorbar <- F
            } else if (zname == "hvel") {
                message("hvel special zlevels")
                #zlevels <- c(2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 25, 50)
                zlevels <- c(2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
                zlevels <- c(-1*rev(zlevels), 0, zlevels)
                axis.labels <- zlevels
                if (zlim[1] < min(zlevels)) zlevels <- c(zlim[1], zlevels)
                if (zlim[2] > max(zlevels)) zlevels <- c(zlevels, zlim[2])
            } else if (zname == "quv") {
                message("quv special zlim")
                nlevels <- 200
                #zlim <- c(5.03, 324.37) # feb; era5: 0.0244510751217604, 294.239959716797 
                #zlim <- c(6.05, 328.23) # may; era5: 0.00584759470075369, 297.601440429688
                #zlim <- c(12.1, 474.4) # aug; era5: 0.077091708779335, 531.362243652344
                #zlim <- c(8.33, 422.71) # nov; era5: 0.108525462448597, 300.630645751953
                zlim <- c(0.00584759470075369, 531.362243652344)
            } else if (zname == "lm_temp2_as_time_slope") {
                #message("lm_temp2_as_time_slope special zlim")
                # Hol-T annual: c(-2.77528234243418, 2.26845881332211)
                #zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=0.25), zlim[2]) # deg C / 7k yrs
                #zlim <- c(-4.3802347784168293998, 2.4162697392007639330)
                #zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=0.5), zlim[2]) # deg C / 7k yrs
                palname <- "colormaps_jaisnd"
            } else if (zname == "lm_THO_as_time_slope") {
                # lm_tho: 360x180: 5 season: c(-4.00305379614524, 3.54971842687043)
                # lm_tho: 3600x1800: 5 season: c(-4.03669006075327, 3.55691367873545)
            } else if (zname == "lm_tsurf_as_time_slope") {
                message("lm_tsurf_as_time_slope special zlim")
                # Hol-T annual: c(-4.3802347784168293998, 2.4162697392007639330)
                # Hol-T: 5 season: -9.440916  6.903386
                # Hol-T: 3600x1800: 5 season: c(-9.46404887704723, 6.91053024191786)
                zlim <- c(-4.3802347784168293998, 2.4162697392007639330)
                #zlim <- c(-4.03669006075327, 3.55691367873545)
                #zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=0.25), zlim[2]) # deg C / 7k yrs
                zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=0.5), zlim[2]) # deg C / 7k yrs
                palname <- "colormaps_jaisnd"
            } else if (T && zname == "lm_aprt_as_time_slope") {
                message("special zlim lm_aprt_as_time_slope")
                # Hol-T and Hol-Tx10 annual: -404.457947500465 233.716296217084
                #zlevels <- c(zlim[1], seq(trunc(zlim[1]), trunc(zlim[2]), b=25), zlim[2]) # mm/month / 7k yrs
                zlevels <- c(-500, -200, -100, -50, -20, 0, 20, 50, 100, 200, 500) # as in bartlein et al. 2011 Fig. 6
                palname <- "BrBG"
            } else if (zname == "wisoaprt_d") {
                if (T && exists("xp") && !all(sapply(lapply(xp, is.na), all))) {
                    message("wisoaprt_d special zlim")
                    zlevels <- c(zlim[1], -30, -28, -26, -24, -22, -20, -18, -16, 
                                 -14, -12, -10, -8, -6, -4, -2, zlim[2])
                    if (min(range(xp)) < min(zlevels)) {
                        zlim[1] <- min(range(xp))
                        zlevels[1] <- zlim[1]
                    }
                    if (max(range(xp)) > max(zlevels)) {
                        zlim[2] <- max(range(xp))
                        zlevels[2] <- zlim[2] 
                    }
                }
                message("wisoaprt_d special colors")
                palname <- "RdYlBu"
                anom_colorbar <- F
            } else if (T && any(zname == c("mlotst", "omldamax"))) {
                message("mlotst omldamax special zlim")
                if (T) {
                    message("special anom_pcnt zlevels")
                    zlevels <- pretty(quantile(z[[i]], probs=c(0.0001, 0.99), na.rm=T), n=11) 
                    if (zlim[1] < min(zlevels)) zlevels <- c(zlim[1], zlevels)
                    if (zlim[2] > max(zlevels)) zlevels <- c(zlevels, zlim[2])
                    axis.addzlims <- F
                    center_around <- 100
                    anom_colorbar <- T
                } else if (F) {
                    #palname <- "RdYlBu"
                    # FMA: 6.053815 3650.000000
                    # SON: 6.04869794845581 4879.9344112022
                    #zlevels <- c(min(zlim), seq(500, zlim[2], b=500))
                    #if (max(zlevels) != zlim[2]) zlevels[length(zlevels)] <- zlim[2]
                    zlevels <- c(seq(min(zlim), 1100, l=100), zlim[2])
                    axis.labels <- c(round(zlim[1]), seq(125, 1000, b=125), round(zlim[2]))
                }
            } else if (zname == "CO2") {
                #contour_only <- T
            } else if (zname == "co2_flx_ocean") {
                #nlevels <- 20
                #palname <- "colormaps_jaisnd"
            } else if (zname == "co2_flx_land") {
                #anom_colorbar <- F
            } else if (zname == "bgc03") { # total alkalinity
                zlevels <- pretty(c(2250, 2350), n=10)
                if (zlim[1] < min(zlevels)) zlevels <- c(zlim[1], zlevels)
                if (zlim[2] > max(zlevels)) zlevels <- c(zlevels, zlim[2])
            } else if (zname == "resolutionkm") {
                message("special resolutionkm zlevels ...")
                # cbscl irregular: 5.512, 212.598
                # cbscl regular 1/4: 5.952736 213.047769
                # cbscl regular 1/10: 5.852757 213.714606
                # lsea2 irregular: 4.425313, 91.178831
                # lsea2 regular 1/4: 4.610265, 90.491457
                # lsea2 regular 1/10: 4.518509 92.602042
                zlevels <- c(zlim[1], seq(6, 18, b=2), seq(20, 50, b=10), seq(75, 150, b=25), zlim[2])
                axis.labels <- as.character(round(zlevels))
                axis.labels[1] <- "4.4"
                axis.labels[length(axis.labels)] <- "212.6"
                axis.addzlims <- F
                znames_method <- "legend"
                colorbar.cex <- 1
            } else if (any(zname == c("tau", "curltau", "ekmanP_ms"))) {
                if (F) {
                    zlevels <- vector("list", l=length(z))
                    for (i in seq_along(zlevels)) {
                        zlevels[[i]] <- quantile(z[[i]], probs=c(0.1, 0.9), na.rm=T)
                    }
                    zlevels <- max(abs(range(zlevels)))
                    zlevels <- c(zlim[1], pretty(c(-zlevels, zlevels), n=11), zlim[2])
                } else if (T) { # anom_pcnt
                    message("special anom_pcnt zlevels")
                    if (zname == "tau") {
                        zlevels <- pretty(quantile(z[[i]], probs=c(0.0001, 0.99), na.rm=T), n=11) 
                    } else if (any(zname == c("curltau", "ekmanP_ms"))) {
                        zlevels <- pretty(quantile(z[[i]], probs=c(0.1, 0.9), na.rm=T), n=11) 
                        zlevels <- zlevels[zlevels > 0]
                    }
                    if (zlim[1] < min(zlevels)) zlevels <- c(zlim[1], zlevels)
                    if (zlim[2] > max(zlevels)) zlevels <- c(zlevels, zlim[2])
                    axis.addzlims <- F
                    center_around <- 100
                    anom_colorbar <- T
                }
            } else if (any(zname == c("Ftemp", "divuvt", "divuvteddy", "divuvttot"))) {
                #zlevels <- c(zlim[1], seq(-500, 500, b=100), zlim[2])
                method <- "exp"
                palname <- "colormaps_jaisnd"
                center_include <- T
                power_min <- -5
                power_lims <- c(-1, -1)
                znames_method <- "legend"
                znames_pos <- "topright"
                znames_cex <- 1
                legend.line <- 6
                if (exists("ll_data")) {
                    if (T && any(sapply(ll_data, names) == "bathy")) {
                        contour_list <- c(contour_list, list(bathy=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["bathy"]][i] <- NA # default
                            if (any(names(ll_data[[i]]) == "bathy")) {
                                contour_list[["bathy"]][[i]] <- list(x=ll_data[[i]][["bathy"]]$lon,
                                                                     y=ll_data[[i]][["bathy"]]$lat,
                                                                     z=ll_data[[i]][["bathy"]]$bathy,
                                                                     levels=c(1000, 2000, 3000),
                                                                     contour_smooth=T,
                                                                     contour_smooth_n_segment_thr=25,
                                                                     contour_smooth_spar=NA)
                                names(contour_list[["bathy"]])[i] <- names(ll_data)[[i]]
                            }
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_bathy")
                    }
                    if (T && any(sapply(ll_data, names) == "u") && any(sapply(ll_data, names) == "v")) {
                        quiver_list <- c(quiver_list, list(hvel=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            quiver_list[["hvel"]][i] <- NA # default
                            if (any(names(ll_data[[i]]) == "u") && any(names(ll_data[[i]]) == "v")) {
                                quiver_list[["hvel"]][[i]] <- list(x=ll_data[[i]][["u"]]$lon,
                                                                   y=ll_data[[i]][["u"]]$lat,
                                                                   u=ll_data[[i]][["u"]]$u,
                                                                   v=ll_data[[i]][["v"]]$v
                                                                   , quiver_thr=0.05 # m s-1
                                                                   , quiver_nxfac=0.15, quiver_nyfac=0.15
                                                                   , quiver_scale=5
                                                                   , quiver_col=col2rgba("black", 0.5)
                                                                  )
                                if (i == 1) {
                                    quiver_list[["hvel"]][[i]]$quiver_legend <- list(#x="x_at[2]", y="y_at[1]",
                                                                                     x=-46, y=61.5,
                                                                                     xvalue=0.5,
                                                                                     label=expression(paste("50 cm s"^"-1")))
                                }
                                names(quiver_list[["hvel"]])[i] <- names(ll_data)[[i]]
                            }
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_hvel")
                    }
                    if (F && any(sapply(ll_data, names) == "mixlay")) {
                        contour_list <- c(contour_list, list(mixlay=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["mixlay"]][i] <- NA # default
                            if (any(names(ll_data[[i]]) == "mixlay")) {
                                contour_list[["mixlay"]][[i]] <- list(x=ll_data[[i]][["mixlay"]]$lon,
                                                                      y=ll_data[[i]][["mixlay"]]$lat,
                                                                      z=ll_data[[i]][["mixlay"]]$mixlay,
                                                                      contour_posneg_soliddashed=F,
                                                                      contour_posneg_redblue=F,
                                                                      #levels=1000,
                                                                      #levels=c(1500, 2000), # m
                                                                      levels=2000,
                                                                      #levels=1500,
                                                                      #col="white", 
                                                                      col=mycols(2)[2], # myred
                                                                      lwd=2,
                                                                      contour_drawlabels=F)
                                names(contour_list[["mixlay"]])[i] <- names(ll_data)[[i]]
                            }
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_mixlay")
                    }
                    if (F && exists("en4")) {
                        contour_list <- c(contour_list, list(en4_mld=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["en4_mld"]][[i]] <- list(x=en4[[1]]$dims$lon,
                                                                   y=en4[[1]]$dims$lat,
                                                                   z=en4[[1]]$data$mld,
                                                                   contour_posneg_soliddashed=F,
                                                                   contour_posneg_redblue=F,
                                                                   #levels=1000,
                                                                   levels=2000, # m
                                                                   #col=mycols(2)[2], # myred
                                                                   col="magenta",
                                                                   lwd=2,
                                                                   lty=2,
                                                                   contour_drawlabels=F)
                            names(contour_list[["en4_mld"]])[i] <- "en4_mld"
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_en4_mld")
                    }
                    if (F && exists("holte_etal_2017")) {
                        contour_list <- c(contour_list, list(holte_etal_2017_mld=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["holte_etal_2017_mld"]][[i]] <- list(x=holte_etal_2017[[1]]$dims$lon,
                                                                               y=holte_etal_2017[[1]]$dims$lat,
                                                                               z=drop(holte_etal_2017[[1]]$data$mld_dt_mean[3,,]), # march
                                                                               contour_posneg_soliddashed=F,
                                                                               contour_posneg_redblue=F,
                                                                               levels=1000,
                                                                               #levels=2000, # m
                                                                               col=mycols(2)[2], # myred
                                                                               lwd=2,
                                                                               lty=3,
                                                                               contour_drawlabels=F)
                            names(contour_list[["holte_etal_2017_mld"]])[i] <- "holte_etal_2017_mld"
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_holte_etal_2017_mld")
                    }
                    if (F && any(sapply(ll_data, names) == "eke")) {
                        contour_list <- c(contour_list, list(eke=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["eke"]][i] <- NA # default
                            if (any(names(ll_data[[i]]) == "eke")) {
                                contour_list[["eke"]][[i]] <- list(x=ll_data[[i]][["eke"]]$lon,
                                                                   y=ll_data[[i]][["eke"]]$lat,
                                                                   z=ll_data[[i]][["eke"]]$eke,
                                                                   contour_posneg_soliddashed=F,
                                                                   contour_posneg_redblue=F,
                                                                   # 0m:
                                                                   #levels=0.01, # m2 s-2
                                                                   #levels=0.015, # m2 s-2
                                                                   #levels=0.003, # m2 s-2
                                                                   #levels=100, # cm2 s-2
                                                                   #levels=150, # cm2 s-2
                                                                   levels=mean(ll_data[[i]][["eke"]]$eke, na.rm=T) + 2*sd(ll_data[[i]][["eke"]]$eke, na.rm=T),
                                                                   #levels=123.5029, # cm2 s-2; mean+2sd Low01 jan-dec lsea
                                                                   #levels=256.5161, # cm2 s-2; mean+2sd LSea5 jan-dec lsea
                                                                   # int:
                                                                   #levels=c(5, 20), # m3 s-2
                                                                   #levels=5,
                                                                   #col="darkgray", 
                                                                   #col="white",
                                                                   col=mycols(3)[3], # myblue
                                                                   lwd=2, 
                                                                   #lty=c(1, 2),
                                                                   lty=1,
                                                                   contour_drawlabels=F)
                                names(contour_list[["eke"]])[i] <- names(ll_data)[[i]]
                            }
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_eke")
                    }
                    if (F && exists("aviso")) {
                        contour_list <- c(contour_list, list(aviso_eke=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["aviso_eke"]][[i]] <- list(x=aviso[[1]]$dims$lon,
                                                                     y=aviso[[1]]$dims$lat,
                                                                     z=aviso[[1]]$data$eke_ltm,
                                                                     contour_posneg_soliddashed=F,
                                                                     contour_posneg_redblue=F,
                                                                     # 0m:
                                                                     #levels=0.005, # m2 s-2
                                                                     #levels=50, # cm2 s-2
                                                                     levels=mean(aviso[[1]]$data$eke_ltm, na.rm=T) + 2*sd(aviso[[1]]$data$eke_ltm, na.rm=T),
                                                                     #levels=38.8666, # cm2 s-2; mean+2sd aviso jan-dec lsea
                                                                     #col=mycols(3)[3], # myblue
                                                                     col="blue",
                                                                     lwd=2, 
                                                                     lty=2,
                                                                     contour_drawlabels=F)
                            names(contour_list[["aviso_eke"]])[i] <- "aviso_eke"
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_aviso_eke")
                    }
                } # if exists("ll_data")
                if (T) {
                    polygon_list <- list(intpoly=list(list(x=polyl$x, y=polyl$y, col=NA, border="black", lwd=3, lty=2),
                                                      #list(x=polyh$x, y=polyh$y, col=NA, border="black", lwd=3, lty=2)))
                                                      list(x=polyl$x, y=polyl$y, col=NA, border="black", lwd=3, lty=2)))
                }
                if (zname == "Ftemp") {
                    cmd_list <- list(plotletter=list("legend(\"bottomleft\", \"a\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)",
                                                     "legend(\"bottomleft\", \"b\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)"))
                } else if (zname == "divuvt") {
                    cmd_list <- list(plotletter=list("legend(\"bottomleft\", \"c\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)",
                                                     "legend(\"bottomleft\", \"d\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)"))
                } else if (zname == "divuvteddy") {
                    cmd_list <- list(plotletter=list("legend(\"bottomleft\", \"e\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)",
                                                     "legend(\"bottomleft\", \"f\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)"))
                }
            } else if (any(zname == c("FeKe", "HRS", "wbeddy"))) {
                method <- "exp"
                palname <- "colormaps_jaisnd"
                center_include <- T
                power_min <- -6
                power_lims <- c(-3, -3)
                znames_method <- "legend"
                znames_pos <- "topright"
                znames_cex <- 1
                legend.line <- 6
                if (exists("ll_data")) {
                    if (T && any(sapply(ll_data, names) == "bathy")) {
                        contour_list <- c(contour_list, list(bathy=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["bathy"]][i] <- NA # default
                            if (any(names(ll_data[[i]]) == "bathy")) {
                                contour_list[["bathy"]][[i]] <- list(x=ll_data[[i]][["bathy"]]$lon,
                                                                     y=ll_data[[i]][["bathy"]]$lat,
                                                                     z=ll_data[[i]][["bathy"]]$bathy,
                                                                     levels=c(1000, 2000, 3000),
                                                                     contour_smooth=T,
                                                                     contour_smooth_n_segment_thr=25,
                                                                     contour_smooth_spar=NA)
                                names(contour_list[["bathy"]])[i] <- names(ll_data)[[i]]
                            }
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_bathy")
                    }
                    if (T && any(sapply(ll_data, names) == "u") && any(sapply(ll_data, names) == "v")) {
                        quiver_list <- c(quiver_list, list(hvel=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            quiver_list[["hvel"]][i] <- NA # default
                            if (any(names(ll_data[[i]]) == "u") && any(names(ll_data[[i]]) == "v")) {
                                quiver_list[["hvel"]][[i]] <- list(x=ll_data[[i]][["u"]]$lon,
                                                                   y=ll_data[[i]][["u"]]$lat,
                                                                   u=ll_data[[i]][["u"]]$u,
                                                                   v=ll_data[[i]][["v"]]$v
                                                                   , quiver_thr=0.05 # m s-1
                                                                   , quiver_nxfac=0.15, quiver_nyfac=0.15
                                                                   , quiver_scale=5
                                                                   , quiver_col=col2rgba("black", 0.5)
                                                                  )
                                if (i == 1) {
                                    quiver_list[["hvel"]][[i]]$quiver_legend <- list(#x="x_at[2]", y="y_at[1]",
                                                                                     x=-46, y=61.5,
                                                                                     xvalue=0.5,
                                                                                     label=expression(paste("50 cm s"^"-1")))
                                }
                                names(quiver_list[["hvel"]])[i] <- names(ll_data)[[i]]
                            }
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_hvel")
                    }
                    if (F && any(sapply(ll_data, names) == "mixlay")) {
                        contour_list <- c(contour_list, list(mixlay=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["mixlay"]][i] <- NA # default
                            if (any(names(ll_data[[i]]) == "mixlay")) {
                                contour_list[["mixlay"]][[i]] <- list(x=ll_data[[i]][["mixlay"]]$lon,
                                                                      y=ll_data[[i]][["mixlay"]]$lat,
                                                                      z=ll_data[[i]][["mixlay"]]$mixlay,
                                                                      contour_posneg_soliddashed=F,
                                                                      contour_posneg_redblue=F,
                                                                      #levels=1000,
                                                                      #levels=c(1500, 2000), # m
                                                                      levels=2000,
                                                                      #levels=1500,
                                                                      #col="white", 
                                                                      col=mycols(2)[2], # myred
                                                                      lwd=2,
                                                                      contour_drawlabels=F)
                                names(contour_list[["mixlay"]])[i] <- names(ll_data)[[i]]
                            }
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_mixlay")
                    }
                    if (F && exists("en4")) {
                        contour_list <- c(contour_list, list(en4_mld=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["en4_mld"]][[i]] <- list(x=en4[[1]]$dims$lon,
                                                                   y=en4[[1]]$dims$lat,
                                                                   z=en4[[1]]$data$mld,
                                                                   contour_posneg_soliddashed=F,
                                                                   contour_posneg_redblue=F,
                                                                   #levels=1000,
                                                                   levels=2000, # m
                                                                   #col=mycols(2)[2], # myred
                                                                   col="magenta",
                                                                   lwd=2,
                                                                   lty=2,
                                                                   contour_drawlabels=F)
                            names(contour_list[["en4_mld"]])[i] <- "en4_mld"
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_en4_mld")
                    }
                    if (F && any(sapply(ll_data, names) == "eke")) {
                        contour_list <- c(contour_list, list(eke=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["eke"]][i] <- NA # default
                            if (any(names(ll_data[[i]]) == "eke")) {
                                contour_list[["eke"]][[i]] <- list(x=ll_data[[i]][["eke"]]$lon,
                                                                   y=ll_data[[i]][["eke"]]$lat,
                                                                   z=ll_data[[i]][["eke"]]$eke,
                                                                   contour_posneg_soliddashed=F,
                                                                   contour_posneg_redblue=F,
                                                                   #levels=20, # m3 s-2
                                                                   levels=c(5, 20), # m3 s-2
                                                                   #col="darkgray", 
                                                                   #col="white",
                                                                   col=mycols(3)[3], # myblue
                                                                   lwd=2, 
                                                                   #lty=1,
                                                                   lty=c(1, 2),
                                                                   contour_drawlabels=F)
                                names(contour_list[["eke"]])[i] <- names(ll_data)[[i]]
                            }
                        }
                        plotname_suffix <- paste0(plotname_suffix, "_eke")
                    }
                    if (F && any(sapply(ll_data, names) == "sic")) {
                        contour_list <- c(contour_list, list(sic=vector("list", l=length(z))))
                        for (i in seq_along(ll_data)) {
                            contour_list[["sic"]][i] <- NA # default
                            if (any(names(ll_data[[i]]) == "sic")) {
                                contour_list[["sic"]][[i]] <- list(x=ll_data[[i]][["sic"]]$lon,
                                                                   y=ll_data[[i]][["sic"]]$lat,
                                                                   z=ll_data[[i]][["sic"]]$sic,
                                                                   contour_posneg_soliddashed=F,
                                                                   contour_posneg_redblue=F,
                                                                   levels=15,
                                                                   #col="cyan", 
                                                                   col="deepskyblue",
                                                                   lwd=2,
                                                                   contour_drawlabels=F)
                                names(contour_list[["sic"]])[i] <- names(ll_data)[[i]]
                            }
                        }
                    }
                }
                if (F) {
                    polygon_list <- list(intpoly=list(list(x=polyl$x, y=polyl$y, col=NA, border="black", lwd=3, lty=2),
                                                      #list(x=polyh$x, y=polyh$y, col=NA, border="black", lwd=3, lty=2)))
                                                      list(x=polyl$x, y=polyl$y, col=NA, border="black", lwd=3, lty=2)))
                }
                if (zname == "FeKe") {
                    cmd_list <- list(plotletter=list("legend(\"bottomleft\", \"a\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)",
                                                     "legend(\"bottomleft\", \"b\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)"))
                } else if (zname == "HRS") {
                    cmd_list <- list(plotletter=list("legend(\"bottomleft\", \"c\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)",
                                                     "legend(\"bottomleft\", \"d\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)"))
                } else if (zname == "wbeddy") {
                    cmd_list <- list(plotletter=list("legend(\"bottomleft\", \"e\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)",
                                                     "legend(\"bottomleft\", \"f\", lty=NA, lwd=NA, pch=NA, x.intersp=-1.5, bty=\"n\", cex=1.5)"))
                }
            } # which variable
            source(paste0(host$homepath, "/functions/image.plot.pre.r"))
            ip <- image.plot.pre(zlim=zlim, nlevels=nlevels, zlevels=zlevels, 
                                 method=method, power_lims=power_lims, power_min=power_min,
                                 axis.labels=axis.labels, axis.addzlims=axis.addzlims, 
                                 palname=palname, anom_colorbar=anom_colorbar, 
                                 center_around=center_around, center_include=center_include,
                                 verbose=F)
            if (any(zlim[1] < min(ip$levels))) warning("zlim[1] < min(ip$levels) in lon vs lat plot. do you want that?")
            if (any(zlim[2] > max(ip$levels))) warning("zlim[2] > max(ip$levels) in lon vs lat plot. do you want that?")
            
            if (zname == "resolutionkm") {
                message("reverse resolutionkm colors ...")
                ip$cols <- rev(ip$cols)
            }

            # default names
            if (F) { # better provide via plot namelist
                if (any(names_legend_p != "")) { # put letters in front of given names
                    names_legend_p[which(names_legend_p != "")] <- paste0(letters[which(names_legend_p != "")], ") ", 
                                                                          names_legend_p[which(names_legend_p != "")])
                }
            }

            ## add stuff to every lon,lat plot after zlim
            
            # add quiver model data to lon,lat plot
            if (plot_groups[plot_groupi] == "samevars") { # todo: implement zuv for samedims
                if (exists("varnames_uv") && !all(is.na(zuv))) {
                    message("\nadd `zuv_samevars[[", zname, "]]` to quiver_list:")
                    cat(capture.output(str(zuv)), sep="\n")
                    stop("update")
                    quiver_list <- list(u=vector("list", l=length(z)))
                    quiver_list$v <- quiver_list$u
                    # just reorder for quiver()
                    for (i in seq_along(z)) {
                        if (all(is.na(zuv[[i]]))) {
                            quiver_list$u[[i]] <- NA
                            quiver_list$v[[i]] <- NA
                        } else {
                            quiver_list$u[[i]] <- zuv[[i]]$u
                            quiver_list$v[[i]] <- zuv[[i]]$v
                        }
                    }
                    quiver_list$nx_fac <- rep(0.5, t=length(quiver_list$u))
                    quiver_list$ny_fac <- rep(0.75, t=length(quiver_list$u))
                    quiver_list$const <- rep(T, t=length(quiver_list$u))
                }
            }
            # finished prepare u,v components if needed

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
            if (T && exists("xp") && !all(sapply(lapply(xp, is.na), all))) {
                message("\nnot all `xp` are NA: reorder `xp` data ", point_datap_suffix, 
                        " into `point_list` and add to lon,lat plot ...")
                point_list <- text_list <- vector("list", l=length(z))
                for (i in seq_along(z)) {
                    if (!all(is.na(xp[[i]]))) {
                        
                        message("set plot specs for `point_list` ", i, "/", length(z), ": ", names_short[i])
                        
                        point_list[[i]] <- list(x=attributes(xp[[i]])$lon,
                                                y=attributes(xp[[i]])$lat)
                        # default xp symbols: black cross
                        point_list[[i]]$col <- rep("black", t=length(point_list[[i]]$x))
                        point_list[[i]]$pch <- rep(4, t=length(point_list[[i]]$x))
                        # declare empty text_list
                        text_list[[i]] <- list(x=rep(NA, t=length(xp[[i]])),
                                               y=rep(NA, t=length(xp[[i]])),
                                               labels=rep(NA, t=length(xp[[i]])),
                                               col=rep(NA, t=length(xp[[i]])))
                        
                        # special cols
                        cols <- NULL
                        if (F) { # constant color
                            message("--> color `point_list[[", i, "]]` by its sign ...")
                            cols <- rep("brown", t=length(xp[[i]])) # color for dry trend
                            if (any(xp[[i]] > 0)) cols[which(xp[[i]] > 0)] <- "blue" # color for wet trend
                        } else if (T) { # same colors as z
                            message("--> color `point_list[[", i, "]]` by the values of xp[[", i, 
                                    "]] with respect to model levels `ip$levels` ...")
                            cols <- findInterval(x=xp[[i]], vec=ip$levels, all.inside=T)
                            cols <- ip$cols[cols]
                        }
                        # special:
                        if (T && any(xp_varname_unique[[i]] == "d18Ononp_scaled_trend")) {
                            inds <- which(xp_varname[[i]] == "d18Ononp_scaled_trend")
                            message("--> color special d18Ononp_scaled: show red/blue symbols at ", length(inds), " locations")
                            cols[inds] <- NA
                            posinds <- which(xp[[i]][inds] >= 0)
                            neginds <- which(xp[[i]][inds] < 0)
                            if (length(posinds) > 0) text_list[[i]]$col[inds][posinds] <- "red"
                            if (length(neginds) > 0) text_list[[i]]$col[inds][neginds] <- "blue"
                        } else if (T && any(xp_fname_unique[[i]] == "PLOT" & xp_varname_unique[[i]]  == "d18Odiatom_scaled_trend")) {
                            inds <- which(xp_fname[[i]] == "PLOT" & xp_varname[[i]] == "d18Odiatom_scaled_trend")
                            message("--> color special PLOT d18Odiatom_scaled: show red/blue symbols at ", length(inds), " locations")
                            cols[inds] <- NA # dont show symbols on lon,lat plot; use the PLOT letters instead
                        }
                        if (!is.null(cols)) point_list[[i]]$bg <- cols
                        
                        # special pchs
                        pchs <- NULL
                        if (T) { # distinguish xp by pch
                            message("--> pch `point_list[[", i, "]]` by the pchs of xp[[", i, "]] (use filled pchs) ...")
                            pchs_unique <- c(pchs_filled_w_border, (21:25)[!match(21:25, pchs_filled_w_border, nomatch=F)]) # declare
                            pchs <- pchs_unique[xp_pchno[[i]]]
                        }
                        if (T && any(xp_varname_unique[[i]]  == "d18Ononp_scaled_trend")) {
                            message("--> pch special d18Ononp_scaled_trend: show plus/minus")
                            inds <- which(xp_varname[[i]]  == "d18Ononp_scaled_trend")
                            pchs[inds] <- NA
                            text_list[[i]]$x[inds] <- attributes(xp[[i]])$lon[inds]
                            text_list[[i]]$y[inds] <- attributes(xp[[i]])$lat[inds]
                            posinds <- which(xp[[i]][inds] >= 0)
                            neginds <- which(xp[[i]][inds] < 0)
                            if (length(posinds) > 0) text_list[[i]]$labels[inds][posinds] <- "+"
                            if (length(neginds) > 0) text_list[[i]]$labels[inds][neginds] <- "-"
                        }
                        if (T && any(xp_fname_unique[[i]] == "PLOT" & xp_varname_unique[[i]]  == "d18Odiatom_scaled_trend")) {
                            inds <- which(xp_fname[[i]] == "PLOT" & xp_varname[[i]]  == "d18Odiatom_scaled_trend")
                            message("--> pch special PLOT d18Odiatom_scaled: show PLOT letters at ", length(inds), " locations")
                            pchs[inds] <- NA # dont show symbols on lon,lat plot; use the PLOT letters instead
                        }
                        if (!is.null(pchs)) point_list[[i]]$pch <- pchs

                    } # if not all xp[[i]] are NA
                } # for i in z
                plotname_suffix <- paste0("_vs_point_datap", point_datap_suffix) 
            } # if xp is defined

            # add land contoures
            if (addland) { # this is lon,lat plot section
                addland_list <- lapply(vector("list", l=length(z)), 
                                       base::append, 
                                       list(data="world", xlim="xlim", ylim="ylim"))
                if (any(models == "mpiom1")) { 
                    inds <- which(models == "mpiom1")
                    addland_list[inds] <- NA
                }
                if (any(models == "fesom")) { 
                    inds <- which(models == "fesom")
                    addland_list[inds] <- NA
                }
            }
            
            # add mpiom land sea mask contours
            mpiom_lsm_segments <- ls(pattern=glob2rx("mpiom_*_land_sea_mask_segments_lon*"))
            if (F && length(mpiom_lsm_segments) > 0) {
                segment_list <- vector("list", l=length(z))
                inds <- seq_along(z) # change here which settings to include
                message("add mpiom land sea mask segments to sub-plots ", 
                        paste(names_legend[inds], collapse=", "), " ...")
                for (i in inds) {
                    segment_list[[i]] <- mpiom_GR30s_land_sea_mask_segments_lon180
                    #segment_list[[i]] <- mpiom_GR15s_land_sea_mask_segments_lon180
                    #segment_list[[i]] <- mpiom_TP04s_land_sea_mask_segments_lon180
                }
                if (addland) addland_list[] <- NA
            } else {
                message("did not find `mpiom_*_land_sea_mask_segments_lon*` in current work space")
            }

            if (exists("add_echam_TR31GR30_oromea_contour") && add_echam_TR31GR30_oromea_contour) {
                message("special: add echam oromea contours to plot ...")
                levels <- image.plot.pre(zlim=range(echam_TR31GR30_oromea$OROMEA), nlevels=5)$levels
                contour_list <- list(x=NULL, y=NULL, z=NULL, levels=levels, drawlabels=F)
                for (i in seq_along(z)) {
                    contour_list$x[[i]] <- echam_TR31GR30_oromea$lon
                    contour_list$y[[i]] <- echam_TR31GR30_oromea$lat
                    contour_list$z[[i]] <- echam_TR31GR30_oromea$OROMEA
                }
            }
            
            # show PLOT coords
            if (exists("PLOT_coords_cmd_list")) {
                message("special: add PLOT coords to plot ...")
                cmd_list <- c(cmd_list, PLOT_coords_cmd_list)
            }

            # determine number of rows and columns
            n <- m <- zoomfac <- NULL
            if (T && all(areas_p == "N30-90") || all(areas_p == "NAsiberia")) {
                message("NAsiberia special nrow ncol")
                n <- length(z); m <- 1
                y_at <- pretty(d$lat[[1]], n=5)
            }
            if (T && length(z) == 2) {
                message("special 2 cols instead of 2 rows (default)")
                n <- 1; m <- 2
            }
            if (grepl("+ortho", proj)) {
                zoomfac <- 1.066
                message("`proj` = \"", proj, "\" --> set `zoomfac` = ", zoomfac)
            }
            source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
            nm <- image.plot.nxm(x=d$lon, y=d$lat, z=z, n=n, m=m, ip=ip, dry=T)

            # open plot device 
            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                               zname, "_", 
                               paste0(names_short_p, "_", seasonsp_p, 
                                      "_", froms_plot_p, "_to_", tos_plot_p, "_", 
                                      areas_p, collapse="_vs_"), 
                               plotname_suffix)
            # add projection since this is lon vs lat plot
            if (proj != "") plotname <- paste0(plotname, "_", gsub("\\s+", "_", proj))
            plotname <- paste0(plotname, ".", p$plot_type)
            if (nchar(plotname) > nchar_max_foutname) {
                message("plotname \"", plotname, "\" too long ...")
                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   zname, "_", 
                                   paste(unique(names_short_p), collapse="_"), "_",
                                   paste(unique(seasonsp_p), collapse="_"), "_", 
                                   paste(unique(froms_plot_p), collapse="_"), "-", 
                                   paste(unique(tos_plot_p), collapse="_"), "_", 
                                   paste(unique(areas_p), collapse="_"), 
                                   plotname_suffix)
                if (proj != "") plotname <- paste0(plotname, "_", gsub("\\s+", "_", proj))
                plotname <- paste0(plotname, ".", p$plot_type)
            }
            if (nchar(plotname) > nchar_max_foutname) {
                message("plotname \"", plotname, "\" too long ...")
                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   zname, "_", 
                                   #paste(unique(names_short_p), collapse="_"), "_",
                                   #names_short_p[1], "_", # only 1 setting to decrease plotname size
                                   names_short_p[1], "_", names_short_p[length(z)], "_", # only 1st and last setting to decrease plotname size
                                   paste(unique(seasonsp_p), collapse="_"), "_", 
                                   paste(unique(froms_plot_p), collapse="_"), "-", 
                                   paste(unique(tos_plot_p), collapse="_"), "_", 
                                   plotname_suffix)
                if (proj != "") plotname <- paste0(plotname, "_", gsub("\\s+", "_", proj))
                plotname <- paste0(plotname, ".", p$plot_type)
            }
            if (nchar(plotname) > nchar_max_foutname) {
                stop("plotname too long")
            }
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            message("open plot ", plotname, " ...")
            # this is the lon vs lat plot: respecting aspect ratio based on dlon and dlat make sense here 
            source("~/scripts/r/functions/myfunctions.r")
            asp <- sapply(lapply(d$lon, range), diff)/sapply(lapply(d$lat, range), diff) # dlon/dlat per setting
            asp <- max(asp) # one asp for potentially differnet dlon/dlat per setting
            asp <- min(asp, aspect_ratio_thr) # not too high aspect ratio for map plot
            pp <- plot_sizes(width_in=nm$ncol*p$map_width_in, asp=asp, verbose=T)
            #pp <- plot_sizes(width_in=nm$ncol*p$map_width_in, height_in=nm$nrow*p$map_width_in/asp, verbose=T)
            if (p$plot_type == "png") {
                png(plotname, width=pp$png_width_px, height=pp$png_height_px,
                    pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
                    family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
                #library(showtext)
                #showtext_auto() 
                #def <- get(".PDF.Options.default", envir = grDevices:::.PSenv)
            }

            # map plot
            source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
            image.plot.nxm(x=d$lon, y=d$lat, z=z, ip=ip, 
                           n=nm$nrow, m=nm$ncol,
                           add_grid=add_grid, proj=proj, zoomfac=zoomfac,
                           y_at=y_at,
                           xlab="Longitude [°]", ylab="Latitude [°]", 
                           zlab=data_info$label, 
                           znames_method=znames_method, znames_pos=znames_pos,
                           znames_cex=znames_cex, znames_labels=names_legend_p, 
                           #useRaster=F,
                           add_contour=F,
                           polygon_list=polygon_list,
                           quiver_list=quiver_list,
                           contour_only=contour_only,
                           contour_list=contour_list, 
                           #contour_posneg_redblue=T, 
                           #contour_smooth=T, contour_smooth_n_pixel_thr=50, contour_spar=1,
                           addland_list=addland_list,
                           point_list=point_list,
                           segment_list=segment_list,
                           text_list=text_list,
                           cmd_list=cmd_list,
                           legend.line=legend.line,
                           colorbar.cex=colorbar.cex,
                           verbose=T)
            
            message("save plot ", plotname, " ...")
            if (p$plot_type != "active") dev.off()
            write(plotname, file=lastfiles_plot_fname, append=T)
            if (p$plot_type == "pdf") {
                if (T) {
                    message("run `", p$pdf_embed_fun, "()` ...")
                    if (p$pdf_embed_fun == "grDevices::embedFonts") {
                        grDevices::embedFonts(plotname, outfile=plotname)
                    } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                        extrafont::embed_fonts(plotname, outfile=plotname)
                    }
                } else {
                    message("todo: sometimes pdf font embedding blurrs colors why?")
                }
            }
            #stop("asd")

            # lon vs lat anomaly plot of 2 settings 
            if (plot_lon_lat_anomaly && length(z) == 2) {

                message("\n\n`plot_lon_lat_anomaly` = T and `length(z)` = 2 ", appendLF=F)

                if (length(d$lon[[1]]) == length(d$lon[[2]]) &&
                    length(d$lat[[1]]) == length(d$lat[[2]])) {
                
                    message("AND both settings have same number of lons and lats\n",
                            "--> plot anomalies setting 2 minus 1: ", 
                            names_short_p[2], " minus ", names_short_p[1], 
                            " as lon vs lat ...\n")
                 
                    # add stuff to plot
                    addland_list <- cmd_list <- segment_list <- polygon_list <- quiver_list <- NULL
                    
                    # add land contoures
                    if (addland) { # this is lon,lat anom plot section
                        addland_list <- lapply(vector("list", l=1), 
                                               base::append, 
                                               list(data="world", xlim="xlim", ylim="ylim"))
                        if (any(models == "mpiom1")) addland_list[] <- NA 
                        if (any(models == "fesom")) addland_list[] <- NA 
                    }

                    # add mpiom land sea mask contours
                    mpiom_lsm_segments <- ls(pattern=glob2rx("mpiom_*_land_sea_mask_segments_lon*"))
                    if (T && length(mpiom_lsm_segments) > 0) {
                        message("add mpiom land sea mask segments")
                        segment_list <- list(mpiom_GR30s_land_sea_mask_segments_lon180)
                        #segment_list <- list(mpiom_GR15s_land_sea_mask_segments_lon180)
                        #segment_list <- list(mpiom_TP04s_land_sea_mask_segments_lon180)
                        if (addland) addland_list[] <- NA
                    }
                    
                    if (exists("PLOT_coords_cmd_list")) {
                        message("special: add PLOT coords to plot ...")
                        cmd_list <- c(cmd_list, PLOT_coords_cmd_list)
                    }
            
                    # add quiver anomaly
                    if (plot_groups[plot_groupi] == "samevars") { # todo: implement zuv for samedims
                        if (exists("varnames_uv") && !any(is.na(zuv))) {
                            message("\nadd `zuv_samevars[[", zname, "]]` to lon vs lat anom plot quiver_list:")
                            cat(capture.output(str(zuv)), sep="\n")
                            stop("update")
                            if (!is.na(zuv[[1]]) && !is.na(zuv[[2]])) {
                                quiver_list <- list(u=list(zuv[[2]]$u - zuv[[1]]$u),
                                                    v=list(zuv[[2]]$v - zuv[[1]]$v))
                                quiver_list$nx_fac <- rep(0.5, t=length(quiver_list$u))
                                quiver_list$ny_fac <- rep(0.75, t=length(quiver_list$u))
                                quiver_list$const <- rep(T, t=length(quiver_list$u))
                            }
                        }
                    }

                    # anomaly colorbar values
                    zanom <- list(z[[2]] - z[[1]])
                    names(zanom) <- paste0(names_legend_p[2], " minus ", names_legend_p[1])
                    
                    message("define lon vs lat anom color levels here if wanted ...")
                    message("get global min/max anom zlim ... ", appendLF=F)
                    zlim <- range(zanom, na.rm=T)
                    op <- options()$digits; options(digits=15); cat("=", zlim, "\n"); options(digits=op)
                    
                    zlevels <- NULL
                    if (zname == "co2_flx_ocean") {
                        message("co2_flx_ocean special anom levels")
                        zlevels <- seq(max(zlim[1], -10), min(zlim[2], 10), b=1)
                        if (zlim[1] < min(zlevels)) zlevels <- c(zlim[1], zlevels)
                        if (zlim[2] > max(zlevels)) zlevels <- c(zlevels, zlim[2])
                    } else if (any(zname == c("curltau", "ekmanP_ms"))) {
                        zlevels <- quantile(zanom[[1]], probs=c(0.1, 0.9), na.rm=T)
                        zlevels <- max(abs(range(zlevels)))
                        zlevels <- c(zlim[1], pretty(c(-zlevels, zlevels), n=11), zlim[2])
                    }
                    source(paste0(host$homepath, "/functions/image.plot.pre.r"))
                    ip <- image.plot.pre(zlim=zlim, zlevels=zlevels, verbose=F)
                    if (any(zlim[1] < min(ip$levels))) warning("zlim[1] < min(ip$levels) in lon vs lat anom plot. do you want that?")
                    if (any(zlim[2] > max(ip$levels))) warning("zlim[2] > max(ip$levels) in lon vs lat anom plot. do you want that?")

                    # determine number of rows and columns
                    source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
                    nm <- image.plot.nxm(x=d$lon[1], y=d$lat[1], z=zanom, ip=ip, dry=T)

                    plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                       varname, "_", 
                                       paste0(rev(names_short_p), "_", rev(seasonsp_p), 
                                              "_", rev(froms_plot_p), "_to_", rev(tos_plot_p), "_", 
                                              rev(areas_p), collapse="_minus_")) 
                    if (proj != "") plotname <- paste0(plotname, "_", gsub("\\s+", "_", proj))
                    plotname <- paste0(plotname, ".", p$plot_type)
                    message("plot ", plotname, " ...")
                    dir.create(dirname(plotname), recursive=T, showWarnings=F)
                    pp <- plot_sizes(width_in=nm$ncol*p$map_width_in, height_in=nm$nrow*p$map_width_in/asp, verbose=T)
                    if (p$plot_type == "png") {
                        png(plotname, width=pp$png_width_px, height=pp$png_height_px,
                            pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
                    } else if (p$plot_type == "pdf") {
                        pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
                            family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
                    }

                    # map plot
                    image.plot.nxm(x=d$lon[1], y=d$lat[1], z=zanom, ip=ip, verbose=T,
                                   xlab="Longitude [°]", ylab="Latitude [°]", 
                                   zlab=data_info$label, 
                                   znames_labels=paste0(names_short_p[2], " minus ", names_short_p[1]),
                                   add_contour=F,
                                   addland_list=addland_list,
                                   quiver_list=quiver_list,
                                   segment_list=segment_list,
                                   cmd_list=cmd_list)
                    
                    message("\nsave plot ", plotname, " ...")
                    dev.off()
                    write(plotname, file=lastfiles_plot_fname, append=T)
                    if (p$plot_type == "pdf") {
                        if (T) {
                            message("run `", p$pdf_embed_fun, "()` ...")
                            if (p$pdf_embed_fun == "grDevices::embedFonts") {
                                grDevices::embedFonts(plotname, outfile=plotname)
                            } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                                extrafont::embed_fonts(plotname, outfile=plotname)
                            }
                        } else {
                            message("todo: sometimes pdf font embedding blurrs colors why?")
                        }
                    }

                } else { # if lon_dim and lat_dim of both settings are of same length
                    message("but lon and lat dims of both settings are of different length --> cannot plot anomaly as lon vs lat")
                }
            } else { # if !plot_lon_lat_anomaly or length(z) != 2
                message("\n`plot_lon_lat_anomaly` = F or length(z) != 2 --> cannot plot anomaly as lon vs lat")
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
            cmd_list <- addland_list <- NULL

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
                message("temp2 tslm1 THO special zlim")
                #zlim <- c(-0.566165227890015, 2.67736038208008) # annual
                #zlim <- c(-2.46028671264648, 2.82926086425781) # jun
                #zlim <- c(-0.738620281219482, 6.65275375366211) # dec
                message("min, max = ", zlim[1], ", ", zlim[2])
                addland_list <- vector("list", l=length(z))
                for (i in seq_along(z)) {
                    addland_list[[i]] <- list(data="world", ylim="ylim")
                }
            }
                
            if (T && exists("mpiom_GR30s_land_sea_mask_segments_lon180")) {
                message("special: add mpiom land sea mask segments to plot ...")
                addland_list <- vector("list", l=length(z))
                for (i in seq_along(z)) {
                    addland_list[[i]] <- list(data=mpiom_GR30s_land_sea_mask_segments_lon180, 
                                              ylim="ylim")
                    if (T) { # use regbox xlims
                        addland_list[[i]]$xlim <- range(lapply(regboxes, "[[", "lons")) 
                    }
                }
            }

            if (exists("PLOT_coords_cmd_list")) {
                message("special: add PLOT coords to plot ...")
                cmd_list <- c(cmd_list, PLOT_coords_cmd_list)
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
            
            pp <- plot_sizes(width_in=p$map_width_in, asp=p$map_asp, verbose=T)
            if (p$plot_type == "png") {
                png(plotname, width=nm$ncol*pp$png_width_px, height=nm$nrow*pp$png_height_px,
                    pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=nm$ncol*pp$pdf_width_in, height=nm$nrow*pp$pdf_height_in,
                    family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
            }

            # plot
            nm <- image.plot.nxm(x=d$time, y=d$lat, z=z
                           #, n=1, m=2 # special
                           #, n=1, m=3 # special
                           , n=2, m=2 # special
                           , ip=ip, verbose=T,
                           individual_zlim=T,
                           contour_only=T, 
                           contour_posneg_soliddashed=F,
                           contour_posneg_redblue=T,
                           #add_contour=T,
                           addland_list=addland_list,
                           cmd_list=cmd_list
                           #xlim=tlimct, 
                           , x_at=tatn, x_labels=tlablt, xlab=tunit, 
                           ylab="Latitude [°]",
                           zlab=data_info$label, znames_labels=names_legend_p)
        
            message("\nsave plot ", plotname, " ...")
            dev.off()
            write(plotname, file=lastfiles_plot_fname, append=T)
            if (p$plot_type == "pdf") {
                if (T) {
                    message("run `", p$pdf_embed_fun, "()` ...")
                    if (p$pdf_embed_fun == "grDevices::embedFonts") {
                        grDevices::embedFonts(plotname, outfile=plotname)
                    } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                        extrafont::embed_fonts(plotname, outfile=plotname)
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

            if (add_ts_to_time_vs_depth) {
                message("\nadd time series to datas time vs depth plot ...")

            } # if add_ts_to_time_vs_depth

            # colorbar values
            zlim <- range(z, na.rm=T)
            zlevels <- axis.labels <- NULL
            if (T && varname == "thetao") {
                message("special thetao levels")
                if (F) { # spinup drift absolute vals
                    zlevels <- c(seq(0, 3, b=0.1), seq(4, 11, b=1))
                    if (zlim[1] < min(zlevels)) zlevels <- c(round(zlim[1], 2), zlevels)
                    if (zlim[2] > max(zlevels)) zlevels <- c(zlevels, round(zlim[2], 2))
                    axis.labels <- zlevels
                } else if (F) { # spinup drift anomaly versus year 1
                    zlevels <- c(seq(round(zlim[1], 1), 0, b=0.25), seq(0.1, round(zlim[2], 1), b=0.1))
                    if (zlim[1] < min(zlevels)) zlevels <- c(round(zlim[1], 2), zlevels)
                    if (zlim[2] > max(zlevels)) zlevels <- c(zlevels, round(zlim[2], 2))
                    axis.labels <- zlevels
                } else if (T) { # transition piControl and esm-piControl upper 50 m
                    zlevels <- c(16, 16.5, 17)
                    axis.labels <- zlevels
                }
            }
            source(paste0(host$homepath, "/functions/image.plot.pre.r"))
            ip <- image.plot.pre(zlim, zlevels=zlevels, axis.labels=axis.labels, verbose=T)

            # determine number of rows and columns
            source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
            nm <- image.plot.nxm(x=d$time, y=d$depth, z=z, ip=ip, dry=T)

            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                               varname, "_", 
                               paste0(names_short_p, "_", areas_p, "_", seasonsp_p, "_", 
                                      froms_plot_p, "_to_", tos_plot_p, "_", 
                                      depth_fromsp_p, "-", depth_tosp_p, "m",
                                      collapse="_vs_"), 
                               plotname_suffix, ".", p$plot_type)
            message("plot ", plotname, " ...")
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            pp <- plot_sizes(width_in=p$ts_width_in, asp=p$ts_asp, verbose=T)
            if (p$plot_type == "png") {
                png(plotname, width=nm$ncol*pp$png_width_px, height=nm$nrow*pp$png_height_px,
                    pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=nm$ncol*pp$pdf_width_in, height=nm$nrow*pp$pdf_height_in,
                    family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
            }

            # plot
            image.plot.nxm(x=d$time, y=d$depth, z=z, ip=ip, verbose=T,
                           #xlim=tlimct, 
                           contour_only=T,
                           #contour_posneg_soliddashed=F, contour_posneg_redblue=T,
                           x_at=tatn, x_labels=tlablt, xlab=tunit, 
                           ylab=depthunit,
                           zlab=data_info$label, znames_labels=names_legend_p,
                           useRaster=F)
        
            message("\nsave plot ", plotname, " ...")
            dev.off()
            write(plotname, file=lastfiles_plot_fname, append=T)
            if (p$plot_type == "pdf") {
                if (T) {
                    message("run `", p$pdf_embed_fun, "()` ...")
                    if (p$pdf_embed_fun == "grDevices::embedFonts") {
                        grDevices::embedFonts(plotname, outfile=plotname)
                    } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                        extrafont::embed_fonts(plotname, outfile=plotname)
                    }
                } else {
                    message("todo: sometimes pdf font embedding blurrs colors why?")
                }
            }

        } # if ndims == 2 and time, depth
        # finished plot `datas` as time vs depth


        ## plot `datas` as time vs lev
        if (ndims == 2 && all(dim_names == c("time", "lev"))) {
       
            if (add_smoothed) {
                message("\n`add_smoothed` = T --> replace z with zma ...")
                z <- zma
            }

            if (add_ts_to_time_vs_depth) {
                message("\nadd time series to datas time vs depth plot ...")

            } # if add_ts_to_time_vs_depth

            # colorbar values
            zlim <- range(z, na.rm=T)
            zlevels <- axis.labels <- NULL
            source(paste0(host$homepath, "/functions/image.plot.pre.r"))
            ip <- image.plot.pre(zlim, zlevels=zlevels, axis.labels=axis.labels, verbose=T)

            # determine number of rows and columns
            source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
            nm <- image.plot.nxm(x=d$time, y=d$lev, z=z, ip=ip, dry=T)

            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                               varname, "_", 
                               paste0(names_short_p, "_", areas_p, "_", seasonsp_p, "_", 
                                      froms_plot_p, "_to_", tos_plot_p, "_", 
                                      lev_fromsp_p, "-", lev_tosp_p, "m",
                                      collapse="_vs_"), 
                               plotname_suffix, ".", p$plot_type)
            message("plot ", plotname, " ...")
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            pp <- plot_sizes(width_in=p$ts_width_in, asp=p$ts_asp, verbose=T)
            if (p$plot_type == "png") {
                png(plotname, width=nm$ncol*pp$png_width_px, height=nm$nrow*pp$png_height_px,
                    pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=nm$ncol*pp$pdf_width_in, height=nm$nrow*pp$pdf_height_in,
                    family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
            }

            # plot
            image.plot.nxm(x=d$time, y=d$lev, z=z, ip=ip, verbose=T,
                           #xlim=tlimct, 
                           contour_only=T,
                           #contour_posneg_soliddashed=F, contour_posneg_redblue=T,
                           x_at=tatn, x_labels=tlablt, xlab=tunit, 
                           ylab=levunit,
                           zlab=data_info$label, znames_labels=names_legend_p,
                           useRaster=F)
        
            message("\nsave plot ", plotname, " ...")
            dev.off()
            write(plotname, file=lastfiles_plot_fname, append=T)
            if (p$plot_type == "pdf") {
                if (T) {
                    message("run `", p$pdf_embed_fun, "()` ...")
                    if (p$pdf_embed_fun == "grDevices::embedFonts") {
                        grDevices::embedFonts(plotname, outfile=plotname)
                    } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                        extrafont::embed_fonts(plotname, outfile=plotname)
                    }
                } else {
                    message("todo: sometimes pdf font embedding blurrs colors why?")
                }
            }

        } # if ndims == 2 and time, lev
        # finished plot `datas` as time vs lev


        ### 1 dim
        ## plot `datas` as time 
        if (ndims == 1 && dim_names == "time") {

            if (!add_unsmoothed && !add_smoothed) {
                warning("both `add_unsmoothed=F` and `add_smoothed=F`. set `add_unsmoothed=T` to show time series.")
                add_unsmoothed <- T # default
            }

            message("\n", varname, " ", mode_p, " plot vs time ...")
            
            # prepare right axis data if necessary
            # save user choice
            if (plot_groupi == 1 && ploti == 1) add_data_right_yaxis_ts_save <- add_data_right_yaxis_ts 
            # re-initiate user choice in every plot group (e.g. samevars, samedims)
            if (ploti == 1) add_data_right_yaxis_ts <- add_data_right_yaxis_ts_save 
            if (add_data_right_yaxis_ts) {
                message("\nprepare data right yaxis ...")
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
                if (F) { # cmip stuff
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
                                                                         y=base::scale(ncvar_get(ncin, "tau_aero_550")),
                                                                         text="Aerosol optical thickness", col="#377EB8", lty=1, 
                                                                         lwd=0.5, pch=NA),
                                                         "tsi_hist"=list(x=tsi_hist_annual$time, y=base::scale(tsi_hist_annual$tsi_hist),
                                                                         text="Total solar irradiance", 
                                                                         col=tsi_hist_annual$col, lty=tsi_hist_annual$lty, 
                                                                         lwd=tsi_hist_annual$lwd, pch=tsi_hist_annual$pch),
                                                         "co2_hist"=list(x=co2_hist$time, y=base::scale(co2_hist$co2_ppm),
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
                } else if (F && varname == "divuvt_budget") {
                    message("add mld to right yaxis")
                    data_right <- list(data=vector("list", l=1))
                    names(data_right$data) <- "MLD"
                    for (i in seq_along(data_right$data)) {
                        inpath <- paste0(host$workpath, "/post/", models[i], "/fldmean/mixlay") 
                        fname <- paste0(prefixes[i], "_", models[i], "_fldmean_mixlay_", 
                                        areas_p[i], "_", 
                                        seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                                        ".nc")
                        ncin <- nc_open(paste0(inpath, "/", fname))
                        data_right$data[[i]] <- list(x=d$time[[i]],
                                                     y=ncvar_get(ncin, "mixlay_mean")/1000,
                                                     text="MLD",
                                                     col="black",
                                                     lty=1, lwd=0.5, pch=NA)
                        if (T) {
                            message("use March MLD")
                            inds <- as.POSIXlt(data_right$data[[i]]$x)
                            inds <- which(inds$mon+1 == 3)
                            data_right$data[[i]]$x <- data_right$data[[i]]$x[inds]
                            data_right$data[[i]]$y <- data_right$data[[i]]$y[inds]
                        }
                    }
                    data_right$label <- eval(substitute(expression(paste("March MLD"[paste(sigma[theta], "=0.125 kg m"^-3)], " [km]"))))
                    data_right$lepos <- "topright"
                    data_right$suffix <- "_with_mld"
                } else if (F) {
                    message("add depth integrated mke to right axis ...")
                    data_right <- list(data=vector("list", l=length(z)))
                    names(data_right$data)[] <- "MKE"
                    for (i in seq_along(data_right$data)) {
                        inpath <- paste0(host$workpath, "/post/", models[i], "/fldint/mke") 
                        fname <- paste0(prefixes[i], "_", models[i], "_fldint_mke_")
                        if (grepl("Low01", prefixes[i])) fname <- paste0(fname, "int0-3600m")
                        if (grepl("LSea5", prefixes[i])) fname <- paste0(fname, "int0-4150m")
                        fname <- paste0(fname, "_",
                                        areas_p[i], "_", 
                                        seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                                        ".nc")
                        ncin <- nc_open(paste0(inpath, "/", fname))
                        data_right$data[[i]] <- list(x=d$time[[i]],
                                                     y=ncvar_get(ncin, "mke")/1e11,
                                                     n_ma=n_mas[i],
                                                     text="MKE",
                                                     col=cols[i],
                                                     lty=2, lwd=1, pch=NA)
                    }
                    data_right$label <- eval(substitute(expression(paste(integral(), " MKE dV [m"^5, " s"^-2, "] " %*% "", 10^11))))
                    data_right$lepos <- "topright"
                    data_right$suffix <- "_with_mke"
                } else if (F) {
                    message("add depth integrated eke to right axis ...")
                    data_right <- list(data=vector("list", l=length(z)))
                    names(data_right$data)[] <- "EKE"
                    for (i in seq_along(data_right$data)) {
                        inpath <- paste0(host$workpath, "/post/", models[i], "/fldint/eke") 
                        fname <- paste0(prefixes[i], "_", models[i], "_fldint_eke_")
                        if (grepl("Low01", prefixes[i])) fname <- paste0(fname, "int0-3600m")
                        if (grepl("LSea5", prefixes[i])) fname <- paste0(fname, "int0-4150m")
                        fname <- paste0(fname, "_",
                                        areas_p[i], "_", 
                                        seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                                        ".nc")
                        ncin <- nc_open(paste0(inpath, "/", fname))
                        data_right$data[[i]] <- list(x=d$time[[i]],
                                                     y=ncvar_get(ncin, "eke")/1e11,
                                                     n_ma=n_mas[i],
                                                     text="EKE",
                                                     col=cols[i],
                                                     lty=2, lwd=1, pch=NA)
                    }
                    data_right$label <- eval(substitute(expression(paste(integral(), " EKE dV [m"^5, " s"^-2, "] " %*% "", 10^11))))
                    data_right$lepos <- "topright"
                    data_right$suffix <- "_with_eke"
                } else if (F) {
                    message("add depth integrated eke_over_tke percent to right axis ...")
                    data_right <- list(data=vector("list", l=length(z)))
                    names(data_right$data)[] <- "eke_over_tke"
                    for (i in seq_along(data_right$data)) {
                        inpath <- paste0(host$workpath, "/post/", models[i], "/fldint/eke_over_tke") 
                        fname <- paste0(prefixes[i], "_", models[i], "_fldint_eke_over_tke_")
                        if (grepl("Low01", prefixes[i])) fname <- paste0(fname, "int0-3600m")
                        if (grepl("LSea5", prefixes[i])) fname <- paste0(fname, "int0-4150m")
                        fname <- paste0(fname, "_",
                                        areas_p[i], "_", 
                                        seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                                        ".nc")
                        ncin <- nc_open(paste0(inpath, "/", fname))
                        data_right$data[[i]] <- list(x=d$time[[i]],
                                                     y=ncvar_get(ncin, "eke")*100,
                                                     n_ma=n_mas[i],
                                                     text="EKE",
                                                     col=cols[i],
                                                     lty=2, lwd=1, pch=NA)
                        if (F) {
                            message("use March EKE percent")
                            inds <- as.POSIXlt(data_right$data[[i]]$x)
                            inds <- which(inds$mon+1 == 1)
                            #inds <- which(inds$mon+1 == 3)
                            data_right$data[[i]]$x <- data_right$data[[i]]$x[inds]
                            data_right$data[[i]]$y <- data_right$data[[i]]$y[inds]
                        }
                    }
                    data_right$label <- "EKE [%]"
                    data_right$lepos <- "bottomright"
                    #data_right$lepos <- c(as.numeric(as.POSIXct("1991-1-1")), 52.5)
                    data_right$suffix <- "_with_eke_over_tke"
                } else if (F) {
                    message("add depth integrated vrs to right axis ...")
                    data_right <- list(data=vector("list", l=length(z)))
                    names(data_right$data)[] <- "VRS"
                    for (i in seq_along(data_right$data)) {
                        inpath <- paste0(host$workpath, "/post/", models[i], "/fldint/VRS") 
                        fname <- paste0(prefixes[i], "_", models[i], "_fldint_VRS_")
                        if (grepl("Low01", prefixes[i])) fname <- paste0(fname, "int0-3600m")
                        if (grepl("LSea5", prefixes[i])) fname <- paste0(fname, "int0-4150m")
                        fname <- paste0(fname, "_",
                                        areas_p[i], "_", 
                                        seasonsf[i], "_", fromsf[i], "-", tosf[i], 
                                        ".nc")
                        ncin <- nc_open(paste0(inpath, "/", fname))
                        data_right$data[[i]] <- list(x=d$time[[i]],
                                                     y=ncvar_get(ncin, "VRS")/1e4,
                                                     n_ma=n_mas[i],
                                                     text="VRS",
                                                     col=cols[i],
                                                     lty=2, lwd=1, pch=NA)
                    }
                    data_right$label <- eval(substitute(expression(paste(integral(), " VRS ",
                                                                         " dV [", var1^5, " ", var2^-3,
                                                                         "] " %*% "", 10^4)),
                                                        list(var1="m", var2="s")))
                    data_right$lepos <- "bottomright"
                    data_right$suffix <- "_with_vrs"
                
                } # load data_right based on variable

                # modify loaded data_right
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
                    warning("provided `add_data_right_yaxis_ts` = T but no data_right was defined for\n",
                            "  zname = \"", zname, "\"; `plot_groups[", plot_groupi, "] = \"", 
                            plot_groups[plot_groupi], "; `ploti` = ", ploti, "\n", 
                            "  --> set `add_data_right_yaxis_ts=F` and continue ...")
                    add_data_right_yaxis_ts <- F
                    data_right <- list(suffix="") # default
                }

            } else { # add_data_right_yaxis_ts=F
                data_right <- list(suffix="") # default
            } # if add_data_right_yaxis_ts

            # data_right after check
            if (add_data_right_yaxis_ts) {
                nsettings_right <- length(data_right$data)
                if (add_smoothed) {
                    for (i in seq_len(nsettings_right)) {
                        if (!is.null(data_right$data[[i]]$n_ma) && !is.na(data_right$data[[i]]$n_ma)) {
                            data_right$data[[i]]$yma <- filter(data_right$data[[i]]$y, 
                                                               rep(1/data_right$data[[i]]$n_ma, 
                                                                   t=data_right$data[[i]]$n_ma))
                        } else {
                            message("no `data_right$data[[", i, "]]$n_ma` defined")
                            data_right$data[[i]]$yma <- data_right$data[[i]]$y
                        }
                    }
                }

                if (!exists("ylim_right")) { # possibly set by user
                    message("use automatic `data_right` yaxis limits ...")
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
                
                # modify `ylim_right`
                if (T && varname == "siarean") {
                    message("add nsidc annual to right ylim ...")
                    ylim_right <- range(ylim_right, nsidc_siextents_annual$siareas, na.rm=T)
                } else if (T && varname == "divuvt_budget") {
                    message("special divuvt_budget MLD ylims")
                    # March MLD low01_s52: c(1.68917355509734, 3.14356900893414) km
                    # March MLD lsea5_s5: c(0.131685243362329, 3.31555376653867) km
                    ylim_right <- c(0.131685243362329, 3.31555376653867)
                }
                message("ylim_right=", appendLF=F)
                dput(ylim_right)
                ylim_right[is.infinite(ylim_right)] <- 0
                if (!exists("yat_right")) {
                    message("use automatic `data_right` yaxis labels ...")
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

            # data_upper after check
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
                    message("use automatic `data_upper` xaxis limits ...")
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
                        z[[i]] <- base::scale(z[[i]], scale=F)
                        if (exists("zma")) zma[[i]] <- base::scale(zma[[i]], scale=F)
                    } else if (scale_ts) {
                        z[[i]] <- base::scale(z[[i]])
                        if (exists("zma")) zma[[i]] <- base::scale(zma[[i]])
                    }
                }
            } # if center_ts or scale_ts

            ## ylim model
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
            
            # update ylim according to ylim_upper
            if (add_data_upper_xaxis_ts) {
                message("\nupdate left yaxis ylim = ", paste(ylim, collapse=", "), 
                        " with ylim_upper = ", paste(ylim_upper, collapse=", "), " ...") 
                ylim <- range(ylim, ylim_upper)
            } 


            ## add data to left yaxis before any model/obs data
            message("\nprepare additional left yaxis before any model/obs data:")
            data_left_before <- list() # default: dont add additional data to left yaxis before any model/obs data
            
            # add to data_left_before
            if (T && exists("nao")) {
                nao_name <- colnames(nao)[2]
                message("add NAO data \"", nao_name, " to data_left_before for divuvt_budget ...")
                inds <- which(nao$time >= tlim[1] & nao$time <= tlim[2])
                nao <- nao[inds,]
                data_left_before[[length(data_left_before)+1]] <- list(x=nao$time, 
                                                                       y=nao[[nao_name]],
                                                                       type="bar",
                                                                       col=col2rgba(c("red", "blue"), 0.1),
                                                                       pch=NA, lty=NA, lwd=NA,
                                                                       lepos="topleft",
                                                                       #lepos="bottomright", 
                                                                       #ncol=2,
                                                                       #lepos=c(as.numeric(as.POSIXct("1986-1-1")), -1.4), 
                                                                       #lepos=c(as.numeric(as.POSIXct("1985-1-1")), -1.75), 
                                                                       #lepos=c(as.numeric(as.POSIXct("1963-1-1")), 2.66),
                                                                       text=c(eval(substitute(expression(paste("NAO", + "")))),
                                                                              eval(substitute(expression(paste("NAO", - "")))))
                                                                       )
                names(data_left_before)[length(data_left_before)] <- "nao"
            } # if add som obs to ylim
            
            # check data_left_before
            if (length(data_left_before) > 0) {
                for (i in seq_along(data_left_before)) {
                    if (is.null(data_left_before[[i]]$type)) data_left_before[[i]]$type <- "p" # default: point
                    if (is.null(data_left_before[[i]]$col)) data_left_before[[i]]$col <- i
                    if (is.null(data_left_before[[i]]$col_rgb)) col2rgba(data_left_before[[i]]$col, alpha_rgb)
                    if (is.null(data_left_before[[i]]$pch)) data_left_before[[i]]$pch <- 1
                    if (is.null(data_left_before[[i]]$lty)) data_left_before[[i]]$lty <- 1
                    if (is.null(data_left_before[[i]]$lwd)) data_left_before[[i]]$lwd <- 1
                    if (is.null(data_left_before[[i]]$n_ma)) data_left_before[[i]]$n_ma <- 1 # no low-pass filter
                    if (is.null(data_left_before[[i]]$lepos)) data_left_before[[i]]$lepos <- NA
                    if (is.null(data_left_before[[i]]$ncol)) data_left_before[[i]]$ncol <- 1
                    if (is.null(data_left_before[[i]]$text)) data_left_before[[i]]$text <- "set text"
                } # for i in data_left_before
            } # if length(data_left_before) > 0

            # apply moving average on data_left_before
            if (length(data_left_before) > 0) {
                for (i in seq_along(data_left_before)) {
                    if (data_left_before[[i]]$n_ma != 1) {
                        stop("implement")
                    }
                }
            } # if length(data_left_before) > 0

            # scale data_left_before
            if (length(data_left_before) > 0) {
                if (center_ts || scale_ts) {
                    if (center_ts) {
                        message("\n`center_ts` = T --> center data_left_before before plot ...")
                    } else if (scale_ts) {
                        message("\n`scale_ts` = T --> scale data_left_before before plot ...")
                    }
                    for (i in seq_along(data_left_before)) {
                        if (center_ts) {
                            data_left_before[[i]]$y <- base::scale(data_left_before[[i]]$y, scale=F)
                            if (!is.null(data_left_before[[i]]$y_lower)) {
                                data_left_before[[i]]$y_lower <- base::scale(data_left_before[[i]]$y_lower, scale=F)
                            }
                            if (!is.null(data_left_before[[i]]$y_upper)) {
                                data_left_before[[i]]$y_upper <- base::scale(data_left_before[[i]]$y_upper, scale=F)
                            }
                        } else if (scale_ts) {
                            data_left_before[[i]]$y <- base::scale(data_left_before[[i]]$y)
                            if (!is.null(data_left_before[[i]]$y_lower)) {
                                data_left_before[[i]]$y_lower <- base::scale(data_left_before[[i]]$y_lower)
                            }
                            if (!is.null(data_left_before[[i]]$y_upper)) {
                                data_left_before[[i]]$y_upper <- base::scale(data_left_before[[i]]$y_upper)
                            }
                        }
                    }
                }
            } # if length(data_left_before) > 0

            # update ylim according to additional data_left_before (e.g. obs)
            if (length(data_left_before) > 0) {
                ylim_left_before <- range(lapply(data_left_before, "[[", "y"), na.rm=T)
                message("\nupdate `ylim_left_before` = ", paste(ylim_left_before, collapse=", "), 
                        " with ylim_left_before = ", paste(ylim_left_before, collapse=", "), " ...") 
                ylim_left_before_lower <- lapply(data_left_before, "[[", "y_lower")
                if (!all(sapply(ylim_left_before_lower, is.null))) {
                    ylim_left_before_lower <- range(ylim_left_before_lower, na.rm=T)
                    message("update `ylim_left_before` = ", paste(ylim_left_before, collapse=", "), 
                            " with ylim_left_before_lower = ", paste(ylim_left_before_lower, collapse=", "), " ...") 
                    ylim_left_before <- range(ylim_left_before, ylim_left_before_lower)
                }
                ylim_left_before_upper <- lapply(data_left_before, "[[", "y_upper")
                if (!all(sapply(ylim_left_before_lower, is.null))) {
                    ylim_left_before_upper <- range(ylim_left_before_upper, na.rm=T)
                    message("update `ylim_left_before` = ", paste(ylim_left_before, collapse=", "), 
                            " with ylim_left_before_upper = ", paste(ylim_left_before_lower, collapse=", "), " ...") 
                    ylim_left_before <- range(ylim_left_before, ylim_left_before_upper)
                }
                message("final `ylim_left_before` = ", paste(ylim_left_before, collapse=", "))
            } # if (length(data_left_before) > 0)


            ## add data to left yaxis (e.g. obs)
            message("\nprepare additional left yaxis data (e.g. obs)")
            data_left <- list() # default: dont add additional data to left yaxis
            
            # add to data_left
            if (F && varname == "temp2") {
                message("add hadcrut4_sat_anom, gistempv4_sat_anom to ylim ...")
                data_left[[length(data_left)+1]] <- list(x=as.POSIXct(hadcrut4_sat_anom_annual$time), 
                                                         y=hadcrut4_sat_anom_annual$hadcrut4_sat_anom,
                                                         y_lower=hadcrut4_sat_anom_annual$hadcrut4_sat_anom_lower_uncert,
                                                         y_upper=hadcrut4_sat_anom_annual$hadcrut4_sat_anom_upper_uncert)
            } # if add som obs to ylim

            # add to data_left
            if (F && varname == "moc_max_26.25deg") {
                message("add rapid$moc_annual to ylim ...")
                okinds <- which(!is.na(moc_rapid$moc) & !is.na(moc_rapid$moc_error)) 
                data_left[[length(data_left)+1]] <- list(x=as.POSIXct(moc_rapid$time[okinds]), 
                                                         y=moc_rapid$moc[okinds],
                                                         y_lower=moc_rapid$moc[okinds] - moc_rapid$moc_error[okinds],
                                                         y_upper=moc_rapid$moc[okinds] + moc_rapid$moc_error[okinds])
            } # if add moc ts
            
            # add to data_left
            if (T && varname == "siarean") {
                message("add nsidc annual to ylim ...")
                data_left[[length(data_left)+1]] <- list(x=nsidc_siextentn_annual$time,
                                                         y=nsidc_siextentn_annual$siarean)
            } # if add nsidc
            
            # add to data_left
            if (zname == "wisoaprt_d" && any(names(pg) == "d18o_w_smow")) {
                varind <- which(names(pg) == "d18o_w_smow")
                for (i in seq_along(pg[[varind]])) { # all d18o_w_smow records loaded from pangaea
                    if (all(grepl("elgygytgyn", areas)) && any(names(pg[[varind]]) == "swann_etal_2010")) {
                        doiind <- which(names(pg[[varind]]) == "swann_etal_2010")
                        for (eventi in seq_along(pg[[varind]][[doiind]])) {
                            message("add d18o_smow pangaea event \"", names(pg[[varind]][[doiind]])[eventi], 
                                    "\" from swann_etal_2010 to data_left ...")
                            inds <- seq_along(pg[[varind]][[doiind]][[eventi]]$dims$time)
                            if (T) {
                                message("only use younger than 10k BP ...")
                                inds <- which(pg[[varind]][[doiind]][[eventi]]$dims$time$year + 1900 > -10000)
                            }
                            data_left[[length(data_left)+1]] <- list(x=pg[[varind]][[doiind]][[eventi]]$dims$time[inds],
                                                                     y=pg[[varind]][[doiind]][[eventi]]$data[inds],
                                                                     type="o", col=mycols(9)[9],
                                                                     pch=2,
                                                                     text="F: El’gygytgyn (Swann et al. 2010)")

                        }
                    } # special case elgygytgyn
                } # for all d18o_w_smow pg records

                # thruo
            } # if zname == "wisoaprt_d" && any(names(pg) == "d18o_w_smow"))
            
            # add to data_left
            if (T && any(varname == c("wisoaprt_d", "wisoaprt_d_post", "wisoevap_d", "wisope_d"))) {
                if (T && exists("kostrova_etal_2021") && all(grepl("emanda", areas))) {
                    message("add kostrova et al. 2021 d18o emanda data to data_left ...")
                    inds <- seq_along(kostrova_etal_2021$data$time)
                    if (T) {
                        message("only use data younger than 10k BP ...")
                        inds <- which(kostrova_etal_2021$data$time$year + 1900 > -10000)
                    }
                    data_left[[length(data_left)+1]] <- list(x=kostrova_etal_2021$data$time[inds], 
                                                             y=kostrova_etal_2021$data$d18o_corr_perm[inds],
                                                             type=kostrova_etal_2021$type, col=kostrova_etal_2021$col, 
                                                             pch=kostrova_etal_2021$pch, cex=kostrova_etal_2021$cex,
                                                             lty=kostrova_etal_2021$lty, lwd=kostrova_etal_2021$lwd.,
                                                             text=kostrova_etal_2021$text, legend.pos="topright")
                }
                if (F && exists("kostrova_etal_2019") && all(grepl("ladoga", areas))) {
                    message("add kostrova et al. 2019 d18o ladoga data to data_left ...")
                    data_left[[length(data_left)+1]] <- list(x=kostrova_etal_2019$time, 
                                                             y=kostrova_etal_2019$d18o,
                                                             type=kostrova_etal_2019$type, col=kostrova_etal_2019$col, 
                                                             pch=kostrova_etal_2019$pch, cex=kostrova_etal_2019$cex,
                                                             lty=kostrova_etal_2019$lty, lwd=kostrova_etal_2019$lwd,
                                                             text=kostrova_etal_2019$text)
                }
                if (T && exists("meyer_etal")) {
                    meyer_etal_tmp <- NULL
                    if (all(grepl("ladoga", areas)) && !is.null(meyer_etal$data$"ladoga")) {
                        meyer_etal_tmp <- meyer_etal$data$"ladoga"
                        meyer_etal_tmp$legend.pos <- "topleft"
                    } else if (all(grepl("shuchye", areas)) && !is.null(meyer_etal$data$"shuchye")) {
                        meyer_etal_tmp <- meyer_etal$data$"shuchye"
                        meyer_etal_tmp$legend.pos <- "topright"
                    } else if (all(grepl("emanda", areas)) && !is.null(meyer_etal$data$"emanda")) {
                        meyer_etal_tmp <- meyer_etal$data$"emanda"
                        meyer_etal_tmp$legend.pos <- "topright"
                    } else if (all(grepl("elgygytgyn", areas)) && !is.null(meyer_etal$data$"elgygytgyn")) {
                        meyer_etal_tmp <- meyer_etal$data$"elgygytgyn"
                        meyer_etal_tmp$legend.pos <- "bottomleft"
                    } else if (all(grepl("two-yurts", areas)) && !is.null(meyer_etal$data$"two-yurts")) {
                        meyer_etal_tmp <- meyer_etal$data$"two-yurts"
                        meyer_etal_tmp$legend.pos <- "bottomleft"
                    } else if (all(grepl("kotokel", areas)) && !is.null(meyer_etal$data$"kotokel")) {
                        meyer_etal_tmp <- meyer_etal$data$"kotokel"
                        meyer_etal_tmp$legend.pos <- "bottom"
                    }
                    if (!is.null(meyer_etal_tmp)) {
                        message("add meyer et al. xlsx d18o data to data_left ...")
                        data_left[[length(data_left)+1]] <- list(x=meyer_etal_tmp$data$time,
                                                                 y=meyer_etal_tmp$data$d18o_corr_perm,
                                                                 type=meyer_etal$type, col=meyer_etal$col,
                                                                 pch=meyer_etal$pch, cex=meyer_etal$cex,
                                                                 lty=meyer_etal$lty, lwd=meyer_etal$lwd,
                                                                 text=meyer_etal_tmp$text, 
                                                                 legend.pos=meyer_etal_tmp$legend.pos)
                    }
                } # if exists("meyer_etal")
            } # if any(varname == c("wisoaprt_d", "wisoaprt_d_post", "wisoevap_d", "wisope_d"))

            # add to data_left
            if (T && exists("noaa_ghcdn")) {
                if (any(varname == c("temp2", "tsurf", "aprt"))) {
                    message("add noadd ghcdn monthly data\n", 
                            "check https://github.com/chrisdane/PLOT/blob/master/lakes/lake_coords_closest_GHCDN_stations.txt ...")
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
                    } else if (all(grepl("two-yurts", areas))) {
                        noaa_ghcdn_tmp <- noaa_ghcdn$RSM00032389_KLJUCHI_RS
                    } else if (all(grepl("kotokel", areas))) {
                        noaa_ghcdn_tmp <- noaa_ghcdn$RSM00030731_GORJACINSK_RS
                    } else {
                        stop("noaa ghcdn data not defined for areas\n",
                             paste(areas, collapse=", "))
                    }
                    data_left[[length(data_left)+1]] <- list(x=noaa_ghcdn_tmp$ts$time, 
                                                             type=noaa_ghcdn_tmp$type, col=noaa_ghcdn_tmp$col,
                                                             pch=noaa_ghcdn$pch, cex=noaa_ghcdn$cex,
                                                             lty=noaa_ghcdn$lty, lwd=noaa_ghcdn$lwd)
                    if (any(varname == c("temp2", "tsurf"))) {
                        data_left[[length(data_left)]]$y <- noaa_ghcdn_tmp$ts$Tavg
                    } else if (varname == "aprt") {
                        data_left[[length(data_left)]]$y <- noaa_ghcdn_tmp$ts$precip
                    }
                } # if temp2, tsurf, aprt
            } # if exists("noaa_ghcdn")
            # finished adding data to data_left

            # check data_left
            if (length(data_left) > 0) {
                for (i in seq_along(data_left)) {
                    if (is.null(data_left[[i]]$type)) data_left[[i]]$type <- "p" # default: point
                    if (is.null(data_left[[i]]$col)) data_left[[i]]$col <- i
                    if (is.null(data_left[[i]]$col_rgb)) col2rgba(data_left[[i]]$col, alpha_rgb)
                    if (is.null(data_left[[i]]$pch)) data_left[[i]]$pch <- 1
                    if (is.null(data_left[[i]]$lty)) data_left[[i]]$lty <- 1
                    if (is.null(data_left[[i]]$lwd)) data_left[[i]]$lwd <- 1
                    if (is.null(data_left[[i]]$n_ma)) data_left[[i]]$n_ma <- 1 # no low-pass filter
                    if (is.null(data_left[[i]]$text)) data_left[[i]]$text <- "set text"
                    if (is.null(data_left[[i]]$legend.pos)) data_left[[i]]$legend.pos <- NA
                } # for i in data_left
            } # if length(data_left) > 0

            # apply moving average on data_left
            if (length(data_left) > 0) {
                for (i in seq_along(data_left)) {
                    if (data_left[[i]]$n_ma != 1) {
                        stop("implement")
                    }
                }
            } # if length(data_left) > 0

            # scale data_left
            if (length(data_left) > 0) {
                if (center_ts || scale_ts) {
                    if (center_ts) {
                        message("\n`center_ts` = T --> center data_left before plot ...")
                    } else if (scale_ts) {
                        message("\n`scale_ts` = T --> scale data_left before plot ...")
                    }
                    for (i in seq_along(data_left)) {
                        if (center_ts) {
                            data_left[[i]]$y <- base::scale(data_left[[i]]$y, scale=F)
                            if (!is.null(data_left[[i]]$y_lower)) {
                                data_left[[i]]$y_lower <- base::scale(data_left[[i]]$y_lower, scale=F)
                            }
                            if (!is.null(data_left[[i]]$y_upper)) {
                                data_left[[i]]$y_upper <- base::scale(data_left[[i]]$y_upper, scale=F)
                            }
                        } else if (scale_ts) {
                            data_left[[i]]$y <- base::scale(data_left[[i]]$y)
                            if (!is.null(data_left[[i]]$y_lower)) {
                                data_left[[i]]$y_lower <- base::scale(data_left[[i]]$y_lower)
                            }
                            if (!is.null(data_left[[i]]$y_upper)) {
                                data_left[[i]]$y_upper <- base::scale(data_left[[i]]$y_upper)
                            }
                        }
                    }
                }
            } # if length(data_left) > 0

            # update ylim according to additional data_left (e.g. obs)
            if (length(data_left) > 0) {
                ylim_left <- range(lapply(data_left, "[[", "y"), na.rm=T)
                message("\nupdate left yaxis ylim = ", paste(ylim, collapse=", "), 
                        " with ylim_left = ", paste(ylim_left, collapse=", "), " ...") 
                ylim <- range(ylim, ylim_left)
                ylim_left_lower <- lapply(data_left, "[[", "y_lower")
                if (!all(sapply(ylim_left_lower, is.null))) {
                    ylim_left_lower <- range(ylim_left_lower, na.rm=T)
                    message("update left yaxis ylim = ", paste(ylim, collapse=", "), 
                            " with ylim_left_lower = ", paste(ylim_left_lower, collapse=", "), " ...") 
                    ylim <- range(ylim, ylim_left_lower)
                }
                ylim_left_upper <- lapply(data_left, "[[", "y_upper")
                if (!all(sapply(ylim_left_lower, is.null))) {
                    ylim_left_upper <- range(ylim_left_upper, na.rm=T)
                    message("update left yaxis ylim = ", paste(ylim, collapse=", "), 
                            " with ylim_left_upper = ", paste(ylim_left_lower, collapse=", "), " ...") 
                    ylim <- range(ylim, ylim_left_upper)
                }
            } # if (length(data_left) > 0)

            # special manual ylim
            if (F) { # special PLOT ylim
                message("special PLOT ylim")
                if (center_ts) {
                    # ladoga: c(-3.30543235329369, 1.8024062688112) 
                    # shuchye: c(-3.31588729672117, 5.07708695323018)
                    # emanda: c(-1.69386684350292, 0.902706467116598)
                    # elgygytgyn: c(-0.859484244878168, 0.837003798075255)
                    # two-yurts: c(-2.05770537576561, 1.94485402526289)
                    # kotokel: c(-4.32039527896077, 2.06724117367673)
                    ylim <- c(-4.32039527896077, 5.07708695323018)
                } else if (scale_ts) {
                    ## meyer et al. 10k BP 
                    # ladoga: c(-2.71845068304792, 1.12244344859212)
                    # shuchye: c(-2.06540423594796, 2.95748401109693)
                    # emanda: c(-1.98885622964664, 2.40795779371568)
                    # kotokel: c(-3.08014665995613, 1.35190103264372)
                    # elgygytgyn: c(-1.47405790466153, 1.61041540752816)
                    # two-yurts: c(-2.1518519172735, 2.0338372598759)
                    # hol-t, hol-tx10; n_mas 250, 50
                    # ladoga: c(-2.85126486366215, 2.32484231975009)
                    # shuchye: c(-2.85992909997141, 2.44526416112507)
                    # emanda: c(-2.06748945256795, 2.55525657817665)
                    # kotokel: c(-2.60277676884169, 2.96409565621687)
                    # elgygytgyn: c(-2.37444352133196, 3.04432535587108)
                    # two-yurts: c(-2.70199788713988, 3.21519386227393) 
                    ylim <- c(-3.08014665995613, 3.215193862273933)
                }
            } # special PLOT ylim
            if (T && varname == "divuvt_budget") { 
                message("special divuvt_budget ylim ...")
                # grl paper Fig. 2 b-c
                # low01: c(-1.26363203719386, 0.371054098020657)
                # lsea5: c(-1.28651984464644, 1.04161513583757)
                #ylim <- c(-1.28651984464644, 1.04161513583757) 
                # grl paper Fig. S3 a-b
                # low01: c(-4.66742440959941, 3.586661964737)
                # lsea5: c(-4.65197020531014, 3.9722749057429)
                #ylim <- c(-4.66742440959941, 3.9722749057429)
                ylim <- c(-4.66742440959941, 4.66742440959941)
            }
            if (F && varname == "FeKe") {
                # low01: 1.36183324948389, 2.14823189882532 (ts); 0.637694378449628, 2.62736108678509 (mon)
                # lsea5: 1.96378045687908, 3.16459677978894 (ts); 0.83301010079561, 3.7371943178027 (mon)
                message("special FeKe ylim ...")
                ylim <- c(0.637694378449628, 3.7371943178027)
            }

            # increase ylim for legend if many settings
            if (F && length(z) > 6) {
                message("\ninrease ylim for ts legend ...")
                ylim[2] <- ylim[2] + 0*diff(ylim)
            }
            
            message("final ylim=", appendLF=F)
            dput(ylim)
            yat <- pretty(ylim, n=6) # 8
            ylab <- format(yat, trim=T)
            
            # plotname
            if (exists("plotprefix")) {
                plotprefixp <- plotprefix
            } else { # default
                plotprefixp <- paste(unique(names_short_p), collapse="_vs_")
                if (F && plot_groups[plot_groupi] == "samedims") { # use only varnames_out_samedims below
                    plotprefixp <- paste0(plotprefixp, "_", paste(unique(varnames_in_p), collapse="_vs_"))
                }
                plotprefixp <- paste0(plotprefixp,
                                      "_", paste(unique(seasonsp_p), collapse="_vs_"), 
                                      "_", paste(unique(froms_plot_p), collapse="_vs_"), 
                                      "_to_", paste(unique(tos_plot_p), collapse="_vs_"), 
                                      paste(unique(n_mas_fname_p), collapse="_vs_"), 
                                      "_", paste(unique(areas_p), collapse="_vs_"), 
                                      collapse="_vs_")
            }
            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                               varname, "_", # individual samevars or common samedims varname
                               plotprefixp, 
                               "_", plot_groups[plot_groupi], # samevars or samedims
                               data_right$suffix, data_upper$suffix, ts_highlight_seasons$suffix,
                               plotname_suffix,
                               "_ts.", p$plot_type)
            if (nchar(plotname) > nchar_max_foutname) {
                stop("plotname too long. define `plotprefix` in plot namelist")
            }
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            
            # get plot sizes
            message("open plot ", plotname, " ...")
            source("~/scripts/r/functions/myfunctions.r") 
            if (F) {
                message("special ts plot size")
                #pp <- plot_sizes(width_in=p$map_width_in, asp=p$map_asp, verbose=T)
                pp <- plot_sizes(width_in=p$ts_width_in, asp=1, verbose=T)
            } else { # default
                pp <- plot_sizes(width_in=p$ts_width_in, asp=p$ts_asp, verbose=T)
            }
            if (p$plot_type == "png") {
                png(plotname, width=pp$png_width_px, height=pp$png_height_px,
                    pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
                    family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
            }

            # set plot margins
            mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
            mar[4] <- 1 # decrease right margin
            if (!add_title && !add_data_upper_xaxis_ts) mar[3] <- 1 # decrease upper margin
            if (tlabsrt == 0) {
                message("decrease lower margin vs time")
                mar[1] <- mar[1]/1.5  # decrease lower margin
            }
            if (add_data_right_yaxis_ts) mar[4] <- mar[2] # right same as left  

            # open plot
            message("mar = ", paste(mar, collapse=", "))
            par(mar=mar)
            
            # add data_left_before
            if (length(data_left_before) > 0) {
                message("\nadd data_left_before to plot ...")
                for (i in seq_along(data_left_before)) {
                    if (i == 1) { # initialize plot
                        plot(data_left_before[[i]]$x, data_left_before[[i]]$y, 
                             t="n", ylim=ylim_left_before,
                             xlab=NA, ylab=NA, axes=F)
                    }
                    if (!is.null(names(data_left_before)[i])) {
                        message("add data_left_before[[", i, "]] = \"", 
                                names(data_left_before)[i], "\" ...")
                    }
                    if (data_left_before[[i]]$type == "bar") {
                        pos_inds <- which(data_left_before[[i]]$y >= 0)
                        neg_inds <- which(data_left_before[[i]]$y < 0)
                        dt <- diff(data_left_before[[i]]$x)[1] * 0.5
                        if (length(pos_inds) > 0) {
                            rect(xleft=data_left_before[[i]]$x[pos_inds] - dt, 
                                 ybottom=0,
                                 xright=data_left_before[[i]]$x[pos_inds] + dt, 
                                 ytop=data_left_before[[i]]$y[pos_inds],
                                 col=data_left_before[[i]]$col[1], border=NA)
                        } 
                        if (length(neg_inds) > 0) {
                            rect(xleft=data_left_before[[i]]$x[neg_inds] - dt, 
                                 ybottom=0,
                                 xright=data_left_before[[i]]$x[neg_inds] + dt, 
                                 ytop=data_left_before[[i]]$y[neg_inds],
                                 col=data_left_before[[i]]$col[2], border=NA)
                        }
                        if (add_legend_left_yaxis_before && !is.na(data_left_before[[i]]$lepos)) {
                            message("add data_left_before vs time legend ...")
                            lepostmp <- data_left_before[[i]]$lepos
                            if (length(lepostmp) == 2) {
                                legend(lepostmp[1], lepostmp[2],
                                       legend=data_left_before[[i]]$text, 
                                       col=data_left_before[[i]]$col, lty=data_left_before[[i]]$lepos,
                                       lwd=data_left_before[[i]]$lwd, 
                                       #pch=data_left_before[[i]]$pch,
                                       pch=15, pt.cex=1.75,
                                       bty="n", x.intersp=-0.2, ncol=data_left_before[[i]]$ncol)
                            } else {
                                legend(lepostmp,
                                       legend=data_left_before[[i]]$text, 
                                       col=data_left_before[[i]]$col, lty=data_left_before[[i]]$lepos,
                                       lwd=data_left_before[[i]]$lwd, 
                                       #pch=data_left_before[[i]]$pch,
                                       pch=15, pt.cex=1.75,
                                       bty="n", x.intersp=-0.2, ncol=data_left_before[[i]]$ncol)
                            }
                        } # if add_legend_left_yaxis_before
                    } else {
                        stop("`data_left_before[[", i, "]]$type` = \"", data_left_before[[i]]$type, "\" not defined")
                    }
                } # for i in data_left_before

                message("finished adding data_left_before --> run `par(new=T)` ...")
                par(new=T) # initiate new coords for following model/obs data

            } # if length(data_left_before) > 0
            
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
            mtext(side=1, tunit, line=2, cex=1)
            #mtext(side=1, tunit, line=2, cex=0.9)

            # add variable label on y-axis
            #label_line <- 2.5
            label_line <- 3
            #label_line <- 3.4
            #label_line <- 3.5
            #label_line <- 4
            #label_line <- 4.5
            #label_cex <- 1
            label_cex <- 0.9
            #label_cex <- 0.75
            message("\nput datas vs time label in `label_line` = ", label_line, 
                    " distance with `label_cex` = ", label_cex, " ...")
            mtext(side=2, data_info$label, line=label_line, cex=label_cex)
            
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
                            season_ind <- which(names(known_seasons) == season)
                            if (length(season_ind) != 1) {
                                stop("implement season \"", season, "\"")
                            } else {
                                season_numbers <- known_seasons[[season_ind]]$inds
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
                        message("\nadd default stuff to ", mode_p, " datas season legend ...")
                        le <- list()
                        if (suppressPackageStartupMessages(require(Hmisc))) {
                            tmp <- Hmisc::largest.empty(x=unlist(d$time), y=unlist(z), method="area")
                            le$pos <- c(x=min(tmp$rect$x), y=max(tmp$rect$y))
                        } else {
                            le$pos <- "bottom" 
                        }
                        le$ncol <- length(ts_highlight_seasons$seasons)
                        le$cex <- lecex
                        le$cex <- 0.85
                        le$text <- ts_highlight_seasons$seasons
                        le$col <- ts_highlight_seasons$cols
                        le$pch <- ts_highlight_seasons$pchs
                        le$lty <- rep(NA, t=length(ts_highlight_seasons$seasons))
                        le$lwd <- rep(NA, t=length(ts_highlight_seasons$seasons))
                        # reorder reading direction from R's default "top-to-bottom-and-then-left-to-right" 
                        # to "left-to-right-and-then-top-to-bottom"
                        if (T) le <- reorder_legend(le)
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
                        if (types_p[i] == "l" && lwds_p[i] == 0) warning("intent to draw line with zero width", .immediate=T)
                        if (types_p[i] == "p" && is.na(pchs_p[i])) warning("intent to draw points with NA character", .immediate=T)
                        if (length(z[[i]]) == 1) {
                            if (types_p[i] == "l") {
                                message("length(z[[", i, "]] = 1 --> switch type = \"", types_p[i], "\" to \"", appendLF=F)
                                types_p[i] <- "p"
                                message(types_p[i], "\"")
                            }
                            if (is.na(pchs_p[i])) {
                                message("length(z[[", i, "]] = 1 --> switch pch = NA to \"", appendLF=F)
                                pch <- c(pchs_hollow, seq_len(255)[-pchs_hollow]) # declare
                                pchs_p[i] <- pch[i]
                                message(pchs_p[i], "\"")
                            }
                        }
                        points(d$time[[i]], z[[i]], type=types_p[i], 
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
            lm_labels <- names_legend_p
            if (any(add_linear_trend)) {
                message("\ncalc and add linear trend against time with `stats::lm()` ...")
                for (i in seq_along(z)) {
                    if (add_linear_trend[i]) {
                        lm_nyear <- length(add_linear_trend_froms[i]:add_linear_trend_tos[i]) # not accurate
                        message("\nsetting ", i, " ", names_short_p[i], " from ", 
                                add_linear_trend_froms[i], " to ", add_linear_trend_tos[i],
                                " (", lm_nyear, " years)")
                        inds <- which(dims[[i]]$timelt$year+1900 >= add_linear_trend_froms[i] &
                                      dims[[i]]$timelt$year+1900 <= add_linear_trend_tos[i])
                        if (length(inds) == 0) {
                            warning("lm: no times found within `add_linear_trend_froms[", i, "]`=", add_linear_trend_froms[i], 
                                    " and `add_linear_trend_tos[", i, "]`=", add_linear_trend_tos[i], 
                                    " --> use complete time series instead for lm")
                            inds <- seq_along(z[[i]])
                        }
                        lm <- stats::lm(z[[i]][inds] ~ d$time[[i]][inds])
                        lm_summary <- summary(lm)
                        print(lm_summary)
                        lm_trend_tot <- lm$fitted.values[length(lm$fitted.values)] - lm$fitted.values[1]
                        lm_from_to <- d$time[[i]][range(inds)] # exact
                        lm_dt_tot_day <- as.numeric(difftime(max(lm_from_to), min(lm_from_to), units="day"))
                        lm_dt_tot_a <- lm_dt_tot_day/365.25
                        lm_trend_a <- lm_trend_tot/lm_dt_tot_a
                        lm_trend_day <- lm_trend_tot/lm_dt_tot_day
                        if (lm_dt_tot_a >= 1) { # show trend per year
                            lm_from_to_pretty <- paste(dims[[i]]$timelt[range(inds)]$year+1900, collapse="-")
                            lm_dt_tot_pretty <- diff(dims[[i]]$timelt[range(inds)]$year) + 1
                            attributes(lm_dt_tot_pretty)$units <- "a"
                            if (lm_nyear >= 10) { # show trend per decade
                                if (lm_nyear >= 100) { # show trend per century
                                    if (lm_nyear >= 1000) { # show trend per millenium
                                        lm_trend_pretty <- lm_trend_tot * 1000 / lm_dt_tot_a
                                        lm_dt_pretty <- 1000; attributes(lm_dt_pretty)$units <- "a" # --> /1000a
                                    } else { # < 1000 a
                                        lm_trend_pretty <- lm_trend_tot * 100 / lm_dt_tot_a
                                        lm_dt_pretty <- 100; attributes(lm_dt_pretty)$units <- "a" # --> /100a
                                    }
                                } else { # < 100 a
                                    lm_trend_pretty <- lm_trend_tot * 10 / lm_dt_tot_a
                                    lm_dt_pretty <- 10; attributes(lm_dt_pretty)$units <- "a" # --> /10a
                                }
                            } else { # < 10 a
                                lm_trend_pretty <- lm_trend_tot * 1 / lm_dt_tot_a
                                lm_dt_pretty <- ""; attributes(lm_dt_pretty)$units <- "a" # --> /a
                            }
                        } else if (lm_dt_tot_day > 31) { # show trend per month
                            lm_from_to_pretty <- unique(dims[[i]]$timelt[range(inds)]$year+1900)
                            lm_dt_tot_pretty <- diff(dims[[i]]$timelt[range(inds)]$mon) + 1
                            attributes(lm_dt_tot_pretty)$units <- "mon"
                            lm_trend_pretty <- lm_trend_tot * 30.5 / lm_dt_tot_day
                            lm_dt_pretty <- ""; attributes(lm_dt_pretty)$units <- "mon" # --> /mon
                        } else if (lm_dt_tot_day <= 31) { # show trend per day
                            stop("not yet")
                        }
                        # calc time when trend line reaches zero (if possible) 
                        lm_zero_time <- NA
                        if (lm$coefficients[2] != 0) { # trend line has some slope
                            if (lm$fitted.values[length(lm$fitted.values)] >= 0 && lm$coefficients[2] < 0 || # positive fit values with negative slope
                                lm$fitted.values[length(lm$fitted.values)] < 0 && lm$coefficients[2] > 0) { # negative fit values with positive slope
                                lm_zero_time <- abs(lm$coefficients[1]/lm$coefficients[2]) # intercept/slope = [x]/([x/time]) = [time]
                                lm_zero_time <- as.POSIXlt(lm_zero_time, o="1970-1-1", tz="UTC")
                                lm_zero_dt_day <- as.numeric(difftime(lm_zero_time, max(d$time[[i]][inds]), unit="day"))
                                lm_zero_dt_a <- lm_zero_dt_day/365.25
                            }
                        }
                        # add trend results to legend label
                        if (F) { # add trend only to label
                            lm_labels[i] <- eval(substitute(expression(paste(lab, " (tr", ""[lm_from_to_pretty], "=", sign, trend, " ", unit, "/", lm_dt_unit, ")")),
                                                            list(lab=names_legend_p[i],
                                                                 lm_from_to_pretty=lm_from_to_pretty,
                                                                 sign=ifelse(lm_summary$coefficients[2,"Estimate"] > 0, "+", "")
                                                                 , trend=round(lm_trend_pretty, 3), 
                                                                 #, trend=round(lm_trend_tot, 4), 
                                                                 unit=data_info$units
                                                                 , lm_dt_unit=paste0(lm_dt_pretty, attributes(lm_dt_pretty)$units)
                                                                 #, lm_dt_unit=paste0(lm_dt_tot_pretty, attributes(lm_dt_tot_pretty)$units)
                                                                 )
                                                            )
                                                )
                        } else if (T) { # add mean and trend to label
                            if (!is.na(lm_zero_time)) { # add time when trend crosses zero
                                lm_labels[i] <- eval(substitute(expression(paste(lab, " (", mu[lm_from_to_pretty], "=", muval, 
                                                                                 "; tr=", sign, trend, " ", unit, "/", lm_dt_unit, " " 
                                                                                 %->% " 0 in ", lm_zero_dt, ")")),
                                                                list(lab=names_legend_p[i],
                                                                     lm_from_to_pretty=lm_from_to_pretty,
                                                                     muval=round(mean(z[[i]][inds], na.rm=T), 2),
                                                                     sign=ifelse(lm_summary$coefficients[2,"Estimate"] > 0, "+", "")
                                                                     , trend=round(lm_trend_pretty, 3), 
                                                                     #, trend=round(lm_trend_tot, 4),
                                                                     unit=data_info$units
                                                                     , lm_dt_unit=paste0(lm_dt_pretty, attributes(lm_dt_pretty)$units),
                                                                     #, lm_dt_unit=paste0(lm_dt_tot_pretty, attributes(lm_dt_tot_pretty)$units),
                                                                     lm_zero_dt=paste0(round(lm_zero_dt_a, 2), "a")
                                                                     )
                                                                )
                                                    )
                            } else {
                                lm_labels[i] <- eval(substitute(expression(paste(lab, " (", mu[lm_from_to_pretty], "=", muval, 
                                                                                 "; tr=", sign, trend, " ", unit, "/", lm_dt_unit, ")")),
                                                                list(lab=names_legend_p[i],
                                                                     lm_from_to_pretty=lm_from_to_pretty,
                                                                     muval=round(mean(z[[i]][inds], na.rm=T), 2),
                                                                     sign=ifelse(lm_summary$coefficients[2,"Estimate"] > 0, "+", "")
                                                                     , trend=round(lm_trend_pretty, 3),
                                                                     #, trend=round(lm_trend_tot, 4),
                                                                     unit=data_info$units
                                                                     , lm_dt_unit=paste0(lm_dt_pretty, attributes(lm_dt_pretty)$units)
                                                                     #, lm_dt_unit=paste0(lm_dt_tot_pretty, attributes(lm_dt_tot_pretty)$units)
                                                                     )
                                                                )
                                                    )
                            }
                        } # if add trend to label
                        message("--> total trend ", round(lm_trend_tot, 4), " ", data_info$units, " in ",
                                lm_dt_tot_a, " years ~ ", lm_dt_tot_day, " days from ",
                                min(lm_from_to), " to ", max(lm_from_to), "\n",
                                "--> trend per year = ", lm_trend_a, " ", data_info$units, "\n", 
                                "--> trend per day = ", lm_trend_day, " ", data_info$units, "\n", 
                                "--> last trend value = ", lm$fitted.values[length(lm$fitted.values)], " ", data_info$units)
                        if (!is.na(lm_zero_time)) {        
                            message("--> linear trendline crosses zero ", data_info$units, " at ", lm_zero_time, ", i.e. in ", 
                                    round(lm_zero_dt_a, 3), " years ~ ", round(lm_zero_dt_day, 3), " days")
                        }
                        # plot regression line within data limits only
                        if (T) {
                            message("draw linear regression line within regression limits only ...")
                            lines(d$time[[i]][inds], lm$fitted.values, 
                                  col=cols_p[i], lwd=lwds_p[i], 
                                  #lty=ltys_p[i]
                                  lty=ltys_p[i] + 1
                                  )
                        # or plot line through whole plot with regression coefficients
                        } else if (F) {
                            message("draw linear regression line through whole plot ...")
                            abline(a=lm$coefficients[1], b=lm$coefficients[2],
                                   col=cols_p[i], lwd=lwds_p[i], 
                                   #lty=ltys_p[i]
                                   lty=ltys_p[i] + 1
                                   )
                        }
                    }
                }
            } # add_linear_trend
            
            # add obs, etc.
            if (length(data_left) > 0) {
                message("\nadd data_left to datas vs time ...")

                # add uncertainties if given
                for (i in seq_along(data_left)) {
                    if (!is.null(data_left[[i]]$y_lower) || !is.null(data_left[[i]]$y_upper)) {
                        message("add data_left[[", i, "]]$text = ", data_left[[i]]$text, " uncertainties to plot ...")
                        if (!is.null(data_left[[i]]$y_lower) && !is.null(data_left[[i]]$y_upper)) {
                            polygon(c(data_left[[i]]$x, rev(data_left[[i]]$x)),
                                    c(data_left[[i]]$y_lower, rev(data_left[[i]]$y_upper)),
                                    col=data_left[[i]]$col_rgb, border=NA)
                        } else if (!is.null(data_left[[i]]$y_lower) && is.null(data_left[[i]]$y_upper)) {
                            stop("implement")
                        } else if (is.null(data_left[[i]]$y_lower) && !is.null(data_left[[i]]$y_upper)) {
                            stop("implement")
                        }
                    }
                } # for i in data_left

                # add data points/lines
                for (i in seq_along(data_left)) {
                    message("add data_left[[", i, "]]$text = ", data_left[[i]]$text, " data to plot ...")
                    points(data_left[[i]]$x, data_left[[i]]$y, 
                           type=data_left[[i]]$type, col=data_left[[i]]$col, 
                           pch=data_left[[i]]$pch, cex=data_left[[i]]$cex, 
                           lty=data_left[[i]]$lty, lwd=data_left[[i]]$lwd)
                } # for i in data_left

            } # add obs if length(data_left) > 0

            # add legend if wanted
            if (T && add_legend) {
                message("\nadd default stuff to datas vs time legend ...")
                le <- list()
                legend_pos <- legend_pos_ts
                if (is.null(legend_pos)) {
                    if (F && suppressPackageStartupMessages(require(Hmisc))) { # adagio::maxempty works better
                        message("get legend position automatically with Hmisc::largest.empty() ... ", appendLF=F) 
                        Hmisc_z <- NULL
                        if (add_unsmoothed) Hmisc_z <- c(Hmisc_z, unlist(z))
                        if (add_smoothed) Hmisc_z <- c(Hmisc_z, unlist(zma))
                        tmp <- Hmisc::largest.empty(x=unlist(d$time), y=unlist(Hmisc_z), method="area")
                        le$pos <- c(x=min(tmp$rect$x), y=max(tmp$rect$y)) # topleft corner of Hmisc result
                    } else if (F && suppressPackageStartupMessages(require(adagio))) { # works better than Hmisc::largest.empty
                        message("get legend position automatically with adagio::maxempty() ... ", appendLF=F) 
                        adagio_z <- NULL
                        if (add_unsmoothed) adagio_z <- c(adagio_z, unlist(z))
                        if (add_smoothed) adagio_z <- c(adagio_z, unlist(zma))
                        okinds <- which(!is.na(adagio_z))
                        tmp <- adagio::maxempty(x=unlist(d$time)[okinds], y=adagio_z[okinds], 
                                                ax=par("usr")[1:2], ay=par("usr")[3:4])
                        #rect(tmp$rect[1], tmp$rect[2], tmp$rect[3], tmp$rect[4])
                        le$pos <- c(x=tmp$rect[1], y=tmp$rect[4]) # topleft corner if x- and y-coords are both increasing (default)
                    } else {
                        message("manually set legend position: ", appendLF=F)
                        #le$pos <- "bottom" 
                        #le$pos <- "topleft" 
                        #le$pos <- "left"
                        le$pos <- "bottomleft"
                        #le$pos <- "topright"
                        #le$pos <- "bottomright" 
                        #le$pos <- c(tatn[1], yat[length(yat)-1])
                        #le$pos <- c(as.numeric(as.POSIXct("2650-1-1", tz="UTC")), 13.45)
                        #le$pos <- c(as.numeric(as.POSIXct("100-1-1", tz="UTC")), yat[length(yat)])
                        #le$pos <- c(as.numeric(as.POSIXct("1946-1-1", tz="UTC")), 1.3)
                    }
                } else {
                    le$pos <- legend_pos
                } # if legend_pos is null or not
                message(paste(le$pos, collapse=", "))
                #le$ncol <- ceiling(length(z)/4) 
                #le$ncol <- length(z)
                #le$ncol <- length(z)/2
                le$ncol <- 1
                #le$ncol <- 2
                #le$ncol <- length(z)
                le$cex <- lecex
                #le$cex <- 0.85
                #le$cex <- 0.7
                #le$cex <- 0.66
                #le$cex <- 0.5
                if (varname == "divuvt_budget") {
                    message("divuvt_budget special ncol")
                    le$ncol <- 4
                    le$cex <- 0.75
                }
                if (le$ncol > length(z)) stop("defined more legend columns than data") 
                names_legend_p_w_lm <- names_legend_p
                if (typeof(lm_labels) == "expression") {
                    names_legend_p_w_lm <- lm_labels # use lm result
                    le$cex <- 0.85
                }
                inds <- which(!is.na(names_legend_p)) # throw out user provided NA
                le$text <- names_legend_p_w_lm[inds]
                le$col <- cols_p[inds]
                le$lty <- ltys_p[inds]
                le$lwd <- lwds_p[inds]
                le$pch <- pchs_p[inds]
                for (i in seq_along(inds)) {
                    if (types_p[inds[i]] == "p") {
                        le$lty[inds[i]] <- NA
                    } else if (types_p[inds[i]] == "l") {
                        le$pch[inds[i]] <- NA
                    }
                }

                if (T && all(grepl("shuchye", areas_p))) {
                    message("special shuchye legend ...")
                    le$text <- "B: Bolshoye Shchuchye"
                    le$pos <- "topleft"
                    le$col <- NA; le$lty <- NA; le$lwd <- NA; le$pch <- NA
                }

                # add data_left to legend
                if (length(data_left) > 0) {

                    # add data_left to model legend
                    message("add ", length(data_left), " data_left entries to legend ...")
                    le$text <- c(le$text, sapply(data_left, "[[", "text"))
                    le$col <- c(le$col, sapply(data_left, "[[", "col"))
                    le$lty <- c(le$lty, sapply(data_left, "[[", "lty"))
                    le$lwd <- c(le$lwd, sapply(data_left, "[[", "lwd"))
                    le$pch <- c(le$pch, sapply(data_left, "[[", "pch"))

                    if (F) { # special: replace model legend with data_left legend
                        message("special: replace model legend by ", length(data_left), " data_left entries ...")
                        le$text <- sapply(data_left, "[[", "text")
                        le$col <- sapply(data_left, "[[", "col")
                        le$lty <- sapply(data_left, "[[", "lty")
                        le$lwd <- sapply(data_left, "[[", "lwd")
                        le$pch <- sapply(data_left, "[[", "pch")
                    }
                    
                    if (T && any(!is.na(sapply(data_left, "[[", "legend.pos")))) {
                        message("special legend placement")
                        legend.pos <- sapply(data_left, "[[", "legend.pos")
                        legend.pos <- legend.pos[which(!is.na(legend.pos))]
                        if (length(legend.pos) == 1) {
                            le$pos <- legend.pos
                        } else {
                            stop("found ", length(legend.pos), " legend.pos in data_left. dont know how to continue")
                        }
                    }

                } # if (length(data_left) > 0)

                # reorder reading direction from R's default top->bottom to left->right
                if (T) le <- reorder_legend(le)
                cat(capture.output(str(le, vec.len=length(le$text))), sep="\n")
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

            if (F) {
                message("\nadd special stuff to datas vs time")
                legend("bottomleft", "c", col="black", lty=NA, pch=NA, lwd=NA, bty="n", 
                       x.intersp=-1.8, y.intersp=0.5, cex=1.25)
            }

            # add box before eventual right axis data
            box()

            if (add_data_right_yaxis_ts) {

                message("\n`add_data_right_yaxis_ts` = T --> add data right yaxis ...")
                par(new=T)
                plot(data_right$data[[1]]$x, data_right$data[[1]]$y, #log="y", 
                     t="n", xlim=tlim, ylim=ylim_right, 
                     xlab=NA, ylab=NA, axes=F)
                # add zero line
                #if (add_zeroline) abline(h=0, col="gray", lwd=0.5, lty=2)
                
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
                #data_right_line <- 2.5
                #data_right_line <- 3
                data_right_line <- 3.5
                #data_right_line <- 4
                #data_right_line <- 4.5
                message("add data_right$label at `data_right_line` = ", data_right_line, " ...")
                mtext(side=4, data_right$label, line=data_right_line, cex=0.9, col=right_axis_col)

                # add obs before model data
                if (T && varname == "siarean") {
                    message("\nadd nsidc annual to right plot ...")
                    lines(nsidc_siextents_annual$time, nsidc_siextents_annual$siareas,
                          col=nsidc_siextents_annual$col, lty=nsidc_siextents_annual$lty,
                          lwd=nsidc_siextents_annual$lwd)
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
                    message("\nadd default stuff to right_data vs time legend ...")
                    le <- list()
                    if (!is.null(data_right$lepos)) {
                        le$pos <- data_right$lepos
                    } else {
                        #le$pos <- "topright" 
                        le$pos <- "bottomright" 
                    }
                    le$ncol <- 1
                    le$cex <- lecex
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
                    if (T && length(z) == 2) {
                        message("special: remove 2nd data_right legend entry")
                        le$text <- le$text[1]
                        le$col <- le$col[1]
                        le$lty <- le$lty[1]
                        le$lwd <- le$lwd[1]
                    }
                    # add stuf to legend here
                    if (F) {
                        message("\nadd non default stuff to ", mode_p, " legend ...")
                    }
                    # reorder reading direction from R's default top->bottom to left->right
                    if (T) le <- reorder_legend(le)
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
                    le$cex <- lecex
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
                    if (T) le <- reorder_legend(le)
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
            write(plotname, file=lastfiles_plot_fname, append=T)
            if (p$plot_type == "pdf") {
                if (T) {
                    message("run `", p$pdf_embed_fun, "()` ...")
                    if (p$pdf_embed_fun == "grDevices::embedFonts") {
                        grDevices::embedFonts(plotname, outfile=plotname)
                    } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                        extrafont::embed_fonts(plotname, outfile=plotname)
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
                write(plotname, file=lastfiles_plot_fname, append=T)

            } # if ts_plot_each_setting_in_subplot

        } # if (ndims_unique == 1 && dim_names == "time")
        # finished plot `datas` as time 
        
        if (ndims_unique == 1 && dim_names == "lat") {

            xlim <- range(d$lat, na.rm=T)
            xat <- pretty(xlim, n=20)
            if (any(xat < min(xlim))) {
                inds <- which(xat < min(xlim))
                xat <- xat[-inds]
            }
            if (any(xat > min(xlim))) {
                inds <- which(xat > max(xlim))
                xat <- xat[-inds]
            }
            xlab <- format(xat, trim=T)

            ylim <- range(z, na.rm=T)
            yat <- pretty(ylim, n=8)
            ylab <- format(yat, trim=T)
            
            # plotname
            plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                               varname, "_",
                               paste0(names_short_p, "_", seasonsp_p, "_",
                                      froms_plot_p, "_to_", tos_plot_p, 
                                      areas_p, collapse="_vs_"), 
                               ts_highlight_seasons$suffix,
                               plotname_suffix)
            if (nchar(plotname) > nchar_max_foutname - 4) { # 4 for "_lat"
                if (plot_groups[plot_groupi] == "samevars") {
                    plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                       varname, "_",
                                       paste0(names_short_p, "_", areas_p, collapse="_vs_"), 
                                       plotname_suffix)
                } else if (plot_groups[plot_groupi] == "samedims") {
                    plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                       varname, "_",
                                       paste0(names_short_p, "_", varnames_in_p, collapse="_vs_"), 
                                       plotname_suffix)
                }
            }
            if (nchar(plotname) > nchar_max_foutname - 4) {
                plotname <- substr(plotname, 1, nchar_max_foutname-4)
            }
            plotname <- paste0(plotname, "_lat.", p$plot_type)
            dir.create(dirname(plotname), recursive=T, showWarnings=F)
            
            # get plot sizes
            message("open plot ", plotname, " ...")
            source("~/scripts/r/functions/myfunctions.r") 
            pp <- plot_sizes(width_in=p$ts_width_in, asp=p$ts_asp, verbose=T)
            if (p$plot_type == "png") {
                png(plotname, width=pp$png_width_px, height=pp$png_height_px,
                    pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
            } else if (p$plot_type == "pdf") {
                pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
                    family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
            }

            # set plot margins
            mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
            mar[4] <- 1 # decrease right margin
            if (!add_title && !add_data_upper_xaxis_ts) mar[3] <- 1 # decrease upper margin
            message("decrease lower margin vs lat")
            mar[1] <- mar[1]/1.5  # decrease lower margin
            if (add_data_right_yaxis_ts) mar[4] <- mar[2] # right same as left  

            # open plot
            message("mar = ", paste(mar, collapse=", "))
            par(mar=mar)
            plot(d$lat[[1]], z[[1]], t="n",
                 xlim=xlim, ylim=ylim, 
                 xaxt="n", yaxt="n",
                 xlab=NA, ylab=NA)
            axis(1, at=xat, labels=xlab)
            axis(2, at=yat, labels=ylab, las=2)

            # add time label on x-axis
            mtext(side=1, "Latitude [°]", line=2, cex=1)

            # add variable label on y-axis
            label_line <- 2.5
            #label_line <- 3
            #label_line <- 3.4
            #label_line <- 3.5
            #label_line <- 4
            #label_line <- 4.5
            #label_cex <- 1
            label_cex <- 0.9
            #label_cex <- 0.75
            message("\nput datas vs lat label in `label_line` = ", label_line, 
                    " distance with `label_cex` = ", label_cex, " ...")
            mtext(side=2, data_info$label, line=label_line, cex=label_cex)
            
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
                abline(v=xat, col="gray", lwd=0.5)
            }
            if (add_ygrid) {
                message("\nadd ygrid ...")
                abline(h=yat, col="gray", lwd=0.5)
            }

            # add zero line
            if (add_zeroline) {
                message("`add_zeroline` = T --> add zeroline") 
                abline(h=0, col="gray", lwd=0.5)
            }
            
            # add data
            for (i in seq_along(z)) {
                lines(d$lat[[i]], z[[i]], 
                      col=cols_p[i], lty=ltys_p[i], lwd=lwds_p[i], pch=pchs_p[i])
            }

            # add legend if wanted
            if (T && add_legend) {
                message("\nadd default stuff to datas vs lat legend ...")
                le <- list()
                if (!exists("lepos")) {
                    if (F && suppressPackageStartupMessages(require(Hmisc))) { # adagio::maxempty works better
                        message("get legend position automatically with Hmisc::largest.empty() ... ", appendLF=F) 
                        Hmisc_z <- NULL
                        if (add_unsmoothed) Hmisc_z <- c(Hmisc_z, unlist(z))
                        if (add_smoothed) Hmisc_z <- c(Hmisc_z, unlist(zma))
                        tmp <- Hmisc::largest.empty(x=unlist(d$lat), y=unlist(Hmisc_z), method="area")
                        le$pos <- c(x=min(tmp$rect$x), y=max(tmp$rect$y)) # topleft corner of Hmisc result
                    } else if (F && suppressPackageStartupMessages(require(adagio))) { # works better than Hmisc::largest.empty
                        message("get legend position automatically with adagio::maxempty() ... ", appendLF=F) 
                        adagio_z <- NULL
                        if (add_unsmoothed) adagio_z <- c(adagio_z, unlist(z))
                        if (add_smoothed) adagio_z <- c(adagio_z, unlist(zma))
                        tmp <- adagio::maxempty(x=unlist(d$lat), y=adagio_z, 
                                                ax=par("usr")[1:2], ay=par("usr")[3:4])
                        #rect(tmp$rect[1], tmp$rect[2], tmp$rect[3], tmp$rect[4])
                        le$pos <- c(x=tmp$rect[1], y=tmp$rect[4]) # topleft corner if x- and y-coords are both increasing (default)
                    } else {
                        message("manually set legend position: ", appendLF=F)
                        le$pos <- "bottom" 
                        #le$pos <- "topleft" 
                        #le$pos <- "left"
                        #le$pos <- "bottomleft"
                        #le$pos <- "topright"
                        #le$pos <- "bottomright" 
                        #le$pos <- c(tatn[1], yat[length(yat)-1])
                        #le$pos <- c(as.numeric(as.POSIXct("2650-1-1", tz="UTC")), 13.45)
                        #le$pos <- c(as.numeric(as.POSIXct("2650-1-1", tz="UTC")), yat[length(yat)])
                        #le$pos <- c(as.numeric(as.POSIXct("1946-1-1", tz="UTC")), 1.3)
                    }
                } else { # if exists("lepos") or not
                    le$pos <- lepos
                } # if exists("lepos") or not
                message(paste(le$pos, collapse=", "))
                #le$ncol <- ceiling(length(z)/4) 
                #le$ncol <- length(z)
                #le$ncol <- length(z)/2
                le$ncol <- 1
                #le$ncol <- 2
                #le$ncol <- length(z)
                if (le$ncol > length(z)) stop("defined more legend columns than data") 
                le$cex <- lecex
                #le$cex <- 0.85
                #le$cex <- 0.7
                #le$cex <- 0.75
                #le$cex <- 0.66
                #le$cex <- 0.5
                le$text <- names_legend_p
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

                # reorder reading direction from R's default top->bottom to left->right
                if (T) le <- reorder_legend(le)
                if (T) cat(capture.output(str(le, vec.len=length(le$text))), sep="\n")
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

            message("\nsave plot ", plotname, " ...")
            dev.off()
            write(plotname, file=lastfiles_plot_fname, append=T)
            if (p$plot_type == "pdf") {
                if (T) {
                    message("run `", p$pdf_embed_fun, "()` ...")
                    if (p$pdf_embed_fun == "grDevices::embedFonts") {
                        grDevices::embedFonts(plotname, outfile=plotname)
                    } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                        extrafont::embed_fonts(plotname, outfile=plotname)
                    }
                } else {
                    message("todo: sometimes pdf font embedding blurrs colors why?")
                }
            }

        } # if (ndims_unique == 1 && dim_names == "lat")
        # finished plot `datas` vs lat
        
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
        
        ## plot `datas` as lat vs depth vs time
        if (ndims == 3 && all(dim_names %in% c("lon", "lat", "time"))) { 

            message("\n", zname, " ", mode_p, " plot lon vs lat vs time ...")

            # if 2 settings
            if (length(z) == 2) {

                # calc t-test for differences of means of two 3d data lon vs lat vs time
                if (calc_ttest_lon_lat_time && all.equal(dim(z[[1]])[1:2], dim(z[[2]])[1:2])) {
                    message("\n`calc_ttest_lon_lat_time`=T && nsettings=2 and nlon and nlat of both settings are the same\n",
                            "   --> calc t-test for differences of means of two 3d data lon vs lat vs time ...")

                    ttest_mean <- ttest_mean_lower <- ttest_mean_upper <- array(NA, c(dim=dim(z[[1]])[1:2]))
                    message("\nrun `stats::t.test()` on ", prod(dim(z[[1]])[1:2]),
                            " locations with alternative \"", ttest_alternative, 
                            "\" and conf.level = 1 - ttest_significance = 1 - ", 
                            ttest_significance, " = ", 1 - ttest_significance, " = ", 
                            (1 - ttest_significance)*100, " % ...")
                    for (loni in seq_len(dim(z[[1]])[1])) {
                        for (lati in seq_len(dim(z[[1]])[2])) {
                            ttest <- stats::t.test(x=z[[1]][loni,lati,], y=z[[2]][loni,lati,],
                                                   alternative=ttest_alternative, 
                                                   conf.level=1 - ttest_significance)
                            if (ttest$p.value < ttest_significance) {
                                ttest_mean[loni,lati] <- diff(ttest$estimate) # = mean(y) - mean(x)
                                ttest_mean_lower[loni,lati] <- ttest$conf.int[1] # lower bound of (mean(y)-mean(x))
                                ttest_mean_upper[loni,lati] <- ttest$conf.int[2] # upper bound
                            }
                        } # for lati
                    } # for loni

                    # save t-test result as netcdf
                    varname_out <- paste0(varnames_in[2], "_minus_", varnames_in[1])
                    path_out <- paste0(host$workpath, "/post/",
                                       paste(rev(unique(models)), collapse="_minus_"), 
                                       "/timmean/", varname_out)
                    fout <- paste0(paste(rev(unique(paste0(prefixes, "_", models))), collapse="_minus_"),
                                   "_timmean_", varname_out, "_", 
                                   paste(rev(unique(paste0(areas_p, "_", seasonsp_p, "_", 
                                                           fromsf, "-", tosf))), collapse="_minus_"), # update for fromsf_p, tosf_p
                                   ".nc")
                    dir.create(path_out, recursive=T, showWarnings=F)
                    if (file.exists(paste0(path_out, "/", fout))) file.remove(paste0(path_out, "/", fout))
                    message("\nsave t-test nc ", path_out, "/", fout , " ...")
                    londim <- ncdim_def("lon", "degrees_east", d$lon[[1]]) 
                    latdim <- ncdim_def("lat", "degrees_north", d$lat[[1]])
                    ttest_mean_var <- ncvar_def(zname, data_infos[[2]][[zname]]$units,
                                                list(londim, latdim),
                                                longname=paste0(data_infos[[2]][[zname]]$long_name, " anomaly"))
                    ttest_mean_lower_var <- ttest_mean_upper_var <- ttest_mean_var
                    ttest_mean_lower_var$name <- paste0(ttest_mean_lower_var$name, "_lower_", 
                                                        (1 - ttest_significance)*100, "_conf") 
                    ttest_mean_upper_var$name <- paste0(ttest_mean_upper_var$name, "_upper_", 
                                                        (1 - ttest_significance)*100, "_conf") 
                    ncout <- nc_create(paste0(path_out, "/", fout), 
                                       list(ttest_mean_var, ttest_mean_lower_var, ttest_mean_upper_var),
                                       force_v4=T)
                    ncvar_put(ncout, ttest_mean_var, ttest_mean)
                    ncatt_put(ncout, ttest_mean_var, "anomaly", 
                              paste(rev(names_short_p), collapse=" minus "))
                    ncvar_put(ncout, ttest_mean_lower_var, ttest_mean_lower)
                    ncatt_put(ncout, ttest_mean_lower_var, "anomaly", 
                              paste(rev(names_short_p), collapse=" minus "))
                    ncvar_put(ncout, ttest_mean_upper_var, ttest_mean_upper)
                    ncatt_put(ncout, ttest_mean_upper_var, "anomaly", 
                              paste(rev(names_short_p), collapse=" minus "))
                    ncatt_put(ncout, 0, "History", 
                              paste0(date(), ": stats::t.test(x=", names_short[1], ", y=", names_short[2], 
                                     ", alternative=", ttest_alternative, ", conf.level=", 1-ttest_significance, ")"))
                    ncatt_put(ncout, 0, names_short[1], paste0(inpaths[1], "/", fnames[1]))
                    ncatt_put(ncout, 0, names_short[2], paste0(inpaths[2], "/", fnames[2]))
                    ncatt_put(ncout, 0, paste0(names_short[1], "_n"), length(d$time[[1]]))
                    ncatt_put(ncout, 0, paste0(names_short[2], "_n"), length(d$time[[2]]))
                    ncatt_put(ncout, 0, "alternative", ttest_alternative)
                    ncatt_put(ncout, 0, "conf.level", 1-ttest_significance)
                    nc_close(ncout)
                    
                    
                    ## plot t-test result 
                    plotname <- paste0(plotpath, "/timmean/", zname, "/",
                                       zname, "_", 
                                       paste(rev(paste0(names_short_p, "_", seasonsp_p, 
                                                    "_", froms_plot_p, "_to_", tos_plot_p, "_n",
                                                    sapply(d$time, length), "_",
                                                    areas_p)), collapse="_minus_"), 
                                       plotname_suffix, 
                                       "_ttest_", ttest_alternative, "_", (1 - ttest_significance)*100, "pcnt",
                                       ".", p$plot_type)
                    dir.create(dirname(plotname), recursive=T, showWarnings=F)
                    message("\nplot ", plotname, " ...")
                    
                    # add stuff to plot
                    cmd_list <- segment_list <- polygon_list <- quiver_list <- NULL
                    addland_list <- list(data="world", xlim="xlim", ylim="ylim")
                    
                    if (exists("PLOT_coords_cmd_list")) {
                        message("special: add PLOT coords to plot ...")
                        cmd_list <- c(cmd_list, PLOT_coords_cmd_list)
                    }
                    
                    if (any(models == "mpiom1")) {
                        message("special: add mpiom land sea mask segments to plot ...")
                        segment_list <- mpiom_GR30_lsm_seg
                        addland_list <- NULL
                    }
            
                    # add quiver anomaly
                    if (plot_groups[plot_groupi] == "samevars") { # todo: implement zuv for samedims
                        if (exists("varnames_uv") && !any(is.na(zuv))) {
                            message("\nadd `zuv_samevars[[", zname, "]]` to lon vs lat anom plot quiver_list:")
                            cat(capture.output(str(zuv)), sep="\n")
                            stop("update")
                            if (!is.na(zuv[[1]]) && !is.na(zuv[[2]])) {
                                quiver_list <- list(u=list(zuv[[2]]$u - zuv[[1]]$u),
                                                    v=list(zuv[[2]]$v - zuv[[1]]$v))
                                quiver_list$nx_fac <- rep(0.5, t=length(quiver_list$u))
                                quiver_list$ny_fac <- rep(0.75, t=length(quiver_list$u))
                                quiver_list$const <- rep(T, t=length(quiver_list$u))
                            }
                        }
                    }

                    # colorbar values
                    zanom <- ttest_mean
                    names(zanom) <- paste0(names_short_p[2], " minus ", names_short_p[1])
                    source(paste0(host$homepath, "/functions/image.plot.pre.r"))
                    ip <- image.plot.pre(range(zanom, na.rm=T), verbose=F)

                    # determine number of rows and columns
                    source(paste0(host$homepath, "/functions/image.plot.nxm.r"))
                    nm <- image.plot.nxm(x=d$lon[1], y=d$lat[1], z=zanom, ip=ip, dry=T)
                    
                    if (p$plot_type == "png") {
                        png(plotname, width=nm$ncol*p$map_width, height=nm$nrow*p$map_height,
                            res=p$ppi, family=p$family_png)
                    } else if (p$plot_type == "pdf") {
                        pdf(plotname, width=nm$ncol*p$inch, 
                            height=p$inch*((nm$nrow*p$map_height)/(nm$ncol*p$map_width)),
                            family=p$family_pdf, encoding=encoding)
                    }

                    # map plot
                    image.plot.nxm(x=d$lon[1], y=d$lat[1], z=zanom, ip=ip, verbose=T,
                                   xlab="Longitude [°]", ylab="Latitude [°]", 
                                   zlab=data_info$label, 
                                   znames_labels=paste0(names_short_p[2], " minus ", names_short_p[1]),
                                   contour_only=F,
                                   addland_list=addland_list,
                                   quiver_list=quiver_list,
                                   segment_list=segment_list,
                                   cmd_list=cmd_list)
                    
                    message("\nsave plot ", plotname, " ...")
                    dev.off()
                    write(plotname, file=lastfiles_plot_fname, append=T)
                    if (p$plot_type == "pdf") {
                        if (T) {
                            message("run `", p$pdf_embed_fun, "()` ...")
                            if (p$pdf_embed_fun == "grDevices::embedFonts") {
                                grDevices::embedFonts(plotname, outfile=plotname)
                            } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                                extrafont::embed_fonts(plotname, outfile=plotname)
                            }
                        } else {
                            message("todo: sometimes pdf font embedding blurrs colors why?")
                        }
                    }

                } # if calc_ttest_lon_lat_time
                # finished calc t-test for differences of means of two 3d data lon vs lat vs time
                 
            } # if length(z) == 2
            
        } # if (ndims == 3 && all(dim_names %in% c("lon", "lat", "time"))) {
        # finished plot `datas` as lon vs lat vs time


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
                
                # manual ylim changes
                if (F && varname == "FeKe") {
                    # low01: 1.36183324948389, 2.14823189882532 (ts); 0.637694378449628, 2.62736108678509 (mon)
                    # lsea5: 1.96378045687908, 3.16459677978894 (ts); 0.83301010079561, 3.7371943178027 (mon)
                    message("special FeKe ylim ...")
                    ylim <- c(0.637694378449628, 3.7371943178027)
                }
                
                yat_mon <- pretty(ylim_mon, n=8)
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
                        message("use automatic `data_right_mon` yaxis labels ...")
                        yat_right_mon <- pretty(ylim_right_mon, n=8)
                    }
                    ylab_right_mon <- format(yat_right_mon, trim=T)
                } # if add_data_right_yaxis_ts_mon finished prepare right axis data

                # plotname
                if (exists("plotprefix")) {
                    plotprefixp <- plotprefix
                } else { # default
                    plotprefixp <- paste(unique(names_short_p), collapse="_vs_")
                    if (F && plot_groups[plot_groupi] == "samedims") {
                        plotprefixp <- paste0(plotprefixp, "_", paste(unique(varnames_in_p), collapse="_vs_"))
                    }
                    plotprefixp <- paste0(plotprefixp,
                                          "_", paste(unique(seasonsp_p), collapse="_vs_"), 
                                          "_", paste(unique(froms_plot_p), collapse="_vs_"), 
                                          "_to_", paste(unique(tos_plot_p), collapse="_vs_"), 
                                          "_", paste(unique(areas_p), collapse="_vs_"), 
                                          collapse="_vs_")
                }
                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   varname, "_",
                                   plotprefixp, 
                                   "_", plot_groups[plot_groupi], # samevars or samedims
                                   data_right$suffix, data_upper$suffix, ts_highlight_seasons$suffix,
                                   plotname_suffix,
                                   "_mon.", p$plot_type)
                if (nchar(plotname) > nchar_max_foutname) {
                    stop("plotname too long. define `plotprefix` in plot namelist")
                }
                dir.create(dirname(plotname), recursive=T, showWarnings=F)
               
                # get plot sizes
                message("open plot ", plotname, " ...")
                pp <- plot_sizes(width_in=p$ts_mon_width_in, asp=p$ts_mon_asp, verbose=T)
                if (T && pp$asp == 1) {
                    message("todo: increase pointsize if asp = 1")
                    pp$png_pointsize <- pp$pdf_pointsize <- 14
                }
                if (p$plot_type == "png") {
                    png(plotname, width=pp$png_width_px, height=pp$png_height_px,
                        pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
                } else if (p$plot_type == "pdf") {
                    pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
                        family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
                }

                # set plot margins
                mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                mar[4] <- 1 # decrease right margin
                if (!add_title) mar[3] <- 1 # decrease upper margin
                if (tlabsrt == 0) mar[1] <- mar[1]/1.5  # decrease lower margin
                if (add_data_right_yaxis_ts_mon) mar[4] <- mar[2] # same as left  

                # open plot
                par(mar=mar)
                plot(dmon$month[[1]], zmon[[1]], t="n",
                     xlim=monlim, ylim=ylim_mon, 
                     xaxt="n", yaxt="n",
                     xlab=NA, ylab=NA)
                axis(1, at=monat, labels=NA) # line
                axis(1, at=monat, labels=monlab, line=-0.5, lwd=0, cex.axis=1) # labels with reduced distance to ticks
                axis(2, at=yat_mon, labels=ylab_mon, las=2)
            
                # add time label on x-axis
                #mtext(side=1, "Month", line=2, cex=1)
            
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
                label_line <- 2.5
                #label_line <- 3
                message("\nput datasmon label in `label_line` = ", label_line, " distance ...")
                mtext(side=2, data_info$label, line=label_line, cex=0.9)

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
                    #le$pos <- "topleft" 
                    le$pos <- "top"
                    #le$pos <- "bottom"
                    #le$pos <- "bottomleft" 
                    #le$pos <- "bottomright" 
                    #le$ncol <- nsettings/2
                    le$ncol <- 1
                    #le$ncol <- 2 
                    le$cex <- lecex
                    #le$cex <- 0.85
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
                    if (T) le <- reorder_legend(le)
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
                
                if (F) {
                    message("\nadd special stuff to datas vs months")
                    legend("bottomleft", 
                           #"topright",
                           "f", col="black", lty=NA, pch=NA, lwd=NA, bty="n", 
                           x.intersp=-1.8, y.intersp=0.5, cex=1.25)
                }

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
                        le$cex <- lecex
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
                        if (T) le <- reorder_legend(le)
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
                write(plotname, file=lastfiles_plot_fname, append=T)
                if (p$plot_type == "pdf") {
                    if (T) {
                        message("run `", p$pdf_embed_fun, "()` ...")
                        if (p$pdf_embed_fun == "grDevices::embedFonts") {
                            grDevices::embedFonts(plotname, outfile=plotname)
                        } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                            extrafont::embed_fonts(plotname, outfile=plotname)
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
                
                if (center_ts || scale_ts) {
                    if (center_ts) {
                        message("\n`center_ts` = T --> center ts before plot ...")
                    } else if (scale_ts) {
                        message("\n`scale_ts` = T --> scale ts before plot ...")
                    }
                    for (i in seq_along(zan)) {
                        if (center_ts) {
                            zan[[i]] <- base::scale(zan[[i]], scale=F)
                        } else if (scale_ts) {
                            zan[[i]] <- base::scale(zan[[i]])
                        }
                    }
                } # if center_ts or scale_ts
                
                # ylims for plot vs years
                message("\n", mode_p, " versus years min / mean / max ", varname, " zan:")
                for (i in seq_along(zan)) {
                    message(names_short_pan[i], ": ", min(zan[[i]], na.rm=T), " / ",
                            mean(zan[[i]], na.rm=T), " / ", max(zan[[i]], na.rm=T))
                }
                ylim_an <- range(zan, na.rm=T)
                message("\nylim_an=", appendLF=F)
                dput(ylim_an)
                ylim_an[is.infinite(ylim_an)] <- 0

                ## add annual data to left yaxis_an (e.g. obs)
                message("\nprepare additional left yaxis data (e.g. obs)")
                data_left_an <- list() # default: dont add additional data to left yaxis
                
                # add to data_left_an
                if (any(varname == c("co2_flx_ocean", "fgco2"))) {
                    if (exists("reccap2") && length(unique(areas) == 1) && names(reccap2)[1] == areas[1]) {
                        message("add reccap2 fgco2 data to ylim_an ...")
                        data_left_an[[length(data_left_an)+1]] <- list(x=reccap2[[areas[1]]]$dims$years, 
                                                                       y=reccap2[[areas[1]]]$data$fgco2_an_mean$vals,
                                                                       y_lower=reccap2[[areas[1]]]$data$fgco2_an_min$vals,
                                                                       y_upper=reccap2[[areas[1]]]$data$fgco2_an_max$vals,
                                                                       col=reccap2[[areas[1]]]$data$fgco2_an_mean$col,
                                                                       col_rgb=reccap2[[areas[1]]]$data$fgco2_an_min$col,
                                                                       text=reccap2[[areas[1]]]$data$fgco2_an_mean$label)
                    }
                    if (exists("gregor_and_fay_2021_ts_an") && length(unique(areas) == 1) && names(gregor_and_fay_2021_ts_an)[1] == areas[1]) {
                        message("add gregor_and_fay_2021_ts_an data to ylim_an ...")
                        data_left_an[[length(data_left_an)+1]] <- list(x=gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_mean$dims$years, 
                                                                       y=gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_mean$data$vals,
                                                                       #y_lower=gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_min$data$vals,
                                                                       #y_upper=gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_max$data$vals,
                                                                       y_lower=gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_mean$data$vals-gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_sd$data$vals,
                                                                       y_upper=gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_mean$data$vals+gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_sd$data$vals,
                                                                       col=gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_mean$data$col,
                                                                       col_rgb=col2rgba(gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_mean$data$col, 0.3),
                                                                       text=gregor_and_fay_2021_ts_an[[areas[1]]]$fgco2_ens_mean$data$label)
                    }
                    if (exists("chau_etal_2020_ts_an") && length(unique(areas) == 1) && names(chau_etal_2020_ts_an)[1] == areas[1]) {
                        message("add chau_etal_2020_ts_an data to ylim_an ...")
                        data_left_an[[length(data_left_an)+1]] <- list(x=chau_etal_2020_ts_an[[areas[1]]]$fgco2$dims$years, 
                                                                       y=chau_etal_2020_ts_an[[areas[1]]]$fgco2$data$vals,
                                                                       y_lower=chau_etal_2020_ts_an[[areas[1]]]$fgco2$data$vals-chau_etal_2020_ts_an[[areas[1]]]$fgco2_uncertainty$data$vals,
                                                                       y_upper=chau_etal_2020_ts_an[[areas[1]]]$fgco2$data$vals+chau_etal_2020_ts_an[[areas[1]]]$fgco2_uncertainty$data$vals,
                                                                       col=chau_etal_2020_ts_an[[areas[1]]]$fgco2$data$col,
                                                                       col_rgb=col2rgba(chau_etal_2020_ts_an[[areas[1]]]$fgco2$data$col, 0.3),
                                                                       text=chau_etal_2020_ts_an[[areas[1]]]$fgco2$data$label)
                    }
                } # if add data to data_left_an

                # check data_left_an
                if (length(data_left_an) > 0) {
                    for (i in seq_along(data_left_an)) {
                        if (is.null(data_left_an[[i]]$type)) data_left_an[[i]]$type <- "l" # default: line
                        if (is.null(data_left_an[[i]]$col)) data_left_an[[i]]$col <- i
                        if (is.null(data_left_an[[i]]$col_rgb)) col2rgba(data_left_an[[i]]$col, alpha_rgb)
                        if (is.null(data_left_an[[i]]$lty)) data_left_an[[i]]$lty <- 1
                        if (is.null(data_left_an[[i]]$lwd)) data_left_an[[i]]$lwd <- 1
                        if (is.null(data_left_an[[i]]$pch)) data_left_an[[i]]$pch <- NA
                        if (is.null(data_left_an[[i]]$pt.lwd)) data_left_an[[i]]$pt.lwd <- 1
                        if (is.null(data_left_an[[i]]$pt.bg)) data_left_an[[i]]$pt.bg <- NA
                        if (is.null(data_left_an[[i]]$pt.cex)) data_left_an[[i]]$pt.cex <- lecex
                        if (!is.null(data_left_an[[i]]$y_lower) || !is.null(data_left_an[[i]]$y_upper)) { # uncertainty
                            data_left_an[[i]]$pch <- 22 
                            data_left_an[[i]]$pt.lwd <- 0
                            data_left_an[[i]]$pt.bg <- data_left_an[[i]]$col_rgb
                            data_left_an[[i]]$pt.cex <- 2
                        }
                        if (is.null(data_left_an[[i]]$text)) data_left_an[[i]]$text <- "set text"
                        if (is.null(data_left_an[[i]]$legend.pos)) data_left_an[[i]]$legend.pos <- NA
                    } # for i in data_left_an
                } # if length(data_left_an) > 0

                # scale data_left_an
                if (length(data_left_an) > 0) {
                    if (center_ts || scale_ts) {
                        if (center_ts) {
                            message("\n`center_ts` = T --> center data_left_an before plot ...")
                        } else if (scale_ts) {
                            message("\n`scale_ts` = T --> scale data_left_an before plot ...")
                        }
                        for (i in seq_along(data_left_an)) {
                            if (center_ts) {
                                data_left_an[[i]]$y <- base::scale(data_left_an[[i]]$y, scale=F)
                                if (!is.null(data_left_an[[i]]$y_lower)) {
                                    data_left_an[[i]]$y_lower <- base::scale(data_left_an[[i]]$y_lower, scale=F)
                                }
                                if (!is.null(data_left_an[[i]]$y_upper)) {
                                    data_left_an[[i]]$y_upper <- base::scale(data_left_an[[i]]$y_upper, scale=F)
                                }
                            } else if (scale_ts) {
                                data_left_an[[i]]$y <- base::scale(data_left_an[[i]]$y)
                                if (!is.null(data_left_an[[i]]$y_lower)) {
                                    data_left_an[[i]]$y_lower <- base::scale(data_left_an[[i]]$y_lower)
                                }
                                if (!is.null(data_left_an[[i]]$y_upper)) {
                                    data_left_an[[i]]$y_upper <- base::scale(data_left_an[[i]]$y_upper)
                                }
                            }
                        }
                    }
                } # if length(data_left_an) > 0

                # update ylim according to additional data_left_an (e.g. obs)
                if (length(data_left_an) > 0) {
                    ylim_left_an <- range(lapply(data_left_an, "[[", "y"), na.rm=T)
                    message("\nupdate left yaxis ylim_an = ", paste(ylim_an, collapse=", "), 
                            " with ylim_left_an = ", paste(ylim_left_an, collapse=", "), " ...") 
                    ylim_an <- range(ylim_an, ylim_left_an)
                    ylim_left_an_lower <- lapply(data_left_an, "[[", "y_lower")
                    if (!all(sapply(ylim_left_an_lower, is.null))) {
                        ylim_left_an_lower <- range(ylim_left_an_lower, na.rm=T)
                        message("update left yaxis ylim_an = ", paste(ylim_an, collapse=", "), 
                                " with ylim_left_an_lower = ", paste(ylim_left_an_lower, collapse=", "), " ...") 
                        ylim_an <- range(ylim_an, ylim_left_an_lower)
                    }
                    ylim_left_an_upper <- lapply(data_left_an, "[[", "y_upper")
                    if (!all(sapply(ylim_left_an_lower, is.null))) {
                        ylim_left_an_upper <- range(ylim_left_an_upper, na.rm=T)
                        message("update left yaxis ylim_an = ", paste(ylim_an, collapse=", "), 
                                " with ylim_left_an_upper = ", paste(ylim_left_an_lower, collapse=", "), " ...") 
                        ylim_an <- range(ylim_an, ylim_left_an_upper)
                    }
                } # if (length(data_left_an) > 0)
            
                # add obs to ylim
                if (T && exists("noaa_ghcdn")) {
                    if (any(varname == c("temp2", "tsurf", "aprt"))) {
                        stop("update for data_left_an")
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
                yat_an <- pretty(ylim_an, n=8)
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
                        message("use automatic `data_right_an` yaxis limits ...")
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
                        message("use automatic_data_an` right yaxis labels ...")
                        yat_right_an <- pretty(ylim_right_an, n=8)
                    }
                    ylab_right_an <- format(yat_right_an, trim=T)
                } # if add_data_right_yaxis_ts_an finished prepare right axis data

                # plotname
                if (exists("plotprefix")) {
                    plotprefixp <- plotprefix
                } else { # default
                    plotprefixp <- paste(unique(names_short_p), collapse="_vs_")
                    if (F && plot_groups[plot_groupi] == "samedims") {
                        plotprefixp <- paste0(plotprefixp, "_", paste(unique(varnames_in_p), collapse="_vs_"))
                    }
                    plotprefixp <- paste0(plotprefixp,
                                          "_", paste(unique(seasonsp_p), collapse="_vs_"), 
                                          "_", paste(unique(froms_plot_p), collapse="_vs_"), 
                                          "_to_", paste(unique(tos_plot_p), collapse="_vs_"), 
                                          "_", paste(unique(areas_p), collapse="_vs_"), 
                                          collapse="_vs_")
                }
                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   varname, "_",
                                   plotprefixp, 
                                   "_", plot_groups[plot_groupi], # samevars or samedims
                                   data_right$suffix, data_upper$suffix, ts_highlight_seasons$suffix,
                                   plotname_suffix,
                                   "_annual.", p$plot_type)
                if (nchar(plotname) > nchar_max_foutname) {
                    stop("plotname too long. define `plotprefix` in plot namelist")
                }
                dir.create(dirname(plotname), recursive=T, showWarnings=F)
                
                # get plot sizes
                message("\nopen plot ", plotname, " ...")
                pp <- plot_sizes(width_in=p$ts_width_in, asp=p$ts_asp, verbose=T)
                if (p$plot_type == "png") {
                    png(plotname, width=pp$png_width_px, height=pp$png_height_px,
                        pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
                } else if (p$plot_type == "pdf") {
                    pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
                        family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
                }

                # set plot margins
                mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                mar[4] <- 1 # decrease right margin
                if (!add_title) mar[3] <- 1 # decrease upper margin
                if (tlabsrt == 0) mar[1] <- mar[1]/1.5  # decrease lower margin
                if (add_data_right_yaxis_ts_an) mar[4] <- mar[2] # same as left  
                
                # open plot
                par(mar=mar)
                plot(dan$year[[1]], zan[[1]], t="n",
                     xlim=anlim, ylim=ylim_an, 
                     xaxt="n", yaxt="n",
                     xlab=NA, ylab=NA)
                axis(1, at=anat, labels=anlab, cex.axis=tlabcex)
                axis(2, at=yat_an, labels=ylab_an, las=2)
            
                # add time label on x-axis
                mtext(side=1, tunit, line=2, cex=1)

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
                label_line <- 3.4
                label_cex <- 0.9
                message("\nput datasan label in `label_line` = ", label_line, 
                        " distance with `label_cex` = ", label_cex, " ...")
                mtext(side=2, data_info$label, line=label_line, cex=label_cex)

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

                # add linear regression trend if wanted
                lm_labels_an <- names_legend_pan
                if (any(add_linear_trend)) {
                    message("\ncalc and add linear trend against years with `stats::lm()` ...")
                    for (i in seq_along(z)) {
                        if (add_linear_trend[i]) {
                            lm_nyear <- length(add_linear_trend_froms[i]:add_linear_trend_tos[i]) # not accurate
                            message("\nsetting ", i, " ", names_short_p[i], " from ", 
                                    add_linear_trend_froms[i], " to ", add_linear_trend_tos[i],
                                    " (", lm_nyear, " nyears)")
                            inds <- which(dan$year[[i]] >= add_linear_trend_froms[i] &
                                          dan$year[[i]] <= add_linear_trend_tos[i])
                            if (length(inds) == 0) {
                                warning("no years found within those years -> use complete time series instead")
                                inds <- seq_along(zan[[i]])
                            }
                            lm <- stats::lm(zan[[i]][inds] ~ dan$year[[i]][inds])
                            lm_summary <- summary(lm)
                            print(lm_summary)
                            lm_trend_tot <- lm$fitted.values[length(lm$fitted.values)] - lm$fitted.values[1]
                            lm_from_to <- dan$year[[i]][range(inds)]
                            lm_dt_tot_a <- max(dan$year[[i]][inds]) - min(dan$year[[i]][inds]) + 1
                            lm_dt_tot_day <- lm_dt_tot_a*365.25
                            lm_trend_a <- lm_trend_tot/lm_dt_tot_a
                            lm_trend_day <- lm_trend_tot/lm_dt_tot_day
                            if (lm_dt_tot_a >= 1) { # show trend per year
                                lm_from_to_pretty <- paste(lm_from_to, collapse="-")
                                lm_dt_tot_pretty <- lm_dt_tot_a
                                attributes(lm_dt_tot_pretty)$units <- "a"
                                if (lm_nyear >= 10) { # show trend per decade
                                    if (lm_nyear >= 100) { # show trend per century
                                        if (lm_nyear >= 1000) { # show trend per millenium
                                            lm_trend_pretty <- lm_trend_tot * 1000 / lm_dt_tot_a
                                            lm_dt_pretty <- 1000; attributes(lm_dt_pretty)$units <- "a" # --> /1000a
                                        } else { # < 1000 a
                                            lm_trend_pretty <- lm_trend_tot * 100 / lm_dt_tot_a
                                            lm_dt_pretty <- 100; attributes(lm_dt_pretty)$units <- "a" # --> /100a
                                        }
                                    } else { # < 100 a
                                        lm_trend_pretty <- lm_trend_tot * 10 / lm_dt_tot_a
                                        lm_dt_pretty <- 10; attributes(lm_dt_pretty)$units <- "a" # --> /10a
                                    }
                                } else { # < 10 a
                                    lm_trend_pretty <- lm_trend_tot * 1 / lm_dt_tot_a
                                    lm_dt_pretty <- ""; attributes(lm_dt_pretty)$units <- "a" # --> /a
                                }
                            } else { # use trend per day
                                stop("this should not happen for annual data")
                            }
                            # calc time when trend line reaches zero (if possible) 
                            lm_zero_year <- NA
                            if (lm$coefficients[2] != 0) { # trend line has some slope
                                if (lm$fitted.values[length(lm$fitted.values)] >= 0 && lm$coefficients[2] < 0 || # positive fit values with negative slope
                                    lm$fitted.values[length(lm$fitted.values)] < 0 && lm$coefficients[2] > 0) { # negative fit values with positive slope
                                    lm_zero_year <- abs(lm$coefficients[1]/lm$coefficients[2]) # intercept/slope = [x]/([x/year]) = [year]
                                    lm_zero_dt_a <- lm_zero_year - max(dan$year[[i]][inds])
                                    lm_zero_dt_day <- lm_zero_dt_a*365.25
                                }
                            }
                            # add trend results to legend label
                            if (F) { # add trend only to label
                                lm_labels_an[i] <- eval(substitute(expression(paste(lab, " (tr", ""[lm_from_to_pretty], "=", sign, trend, " ", unit, "/", lm_dt_unit, ")")),
                                                                   list(lab=names_legend_pan[i],
                                                                        lm_from_to_pretty=lm_from_to_pretty,
                                                                        sign=ifelse(lm_summary$coefficients[2,"Estimate"] > 0, "+", "")
                                                                        , trend=round(lm_trend_pretty, 4), 
                                                                        #, trend=round(lm_trend_tot, 4), 
                                                                        unit=data_info$units
                                                                        , lm_dt_unit=paste0(lm_dt_pretty, attributes(lm_dt_pretty)$units)
                                                                        #, lm_dt_unit=paste0(lm_dt_tot_pretty, attributes(lm_dt_tot_pretty)$units)
                                                                        )
                                                                   )
                                                       )
                            } else if (T) { # add mean and trend to label
                                if (!is.na(lm_zero_year)) { # add time when trend crosses zero
                                    lm_labels_an[i] <- eval(substitute(expression(paste(lab, " (", mu[lm_from_to_pretty], "=", muval, 
                                                                                        "; tr=", sign, trend, " ", unit, "/", lm_dt_unit, " " 
                                                                                        %->% " 0 in ", lm_zero_dt, ")")),
                                                                       list(lab=names_legend_pan[i],
                                                                            lm_from_to_pretty=lm_from_to_pretty,
                                                                            muval=round(mean(zan[[i]][inds], na.rm=T), 2),
                                                                            sign=ifelse(lm_summary$coefficients[2,"Estimate"] > 0, "+", "")
                                                                            , trend=round(lm_trend_pretty, 4), 
                                                                            #, trend=round(lm_trend_tot, 4),
                                                                            unit=data_info$units
                                                                            , lm_dt_unit=paste0(lm_dt_pretty, attributes(lm_dt_pretty)$units),
                                                                            #, lm_dt_unit=paste0(lm_dt_tot_pretty, attributes(lm_dt_tot_pretty)$units),
                                                                            lm_zero_dt=paste0(round(lm_zero_dt_a, 2), "a")
                                                                            )
                                                                       )
                                                           )
                                } else {
                                    lm_labels_an[i] <- eval(substitute(expression(paste(lab, " (", mu[lm_from_to_pretty], "=", muval, 
                                                                                        "; tr=", sign, trend, " ", unit, "/", lm_dt_unit, ")")),
                                                                       list(lab=names_legend_pan[i],
                                                                            lm_from_to_pretty=lm_from_to_pretty,
                                                                            muval=round(mean(zan[[i]][inds], na.rm=T), 2),
                                                                            sign=ifelse(lm_summary$coefficients[2,"Estimate"] > 0, "+", "")
                                                                            , trend=round(lm_trend_pretty, 4), 
                                                                            #, trend=round(lm_trend_tot, 4),
                                                                            unit=data_info$units
                                                                            , lm_dt_unit=paste0(lm_dt_pretty, attributes(lm_dt_pretty)$units)
                                                                            #, lm_dt_unit=paste0(lm_dt_tot_pretty, attributes(lm_dt_tot_pretty)$units)
                                                                            )
                                                                       )
                                                           )
                                }
                            } # if add trend to label
                            message("--> total trend ", round(lm_trend_tot, 4), " ", data_info$units, " in ",
                                    lm_dt_tot_a, " years ~ ", lm_dt_tot_day, " days from ",
                                    min(lm_from_to), " to ", max(lm_from_to), "\n",
                                    "--> trend per year = ", lm_trend_a, " ", data_info$units, "\n", 
                                    "--> trend per day = ", lm_trend_day, " ", data_info$units, "\n", 
                                    "--> last trend value = ", lm$fitted.values[length(lm$fitted.values)], " ", data_info$units)
                            if (!is.na(lm_zero_year)) {        
                                message("--> linear trendline crosses zero ", data_info$units, " at year ", lm_zero_year, ", i.e. in ", 
                                        round(lm_zero_dt_a, 3), " years ~ ", round(lm_zero_dt_day, 3), " days")
                            }
                            # plot regression line within data limits only
                            if (F) {
                                message("draw linear regression line within regression limits only ...")
                                lines(d$time[[i]][inds], lm$fitted.values, 
                                      col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i]+1)
                            # or plot line through whole plot with regression coefficients
                            } else if (T) {
                                message("draw linear regression line through whole plot ...")
                                abline(a=lm$coefficients[1], b=lm$coefficients[2],
                                       col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i]+1)
                            }
                        }
                    }
                } # add_linear_trend

                # add obs 
                if (length(data_left_an) > 0) {
                    message("\nadd data_left to datasan vs years ...")

                    # add uncertainties if given
                    for (i in seq_along(data_left_an)) {
                        if (!is.null(data_left_an[[i]]$y_lower) || !is.null(data_left_an[[i]]$y_upper)) {
                            message("add data_left_an[[", i, "]]$text = ", data_left_an[[i]]$text, " uncertainties to plot ...")
                            if (!is.null(data_left_an[[i]]$y_lower) && !is.null(data_left_an[[i]]$y_upper)) {
                                polygon(c(data_left_an[[i]]$x, rev(data_left_an[[i]]$x)),
                                        c(data_left_an[[i]]$y_lower, rev(data_left_an[[i]]$y_upper)),
                                        col=data_left_an[[i]]$col_rgb, border=NA)
                            } else if (!is.null(data_left_an[[i]]$y_lower) && is.null(data_left_an[[i]]$y_upper)) {
                                stop("implement")
                            } else if (is.null(data_left_an[[i]]$y_lower) && !is.null(data_left_an[[i]]$y_upper)) {
                                stop("implement")
                            }
                        }
                    } # for i in data_left_an

                    # add data points/lines
                    for (i in seq_along(data_left_an)) {
                        message("add data_left_an[[", i, "]]$text = ", data_left_an[[i]]$text, " data to plot ...")
                        points(data_left_an[[i]]$x, data_left_an[[i]]$y, 
                               type=data_left_an[[i]]$type, col=data_left_an[[i]]$col, 
                               pch=data_left_an[[i]]$pch, cex=data_left_an[[i]]$cex, 
                               lty=data_left_an[[i]]$lty, lwd=data_left_an[[i]]$lwd)
                    } # for i in data_left_an

                } # if length(data_left_an) > 0
                # finished adding obs
                
                # add legend if wanted
                if (T && add_legend) {
                    message("\nadd default stuff to datasan vs years legend ...")
                    le <- list()
                    le$pos <- "topleft" 
                    #le$pos <- "top"
                    #le$pos <- "bottom"
                    #le$pos <- "bottomleft" 
                    #le$pos <- "bottomright" 
                    #le$ncol <- nsettings/2
                    le$ncol <- 1
                    #le$ncol <- 2 
                    names_legend_pan_w_lm <- names_legend_pan
                    if (typeof(lm_labels_an) == "expression") {
                        names_legend_pan_w_lm <- lm_labels_an # use lm result
                        le$cex <- 0.85
                    }
                    inds <- which(!is.na(names_legend_pan)) # throw out user provided NA
                    le$text <- names_legend_pan_w_lm[inds]
                    le$cex <- lecex
                    #le$cex <- 0.66
                    #le$cex <- 0.85
                    le$col <- cols_pan[inds]
                    le$lty <- ltys_pan[inds]
                    le$lwd <- lwds_pan[inds]
                    le$pch <- pchs_pan[inds]
                    for (i in seq_along(inds)) {
                        if (types_pan[inds[i]] == "p") {
                            le$lty[inds[i]] <- NA
                        } else if (types_pan[inds[i]] == "l") {
                            le$pch[inds[i]] <- NA
                        }
                    }
                    le$pt.lwd <- le$pt.bg <- le$pt.cex <- rep(NA, t=length(zan)) # uncertainty default
                    
                    # add stuff to datasn legend here
                    if (length(data_left_an) > 0) {
                        # add data_left_an to model legend
                        message("add ", length(data_left_an), " data_left_an entries to legend ...")
                        le$text <- c(le$text, sapply(data_left_an, "[[", "text"))
                        le$col <- c(le$col, sapply(data_left_an, "[[", "col"))
                        le$lty <- c(le$lty, sapply(data_left_an, "[[", "lty"))
                        le$lwd <- c(le$lwd, sapply(data_left_an, "[[", "lwd"))
                        le$pch <- c(le$pch, sapply(data_left_an, "[[", "pch"))
                        le$pt.lwd <- c(le$pt.lwd, sapply(data_left_an, "[[", "pt.lwd"))
                        le$pt.bg <- c(le$pt.bg, sapply(data_left_an, "[[", "pt.bg"))
                        le$pt.cex <- c(le$pt.cex, sapply(data_left_an, "[[", "pt.cex"))

                        if (F) { # special: replace model legend with data_left_an legend
                            message("special: replace model legend by ", length(data_left_an), " data_left_an entries ...")
                            le$text <- sapply(data_left_an, "[[", "text")
                            le$col <- sapply(data_left_an, "[[", "col")
                            le$lty <- sapply(data_left_an, "[[", "lty")
                            le$lwd <- sapply(data_left_an, "[[", "lwd")
                            le$pch <- sapply(data_left_an, "[[", "pch")
                        }
                        
                        if (T && any(!is.na(sapply(data_left_an, "[[", "legend.pos")))) {
                            message("special legend placement")
                            legend.pos <- sapply(data_left_an, "[[", "legend.pos")
                            legend.pos <- legend.pos[which(!is.na(legend.pos))]
                            if (length(legend.pos) == 1) {
                                le$pos <- legend.pos
                            } else {
                                stop("found ", length(legend.pos), " legend.pos in data_left_an. dont know how to continue")
                            }
                        }
                    } # if (length(data_left_an) > 0) {
                    
                    # reorder reading direction from R's default top->bottom to left->right
                    if (T) le <- reorder_legend(le)
                    cat(capture.output(str(le, vec.len=length(le$text))), sep="\n")
                    if (length(le$pos) == 1) {
                        legend(le$pos, legend=le$text, 
                               lty=le$lty, lwd=le$lwd, pch=le$pch, col=le$col, 
                               pt.lwd=le$pt.lwd, pt.bg=le$pt.bg, pt.cex=le$pt.cex,
                               cex=le$cex, ncol=le$ncol, x.intersp=0.2, bty="n")
                    } else if (length(le$pos) == 2) {
                        legend(x=le$pos[1], y=le$pos[2], legend=le$text, 
                               lty=le$lty, lwd=le$lwd, pch=le$pch, col=le$col, 
                               pt.lwd=le$pt.lwd, pt.bg=le$pt.bg, pt.cex=le$pt.cex,
                               cex=le$cex, ncol=le$ncol, x.intersp=0.2, bty="n")
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
                        le$cex <- lecex
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
                        if (T) le <- reorder_legend(le)
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
                write(plotname, file=lastfiles_plot_fname, append=T)
                if (p$plot_type == "pdf") {
                    if (T) {
                        message("run `", p$pdf_embed_fun, "()` ...")
                        if (p$pdf_embed_fun == "grDevices::embedFonts") {
                            grDevices::embedFonts(plotname, outfile=plotname)
                        } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                            extrafont::embed_fonts(plotname, outfile=plotname)
                        }
                    } else {
                        message("todo: sometimes pdf font embedding blurrs colors why?")
                    }
                }

                # redfit of annual data
                if (plot_redfit) {
                    message("\n`plot_redfit`=T --> calc and plot `dplR::redfit()` of annual data ...")
                    library(dplR)
                    redf <- vector("list", l=length(zan))
                    for (i in seq_along(zan)) {
                        if (F) { # use moving average data
                            warning("using ma data for dplR::redfit() yields strange very low ci")
                            if (!exists("zma")) {
                                stop("not yet")
                            } # if !exists("zma")
                            x <- zma[[i]]
                        } else if (T) { # use annual data
                            x <- zan[[i]]
                        }
                        if (F) { # test example from redfit
                            data(cana157)
                            dan$year[[i]] <- time(cana157) 
                            x <- cana157[,1]$TTRSTD
                            attr(x, "npy") <- 1 # annual data
                        }
                        message("setting ", i, "/", length(zan), " ", names(zan)[i], ": time series of len ", 
                                length(x), " with npy = ", attr(x, "npy"), " ...")
                        #tmp <- base::tryCatch(redfit(rnorm(100)), error=function(e) e, warning=function(w) w)
                        tmp <- base::tryCatch(redfit(x), error=function(e) e, warning=function(w) w)
                        if (length(tmp) == 2 && # no success
                            !grepl("NA values removed", tmp$message) &&
                            !grepl("redfitGetrho returned rho", tmp$message) &&
                            !grepl("rho estimation: <= 0", tmp$message)) { # add further warnings which are ok here if necessary
                            message("redfit error: ", tmp$message, " --> skip")
                            redf[[i]] <- NA
                        } else { # success
                            # acf of time series
                            rhopre <- NULL # default
                            if (length(tmp) == 2 && grepl("rho estimation: <= 0", tmp$message)) {
                                # calc autocorrelation of x
                                message("redfit() yields warning ", tmp$message, " --> calc acf of x with stats::acf(x) ...")
                                acf <- stats::acf(x, plot=F)
                                if (T) {
                                    rhopre <- acf$acf[2] # acf1
                                } else if (F) {
                                    rhopre <- mean(acf$acf)
                                }
                            }
                            redf[[i]] <- redfit(x, rhopre=rhopre)
                            redf[[i]]$period <- 1/redf[[i]]$freq/attr(x, "npy")
                            message("ok (acf = ", round(redf[[i]]$rho, 3), "). calc period = 1/freq/npy = 1/", 
                                    mean(redf[[i]]$freq), "/", attr(x, "npy"), " = ", 
                                    mean(redf[[i]]$period[2:length(redf[[i]]$period)]), # exclude period[1] = 1/freq[1] = 1/0 = Inf
                                    " years (mean) ...")
                            attr(redf[[i]]$period, "units") <- "year"
                            if (plot_redfit_pcnt) {
                                fac <- 1/max(redf[[i]]$gxxc)*100
                                redf[[i]]$gxxc <- fac*redf[[i]]$gxxc
                                redf[[i]]$ci80 <- fac*redf[[i]]$ci80
                                redf[[i]]$ci90 <- fac*redf[[i]]$ci90
                                redf[[i]]$ci95 <- fac*redf[[i]]$ci95
                                redf[[i]]$ci99 <- fac*redf[[i]]$ci99
                            }
                        }
                    } # for i zma

                    # plot red noise corrected spectrum of the data 
                    if (any(is.list(redf))) { # any success

                        xlim <- ylim <- inds <- vector("list", l=length(redf))
                        for (i in seq_along(redf)) {
                            # consider only from max(min(period),<x> years) to <fac>*nyears of time series
                            if (is.list(redf[[i]])) {
                                fromind <- which.min(abs(redf[[i]]$period - 0.5*length(dan$year[[i]])))[1]
                                #fromind <- which.min(abs(redf[[i]]$period - 0.75*length(dan$year[[i]])))[1]
                                #fromind <- which.min(abs(redf[[i]]$period - 1*length(dan$year[[i]])))[1]
                                #toind <- which.min(abs(redf[[i]]$period - 0.5))[1]
                                toind <- which.min(abs(redf[[i]]$period - 1))[1]
                                inds[[i]] <- fromind:toind
                                xlim[[i]] <- range(redf[[i]]$period[inds[[i]]])
                                ylim[[i]] <- range(redf[[i]]$gxxc[inds[[i]]])
                            } else {
                                inds[[i]] <- xlim[[i]] <- ylim[[i]] <- NA
                            }
                        }
                        if (T) { # take minimum xlim of all settings
                            message("special: take minimum period length for redfit plot xaxis")
                            xlim <- c(min(sapply(xlim, "[", 1), na.rm=T), min(sapply(xlim, "[", 2), na.rm=T))
                        } else { # default
                            xlim <- range(xlim, na.rmT=)
                        }
                        ylim <- range(ylim, na.rm=T)
                        xat <- c(2, 5, seq(10, round(xlim[2]/10)*10, b=5))
                        #xat <- c(2, 5, seq(10, round(xlim[2]/10)*10, b=10))
                        xticks <- vector("list", l=length(xat)-1)
                        for (i in seq_along(xticks)) {
                            if (xat[i+1] - xat[i] > 1) {
                                xticks[[i]] <- (xat[i]+1):(xat[i+1]-1)
                            }
                        }
                        xticks <- unlist(xticks)

                        # plotname
                        if (exists("plotprefix")) {
                            plotprefixp <- plotprefix
                        } else { # default
                            plotprefixp <- paste(unique(names_short_p), collapse="_vs_")
                            if (F && plot_groups[plot_groupi] == "samedims") { # use only varnames_out_samedims below
                                plotprefixp <- paste0(plotprefixp, "_", paste(unique(varnames_in_p), collapse="_vs_"))
                            }
                            plotprefixp <- paste0(plotprefixp,
                                                  "_", paste(unique(seasonsp_p), collapse="_vs_"), 
                                                  "_", paste(unique(froms_plot_p), collapse="_vs_"), 
                                                  "_to_", paste(unique(tos_plot_p), collapse="_vs_"), 
                                                  "_", paste(unique(areas_p), collapse="_vs_"), 
                                                  collapse="_vs_")
                        }
                        plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                           varname, "_", # individual samevars or common samedims varname
                                           plotprefixp, 
                                           "_", plot_groups[plot_groupi], # samevars or samedims
                                           data_right$suffix, data_upper$suffix, ts_highlight_seasons$suffix,
                                           plotname_suffix,
                                           "_annual_redfit.", p$plot_type)
                        if (nchar(plotname) > nchar_max_foutname) {
                            stop("plotname too long. define `plotprefix` in plot namelist")
                        }
                        dir.create(dirname(plotname), recursive=T, showWarnings=F)
                        
                        # get plot sizes
                        message("open plot ", plotname, " ...")
                        source("~/scripts/r/functions/myfunctions.r") 
                        pp <- plot_sizes(width_in=p$map_width_in, asp=p$map_asp, verbose=T)
                        if (p$plot_type == "png") {
                            png(plotname, width=pp$png_width_px, height=pp$png_height_px,
                                pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
                        } else if (p$plot_type == "pdf") {
                            pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
                                family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
                        }

                        # set plot margins
                        mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                        mar[4] <- 1 # decrease right margin

                        # open plot
                        message("mar = ", paste(mar, collapse=", "))
                        par(mar=mar)
                        plot(0, t="n", log="x", 
                             xlim=xlim, ylim=ylim, 
                             xaxt="n", yaxt="n",
                             xlab=NA, ylab=NA)
                        axis(1, at=xat, labels=xat)
                        axis(1, at=xticks, labels=F, tck=-0.025)
                        axis(2, at=pretty(ylim, n=10), las=2)
                        mtext("Period [years]", side=1, line=2.5)
                        if (plot_redfit_pcnt) {
                            mtext("Spectrum [% of max]", side=2, line=4)
                        } else {
                            mtext("Spectrum", side=2, line=4)
                        }
                        cnt <- 0
                        for (i in seq_along(redf)) {
                            if (is.list(redf[[i]])) {
                                message("add refit of setting ", i, " with period from ", xlim[1], " to ", xlim[2], " years")
                                ci_y <- ci_labels <- c()
                                if (F) {
                                    lines(redf[[i]]$period[inds[[i]]], redf[[i]]$ci80[inds[[i]]], col=cols_rgb[i], lty=3)
                                    ci_y <- c(ci_y, redf[[i]]$ci80[max(inds[[i]])])
                                    ci_labels <- c(ci_labels, "80")
                                }
                                if (T) {
                                    lines(redf[[i]]$period[inds[[i]]], redf[[i]]$ci95[inds[[i]]], col=cols_rgb[i], lty=2)
                                    ci_y <- c(ci_y, redf[[i]]$ci95[max(inds[[i]])])
                                    ci_labels <- c(ci_labels, "95")
                                }
                                if (F) {
                                    lines(redf[[i]]$period[inds[[i]]], redf[[i]]$ci99[inds[[i]]], col=cols_rgb[i], lty=1)
                                    ci_y <- c(ci_y, redf[[i]]$ci99[max(inds[[i]])])
                                    ci_labels <- c(ci_labels, "99")
                                }
                                lines(redf[[i]]$period[inds[[i]]], redf[[i]]$gxxc[inds[[i]]], col=cols[i])
                                if (cnt == 0) { # indicate ci levels once
                                    text(mean(c(10^par("usr")[1], 2)), ci_y,
                                         labels=ci_labels, col=cols_rgb[i], cex=0.75)
                                    cnt <- cnt + 1
                                }
                            }
                        }
                        box()

                        # add legend to redfit
                        if (add_legend) {
                            message("\nadd default stuff to time vs redfit legend ...")
                            le <- list()
                            le$pos <- "topleft" 
                            le$ncol <- 1
                            le$cex <- lecex
                            le$text <- names_legend_p
                            le$col <- cols_p
                            le$lty <- ltys_p
                            le$lwd <- lwds_p
                            le$pch <- pchs_p
                            # reorder reading direction from R's default top->bottom to left->right
                            if (T) le <- reorder_legend(le)
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
                        
                        # save redfit plot
                        message("\nsave plot ", plotname, " ...")
                        dev.off()
                        write(plotname, file=lastfiles_plot_fname, append=T)
                        if (p$plot_type == "pdf") {
                            if (T) {
                                message("run `", p$pdf_embed_fun, "()` ...")
                                if (p$pdf_embed_fun == "grDevices::embedFonts") {
                                    grDevices::embedFonts(plotname, outfile=plotname)
                                } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                                    extrafont::embed_fonts(plotname, outfile=plotname)
                                }
                            } else {
                                message("todo: sometimes pdf font embedding blurrs colors why?")
                            }
                        }

                    } # if any !is.na(redf)
                } # if plot_redfit
            
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

            ## plot `datasltm` vs ltm_range
            if (ndims_ltm == 1 && all(dim_names_ltm == "ltm_range")) {
                
                # barplot of ltm of all settings
                message("\n", varname, " ", mode_p, " ltm plot vs ltm_range ...")
                
                # get barplot xat and xlim
                xat_ltm <- as.vector(barplot(sapply(zltm, "[[", 1), plot=F))
                xlim_ltm <- range(xat_ltm)
                message("xlim_ltm = ", appendLF=F)
                dput(xlim_ltm)

                ylim_ltm <- range(zltm, na.rm=T)
                # extend data range by 4 pcnt for barplot(), similarly as default yaxs="r" of plot()
                ylim_ltm <- ylim_ltm + c(-1, 1)*0.04*(max(ylim_ltm)-min(ylim_ltm))
                message("ylim_ltm = ", appendLF=F)
                dput(ylim_ltm)
                yat_ltm <- pretty(ylim_ltm, n=8)
                
                # plotname
                if (exists("plotprefix")) {
                    plotprefixp <- plotprefix
                } else { # default
                    plotprefixp <- paste(unique(names_short_p), collapse="_vs_")
                    if (F && plot_groups[plot_groupi] == "samedims") {
                        plotprefixp <- paste0(plotprefixp, "_", paste(unique(varnames_in_p), collapse="_vs_"))
                    }
                    plotprefixp <- paste0(plotprefixp,
                                          "_", paste(unique(seasonsp_p), collapse="_vs_"), 
                                          "_", paste(unique(froms_plot_p), collapse="_vs_"), 
                                          "_to_", paste(unique(tos_plot_p), collapse="_vs_"), 
                                          "_", paste(unique(areas_p), collapse="_vs_"), 
                                          collapse="_vs_")
                }
                plotname <- paste0(plotpath, "/", mode_p, "/", varname, "/",
                                   varname, "_",
                                   plotprefixp, 
                                   "_", plot_groups[plot_groupi], # samevars or samedims
                                   data_right$suffix, data_upper$suffix, ts_highlight_seasons$suffix,
                                   plotname_suffix,
                                   "_timmean.", p$plot_type)
                if (nchar(plotname) > nchar_max_foutname) {
                    stop("plotname too long. define `plotprefix` in plot namelist")
                }
                dir.create(dirname(plotname), recursive=T, showWarnings=F)
                
                # get plot sizes
                message("open plot ", plotname, " ...")
                pp <- plot_sizes(width_in=p$map_width_in, asp=p$map_asp, verbose=T)
                if (p$plot_type == "png") {
                    png(plotname, width=pp$png_width_px, height=pp$png_height_px,
                        pointsize=pp$png_pointsize, res=pp$png_ppi, family=p$png_family)
                } else if (p$plot_type == "pdf") {
                    pdf(plotname, width=pp$pdf_width_in, height=pp$pdf_height_in,
                        family=p$pdf_family, encoding=encoding, pointsize=pp$pdf_pointsize)
                }

                # set plot margins
                mar <- c(5.1, 6.1, 4.1, 4.1) + 0.1 # my default margins
                mar[4] <- 1 # decrease right margin
                if (!add_title) mar[3] <- 1 # decrease upper margin
                # increase lower margin for vertical setting names
                if (F) mar[1] <- max(nchar(names_legend_pltm))*0.55 
                
                # open plot
                par(mar=mar)
                if (F) {
                    plot(0, t="n",
                         xlim=xlim_ltm, ylim=ylim_ltm,
                         xaxt="n", yaxt="n",
                         xlab="", ylab="")
                } else if (T) {
                    barplot(sapply(zltm, "[[", 1),
                            ylim=ylim_ltm, xpd=F, 
                            col=cols_pltm, border=NA, 
                            #xaxt="n", yaxt="n",
                            axes=F,
                            xlab="", ylab="", 
                            names.arg=names_legend_pltm) # uses axis() to draw labels at column means; supressed if xaxt="n"
                }
                axis(1, at=xat_ltm, labels=F) # ticks only
                if (F) {
                    text(x=xat_ltm, 
                         #y=par("usr")[3], 
                         # y-pos of labels: `bottom minus 0.05*dy scaled by plot asp`; is this rly a good solution?
                         #y=par("usr")[3] - abs(diff(par("usr")[3:4])*0.05/p$map_asp),
                         y=par("usr")[3] - 0.4*abs(diff(yat_ltm)[1]),
                         labels=names_legend_pltm, 
                         adj=1, srt=90, xpd=T) # vertical labels
                }
                axis(2, at=yat_ltm, las=2)

                # add variable label on y-axis
                #label_line <- 2.5
                label_line <- 3
                #label_line <- 3.4
                #label_line <- 3.5
                #label_line <- 4
                #label_line <- 4.5
                #label_cex <- 1
                #label_cex <- 0.9
                label_cex <- 0.75
                message("\nput datasltm label in `label_line` = ", label_line, 
                        " distance with `label_cex` = ", label_cex, " ...")
                mtext(side=2, data_info$label, line=label_line, cex=label_cex)
                
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
                    abline(v=xat_ltm, col="gray", lwd=0.5)
                }
                if (add_ygrid) {
                    message("\nadd ygrid ...")
                    abline(h=yat_ltm, col="gray", lwd=0.5)
                }

                # add zero line
                if (add_zeroline) {
                    message("`add_zeroline` = T --> add zeroline") 
                    abline(h=0, col="black", lwd=0.5)
                }

                # add data as barplot
                if (F) {
                    barplot(sapply(zltm, "[[", 1), 
                            #xlim=range(seq_along(zltm)),
                            xaxt="n", yaxt="n", 
                            xlab="", ylab="", 
                            col=cols_pltm, border=NA, 
                            add=T)
                }

                # add value above/under bar
                if (T) {
                    message("\nadd value above/below bars ...")
                    y <- sapply(zltm, "[[", 1)
                    y <- y + ifelse(y < 0, -1, 1)*abs(diff(par("usr")[3:4])*0.025/p$map_asp)
                    text(xat_ltm, y, round(sapply(zltm, "[[", 1), 2), cex=0.5)
                }

                if (zname == "co2_flx_total") {
                    message("co2_flx_total special: add +-0.1 PgC yr-1 spinup thr")
                    abline(h=0.1*c(1, -1))
                }

                box()

                message("\nsave plot ", plotname, " ...")
                dev.off()
                write(plotname, file=lastfiles_plot_fname, append=T)
                if (p$plot_type == "pdf") {
                    if (T) {
                        message("run `", p$pdf_embed_fun, "()` ...")
                        if (p$pdf_embed_fun == "grDevices::embedFonts") {
                            grDevices::embedFonts(plotname, outfile=plotname)
                        } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                            extrafont::embed_fonts(plotname, outfile=plotname)
                        }
                    } else {
                        message("todo: sometimes pdf font embedding blurrs colors why?")
                    }
                }
                
            ## plot `datasltm` as lat vs depth (e.g. moc)
            } else if (ndims_ltm == 2 && all(dim_names_ltm == c("lat", "depth"))) {

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

                # plot
                image.plot.nxm(dltm$lat, dltm$depth, z=zltm, ip=ip, verbose=F,
                               xlab="Latitude [°]", ylab=ylab,
                               zlab=data_info$label, znames_labels=names_legend_pltm,
                               image_list=moc_topo)
            
                message("\nsave plot ", plotname, " ...")
                dev.off()
                write(plotname, file=lastfiles_plot_fname, append=T)
                if (p$plot_type == "pdf") {
                    if (T) {
                        message("run `", p$pdf_embed_fun, "()` ...")
                        if (p$pdf_embed_fun == "grDevices::embedFonts") {
                            grDevices::embedFonts(plotname, outfile=plotname)
                        } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                            extrafont::embed_fonts(plotname, outfile=plotname)
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

            if (exists(paste0(scatter_s1_vs_s1_varname, "_datas"))) { # set in plot namelist
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
                            if (add_1to1_line) {
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
                                le$cex <-lecex
                                le$cex <- 0.85
                                le$text <- names(season_cols) #names_legend[i]
                                le$col <- season_cols #"black"
                                le$lty <- NA
                                le$lwd <- NA
                                #le$pchs <- scatterpchs_vstime[i]
                                le$pch <- scatterpchs_vstime
                                if (T) le <- reorder_legend(le)
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
                            write(plotname, file=lastfiles_plot_fname, append=T)
                            if (p$plot_type == "pdf") {
                                if (T) {
                                    message("run `", p$pdf_embed_fun, "()` ...")
                                    if (p$pdf_embed_fun == "grDevices::embedFonts") {
                                        grDevices::embedFonts(plotname, outfile=plotname)
                                    } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                                        extrafont::embed_fonts(plotname, outfile=plotname)
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

            message("\n****************** `plot_scatter_v1_vs_v2`=T --> scatterplot varx vs vary *******************")
            message("\n`varnamex` = \"", varnamex, "\", `varnamey` = \"", varnamey, "\"")

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
                if (T && any(varname == c("temp2_vs_toa_imbalance", "tas_vs_toa_imbalance")) && 
                    any(grepl("piControl", names_short))) {
                    message("\nspecial: prepare equilibrium climate sensitivity (ECS): ",
                            "see https://github.com/ESMValGroup/ESMValTool/issues/1814")
                    inds <- seq_along(z)
                    if (length(which(grepl("piControl", names_short))) == 1) {
                        piind <- which(grepl("piControl", names_short))
                        inds <- inds[-piind]
                    } else {
                        stop("not defined")
                    }
                    if (F) { # subtract PI of year 1849 (last year before deck experiments start)
                        message("case 1: anomaly `delta data = data_experiment minus data_piControl(ntime)` ...")
                        for (i in inds) {
                            varx[[i]] <- varx[[i]] - varx[[piind]][length(varx[[piind]])]
                            varx_infos[[i]]$label <- "2m temperature change [K]"
                            vary[[i]] <- vary[[i]] - vary[[piind]][length(vary[[piind]])]
                            vary_infos[[i]]$label <- eval(substitute(expression(paste("TOA imbalance change [W m"^paste(-2), "]"))))
                        }
                        # last: pi itself
                        varx[[piind]] <- varx[[piind]] - varx[[piind]][length(varx[[piind]])]
                        varx_infos[[piind]]$label <- "2m temperature change [K]"
                        #vary[[piind]] <- vary[[piind]] - vary[[piind]][length(varx[[piind]])]
                        #vary_infos[[piind]]$label <- eval(substitute(expression(paste("TOA imbalance change [W m"^paste(-2), "]"))))
                    
                    } else if (F) { # subtract last 100-yr mean PI
                        message("case 2: anomaly `delta data = data_experiment minus mean(data_piControl)` ...")
                        for (i in inds) {
                            varx[[i]] <- varx[[i]] - mean(varx[[piind]])
                            varx_infos[[i]]$label <- "2m temperature change [K]"
                            vary[[i]] <- vary[[i]] - mean(vary[[piind]])
                            vary_infos[[i]]$label <- eval(substitute(expression(paste("TOA imbalance change [W m"^paste(-2), "]"))))
                        }
                        # last: pi itself
                        varx[[piind]] <- varx[[piind]] - mean(varx[[piind]])
                        varx_infos[[piind]]$label <- "2m temperature change [K]"
                        #vary[[piind]] <- vary[[piind]] - mean(vary[[piind]])
                        #vary_infos[[piind]]$label <- eval(substitute(expression(paste("TOA imbalance change [W m"^paste(-2), "]"))))
                    
                    } else if (F) { # subtract respective annual PI years 
                        message("case 3: anomaly `delta data(year_i) = data_experiment(year_i) minus data_piControl(year_i)` ...")
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
                            varx_infos[[i]]$label <- "2m temperature change [K]"
                            vary[[i]] <- vary[[i]] - vary[[piind]]
                            vary_infos[[i]]$label <- eval(substitute(expression(paste("TOA imbalance change [W m"^paste(-2), "]"))))
                        }
                        # last: pi itself
                        varx[[piind]] <- varx[[piind]] - varx[[piind]] # = all zero
                        varx_infos[[piind]]$label <- "2m temperature change [K]"
                        #vary[[piind]] <- vary[[piind]] - vary[[piind]] # = all zero
                        vary_infos[[piind]]$label <- eval(substitute(expression(paste("TOA imbalance change [W m"^paste(-2), "]"))))
                    
                    } else if (T) { # subtract linear fit of PI (same as in esmvaltool)
                        message("case 4: anomaly `delta data = data_experiment minus lm(data_piControl)` as in \n",
                                "ESMValTool: https://github.com/ESMValGroup/ESMValTool/issues/1814#issuecomment-691939774\n",
                                "--> who refer to Figure 9.42 of AR5 who refer to refer Andrews et al. 2012:\n",
                                "--> \"We calculate differences between the abrupt4xCO2 and piControl experiments by subtracting ",
                                "a linear fit of the corresponding control timeseries from the perturbation run, removing any ",
                                "model drift without adding control noise.\"")
                        # piControl fit
                        varx_lm <- lm(varx[[piind]] ~ dims[[piind]]$time)
                        vary_lm <- lm(vary[[piind]] ~ dims[[piind]]$time)
                        message("linear trend of ", names_short[piind], " xdata ", varnamex, " = ", 
                                varx_lm$fitted.values[length(varx[[piind]])] - varx_lm$fitted.values[1], 
                                " ", varx_infos[[piind]]$units, " / ",
                                length(unique(dims[[piind]]$timelt$year+1900)), " years (from ",
                                min(d$time[[piind]]), " to ", max(d$time[[piind]]), ")")
                        message("linear trend of ", names_short[piind], " ydata ", varnamey, " = ", 
                                vary_lm$fitted.values[length(vary[[piind]])] - vary_lm$fitted.values[1], 
                                " ", vary_infos[[piind]]$units, " / ",
                                length(unique(dims[[piind]]$timelt$year+1900)), " years (from ",
                                min(d$time[[piind]]), " to ", max(d$time[[piind]]), ")")
                        for (i in inds) {
                            if (length(varx[[i]]) != length(varx_lm$fitted.values)) {
                                stop("varx[[", i, "]] and varx_lm$fitted.values are of different length")
                            }
                            if (length(vary[[i]]) != length(vary_lm$fitted.values)) {
                                stop("vary[[", i, "]] and vary_lm$fitted.values are of different length")
                            }
                            if (!all(dims[[i]]$timelt$year == dims[[piind]]$timelt$year)) {
                                stop("years of setting ", i, " and ", piind, " differ")
                            }
                            varx[[i]] <- varx[[i]] - varx_lm$fitted.values
                            varx_infos[[i]]$label <- "2m temperature change [K]"
                            vary[[i]] <- vary[[i]] - vary_lm$fitted.values
                            vary_infos[[i]]$label <- eval(substitute(expression(paste("TOA imbalance change [W m"^paste(-2), "]"))))
                        }
                        # last: pi itself
                        varx[[piind]] <- varx[[piind]] - varx_lm$fitted.values # = fluctuation around zero
                        varx_infos[[piind]]$label <- "2m temperature change [K]"
                        #vary[[piind]] <- vary[[piind]] - vary_lm$fitted.values # = fluctuation around zero
                        vary_infos[[piind]]$label <- eval(substitute(expression(paste("TOA imbalance change [W m"^paste(-2), "]"))))
                    
                    } else {
                        warning("did not subtract any piControl values for ECS calculation. this is rather not ok!")
                    }
                    message()
                } # if varname == "temp2_vs_toa_imbalance" do ECS/TCR stuff

                xlim <- range(varx, na.rm=T)
                if (T) {
                    message("special xlim")
                    xlim <- range(-0.372528076171875, 7.16) # awi-esm-1-1-lr and awi-esm-1-2-lr dT_{eq,4xco2}
                    # awi-esm-1-1-lr: -0.372528076171875 / 7.16
                    # awi-esm-1-2-lr: -0.325225830078125 / 7.16
                }
                ylim <- range(vary, na.rm=T)
                if (T) {
                    message("special ylim")
                    ylim <- range(-1.83512371778488, 7.87) # largest awi-esm-1-1-lr and awi-esm-1-2-lr dN_{eq_4xco2}
                    # awi-esm-1-1-lr: -1.69532012939453 / 7.87
                    # awi-esm-1-2-lr: -1.83512371778488 / 7.87
                }
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
                if (add_1to1_line) {
                    message("add 1:1 line ...")
                    abline(a=0, b=1, col="gray") # a=intercept, b=slope
                }

                # add data to scatter plot
                message("add data ...")
                plotorder <- seq_along(varx)
                
                # special: change plot order
                if (T && any(varname == c("temp2_vs_toa_imbalance", "tas_vs_toa_imbalance")) && 
                    any(grepl("piControl", names_short))) {
                    # plot PI last
                    message("special: change plot order from ", 
                            paste(plotorder, collapse=","), " to ", appendLF=F)
                    plotorder <- c(plotorder[-which(grepl("piControl", names_short))],
                                   which(grepl("piControl", names_short)))
                    if (length(plotorder) == 0) stop("length(plotorder) new is 0")
                    message(paste(plotorder, collapse=","), " ...")
                }

                # special: add gray dots of data first
                if (T && any(varname == c("temp2_vs_toa_imbalance", "tas_vs_toa_imbalance"))) {
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
                    if (T && any(varname == c("temp2_vs_toa_imbalance", "tas_vs_toa_imbalance"))) {
                        if (T && grepl("piControl", names_short[i])) {
                            if (T) {
                                message("special: plot only time mean for piControl")
                                points(mean(varx[[i]]), mean(vary[[i]]), 
                                       #col=cols_rgb[i], 
                                       col=cols[i],
                                       #pch=scatterpchs[i], 
                                       pch=4,
                                       cex=scattercexs[i])
                            } else {
                                message("special: do not plot piControl")
                            }
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
                lm_text <- c()
                if (any(add_linear_trend)) {
                    message("\nadd linear trend in scatterplot varx vs vary ...")
                    lms_lin <- vector("list", l=length(varx))
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
                                           (any(varname == c("temp2_vs_toa_imbalance", "tas_vs_toa_imbalance")) && 
                                            grepl("abrupt-4xCO2", names_short[i]))) {
                                    message("draw linear regression line through whole plot ...")
                                    abline(a=lms_lin[[i]]$coefficients[1], b=lms_lin[[i]]$coefficients[2],
                                           col=cols_p[i], lwd=lwds_p[i], lty=ltys_p[i])
                                }
                                if (F) {
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
                                } else {
                                    message("do not add linear trend specs to legend")
                                }
                            } # if lm sucessfull or not
                           
                            # special stuff after linear regression 
                            if (T && any(varname == c("temp2_vs_toa_imbalance", "tas_vs_toa_imbalance"))) {
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
                                    
                                    if (F) { # add different linear regressions
                                        message("\nspecial: add different linear regressions")
                                        lms_special <- vector("list", l=2)
                                        for (j in seq_len(2)) {
                                            if (j == 1) time_inds <- which(dims[[i]]$timelt$year+1900 >= 1900) # last 100 years only (1900:1999)
                                            if (j == 2) time_inds <- which(dims[[i]]$timelt$year+1900 >= 1950) # last 50 years only (1950:1999)
                                            message("calc lm from ", paste(range(dims[[i]]$timelt[time_inds]), collapse=" to "), 
                                                    " (", length(unique(dims[[i]]$timelt$year[time_inds])), " years) ...")
                                            lms_special[[j]] <- list(time=dims[[i]]$timelt[time_inds],
                                                                     lm=lm(vary[[i]][time_inds] ~ varx[[i]][time_inds]))
                                            lm_summary <- summary(lms_special[[j]]$lm)
                                            lms_special[[j]]$lm_summary <- lm_summary
                                            intercept <- as.vector(lm_summary$coefficients[1,1])
                                            intercept_error <- as.vector(lm_summary$coefficients[1,2])
                                            intercept_pval <- paste0("=", lm_summary$coefficients[1,4])
                                            slope <- as.vector(lm_summary$coefficients[2,1])
                                            slope_error <- as.vector(lm_summary$coefficients[2,2])
                                            slope_pval <- lm_summary$coefficients[2,4]
                                            if (slope_pval < 1e-5) { 
                                                slope_pval <- "<1e-5"
                                            } else {
                                                slope_pval <- paste0("=", format(slope_pval, trim=T))
                                            }
                                            message("--> draw intercept = ", intercept, " and slope = ", slope, 
                                                    " (pval", slope_pval, ", r=", round(sqrt(lm_summary$r.squared), 2), 
                                                    ") with lty = ", 1+j, " ...")
                                            abline(a=intercept, b=slope, col=cols_p[i], lwd=lwds_p[i], lty=1+j)
                                            alpha <- slope
                                            alpha_error <- slope_error
                                            radiative_forcing_F <- intercept
                                            radiative_forcing_F_error <- intercept_error
                                            deltaT_eq_4x <- radiative_forcing_F/abs(alpha)
                                            deltaT_eq_4x_error <- sort(c((radiative_forcing_F - radiative_forcing_F_error)/(abs(alpha) - alpha_error),
                                                                         (radiative_forcing_F + radiative_forcing_F_error)/(abs(alpha) + alpha_error)))
                                            deltaT_eq_2x <- deltaT_eq_4x/2
                                            deltaT_eq_2x_error <- deltaT_eq_4x_error/2
                                            message("--> ECS = ", round(deltaT_eq_2x, 2), " K")
                                        } # for j
                                    } # if special

                                } # if abrupt-4xCO2
                            } # if temp2_vs_toa_imbalance
                            
                        } # if add_linear_trend[i]
                                
                    } # for i in seq_along(varx)
                    if (exists("deltaT_eq_2x") && exists("average_deltaT_1pct_co2_doubled")) {
                        CER <- average_deltaT_1pct_co2_doubled/deltaT_eq_2x # climate equilibrium ratio
                        message("min/max of climate equilibirum ratio CER = TCR/ECS = ", min(CER), "/", max(CER))
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
                
                # special: transient climate response TCR after winton et al. 2014
                # --> global warming as modeled in the 1pct experiment when the pi CO2 value doubled
                for (i in seq_along(varx)) {
                    if (grepl("1pctCO2", names_short[i]) && exists("co2_hist") && exists("co2_1pct")) {
                        message("\nspecial: transient climate response TCR based on `co2_hist` and `co2_1pct` after\n",
                                "Winton et al. 2014 (https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1002/2014GL061523):\n",
                                "   \"The transient sensitivity is quantified with the transient climate\n",
                                "   response (TCR), the global surface warming at CO2 doubling in a 1%/year\n",
                                "   CO2 increase experiment.\"\n",
                                "and later:\n",
                                "   \"The values for the transient climate response (TCR)—the year 61–80 \n",
                                "   average global warming—are ...\"\n",
                                "--> length(61:80) = ", length(61:80), " years used by Winton et al. 2014\n",
                                "see https://github.com/ESMValGroup/ESMValTool/issues/1901 ...")
                        co2_hist_1850_ind <- which(co2_hist$time$year+1900 == 1850)
                        if (length(co2_hist_1850_ind) != 1) stop("not defined for current `co2_hist` data")
                        co2_hist_1850 <- drop(co2_hist$co2_ppm[co2_hist_1850_ind])
                        co2_1pct_doubled_1850_ind <- which.min(abs(co2_1pct$co2_ppm - 2*co2_hist_1850))[1]
                        co2_1pct_doubled <- drop(co2_1pct$co2_ppm[co2_1pct_doubled_1850_ind])
                        co2_1pct_doubled_time <- co2_1pct$time[co2_1pct_doubled_1850_ind]
                        if (F) { # consider real dt (result index is 71)
                            co2_1pct_doubled_1850_model_ind <- difftime(d$time[[i]], 
                                                                        rep(co2_1pct_doubled_time, t=length(d$time[[i]])))
                            co2_1pct_doubled_1850_model_ind <- which.min(abs(co2_1pct_doubled_1850_model_ind))[1]
                        } else if (T) { # consider only matching year (result index is 72)
                            co2_1pct_doubled_1850_model_ind <- which.min(abs(as.POSIXlt(d$time[[i]])$year - co2_1pct_doubled_time$year))[1]
                        }
                        deltaT_1pct_co2_doubled <- varx[[i]][co2_1pct_doubled_1850_model_ind]
                        message("TCR_{1year} = dT_{modelexp_1pctCO2}[CO2_{CMIP6_1pctCO2}=2xCO2_{CMIP6_piControl}]\n",
                                "with CO2_{CMIP6_piControl} from e.g. ", co2_hist$file, "\n",
                                "     CO2_{CMIP6_1pctCO2} from e.g. ", co2_1pct$file, "\n",
                                "--> CO2_{CMIP6_piControl} = CO2_{CMIP6_historical,year=1850} = ",
                                "CO2_{CMIP6_1pctCO2,year=1850} = ", co2_hist_1850, " ppm\n",
                                "--> 2 x ", co2_hist_1850, " ppm = ", 2*co2_hist_1850, " ppm\n",
                                "--> closest CO2_{CMIP6_1pctCO2} = ", co2_1pct_doubled, 
                                " ppm from ", co2_1pct$time[co2_1pct_doubled_1850_ind], 
                                " (time from ", co2_1pct$file, ")\n",
                                "--> counting from year 1850 (=year number 1), this year ",
                                co2_1pct$time[co2_1pct_doubled_1850_ind]$year+1900, 
                                " is year number ",
                                co2_1pct$time[co2_1pct_doubled_1850_ind]$year+1900-1850+1, " (=",
                                co2_1pct$time[co2_1pct_doubled_1850_ind]$year+1900, "-1850+1)\n",
                                "--> the closest 1pctCO2 model year (not date) to this 1pctCO2-year is ",
                                "model-year number ", co2_1pct_doubled_1850_model_ind, ": ", 
                                d$time[[i]][co2_1pct_doubled_1850_model_ind], "\n",
                                "--> dT_{modelexp_1pctCO2,model-year[", co2_1pct_doubled_1850_model_ind, "]=", 
                                dims[[i]]$timelt[co2_1pct_doubled_1850_model_ind]$year+1900, "} = ", 
                                deltaT_1pct_co2_doubled, " K = ",
                                round(deltaT_1pct_co2_doubled, 2), " K")
                        
                        # use dt averaged over temporal period as TCR 
                        TCR_years_from_to <- c(1910, 1929) # 20-year mean as in winton et al. 2014: years 61-80
                        #TCR_years_from_to <- c(1910, 1930) # 21-year mean semmler et al. 2020: years 61-81 
                        year_inds <- which(as.POSIXlt(d$time[[i]])$year+1900 >= TCR_years_from_to[1] &
                                           as.POSIXlt(d$time[[i]])$year+1900 <= TCR_years_from_to[2])
                        if (length(year_inds) == 0) {
                            stop("cannot calc TCR between years ", TCR_years_from_to[1], " and ", 
                                 TCR_years_from_to[2], ": out of range of d$time[[", i, "]]")
                        }
                        if (any(duplicated(year_inds))) stop("some year_inds occur more than once. use annual values here")
                        average_deltaT_1pct_co2_doubled <- mean(varx[[i]][year_inds])
                        message("bar(TCR) = temporal average of dT_{modelexp_1pctCO2} over wanted calendar years from ",
                                TCR_years_from_to[1], " to ", TCR_years_from_to[2], "\n",
                                "--> closest model-dates are ", d$time[[i]][year_inds[1]], " to ",
                                d$time[[i]][year_inds[length(year_inds)]], "\n",
                                "--> year indices ", min(year_inds), " to ", max(year_inds), "\n",
                                "--> nyears = ", length(unique(as.POSIXlt(d$time[[i]])$year[year_inds])), "\n",
                                "--> bar(TCR) = ", average_deltaT_1pct_co2_doubled, " K = ", 
                                round(average_deltaT_1pct_co2_doubled, 2), " K")
                        lm_text <- c(lm_text, 
                                     eval(substitute(expression(paste(
    "TCR = ", bar(paste(Delta, "T"))["1%CO2"]^paste(nyears, " years"), "(CO"[2], "=2" %*% "CO"[paste("2,PI")], "=", co2_1pct_doubled, " ppm)")),
                                                     list(nyears=length(year_inds),
                                                          co2_hist_1850=round(co2_hist_1850), co2_1pct_doubled=round(co2_1pct_doubled)))),
                                     eval(substitute(expression(paste(
    "    = ", bar(paste(Delta, "T"))["1%CO2"]^paste("years", yearindsmin, "-", yearindsmax, "=", yearmin, "-", yearto), " = ", average_deltaT_1pct_co2_doubled, " K")),
                                                     list(yearindsmin=min(year_inds), yearindsmax=max(year_inds),
                                                          yearmin=as.POSIXlt(d$time[[i]][year_inds[1]])$year+1900,
                                                          yearto=as.POSIXlt(d$time[[i]][year_inds[length(year_inds)]])$year+1900,
                                                          average_deltaT_1pct_co2_doubled=round(average_deltaT_1pct_co2_doubled, 2))))
                                    )
                    } # TCR: if 1pctCO2 and exists("co2_hist")
                } # for i in seq_along(varx)
                
                if (!is.null(lm_text)) {
                    message("add special linear regression infos to plot")
                    legend("topright", 
                           lm_text, col="black", #text.col=text_cols[i], 
                           lty=NA, pch=NA, lwd=NA, bty="n", 
                           cex=0.8, y.intersp=1.1)
                }

                # add legend if wanted
                if (add_legend) {
                    message("\nadd default stuff to plot_scatter_v1_vs_v2 legend ...")
                    le <- list()
                    #le$pos <- "topright"
                    le$pos <- "bottomright"
                    le$ncol <- 1
                    #le$ncol <- 2 
                    le$cex <- lecex
                    le$text <- names_legend_p_w_lm # = names_legend_p if no lm
                    le$col <- cols_p
                    #le$col <- cols_rgb
                    #le$col <- cols
                    le$text_cols <- text_cols_p
                    le$lty <- NA
                    le$lwds <- NA
                    le$pchs <- scatterpchs
                    #le$pchs <- pchs_p
                    if (T && any(varname == c("temp2_vs_toa_imbalance", "tas_vs_toa_imbalance"))) {
                        message("special legend pchs")
                        le$pchs <- rep(NA, t=length(varx))
                    }
                    # add stuf to legend here
                    if (F) {
                        message("add non default stuff to plot_scatter_v1_vs_v2 legend ...")

                    }
                    # reorder reading direction from R's default top->bottom to left->right
                    if (T) le <- reorder_legend(le)
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
                write(plotname, file=lastfiles_plot_fname, append=T)
                if (p$plot_type == "pdf") {
                    if (T) {
                        message("run `", p$pdf_embed_fun, "()` ...")
                        if (p$pdf_embed_fun == "grDevices::embedFonts") {
                            grDevices::embedFonts(plotname, outfile=plotname)
                        } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                            extrafont::embed_fonts(plotname, outfile=plotname)
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
                            if (add_1to1_line) {
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
                                le$cex <- lecex
                                le$cex <- 0.85
                                le$text <- names(season_cols) #names_legend[i]
                                le$col <- season_cols #"black"
                                le$lty <- NA
                                le$lwds <- NA
                                #le$pchs <- scatterpchs_vstime[i]
                                le$pchs <- scatterpchs_vstime
                                if (T) le <- reorder_legend(le)
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
                            write(plotname, file=lastfiles_plot_fname, append=T)
                            if (p$plot_type == "pdf") {
                                if (T) {
                                    message("run `", p$pdf_embed_fun, "()` ...")
                                    if (p$pdf_embed_fun == "grDevices::embedFonts") {
                                        grDevices::embedFonts(plotname, outfile=plotname)
                                    } else if (p$pdf_embed_fun == "extrafont::embed_fonts") {
                                        extrafont::embed_fonts(plotname, outfile=plotname)
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

