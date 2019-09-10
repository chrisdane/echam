## R

rm(list=ls()); graphics.off()

# Host options
machine <- system("hostname -f", intern=T)
message("Run on ", machine, ":")
if (regexpr("ollie", machine) != -1 ||
    regexpr("prod-", machine) != -1 ||
    regexpr("fat-", machine) != -1) {
    machine_tag <- "ollie"
    workpath <- "/work/ollie/cdanek/post"
    plotpath <- "/work/ollie/cdanek/plots"
} else if (regexpr("hpc.dkrz", machine) != -1) {
    machine <- substr(machine, 1, regexpr(".hpc.dkrz", machine) - 1)
    machine_tag <- "mistral"
    workpath <- "/work/ab0246/a270073/post"
    plotpath <- "/work/ab0246/a270073/plots"
} else {
    stop("machine ", machine, " is unknown. stop")
}
message("workpath = ", workpath)
message("plotpath = ", plotpath)
# =====================================

fctbackup <- `[`; `[` <- function(...) { fctbackup(..., drop=F) }

# =====================================
# 6 settings

if (T) {
    models <- c("awicm-CMIP6", rep("awicm-test", t=5))
    runidsf <- c("PI-CTRL4", rep("CMIP6/CMIP_PMIP/dynveg_true/hist", t=5))
    runidsp <- c("PI-CTRL4", rep("hist", t=5))
    runidsl <- c("spinup day", paste0("hist ", c("1hr", "3hr", "6hr", "day", "mon")))
    fromsf <- c(2911, rep(1850, t=5))
    tosf <- c(2999, rep(1886, t=5))
    #froms_shift <- c(1849, rep(NA, t=5)) # from 2999
    froms_shift <- c(1761, rep(NA, t=5)) # from 2911 
    fromsp <- c("1846-01-01 00:00:00", rep(1850, t=5))
    #tosp <- c(1849, rep("1850-01-31 23:59:59", t=5))
    tosp <- c(1849, rep("1863-01-31 23:59:59", t=5))
    seasonsf <- rep("Jan-Dec", t=6)
    seasonsp <- seasonsf
    areas <- rep("global", t=6)
    varnames <- rep("temp2", t=6)
    submodels <- rep("echam", t=6)
    streams <- c("g3bid", "ma", "echam3hr", "echam6hr", "echamday", "echammon")
    #n_mas <- c(30, 24*30, 8*30, 4*30, 30, 1)
}

# ==================================================

# options across runids
mode <- "fldmean" # "timmean" "fldmean"
add_title <- F
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
nrunids <- length(runidsf)
if (!exists("levs")) levs <- rep("", t=nrunids)
if (!exists("cols")) cols <- 1:nrunids
if (!exists("ltys")) ltys <- rep(1, t=nrunids)
if (!exists("lwds")) lwds <- rep(1, t=nrunids)
if (!exists("pchs")) pchs <- rep(NA, t=nrunids)
if (!exists("n_mas")) n_mas <- rep(1, t=nrunids) # no moving average effect
if (!exists("fromsp")) fromsp <- fromsf
if (!exists("tosp")) tosp <- tosf
levsf <- levs
levsf[levs != ""] <- paste0("_", levsf[levs != ""], "m")
tos_shift <- froms_shift

base <- 10
power <- 0 # default: 0 --> 10^0 = 1e0 = 1 --> nothing happens
alpha <- 0.15 # transparent: 0,1 (1 fully transparent)
cols_rgb <- rgb(t(col2rgb(cols)/255), alpha=alpha)

# allocate
datas <- vector("list", l=nrunids)
names(datas) <- runidsl
times <- times2 <- times3 <- timesp <- timeslt <- lons <- lats <- datas

message("\nRead data ...\n")
# read data
for (i in 1:nrunids) {
   
    message("runid ", appendLF=F)
    print(i)
    inpath <- paste0(workpath, "/", models[i], "/", runidsf[i], "/", submodels[i], "/", 
                     mode, "/", areas[i], "/", varnames[i])
    fname <- paste0(inpath, "/", 
                    models[i], "_", runidsp[i], "_", varnames[i], "_", streams[i], "_", 
                    mode, "_", areas[i], levsf[i], "_", seasonsf[i], "_", fromsf[i], "-", tosf[i], ".nc")

    message("open ", appendLF=F)
    print(fname)
    ncin <- nc_open(fname)

    if (any(names(ncin$dim) == "time")) times[[i]] <- ncin$dim$time$vals
    if (any(names(ncin$dim) == "time2")) times2[[i]] <- ncin$dim$time2$vals
    if (any(names(ncin$dim) == "time3")) times3[[i]] <- ncin$dim$time3$vals
    if (any(names(ncin$dim) == "lon")) lons[[i]] <- ncin$dim$lon$vals
    if (any(names(ncin$dim) == "lat")) lats[[i]] <- ncin$dim$lat$vals

    vars <- vector("list", l=ncin$nvars)
    names(vars) <- names(ncin$var)
    for (vi in 1:length(vars)) { # 
        vars[[vi]] <- ncvar_get(ncin, names(vars)[vi])
    }
    datas[[i]] <- vars
    rm(vars)

    # which time to use?
    if (!is.null(times[[i]])) timesp[[i]] <- times[[i]] # default
    if (!is.null(times2[[i]])) timesp[[i]] <- times2[[i]]
    if (!is.null(times3[[i]])) timesp[[i]] <- times3[[i]] 

    # make posix object
    if (!is.null(times[[i]])) {
        
        timein_units <- ncin$dim$time$units
        message("make POSIXlt from timein_units = ", appendLF=F)
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
            timein_lt <- as.POSIXlt(times[[i]]*timein_fac, origin=timein_origin, tz="UTC")
        # case 2: e.g. "day as %Y%m%d.%f"
        } else if (regexpr(" as ", timein_units) != -1) { 
            timein_unit <- substr(timein_units, 1, regexpr(" as ", timein_units) - 1)
            timein_format <- substr(timein_units, regexpr(" as ", timein_units) + 4, nchar(timein_units))
            if (timein_format == "%Y%m%d.%f") { # e.g. "29991201.9944444"
                hours <- 24*(times[[i]] - floor(times[[i]]))
                mins <- 60*(hours - floor(hours))
                secs <- 60*(mins - floor(mins))
                hours <- floor(hours)
                mins <- floor(mins)
                secs <- floor(secs)
                timein_lt <- as.POSIXlt(paste0(substr(times[[i]], 1, 4), "-", 
                                               substr(times[[i]], 5, 6), "-",
                                               substr(times[[i]], 7, 8), " ",
                                               hours, ":", mins, ":", secs), tz="UTC")
            } else {
                stop("timein_format=", timein_format, " not defined")
            }
        }
        timeslt[[i]] <- timein_lt
        
        # apply different times to e.g. senseless spinup years
        # as.POSIXlt's 'year' starts at 1900
        if (!is.na(froms_shift[i])) {
            # from year in  = min(timeslt[[i]]$year) + 1900
            message("range(timeslt[[", i, "]]) = ", appendLF=F)
            print(range(timeslt[[i]]))
            shift_by <- -(min(timeslt[[i]]$year) + 1900 - froms_shift[i]) 
            message("shift fromsf[", i, "]=", fromsf[i], 
                    " to froms_shift[", i, "]=", froms_shift[i], " by ", shift_by, " years ...")
            timeslt[[i]]$year <- timeslt[[i]]$year + shift_by
            tos_shift[i] <- max(timeslt[[i]]$year) + 1900
        } # if !is.na(froms_shift[i])

        # check time 
        message("range(timeslt[[", i, "]]) = ", appendLF=F)
        print(range(timeslt[[i]]))

    } # if (!is.null(times[[i]]))

    message()

} # for i nrunids

# set variable specific things
if (F) { # for testing
    datas[[1]][[2]] <- datas[[1]][[1]] + 10
    names(datas[[1]])[2] <- names(datas[[1]])[1]
}
nvars_per_runid <- sapply(datas, length)
v <- datas
for (i in 1:nrunids) {
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

# apply offset or mult_fac
for (i in 1:nrunids) {
    for (vi in 1:nvars_per_runid[i]) {
        if (!is.null(v[[i]][[vi]]$offset) &&
            !is.null(v[[i]][[vi]]$mult_fac)) {
            stop("both \"offset\" and \"mult_fac\" are defined for runid ", 
                 runidsl[i], " variable ", names(v[[i]])[vi], ". dont know which to take")
        }
        if (!is.null(v[[i]][[vi]]$offset)) {
            cmd <- paste0("datas[[", i, "]][[", vi, "]] <- datas[[", i, "]][[", vi, "]] ", 
                          v[[i]][[vi]]$offset[1], " ", v[[i]][[vi]]$offset[2])
            message("eval ", cmd, " ...")
            eval(parse(text=cmd))
        }
    }
}

# check stuff if times are set
if (all(!sapply(times, is.null))) {

    message("\nfind temporal subsets based on fromsp to tosp ...")
    message("timeslt ranges:")
    print(lapply(timeslt, range))
    
    for (i in 1:nrunids) {
        # check if first time of data and fromsp are different
        if (nchar(fromsp[i]) == 4) { # standard YYYY format was provided --> take earliest possible
            fromsplt <- as.POSIXlt(paste0(fromsp[i], "-01-01 00:00:00"), tz="UTC")
        } else { # POSIX format was provided
            fromsplt <- as.POSIXlt(fromsp[i], tz="UTC")
        }
        if (nchar(tosp[i]) == 4) { # standard YYYY format was provided --> take last possible 
            tosplt <- as.POSIXlt(paste0(tosp[i], "-12-31 23:59:59"), tz="UTC")
        } else { # POSIX format was provided
            tosplt <- as.POSIXlt(tosp[i], tz="UTC")
        }
        # find inds between wanted times
        inds <- which(timeslt[[i]] >= fromsplt & timeslt[[i]] <= tosplt)
        if (length(inds) > 0) { # take subset only if length(inds) > 0
            if (length(inds) != length(timeslt[[i]])) {
                message(i, " ", runidsl[i], ": subset ", length(inds), " of ", 
                        length(timeslt[[i]]), " datapoints:", appendLF=F)
                timeslt[[i]] <- timeslt[[i]][inds]
                for (vi in 1:length(datas[[i]])) {
                    datas[[i]][[vi]] <- datas[[i]][[vi]][inds]
                }
                print(range(timeslt[[i]]))
            }
        }
    } # find temporal subsets for all runids

    # subset seasons from data if wanted (=seasonsp)
    # check which seasonsf and seasonp differ
    for (i in 1:nrunids) {
        if (seasonsp[i] != seasonsf[i]) {
            message("todo")
        }
    }

    message("\ntimeslt ranges:")
    print(lapply(timeslt, range))

    # POSIXlt as numeric
    timesn <- lapply(timeslt, as.numeric)
    
    # apply ma
    if (all(n_mas == 1) && !add_unsmoothed) {
        stop("add_unsmoothed is false but all moving average filers are 1 (n_mas)")
    }
    if (any(n_mas != 1)) {
        message("\napply moving averages ...")
        datasma <- datas
        for (i in 1:nrunids) {
            for (vi in 1:length(datasma[[i]])) { # for all vars
                message("n_ma ", runidsl[i], ": ", n_mas[i], 
                        " (n = ", length(datas[[i]][[vi]]), 
                        " --> ", length(datas[[i]][[vi]]), "/", n_mas[i], " = ", 
                        length(datas[[i]][[vi]])/n_mas[i], ")")
                datasma[[i]][[vi]] <- filter(datas[[i]][[vi]], filter=rep(1/n_mas[i], t=n_mas[i]))
            }
        }
    } # if any n_mas != 1

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

# save data for later
cmd <- paste0(paste0(unique(varnames), collapse="_"), "_datas <- datas")
message("\nfor later save ", cmd, " ...\n")
eval(parse(text=cmd))

# start mode specific things
if (mode == "fldmean") {

    message("fldmean plot ...")
    
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
            message("\ntemporal min/mean/max datama:")
            for (i in 1:nrunids) {
                message(runidsl[i], " = ", min(datama[[i]][[1]], na.rm=T), "/",
                        mean(datama[[i]][[1]], na.rm=T), "/", max(datama[[i]][[1]], na.rm=T))
            }
            ylim <- range(datama, na.rm=T)
            if (add_unsmoothed) {
                message("\ntemporal min/mean/max data:")
                for (i in 1:nrunids) {
                    message(runidsl[i], " = ", min(data[[i]][[1]], na.rm=T), "/",
                            mean(data[[i]][[1]], na.rm=T), "/", max(data[[i]][[1]], na.rm=T))
                }
                ylim <- range(ylim, data, na.rm=T)
            }
        } else {
            message("\ntemporal min/mean/max data:")
            for (i in 1:nrunids) {
                message(runidsl[i], " = ", min(data[[i]][[1]], na.rm=T), "/",
                        mean(data[[i]][[1]], na.rm=T), "/", max(data[[i]][[1]], na.rm=T))
            }
            ylim <- range(data, na.rm=T)
        }
        message("\nylim=", appendLF=F)
        dput(ylim)

        yat <- pretty(ylim)

        # plotname
        plotname <- paste0(plotpath, "/", mode, "/", varsf, "/",
                           paste0(unique(paste0(models, "_", runidsp)), collapse="_vs_"), "_",
                           paste0(unique(areas), collapse="_vs_"), "_",
                           ifelse(any(levsf != ""), 
                                  paste0(unique(levsf), collapse="_vs_"), ""), "_",
                           paste0(unique(streams), collapse="_vs_"), "_",
                           paste0(unique(seasonsp), collapse="_vs_"), "_",
                           gsub(" ", "_", paste0(tlimlt, collapse="-")),
                           ".", p$plot_type)
        dir.create(dirname(plotname), recursive=T, showWarnings=F)
        message("\nSave ", plotname, " ...")
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
            message("\nadd title ...")
        }

        # add variable label
        mtext(side=2, v[[1]][[vi]]$label, line=3.4, cex=0.9)

        # add grid
        if (add_grid) {
            message("\nadd grid ...")
            abline(v=tatn, h=yat, col="gray", lwd=0.5)
        }

        # add data
        for (i in 1:nrunids) {

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

                # add first data point
                if (T) {
                    if (i == 1) message("\nadd first data point")
                    points(timeslt[[i]][1], data[[i]][[1]][1], 
                          col=cols[i], lty=ltys[i], lwd=lwds[i], 
                          pch=1)
                } # add first data point

                # add linear regression trend if wanted
                if (add_trend) {

                }

            } # if (!is.null(data[[i]][[1]]))

        } # for i nrunids add data 

        # add legend if wanted
        if (add_legend) {
            le <- list()
            #le$pos <- "topleft" 
            le$pos <- "bottomleft" 
            #le$pos <- "bottomright" 
            #le$ncol <- nrunids/2
            le$ncol <- 1
            #le$ncol <- 2 
            le$text <- runidsl
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
        dev.off()

    } # for vi max(nvars_per_runid)

} # if mode = fldmean

message("\nfinish\n")
