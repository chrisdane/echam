# r

# berger orbital parameter for last 800ka
if (F && exists("my_orb_berger")) {
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
} # plot berger orbital parameter for last 800ka
   

# laskar orbital parameter for last 800ka
if (F && exists("my_orb_laskar")) { # plot laskar
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
} # plot laskar orbital parameter for last 800ka


# compare berger and laskar orb
if (F && exists("my_orb_berger") && exists("my_orb_laskar")) {
    message("\ndisable here if you do not want to compare berger vs laskar orbital parameters ...")
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
} # compare berger and laskar orb


# compare koehler et al. 2017 vs paul
if (F && exists("koehler_etal_2017")) { 
    message("\ndisable here if you do not want to compare pauls and koehlers et al. 2017 CO2 ...")
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


if (F && exists("meyer_etal")) { # plot meyer et al data
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
} # if plot meyer et al data


# plot silt data
if (F && save_silt_list) {
    message("\ndisable here if you do not want to plot silt data ...")
    x <- y <- list()
    legend_text <- c()
    cnt <- 0
    #silt_from <- -10000
    silt_from <- -7000
    for (i in seq_along(silt_all)) {
        for (j in seq_along(silt_all[[i]]$data)) {
            cnt <- cnt + 1
            if (silt_from == "all") {
                tinds <- seq_along(silt_all[[i]]$data[[j]]$time)
            } else { 
                # find all closest values to wanted limit and take the closest in terms of index
                tinds <- which(abs(silt_all[[i]]$data[[j]]$time$year+1900 - silt_from) == 
                               min(abs(silt_all[[i]]$data[[j]]$time$year+1900 - silt_from)))
                if (all(silt_from >= silt_all[[i]]$data[[j]]$time[tinds])) {
                    tinds <- (tinds[length(tinds)]):length(silt_all[[i]]$data[[j]]$time)
                } else {
                    tinds <- 1:tinds[1]
                }
            }
            if (silt_all[[i]]$data[[j]]$text == "Praetorius et al. 2008") {
                y[[cnt]] <- scale(silt_all[[i]]$data[[j]]$silt_fraction[tinds])
                legend_text[cnt] <- paste0("A ", silt_all[[i]]$data[[j]]$text)
            } else if (silt_all[[i]]$data[[j]]$text == "Hoogakker et al. 2011 (Gardar Drift; NEA)") {
                y[[cnt]] <- filter(scale(silt_all[[i]]$data[[j]]$silt_size[tinds]), filter=rep(1/30, t=30))
                legend_text[cnt] <- paste0("B ", silt_all[[i]]$data[[j]]$text)
            } else if (silt_all[[i]]$data[[j]]$text == "Thornalley et al. 2013 (Iceland Basin stack)") {
                y[[cnt]] <- scale(silt_all[[i]]$data[[j]]$silt_size_change[tinds])
                legend_text[cnt] <- paste0("C ", silt_all[[i]]$data[[j]]$text)
            } else if (silt_all[[i]]$data[[j]]$text == "Mjell et al. 2015") {
                y[[cnt]] <- filter(scale(silt_all[[i]]$data[[j]]$silt_size[tinds]), filter=rep(1/30, t=30))
                legend_text[cnt] <- paste0("D ", silt_all[[i]]$data[[j]]$text)
            } else {
                cnt <- cnt - 1
                next # data of ref
            }
            x[[cnt]] <- silt_all[[i]]$data[[j]]$time[tinds]
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
    message("\ndisable here if you do not want to plot Pa/Th data ...")
    #PaTh_from <- "all"
    #PaTh_from <- -12000
    #PaTh_from <- -10000
    PaTh_from <- -7000
    message("plot all ", length(PaTh_all), " Pa/Th proxy data from ", PaTh_from, " ...") 
    library(RColorBrewer) # https://www.r-bloggers.com/palettes-in-r/
    cols_PaTh <- c("black", "#E41A1C", "#377EB8", 
                   brewer.pal(max(3, length(PaTh_all)), "Dark2")[1:(length(PaTh_all)-3)])
    tmp <- lonlim <- latlim <- tlim <- zlim <- list()
    cnt <- 0
    for (i in seq_along(PaTh_all[[i]])) {
        for (j in seq_along(PaTh_all[[i]]$data)) {
            if (PaTh_from == "all") {
                tinds <- seq_along(PaTh_all[[i]]$data[[j]]$time)
            } else { 
                # find all closest values to wanted limit and take the closest in terms of index
                tinds <- which(abs(PaTh_all[[i]]$data[[j]]$time$year+1900 - PaTh_from) == 
                               min(abs(PaTh_all[[i]]$data[[j]]$time$year+1900 - PaTh_from)))
                if (all(PaTh_from >= PaTh_all[[i]]$data[[j]]$time[tinds])) {
                    tinds <- (tinds[length(tinds)]):length(PaTh_all[[i]]$data[[j]]$time)
                } else {
                    tinds <- 1:tinds[1]
                }
            }
            if (length(tinds) > 1) {
                cnt <- cnt + 1
                lonlim[[cnt]] <- range(PaTh_all[[i]]$data[[j]]$lon)
                latlim[[cnt]] <- range(PaTh_all[[i]]$data[[j]]$lat)
                tlim[[cnt]] <- range(PaTh_all[[i]]$data[[j]]$timen[tinds])
                zlim[[cnt]] <- range(PaTh_all[[i]]$data[[j]][["Pa/Th"]][tinds], na.rm=T)
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
    for (i in seq_along(PaTh_all[[i]])) {
        for (j in seq_along(PaTh_all[[i]]$data)) {
            if (PaTh_from == "all") {
                tinds <- seq_along(PaTh_all[[i]]$data[[j]]$time)
            } else {
                # find all closest values to wanted limit and take the closest in terms of index
                tinds <- which(abs(PaTh_all[[i]]$data[[j]]$time$year+1900 - PaTh_from) == 
                               min(abs(PaTh_all[[i]]$data[[j]]$time$year+1900 - PaTh_from)))
                if (all(PaTh_from >= PaTh_all[[i]]$data[[j]]$time[tinds])) {
                    tinds <- (tinds[length(tinds)]):length(PaTh_all[[i]]$data[[j]]$time)
                } else {
                    tinds <- 1:tinds[1]
                }
            }
            x <- PaTh_all[[i]]$data[[j]]$timen[tinds]
            y <- PaTh_all[[i]]$data[[j]][["Pa/Th"]][tinds]
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
                    PaTh_all[[i]]$data[[j]]$trend_col <- col
                } else { # color by author
                    col <- cols_PaTh[i]
                }
                lines(x, y, 
                      col=col, 
                      #type=PaTh_all[[i]]$type,
                      #lty=PaTh_all[[i]]$lty, 
                      lty=cnt,
                      lwd=PaTh_all[[i]]$lwd)
                # add counter to plot
                text(x[first_time_ind], y[first_time_ind],
                     labels=cnt, col=col, cex=0.5)
                text(x[last_time_ind], y[last_time_ind],
                     labels=cnt, col=col, cex=0.5)
                legend_ids[cnt] <- PaTh_all[[i]]$data[[j]]$id 
                legend_names[cnt] <- paste0(cnt, " (", i, ",", j, ") ", PaTh_all[[i]]$data[[j]]$text, 
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
    if (exists("praetorius_etal_2008")) {
        lonlim <- range(lonlim, praetorius_etal_2008$data[[1]]$lon)
        latlim <- range(latlim, praetorius_etal_2008$data[[1]]$lat) 
    }
    if (exists("hoogakker_etal_2011")) {
        lonlim <- range(lonlim, hoogakker_etal_2011$data[[2]]$lon)
        latlim <- range(latlim, hoogakker_etal_2011$data[[2]]$lat) 
    }
    if (exists("thornalley_etal_2013")) {
        lonlim <- range(lonlim, thornalley_etal_2013$data[[1]]$lon)
        latlim <- range(latlim, thornalley_etal_2013$data[[1]]$lat) 
    }
    if (exists("mjell_etal_2015")) {
        lonlim <- range(lonlim, mjell_etal_2015$data[[1]]$lon)
        latlim <- range(latlim, mjell_etal_2015$data[[1]]$lat) 
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
    for (i in seq_along(PaTh_all)) {
        for (j in seq_along(PaTh_all[[i]]$data)) {
            if (T) {
                if (any(names(PaTh_all[[i]]$data[[j]]) == "trend_col")) {
                    cnt <- cnt + 1
                    text(PaTh_all[[i]]$data[[j]]$lon, PaTh_all[[i]]$data[[j]]$lat, 
                         #labels=paste0(i, ",", j), 
                         labels=cnt,
                         col=PaTh_all[[i]]$data[[j]]$trend_col,
                         cex=0.66)
                }
            } else {
                text(PaTh_all[[i]]$data[[j]]$lon, PaTh_all[[i]]$data[[j]]$lat, 
                     labels=paste0(i, ",", j), col=cols_PaTh[i],
                     cex=0.66)
            }
        }
    }
    # add silt locations
    if (exists("praetorius_etal_2008")) {
        text(praetorius_etal_2008$data[[1]]$lon, praetorius_etal_2008$data[[1]]$lat, 
             labels="A", cex=0.66)
    }
    if (exists("hoogakker_etal_2011")) {
        text(hoogakker_etal_2011$data[[2]]$lon, hoogakker_etal_2011$data[[2]]$lat, 
             labels="B", cex=0.66)
    }
    if (exists("thornalley_etal_2013")) {
        text(thornalley_etal_2013$data[[1]]$lon, thornalley_etal_2013$data[[1]]$lat, 
             labels="C", cex=0.66)
    }
    if (exists("mjell_etal_2015")) {
        text(mjell_etal_2015$data[[1]]$lon, mjell_etal_2015$data[[1]]$lat, 
             labels="D", cex=0.66)
    }
    dev.off()
} # if save_PaTh_list

