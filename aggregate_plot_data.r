# r

# save all data loaded by namelist.plot.r to a single ncdf
message("************************* aggregate_plot_data.r start *************************")

lacroix <- NULL
if (any(zname == c("co2_flx_ocean", "fgco2")) && 
    all(modes == "fldint") && 
    length(unique(areas)) == 1 &&
    models[1] != "gregor_and_fay_2021") { # load lacroix_etal_2020 river flux adjustment rfa
    flacroix <- paste0(host$workpath, 
                       "/post/lacroix_etal_2020/fldint/fgco2/lacroix_etal_2020_lacroix_etal_2020_fldint_fgco2_",
                       areas[1], "_Jan-Dec_2022-2022.nc")
    if (!file.exists(flacroix)) stop("flacroix not found")
    lacroix <- nc_open(flacroix)
    lacroix <- ncvar_get(lacroix, "fgco2") * 12.0107 * 365.25*86400 / 1e15 * -1 # molC s-1 --> PgC yr-1; >0: upward --> >0: downward
    lacroix <- as.vector(lacroix)
    message("add lacroix = ", lacroix)
}
modeldim <- ncdf4::ncdim_def(name="model", units="", vals=seq_along(z))
fromto <- seq(anlim[1], anlim[2], b=1)
tvals <- paste0(rep(fromto, e=12), "-", rep(1:12, t=length(fromto)), "-", rep(15, t=length(fromto)*12)) # all monthly data at same time points
tvals <- as.POSIXct(tvals, tz="UTC")
tdim <- ncdf4::ncdim_def(name="time", units="seconds since 1970-1-1", vals=as.numeric(tvals))
# put all datas on same %Y-%m date
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
arr_sd <- apply(arr, 2, sd, na.rm=T)
ncvar <- ncdf4::ncvar_def(name=paste0(zname, "_mon"), units=data_info$units, dim=list(modeldim, tdim), missval=NA)
ncvar_min <- ncdf4::ncvar_def(name=paste0(zname, "_mon_min"), units=data_info$units, dim=tdim, missval=NA)
ncvar_max <- ncdf4::ncvar_def(name=paste0(zname, "_mon_max"), units=data_info$units, dim=tdim, missval=NA)
ncvar_mean <- ncdf4::ncvar_def(name=paste0(zname, "_mon_mean"), units=data_info$units, dim=tdim, missval=NA)
ncvar_median <- ncdf4::ncvar_def(name=paste0(zname, "_mon_median"), units=data_info$units, dim=tdim, missval=NA)
ncvar_sd <- ncdf4::ncvar_def(name=paste0(zname, "_mon_sd"), units=data_info$units, dim=tdim, missval=NA)
if (!is.null(lacroix)) {
    arr_lacroix <- arr + lacroix
    arr_lacroix_min <- apply(arr_lacroix, 2, min, na.rm=T)
    arr_lacroix_max <- apply(arr_lacroix, 2, max, na.rm=T)
    arr_lacroix_mean <- apply(arr_lacroix, 2, mean, na.rm=T)
    arr_lacroix_median <- apply(arr_lacroix, 2, median, na.rm=T)
    arr_lacroix_sd <- apply(arr_lacroix, 2, sd, na.rm=T)
    ncvar_lacroix <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_mon"), units=data_info$units, dim=list(modeldim, tdim), missval=NA)
    ncvar_lacroix_min <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_mon_min"), units=data_info$units, dim=tdim, missval=NA)
    ncvar_lacroix_max <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_mon_max"), units=data_info$units, dim=tdim, missval=NA)
    ncvar_lacroix_mean <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_mon_mean"), units=data_info$units, dim=tdim, missval=NA)
    ncvar_lacroix_median <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_mon_median"), units=data_info$units, dim=tdim, missval=NA)
    ncvar_lacroix_sd <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_mon_sd"), units=data_info$units, dim=tdim, missval=NA)
}
# annual means
anvals <- as.POSIXct(paste0(fromto, "-1-1"), tz="UTC") # placeholder
for (i in seq_along(anvals)) {
    anvals[i] <- mean(as.POSIXct(paste0(fromto[i], "-", c(1, 12), "-", c(1, 31)), tz="UTC")) # average of year
}
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
arr_an_sd <- apply(arr_an, 2, sd, na.rm=T)
ncvar_an <- ncdf4::ncvar_def(name=paste0(zname, "_an"), units=data_info$units, dim=list(modeldim, andim), missval=NA)
ncvar_an_min <- ncdf4::ncvar_def(name=paste0(zname, "_an_min"), units=data_info$units, dim=andim, missval=NA)
ncvar_an_max <- ncdf4::ncvar_def(name=paste0(zname, "_an_max"), units=data_info$units, dim=andim, missval=NA)
ncvar_an_mean <- ncdf4::ncvar_def(name=paste0(zname, "_an_mean"), units=data_info$units, dim=andim, missval=NA)
ncvar_an_median <- ncdf4::ncvar_def(name=paste0(zname, "_an_median"), units=data_info$units, dim=andim, missval=NA)
ncvar_an_sd <- ncdf4::ncvar_def(name=paste0(zname, "_an_sd"), units=data_info$units, dim=andim, missval=NA)
if (!is.null(lacroix)) {
    arr_an_lacroix <- arr_an + lacroix
    arr_an_lacroix_min <- apply(arr_an_lacroix, 2, min, na.rm=T)
    arr_an_lacroix_max <- apply(arr_an_lacroix, 2, max, na.rm=T)
    arr_an_lacroix_mean <- apply(arr_an_lacroix, 2, mean, na.rm=T)
    arr_an_lacroix_median <- apply(arr_an_lacroix, 2, median, na.rm=T)
    arr_an_lacroix_sd <- apply(arr_an_lacroix, 2, sd, na.rm=T)
    ncvar_an_lacroix <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_an"), units=data_info$units, dim=list(modeldim, andim), missval=NA)
    ncvar_an_lacroix_min <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_an_min"), units=data_info$units, dim=andim, missval=NA)
    ncvar_an_lacroix_max <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_an_max"), units=data_info$units, dim=andim, missval=NA)
    ncvar_an_lacroix_mean <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_an_mean"), units=data_info$units, dim=andim, missval=NA)
    ncvar_an_lacroix_median <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_an_median"), units=data_info$units, dim=andim, missval=NA)
    ncvar_an_lacroix_sd <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_an_sd"), units=data_info$units, dim=andim, missval=NA)
}
# monthly clim
monvals <- as.POSIXct(paste0(max(fromto), "-", 1:12, "-15"), tz="UTC") # average of month in placeholder year
mondim <- ncdf4::ncdim_def(name="months", units="seconds since 1970-1-1", vals=as.numeric(monvals))
arr_mon <- array(NA, dim=c(nsettings, length(monvals)))
for (i in seq_along(zmon)) {
    for (ti in seq_along(zmon[[i]])) {
        ind <- which(gsub("^0", "", format(monvals, "%m")) == dmon$month[[i]][ti])
        if (length(ind) != 1) stop("this should not happen")
        arr_mon[i,ind] <- zmon[[i]][ti]
    }
    if (length(unique(as.vector(arr_mon[i,]))) == 1) { # OCIM-v2014 has constant monthly values
        message("exclude ", models[i], " from mon ens stats ...")
        arr_mon[i,] <- NA
    }
}
arr_mon_min <- apply(arr_mon, 2, min, na.rm=T)
arr_mon_max <- apply(arr_mon, 2, max, na.rm=T)
arr_mon_mean <- apply(arr_mon, 2, mean, na.rm=T)
arr_mon_median <- apply(arr_mon, 2, median, na.rm=T)
arr_mon_sd <- apply(arr_mon, 2, sd, na.rm=T)
ncvar_mon <- ncdf4::ncvar_def(name=paste0(zname, "_ymonmean"), units=data_info$units, dim=list(modeldim, mondim), missval=NA)
ncvar_mon_min <- ncdf4::ncvar_def(name=paste0(zname, "_ymonmean_min"), units=data_info$units, dim=mondim, missval=NA)
ncvar_mon_max <- ncdf4::ncvar_def(name=paste0(zname, "_ymonmean_max"), units=data_info$units, dim=mondim, missval=NA)
ncvar_mon_mean <- ncdf4::ncvar_def(name=paste0(zname, "_ymonmean_mean"), units=data_info$units, dim=mondim, missval=NA)
ncvar_mon_median <- ncdf4::ncvar_def(name=paste0(zname, "_ymonmean_median"), units=data_info$units, dim=mondim, missval=NA)
ncvar_mon_sd <- ncdf4::ncvar_def(name=paste0(zname, "_ymonmean_sd"), units=data_info$units, dim=mondim, missval=NA)
if (!is.null(lacroix)) {
    arr_mon_lacroix <- arr_mon + lacroix
    arr_mon_lacroix_min <- apply(arr_mon_lacroix, 2, min, na.rm=T)
    arr_mon_lacroix_max <- apply(arr_mon_lacroix, 2, max, na.rm=T)
    arr_mon_lacroix_mean <- apply(arr_mon_lacroix, 2, mean, na.rm=T)
    arr_mon_lacroix_median <- apply(arr_mon_lacroix, 2, median, na.rm=T)
    arr_mon_lacroix_sd <- apply(arr_mon_lacroix, 2, sd, na.rm=T)
    ncvar_mon_lacroix <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_ymonmean"), units=data_info$units, dim=list(modeldim, mondim), missval=NA)
    ncvar_mon_lacroix_min <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_ymonmean_min"), units=data_info$units, dim=mondim, missval=NA)
    ncvar_mon_lacroix_max <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_ymonmean_max"), units=data_info$units, dim=mondim, missval=NA)
    ncvar_mon_lacroix_mean <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_ymonmean_mean"), units=data_info$units, dim=mondim, missval=NA)
    ncvar_mon_lacroix_median <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_ymonmean_median"), units=data_info$units, dim=mondim, missval=NA)
    ncvar_mon_lacroix_sd <- ncdf4::ncvar_def(name=paste0(zname, "_rfa_ymonmean_sd"), units=data_info$units, dim=mondim, missval=NA)
}
# output
vars <- list(ncvar, ncvar_min, ncvar_max, ncvar_mean, ncvar_median, ncvar_sd,
             ncvar_an, ncvar_an_min, ncvar_an_max, ncvar_an_mean, ncvar_an_median, ncvar_an_sd,
             ncvar_mon, ncvar_mon_min, ncvar_mon_max, ncvar_mon_mean, ncvar_mon_median, ncvar_mon_sd)
if (!is.null(lacroix)) {
    vars <- c(vars, 
              list(ncvar_lacroix, ncvar_lacroix_min, ncvar_lacroix_max, ncvar_lacroix_mean, ncvar_lacroix_median, ncvar_lacroix_sd,
                   ncvar_an_lacroix, ncvar_an_lacroix_min, ncvar_an_lacroix_max, ncvar_an_lacroix_mean, ncvar_an_lacroix_median, ncvar_an_lacroix_sd,
                   ncvar_mon_lacroix, ncvar_mon_lacroix_min, ncvar_mon_lacroix_max, ncvar_mon_lacroix_mean, ncvar_mon_lacroix_median, ncvar_mon_lacroix_sd))
}
message("create fout ", fout, " ...")
outnc <- ncdf4::nc_create(fout, vars=vars, force_v4=T)
ncdf4::ncvar_put(outnc, ncvar, arr)
ncdf4::ncvar_put(outnc, ncvar_min, arr_min)
ncdf4::ncvar_put(outnc, ncvar_max, arr_max)
ncdf4::ncvar_put(outnc, ncvar_mean, arr_mean)
ncdf4::ncvar_put(outnc, ncvar_median, arr_median)
ncdf4::ncvar_put(outnc, ncvar_sd, arr_sd)
ncdf4::ncvar_put(outnc, ncvar_an, arr_an)
ncdf4::ncvar_put(outnc, ncvar_an_min, arr_an_min)
ncdf4::ncvar_put(outnc, ncvar_an_max, arr_an_max)
ncdf4::ncvar_put(outnc, ncvar_an_mean, arr_an_mean)
ncdf4::ncvar_put(outnc, ncvar_an_median, arr_an_median)
ncdf4::ncvar_put(outnc, ncvar_an_sd, arr_an_sd)
ncdf4::ncvar_put(outnc, ncvar_mon, arr_mon)
ncdf4::ncvar_put(outnc, ncvar_mon_min, arr_mon_min)
ncdf4::ncvar_put(outnc, ncvar_mon_max, arr_mon_max)
ncdf4::ncvar_put(outnc, ncvar_mon_mean, arr_mon_mean)
ncdf4::ncvar_put(outnc, ncvar_mon_median, arr_mon_median)
ncdf4::ncvar_put(outnc, ncvar_mon_sd, arr_mon_sd)
if (!is.null(lacroix)) {
    ncdf4::ncvar_put(outnc, ncvar_lacroix, arr_lacroix)
    ncdf4::ncvar_put(outnc, ncvar_lacroix_min, arr_lacroix_min)
    ncdf4::ncvar_put(outnc, ncvar_lacroix_max, arr_lacroix_max)
    ncdf4::ncvar_put(outnc, ncvar_lacroix_mean, arr_lacroix_mean)
    ncdf4::ncvar_put(outnc, ncvar_lacroix_median, arr_lacroix_median)
    ncdf4::ncvar_put(outnc, ncvar_lacroix_sd, arr_lacroix_sd)
    ncdf4::ncvar_put(outnc, ncvar_an_lacroix, arr_an_lacroix)
    ncdf4::ncvar_put(outnc, ncvar_an_lacroix_min, arr_an_lacroix_min)
    ncdf4::ncvar_put(outnc, ncvar_an_lacroix_max, arr_an_lacroix_max)
    ncdf4::ncvar_put(outnc, ncvar_an_lacroix_mean, arr_an_lacroix_mean)
    ncdf4::ncvar_put(outnc, ncvar_an_lacroix_median, arr_an_lacroix_median)
    ncdf4::ncvar_put(outnc, ncvar_an_lacroix_sd, arr_an_lacroix_sd)
    ncdf4::ncvar_put(outnc, ncvar_mon_lacroix, arr_mon_lacroix)
    ncdf4::ncvar_put(outnc, ncvar_mon_lacroix_min, arr_mon_lacroix_min)
    ncdf4::ncvar_put(outnc, ncvar_mon_lacroix_max, arr_mon_lacroix_max)
    ncdf4::ncvar_put(outnc, ncvar_mon_lacroix_mean, arr_mon_lacroix_mean)
    ncdf4::ncvar_put(outnc, ncvar_mon_lacroix_median, arr_mon_lacroix_median)
    ncdf4::ncvar_put(outnc, ncvar_mon_lacroix_sd, arr_mon_lacroix_sd)
}
for (i in seq_along(z)) {
    ncdf4::ncatt_put(outnc, "model", i, names_short[i])
}
ncdf4::nc_close(outnc)

message("************************* aggregate_plot_data.r end *************************")

