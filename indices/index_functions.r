# r

north_pacific_index <- function(slp_select) {

} # north_pacific_index


siberian_high_index <- function(DJF_slp_fldmean_detrend, sd_thr=1.5, varname) {

    message("\nstart siberian_high_index()")
    if (missing(varname)) stop("provide varname to load from file")

    nc <- nc_open(DJF_slp_fldmean_detrend)
    varnames <- names(nc$var)
    if (!(varname %in% varnames)) {
        stop("provided varname ", varname, " not in varnames of nc file (",
             paste(varnames, collapse=", "))
    }
    
    # load data
    message("load ", varname, " (this data must be fldmean of detrended DJF slp data) ...")
    slp <- ncvar_get(nc, varname)
    
    # normalize data
    slp <- scale(slp)[,1]
    
    # find values below/above `sd_thr` std devs
    sd_plus_inds <- which(slp > sd_thr)
    if (length(sd_plus_inds) > 0) {
        sd_plus_file <- paste0(dirname(DJF_slp_fldmean_detrend), "/", 
                               tools::file_path_sans_ext(basename(DJF_slp_fldmean_detrend)), 
                               "_inds_gt_", sd_thr, "_sd.txt")
        message("save ", length(sd_plus_inds), "/", length(slp), " = ",
                round(length(sd_plus_inds)/length(slp)*100), "% timesteps > ",
                sd_thr, " std devs to ", sd_plus_file, " ...")
        write(paste(sd_plus_inds, collapse=","), file=sd_plus_file)
    } else {
        message("zero/", length(slp), " timesteps > ", sd_thr, " std devs.")
    }
    sd_minus_inds <- which(slp < -sd_thr)
    if (length(sd_minus_inds) > 0) {
        sd_minus_file <- paste0(dirname(DJF_slp_fldmean_detrend), "/",  
                               tools::file_path_sans_ext(basename(DJF_slp_fldmean_detrend)), 
                               "_inds_lt_", sd_thr, "_sd.txt")
        message("save ", length(sd_minus_inds), "/", length(slp), " = ",
                round(length(sd_minus_inds)/length(slp)*100), "% timesteps < -",
                sd_thr, " std devs to ", sd_minus_file, " ...")
        write(paste(sd_minus_inds, collapse=","), file=sd_minus_file)
    } else {
        message("zero/", length(slp), " timesteps < ", sd_thr, " std devs.")
    }
    
    message("finished siberian_high_index()")
    ret <- list(index_name="Siberian High Index", index_name_short="SHI",
                index_fname="siberian_high_index", f=f, 
                outname=tools::file_path_sans_ext(basename(DJF_slp_fldmean_detrend)),
                index=slp, 
                sd_thr=sd_thr, sd_plus_inds=sd_plus_inds, sd_minus_inds=sd_minus_inds)
    return(ret)

} # siberian_high_index


