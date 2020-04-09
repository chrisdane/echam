# r


# todo: how to load functions from another repo without the "subrepo" hazle?
if (file.exists("~/scripts/r/functions/myfunctions.r")) {
    message("source \"~/scripts/r/functions/myfunctions.r\" ...")
    source("~/scripts/r/functions/myfunctions.r")
    # needed: ht(), make_posixlt_origin_function(), is.leap() 
} else {
    stop("could not load \"~/scripts/r/functions/myfunctions.r\"")
}


# get file format
cdo_get_filetype <- function(fin, cdo="cdo", verbose=T) {

    cmd <- paste0(cdo, " showformat ", fin)
    if (verbose) message("run `", cmd, "`")
    input_format <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))
    if (!is.null(input_format$warning)) {
        stop(input_format$warning)
    } else {
        if (verbose) message("--> \"", input_format$value, "\"", appendLF=F)
    }
    if (any(input_format$value == c("GRIB", "EXTRA  BIGENDIAN", "EXTRA  LITTLEENDIAN"))) {
        if (verbose) message("--> convert to netcdf ...")
        convert_to_nc <- T
        file_type <- "grb"
    } else if (any(input_format$value == c("netCDF", "NetCDF2"))) {
        if (verbose) message("--> no need to convert to netcdf ...")
        convert_to_nc <- F
        file_type <- "nc"
    } else {
        if (verbose) message("--> not defined. set `convert_to_nc` to F and continue ...")
        convert_to_nc <- F
        file_type <- input_format$value
    }

    return(list(convert_to_nc=convert_to_nc, file_type=file_type))

} # cdo_get_filetype

