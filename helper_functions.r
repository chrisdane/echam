# r

# headtail
ht <- function(d, n=7) { 
    print(head(d, n))
    message(system("printf \"   ⋮\"", intern=T))
    print(tail(d, n))
}

# catch errors
tryCatch.W.E <- function(expr) {

    # from `demo(error.catching)`
    W <- NULL
    w.handler <- function(w) { # warning handler
        W <<- w
        invokeRestart("muffleWarning")
    }
    list(value=withCallingHandlers(tryCatch(expr, error=function(e) e),
                                   warning=w.handler), 
         warning=W)

} # tryCatch.W.E

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


# leap year
is.leap <- function(years) {
    return(((years %% 4 == 0) & (years %% 100 != 0)) | (years %% 400 == 0))
}

# make POSIX time with negative years
# todo: make_posixlt_origin_function from myfunctions.r

