# helper functions for echam repo

# host options
get_host <- function() {
    message("******* get_host() *******")
    hostname <- system("hostname", intern=T)
    if (any(sapply(c("ollie", "prod-", "fat-"), grepl, hostname))) {
        machine_tag <- "ollie"
        homepath <- "~/scripts/r"
        workpath <- "/work/ollie/cdanek"
    } else if (any(sapply(c("mlogin", "mistralpp"), grepl, hostname))) {
        machine_tag <- "mistral"
        homepath <- "~/scripts/r"
        #workpath <- "/work/ba0941/a270073"
        workpath <- "/work/ab0246/a270073"
    } else if (any(sapply(c("paleosrv1", "fu-"), grepl, hostname))) {
        machine_tag <- "paleosrv"
        homepath <- "~/scripts/r"
        workpath <- "/isibhv/projects/paleo_work/cdanek"
    } else if (any(sapply("stan", grepl, hostname))) {
        machine_tag <- "stan"
        homepath <- "~/scripts/r"
        workpath <- "/ace/user/cdanek"
    } else {
        machine_tag <- "unknown"
        homepath <- "~/scripts/r"
        workpath <- homepath
    }
    message("hostname    = \"", hostname, "\"\n",
            "machine_tag = \"", machine_tag, "\"\n",
            "homepath    = \"", homepath, "\"\n",
            "workpath    = \"", workpath, "\"\n",
            "******* get_host() ******")
    return(host=list(hostname=hostname, hostname_f=system("hostname -f", intern=T), 
                     machine_tag=machine_tag, homepath=homepath, workpath=workpath))
} # get_host()


# get file format
cdo_get_filetype <- function(fin, cdo="cdo", verbose=T) {

    cmd <- paste0(cdo, " showformat ", fin)
    if (verbose) message("run `", cmd, "`")
    input_format <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))
    if (!is.null(input_format$warning)) {
        stop(input_format$warning)
    } else {
        if (verbose) message("--> \"", input_format$value, "\" --> ", appendLF=F)
    }
    if (any(input_format$value == c("GRIB", "EXTRA  BIGENDIAN", "EXTRA  LITTLEENDIAN"))) {
        if (verbose) message("convert to netcdf ...")
        convert_to_nc <- T
        file_type <- "grb"
    } else if (any(input_format$value == c("netCDF", "NetCDF", "NetCDF2", "NetCDF4 classic zip"))) {
        if (verbose) message("no need to convert to netcdf ...")
        convert_to_nc <- F
        file_type <- "nc"
    } else {
        if (verbose) message("not defined in helper_functions.r:cdo_get_filetype() -> assume that conversion to nc is not needed -> set `convert_to_nc` to F and continue ...")
        convert_to_nc <- F
        file_type <- input_format$value
    }

    return(list(convert_to_nc=convert_to_nc, file_type=file_type))

} # cdo_get_filetype

