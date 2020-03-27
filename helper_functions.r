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
    if (verbose) message("\nrun `", cmd, "`")
    input_format <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))
    if (!is.null(input_format$warning)) {
        stop(input_format$warning)
    } else {
        if (verbose) message(" --> \"", input_format$value, "\"", appendLF=F)
    }
    if (any(input_format$value == c("GRIB", "EXTRA  BIGENDIAN", "EXTRA  LITTLEENDIAN"))) {
        if (verbose) message(" --> convert to netcdf ...")
        convert_to_nc <- T
        file_type <- "grb"
    } else if (input_format$value == "netCDF") {
        if (verbose) message(" --> no need to convert to netcdf ...")
        convert_to_nc <- F
        file_type <- "nc"
    } else {
        if (verbose) message(" --> not defined. set `convert_to_nc` to F and continue ...")
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
make_posixlt_origin_function <- function(years, origin_in=0, origin_out=0, verbose=0) {

    # input: 
    #   years (numeric; negative for before `origin_in`)
    #   origin_in (numeric; origin of input years; default: 0 BC/AD)
    #   origin_out (numeric; origin of output years; default: 0 BC/AD)
    # output:
    #   dates (POSIXlt; date values with respect to `origin_out`)

    if (verbose > 0) {
        message("years:")
        print(head(years))
        print(tail(years))
        message("origin_in = ", origin_in, "\n",
                "origin_out = ", origin_out)
    }

    # if any positive years since origin_in
    pos_posixlt <- NULL # default
    pos_years <- which(years >= 0)
    if (length(pos_years) > 0) {
        
        # as.POSIXlt("9999-01-01") -> ok
        below10k_pos_posixlt <- NULL # default
        below10k_pos_years <- which(years[pos_years] < 9999)
        if (length(below10k_pos_years) > 0) {
            below10k_pos_posixlt <- as.POSIXlt(paste0(years[pos_years][below10k_pos_years], "-06-30"), tz="UTC") 
            if (verbose > 0) message("class(below10k_pos_posixlt) = ", class(below10k_pos_posixlt))
        }

        # as.POSIXlt("10000-01-01") -> error "not in a standard unambiguous format"
        over10k_pos_posixlt <- NULL # default
        over10k_pos_years <- which(years[pos_years] > 9999)
        if (length(over10k_pos_years) > 0) {
            over10k_pos_posixlt <- as.POSIXlt(paste0("1337-06-30"), tz="UTC") # vector for saving results
            for (yeari in seq_along(over10k_pos_years)) {
                by <- paste0(years[over10k_pos_years][yeari], " years")
                tmp <- seq.POSIXt(from=as.POSIXlt("0000-06-30", tz="UTC"), l=2, b=by, tz="UTC")[2]
                over10k_pos_posixlt[yeari] <- as.POSIXlt(tmp)
                if (verbose > 1) message("year ", years[over10k_pos_years][yeari], ": from 0 by ", by, " --> ", 
                                     over10k_pos_posixlt[yeari], " since ", origin_in)
            } # for yeari
            if (verbose > 0) message("class(over10k_pos_posixlt) = ", class(over10k_pos_posixlt))
        }

        # combine posivitve years below and above 10k
        if (!is.null(below10k_pos_posixlt) && !is.null(over10k_pos_posixlt)) {
            pos_posixlt <- c(below10k_pos_posixlt, over10k_pos_posixlt)
        } else if (!is.null(below10k_pos_posixlt) && is.null(over10k_pos_posixlt)) {
            pos_posixlt <- below10k_pos_posixlt
        } else if (is.null(below10k_pos_posixlt) && !is.null(over10k_pos_posixlt)) {
            pos_posixlt <- over10k_pos_posixlt
        } 
        if (verbose > 0) message("class(pos_posixlt) = ", class(pos_posixlt))

    } # if any positive years since origin_in
    
    # if any negative years since origin_in
    neg_posixlt <- NULL # default
    neg_years <- which(years < 0)
    if (length(neg_years) > 0) {

        # as.POSIXlt("-0001-01-01") --> negative year gives error "not in a standard unambiguous format" 
        # --> but with seq.POSIXt it works
        neg_posixlt <- as.POSIXlt(paste0("1337-06-30"), tz="UTC") # vector for saving results
        for (yeari in seq_along(years[neg_years])) {
            by <- paste0(years[neg_years][yeari], " years")
            tmp <- seq.POSIXt(from=as.POSIXlt("0000-06-30", tz="UTC"), l=2, b=by, tz="UTC")[2]
            neg_posixlt[yeari] <- as.POSIXlt(tmp)
            if (verbose > 1) message("year ", years[neg_years][yeari], ": from 0 by ", by, " --> ", 
                                 neg_posixlt[yeari], " since ", origin_in)
        } # for yeari
        if (verbose > 0) message("class(neg_posixlt) = ", class(neg_posixlt))

    } # if any negative years since origin_in

    # combine negative and positive posix dates
    if (!is.null(neg_posixlt) && !is.null(pos_posixlt)) {
        posixlt <- c(neg_posixlt, pos_posixlt)
    } else if (!is.null(neg_posixlt) && is.null(pos_posixlt)) {
        posixlt <- neg_posixlt
    } else if (is.null(neg_posixlt) && !is.null(pos_posixlt)) {
        posixlt <- pos_posixlt
    } else {
        stop("this should not happen")
    }
    if (verbose > 0) message("3 class(posixlt) = ", class(posixlt))
        
    # shift to new origin
    if (origin_out != origin_in) {
        
        shift_by <- origin_in - origin_out
        if (verbose > 0) message("shift\n   ", min(posixlt), " to ", max(posixlt), " since ", origin_in, "\n", 
                             "by ", shift_by, " years:")
        posixlt$year <- posixlt$year + shift_by
        if (verbose > 0) {
            message("   ", min(posixlt), " to ", max(posixlt), " since ", origin_out)
            message("4 class(posixlt) = ", class(posixlt))
        }

    } # if origin_out != origin_in or not

    # append origin
    posixlt$origin <- origin_out
    if (verbose > 0) message("5 class(posixlt) = ", class(posixlt))
    
    # fix time zone
    posixlt$zone <- rep("UTC", t=length(posixlt))
    if (verbose > 0) message("6 class(posixlt) = ", class(posixlt))

    return(posixlt)

} # make_posixlt_origin_function




