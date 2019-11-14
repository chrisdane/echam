##
rm(list=ls()); graphics.off()

## Host options
machine <- system("hostname -f", intern=T)
message(paste0("Run on ", machine, ":"))
if (regexpr("ollie", machine) != -1 ||
    regexpr("prod-", machine) != -1 ||
    regexpr("fat-", machine) != -1) {
    machine_tag <- "ollie"
    homepath <- "~/scripts/r"
    workpath <- "/work/ollie/cdanek"
} else if (regexpr("hpc.dkrz", machine) != -1) {
    machine <- substr(machine, 1, regexpr(".hpc.dkrz", machine) - 1)
    machine_tag <- "mistral"
    homepath <- "~/scripts/r"
    #workpath <- "/work/ba0941/a270073"
    workpath <- "/work/ab0246/a270073"
} else {
    message(paste0("   (unknown machine, use default paths)"))
    homepath <- "~/scripts/r"
    workpath <- homepath
}
message(paste0("   homepath = ", homepath))
message(paste0("   workpath = ", workpath))

## load packages if not interactive
if (!interactive()) {
    ht <- function(d, n=7) {
        print(head(d, n))
        message(system("bold=`tput bold`; printf \"   ${bold}⋮\"", intern=T))
        print(tail(d, n))
    }
}

## user input

# ======================================================
# 2 settings
if (T) {
    datapaths <- c("/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/hist/outdata/echam",
                   "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/hist_LUH/outdata/echam")
    fpatterns <- c(#"hist5_echam6_echammon_<YYYY>.grb",
                   "hist_echam6_echammon_<YYYY><MM>.nc",
                   "hist_LUH_echam6_echammon_<YYYY><MM>.grb")
    fvarnames <- c("temp2", "temp2")
    codes <- c(NA, 167) # necessary for grb files
    models <- c("echam6", "echam6") # for check if `cdo -t echam6` can be used
    froms <- c(1850, 1850)
    tos <- c(1851, 1851)
    #modes <- c("fldmean", "fldmean")
    modes <- c("timmean", "timmean")
    names_out <- c("hist5", "hist_LUH") 
    postpaths <- paste(workpath, "post", models, modes, fvarnames, sep="/")
}

verbose <- 1 # 0,1
clean <- T # remove tmp files
cdo_silent <- "" # "-s" for silent or ""
cdo_force <- F # redo cdo command although outout file already exists
set_rel_time <- T # conversion from absolute (default) to relative time
add_my_time <- F # my time for ts output (needs package ncdf4) 

# ======================================================

# Check user input and set defaults
exist_checks <- c("datapaths", "fpatterns", "fvarnames",
                  "models", "froms", "tos", "modes",
                  "names_out", "postpaths")
if (!all(sapply(exist_checks, exists))) {
    missing_vars <- sapply(exist_checks, exists)
    stop("you have to define the variable", 
         ifelse(length(which(missing_vars)) > 1, "s", ""),
         " '", paste0(names(missing_vars)[missing_vars], collapse="', '"), "'.")
}
if (any(file.access(datapaths, mode=4) != 0)) { # check read permission
    nonreadable_paths <- which(file.access(datapaths, mode=2) != 0)
    stop("have not read permission in datapath", ifelse(length(nonreadable_paths) > 1, "s", ""),
         " '", paste0(datapaths[nonreadable_paths], collapse="', '"), "'.")
}
datapaths <- normalizePath(datapaths)
if (any(file.access(postpaths, mode=0) != 0)) { # check existance
    nonexisting_paths <- which(file.access(postpaths, mode=0) != 0)
    for (i in postpaths[nonexisting_paths]) {
        permission_check <- tryCatch(dir.create(i, recursive=T), error=function(e) e, warning=function(w) w)
        if (typeof(permission_check) == "logical" || # dir creation permission
            grepl("already exists", permission_check$message)) { # just warning that directory already exists
            message("create postpath '", i, "' ...")
            dir.create(i, recursive=T, showWarnings=F)
        } else { # no dir creation permission
            stop("have no write permission to create postpath '", i, "'. error message:\n",
                 permission_check)
        }
    }
}
postpaths <- normalizePath(postpaths)
nsettings <- length(names_out)
if (!exists("froms_shift")) froms_shift <- rep(NA, t=nsettings)
if (any(!is.na(froms_shift)) && add_my_time == F) {
    stop("some of \"froms_shift\" is not NA but \"add_my_time\" is False")
}
if (!exists("codes")) codes <- rep(NA, t=nsettings)
if (!exists("areas_out")) areas_out <- rep("global", t=nsettings)
if (!exists("seasons_out")) seasons_out <- c("Jan-Dec", "Jan-Dec")
if (!exists("levs_out")) levs_out <- rep(NA, t=nsettings)
lev_fnames <- levs_out
lev_fnames[which(!is.na(levs_out))] <- paste0("_", levs_out[which(!is.na(levs_out))], "m")

pid <- Sys.getpid()

for (i in 1:nsettings) {

    message("\n", "*********** setting ", i, "/", nsettings, " *************")
    message("datapath = ", datapaths[i])
    message("fpattern = ", fpatterns[i])
    message("fvarname = ", fvarnames[i])
    if (!is.na(codes[i])) message("code = ", codes[i])
    message("model = ", models[i])
    message("from = ", froms[i])
    message("to = ", tos[i])
    message("season_out = ", seasons_out[i])
    if (!is.na(froms_shift[i])) message("from_shift = ", froms_shift[i])
    message("area_out = ", areas_out[i])
    if (!is.na(levs_out[i])) message("lev_out = ", levs_out[i])
    message("postpath = ", postpaths[i])

    ## files of complete input time period
    # e.g.
    # hist_echam6_echam_185001.nc
    # PI-CTRL4_echam6_g3bid_299901.grb 
    fpattern <- fpatterns[i]
    # replace <YYYY> if existing
    if (grepl("<YYYY>", fpattern)) {
        fpattern <- gsub("<YYYY>", "*", fpattern)
    }
    # replace <MM> if existing
    if (grepl("<MM>", fpattern)) {
        fpattern <- gsub("<MM>", "*", fpattern)
    }

    cmd <- paste0("ls ", datapaths[i], "/", fpattern)
    message("\n", "files=`", cmd, "`")
    files <- system(cmd, intern=T)
    if (length(files) == 0) stop("no files found")

    # separate into dirname and basename
    dirnames <- dirname(files)
    basenames <- basename(files)
    df <- data.frame(basenames)

    # check file types
    # lazy approach: take everything after last . (dot) in file name
    filetype <- substr(basenames, regexpr("\\.[^\\.]*$", basenames) + 1, nchar(basenames))
    if (length(unique(filetype)) > 1) stop("this is not implemented")
    filetype <- unique(filetype)
    if (!any(filetype == c("grb", "nc"))) {
        stop("filetype '", filetype, "' not implemented yet")
    }

    # all parts from file name without YYYY, MM, etc. and ending
    prefix <- gsub("\\*", "", fpattern)
    prefix <- gsub(paste0(".", filetype), "", prefix)

    # identify correct YYYY, MM, etc. in found files
    if (grepl("<YYYY>", fpatterns[i])) {
        patterninds <- regexpr("<YYYY>", fpatterns[i])
        patterninds <- c(patterninds, 
                         patterninds + attributes(patterninds)$match.length - 3) 
        df$YYYY <- substr(basenames, patterninds[1], patterninds[2]) 
        years_in <- as.integer(df$YYYY)
    } else {
        stop("<YYYY> should be included in `fpatterns[", i, "]`=", fpatterns[i], " !?")
    }
    if (grepl("<MM>", fpatterns[i])) {
        patterninds <- regexpr("<MM>", fpatterns[i])
        patterninds <- c(patterninds - 2,
                         patterninds - 2 + attributes(patterninds)$match.length - 3) 
        MM_in <- substr(basenames, patterninds[1], patterninds[2]) 
        df$MM <- MM_in
    }
    
    if (verbose > 0) {
        message("\n", "found ", length(files), " ", filetype, " file", 
                ifelse(length(files) > 1, "s", ""), ". files:")
        if (length(files) > 1) {
            ht(df)
        } else {
            print(df)
        }
    } 

    # wanted years
    from <- as.POSIXlt(paste0(froms[i], "-01-01"), tz="UTC")
    to <- as.POSIXlt(paste0(tos[i], "-12-31"), tz="UTC")
    years_wanted <- (unclass(from)$year+1900):(unclass(to)$year+1900)

    # check if some wanted years are out of found years
    if (any(years_wanted %in% years_in == F)) {
        stop("wanted year", 
             ifelse(length(which(!(years_wanted %in% years_in))) > 1, "s", ""),
             " ", paste0(years_wanted[!(years_wanted %in% years_in)], collapse=","),
             " ", ifelse(length(which(!(years_wanted %in% years_in))) > 1, "are", "is"),
             " not included in found year", 
             ifelse(length(years_in) > 1, "s", ""), " ", 
             paste0(range(years_in), collapse="-"), ".")
    }

    # remove found years out of wanted years
    outside_years_inds <- which(years_in %in% years_wanted == F)
    if (length(outside_years_inds) > 0) {
        message("\n", "remove ", length(outside_years_inds), " file",
                ifelse(length(outside_years_inds) > 1, "s", ""),
                " outside of ", paste0(range(years_wanted), collapse="-"), " ...")
        files <- files[-outside_years_inds]
        dirnames <- dirnames[-outside_years_inds]
        basenames <- basenames[-outside_years_inds]
        df <- df[-outside_years_inds,]
        if (verbose > 0) {
            message("\n", "found ", length(files), " ", filetype, " file", 
                    ifelse(length(files) > 1, "s", ""), ". files:")
            if (length(files) > 1) {
                ht(df)
            } else {
                print(df)
            }
        } 
    }

    # apply season
    if (seasons_out[i] != "Jan-Dec") {
        stop("not yet")
    }

    # select variable of every file (with grb, only `cdo selcode` works, `cdo selvar` not)
    message("\n", "Select variable ", fvarnames[i], " ...")
    nf <- length(files)
    fsels <- data.frame(basenames=rep(NA, t=nf))
    for (fi in 1:nf) {

        # select area
        if (areas_out[i] != "global") {
            stop("not yettt")
        }

        # select level
        if (!is.na(levs_out[i])) {
            stop("not yet")
            #cmd <- paste0(cmd, " -sellevel,", levs_out[i])
        }
        
        if (filetype == "grb") {
            if (is.na(codes[i])) { # code not provided
                stop("cannot process setting ", i, "/", nsettings, 
                     ": provide a code number of the wanted variable '", fvarnames[i], 
                     "'.")

            } else { # code provided
                fsels$basenames[fi] <- paste0(gsub(paste0(".", filetype), "", df$basenames[fi]), 
                                              "_selcode_", codes[i], "_selvar_", fvarnames[i], ".", filetype)
                cmd <- paste0("cdo ", cdo_silent, " selcode,", codes[i], " ",
                              files[fi], " ", postpaths[i], "/", fsels$basenames[fi]) 
            }
        
        } else if (filetype == "nc") {
            fsels$basenames[fi] <- paste0(gsub(paste0(".", filetype), "", df$basenames[fi]), 
                                          "_selvar_", fvarnames[i], ".", filetype)
            cmd <- paste0("cdo ", cdo_silent, " selvar,", fvarnames[i], " ", 
                          files[fi], " ", postpaths[i], "/", fsels$basenames[fi]) 
        }
        
        # skip variable selection if selcode/selvar file already exists
        stamp <- df$YYYY[fi]
        stamp2 <- paste0("/", df$YYYY[nf])
        if (grepl("<MM>", fpatterns[i])) {
            stamp <- paste0(stamp, df$MM[fi])
            stamp2 <- paste0(stamp2, df$MM[nf])
        }
        stamp <- paste0(stamp, stamp2, " ") 
        if (fi == 1) message(cmd)
        cat("\r", stamp)
        if (file.access(paste0(postpaths[i], "/", fsels$basenames[fi]), mode=0) == 0 && cdo_force == F) {
            if (fi == 1) message("fout already exists, skip (set `cdo_force=T` if you want to redo this cdo command).")
        } else {
            system(cmd)
        }
        if (fi == nf) message()
    
    } # for fi nf

    if (verbose > 0) {
        message("\n", "selected ", fvarnames[i], " from ", length(fsels$basenames), 
                " ", filetype, " file", ifelse(length(fsels$basenames) > 1, "s", ""), ". fsels:")
        if (length(fsels$basenames) > 1) {
            ht(fsels)
        } else {
            print(fsels)
        }
    }

    # calculations per file
    message("\n", "Calculate ", modes[i], " ...")
    fcalcs <- data.frame(basenames=rep(NA, t=nf))
    for (fi in 1:nf) {

        stamp <- df$YYYY[fi]
        stamp2 <- paste0("/", df$YYYY[nf])
        if (grepl("<MM>", fpatterns[i])) {
            stamp <- paste0(stamp, df$MM[fi])
            stamp2 <- paste0(stamp2, df$MM[nf])
        }
        stamp <- paste0(stamp, stamp2, " ") 
        
        # convert to nc first if grb
        if (filetype == "grb") {
            cmd <- paste0("cdo ", cdo_silent)
            if (models[i] == "echam6") {
                cmd <- paste0(cmd, " -t echam6")
            }
            cmd <- paste0(cmd, " -f nc copy ", postpaths[i], "/", fsels$basenames[fi], " ",  
                          postpaths[i], "/", fsels$basenames[fi], ".nc")
            if (fi == 1) message(cmd)
            cat("\r", stamp)
            if (file.access(paste0(postpaths[i], "/", fcalcs$basenames[fi], ".nc"), mode=0) == 0 && cdo_force == F) {
                if (fi == 1) message("fout already exists, skip (set `cdo_force=T` if you want to redo this cdo command).")
            } else {
                system(cmd)
            }
            if (clean) {
                if (fi == 1) message("Remove grb files (set `clean=F` if these files should not be deleted) ...")
                check <- file.remove(paste0(postpaths[i], "/", fsels$basenames[fi]))
                if (!check) stop("something went wrong")
            }
            # overwrite fsels
            fsels$basenames[fi] <- paste0(fsels$basenames[fi], ".nc")
        }

        # output file of calculation
        fcalcs$basenames[fi] <- paste0(fsels$basenames[fi])
        # append mode
        fcalcs$basenames[fi] <- gsub(paste0("_selvar_", fvarnames[i]), 
                                     paste0("_selvar_", fvarnames[i], "_", modes[i]),
                                     fcalcs$basenames[fi])
        
        # apply calculation mode
        cmd <- paste0("cdo ", cdo_silent)
        if (modes[i] == "timmean") {
            cmd <- paste0(cmd, " -timmean")
        } else if (modes[i] == "fldmean") {
            cmd <- paste0(cmd, " -fldmean")
        } else {
            stop("mode '", modes[i], "' not defined.")
        }
        cmd <- paste0(cmd, " ", postpaths[i], "/", fsels$basenames[fi], " ",
                      postpaths[i], "/", fcalcs$basenames[fi])
        
        if (fi == 1) message(cmd)
        cat("\r", stamp)
        if (file.access(paste0(postpaths[i], "/", fcalcs$basenames[fi]), mode=0) == 0 && cdo_force == F) {
            if (fi == 1) message("fout already exists, skip (set `cdo_force=T` if you want to redo this cdo command).")
        } else {
            system(cmd)
        }
        if (fi == nf) message()

    } # for fi nf
    
    if (verbose > 0) {
        message("\n", "calculated ", fvarnames[i], " ", modes[i], " from ", length(fcalcs$basenames), 
                " file", ifelse(length(fcalcs$basenames) > 1, "s", ""), ". fcalcs:")
        if (length(fcalcs$basenames) > 1) {
            ht(fcalcs)
        } else {
            print(fcalcs)
        }
    }
       
    # remove fsels
    if (clean) {
        message("\n", "Remove fsels (set `clean=F` if these files should not be deleted) ...")
        check <- file.remove(paste0(postpaths[i], "/", fsels$basenames))
        if (!all(check)) stop("something went wrong")
    }

    # if wanted, for "mean_ts" mode, set own time:
    # ncap2 -s time=time-0.9944444
    # at the end of the script the command string is too long for ncap2:
    # error: /sw/rhel6-x64/nco/nco-4.7.5-gcc64/bin/ncap2: Argument list too long
    # --> do it for every file
    if (add_my_time) {
      
        stop("update")

        if (interactive()) {
            message("load 'ncdf4' package ...")
            library(ncdf4)
        }

        for (fi in 1:nf) {

            if (modes[i] == "fldmean") {
            
                #message("set own time ...")
                # convert any unit to seconds for POSIX, e.g. 
                # "days since 1538-1-1 00:00:00"
                # "day as %Y%m%d.%f"
                
                ncin <- nc_open(fout_prep[fi,k])
                timein <- ncin$dim$time$vals
                timein_units <- ncin$dim$time$units 
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
                    timein_lt <- as.POSIXlt(timein*timein_fac, origin=timein_origin, tz="UTC")
                
                # case 2: e.g. "day as %Y%m%d.%f" (e.g. 18510131.9944444)
                # `cdo -f nc copy` makes this absolute time axis 
                } else if (regexpr(" as ", timein_units) != -1) { 
                    timein_unit <- substr(timein_units, 1, regexpr(" as ", timein_units) - 1)
                    timein_format <- substr(timein_units, regexpr(" as ", timein_units) + 4, nchar(timein_units))
                    if (timein_format == "%Y%m%d.%f") { # e.g. "29991201.9944444"
                        hours <- 24*(timein - floor(timein))
                        mins <- 60*(hours - floor(hours))
                        secs <- 60*(mins - floor(mins))
                        hours <- floor(hours)
                        mins <- floor(mins)
                        secs <- floor(secs)
                        timein_lt <- as.POSIXlt(paste0(substr(timein, 1, 4), "-", 
                                                       substr(timein, 5, 6), "-",
                                                       substr(timein, 7, 8), " ",
                                                       hours, ":", mins, ":", secs), tz="UTC")
                    } else {
                        stop("timein_format=", timein_format, " not defined")
                    }
                }

                # smallest output interval so far is on the order of hours (3hr)
                # as.POSIXlt's 'year' starts at 1900
                # as.POSIXlt's 'yday' and 'mon' start at 0
                if (fi == 1) source("functions/leap_function.r")
                year <- 1900 + timein_lt$year
                yday_frac <- (timein_lt$yday + 1)/ifelse(is.leap(year), 366, 365)
                hours <- timein_lt$hour
                hours[hours == 0] <- 24
                hday_frac <- hours/24
                timeout <- year + hday_frac*yday_frac
                
                # check time 
                df <- data.frame(timein=timein,
                                 timein_lt=timein_lt,
                                 timeout=timeout)
                # change possibly senseless spinup years to fromsp
                if (!is.na(froms_shift[i]) && froms[i] != froms_shift[i]) {
                    timeout2 <- rep(froms_shift[i], t=length(year)) + timeout - floor(timeout)
                    df <- data.frame(df, timeout2=timeout2)
                }
                op <- getOption("digits")
                options(digits=12)
                message("timein_units = ", appendLF=F)
                print(timein_units)
                print(df)
                options(digits=op) # back to default
                #stop("asd")

                # overwrite old time with new time (-O without asking !!!)
                # ncap2: WARNING assign(): Var being read and written in ASSIGN tim 
                # can be ignored
                #ncap2 -s 'defdim("time",1);time[time]=74875.0;time@long_name="Time"; etc.etc.etc.' -O ~/nco/data/in.nc ~/foo.nc
                #cmd <- paste0("ncap2 -O -s 'time(:)={", paste0(sprintf("%f", timeout), collapse=","), "}' ",
                cmd <- paste0("ncap2 -O -s 'defdim(\"time2\",", length(timeout), "); time2[time2]={", 
                              paste0(sprintf("%f", timeout), collapse=","), 
                              "}; time2@long_name=\"Time in fraction of a year start from ", timeout[1], "\"' ",
                              fout_prep[fi,k], " ", fout_prep[fi,k])
                message("\n", cmd)
                system(cmd)

                if (!is.na(froms_shift[i]) && froms[i] != froms_shift[i]) {
                    cmd <- paste0("ncap2 -O -s 'defdim(\"time3\",", length(timeout2), "); time3[time3]={", 
                                  paste0(sprintf("%f", timeout2), collapse=","), 
                                  "}; time3@long_name=\"Time in fraction of a year start from ", timeout2[1], "\"' ",
                                  fout_prep[fi,k], " ", fout_prep[fi,k])
                    message("\n", cmd)
                    system(cmd)
                }

                # make record dim for ncrcat
                cmd <- paste0("ncks -O -4 --mk_rec_dmn time ", fout_prep[fi,k], " ", fout_prep[fi,k])
                message("\n", cmd)
                system(cmd)
                cmd <- paste0("ncks -O -4 --mk_rec_dmn time2 ", fout_prep[fi,k], " ", fout_prep[fi,k])
                message("\n", cmd)
                system(cmd)
                if (!is.na(froms_shift[i]) && froms[i] != froms_shift[i]) {
                    cmd <- paste0("ncks -O -4 --mk_rec_dmn time3 ", fout_prep[fi,k], " ", fout_prep[fi,k])
                    message("\n", cmd)
                    system(cmd)
                }

            } # if modes "fldmean"
        } # for fi nf
    } # if add_my_time
        
    # Cat or ensmean depending on mode 
    fcat <- paste0(prefix, seasons_out[i], "_", froms[i], "-", tos[i])
    if (filetype == "grb") {
        fcat <- paste0(fcat, "_code", codes[i], "_", fvarnames[i])
    } else if (filetype == "nc") {
        fcat <- paste0(fcat, "_", fvarnames[i])
    }
    fcat <- paste0(fcat, "_", modes[i], "_", areas_out[i], 
                   ifelse(filetype != "nc", paste0(".", filetype), ""), ".nc") 
    if (file.access(paste0(postpaths[i], "/", fcat), mode=0) == 0 && cdo_force == F) {
        message("fout already exists, skip (set `cdo_force=T` if you want to redo this cdo command).")
    } else {
        # cat
        if (any(modes[i] == c("fldmean"))) {
            # cat files in time
            message("\n", "Cat ", length(fcalcs$basenames), " ", fvarnames[i], " ", modes[i], " files in time ...")
            cmd <- paste0("cdo ", cdo_silent, " cat <fcalcs> ", postpaths[i], "/", fcat)
            message(cmd)
            cmd <- gsub("<fcalcs>", paste0(paste0(postpaths[i], "/", fcalcs$basenames), collapse=" "), cmd)
            system(cmd)
        # ensmean
        } else if (any(modes[i] == c("timmean"))) {
                
            # check maximum number of input files for cdo ensmean:
            nmax <- as.integer(system("ulimit -n", intern=T))
            if (length(fcalcs$basenames) > nmax) {
                stop("cannot compute ", modes[i], " of ", length(fcalcs$basnames), 
                     " files because `cdo ensmean` maximum files is ", nmax)
            }
            message("\n", "Ensmean ", length(fcalcs$basenames), " ", fvarnames[i], " ", modes[i], " files in time ...")
            cmd <- paste0("cdo ", cdo_silent, " -O ensmean <fcalcs> ", postpaths[i], "/", fcat)
            message(cmd)
            cmd <- gsub("<fcalcs>", paste0(paste0(postpaths[i], "/", fcalcs$basenames), collapse=" "), cmd)
            system(cmd)
            
        } # depending on mode cat or ensmean
    } # if fcat already exists or not

    # set relative time axis (cdos default)
    if (set_rel_time) {
        message("\n", "Make relative time axis ...")
        cmd <- paste0("cdo ", cdo_silent, " -r copy ", postpaths[i], "/", fcat, 
                      " ", postpaths[i], "/", fcat)
        message(cmd)
        system(cmd)
    }
    
    # change from code to proper variable name
    #if (filetype == "grb") {
    #    stop("update")
    #    cmd <- paste0("cdo ", cdo_silent, " chname,var", codes[i], ",", varnames[i], " ", 
    #                  fout_prep[fi,k], " ", fout_prep[fi,k])
    #    message("\n", cmd)
    #    system(cmd)
    #}
    
    # remove fcalcs
    if (clean) {
        message("\n", "Remove fcalcs (set `clean=F` if these files should not be deleted) ...")
        check <- file.remove(paste0(postpaths[i], "/", fcalcs$basenames))
        if (!all(check)) stop("something went wrong")
    }

} # for i nsettings

message("\nfinished\n")


