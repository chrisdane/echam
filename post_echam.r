## R

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
        message(system("printf \"   ⋮\"", intern=T))
        print(tail(d, n))
    }
}

# user input
fnml <- "namelist.post.r"
message("\n", "Read ", fnml, " ...")
source(fnml)

# Check user input and set defaults
message("\n", "Check user input ...")
exist_checks <- c("datapaths", "fpatterns", "fvarnames",
                  "models", "froms", "tos", "modes",
                  "postpaths")
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
        if (file.access(i, mode=0) == 0) {
            # postpath was created in a step before
            next # path
        }
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
nsettings <- length(datapaths)
if (!exists("suffixs")) suffixs <- rep(NA, t=nsettings)
if (!exists("froms_shift")) froms_shift <- rep(NA, t=nsettings)
if (any(!is.na(froms_shift)) && add_my_time == F) {
    stop("some of \"froms_shift\" is not NA but \"add_my_time\" is False")
}
if (!exists("codes")) codes <- rep(NA, t=nsettings)
if (!exists("areas_out")) areas_out <- rep("global", t=nsettings)
if (!exists("season_inds")) {
    season_inds <- vector("list", l=nsettings)
    for (i in 1:nsettings) season_inds[[i]] <- 1:12
}
season_names <- rep(NA, t=nsettings)
for (i in 1:nsettings) {
    if (length(season_inds[[i]]) == 12 && season_inds[[i]] == 1:12) { # default case: annual
        season_names[i] <- "Jan-Dec"
    } else { # all other: first letters of months
        season_names[i] <- paste(substr(month.abb[season_inds[[i]]], 1, 1), collapse="")
    }
}
if (!exists("levs_out")) levs_out <- rep(NA, t=nsettings)
lev_fnames <- levs_out
lev_fnames[which(!is.na(levs_out))] <- paste0("_", levs_out[which(!is.na(levs_out))], "m")

message("verbose = ", verbose)
message("clean = ", clean)
message("cdo_silent = \"", cdo_silent, "\"")
message("cdo_force = ", cdo_force) #  -O necessary for ens<STAT>, merge, mergetime
message("cdo_OpenMP_threads = \"", cdo_OpenMP_threads, "\"") # OMP supported operators: https://code.mpimet.mpg.de/projects/cdo/wiki/OpenMP_support
message("cdo_wout_loop = ", cdo_wout_loop)
message("set_rel_time = ", set_rel_time)
message("add_my_time = ", add_my_time)

elapsed <- vector("list", l=nsettings)
for (i in 1:nsettings) {

    tic <- Sys.time()
    message("\n", "*********** setting ", i, "/", nsettings, " *************")
    message("datapath = ", datapaths[i])
    message("fpattern = ", fpatterns[i])
    message("fvarname = ", fvarnames[i])
    if (!is.na(codes[i])) message("code = ", codes[i])
    message("model = ", models[i])
    message("from = ", froms[i])
    message("to = ", tos[i])
    message("season_name = ", season_names[i])
    message("season_inds = ", paste0(season_inds[[i]], collapse=","))
    if (!is.na(froms_shift[i])) message("from_shift = ", froms_shift[i])
    message("area_out = ", areas_out[i])
    if (!is.na(levs_out[i])) message("lev_out = ", levs_out[i])
    message("postpath = ", postpaths[i])
    message("mode = ", modes[i])

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
    # append "_" as last character
    if (substr(prefix, nchar(prefix), nchar(prefix)) != "_") {
        prefix <- paste0(prefix, "_")
    }

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

    # remove files out of wanted season
    cmdselmon <- "" # default
    if (season_names[i] != "Jan-Dec") {
        message("\n", "season_inds = ", paste(season_inds[[i]], collapse=","), 
                " -> season = ", season_names[i])
        if (grepl("<MM>", fpatterns[i])) {
            file_season_inds <- which(as.integer(df$MM) %in% season_inds[[i]])
            if (length(file_season_inds) == 0) { # no files in wanted season 
                stop("no files found at season_inds = ", paste(season_inds[[i]], collapse=","), 
                     " based on given <MM> pattern.")
            }
            message("remove ", length(file_season_inds), " files out of these months. files:")
            files <- files[file_season_inds]
            dirnames <- dirnames[file_season_inds]
            basenames <- basenames[file_season_inds]
            df <- df[file_season_inds,]
            ht(df)

        # end if <MM> is in fpatterns
        } else {
            cmd <- paste0("cdo ", cdo_silent, " showmon ", files[1])
            message("run \"", cmd, "\"")
            months_per_file <- system(cmd, intern=T)
            if (months_per_file != "") {
                months_per_file <- strsplit(months_per_file, "\\s+")[[1]]
                if (any(months_per_file == "")) months_per_file <- months_per_file[-which(months_per_file == "")]
            }
            if (length(months_per_file) == 1 && months_per_file == "") { # `cdo showmon` was not successfull
                stop("input files do not have proper time axis. not implemented yet")
            }
            message("determined months_per_file = ", paste(months_per_file, collapse=","), " (by `cdo showmon`)")
            selmon_season_inds <- which(months_per_file %in% season_inds[[i]])
            if (length(selmon_season_inds) == 0) { # no files in wanted season 
                stop("wanted season_inds = ", paste(season_inds[[i]], collapse=","), 
                     " not found in determined seasons ", paste(months_per_file, collapse=","), 
                     " (by `cdo showmon`) in files[1] = ", files[1], ".")
            }
            cmdselmon <- paste0("-selmon,", paste(months_per_file[selmon_season_inds], collapse=",")) 
            message("add `", cmdselmon, "` to cdo command ...")

        } # if <MM> is given in fpatterns or not
        

    } # if season_name != "Jan_Dec"

    ## construct cdo command (chained cdo commands will be executed from right to left)
    # prefix
    cmdprefix <- paste0("cdo ", cdo_silent, " ", cdo_OpenMP_threads)

    # n-th command: cat/mergetime/etc. command
    if (modes[i] == "timmean") {
        nmax <- as.integer(system("ulimit -n", intern=T))
        if (length(files) > nmax) {
            stop("cannot compute ", modes[i], " of ", length(files), 
                 " files because `cdo ensmean` maximum files is ", nmax)
        }
        cmdcat <- "-O ensmean"
    } else if (modes[i] == "fldmean") {
        if (F) { # could not figure out a significant time difference between cat and mergetime
            cmdcat <- "cat"
        } else if (T) {
            cmdcat <- "mergetime"
        }
    } else {
        #stop("cat/mergetime not defined for mode '", modes[i], "' not defined.")
    } # which cat/mergetime depending on mode

    # (n-1)-th command: convert to nc if grb
    cmdconvert <- "" # default
    if (filetype == "grb") {
        if (models[i] == "echam6") {
            cmdconvert <- paste0(cmdconvert, " -t echam6")
        }
        cmdconvert <- paste0(cmdconvert, " -f nc")
    } # if grb
    
    # (n-2)-th command: calculation
    if (modes[i] == "timmean") {
        cmdcalc <- "-timmean"
    } else if (modes[i] == "fldmean") {
        cmdcalc <- "-fldmean"
    } else if (modes[i] == "volint") {
        cmdcalc <- "-fldsum -vertsum"
    } else {
        stop("calculation for mode '", modes[i], "' not defined.")
    } # which calculation depending on mode

    # (n-3)-th command: select level
    if (!is.na(levs_out[i])) {
        cmdsellevel <- paste0("-sellevel,", paste0(levs_out[i], collapse=","))
        stop("not yet") 
    }
    
    # (n-4)-th command: select variable
    if (filetype == "grb") {
        if (is.na(codes[i])) { # code not provided
            stop("cannot process setting ", i, "/", nsettings, 
                 ": provide a code number of the wanted variable '", fvarnames[i], 
                 "'.")
        } else { # code provided
            cmdselect <- paste0("-select,code=", codes[i])
        }
    } else if (filetype == "nc") {
        cmdselect <- paste0("-select,name=", fvarnames[i])
    }
    
    
    # select area
    if (areas_out[i] != "global") {
        stop("not yettt")
    }

    ## run chained cdo operators
    # run only 1 cdo command for all files
    if (cdo_wout_loop == T) {
        message("\n", "`cdo_wout_loop=T` --> run only one cdo command with all files as input ...")
        fout <- paste0(postpaths[i],"/", prefix, suffixs[i],
                       "_", modes[i],
                       ifelse(filetype == "grb", paste0("_selcode_", codes[i]), ""),
                       "_selname_", fvarnames[i], 
                       ifelse(!is.na(levs_out[i]), paste0("_sellev_", levs_out[i]), ""),
                       "_", areas_out[i],
                       "_", season_names[i], "_", froms[i], "-", tos[i], 
                       ".nc")
        fout_exist_check <- file.access(fout, mode=0)
        if (fout_exist_check == 0 && cdo_force == F) { # running command not necessary
            message("fout ", fout, " already exists and `cdo_force=F`. skip.")
        } else { # run command
            if (fout_exist_check == 0 && cdo_force == T) {
                message("fout ", fout, " already exists and `cdo_force=T`. delete already existing file ...")
                check <- file.remove(fout)
                if (!check) warning("something went wrong deleting file ", fout)
            }
            cmd <- paste0(cmdprefix, " ", cmdconvert, 
                          #" ", cmdcat, 
                          " ", cmdcalc, " ", cmdselmon, " ", cmdselect, 
                          " <files> ", fout, " || echo error")
            message("run \"", cmd, "\"")
            cmd <- gsub("<files>", paste0(paste0(files), collapse=" "), cmd)
            # system() does not run the command if its very long
            # workaround: save command in script and run in shell directly:
            if (T) {
                scriptname <- paste0(postpaths[i], "/cmd_", Sys.getpid(), ".txt")
                writeLines(cmd, con=scriptname)
                system(paste0("chmod 755 ", scriptname))
                ticcmd <- Sys.time()
                message("run temporary file ", scriptname, " ...")
                system(paste0(dirname(scriptname), "/./", basename(scriptname)))
                toccmd <- Sys.time()
                system(paste0("rm ", scriptname))
            } else {
                ticcmd <- Sys.time()
                system(cmd, wait=T)
                toccmd <- Sys.time()
            }
            if (!file.exists(fout)) stop("fout does not exist but it should")
            elapsedcmd <- toccmd - ticcmd
            message("took ", elapsedcmd , " ", attributes(elapsedcmd)$units, " for ", 
                    length(files), " file", ifelse(length(files) > 1, "s", ""), " (",
                    length(years_wanted), " year", ifelse(length(years_wanted) > 1, "s", ""), ").")
        }

    # run cdo commands for every file
    } else if (cdo_wout_loop == F) {
        message("\n", "`cdo_wout_loop=F` --> run cdo command on all files separatly ...")
        nf <- length(files)
        fcalcs <- data.frame(basenames=rep(NA, t=nf))
        elapsedcmd <- rep(NA, t=nf)
        laststamp <- df$YYYY[nf]
        if (grepl("<MM>", fpatterns[i])) laststamp <- paste0(laststamp, df$MM[nf])
        for (fi in 1:nf) {

            # timestamp
            stamp <- df$YYYY[fi]
            if (grepl("<MM>", fpatterns[i])) {
                stamp <- paste0(stamp, df$MM[fi])
            }
            message(paste0("file ", fi, "/", nf, " (timestamp ", stamp, "/", laststamp, ") ")) 
           
            fout <- paste0(postpaths[i],"/", prefix, suffixs[i],
                           "_", modes[i],
                           ifelse(filetype == "grb", paste0("_selcode_", codes[i]), ""),
                           "_selname_", fvarnames[i], 
                           ifelse(!is.na(levs_out[i]), paste0("_sellev_", levs_out[i]), ""),
                           "_", areas_out[i],
                           "_", stamp,
                           "_tmp.nc")
            fout_exist_check <- file.access(fout, mode=0)
            if (fout_exist_check == 0 && cdo_force == F) { # running command not necessary
                if (fi == 1) message("fout ", fout, " already exists and `cdo_force=F`. skip.")
            } else { # run command
                if (fout_exist_check == 0 && cdo_force == T) {
                    if (fi == 1) message("fout ", fout, " already exists and `cdo_force=T`. delete already existing file ...")
                    check <- file.remove(fout)
                    if (!check) warning("something went wrong deleting file ", fout)
                }
                cmd <- paste0(cmdprefix, " ", cmdconvert, " ", cmdcalc, " ", cmdselect, " ", files[fi], " ", fout, " || echo error")
                if (fi == 1) message("run \"", cmd, "\"")
                ticcmd <- Sys.time()
                system(cmd, wait=T)
                toccmd <- Sys.time()
                elapsedcmd[i] <- toccmd - ticcmd
                message("took ", elapsedcmd[i] , " sec for ", stamp, ".")
            }

            fcalcs$basenames[fi] <- basename(fout)
            
            # if wanted, for "mean_ts" mode, set own time:
            # ncap2 -s time=time-0.9944444
            # at the end of the script the command string is too long for ncap2:
            # error: /sw/rhel6-x64/nco/nco-4.7.5-gcc64/bin/ncap2: Argument list too long
            # --> do it for every file
            if (add_my_time) {
              
                stop("update")

                if (!interactive()) {
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
                        system(cmd, wait=T)

                        if (!is.na(froms_shift[i]) && froms[i] != froms_shift[i]) {
                            cmd <- paste0("ncap2 -O -s 'defdim(\"time3\",", length(timeout2), "); time3[time3]={", 
                                          paste0(sprintf("%f", timeout2), collapse=","), 
                                          "}; time3@long_name=\"Time in fraction of a year start from ", timeout2[1], "\"' ",
                                          fout_prep[fi,k], " ", fout_prep[fi,k])
                            message("\n", cmd)
                            system(cmd, wait=T)
                        }

                        # make record dim for ncrcat
                        cmd <- paste0("ncks -O -4 --mk_rec_dmn time ", fout_prep[fi,k], " ", fout_prep[fi,k])
                        message("\n", cmd)
                        system(cmd, wait=T)
                        cmd <- paste0("ncks -O -4 --mk_rec_dmn time2 ", fout_prep[fi,k], " ", fout_prep[fi,k])
                        message("\n", cmd)
                        system(cmd, wait=T)
                        if (!is.na(froms_shift[i]) && froms[i] != froms_shift[i]) {
                            cmd <- paste0("ncks -O -4 --mk_rec_dmn time3 ", fout_prep[fi,k], " ", fout_prep[fi,k])
                            message("\n", cmd)
                            system(cmd, wait=T)
                        }

                    } # if modes "fldmean"
                } # for fi nf
            } # if add_my_time

        } # for fi nf
    
        if (verbose > 0) {
            message("\n", "calculated ", fvarnames[i], " ", modes[i], " from ", length(fcalcs$basenames), 
                    " file", ifelse(length(fcalcs$basenames) > 1, "s", ""), " in ", mean(elapsedcmd), 
                    " sec. fcalcs:")
            if (length(fcalcs$basenames) > 1) {
                ht(fcalcs)
            } else {
                print(fcalcs)
            }
        }
            
        # Cat or ensmean (`cdocat`) depending on mode on processed files (fcalcs) 
        message("\n", "Process ", length(fcalcs$basenames), " ", fvarnames[i], " ", modes[i], " files in time ...")
        fout <- paste0(postpaths[i],"/", prefix, suffixs[i],
                       "_", modes[i],
                       ifelse(filetype == "grb", paste0("_selcode_", codes[i]), ""),
                       "_selname_", fvarnames[i], 
                       ifelse(!is.na(levs_out[i]), paste0("_sellev_", levs_out[i]), ""),
                       "_", areas_out[i],
                       "_", season_names[i], "_", froms[i], "-", tos[i], 
                       ".nc")
        if (file.access(fout, mode=0) == 0 && cdo_force == F) { # running command not necessary
            message("fout ", fout, " already exists and `cdo_force=F`. skip.")
        } else { # run command

            if (file.access(fout, mode=0) == 0 && cdo_force == T) {
                message("fout ", fout, " already exists and `cdo_force=T`. delete already existing file ...")
                check <- file.remove(fout)
                if (!check) warning("something went wrong deleting file ", fout)
            }
            
            cmd <- paste0("cdo ", cdo_silent, " ", cdo_OpenMP_threads, " ", cmdcat, " <fcalcs> ", fout)
            cmd <- paste0(cmd, " || echo error")
            message("run \"", cmd, "\"")
            cmd <- gsub("<fcalcs>", paste0(paste0(postpaths[i], "/", fcalcs$basenames), collapse=" "), cmd)
            system(cmd, wait=T)
            if (!file.exists(fout)) stop("fout does not exist but it should")
        
        } # if fout already exists or not

        # remove fcalcs
        if (clean) {
            message("\n", "`clean=T` --> remove intermediate calculation files fcalcs ...")
            check <- file.remove(paste0(postpaths[i], "/", fcalcs$basenames))
            if (!all(check)) warning("something went wrong rm fcalcs")
        }

    } # run_wout_loop == T or F
    
    # set relative time axis
    if (set_rel_time) {
        message("\n", "Make relative time axis ...")
        cmd <- paste0("cdo ", cdo_silent, " -r copy ", fout, " ", fout)
        cmd <- paste0(cmd, " || echo error")
        message("run \"", cmd, "\"")
        system(cmd, wait=T)
    }
    
    # change from code to proper variable name
    #if (filetype == "grb") {
    #    stop("update")
    #    cmd <- paste0("cdo ", cdo_silent, " ", cdo_OpenMP_threads, " chname,var", codes[i], ",", varnames[i], " ", 
    #                  fout_prep[fi,k], " ", fout_prep[fi,k])
    #    message("\n", cmd)
    #    system(cmd, wait=T)
    #}
    
    toc <- Sys.time()
    elapsed[[i]] <- toc - tic
    message("\n", "setting ", i, "/", nsettings , " took ", elapsed[[i]], " ", 
            attributes(elapsed[[i]])$units, " for ", modes[i], " calculation of ", 
            length(years_wanted), " year", ifelse(length(years_wanted) > 1, "s", ""), ".")

} # for i nsettings

message("\n", "finished", "\n")

for (i in 1:nsettings) {
    message("setting ", i, "/", nsettings , "\n",
            "  ", datapaths[i], "/", fpatterns[i], "\n",
            "took ", elapsed[[i]], " ", 
            attributes(elapsed[[i]])$units, " for ", modes[i], " calculation of ", 
            length(years_wanted), " year", ifelse(length(years_wanted) > 1, "s", ""), ".")
}

message("\n", "grep this log file for lines that begin with \"error\": grep -n \"^error\" <logfile>", 
        "\n", "be happy if nothing is returned", "\n")

