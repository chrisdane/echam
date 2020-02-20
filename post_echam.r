## R

rm(list=ls()); graphics.off()

# load packages if not interactive
if (!interactive()) {
    ht <- function(d, n=7) { # headtail
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
message("verbose = ", verbose)
message("clean = ", clean)

# check if cdo is available
if (!exists("cdo")) {
    cmd <- paste0("which cdo")
    message("check if cdo can be found: run `", cmd, "`")
    cdo <- system(cmd, intern=T)
    if (!is.null(attributes(cdo)$status)) {
        stop("`which cdo` gave exit status ", attributes(cdo)$status)
    }
}
cdo_version <- paste0(cdo, " --version 2>&1")
cdo_version <- system(cdo_version, intern=T)
cdo_version <- cdo_version[1] # e.g. "Climate Data Operators version 1.7.0 (http://mpimet.mpg.de/cdo)"
cdo_version <- strsplit(cdo_version, " ")[[1]]
cdo_version <- sapply(cdo_version, strsplit, split="\\.")
cdo_version <- suppressWarnings(lapply(cdo_version, as.numeric))
tmp <- sapply(cdo_version, is.na)
tmp <- lapply(tmp, "==", "FALSE")
tmp <- sapply(tmp, any)
if (any(tmp)) {
    if (length(which(tmp)) == 1) {
        cdo_version <- cdo_version[[which(tmp)]] # numeric vector of length 3; e.g.: 1 7 0
    } else {
        stop("the case of more than 1 as.numeric() of `cdo --version?` is not implemented here")
    }
} else {
    stop("the case of 0 as.numeric() of `cdo --version?` is not implemented here")
}
message("cdo = ", cdo)
message("cdo_version = ", paste(cdo_version, collapse="."))
message("cdo_silent = \"", cdo_silent, "\"")
message("cdo_force = ", cdo_force) #  -O necessary for ens<STAT>, merge, mergetime
message("cdo_OpenMP_threads = \"", cdo_OpenMP_threads, "\"") # OMP supported operators: https://code.mpimet.mpg.de/projects/cdo/wiki/OpenMP_support
message("cdo_set_rel_time = ", cdo_set_rel_time)
message("cdo_run_from_script = ", cdo_run_from_script)

# check if ncap2 is available
if (!exists("nco_ncap2")) {
    cmd <- paste0("which ncap2")
    message("check if ncap2 can be found: run `", cmd, "`")
    nco_ncap2 <- system(cmd, intern=T)
    if (!is.null(attributes(nco_ncap2)$status)) {
        stop("`which ncap2` gave exit status ", attributes(nco_ncap2)$status)
    }
}
message("ncap2 = ", nco_ncap2)

# check if ncrcat is available
if (!exists("nco_ncrcat")) {
    cmd <- paste0("which ncrcat")
    message("check if ncrcat can be found: run `", cmd, "`")
    nco_ncrcat <- system(cmd, intern=T)
    if (!is.null(attributes(nco_ncrcat)$status)) {
        stop("`which ncrcat` gave exit status ", attributes(nco_nrcat)$status)
    }
}
message("ncrcat = ", nco_ncrcat)

# check if ncatted is available
if (!exists("nco_ncatted")) {
    cmd <- paste0("which ncatted")
    message("check if ncatted can be found: run `", cmd, "`")
    nco_ncatted <- system(cmd, intern=T)
    if (!is.null(attributes(nco_ncatted)$status)) {
        stop("`which ncatted` gave exit status ", attributes(nco_ncatted)$status)
    }
}
message("ncatted = ", nco_ncatted)

# check if necessary user variables exist
exist_checks <- c("datapaths", "fpatterns", "fvarnames",
                  "models", "froms", "tos", "modes")
if (!all(sapply(exist_checks, exists))) {
    missing_vars <- !sapply(exist_checks, exists)
    stop("you have to define the variable", 
         ifelse(length(which(missing_vars)) > 1, "s", ""),
         " \"", paste0(names(missing_vars)[missing_vars], collapse="\", \""), "\"")
}
if (any(file.access(datapaths, mode=4) != 0)) { # check read permission
    nonreadable_paths <- which(file.access(datapaths, mode=2) != 0)
    stop("have not read permission in datapath", ifelse(length(nonreadable_paths) > 1, "s", ""),
         " '", paste0(datapaths[nonreadable_paths], collapse="', '"), "'.")
}
datapaths <- normalizePath(datapaths)
nsettings <- length(datapaths)
if (!exists("ftypes")) ftypes <- rep("f", t=nsettings)
if (!exists("prefixs")) prefixs <- rep(NA, t=nsettings)
if (!exists("new_time_origins")) new_time_origins <- rep(NA, t=nsettings)
if (F && is.numeric(new_time_origins) && any(new_time_origins == 0) ||
    is.character(new_time_origins) && any(new_time_origins == "0000")) {
    stop("new_time_origins must not be zero for ncview due to\n",
         "Error in utCalendar2: the Gregorian calendar has no year 0. ",
         "Use the \"Gregorian_y0\" calendar if you want to include year 0. ",
         "internal error: udu_fmt_time can't convert to calendar value!")
}
if (!exists("codes")) codes <- rep(NA, t=nsettings)
if (!exists("areas_out")) areas_out <- rep("global", t=nsettings)
# check if postpaths can be created/have writing rights
if (!exists("postpaths")) {
    postpaths <- paste(workpath, "post", models, modes, fvarnames, sep="/")
} else {
    postpaths <- normalizePath(postpaths)
}
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
cdo_set_rel_time_old <- cdo_set_rel_time # for next setting i
if (!exists("wiso_smow_files")) wiso_smow_files <- rep(NA, t=nsettings)
if (!exists("wiso_code_tables")) wiso_code_tables <- rep(NA, t=nsettings)
if (!exists("wiso_paramater_tables")) wiso_paramater_tables <- rep(NA, t=nsettings)


# do for every model setting
elapsed <- vector("list", l=nsettings)
for (i in 1:nsettings) {

    tic <- Sys.time()
    message("\n", "*********** setting ", i, "/", nsettings, " *************")
    message("datapath = ", datapaths[i])
    message("model = ", models[i])
    message("fpattern = ", fpatterns[i])
    message("ftype = ", ftypes[i])
    message("fvarname = ", fvarnames[i])
    if (!is.na(codes[i])) message("code = ", codes[i])
    message("postpath = ", postpaths[i])
    message("mode = ", modes[i])
    message("from = ", froms[i])
    message("to = ", tos[i])
    message("season_name = ", season_names[i])
    message("season_inds = ", paste0(season_inds[[i]], collapse=","))
    if (!is.na(new_time_origins[i])) message("new_time_origin = ", new_time_origins[i])
    message("area_out = ", areas_out[i])
    if (!is.na(levs_out[i])) message("lev_out = ", levs_out[i])

    # get file type
    if (!exists("filetypes")) {
        # lazy approach: take everything after last . (dot) in file name
        filetype <- substr(fpatterns[i], regexpr("\\.[^\\.]*$", fpatterns[i]) + 1, nchar(fpatterns[i]))
    } else {
        filetype <- filetypes[i]
    }
    if (!any(filetype == c("grb", "nc"))) {
        stop("filetype \"", filetype, "\" not implemented yet")
    }

    # output fname
    fout <- paste0(postpaths[i], "/", 
                   ifelse(!is.na(prefixs[i]), prefixs[i], ""),
                   "_", modes[i],
                   ifelse(filetype == "grb", paste0("_selcode_", codes[i]), ""),
                   "_", fvarnames[i], 
                   ifelse(!is.na(levs_out[i]), paste0("_sellevel_", levs_out[i]), ""),
                   "_", areas_out[i],
                   "_", season_names[i], "_", froms[i], "-", tos[i], 
                   ".nc")
    message("fout = ", fout)
    
    fout_exist_check <- file.access(fout, mode=0)
    if (T && fout_exist_check == 0) {
        message("\n ************** redo although output exists for testing **************")
        fout_exist_check <- -1
    }
    
    # fout already exists 
    if (fout_exist_check == 0 && cdo_force == F) {

        message("final fout=\n   ", fout, "\nalready exists and `cdo_force=F`. skip.")

    # fout does not exist --> run chained cdo operators for all files
    } else { 
        
        # delete fout if it already exists 
        if (fout_exist_check == 0 && cdo_force == T) {
            message("final fout=\n   ", fout, "\nalready exists and `cdo_force=T`. delete already existing file ...")
            check <- file.remove(fout)
            if (!check) warning("something went wrong deleting file ", fout)
        }

        # read input file names
        # e.g.
        # hist_echam6_echam_185001.nc
        # PI-CTRL4_echam6_g3bid_299901.grb 
        fpattern <- fpatterns[i]
        # replace <YYYY> by * if existing
        if (grepl("<YYYY>", fpattern)) {
            fpattern <- gsub("<YYYY>", "*", fpattern)
        }
        # replace <MM> by * if existing
        if (grepl("<MM>", fpattern)) {
            fpattern <- gsub("<MM>", "*", fpattern)
        }

        # find based on fpattern files
        #cmd <- paste0("ls ", datapaths[i], "/", fpattern) 
        # --> this may result in `-bash: /bin/ls: Argument list too long`
        cmd <- paste0("find ", datapaths[i], " -type ", ftypes[i], " -name '", fpattern, "' -printf \"%f\\n\" | sort")
        # --> `find` does not have this limit 
        message("\n", "run `", cmd, "` ...")
        ticcmd <- Sys.time()
        files <- system(cmd, intern=T)
        toccmd <- Sys.time()
        if (length(files) == 0) stop("no files found")
        elapsedcmd <- toccmd - ticcmd
        message("`find` of ", length(files), " files took ", elapsedcmd, " ", attributes(elapsedcmd)$units) 

        # separate into dirname and basename
        dirnames <- dirname(files)
        basenames <- basename(files)
        df <- data.frame(basenames)

        # identify correct YYYY, MM, etc. based on file names
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
            df$MM <- substr(basenames, patterninds[1], patterninds[2]) 
            MM_in <- as.integer(df$MM)
        }
        
        if (verbose > 0) {
            message("\n", "found ", length(files), " ", filetype, " file", 
                    ifelse(length(files) > 1, "s", ""), ":")
            if (length(files) > 1) {
                ht(df)
            } else {
                print(df)
            }
            message("\nderived years based on file names:")
            ht(years_in)
            if (grepl("<MM>", fpatterns[i])) {
                message("\nderived months based on file names:")
                ht(MM_in)
            }
        }

        # wanted years
        from <- as.POSIXlt(paste0(froms[i], "-01-01"), tz="UTC")
        to <- as.POSIXlt(paste0(tos[i], "-12-31"), tz="UTC")
        years_wanted <- (unclass(from)$year+1900):(unclass(to)$year+1900)

        # check if some wanted years are out of found years, which were found based on the file names
        if (any(years_wanted %in% years_in == F)) {
            stop("wanted year", 
                 ifelse(length(which(!(years_wanted %in% years_in))) > 1, "s", ""),
                 " ", paste0(years_wanted[!(years_wanted %in% years_in)], collapse=","),
                 " ", ifelse(length(which(!(years_wanted %in% years_in))) > 1, "are", "is"),
                 " not included in found year", 
                 ifelse(length(years_in) > 1, "s", ""), " ", 
                 paste0(range(years_in), collapse="-"), ".")
        }

        # remove found years (which were found based on the file names) out of wanted years
        outside_years_inds <- which(years_in %in% years_wanted == F)
        if (length(outside_years_inds) > 0) {
            message("\n", "remove ", length(outside_years_inds), " file",
                    ifelse(length(outside_years_inds) > 1, "s", ""),
                    " outside of wanted years defined by froms[", i, "] = ", 
                    froms[i], " to tos[", i, "] = ", tos[i], " ...")
            files <- files[-outside_years_inds]
            dirnames <- dirnames[-outside_years_inds]
            basenames <- basenames[-outside_years_inds]
            df <- df[-outside_years_inds,]
            years_in <- years_in[-outside_years_inds]
            if (grepl("<MM>", fpatterns[i])) MM_in <- MM_in[-outside_years_inds]
            if (verbose > 0) {
                message("\n", "found ", length(files), " ", filetype, " file", 
                        ifelse(length(files) > 1, "s", ""), ":")
                if (length(files) > 1) {
                    ht(df)
                } else {
                    print(df)
                }
            } 
        }

        # remove found months (which were found based on the file names) out of wanted season
        cdoselmon <- "" # default
        if (season_names[i] != "Jan-Dec") {
            message("\n", "season_inds = ", paste(season_inds[[i]], collapse=","), 
                    " -> season = ", season_names[i])
            if (grepl("<MM>", fpatterns[i])) { # <MM> is in fpatterns
                file_season_inds <- which(MM_in %in% season_inds[[i]])
                if (length(file_season_inds) == 0) { # no files in wanted season 
                    stop("no files found at season_inds = ", paste(season_inds[[i]], collapse=","), 
                         " based on given <MM> pattern.")
                }
                message("remove ", length(file_season_inds), " files out of these months. files:")
                files <- files[file_season_inds]
                dirnames <- dirnames[file_season_inds]
                basenames <- basenames[file_season_inds]
                df <- df[file_season_inds,]
                years_in <- years_in[-outside_years_inds]
                MM_in <- MM_in[-outside_years_inds]
                if (verbose > 0) ht(df)

            } else { # <MM> is not in fpatterns
                cmd <- paste0("cdo ", cdo_silent, " showmon ", files[1])
                message("run `", cmd, "`")
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
                cdoselmon <- paste0("-selmon,", paste(months_per_file[selmon_season_inds], collapse=",")) 
                message("to do: add `", cdoselmon, "` to cdo command ...")

            } # if <MM> is given in fpatterns or not
        } # if season_name != "Jan_Dec"

        # sort found years and months (which were found based on the file names)
        # e.g.:
        # 1   Hol-T_echam5_wiso_mm_699701.nc 6997 01
        # 2   Hol-T_echam5_wiso_mm_699712.nc 6997 12
        # 86  Hol-T_echam5_wiso_mm_699702.nc 6997 02
        # sort years
        if (!all(diff(years_in) >= 0) && !all(diff(years_in) <= 0) || # not monotonically increasing/decreasing
            years_in[1] != as.numeric(froms[i]) || years_in[length(years_in)] != as.numeric(tos[i])) { # or files are not in wanted order
            message("\n", "years obtained from file names are not monotonically increasing or decreasing\n",
                    " --> sort according to `froms[", i, "]` = \"", froms[i], 
                    "\" to `tos[", i, "]` = \"", tos[i], "\" ...")
            #years_in_ordered_inds <- sort(years_unique, index.return=T)$ix 
            # -> the above does not keep the wanted order (increasing or decreasing years)
            years_unique <- years_wanted # keep order here (increasing or decreasing years)
            years_in_ordered_inds <- rep(NA, t=length(years_in))
            inds_all <- seq_len(length(years_in))
            for (yi in seq_len(length(years_unique))) { # this keeps the wanted year order from:to
                inds_yi <- which(years_in == years_unique[yi])
                first_NA_ind <- which(is.na(years_in_ordered_inds))[1]
                inds_all <- first_NA_ind:(first_NA_ind + length(inds_yi) - 1)
                years_in_ordered_inds[inds_all] <- inds_yi
            }
            # update:
            files <- files[years_in_ordered_inds]
            dirnames <- dirnames[years_in_ordered_inds]
            basenames <- basenames[years_in_ordered_inds]
            df <- df[years_in_ordered_inds,]
            years_in <- years_in[years_in_ordered_inds]
            if (grepl("<MM>", fpatterns[i])) MM_in <- MM_in[years_in_ordered_inds]
            if (verbose > 0) ht(df) 
        } # if years_in are not monotonically increasing/decreasing

        # sort months
        if (F) { # not correct yet; however, not needed because of `find ... | sort`
            if (grepl("<MM>", fpatterns[i])) {
                #if (MM_in) { # <-- correct condition missing
                    message("\n", "months obtained from file names are not monotonically increasing\n",
                            " --> sort from 1 to 12 ...")
                    # at this points, `years_in` are allready in correct order
                    stop("asd")
                    years_unique <- unique(years_in)
                    MM_in_ordered_inds <- rep(NA, t=length(MM_in))
                    inds_all <- seq_len(length(MM_in))
                    for (yi in seq_len(length(years_unique))) {
                        inds_yeari <- which(years_in == years_unique[yi])
                        inds_yeari_lhs <- ((yi-1)*length(inds_yeari)+1):(yi*length(inds_yeari))
                        inds_yeari_rhs <- inds_all[inds_yeari][sort(MM_in[inds_yeari], index.return=T)$ix]
                        MM_in_ordered_inds[inds_yeari_lhs] <- inds_yeari_rhs
                    } # for yi years_unique
                    # update:
                    files <- files[MM_in_ordered_inds]
                    dirnames <- dirnames[MM_in_ordered_inds]
                    basenames <- basenames[MM_in_ordered_inds]
                    df <- df[MM_in_ordered_inds,]
                    years_in <- years_in[MM_in_ordered_inds]
                    MM_in <- MM_in[MM_in_ordered_inds]
                    if (verbose > 0) ht(df)
                #} # if MM_in are not monotonically increasing/decreasing
            } # if <MM> is given in fpatterns or not
        } # F

        # construct cdo command (chained cdo commands will be executed from right to left)
        # prefix
        cdoprefix <- paste0(cdo, " ", cdo_silent)

        # convert to nc if grb
        cdoconvert <- "" # default
        if (filetype == "grb") {
            if (any(models[i] == c("echam4", "echam5", "echam6", "mpiom1", "ecmwf", "remo", 
                                   "cosmo002", "cosmo201", "cosmo202", "cosmo203", "cosmo205", "cosmo250"))) {
                cdoconvert <- paste0(cdoconvert, " -t ", models[i])
            }
            cdoconvert <- paste0(cdoconvert, " -f nc")
        } # if grb
        
        # check if requested variable is in first found file
        message("\ncheck if requested variable ", appendLF=F)
        if (filetype == "grb") {
            if (is.na(codes[i])) { # code not provided
                stop("cannot process setting ", i, "/", nsettings, 
                     ": provide a code number of the requested variable '", fvarnames[i], 
                     "'.")
            } else { # code provided
                cdoselect <- paste0("-select,code=", codes[i])
            }
            message("\"", codes[i], "\"", appendLF=F)
        } else if (filetype == "nc") {
            cdoselect <- paste0("-select,name=", fvarnames[i])
            message("\"", fvarnames[i], "\"", appendLF=F)
        }
        message(" is present in first found file ...")
        tmpfile <- paste0(postpaths[i], "/tmp_variable_check_", Sys.getpid(), ".", filetype)
        cmd <- paste0(cdoprefix, " ", cdoselect, " ", datapaths[i], "/", files[1], " ", tmpfile)
        message("run `", cmd, "`")
        check <- system(cmd, intern=T)
        
        # if requested variable was not found in first found file
        if (F) {
            message("\n*********** for testing set variable to not found ************\n")
            check <- 1
        }
        if (length(check) != 0) { 

            message("--> requested variable \"", fvarnames[i], "\" was not found in first file\n",
                    "  \"", datapaths[i], "/", files[1], "\"")

            # special case: requested variable is one of the wiso delta variables
            #if (fvarnames[i] %in% known_wiso_d_vars$vars) {
            if (fvarnames[i] %in% names(cdo_known_cmds)) {

                message("\nrequested variable \"", fvarnames[i], "\" is one of the variables defined in `cdo_known_cmds`:")
                for (vari in 1:length(cdo_known_cmds)) {
                    message("   ", vari, " \"", names(cdo_known_cmds)[vari], "\": ", appendLF=F)
                    if (length(cdo_known_cmds[[vari]]$cmd) == 1) {
                        message("`", cdo_known_cmds[[vari]]$cmd, "`")
                    } else {
                        message("\n", appendLF=F)
                        for (cmdi in 1:length(cdo_known_cmds[[vari]]$cmd)) {
                            message("      `", cdo_known_cmds[[vari]]$cmd[cmdi], "`")
                        }
                    }
                }

                # check command if all necessary input files are available
                cmdsin <- cdo_known_cmds[[fvarnames[i]]]$cmd
                cmdsout <- cmdsin
                for (cmdi in 1:length(cmdsin)) {
                    message("\ncheck user cmd ", cmdi, "/", length(cmdsin), " for \"<\" and \">\": \"", cmdsin[cmdi], "\" ...")
                    replace_inds_open <- gregexpr("<", cmdsin[cmdi])[[1]]
                    replace_inds_close <- gregexpr(">", cmdsin[cmdi])[[1]]
                    if (length(replace_inds_open) != length(replace_inds_close)) {
                        stop("you provided a different number of \"<\" and \">\" in this command.")
                    }
                    if (length(replace_inds_open) == 1 && replace_inds_open == -1) { # not a single "<" was found in command
                        # nothing to do, let user commands as they are in the namelist
                    } else {
                        for (cmdj in 1:length(replace_inds_open)) {
                            # check if a variable in the current workspace exists with the same name
                            pattern <- substr(cmdsin[cmdi], replace_inds_open[cmdj] + 1, replace_inds_close[cmdj] - 1)
                            if (exists(eval(pattern))) { # variable with the name of the pattern exists
                                eval(parse(text=paste0("replacement <- ", pattern, "[i]")))
                            } else { # no such a variable exists
                                # assume an input file is needed
                                replacement <- fout
                                replacement <- gsub(fvarnames[i], pattern, replacement)
                                if (!file.exists(replacement)) {
                                    stop("\nfound pattern \"<", pattern, ">\" is not a defined variable in the current workspace",
                                         " --> assume it should be an input file. however, file\n   \"", replacement, "\"\n",
                                         "was not found.\n")
                                }
                            }
                            # replace found variable/file with pattern in command
                            message("   replace \"<", pattern, ">\" with \"", replacement, "\" ...")
                            cmdsout[cmdi] <- gsub(paste0("<", pattern, ">"), replacement, cmdsout[cmdi])

                        } # for all occurences of "<"
                        message("--> \"", cmdsout[cmdi], "\"")
                    } # if "<" and ">" were found in command
                } # for cmdi all commands of cdo_known_cmds$variable

                # run modified cdo commands
                for (cmdi in 1:length(cmdsout)) {
                    cmd <- cmdsout[cmdi]
                    tmpfiles <- c()
                    if (length(cmdsout) > 1) {
                        if (cmdi == 1) { # first
                            tmpfile <- paste0(postpaths[i], "/cmdout_", cmdi, "_of_", length(cmdsout), "_tmp_", Sys.getpid())
                            cmd <- paste0(cmd, " ", tmpfile)
                            tmpfiles <- c(tmpfiles, tmpfile)
                        } else if (cmdi == length(cmdsout)) { # last
                            tmpfile <- paste0(postpaths[i], "/cmdout_", cmdi-1, "_of_", length(cmdsout), "_tmp_", Sys.getpid())
                            cmd <- paste0(cmd, " ", tmpfile, " ", fout)
                            tmpfiles <- c(tmpfiles, tmpfile)
                        } else { # in between
                            tmpfile <- paste0(postpaths[i], "/cmdout_", cmdi-1, "_of_", length(cmdsout), "_tmp_", Sys.getpid())
                            cmd <- paste0(cmd, " ", tmpfile)
                            tmpfiles <- c(tmpfiles, tmpfile)
                            tmpfile <- paste0(postpaths[i], "/cmdout_", cmdi, "_of_", length(cmdsout), "_tmp_", Sys.getpid())
                            cmd <- paste0(cmd, " ", tmpfile)
                            tmpfiles <- c(tmpfiles, tmpfile)
                        }
                    } else { # only 1 cmd
                        cmd <- paste0(cmd, " ", fout)
                    }
                    message("\nrun ", cmdi, "/", length(cmdsout), ": `", cmd, "`")
                    system(cmd)
                } # run all (possibly modifed) user commands

                if (clean) {
                    for (fi in tmpfiles) {
                        system(paste0("rm -v ", fi))
                    }
                }
               
                # stop script here
                toc <- Sys.time()
                elapsed[[i]] <- toc - tic
                return(message("\nskip rest of script"))

            # else requested variable is not defined in `cdo_known_cmds`
            } else { 

                message("and no command was defined in `cdo_known_cmds` in namelist.post.r")
                stop()

            } # if special case if requested variable is one of the wiso delta variables or not
            
        # else if requested variable was found in first found file
        } else { 
           
            message("--> requested variable was found in first file")
            if (clean) system("rm -v ", tmpfile)
        
        } # finished check if requested variable is in first found file

        ## continue with default case; not special wiso delta variables
        message("\nconstruct cdo command chain ...")

        # nth command: cat/mergetime/etc. command
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

        # construct necessary cdo commands
        # (n-2)-th command: calculation
        if (modes[i] == "select") {
            cdocalc <- "" # variable selection only
        } else if (modes[i] == "timmean") {
            cdocalc <- "-timmean"
        } else if (modes[i] == "fldmean") {
            cdocalc <- "-fldmean"
        } else if (modes[i] == "volint") {
            message("only test")
            cdocalc <- "-fldsum -vertsum"
        } else if (modes[i] == "yearsum") {
            cdocalc <- "-yearsum" 
        } else if (modes[i] == "timsum") {
            cdocalc <- "-timsum"
        } else {
            stop("calculation for mode '", modes[i], "' not defined.")
        } # which calculation depending on mode

        # (n-3)-th command: select level
        cdosellevel <- "" # default
        if (!is.na(levs_out[i])) {
            cdosellevel <- paste0("-sellevel,", paste0(levs_out[i], collapse=","))
        }
        
        # select area
        if (areas_out[i] != "global") {
            stop("not yettt")
        }

        # construct cdo command
        # cdo version must be >= 1.9.4 to chain commands
        #   `-select,name=` 
        # and 
        #   `-f <type> copy`, `-fldmean`, `-selmon`, `-sellevel`, etc.
        if (cdo_version[1] < 1 ||
            cdo_version[1] == 1 && cdo_version[2] < 9 || 
            cdo_version[1] == 1 && cdo_version[2] > 8 && cdo_version[3] < 4) { 
            
            # if cdo version < 1.9.4
            cdo_chain <- "old"
            message("\n", "cdo version ", paste(cdo_version, collapse="."), " < 1.9.4 --> have to run separate")
            message("   `-select,name=`\n",
                    "and possible\n",
                    "   `-f <type> copy`, `-fldmean`, `-selmon`, `-sellevel`, etc.\n",
                    "cdo commands:")

            # 1st cmd: `-select,name=`
            tmpfile <- paste0(postpaths[i], "/tmp_selection_", Sys.getpid(), ".nc")
            cmd_select <- paste0(cdoprefix, " ", cdoselect,  
                                 " <files> ", tmpfile, " || echo error")
            # 2nd cmd (if needed):
            cmd_calc <- "" # default: nothing
            if (cdoconvert != "" || cdosellevel != "" || cdoselmon != "" || modes[i] != "select") {
                cmd_calc <- cdoprefix
            }
            # 2nd cmd (if needed): `-f <type copy>`
            if (cdoconvert != "") {
                cmd_calc <- paste0(cmd_calc, " ", cdoconvert)
            }
            # 2nd cmd (if needed): `-fldmean` etc.
            if (modes[i] != "select") {
                cmd_calc <- paste0(cmd_calc, " ", cdocalc) 
            }
            # 2nd cmd (if needed): `-selmon`
            if (cdoselmon != "") {
                cmd_calc <- paste0(cmd_calc, " ", cdoselmon)
            }
            # 2nd cmd (if needed): `-sellevel`
            if (cdosellevel != "") {
                cmd_calc <- paste0(cmd_calc, " ", cdosellevel)
            }
            # 2nd cmd (if needed: <add further>
            # ...
            # 2nd cmd (if needed): in out
            if (cmd_calc != "") {
                # input: result of `-select,name=`
                # output: result of `-f <type> copy`, `-fldmean`, `-selmon`, `-sellevel`, etc. --> wanted `fout`
                cmd_calc <- paste0(cmd_calc, " ", tmpfile, " ", fout) 
            } # if cmd_calc != ""
           
            # 2nd cmd alternative: just selection: nothing to do but renaming to wanted `fout`
            if (cmd_calc == "") {
                # in: result of `-select,name=`
                # out: wanted `fout`
                cmd_calc <- paste0("cp -v ", tmpfile, " ", fout, " || echo error") 
            }
            message("\nrun\n",
                    "   1: `", cmd_select, "`\n",
                    "   2: `", cmd_calc, "`")
            
            # replace multiple spaces by single spaces
            nchar_with_mulitiple_spaces <- nchar(cmd_select)
            cmd_select <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd_select, perl=T)
            nchar_with_single_spaces <- nchar(cmd_select)
            #message("removed ", nchar_with_mulitiple_spaces-nchar_with_single_spaces, 
            #        " multiple spaces from selection command")
            
            # replace <files> with actual files
            #files <- files[1:29355]
            cmd_select_tmp <- gsub("<files>", 
                                   paste(paste0(datapaths[i], "/", files), collapse=" "), 
                                   cmd_select)
            
            # check if cmd_select command is longer than cdo_nchar_max_arglist = 2612710
            nchar_cmd_select <- nchar(substr(cmd_select_tmp, start=nchar(cdo) + 1, stop=nchar(cmd_select_tmp))) 
            message("\n", "cdo selection argument list is ", nchar_cmd_select, " characters long")
            if (nchar_cmd_select > cdo_nchar_max_arglist) { # too long
                
                # find maximum number of files per chunk
                message("--> this is longer than `cdo_nchar_max_arglist` = ", cdo_nchar_max_arglist, 
                        " and would yield the error \"Argument list too long\"")
                nchar_needed_per_file <- nchar(paste0(datapaths[i], "/", files[1], " "))
                nchar_wout_files <- nchar(gsub("<files>", "", cmd_select))
                nchar_avail <- cdo_nchar_max_arglist - nchar_wout_files
                nfiles_per_chunk <- floor(nchar_avail/nchar_needed_per_file)
                
                # if monthly files, do not separate files from within a year
                # --> necessary that e.g. `cdo yearsum` sums all files of a year 
                if (grepl("<MM>", fpatterns[i])) {
                    if (years_in[nfiles_per_chunk+1] != years_in[nfiles_per_chunk] + 1) {
                        one_year_earlier_inds <- which(years_in == years_in[nfiles_per_chunk] - 1)
                        if (length(one_year_earlier_inds) == 0) {
                            stop("this should not happen as all files are of one year and still ",
                                 "the cdo command is longer than ", cdo_nchar_max_arglist, " characters.")
                        }
                        nfiles_per_chunk <- one_year_earlier_inds[length(one_year_earlier_inds)]
                    } # if next entry is not next year
                } # if monthly files

                cmd_select_chunk_inds <- seq(1, length(files), b=nfiles_per_chunk)
                if (max(cmd_select_chunk_inds) < length(files)) {
                    cmd_select_chunk_inds <- c(cmd_select_chunk_inds, length(files))
                } else if (max(cmd_select_chunk_inds) > length(files)) {
                    stop("this should not happen")
                }
                nchunks <- length(cmd_select_chunk_inds) - 1
                message("--> run cdo selection of ", length(files), 
                        " files in ", nchunks, " chunk", ifelse(nchunks > 1, "s", ""), 
                        " of maximum ", nfiles_per_chunk, " files per chunk:")
                tmpfile_vec <- fout_vec <- rep(NA, t=nchunks)
                cmd_select_list <- cmd_calc_list <- chunk_inds_list <- vector("list", l=nchunks)
                for (select_chunki in seq_len(nchunks)) {
                    inds_chunki <- cmd_select_chunk_inds[select_chunki]:(cmd_select_chunk_inds[select_chunki+1] - 1)
                    if (select_chunki == nchunks) {
                        inds_chunki <- cmd_select_chunk_inds[select_chunki]:cmd_select_chunk_inds[select_chunki+1]
                    }
                    tmpfile_vec[select_chunki] <- gsub(".nc", paste0("_chunk_", select_chunki, "_of_", nchunks, ".nc"), tmpfile)
                    fout_vec[select_chunki] <- gsub(".nc", paste0("_chunk_", select_chunki, "_of_", nchunks, ".nc"), fout)
                    cmd_select_chunki <- gsub("<files>", 
                                              paste(paste0(datapaths[i], "/", files[inds_chunki]), collapse=" "), 
                                              cmd_select)
                    cmd_select_chunki <- gsub(tmpfile, tmpfile_vec[select_chunki], cmd_select_chunki)
                    cmd_select_list[[select_chunki]]$n <- length(inds_chunki)
                    cmd_select_list[[select_chunki]]$cmd <- cmd_select_chunki
                    cmd_calc_chunki <- gsub(tmpfile, tmpfile_vec[select_chunki], cmd_calc)
                    cmd_calc_chunki <- gsub(fout, fout_vec[select_chunki], cmd_calc_chunki)
                    cmd_calc_list[[select_chunki]]$cmd <- cmd_calc_chunki
                    chunk_inds_list[[select_chunki]] <- inds_chunki
                    message("chunk ", select_chunki, "/", nchunks, ": files ", min(inds_chunki), " to ", 
                            max(inds_chunki), "\n  --> ", length(inds_chunki), " files from \"", 
                            files[min(inds_chunki)], "\" to \"", files[max(inds_chunki)], "\", ",
                            "\n  --> nchar(selection cmd) = ", nchar(cmd_select_list[[select_chunki]]$cmd), ") ...")
                } # for select_chunki
            
            } else { # not too long
                message("--> this is not longer than `cdo_nchar_max_arglist` = ", cdo_nchar_max_arglist, 
                        " and does not yield the error \"Argument list too long\"")
                cmd_select_list <- cmd_calc_list <- chunk_inds_list <- vector("list", l=1)
                cmd_select_list[[1]] <- list(cmd=cmd_select_tmp, n=length(files))
                cmd_calc_list[[1]] <- list(cmd=cmd_calc)
                chunk_inds_list[[1]] <- 1:length(files)
                nchunks <- length(cmd_select_list)
                fout_vec <- fout
                message("--> run cdo selection of ", length(files), " files in ", 
                        nchunks, " chunk", ifelse(nchunks > 1, "s", ""), " ...")
            }

        } else { 
            # else if cdo version >= 1.9.4 
            cdo_chain <- "new"
            cmd <- paste0(cdoprefix, " ", cdoconvert, 
                          #" ", cmdcat, 
                          " ", cdocalc, " ", cdoselmon, " ", cdosellevel, " ", cdoselect,  
                          " <files> ", fout, " || echo error")
            message("\n", "run `", cmd, "`")
            
            # check if cmd command is longer than cdo_nchar_max_arglist = 2612710
            cmd_tmp <- gsub("<files>", paste(paste0(datapaths[i], "/", files), collapse=" "), cmd)
            nchar_cmd <- nchar(substr(cmd_tmp, start=nchar(cdo) + 1, stop=nchar(cmd_tmp))) 
            message("\n", "cdo argument list is ", nchar_cmd, " characters long")
            if (nchar_cmd_select > cdo_nchar_max_arglist) { # too long
                message("--> this is longer than `cdo_nchar_max_arglist` = ", 
                        cdo_nchar_max_arglist, " and would yield the error ",
                        "\"Argument list too long\"")
                stop("check for argument list too long")
            } else {
                cmd_list <- list(cmd=cmd, n=length(files))
                nchunks <- length(cmd_list)
            }

        } # if cdo_version >= 1.9.4 or not
       
        # run cdo selection and calculation command either from file (`$ . file` or via base::system(cmd)
        for (chunki in seq_len(nchunks)) { # for possible chunks if argument is too long
            message("\nchunk ", chunki, "/", nchunks, " cdo selection")
            if (cdo_run_from_script) {
                scriptname <- paste0(postpaths[i], "/tmp_cmd_", Sys.getpid(), "_chunk_", 
                                     chunki, "_of_", nchunks, ".txt")
                if (cdo_chain == "new") {
                    writeLines(cmd_list[[chunki]]$cmd, con=scriptname)
                    nfiles_per_chunk <- cmd_list[[chunki]]$n
                } else if (cdo_chain == "old") {
                    writeLines(c(cmd_select_list[[chunki]]$cmd, cmd_calc_list[[chunki]]$cmd), con=scriptname)
                    nfiles_per_chunk <- cmd_select_list[[chunki]]$n
                }
                cmd_source <- paste0(". ", scriptname)
                message("run `", cmd_source, "`")
                if (nfiles_per_chunk > 1000) {
                    message("this may take some time for ", nfiles_per_chunk, " files ...")
                }
                ticcmd <- Sys.time()
                if (file.exists(fout_vec[chunki]) && !cdo_force) {
                    message("fout_vec[", chunki, "] = ", fout_vec[chunki], 
                            " already exists and `cdo_force`=F. skip ...")
                } else {
                    system(cmd_source)
                }
                toccmd <- Sys.time()
                # output file exists?
                if (!file.exists(fout_vec[chunki])) {
                    stop("fout_vec[", chunki, "] = ", fout_vec[chunki], " does not exist but it should")
                }
                if (clean && cdo_chain == "old"){
                    if (nchunks > 1) { 
                        system(paste0("rm -v ", tmpfile_vec[chunki]))
                    } else if (nchunks == 1) {
                        system(paste0("rm -v ", tmpfile))
                    }
                    system(paste0("rm -v ", scriptname))
                }
            } else if (!cdo_run_from_script) {
                stop("update for cdo_chain old/new")
                ticcmd <- Sys.time()
                system(cmd, wait=T)
                toccmd <- Sys.time()
            } # if cdo_run_from_script
            
            # elapsed
            elapsedcmd <- toccmd - ticcmd
            message("chunk ", chunki, "/", nchunks, " selection and calculation took ", 
                    elapsedcmd , " ", attributes(elapsedcmd)$units, " for ", 
                    nfiles_per_chunk, " file", ifelse(nfiles_per_chunk > 1, "s", ""))
        
        } # for chunki: possible chunks if argument is too long
     
        # from here, cdo selection and calculation finished for all chunks
        # -> time dimension values are still the original, `new_time_origins` was not applied yet
        # -> time dimension values are probably shifted from monthly to annual by e.g. `cdo yearsum`
        # -> chunks are not catted yet

        # todo
        message("\ntodo: compare filename and `froms`/`tos` dates with\n",
                "time dimension values from the actual nc file")
        
        # change time values of the time dimension if new origin is set
        if (exists("new_time_origins") && is.finite(new_time_origins[i])) {

            message("\nuser set `new_time_origins[", i, "]` = ", new_time_origins[i])
            message(" --> check time dimension values of ", nchunks, " chunk file", 
                    ifelse(nchunks > 1, "s", ""), " and apply the new origin if necessary ...")

            # get time dimension values of result of cdo select and calc
            dates_in_list <- vector("list", l=nchunks)
            for (chunki in seq_len(nchunks)) {

                message("\nchunk ", chunki, "/", nchunks, " ...")

                # get time dimension values with `cdo showdate` on the result 
                # of `cdo -fldmean -select,name=var` 
                cdo_showdate_file <- paste0(dirname(fout_vec[chunki]), "/tmp_cdo_showdate_",
                                            Sys.getpid(), "_chunk_", chunki, "_of_", nchunks, ".txt")
                cmd <- paste0(cdoprefix, " showdate ", fout_vec[chunki],
                              " > ", cdo_showdate_file)
                message("run `", cmd, "`")
                system(cmd)
                cdo_dates <- readLines(cdo_showdate_file)
                cdo_dates <- strsplit(cdo_dates, " ")[[1]]
                if (any(cdo_dates == "")) {
                    cdo_dates <- cdo_dates[-which(cdo_dates == "")]
                }
                if (length(cdo_dates) == 0) stop("sth went wrong")
                if (any(is.na(cdo_dates))) stop("there are NAs in cdo_dates")
                message("`cdo showdate` yields ", length(cdo_dates), " dates:")
                ht(cdo_dates, n=25)
                
                # save for every chunk
                if (chunki == 1) {
                    dates_in_list[[chunki]]$inds <- 1:length(cdo_dates)
                } else {
                    dates_in_list[[chunki]]$inds <- seq(max(dates_in_list[[chunki-1]]$inds) + 1, l=length(cdo_dates))
                }
                dates_in_list[[chunki]]$dates <- cdo_dates
                dates_in_list[[chunki]]$years <- as.numeric(substr(cdo_dates, 1, 4))
                dates_in_list[[chunki]]$months <- as.numeric(substr(cdo_dates, 6, 7))
                dates_in_list[[chunki]]$days <- as.numeric(substr(cdo_dates, 9, 10))
           
                if (clean) system(paste0("rm -v ", cdo_showdate_file))

            } # for chunki nchunks

            # construct and apply new dates if necessary
            ntime_in <- length(years_in)
            ntime_out <- sum(sapply(sapply(dates_in_list, "[", "dates"), length))
            if (dates_in_list[[1]]$years[1] != new_time_origins[i]) {
                
                message("\nthe first input year of the ", ntime_out, " dates of `cdo ", 
                        cdocalc, " ", cdoselect, "` result is ", 
                        dates_in_list[[1]]$years[1], " != `new_time_origins[", i, "]` = ", 
                        new_time_origins[i], "\n --> find new time dimension values ...")
                
                # new dates with respect to `new_time_origins` set by user
                dates_out_list <- vector("list", l=nchunks)
                for (chunki in seq_len(nchunks)) {

                    message("\nchunk ", chunki, "/", nchunks, " ...")
                  
                    # new years based on `new_time_origins` and years from file names 
                    # (`years_in`) and NOT the years from nc time dimension values
                    # --> workaround for wrong model years due to echam calendar limitations
                    # --> `cdo shifttime` would not work
                    if (ntime_out == ntime_in) {
                        years_out <- new_time_origins[i] + years_in[chunk_inds_list[[chunki]]] - 1
                    } else if (ntime_out == length(years_wanted)) {
                        years_out <- new_time_origins[i] + years_wanted[dates_in_list[[chunki]]$inds] - 1
                    } else if (ntime_out == 1) { # through e.g. `cdo timsum`
                        years_out <- new_time_origins[i] + floor(mean(years_wanted)) - 1
                    } else { 
                        stop("not definedddddd")
                    }

                    # check new years for ncview minimum time origin `ncview_min_origin`
                    if (chunki == 1) {

                        # check if wanted origin is allowed:
                        # Error in utCalendar2: year -6993 is out of range of 
                        # the Gregorian calendar routines; must have year >= -4714
                        # internal error: udu_fmt_time can't convert to calendar value!
                        if (years_out[1] < ncview_min_origin) {
                            message("\n --> with user choice `new_time_origins[", i, "]` = ", new_time_origins[i], 
                                    ", the new first year is ", years_out[1], " < ", ncview_min_origin, "\n", 
                                    " --> this is not allowed by ncview")
                            if (!exists("new_time_origins_ncview_offsets")) {
                                new_time_origins_ncview_offsets <- rep(NA, t=nsettings)
                            }
                            new_time_origins_ncview_offsets[i] <- years_out[1] - ncview_min_origin
                            message(" --> set `new_time_origins_ncview_offsets[", i, "]` = ", 
                                    years_out[1], " - ", ncview_min_origin, " = ", 
                                    new_time_origins_ncview_offsets[i])
                            years_out <- years_out - new_time_origins_ncview_offsets[i]
                            new_time_origins[i] <- years_out[1]
                            message(" --> override user `new_time_origins[", i, "]` = ", new_time_origins[i], "\n")
                        }
                    } # if chunki == 1

                    # new years with possibly adapted `new_time_origins` for ncview 
                    if (ntime_out == ntime_in) {
                        years_out <- new_time_origins[i] + years_in[chunk_inds_list[[chunki]]] - 1
                    } else if (ntime_out == length(years_wanted)) {
                        years_out <- new_time_origins[i] + years_wanted[dates_in_list[[chunki]]$inds] - 1
                    } else if (ntime_out == 1) {
                        years_out <- new_time_origins[i] + floor(mean(years_wanted)) - 1
                    }
                    
                    # new months
                    months_out <- dates_in_list[[chunki]]$months
                    #if (ntime_out == ntime_in && grepl("<MM>", fpatterns[i])) {
                    #    # if months were detected from input file names, use them
                    #    months_out <- MM_in[chunk_inds_list[[chunki]]]
                    #} else {
                    #    # else use months from time dimension values from `cdo showdate`
                    #    months_out <- dates_in_list[[chunki]]$months
                    #}
                    
                    # new days
                    # todo: get days from file names like `years_in` and `MM_in` if present
                    days_out <- dates_in_list[[chunki]]$days
                    #if (length(days_out) != length(months_out)) {
                    #    # e.g. if input were monthly files and `cdo -yearsum` yields annual files
                    #    if (length(days_out) == length(years_wanted)) {
                    #        days_out_tmp <- rep(NA, t=length(months_out))
                    #        for (yi in 1:length(years_wanted)) {
                    #            yinds <- which(years_out == years_wanted[yi])
                    #            days_out_tmp[yinds] <- days_out[yi]
                    #        }
                    #        days_out <- days_out_tmp
                    #    } else {
                    #        stop("not definedddd")
                    #    }
                    #}

                    # check new dates for February 30
                    # `cdo showdate` of the result of `cdo yearsum` yields "YYYY-06-30"
                    # for all years, i.e. all days are "30". In combination with the months
                    # of the input file names (if present), wrong combinations like 
                    # "YYYY-02-30" are possible.
                    feb30_inds <- which(months_out == 2 & days_out == 30)
                    if (length(feb30_inds) > 0) {
                        days_out[feb30_inds] <- 28
                    }

                    # new dates as YYYY-MM-DD
                    #dates_out <- paste0(sprintf("%04i", years_out), "-", 
                    #                    sprintf("%02i", months_out), "-",
                    #                    sprintf("%02i", days_out))
                    dates_out <- paste0(years_out, "-", months_out, "-", days_out)
                    message("dates_out:")
                    ht(dates_out, n=25)

                    #if (chunki == 2) stop("asd")

                    # construct new dates used by nco ncap2
                    if (i == 1 && chunki == 1) {
                        if (!exists("new_time_units")) {
                            if (cdo_set_rel_time) { # relative time
                                #new_time_units <- paste0(sprintf("%04i", new_time_origins[i]), "-",
                                #                         sprintf("%02i", months_out[1]), "-",
                                #                         sprintf("%02i", days_out[1]))
                                #new_time_units <- paste0(new_time_origins[i], "-",
                                #                         months_out[1], "-",
                                #                         days_out[1])
                                #new_time_units <- paste0("days since ", dates_out[1], " 00:00:00")
                                #new_time_units <- paste0("days since ", dates_out[1])
                                #new_time_units <- paste0("days since ", new_time_origins[i], "-", months_out[1], "-", days_out[1])
                                new_time_units <- paste0("days since ", new_time_origins[i], "-01-01")
                                new_time_units <- rep(new_time_units, t=nsettings)
                            } else { # absolute time
                                new_time_units <- rep("day as %Y%m%d.%f", t=nsettings) # only allowed absolute time for cdo
                            }
                            message("\n`new_time_units` is not set and `cdo_set_rel_time`=", 
                                    cdo_set_rel_time, " --> use default \"", new_time_units, "\"")
                        } # if `new_time_units` was not set by user
                    } # only once at beginning

                    message("\nconstruct new times values with `new_time_units[", 
                            i, "]` = \"", new_time_units[i], "\" ...")
                    if (cdo_set_rel_time) {
                        dates_out_ncap <- as.POSIXlt(dates_out, tz="UTC")
                        tmp <- difftime(dates_out_ncap[1], 
                                        as.POSIXlt(paste0(new_time_origins[i], "-01-01"), tz="UTC"), 
                                        units="days")
                        tmp <- as.numeric(tmp) # days since first date
                        if (ntime_out > 1){
                            dates_out_ncap <- difftime(dates_out_ncap[2:length(dates_out_ncap)], 
                                                       dates_out_ncap[1:(length(dates_out_ncap) - 1)], 
                                                       units="days")
                            dates_out_ncap <- c(tmp, tmp + cumsum(as.numeric(dates_out_ncap)))
                        } else {
                            dates_out_ncap <- tmp
                        }

                    } else { # absolute time
                        if (new_time_units[i] == "years as %Y.%f") {
                            if (i == 1 && chunki == 1) {
                                dt_mon <- 1/12
                                ndays_per_month <- c(Jan=31, Feb=28, Mar=31, Apr=30, May=31, Jun=30,
                                                     Jul=31, Aug=31, Sep=30, Oct=31, Noc=30, Dec=31)
                                source("~/scripts/r/functions/leap_function.r")
                            }
                            dates_out_ncap <- years_out
                            dates_out_ncap <- dates_out_ncap + dt_mon*(months_out - 1)
                            for (mi in seq_len(length(unique(months_out)))) {
                                mi_number <- unique(months_out)[mi] # 1,2,..., or 12
                                mi_inds <- which(months_out == mi_number)
                                ndays_per_monthi <- rep(ndays_per_month[mi_number], t=length(mi_inds))
                                if (mi_number == 2) {
                                    leap_inds <- is.leap(years_out[mi_inds])
                                    if (any(leap_inds)) {
                                        ndays_per_monthi[leap_inds] <- ndays_per_monthi[leap_inds] + 1
                                    }
                                }
                                dates_out_ncap[mi_inds] <- dates_out_ncap[mi_inds] + 
                                    dt_mon*((days_out[mi_inds] - 1)/ndays_per_monthi)
                            }
                            dates_out_ncap <- sprintf("%f", dates_out_ncap) 
                        
                        } else { 
                            # default absolute time unit "day as %Y%m%d.%f" --> only allowed absolute time unit for cdo
                            stop("update")
                            dates_out_ncap <- paste0(sprintf("%04i", years_out), 
                                                     sprintf("%02i", months_out),
                                                     sprintf("%02i", days_out), ".0")
                        }
                    } # if cdo_set_rel_time or not
                    message("dates_out_ncap:")
                    ht(dates_out_ncap, n=25)
                    if (length(dates_out_ncap) != length(dates_out)) stop("thissss should not happen")

                    # save for every chunk
                    dates_out_list[[chunki]]$dates <- dates_out
                    dates_out_list[[chunki]]$years <- years_out
                    dates_out_list[[chunki]]$months <- months_out
                    dates_out_list[[chunki]]$days <- days_out
                    dates_out_list[[chunki]]$dates_ncap <- dates_out_ncap
                
                } # for chunki nchunks
                #stop("asd")

                message("\nmodify time dimension values with nco ncap2 ...")
                nco_fout_vec <- fout_vec 
                for (chunki in seq_len(nchunks)) {

                    message("\nmodify time dimension values of chunk ", chunki, "/", nchunks, " ...")
                    
                    nco_fout_vec[chunki] <- gsub(".nc", 
                                                 paste0("_origin_", new_time_origins[i], ".nc"),
                                                 fout_vec[chunki])
                    # if input has absolute time units but `new_time_units` shall be relative,
                    # the input needs to be converted from absolute to relative time units
                    # BEFORE setting new time values with nco ncap2
                    #cmd_cp_and_mv <- paste0("cp ", fout_vec[chunki], " ", nco_fout_vec[chunki])
                    message("\nneed to update this with a check\n")
                    cmd_cp_and_mv <- paste0(cdoprefix, " -r copy ", fout_vec[chunki], " ", nco_fout_vec[chunki])
                    cmd_ncap2 <- paste0(nco_ncap2, " -O -s 'time(:)={<dates_out_ncap>}; time@units=\"",
                                        new_time_units[i], "\"' ", nco_fout_vec[chunki], " ", 
                                        nco_fout_vec[chunki], " || echo error")
                    message("run 1: `", cmd_cp_and_mv, "`\n", 
                            "    2: `", cmd_ncap2, "`")
                    cmd_ncap2_tmp <- gsub("<dates_out_ncap>", 
                                          paste(dates_out_list[[chunki]]$dates_ncap, collapse=","), 
                                          cmd_ncap2)
                    
                    # check if nco ncap2 argument is too long
                    nchar_cmd_ncap2 <- nchar(cmd_ncap2_tmp)
                    message(" --> nco ncap2 argument list is ", nchar_cmd_ncap2, " characters long")
                    if (nchar_cmd_ncap2 > nco_nchar_max_arglist) {
                        
                        # run nco ncap2 command in chunks from file 
                        message("--> this is longer than `nco_nchar_max_arglist` = ", nco_nchar_max_arglist, 
                                " and would yield the error \"Argument list too long\"")
                        
                        # find nco chunks for this cdo chunk (chunki)
                        nchar_needed_per_date <- max(nchar(dates_out_list[[chunki]]$dates_ncap)) + 1
                        nchar_wout_files <- nchar(gsub("<dates_out_ncap>", "", cmd_ncap2))
                        nchar_avail <- nco_nchar_max_arglist - nchar_wout_files - 100 # ~100 characters more due to nco_tmpfile_vec
                        ndates_per_chunk <- floor(nchar_avail/nchar_needed_per_date)
                        cmd_nco_ncap2_chunk_inds <- seq(1, length(dates_out_list[[chunki]]$dates_ncap), b=ndates_per_chunk)
                        if (max(cmd_nco_ncap2_chunk_inds) < length(dates_out_list[[chunki]]$dates_ncap)) {
                            cmd_nco_ncap2_chunk_inds <- c(cmd_nco_ncap2_chunk_inds, length(dates_out_list[[chunki]]$dates_ncap))
                        } else if (max(cmd_nco_ncap2_chunk_inds) > length(dates_out_list[[chunki]]$dates_ncap)) {
                            stop("this should not happen")
                        }
                        nchunks_nco_ncap2 <- length(cmd_nco_ncap2_chunk_inds) - 1
                        message("--> run `nco ncap2` of ", length(dates_out_list[[chunki]]$dates_ncap), 
                                " dates in ", nchunks_nco_ncap2, " chunks of maximum ", ndates_per_chunk, 
                                " dates per chunk:")
                        
                        nco_tmpfile_vec <- rep(NA, t=nchunks_nco_ncap2)
                        nco_ncap2_list <- cmd_seltimestep_list <- nco_ncap2_chunk_inds_list <- vector("list", l=nchunks_nco_ncap2)
                        for (nco_ncap2_chunki in seq_len(nchunks_nco_ncap2)) {
                            
                            message("\nnco ncap2 chunk ", nco_ncap2_chunki, "/", nchunks_nco_ncap2, " of cdo chunk ",
                                    chunki, "/", nchunks, " cmd generation:")
                            
                            inds_chunki <- cmd_nco_ncap2_chunk_inds[nco_ncap2_chunki]:(cmd_nco_ncap2_chunk_inds[nco_ncap2_chunki+1] - 1)
                            if (nco_ncap2_chunki == nchunks_nco_ncap2) {
                                inds_chunki <- cmd_nco_ncap2_chunk_inds[nco_ncap2_chunki]:cmd_nco_ncap2_chunk_inds[nco_ncap2_chunki+1]
                            }
                            nco_tmpfile_vec[nco_ncap2_chunki] <- gsub(".nc", 
                                                                      paste0("_nco_ncap2_chunk_", nco_ncap2_chunki, 
                                                                             "_of_", nchunks_nco_ncap2, ".nc"), 
                                                                      nco_fout_vec[chunki])
                            cmd_seltimestep_list[[nco_ncap2_chunki]] <- paste0(cdoprefix, " seltimestep,", 
                                                                               inds_chunki[1], "/", inds_chunki[length(inds_chunki)], " ",
                                                                               fout_vec[chunki], " ", nco_tmpfile_vec[nco_ncap2_chunki]) 
                            message("run `", cmd_seltimestep_list[[nco_ncap2_chunki]], "`")
                            cmd_nco_ncap2_chunki <- gsub(nco_fout_vec[chunki], nco_tmpfile_vec[nco_ncap2_chunki], cmd_ncap2)
                            message("    `", cmd_nco_ncap2_chunki, "`")
                            cmd_nco_ncap2_chunki <- gsub("<dates_out_ncap>", 
                                                         paste(dates_out_list[[chunki]]$dates_ncap[inds_chunki], collapse=","),
                                                         cmd_nco_ncap2_chunki)
                                
                            nco_ncap2_list[[nco_ncap2_chunki]]$n <- length(inds_chunki)
                            nco_ncap2_list[[nco_ncap2_chunki]]$cmd <- cmd_nco_ncap2_chunki
                            nco_ncap2_chunk_inds_list[[nco_ncap2_chunki]] <- inds_chunki
                            message("timesteps ", min(inds_chunki), " to ", max(inds_chunki), " (", length(inds_chunki), 
                                    " dates from \"", dates_out_list[[chunki]]$dates[min(inds_chunki)], "\" to \"", 
                                    dates_out_list[[chunki]]$dates[max(inds_chunki)], "\", ",
                                    "nchar(nco ncap2 cmd) = ", nchar(nco_ncap2_list[[nco_ncap2_chunki]]$cmd), ") ...")
                        
                        } # for nco_ncap2_chunki

                        # run cdo seltimestep and nco ncap2
                        for (nco_ncap2_chunki in seq_len(nchunks_nco_ncap2)) {

                            message("\nnco ncap2 chunk ", nco_ncap2_chunki, "/", nchunks_nco_ncap2, 
                                    " of cdo chunk ", chunki, "/", nchunks, " cmd source:")
                            nco_ncap2_txt <- paste0(dirname(fout_vec[chunki]), "/tmp_nco_ncap2_", Sys.getpid(), "_chunk_", 
                                                    nco_ncap2_chunki, "_of_", nchunks_nco_ncap2, "_of_cdo_chunk_", 
                                                    chunki, "_of_", nchunks, ".txt")
                            writeLines(c(cmd_seltimestep_list[[nco_ncap2_chunki]], nco_ncap2_list[[nco_ncap2_chunki]]$cmd), 
                                       con=nco_ncap2_txt)
                            cmd_source <- paste0(". ", nco_ncap2_txt)
                            message("run `", cmd_source, "` ...")
                            system(cmd_source)

                            if (!file.exists(nco_tmpfile_vec[nco_ncap2_chunki])) { # output file exists?
                                stop("nco_tmpfile_vec[", nco_ncap2_chunki, "] = ", nco_tmpfile_vec[nco_ncap2_chunki], 
                                     " does not exist but it should")
                            }
                            
                            if (clean) system(paste0("rm -v ", nco_ncap2_txt))

                        } # for nco_ncap_chunki

                        # cat nco ncap2 chunks together
                        message("\n", "cat ", nchunks_nco_ncap2, " nco ncap2 chunks of cdo chunk ", 
                                chunki, "/", nchunks, " together:")
                        #if (new_time_units[i] == "day as %Y%m%d.%f") { 
                        if (F) {
                            # destroys non-default time formats not known by cdo
                            # also, makes "day as %Y%m%d.%f" --> "days since YYYY-MM-DD"
                            cmd_cat <- paste0(cdoprefix, " cat ", paste(nco_tmpfile_vec, collapse=" "), 
                                              " ", nco_fout_vec[chunki])
                        } else { # keeps non-default time formats
                            cmd_cat <- paste0(nco_ncrcat, " -O ", paste(nco_tmpfile_vec, collapse=" "), 
                                              " ", nco_fout_vec[chunki])
                        }
                        message("run `", cmd_cat, "` ...")
                        system(cmd_cat)

                    } else if (nchar_cmd_ncap2 <= nco_nchar_max_arglist) {
                      
                        # do not select time steps in nco ncap2 chunks but just make a copy and rename
                        system(cmd_cp_and_mv)
                        #system(paste0("cdo -r copy ", nco_fout_vec[chunki], " ~/tmp && mv ~/tmp ", nco_fout_vec[chunki])) 
                        system(cmd_ncap2_tmp)
                        # --> this call does not capture "-bash: /usr/bin/ncap2: Argument list too long"
                        # however, for cdo it does! dont know why

                    } # if nco ncap2 argument is too long

                } # for cdo chunki: possible chunks if argument is too long
       
                # new origin was applied to result of cdo selection and calculation
                # --> continue with these files
                fout_vec_old <- fout_vec
                fout_vec <- nco_fout_vec
            
            } else { # if dates_in_list[[1]]$years[1] == new_time_origins[i] --> new dates already have new_time_origin
                
                message("\nthe first input year of the ", ntime_out, " dates of `cdo ", 
                        cdocalc, " ", cdoselect, "` result is ", 
                        dates_in_list[[1]]$years[1], " == `new_time_origins[", i, "]` = ", 
                        new_time_origins[i], "\n --> no need to find new time dimension values ...")

            } # if dates_in_list[[1]]$years[1] != new_time_origins[i] --> new dates wanted
        
        } else { # if exists("new_time_origins") && is.finite(new_time_origins[i])
            
            message()
            if (!exists("new_time_origins")) {
                message("\n`new_time_origins` not set --> no need to set new time dimension values")
            } else {
                if (!is.finite(new_time_origins[i])) {
                    message("`new_time_origins` is set but `new_time_origins[", i, "]` = ", 
                            new_time_origins[i], " is not finite",
                            " --> cannot set new time dimension values")
                }
            }

        } # if exists("new_time_origins") && is.finite(new_time_origins[i]) 

        # cat chunks together (if needed) and remove temporary files
        if (nchunks > 1) {
            message("\n", "cat ", nchunks, " cdo chunks together ...")
            if (F) {
                # destroys non-default time formats not known by cdo
                # also, makes "day as %Y%m%d.%f" --> "days since YYYY-MM-DD"
                cmd_cat <- paste0(cdoprefix, " cat ", paste(fout_vec, collapse=" "), " ", fout)
            } else { # keeps non-default time formats
                cmd_cat <- paste0(nco_ncrcat, " -O ", paste(fout_vec, collapse=" "), " ", fout)
            }
            message("run `", cmd_cat, "` ...")
            system(cmd_cat)
        
        # or rename of necessary 
        } else {
            cmd_rename <- paste0("mv -v ", fout_vec, " ", fout)
            message("run `", cmd_rename, "` ...")
            system(cmd_rename)
        }

        # clean chunk files which were catted
        if (!file.exists(fout)) { # output file exists?
            stop("fout = ", fout, " does not exist but it should")
        }
        if (clean && nchunks > 1) {
            cmd_rm <- paste0("rm ", paste(fout_vec, collapse=" "))
            message(cmd_rm)
            system(cmd_rm)
        }

    } # if fout_exist_check (if output file already exists or not)

    # set relative time axis
    if (F && cdo_set_rel_time) {
        message("\n", "`cdo_set_rel_time`=T --> set relative time axis ...")
        tmpfile <- paste0(postpaths[i], "/tmp_reltime_", Sys.getpid())
        cmd <- paste0("cdo ", cdo_silent, " -r copy ", fout, " ", tmpfile, 
                      " && mv ", tmpfile, " ", fout)
        cmd <- paste0(cmd, " || echo error")
        message("run `", cmd, "`")
        system(cmd)
    } else {
        message("\n", "`cdo_set_rel_time`=F --> do not set relative time axis ...")
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
            attributes(elapsed[[i]])$units, " for ", modes[i], " calculation\n")

    # restore user options for next setting
    cdo_set_rel_time <- cdo_set_rel_time_old

} # for i nsettings

message("\n", "finished", "\n")

for (i in 1:nsettings) {
    message("setting ", i, "/", nsettings , "\n",
            "  ", datapaths[i], "/", fpatterns[i], "\n",
            "took ", elapsed[[i]], " ", 
            attributes(elapsed[[i]])$units, " for ", modes[i], " calculation\n")
}

if (!interactive()) {
    message("\n", "grep this log file for lines that begin with \"error\": grep -n \"^error\" <logfile>", 
            "\n", "be happy if nothing is returned", "\n")
}

