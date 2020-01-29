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

# check if cdo is available
if (!exists("cdo")) {
    cmd <- paste0("which cdo")
    message(cmd)
    cdo <- system(cmd, intern=T)
    if (!is.null(attributes(cdo)$status)) {
        stop("`which cdo` gave exist status ", attributes(cdo)$status)
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
    
# user input
fnml <- "namelist.post.r"
message("\n", "Read ", fnml, " ...")
source(fnml)

# Check user input and set defaults
message("\n", "Check user input ...")
exist_checks <- c("datapaths", "fpatterns", "fvarnames",
                  "models", "froms", "tos", "modes")
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
nsettings <- length(datapaths)
if (!exists("ftypes")) ftypes <- rep("f", t=nsettings)
if (!exists("suffixs")) suffixs <- rep(NA, t=nsettings)
if (!exists("froms_shift")) froms_shift <- rep(NA, t=nsettings)
if (any(!is.na(froms_shift)) && add_my_time == F) {
    stop("some of \"froms_shift\" is not NA but \"add_my_time\" is False")
}
if (!exists("codes")) codes <- rep(NA, t=nsettings)
if (!exists("areas_out")) areas_out <- rep("global", t=nsettings)
if (exists("postpaths")) {
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
} else { # postpaths does not exist
    postpaths <- paste(workpath, "post", models, modes, fvarnames, sep="/")
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

message("verbose = ", verbose)
message("clean = ", clean)
message("cdo = ", cdo)
message("cdo_version = ", paste(cdo_version, collapse="."))
message("cdo_silent = \"", cdo_silent, "\"")
message("cdo_force = ", cdo_force) #  -O necessary for ens<STAT>, merge, mergetime
message("cdo_OpenMP_threads = \"", cdo_OpenMP_threads, "\"") # OMP supported operators: https://code.mpimet.mpg.de/projects/cdo/wiki/OpenMP_support
message("cdo_wout_loop = ", cdo_wout_loop)
message("cdo_set_rel_time = ", cdo_set_rel_time)
message("cdo_run_from_script = ", cdo_run_from_script)
message("add_my_time = ", add_my_time)

elapsed <- vector("list", l=nsettings)
for (i in 1:nsettings) {

    tic <- Sys.time()
    message("\n", "*********** setting ", i, "/", nsettings, " *************")
    message("datapath = ", datapaths[i])
    message("fpattern = ", fpatterns[i])
    message("ftype = ", ftypes[i])
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

    # files of complete input time period
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

    # check file types
    # lazy approach: take everything after last . (dot) in file name
    filetype <- substr(basenames, regexpr("\\.[^\\.]*$", basenames) + 1, nchar(basenames))
    if (length(unique(filetype)) > 1) stop("more than 1 filetype in the file list is not implemented")
    filetype <- unique(filetype)
    if (!any(filetype == c("grb", "nc"))) {
        stop("filetype \"", filetype, "\" not implemented yet")
    }

    # get prefix: all parts from fpattern without <YYYY>, <MM>, *, etc. and ending
    prefix <- gsub("\\*", "", fpattern)
    prefix <- gsub(paste0(".", filetype), "", prefix)
    # append "_" as last character
    if (substr(prefix, nchar(prefix), nchar(prefix)) != "_") {
        prefix <- paste0(prefix, "_")
    }

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
    cmdselmon <- "" # default
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
            cmdselmon <- paste0("-selmon,", paste(months_per_file[selmon_season_inds], collapse=",")) 
            message("add `", cmdselmon, "` to cdo command ...")

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
    cdoprefix <- paste0(cdo, " ", cdo_silent, " ", cdo_OpenMP_threads)

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
    cdoconvert <- "" # default
    if (filetype == "grb") {
        if (models[i] == "echam6") {
            cdoconvert <- paste0(cdoconvert, " -t echam6")
        }
        cdoconvert <- paste0(cdoconvert, " -f nc")
    } # if grb
    
    # (n-2)-th command: calculation
    if (modes[i] == "timmean") {
        cdocalc <- "-timmean"
    } else if (modes[i] == "fldmean") {
        cdocalc <- "-fldmean"
    } else if (modes[i] == "volint") {
        cdocalc <- "-fldsum -vertsum"
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
            cdoselect <- paste0("-select,code=", codes[i])
        }
    } else if (filetype == "nc") {
        cdoselect <- paste0("-select,name=", fvarnames[i])
    }
    
    
    # select area
    if (areas_out[i] != "global") {
        stop("not yettt")
    }

    # run chained cdo operators
    # run only 1 cdo command for all files
    if (cdo_wout_loop == T) {
        message("\n", "`cdo_wout_loop=T` --> run only one cdo command with all files as input ...")
        # output fname
        fout <- paste0(postpaths[i],"/", prefix, 
                       ifelse(!is.na(suffixs[i]), paste0("_", suffixs[i]), ""),
                       modes[i],
                       ifelse(filetype == "grb", paste0("_selcode_", codes[i]), ""),
                       "_", fvarnames[i], 
                       ifelse(!is.na(levs_out[i]), paste0("_sellev_", levs_out[i]), ""),
                       "_", areas_out[i],
                       "_", season_names[i], "_", froms[i], "-", tos[i], 
                       ".nc")
        
        fout_exist_check <- file.access(fout, mode=0)
        if (fout_exist_check == 0 && cdo_force == F) { # running command not necessary
            message("fout=\n   ", fout, "\nalready exists and `cdo_force=F`. skip.")
        
        } else { # run command
            if (fout_exist_check == 0 && cdo_force == T) {
                message("fout=\n   ", fout, "\nalready exists and `cdo_force=T`. delete already existing file ...")
                check <- file.remove(fout)
                if (!check) warning("something went wrong deleting file ", fout)
            }
            
            # construct cdo command
            # cdo version must be >= 1.9.4 so that e.g. 
            # `cdo -fldmean -select,name=temp2 *.nc tmp.nc`, i.e. selection and calculation in one call, works
            if (cdo_version[1] <= 1 && cdo_version[2] <= 8 || 
                cdo_version[1] <= 1 && cdo_version[2] >= 9 && cdo_version[3] <= 3) { # cdo version < 1.9.4
                cdo_chain <- "old"
                tmpfile <- paste0(postpaths[i], "/tmp_selection_", Sys.getpid(), ".nc")
                message("\n", "cdo version ", paste(cdo_version, collapse="."), " < 1.9.4",
                        " --> have to run separate selection and calculation cdo commands:")
                cmd_select <- paste0(cdoprefix, " ", cdoconvert, " ",
                                     cmdselmon, " ", cdoselect,  
                                     " <files> ", tmpfile, " || echo error")
                cmd_calc <- paste0(cdoprefix, " ", cdocalc, " ", 
                                   tmpfile, " ", fout, " || echo error")
                message("run `", cmd_select, "`\n",
                        "    `", cmd_calc, "`")
                
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
                
                # check if cmd_select command is longer than nchar_max_arglist = 2612710
                nchar_cmd_select <- nchar(substr(cmd_select_tmp, start=nchar(cdo) + 1, stop=nchar(cmd_select_tmp))) 
                message("\n", "cdo selection argument list is ", nchar_cmd_select, " characters long")
                if (nchar_cmd_select > nchar_max_arglist) { # too long
                    message("--> this is longer than ", nchar_max_arglist, 
                            " and would yield the error \"Argument list too long\"")
                    nchar_needed_per_file <- nchar(paste0(datapaths[i], "/", files[1], " "))
                    nchar_wout_files <- nchar(gsub("<files>", "", cmd_select))
                    nchar_avail <- nchar_max_arglist - nchar_wout_files
                    nfiles_per_chunk <- floor(nchar_avail/nchar_needed_per_file)
                    cmd_select_chunk_inds <- seq(1, length(files), b=nfiles_per_chunk)
                    if (max(nfiles_per_chunk) < length(files)) {
                        cmd_select_chunk_inds <- c(cmd_select_chunk_inds, length(files))
                    } else if (max(nfiles_per_chunk) > length(files)) {
                        stop("this should not happen")
                    }
                    nchunks <- length(cmd_select_chunk_inds) - 1
                    message("--> run cdo selection of ", length(files), 
                            " files in ", nchunks, " chunks of maximum ", nfiles_per_chunk, 
                            " files per chunk:")
                    tmpfile_vec <- fout_vec <- rep(NA, t=nchunks)
                    cmd_select_list <- cmd_calc_list <- chunk_inds_list <- vector("list", l=nchunks)
                    for (select_chunki in seq_len(nchunks)) {
                        inds_chunki <- cmd_select_chunk_inds[select_chunki]:(cmd_select_chunk_inds[select_chunki+1] - 1)
                        if (select_chunki == nchunks) {
                            inds_chunki <- cmd_select_chunk_inds[select_chunki]:cmd_select_chunk_inds[select_chunki+1]
                        }
                        tmpfile_vec[select_chunki] <- gsub(".nc", paste0("_chunk_", select_chunki, ".nc"), tmpfile)
                        fout_vec[select_chunki] <- gsub(".nc", paste0("_chunk_", select_chunki, ".nc"), fout)
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
                        message("chunk ", select_chunki, "/", nchunks, ": files ",
                                min(inds_chunki), " to ", max(inds_chunki), " (", length(inds_chunki), 
                                " files from \"", files[min(inds_chunki)], "\" to \"", files[max(inds_chunki)], "\", ",
                                "nchar(cmd) = ", nchar(cmd_calc_list[[select_chunki]]$cmd), ") ...")
                    } # for select_chunki
                
                } else { # not too long
                    cmd_select_list <- cmd_calc_list <- vector("list", l=1)
                    cmd_select_list[[1]] <- list(cmd=cmd_select_tmp, n=length(files))
                    cmd_calc_list[[1]] <- list(cmd=cmd_calc)
                    nchunks <- length(cmd_select_list)
                    fout_vec <- fout
                }

            } else { # cdo version >= 1.9.4 
                cdo_chain <- "new"
                cmd <- paste0(cdoprefix, " ", cdoconvert, 
                              #" ", cmdcat, 
                              " ", cdocalc, " ", cmdselmon, " ", cdoselect,  
                              " <files> ", fout, " || echo error")
                message("\n", "run `", cmd, "`")
                
                # check if cmd command is longer than nchar_max_arglist = 2612710
                cmd_tmp <- gsub("<files>", paste(paste0(datapaths[i], "/", files), collapse=" "), cmd)
                nchar_cmd <- nchar(substr(cmd_tmp, start=nchar(cdo) + 1, stop=nchar(cmd_tmp))) 
                message("\n", "cdo argument list is ", nchar_cmd, " characters long")
                if (nchar_cmd_select > nchar_max_arglist) { # too long
                    message("--> this is longer than ", nchar_max_arglist, 
                            " and would yield the error \"Argument list too long\"")
                    stop("check for argument list too long")
                } else {
                    cmd_list <- list(cmd=cmd, n=length(files))
                    nchunks <- length(cmd_list)
                }

            } # if cdo_version >= 1.9.4 or not
           
            # run cdo command either from file (`$ . file` or via base::system(cmd)
            for (chunki in seq_len(nchunks)) { # for possible chunks if argument is too long
                message("cdo selection chunk ", chunki, "/", nchunks, ": ", appendLF=F)
                if (cdo_run_from_script) {
                    scriptname <- paste0(postpaths[i], "/cmd_", Sys.getpid(), "_chunk_", chunki, ".txt")
                    if (cdo_chain == "new") {
                        writeLines(cmd_list[[chunki]]$cmd, con=scriptname)
                        nfiles_per_chunk <- cmd_list[[chunki]]$n
                    } else if (cdo_chain == "old") {
                        writeLines(c(cmd_select_list[[chunki]]$cmd, cmd_calc_list[[chunki]]$cmd), con=scriptname)
                        nfiles_per_chunk <- cmd_select_list[[chunki]]$n
                    }
                    cmd_source <- paste0(". ", scriptname)
                    message("`", cmd_source, "` ...")
                    ticcmd <- Sys.time()
                    if (T) system(cmd_source)
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
           
            # change time values if the time dimension accordingly to the wanted years
            dates_in_list <- dates_out_list <- vector("list", l=nchunks)
            for (chunki in seq_len(nchunks)) {
            
                message("\ncheck time dimension values of output file ...")

                # get years of files
                cdo_showdate_file <- paste0(dirname(fout_vec[chunki]), "/cdo_showdate_",
                                            Sys.getpid(), "_chunk_", chunki, ".txt")
                cmd <- paste0(cdo, " showdate ", fout_vec[chunki],
                              " > ", cdo_showdate_file)
                message("run `", cmd, "`")
                system(cmd)
                cdo_dates <- readLines(cdo_showdate_file)
                cdo_dates <- strsplit(cdo_dates, " ")[[1]]
                if (any(cdo_dates == "")) {
                    cdo_dates <- cdo_dates[-which(cdo_dates == "")]
                }
                if (length(cdo_dates) == 0) {
                    stop("sth went wrong")
                }
                if (any(is.na(cdo_dates))) {
                    stop("--> there are NAs in cdo_dates")
                }
                message("`cdo showdate` yields ", length(cdo_dates), " dates:")
                ht(cdo_dates)
                cdo_years <- as.numeric(substr(cdo_dates, 1, 4))
                cdo_months <- as.numeric(substr(cdo_dates, 6, 7))
                cdo_days <- as.numeric(substr(cdo_dates, 9, 10))
                dates_in_list[[chunki]]$dates <- cdo_dates
                dates_in_list[[chunki]]$years <- cdo_years
                dates_in_list[[chunki]]$months <- cdo_months
                dates_in_list[[chunki]]$days <- cdo_days

                # check if changing time dimension values of output nc file is necessary
                if (cdo_years[1] != as.numeric(froms[i]) ||
                    cdo_years[length(cdo_years)] != as.numeric(tos[i])) {
                    if (cdo_years[1] != years_wanted[1]) {
                        message("cdo_years[1] = ", cdo_years[1], 
                                " != as.numeric(froms[", i, "]) = ", as.numeric(froms[i]))
                    }
                    if (cdo_years[length(cdo_years)] != as.numeric(tos[i])) {
                        message("cdo_years[", length(cdo_years), "] = ", cdo_years[length(cdo_years)], 
                                " != as.numeric(tos[", i, "]) = ", as.numeric(tos[i]))
                    }
                    message(" --> change time dimension values of output nc file ...")
                    
                    # todo: check for months and days ...
                    years_out <- years_wanted[1] + (cdo_years - cdo_years[1])
                    months_out <- cdo_months
                    days_out <- cdo_days

                    # new dates as dates
                    dates_out <- paste0(sprintf("%04i", years_out), "-", 
                                        sprintf("%02i", months_out), "-",
                                        sprintf("%02i", days_out))
                    message("dates_out:")
                    ht(dates_out)

                    # new dates as $y.%f:
                    if (new_time_units[i] == "years") {
                        if (exists("new_time_origins") && is.finite(new_time_origins[i])) {
                            message(" --> `new_time_origins[", i, "]` = ", new_time_origins[i], 
                                    "; use this as new origin ...")
                            dates_out_abs <- new_time_origins[i] - 2 + years_wanted[1] + (cdo_years - cdo_years[1])
                            dates_out_abs <- dates_out_abs + cdo_months/12
                        } else {
                            message("--> `new_time_origins[", i, "]` is not set; use froms[", i, "] = ", 
                                    froms[i], " as new origin ...")
                            dates_out_abs <- years_wanted[1] + (cdo_years - cdo_years[1])
                            dates_out_abs <- dates_out_abs + (cdo_months - 1)/12
                        }
                        for (mi in seq_len(length(unique(cdo_months)))) {
                            monthi_ind <- unique(cdo_months)[mi]
                            monthi_name <- month.abb[monthi_ind]
                            monthi_inds <- which(cdo_months == monthi_name)
                            dates_out_abs[monthi_inds] <- dates_out_abs[monthi_inds] + 
                                            ((cdo_months[monthi_inds] - 1)/12)/(cdo_days[monthi_inds] - 1)
                        }
                    } else {
                        stop("other new time units are not defined yet")
                    }
                    message("dates_out_abs:")
                    ht(dates_out_abs)

                    # check if time can be relative or not
                    #new_time_origins
                    if (cdo_set_rel_time) {
                        if (dates_out_abs[1] < -4714) {
                            # $ncview out.nc
                            # Error in utCalendar2: year -6993 is out of range of 
                            # the Gregorian calendar routines; must have year >= -4714
                            # internal error: udu_fmt_time can't convert to calendar value!
                            message("\n`cdo_set_rel_time`=T but dates_out_abs[1] = ", dates_out_abs[1], "\n",
                                    " --> cannot set relative time since for ncview the minimum year must be >= -4714\n",
                                    " --> set `cdo_set_rel_time=F` and continue ...\n")
                            cdo_set_rel_time <- F
                        }
                    }

                    # save for every chunk
                    dates_out_list[[chunki]]$dates <- dates_out
                    dates_out_list[[chunki]]$dates_abs <- dates_out_abs
                    dates_out_list[[chunki]]$years <- years_out
                    dates_out_list[[chunki]]$months <- months_out
                    dates_out_list[[chunki]]$days <- days_out

                    # apply new time
                    nco_ncap2 <- paste0("ncap2 -O -s 'time(:)={<dates_out_abs>}; ",
                                        "time@units=\"", new_time_units[i])
                    if (cdo_set_rel_time) {
                        nco_ncap2 <- paste0(nco_ncap2, " since ", 
                                            sprintf("$04i", new_time_origins[i]), "-",
                                            sprintf("%02i", dates_out_list[[1]]$months[1]), "-", 
                                            sprintf("%02i", dates_out_list[[1]]$days[1]))
                    } else {
                        nco_ncap2 <- paste0(nco_ncap2, " as %Y.%f")
                    }
                    nco_ncap2 <- paste0(nco_ncap2, "\"' ", 
                                        fout_vec[chunki], " ", fout_vec[chunki], " || echo error")
                    message("run `", nco_ncap2, "`")
                    nco_ncap2 <- gsub("<dates_out_abs>", paste0(sprintf("%f", dates_out_abs), collapse=","), nco_ncap2)
                    system(nco_ncap2)

                    cmd_rm <- paste0("rm -v ", cdo_showdate_file)
                    system(cmd_rm)
                    
                    # change ref time aka origin
                    #cmd <- paste0(cdo, " -setreftime,'", dates_out_list[[1]]$dates[1], "','00:00:00',1year ", 
                    #              fout, " ", tmpfile, " && mv ", tmpfile, " ", fout)
                    #cmd <- paste0(cmd, " || echo error")
                    #message("run `", cmd, "`")
                    #stop("asdadasd")
                    #system(cmd)

                } else {
                    dates_out_list[[chunki]] <- dates_in_list[[chunki]]
                
                } # if changing time dimension values of output nc file is necessary  

            } # for chunki: possible chunks if argument is too long

            # cat chunks together (if needed) and remove temporary files
            if (nchunks > 1) {
                message("\n", "cat chunks together ...")
                cmd_cat <- paste0(cdoprefix, " cat ", paste(fout_vec, collapse=" "), " ", fout)
                message("run `", cmd_cat, "` ...")
                system(cmd_cat)
            }

            # clean chunk files which were catted
            if (!file.exists(fout)) { # output file exists?
                stop("fout = ", fout, " does not exist but it should")
            }
            if (nchunks > 1 && clean) {
                cmd_rm <- paste0("rm ", paste(fout_vec, collapse=" "))
                message(cmd_rm)
                system(cmd_rm)
            }

        } # if fout_exist_check (if output file already exists or not)

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
                           "_", fvarnames[i], 
                           ifelse(!is.na(levs_out[i]), paste0("_sellev_", levs_out[i]), ""),
                           "_", areas_out[i],
                           "_", stamp,
                           "_tmp.nc")
            fout_exist_check <- file.access(fout, mode=0)
            if (fout_exist_check == 0 && cdo_force == F) { # running command not necessary
                if (fi == 1) message("fout=\n   ", fout, "\nalready exists and `cdo_force=F`. skip.")
            } else { # run command
                if (fout_exist_check == 0 && cdo_force == T) {
                    if (fi == 1) message("fout\n  ", fout, "\nalready exists and `cdo_force=T`. delete already existing file ...")
                    check <- file.remove(fout)
                    if (!check) warning("something went wrong deleting file ", fout)
                }
                cmd <- paste0(cdoprefix, " ", cdoconvert, " ", cdocalc, " ", cdoselect, " ", files[fi], " ", fout, " || echo error")
                if (fi == 1) message("run `", cmd, "`")
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
                       "_", fvarnames[i], 
                       ifelse(!is.na(levs_out[i]), paste0("_sellev_", levs_out[i]), ""),
                       "_", areas_out[i],
                       "_", season_names[i], "_", froms[i], "-", tos[i], 
                       ".nc")
        if (file.access(fout, mode=0) == 0 && cdo_force == F) { # running command not necessary
            message("fout=\n   ", fout, "\nalready exists and `cdo_force=F`. skip.")
        } else { # run command

            if (file.access(fout, mode=0) == 0 && cdo_force == T) {
                message("fout=\n   ", fout, "\nalready exists and `cdo_force=T`. delete already existing file ...")
                check <- file.remove(fout)
                if (!check) warning("something went wrong deleting file ", fout)
            }
            
            cmd <- paste0("cdo ", cdo_silent, " ", cdo_OpenMP_threads, " ", cmdcat, " <fcalcs> ", fout)
            cmd <- paste0(cmd, " || echo error")
            message("run `", cmd, "`")
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
    if (cdo_set_rel_time) {
        message("\n", "`cdo_set_rel_time`=T --> set relative time axis ...")
        tmpfile <- paste0(postpaths[i], "/tmp_reltime_", Sys.getpid())
        cmd <- paste0("cdo ", cdo_silent, " -r copy ", fout, " ", tmpfile, 
                      " && mv ", tmpfile, " ", fout)
        cmd <- paste0(cmd, " || echo error")
        message("run `", cmd, "`")
        system(cmd)
        
    } else {
        message("\n", "`cdo_set_rel_time`=F --> no not set relative time axis ...")
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

if (!interactive()) {
    message("\n", "grep this log file for lines that begin with \"error\": grep -n \"^error\" <logfile>", 
            "\n", "be happy if nothing is returned", "\n")
}

