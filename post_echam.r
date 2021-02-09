# r

rm(list=ls()); graphics.off()

options(warn=2) # stop on warnings
#options(warn=0) # back to default

# load necessary libraries
requirements <- readLines("requirements_post.txt")
for (r in requirements) if (substr(r, 1, 1) != "#") library(r, character.only=T)

# load helper functions of this repo
message("\nload helper_functions.r ...")
source("helper_functions.r")

# get host options
host <- get_host()

# todo: how to load helper functions from another repo without the subrepo hassle?
if (file.exists(paste0(host$homepath, "/functions/myfunctions.r"))) {
    message("\nload ", host$homepath, "/functions/myfunctions.r ...")
    source(paste0(host$homepath, "/functions/myfunctions.r"))
    # post_echam.r needs the myfunctions.r functions: 
    # ht(), make_posixlt_origin(), is.leap(), tryCatch.W.E(), identical_list(), cdo_get_filetype()
} else {
    stop("\ncould not load ", host$homepath, "/functions/myfunctions.r")
}

# load user input namelist for this repo
message("\nload and check namelist.post.r ...")
source("namelist.post.r")

# Check user input and set defaults
message("verbose = ", verbose)
message("clean = ", clean)

# check if cdo is available
if (!exists("cdo")) {
    cmd <- paste0("which cdo")
    message("`cdo` not set by user -> check if cdo binary can be found: run `", cmd, "`")
    cdo <- system(cmd, intern=T)
    if (!is.null(attributes(cdo)$status)) {
        stop("`which cdo` gave exit status ", attributes(cdo)$status)
    } else {
        message("found ", appendLF=F)
    }
}
message("cdo = ", cdo)
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
message("cdo_version = ", paste(cdo_version, collapse="."))
message("cdo_silent = \"", cdo_silent, "\"")
message("cdo_select_no_history = \"", cdo_select_no_history, "\"")
message("cdo_force = ", cdo_force) #  -O necessary for ens<STAT>, merge, mergetime
message("cdo_OpenMP_threads = \"", cdo_OpenMP_threads, "\"") # OMP supported operators: https://code.mpimet.mpg.de/projects/cdo/wiki/OpenMP_support
message("cdo_set_rel_time = ", cdo_set_rel_time)
message("cdo_run_from_script = ", cdo_run_from_script)

# check if necessary user variables exist
exist_checks <- c("datapaths", "fpatterns", "fvarnames",
                  "models", "froms", "tos", "modes")
if (!all(sapply(exist_checks, exists))) {
    missing_vars <- !sapply(exist_checks, exists)
    stop("\nyou have to define the variable", 
         ifelse(length(which(missing_vars)) > 1, "s", ""),
         " \"", paste0(names(missing_vars)[missing_vars], collapse="\", \""), "\"")
}
if (any(file.access(datapaths, mode=4) != 0)) { # check read permission
    nonreadable_paths <- which(file.access(datapaths, mode=2) != 0)
    stop("\nnot existing/no permission to read of datapath", ifelse(length(nonreadable_paths) > 1, "s", ""),
         " '", paste0(datapaths[nonreadable_paths], collapse="', '"), "'.")
}
datapaths <- normalizePath(datapaths)
nsettings <- length(datapaths)
#if (!exists("ftypes")) ftypes <- rep("f", t=nsettings)
if (!exists("prefixes")) prefixes <- rep(NA, t=nsettings)
if (!exists("codes")) codes <- rep(NA, t=nsettings)
if (!exists("areas_out")) {
    if (exists("areas_out_list")) {
        if (is.list(areas_out_list)) {
            areas_out <- rep(NA, t=length(areas_out_list))
            for (i in seq_along(areas_out_list)) {
                if (!is.null(areas_out_list[[i]]$name)) {
                    areas_out[i] <- areas_out_list[[i]]$name
                } else {
                    stop("`areas_out_list[[", i, "]]´ is empty")
                }
            }
        } else {
            stop("`areas_out_list` is given but not a list. dont know how to interpret")
        }
    } else {
        areas_out <- rep("global", t=nsettings)
    }
}
# check if postpaths can be created/have writing rights
if (!exists("postpaths")) {
    postpaths <- paste(host$workpath, "post", models, modes, fvarnames, sep="/")
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
            message("create postpath \"", i, "\"")
            dir.create(i, recursive=T, showWarnings=F)
        } else { # no dir creation permission
            stop("have no write permission to create postpath \"", i, "\". error message:\n",
                 permission_check)
        }
    }
}
if (exists("season_names")) {
    if (exists("season_inds")) {
        # check is user provided season_names and season_inds fit to each other
        stop("todo")
    } else if (!exists("season_inds")) {
        # find season_inds based on given season_names 
        season_inds <- vector("list", l=nsettings)
        for (i in seq_len(nsettings)) {
            if (season_names[i] == "annual") {
                season_inds[[i]] <- 1 # just placeholder
            } else {
                message("todo")
                season_inds[[i]] <- 1:12
            }
        }
    }
} else if (!exists("season_names")) {
    if (exists("season_inds")) {
        # find season_names based on given season_inds
    } else if (!exists("season_inds")) {
        # no season_names or season_inds given. use default: annual
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
}
if (!exists("sellevels")) sellevels <- rep(NA, t=nsettings)
if (!exists("sellevsidx")) sellevsidx <- rep(NA, t=nsettings)
inds <- which(!is.na(sellevels) & !is.na(sellevsidx))
if (length(inds) > 0) {
    stop("sellevels[", paste(inds, collapse=","), "] = ", paste(sellevels[inds], collapse=", "),
         " and sellevsidx[", paste(inds, collapse=","), "] = ", paste(sellevsidx[inds], collapse=", "), 
         ". decide for one")
}
if (!exists("lev_fnames")) lev_fnames <- rep("", t=nsettings)
if (any(!is.na(sellevels))) {
    lev_fnames[which(!is.na(sellevels))] <- paste0("_sellevel_", sellevels[which(!is.na(sellevels))])
}
if (any(!is.na(sellevsidx))) {
    lev_fnames[which(!is.na(sellevsidx))] <- paste0("_sellevidx_", sellevsidx[which(!is.na(sellevsidx))])
}

cdo_set_rel_time_old <- cdo_set_rel_time # for next setting i

if (!exists("cdo_before_calcs")) cdo_before_calcs <- rep("", t=nsettings)

# check for new times if wanted
message("check if `new_date_list` is set and correct ... ", appendLF=F)
if (!exists("new_date_list")) {
    message("-> `new_date_list` is not set. user does not want new time values.")
    new_date_list <- NULL
} else {
    message("-> `new_date_list` is not NULL")
    for (i in 1:nsettings) {
        if (is.null(new_date_list[[i]]$years)) {
            # user did not provide final years to use 
            if (!is.null(new_date_list[[i]]$year_origin) && 
                is.null(new_date_list[[i]]$use)) {
                new_date_list[[i]]$use <- "filename" # default
                message("`new_date_list[[", i, "]]$year_origin` = ", new_date_list[[i]]$year_origin, 
                        " but its not mentioned which numbers to use for the new years. set `new_date_list[[", i, "]]$use = \"",
                        new_date_list[[i]]$use, "\" (default) and continue ...")
            }
            if (!is.null(new_date_list[[i]]$use) && 
                is.null(new_date_list[[i]]$year_origin)) {
                new_date_list[[i]]$year_origin <- 0 # default
                message("`new_date_list[[", i, "]]$use` = ", new_date_list[[i]]$use,
                        " but the origin of the new years is not mentioned. set `new_date_list[[", i, "]]$year_origin = ",
                        new_date_list[[i]]$year_origin, " (default) and continue ...")
            }
        } else if (!is.null(new_date_list[[i]]$years)) {
            # user did provide final years to use 
            message("`new_date_list[[", i, "]]$years` (", length(new_date_list[[i]]$years), " entries) = ")
            ht(new_date_list[[i]]$years)
            if (!is.null(new_date_list[[i]]$use)) {
                message(" but also `new_date_list[[", i, "]]$use` = ", new_date_list[[i]]$use, 
                        " is given. the latter will be ignored.")
            }
            if (!is.null(new_date_list[[i]]$year_origin)) {
                message(" but also `new_date_list[[", i, "]]$year_origin` = ", new_date_list[[i]]$year_origin, 
                        " is given. the latter will be ignored.")
            }
        } # final years are given or not
    } # for i nsettings

    # check if ncap2 is available
    if (!exists("nco_ncap2")) {
        cmd <- paste0("which ncap2")
        message("   `nco_ncap2` not set by user -> check if ncap2 binary can be found to set new times of nc file: run `", cmd, "`")
        nco_ncap2 <- system(cmd, intern=T)
        if (!is.null(attributes(nco_ncap2)$status)) {
            stop("`which ncap2` gave exit status ", attributes(nco_ncap2)$status)
        }
    }
    message("   ncap2 = ", nco_ncap2)

    # check if ncrcat is available
    if (!exists("nco_ncrcat")) {
        cmd <- paste0("which ncrcat")
        message("   `nco_ncrcat` not set by user -> check if ncrcat binary can be found to eventually cat chunks with new times of nc file: run `", cmd, "`")
        nco_ncrcat <- system(cmd, intern=T)
        if (!is.null(attributes(nco_ncrcat)$status)) {
            stop("`which ncrcat` gave exit status ", attributes(nco_nrcat)$status)
        }
    }
    message("   ncrcat = ", nco_ncrcat)

} # if new_date_list provided or not

# check wiso stuff
if (!exists("wiso_smow_files")) wiso_smow_files <- rep(NA, t=nsettings)
if (!exists("wiso_code_tables")) wiso_code_tables <- rep(NA, t=nsettings)
if (!exists("wiso_paramater_tables")) wiso_paramater_tables <- rep(NA, t=nsettings)

# check mpiom stuff
if (any(models == "mpiom1")) {
    message("\nsome of provided `models` are \"mpiom1\" --> load mpiom/mpiom_functions.r ...")
    source("mpiom/mpiom_functions.r")
}

# special filename patterns
special_patterns <- c("<YYYY>", "<YYYY_from>", "<YYYY_to>", 
                      "<MM>", "<MM_from>", "<MM_to>",
                      "<DD>", "<DD_from>", "<DD_to>")

message("\nnameslist.post.r checks finished. start running post_echam.r for ", nsettings, 
        " model setup", ifelse(nsettings > 1, "s", ""), " ...")

# do for every model setting
elapsed <- vector("list", l=nsettings)
for (i in 1:nsettings) {

    tic <- Sys.time()
    message("\n", "*********** setting ", i, "/", nsettings, " *************")
    message("datapath = ", datapaths[i])
    message("model = ", models[i])
    message("fpattern = ", fpatterns[i])
    #message("ftype = ", ftypes[i])
    message("fvarname = ", fvarnames[i])
    if (!is.na(codes[i])) message("code = ", codes[i])
    message("postpath = ", postpaths[i])
    message("prefix = ", prefixes[i])
    message("mode = ", modes[i])
    message("from = ", froms[i])
    message("to = ", tos[i])
    message("season_name = ", season_names[i])
    message("season_inds = ", paste0(season_inds[[i]], collapse=","))
    message("area_out = ", areas_out[i])
    if (!is.na(sellevels[i])) message("sellevel = ", sellevels[i])
    if (!is.na(sellevsidx[i])) message("sellevidx = ", sellevsidx[i])
    if (!is.null(new_date_list[[i]])) {
        if (!is.null(new_date_list[[i]]$years)) {
            message("new_date_list[[", i, "]]$years = ")
            ht(new_date_list[[i]]$years)
        } else {
            message("new_date_list[[", i, "]]$use = ", new_date_list[[i]]$use, "\n",
                    "new_date_list[[", i, "]]$year_origin = ", new_date_list[[i]]$year_origin)
        }
    }

    # output fname
    fout <- paste0(postpaths[i], "/", 
                   ifelse(!is.na(prefixes[i]), paste0(prefixes[i], "_"), ""),
                   models[i], "_", modes[i],
                   #todo: include code number in fout or not?
                   #ifelse(!is.na(codes[i]), paste0("_selcode_", codes[i]), ""),
                   "_", fvarnames[i], 
                   lev_fnames[i], 
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

        # replace potential <pattern> in `fpatterns[i]`
        message("\ncheck `fpatterns[", i, "]` =\n   \"", fpatterns[i], "\"\nfor \"<...>\" patterns to replace ...")
        sub_list <- NULL # default
        pattern_inds_open <- gregexpr("<", fpatterns[i])[[1]] # returns n inds if found or -1 
        pattern_inds_closed <- gregexpr(">", fpatterns[i])[[1]]
        if (!all(pattern_inds_open == -1) || !all(pattern_inds_closed == -1)) {
            if (length(pattern_inds_open) != length(pattern_inds_closed)) {
                stop("in `fpatterns[", i, "]` you provided ", length(pattern_inds_open), 
                     " opening brackets \"<\" to indicate a file pattern to replace but ", 
                     length(pattern_inds_closed), " closing brackets \">\". there must be a \"<\" for every \">\".")
            }
            n_patterns_per_file <- length(pattern_inds_open)
            sub_list <- vector("list", l=n_patterns_per_file)
            for (pati in seq_len(n_patterns_per_file)) {
                pattern <- substr(fpatterns[i], pattern_inds_open[pati], pattern_inds_closed[pati]) # pattern to replace with leading "<" and trailing ">"
                sub_list[[pati]]$pattern <- pattern
                sub_list[[pati]]$pattern_inds <- c(pattern_inds_open[pati], pattern_inds_closed[pati])
                # special patterns: replace <YYYY*>, <MM*>, etc. with "*"
                if (pattern %in% special_patterns) {
                    message("   replace special pattern \"", pattern, "\" by \"?\"")
                    sub_list[[pati]]$replacement <- "?"
                    if (any(pattern == c("<YYYY>", "<YYYY_from>", "<YYYY_to>"))) {
                        sub_list[[pati]]$replacement_times <- 4
                    } else if (any(pattern == c("<MM>", "<MM_from>", "<MM_to>"))) {
                        sub_list[[pati]]$replacement_times <- 2
                    } else if (any(pattern == c("<DD>", "<DD_from>", "<DD_to>"))) {
                        sub_list[[pati]]$replacement_times <- 2
                    } else {
                        stop("not pattern \"", pattern, "\" not defined yet")
                    }
                # all other patterns: replace <pattern> by value of object in the current work space named `pattern`
                } else { 
                    obj <- substr(pattern, 2, nchar(pattern)-1) # pattern string without leading "<" and trailing ">"
                    if (exists(eval(obj))) { # variable with the name of the pattern exists
                        eval(parse(text=paste0("length_of_obj <- length(", obj, ")")))
                        if (length_of_obj == nsettings) { # assume that the entry of setting i should be replaced
                            eval(parse(text=paste0("replacement <- ", obj, "[i]")))
                        } else {
                            eval(parse(text=paste0("replacement <- ", obj)))
                        }
                    } else { # no such a variable exists
                        stop("   did not find an object named \"", obj, "\" to replace the pattern \"", 
                             pattern, "\" in `fpatterns[", i, "]`. dont know how to interpret this case.")
                    }
                    message("   replace pattern \"", pattern, "\" by \"", replacement, "\"")
                    sub_list[[pati]]$replacement <- replacement
                    #sub_list[[pati]]$replacement_times <- nchar(replacement)
                    sub_list[[pati]]$replacement_times <- 1 # dont repeat replacement pattern in this default case
                } # special or default <pattern>
                sub_list[[pati]]$replacement <- paste(rep(sub_list[[pati]]$replacement, t=sub_list[[pati]]$replacement_times),
                                                      collapse="")
                sub_list[[pati]]$replacement_length <- nchar(sub_list[[pati]]$replacement)
                sub_list[[pati]]$nchar_diff <- sub_list[[pati]]$replacement_length - nchar(sub_list[[pati]]$pattern)
            } # for pati n <patterns> to replace

            # apply replacements of patterns one by one (thats why `sub()` and not `gsub()`; the latter would replace all occurences at once)
            fpattern <- fpatterns[i]
            for (pati in seq_along(sub_list)) {
                fpattern <- sub(pattern=sub_list[[pati]]$pattern, replacement=sub_list[[pati]]$replacement, x=fpattern)
            }
            message("   -> \"", fpattern, "\"")
            
            # find replacement inds
            for (pati in seq_along(sub_list)) {
                if (pati == 1) { # first pattern
                    sub_list[[pati]]$replacement_inds <- sub_list[[pati]]$pattern_inds[1]
                } else if (pati == length(sub_list)) { # last
                    sub_list[[pati]]$replacement_inds <- sub_list[[pati]]$pattern_inds[1] + sum(sapply(sub_list[1:(pati-1)], "[[", "nchar_diff"))
                } else { # all other patterns in between first and last
                    sub_list[[pati]]$replacement_inds <- sub_list[[pati]]$pattern_inds[1] + sum(sapply(sub_list[1:(pati-1)], "[[", "nchar_diff"))
                }
                sub_list[[pati]]$replacement_inds[2] <- sub_list[[pati]]$replacement_inds[1] + sub_list[[pati]]$replacement_length - 1
            }
        } else {
            message("   no \"<...>\" strings detected ...")
            fpattern <- fpatterns[i]
        } # if user provided <patterns>

        # find files based on datapath and fpattern with potential <patterns> applied
        # todo: search for files and links and compare
        #cmd <- paste0("ls ", datapaths[i], "/", fpattern) 
        # --> this may result in `-bash: /bin/ls: Argument list too long`
        #cmd <- paste0("find ", datapaths[i], " -type ", ftypes[i], " -name \"", fpattern, "\" -printf \"%f\\n\" | sort")
        cmd <- paste0("find ", datapaths[i], " -name \"", fpattern, "\" -printf \"%f\\n\" | sort")
        # --> `find` does not have this limit 
        message("\nrun `", cmd, "` ...")
        ticcmd <- Sys.time()
        files <- system(cmd, intern=T)
        toccmd <- Sys.time()
        if (length(files) == 0) stop("Zero files found. Are `datapaths` and `fpatterns` correct?")
        elapsedcmd <- toccmd - ticcmd
        message("`find` of ", length(files), " files took ", elapsedcmd, " ", attributes(elapsedcmd)$units) 

        if (datapaths[i] == "/ace/user/stschuet/Hol-T_echam5_wiso_links") {
            if (any(files == "Hol-T_echam5_wiso_link_555006")) {
                message("\nspecial: remove Hol-T_echam5_wiso_link_555006 from steffens links ...")
                files <- files[-which(files == "Hol-T_echam5_wiso_link_555006")]
            }
        }

        # separate into dirname and basename
        df <- data.frame(files, stringsAsFactors=F)

        # show found files
        if (verbose > 0) {
            message("\nfound ", length(files), " file", 
                    ifelse(length(files) > 1, "s", ""), ":")
            if (length(files) > 1) {
                ht(df, n=30)
            } else {
                print(df)
            }
        }
        
        # identify years/months/etc. of found files based on <YYYY*>, <MM*>, etc. patterns if given or, alternatively, based on `cdo showdate`
        if (any(special_patterns %in% sapply(sub_list, "[[", "pattern"))) {
            message("\nfind years/months/etc. of based on <YYYY*>, <MM*>, etc. patterns of found files ...")
            
            special_patterns_in_filenames <- special_patterns[which(special_patterns %in% sapply(sub_list, "[[", "pattern"))]
            for (pati in seq_along(special_patterns_in_filenames)) {
                pattern_inds <- which(sapply(sub_list, "[[", "pattern") == special_patterns_in_filenames[pati])
                pattern_list <- vector("list", l=length(pattern_inds))
                for (patj in seq_along(pattern_inds)) {
                    pattern_list[[patj]] <- substr(files, 
                                                   sub_list[[pattern_inds[patj]]]$replacement_inds[1],
                                                   sub_list[[pattern_inds[patj]]]$replacement_inds[2])
                }
                # check if all found values for <YYYY*>, <MM*>, etc. patterns are identical
                if (identical_list(pattern_list)) {
                    df[sub(">", "", sub("<", "", special_patterns_in_filenames[pati]))] <- pattern_list[[1]]
                    if (special_patterns_in_filenames[pati] == "<YYYY>") {
                        years_filenames <- as.integer(df$YYYY)
                    } else if (special_patterns_in_filenames[pati] == "<YYYY_from>") {
                        years_filenames_from <- as.integer(df$YYYY_from)
                    } else if (special_patterns_in_filenames[pati] == "<YYYY_to>") {
                        years_filenames_to <- as.integer(df$YYYY_to)
                    } else if (special_patterns_in_filenames[pati] == "<MM>") {
                        months_filenames <- as.integer(df$MM)
                    }
                    # todo: MM_from, MM_to
                } else {
                    message("pattern \"", special_patterns_in_filenames[pati], "\" occurs ", length(pattern_list), 
                            " times in `fpatterns[", i, "]` but their respective values differ from each other:")
                    for (patj in seq_along(pattern_list)) ht(pattern_list[[patj]])
                    stop("dont know how to interpret this. maybe changing to one of \"", 
                         paste(special_patterns, collapse="\", \""), "\" helps")
                }
            } # for pati all special patterns in fnames

        } else { # no <YYYY*>, <MM*>, etc. special patterns given by user

            if (length(files) == 1) { # assume that user wants to use one specific file
                years_filenames <- froms[i]:tos[i]

            } else {
                message("\nno <YYYY*> or <MM*> patterns provided --> find years/months/etc. of based on `cdo showdate` of found files ...")
                years_filenames <- vector("list", l=length(files))
                for (fi in seq_along(files)) {
                    cmd <- paste0(cdo, " showdate ", datapaths[i], "/", files[fi])
                    message("run `", cmd, "`")
                    dates <- system(cmd, intern=T)
                    dates <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", dates, perl=T) # remove double and leading blanks
                    dates <- strsplit(dates, " ")[[1]]
                    if (verbose) {
                        message("\n", length(dates), " cdo dates of this file:")
                        ht(dates, n=20)
                    }
                    years_filenames[[fi]] <- substr(dates, 1, 4)
                }
                years_filenames <- as.numeric(unlist(years_filenames))
            } # if length(files) == 1 or not

        } # if <YYYY*> or <MM*> patterns are given by user or not
       
        # show years, months, etc. of found files
        if (verbose > 0) {
            message("\nfound years/months/etc. based on ", length(files), " file", 
                    ifelse(length(files) > 1, "s", ""), ":")
            if (length(files) > 1) {
                ht(df)
            } else {
                print(df)
            }
        }
            
        # special treatment: if only YYYY_from and YYYY_to were provided, but not YYYY, derive `years_filenames` now
        if (!exists("years_filenames")) {
            if (exists("years_filenames_from") && exists("years_filenames_to")) {
                message("\nderive input years based on \"<YYYY_from>\" and \"<YYYY_to>\" in a consecutive order ...") 
                years_filenames <- as.vector(mapply(function(x,y) x:y, years_filenames_from, years_filenames_to))
                years_filenames <- unlist(years_filenames) # is list if not all x[i]:y[i] sequences from the line above are of same length
                if (any(diff(years_filenames) < 0)) {
                    stop("derived `years_filenames` are not monotonically increasing")
                }
            } else {
                stop("this is not implemented yet")
            }
            message("derived ", length(years_filenames), " years:")
            ht(years_filenames, n=30)
        } # if years_filenames does not exist

        # todo: same as above with months_filenames

        # check if found input years are strange: dt not constant
        if (length(years_filenames) > 1 &&
            diff(range(years_filenames)) > 1 && 
            length(unique(diff(unique(years_filenames)))) != 1) {
            message("found years have non-constant dt. evaulate further with e.g. `diff(unique(years_filenames))`")
        }

        ## remove found years (which were found based on the file names) out of wanted years
        # wanted years
        #from <- as.POSIXlt(paste0(froms[i], "-01-01"), tz="UTC")
        #to <- as.POSIXlt(paste0(tos[i], "-12-31"), tz="UTC")
        #years_wanted <- (unclass(from)$year+1900):(unclass(to)$year+1900)

        # check if some wanted years are out of found years, which were found based on the file names
        #if (any(years_wanted %in% years_filenames == F)) {
        message("\ngiven `froms[", i, "]` = ", froms[i], " and `tos[", i, "]` = ", tos[i])
        if (as.integer(froms[i]) < min(years_filenames) || 
            as.integer(tos[i]) > max(years_filenames)) {
            if (fvarnames[i] %in% names(cdo_known_cmds)) {
                years_wanted <- froms[i]:tos[i] 
                # try to apply command later
            } else {
                stop("--> these are out of found years from filenames: ", 
                     min(years_filenames), " to ", max(years_filenames))
            }
        } else {
            #from_ind <- which.min(abs(years_filenames - as.integer(froms[i])))[1]
            #to_ind <- which.min(abs(years_filenames - as.integer(tos[i])))
            #to_ind <- to_ind[length(to_ind)]
            from_ind <- which(years_filenames == as.integer(froms[i]))[1]
            to_ind <- which(years_filenames == as.integer(tos[i]))
            to_ind <- to_ind[length(to_ind)]
            years_wanted <- years_filenames[from_ind:to_ind]
            message("--> found filename years from inds ", from_ind, " to ", to_ind, 
                    " (from total 1 to ", length(years_filenames), "): ",
                    min(years_wanted), " to ", max(years_wanted))
        }
        outside_years_inds <- which(years_filenames %in% years_wanted == F)
        cdoselyear <- "" # default: none
        
        if (length(outside_years_inds) > 0) {
            message("--> some input years are not needed. throw out ...")

            # remove _files_ of years outside of wanted range if one year per file
            if (length(files) == length(years_filenames)) { 
                message("   case a) length(files) = ", length(files), " == length(years_filenames) = ", 
                        length(years_filenames), "\n",
                        "      --> assume that data of one year max is saved in one file\n",
                        "      --> remove ", length(outside_years_inds), " file",
                        ifelse(length(outside_years_inds) > 1, "s", ""),
                        " outside of wanted years defined by froms[", i, "] = ", 
                        froms[i], " to tos[", i, "] = ", tos[i], " ...")
                files <- files[-outside_years_inds]
                df <- df[-outside_years_inds,]
                years_filenames <- years_filenames[-outside_years_inds]
                if (grepl("<MM>", fpatterns[i])) months_filenames <- months_filenames[-outside_years_inds]
                if (verbose > 0) {
                    message("      --> ", length(files), " file", ifelse(length(files) > 1, "s", ""), 
                            " remaining:")
                    if (length(files) > 1) {
                        ht(df)
                    } else {
                        print(df)
                    }
                } 
        
            # else remove _timepoints_ of years outside of wanted range if more than one year per file
            } else if (length(files) != length(years_filenames)) {
                if (length(files) == 1) {
                    # case b1) only 1 input file with all available years
                    message("   case b1) length(files) = ", length(files), " != length(years_filenames) = ", 
                            length(years_filenames), " AND length(files) == 1\n",
                            "      --> assume that data of more than one year is saved in one file\n",
                            "      --> remove ", length(outside_years_inds), " timestep",
                            ifelse(length(outside_years_inds) > 1, "s", ""),
                            " outside of wanted years defined by froms[", i, "] = ", 
                            froms[i], " to tos[", i, "] = ", tos[i], " ...")
                } else {
                    # case b2) more than one input files with multiple years
                    message("   case b2) length(files) = ", length(files), " != length(years_filenames) = ", 
                            length(years_filenames), " AND length(files) != 1\n",
                            "      --> assume that data of more than one year is saved in more than one file\n",
                            "      --> remove ", length(outside_years_inds), " timestep",
                            ifelse(length(outside_years_inds) > 1, "s", ""),
                            " outside of wanted years defined by froms[", i, "] = ", 
                            froms[i], " to tos[", i, "] = ", tos[i], " ...")
                    if (!any(names(df) == "YYYY_from") || !any(names(df) == "YYYY_to")) {
                        stop("this should not happen")
                    }
                    inds <- rep(F, t=length(files))
                    for (fi in seq_along(files)) {
                        years_to_check <- df$YYYY_from[fi]:df$YYYY_to[fi]
                        if (any(years_to_check %in% years_wanted)) {
                            inds[fi] <- T
                        }
                    }
                    files <- files[inds]
                    df <- df[inds,]
                    years_filenames <- years_filenames[-outside_years_inds]
                    if (verbose > 0) {
                        message("      --> ", length(files), " file", ifelse(length(files) > 1, "s", ""), 
                                " remaining:")
                        if (length(files) > 1) {
                            ht(df)
                        } else {
                            print(df)
                        }
                    } 
                } # case b1 or case b2
                cdoselyear <- paste0("-selyear,", froms[i], "/", tos[i]) # for case b1 and b2
                message("      --> `cdoselyear` = \"", cdoselyear, "\"")
            
            } # if (length(files) == length(years_filenames)) or not
        
        } # length(outside_years_inds) > 0

        # remove found months (which were found based on the file names) out of wanted season
        cdoselmon <- "" # default
        if (season_names[i] != "Jan-Dec" && season_names[i] != "annual") {
            message("\n", "season_inds = ", paste(season_inds[[i]], collapse=","), 
                    " -> season = ", season_names[i])
            if (grepl("<MM>", fpatterns[i])) { # <MM> is in fpatterns
                file_season_inds <- which(months_filenames %in% season_inds[[i]])
                if (length(file_season_inds) == 0) { # no files in wanted season 
                    stop("no files found at season_inds = ", paste(season_inds[[i]], collapse=","), 
                         " based on given <MM> pattern.")
                }
                message("remove ", length(file_season_inds), " files out of these months. files:")
                files <- files[file_season_inds]
                df <- df[file_season_inds,]
                years_filenames <- years_filenames[-outside_years_inds]
                months_filenames <- months_filenames[-outside_years_inds]
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
        if ((!all(years_filenames == cummax(years_filenames)) && !all(years_filenames == cummin(years_filenames))) # not monotonically increasing/decreasing
            #|| years_filenames[1] != as.numeric(froms[i]) || years_filenames[length(years_filenames)] != as.numeric(tos[i]) # or files are not in wanted order
            ) { 
            message("\nyears obtained from filenames are not monotonically increasing or decreasing\n",
                    " --> sort according to `froms[", i, "]` = \"", froms[i], 
                    "\" to `tos[", i, "]` = \"", tos[i], "\" ...")
            #years_filenames_ordered_inds <- sort(years_unique, index.return=T)$ix 
            # -> the above does not keep the wanted order (increasing or decreasing years)
            years_unique <- years_wanted # keep order here (increasing or decreasing years)
            years_filenames_ordered_inds <- rep(NA, t=length(years_filenames))
            inds_all <- seq_len(length(years_filenames))
            for (yi in seq_len(length(years_unique))) { # this keeps the wanted year order from:to
                inds_yi <- which(years_filenames == years_unique[yi])
                first_NA_ind <- which(is.na(years_filenames_ordered_inds))[1]
                inds_all <- first_NA_ind:(first_NA_ind + length(inds_yi) - 1)
                years_filenames_ordered_inds[inds_all] <- inds_yi
            }
            # update:
            files <- files[years_filenames_ordered_inds]
            df <- df[years_filenames_ordered_inds,]
            years_filenames <- years_filenames[years_filenames_ordered_inds]
            if (grepl("<MM>", fpatterns[i])) months_filenames <- months_filenames[years_filenames_ordered_inds]
            if (verbose > 0) ht(df) 
        } # if years_filenames are not monotonically increasing/decreasing

        # sort months
        if (F) { # not correct yet; however, not needed because of `find ... | sort`
            if (grepl("<MM>", fpatterns[i])) {
                #if (months_filenames) { # <-- correct condition missing
                    message("\n", "months obtained from file names are not monotonically increasing\n",
                            " --> sort from 1 to 12 ...")
                    # at this points, `years_filenames` are allready in correct order
                    stop("asd")
                    years_unique <- unique(years_filenames)
                    months_filenames_ordered_inds <- rep(NA, t=length(months_filenames))
                    inds_all <- seq_len(length(months_filenames))
                    for (yi in seq_len(length(years_unique))) {
                        inds_yeari <- which(years_filenames == years_unique[yi])
                        inds_yeari_lhs <- ((yi-1)*length(inds_yeari)+1):(yi*length(inds_yeari))
                        inds_yeari_rhs <- inds_all[inds_yeari][sort(months_filenames[inds_yeari], index.return=T)$ix]
                        months_filenames_ordered_inds[inds_yeari_lhs] <- inds_yeari_rhs
                    } # for yi years_unique
                    # update:
                    files <- files[months_filenames_ordered_inds]
                    df <- df[months_filenames_ordered_inds,]
                    years_filenames <- years_filenames[months_filenames_ordered_inds]
                    months_filenames <- months_filenames[months_filenames_ordered_inds]
                    if (verbose > 0) ht(df)
                #} # if months_filenames are not monotonically increasing/decreasing
            } # if <MM> is given in fpatterns or not
        } # F

        # todo: if links, check for broken links

        # get format of input files
        convert_to_nc <- F # default: no conversion to nc needed or wanted
        message("\nget input file format from first found file ...")
        cmd <- paste0("cdo showformat ", datapaths[i], "/", files[1])
        input_file_type <- cdo_get_filetype(paste0(datapaths[i], "/", files[1]))
        if ((input_file_type$file_type == "non-nc" && cdo_convert_grb2nc) ||
            !is.null(new_date_list[[i]])) { # conversion to nc is needed and/or wanted
            # being overly verbose here:
            if ((input_file_type$file_type == "non-nc" && cdo_convert_grb2nc) &&
                !is.null(new_date_list[[i]])) {
                message("--> input is non-nc and `cdo_convert_grb2nc`=T and new_date_list[[", i, 
                        "]] is not null --> convert non-nc postprocessing result to nc since its wanted and new dates are wanted")
            } else if ((input_file_type$file_type == "non-nc" && cdo_convert_grb2nc) &&
                       is.null(new_date_list[[i]])) {
                message("--> input is non-nc and `cdo_convert_grb2nc`=T --> convert non-nc postprocessing result to nc")
            } else if (!(input_file_type$file_type == "non-nc" && cdo_convert_grb2nc) &&
                       !is.null(new_date_list[[i]])) {
                message("--> input is ", input_file_type$file_type, " and `cdo_convert_grb2nc`=", cdo_convert_grb2nc,
                        " but new_date_list[[", i, "]] is not null --> ", 
                        "convert postprocessing result to nc to apply new dates with nco ncap2")
            }
            convert_to_nc <- T
        } else { # conversion is not needded and/or wanted
            message("--> input is ", input_file_type$file_type, " and `cdo_convert_grb2nc`=", cdo_convert_grb2nc, 
                    " and no new dates are wanted --> conversion of postprocessing result to nc not needed and/or wanted")
        }

        # construct cdo command (chained cdo commands will be executed from right to left)
        # prefix
        cdoprefix <- paste0(cdo, " ", cdo_silent)

        # convert to nc if grb
        cdoconvert <- "" # default: no conversion
        if (convert_to_nc) {
            if (any(models[i] == c("echam4", "echam5", "echam6", "mpiom1", "ecmwf", "remo", 
                                   "cosmo002", "cosmo201", "cosmo202", "cosmo203", "cosmo205", "cosmo250"))) {
                cdoconvert <- paste0(cdoconvert, " -t ", models[i])
            }
            cdoconvert <- paste0(cdoconvert, " -f nc")
        } # convert if wanted and input is grb
       

        # run special functions instead of the default process or any entry of `cdo_known_cmds`
        if (F) { # set conditions here

        # else no special function: continue with default or any entry of `cdo_known_cmds`
        } else {

            # check if requested variable is in first found file
            message("\ncheck if requested variable ", appendLF=F)

            if (!is.na(codes[i])) { # code not provided
                cdoselect <- paste0(cdo_select_no_history, " -select,code=", codes[i])
                message("\"var", codes[i], "\"", appendLF=F)
            } else {
                cdoselect <- paste0(cdo_select_no_history, " -select,name=", fvarnames[i])
                message("\"", fvarnames[i], "\"", appendLF=F)
            }
            message(" is present in first found file ...")
            
            # 1st (faster) try: `cdo partab`
            if (T) {
                cmd <- paste0(cdoprefix, " ", cdo_select_no_history, " partab ", datapaths[i], "/", files[1])
                message("run `", cmd, "`")
                check <- system(cmd, intern=T)
                if (!is.na(codes[i])) { # code not provided
                    teststring <- paste0("name=var", codes[i])
                } else {
                    teststring <- paste0("name=", fvarnames[i])
                }
                if (!any(grepl(teststring, check))) { # requested variable not found in files[1]
                    message("   teststring \"", teststring, "\" not found in partab")
                    attributes(check) <- list(status=1)
                }

            # 2nd (slower) try: `cdo select`
            } else if (F) {
                varcheck_file <- paste0(postpaths[i], "/tmp_variable_check_", Sys.getpid(), ".", files[1])
                cmd <- paste0(cdoprefix, " ", cdoconvert, " ", cdoselect, " ", datapaths[i], "/", files[1], " ", varcheck_file)
                message("run `", cmd, "`")
                check <- system(cmd, intern=T)
                if (clean) system(paste0("rm -v ", varcheck_file))
            }
            # finished if requested variable was not found in first found file
            

            if (F) { # for testing
                message("\n*********** for testing set variable to not found ************\n")
                attributes(check) <- list(status=1)
            }
            

            if (!is.null(attributes(check))) { # requested variable not in first found file

                message("--> requested variable \"", fvarnames[i], "\" was not found in first file:\n",
                        "   \"", datapaths[i], "/", files[1], "\"")

                # special case: requested variable is one of the wiso delta variables
                #if (fvarnames[i] %in% known_wiso_d_vars$vars) {
                if (fvarnames[i] %in% names(cdo_known_cmds)) {

                    own_cmd <- T
                    nchunks <- 1 # for rest of script
                    fout_vec <- fout

                    message("\nhowever, requested variable \"", fvarnames[i], 
                            "\" is one of the variables defined in `cdo_known_cmds`:")
                    vari <- which(names(cdo_known_cmds) == fvarnames[i])
                    message("   cdo_known_cmds[[", vari, "]]: \"", names(cdo_known_cmds)[vari], "\": ", appendLF=F)
                    if (length(cdo_known_cmds[[vari]]$cmd) == 1) {
                        message("`", cdo_known_cmds[[vari]]$cmd, "`")
                    } else {
                        message("\n", appendLF=F)
                        for (cmdi in seq_along(cdo_known_cmds[[vari]]$cmd)) {
                            message("      cmd ", cmdi, "/", length(cdo_known_cmds[[vari]]$cmd), ": `", 
                                    cdo_known_cmds[[vari]]$cmd[cmdi], "`")
                        }
                    }

                    # check command if all necessary input files are available
                    cmdsin <- cdo_known_cmds[[fvarnames[i]]]$cmd
                    cmdsout <- cmdsin
                    for (cmdi in 1:length(cmdsin)) { # todo: update with `sub_list` as above
                        message("\ncheck user cmd ", cmdi, "/", length(cmdsin), " for \"<\" and \">\": `", cmdsin[cmdi], "` ...")
                        replace_inds_open <- gregexpr("<", cmdsin[cmdi])[[1]]
                        replace_inds_close <- gregexpr(">", cmdsin[cmdi])[[1]]
                        if (length(replace_inds_open) != length(replace_inds_close)) {
                            stop("you provided a different number of \"<\" and \">\" in this command.")
                        }
                        if (length(replace_inds_open) == 1 && replace_inds_open == -1) { # not a single "<" was found in command
                            # nothing to do, let user commands as they are in the namelist
                        } else {
                            for (cmdj in seq_along(replace_inds_open)) {
                                # check if a variable in the current workspace exists with the same name
                                pattern <- substr(cmdsin[cmdi], replace_inds_open[cmdj] + 1, replace_inds_close[cmdj] - 1)
                                if (exists(eval(pattern))) { # case 1/2: variable with the name of the pattern exists
                                    eval(parse(text=paste0("length_of_pattern_var <- length(", pattern, ")")))
                                    if (length_of_pattern_var == nsettings) { # assume that the entry of setting i should be replaced
                                        eval(parse(text=paste0("replacement <- ", pattern, "[i]")))
                                    } else {
                                        eval(parse(text=paste0("replacement <- ", pattern)))
                                    }
                                } else { # case 2/2: no variable named `pattern` exists in current work space
                                    if (pattern == "nco_ncatted") { # special case 2a
                                        message("   detected special pattern \"<", pattern, ">\" --> run system(\"which ncatted\") ...")
                                        replacement <- system("which ncatted", intern=T)
                                    } else { # case 2b: assume an input file is needed
                                        replacement <- fout
                                        replacement <- gsub(fvarnames[i], pattern, replacement)
                                        if (!file.exists(replacement)) {
                                            stop("\nfound pattern \"<", pattern, 
                                                 ">\" is not a defined variable in the current workspace\n",
                                                 "--> assume it should be an input file. however, file\n   \"", replacement, "\"\n",
                                                 "does not exist\n")
                                        }
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
                    cmdout_files <- c()
                    for (cmdi in 1:length(cmdsout)) {
                        cmd <- cmdsout[cmdi]
                        if (length(cmdsout) > 1) {
                            if (cmdi == 1) { # first
                                cmdout_file <- paste0(postpaths[i], "/tmp_cmdout_", cmdi, "_of_", length(cmdsout), "_tmp_", Sys.getpid())
                                cmd <- paste0(cmd, " ", cmdout_file)
                                cmdout_files <- c(cmdout_files, cmdout_file)
                            } else if (cmdi == length(cmdsout)) { # last
                                cmdout_file <- paste0(postpaths[i], "/tmp_cmdout_", cmdi-1, "_of_", length(cmdsout), "_tmp_", Sys.getpid())
                                cmd <- paste0(cmd, " ", cmdout_file, " ", fout)
                                cmdout_files <- c(cmdout_files, cmdout_file)
                            } else { # in between
                                cmdout_file <- paste0(postpaths[i], "/tmp_cmdout_", cmdi-1, "_of_", length(cmdsout), "_tmp_", Sys.getpid())
                                cmd <- paste0(cmd, " ", cmdout_file)
                                cmdout_files <- c(cmdout_files, cmdout_file)
                                cmdout_file <- paste0(postpaths[i], "/tmp_cmdout_", cmdi, "_of_", length(cmdsout), "_tmp_", Sys.getpid())
                                cmd <- paste0(cmd, " ", cmdout_file)
                                cmdout_files <- c(cmdout_files, cmdout_file)
                            }
                        } else { # only 1 cmd
                            cmd <- paste0(cmd, " ", fout)
                        }
                        message("\nrun ", cmdi, "/", length(cmdsout), ": `", cmd, "`")
                        system(cmd)
                    } # run all (possibly modifed) user commands

                    if (clean) { 
                        for (fi in cmdout_files) {
                            if (file.exists(fi)) {
                                if (verbose > 0) message("run `rm -v ", fi, "`")
                                system(paste0("rm -v ", fi))
                            }
                        }
                    }
                   
                    # stop script here
                    toc <- Sys.time()
                    elapsed[[i]] <- toc - tic

                # else requested variable is not defined in `cdo_known_cmds`
                } else { 

                    message("and none of `cdo_known_cmds` is named \"", 
                            fvarnames[i], "\" in namelist.post.r.")
                    stop("dont know how to proceed")

                } # if special case if requested variable is one of the wiso delta variables or not
               

            # else if requested variable was found in first found file (the default)
            } else { 
                
                own_cmd <- F

                # continue with default case -> cdo cmd and not any of `cdo_known_cmds`
                message("--> requested variable \"", fvarnames[i], "\" was found in first file")
            
                # construct necessary cdo commands
                message("\nconstruct cdo command chain (cdo version = ", 
                        paste(cdo_version, collapse="."), ") ...")

                ## cat/mergetime/etc.
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

                ## calculation
                if (modes[i] == "select") {
                    cdocalc <- "" # variable selection only
                } else if (modes[i] == "volint") {
                    message("only test")
                    cdocalc <- "-fldsum -vertsum"
                } else { # else default:
                    cdocalc <- paste0("-", modes[i]) # default; e.g. "-fldmean"
                } # which calculation depending on mode
                if (cdo_before_calcs[i] != "") cdocalc <- paste0(cdocalc, " -", cdo_before_calcs[i])
                message("`modes[", i, "]` = \"", modes[i], "\" --> `cdocalc` = \"", cdocalc, "\" ...")

                ## sellevel
                cdosellevel <- "" # default: none
                if (!is.na(sellevels[i])) {
                    cdosellevel <- paste0("-sellevel,", paste0(sellevels[i], collapse=","))
                } else if (!is.na(sellevsidx[i])) {
                    cdosellevel <- paste0("-sellevidx,", paste0(sellevsidx[i], collapse=","))
                }

                ## sellonlatbox
                cdoselarea <- "" # default: none
                if (areas_out[i] != "global") {
                    if (exists("areas_out_list")) {
                        if (!is.null(areas_out_list[[i]]$sellonlatbox)) {
                            cdoselarea <- paste0("-sellonlatbox,", 
                                                 areas_out_list[[i]]$sellonlatbox["lon1"], ",",
                                                 areas_out_list[[i]]$sellonlatbox["lon2"], ",", 
                                                 areas_out_list[[i]]$sellonlatbox["lat1"], ",",
                                                 areas_out_list[[i]]$sellonlatbox["lat2"])
                        } else if (!is.null(areas_out_list[[i]]$selindexbox)) {
                            cdoselarea <- paste0("-selindexbox,", 
                                                 areas_out_list[[i]]$selindexbox["idx1"], ",",
                                                 areas_out_list[[i]]$selindexbox["idx2"], ",", 
                                                 areas_out_list[[i]]$selindexbox["idy1"], ",",
                                                 areas_out_list[[i]]$selindexbox["idy2"])
                        } else {
                            stop("not yet")
                        }
                    }
                } # if areas_out != "global" 

                # cdoselyear defined earlier

                # add further cdo chain commands here
                # ...

                ## construct cdo command
                # cdo version must be >= 1.9.4 to chain commands like
                #   `-select,name=`, `-sellevel`, `-selmon`
                # and 
                #   `-f <type> copy`, `-fldmean`, etc.

                # separate cdo selection and calculation commands if 
                if (!is.null(new_date_list[[i]]) || # set new time values to result of selection (before calculation)
                    (is.null(new_date_list[[i]]) && # or if no new time values are needed but cdo version < 1.9.4
                     (cdo_version[1] < 1 ||
                      cdo_version[1] == 1 && cdo_version[2] < 9 || 
                      cdo_version[1] == 1 && cdo_version[2] > 8 && cdo_version[3] < 4))) { 
                    
                    cdo_chain <- "separate"
                    message(ifelse(!is.null(new_date_list[[i]]), 
                                   paste0("`new_date_list[[", i, "]]` is not NULL"),
                                   paste0("cdo version ", paste(cdo_version, collapse="."), " < 1.9.4")),
                            " --> have to run separate cdo selection\n",
                            ifelse(!is.null(new_date_list[[i]]),
                                   "   `[[-t <model>] -f <type> [copy]] -sellevel,<lev> -select,name=<varname>`, etc.\n",
                                   "   `[[-t <model>] -f <type> [copy]] -selmon,<mon> -sellevel,<lev> -select,name=<varname>`, etc.\n"),
                            "and calculation\n",
                            ifelse(!is.null(new_date_list[[i]]),
                                   "   `-fldmean`, `-selmon`, etc.\n",
                                   "   `-fldmean`, etc.\n"),
                            "commands ...")

                    ## 1st cmd: selection
                    cmd_select <- cdoselect # always needed: `-select,name=<varname>`

                    # allowed chaining with early cdo version: `-f <type copy>`
                    if (cdoconvert != "") {
                        cmd_select <- paste0(cdoconvert, " ", cmd_select)
                    }
                    
                    # check for further selection commands if wanted
                    # ...

                    # end of selection: put prefix and in/out
                    selfile <- paste0(postpaths[i], "/tmp_select_", 
                                      Sys.getpid(), 
                                      #34949,
                                      #5245,
                                      ".nc")
                    cmd_select <- paste0(cdoprefix, " ", cmd_select, " <files> ", selfile)
                    if (F) cmd_select <- paste0(cmd_select, " || echo error")
                    # todo: add `copy` if only convert
                   

                    ## 2nd cmd: calculation (if needed)
                    cmd_calc <- "" # default: no calculation is needed
                    
                    # check for `-fldmean` etc.
                    if (modes[i] != "select") {
                        cmd_calc <- paste0(cmd_calc, " ", cdocalc) 
                    }
                    
                    # check for `-sellonlatbox`
                    if (cdoselarea != "") {
                        cmd_calc <- paste0(cmd_calc, " ", cdoselarea)
                    }
                    
                    # check for `-sellev`
                    if (cdosellevel != "") {
                        cmd_calc <- paste0(cmd_calc, " ", cdosellevel)
                    }
                    
                    # check for `-selmon`
                    if (cdoselmon != "") {
                        cmd_calc <- paste0(cmd_calc, " ", cdoselmon)
                    }

                    # check for `-selyear`
                    if (cdoselyear != "") {
                        cmd_calc <- paste0(cmd_calc, " ", cdoselyear)
                    }
                    
                    # check for further calculation commands if wanted
                    # ...
                    
                    # end of calculation: put prefix and in/out
                    if (cmd_calc != "") {
                        cmd_calc <- paste0(cdoprefix, " ", cmd_calc, " ", selfile, " ", fout)
                    } else if (cmd_calc == "") { 
                        # just selection (and conversion) is needed: nothing to do but renaming to wanted `fout`
                        # in: result of `[-t <model>] -select,name=`
                        # out: wanted `fout`
                        if (clean) {
                            cmd_calc <- paste0("mv -v ", selfile, " ", fout) 
                        } else if (T) {
                            cmd_calc <- paste0("cp -v ", selfile, " ", fout) 
                        }
                    }
                    if (F) cmd_calc <- paste0(cmd_calc, " || echo error")

                    message("\nrun\n",
                            "   1: `", cmd_select, "`\n",
                            ifelse(!is.null(new_date_list[[i]]), 
                                   paste0("   2: `new_date_list[[", i, "]]` is not NULL --> set new time values with ncap2\n"), ""),
                            "   ", ifelse(!is.null(new_date_list[[i]]), "3", "2"), ": `", cmd_calc, "`")

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
                            if (years_filenames[nfiles_per_chunk+1] != years_filenames[nfiles_per_chunk] + 1) {
                                one_year_earlier_inds <- which(years_filenames == years_filenames[nfiles_per_chunk] - 1)
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
                        selfile_vec <- fout_vec <- rep(NA, t=nchunks)
                        cmd_select_list <- cmd_calc_list <- chunk_inds_list <- vector("list", l=nchunks)
                        for (select_chunki in seq_len(nchunks)) {
                            inds_chunki <- cmd_select_chunk_inds[select_chunki]:(cmd_select_chunk_inds[select_chunki+1] - 1)
                            if (select_chunki == nchunks) {
                                inds_chunki <- cmd_select_chunk_inds[select_chunki]:cmd_select_chunk_inds[select_chunki+1]
                            }
                            selfile_vec[select_chunki] <- gsub(".nc", paste0("_chunk_", select_chunki, "_of_", nchunks, ".nc"), selfile)
                            fout_vec[select_chunki] <- gsub(".nc", paste0("_chunk_", select_chunki, "_of_", nchunks, ".nc"), fout)
                            cmd_select_chunki <- gsub("<files>", 
                                                      paste(paste0(datapaths[i], "/", files[inds_chunki]), collapse=" "), 
                                                      cmd_select)
                            cmd_select_chunki <- gsub(selfile, selfile_vec[select_chunki], cmd_select_chunki)
                            cmd_select_list[[select_chunki]]$n <- length(inds_chunki)
                            cmd_select_list[[select_chunki]]$cmd <- cmd_select_chunki
                            cmd_calc_chunki <- gsub(selfile, selfile_vec[select_chunki], cmd_calc)
                            cmd_calc_chunki <- gsub(fout, fout_vec[select_chunki], cmd_calc_chunki)
                            cmd_calc_list[[select_chunki]]$cmd <- cmd_calc_chunki
                            chunk_inds_list[[select_chunki]] <- inds_chunki
                            message("chunk ", select_chunki, "/", nchunks, ": files ", min(inds_chunki), " to ", 
                                    max(inds_chunki), "\n  --> ", length(inds_chunki), " files from \"", 
                                    files[min(inds_chunki)], "\" to \"", files[max(inds_chunki)], "\", ",
                                    "\n  --> nchar(selection cmd) = ", nchar(cmd_select_list[[select_chunki]]$cmd), ") ...")
                        } # for select_chunki
                    
                    } else { # cdo selection (and possible calculation) command(s) not too long
                        message("--> this is not longer than `cdo_nchar_max_arglist` = ", cdo_nchar_max_arglist, 
                                " and hence does not yield the error \"Argument list too long\"")
                        cmd_select_list <- cmd_calc_list <- chunk_inds_list <- vector("list", l=1)
                        cmd_select_list[[1]] <- list(cmd=cmd_select_tmp, n=length(files))
                        cmd_calc_list[[1]] <- list(cmd=cmd_calc)
                        chunk_inds_list[[1]] <- 1:length(files)
                        nchunks <- length(cmd_select_list)
                        fout_vec <- fout
                        selfile_vec <- selfile
                        message("--> run cdo selection of ", length(files), " files in ", 
                                nchunks, " chunk", ifelse(nchunks > 1, "s", ""), " ...")
                    }

                # or combined cdo selection and calculation commands if
                } else if (is.null(new_date_list[[i]]) && # no new times values are wanted 
                           (cdo_version[1] >= 2 || # and cdo version >= 1.9.4
                            cdo_version[1] == 1 && cdo_version[2] >= 10 || 
                            cdo_version[1] == 1 && cdo_version[2] >= 9 && cdo_version[3] >= 4)) { 
                    
                    cdo_chain <- "alltogether"
                    message("\ncdo version ", paste(cdo_version, collapse="."), " >= 1.9.4",
                            " AND `new_date_list[[", i, "]]` is NULL\n",
                            "--> can run a single cdo selection and calculation command as e.g.\n",
                            "   `cdo [[-t <model>] -f <type> [copy]] -fldmean -sellevel,<lev> -select,name=<varname>`\n",
                            "   `cdo [[-t <model>] -f <type> [copy]] -fldmean -selmon,<mon> -sellevel,<lev> -select,name=<varname>`\n",
                            "...")

                    cmd <- paste0(cdoprefix, " ", cdoconvert, 
                                  #" ", cmdcat, 
                                  " ", cdocalc, " ", 
                                  cdosellevel, " ", cdoselarea, " ", 
                                  cdoselmon, " ", cdoselyear, " ",  
                                  cdoselect, " ", 
                                  " <files> ", fout)
                    if (F) {
                        cmd <- paste0(cmd, " || echo error")
                    }

                    # replace multiple spaces by single spaces
                    cmd <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=T)
                    message("run `", cmd, "`")
                    
                    # check if cmd command is longer than cdo_nchar_max_arglist = 2612710
                    cmd_tmp <- gsub("<files>", paste(paste0(datapaths[i], "/", files), collapse=" "), cmd)
                    nchar_cmd <- nchar(substr(cmd_tmp, start=nchar(cdo) + 1, stop=nchar(cmd_tmp))) 
                    message("\n", "cdo argument list is ", nchar_cmd, " characters long")
                    
                    if (nchar_cmd > cdo_nchar_max_arglist) { # too long
                        message("--> this is longer than `cdo_nchar_max_arglist` = ", 
                                cdo_nchar_max_arglist, " and would yield the error ",
                                "\"Argument list too long\"")
                        stop("implement for argument list too long")
                    } else {
                        cmd_list <- list(list(cmd=cmd_tmp, n=length(files)))
                        nchunks <- 1
                        fout_vec <- fout
                    }

                } else { # if impossible combination of `new_date_list` and `cdo_version`
                   
                    message("`new_date_list[[", i, "]]`:")
                    cat(capture.output(str(new_date_list[[i]])), sep="\n")
                    message("cdo version = ", paste(cdo_version, collapse="."))
                    stop("this case should not occur")

                } # construct cdo commands depending on `new_date_list` and `cdo_version`
               
                # run cdo selection (and possible calculation) command(s) 
                # either from file (`$ . <scriptfile>` or via base::system(cmd)
                for (chunki in seq_len(nchunks)) { # for possible chunks if argument is too long
                    
                    message("\nchunk ", chunki, "/", nchunks, " cdo selection")
                    
                    if (cdo_run_from_script) {
                        if (cdo_chain == "alltogether") {
                            scriptname <- paste0(postpaths[i], "/tmp_select_and_calc_cmd_", 
                                                 Sys.getpid(), "_chunk_", chunki, "_of_", nchunks, ".txt")
                            writeLines(cmd_list[[chunki]]$cmd, con=scriptname)
                            nfiles_per_chunk <- cmd_list[[chunki]]$n
                        } else if (cdo_chain == "separate") {
                            if (!is.null(new_date_list[[i]])) { # new time values are wanted: have to run selection before calculation
                                scriptname <- paste0(postpaths[i], "/tmp_select_cmd_", 
                                                     Sys.getpid(), 
                                                     #34949,
                                                     #5245,
                                                     "_chunk_", chunki, "_of_", nchunks, ".txt")
                                writeLines(cmd_select_list[[chunki]]$cmd, con=scriptname)
                            } else { # no new time values wanted: selection and calculation can be executed directly after another
                                scriptname <- paste0(postpaths[i], "/tmp_select_and_calc_cmd_", 
                                                     Sys.getpid(), "_chunk_", chunki, "_of_", nchunks, ".txt")
                                writeLines(c(cmd_select_list[[chunki]]$cmd, cmd_calc_list[[chunki]]$cmd), con=scriptname)
                            }
                            nfiles_per_chunk <- cmd_select_list[[chunki]]$n
                        }
                        cmd_source <- paste0(". ", scriptname)
                        message("run `", cmd_source, "`")
                        message("this may take some time for ", nfiles_per_chunk, " files ...")
                        #stop("asd")

                        # run command if cdo selection (and possible calculation) result does not exist already
                        ticcmd <- toccmd <- NULL # default
                        if (cdo_chain == "separate" && !is.null(new_date_list[[i]])) {
                            if (file.exists(selfile_vec[chunki]) && !cdo_force) {
                                message("`selfile_vec[", chunki, "]` =\n",
                                        "   \"", selfile_vec[chunki], "\n",
                                        "already exists and `cdo_force`=F. skip ...")
                            } else {
                                ticcmd <- toccmd <- 0
                            }
                        } else {
                            if (file.exists(fout_vec[chunki]) && !cdo_force) {
                                message("`fout_vec[", chunki, "]` =\n",
                                        "   \"", fout_vec[chunki], "\"\n",
                                        "already exists and `cdo_force`=F. skip ...")
                            } else {
                                ticcmd <- toccmd <- 0
                            }
                        }
                        if (!is.null(ticcmd) && !is.null(toccmd)) {
                            ticcmd <- Sys.time()
                            system(cmd_source)
                            toccmd <- Sys.time()
                        }
                        
                        # after cdo selection (and possible calculation) command(s)
                        if (cdo_chain == "separate" && !is.null(new_date_list[[i]])) {
                            if (!file.exists(selfile_vec[chunki])) {
                                stop("selfile_vec[", chunki, "] = ", selfile_vec[chunki], " does not exist but it should")
                            }
                        } else {
                            if (!file.exists(fout_vec[chunki])) {
                                stop("`fout_vec[", chunki, "]` = \n",
                                     "   \"", fout_vec[chunki], "\"\n",
                                     "does not exist but it should")
                            }
                        }

                        # clean
                        if (clean) {
                            system(paste0("rm -v ", scriptname))
                            if (cdo_chain == "separate" && is.null(new_date_list[[i]])) { 
                                # in this case, cdo sepration and calculation results are in different files
                                # --> remove the selection results which are not needed anymore
                                if (file.exists(selfile_vec[chunki])) {
                                    # selfile does not exists if selection step was skipped since it already existed on disc 
                                    system(paste0("rm -v ", selfile_vec[chunki]))
                                }
                            }
                        }

                    } else if (!cdo_run_from_script) {
                        stop("update for cdo_chain separate/alltogether")
                        ticcmd <- Sys.time()
                        system(cmd)
                        toccmd <- Sys.time()
                    } # if cdo_run_from_script
                    
                    # elapsed
                    if (!is.null(ticcmd) && !is.null(toccmd)) {
                        elapsedcmd <- toccmd - ticcmd
                        message("chunk ", chunki, "/", nchunks, " selection and calculation took ", 
                                elapsedcmd , " ", attributes(elapsedcmd)$units, " for ", 
                                nfiles_per_chunk, " file", ifelse(nfiles_per_chunk > 1, "s", ""))
                    }

                } # for chunki: possible chunks if argument is too long
         
                # apply new time values to result of the cdo selection if wanted and then 
                # apply the cdo calculation
                if (!is.null(new_date_list[[i]])) {

                    message("\n`new_date_list[[", i, "]]` is not NULL:")
                    cat(capture.output(str(new_date_list[[i]])), sep="\n")
                    if (!is.null(new_date_list[[i]]$years)) {
                        message("new_date_list[[i]]$years:")
                        ht(new_date_list[[i]]$years, n=20)
                    }
                    message("\n--> check time dimension values of ", nchunks, " chunk", 
                            ifelse(nchunks > 1, "s", ""), " and apply the new origin if necessary ...")

                    # get time dimension values of result of cdo select and calc
                    # -> necessary to get months/days/etc. also if new_date_list[[i]]$years are given or 
                    #    new_date_list[[i]]$use == "filename" 
                    dates_in_list <- vector("list", l=nchunks)
                    for (chunki in seq_len(nchunks)) {

                        message("\nchunk ", chunki, "/", nchunks, " ...")
                        
                        # get ntime of fout
                        cmd <- paste0(cdoprefix, " ntime ")
                        cmd <- paste0(cmd, selfile_vec[chunki])
                        message("\nrun `", cmd, "`")
                        cdo_ntime <- as.integer(system(cmd, intern=T))
                        message("--> cdo_ntime = ", cdo_ntime)

                        # get time dimension values with `cdo showtimstamp` on the result 
                        # of `cdo -fldmean -select,name=var` 
                        # note: `cdo date` only shows the correct number of dates if the `time` dim is 
                        #       defined correctly. `cdo showtimestamp` or `cdo showtime`, instead, always
                        #       return the correct number of time values
                        #       --> use `cdo showtimestamp` here instead of `cdo showdate`
                        cdo_showtimestamp_file <- paste0(dirname(fout_vec[chunki]), "/tmp_cdo_showtimestamp_",
                                                         Sys.getpid(), "_chunk_", chunki, "_of_", nchunks, ".txt")
                        cmd <- paste0(cdoprefix, " showtimestamp ")
                        cmd <- paste0(cmd, selfile_vec[chunki])
                        cmd <- paste0(cmd, " > ", cdo_showtimestamp_file)
                        message("\nrun `", cmd, "` # caution: `cdo showtimestamp` does not print erroneous or duplicate dates")
                        system(cmd)
                        cdo_timestamps <- scan(cdo_showtimestamp_file, what="char", quiet=T)
                        if (length(cdo_timestamps) == 0) stop("sth went wrong")
                        if (clean) system(paste0("rm -v ", cdo_showtimestamp_file))
                        message("\n`cdo showtimestamp` yields ", length(cdo_timestamps), " dates:")
                        ht(cdo_timestamps, n=25)
                        

                        if (cdo_ntime != length(cdo_timestamps)) {
                            message("\nwarning: length(`cdo ntime`) = ", cdo_ntime, 
                                    " and length(`cdo showtimestamp`) = ", length(cdo_timestamps), " differ")
                            if (cdo_ntime > length(cdo_timestamps)) {
                                message("length(`cdo ntime`) > length(`cdo showtimestamp`) --> ", 
                                        cdo_ntime - length(cdo_timestamps), " date entries missing by `cdo showtimestamp` --> ",
                                        "possibly incorrect date values exist")
                            } else if (cdo_ntime < length(cdo_timestamps)) {
                                message("length(`cdo ntime`) < length(`cdo showtimestamp`) --> ",
                                        length(cdo_timestamps) - cdo_ntime, 
                                        " `cdo showtimestamp`-dates more than time steps found by `cdo ntime`; never happened")
                            }
                            library(ncdf4)
                            ncin <- nc_open(selfile_vec[chunki])
                            time <- ncin$dim$time$vals
                            if (grepl(" as ", ncin$dim$time$units)) { # e.g. "day as %Y%m%d.%f" --> YYYYMMDD
                                time <- as.character(time)
                                not_YYYYMMDD_inds <- which(nchar(time) != 8)
                                ncin_dates <- paste0(substr(time, 1, 4), "-", 
                                                     substr(time, 5, 6), "-", 
                                                     substr(time, 7, 8))
                                if (length(not_YYYYMMDD_inds) > 0) {
                                    ncin_dates[not_YYYYMMDD_inds] <- NA
                                    not_YYYYMMDD_chunk_end_inds <- which(diff(not_YYYYMMDD_inds) != 1)
                                    if (length(not_YYYYMMDD_chunk_end_inds) == 1) {
                                        if (not_YYYYMMDD_chunk_end_inds[1] != 1) {
                                            not_YYYYMMDD_chunk_end_inds <- c(1, not_YYYYMMDD_chunk_end_inds)
                                        } else {
                                            message("should not happen")
                                        }
                                    }
                                    for (j in seq_along(not_YYYYMMDD_chunk_end_inds)) {
                                        if (j == 1) {
                                            not_YYYYMMDD_chunk_inds <- not_YYYYMMDD_inds[(not_YYYYMMDD_chunk_end_inds[j]):not_YYYYMMDD_chunk_end_inds[j+1]]
                                        } else if (j == length(not_YYYYMMDD_chunk_end_inds)) {
                                            not_YYYYMMDD_chunk_inds <- not_YYYYMMDD_inds[(not_YYYYMMDD_chunk_end_inds[j]+1):length(not_YYYYMMDD_inds)]
                                        } else {
                                            not_YYYYMMDD_chunk_inds <- not_YYYYMMDD_inds[(not_YYYYMMDD_chunk_end_inds[j]+1):not_YYYYMMDD_chunk_end_inds[j+1]]
                                        }
                                        message("wrong YYYYMMDD dates from ", not_YYYYMMDD_chunk_inds[1], " to ", 
                                                not_YYYYMMDD_chunk_inds[length(not_YYYYMMDD_chunk_inds)], ":")
                                        ht(time[not_YYYYMMDD_chunk_inds])
                                    }
                                    stop("fix this. may setting a dummy time axis helps temporarily: `cdo settaxis,yyyy-mm-dd_origin,,1mon in out`")
                                } # if wrong times exist
                            } else {
                                stop("not defined yet")
                            }
                            date <- as.Date(ncin_dates) # --> possibly NA due to February 29 of non-leap year
                            if (any(is.na(date))) {
                                years_fout <- as.integer(substr(time, 1, 4))
                            } else {
                                timelt <- as.POSIXlt(date)
                                years_fout <- timelt$year + 1900
                            }
                            stop("fix input times")
                        } # if cdo_ntime != length(cdo_timestamps)
                        
                        # save for every chunk
                        dates_in_list[[chunki]]$cdo_ntime <- cdo_ntime
                        dates_in_list[[chunki]]$file_inds <- chunk_inds_list[[chunki]]
                        if (chunki == 1) {
                            dates_in_list[[chunki]]$time_inds <- seq_along(cdo_timestamps)
                        } else {
                            dates_in_list[[chunki]]$time_inds <- seq(max(dates_in_list[[chunki-1]]$time_inds) + 1, l=length(cdo_timestamps))
                        }
                        dates_in_list[[chunki]]$dates <- cdo_timestamps # "YYYY-MM-DDTHH:MM:SS" 
                        dates_in_list[[chunki]]$years <- as.integer(substr(cdo_timestamps, 1, 4)) # YYYY
                        dates_in_list[[chunki]]$months <- as.integer(substr(cdo_timestamps, 6, 7)) # MM
                        dates_in_list[[chunki]]$days <- as.integer(substr(cdo_timestamps, 9, 10)) # DD

                        # incorrect `cdo showtimestap`-months or -days may equal `0`
                        if (any(dates_in_list[[chunki]]$months == 0)) {
                            message("correct `cdo showtimestap`-months from 0 to 1 ...")
                            dates_in_list[[chunki]]$months[which(dates_in_list[[chunki]]$months == 0)] <- 1
                        }
                        if (any(dates_in_list[[chunki]]$days == 0)) {
                            message("correct `cdo showtimestap`-days from 0 to 1 ...")
                            dates_in_list[[chunki]]$days[which(dates_in_list[[chunki]]$days == 0)] <- 1
                        }
                   
                    } # for chunki nchunks

                    # summary of dates of cdo selection result
                    dates_in_year_range <- range(lapply(dates_in_list, "[", "years"))
                    dates_in_date_range <- range(lapply(dates_in_list, "[", "dates"))
                    cdo_ntime_fout <- sum(sapply(dates_in_list, "[[", "cdo_ntime")) # all time points of final fout
                    message("\n`cdo showtimestamp` of all chunks of cdo selection result yields `dates_in_list`:")
                    cat(capture.output(str(dates_in_list)), sep="\n")
                    message("range(lapply(dates_in_list, \"[\", \"dates\")) = ", appendLF=F)
                    dput(dates_in_date_range)
                    message("range(lapply(dates_in_list, \"[\", \"years\")) = ", appendLF=F)
                    dput(dates_in_year_range)
                    message("`cdo ntime` of all chunks of cdo selection result is ", cdo_ntime_fout)
                        
                    # check if user provided years is of same length as actual data
                    if (!is.null(new_date_list[[i]]$years)) {
                        if (length(new_date_list[[i]]$years) != cdo_ntime_fout) {
                            stop("provided `new_date_list[[", i, "]]$years` is of length ", 
                                 length(new_date_list[[i]]$years), 
                                 " but `cdo ntime` of cdo selection (and calculation) result is ", 
                                 cdo_ntime_fout)
                        }
                    }

                    # construct new dates
                    message("\nconstruct new time dimension values of ", nchunks, " chunk", 
                            ifelse(nchunks > 1, "s", ""), " ...")
                    dates_out_list <- vector("list", l=nchunks)
                    for (chunki in seq_len(nchunks)) {

                        message("\nchunk ", chunki, "/", nchunks, " ...")
                        
                        # new years
                        if (is.null(new_date_list[[i]]$years)) { # user did not provide new years
                            
                            # use the years from input filenames to construct new cdo years
                            if (new_date_list[[i]]$use == "filename") {
                            
                                message("\n`new_date_list[[", i, "]]$use` = \"", new_date_list[[i]]$use, 
                                        "\" --> use years from input filenames as new cdo years ...")
                                
                                #if (cdo_ntime_fout == 1) { # through e.g. `cdo timmean` or `timsum`
                                #    years_out <- new_date_list[[i]]$year_origin + floor(mean(years_wanted)) - 1
                                #} else if (cdo_ntime_fout == length(years_wanted)) { # through e.g `cdo yearmean` or `yearsum`
                                #    years_out <- new_date_list[[i]]$year_origin + years_wanted[dates_in_list[[chunki]]$time_inds] - 1
                                #} else {
                                #    years_out <- new_date_list[[i]]$year_origin + years_filenames[chunk_inds_list[[chunki]]] - 1
                                #}
                                
                                # input and wanted years
                                years_filenames_chunki <- years_filenames[dates_in_list[[chunki]]$file_inds]
                                message("years_filenames_chunki[1 to n] = ", years_filenames_chunki[1], " to ", 
                                        years_filenames_chunki[length(years_filenames_chunki)], 
                                        " (n = ", length(years_filenames_chunki), ")")
                                #years_in_chunki <- unique(dates_in_list[[chunki]]$years)
                                years_in_chunki <- dates_in_list[[chunki]]$years
                                message("years_in_chunki[1 to n] = ", years_in_chunki[1], " to ", 
                                        years_in_chunki[length(years_in_chunki)], " (n = ",
                                        length(years_in_chunki), ")") 

                                # possible mismatch of cdo years and filename years
                                #   e.g. annual files with monthly data:
                                #   --> years_filename_chunki = nyears
                                #   --> years_in_chunki = nyears*12

                                # if input years are of different length than wanted filename years
                                if (length(years_filenames_chunki) != length(years_in_chunki)) {

                                    message("\nlength(years_filenames_chunki) = ", length(years_filenames_chunki), 
                                            " != length(years_in_chunki) = ", length(years_in_chunki), "\n",
                                            " --> ", length(years_filenames_chunki) - length(years_in_chunki), 
                                            " years difference\n",
                                            " --> try to get some debugging information ...")
                                    npy <- rep(NA, t=length(years_in_chunki))
                                    for (yi in seq_along(years_in_chunki)) {
                                        year_inds <- which(dates_in_list[[chunki]]$years == years_in_chunki[yi])
                                        npy[yi] <- length(year_inds)
                                    }
                                    npy_unique <- unique(npy)
                                    message("\nunique occurences per year:")
                                    for (npy_uniquei in seq_along(npy_unique)) {
                                        message("years that occur ", npy_unique[npy_uniquei], " times:")
                                        cat(capture.output(str(years_in_chunki[which(npy == npy_unique[npy_uniquei])])), sep="\n")
                                    }

                                    # special case
                                    if (datapaths[i] == "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom" &&
                                        fpatterns[i] == "fort.75_fort_<YYYY>0101_<YYYY>1231.nc") { # daily moc data
                                        message("\nfix special case manually:\n",
                                                "6 years occur twice in annual fort.75* files of Hol-T2 ",
                                                "(`cdo showdate` reports them 730/732 times): 8817,8824,8947,8969,9002,9004")
                                        broken_years <- c(8817,8824,8947,8969,9002,9004)
                                        broken_year_inds <- dec31_inds <- dec31_first_year_inds <- vector("list", l=length(broken_years))
                                        for (yi in seq_along(broken_years)) {
                                            broken_year_inds[[yi]] <- which(dates_in_list[[chunki]]$years == broken_years[yi])
                                            dec31_inds[[yi]] <- broken_year_inds[[yi]][which(dates_in_list[[chunki]]$dates[broken_year_inds[[yi]]] == paste0(broken_years[yi], "-12-31"))]
                                            dec31_first_year_inds[[yi]] <- dec31_inds[[yi]][1]
                                        }
                                        tmp <- dates_in_list[[chunki]]$years
                                        # rule: first 365/366 days of broken year are ok; second 365/366 days of broken year need to get shifted by 1 year
                                        for (yi in seq_along(broken_years)) {
                                            shift_year_inds <- (dec31_first_year_inds[[yi]]+1):(length(tmp))
                                            message("shift broken year ", yi, "/", length(broken_years), " dates from ", 
                                                    min(shift_year_inds), " to ", max(shift_year_inds), ": ",
                                                    dates_in_list[[chunki]]$dates[min(shift_year_inds)], " to ", 
                                                    dates_in_list[[chunki]]$dates[max(shift_year_inds)], " by 1 year")
                                            message("--> from ", dates_in_list[[chunki]]$years[min(shift_year_inds)], " to ", 
                                                    dates_in_list[[chunki]]$years[max(shift_year_inds)], appendLF=F)
                                            tmp[shift_year_inds] <- tmp[shift_year_inds] + 1
                                            message(" to ", tmp[min(shift_year_inds)], " to ", tmp[max(shift_year_inds)])
                                        }
                                        # update with fixed years
                                        dates_in_list[[chunki]]$years <- as.integer(tmp)
                                        dates_in_list[[chunki]]$dates <- paste0(dates_in_list[[chunki]]$years, "-",
                                                                                dates_in_list[[chunki]]$months, "-",
                                                                                dates_in_list[[chunki]]$days)
                                        #years_in_chunki <- unique(dates_in_list[[chunki]]$years)

                                    } else if (datapaths[i] == "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom" &&
                                               fpatterns[i] == "fort.75_fort_<YYYY>0101_<YYYY>1231_monmean.nc") { # monthly moc data
                                        message("\nfix special case manually:\n",
                                                "6 years occur twice in annual fort.75* files of Hol-T2 ",
                                                "(`cdo showdate` reports them 730/732 times): 8817,8824,8947,8969,9002,9004")
                                        broken_years <- c(8817,8824,8947,8969,9002,9004)
                                        broken_year_inds <- vector("list", l=length(broken_years))
                                        for (yi in seq_along(broken_years)) {
                                            broken_year_inds[[yi]] <- which(dates_in_list[[chunki]]$years == broken_years[yi])
                                        }
                                        tmp <- dates_in_list[[chunki]]$years
                                        # rule: first 12 months of broken year are ok; second 12 months of broken year need to get shifted by 1 year
                                        for (yi in seq_along(broken_years)) {
                                            shift_year_inds <- broken_year_inds[[yi]][13]:length(tmp)
                                            message("shift broken year ", yi, "/", length(broken_years), " dates from ", 
                                                    min(shift_year_inds), " to ", max(shift_year_inds), ": ",
                                                    dates_in_list[[chunki]]$dates[min(shift_year_inds)], " to ", 
                                                    dates_in_list[[chunki]]$dates[max(shift_year_inds)], " by 1 year")
                                            message("--> from ", dates_in_list[[chunki]]$years[min(shift_year_inds)], " to ", 
                                                    dates_in_list[[chunki]]$years[max(shift_year_inds)], appendLF=F)
                                            tmp[shift_year_inds] <- tmp[shift_year_inds] + 1
                                            message(" to ", tmp[min(shift_year_inds)], " to ", tmp[max(shift_year_inds)])
                                        }
                                        # update with fixed years
                                        dates_in_list[[chunki]]$years <- as.integer(tmp)
                                        dates_in_list[[chunki]]$dates <- paste0(dates_in_list[[chunki]]$years, "-",
                                                                                dates_in_list[[chunki]]$months, "-",
                                                                                dates_in_list[[chunki]]$days)
                                        #years_in_chunki <- unique(dates_in_list[[chunki]]$years)
                                    
                                    } else {

                                        if (length(npy_unique) == 1 && 
                                            npy_unique*length(years_filenames_chunki) == length(years_in_chunki)) {
                                            message("\nall input years occur ", npy_unique, " times\n",
                                                    "--> repeat every filename year ", npy_unique, " times for new years\n",
                                                    "--> change here if this is not correct!")
                                            years_filenames_chunki <- rep(years_filenames_chunki, e=npy_unique)

                                        } else {
                                            stop("\nhave to fix manually")
                                        }
                                    
                                    } # special cases or if length(npy_unique) == 1
                               
                                    # updated cdo input years
                                    if (length(years_filenames_chunki) != length(years_in_chunki)) {
                                        stop("still broken!")
                                    }

                                } # if (length(years_filenames_chunki) != length(years_in_chunki)) fix
                                
                                # loop through all years of cdo selection chunk result and apply new years
                                message("\nconstruct new years ...")
                                years_out <- rep(NA, t=dates_in_list[[chunki]]$cdo_ntime)
                                for (yi in seq_along(years_in_chunki)) {
                                    year_inds <- which(dates_in_list[[chunki]]$years == years_in_chunki[yi])
                                    years_out[year_inds] <- years_filenames_chunki[yi]
                                }
                            
                            # or use the years from `cdo showdate` to construct new cdo years
                            } else if (new_date_list[[i]]$use == "cdo") {
                                stop("cdo not yetttt")
                            } # use filename or cdo years

                        } else { # if new years are given by user
                            years_out <- new_date_list[[i]]$years[dates_in_list[[chunki]]$time_inds]
                        }
                        message("new ", length(years_out), " years_out:")
                        ht(years_out)
                        
                        # new months
                        months_out <- dates_in_list[[chunki]]$months
                        
                        # new days
                        # todo: get days from file names like `years_filenames_chunki` and `months_filenames` if present
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
                        
                        # new dates as YYYY-MM-DD before checks
                        dates_out <- paste0(years_out, "-", months_out, "-", days_out)

                        # check new dates for February 30
                        # `cdo showdate` of the result of `cdo yearsum` yields "YYYY-06-30"
                        # for all years, i.e. all days are "30". In combination with the months
                        # of the input file names (if present), wrong combinations like 
                        # "YYYY-02-30" are possible.
                        feb30_inds <- which(months_out == 2 & days_out == 30)
                        if (length(feb30_inds) > 0) {
                            message("new time would yield february 30:")
                            ht(dates_out[feb30_inds])
                            message(" --> set day of these time points to 28")
                            days_out[feb30_inds] <- 28
                        }

                        # check new dates for February 29 of new non-leap years
                        feb29_inds <- months_out == 2 & days_out == 29
                        leap_inds <- is.leap(years_out)
                        feb29_nonleap_inds <- which(feb29_inds & !leap_inds)
                        if (length(feb29_nonleap_inds) > 0) {

                            message("\nnew time would yield these ", length(feb29_nonleap_inds), 
                                    " february 29 dates in non-leap years:")
                            ht(dates_out[feb29_nonleap_inds])
                            
                            # case 1/2: temporal output interval is daily: have to remove the wrong february 29 dates (`cdo del29feb`)
                            # case 2/2: temporal output interval is not daily: simply set the day to something else then 29
                            feb29_years <- years_out[feb29_nonleap_inds]
                            npy <- rep(NA, t=length(feb29_years))
                            message("--> decide how to proceed ...")
                            for (yi in seq_along(feb29_years)) {
                                year_inds <- which(years_out == feb29_years[yi])
                                npy[yi] <- length(year_inds)
                            }
                            npy_unique <- unique(npy)
                            message("\nunique occurences:")
                            for (npy_uniquei in seq_along(npy_unique)) {
                                message("years with wrong february 29 that occur ", npy_unique[npy_uniquei], " times:")
                                cat(capture.output(str(feb29_years[which(npy == npy_unique[npy_uniquei])])), sep="\n")
                            }
                            if (length(npy_unique) == 1 && npy_unique == 366) { # case 1
                                message("\n--> found ", npy_unique, " timesteps per year\n",
                                        "--> assume that data is daily\n",
                                        "--> remove wrong february 29 timesteps ...")

                                cdo_del29feb_cmd <- paste0(cdoprefix, " -delete,timestep=<wrong_feb29_timesteps> ",
                                                           selfile_vec[chunki], " ", dirname(selfile_vec[chunki]), 
                                                           "/tmp_delete_feb29_chunk_", i, "_of_", nchunks, ".nc && mv ",
                                                           dirname(selfile_vec[chunki]),
                                                           "/tmp_delete_feb29_chunk_", i, "_of_", nchunks, ".nc ", 
                                                           selfile_vec[chunki])
                                message("run `", cdo_del29feb_cmd, "` ...")
                                cdo_del29feb_cmd <- gsub("<wrong_feb29_timesteps>", 
                                                         paste(feb29_nonleap_inds, collapse=","),
                                                         cdo_del29feb_cmd)
                                system(cdo_del29feb_cmd)
                                years_out <- years_out[-feb29_nonleap_inds]
                                months_out <- months_out[-feb29_nonleap_inds]
                                days_out <- days_out[-feb29_nonleap_inds]
                                
                            } else { # case 2
                                message("\n--> found timesteps per year are not of length 366\n",
                                        "--> assume that data is not daily\n",
                                        "--> simply set day of wrong february 29 timesteps to 28\n",
                                        "--> define another case here if this is not correct")
                                days_out[feb29_nonleap_inds] <- 28
                            } # case 1 or 2

                        } # if wrong february 29 dates exist in new dates

                        # new dates as YYYY-MM-DD after checks
                        #dates_out <- paste0(sprintf("%04i", years_out), "-", 
                        #                    sprintf("%02i", months_out), "-",
                        #                    sprintf("%02i", days_out))
                        dates_out <- paste0(years_out, "-", months_out, "-", days_out)
                        message("\nnew dates_out:")
                        ht(dates_out, n=25)

                        # construct new dates used by nco ncap2
                        if (i == 1 && chunki == 1) {
                            if (is.null(new_date_list[[i]]$nc_time_units)) { # user did not provide units of netcdf time dimension
                                if (cdo_set_rel_time) { # relative time
                                    if (is.null(new_date_list[[i]]$nc_time_origin)) {
                                        stop("`cdo_set_rel_time`=T but new_date_list[[", i, "]]$nc_time_origin is not set.")
                                    }
                                    nc_time_units <- paste0("days since ", new_date_list[[i]]$nc_time_origin, "-01-01")
                                    nc_time_units <- rep(nc_time_units, t=nsettings)
                                } else { # absolute time
                                    nc_time_units <- rep("day as %Y%m%d.%f", t=nsettings) # only allowed absolute time for cdo
                                }
                                message("\n`new_date_list[[", i, "]]$nc_time_units` is not set and `cdo_set_rel_time`=", 
                                        cdo_set_rel_time, " --> use default \"", nc_time_units, "\"")
                                new_date_list[[i]]$nc_time_units <- nc_time_units
                            } # if `nc_time_units` was not set by user
                        } # only once at beginning

                        message("\nconstruct new times values with `new_date_list[[", 
                                i, "]]$nc_time_units` = \"", new_date_list[[i]]$nc_time_units, "\":")
                        if (cdo_set_rel_time) {
                            if (F) { # old
                                dates_out_ncap <- as.POSIXlt(dates_out, tz="UTC") # old
                            } else if (T) { # new
                                message("run `make_posixlt_origin(years_out=c(",
                                        paste(c(head(years_out), "...", tail(years_out)), collapse=","), "))` ...")
                                dates_out_ncap <- make_posixlt_origin(years_out)
                                dates_out_ncap$mon <- months_out - 1 # posix months start counting from zero
                                dates_out_ncap$mday <- days_out
                            }
                            tmp <- difftime(dates_out_ncap[1], 
                                            as.POSIXlt(paste0(new_date_list[[i]]$nc_time_origin, "-01-01"), tz="UTC"), 
                                            units="days")
                            tmp <- as.numeric(tmp) # days since first date
                            if (cdo_ntime_fout > 1){
                                dates_out_ncap <- difftime(dates_out_ncap[2:length(dates_out_ncap)], 
                                                           dates_out_ncap[1:(length(dates_out_ncap) - 1)], 
                                                           units="days")
                                dates_out_ncap <- c(tmp, tmp + cumsum(as.numeric(dates_out_ncap)))
                            } else {
                                dates_out_ncap <- tmp
                            }

                        } else { # absolute time
                            if (new_date_list[[i]]$nc_time_units == "years as %Y.%f") {
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
                        message("\ndates_out_ncap:")
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

                    # apply new dates to cdo selection result
                    nco_fout_vec <- selfile_vec 
                    for (chunki in seq_len(nchunks)) {

                        message("\nmodify time dimension values of chunk ", chunki, "/", nchunks, " ...")
                        nco_fout_vec[chunki] <- gsub(".nc", 
                                                     paste0("_origin_", new_date_list[[i]]$nc_time_origin, ".nc"),
                                                     selfile_vec[chunki])
                       
                        #if (!file.exists(nco_fout_vec[chunki])) { # why did i do this?
                        if (T) {

                            # if input has absolute time units but `new_date_list[[i]]$nc_time_units` shall be relative,
                            # the input needs to be converted from absolute to relative time units
                            # before setting new time values with nco ncap2
                            # todo: need to update this with a check
                            #cmd_cp_and_mv <- paste0("cp ", fout_vec[chunki], " ", nco_fout_vec[chunki])
                            cmd_cp_and_mv <- paste0(cdoprefix, " --no_history -r copy ", selfile_vec[chunki], " ", nco_fout_vec[chunki])
                            cmd_ncap2 <- paste0(nco_ncap2, " -O -h -s 'time(:)={<dates_out_ncap>}; time@units=\"",
                                                new_date_list[[i]]$nc_time_units, "\"' ", nco_fout_vec[chunki], " ", 
                                                nco_fout_vec[chunki], " || echo error")
                            message("run 1: `", cmd_cp_and_mv, "`\n", 
                                    "    2: `", cmd_ncap2, "`")
                            cmd_ncap2_tmp <- gsub("<dates_out_ncap>", 
                                                  paste(dates_out_list[[chunki]]$dates_ncap, collapse=","), 
                                                  cmd_ncap2)
                            
                            # check if nco ncap2 argument is too long
                            nchar_cmd_ncap2 <- nchar(cmd_ncap2_tmp)
                            message("nco ncap2 argument with all input files is ", nchar_cmd_ncap2, " characters long")
                            if (nchar_cmd_ncap2 > nco_nchar_max_arglist) {
                                
                                # run nco ncap2 command in chunks from file 
                                message("--> this is longer than `nco_nchar_max_arglist` = ", nco_nchar_max_arglist, 
                                        " and would yield the error \"Argument list too long\"")
                                
                                # find nco chunks for this cdo chunk (chunki)
                                nchar_needed_per_date <- max(nchar(dates_out_list[[chunki]]$dates_ncap)) + 1
                                nchar_wout_files <- nchar(gsub("<dates_out_ncap>", "", cmd_ncap2))
                                nchar_avail <- nco_nchar_max_arglist - nchar_wout_files - 100 # ~100 characters more due to ncofile_vec
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
                               
                                # construct nco ncap2 cmd chunks
                                ncofile_vec <- rep(NA, t=nchunks_nco_ncap2)
                                nco_ncap2_list <- cmd_seltimestep_list <- nco_ncap2_chunk_inds_list <- vector("list", l=nchunks_nco_ncap2)
                                for (nco_ncap2_chunki in seq_len(nchunks_nco_ncap2)) {
                                    
                                    message("\nnco ncap2 chunk ", nco_ncap2_chunki, "/", nchunks_nco_ncap2, " of cdo chunk ",
                                            chunki, "/", nchunks, " cmd generation:")
                                    
                                    inds_chunki <- cmd_nco_ncap2_chunk_inds[nco_ncap2_chunki]:(cmd_nco_ncap2_chunk_inds[nco_ncap2_chunki+1] - 1)
                                    if (nco_ncap2_chunki == nchunks_nco_ncap2) {
                                        inds_chunki <- cmd_nco_ncap2_chunk_inds[nco_ncap2_chunki]:cmd_nco_ncap2_chunk_inds[nco_ncap2_chunki+1]
                                    }
                                    ncofile_vec[nco_ncap2_chunki] <- gsub(".nc", 
                                                                          paste0("_nco_ncap2_chunk_", nco_ncap2_chunki, 
                                                                                 "_of_", nchunks_nco_ncap2, ".nc"), 
                                                                          nco_fout_vec[chunki])
                                    cmd_seltimestep_list[[nco_ncap2_chunki]] <- paste0(cdoprefix, " seltimestep,", inds_chunki[1], "/",
                                                                                       inds_chunki[length(inds_chunki)], " ",
                                                                                       selfile_vec[chunki], " ", 
                                                                                       ncofile_vec[nco_ncap2_chunki]) 
                                    message("run 1: `", cmd_seltimestep_list[[nco_ncap2_chunki]], "`")
                                    cmd_nco_ncap2_chunki <- gsub(nco_fout_vec[chunki], ncofile_vec[nco_ncap2_chunki], cmd_ncap2)
                                    message("    2: `", cmd_nco_ncap2_chunki, "`")
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
                                nco_ncap2_txt <- rep(NA, t=nchunks_nco_ncap2)
                                for (nco_ncap2_chunki in seq_len(nchunks_nco_ncap2)) {

                                    message("\nnco ncap2 chunk ", nco_ncap2_chunki, "/", nchunks_nco_ncap2, 
                                            " of cdo chunk ", chunki, "/", nchunks, " cmd source:")
                                    nco_ncap2_txt[nco_ncap2_chunki] <- paste0(dirname(selfile_vec[chunki]), "/tmp_", Sys.getpid(), 
                                                            "_nco_ncap2_chunk_", nco_ncap2_chunki, "_of_", 
                                                            nchunks_nco_ncap2, "_of_cdo_chunk_", chunki, "_of_", 
                                                            nchunks, ".txt")
                                    writeLines(c(cmd_seltimestep_list[[nco_ncap2_chunki]],  # selection cmd
                                                 nco_ncap2_list[[nco_ncap2_chunki]]$cmd),  # ncap cmd
                                               con=nco_ncap2_txt[nco_ncap2_chunki])
                                    cmd_source <- paste0(". ", nco_ncap2_txt[nco_ncap2_chunki])
                                    message("run `", cmd_source, "` ...")
                                    system(cmd_source)

                                    if (!file.exists(ncofile_vec[nco_ncap2_chunki])) { # selection for ncap chunki file exists?
                                        stop("ncofile_vec[", nco_ncap2_chunki, "] = ", ncofile_vec[nco_ncap2_chunki], 
                                             " does not exist but it should")
                                    }

                                } # for nco_ncap_chunki

                                # cat nco ncap2 chunks together
                                message("\ncat ", nchunks_nco_ncap2, " nco ncap2 chunks of cdo chunk ", 
                                        chunki, "/", nchunks, " together:")
                                #if (new_date_list[[i]]$nc_time_units == "day as %Y%m%d.%f") { 
                                if (F) {
                                    # destroys non-default time formats not known by cdo
                                    # also, makes "day as %Y%m%d.%f" --> "days since YYYY-MM-DD"
                                    cmd_cat <- paste0(cdoprefix, " cat ", paste(ncofile_vec, collapse=" "), 
                                                      " ", nco_fout_vec[chunki])
                                } else { # keeps non-default time formats
                                    cmd_cat <- paste0(nco_ncrcat, " -O ", paste(ncofile_vec, collapse=" "), 
                                                      " ", nco_fout_vec[chunki], " || error")
                                }
                                message("run `", cmd_cat, "` ...")
                                system(cmd_cat)
                                
                                if (clean) {
                                    if (file.exists(nco_fout_vec[chunki])) {
                                        cmd <- paste0("rm -v ", paste(ncofile_vec, collapse=" "))
                                        message("run `", cmd, "` ... (not enabled for safety)")
                                        #system(cmd)
                                        cmd <- paste0("rm -v ", paste(nco_ncap2_txt, collapse=" "))
                                        message("run `", cmd, "` ... (not enabled for safety)")
                                        #system(cmd)
                                    } else {
                                        message("`clean`=T but `nco_four_vec[chunki=", chunki, "]`=\"", nco_fout_vec[chunki], 
                                                "\" does not exist. do not clean tmp files.")
                                    }
                                } # clean

                            } else if (nchar_cmd_ncap2 <= nco_nchar_max_arglist) {
                              
                                message("--> this is not longer than `nco_nchar_max_arglist` = ", nco_nchar_max_arglist, 
                                        " and hence does not yield the error \"Argument list too long\"")
                                # do not select time steps in nco ncap2 chunks but just make a copy and rename
                                system(cmd_cp_and_mv)
                                #system(paste0("cdo -r copy ", nco_fout_vec[chunki], " ~/tmp && mv ~/tmp ", nco_fout_vec[chunki])) 
                                system(cmd_ncap2_tmp)
                                # --> this call does not capture "-bash: /usr/bin/ncap2: Argument list too long"
                                # however, for cdo it does! dont know why

                            } # if nco ncap2 argument is too long

                        } else { # if file.exists(nco_fout_vec[chunki])

                            message("\nnco_fout_vec[chunki] = \"", nco_fout_vec[chunki], "\" alread exists. skip ncap2 command")

                        } # if file.exists(nco_fout_vec[chunki])

                    } # for cdo chunki: possible chunks if argument is too long
           
                    # new origin was applied to result of cdo selection 
                    # --> continue with these files
                    selfile_vec_old <- selfile_vec
                    selfile_vec <- nco_fout_vec
               
                    # run cdo calculation after applying new dates to cdo selection result
                    for (chunki in seq_len(nchunks)) { # for possible chunks if argument is too long
                  
                        message("\nrun calculation of chunk ", chunki, "/", nchunks, " ...")
                        
                        # update selfile in calc command
                        cmd_calc_chunki <- cmd_calc_list[[chunki]]$cmd
                        cmd_calc_chunki <- gsub(selfile_vec_old[chunki], selfile_vec[chunki], cmd_calc_chunki)
                        message("\nrun `", cmd_calc_chunki, "` ...")

                        if (cdo_run_from_script) {
                            
                            scriptname <- paste0(postpaths[i], "/tmp_calc_cmd_", Sys.getpid(), "_chunk_", 
                                                 chunki, "_of_", nchunks, ".txt")
                            writeLines(cmd_calc_chunki, con=scriptname)
                            nfiles_per_chunk <- cmd_select_list[[chunki]]$n
                            cmd_source <- paste0(". ", scriptname)
                            if (file.exists(fout_vec[chunki]) && !cdo_force) {
                                message("fout_vec[", chunki, "] =\n",
                                        "   \"", fout_vec[chunki], "\"\n",
                                        "already exists and `cdo_force`=F. skip ...")
                                ticcmd <- toccmd <- NULL
                            } else {
                                message("via `", cmd_source, "`")
                                message("this may take some time for ", nfiles_per_chunk, " files ...")
                                ticcmd <- Sys.time()
                                system(cmd_source)
                                toccmd <- Sys.time()
                            }

                            # output file exists?
                            if (!file.exists(fout_vec[chunki])) {
                                stop("fout_vec[", chunki, "] = ", fout_vec[chunki], " does not exist but it should")
                            }
                        
                        } else if (!cdo_run_from_script) {
                            stop("update for cdo_chain separate/alltogether")
                            ticcmd <- Sys.time()
                            system(cmd)
                            toccmd <- Sys.time()
                        } # if cdo_run_from_script
                        
                        # elapsed
                        if (!is.null(ticcmd) && !is.null(toccmd)) {
                            elapsedcmd <- toccmd - ticcmd
                            message("chunk ", chunki, "/", nchunks, " calculation took ", 
                                    elapsedcmd , " ", attributes(elapsedcmd)$units, " for ", 
                                    nfiles_per_chunk, " file", ifelse(nfiles_per_chunk > 1, "s", ""))
                        }
                            
                        # clean
                        if (clean) {
                            if (cdo_run_from_script) system(paste0("rm -v ", scriptname))
                            system(paste0("rm -v ", selfile_vec_old[chunki]))
                            system(paste0("rm -v ", selfile_vec[chunki]))
                        } # if clean

                    } # for chunki: possible chunks if argument is too long
                
                } # if !is.null(new_date_list[[i]])
                # finished applying new dates to cdo selection result and subsequent cdo calculation

            } # finished check if requested variable is in first found file

        } # if run special function or default way
       
        ## from here, cdo selection and calculation or commands from `cdo_known_cmds` or special functions are finished for all chunks
        # -> time dimension values are still the original, `new_date_list` was not applied yet
        # -> time dimension values are probably shifted from monthly to annual by e.g. `cdo yearsum`
        # -> chunks, if any, are not catted yet

        #todo: compare filename and `froms`/`tos` dates with time dimension values from the actual nc file
        
        # cat chunks together (if needed) and remove temporary files
        if (nchunks > 1) {
            message("\ncat ", nchunks, " cdo chunks together ...")
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
            if (fout_vec != fout) {
                message("\nrename temporary file to fout ...")
                cmd_rename <- paste0("mv -v ", fout_vec, " ", fout)
                system(cmd_rename)
            }
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

        # change from/to in final fout if new dates were applied
        if (!own_cmd && !is.null(new_date_list[[i]]$years)) {
            message("\nrename initial fout according to new dates ...")
            cmd <- paste0("mv -v ", fout, " ")
            from_new <- range(sapply(dates_out_list, "[", "years"))[1]
            to_new <- range(sapply(dates_out_list, "[", "years"))[2]
            fout <- gsub(paste0(froms[i], "-", tos[i]), 
                         paste0(sprintf(paste0("%0", nchar(froms[i]), "i"), from_new), "-",
                                sprintf(paste0("%0", nchar(tos[i]), "i"), to_new)),
                         fout)
            cmd <- paste0(cmd, fout)
            system(cmd)
        } # if !is.null(new_date_list[[i]]$years)

    } # if fout_exist_check (if output file already exists or not)

    # set relative time axis
    if (!own_cmd && cdo_set_rel_time && is.null(new_date_list[[i]])) {
        message("\n", "`cdo_set_rel_time`=T --> set relative time axis ...")
        reltime_file <- paste0(postpaths[i], "/tmp_reltime_", Sys.getpid())
        cmd <- paste0("cdo ", cdo_silent, " --no_history -r copy ", fout, " ", reltime_file, 
                      " && mv ", reltime_file, " ", fout)
        cmd <- paste0(cmd, " || echo error")
        message("run `", cmd, "`")
        system(cmd)
    } else {
        message("\n", "`cdo_set_rel_time`=F --> do not set relative time axis ...")
    }

    # run special functions in the end    
    if (models[i] == "mpiom1" && any(fvarnames[i] == c("amoc", "gmoc"))) { # run mpiom_moc_make_bottom_topo()
            
        message("\n`models[", i, "]` = \"", models[i], "\" and `fvarnames[", i, "]` = ",
                "\"", fvarnames[i], "\" --> run mpiom_moc_make_bottom_topo() ...")
        
        cmd <- "mpiom_moc_make_bottom_topo(cdo=cdo, varname=fvarnames[i], fin=fout, fout=fout"
        if (exists("mpiom_moc_make_bottom_topo_arg_list")) {
            for (argi in seq_along(mpiom_moc_make_bottom_topo_arg_list[[i]])) {
                cmd <- paste0(cmd, ", ", 
                              names(mpiom_moc_make_bottom_topo_arg_list[[i]])[argi], 
                              "=mpiom_moc_make_bottom_topo_arg_list[[", i, "]]$",
                              names(mpiom_moc_make_bottom_topo_arg_list[[i]])[argi])
            }
        }
        cmd <- paste0(cmd, ")")
        message("run `", cmd, "`")
        eval(parse(text=cmd))

        # get moc time series if not timmean
        if (modes[i] != "timmean") {

            message("\n`modes[", i, "] = \"", modes[i], "\" != \"timmean", 
                    "\" --> get moc time series --> run mpiom_moc_extract_ts() ...")
           
            cmd <- "mpiom_moc_extract_ts(fin=fout"
            if (exists("mpiom_moc_extract_ts_arg_list")) {
                for (argi in seq_along(mpiom_moc_extract_ts_arg_list[[i]])) {
                    cmd <- paste0(cmd, ", ", 
                                  names(mpiom_moc_extract_ts_arg_list[[i]])[argi], 
                                  "=mpiom_moc_extract_ts_arg_list[[", i, "]]$",
                                  names(mpiom_moc_extract_ts_arg_list[[i]])[argi])
                }
            }
            cmd <- paste0(cmd, ")")
            message("run `", cmd, "`")
            eval(parse(text=cmd))

        } # if modes[i] != "timmean"

    } # if mpiom1 and *moc

    toc <- Sys.time()
    elapsed[[i]] <- toc - tic
    message("\nsetting ", i, "/", nsettings , " took ", elapsed[[i]], " ", 
            attributes(elapsed[[i]])$units, " for ", models[i], " ", modes[i], " calculation of ",
            length(files), " file", ifelse(length(files) > 1, "s", ""))

    # restore user options for next setting
    cdo_set_rel_time <- cdo_set_rel_time_old

} # for i nsettings

message("\nfinished")

for (i in seq_len(nsettings)) {
    message("\nsetting ", i, "/", nsettings , "\n",
            "  ", datapaths[i], "/", fpatterns[i], "\n",
            "took ", elapsed[[i]], " ", 
            attributes(elapsed[[i]])$units, " for ", models[i], " ", modes[i], " calculation of ", 
            length(files), " file", ifelse(length(files) > 1, "s", ""), "\n")
}

if (!interactive()) {
    message("grep this log file for lines that begin with \"error\": grep -n \"^error\" <logfile>", 
            "\nbe happy if nothing is returned\n")
}

options(warn=0) # back to default

