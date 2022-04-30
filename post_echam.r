# r

# post processing of model output
# 1: namelist.general.post.r
# 2: helper_functions.r
# 3: myfunctions.r
# 4: namelist.post.r
# 5: post_echam.r

# host check
if (T && host$machine_tag == "mistral") {
    hostname <- Sys.info()["nodename"] # = system("hostname", intern=T)
    if (any(grepl("mlogin", hostname))) {
        stop("machine is \"mistral\" but node \"", hostname, 
             "\" is a login node\n--> change to `ssh -Y mistralpp.dkrz.de` ",
             "or `ssh -Y mistralpp[1-5]` and rerun the script.")
    }
}

# load libraries necessary for plot_echam.r
message("\nload packages defined in ", host$repopath, "/requirements_post.txt ...")
requirements <- readLines(paste0(host$repopath, "/requirements_post.txt"))
for (r in requirements) {
    r <- trimws(r)
    if (substr(r, 1, 1) != "#") { # current line is not a comment
        if (grepl(" ", r)) { # there is a space
            r <- substr(r, 1, regexpr(" ", r)-1) # everything until first space
        }
        message("   ", r)
        suppressPackageStartupMessages(library(r, character.only=T))
    }
}

## check user input from namelist.post.r
message("verbose = ", verbose)
message("post_force = ", post_force) 
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
cdo_version <- base::numeric_version(paste(cdo_version, collapse="."))
message("cdo_version = ", cdo_version)
message("cdo_silent = \"", cdo_silent, "\"")
message("cdo_select_no_history = \"", cdo_select_no_history, "\"")
message("cdo_OpenMP_threads = \"", cdo_OpenMP_threads, "\"") # OMP supported operators: https://code.mpimet.mpg.de/projects/cdo/wiki/OpenMP_support
message("cdo_set_rel_time = ", cdo_set_rel_time)
message("cdo_run_from_script = ", cdo_run_from_script)
# -O necessary for ens<STAT>, merge, mergetime

# check if necessary user variables exist and are of correct length
exist_checks <- c("models", "datapaths", "fpatterns", "prefixes", 
                  "fvarnames", "froms", "tos", "modes")
if (!all(sapply(exist_checks, exists))) {
    missing_vars <- !sapply(exist_checks, exists)
    stop("\nmust provide variable", 
         ifelse(length(which(missing_vars)) > 1, "s", ""),
         " `", paste0(names(missing_vars)[missing_vars], collapse="`, `"), 
         "` in post namelist")
}
nsettings <- length(datapaths)
for (i in seq_along(exist_checks)) {
    cmd <- paste0("length(", exist_checks[i], ")")
    length_of_obj <- eval(parse(text=cmd))
    if (length_of_obj != nsettings) {
        stop("variable `", exist_checks[i], "` is of length ", length_of_obj, 
             " but must be of length ", nsettings, " (as `datapaths`)")
    }
}

# convert given modes to list if not provided as list
if (!is.list(modes)) {
    modesp <- vector("list", l=nsettings)
    for (i in seq_len(nsettings)) {
        modesp[[i]] <- modes[i]
        names(modesp)[i] <- modes[i]
    }
    modes <- modesp
}
if (is.null(names(modes)) || any(names(modes) == "")) {
    inds <- which(names(modes) == "")
    stop("provide a proper mode name for `modes` list entries ", paste(inds, collapse=", "))
}
if (any(grepl(" ", names(modes)))) { # replace space " " by "_" in mode names
    inds <- which(grepl(" ", names(modes)))
    message("replace spaces \" \" with \"_\" in mode names:\n",
            paste(paste0("   ", names(modes)[inds]), collapse="\n"))
    names(modes)[inds] <- gsub(" ", "_", names(modes)[inds])
    message("to\n",
            paste(paste0("   ", names(modes)[inds]), collapse="\n"))
}
#if (!exists("ftypes")) ftypes <- rep("f", t=nsettings)
if (!exists("codes_files")) {
    codes_files <- rep(NA, t=nsettings)
} else { # codes_files was provided
    if (length(codes_files) != nsettings) {
        stop("provided `code_files` is of length ", length(codes_files), " but nsettings = ", nsettings)
    }
    if (any(!file.exists(codes_files))) {
        inds <- which(!file.exists(codes_files))
        stop("provided `codes_files`\n", paste(codes_files[inds], collapse="\n"), "\n",
             ifelse(length(inds) > 1, "do", "does"), " not exist")
    }
}
if (!exists("codes")) codes <- rep(NA, t=nsettings)
if (!exists("areas_out_list")) areas_out_list <- NULL
if (!exists("mask_list")) mask_list <- NULL
if (!exists("areas_out")) {
    if (is.null(areas_out_list) && is.null(mask_list)) {
        areas_out <- rep("global", t=nsettings) # default
    } else {
        areas_out <- rep(NA, t=nsettings)
        for (i in seq_along(areas_out)) {
            if ((!is.null(areas_out_list) && !is.null(areas_out_list[[i]]$name)) &&
                (!is.null(mask_list) && !is.null(mask_list[[i]]$name))) {
                stop("both `areas_out_list[[", i, "]]$name` and `mask_list[[", i, "]]$name` are given. dont know which to use.")
            } else {
                if (!is.null(areas_out_list[[i]]$name)) {
                    areas_out[i] <- areas_out_list[[i]]$name
                } else if (!is.null(mask_list[[i]]$name)) {
                    areas_out[i] <- mask_list[[i]]$name
                } else {
                    stop("either `areas_out_list[[", i, "]]$name` or `mask_list[[", i, "]]$name` must be given")
                }
            }
        }
    }
}
if (any(is.na(areas_out))) stop("this should not happen")
if (!exists("cdoshifttimes")) cdoshifttimes <- rep("", t=nsettings)

# check postpaths
if (exists("workpath")) {
    host$workpath <- workpath # overwrite default
} else {
    workpath <- host$workpath
    message("`workpath` not given --> use result from helper_functions.r:get_host() = ", workpath)
}
if (!exists("postpaths")) {
    postpaths <- paste(host$workpath, "post", models, names(modes), fvarnames, sep="/")
}
if (any(!dir.exists(postpaths))) {
    nonexisting_paths <- postpaths[which(!dir.exists(postpaths))]
    for (i in nonexisting_paths) { # try to create
        message("create postpath \"", i, "\"")
        dir.create(i, recursive=T, showWarnings=F)
        if (!dir.exists(i)) stop("no sucess")
    }
}
postpaths <- normalizePath(postpaths)

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
        for (i in seq_len(nsettings)) season_inds[[i]] <- 1:12
    }
    season_names <- rep(NA, t=nsettings)
    for (i in seq_len(nsettings)) {
        if (length(season_inds[[i]]) == 12 && season_inds[[i]] == 1:12) { # default case: annual
            season_names[i] <- "Jan-Dec"
        } else if (length(season_inds[[i]]) == 1) { # only 1 month, e.g. "Jan"
            season_names[i] <- month.abb[season_inds[[i]]]
        } else { # several months: first letters of months, e.g. "JFM"
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
    #lev_fnames[which(!is.na(sellevels))] <- paste0("_sellevel_", gsub(",", "_", sellevels[which(!is.na(sellevels))]))
    for (i in seq_len(nsettings)) {
        if (!is.na(sellevels[i])) {
            levs <- sellevels[i]
            if (is.character(levs)) {
                levs <- strsplit(levs, ",")[[1]]
                options(warn=2) # stop on warnings
                levs <- as.numeric(levs) # error if not successful
                options(warn=0) # back to default
            }
            if (length(levs) == 1) {
                lev_fnames[i] <- paste0("_sellevel_", levs)
            } else {
                lev_fnames[i] <- paste0("_sellevel_", min(levs), "-", max(levs))
            }
            rm(levs)
        }
    }
}
if (any(!is.na(sellevsidx))) {
    lev_fnames[which(!is.na(sellevsidx))] <- paste0("_sellevidx_", gsub(",", "_", sellevsidx[which(!is.na(sellevsidx))]))
    for (i in seq_len(nsettings)) {
        if (!is.na(sellevsidx[i])) {
            levs <- sellevsidx[i]
            if (is.character(levs)) {
                levs <- strsplit(levs, ",")[[1]]
                options(warn=2) # stop on warnings
                levs <- as.numeric(levs) # error if not successful
                options(warn=0) # back to default
            }
            if (length(levs) == 1) {
                lev_fnames[i] <- paste0("_sellevidx_", levs)
            } else {
                lev_fnames[i] <- paste0("_sellevidx_", min(levs), "-", max(levs))
            }
            rm(levs)
        }
    }
}

cdo_set_rel_time_old <- cdo_set_rel_time # for next setting i

if (!exists("cdo_before_calcs")) cdo_before_calcs <- rep("", t=nsettings)
if (!exists("cdo_after_calcs")) cdo_after_calcs <- rep("", t=nsettings)
if (!is.list(cdo_before_calcs)) {
    cdo_before_calcsp <- vector("list", l=nsettings)
    for (i in seq_along(cdo_before_calcsp)) {
        cdo_before_calcsp[[i]] <- cdo_before_calcs[i]
    }
    cdo_before_calcs <- cdo_before_calcsp
}
if (!is.list(cdo_after_calcs)) {
    cdo_after_calcsp <- vector("list", l=nsettings)
    for (i in seq_along(cdo_after_calcsp)) {
        cdo_after_calcsp[[i]] <- cdo_after_calcs[i]
    }
    cdo_after_calcs <- cdo_after_calcsp
}

# check for new times if wanted
message("check if `new_date_list` is set and correct ... ", appendLF=F)
if (!exists("new_date_list")) {
    message("-> `new_date_list` is not set. user does not want new time values.")
    new_date_list <- NULL
} else {
    message("-> `new_date_list` is not NULL")
    # check sticky requirement of functions:make_posixlt_origin() 
    message("load sticky package for functions:make_posixlt_origin() ...")
    library(sticky)
    for (i in seq_len(nsettings)) {
        if (!is.null(new_date_list[[i]]$dates)) { # user provided new dates
            message("`new_date_list[[", i, "]]$dates` (", length(new_date_list[[i]]$dates), " entries) = ")
            ht(new_date_list[[i]]$dates)
            if (!any(class(new_date_list[[i]]$dates) == "POSIXt")) {
                message("convert to POSIX ...")
                new_date_list[[i]]$dates <- as.POSIXct(new_date_list[[i]]$dates, tz="UTC")
                ht(new_date_list[[i]]$dates)
            }

        } else if (is.null(new_date_list[[i]]$dates)) { # user did not provide new dates

            if (!is.null(new_date_list[[i]]$years)) { # user provided new years
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
            } else if (is.null(new_date_list[[i]]$years)) { # user did not provide new years
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
            } # new years?
        } # new dates?
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
    if (length(mpiom1_remap) != nsettings) { 
        if (length(mpiom1_remap) == 1) { # repeat for all settings
            mpiom1_remap <- rep(mpiom1_remap, t=nsettings)
        } else {
            stop("`mpiom1_remap` is of length ", length(mpiom1_remap), " but nsettings = ", 
                 nsettings, ". dont know how to proceed")
        }
    }
}

# special filename patterns
special_patterns <- c("<YYYY>", "<YYYY_from>", "<YYYY_to>", 
                      "<MM>", "<MM_from>", "<MM_to>",
                      "<DD>", "<DD_from>", "<DD_to>")

message("\nnameslist.post.r checks finished. start running post_echam.r for ", nsettings, 
        " model setup", ifelse(nsettings > 1, "s", ""), " ...")

# clear lastfiles_post file
lastfiles_post_fname <- paste0("lastfiles_post_", Sys.getpid(), ".txt") # in same path as this post_echam.r-call
lastfiles_post_fname <- paste0(normalizePath(dirname(lastfiles_post_fname)), "/", lastfiles_post_fname)
message("\ncreate file for storing post filenames \"", lastfiles_post_fname, "\" ...")
invisible(file.create(lastfiles_post_fname)) # error if no success

# do for every model setting
elapsed <- vector("list", l=nsettings)
for (i in seq_len(nsettings)) {

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
    message("mode = \"", names(modes)[i], "\" = ", paste(modes[[i]], collapse=", "))
    message("from = ", froms[i])
    message("to = ", tos[i])
    message("season_name = ", season_names[i])
    message("season_inds = ", paste0(season_inds[[i]], collapse=","))
    message("area_out = ", areas_out[i])
    if (!is.na(sellevels[i])) message("sellevel = ", sellevels[i])
    if (!is.na(sellevsidx[i])) message("sellevidx = ", sellevsidx[i])
    if (!is.null(new_date_list[[i]])) {
        if (!is.null(new_date_list[[i]]$dates)) {
            message("new_date_list[[", i, "]]$dates = ")
            ht(new_date_list[[i]]$dates)
        } else {
            if (!is.null(new_date_list[[i]]$years)) {
                message("new_date_list[[", i, "]]$years = ")
                ht(new_date_list[[i]]$years)
            } else {
                message("new_date_list[[", i, "]]$use = ", new_date_list[[i]]$use, "\n",
                        "new_date_list[[", i, "]]$year_origin = ", new_date_list[[i]]$year_origin)
            }
        }
    }

    # output fname
    fout <- paste0(postpaths[i], "/", 
                   ifelse(!is.na(prefixes[i]), paste0(prefixes[i], "_"), ""),
                   models[i], "_", names(modes)[i],
                   #todo: include code number in fout or not?
                   #ifelse(!is.na(codes[i]), paste0("_selcode_", codes[i]), ""),
                   "_", fvarnames[i], 
                   lev_fnames[i], 
                   "_", areas_out[i],
                   "_", season_names[i], "_", froms[i], "-", tos[i], 
                   ".nc")
    message("fout = ", fout)
    
    fout_exist_check <- file.access(fout, mode=0)
    if (F && fout_exist_check == 0) {
        message("\n ************** redo although output exists for testing **************")
        fout_exist_check <- -1
    }
    
    # fout already exists 
    if (fout_exist_check == 0 && post_force == F) {

        message("final fout=\n   ", fout, "\nalready exists and `post_force=F`. skip.")

    # fout does not exist --> run chained cdo operators for all files
    } else { 
        
        # delete fout if it already exists 
        if (fout_exist_check == 0 && post_force == T) {
            message("final fout=\n   ", fout, "\nalready exists and `post_force=T`. delete already existing file ...")
            check <- file.remove(fout)
            if (!check) warning("something went wrong deleting file ", fout)
        }

        # replace potential "<...>" strings in `datapaths[i]`
        message("\ncheck `datapaths[", i, "]` =\n   \"", datapaths[i], "\"\nfor \"<...>\" patterns to replace ...")
        sub_list <- NULL # default
        pattern_inds_open <- gregexpr("<", datapaths[i])[[1]] # returns n inds if found or -1 
        pattern_inds_closed <- gregexpr(">", datapaths[i])[[1]]
        if (!all(pattern_inds_open == -1) || !all(pattern_inds_closed == -1)) {
            if (length(pattern_inds_open) != length(pattern_inds_closed)) {
                stop("in `datapaths[", i, "]` you provided ", length(pattern_inds_open), 
                     " opening brackets \"<\" to indicate a file pattern to replace but ", 
                     length(pattern_inds_closed), " closing brackets \">\". there must be a \"<\" for every \">\".")
            }
            n_patterns_per_file <- length(pattern_inds_open)
            sub_list <- vector("list", l=n_patterns_per_file)
            for (pati in seq_len(n_patterns_per_file)) {
                pattern <- substr(datapaths[i], pattern_inds_open[pati], pattern_inds_closed[pati]) # pattern to replace with leading "<" and trailing ">"
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
                             pattern, "\" in `datapaths[", i, "]`. dont know how to interpret this case.")
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
            datapath <- datapaths[i]
            for (pati in seq_along(sub_list)) {
                datapath <- sub(pattern=sub_list[[pati]]$pattern, replacement=sub_list[[pati]]$replacement, x=datapath)
            }
            message("   -> \"", datapath, "\"")
        } else {
            message("   no \"<...>\" strings detected in `datapaths[", i, "]` ...")
            datapath <- datapaths[i]
        } # if user provided "<...>" strings in datapaths
        
        warn <- options()$warn
        options(warn=0) # dont stop on warning since datapath may not exist for `cdo_known_cmds`
        datapath <- normalizePath(datapath)
        options(warn=warn) # restore

        # replace potential "<...>" strings in `fpatterns[i]`
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
            message("   no \"<...>\" strings detected in `fpatterns[", i, "]` ...")
            fpattern <- fpatterns[i]
        } # if user provided "<...>" string in fpatterns
        # this sublist based on `fpatterns[i]` will be used later

        # find files based on datapath and fpattern with potential <patterns> applied
        # todo: search for files and links and compare
        #cmd <- paste0("ls ", datapaths[i], "/", fpattern) 
        # --> this may result in `-bash: /bin/ls: Argument list too long`
        #cmd <- paste0("find ", datapath, " -maxdepth 1 -type ", ftypes[i], " -name \"", fpattern, "\" -printf \"%f\\n\" | sort")
        cmd <- paste0("find ", datapath, " -maxdepth 1 -name \"", fpattern, "\" -printf \"%f\\n\" | sort")
        # --> `find` does not have this limit 
        message("\nrun `", cmd, "` ...")
        ticcmd <- Sys.time()
        files <- system(cmd, intern=T)
        toccmd <- Sys.time()
        if (length(files) == 0) stop("Zero files found. Is the `find`-command above correct?")
        elapsedcmd <- toccmd - ticcmd
        message("`find` of ", length(files), " files took ", elapsedcmd, " ", attributes(elapsedcmd)$units) 

        if (datapath == "/ace/user/stschuet/Hol-T_echam5_wiso_links") {
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
        
        # identify years/months/etc. of found files based on <YYYY*>, <MM*>, etc. 
        # patterns if given or, alternatively, based on `cdo showdate`
        if (any(special_patterns %in% sapply(sub_list, "[[", "pattern"))) {
            message("\nfind years/months/etc. based on `special_patterns`=\n",
                    paste(paste0("   \"", special_patterns), collapse="\"\n"), "\"\n",
                    "of found files ...")
            
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
                    } else if (special_patterns_in_filenames[pati] == "<MM_from>") {
                        months_filenames_from <- as.integer(df$MM_from)
                    } else if (special_patterns_in_filenames[pati] == "<MM_to>") {
                        months_filenames_to <- as.integer(df$MM_to)
                    }
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
                years_filenames <- months_filenames <- vector("list", l=length(files))
                for (fi in seq_along(files)) {
                    cmd <- paste0(cdo, " showdate ", datapath, "/", files[fi])
                    message("\nfile ", fi, "/", length(files), ": run `", cmd, "`")
                    dates <- system(cmd, intern=T)
                    dates <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", dates, perl=T) # remove double and leading blanks
                    dates <- strsplit(dates, " ")[[1]] # e.g. "2731-02-01"
                    if (verbose) {
                        message(length(dates), " cdo dates of this file:")
                        ht(dates, n=5)
                    }
                    years_filenames[[fi]] <- substr(dates, 1, 4) # e.g. "2731"
                    months_filenames[[fi]] <- substr(dates, 6, 7) # e.g. "02"
                }
                years_filenames <- as.integer(unlist(years_filenames))
                years_filenames_unique <- unique(years_filenames)
                if (length(years_filenames) == length(files)) { # case1: one timepoint per file
                    df$YYYY <- years_filenames
                    df$MM <- months_filenames
                } else if (length(years_filenames) != length(files)) { # case2: more than one timepoint per file
                    if (length(years_filenames_unique) == length(files)) { # case 2.1: same number of unique years as files
                        df$YYYY <- years_filenames_unique
                    } else {
                        stop("not defined; maybe old fesom1 output with wrong nc dates?")
                    }
                }
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
            as.integer(tos[i]) > max(years_filenames)) { # some wanted years out of years_filenames
            if (fvarnames[i] %in% names(cdo_known_cmds)) {
                years_wanted <- froms[i]:tos[i] 
                # try to apply command later
            } else {
                stop("--> these given years are not within found years: ", 
                     min(years_filenames), " to ", max(years_filenames))
            }
        } else { # all wanted years within years_filenames
            #from_ind <- which.min(abs(years_filenames - as.integer(froms[i])))[1]
            #to_ind <- which.min(abs(years_filenames - as.integer(tos[i])))
            #to_ind <- to_ind[length(to_ind)]
            from_ind <- which(years_filenames == as.integer(froms[i]))[1]
            if (is.na(from_ind)) {
                stop("wanted start year `froms[", i, "]` = ", froms[i], " not available in `years_filenames`")
            }
            to_ind <- which(years_filenames == as.integer(tos[i]))
            to_ind <- to_ind[length(to_ind)]
            if (is.na(to_ind)) {
                stop("wanted end year `tos[", i, "]` = ", tos[i], " not available in `years_filenames`")
            }
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
                if (length(files) == 0) stop("zero files")
        
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
                    if (length(files) == 0) stop("zero files")
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
            if (grepl("<MM>", fpatterns[i])) { # <MM> given per file
                file_season_inds <- which(months_filenames %in% season_inds[[i]])
                if (length(file_season_inds) == 0) { # no files in wanted season 
                    stop("no files found at season_inds = ", paste(season_inds[[i]], collapse=","), 
                         " based on given <MM> pattern.")
                }
                message("keep ", length(file_season_inds), " files out of these months. files:")
                files <- files[file_season_inds]
                df <- df[file_season_inds,]
                years_filenames <- years_filenames[-outside_years_inds]
                months_filenames <- months_filenames[-outside_years_inds]
                if (verbose > 0) ht(df)

            #} else if (grepl("<MM_from>", fpatterns[i])) {
            #    # this does not help?

            } else { # no "<MM>" string in `fpatterns[i]`
                cmd <- paste0("cdo ", cdo_silent, " showmon ", datapath, "/", files[1])
                message("run `", cmd, "`")
                months_per_file <- system(cmd, intern=T)
                if (months_per_file != "") {
                    months_per_file <- strsplit(months_per_file, "\\s+")[[1]]
                    if (any(months_per_file == "")) months_per_file <- months_per_file[-which(months_per_file == "")]
                }
                if (length(months_per_file) == 1 && months_per_file == "") { # `cdo showmon` was not successfull
                    stop("input files do not have proper time axis. not implemented yet")
                }
                months_per_file <- as.integer(months_per_file)
                selmon_season_inds <- which(months_per_file %in% season_inds[[i]])
                if (length(selmon_season_inds) == 0) { # no files in wanted season
                    stop("found zero ", season_name, " months in this file")
                }
                cdoselmon <- paste0("-selmon,", paste(season_inds[[i]], collapse=",")) 
                message("--> defined `cdoselmon = \"", cdoselmon, "\"")

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
        input_file_type <- ncdump_get_filetype(paste0(datapath, "/", files[1]))
        # conversion is needed if:
        if ((input_file_type == "non-nc" && cdo_convert_grb2nc) || # case1: wanted
            (input_file_type == "non-nc" && !is.null(new_date_list[[i]]))) { # case2: needed
            if (verbose > 0) {
                message("--> input is not of type nc and ", appendLF=F)
                if (cdo_convert_grb2nc && !is.null(new_date_list[[i]])) {
                    message("`cdo_convert_grb2nc`=T and new_date_list[[", i, 
                            "]] is not null --> convert non-nc postprocessing result to nc since its wanted and new dates are wanted")
                } else if (cdo_convert_grb2nc && is.null(new_date_list[[i]])) {
                    message("`cdo_convert_grb2nc`=T --> convert non-nc postprocessing result to nc")
                } else if (!cdo_convert_grb2nc && !is.null(new_date_list[[i]])) {
                    message("`cdo_convert_grb2nc`=F but new_date_list[[", i, "]] is not null --> ", 
                            " have to convert postprocessing result to nc to apply new dates with nco ncap2")
                }
            }
            convert_to_nc <- T
        } else { # conversion is not needded and/or wanted
            message("--> input is ", input_file_type, 
                    " --> conversion of postprocessing result to nc not necessary")
        }

        # construct cdo command (chained cdo commands will be executed from right to left)
        # prefix
        cdoprefix <- paste0(cdo, " ", cdo_silent)

        # convert to nc if grb
        cdoconvert <- "" # default: no conversion
        if (convert_to_nc) {

            # try to apply paramter tables for grb->nc conversion
            message("\ngrb->nc conversion necessary --> try to determine codes file ...")

            if (!is.na(codes_files[i])) {
                message("--> use provided `codes_files[", i, "]` = \"", codes_files[i], "\"")
                cdoconvert <- paste0(cdoconvert, " -t ", codes_files[i])

            } else if (is.na(codes_files[i])) {
                
                message("--> `codes_files[", i, "]` not provided --> try to find one ...")
                
                # try 1/3: check if there is a "<filename_wout_ext>.codes" file in data dir
                codes_filesi <- paste0(tools::file_path_sans_ext(files[1]), ".codes")
                message("try 1/3: find pattern \"", datapath, "/", codes_filesi, "\" ... ", appendLF=F)
                codes_filesi <- list.files(datapath, pattern=codes_filesi, full.names=T)
                if (length(codes_filesi) == 1) {
                    message()
                } else if (length(codes_filesi) == 0) { # no .codes file found
                    message("no")
                    codes_filesi <- NULL
                } else if (length(codes_filesi) > 1) {
                    stop("found ", length(codes_filesi), " .codes files:\n",
                         paste(codes_filesi, collapse="\n"))
                }
                
                # try 2/3: check if there is a "<filename_wout_ext_and_wout_year>.codes" file in path `datadir/../../log`
                # data: piControl_2801_jsbach_jsbach_YYYY.grb
                # code: piControl_2801_jsbach_jsbach.codes 
                if (is.null(codes_filesi)) {
                    codes_filesi <- tools::file_path_sans_ext(files[1])
                    codes_filesi <- paste0(substr(codes_filesi, 1, nchar(codes_filesi)-5), ".codes")
                    message("try 2/3: find pattern \"", datapath, "/../../log/", codes_filesi, "\" ... ", appendLF=F)
                    # if last 5 characters of datafile wout extenstion ends with "_YYYY" and if path `datadir/../../log` exists
                    codes_filesi <- list.files(paste0(datapath, "/../../log"), pattern=codes_filesi, full.names=T)
                    if (length(codes_filesi) == 1) {
                        message()
                        codes_filesi <- normalizePath(codes_filesi)
                    } else if (length(codes_filesi) == 0) {
                        message("no")
                        codes_filesi <- NULL
                    } else if (length(codes_filesi) > 1) {
                        stop("found ", length(codes_filesi), " .codes files:\n",
                             paste(codes_filesi, collapse="\n"))
                    }
                }

                # try 3/3: use default parameter code table name if available for current model
                if (is.null(codes_filesi)) {
                    message("try 3/3: check if current model \"", models[i], "\" is one of `cdo -t` default partables (check `cdo -h`) ... ", appendLF=F)
                    if (any(models[i] == c("echam4", "echam5", "echam6", "mpiom1", 
                                           "ecmwf", "remo", "cosmo002", "cosmo201", 
                                           "cosmo202", "cosmo203", "cosmo205", "cosmo250"))) {
                        message()
                        codes_filesi <- models[i] 
                    } else {
                        message("no")
                    }
                }
                
                if (!is.null(codes_filesi)) {
                    message("--> use partab \"", codes_filesi, "\" ...")
                    cdoconvert <- paste0(cdoconvert, " -t ", codes_filesi)
                }

            } # if `codes_files` was provided or not
            cdoconvert <- paste0(cdoconvert, " -f nc")
        } # convert if wanted and input is grb

        # run special functions instead of the default process or any entry of `cdo_known_cmds`
        if (F) { # set conditions here

        # else no special function: continue with default or any entry of `cdo_known_cmds`
        } else {

            # check if requested variable is in first found file
            message("\ncheck if requested variable \"", fvarnames[i], "\" ", appendLF=F)

            if (!is.na(codes[i])) { # code not provided
                cdoselect <- paste0(cdo_select_no_history, " -select,code=", codes[i])
                message("(\"var", codes[i], "\") ", appendLF=F)
            } else {
                cdoselect <- paste0(cdo_select_no_history, " -select,name=", fvarnames[i])
            }
            message("is present in first found file ...")
            
            #cmd <- paste0(cdoprefix, " ", cdo_select_no_history, " partab ", datapath, "/", files[1]) # old
            cmd <- paste0(cdoprefix, " ", cdo_select_no_history, " showname ", datapath, "/", files[1], " 2>&1")
            message("run `", cmd, "`")
            var_exist <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))$value
            message("-->\n", paste(var_exist, collapse="\n"))
            if (!is.null(attributes(var_exist))) { # no success
                stop("sth is wrong with this file")
            }
            var_exist <- trimws(var_exist)
            if (!is.na(codes[i])) { # code provided
                teststring <- paste0("var", codes[i])
            } else {
                teststring <- fvarnames[i]
            }
            if (any(grepl(teststring, var_exist))) {
                var_exist <- T # wanted variable is in file
            } else {
                var_exist <- F
            }
            # finished if requested variable was not found in first found file
            

            if (F) { # for testing
                message("\n*********** for testing set variable to not found ************\n")
                var_exist <- F
            }
            

            if (!var_exist) { # requested variable not in first found file

                message("--> requested variable \"", fvarnames[i], "\" ", appendLF=F)
                if (!is.na(codes[i])) message("(\"var", codes[i], "\") ", appendLF=F)
                message("was not found in first file:\n", 
                        "   \"", datapath, "/", files[1], "\"")

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
                                if (exists(eval(pattern)) && 
                                    !any(class(eval(parse(text=pattern))) == c("function", "standardGeneric"))) { 
                                    # case 1/2: variable with the name of the pattern exists and its not a function
                                    eval(parse(text=paste0("length_of_pattern_var <- length(", pattern, ")")))
                                    if (length_of_pattern_var == nsettings) { # assume that the entry of setting i should be replaced
                                        eval(parse(text=paste0("replacement <- ", pattern, "[i]")))
                                    } else {
                                        eval(parse(text=paste0("replacement <- ", pattern)))
                                    }
                                } else { 
                                    # case 2/2: no variable named `pattern` exists in current work space
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
                        message()
                        for (fi in cmdout_files) {
                            if (file.exists(fi)) {
                                if (verbose > 0) message("`clean`=T --> run `rm -v ", fi, "`")
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
            } else if (var_exist) { 
                
                own_cmd <- F

                # continue with default case -> cdo cmd and not any of `cdo_known_cmds`
                message("--> requested variable \"", fvarnames[i], "\" ", appendLF=F)
                if (!is.na(codes[i])) message("(\"var", codes[i], "\") ", appendLF=F)
                message("was found in first file")
            
                # construct necessary cdo commands
                message("\nconstruct cdo command chain (cdo version = ", cdo_version, ") ...")

                ## cat/mergetime/etc.
                if (all(modes[[i]] == "timmean")) {
                    nmax <- as.integer(system("ulimit -n", intern=T))
                    if (length(files) > nmax) {
                        stop("cannot compute timmean of ", length(files), 
                             " files because `cdo ensmean` maximum files is `ulimit -n` = ", nmax)
                    }
                    cmdcat <- "-O ensmean"
                } else if (all(modes[[i]] == "fldmean")) {
                    if (F) { # could not figure out a significant time difference between cat and mergetime
                        cmdcat <- "cat"
                    } else if (T) {
                        cmdcat <- "mergetime"
                    }
                } else {
                    #stop("cat/mergetime not defined for mode '", modes[[i]], "' not defined.")
                } # which cat/mergetime depending on mode

                ## calculation cmd
                
                cdocalc <- rep("", t=length(modes[[i]]))
                for (cdocalci in seq_along(cdocalc)) {
                    # if special cdo calc case
                    if (modes[[i]][cdocalci] == "select") {
                        cdocalc[cdocalci] <- "" # variable selection only
                    } else if (modes[[i]][cdocalci] == "fldint") {
                        cdocalc[cdocalci] <- "" # combination `-mul -select` does not work; need to apply -mul at the end
                        if (cdo_version < base::numeric_version("1.9.6")) {
                            stop("need cdo version >= 1.9.8 for fldint")
                        }
                    } else { # default
                        cdocalc[cdocalci] <- paste0("-", modes[[i]][cdocalci]) # e.g. "-fldmean"
                    } # which cdo calculation depending on mode
                } # for all cdo calc operators
                cdocalc <- paste(cdocalc, collapse=" ")
                message("\n`modes[[", i, "]]` = \"", paste(modes[[i]], collapse=", "), 
                        "\" --> `cdocalc` = \"", cdocalc, "\" ...")
                
                if (all(cdo_before_calcs[[i]] == "")) {
                    message("\n`cdo_before_calcs[[", i, "]]` not given --> do not run some command before `cdo ", 
                            cdocalc, "` ...")
                } else {
                    message("\n`cdo_before_calcs[[", i, "]]` = ", 
                            paste(cdo_before_calcs[[i]], collapse=", "), " --> run `", appendLF=F)
                    cdo_before_calc <- paste(paste0("-", cdo_before_calcs[[i]]), collapse=" ")
                    message(cdo_before_calc, "` before cdo `", cdocalc, "` ...")
                    cdocalc <- paste0(cdocalc, " ", cdo_before_calc)
                }
                
                if (all(cdo_after_calcs[[i]] == "")) {
                    message("\n`cdo_after_calcs[", i, "]` not given --> do not run some command after `cdo ", 
                            cdocalc, "` ...")
                } else {
                    message("\n`cdo_after_calcs[[", i, "]]` = ", 
                            paste(cdo_after_calcs[[i]], collapse=", "), " --> run `", appendLF=F)
                    cdo_after_calc <- paste(paste0("-", cdo_after_calcs[[i]]), collapse=" ")
                    message(cdo_after_calc, "` after cdo `", cdocalc, "` ...")
                    cdocalc <- paste0(cdo_after_calc, " ", cdocalc)
                }

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
                    if (!is.null(areas_out_list)) {
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

                ## shifttime 
                cdoshifttime <- "" # default: not shifttime
                if (cdoshifttimes[i] != "") {
                    # derive dt of data based on `cdo showtimestamp`
                    # --> this is not always correct if there are strange data gaps
                    message("\nprovided `cdoshifttimes[", i, "]` = \"", cdoshifttimes[i], "\"")
                    if (grepl("dt", cdoshifttimes[i])) {
                        message("--> detected \"dt\" --> get time intervall of input files ...")
                        cmd <- paste0(cdo, " tinfo ", datapath, "/", files[1])
                        message("run `", cmd, "` ...")
                        dt <- system(cmd, intern=T)
                        dt <- dt[which(grepl(" Increment           :", dt))] # e.g. " Increment           :  10 years"
                        message("--> \"", dt, "\"")
                        dt <- strsplit(dt, ":")[[1]][2] # e.g. "  10 years"
                        dt <- trimws(dt) # e.g. "10 years" 
                        dt <- gsub("\\s+", "", dt) # e.g. "10years" --> usable by `cdo shifftime`
                        message("--> dt = ", dt)
                        if (dt == "0seconds") { # `cdo tinfo` only 1 timestep or no success
                            message("--> data has only 1 timestep or no proper \"time\" dim found\n",
                                    "--> do not apply shifftime\n",
                                    "--> set `cdoshifttimes[", i, "] = \"[-]<tunit>\" in the post namelist and rerun the script")
                            cdoshifttimes[i] <- "" # do not apply shifttime
                        } else { # `cdo tinfo` success
                            message("--> replace \"dt\" in cdoshifttimes[", i, "]` = \"", 
                                    cdoshifttimes[i], "\" with \"", dt, "\" ...")
                            cdoshifttimes[i] <- sub("dt", dt, cdoshifttimes[i])
                        } # if `cdo showtimestamp` returned something
                    } # if "dt" is in provided `cdoshifttimes[i]`
                    cdoshifttime <- paste0("-shifttime,", cdoshifttimes[i])
                    message("--> `cdoshifttime` = \"", cdoshifttime, "\"")
                } else {
                    message("\n`cdoshifttimes[", i, "]` not provided. set to e.g. \"-1mo\" or \"-1dt\", if wanted")
                } # if cdoshifttime is provided

                # add further cdo chain commands here
                # ...

                ## construct cdo command
                # cdo version must be >= 1.9.4 to chain commands like
                #   `-select,name=`, `-sellevel`, `-selmon`
                # and 
                #   `-f <type> copy`, `-fldmean`, etc.

                # separate cdo selection and calculation commands if 
                if (cdo_version < base::numeric_version("1.9.4") ||
                    !is.null(new_date_list[[i]]) || # set new time values to result of selection (before calculation)
                    !is.null(mask_list[[i]])) { # apply mask before calculation
                    
                    cdo_chain <- "separate"
                    message("\nhave to run separate cdo selection and calculation commands because")
                    if (cdo_version < base::numeric_version("1.9.4")) {
                        message("- cdo version ", cdo_version ," < 1.9.4")
                    }
                    if (!is.null(new_date_list[[i]])) {
                        message("- `new_date_list[[", i, "]]` is not NULL")
                    }
                    if (!is.null(mask_list[[i]])) {
                        message("- `mask_list[[", i, "]]` is not NULL")
                    }

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
                    if (all(modes[[i]] != "select")) {
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
                    
                    # shifttime after variable selection
                    if (cdoshifttime != "") {
                        cmd_calc <- paste0(cmd_calc, " ", cdoshifttime)
                    }
                    
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

                    cnt <- 1
                    msg <- paste0("\nrun\n   ", cnt, ": `", cmd_select, "`\n")
                    if (!is.null(new_date_list[[i]])) {
                        cnt <- cnt + 1
                        msg <- paste0(msg, "   ", cnt, ": `new_date_list[[", i, "]]` is not NULL --> set new time values to selection result with ncap2\n")
                    }
                    if (!is.null(mask_list[[i]])) {
                        cnt <- cnt + 1
                        msg <- paste0(msg, "   ", cnt, ": `mask_list[[", i, "]]` is not NULL --> apply mask to selection result with cdo\n")
                    }
                    cnt <- cnt + 1
                    msg <- paste0(msg, "   ", cnt, ": `", cmd_calc, "`")
                    message(msg)
                    rm(cnt)

                    # replace multiple spaces by single spaces
                    nchar_with_mulitiple_spaces <- nchar(cmd_select)
                    cmd_select <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd_select, perl=T)
                    nchar_with_single_spaces <- nchar(cmd_select)
                    #message("removed ", nchar_with_mulitiple_spaces-nchar_with_single_spaces, 
                    #        " multiple spaces from selection command")
                    
                    # replace <files> with actual files
                    #files <- files[1:29355]
                    cmd_select_tmp <- gsub("<files>", 
                                           paste(paste0(datapath, "/", files), collapse=" "), 
                                           cmd_select)
                    
                    # check if cmd_select command is longer than cdo_nchar_max_arglist = 2612710
                    nchar_cmd_select <- nchar(substr(cmd_select_tmp, start=nchar(cdo) + 1, stop=nchar(cmd_select_tmp))) 
                    message("\n", "cdo selection argument list is ", nchar_cmd_select, " characters long")
                    if (nchar_cmd_select > cdo_nchar_max_arglist) { # too long
                        
                        # find maximum number of files per chunk
                        message("--> this is longer than `cdo_nchar_max_arglist` = ", cdo_nchar_max_arglist, 
                                " and would yield the error \"Argument list too long\"")
                        nchar_needed_per_file <- nchar(paste0(datapath, "/", files[1], " "))
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
                                                      paste(paste0(datapath, "/", files[inds_chunki]), collapse=" "), 
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
                                " --> does not yield the error \"Argument list too long\"")
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

                # or combined cdo selection and calculation commands if possible
                } else {
                    
                    cdo_chain <- "alltogether"
                    message("\ncan run a single cdo selection and calculation command ...")

                    cmd <- paste0(cdoprefix, " ", cdoconvert, 
                                  #" ", cmdcat, 
                                  " ", cdocalc, " ", 
                                  cdosellevel, " ", cdoselarea, " ", 
                                  cdoselmon, " ", cdoselyear, " ",  
                                  cdoshifttime, " ", cdoselect, " ",
                                  " <files> ", fout)
                    if (F) {
                        cmd <- paste0(cmd, " || echo error")
                    }

                    # replace multiple spaces by single spaces
                    cmd <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cmd, perl=T)
                    message("run `", cmd, "`")
                    
                    # check if cmd command is longer than `cdo_nchar_max_arglist`
                    cmd_tmp <- sub("<files>", paste(paste0(datapath, "/", files), collapse=" "), cmd)
                    nchar_cmd <- nchar(substr(cmd_tmp, start=nchar(cdo) + 1, stop=nchar(cmd_tmp))) 
                    message("\n", "cdo command `cmd_tmp` is ", nchar_cmd, " characters long")
                    
                    if (nchar_cmd > cdo_nchar_max_arglist) { # too long
                        message("--> this is longer than `cdo_nchar_max_arglist` = ", 
                                cdo_nchar_max_arglist, " and would yield the error ",
                                "\"Argument list too long\"")
                        stop("implement for argument list too long")
                    } else {
                        message("--> this is not longer than `cdo_nchar_max_arglist` = ", 
                                cdo_nchar_max_arglist, " and does not yield the error ",
                                "\"Argument list too long\"")
                        cmd_list <- list(list(cmd=cmd_tmp, n=length(files)))
                        nchunks <- 1
                        fout_vec <- fout
                    }

                } # construct cdo commands depending on cdo version, `new_date_list` and `mask_list`
               
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
                            if (!is.null(new_date_list[[i]]) || !is.null(mask_list[[i]])) { 
                                # new time values are wanted: have to run selection before calculation
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
                        message("this may take some time for ", nfiles_per_chunk, " file",
                                ifelse(nfiles_per_chunk > 1, "s", ""), " ...")
                        #stop("asd")

                        # run command if cdo selection (and possible calculation) result does not exist already
                        ticcmd <- toccmd <- NULL # default
                        if (cdo_chain == "separate" && 
                            (!is.null(new_date_list[[i]]) || !is.null(mask_list[[i]]))) {
                            if (file.exists(selfile_vec[chunki]) && !post_force) {
                                message("`selfile_vec[", chunki, "]` =\n",
                                        "   \"", selfile_vec[chunki], "\n",
                                        "already exists and `post_force`=F. skip ...")
                            } else {
                                ticcmd <- toccmd <- 0
                            }
                        } else {
                            if (file.exists(fout_vec[chunki]) && !post_force) {
                                message("`fout_vec[", chunki, "]` =\n",
                                        "   \"", fout_vec[chunki], "\"\n",
                                        "already exists and `post_force`=F. skip ...")
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
                        if (cdo_chain == "separate" && 
                            (!is.null(new_date_list[[i]]) || !is.null(mask_list[[i]]))) {
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
                            if (cdo_chain == "separate" && 
                                is.null(new_date_list[[i]]) && is.null(mask_list[[i]])) { 
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
         
                # apply new time values to result of cdo selection if wanted before cdo calculation
                if (!is.null(new_date_list[[i]])) {

                    message("\n`new_date_list[[", i, "]]` is not NULL:")
                    cat(capture.output(str(new_date_list[[i]])), sep="\n")
                    if (!is.null(new_date_list[[i]]$dates)) {
                        message("new_date_list[[i]]$dates:")
                        ht(new_date_list[[i]]$dates, n=20)
                    } else {
                        if (!is.null(new_date_list[[i]]$years)) {
                            message("new_date_list[[i]]$years:")
                            ht(new_date_list[[i]]$years, n=20)
                        }
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
                        cmd <- paste0(cdoprefix, " -s ntime ", selfile_vec[chunki])
                        message("\nrun `", cmd, "`")
                        cdo_ntime <- system(cmd, intern=T)
                        message("ntime --> \"", cdo_ntime, "\"", appendLF=F)
                        if (cdo_ntime == "") {
                            stop("something is wrong with this file")
                        }
                        cdo_ntime <- as.integer(cdo_ntime)
                        message(" --> ", cdo_ntime)

                        # get time dimension values with `cdo showtimstamp` on the result 
                        # of `cdo -fldmean -select,name=var` 
                        # note: `cdo date` only shows the correct number of dates if the `time` dim is 
                        #       defined correctly. `cdo showtimestamp` or `cdo showtime`, instead, always
                        #       return the correct number of time values
                        #       --> use `cdo showtimestamp` here instead of `cdo showdate`
                        cdo_showtimestamp_file <- paste0(dirname(fout_vec[chunki]), "/tmp_cdo_showtimestamp_",
                                                         Sys.getpid(), "_chunk_", chunki, "_of_", nchunks, ".txt")
                        cmd <- paste0(cdoprefix, " -s showtimestamp ", selfile_vec[chunki],
                                      " > ", cdo_showtimestamp_file)
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
                    msg <- NULL
                    if (!is.null(new_date_list[[i]]$dates)) {
                        if (length(new_date_list[[i]]$dates) != cdo_ntime_fout) {
                            msg <- paste0("provided `new_date_list[[", i, "]]$dates` is of length ", 
                                          length(new_date_list[[i]]$dates), 
                                          " but `cdo ntime` of cdo selection (and calculation) result is ", 
                                          cdo_ntime_fout)
                        }
                    } else {
                        if (!is.null(new_date_list[[i]]$years)) {
                            if (length(new_date_list[[i]]$years) != cdo_ntime_fout) {
                            msg <- paste0("provided `new_date_list[[", i, "]]$years` is of length ", 
                                          length(new_date_list[[i]]$years), 
                                          " but `cdo ntime` of cdo selection (and calculation) result is ", 
                                          cdo_ntime_fout)
                            }
                        }
                    }
                    if (!is.null(msg)) stop(msg)

                    # construct new dates
                    message("\nconstruct new time dimension values of ", nchunks, " chunk", 
                            ifelse(nchunks > 1, "s", ""), " ...")
                    dates_out_list <- vector("list", l=nchunks)
                    for (chunki in seq_len(nchunks)) {

                        message("\nchunk ", chunki, "/", nchunks, " ...")
                        
                        # user did not provide new dates or years
                        # --> construct new years
                        if (is.null(new_date_list[[i]]$dates) && is.null(new_date_list[[i]]$years)) { 
                            
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
                                    if (datapath == "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom" &&
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

                                    } else if (datapath == "/ace/user/cdanek/out/cosmos-aso-wiso/Hol-T/outdata/mpiom" &&
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

                        } else { # if new dates or years are given by user

                            if (!is.null(new_date_list[[i]]$dates)) { # user provided dates
                                dates_out <- new_date_list[[i]]$dates[dates_in_list[[chunki]]$time_inds]
                                message("new ", length(dates_out), " dates_out:")
                                ht(dates_out)
                                dates_out_lt <- as.POSIXlt(dates_out)
                                years_out <- dates_out_lt$year+1900
                                months_out <- dates_out_lt$mon+1
                                days_out <- dates_out_lt$mday
                                dates_out_ncap <- dates_out_lt
                            
                            } else if (!is.null(new_date_list[[i]]$years)) { # user provided years
                                # --> construct months and days from cdo file information 
                                years_out <- new_date_list[[i]]$years[dates_in_list[[chunki]]$time_inds]
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

                                if (cdo_set_rel_time) {
                                    message("\n`cdo_set_rel_time`=T --> run `make_posixlt_origin(years_out=c(",
                                            paste(c(head(years_out), "...", tail(years_out)), collapse=","), "))` ...")
                                    dates_out_ncap <- make_posixlt_origin(years_out)
                                    dates_out_ncap$mon <- months_out - 1 # posix months start counting from zero
                                    dates_out_ncap$mday <- days_out
                                }

                            } # if user provided new years
                        } # if user provided new dates or years

                        # construct new ncap2 dates and time unit and origin
                        if (cdo_set_rel_time) { # relative time
                            if (is.null(new_date_list[[i]]$nc_time_origin)) {
                                message("`\ncdo_set_rel_time`=T but new_date_list[[", i, "]]$nc_time_origin is not set\n",
                                        "--> use first date ", dates_out[1], " as origin")
                                new_date_list[[i]]$nc_time_origin <- dates_out[1]
                            }
                            new_date_list[[i]]$nc_time_units <- paste0("days since ", new_date_list[[i]]$nc_time_origin)
                            dates_out_ncap_dt_rel <- difftime(time1=dates_out_ncap[1], 
                                                              time2=new_date_list[[i]]$nc_time_origin, 
                                                              units="days") # days since origin
                            if (cdo_ntime_fout > 1) {
                                dates_out_ncap <- difftime(dates_out_ncap[2:length(dates_out_ncap)], 
                                                           dates_out_ncap[1:(length(dates_out_ncap) - 1)], 
                                                           units="days")
                                dates_out_ncap <- c(dates_out_ncap_dt_rel, dates_out_ncap_dt_rel + cumsum(as.numeric(dates_out_ncap)))
                            } else {
                                dates_out_ncap <- dates_out_ncap_dt_rel
                            }
                        } else { # absolute time
                            if (is.null(new_date_list[[i]]$nc_time_units)) { # use default absolute time format
                                # default absolute time unit "day as %Y%m%d.%f" --> only allowed absolute time unit for cdo
                                new_date_list[[i]]$nc_time_units <- "day as %Y%m%d.%f" # only allowed absolute time for cdo
                                message("`\ncdo_set_rel_time`=F but new_date_list[[", i, "]]$nc_time_units is not set\n",
                                        "--> use default absolute time unit \"", new_date_list[[i]]$nc_time_units, "\"")
                            }
                            if (new_date_list[[i]]$nc_time_units == "day as %Y%m%d.%f") { # default
                                dates_out_ncap <- paste0(sprintf("%04i", years_out), 
                                                         sprintf("%02i", months_out),
                                                         sprintf("%02i", days_out), ".0")
                            } else if (new_date_list[[i]]$nc_time_units == "years as %Y.%f") {
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
                                stop("absolute time unit `new_date_list[[", i, "]]$nc_time_units` = \"", 
                                     new_date_list[[i]]$nc_time_units, "\" not defined")
                            }
                            
                            message("\ndates_out_ncap:")
                            ht(dates_out_ncap, n=25)
                            if (length(dates_out_ncap) != length(dates_out)) stop("this should not happen")

                        } # if cdo_set_rel_time or not
                        
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
                       
                        if (file.exists(nco_fout_vec[chunki])) {
                            message("\nnco_fout_vec[chunki] = \"", nco_fout_vec[chunki], "\" alread exists. skip ncap2 command")
                        
                        } else {
                            # if input has absolute time units but `new_date_list[[i]]$nc_time_units` shall be relative,
                            # the input needs to be converted from absolute to relative time units
                            # before setting new time values with nco ncap2
                            # todo: need to update this with a check
                            #cmd_cp_and_mv <- paste0("cp ", fout_vec[chunki], " ", nco_fout_vec[chunki])
                            cmd_cp_and_mv <- paste0(cdoprefix, " --no_history -r copy ", selfile_vec[chunki], " ", nco_fout_vec[chunki])
                            
                            # get time dimension name
                            # --> after many tries with cdo/nco, the combination of ncdump and `known_dimnames` is the best way
                            ncdump <- Sys.which("ncdump")
                            if (ncdump == "") stop("did not find program ncdump")
                            tdimname <- paste0(ncdump , " -h ", selfile_vec[chunki])
                            message("run `", tdimname, "` ...")
                            tdimname <- system(tdimname, intern=T)
                            tdimname <- tdimname[(which(tdimname == "dimensions:")+1):(which(tdimname == "variables:")-1)] # e.g. "\ttime = UNLIMITED ; // (432 currently)"
                            tdimname <- gsub("^\t", "", tdimname) # e.g. "time = UNLIMITED ; // (432 currently)"
                            tdimname <- strsplit(tdimname, "=") # e.g. "time ", " UNLIMITED ; // (432 currently)"
                            tdimname <- sapply(tdimname, "[[", 1) # e.g. "time "
                            tdimname <- trimws(tdimname) # e.g. "time"
                            ind <- which(!is.na(match(tdimname, known_dimnames$time)))
                            if (length(ind) != 1) {
                                stop("could not find the time dimension in ", length(tdimnames), " file dimensions ", 
                                     paste(tdimname, collapse=", "), " based on `known_dimnames$time = ", 
                                     paste(knonw_dimnames$time, collapse=", "))
                            }
                            tdimname <- tdimname[ind]
                            message("--> input time dim name = \"", tdimname, "\"")
                           
                            # ncap2 command with new time vals
                            cmd_ncap2 <- paste0(nco_ncap2, " -O -h -s '", tdimname, "(:)={<dates_out_ncap>}; ", tdimname, "@units=\"",
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
                                    ncofile_vec[nco_ncap2_chunki] <- sub(".nc", # all files are .nc if new times shall be applied
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
                                        " --> does not yield the error \"Argument list too long\"")
                                # do not select time steps in nco ncap2 chunks but just make a copy and rename
                                system(cmd_cp_and_mv)
                                #system(paste0("cdo -r copy ", nco_fout_vec[chunki], " ~/tmp && mv ~/tmp ", nco_fout_vec[chunki])) 
                                system(cmd_ncap2_tmp)
                                # --> this call does not capture "-bash: /usr/bin/ncap2: Argument list too long"
                                # however, for cdo it does! dont know why
                                
                                if (clean) {
                                    if (file.exists(selfile_vec[chunki])) {
                                        cmd <- paste0("rm -v ", selfile_vec)
                                        message("run `", cmd, "` ...")
                                        system(cmd)
                                    }
                                }

                            } # if nco ncap2 argument is too long

                        } # if file.exists(nco_fout_vec[chunki])

                    } # for cdo chunki: possible chunks if argument is too long
           
                    # new time was applied to result of cdo selection 
                    # --> continue with these files
                    selfile_vec_old <- selfile_vec
                    selfile_vec <- nco_fout_vec
                
                } # if !is.null(new_date_list[[i]])
                
                # apply new mask to result of cdo selection if wanted before cdo calculation
                if (!is.null(mask_list[[i]])) {

                    message("\n`mask_list[[", i, "]]` is not NULL:")
                    cat(capture.output(str(mask_list[[i]])), sep="\n")
                    message("\n--> apply mask to ", nchunks, " selection result chunk", 
                            ifelse(nchunks > 1, "s", ""), " ...")

                    # construct and apply mask command
                    maskfile_vec <- selfile_vec 
                    for (chunki in seq_len(nchunks)) {
                    
                        message("\nchunk ", chunki, "/", nchunks, " ...")
                        
                        maskfile_vec[chunki] <- paste0(dirname(selfile_vec[chunki]), "/", 
                                                       tools::file_path_sans_ext(basename(selfile_vec[chunki])),
                                                       "_mask.", tools::file_ext(selfile_vec[chunki]))
                        
                        if (file.exists(maskfile_vec[chunki])) {
                            message("\nmaskfile_vec[chunki] = \"", maskfile_vec[chunki], "\" alread exists. skip mask command")
                        } else {
                        
                            if (!is.null(mask_list[[i]]$cdo_mask)) {
                                #cdo -setctomiss,0 -mul [ -select,name=var7 data.nc ] -eqc,5 -select,name=open_ocean ", workpath, "/mesh/lsm/T63/RECCAP2_region_masks_all_lonlat_T63_remapnn.nc test
                                #cdo -setctomiss,0 -mul data.nc -eqc,5 -select,name=open_ocean ", workpath, "/mesh/lsm/T63/RECCAP2_region_masks_all_lonlat_T63_remapnn.nc test
                                message("cdo_mask: `", mask_list[[i]]$cdo_mask, "`")
                                cmd <- paste0(cdoprefix, " -setctomiss,0 -mul ", selfile_vec[chunki], " ", mask_list[[i]]$cdo_mask, " ", maskfile_vec[chunki])
                            } else {
                                stop("only cdo case implemented for mask yet")
                            }
                            mask_list[[chunki]]$mask_cmd <- cmd
                            message("\nrun mask_cmd: `", cmd, "`")
                            system(mask_list[[chunki]]$mask_cmd)
                        } # if (file.exists(maskfile_vec[chunki]))

                    } # for chunki 
                    
                    # --> continue with masked files
                    selfile_vec_old <- selfile_vec
                    selfile_vec <- maskfile_vec
                    if (clean) {
                        message("`\nclean`=T --> remove cdo selection results before mask ...")
                        for (chunki in seq_len(nchunks)) {
                            message("rm -v ", selfile_vec_old[chunki])
                            system(paste0("rm -v ", selfile_vec_old[chunki]))
                        }
                    }
                } # if !is.null(mask_list[[i]])

                # run cdo calculation after applying new dates and/or mask to cdo selection result
                if (cdo_chain == "separate") {
                    if (grepl("fldint", modes[[i]])) {
                        # non-default command `fldint` is calculated at the end
                        # add further commands here if necessary
                        message("\nmodes[[", i, "]] = \"", paste(modes[[i]], collapse="\", \""), "\" includes \"fldint\" --> ",
                                "skip calculation here but rename tmp files:")
                        for (chunki in seq_len(nchunks)) { # for possible chunks if argument is too long
                            message("chunk ", chunki, "/", nchunks, ": mv ", selfile_vec[chunki], " ", fout_vec[chunki])
                            invisible(file.rename(selfile_vec[chunki], fout_vec[chunki]))
                        }

                    } else { # run separate cdo calc after applying new dates and/or mask to cdo selection result
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
                                if (file.exists(fout_vec[chunki]) && !post_force) {
                                    message("fout_vec[", chunki, "] =\n",
                                            "   \"", fout_vec[chunki], "\"\n",
                                            "already exists and `post_force`=F. skip ...")
                                    ticcmd <- toccmd <- NULL
                                } else {
                                    message("via `", cmd_source, "`")
                                    message("this may take some time for ", nfiles_per_chunk, " file",
                                            ifelse(nfiles_per_chunk > 1, "s", ""), " ...")
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
                    
                    } # if modes[i] != fldint
                } # if cdo_chain == "separate"
                # finished applying new dates and/or mask to cdo selection result and subsequent cdo calculation

            } # finished check if requested variable is in first found file

        } # if run special function or default way
       
        ## from here, cdo selection and calculation or commands from `cdo_known_cmds` or special functions are finished for all chunks
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
        
        # or just rename if necessary 
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
        if (!own_cmd && !is.null(new_date_list[[i]])) {
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

            # todo: remove original calendar time attribute if new dates were set
            if (!is.null(new_date_list[[i]]$dates)) {
                message("\ncheck if original time has `calendar` attribute ...")
                tinfo <- paste0(cdo, " -s tinfo ", fout)
                message("run `", tinfo, "` ...")
                tinfo <- system(tinfo, intern=T)
                if (any(grepl("Calendar = ", tinfo))) {
                    message("--> detected `calendar` attribute:\n", tinfo[grepl("Calendar = ", tinfo)])
                    # get time dimension name
                    # --> after many tries with cdo/nco, the combination of ncdump and `known_dimnames` is the best way
                    ncdump <- Sys.which("ncdump")
                    if (ncdump == "") stop("did not find program ncdump")
                    tdimname <- paste0(ncdump , " -h ", fout)
                    message("run `", tdimname, "` ...")
                    tdimname <- system(tdimname, intern=T)
                    tdimname <- tdimname[(which(tdimname == "dimensions:")+1):(which(tdimname == "variables:")-1)] # e.g. "\ttime = UNLIMITED ; // (432 currently)"
                    tdimname <- gsub("^\t", "", tdimname) # e.g. "time = UNLIMITED ; // (432 currently)"
                    tdimname <- strsplit(tdimname, "=") # e.g. "time ", " UNLIMITED ; // (432 currently)"
                    tdimname <- sapply(tdimname, "[[", 1) # e.g. "time "
                    tdimname <- trimws(tdimname) # e.g. "time"
                    ind <- which(!is.na(match(tdimname, known_dimnames$time)))
                    if (length(ind) != 1) {
                        stop("could not find the time dimension in ", length(tdimnames), " file dimensions ", 
                             paste(tdimname, collapse=", "), " based on `known_dimnames$time = ", 
                             paste(knonw_dimnames$time, collapse=", "))
                    }
                    tdimname <- tdimname[ind]
                    message("--> input time dim name = \"", tdimname, "\"")
                    
                    nco_ncatted <- Sys.which("ncatted")
                    if (nco_ncatted == "") {
                        warning("ncatted not found. skip removing `calendar` attribute of time dim \"", tdimname, "\"")
                    } else { # remove calendar attribute
                        cmd <- paste0(nco_ncatted, " -O -a calendar,", tdimname, ",d,, ", fout) # capital `Calendar` does not work, must be `calendar`
                        message("--> remove `calendar` attribute of time dim \"", tdimname, "\": run `", cmd, "` ...")
                        system(cmd)
                    }
                } # if time dim has Calendar attribute
            } # if new dates were provided
        } # if !is.null(new_date_list[[i]])

        if (grepl("fldint", modes[[i]])) {
            message("\nmodes[[", i, "]] = \"", paste(modes[[i]], collapse="\", \""), "\" includes \"fldint\" --> ",
                    "calc spatial integral via result of `cdo gridarea` ...")
            check <- T # default
            # check if model is already supported
            if (any(models[i] == c("fesom", "mom4"))) {
                message("model \"", models[i], "\" is not supported yet --> skip fldint calculation")
                check <- F
            }
            # check if fout still has lon,lat dims
            cmd <- paste0(cdo, " -s griddes ", fout)
            message("run `", cmd, "` ...")
            griddes <- system(cmd, intern=T)
            gridtype <- which(base::startsWith(griddes, "gridtype"))
            gridtype <- gsub(" ", "", griddes[gridtype]) # e.g. "gridtype  = curvilinear"
            gridtype <- strsplit(gridtype, "=")[[1]][2]
            message("--> gridtype = \"", gridtype, "\"")
            if (gridtype == "generic") {
                stop("generic gridtype not supported for fldint")
            } else { # gridtype not generic
                xsize <- which(base::startsWith(griddes, "xsize"))
                xsize <- gsub(" ", "", griddes[xsize]) # e.g. "xsize=192"
                xsize <- as.integer(strsplit(xsize, "=")[[1]][2]) # e.g. 192
                message("--> xsize= \"", xsize, "\"")
                ysize <- which(base::startsWith(griddes, "ysize"))
                ysize <- gsub(" ", "", griddes[ysize]) # e.g. "ysize=96"
                ysize <- as.integer(strsplit(ysize, "=")[[1]][2]) # e.g. 96
                message("--> ysize= \"", ysize, "\"")
                if (xsize == 1 && ysize == 1) {
                    message("xsize and ysize = 1 --> skip fldint calculation")
                    check <- F
                }
                if (check) {
                    area_file <- paste0(postpaths[i], "/tmp_area_m2_", Sys.getpid())
                    cmd <- paste0(cdo, " gridarea ", fout, " ", area_file)
                    message("run `", cmd, "`")
                    system(cmd)
                    mul_file <- paste0(postpaths[i], "/tmp_mul_", Sys.getpid())
                    cmd <- paste0(cdo, " -fldsum -mul ", fout, " ", area_file, " ", mul_file, 
                                  " && mv ", mul_file, " ", fout)
                    cmd <- paste0(cmd, " || echo error")
                    message("run `", cmd, "`")
                    system(cmd)
                    if (!file.exists(fout)) {
                        stop("multiplication result data*area file ", fout, " does not exist")
                    }
                    # check if success --> result has nx = ny = 1
                    message("check if fldint result has nx = ny = 1 ...")
                    cmd <- paste0(cdo, " -s griddes ", fout)
                    message("run `", cmd, "` ...")
                    griddes <- system(cmd, intern=T)
                    xsize <- which(base::startsWith(griddes, "xsize"))
                    xsize <- gsub(" ", "", griddes[xsize]) # e.g. "xsize=192"
                    xsize <- as.integer(strsplit(xsize, "=")[[1]][2]) # e.g. 192
                    message("--> xsize= \"", xsize, "\"")
                    ysize <- which(base::startsWith(griddes, "ysize"))
                    ysize <- gsub(" ", "", griddes[ysize]) # e.g. "ysize=96"
                    ysize <- as.integer(strsplit(ysize, "=")[[1]][2]) # e.g. 96
                    message("--> ysize= \"", ysize, "\"")
                    if (xsize != 1 || ysize != 1) {
                        stop("xsize and/or ysize != 1 --> fldint error")
                    }
                    if (clean) invisible(file.remove(area_file))
                } # if check
            } # if input gridtype is generic
        } # if fldint

        # set relative time axis
        if (!own_cmd && cdo_set_rel_time && is.null(new_date_list[[i]])) {
            message("\n`cdo_set_rel_time`=T --> set relative time axis ...")
            reltime_file <- paste0(postpaths[i], "/tmp_reltime_", Sys.getpid())
            cmd <- paste0("cdo ", cdo_silent, " --no_history -r copy ", fout, " ", reltime_file, 
                          " && mv ", reltime_file, " ", fout)
            cmd <- paste0(cmd, " || echo error")
            message("run `", cmd, "`")
            system(cmd)
        } else {
            message("\n", "`cdo_set_rel_time`=F --> do not set relative time axis ...")
        }

        # set time_bnds if needed (if there is time dim) and not already there
        # --> time_bnds are necessary for `cdo cat`
        message("\ncheck if time_bnds are needed (= if there is a time dim) and not there yet ...") 
        # needs ncap2 >= 4.6.7
        # todo: how to check if file has time dims?
        cmd <- paste0(cdo, " -s sinfo ", fout) # short info 
        message("run `", cmd, "` ...")
        sinfo <- system(cmd, intern=T)
        if (any(grepl("   Time coordinate :", sinfo))) { # there is time dim
            if (any(grepl("Bounds = true", sinfo))) { 
                message("--> \"Bounds = true\" found --> time_bnds already set")
            } else { # time_bnds not set
                message("--> \"Bounds = true\" missing --> time_bnds not set")
                make_bounds <- F # default: ncap2:make_bounds not available
                if (!exists("nco_ncap2")) {
                    message("`nco_ncap2` not set by user -> check if ncap2 binary can be found to set time_bnds of nc file: run `which(ncap2)`")
                    nco_ncap2 <- Sys.which("ncap2")
                }
                if (nco_ncap2 == "") {
                    warning("ncap2 not found. skip make_bounds")
                } else {
                    message("--> ncap2 = ", nco_ncap2)
                    nco_ncap2_version <- paste0(nco_ncap2, " --version 2>&1")
                    nco_ncap2_version <- system(nco_ncap2_version, intern=T)
                    nco_ncap2_version <- nco_ncap2_version[2] # e.g. "ncap2 version 4.4.4"
                    nco_ncap2_version <- strsplit(nco_ncap2_version, " ")[[1]]
                    nco_ncap2_version <- nco_ncap2_version[3]
                    nco_ncap2_version <- base::numeric_version(paste(nco_ncap2_version, collapse="."))
                    message("--> ncap2 version = ", nco_ncap2_version, appendLF=F)
                    if (nco_ncap2_version < base::numeric_version("4.6.7")) {
                        message(" < 4.6.7 --> need ncap2 version >= 4.6.7. skip make_bounds")
                    } else {
                        message(" >= 4.6.7 --> ok")
                        make_bounds <- T # success
                    }
                }
                if (make_bounds) {
                    # get time dimension name
                    # --> after many tries with cdo/nco, the combination of ncdump and `known_dimnames` is the best way
                    if (!exists("ncudmp")) {
                        message("`ncdump` not set by user -> check if ncdump binary can be found: run `which(ncdump)`")
                        ncdump <- Sys.which("ncdump")
                    }
                    if (ncdump == "") stop("did not find program ncdump")
                    tdimname <- paste0(ncdump , " -h ", fout)
                    message("run `", tdimname, "` ...")
                    tdimname <- system(tdimname, intern=T)
                    tdimname <- tdimname[(which(tdimname == "dimensions:")[1]+1):(which(tdimname == "variables:")[1]-1)] # e.g. "\ttime = UNLIMITED ; // (432 currently)"
                    tdimname <- gsub("^\t", "", tdimname) # e.g. "time = UNLIMITED ; // (432 currently)"
                    tdimname <- strsplit(tdimname, "=") # e.g. "time ", " UNLIMITED ; // (432 currently)"
                    tdimname <- sapply(tdimname, "[[", 1) # e.g. "time "
                    tdimname <- trimws(tdimname) # e.g. "time"
                    message("--> ", paste(tdimname, collapse=", "))
                    ind <- which(!is.na(match(tdimname, known_dimnames$time)))
                    if (length(ind) != 1) {
                        stop("could not find the time dimension in ", length(tdimnames), " file dimensions ", 
                             paste(tdimname, collapse=", "), " based on `known_dimnames$time = ", 
                             paste(knonw_dimnames$time, collapse=", "))
                    }
                    tdimname <- tdimname[ind]
                    message("--> input time dim name = \"", tdimname, "\"")
                    fout_ncap2 <- paste0(fout, "_ncap2_make_bounds")
                    cmd <- paste0(nco_ncap2, " -4 -s 'defdim(\"bnds\",2); time_bnds=make_bounds(", tdimname, ",$bnds,\"time_bnds\");' ", 
                                  fout, " ", fout_ncap2)
                    # -4 for ncdf4 support and large files
                    # problem: exceeded memory limit (3006140416 > 2684354560), being killed
                    message("run `", cmd, "` ...")
                    system(cmd)
                    if (!file.exists(fout_ncap2)) {
                        stop("ncap2 make_bounds file ", fout_ncap2, " does not exist but should")
                    }
                    invisible(file.rename(fout_ncap2, fout))
                } # if ncap2:make_bounds available
            } # if time_bnds not yet set
        } # if fout has time dim


        ## run special functions at the end    
        if (models[i] == "mpiom1") {
            
            # run mpiom1_remap2lonlat() function on timmean output
            if (mpiom1_remap[i]) {
                message("\n`models[", i, "]` = \"", models[i], 
                        "\" and `mpiom1_remap` = T --> run `mpiom1_remap2lonlat()` ...")
                cmd <- "mpiom1_remap2lonlat(files=fout, cdo=cdo"
                if (exists("mpiom1_remap2lonlat_arg_list")) {
                    if (!is.null(mpiom1_remap2lonlat_arg_list[[i]])) {
                        message("--> use provided `mpiom1_remap2lonlat_arg_list[[", i, "]]`:")
                        cat(capture.output(str(mpiom1_remap2lonlat_arg_list[[i]])), sep="\n")
                        for (argi in seq_along(mpiom1_remap2lonlat_arg_list[[i]])) {
                            cmd <- paste0(cmd, ", ", 
                                          names(mpiom1_remap2lonlat_arg_list[[i]])[argi], 
                                          "=mpiom1_remap2lonlat_arg_list[[", i, "]]$",
                                          names(mpiom1_remap2lonlat_arg_list[[i]])[argi])
                        }
                    } else {
                        message("--> provided `mpiom1_remap2lonlat_arg_list[[", i, "]]` is NULL --> use defaults")
                    }
                } else {
                    message("--> `mpiom1_remap2lonlat_arg_list` not provided --> use defaults")
                }
                cmd <- paste0(cmd, ")")
                message("run `", cmd, "` ...")
                eval(parse(text=cmd))

            } else {
                message("\n`models[", i, "]` = \"", models[i], 
                        "\" but `mpiom1_remap` = F --> do not run `mpiom1_remap2lonlat()` ...")

            } # if mpiom1_remap[i] or not

            if (any(fvarnames[i] == c("amoc", "gmoc"))) { # run mpiom_moc_make_bottom_topo()
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
                if (!(any(grepl("timmean", modes[[i]])))) {
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

                } # if !(any(grepl("timmean", modes[[i]])))

            } # if *moc
        
        } # which model
        # finished running model-dependent stuff in the end
        
        toc <- Sys.time()
        elapsed[[i]] <- toc - tic
        message("\nsetting ", i, "/", nsettings , " took ", elapsed[[i]], " ", 
                attributes(elapsed[[i]])$units, " for ", models[i], " ", names(modes)[i], 
                " = ", paste(modes[[i]], collapse=", "), " calculation of ",
                length(files), " file", ifelse(length(files) > 1, "s", ""))

        # restore user options for next setting
        cdo_set_rel_time <- cdo_set_rel_time_old
    
    } # if fout_exist_check (if output file already exists or not)            
        
    # add output to lastfiles post file
    write(fout, file=lastfiles_post_fname, append=T)

} # for i nsettings

if (options()$warn != 0) options(warn=0) # back to default

message("\nfinished\n")

