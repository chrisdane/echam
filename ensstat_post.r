# r

# ensemble stats of post processed data

rm(list=ls()); graphics.off()
warn <- options()$warn

cdo <- Sys.which("cdo")
ensstats <- c("ensmean", "ensmedian", "ensmin", "ensmax", 
              "ensstd", "ensstd1", "ensvar", "ensvar1", 
              "enspctl,10", "enspctl,25", "enspctl,75", "enspctl,90")
calc_annual_means_before <- T
force <- F

patterns_in <- c("_historical_r", 
                 paste0("_ssp", c(126, 245, 370, 585), "_r"), 
                 paste0("_historical_and_ssp", c("126", "245", "370", "585"), "_r"))
patterns_out <- c("historical",
                  paste0("ssp", c(126, 245, 370, 585)),
                  paste0("historical_and_ssp", c("126", "245", "370", "585")))
if (length(patterns_in) != length(patterns_out)) stop("patterns_in and patterns_out must be of same length")
patterns_in2 <- c("1970-2014",
                  rep("2015-2100", t=4),
                  rep("1970-2100", t=4))
patterns_out2 <- patterns_in2
if (length(patterns_in2) != length(patterns_out2)) stop("patterns_in2 and patterns_out2 must be of same length")

if (F) {
    patterns <- paste0("*", patterns_in, "*_fldmean_rsdo_sellevidx_1_NH_66_Jan-Dec_", patterns_in2, ".nc")
    fouts <- paste0("/work/ba1103/a270073/post/cmip6/fldmean/rsdo/ensstat_cmip6_",
                    patterns_out, "_<n_realizations>_realizations_fldmean_rsdo_sellevidx_1_NH_66_Jan-Dec_", 
                    patterns_out2, ".nc")
} else if (F) {
    patterns <- paste0("*", patterns_in, "*_fldmean_thetao_levrange_*_vertmean_NH_66_Jan-Dec_", patterns_in2, ".nc")
    fouts <- paste0("/work/ba1103/a270073/post/cmip6/fldmean/thetao/ensstat_cmip6_",
                    patterns_out, "_<n_realizations>_realizations_fldmean_thetao_levrange_100-600m_vertmean_NH_66_Jan-Dec_", 
                    patterns_out2, ".nc")
} else if (F) {
    patterns <- paste0("*", patterns_in, "*_fldmean_mlotst_NH_66_Mar_", patterns_in2, ".nc")
    fouts <- paste0("/work/ba1103/a270073/post/cmip6/fldmean/mlotst/ensstat_cmip6_",
                    patterns_out, "_<n_realizations>_realizations_fldmean_mlotst_NH_66_Mar_", 
                    patterns_out2, ".nc")
} else if (F) {
    patterns <- paste0("*", patterns_in, "*_fldsum_siarea_NH_66_Sep_", patterns_in2, ".nc")
    fouts <- paste0("/work/ba1103/a270073/post/cmip6/fldsum/siarea/ensstat_cmip6_",
                    patterns_out, "_<n_realizations>_realizations_fldsum_siarea_NH_66_Sep_", 
                    patterns_out2, ".nc")
} else if (F) {
    patterns <- paste0("*", patterns_in, "*_fldmean_o2_levrange_*_vertmean_NH_66_Jan-Dec_", patterns_in2, ".nc")
    fouts <- paste0("/work/ba1103/a270073/post/cmip6/fldmean/o2/ensstat_cmip6_",
                    patterns_out, "_<n_realizations>_realizations_fldmean_o2_levrange_100-600m_vertmean_NH_66_Jan-Dec_", 
                    patterns_out2, ".nc")
} else if (F) {
    patterns <- paste0("*", patterns_in, "*_fldint_intpp_NH_66_Jan-Dec_", patterns_in2, ".nc")
    fouts <- paste0("/work/ba1103/a270073/post/cmip6/fldint/intpp/ensstat_cmip6_",
                    patterns_out, "_<n_realizations>_realizations_fldint_intpp_NH_66_Jan-Dec_", 
                    patterns_out2, ".nc")
} else if (T) {
    patterns <- paste0("*", patterns_in, "*_fldint_epc100_NH_66_Jan-Dec_", patterns_in2, ".nc")
    fouts <- paste0("/work/ba1103/a270073/post/cmip6/fldint/epc100/ensstat_cmip6_",
                    patterns_out, "_<n_realizations>_realizations_fldint_epc100_NH_66_Jan-Dec_", 
                    patterns_out2, ".nc")
} else if (F) {
    patterns <- paste0("*", patterns_in, "*_fldint_remoc_vertint_NH_66_Jan-Dec_", patterns_in2, ".nc")
    fouts <- paste0("/work/ba1103/a270073/post/cmip6/fldint/remoc/ensstat_cmip6_",
                    patterns_out, "_<n_realizations>_realizations_fldint_remoc_vertint_NH_66_Jan-Dec_", 
                    patterns_out2, ".nc")
}
    
postpaths <- rep("/work/ba1103/a270073/post", t=length(patterns))

#########################################################################

if (cdo == "") stop("could not find cdo")
if (length(postpaths) != length(patterns)) stop("`postpaths` and `patterns` must be of same length")
if (length(postpaths) != length(fouts)) stop("`postpaths` and `fouts` must be of same length")
if (length(patterns) != length(fouts)) stop("`patterns` and `fouts` must be of same length")
if (!all(grepl("<n_realizations>_realizations", fouts))) stop("all `fouts` must contain the pattern \"<n_realizations>_realizations\"")

dfs <- vector("list", l=length(patterns))
for (pati in seq_along(patterns)) {
    cmd <- paste0("find ", postpaths[pati], "/ -name \"", patterns[pati], "\" | sort")
    message("********************************************************************************************\n",
            "pattern ", pati, "/", length(patterns), "\n",
            "run `", cmd, "` ...")
    fs <- system(cmd, intern=T)
    if (length(fs) == 0) {
        message("--> found zero files. skip")
        next # pati
    }

    # remove files
    if (any(startsWith(patterns[pati], paste0("*_ssp", c(126, 245, 370, 585), "_r*_")))) {
        inds <- grep("_historical_and_ssp", fs)
        if (length(inds) > 0) {
            message("special remove ", length(inds), " \"historical_and_ssp\" files ...")
            fs <- fs[-inds]
            if (length(fs) == 0) {
                message("--> found zero files. skip")
                next # pati
            }
        }
    }
    
    # special: exclude CESM2-WACCM
    inds <- grep("_CESM2-WACCM_", fs)
    if (F && length(inds) > 0) {
        message("\nspecial: exclude ", length(inds), " _CESM2-WACCM_ files:")
        print(fs[inds])
        fs <- fs[-inds]
    }

    # check if ensstat result already exists and is mistakenly included in input files
    foutm1 <- sub("<n_realizations>", length(fs)-1, fouts[pati])
    if (any(fs == foutm1)) { 
        message("\nensstat file\n",
                "  ", foutm1, "\n",
                "already exists and was included by `pattern` --> exclude it from files ...")
        fs <- fs[-which(fs == foutm1)]
    }
    if (length(fs) == 0) {
        message("--> found zero files. skip")
        next # pati
    }
    
    # get realizations; my convention: models
    realizations <- basename(dirname(dirname(dirname(fs)))) 
    df <- data.frame(file=fs, realization=realizations)
    message("work on ", length(fs), " files:")
    print(df, right=F, width=300)
   
    # check realization duplicates; todo: implement further checks
    if (any(duplicated(realizations))) {
        realizations_unique <- unique(realizations)
        indsok <- rep(NA, t=length(realizations_unique))
        for (ri in seq_along(realizations_unique)) {
            inds <- which(realizations == realizations_unique[ri])
            if (length(inds) == 1) {
                indsok[ri] <- inds # continue with unique realization
            } else {
                message("there are ", length(inds), " ", realizations_unique[ri], " realizations. which shall be incuded? (e.g. ", inds[1], "):")
                print(df[inds,], right=F, width=300)
                if (interactive()) {
                    ind <- base::readline()
                } else {
                    ind <- base::readLines("stdin", n=1)
                }
                options(warn=2); ind <- as.integer(ind); options(warn=warn)
                if (is.na(match(ind, inds))) stop("provided ", ind, " is not one of ", paste(inds, collapse=","))
                indsok[ri] <- ind
            }
        } # for ri
        if (anyNA(indsok)) stop("this should not happen")
        realizations <- realizations[indsok]
        fs <- fs[indsok]
        df <- data.frame(file=fs, realization=realizations)
        message("\nremoved duplicated realizations and continue with ", length(fs), " files:")
        print(df, right=F, width=300)
        if (any(duplicated(realizations))) stop("there are still duplicated realizations")
    } # if any realization is duplicated
    
    # replace input files with annual means
    if (calc_annual_means_before) { 
        message("\ncalc and continue with yearmean ...")
        for (fi in seq_along(fs)) {
            if (grepl("_Jan-Dec_", fs[fi])) {
                fouti <- sub("_Jan-Dec_", "_annual_", fs[fi])
                if (!file.exists(fouti)) {
                    if (file.access(dirname(fouti), mode=2) != 0) {
                        stop("dirname of fouti = ", fouti, " not writeable. implement")
                    }
                    cmd <- cdo
                    if (F) cmd <- paste0(cmd, " -setlevel,0") # special
                    cmd <- paste0(cmd, " -yearmean ", fs[fi], " ", fouti)
                    message("run `", cmd, "` ...")
                    check <- system(cmd)
                    if (check != 0) stop("error")
                } else {
                    message("fouti ", fouti, " already exists. skip")
                }
                fs[fi] <- fouti
            } else {
                message("input file does not have \"_Jan-Dec_\" pattern. skip")
            }
        } # for fi
        df$file <- fs
        fouts[pati] <- sub("_Jan-Dec_", "_annual_", fouts[pati])
    } # special

    # outdir and fout
    fout <- sub("<n_realizations>", length(fs), fouts[pati])
    dir.create(dirname(fout), recursive=T, showWarnings=F)
    if (!dir.exists(dirname(fout))) stop("could not create outdir ", dirname(fout))
    
    # save for later
    dfs[[pati]] <- list(df=df, fout=fout)
   
    # calc ensstats
    if (file.exists(fout) && !force) {
        message("\n--> ensstat fout ", fout, " already exists and `force` is F. skip")
    } else {
        fouts_ens <- rep(NA, t=length(ensstats))
        for (ensi in seq_along(ensstats)) {
            fouts_ens[ensi] <- paste0(dirname(fout), "/", gsub("[[:punct:]]", "_", ensstats[ensi]), "_", Sys.getpid(), ".nc")
            cmd <- paste0(cdo, 
                          #" --pedantic", # stop on warnings
                          " -setname,", gsub("[[:punct:]]", "_", ensstats[ensi]), 
                          " -", ensstats[ensi], " <files> ", fouts_ens[ensi])
            message("\nensstat ", ensi, ": run `", cmd, "` ...")
            cmd <- sub("<files>", paste(fs, collapse=" "), cmd)
            check <- system(cmd)
            if (check != 0) stop("error")
            if (F) { # my debug
                for (fi in seq_along(fs)) { print(fs[fi]); system(paste0("cdo nlevel ", fs[fi])) }
                for (fi in seq_along(fs)) { print(fs[fi]); system(paste0("cdo showlevel ", fs[fi])) }
            }
        } # for ensi

        # merge all ensstat results
        cmd <- paste0(cdo, 
                      " -setattribute,realizations='", paste(realizations, collapse=";"), 
                      "' -merge ", paste(fouts_ens, collapse=" "), " ", fout)
        message("\nmerge ensstats: `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error")

        invisible(file.remove(fouts_ens))
    } # if fout already exists

} # for pati

if (T) { # special: only realizations of several patterns/experiments

    if (T) { # loziel
        pattern_inds <- lapply(patterns, startsWith, 
                               c("*_historical_and_ssp126_r*", "*_historical_and_ssp245_r*",
                                 "*_historical_and_ssp370_r*", "*_historical_and_ssp585_r*"))
        fout_pattern <- "of_5_exps_historical_ssp126_245_370_585_"
    }
    pattern_inds <- which(sapply(pattern_inds, any))
    if (length(pattern_inds) > 0) {
        message("\n##############################################################################################\n",
                "part 2: calc ensstats over realizations of files of those ", length(pattern_inds), " patterns:\n",
                paste(patterns[pattern_inds], collapse="\n"))
        
        df <- lapply(dfs[pattern_inds], "[[", "df")
        realizations <- lapply(df, "[[", "realization")
        realizations_unique <- sort(unique(unlist(realizations)))
        realizations_check <- rep(F, t=length(realizations_unique))
        names(realizations_check) <- realizations_unique
        for (ri in seq_along(realizations_check)) {
            realization_inds <- lapply(realizations, "==", realizations_unique[ri])
            realization_inds <- sapply(lapply(realization_inds, which), length)
            realization_inds <- which(realization_inds == 1)
            message("**************************************************************************\n",
                    "realization ", ri, "/", length(realizations_unique), " ", realizations_unique[ri], 
                    " is in ", length(realization_inds), "/", length(pattern_inds), " patterns:")
            if (length(realization_inds) > 0) {
                message(paste(patterns[realization_inds], collapse="\n"))
            }
            if (length(realization_inds) == length(pattern_inds)) { # if realization is in all wanted experiment patterns
                realizations_check[ri] <- T
            }
        } # for ri

        if (T) { # loziel; continue with CESM2, CESM2-WACCM, IPSL-CM6A-LR, MPI-ESM1-2-HR, MPI-ESM1-2-LR only
            mymodels <- c("CESM2", "CESM2-WACCM", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR")
            message("\nspecial: continue with models ", paste(mymodels, collapse=", "), " only ...")
            myinds <- match(mymodels, names(realizations_check))
            if (any(is.na(myinds))) stop("asdasdassdda")
            realizations_check[] <- F
            realizations_check[myinds] <- T
            fout_pattern <- paste0(fout_pattern, "_", length(mymodels), "_models_", paste(mymodels, collapse="_"), "_")
            fout_pattern <- gsub("__", "_", fout_pattern)
        }

        if (any(realizations_check)) { # some realizations occur in all wanted experiment patterns
            realizations_check <- names(realizations_check)[which(realizations_check)]
            message("\n--> ", length(realizations_check), " realizations of ", 
                    length(realizations_unique), " unique realizations occur in all wanted patterns:\n",
                    paste(realizations_check, collapse="\n"))
            
            for (pati in seq_along(pattern_inds)) {
                
                fout <- fouts[pattern_inds[pati]]
                fout <- sub("<n_realizations>_realizations_", 
                            paste0(length(realizations_check), "_realizations_", fout_pattern), 
                            fout)
                message("\n**********************************************\n",
                        "ensstats of pattern ", pati, "/", length(pattern_inds), ": ", patterns[pattern_inds[pati]], "\n",
                        "--> fout = ", fout)
                if (file.exists(fout) && !force) {
                    message("\n--> fout already exists and `force` is F. skip")
                } else {
                    
                    # calc ensstats
                    fouts_ens <- rep(NA, t=length(ensstats))
                    for (ensi in seq_along(ensstats)) {
                        fouts_ens[ensi] <- paste0(dirname(fout), "/", gsub("[[:punct:]]", "_", ensstats[ensi]), "_", Sys.getpid(), ".nc")
                        cmd <- paste0(cdo, 
                                      #" --pedantic", # stop on warnings
                                      " -setname,", gsub("[[:punct:]]", "_", ensstats[ensi]), 
                                      " -", ensstats[ensi], " <files> ", fouts_ens[ensi])
                        message("\nensstat ", ensi, ": run `", cmd, "` ...")
                        realization_inds <- match(realizations_check, dfs[[pattern_inds[pati]]]$df$realization) 
                        if (anyNA(realization_inds)) stop("this should not happen")
                        if (length(realization_inds) != length(realizations_check)) stop("this should not happen")
                        cmd <- sub("<files>", paste(dfs[[pattern_inds[pati]]]$df$file[realization_inds], collapse=" "), cmd)
                        print(dfs[[pattern_inds[pati]]]$df$file[realization_inds])
                        check <- system(cmd)
                        if (check != 0) stop("error")
                    } # for ensi
                    
                    # merge all ensstat results
                    cmd <- paste0(cdo, 
                                  " -setattribute,realizations='", paste(realizations_check, collapse=";"), 
                                  "' -merge ", paste(fouts_ens, collapse=" "), " ", fout)
                    message("\nmerge ensstats: `", cmd, "` ...")
                    check <- system(cmd)
                    if (check != 0) stop("error")
                
                    invisible(file.remove(fouts_ens))

                } # if fout already exists
                
            } # for pati

        } else {
            message("\n--> not a single realization occurs in all those wanted patterns")
        }
    } else {
        message("\ndid not find provided patterns in `patterns` to calc ensstats over several patterns")
    }

} else {
    message("\nenable here to calc ensstats over several `patterns`")
} # special: only realizations of several experiments

message("\nfinished\n")

