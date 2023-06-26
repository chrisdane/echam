# r

rm(list=ls()); graphics.off()
options(warn=2) # stop on warning

force <- F
postpath <- "/work/ba1103/a270073/post"

historical_grep <- "_historical_"
#historical_grep <- "_historical2_"
ssp_grep <- "_ssp126_"
replacement <- "_historical_and_ssp126_"

#########################################################################

models <- list.dirs(postpath, full.names=F, recursive=F)
for (modeli in seq_along(models)) {

    modes <- list.dirs(paste0(postpath, "/", models[modeli]), full.names=F, recursive=F)
    
    # throw out timmean
    timmean_ind <- which(modes == "timmean")
    if (length(timmean_ind) == 1) modes <- modes[-timmean_ind]

    for (modei in seq_along(modes)) {
        
        vars <- list.dirs(paste0(postpath, "/", models[modeli], "/", modes[modei]), full.names=F, recursive=F)
        for (vari in seq_along(vars)) {
        
            files <- list.files(paste0(postpath, "/", models[modeli], "/", modes[modei], "/", vars[vari]), full.names=T, recursive=F)
            paths <- dirname(files)
            files <- basename(files)

            # remove already existing e.g. "historical_and_ssp126" files
            replacement_inds <- grepl(glob2rx(paste0("*", replacement, "*", models[modeli], "_", modes[modei], "_", vars[vari], "*")), files)
            if (any(replacement_inds)) {
                files <- files[-which(replacement_inds)]
                paths <- paths[-which(replacement_inds)]
            }
            if (length(files) > 0) {

                # check if any *<historical_grep>*
                hist_inds <- grepl(glob2rx(paste0("*", historical_grep, "*", models[modeli], "_", modes[modei], "_", vars[vari], "*")), files)
                if (any(hist_inds)) {
                    hist_files <- files[hist_inds]
                    hist_paths <- paths[hist_inds]

                    # check if any *<ssp_grep>*
                    ssp_inds <- grepl(glob2rx(paste0("*", ssp_grep, "*", models[modeli], "_", modes[modei], "_", vars[vari], "*")), files)
                    if (any(ssp_inds)) {
                        ssp_files <- files[ssp_inds]
                        ssp_paths <- files[ssp_inds]

                        # remove experiment and date from historical; e.g. ACCESS-CM2_historical_r1i1p1f1_ACCESS-CM2_fldmean_mlotst_g19_EQU_Jan-Dec_1982-2014.nc
                        hist_files_blank <- sub(historical_grep, "_", hist_files) # e.g. ACCESS-CM2_r1i1p1f1_ACCESS-CM2_fldmean_mlotst_g19_EQU_Jan-Dec_1982-2014.nc
                        underscore_inds <- gregexpr("_", hist_files_blank)
                        underscore_inds <- sapply(underscore_inds, max) # position of last "_"
                        hist_from_to <- substr(hist_files_blank, underscore_inds+1, nchar(hist_files_blank)) # remove last part until last "_" from end of filename; e.g. 1982-2014.nc
                        hist_from_to <- tools::file_path_sans_ext(hist_from_to) # 1982-2014
                        hist_from_to <- strsplit(hist_from_to, "-")
                        hist_from_to <- lapply(hist_from_to, as.integer) # will stop if no success
                        for (fi in seq_along(hist_from_to)) {
                            if (length(hist_from_to[[fi]]) == 1) hist_from_to[[fi]][2] <- hist_from_to[[fi]][1] # only one year
                        }
                        hist_files_blank <- substr(hist_files_blank, 1, underscore_inds) # e.g. r1i1p1f1_ACCESS-CM2_fldmean_mlotst_g19_EQU_Jan-Dec_

                        # remove experiment and date from ssp; e.g. ACCESS-CM2_ssp126_r1i1p1f1_ACCESS-CM2_fldmean_mlotst_g19_EQU_Jan-Dec_1982-2014.nc
                        ssp_files_blank <- sub(ssp_grep, "_", ssp_files) # e.g. ACCESS-CM2_r1i1p1f1_ACCESS-CM2_fldmean_mlotst_g19_EQU_Jan-Dec_1982-2014.nc
                        underscore_inds <- gregexpr("_", ssp_files_blank)
                        underscore_inds <- sapply(underscore_inds, max) # position of last "_"
                        ssp_from_to <- substr(ssp_files_blank, underscore_inds+1, nchar(ssp_files_blank)) # remove last part until last "_" from end of filename; e.g. 2015-2019.nc
                        ssp_from_to <- tools::file_path_sans_ext(ssp_from_to) # 2015-2019
                        ssp_from_to <- strsplit(ssp_from_to, "-")
                        ssp_from_to <- lapply(ssp_from_to, as.integer) # will stop if no success
                        for (fi in seq_along(ssp_from_to)) {
                            if (length(ssp_from_to[[fi]]) == 1) ssp_from_to[[fi]][2] <- ssp_from_to[[fi]][1] # only one year
                        }
                        ssp_files_blank <- substr(ssp_files_blank, 1, underscore_inds) # e.g. r1i1p1f1_ACCESS-CM2_fldmean_mlotst_g19_EQU_Jan-Dec_ 

                        # check for every historical file if there is an equal ssp counterpart and 
                        # check if there is no gap between the last hist year and the first ssp year of the identical files 
                        for (fi in seq_along(hist_files)) {
                            for (fj in seq_along(ssp_files)) {
                                if (hist_files_blank[fi] == ssp_files_blank[fj] &&
                                    diff(c(hist_from_to[[fi]][2], ssp_from_to[[fj]][1])) == 1) {
                                    fout <- sub(historical_grep, replacement, hist_files[fi])
                                    fout <- sub(paste0("_", hist_from_to[[fi]][1], "-", hist_from_to[[fi]][2]),
                                                paste0("_", hist_from_to[[fi]][1], "-", ssp_from_to[[fj]][2]),
                                                fout)
                                    fout <- paste0(postpath, "/", models[modeli], "/", modes[modei], "/", vars[vari], "/", fout)
                                    if (file.exists(fout) && !force) { 
                                        #message("output file ", fout, " already exists and `force`=F. skip. rerun with `force`=T if you want to overwrite this file")
                                    } else {
                                        message("*****************************************************************\n",
                                                "model ", modeli, "/", length(models), " ", models[modeli], ", ",
                                                "mode ", modei, "/", length(modes), " ", modes[modei], ", ", 
                                                "var ", vari, "/", length(vars), " ", vars[vari], 
                                                ": historical and ssp files ", fi, " and ", fj, "\n  ", 
                                                hist_files[fi], " from ", hist_from_to[[fi]][1], " to ", hist_from_to[[fi]][2], "\n  ", 
                                                ssp_files[fj], " from ", ssp_from_to[[fj]][1], " to ", ssp_from_to[[fj]][2], "\n",
                                                "are equal and there is no gap between last historical year ", hist_from_to[[fi]][2], 
                                                " and first ssp year ", ssp_from_to[[fj]][1], " --> merge them ...")
                                        cmd <- paste0("cdo mergetime ", 
                                                      postpath, "/", models[modeli], "/", modes[modei], "/", vars[vari], "/", hist_files[fi], " ",
                                                      postpath, "/", models[modeli], "/", modes[modei], "/", vars[vari], "/", ssp_files[fj], " ",
                                                      fout)
                                        message("run `", cmd, "` ...")
                                        check <- system(cmd)
                                        if (check != 0) stop("cmd failed")
                                        if (!file.exists(fout)) stop("fout does not exist")
                                    } # if fout already exists
                                } # if hist_files_blank[fi] == ssp_files_blank[fi] && diff(c(hist_from_to[[fi]][2], ssp_from_to[[fj]][1])) == 1
                            } # for fj ssp_files
                        } # for fi hist_files

                    } # if any(ssp_inds)
                } # if any(hist_inds)
            } # if any files left after removing already existing *<replacement>* files
        } # for vari
    } # for modei
} # for modeli

message("\nfinished\n")

