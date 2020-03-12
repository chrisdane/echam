# r

correct_mpiom_partabn <- function(partabn_file) {

    if (missing(partabn_file) || !is.character(partabn_file)) {
        stop("provide `partabn_file=\"/path/to/mpiom_partabn\"`")
    }
    
    partabn <- readLines(partabn_file)

    # "  NAME=c1_PSIGULF" --> "  out_name=c1_PSIGULF"
    # -> keep indents! 
    name_inds <- which(grepl("  NAME=", partabn))
    if (length(name_inds) > 0) {
        for (i in seq_along(name_inds)) {
            laste_equal_sign_character_ind <- regexpr("\\=[^\\=]*$", partabn[name_inds[i]])
            given_name <- substr(partabn[name_inds[i]], 
                                 laste_equal_sign_character_ind+1, 
                                 nchar(partabn[name_inds[i]]))
            message(i, "/", length(name_inds), ": \"", partabn[name_inds[i]], "\" --> ", appendLF=F)
            partabn[name_inds[i]] <- paste0("  out_name=", given_name) 
            message("\"", partabn[name_inds[i]], "\"")
        }
    }

    # "  CODE=1" --> "  name=var12"
    # -> keep indents! 
    code_inds <- which(grepl("  CODE=", partabn))
    if (length(code_inds) > 0) {
        for (i in seq_along(code_inds)) {
            laste_equal_sign_character_ind <- regexpr("\\=[^\\=]*$", partabn[code_inds[i]])
            given_code_number <- substr(partabn[code_inds[i]], 
                                        laste_equal_sign_character_ind+1, 
                                        nchar(partabn[code_inds[i]]))
            message(i, "/", length(code_inds), ": \"", partabn[code_inds[i]], "\" --> ", appendLF=F)
            partabn[code_inds[i]] <- paste0("  name=var", given_code_number) 
            message("\"", partabn[code_inds[i]], "\"")
        }
    }

    fout <- paste0(dirname(partabn_file), "/", 
                   tools::file_path_sans_ext(basename(partabn_file)),
                   "_corrected.txt")
    message("\nsave \"", fout, "\" ...")
    write(partabn, file=fout)

} # correct_mpiom_partabn


mpiom_extract_fort_tar_data <- function(fort_tar_files, outpath, 
                                        keep_ext=T, partabn_ext,
                                        keep_fort.75=T, keep_fort.90=T, 
                                        verbose=T) {

    if (missing(fort_tar_files) || !is.character(fort_tar_files)) {
        stop("provide `fort_tar_files=\"/path/to/fort_YYYY0101_YYYY1231.tar\"` or\n",
             "`fort_tar_files=c(\"/path/to/fort_YYYY0101_YYYY1231.tar\", \"/path/to/fort_ZZZZ0101_ZZZZ1231.tar\")`")
    }

    if (missing(outpath)) {
        outpath <- dirname(fort_tar_files[1])
        if (verbose) {
            message("mpiom_extract_timeser_data(): `outpath` not given, use dirname(fort_tar_files[1]=\"", 
                    fort_tar_files[fi], "\") = \"", outpath, "\"")
        }
    } else {
        if (length(outpath) != 1) {
            stop("`outpath` needs to be of length 1 and not ", length(outpath))
        }
    }

    for (fi in seq_along(fort_tar_files)) {

        if (verbose) message("file ", fi, "/", length(fort_tar_files), ": ", fort_tar_files[fi])

        # get files from tar file
        cmd <- paste0("tar -tf ", fort_tar_files[fi])
        if (verbose) message("   run `", cmd, "` ...")
        tarfiles <- system(cmd, intern=T)
        if (verbose) message("   tarfiles = \"", paste(tarfiles, collapse="\", \""), "\"")

        # get .ext file
        if (keep_ext) {
            if (verbose) message("   `keep_ext`=T")
            if (any(grepl("TIMESER", tarfiles) & grepl(".ext", tarfiles))) {
                timeser_ext_file <- tarfiles[which(grepl("TIMESER", tarfiles) & grepl(".ext", tarfiles))]
                if (length(timeser_ext_file) != 1) {
                    stop("found ", length(timeser_ext_file), " \"TIMESER*.ext\" files:\n",
                         paste(timeser_ext_file, collapse=", "))
                }
                cmd <- paste0("tar -xf ", fort_tar_files[fi], " -C ", outpath, " ", timeser_ext_file)
                if (verbose) message("      run `", cmd, "` ...")
                system(cmd)

                # convert .ext to nc
                cmd <- paste0("cdo -f nc copy ", outpath, "/", timeser_ext_file, " ", outpath, "/", timeser_ext_file, ".nc")
                if (verbose) message("      run `", cmd, "` ...")
                system(cmd)

                # apply partable
                if (fi == 1) { 
                    if (missing(partabn_ext)) { # check if partable exists
                        stop("provide partable for mpiom time series data via `partabn_ext=\"/path/to/partable\"`")
                    }
                    if (file.access(partabn_ext, mode=4)) { # mode=4: read
                        stop("partable defined by `partabn_ext` = \"", partabn_ext, "\" not readable")
                    }
                }
                cmd <- paste0("cdo -setpartabn,", partabn_ext, " ", 
                              outpath, "/", timeser_ext_file, ".nc ", outpath, "/tmp && mv ", 
                              outpath, "/tmp ", outpath, "/", timeser_ext_file, ".nc")
                if (verbose) message("      run `", cmd, "` ...")
                system(cmd)
                
                # remove original file
                file.remove(paste0(outpath, "/", timeser_ext_file))

            } else {
                message("   `keep_ext`=T but there is no file called with \"TIMESER\" and \".ext\" patterns in archive \"", fort_tar_files[fi], "\"")
            } # if "TIMESER*.ext" file in tar file
        } # if keep_ext
        
        # files fort.75 (var 100, 101), fort.90 (var 69), fort.100, fort.101, fort.102 
        # would be overwritten by output of next tar file
        if (keep_fort.75) {
            if (verbose) message("   `keep_fort.75`=T")
            if (any(tarfiles == "fort.75")) {
                cmd <- paste0("tar -xf ", fort_tar_files[fi], " -C ", outpath, " fort.75")
                if (verbose) message("      run `", cmd, "` ...")
                system(cmd)

                # convert to nc
                cmd <- paste0("cdo -f nc copy ", outpath, "/fort.75 ",
                              outpath, "/fort.75_", tools::file_path_sans_ext(basename(fort_tar_files[fi])), ".nc")
                if (verbose) message("      run `", cmd, "` ...")
                system(cmd)

                # remove original file
                file.remove(paste0(outpath, "/fort.75"))

            } else {
                message("   `keep_fort.75`=T but there is no file called \"fort.75\" in archive \"", fort_tar_files[fi], "\"")
            }
        } # if keep_fort.75

        if (keep_fort.90) {
            if (verbose) message("   `keep_fort.90`=T")
            if (any(tarfiles == "fort.90")) {
                cmd <- paste0("tar -xf ", fort_tar_files[fi], " -C ", outpath, " fort.90")
                if (verbose) message("      run `", cmd, "` ...")
                system(cmd)

                # convert to nc
                cmd <- paste0("cdo -f nc copy ", outpath, "/fort.90 ",
                              outpath, "/fort.90_", tools::file_path_sans_ext(basename(fort_tar_files[fi])), ".nc")
                if (verbose) message("      run `", cmd, "` ...")
                system(cmd)

                # remove original file
                file.remove(paste0(outpath, "/fort.90"))

            } else {
                message("   `keep_fort.90`=T but there is no file called \"fort.90\" in archive \"", fort_tar_files[fi], "\"")
            }
        } # if keep_fort.90

    } # for fi fort_tar_files

} # mpiom_extract_fort_tar_data


mpiom_calc_moc <- function(varname="amoc", mpiom_model_res="GR30L40", mpiom_reg_res="r360x180") {

    if (missing(varname) ||
        !any(varname == c("gmoc", "amoc"))) {
        stop("mpiom_calc_moc(): choose \"gmoc\" (global MOC; code 100 from fort.75) or ",
             "\"amoc\" (Atlantic MOC; code 101 from fort.75)")
    }


} # mpiom_calc_moc function


