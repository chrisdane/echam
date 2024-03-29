# r

mpiom1_remap2lonlat <- function(files, cdo_select=NULL, 
                                remap_method="remapnn",
                                reg_res=c(nlon=360, nlat=180),
                                outpath, 
                                mpiom_grid_files, # only needed if input is non-nc
                                fout_rename_pattern=NULL,
                                fout_rename_variable_pattern=NULL,
                                cdo=Sys.which("cdo"), 
                                convert2nc=T, verbose=T) {
    
    # todo: cdo_get_filetype() dependence from `functions` repo
    source("~/scripts/r/functions/myfunctions.r")

    # check host
    host <- get_host()
    if (host$machine_tag == "mistral") {
        hostname <- Sys.info()["nodename"] # = system("hostname", intern=T)
        if (any(grepl("mlogin", hostname))) {
            stop("machine is \"mistral\" but node \"", hostname, "\" is not mistralpp.\n",
                 "--> change to mistralpp and rerun script.")
        }
    }

    # checks
    if (missing(files) || is.null(files) || 
        (!missing(files) && !is.character(files) && !is.list(files))) {
        stop("provide `files=\"/path/to/mpiom_YYYY.grb/.nc\"` or\n",
             "`files=c(\"/path/to/mpiom_YYYY.grb/.nc\", \"/path/to/mpiom_ZZZZ.grb/.nc\")` for scalar variables or\n",
             "`files=list(u=\"/path/to/mpiom_u_YYYY.grb/.nc\", v=\"/path/to/mpiom_v_YYYY.grb/.nc\")` or\n",
             "`files=list(uname=c(\"/path/to/mpiom_uname_YYYY.grb/.nc\", \"/path/to/mpiom_uname_ZZZZ.grb/.nc\"), ",
             "vname=c(\"/path/to/mpiom_vname_YYYY.grb/.nc\", /path/to/mpiom_vname_ZZZZ.grb/.nc\")` for vector variables")
    }
    if (is.character(files)) { # provided files is character
        message("provided `files` is not a list --> assume scalar variable")
        variable_type <- "scalar"
        files <- list(files)
    } else if (is.list(files)) { # provided files is list
        if (length(files) == 1) {
            message("provided list `files` is of length 1 --> assume scalar variable")
            variable_type <- "scalar"
        } else if (length(files) == 2) {
            message("provided list `files` is of length 2 --> assume vector variable")
            variable_type <- "vector"
            if (is.null(names(files))) {
                stop("for vector variables, the names of provided `files`-list must equal the names of the two vector variables")
            }
        } else {
            stop("provided list `files` must either be of length 1 or 2 and not ", length(files))
        }
    }
    # from here, `files` is list with unnamed entry `files[[1]]` if scalar variable is wanted 
    # or (list[[1]] = list$<uname>, list[[2]] = list$<vname>) if vector variable
        
    if (!missing(cdo_select) && !is.null(cdo_select)) { # `cdo_select` provided
        if (variable_type == "scalar" && length(cdo_select) != 1) {
            stop("variable type is scalar but provided `cdo_select` is of length ", 
                 length(cdo_select), " but should be of length 1")
        }
        if (variable_type == "vector" && length(cdo_select) != 2) {
            stop("variable type is vector but provided `cdo_select` is of length ", 
                 length(cdo_select), " but should be of length 2 (1 for u and 1 for v file)")
        }
        if (any(!is.character(cdo_select))) {
            stop("some of `cdo_select` = \"", paste(cdo_select, collapse="\", \""), "\" is not of type character")
        }
    }

    if (is.null(remap_method)) {
        #remap_method <- "remappbil" 
        remap_method <- "remapnn" # nearest neighbour better preserves land missvals compared to bilinear interp
        message("provided `remap_method` is null --> set to default \"", remap_method, "\"")
    }
    if (is.null(reg_res)) {
        message("provided `reg_res` is null --> set to default 360x180")
        reg_res <- c(nlon=360, nlat=180)
    }

    if (missing(outpath) || is.null(outpath)) {
        outpath <- dirname(files[[1]][1])
        if (verbose) {
            message("`outpath` not given or NULL, use `dirname` of first file = \"", outpath, "\"")
        }
    } else {
        if (length(outpath) != 1) {
            stop("`outpath` needs to be of length 1 and not ", length(outpath))
        }
    }
    if (file.access(outpath, mode=0) == -1) { # output dir not existing
        dir.create(outpath, recursive=T)
    } else { # output dir exists
        if (file.access(outpath, mode=2) == -1) { # no write permission
            stop("no write permission in `outpath` = \"", outpath, "\"")
        }
    }

    if (missing(fout_rename_pattern) || is.null(fout_rename_pattern)) {
        stop("provide `fout_rename_pattern` of type character")
    } else {
        if (!is.character(fout_rename_pattern)) {
            stop("provided `fout_rename_pattern` must be of type character")
        }
        file <- basename(files[[1]][1])
        if (!grepl(fout_rename_pattern, file)) {
            stop("provided `fout_rename_pattern` = \"", fout_rename_pattern, 
                 "\" does not occur in basename(files[[1]][1]) = \"", file, "\"")
        }
    }
    if (variable_type == "vector") {
        if (missing(fout_rename_variable_pattern) || is.null(fout_rename_variable_pattern)) {
            stop("variable is of type vector --> provide `fout_rename_variable_pattern` = ",
                 "c(\"in\"=\"<pattern_to_replace>\", \"out\"=\"<replacement>\")`")
        }
    }

    # do for all input files
    nfiles <- length(files[[1]])
    for (fi in seq_len(nfiles)) {
        
        if (variable_type == "scalar") {
            sfile <- files[[1]][fi]
            if (verbose) message("\nfile ", fi, "/", nfiles, ": ", sfile)
            if (!file.exists(sfile)) stop("sfile does not exist")
        } else if (variable_type == "vector") {
            ufile <- files[[1]][fi]
            vfile <- files[[2]][fi]
            if (verbose) message("\nfile ", fi, "/", nfiles, ": ", 
                                 names(files)[1], " = ", ufile, ", ", 
                                 names(files)[2], " = ", vfile)
            if (!file.exists(ufile)) stop("ufile does not exist")
            if (!file.exists(vfile)) stop("vfile does not exist")
        }

        # get format of input files
        if (variable_type == "scalar") {
            file_type <- cdo_get_filetype(sfile)$file_type
        } else if (variable_type == "vector") {
            file_type <- cdo_get_filetype(ufile)$file_type
        }

        # decide if conversion is wanted or not
        if (!convert2nc && file_type == "non-nc") { # if conversion is not wanted but needed
            convert2nc <- T
        }
        if (convert2nc && file_type == "nc") { # if conversion to nc is wanted but not needed
            convert2nc <- F 
        }

        # regridding command depends if mpiom grid is GR* or TP* (see README.md)
        # --> found no way of automatic decision
        # --> workaround: use cdo ngridsize
        if (variable_type == "scalar") {
            cmd <- paste0(cdo, " ngridpoints ", sfile)
        } else if (variable_type == "vector") {
            cmd <- paste0(cdo, " ngridpoints ", ufile)
        }
        message("run `", cmd, "` ...")
        ngridpoints <- system(cmd, intern=T)[1]
        options(warn=2) # stop on warnings
        ngridpoints <- as.integer(ngridpoints)
        options(warn=0) # back to default
        if (any(ngridpoints == c(12120, 56320))) { # GR30, GR15
            input_grid_type <- "GR"
        } else if (any(ngridpoints == c(324008))) { # TP04, ...
            input_grid_type <- "TP"
            if (ngridpoints == 324008) {
                input_res=c(nlon=802, nlat=404) # needed for regridding
            } else {
                stop("not yet")
            }
        } else {
            stop("mpiom grid with `cdo ngridpoints` = ", ngridpoints, " not implemented yet")
        }
        message("--> ngridpoints = ", ngridpoints, " --> mpiom grid type = ", input_grid_type)
        
        # check availability of mpiom grid files if input files are non-nc and/or vector variables
        # --> `mpiom_grid_files` is list with entry `mpiom_grid_files$s` if scalar variable is wanted 
        # or (mpiom_grid_files$u, mpiom_grid_files$v) if vector variable
        # `mpiom_grid_files$u` = "<file>" or `mpiom_grid_files$u` = "-sellevel,6 -selvar,amsuo <file>" are possible
        # e.g. /pool/data/MPIOM/GR15/GR15s.nc, GR15u.nc, GR15v.nc, GR15L40_fx.nc
        #      /pool/data/MPIOM/input/r0013/GR15/GR15L40_fx.nc (has variable weto)
        #      /pool/data/MPIOM/TP04/TP04s.nc, TP04u.nc, TP04v.nc, TP04L40_fx.nc
        #      /pool/data/MPIOM/input/r0013/TP04/TP04L40_fx.nc (was variable weto)
        if (file_type == "non-nc" || variable_type == "vector") {
            if (missing(mpiom_grid_files) || is.null(mpiom_grid_files)) {
                stop("provide `mpiom_grid_files`")
            }
            if (!is.list(mpiom_grid_files)) {
                stop("provided `mpiom_grid_files` must be a list")
            }
            if (is.null(names(mpiom_grid_files))) {
                stop("provided `mpiom_grid_files` has no names")
            }
            if (file_type == "non-nc") {
                if (variable_type == "scalar") {
                    if (!any(names(mpiom_grid_files) == "s")) {
                        stop("provided `mpiom_grid_files` has no \"s\"-entry pointing to /path/", input_grid_type, "??s.nc")
                    }
                } else if (variable_type == "vector") {
                    if (!any(names(mpiom_grid_files) == "u")) {
                        stop("provided `mpiom_grid_files` has no \"u\"-entry pointing to /path/", input_grid_type, "??u.nc")
                    }
                    if (!any(names(mpiom_grid_files) == "v")) {
                        stop("provided `mpiom_grid_files` has no \"v\"-entry pointing to /path/", input_grid_type, "??v.nc")
                    }
                }
            }
            if (variable_type == "vector") {
                if (!any(names(mpiom_grid_files) == "amsuo")) {
                    stop("provided `mpiom_grid_files` has no \"amsuo\"-entry pointing to /path/", input_grid_type, "??L??_fx.nc")
                }
                if (!any(names(mpiom_grid_files) == "amsue")) {
                    stop("provided `mpiom_grid_files` has no \"amsue\"-entry pointing to /path/", input_grid_type, "??L??_fx.nc")
                }
            }
        }
       
        # get output filename based on input
        if (variable_type == "scalar") {
            fout <- basename(sfile)
        } else if (variable_type == "vector") {
            fout <- basename(ufile)
        }
        if (verbose) message("fout = \"", fout, "\"")
        cdo_select_fname <- "" # default: no additional cdo selection option in result filename
        if (!is.null(cdo_select)) {
            if (variable_type == "scalar") {
                cdo_select_fname <- gsub("[[:punct:]]", "_", cdo_select) # e.g. "select,code=2" --> "select_code_2"
            } else if (variable_type == "vector") {
                cdo_select_fname <- gsub("[[:punct:]]", "_", cdo_select[1])
                if (cdo_select[1] != cdo_select[2]) {
                    cdo_select_fname <- paste0("u_", cdo_select_fname, "_",
                                               "v_", gsub("[[:punct:]]", "_", cdo_select[2]))
                }
            }
            cdo_select_fname <- paste0("_", cdo_select_fname)
        } # cdo_select or not
        replacement <- paste0(fout_rename_pattern, cdo_select_fname, "_", remap_method, "_r", reg_res["nlon"], "x", reg_res["nlat"])
        fout <- sub(fout_rename_pattern, replacement, fout)
        if (verbose) message("replace `fout_rename_pattern` = \"", fout_rename_pattern, 
                             "\" with \"", replacement, "\"\n",
                             "--> `fout` = \"", fout, "\" ...")
        if (variable_type == "vector") {
            fout <- sub(fout_rename_variable_pattern["in"], fout_rename_variable_pattern["out"], fout)
            if (verbose) message("replace `fout_rename_variable_pattern[\"in\"]` = \"", 
                                 fout_rename_variable_pattern["in"], 
                                 "\" with `fout_rename_variable_pattern[\"out\"]` = \"", 
                                 fout_rename_variable_pattern["out"], "\"\n",
                                 "--> `fout` = \"", fout, "\" ...")
        }
        fout <- paste0(outpath, "/", fout) # add path to fout
        
        # construct regridding command 
        cmd <- ""
        
        # add nc conversion 
        if (convert2nc) { # input is grb
            cmd <- paste0(cmd, " -t mpiom1 -f nc copy")
            if (tools::file_ext(fout) != "nc") fout <- paste0(fout, ".nc")
        }

        # prepare input file strings for cdo command
        # - default: simply the filenames
        # - if `cdo_select` is provided: filenames with cdo selection options as prefix
        if (variable_type == "scalar") {
            sin <- sfile
            if (!is.null(cdo_select)) sin <- paste0(cdo_select, " ", sin)
        } else if (variable_type == "vector") {
            uin <- ufile
            vin <- vfile
            if (!is.null(cdo_select)) {
                uin <- paste0(cdo_select[1], " ", uin)
                vin <- paste0(cdo_select[2], " ", vin)
            }
        }

        # prepare remapping command (see README.md)
        if (input_grid_type == "GR") {
            if (file_type == "non-nc") {
                if (variable_type == "scalar") {
                    cmd <- paste0(cmd,
                                  " -setgrid,", mpiom_grid_files$s, 
                                  " -sethalo,-1,-1 ", sin)
                } else if (variable_type == "vector") {
                    #cdo -remapbil,r360x180 -mrotuvb -setgrid,grid_u -sethalo,-1,-1 uin -setgrid,grid_v -sethalo,-1,-1 vin uvout
                    stop("not yet")
                } 
            } else if (file_type == "nc") {
                if (variable_type == "scalar") {
                    cmd <- paste0(cmd,
                                  " -sethalo,-1,-1 ", sin)
                } else if (variable_type == "vector") {
                    cmd <- paste0(cmd,
                                  " -sethalo,-1,-1 -mrotuvb ", uin, " ", vin)
                }
            }
        } else if (input_grid_type == "TP") {
            if (file_type == "non-nc") {
                if (variable_type == "scalar") {
                    #cdo -remapbil,r360x180 -selindexbox,1,800,3,404 -setgrid,grid_s -sethalo,-1,-1 sin sout
                    stop("not yet")
                } else if (variable_type == "vector") {
                    #cdo -remapbil,r360x180 -selindexbox,1,800,3,404 -mrotuvb -setgrid,grid_u -sethalo,-1,-1 uin -setgrid,grid_v -sethalo,-1,-1 vin uvout
                    stop("not yet")
                } 
            } else if (file_type == "nc") {
                if (variable_type == "scalar") {
                    cmd <- paste0(cmd,
                                  " -selindexbox,2,", input_res["nlon"]-1, ",3,", input_res["nlat"], " ", sin)
                } else if (variable_type == "vector") {
                    cmd <- paste0(cmd,
                                  " -selindexbox,2,", input_res["nlon"]-1, ",3,", input_res["nlat"],
                                  " -mrotuvb ", uin, " ", vin)
                }
            }
        } # which input_grid_type GR or TP

        # add remap command directly if possible
        if (variable_type == "scalar") {
            cmd <- paste0(cdo, " -P 8 -", remap_method, ",r", reg_res["nlon"], "x", reg_res["nlat"], " ", cmd)
        } else if (variable_type == "vector") { # need to treat missvals before remapping; see below
            cmd <- paste0(cdo, " ", cmd)
        }

        # add fout
        cmd <- paste0(cmd, " ", fout)
        
        # run command
        # e.g. `cdo -P 4 -t mpiom1 -f nc copy -remapcon2,r360x180 -setgrid,../GR30.nc -select,code=183 Hol-Tx10_mpiom_32900101_32901231.grb zmld_curvilinear_setgrid_remapcon2_r360x180.nc`
        if (verbose) message("run `", cmd, "` ...")
        system(cmd)
        
        # restore missval for vector variables after rotation and before remapping
        # amsuo         Sea binary mask at u vector point
        # amsue         Sea binary mask at v vector point
        # weto          Sea binary mask at pressure point   
        # e.g. cdo ifthen -sethalo,-1,-1 -sellevel,6 -selvar,amsuo /pool/data/MPIOM/input/r0013/GR15/GR15L40_fx.nc -selvar,uo rot.nc u_miss.nc
        if (variable_type == "vector") {
            message("\nrestore missvals of vector variables ...")
            if (input_grid_type == "GR") {
                if (file_type == "non-nc") {
                    stop("not yet")
                } else if (file_type == "nc") {
                    cmd <- paste0(cdo, " ifthen -sethalo,-1,-1 ", mpiom_grid_files$amsuo, " -selvar,", names(files)[1], " ", 
                                  fout, " ", 
                                  dirname(fout), "/tmp_", names(files)[1], "_miss_", basename(fout))
                    if (verbose) message("run `", cmd, "` ...")
                    system(cmd)
                    cmd <- paste0(cdo, " ifthen -sethalo,-1,-1 ", mpiom_grid_files$amsue, " -selvar,", names(files)[2], " ", 
                                  fout, " ", 
                                  dirname(fout), "/tmp_", names(files)[2], "_miss_", basename(fout))
                    if (verbose) message("run `", cmd, "` ...")
                    system(cmd)
                }
            } else if (input_grid_type == "TP") {
                if (file_type == "non-nc") {
                    stop("not yet")
                } else if (file_type == "nc") {
                    cmd <- paste0(cdo, " ifthen", 
                                  " -selindexbox,2,", input_res["nlon"]-1, ",3,", input_res["nlat"], " ",
                                  mpiom_grid_files$amsuo, " -selvar,", names(files)[1], " ", 
                                  fout, " ", 
                                  dirname(fout), "/tmp_", names(files)[1], "_miss_", basename(fout))
                    if (verbose) message("run `", cmd, "` ...")
                    system(cmd)
                    cmd <- paste0(cdo, " ifthen", 
                                  " -selindexbox,2,", input_res["nlon"]-1, ",3,", input_res["nlat"], " ",
                                  mpiom_grid_files$amsue, " -selvar,", names(files)[2], " ", 
                                  fout, " ", 
                                  dirname(fout), "/tmp_", names(files)[2], "_miss_", basename(fout))
                    if (verbose) message("run `", cmd, "` ...")
                    system(cmd)
                }
            }

            # merge u- and v-components with restored missvals and remap
            cmd <- paste0(cdo , 
                          " -P 8 -", remap_method, ",r", reg_res["nlon"], "x", reg_res["nlat"], 
                          " -merge ", 
                          dirname(fout), "/tmp_", names(files)[1], "_miss_", basename(fout), " ",
                          dirname(fout), "/tmp_", names(files)[2], "_miss_", basename(fout), " ",
                          fout)
            if (verbose) message("\nrun remapping `", cmd, "` ...")
            system(cmd)

            # remove tmp files
            file.remove(paste0(dirname(fout), "/tmp_", names(files), "_miss_", basename(fout)))
            
        } # if vector

        # fix negative values due to interpolation
        if (!is.null(cdo_select)) {
            if (any(cdo_select == c("select,code=183", "select,code=15"))) {
                message("special: set negative values to zero")
                if (cdo_select == "select,code=183") {
                    cmd <- paste0("ncap2 -O -s \"where(zmld<0) zmld=0\" ", fout, " ", fout)
                } else if (cdo_select == "select,code=15") {
                    cmd <- paste0("ncap2 -O -s \"where(SICOMO<0) SICOMO=0\" ", fout, " ", fout)
                }
                if (verbose) message("run `", cmd, "` ...")
                system(cmd)
            }
        }

    } # for fi files

} # mpiom1_remap2lonlat


mpiom_ext_to_nc <- function(ext_files, partab_ext, outpath, 
                            cdo=Sys.which("cdo"), verbose=T) {

    if (missing(ext_files) || !is.character(ext_files)) {
        stop("provide `ext_files=\"/path/to/TIMESER.YYYY0101_YYYY1231.ext\"` or\n",
             "`ext_files=c(\"/path/to/TIMESER.YYYY0101_YYYY1231.ext\", \"/path/to/TIMESER.ZZZZ0101_ZZZZ1231.ext\")`")
    }
    
    if (missing(partab_ext)) { # check if partable exists
        stop("provide partable for mpiom time series data via `partab_ext=\"/path/to/partable\"`")
    }
    if (file.access(partab_ext, mode=4)) { # mode=4: read
        stop("partable defined by `partab_ext` = \"", partab_ext, "\" not readable")
    }
    partab_ext <- normalizePath(partab_ext)
    
    if (missing(outpath)) {
        outpath <- dirname(ext_files[1])
        if (verbose) {
            message("`outpath` not given, use dirname(ext_files[1]=\"", 
                    ext_files[1], "\") = \"", outpath, "\"")
        }
    } else {
        if (length(outpath) != 1) {
            stop("`outpath` needs to be of length 1 and not ", length(outpath))
        }
    }
    if (file.access(outpath, mode=0)) { # existance?
        dir.create(outpath, recursive=T)
    } else {
        if (file.access(outpath, mode=2)) { # write permission?
            stop("no write permission in `outpath` = \"", outpath, "\"")
        }
    }

    for (fi in seq_along(ext_files)) {
        
        if (verbose) message("\nfile ", fi, "/", length(ext_files), ": ", ext_files[fi])
        
        fout <- paste0(outpath, "/", basename(ext_files[fi]), ".nc")
        
        # apply partable and convert .ext to nc
        cmd <- paste0(cdo, " -f nc -setpartab,", partab_ext, " ", ext_files[fi], " ", fout)
        if (verbose) message("   run `", cmd, "` ...")
        system(cmd)
            
    } # for fi ext_files

} # mpiom_ext_to_nc

mpiom_extract_fort_tar_data <- function(fort_tar_files, outpath, 
                                        keep_ext=T, partab_ext,
                                        keep_fort.75=T, keep_fort.90=T, 
                                        cdo="cdo", verbose=T) {

    if (missing(fort_tar_files) || !is.character(fort_tar_files)) {
        stop("provide `fort_tar_files=\"/path/to/fort_YYYY0101_YYYY1231.tar\"` or\n",
             "`fort_tar_files=c(\"/path/to/fort_YYYY0101_YYYY1231.tar\", \"/path/to/fort_ZZZZ0101_ZZZZ1231.tar\")`")
    }

    if (missing(outpath)) {
        outpath <- dirname(fort_tar_files[1])
        if (verbose) {
            message("`outpath` not given, use dirname(fort_tar_files[1]=\"", 
                    fort_tar_files[fi], "\") = \"", outpath, "\"")
        }
    } else {
        if (length(outpath) != 1) {
            stop("`outpath` needs to be of length 1 and not ", length(outpath))
        }
    }
    if (file.access(outpath, mode=0)) { # existance?
        dir.create(outpath, recursive=T)
    } else {
        if (file.access(outpath, mode=2)) { # write permission?
            stop("no write permission in `outpath` = \"", outpath, "\"")
        }
    }

    for (fi in seq_along(fort_tar_files)) {

        if (verbose) message("\nfile ", fi, "/", length(fort_tar_files), ": ", fort_tar_files[fi])

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

                # apply partable and convert .ext to nc
                if (fi == 1) { 
                    if (missing(partab_ext)) { # check if partable exists
                        stop("provide partable for mpiom time series data via `partab_ext=\"/path/to/partable\"`")
                    }
                    if (file.access(partab_ext, mode=4)) { # mode=4: read
                        stop("partable defined by `partab_ext` = \"", partab_ext, "\" not readable")
                    }
                    partab_ext <- normalizePath(partab_ext)
                }
                cmd <- paste0(cdo, " -f nc -setpartab,", partab_ext, " ", 
                              outpath, "/", timeser_ext_file, " ", 
                              outpath, "/", timeser_ext_file, ".nc")
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
                cmd <- paste0(cdo, " -f nc copy ", outpath, "/fort.75 ",
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
                cmd <- paste0(cdo, " -f nc copy ", outpath, "/fort.90 ",
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


mpiom_fort.75_temporal_mean <- function(fort.75_files, outpath, 
                                        cdo_temporal_mean="monmean", 
                                        cdo="cdo", verbose=T) {

    if (missing(fort.75_files) || !is.character(fort.75_files)) {
        stop("provide `fort.75_files=\"/path/to/fort.75\"` or\n",
             "`fort_tar_files=c(\"/path/to/fort.75_fort_YYYY0101_YYYY1231.nc\", \"/path/to/fort.75_fort_ZZZZ0101_ZZZZ1231.nc\")`")
    }
    
    if (missing(outpath)) {
        outpath <- dirname(fort.75_files[1])
        if (verbose) {
            message("`outpath` not given, use dirname(fort.75_files[1]=\"", 
                    fort.75_files[1], "\") = \"", outpath, "\"")
        }
    } else {
        if (length(outpath) != 1) {
            stop("`outpath` needs to be of length 1 and not ", length(outpath))
        }
    }
    if (file.access(outpath, mode=0)) { # existance?
        dir.create(outpath, recursive=T)
    } else {
        if (file.access(outpath, mode=2)) { # write permission?
            stop("no write permission in `outpath` = \"", outpath, "\"")
        }
    }

    for (fi in seq_along(fort.75_files)) {
    
        if (verbose) message("\n", fi, "/", length(fort.75_files), ": ", fort.75_files[fi])
        
        # fort.75_fort_00040101_00041231.nc
        fout <- paste0(outpath, "/", 
                       gsub(".nc", paste0("_", cdo_temporal_mean, ".nc"), basename(fort.75_files[fi])))
        cmd <- paste0(cdo, " -", cdo_temporal_mean, " ", fort.75_files[fi], " ", fout)
        if (verbose) message("run `", cmd, "` ...")
        stop("check: why dout does not have cdo_temporal_mean in it?")
        system(cmd)

    } # for fi fort.75_files

} # mpiom_fort.75_temporal_mean


mpiom_moc_make_bottom_topo <- function(varname="amoc", fin, fout, 
                                       mpiom_model_res=c(setup="GR30", nlev="L40"), 
                                       reg_res=c(nlon=360, nlat=180), 
                                       cdo="cdo", verbose=T) {
   
    # adapted from https://gitlab.awi.de/paleodyn/model-analysis/blob/master/previous_scripts/ANALYSIS_make_amoc.sh

    if (!any(varname == c("gmoc", "amoc"))) {
        stop("choose \"gmoc\" (global MOC; code 100 from fort.75) or ",
             "\"amoc\" (Atlantic MOC; code 101 from fort.75) for `varname`")
    }
    
    if (missing(fin)) {
        stop("provide `fin=\"/path/to/mpiom/[ag]moc/result/of/post_echam.r\"`")
    }

    if (missing(fout)) stop("provide `fout=\"/path/to/[ag]moc/result/of/post_echam.r/with/bottom/topo\"`")
    outpath <- dirname(fout)
    if (file.access(outpath, mode=0)) { # existance?
        dir.create(outpath, recursive=T)
    } else {
        if (file.access(outpath, mode=2)) { # write permission?
            stop("no write permission in `outpath` = dirname(`fout`) = \"", outpath, "\"")
        }
    }

    if (class(mpiom_model_res) != "character" ||
        length(mpiom_model_res) != 2 ||
        names(mpiom_model_res) != c("setup", "nlev")) {
        stop("provide `mpiom_model_res` as e.g. `mpiom_model_res=c(setup=\"GR30\", nlev=\"L40\")`")
    }
    
    if (class(reg_res) != "numeric" ||
        length(reg_res) != 2 ||
        names(reg_res) != c("nlon", "nlat")) {
        stop("provide `reg_res` as e.g. `reg_res=c(nlon=360, nlat=180)`")
    }

    # get format of input files
    cmd <- paste0(cdo, " showformat ", fin)
    if (verbose) message("\nrun `", cmd, "`")
    input_format <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))
    if (!is.null(input_format$warning)) {
        stop(input_format$warning)
    } else {
        if (verbose) message(" --> \"", input_format$value, "\"")
    }
    if (any(input_format$value == c("GRIB", "EXTRA  BIGENDIAN"))) {
        if (verbose) message("convert input files of type \"", input_format$value, "\" to netcdf ...")
        convert_to_nc <- T
    } else if (input_format$value == "netCDF") {
        convert_to_nc <- F
    } else {
        stop("input file type \"", input_format$value, "\" not defined for this function")
    }

    # regular interpolation and optional conversion to nc
    cmd <- cdo
    if (convert_to_nc) cmd <- paste0(cmd, " -f nc")
    cmd <- paste0(cmd, " setgrid,r1x", reg_res["nlat"], " ", fin, " ", 
                  outpath, "/tmp && mv ", outpath, "/tmp ", fout) # fin and fout are possibly the same
    if (verbose) message("run `", cmd, "`")
    system(cmd)

    # get bottom topography based on available model resolution files
    tracer_file <- paste0(pwd, "/mpiom/mpiom_r", reg_res["nlon"], "x", reg_res["nlat"], 
                          mpiom_model_res["nlev"], "_geographic_grid.nc") 
    if (!file.exists(tracer_file)) {
        stop("`tracer_file` = \"", tracer_file, "\" does not exist")
    }
    bek_file <- paste0(pwd, "/mpiom/mpiom_r", reg_res["nlon"], "x", reg_res["nlat"], "_bek.nc")
    if (!file.exists(bek_file)) {
        stop("`bek_file` = \"", bek_file, "\" does not exist")
    }

    # generate moc mask file, i.e. set basins out of moc area to miss val
    basin_mask_file <- paste0(pwd, "/mpiom/mpiom_r", 
                              reg_res["nlon"], "x", reg_res["nlat"], mpiom_model_res["nlev"],
                              "_", varname, "_mask.nc")
    if (!file.exists(basin_mask_file)) {
        
        if (verbose) message("\n`basin_mask_file` = \"", basin_mask_file, "\" does not exist --> create ...")
        
        #${cdo} chname,SAO,bek -ifthenelse -setctomiss,0 -setrtomiss,6,12 ${bek_file} -setrtoc,-1000,1000,1 $tracer_file -setrtomiss,-1000,1000 $tracer_file basin_mask.nc
        cmd <- paste0(cdo, " chname,SAO,bek -ifthenelse -setctomiss,0")
        if (varname == "amoc") {
            cmd <- paste0(cmd, " -setrtomiss,6,12") 
        } else if (varname == "gmoc") {
            # global moc: dont set any basin to missval
        }
        cmd <- paste0(cmd, " ", bek_file, " -setrtoc,-1000,1000,1 ", tracer_file, " -setrtomiss,-1000,1000 ",
                      tracer_file, " ", basin_mask_file)
        if (verbose) message("run `", cmd, "`")
        system(cmd)
        
        #${cdo} div basin_mask.nc basin_mask.nc tmp.nc
        #mv tmp.nc basin_mask.nc
        cmd <- paste0(cdo, " div ", basin_mask_file, " ", basin_mask_file, " ", 
                      pwd, "/mpiom/tmp && mv ", pwd, "/mpiom/tmp ", basin_mask_file)
        if (verbose) message("run `", cmd, "`")
        system(cmd)

    } else {
        message("\nfound `basin_mask_file` = \"", basin_mask_file, "\"")
    } # if !file.exists(basin_mask_file)

    # calculate zonal average sea floor elevation
    basin_zonmean_mask_file <- paste0(pwd, "/mpiom/mpiom_r",
                                      reg_res["nlon"], "x", reg_res["nlat"], mpiom_model_res["nlev"],
                                      "_", varname, "_mask_zonmean.nc") 
    if (!file.exists(basin_zonmean_mask_file)) {
        
        if (verbose) message("\n`basin_zonmean_mask_file` = \"", basin_zonmean_mask_file, "\" does not exist --> create ...")
        
        #${cdo} setmisstoc,0 -setctomiss,-100 -setrtoc,0,100,1 -setmisstoc,-100 -zonmean -mul basin_mask.nc $tracer_file basin_zonal_average.nc
        cmd <- paste0(cdo, " setmisstoc,0 -setctomiss,-100 -setrtoc,0,100,1 -setmisstoc,-100 -zonmean -mul ", basin_mask_file, 
                      " ", tracer_file, " ", basin_zonmean_mask_file)
        if (verbose) message("run `", cmd, "`")
        system(cmd)

        #${cdo} splitlevel basin_zonal_average.nc split
        already_existing_split_files <- list.files(paste0(pwd, "/mpiom"), pattern=glob2rx("tmp_split*"), full.names=T)
        if (length(already_existing_split_files) > 0) {
            stop("there are already temporary \"split*\" files:\n",
                 paste(already_existing_split_files, collapse=","), "\n",
                 "remove them and rerun")
        }
        cmd <- paste0(cdo, " splitlevel ", basin_zonmean_mask_file, " ", pwd, "/mpiom/tmp_split")
        if (verbose) message("run `", cmd, "`")
        system(cmd)

        split_files <- list.files(paste0(pwd, "/mpiom"), pattern=glob2rx("tmp_split*"), full.names=T)
        depth_files <- paste0(dirname(split_files), "/tmp_depth_", basename(split_files))
        for (fi in seq_along(split_files)) {
            if (verbose) message("\n", fi, "/", length(split_files), ": ", split_files[fi])
            #z=$(${cdo} showlevel $i | sed -e 's/^[ \t]*//')
            cmd <- paste0(cdo, " showlevel ", split_files[fi])
            if (verbose) message("run `", cmd, "`")
            cmd <- system(cmd, intern=T)
            z <- gsub(" ", "", cmd)
            #${cdo} setrtoc,-1000,1000,1 $i mask.nc
            cmd <- paste0(cdo, " setrtoc,-1000,1000,1 ", split_files[fi], " ", pwd, "/mpiom/tmp_mask.nc")
            if (verbose) message("run `", cmd, "`")
            system(cmd)
            #${cdo} mulc,$z mask.nc depths$i.nc
            cmd <- paste0(cdo, " mulc,", z, " ", pwd, "/mpiom/tmp_mask.nc ", depth_files[fi])
            if (verbose) message("run `", cmd, "`")
            system(cmd)
            file.remove(c(split_files[fi], paste0(pwd, "/mpiom/tmp_mask.nc")))
        } # for fi split_files
        
        basin_zonmean_max_depth_file <- paste0(pwd, "/mpiom/mpiom_r",
                                               reg_res["nlon"], "x", reg_res["nlat"], mpiom_model_res["nlev"],
                                               "_", varname, "_zonal_max_depths.nc")
        #${cdo} ensmax depths*.nc max_depth.nc
        cmd <- paste0(cdo, " ensmax ", paste(depth_files, collapse=" "), " ", basin_zonmean_max_depth_file)
        if (verbose) message("\nrun `", cmd, "`")
        system(cmd)
        file.remove(depth_files)

        basin_zonmean_mean_depth_file <- paste0(pwd, "/mpiom/mpiom_r", 
                                                reg_res["nlon"], "x", reg_res["nlat"], mpiom_model_res["nlev"],
                                                "_", varname, "_zonal_mean_depths.nc")
        #${cdo} zonmean max_depth.nc zon_mean_depth.nc
        cmd <- paste0(cdo, " zonmean ", basin_zonmean_max_depth_file, " ", basin_zonmean_mean_depth_file)
        if (verbose) message("\nrun `", cmd, "`")
        system(cmd)
    
    } else {
        message("\nfound `basin_zonmean_mask_file` = \"", basin_zonmean_mask_file, "\"")
    } # if !file.exists(basin_zonmean_mask_file)

    #we need to cut the lowest level in the overturning data set (since the salinity data set has one level less)
    #get the list of levels in the overturning dataset
    #list=$(${cdo} showlevel -selvar,var101 $streamfun_infile)
    cmd <- paste0(cdo, " showlevel ", fout)
    if (verbose) message("\nrun `", cmd, "`")
    levels <- system(cmd, intern=T)

    #store levels in an array
    #counter=0; for i in $list; do arr[$counter]=$i; counter=$counter+1; done
    levels <- strsplit(levels, " ")[[1]]
    if (any(levels == "")) {
        levels <- levels[-which(levels == "")]
    }

    #generate level list that can be used to select all levels except the lowest one
    #level_str=""
    #for ((i=0; i<40; i++));
    #do
	#level_str=$level_str${arr[$i]}
	#if [ $i -lt 39 ]; then
	#    level_str=$level_str","
	#fi
    #done
    levels <- levels[-length(levels)]
    write(cbind(seq_along(levels), levels), ncolumns=2, 
          file=paste0(pwd, "/mpiom/mpiom_", mpiom_model_res["nlev"], "_levels.txt"))

    #extract AMOC for all levels except the lowest one from the data set
    #${cdo} sellevel,$level_str -selvar,var101 $streamfun_infile tmp.nc
    cmd <- paste0(cdo, " sellevel,", paste(levels, collapse=","), " ", fout, " ", 
                  outpath, "/tmp && mv ", outpath, "/tmp ", fout)
    if (verbose) message("\nrun `", cmd, "`")
    system(cmd)

    #set streamfunction to missing value where there is (zonal average) topography present
    #${cdo} ifthen basin_zonal_average.nc tmp.nc $streamfun_outfile
    cmd <- paste0(cdo, " ifthen ", basin_zonmean_mask_file, " ", fout, " ",
                  outpath, "/tmp && mv ", outpath, "/tmp ", fout)
    if (verbose) message("\nrun `", cmd, "`")
    system(cmd)

    #increase streamfunction by the "infinitely" small value of 0.00001 in order to avoid that ferret plots topography where the streamfunction is exactly 0
    #${cdo} addc,0.00001 $streamfun_outfile tmp.nc
    #mv tmp.nc $streamfun_outfile

    #clean up
    #rm split* mask.nc depths* basin_mask.nc basin_zonal_average.nc max_depth.nc zon_mean_depth.nc

    if (F) {
        #${cdo} -s -divc,1e6 -setgrid,r180x40 tmp ${expid}_mpiom_MOC_complete_180x40_Sv.nc
        cmd <- paste0(cdo, " -setgrid,r", reg_res["nlat"], "x", mpiom_model_res["nlev"], " ", fout, " ",
                      outpath, "/tmp && mv ", outpath, "/tmp ", fout)
        if (verbose) message("\nrun `", cmd, "`")
        system(cmd)
        # Error (gridDefine) : ysize undefined!
    }

    if (F) { # set units m3 s-1
        cmd <- paste0(cdo, " setunit,\"m3 s-1\" ", fout, " ",
                      outpath, "/tmp && mv ", outpath, "/tmp ", fout)
    } else if (T) { # set units Sv
        cmd <- paste0(cdo, " -setunit,\"Sv\" -divc,1e6 ", fout, " ",
                      outpath, "/tmp && mv ", outpath, "/tmp ", fout)
    }
    if (verbose) message("\nrun `", cmd, "`")
    system(cmd)

    # "var101" --> "amoc"
    cmd <- paste0(cdo, " setname,", varname, " ", fout, " ", 
                  outpath, "/tmp && mv ", outpath, "/tmp ", fout)
    if (verbose) message("\nrun `", cmd, "`")
    system(cmd)

} # mpiom_moc_make_bottom_topo


mpiom_moc_extract_ts <- function(fin, outpath,
                                 sellevidx=list(c(from=15, to=31)), sellevel, 
                                 sellonlatbox=list(c(lon1=0, lon2=0, lat1=45, lat2=60)),
                                 cdo="cdo", verbose=T) {
   
    # adapted from https://gitlab.awi.de/paleodyn/model-analysis/blob/master/previous_scripts/ANALYSIS_make_amoc.sh

    if (missing(fin)) {
        stop("provide `fin=\"/path/to/mpiom/[ag]moc/result/of/post_echam.r\"`")
    }

    if (missing(outpath)) {
        outpath <- dirname(fin)
    }
    if (file.access(outpath, mode=0)) { # existance?
        dir.create(outpath, recursive=T)
    } else {
        if (file.access(outpath, mode=2)) { # write permission?
            stop("no write permission in `outpath` = dirname(`fin`) = \"", outpath, "\"")
        }
    }

    if (!is.list(sellonlatbox)) stop("`sellonlatbox` must be a list")
    
    if (!missing(sellevel)) { # use depth values
        if (!is.list(sellevel)) stop("`sellevel` must be a list")
        if (length(sellevel) != length(sellonlatbox)) {
            stop("`sellevel` is given but of different length as `sellonlatbox` (", 
                 length(sellevel), " != ", length(sellonlatbox))
        }
    } else { # use depth index
        if (!is.list(sellevidx)) stop("`sellevidx` must be a list")
        if (length(sellevidx) != length(sellonlatbox)) {
            stop("`sellevidx` is of different length as `sellonlatbox` (", 
                 length(sellevidx), " != ", length(sellonlatbox))
        }
    }
   
    # get input depth levels
    cmd <- paste0(cdo, " showlevel ", fin)
    if (verbose) message("\nrun `", cmd, "`")
    levels <- system(cmd, intern=T)
    levels <- strsplit(levels, " ")[[1]]
    if (any(levels == "")) {
        levels <- levels[-which(levels == "")]
    }

    if (!missing(sellevel)) {
        sellevidx <- vector("list", l=length(sellonlatbox))
    }

    # for all wanted regions
    for (i in seq_along(sellonlatbox)) {

        if (verbose) message("\nmoc ts setting ", i, "/", length(sellonlatbox), ":")

        #${cdo} -s -vertmax -mermax -sellevidx,15/31 -sellonlatbox,0,0,45,60 ${expid}_mpiom_MOC_complete_180x40_Sv.nc ${expid}_mpiom_MOC_complete_180x40_Sv_index_45-60N.nc
        cmd <- cdo
        if (!missing(sellevel)) { # use depth values if provided
            sellevidx[[i]] <- list(from=which.min(abs(levels - sellevel[[i]]["from"])),
                                   to=which.min(abs(levels - sellevel[[i]]["to"])))
            if (sellevel[[i]]["from"] == sellevel[[i]]["to"]) { # maximum at one depth
                cmd_sellev <- paste0("intlevel,", sellevel[[i]]["from"])
                depth_fname <- paste0("-", sellevel[[i]]["from"])
            } else { # maximum in depth range
                cmd <- paste0(cmd, " -vertmax")
                cmd_sellev <- paste0("intlevel,", sellevel[[i]]["from"], "", sellevel[[i]]["to"])
                depth_fname <- paste0("-", sellevel[[i]]["from"], "to-", sellevel[[i]]["to"])
            }
        } else if (missing(sellevel)) { # use depth index
            if (sellevidx[[i]]["from"] == sellevidx[[i]]["to"]) { # maximum at one depth
                stop("todo")
                cmd_sellev <- ""
                depth_fname <- paste0("-", levels[sellevidx[[i]]["from"]])
            } else { # maximum in depth range
                cmd <- paste0(cmd, " -vertmax")
                cmd_sellev <- paste0("sellevidx,", sellevidx[[i]]["from"], "/", sellevidx[[i]]["to"])
                depth_fname <- paste0("-", levels[sellevidx[[i]]["from"]], "to-", levels[sellevidx[[i]]["to"]])
            }
        } # if missing(sellevel) or not
        depth_fname <- paste0("_sellevel_", depth_fname, "m")

        if (sellonlatbox[[i]]["lat1"] == sellonlatbox[[i]]["lat2"]) { # maximum along lat
            cmd_sellonlat <- paste0("remapnn,lon=0/lat=", sellonlatbox[[i]]["lat1"])
            area_fname <- sellonlatbox[[i]]["lat1"]
        } else { # maximum in latitude range
            cmd <- paste0(cmd, " -mermax")
            cmd_sellonlat <- paste0("sellonlatbox,", paste0(sellonlatbox[[i]], collapse=","))
            area_fname <- paste0(sellonlatbox[[i]]["lat1"], "to", sellonlatbox[[i]]["lat2"])
        }
        area_fname <- paste0("_moc", area_fname, "N")
        fout_replacement <- paste0(depth_fname, area_fname, "_")

        fout <- gsub("_global_", fout_replacement, basename(fin))
        cmd <- paste0(cmd, " -", cmd_sellev, " -", cmd_sellonlat, " ", fin, " ", 
                      outpath, "/", fout)
        if (verbose) message("run `", cmd, "`")
        system(cmd)

        # add used sellevidx as global argument to file
        cmd <- paste0(cdo, " setattribute,sellevidx=\"", paste(sellevidx[[i]], collapse="to"), "\" ", fout, " ",
                      outpath, "/tmp && mv ", outpath, "/tmp ", fout)
        if (verbose) message("run `", cmd, "`")
        system(cmd)

    } # for i sellonlatbox

} # mpiom_moc_extract_ts


# get mpiom land-sea-mask outline
mpiom_get_lsm_segments <- function(f_data="mpiom_GR30_curvilinear_120x101_data.nc", 
                                   f_grid="GR30s.nc",
                                   f_out) {

    if (missing(f_data)) stop("`f_data` is missing")
    if (missing(f_grid)) stop("`f_grid` is missing")
    if (!file.exists(f_data)) stop("`f_data` = ", f_data, " does not exist")
    if (!file.exists(f_grid)) stop("`f_grid` = ", f_grid, " does not exist")
    if (file.access(f_data, mode=4) == -1) stop("`f_data` = ", f_data, " not readable")
    if (file.access(f_grid, mode=4) == -1) stop("`f_grid` = ", f_grid, " not readable")

    if (missing(f_out)) {
        f_out <- c(paste0("mpiom_", tools::file_path_sans_ext(f_grid), "_land_sea_mask_segments_lon360.txt"),
                  paste0("mpiom_", tools::file_path_sans_ext(f_grid), "_land_sea_mask_segments_lon180.txt"))
    } else {
        if (length(f_out) != 2) stop("`f_out` must be of length 2. one for 0,360 and one for -180,180 lons")
    }

    # get data
    message("read `f_data` = ", f_data, " ...")
    nc_data <- nc_open(f_data)
    data <- ncvar_get(nc_data, "lm_THO_as_time_slope")
    nx <- dim(data)[1]; ny <- dim(data)[2]

    # get 4 lon,lat coords per curvilinear box
    message("read `f_grid` = ", f_grid, " ...")
    nc_grid <- nc_open(f_grid)
    lon_centers <- ncvar_get(nc_grid, "grid_center_lon")
    lat_centers <- ncvar_get(nc_grid, "grid_center_lat")
    lon_corners <- ncvar_get(nc_grid, "grid_corner_lon")
    lat_corners <- ncvar_get(nc_grid, "grid_corner_lat")

    if (dim(lon_centers)[1] != nx ||
        dim(lon_centers)[2] != ny) {
        stop("dimensions of data ", nx, ",", ny, 
             " do not fit to dimensions of lon/lat_centers ", 
             dim(lon_centers)[1], ",", dim(lon_centers)[2])
    }

    # for all land (NA) points
    x0 <- y0 <- x1 <- y1 <- c()
    cnt <- 0
    land_xy <- which(is.na(data), arr.ind=T)
    message("find land-sea-mask segments for ", length(land_xy[,"row"]), " land points ...") 
    for (i in seq_along(land_xy[,"row"])) { 
        
        # find neighbour points
        if (T) { # dont consider boundaries
            x2check <- y2check <- c()
            if (land_xy[i,"row"] > 1) x2check <- c(x2check, land_xy[i,"row"] - 1) 
            if (land_xy[i,"row"] < nx) x2check <- c(x2check, land_xy[i,"row"] + 1) 
            if (land_xy[i,"col"] > 1) y2check <- c(y2check, land_xy[i,"col"] - 1)
            if (land_xy[i,"col"] < ny) y2check <- c(y2check, land_xy[i,"col"] + 1)
        } else if (F) { # todo: consider also boundaries (see todo below)
            x2check <- c(land_xy[i,"row"] - 1, land_xy[i,"row"] + 1)
            y2check <- c(land_xy[i,"col"] - 1, land_xy[i,"col"] + 1)
        }
        check_xy <- rbind(cbind(x2check, rep(land_xy[i,"col"], t=length(x2check))),
                          cbind(rep(land_xy[i,"row"], t=length(y2check)), y2check))
        dimnames(check_xy)[1] <- list(NULL)
        colnames(check_xy) <- c("row", "col")
    
        lonlat_center_land <- c(lon_centers[land_xy[i,"row"],land_xy[i,"col"]],
                                lat_centers[land_xy[i,"row"],land_xy[i,"col"]])
        
        # check 4 neighbour points: left, right, below and above if any is boundary (index=0) or ocean (=not NA)
        for (j in seq_along(check_xy[,"row"])) {
            
            if ((any(check_xy[j,] == 0) || check_xy[j,"row"] > nx || check_xy[j,"col"] > ny) || # neighbour is boundary 
                !is.na(data[check_xy[j,"row"],check_xy[j,"col"]])) { # neighbour is ocean
                
                cnt <- cnt + 1 # total number of boundary segments
                message("seg ", cnt, " between land ", 
                        land_xy[i,"row"], ",", land_xy[i,"col"], 
                        " (", paste(lonlat_center_land, collapse="°,"), "°) and ", appendLF=F) 
                
                lonlat_center_oce <- lon_intersect <- lat_intersect <- NULL
                bnd <- oce <- F

                # neighbour is boundary
                # todo: wich of the 4 corner points of a land cell (NA) are the boundaries?
                if (any(check_xy[j,] == 0) || check_xy[j,"row"] > nx || check_xy[j,"col"] > ny) { 
                
                    message("boundary ", check_xy[j,"row"], ",", check_xy[j,"col"]) 
                    bnd <- T

                    # get corner-points of land cell that are also boundary
                    if (check_xy[j,"row"] == 0) { # left boundary
                        lon_intersect <- lon_corners[3:4,1,check_xy[j,"col"]]
                    } else if (check_xy[j,"row"] > nx) { # right boundary
                        lon_intersect <- lon_corners[1:2,nx,check_xy[j,"col"]]
                    } else { # use corner lons of land
                        if (check_xy[j,"col"] == 0) { # lower boundary
                            lon_intersect <- lon_corners[2:3,land_xy[i,"row"],land_xy[i,"col"]]
                        } else if (check_xy[j,"col"] > ny) { # upper boundary 
                            lon_intersect <- lon_corners[c(4,1),land_xy[i,"row"],land_xy[i,"col"]]
                        }
                    }
                    if (check_xy[j,"col"] == 0) { # lower boundary
                        lat_intersect <- lat_corners[2:3,check_xy[j,"row"],1]
                    } else if (check_xy[j,"col"] > ny) { # upper boundary
                        lat_intersect <- lat_corners[c(4,1),check_xy[j,"row"],ny]
                    } else { # use corner lats of land
                        if (check_xy[j,"row"] == 0) { # left boundary
                            lat_intersect <- lat_corners[3:4,land_xy[i,"row"],land_xy[i,"col"]]
                        } else if (check_xy[j,"row"] > nx) { # right boundary
                            lat_intersect <- lat_corners[1:2,land_xy[i,"row"],land_xy[i,"col"]]
                        }
                    }
                    #return(as.list(environment())) # for testing
                    #lon_intersect <- NULL
                    #lat_intersect <- NULL

                    if (F) {
                        inds=1:5
                        dev.new()
                        plot(0, t="n", xlim=range(r$x0[inds], r$x1[inds]), ylim=range(r$y0[inds], r$y1[inds]))
                        segments(r$x0[inds], r$y0[inds], r$x1[inds], r$y1[inds], col=1:5)
                        title("lower: 3:2")
                    }
                
                # neighbour is ocean 
                } else if (!is.na(data[check_xy[j,"row"],check_xy[j,"col"]])) { 
                    
                    lonlat_center_oce <- c(lon_centers[check_xy[j,"row"],check_xy[j,"col"]],
                                           lat_centers[check_xy[j,"row"],check_xy[j,"col"]])
                    message("ocean ", check_xy[j,"row"], ",", check_xy[j,"col"], 
                            " (", paste(lonlat_center_oce, collapse="°,"), "°)")
                    oce <- T

                    # get corner-points used by both neighbouring land and oce cells
                    lon_intersect <- intersect(lon_corners[,land_xy[i,"row"],land_xy[i,"col"]], 
                                               lon_corners[,check_xy[j,"row"],check_xy[j,"col"]])
                    if (length(lon_intersect) != 2) stop("there must be 2 lon_intersect")
                    lat_intersect <- intersect(lat_corners[,land_xy[i,"row"],land_xy[i,"col"]], 
                                               lat_corners[,check_xy[j,"row"],check_xy[j,"col"]]) 
                    if (length(lat_intersect) != 2) stop("there must be 2 lat_intersect")
                
                } # if neighbour is boundary or ocean

                # save
                if (!is.null(lon_intersect) && !is.null(lat_intersect)) {
                    x0 <- c(x0, lon_intersect[1])
                    y0 <- c(y0, lat_intersect[1])
                    x1 <- c(x1, lon_intersect[2])
                    y1 <- c(y1, lat_intersect[2])
                }

                # check
                if (T) {
                    if (cnt == 1) {
                        plotname <- paste0("segs.pdf")
                        pdf(plotname)
                    }
                    xlim <- range(lonlat_center_land[1],
                                  lon_corners[,land_xy[i,"row"],land_xy[i,"col"]])
                    if (oce) {
                        xlim <- range(xlim, lonlat_center_oce[1], 
                                      lon_corners[,check_xy[j,"row"],check_xy[j,"col"]])
                    }
                    ylim <- range(lonlat_center_land[2],
                                  lat_corners[,land_xy[i,"row"],land_xy[i,"col"]])
                    if (oce) {
                        ylim <- range(ylim, lonlat_center_oce[2],
                                      lat_corners[,check_xy[j,"row"],check_xy[j,"col"]])
                    }
                    plot(0, t="n", xlab="lon", ylab="lat", 
                         xlim=xlim, ylim=ylim, xaxt="n", yaxt="n") 
                    axis(1, at=pretty(xlim, n=15))
                    axis(2, at=pretty(ylim, n=15), las=2)
                    text(lon_corners[,land_xy[i,"row"],land_xy[i,"col"]],
                         lat_corners[,land_xy[i,"row"],land_xy[i,"col"]], 1:4, col="darkgreen")
                    text(lonlat_center_land[1], lonlat_center_land[2], "land (NA)", col="darkgreen")
                    if (bnd) {
                        title(paste0("segment ", cnt, ": boundary point"))
                    }
                    if (oce) {
                        title(paste0("segment ", cnt, ": ocean point"))
                        text(lon_corners[,check_xy[j,"row"],check_xy[j,"col"]],
                             lat_corners[,check_xy[j,"row"],check_xy[j,"col"]], 1:4, col="blue")
                        text(lonlat_center_oce[1], lonlat_center_oce[2], "ocean (not NA)", col="blue", pch=4)
                    }
                    if (!is.null(lon_intersect) && !is.null(lat_intersect)) {
                        segments(lon_intersect[1], lat_intersect[1], lon_intersect[2], lat_intersect[2])
                    }
                } # check
                
            } # if neighbour is ocean or boundary
        } # for j check_xy 4 neighbours
    } # for i land_xy NA points

    # save segments in 0,360 and -180,180 deg longitude
    x0180 <- x0
    x0180[x0180 > 180] <- x0180[x0180 > 180] - 360
    y0180 <- y0
    x1180 <- x1
    x1180[x1180 > 180] <- x1180[x1180 > 180] - 360
    y1180 <- y1

    # remove cyclic segments
    cycl_thr <- summary(abs(x0 - x1))[3]*100 # thr = 100*median(dx)
    cycl_inds <- which(abs(x0 - x1) > cycl_thr)
    if (length(cycl_inds) > 0) {
        message("remove ", length(cycl_inds), " cyclic segments whose abs(dx) > 100*median(dx) = ", 
                cycl_thr, "° lon from 0,360 lons ...")
        x0 <- x0[-cycl_inds]
        y0 <- y0[-cycl_inds]
        x1 <- x1[-cycl_inds]
        y1 <- y1[-cycl_inds]
    }
    cycl_thr <- summary(abs(x0180 - x1180))[3]*100 # thr = 100*median(dx)
    cycl_inds180 <- which(abs(x0180 - x1180) > cycl_thr)
    if (length(cycl_inds180) > 0) {
        message("remove ", length(cycl_inds180), " cyclic segments whose abs(dx) > 100*median(dx) = ", 
                cycl_thr, "° lon from -180,180 lons ...")
        x0180 <- x0180[-cycl_inds180]
        y0180 <- y0180[-cycl_inds180]
        x1180 <- x1180[-cycl_inds180]
        y1180 <- y1180[-cycl_inds180]
    }

    if (!is.null(dev.list())) {
        image.plot(seq_len(nx), seq_len(ny), data,
                   xlab=paste0("x index (nx=", nx, ")"), 
                   ylab=paste0("y index (ny=", ny, ")"))

        plot(0, xlim=range(x0,x1), ylim=range(y0,y1), t="n")
        maps::map("world2", add=T, interior=F, lwd=0.5)
        segments(x0, y0, x1, y1)

        plot(0, xlim=range(x0180,x1180), ylim=range(y0180,y1180), t="n")
        maps::map("world", add=T, interior=F, lwd=0.5)
        segments(x0180, y0180, x1180, y1180)
        
        message("save ", plotname, " ...")
        dev.off()
    }

    # save segments as ascii
    message("save ", f_out[1], " ...")
    write.table(data.frame(x0=x0, y0=y0, x1=x1, y1=y1), 
                file=f_out[1], row.names=F)
    message("save ", f_out[2], " ...")
    write.table(data.frame(x0=x0180, y0=y0180, x1=x1180, y1=y1180), 
                file=f_out[2], row.names=F)

    message("finished")

    return(as.list(environment())) # for testing

} # mpiom_get_lsm_segments

