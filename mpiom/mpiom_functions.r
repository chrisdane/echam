# r

mpiom_remap2lonlat <- function(files, cdo_select="", outpath, fout_rename_pattern, 
                               mpiom_model_grid="mpiom_GR30_model_grid_standard.nc", 
                               reg_res=c(nlon=120, nlat=101),
                               remap_method="remapcon2", 
                               cdo="cdo", convert2nc=T, verbose=T) {
    
    # todo: cdo_get_filetype() dependence from `functions` repo
    library(stringr)

    if (missing(files) || !is.character(files)) {
        stop("provide `files=\"/path/to/expid_mpiom_YYYY0101_YYYY1231.grb\"` or\n",
             "`files=c(\"/path/to/expid_mpiom_YYYY0101_YYYY1231.grb\", \"/path/to/expd_mpiom_ZZZZ10101_ZZZZ1231.nc\")`")
    }
    
    if (missing(outpath)) {
        outpath <- dirname(files[1])
        if (verbose) {
            message("`outpath` not given, use dirname(files[1]=\"", 
                    files[1], "\") = \"", outpath, "\"")
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

    if (fout_rename_pattern == "") {
        stop("`fout_rename_pattern` is empty string. dont know how to rename input file")
    }

    for (fi in seq_along(files)) {
        
        if (verbose) message("\nfile ", fi, "/", length(files), ": ", files[fi])
        
        # get format of input files
        if (convert2nc) { # if conversion to nc is wanted
            convert2nc <- cdo_get_filetype(files[fi])$file_type
            if (convert2nc == "non-nc") {
                convert2nc <- T
            } else {
                convert2nc <- F
            }
        }

        # output filename
        #fout <- paste0(outpath, "/", tools::file_path_sans_ext(basename(files[fi])))
        fout <- basename(files[fi])

        cmd <- paste0(cdo, " -P 8")
        if (convert2nc) {
            cmd <- paste0(cmd, " -t mpiom1 -f nc copy")
            if (tools::file_ext(fout) != "nc") fout <- paste0(fout, ".nc")
        }
        cmd <- paste0(cmd ," -", remap_method, ",r", reg_res["nlon"], "x", reg_res["nlat"], 
                      " -setgrid,", mpiom_model_grid)
        
        # variable selection before remapping
        if (cdo_select != "") {
            if (!grepl("select", cdo_select)) {
                stop("provided `cdo_select` = \"", cdo_select, "\" does not contain \"select\"")
            }
            if (convert2nc) {
                if (!grepl("code", cdo_select)) {
                    stop("input file needs to get converted to nc but provided `cdo_select` = \"", 
                         cdo_select, "\" does not contain \"code\"")
                }
            } else {
                if (!grepl("name", cdo_select)) {
                    stop("provided `cdo_select` = \"", cdo_select, "\" does not contain \"name\"")
                }
            }
            cmd <- paste0(cmd, " -", cdo_select)
            tmp <- stringr::str_replace_all(cdo_select, "[[:punct:]]", "_")
            tmp <- stringr::str_replace_all(tmp, "=", "_")
            fout <- paste0(fout, "_", tmp)
            # edit output filename
            fout <- gsub(fout_rename_pattern, 
                         paste0(fout_rename_pattern, "_", tmp, "_", remap_method, "_r", reg_res["nlon"], "x", reg_res["nlat"]),
                         fout)
        } else {
            # edit output filename
            fout <- gsub(fout_rename_pattern, 
                         paste0(fout_rename_pattern, "_", remap_method, "_r", reg_res["nlon"], "x", reg_res["nlat"]),
                         fout)
        } # cdo_select or not

        # run command with final output filename
        fout <- paste0(outpath, "/", fout)
        cmd <- paste0(cmd, " ", files[fi], " ", fout)
        if (verbose) message("run `", cmd, "` ...")
        #cdo -P 4 -t mpiom1 -f nc copy -remapcon2,r360x180 -setgrid,../mpiom_GR30_model_grid_standard.nc -select,code=183 Hol-Tx10_mpiom_32900101_32901231.grb zmld_curvilinear_setgrid_remapcon2_r360x180.nc 
        system(cmd)
        
        # fix negative values due to interpolation
        if (cdo_select != "") {
            if (any(cdo_select == c("select,code=183", "select,code=15"))) {
                message("special: set negative values to zero!")
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

} # mpiom_remap2lonlat


mpiom_ext_to_nc <- function(ext_files, partab_ext, outpath, cdo="cdo", verbose=T) {

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

