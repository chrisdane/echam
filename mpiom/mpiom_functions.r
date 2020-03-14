# r

mpiom_ext_to_nc <- function(ext_files, partab_ext, outpath, verbose=T) {

    if (missing(ext_files) || !is.character(ext_files)) {
        stop("provide `fort_tar_files=\"/path/to/TIMESER.YYYY0101_YYYY1231.ext\"` or\n",
             "`fort_tar_files=c(\"/path/to/TIMESER.YYYY0101_YYYY1231.ext\", \"/path/to/TIMESER.ZZZZ0101_ZZZZ1231.ext\")`")
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
                    ext_files[fi], "\") = \"", outpath, "\"")
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
        cmd <- paste0("cdo -f nc -setpartab,", partab_ext, " ", ext_files[fi], " ", fout)
        if (verbose) message("   run `", cmd, "` ...")
        system(cmd)
            
    } # for fi ext_files

} # mpiom_ext_to_nc

mpiom_extract_fort_tar_data <- function(fort_tar_files, outpath, 
                                        keep_ext=T, partab_ext,
                                        keep_fort.75=T, keep_fort.90=T, 
                                        verbose=T) {

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
                cmd <- paste0("cdo -f nc -setpartab,", partab_ext, " ", 
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


mpiom_moc_make_bottom_topo <- function(cdo="cdo", varname="amoc", fin, fout, 
                                       mpiom_model_res=c(horiz="GR30", lev="L40"), reg_res=c(nlon=360, nlat=180), 
                                       verbose=T) {
   
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
        names(mpiom_model_res) != c("horiz", "lev")) {
        stop("provide `mpiom_model_res` as e.g. `mpiom_model_res=c(horiz=\"GR30\", lev=\"L40\")`")
    }
    
    if (class(reg_res) != "numeric" ||
        length(reg_res) != 2 ||
        names(reg_res) != c("nlon", "nlat")) {
        stop("provide `reg_res` as e.g. `reg_res=c(nlon=360, nlat=180)`")
    }

    # get format of input files
    cmd <- paste0("cdo showformat ", fin)
    if (verbose) message("run `", cmd, "`")
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
                          mpiom_model_res["lev"], "_geographic_grid.nc") 
    if (!file.exists(tracer_file)) {
        stop("`tracer_file` = \"", tracer_file, "\" does not exist")
    }
    bek_file <- paste0(pwd, "/mpiom/mpiom_r", reg_res["nlon"], "x", reg_res["nlat"], "_bek.nc")
    if (!file.exists(bek_file)) {
        stop("`bek_file` = \"", bek_file, "\" does not exist")
    }

    # generate moc mask file, i.e. set basins out of moc area to miss val
    basin_mask_file <- paste0(pwd, "/mpiom/mpiom_r", 
                              reg_res["nlon"], "x", reg_res["nlat"], mpiom_model_res["lev"],
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

    } # if !file.exists(basin_mask_file)

    # calculate zonal average sea floor elevation
    basin_zonmean_mask_file <- paste0(pwd, "/mpiom/mpiom_r",
                                      reg_res["nlon"], "x", reg_res["nlat"], mpiom_model_res["lev"],
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
                                               reg_res["nlon"], "x", reg_res["nlat"], mpiom_model_res["lev"],
                                               "_", varname, "_zonal_max_depths.nc")
        #${cdo} ensmax depths*.nc max_depth.nc
        cmd <- paste0(cdo, " ensmax ", paste(depth_files, collapse=" "), " ", basin_zonmean_max_depth_file)
        if (verbose) message("\nrun `", cmd, "`")
        system(cmd)
        file.remove(depth_files)

        basin_zonmean_mean_depth_file <- paste0(pwd, "/mpiom/mpiom_r", 
                                                reg_res["nlon"], "x", reg_res["nlat"], mpiom_model_res["lev"],
                                                "_", varname, "_zonal_mean_depths.nc")
        #${cdo} zonmean max_depth.nc zon_mean_depth.nc
        cmd <- paste0(cdo, " zonmean ", basin_zonmean_max_depth_file, " ", basin_zonmean_mean_depth_file)
        if (verbose) message("\nrun `", cmd, "`")
        system(cmd)
    
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
    # --> keep depth files re-use if needed

    # "var101" --> "amoc"
    cmd <- paste0(cdo, " setname,", varname, " ", fout, " ", 
                  outpath, "/tmp && mv ", outpath, "/tmp ", fout)
    if (verbose) message("\nrun `", cmd, "`")
    system(cmd)

} # mpiom_moc_make_bottom_topo


