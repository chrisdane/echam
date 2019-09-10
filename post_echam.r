##
rm(list=ls()); graphics.off()

## Host options
machine <- system("hostname -f", intern=T)
message(paste0("Run on ", machine, ":"))
if (regexpr("ollie", machine) != -1 ||
    regexpr("prod-", machine) != -1 ||
    regexpr("fat-", machine) != -1) {
    machine_tag <- "ollie"
    homepath <- "~/scripts/r/"
    workpath <- "/work/ollie/cdanek/"
} else if (regexpr("hpc.dkrz", machine) != -1) {
    machine <- substr(machine, 1, regexpr(".hpc.dkrz", machine) - 1)
    machine_tag <- "mistral"
    homepath <- "~/scripts/r/"
    #workpath <- "/work/ba0941/a270073/"
    workpath <- "/work/ab0246/a270073/"
} else {
    message(paste0("   (unknown machine, use default paths)"))
    homepath <- "~/scripts/r/"
    workpath <- homepath
}
message(paste0("   homepath = ", homepath))
message(paste0("   workpath = ", workpath))

## load packages if not interactive
if (!interactive()) {
    message("load ncdf4 ...")
    library(ncdf4)
}

## user input

# ======================================================
# 1 setting
if (F) {
    models <- "awicm-CMIP6_hu"
    runids_plot <- "pi"
    froms <- 2699
    tos <- 2700
    old_post_tags <- T
} else if (F) {
    models <- "awicm-CMIP6"
    #runids_plot <- "restart_from_hu_oceanonly"
    #runids_plot <- "restart_from_restart_from_hu_oceanonly"
    #runids_plot <- "PI-CTRL_nodynveg"
    #runids_plot <- "PI-CTRL"
    #runids_plot <- "PI-CTRL2"
    #runids_plot <- "PI-CTRL3"
    runids_plot <- "PI-CTRL4"
    #froms <- 2701
    #froms <- 2714
    #froms <- 2800
    #froms <- 2869
    #froms <- 2998
    #froms <- 2900
    froms <- 2911 
    froms_shift <- 1761
    #froms <- 2999 
    #froms_shift <- 1849 # new time will be this, e.g. for senseless spinup years
    #tos <- froms
    #tos <- 2712
    #tos <- 2870
    #tos <- 2900
    #tos <- 2998
    tos <- 2999 # 4
    varnames <- "temp2"
    codes <- 167
    streams <- "g3bid" 
    #varnames <- "cloud_cover"
    #codes <- 162
    #streams <- "aclcim"
    #varnames <- "layer_moisture" # (3,19,78,268,698) m
    #codes <- 84
    #streams <- "jsbid"
    submodels <- "echam"
} else if (F) {
    models <- "awicm-test"
    runids_fname <- "CMIP6/CMIP_PMIP/dynveg_true/hist"
    runids_plot <- "hist"
    froms <- 1850
    tos <- froms
    #tos <- 1864
    varnames <- "temp2"
    submodels <- "echam"
    streams <- "echam3hr" 

# ======================================================
# 5 settings
} else if (T)  {
    models <- rep("awicm-test", t=5)
    runids_fname <- rep("CMIP6/CMIP_PMIP/dynveg_true/hist", t=5)
    runids_plot <- rep("hist", t=5)
    froms <- rep(1850, t=5)
    tos <- rep(froms, t=5)
    tos<- rep(1886, t=5)
    varnames <- rep("temp2", t=5)
    submodels <- rep("echam", t=5) 
    streams <- c("echammon", "echamday", "echam6hr", "echam3hr", "ma")
}

# echam streams so far:
# echam glim sp6h spim glday g3bim g3bid g3bday g3b1hi co2 aclcim accw
# cfdiag3hr cfdiagday cfdiagmon co2 tdiagmon
# ma echam3hr echam6hr echamday echammon
# echamdaymax echamdaymin
# ATM_mm BOT_MM LOG_MM

#modes <- c("timmean", "fldmean") # "timmean" "fldmean"
#modes <- "timmean"
modes <- "fldmean"
season_plot <- "Jan-Dec" # 

clean <- T # remove tmp files
set_rel_time <- F # conversion from absolute (default) to relative time
add_my_time <- T # my time for ts output 
cdo_silent <- "-s" # "-s" for silent or "" for non-silent

# ======================================================

## Declare
if (!exists("runids_plot")) stop("provide \"runids_plot\"")
nrunids <- length(runids_plot)
nmodes <- length(modes)
if (!exists("runids_fname") && exists("runids_plot")) runids_fname <- runids_plot
if (!exists("levs")) levs <- rep("", t=nrunids)
if (!exists("areas")) areas <- rep("global", t=nrunids)
if (!exists("codes")) codes <- rep(NA, t=nrunids)
if (!exists("old_post_tags")) old_post_tags <- rep(F, t=nrunids)
if (!exists("froms_shift")) froms_shift <- rep(NA, t=nrunids)
lev_fnames <- levs
lev_fnames[levs != ""] <- paste0("_", levs[levs != ""], "m")

## Check
if (!add_my_time && any(!is.na(froms_shift))) {
    stop("\"froms_shift\" is different from NA but \"add_my_time\" is False")
}

message("\nRun post_echam.r for modes: ", paste0(modes, collapse=","), " ...\n")

for (i in 1:nrunids) {

    message("\n*********** setting ", i, "/", nrunids, " *************")
    message("model:       ", models[i])
    message("runid_plot:  ", runids_plot[i])
    message("runid_fname: ", runids_fname[i])
    message("submodel:    ", submodels[i])
    message("varname:     ", varnames[i])
    message("code:        ", codes[i])
    message("stream:      ", streams[i])
    message("from:        ", froms[i])
    message("from_shift:  ", froms_shift[i])
    message("to:          ", tos[i])
    message("area:        ", areas[i])
    message("lev:         ", levs[i])

    ## paths
    pathin <- paste0(workpath, models[i], "/", runids_fname[i], "/outdata/", submodels[i])
    pathout <- paste0(workpath, "post/", models[i], "/", runids_fname[i], "/", submodels[i])
    if (models[i] == "awicm-CMIP6_hu") {
        pathin <- "/work/ollie/ukrebska/AWI_CM/pi477_u/cpl_output"
    } 
    pid <- Sys.getpid()
    pathtmp <- paste0(pathout, "tmp_", pid) # temporary path
    dir.create(pathtmp, recursive=T, showWarnings=F)

    ## time
    years <- froms[i]:tos[i]
    nyears <- length(years)

    ## files
    if (old_post_tags[i]) {
        cmd <- paste0("ls ", pathin, "/*MM_*echam.nc")
    } else {
        # e.g.
        # hist_echam6_echam_185001.nc
        # PI-CTRL4_echam6_g3bid_299901.grb 
        cmd <- paste0("ls ", pathin, "/*_*", submodels[i], "*_", streams[i], "_*")
    }
    message("\nfin <- ", cmd)
    fin <- system(cmd, intern=T)
    if (length(fin) == 0) stop("no files found")
    message("--> nf = ", length(fin))

    # this raises a warning due to .codes files
    yearsin <- as.integer(substr(fin, 
                                 start=regexpr(paste0("_", streams[i], "_"), fin) + nchar(streams[i]) + 2,
                                 stop=regexpr(paste0("_", streams[i], "_"), fin) + nchar(streams[i]) + 2 + 3))

    ## check time (this also removes .codes files)
    if (any(yearsin %in% years == F)) {
        outside_years_inds <- which(yearsin %in% years == F)
        if (length(outside_years_inds) == length(fin)) {
            stop("none files are within years ", paste0(range(years), collapse="-"))
        }
        message("remove ", length(outside_years_inds), " files outside of ", 
                paste0(range(years), collapse="-"), " ...")
        fin <- fin[-outside_years_inds]
        message("--> nf = ", length(fin))
    }

    ## apply season
    if (season_plot != "Jan-Dec") {
        stop("not yet")
        if (length(fin) == 0) stop("no files within time range")
    }

    nf <- length(fin)

    ## check for file type
    filetypes <- substr(fin, 
                        start=regexpr("\\.", fin) + 1,
                        stop=nchar(fin))
    filetype <- unique(filetypes)
    if (length(filetype) != 1) stop("there are more than 1 different filetypes")

    ## prepare
    if (filetype == "nc") {
        fout_prepare_check <- fin[1]

    } else if (filetype == "grb") {
        message("\nconvert 1 grb to nc to check file content ...")
        fout_prepare_check <- paste0(pathtmp, "/", basename(fin[1]), ".nc")
        cmd <- paste0("cdo ", cdo_silent, " -f nc copy ", fin[1], " ", fout_prepare_check)
        message("\n", cmd)
        system(cmd)

    } else {
        stop(paste0("cannot handle files of type ", filetype))
    }

    ## check for variable name and dimensions
    message("\ncheck variable names and dimensions of input ...")
    library(ncdf.tools)
    check_vars <- ncdf.tools::infoNcdfVars(fout_prepare_check)
   
    # check if user variable is in loaded data
    if (!is.na(codes[i])) {
        if (!any(check_vars[,"name"] == paste0("var", codes[i]))) {
            stop("variable 'var", codes[i], "' not found in\n   ", fout_prepare_check)
        } else {
            check_nvars <- check_vars[check_vars[,"name"] == paste0("var", codes[i]),"n.dims"]
        }
    } else if (is.na(codes[i])) {
        if (!any(check_vars[,"name"] == varnames[i])) {
            stop("variable '", varnames[i], "' not found in\n   ", fout_prepare_check)
        } else {
            check_nvars <- check_vars[check_vars[,"name"] == varnames[i],"n.dims"]
        }
    }
 
    # check input dimensions
    check_vars <- check_vars[,paste0(1:check_nvars, ".dim")]
    check_dims <- ncdf.tools::infoNcdfDims(fout_prepare_check)[,c("name", "length")]
    
    # check if input *file* has level/depth dim
    if (any(check_dims[,"name"] == "lev", na.rm=T) ||
        any(check_dims[,"name"] == "depth", na.rm=T)) { # input file has level/depth dim
       
        # check if input *variable* as levels/depth dim
        if (any(check_vars == "lev", na.rm=T) ||
            any(check_vars == "depth", na.rm=T)) {

            # check if user provided a depth level
            if (levs[i] == "" && is.na(levs[i])) {
                stop(paste0("error: input files have a 'lev'/'depth' dimension but you 'levs[", 
                            i, "]'=", levs[i], " m. set a proper depth."))
            } else {

                # check if user provided depth is a correct choice
                input_coords <- ncdf.tools::readNcdfCoordinates(fout_prepare_check)
                if (any(check_dims[,"name"] == "lev")) { # echam
                    input_depths <- input_coords$"lev"
                } else if (any(check_dims[,"name"] == "depth")) { # jsbach
                    input_depths <- input_coords$"depth"    
                }

                if (!any(input_depths == levs[i])) {
                    message(paste0("error: your provided depth level 'levs[", 
                                i, "]'=", levs[i], " is not included in (",
                                paste0(input_depths, collapse=","), ")."))
                    stop("choose one of them and rerun the script.")
                } else {
                    lev_fnames[i] <- paste0("_", levs[i], "m")
                }
            } # user provided a depth level?
        } # input variable has depth dim?
    } else { 
        lev_fnames[i] <- levs[i]
    } # input file has depth dim?

    message("\ncalculate ", varnames[i], 
            ifelse(levs[i] != "", paste0(" of level ", levs[i]), ""),
            " from ", froms[i], "-", tos[i], " (", length(years), " years) ...")
    #cat(paste0(fin, "\n"))

    systime1 <- array(NA, c(nmodes, nf, 5))
    fout_prepare <- array(NA, c(nf, nmodes))
    colnames(fout_prepare) <- modes

    # prepare: select variable and area (and depth/level) (and convert to nc in case of grb)
    for (j in 1:nf) {
        message("\n*********************")
        message(j, "/", nf)

        # prepare
        if (filetype == "grb") {
            fin_tmp <- paste0(pathtmp, "/", basename(fin[j]), ".nc")
            cmd <- paste0("cdo ", cdo_silent, " -f nc copy ", fin[j], " ", fin_tmp)
            message("\n", cmd)
            system(cmd)
        } else {
            fin_tmp <- fin[j]
        }

        for (k in 1:nmodes) { # timmean fldmean 

            message("\nmode ", k, "/", nmodes, ": ", modes[k])
            fout_prepare[j,k] <- paste0(pathtmp, "/",
                                        varnames[i], "_", modes[k], "_", areas[i], lev_fnames[i], "_", models[i], "_", 
                                        basename(fin_tmp))

            # multiple cdo args: execution from right to left
            if (areas[i] == "global") {
                
                cmd <- paste0("cdo ", cdo_silent, " ")

                # select mode
                if (modes[k] == "timmean") {
                    cmd <- paste0(cmd, "-timmean")
                } else if (modes[k] == "fldmean") {
                    cmd <- paste0(cmd, "-fldmean")
                } else {
                    stop(paste0("mode '", modes[k], "' not defined."))
                }

                # select level
                if (levs[i] != "" && !is.na(levs[i])) {
                    cmd <- paste0(cmd, " -sellevel,", levs[i])
                }

            } else {
                stop(paste0("area '", areas[i], "' is not implemented yet"))
            
            } # areas[i] == "global

            # select variable
            if (!is.na(codes[i])) {
                cmd <- paste0(cmd, " -selvar,var", codes[i])
            } else if (is.na(codes[i])) { # already post processed data
                cmd <- paste0(cmd, " -selvar,", varnames[i])
            }
            cmd <- paste0(cmd, " ", fin_tmp, " ", fout_prepare[j,k])

            message("\n", cmd)
            #stop("asd")
            systime1[k,j,] <- system.time({ system(cmd) }) # ~96 sec for 100 gb file

            ## if wanted, at the end of preparing, if there are grb-converted-to-nc files, delete them
            ## do not remove fin!!!
            if (clean && k == nmodes && filetype == "grb") {
                cmd <- paste0("rm ", fin_tmp)
                message("\n", cmd)
                system(cmd)
            }

            ## change from code to proper variable name
            if (!is.na(codes[i])) {
                cmd <- paste0("cdo ", cdo_silent, " chname,var", codes[i], ",", varnames[i], " ", 
                              fout_prepare[j,k], " ", fout_prepare[j,k])
                message("\n", cmd)
                system(cmd)
            }

            # if wanted, for "mean_ts" mode, set own time:
            # ncap2 -s time=time-0.9944444
            # at the end of the script the command string is too long for ncap2:
            # error: /sw/rhel6-x64/nco/nco-4.7.5-gcc64/bin/ncap2: Argument list too long
            if (add_my_time) {
                
                if (modes[k] == "fldmean") {
                
                    #message("set own time ...")
                    # convert any unit to seconds for POSIX, e.g. 
                    # "days since 1538-1-1 00:00:00"
                    # "day as %Y%m%d.%f"
                    
                    ncin <- nc_open(fout_prepare[j,k])
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
                    
                    # case 2: e.g. "day as %Y%m%d.%f"
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
                    if (j == 1) source("functions/leap_function.r")
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
                                  fout_prepare[j,k], " ", fout_prepare[j,k])
                    message("\n", cmd)
                    system(cmd)

                    if (!is.na(froms_shift[i]) && froms[i] != froms_shift[i]) {
                        cmd <- paste0("ncap2 -O -s 'defdim(\"time3\",", length(timeout2), "); time3[time3]={", 
                                      paste0(sprintf("%f", timeout2), collapse=","), 
                                      "}; time3@long_name=\"Time in fraction of a year start from ", timeout2[1], "\"' ",
                                      fout_prepare[j,k], " ", fout_prepare[j,k])
                        message("\n", cmd)
                        system(cmd)
                    }

                    # make record dim for ncrcat
                    cmd <- paste0("ncks -O -4 --mk_rec_dmn time ", fout_prepare[j,k], " ", fout_prepare[j,k])
                    message("\n", cmd)
                    system(cmd)
                    cmd <- paste0("ncks -O -4 --mk_rec_dmn time2 ", fout_prepare[j,k], " ", fout_prepare[j,k])
                    message("\n", cmd)
                    system(cmd)
                    if (!is.na(froms_shift[i]) && froms[i] != froms_shift[i]) {
                        cmd <- paste0("ncks -O -4 --mk_rec_dmn time3 ", fout_prepare[j,k], " ", fout_prepare[j,k])
                        message("\n", cmd)
                        system(cmd)
                    }

                } # if modes[k] == "mean_ts"
            
            } # if add_my_time

        } # for k nmodes

    } # for j fin

    for (k in 1:nmodes) {
        message("\n********************")
        message(paste0("preparing mode ", modes[k], ": ", nf, " files took ", 
                     round(sum(apply(systime1[k,,], 1, sum))/60, 3), 
                     " minutes in total (", round(mean(apply(systime1[k,,], 1, sum)), 3), 
                     " seconds each on average)"))
    }

    ## speed testing cdo vs ncdf.tools
    if (F) {
        library(ncdf.tools)
        systime2 <- rep(NA, t=length(fin))
        for (i in 1:length(fin)) {
            systime2[i] <- system.time({
                data <- ncdf.tools::readNcdf(paste0(pathin, "/", fin[i]), var.name=varnames[i])
                data <- apply(data, c(1, 2), mean) # ltm
            })[3] # ~93 sec
        }
        message("\nncdf.tools: ", length(fin), " files took ", round(sum(systime2)/60, 3),
                " minutes in total (", round(mean(systime2)/60, 3), 
                " minutes on average)")
    }

    message("******************")

    ## start calc
    for (k in 1:nmodes) {

        message("\nmode ", k, "/", nmodes, ": ", modes[k])

        ## load preapred files
        fin2 <- fout_prepare[,k]

        pathout_tmp <- paste0(pathout, "/", modes[k], "/", areas[i], "/", varnames[i], "/")
        dir.create(pathout_tmp, recursive=T, showWarnings=F)
        fout2 <- paste0(pathout_tmp, "/", models[i], "_", runids_plot[i], "_",
                        varnames[i], 
                        #ifelse(!is.null(code), paste0("_var", code), ""),
                        "_", streams[i], "_", modes[k],  
                        lev_fnames[i], "_", areas[i], "_",  
                        season_plot, "_", froms[i], "-", tos[i], ".nc")

        if (modes[k] == "timmean") {

            # check maximum number of input files for cdo ensmean:
            nmax <- as.integer(system("ulimit -n", intern=T))
            if (length(fin2) > nmax) {
                stop(paste0("cannot compute ltm mean of ", length(fin2), 
                            " files because cdo ensmean maximum files is ", nmax))
            }
            cmd <- "cdo -O ensmean"
        
        } else if (modes[k] == "fldmean") {

            if (add_my_time) { 
                # if add_my_time, cdo mergetime comes with this warning:
                # Warning (splitTimevalue) : Reset wrong date/time to 0000-01-11 00:00:00!
                # and kills my own time, so use ncrcat instead
                cmd <- "ncrcat -O"

            } else {
                cmd <- "cdo -O mergetime"
            }
        }

        cmd <- paste0(cmd, " ", paste0(fin2, collapse=" "), " ", fout2) 
        message("\n", cmd)

        ## note: system() does not run the command if its very long. i dont know why
        # workaround: save command in script and run in shell directly:
        if (T) {
            scriptname <- paste0(pathout, "/cmd_", modes[k], "_", pid, ".txt")
            message("\n--> run the above command via ", scriptname, " ...")
            writeLines(cmd, con=scriptname)
            system(paste0("chmod 755 ", scriptname))
            system(paste0(dirname(scriptname), "/./", basename(scriptname)))
            system(paste0("rm ", scriptname))
        } else {
            system(cmd)
        }

        # remove record dim of fout2 files
        if (add_my_time && modes[k] == "fldmean") {
            cmd_tmp <- paste0("ncks -O --fix_rec_dmn time ", fout2, " ", fout2)
            message("\n", cmd_tmp)
            system(cmd_tmp)
            cmd_tmp <- paste0("ncks -O --fix_rec_dmn time2 ", fout2, " ", fout2)
            message("\n", cmd_tmp)
            system(cmd_tmp)
            if (!is.na(froms_shift[i]) && froms[i] != froms_shift[i]) {
                cmd_tmp <- paste0("ncks -O --fix_rec_dmn time3 ", fout2, " ", fout2)
                message("\n", cmd_tmp)
                system(cmd_tmp)
            }
        }

        # remove record dim of tmp files
        if (!clean && add_my_time && modes[k] == "fldmean") {
            for (l in 1:length(fin2)) {
                cmd_tmp <- paste0("ncks -O --fix_rec_dmn time ", fin2[l], " ", fin2[l])
                message("\n", cmd_tmp)
                system(cmd_tmp)
                cmd_tmp <- paste0("ncks -O --fix_rec_dmn time2 ", fin2[l], " ", fin2[l])
                message("\n", cmd_tmp)
                system(cmd_tmp)
                if (!is.na(froms_shift[i]) && froms[i] != froms_shift[i]) {
                    cmd_tmp <- paste0("ncks -O --fix_rec_dmn time3 ", fin2[l], " ", fin2[l])
                    message("\n", cmd_tmp)
                    system(cmd_tmp)
                }
            } # for l fin2
        }

        # clean up
        ## do not remove fin!!!
        if (clean) {
            if (file.exists(fout2)) {
                message("\nclean:")
                cmd <- paste0("rm ", paste0(fin2, collapse=" "))
                message("\n", cmd)
                system(cmd)
                cmd <- paste0("rm -r ", pathtmp)
                message("\n", cmd)
                system(cmd)
            } else {
                message("\ncannot clean: output file fout2=\n", fout2, "\ndoes not exist!!")
            }
        } # if clean

        # set relative time?
        if (!add_my_time && set_rel_time && modes[k] == "fldmean") {
            if (file.exists(fout2)) {
                message("\nset relative time ...")
                cmd <- paste0("cdo ", cdo_silent, " -r copy ", fout2, " ", fout2)
                message("\n", cmd)
                system(cmd)
            } else {
                message("\ncannot set relative time: output file fout2=\n", fout2, "\ndoes not exist!!")
            }
        } # if set_rel_time

    } # for k nmodes

} # for i nrunids

message("\nfinish\n")


