# r

pg_verbose <- F
pdois <- list()

# d18O_* data from ice core
if (F) {
    pdois <- c(pdois,
               list("petit_etal_1999"=
                    list(pdoi="10.1594/PANGAEA.55505",
                         vars=list("dD_ice_smow"=list(inputname="δD [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (F) {
    pdois <- c(pdois,
               list("lorius_etal_1985"=
                    list(pdoi="10.1594/PANGAEA.860950",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (F) {
    pdois <- c(pdois, 
               list("masson-delmotte_etal_2011"=
                    list(pdoi="10.1594/PANGAEA.785228",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("vasilchuk_etal_2020"=
                    list(pdoi="10.1594/PANGAEA.917714",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("vasilchuk_etal_2020_time"="Comm")),
                                   "T_air_jan_deg"=list(inputname="T air (1) [°C]",
                                                        dims=list("vasilchuk_etal_2020_time"="Comm")),
                                   "T_air_win_deg"=list(inputname="T win [°C]",
                                                        dims=list("vasilchuk_etal_2020_time"="Comm"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("holme_etal_2019"=
                    list(pdoi="10.1594/PANGAEA.901844",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW] (annual, vs. V-SMOW, normalize...)", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (F) {
    pdois <- c(pdois, 
               list("van_bellen_etal_2019"=
                    list(pdoi="10.1594/PANGAEA.905297",
                         vars=list("d18o_w_smow"=list(inputname="Comment (air temperature - 1,2,3,4,5,6...)",
                                                      dims=list("kyr_before_1950_start"="Age [ka BP] (start)",
                                                                "kyr_before_1950_end"="Age [ka BP] (end)"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849160",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849159",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849155",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849237",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849150",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849151",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849157",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849156",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849154",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849148",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849158",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849153",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849152",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("weißbach_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.849149",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("vinther_etal_2010"=
                    list(pdoi="10.1594/PANGAEA.786356",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW] (annual)", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("vinther_etal_2010"=
                    list(pdoi="10.1594/PANGAEA.786354",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW] (annual)", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("vinther_etal_2010"=
                    list(pdoi="10.1594/PANGAEA.786302",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW] (annual)", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("vinther_etal_2010"=
                    list(pdoi="10.1594/PANGAEA.786299",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("vinther_etal_2010"=
                    list(pdoi="10.1594/PANGAEA.786289",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("white_etal_2009"=
                    list(pdoi="10.1594/PANGAEA.716878",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("rasmussen_etal_2007"=
                    list(pdoi="10.1594/PANGAEA.586860",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("rasmussen_etal_2007"=
                    list(pdoi="10.1594/PANGAEA.586863",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("andersen_etal_2007"=
                    list(pdoi="10.1594/PANGAEA.586841",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("andersen_etal_2007"=
                    list(pdoi="10.1594/PANGAEA.586836",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("miller_and_schwager_2004"=
                    list(pdoi="10.1594/PANGAEA.218274",
                         vars=list("d18o_w_smow"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                      dims=list("kyr_before_1950"="Age [ka BP]"))))))
}

# pa_th
if (F) {
    pdois <- c(pdois, 
               list("lippold_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.863978",
                         vars=list("pa_th"=list(inputname="(231Pa/230Th)", 
                                                dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (F) {
    #ng et al. 2018
    pdoi <- c("10.1594/PANGAEA.890927", "10.1594/PANGAEA.890928", "10.1594/PANGAEA.890929", "10.1594/PANGAEA.890930") 
    #gherardi et al. 2009
    pdoi <- "10.1594/PANGAEA.760022"
    if (any(tmp_names == "MD95-2037")) {
        message("use MD95-2037 of lippold et al. 2012 instead of gherardi et al. 2009")
        tmp_names <- tmp_names[-which(tmp_names == "MD95-2037")]
    }
    if (any(tmp_names == "SU90-44")) {
        message("use SU90-44 of lippold et al. 2012 instead of gherardi et al. 2009")
        tmp_names <- tmp_names[-which(tmp_names == "SU90-44")]
    }
    if (tmp_names[i] == "SU90-44") {
        lon <- -17.910000; lat <- 50.103333
    } else if (tmp_names[i] == "MD95-2027") {
        lon <- -47.413200; lat <- 41.744500
    } else if (tmp_names[i] == "MD95-2037") {
        lon <- -32.031167; lat <- 37.087167  
    } else {
        stop("this should not happen")
    }
    # süfke et al. 2019
    pdoi <- c("10.1594/PANGAEA.908149", "10.1594/PANGAEA.908150", "10.1594/PANGAEA.908151", "10.1594/PANGAEA.908152",
              "10.1594/PANGAEA.908153", # updated GeoB1523-1 of lippold et al. 2016
              "10.1594/PANGAEA.908154", # updated KNR140-12JPC of lippold et al. 2016
              "10.1594/PANGAEA.908155") # updated GeoB1515-1 of lippold et al. 2016
    if (names(tmp[[1]]$metadata$events)[1] == "KNR140-12JPC (KNR140-2-12JPC)") {
        message("use shorter name \"KNR140-12JPC\" instead of \"", names(tmp[[1]]$metadata$events)[1], "\"")
        names(tmp[[1]]$metadata$events)[1] <- "KNR140-12JPC"
    }
    if (tmp_names[i] == "GeoB1515-1") {
        lon <- -43.666667; lat <- 4.238333
    } else {
        stop("this should not happen")
    }
    if (pdoi[i] == "10.1594/PANGAEA.908155") {
        message("instead of erroneous pangaea data:")
        message(paste(PaTh, collapse=","))
        message("use values from palo20807-sup-0001-2019pa003737-ts01.xls:")
        PaTh <- c(0.060,0.047,0.064,0.082,0.090,0.096,0.078,0.070,0.079,0.088,0.073,0.078,0.083,0.081)
        message(paste(PaTh, collapse=","))
        if (length(years) != length(PaTh)) stop("years and PaTh are of different length")
    }
    #lippold et al. 2016
    pdoi <- "10.1594/PANGAEA.863978" 
    if (any(tmp_names == "GeoB1515-1")) {
        message("use GeoB1515-1 of süfke et al. 2019 instead of lippold et al. 2016")
        tmp_names <- tmp_names[-which(tmp_names == "GeoB1515-1")]
    }
    if (any(tmp_names == "GeoB1523-1")) {
        message("use GeoB1523-1 of süfke et al. 2019 instead of lippold et al. 2016")
        tmp_names <- tmp_names[-which(tmp_names == "GeoB1523-1")]
    }
    if (any(tmp_names == "KNR140-12JPC")) {
        message("use KNR140-12JPC of süfke et al. 2019 instead of lippold et al. 2016")
        tmp_names <- tmp_names[-which(tmp_names == "KNR140-12JPC")]
    }
    #lippold et al. 2012
    pdoi <- "10.1594/PANGAEA.788550"
    if (any(tmp_names == "177-1089A")) {
        message("use 177-1089A from lippold et al. 2016")
        tmp_names <- tmp_names[-which(tmp_names == "177-1089A")]
    }
    if (any(tmp_names == "M35003-4")) {
        message("use M35003-4 from lippold et al. 2016")
        tmp_names <- tmp_names[-which(tmp_names == "M35003-4")]
    }
    if (any(tmp_names == "GeoB1515-1")) {
        message("use GeoB1515-1 from süfke et al. 2019")
        tmp_names <- tmp_names[-which(tmp_names == "GeoB1515-1")]
    }
    if (any(tmp_names == "GeoB1523-1")) {
        message("use GeoB1523-1 from süfke et al. 2019")
        tmp_names <- tmp_names[-which(tmp_names == "GeoB1523-1")]
    }
    if (any(tmp_names == "KNR140-12JPC")) {
        message("use KNR140-12JPC from süfke et al. 2019")
        tmp_names <- tmp_names[-which(tmp_names == "KNR140-12JPC")]
    }
    # lippold et al. 2009
    pdoi <- "10.1594/PANGAEA.763199"
    if (tmp_names[i] == "172-1063B") {
        lon <- -57.614940; lat <- 33.686470
    } else if (tmp_names[i] == "172-1063D") {
        lon <- -57.615110; lat <- 33.686190
    } else {
        stop("this should not happen")
    }
    # meckler et al. 2013
    pdoi <- "10.1594/PANGAEA.810309"
    # mulitza et al. 2017
    pdoi <- "10.1594/PANGAEA.877699"
} # pa_th

# silt_fraction
if (F) {
    #praetorius et al. 2008
    #"Size fraction 0.063-0.010 mm, sortable silt [%] (63-10 µm sort silt)"
    pdoi <- "10.1594/PANGAEA.769648"
    # moffa-sanchez et al. 2015
    pdoi <- c("10.1594/PANGAEA.899381", "10.1594/PANGAEA.899382")
    # miettinen et al. 2012
} # silt

#################################################

pg <- list()
if (length(pdois) == 0) {
    message("`length(pdois)=0`. nothing to do")

} else {
    # step 1/2: load data and save in lipd-like format
    message("load ", length(pdois), " pangaea dois ...")
    
    for (pgi in seq_along(pdois)) {
        if (pgi == 1) library(pangaear)
        message("**********************************************\n",
                "load doi ", pgi, "/", length(pdois), appendLF=F)
        if (is.null(pdois[[pgi]]$pdoi)) stop("`pdois[[", pgi, "]]$pdoi` is null")
        if (is.null(names(pdois)[pgi])) stop("`names(pdois)[", pgi, "]` is null")
        message(": \"", pdois[[pgi]]$pdoi, "\" (", names(pdois)[pgi], ") ...")
      
        pgin <- pangaear::pg_data(pdois[[pgi]]$pdoi, mssgs=F)
        
        # check every dataset of current doi for wanted variables
        df <- data.frame()
        for (datai in seq_along(pgin)) { 
            
            if (!any(names(pgin[[datai]]$metadata) == "events")) {
                message("dataset `pgin[[", datai, "]]$metadata` (https://doi.pangaea.de/", 
                        pgin[[datai]]$doi, ") has no \"events\"-entry. skip")
            } else {
            
                # get current events of current data of current doi
                if (pg_verbose) message("   check dataset ", datai, "/", length(pgin), " of this doi ...")
                locs <- lons <- lats <- c() # per event of current dataset of current doi
                
                # case 1/2: `metadata$events` is list
                if (is.list(pgin[[datai]]$metadata$events)) {  
                    if (pg_verbose) message("      event case 1")
                    events <- pgin[[datai]]$metadata$events
                    locs <- lons <- lats <- NA
                    if (any(names(events) == "LOCATION")) locs <- as.character(events$LOCATION)
                    if (any(names(events) == "LONGITUDE")) lons <- as.numeric(events$LONGITUDE)
                    if (any(names(events) == "LATITUDE")) lats <- as.numeric(events$LATITUDE)
                    events <- names(events)[1]

                # case 2/2: `metadata$events` is not a list
                } else if (!is.list(pgin[[datai]]$metadata$events)) { 
                    if (pg_verbose) message("      event case 2")
                    event_string <- pgin[[datai]]$metadata$events # long string
                    if (length(event_string) != 1) {
                        stop("dataset events `pgin[[", datai, "]]$events` = \"", 
                             paste(event_string, collapse="\"\n\""), 
                             "\" is not of length 1.")
                    }
                    
                    # more than one event
                    if (any(names(pgin[[datai]]$data) == "Event")) { 
                        events <- unique(pgin[[datai]]$data$Event)
                    # just one event
                    } else { 
                        first_star_char <- regexpr("\\*", event_string) 
                        if (first_star_char == -1) {
                            stop("did not find a star \"*\" character in event \"", 
                                 event_string, "\".")
                        }
                        events <- substr(event_string, 1, first_star_char-2)
                    }
                    if (pg_verbose) message("      check ", length(events), " event", 
                            ifelse(length(events) > 1, "s", ""), ": \"",
                            paste(events, collapse="\", \""), "\"")

                    if (!any(search() == "package:stringr")) library(stringr)
                    eventinds <- stringr::str_locate_all(event_string, events)
                    if (any(sapply(eventinds, "dim")[1,] == 0)) {
                        inds <- which(sapply(eventinds, "dim")[1,] == 0)
                        stop(length(inds), " event", ifelse(length(inds) > 1, "s", ""), 
                             " \"", paste(events[inds], collapse="\", \""), "\" occur ", 
                             paste(sapply(eventinds[inds], dim)[1,], collapse=", "),
                             " times (and not once) in ", nchar(event_string), 
                             " characters long `event_string` = `pgin[[datai=", 
                             datai, "]]$metadata$events`. solve this")
                    }
                    # use first occurence of event
                    eventinds <- lapply(eventinds, "[", 1,)
                    sortinds <- sort(sapply(eventinds, "[", 1), index.return=T)$ix
                    eventinds <- eventinds[sortinds]
                    events <- events[sortinds]
                    names(eventinds) <- events
                    
                    # get event infos of current dataset of current doi from long string
                    for (eventi in seq_along(events)) {
                        # get eventi-substring from long all-events string
                        inds <- eventinds[[eventi]]["start"] # from current
                        if (eventi == length(events)) { # to end
                            inds[2] <- nchar(event_string)
                        } else { # to next event
                            inds[2] <- eventinds[[eventi+1]]["start"] - 2
                        }
                        event <- substr(event_string, inds[1], inds[2])
                        if (pg_verbose) message("         check eventi=", eventi, " \"", event, "\" ...")
                        if (grepl("LOCATION:", event)) {
                            loc <- regexpr("LOCATION:", event)
                            next_star_char <- regexpr("\\*", substr(event, loc, nchar(event)))
                            loc <- substr(event, loc+10, loc+next_star_char-3)
                            locs <- c(locs, loc)
                        } else {
                            locs <- c(locs, NA)
                        }
                        if (grepl("LONGITUDE:", event)) {
                            lon <- regexpr("LONGITUDE:", event)
                            next_star_char <- regexpr("\\*", substr(event, lon, nchar(event)))
                            lon <- as.numeric(substr(event, lon+11, lon+next_star_char-3))
                            lons <- c(lons, lon)
                        } else {
                            lons <- c(lons, NA)
                        }
                        if (grepl("LATITUDE:", event)) {
                            lat <- regexpr("LATITUDE:", event)
                            next_star_char <- regexpr("\\*", substr(event, lat, nchar(event)))
                            lat <- as.numeric(substr(event, lat+10, lat+next_star_char-3))
                            lats <- c(lats, lat)
                        } else {
                            lats <- c(lats, NA)
                        }
                    } # for eventi in events of current dataset of current doi
              
                # all other cases
                } else {
                    message("`pgin[[", datai, "]]`:") 
                    cat(capture.output(str(pgin[[datai]])), sep="\n")
                    stop("case not defined")
                }
                
                rows <- data.frame(event=events, loc=locs, lon=lons, lat=lats, stringsAsFactors=F)
                if (pg_verbose) {
                    message("dataset ", datai, "/", length(pgin), " has ", 
                            dim(rows)[1], " event", ifelse(dim(rows)[1] > 1, "s", ""), ":")
                    ht(rows)
                }
                df <- rbind(df, rows)

            } # events are defined at all
        } # for datai in seq_along(pgin)
          
        # several events in current dataset in current doi
        if (dim(df)[1] > 1) {

            # check if any event/lon/lat-combi occurs more than once 
            sameinds <- vector("list", l=dim(df)[1])
            for (eventi in seq_len(dim(df)[1])) {
                sameinds[[eventi]] <- eventi # default: no duplicate event/lon/lat-combi
                if (all(!is.na(c(df$event[eventi], df$lon[eventi], df$lat[eventi])))) {
                    sameinds[[eventi]] <- which(df$event == df$event[eventi] &
                                                df$lon == df$lon[eventi] &
                                                df$lat == df$lat[eventi])
                }
            } # for eventi 

            # if any event/lon/lat-combi occurs more often than once
            if (any(duplicated(sameinds))) {
                sameinds <- sameinds[-which(duplicated(sameinds))]
                df2 <- data.frame()
                for (eventi in seq_along(sameinds)) {
                    message("merge ", length(sameinds[[eventi]]), 
                            " identical event/lon/lat-combis ", 
                            paste(sameinds[[eventi]], collapse=","), 
                            ":\n", paste(paste0("event=\"", df$event[sameinds[[eventi]]], "\"; ",
                                                "lon=", df$lon[sameinds[[eventi]]], "; ",
                                                "lat=", df$lat[sameinds[[eventi]]]), 
                                     collapse="\n"))
                    df2 <- rbind(df2, cbind(df[sameinds[[eventi]][1],], 
                                            datainds=paste(sameinds[[eventi]], collapse=";")))
                }
                df <- df2
            } else { # every event/lon/lat-combi occurs only once
                df <- cbind(df, datainds=rep(1, t=length(sameinds)))
            }

        # only one event in current dataset in current doi
        } else { 
            df <- cbind(df, datainds=1)
        } # if dim(df)[1]) > 1 or not
            
        # check all events of all datasets of current doi for any wanted variables 
        message("check ", dim(df)[1], " event", 
                ifelse(dim(df)[1] > 1, "s", ""))
        ht(df)
        message("for any wanted variables ...")
        if (is.null(pdois[[pgi]]$vars)) stop("`pdois[[", pgi, "]]$vars` is null")
        for (vi in seq_along(pdois[[pgi]]$vars)) {
            # check every event of current doi if it has current wanted variable
            for (eventi in seq_len(dim(df)[1])) { 
                
                # get wanted variable of all events and datsets of current doi
                if (is.character(df$datainds[eventi])) {
                    datainds <- as.integer(strsplit(df$datainds[eventi], ";")[[1]])
                } else {
                    datainds <- df$datainds[eventi]
                    if (length(datainds) > 1) stop("this should not happen")
                }
                for (datai in seq_along(datainds)) {
                    dataind <- datainds[datai]
                    if (pg_verbose) {
                        message("   ", length(pgin[[dataind]]$data), " input variables of pgin[[", dataind, 
                                "]]-event ", eventi, "/", dim(df)[1], " \"", df$event[eventi], 
                                "\": \"", paste(names(pgin[[dataind]]$data), collapse="\", \""), 
                                "\"")
                        message("   wanted variable name of current doi: \"", 
                                pdois[[pgi]]$vars[[vi]]$inputname, "\"")
                    }
                    # save current variable of current event of current doi since this variable is wanted
                    if (any(names(pgin[[dataind]]$data) == pdois[[pgi]]$vars[[vi]]$inputname)) {
                        # get inds of event if necessary
                        if (any(names(pgin[[dataind]]$data) == "Event")) {
                            eventinds <- which(pgin[[dataind]]$data$Event == df$event[eventi])
                            if (length(eventinds) == 0) {
                                stop("found zero events in pgin[[", dataind, "]]$data$Event named \"", 
                                     df$event[eventi], "\"")
                            }
                        } else { # use all available
                            eventinds <- seq_along(pgin[[dataind]]$data[[pdois[[pgi]]$vars[[vi]]$inputname]])
                        }
                        # save dimension(s) of variable
                        dimlist <- list()
                        for (dimi in seq_along(pdois[[pgi]]$vars[[vi]]$dims)) {
                            dimind <- NULL
                            if (any(names(pgin[[dataind]]$data) == pdois[[pgi]]$vars[[vi]]$dims[[dimi]])) {
                                dimind <- which(names(pgin[[dataind]]$data) == pdois[[pgi]]$vars[[vi]]$dims[[dimi]])
                            }
                            if (is.null(dimind)) {
                                message("pgi=", pgi, " ", names(pdois)[pgi], 
                                        ": none of the ", length(names(pgin[[dataind]]$data)), 
                                        " loaded parameter names\n   \"", 
                                        paste(names(pgin[[dataind]]$data), collapse="\"\n   \""), "\"\n",
                                        "equals the provided `pdois[[pgi]]$vars[[vi]]$dims[[dimi]]` = \"", 
                                        pdois[[pgi]]$vars[[vi]]$dims[[dimi]], "\"")
                                stop("solve this")
                            }
                            dimlist[[dimi]] <- as.array(pgin[[dataind]]$data[[dimind]][eventinds])
                            names(dimlist)[dimi] <- names(pdois[[pgi]]$vars[[vi]]$dims)[dimi]
                        } # for dimi
                        outputname <- names(pdois[[pgi]]$vars)[vi]
                        #stop("asd")
                        #pg <- list()
                        #if (is.null(pg[[outputname]][[names(pdois)[pgi]]])) { # if doi-entry not already there
                        #    pg[[outputname]][[names(pdois)[pgi]]] <- list()
                        #}
                        # if event-entry not already there
                        if (is.null(pg[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]])) { 
                            pg[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]] <- 
                                list(doi=pdois[[pgi]]$pdoi, loc=df$loc[eventi], 
                                     lon=df$lon[eventi], lat=df$lat[eventi])
                        } 
                        # save variable
                        pg[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]]$data[[datai]] <- 
                            as.array(pgin[[dataind]]$data[[pdois[[pgi]]$vars[[vi]]$inputname]][eventinds])
                        pg[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]]$dims[[datai]] <- 
                            dimlist
                        names(pg[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]]$data)[datai] <-
                            names(pdois[[pgi]]$vars)[vi]
                        names(pg[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]]$dims)[datai] <- 
                            names(pdois[[pgi]]$vars)[vi]

                    } # if current variable of current event of current doi is wanted
                } # for datai all datas of current event of current doi
            } # for eventi in df
        } # for vi in wanted vars
    } # for pgi in pdois
    message("\nfinished loading ", length(pdois), " pangaea dois:")
    cat(capture.output(str(pg, max.level=2)), sep="\n")
    
    # step 2/2: modify dimensions if necessary
    message("\ncheck/modify dims/data of ", length(pdois), " pangaea dois ...")
    for (pvari in seq_along(pg)) {
        for (pdoi in seq_along(pg[[pvari]])) {
            for (eventi in seq_along(pg[[pvari]][[pdoi]])) {
                for (vi in seq_along(pg[[pvari]][[pdoi]][[eventi]]$dims)) {
                    for (dimi in seq_along(pg[[pvari]][[pdoi]][[eventi]]$dims[[vi]])) {
                        
                        dimin <- names(pg[[pvari]][[pdoi]][[eventi]]$dims[[vi]])[dimi]
                        dimvals <- data <- NULL # default: nothing to do
                        
                        # convert `kyr_before_1950` --> `year from 1950 CE`
                        if (dimin == "kyr_before_1950") {
                            dimout <- "yr_from_1950"
                            dimvals <- pg[[pvari]][[pdoi]][[eventi]]$dims[[vi]][[dimi]]
                            if (all(diff(dimvals) > 0)) { # flip timevals and time-dim of data
                                dimvals <- rev(dimvals)
                                data <- pg[[pvari]][[pdoi]][[eventi]]$data[[vi]]
                                timedimind <- which(dim(data) == length(dimvals))
                                if (length(timedimind) != 1) {
                                    stop("rare case that more than one dims of data `pg[[pvari=", 
                                         pvari, "]][[pdois=", pdoi, "]][[eventi=", eventi, 
                                         "]]$data[[vi=", vi, "]]` are of length ", 
                                         length(dimvals), 
                                         "(the length of the time-dimvals). solve manually")
                                }
                                cmd <- rep(",", t=length(dim(data)))
                                cmd[timedimind] <- paste0(length(dimvals), ":1")
                                cmd <- paste(cmd, collapse="")
                                cmd <- paste0("data <- data[", cmd, "]")
                                #message("   run `", cmd, "` ...")
                                eval(parse(text=cmd))
                            } # if flip
                            dimvals <- -1000*dimvals
                            dimvals <- make_posixlt_origin(dimvals, origin_in=1950, origin_out=1950)
                        
                        # convert `"Modern` --> `0 from 1950 CE` and "Holocene"` --> `-6000 from 1950 CE`
                        } else if (dimin == "vasilchuk_etal_2020_time") {
                            dimout <- "yr_from_1950"
                            dimvals <- pg[[pvari]][[pdoi]][[eventi]]$dims[[vi]][[dimi]]
                            if (any(dimvals == "Modern")) dimvals[which(dimvals == "Modern")] <- 0
                            if (any(dimvals == "Holocene")) dimvals[which(dimvals == "Holocene")] <- -6000
                            dimvals <- as.numeric(dimvals)
                            dimvals <- make_posixlt_origin(dimvals, origin_in=1950, origin_out=1950)
                        } # do dimin == "..." dim-specific things

                        # replace original dims/data with modified
                        if (!is.null(dimvals)) {
                            pg[[pvari]][[pdoi]][[eventi]]$dims[[vi]][[dimi]] <- dimvals
                            names(pg[[pvari]][[pdoi]][[eventi]]$dims[[vi]])[dimi] <- dimout
                        }
                        if (!is.null(data)) {
                            pg[[pvari]][[pdoi]][[eventi]]$data[[vi]] <- data
                        }

                    } # for dimi
                } # for vi
            } # for eventi
        } # for pdoi
    } # for pvari
    
    message("\nfinished modifying ", length(pdois), " pangaea dois")

    # todo: check double entries

} # if length(pdois) > 0


