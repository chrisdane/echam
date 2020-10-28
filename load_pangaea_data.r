# r

rm(list=ls())

options(warn=2) # stop on warnings
options(digits=10)
options(stringsAsFactors=F)

verbose <- F

pdois <- list()
if (T) {
    pdois <- c(pdois,
               list("petit_etal_1999"=
                    list(pdoi="10.1594/PANGAEA.55505",
                         vars=list("dDice"=list(inputname="δD [‰ SMOW]", 
                                                dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois,
               list("lorius_etal_1985"=
                    list(pdoi="10.1594/PANGAEA.860950",
                         vars=list("d18oh2o"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                 dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("masson-delmotte_etal_2011"=
                    list(pdoi="10.1594/PANGAEA.785228",
                         vars=list("d18oh2o"=list(inputname="δ18O H2O [‰ SMOW]", 
                                                dims=list("kyr_before_1950"="Age [ka BP]"))))))
}
if (T) {
    pdois <- c(pdois, 
               list("lippold_etal_2016"=
                    list(pdoi="10.1594/PANGAEA.863978",
                         vars=list("Pa/Th"=list(inputname="(231Pa/230Th)", 
                                                dims=list("kyr_before_1950"="Age [ka BP]"))))))
}

if (length(pdois) > 0) {
    pangaea <- list()
    for (pgi in seq_along(pdois)) {
        if (pgi == 1) library(pangaear)
        message("**********************************************\n",
                "load doi ", pgi, "/", length(pdois), ": \"", 
                pdois[[pgi]]$pdoi, "\" (", names(pdois)[pgi], ") ...")
        pg <- pangaear::pg_data(pdois[[pgi]]$pdoi, mssgs=F)
        df <- data.frame()
        for (datai in seq_along(pg)) { # check every dataset of current doi for wanted variables
            
            if (!any(names(pg[[datai]]$metadata) == "events")) {
                stop("dataset `pg[[", datai, "]]$metadata` has no \"events\"-entry")
            }
            
            # get current events of current data of current doi
            if (verbose) message("   check dataset ", datai, "/", length(pg), " of this doi ...")
            locs <- lons <- lats <- c() # per event of current dataset of current doi
            
            # case 1/2: `metadata$events` is list
            if (is.list(pg[[datai]]$metadata$events)) {  
                if (verbose) message("      event case 1")
                events <- pg[[datai]]$metadata$events
                locs <- lons <- lats <- NA
                if (any(names(events) == "LOCATION")) locs <- events$LOCATION
                if (any(names(events) == "LONGITUDE")) lons <- as.numeric(events$LONGITUDE)
                if (any(names(events) == "LATITUDE")) lats <- as.numeric(events$LATITUDE)
                events <- names(events)[1]

            # case 2/2: `metadata$events` is not a list
            } else if (!is.list(pg[[datai]]$metadata$events)) { 
                if (verbose) message("      event case 2")
                event_string <- pg[[datai]]$metadata$events # long string
                if (length(event_string) != 1) {
                    stop("dataset events `pg[[", datai, "]]$events` = \"", 
                         paste(event_string, collapse="\"\n\""), 
                         "\" is not of length 1.")
                }
                
                # more than one event
                if (any(names(pg[[datai]]$data) == "Event")) { 
                    events <- unique(pg[[datai]]$data$Event)
                # just one event
                } else { 
                    first_star_char <- regexpr("\\*", event_string) 
                    if (first_star_char == -1) {
                        stop("did not find a star \"*\" character in event \"", 
                             event_string, "\".")
                    }
                    events <- substr(event_string, 1, first_star_char-2)
                }
                if (verbose) message("      check ", length(events), " event", 
                        ifelse(length(events) > 1, "s", ""), ": \"",
                        paste(events, collapse="\", \""), "\"")

                if (!any(search() == "package:stringr")) library(stringr)
                eventinds <- stringr::str_locate_all(event_string, events)
                if (any(sapply(eventinds, "dim")[1,] == 0)) {
                    inds <- which(sapply(eventinds, "dim")[1,] == 0)
                    stop("event", ifelse(length(inds) > 1, "s", ""), " \"", 
                         paste(events[inds], collapse="\", \""), "\" not found in event-string ",
                         "`pg[[", datai, "]]$metadata$events = \"", event_string, "\"")
                }
                names(eventinds) <- events
                sortinds <- sort(sapply(eventinds, "[", 1), index.return=T)$ix
                eventinds <- eventinds[sortinds]
                
                # get event infos of current dataset of current doi from long string
                for (eventi in seq_along(events)) {
                    # get eventi-substring from long all-events string
                    inds <- eventinds[[eventi]][,"start"] # from current
                    if (eventi == length(events)) {
                        inds[2] <- nchar(event_string) # to end
                    } else {
                        inds[2] <- eventinds[[eventi+1]][,"start"] - 2 # to next event
                    }
                    event <- substr(event_string, inds[1], inds[2])
                    if (verbose) message("         check event substr \"", event, "\" ...")
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
           
            } else {
                message("`pg[[", datai, "]]`:") 
                cat(capture.output(str(pg[[datai]])), sep="\n")
                stop("case not defined")
            }
            
            rows <- data.frame(event=events, loc=locs, lon=lons, lat=lats)
            if (verbose) {
                message("dataset ", datai, "/", length(pg), " has ", 
                        dim(rows)[1], " event", ifelse(dim(rows)[1] > 1, "s", ""), ":")
                ht(rows)
            }
            df <- rbind(df, rows)
        
        } # for datai in seq_along(pg)
          
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
                    if (verbose) {
                        message("   ", length(pg[[dataind]]$data), " input variables of pg[[", dataind, 
                                "]]-event ", eventi, "/", dim(df)[1], " \"", df$event[eventi], 
                                "\": \"", paste(names(pg[[dataind]]$data), collapse="\", \""), 
                                "\"")
                        message("   wanted variable name of current doi: \"", 
                                pdois[[pgi]]$vars[[vi]]$inputname, "\"")
                    }
                    # save current variable of current event of current doi since this variable is wanted
                    if (any(names(pg[[dataind]]$data) == pdois[[pgi]]$vars[[vi]]$inputname)) {
                        # get inds of event if necessary
                        if (any(names(pg[[dataind]]$data) == "Event")) {
                            eventinds <- which(pg[[dataind]]$data$Event == df$event[eventi])
                            if (length(eventinds) == 0) {
                                stop("found zero events in pg[[", dataind, "]]$data$Event named \"", df$event[eventi], "\"")
                            }
                        } else { # use all available
                            eventinds <- seq_along(pg[[dataind]]$data[[pdois[[pgi]]$vars[[vi]]$inputname]])
                        }
                        # save dimension(s) of variable
                        dimlist <- list()
                        for (dimi in seq_along(pdois[[pgi]]$vars[[vi]]$dims)) {
                            dimind <- NULL
                            if (any(names(pg[[dataind]]$data) == pdois[[pgi]]$vars[[vi]]$dims[[dimi]])) {
                                dimind <- which(names(pg[[dataind]]$data) == pdois[[pgi]]$vars[[vi]]$dims[[dimi]])
                            }
                            if (is.null(dimind)) {
                                stop("not any of the pg[[", dataind, 
                                     "]]-event ", eventi, "/", dim(df)[1], " \"", df$event[eventi], 
                                     "\" input variables \"", paste(names(pg[[dataind]]$data), collapse="\", \""), 
                                     "\" is the needed dimension pdois[[", pgi, "]]$vars[[", vi, 
                                     "]]$dims[[", dimi, "]] = \"", pdois[[pgi]]$vars[[vi]]$dims[[dimi]], "\"")
                            }
                            dimlist[[dimi]] <- pg[[dataind]]$data[[dimind]][eventinds]
                            names(dimlist)[dimi] <- names(pdois[[pgi]]$vars[[vi]]$dims)[dimi]
                        } # for dimi
                        outputname <- names(pdois[[pgi]]$vars)[vi]
                        #stop("asd")
                        #pangaea <- list()
                        #if (is.null(pangaea[[outputname]][[names(pdois)[pgi]]])) { # if doi-entry not already there
                        #    pangaea[[outputname]][[names(pdois)[pgi]]] <- list()
                        #}
                        # if event-entry not already there
                        if (is.null(pangaea[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]])) { 
                            pangaea[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]] <- 
                                list(doi=pdois[[pgi]]$pdoi, loc=df$loc[eventi], 
                                     lon=df$lon[eventi], lat=df$lat[eventi])
                        } 
                        # save variable
                        pangaea[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]]$data[[datai]] <- 
                            pg[[dataind]]$data[[pdois[[pgi]]$vars[[vi]]$inputname]][eventinds]
                        pangaea[[outputname]][[names(pdois)[pgi]]][[df$event[eventi]]]$dims[[datai]] <- 
                            dimlist
                    } # if current variable of current event of current doi is wanted
                } # for datai all datas of current event of current doi
            } # for eventi in df
        } # for vi in wanted vars
    } # for pgi in pdois
} # if length(pdois) > 0

