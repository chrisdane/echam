# load and/or define helper functions for echam repo

# host options
get_host <- function(verbose=F) {
    message("******* get_host() *******")
    hostname <- Sys.info()["nodename"] # = system("hostname", intern=T)
    if (any(sapply(c("ollie", "prod-", "fat-"), grepl, hostname))) { # ollie
        machine_tag <- "ollie"
        homepath <- "~/scripts/r"
        workpath <- paste0("/work/ollie/", Sys.info()["user"])
    } else if (any(sapply(c("mlogin", 
                            "mistralpp", 
                            "m[0-9][0-9][0-9][0-9][0-9]"), # mistral; compute nodes: m12345 
                          grepl, hostname))) {
        machine_tag <- "mistral"
        homepath <- "~/scripts/r"
        if (F) {
            workpath <- paste0("/work/ba0941/", Sys.info()["user"])
        } else if (T) {
            workpath <- paste0("/work/ab0246/", Sys.info()["user"])
        }
    } else if (any(sapply(c("levante"), grepl, hostname))) { # levante
        machine_tag <- "levante"
        homepath <- "~/scripts/r"
        if (T) {
            workpath <- paste0("/work/ba1103/", Sys.info()["user"])
        } else if (F) {
            workpath <- paste0("/work/ab1095/", Sys.info()["user"])
        } else if (F) {
            workpath <- paste0("/work/ab0246/", Sys.info()["user"])
        }
    } else if (any(sapply(c("paleosrv1", "fu-"), grepl, hostname))) { # paleosrv
        machine_tag <- "paleosrv"
        homepath <- "~/scripts/r"
        workpath <- paste0("/isibhv/projects/paleo_work/", Sys.info()["user"])
    } else if (any(sapply("stan", grepl, hostname))) { # stan
        machine_tag <- "stan"
        homepath <- "~/scripts/r"
        workpath <- paste0("/ace/user/", Sys.info()["user"])
    } else { # defaults if unknown machine 
        machine_tag <- "unknown"
        homepath <- "~/scripts/r"
        workpath <- "~/data"
        message("hostname \"", hostname, "\" unknown; Sys.info():")
        print(Sys.info())
    }
    if (verbose) {
        message("hostname    = \"", hostname, "\"\n",
                "machine_tag = \"", machine_tag, "\"\n",
                "homepath    = \"", homepath, "\"\n",
                "workpath    = \"", workpath, "\"\n",
                "******* get_host() ******")
    }
    return(host=list(hostname=hostname, 
                     hostname_f=system("hostname -f", intern=T), 
                     machine_tag=machine_tag, 
                     homepath=homepath, 
                     workpath=workpath))
} # get_host()

