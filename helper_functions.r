# load and/or define helper functions for echam repo

# load myfunctions
source("~/scripts/r/functions/myfunctions.r")

# host options
get_host <- function() {
    message("******* get_host() *******")
    hostname <- system("hostname", intern=T)
    if (any(sapply(c("ollie", "prod-", "fat-"), grepl, hostname))) {
        machine_tag <- "ollie"
        homepath <- "~/scripts/r"
        workpath <- "/work/ollie/cdanek"
    } else if (any(sapply(c("mlogin", "mistralpp"), grepl, hostname))) {
        machine_tag <- "mistral"
        homepath <- "~/scripts/r"
        #workpath <- "/work/ba0941/a270073"
        workpath <- "/work/ab0246/a270073"
    } else if (any(sapply(c("paleosrv1", "fu-"), grepl, hostname))) {
        machine_tag <- "paleosrv"
        homepath <- "~/scripts/r"
        workpath <- "/isibhv/projects/paleo_work/cdanek"
    } else if (any(sapply("stan", grepl, hostname))) {
        machine_tag <- "stan"
        homepath <- "~/scripts/r"
        workpath <- "/ace/user/cdanek"
    } else {
        machine_tag <- "unknown"
        homepath <- "~/scripts/r"
        workpath <- homepath
    }
    message("hostname    = \"", hostname, "\"\n",
            "machine_tag = \"", machine_tag, "\"\n",
            "homepath    = \"", homepath, "\"\n",
            "workpath    = \"", workpath, "\"\n",
            "******* get_host() ******")
    return(host=list(hostname=hostname, hostname_f=system("hostname -f", intern=T), 
                     machine_tag=machine_tag, homepath=homepath, workpath=workpath))
} # get_host()

