# r

# executed from plot_echam.r

for (i in seq_along(regboxes)) {

    if (!is.na(regboxes[[i]]$regbox)) {
        
        # add further areas here
        if (regboxes[[i]]$regbox == "northeast_europe") {
            regboxes[[i]]$lons <- c(-120, 200)
            regboxes[[i]]$lats <- c(15, 90)
        } else if (regboxes[[i]]$regbox == "N30-90") {
            regboxes[[i]]$lons <- c(-180, 180)
            regboxes[[i]]$lats <- c(30, 90)
        } else if (regboxes[[i]]$regbox == "NAsiberia") {
            regboxes[[i]]$lons <- c(-65, 180)
            regboxes[[i]]$lats <- c(30, 90)
        }

    } # if !is.na(regboxes[[i]]$regbox)

} # for i seq_along(regboxes)

