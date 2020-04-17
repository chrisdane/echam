# run plot_echam.r in loop with input defined by namelist.loop.plot.r

rm(list=ls()); graphics.off()

# load namelist.loop.plot.r first time for check and to get number of runs
message("\nload and check namelist.loop.plot.r ...")
source("namelist.loop.plot.r")

# check all objects with more than 1 dims if user made something stupid
all_loop_objs <- ls() # all objects defined in namelist.loop.plot.r
array_loop_objs <- sapply(all_loop_objs, function(x) eval(parse(text=paste0("dim(", x, ")"))))
if (any(sapply(array_loop_objs, is.null))) {
    nainds <- which(sapply(array_loop_objs, is.null))
    array_loop_objs[nainds] <- NULL
}
if (length(unique(sapply(array_loop_objs, length))) != 1) {
    stop("array objects from namelist.loop.plot.r are not all of same dim")
}
if (unique(sapply(array_loop_objs, length)) != 2) {
    stop("array objects from namelist.loop.plot.r must have 2 dims")
}

# determine number of runs
if (!(any(all_loop_objs == "nruns"))) {
    nruns <- array_loop_objs[[1]][1]
    message("`nruns` not given by namelist.loop.plot.r. use first dim of first obj: ", nruns)
} else {
    message("`nruns` provided by namelist.loop.plot.r: ", nruns)
}

## generate and run a new "plot_echam.r" `nruns` times
# in new plot_echam.r the "source("namelist.plot.r")" part is replaced by 
# 1) reading only the general part of namelist.plot.r and
# 2) the i-th entry of the objects loaded by namelist.loop.plot.r above
message("\ngenerate and run plot_echam.r ", nruns, " times ...")
for (runi in seq_len(nruns)) {
    
    message("\nrun ", runi, "/", nruns)
    
    # read template (default) plot_echam.r
    plot_echam.r <- scan("plot_echam.r", what="char", sep="\n", blank.lines.skip=F, quiet=T)
    source_ind <- which(grepl("source(\"namelist.plot.r\")", plot_echam.r, fixed=T))
    if (length(source_ind) == 0) stop("could not find \"source(\"namelist.plot.r\")\" entry in plot_echam.r")
    
    # part 1/2 of new plot_echam.r: general part of namelist.plot.r between
    namelist.plot.r <- scan("namelist.plot.r", what="char", sep="\n", blank.lines.skip=F, quiet=T)
    general_namelist.plot.r_inds <- sapply(c("<-- namelist.plot.r general part start -->",
                                             "<-- namelist.plot.r general part end -->"), 
                                           grepl, namelist.plot.r)
    general_namelist.plot.r_inds <- apply(general_namelist.plot.r_inds, 2, which)
    if (is.list(general_namelist.plot.r_inds)) {
        stop("could not find \"<-- namelist.plot.r general part start -->\" and ",
             "\"<-- namelist.plot.r general part end -->\" entries in namelist.plot.r")
    }

    # part 2/2 of new plot_echam.r: add all objects defined by namelist.loop.plot.r (i-th entries of array-objects)
    namelist.loop.plot.r <- rep(NA, t=length(all_loop_objs))
    for (obji in seq_along(all_loop_objs)) {
        obj <- eval(parse(text=all_loop_objs[obji]))
        if (is.vector(obj)) {
            if (!all(is.na(obj))) {
                namelist.loop.plot.r[obji] <- paste0(all_loop_objs[obji], " <- c(")
            }
        } else { # if array
            if (!all(is.na(obj[runi,]))) {    
                namelist.loop.plot.r[obji] <- paste0(all_loop_objs[obji], " <- c(")
            }
        }
        # rhs if not NA
        if (!is.na(namelist.loop.plot.r[obji])) {
            ischar <- is.character(obj)
            if (!is.vector(obj)) { # take i-th entry if array
                namelist.loop.plot.r[obji] <- paste0(namelist.loop.plot.r[obji],
                                                     ifelse(ischar, "\"", ""),
                                                     paste(obj[runi,], collapse=ifelse(ischar, "\", \"", ", "))) 
            } else { # or all entries if vector
                namelist.loop.plot.r[obji] <- paste0(namelist.loop.plot.r[obji], 
                                                     ifelse(ischar, "\"", ""),
                                                     paste(obj, collapse=ifelse(ischar, "\", \"", ", ")))
            }
            namelist.loop.plot.r[obji] <- paste0(namelist.loop.plot.r[obji],
                                                 ifelse(ischar, "\"", ""), ")")
        }
    }
    if (any(is.na(namelist.loop.plot.r))) {
        namelist.loop.plot.r <- namelist.loop.plot.r[-which(is.na(namelist.loop.plot.r))]
    }

    # cat part 1 and part 2
    new_lines <- c(paste0("message(\"\\nread namelist.plot.r[", general_namelist.plot.r_inds[1], ":", 
                   general_namelist.plot.r_inds[2], "] only for general options ...\")"),
                   paste0("source(textConnection(readLines(\"namelist.plot.r\")[", general_namelist.plot.r_inds[1], ":", 
                          general_namelist.plot.r_inds[2], "]))"),
                   "", "# put values from namelist.loop.plot.r", namelist.loop.plot.r)

    # overwrite settings from namelist.plot.r with settings from namelist.loop.plot.r
    plot_echam.r <- c(plot_echam.r[1:(source_ind-2)], 
                      new_lines,
                      plot_echam.r[(source_ind+1):length(plot_echam.r)])
    
    # save temporary plot_echam.r and run
    if (runi == 1) dir.create("tmp", recursive=T, showWarnings=F)
    plot_echam.r_fname <- paste0("tmp/plot_echam_", 
                                 varnames_in[runi,1], "_", seasonsp[runi,1], "_", areas[runi,1], ".r")
    message("save ", plot_echam.r_fname)
    write(plot_echam.r, file=plot_echam.r_fname)
    cmd <- paste0("nohup Rscript ", plot_echam.r_fname, " > ", plot_echam.r_fname, ".log 2>&1 &")
    message("run `", cmd, "`")
    system(cmd)

} # for i nruns

