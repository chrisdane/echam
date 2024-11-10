# r

# calc siarea from post_echam result of siconc

rm(list=ls()); graphics.off()
options(warn=2) # stop on warning

force <- F
cdo <- Sys.which("cdo")

if (T) {
    postpath <- "/work/ba1103/a270073/post"
    cmd <- paste0("find ", postpath, "/ -name \"*_select_siconc_NH_66_Sep_*.nc\" | sort")
}

#########################################################################

message("run `", cmd, "` ...")
fs <- system(cmd, intern=T)
if (length(fs) == 0) stop("found zero files")
message("found ", length(fs), " files:")
print(fs)

errinds <- rep(F, t=length(fs))
for (fi in seq_along(fs)) {

    message("********************************************************************************************************\n",
            "file ", fi, "/", length(fs), ": ", fs[fi])
    
    fout <- fs[fi]
    fout <- sub("/select/siconc/", "/fldsum/siarea/", fout)
    fout <- sub("_select_siconc_", "_fldsum_siarea_", fout)
    if (file.exists(fout) && !force) {
        message("fout ", fout, " already exists and `force` is false. skip")
    } else {

        dir.create(dirname(fout), recursive=T, showWarnings=F)

        # get gridarea
        fgridarea <- paste0("/tmp/gridarea_", basename(fs[fi]), "_", Sys.getpid(), ".nc")
        cmd <- paste0(cdo, " gridarea ", fs[fi], " ", fgridarea)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) {
            message("error on getting gridarea, skip.")
            errinds[fi] <- T
        
        } else {
            # calc siarea
            cmd <- paste0(cdo, " -setunit,m2 -setname,siarea -divc,100 -fldsum -mul ", fgridarea, " ", fs[fi], " ", fout) # cmip6:siconc is in % --> /100
            message("run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("error")
            invisible(file.remove(fgridarea))
        
        } # if gridarea successful

    } # if fout already exists

} # for fi

if (any(errinds)) {
    errinds <- which(errinds)
    message("\n`cdo gridarea` failed for ", length(errinds), " files:")
    print(fs[errinds])
}


