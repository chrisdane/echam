# this script is used for task 1/2 of `slurm_cronjob.sh`

repopath <- normalizePath("~/scripts/r/echam") # machine dependent

# load namelist.post.r defaults from repopath
nml_post <- paste0(repopath, "/namelist.post.r")
message("load general part of \"", nml_post, "\" ...")
namelist.post.r <- scan(nml_post, what="char", sep="\n", blank.lines.skip=F, quiet=T)
ind <- grep("<-- namelist.post.r general part end -->", namelist.post.r)
if (length(ind) == 0) {
    stop("could not find \"<-- namelist.post.r general part end -->\" entry in ", nml_post)
}
source(textConnection(namelist.post.r[seq_len(ind)]))

# load slurm cronjob specific awi-esm-1-1-lr_kh800 esm-piControl
models <- "fesom"
datapaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_co2fsign/outdata/fesom"
fpatterns <- "aCO2_fesom_<YYYY>0101.nc"
prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_co2fsign"
fvarnames <- "aCO2"
cdoshifttimes <- "-1dt" # for fesom
modes <- "select"
froms <- 2686

# get most recent outdata years
files <- list.files(datapaths, pattern=glob2rx(sub("<YYYY>", "????", fpatterns))) # "aCO2_fesom_26860101.nc"
if (length(files) == 0) stop("found zero files")
tos <- as.integer(substr(files, 12, 15))
tos <- max(tos)

# replace "tosf <-<anything until end of line>" with "tosf <- YYYY" in namelist.plot for task 2
cmd <- paste0("sed -i \"s/tosf <-.*/tosf <- ", tos, "/g\" namelist.plot.r")
message("\nnamelist.post.r: run `", cmd, "` ...\n")
system(cmd)

