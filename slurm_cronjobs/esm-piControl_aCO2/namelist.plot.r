# this script is used for task 2/2 of `slurm_cronjob.sh`

repopath <- normalizePath("~/scripts/r/echam") # machine dependent

# load namelist.plot.r defaults from repopath
nml_plot <- paste0(repopath, "/namelist.plot.r")
message("load general part of \"", nml_plot, "\" ...")
namelist.plot.r <- scan(nml_plot, what="char", sep="\n", blank.lines.skip=F, quiet=T)
ind <- grep("<-- namelist.plot.r general part end -->", namelist.plot.r)
if (length(ind) == 0) {
    stop("could not find \"<-- namelist.plot.r general part end -->\" entry in ", nml_plot)
}
source(textConnection(namelist.plot.r[seq_len(ind)]))

# load slurm cronjob specific awi-esm-1-1-lr_kh800 esm-piControl co2fsign
models <- "fesom"
prefixes <- "awi-esm-1-1-lr_kh800_esm-piControl_co2fsign"
names_short <- "esm_piControl_co2fsign"
names_legend <- "esm-piControl"
varnames_in <- "aCO2"
add_linear_trend <- F
fromsf <- 2686
# rhs of next line will be added by step 1; dont do it by hand
tosf <- 2939
new_origins <- 1
modes <- "select"

