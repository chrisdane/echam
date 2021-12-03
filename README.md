# echam

# install

```
git clone --recurse-submodules https://github.com/chrisdane/echam
```

# run

Load R if necessary:
```bash
module load r # or R
```
Running 
```R
source("example_namelist.post.r")
```
in an active R session or
```bash
nohup Rscript example_namelist.post.r > example.log 2>&1 &
```
in background with
```bash
cat <<EOF >> example_namelist.post.r
# load defaults
repopath <- getwd() # change here if necessary
repopath <- normalizePath(repopath, mustWork=T) # error if not found
source(paste0(repopath, "/namelist.general.post.r"))

# my setting
models <- "CanESM5" # ocean: nemo3.4.1: native: orca1 tripolar; cdo: curvilinear; fldint ok
datapaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/CCCma/CanESM5/piControl/r1i1p1f1/Omon/fgco2/gn/v20190429"
fpatterns <- "fgco2_Omon_CanESM5_piControl_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
fvarnames <- "fgco2"
prefixes <- "CanESM5_piControl_r1i1p1f1"
modes <- "fldint"
froms <- 6150
tos <- 6200

# run
source(paste0(repopath, "/post_echam.r"))
EOF
```
will calculate fldint from years 6150 to 6200 of variable fgco2 of all files found with pattern `fpatterns` in directory `datapaths` and save it as nc file named "`prefixes`....nc".

# notes

Allowed special patterns in `fpatterns` are printed in log. `modes` can be anything from `cdo`. The mode "fldint" works as soon as `cdo gridarea` does not return an error.

Adding 
```
cdo_after_calcs <- list(c("setunit,\"PgC yr-1\"", "mulc,365.25", "mulc,86400", "divc,1e12")) # kgC s-1 --> PgC yr-1
```
to the namelist adds the given cdo commands to the calculation.

