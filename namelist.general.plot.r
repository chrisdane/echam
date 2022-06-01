# r

# input for plot_echam.r
message("###################### namelist.general.plot.r start ##########################")

# clear work space
if (T) {
    message("clear work space ...")
    ws <- ls()
    ws <- ws[-which(ws == "repopath")]
    rm(list=ws)
}

# load helper functions of this repo
script_helper_functions <- paste0(repopath, "/helper_functions.r")
message("load `repopath`/helper_functions.r = ", script_helper_functions, " ...")
source(script_helper_functions) # get_host()

# get host options
host <- get_host()
host$repopath <- repopath

# todo: how to load functions from another repo without the subrepo hassle?
if (file.exists(paste0(host$homepath, "/functions/myfunctions.r"))) {
    message("load `host$homepath`/functions/myfunctions.r = ", host$homepath, "/functions/myfunctions.r ...")
    source(paste0(host$homepath, "/functions/myfunctions.r"))
    # dependencies:
    # setDefaultPlotOptions(), reorder_legend(), mycols(), get_pval(), make_posixlt_origin()
} else {
    stop("could not load ", host$homepath, "/functions/myfunctions.r")
}

# known dimnames
known_dimnames <- list(time=c("time", "Time",  "TIME", "time_mon"),
                       lon=c("lon", "lons", "longitude", "Longitude", "LONGITUDE", "nxi"),
                       lat=c("lat", "lats", "latitude", "Latitude", "LATITUDE", "nyi"),
                       depth=c("depth_2"))

# ignore dimnames
ignore_dimnames <- c("bnds")

# known varnames; use cmor names
known_vars <- list(fgco2=c("fgco2", "co2_flx_ocean"))

# ignore variables
ignore_vars <- c("bnds",
                 "time_bnds", "time_mon",
                 "hyai", "hybi", "hyam", "hybm",
                 "plev", "height", 
                 "depth", "depthvec", 
                 "lm_*_as_time_std_error", "lm_*_as_time_t_val", "lm_*_as_time_p_val",
                 "timestamp", "timevec", "timechar", "xi", "yi", # old rfesom 
                 "moc_reg_lat")


# general script options
squeeze <- T # drop dims with length=1 (e.g. lon and lat after fldmean)
nchar_max_foutname <- 255 - 4 # -4 for extension ".png" or ".pdf"
load_pangaea_data <- F
load_special_data <- T
plot_special_data <- F

# calc options
calc_monthly_and_annual_climatology <- T
calc_ttest_lon_lat_time <- T

# stats options
ttest_alternative <- "two.sided" # differences in means
ttest_significance <- 0.05 # p-value (*100 for %)

# plot device options
png_family <- NULL
if (host$machine_tag == "mistral") {
    png_family <- "Nimbus Sans L" # mistral R36 default png font Helvetica broken
}
p <- myDefaultPlotOptions(#plot_type="png"
                          #plot_type="active"
                          ,png_family=png_family
                          #,png_family="Droid Sans Mono" 
                          #,png_family="Helvetica" 
                          #,pdf_family="CM Roman"
                          #,ts_asp=0.75
                          #,verbose=T
                          )
plus_minus_symbol <- "\u00b1"

# general plot options
bilinear_interp_factor <- 1 # only effect if > 1
plot_samedims <- T
add_title <- F
add_legend <- T
legend_pos <- legend_pos_ts <- legend_pos_mon <- legend_pos_an <- legend_pos_ltm <- NULL

# my wanted pch order: 1: circle, 2: triangle up, 3: square, 4: diamond
# --> bring hollow, filled wout borders and filled with borders symbols in same order
pchs_hollow <- c(1, 2, 0, 5) # circle, triangle up, square, diamond
pchs_filled_wout_border <- c(16, 17, 15, 18) # circle, triangle up, square, diamond
pchs_filled_w_border <- c(21, 24, 22, 23) # circle, triangle up, square, diamond

# encoding <- getOption("encoding") leads to "failed to load encoding file 'native.enc'"
encoding <- NULL 
alpha_rgb <- 0.2 # transparent: 0,1 (0 fully transparent)

known_seasons <- list("Jan"=list(inds=1),
                      "Feb"=list(inds=2),
                      "Mar"=list(inds=3),
                      "Apr"=list(inds=4),
                      "May"=list(inds=5),
                      "Jun"=list(inds=6),
                      "Jul"=list(inds=7),
                      "Aug"=list(inds=8),
                      "Sep"=list(inds=9),
                      "Oct"=list(inds=10),
                      "Nov"=list(inds=11),
                      "Dec"=list(inds=12),
                      "DJF"=list(inds=c(12, 1, 2), col="blue"),
                      "MAM"=list(inds=3:5, col="darkgreen"),
                      "JJA"=list(inds=6:8, col="red"),
                      "SON"=list(inds=9:11, col="brown"),
                      "NDJFM"=list(inds=c(11:12, 1:3)),
                      "Jan-Dec"=list(inds=1:12))
# defaultseascols: "blue", "darkgreen", "red", "brown"
# myseascols: blue: "#377EB8", green: "#1B9E77", red: "#E41A1C", brown: "#D95F02"
for (i in seq_along(known_seasons)) {
    if (names(known_seasons)[i] != "Jan-Dec") {
        known_seasons[[paste0(names(known_seasons)[i], "mean")]] <- known_seasons[[i]]
    } else if (names(known_seasons)[i] == "Jan-Dec") {
        known_seasons$annual <- known_seasons[["Jan-Dec"]]
    }
    if (is.null(known_seasons[[i]]$col)) known_seasons[[i]]$col <- "black"
    known_seasons[[i]]$col_rgb <- rgb(t(col2rgb(known_seasons[[i]]$col)/255), alpha=alpha_rgb)
}

## time series plot options
# woa13 seasons: "JFM" "AMJ" "JAS" "OND"
# other seasons: "Jan-Dec" "DJF" "MAM" "JJA" "SON" "JJ"
# months: "Feb" "Jul" "Jan"  "Jul"
add_xgrid <- F
add_ygrid <- F
add_zeroline <- T
add_unsmoothed <- F
add_smoothed <- T
add_sd <- F
add_linear_trend <- F
add_nonlinear_trend <- F
add_1to1_line <- T
add_scatter_density <- F
center_ts <- scale_ts <- detrend_ts <- diff_ts <- F
ts_highlight_seasons <- list(#bool=T,
                             bool=F,
                             seasons=c("DJF", "MAM", "JJA", "SON"),
                             #t="l",
                             t="p",
                             #cols=c("blue", "darkgreen", "red", "brown")
                             cols=rgb(t(col2rgb(c("blue", "darkgreen", "red", "brown"))/255), alpha=alpha_rgb),
                             ltys=c(1,2,3,4),
                             lwds=c(1,1,1,1),
                             #pchs=1:4,
                             pchs=c(16, 16, 16, 16),
                             suffix="_highlight_seasons") 
show_first_data_point <- F
ts_plot_each_setting_in_subplot <- F
add_data_left_yaxis_before_ts <- T
add_data_right_yaxis_ts <- F
add_cor_data_left_and_right_ts <- F
add_data_upper_xaxis_ts <- F
add_data_right_yaxis_ts_mon <- F
add_data_right_yaxis_ts_an <- T
add_cor_data_left_and_right_ts_an <- F
add_legend_right_yaxis <- T
add_legend_upper_xaxis <- F
add_legend_left_yaxis_before <- T

plot_scatter_s1_vs_s2 <- F
#scatter_s1_vs_s1_varname <- "temp2"
scatter_s1_vs_s1_varname <- "tsurf"
#scatter_s1_vs_s1_varname <- "aprt"
#scatter_s1_vs_s1_varname <- "wisoaprt_d"

plot_scatter_v1_vs_v2 <- T # uses `datas`
varnamex <- varnamey <- "abc_datas" # default
#varnamex <- "temp2_datas" # temp2/tas vs toa_imbalance: TOA imbalance gregory et al. 2004 stuff 
varnamex <- "tas_datas"
#varnamex <- "tsurf_datas"
#varnamex <- "tsurfaprt_datas"
#varnamex <- "quv_datas"
#varnamex <- "quv_direction_datasan"
#varnamex <- "lm_temp2_as_time_slope_datas"
#varnamex <- "aprt_datas"
varnamey <- "toa_imbalance_datas"
#varnamey <- "quv_direction_datas"
#varnamey <- "wisoaprt_d_datas"
#varnamey <- "lm_wisoaprt_d_post_as_time_slope_datas"

## time vs depth:
add_ts_to_time_vs_depth <- T

## map (lon vs lat) plot options
plot_lon_lat_anomaly <- T
reorder_lon_from_0360_to_180180 <- T
addland <- T
add_grid <- F
aspect_ratio_thr <- 4/3 # 2 # maximum dlon/dlat ratio for map plot
proj <- "" # default: no projection
echam6_global_setNA <- NA # one of 3 options: NA, "ocean", "land"

# plot red-noise spectra
plot_redfit <- F
plot_redfit_pcnt <- F
plot_redfit_pcnt <- T 

# constants
Aearth <- 5.100656e14 # m2

# clear work space if non-clean restart of plot_echam.r, i.e. without rm of everything
objs <- c("postpaths", "plotpath", 
          "names_legend", 
          "codes", 
          "seasonsf", "fromsp", "tosp", "seasonsp", "n_mas", "new_origins", "time_ref",
          "remove_setting", "remove_mean_froms", "remove_mean_tos",
          "levs", "levsf", 
          "depths", "depthsf", "depth_fromsf", "depth_tosf", "depth_fromsp", "depth_tosp",
          "areas", "regboxes",
          "reg_dxs", "reg_dys", 
          "types", "ltys", "cols", "lwds", "pchs", "text_cols", "cols_samedims")
suppressWarnings(rm(list=objs))

message("###################### namelist.general.plot.r finish ##########################")

