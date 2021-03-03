# input for plot.echam.r

# dependencies: myfunctions.r

## general part
# <-- namelist.plot.r general part start --> # keep this line for plot_loop_echam.r

# ignore these netcdf variables
ignore_vars <- c("time_bnds", "timestamp", 
                 "hyai", "hybi", "hyam", "hybm",
                 "plev", "height", 
                 "depthvec", 
                 "moc_reg_lat")
message("\nthese variables will be ignored:\n",
		"\"", paste(ignore_vars, collapse="\", \""), "\"")

# general script options
squeeze <- T # drop dims with length=1 (e.g. lon and lat after fldmean)
nchar_max_foutname <- 255

# stats options
ttest_alternative <- "two.sided" # differences in means
ttest_significance <- 0.05 # % p-value

# calc options
calc_monhtly_and_annual_climatology <- F
calc_ttest_lon_lat_time <- T

# plot options
pchs_hollow <- c(1, 2, 0, 5) # bring hollow, filled wout borders and filled with borders in same order
pchs_filled_wout_border <- c(16, 17, 15, 18) # 1: circle, 2: triangle up, 3: square, 4: diamond
pchs_filled_w_border <- c(21, 24, 22, 23)
add_title <- F
add_legend <- T
message("\nrun myfunctions.r:myDefaultPlotOptions() ...")
p <- myDefaultPlotOptions(plot_type="png", 
                          #plot_type="pdf"
                          #,family_png="Droid Sans Mono", 
                          #,family_pdf="Droid Sans Mono"
                          #,family_pdf="CM Roman"
                          , verbose=T)
# encoding <- getOption("encoding") leads to "failed to load encoding file 'native.enc'"
encoding <- NULL 
alpha_rgb <- 0.2 # transparent: 0,1 (0 fully transparent)

known_seasons <- list("DJF"=list(inds=c(12, 1, 2), col="blue"),
                      "MAM"=list(inds=3:5, col="darkgreen"),
                      "JJA"=list(inds=6:8, col="red"),
                      "SON"=list(inds=9:11, col="brown"),
                      "NDJFM"=list(inds=c(11:12, 1:3), col="black"),
                      "Jan-Dec"=list(inds=1:12, col="black"))
# defaultseascols: "blue", "darkgreen", "red", "brown"
# myseascols: blue: "#377EB8", green: "#1B9E77", red: "#E41A1C", brown: "#D95F02"
for (i in seq_along(known_seasons)) {
    if (names(known_seasons)[i] != "Jan-Dec") {
        known_seasons[[paste0(names(known_seasons)[i], "mean")]] <- known_seasons[[i]]
    } else if (names(known_seasons)[i] == "Jan-Dec") {
        known_seasons$annual <- known_seasons[["Jan-Dec"]]
    }
    known_seasons[[i]]$col_rgb <- rgb(t(col2rgb(known_seasons[[i]]$col)/255), alpha=alpha_rgb)
}

# time series plot options
# woa13 seasons: "JFM" "AMJ" "JAS" "OND"
# other seasons: "Jan-Dec" "DJF" "MAM" "JJA" "SON" "JJ"
# months: "Feb" "Jul" "Jan"  "Jul"
add_xgrid <- F
add_ygrid <- F
add_zeroline <- T
add_unsmoothed <- F
add_smoothed <- T
add_sd <- F
add_linear_trend <- T
add_nonlinear_trend <- F
add_1to1_line <- T
add_scatter_density <- F
center_ts <- F # either center_ts or scale_ts or none but not both
scale_ts <- F
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
add_data_right_yaxis_ts <- T
add_cor_data_left_and_right_ts <- F
add_data_upper_xaxis_ts <- F
add_data_right_yaxis_ts_mon <- F
add_data_right_yaxis_ts_an <- F
add_cor_data_left_and_right_ts_an <- F
add_legend_right_yaxis <- T
add_legend_upper_xaxis <- F

plot_scatter_s1_vs_s2 <- F
#scatter_s1_vs_s1_varname <- "temp2"
scatter_s1_vs_s1_varname <- "tsurf"
#scatter_s1_vs_s1_varname <- "aprt"
#scatter_s1_vs_s1_varname <- "wisoaprt_d"

plot_scatter_v1_vs_v2 <- T # uses `datas`
varnamex <- varnamey <- "abc_datas" # default
#varnamex <- "temp2_datas" # temp2 vs toa_imbalance: TOA imbalance gregory et al. 2004 stuff 
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

# time vs depth:
add_ts_to_time_vs_depth <- T

# special
plot_redfit <- F

# map (lon vs lat) plot options
proj <- "rectangular" #"rectangular"
addland <- T
reorder_lon_from_0360_to_180180 <- T
add_grid <- F
respect_asp <- T
aspect_ratio_thr <- 2 # maximum dlon/dlat ratio for plot

# clear work space if non-clean restart of plot_echam.r (i.e. without rm of everything)
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

# <-- namelist.plot.r general part end --> # keep this line for plot_loop_echam.r

## setting specific part

# 1 setting
if (F) { # awi-esm-1-1-lr hist
    #prefixes <- "historical_echam6_echammon_awi-esm-1-1-lr"
    prefixes <- "historical_echam6_echammon_yearmean_awi-esm-1-1-lr"
    models <- "echam6"
    names_short <- "historical_wrt_1961_90"
    names_legend <- "historical"
    fromsf <- 1850
    tosf <- 2014
    varnames_in <- "temp2"
    modes <- "fldmean"
    n_mas <- 1
    cols <- "#E41A1C"
    remove_mean_froms <- 1961
    remove_mean_tos <- 1990

} else if (F) { # Hol-Tx10 on paleosrv or Hol-T on stan
    models <- "echam5"
    #models <- "mpiom1"
    #models <- "jsbach"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_main_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_main_mm_plev"
    #prefixes <- "cosmos-aso-wiso_Hol-Tx10_wiso_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-T"
    prefixes <- "cosmos-aso-wiso_Hol-T_main_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-T_wiso_mm"
    #prefixes <- "Hol-T_stschuett_echam5_wiso" # steffens data
    #prefixes <- "cosmos-aso-wiso_Hol-T_grb_code_2_remapcon2_r120x101"
    #prefixes <- "cosmos-aso-wiso_Hol-T_grb_code_15_remapcon2_r120x101_gt_0.15_times_area"
    #prefixes <- "cosmos-aso-wiso_Hol-T_veg_mm"
    #prefixes <- "cosmos-aso-wiso_Hol-T_fort_75_monmean"
    #names_short <- "Hol-Tx10"
    names_short <- "Hol-T"
    #names_short <- "Hol-T_st"
    #names_legend <- names_short
    #names_legend <- "Hol-Tx10 DJF"
    #names_legend <- "Hol-Tx10 MAM"
    #names_legend <- "Hol-Tx10 JJA"
    #names_legend <- "Hol-Tx10 SON"
    names_legend <- ""
    #names_legend <- "Annual"
    #names_legend <- "global"
    #names_legend <- "60-90N"
    #names_legend <- "Hol-Tx10 Ladoga"
    #names_legend <- "Hol-T Ladoga"
    #names_legend <- "cosmos-aso-wiso Ladoga"
    #names_legend <- "COSMOS transient"
    #names_legend <- "transient"
    #names_legend <- "Hol-Tx10 Shuchye"
    #names_legend <- "Hol-T Shuchye"
    #names_legend <- "Hol-Tx10 Levinson-Lessing"
    #names_legend <- "Hol-T Levinson-Lessing"
    #names_legend <- "Hol-Tx10 Taymyr"
    #names_legend <- "Hol-T Taymyr"
    #names_legend <- "Hol-Tx10 Emanda"
    #names_legend <- "Hol-T Emanda"
    #names_legend <- "Hol-Tx10 Elgygytgyn"
    #names_legend <- "Hol-T Elgygytgyn"
    #fromsf <- "0001" # Hol-Tx10
    fromsf <- "0004" # Hol-T; beginning of chunk 1
    #fromsf <- "0100"
    #fromsf <- "6971" # last 30 years of Hol-T
    #tosf <- "0129"
    #tosf <- "5903" # Hol-T; end of chunk 2
    #tosf <- "6173" # 
    #tosf <- "6821" # 
    tosf <- "7000" # Hol-T; end of chunk 3
    #tosf <- "7001" # Hol-Tx10
    #new_origins <- -7000 # Hol-Tx10
    #new_origins <- -5050 # Hol-Tx10 plus 1950
    new_origins <- -6996 # Hol-T; model year 1 = 6999 BP -> model year 4 = 6999 BP - 3 = 6996 BP
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- 1
    #n_mas <- 30
    #n_mas <- 5*12
    #n_mas <- 100
    #n_mas <- 150
    #n_mas <- 20/3*12
    #n_mas <- 10*12
    #n_mas <- 20*12
    #n_mas <- 3*90
    #n_mas <- 30*12
    #n_mas <- 100*12
    #remove_mean_froms <- -827
    #remove_mean_tos <- -827
    #remove_mean_froms <- -6996
    #remove_mean_tos <- -6996
    #remove_mean_froms <- -7000
    #remove_mean_tos <- -7000
    #seasonsf <- "annual"
    #seasonsf <- "Jun"
    #seasonsf <- "Jun"
    #seasonsf <- "Dec"
    #seasonsf <- "yearsum"
    seasonsf <- "NDJFM"
    #seasonsp <- "Feb" # cdo's default season timestamps: DJF->Feb, MAM->May, JJA->Aug, SON->Nov
    #seasonsp <- "Mar"
    #seasonsp <- "May"
    #seasonsp <- "Jun"
    #seasonsp <- "Jul"
    #seasonsp <- "Aug"
    #seasonsp <- "Sep"
    #seasonsp <- "Nov"
    #seasonsp <- "Dec"
    #seasonsp <- "DJF"
    #seasonsp <- "MAM"
    #seasonsp <- "JJA"
    #seasonsp <- "SON"
    seasonsp <- "NDJFMmean"
    #varnames_in <- "temp2"
    #varnames_in <- "tsurf"
    #varnames_in <- "tslm1"
    varnames_in <- "psl"
    #varnames_in <- "aprt"
    #varnames_in <- "aprs"
    #varnames_in <- "wisoaprt_d"
    #varnames_in <- "wisoaprt_d_post"
    #levs <- 2
    #varnames_in <- "temp2aprt"
    #varnames_in <- "tsurfaprt"
    #varnames_in <- "ptemp"
    #varnames_in <- "srad0"
    #varnames_in <- "srad0d"
    #varnames_in <- "lm_temp2_as_time_slope"
    #varnames_in <- "lm_tsurf_as_time_slope"
    #varnames_in <- "lm_aprt_as_time_slope"
    #varnames_in <- "lm_wisoaprt_d_sellevel_2_as_temp2_slope"
    #varnames_in <- "lm_wisoaprt_d_sellevel_2_as_ptemp_slope"
    #varnames_in <- "lm_wisoaprt_d_post_as_time_slope"
    #levs <- 2
    #varnames_in <- "quv"
    #varnames_in <- "quv_direction"
    #levsf <- "_int1000-100hPa"
    #varnames_out_samedims <- "quv"
    #names_legend_samedims <- c("qu", "qv", "quv")
    #varnames_uv <- list(quv=c(u="qu", v="qv")) # for quiver
    #varnames_in <- "THO"
    #levs <- 6
    #varnames_in <- "SICOMO"
    #varnames_in <- "act_fpc"
    #varnames_in <- "lm_act_fpc_as_time_slope"
    #codes <- 31
    #levsf <- "_sum1-4lev"
    #varnames_in <- "lm_albedo_as_time_slope"
    #varnames_in <- "amoc"
    #codes <- 101
    modes <- "select"
    #modes <- "timmean"
    #modes <- "fldmean"
    #modes <- "timmean_yearsum"
    #modes <- "yseasmean"
    #modes <- "yearsum"
    #modes <- "zonmean"
    #modes <- "vertsum"
    #modes <- "fldsum"
    #areas <- "northern_hemisphere"
    #areas <- "sibiria"
    #areas <- "60-90N"
    #areas <- "ladoga_remapnn"
    #areas <- "shuchye_remapnn"
    #areas <- "levinson-lessing_remapnn"
    #areas <- "taymyr_remapnn"
    #areas <- "emanda_remapnn"
    #areas <- "elgygytgyn_remapnn"
    #areas <- "two-jurts_remapnn"
    #areas <- "kotokel_remapnn"
    #areas <- "moc26.5N"
    #levs <- "-0to-5420m"
    #regboxes <- list(list(regbox="northeast_europe"))
    #regboxes <- list(list(regbox="NAsiberia"))

# =====================================
# 2 settings
} else if (F) { # awi-esm-1-1-lr 1pct 4CO2
    #models <- rep("echam6", t=2)
    models <- rep("fesom", t=2)
    if (F) {
        prefixes <- c("awi-esm-1-1-lr_1percCO2_monthly_mean",
                      "awi-esm-1-1-lr_4CO2_monthly_mean")
    }
    names_short <- c("1pctCO2", "abrupt-4xCO2") 
    if (F) {
        varnames_in <- rep("mlotst", t=2)
        varnames_in <- rep("thetao", t=2)
        #varnames_in <- rep("so", t=2)
        #varnames_in <- rep("potdens", t=2)
        #areas <- rep("LSstolpe18", t=2)
        areas <- rep("GIN2", t=2)
        depths <- rep("0-5900", t=2)
        remove_mean_froms <- c(1850, 1850)
        remove_mean_tos <- remove_mean_froms
        depth_fromsp <- rep(-3500, t=2)
    }
    if (F) { # transient pi last 100
        fromsf <- c(1850, 1850)
        tosf <- c(2099, 2099)
    } else if (F) { # ltm last 30
        fromsf <- c(2070, 2070)
        tosf <- c(2099, 2099)
    }
    #n_mas <- rep(60, t=2)
    n_mas <- rep(36, t=2)
    #n_mas <- rep(1, t=2)
    if (F) {
        names_legend <- c(eval(substitute(expression(paste("1%CO"[2])))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2])))))
    } else if (F) {
        names_legend <- c(eval(substitute(expression(paste("1%CO"[2], " last 30 years mean")))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " last 30 years mean")))))
    } else if (F) {
        names_legend <- c(eval(substitute(expression(paste("1%CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsp[3], from=fromsp[1], to=tosp[1]))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsp[4], from=fromsp[2], to=tosp[2]))))
    }

} else if (F) { # Hol-T with vs without orbital acceleration
    #prefixes <- c("cosmos-aso-wiso_Hol-T", "cosmos-aso-wiso_Hol-Tx10")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_main_mm", "cosmos-aso-wiso_Hol-Tx10_main_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_main_mm_plev", "cosmos-aso-wiso_Hol-Tx10_main_mm_plev")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_main_mm", "cosmos-aso-wiso_Hol-Tx10_wiso_mm")
    prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "cosmos-aso-wiso_Hol-Tx10_wiso_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_timeser_ext", "cosmos-aso-wiso_Hol-Tx10_timeser_ext")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_fort_75_monmean", "cosmos-aso-wiso_Hol-Tx10_fort_75_monmean")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_grb_code_183_remapcon2_r120x101",
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_183_remapcon2_r120x101")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "Hol-T_stschuett_echam5_wiso") # me vs st annual
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "Hol-T_echam5_wiso") # me vs me_w_st
    models <- c("echam5", "echam5")
    #models <- c("mpiom1", "mpiom1")
    names_short <- c("Hol-T", "Hol-Tx10")
    #names_short <- c("ch", "st")
    #names_short <- c("ch", "ch_w_st")
    #names_legend <- names_short
    names_legend <- c("COSMOS", "COSMOS x10")
    #names_legend <- c("COSMOS transient", "COSMOS transient x10")
    #names_legend <- c("transient", "transient x10")
    fromsf <- c("0004", "0001")
    #fromsf <- c("0004", "0004")
    #tosf <- c("7000", "7000")
    tosf <- c("7000", "7001")
    new_origins <- c(-6996, -7000)
    #new_origins <- c(-6996, -6996)
    time_ref <- 1950 # any string, e.g. "BP", or number
    #fromsp <- c(-20, -20) # with respect to `new_origin` (if defined) or `fromsf`
    #tosp <- c(0, 0)
    #types <- c("o", "o")
    #varnames_in <- c("temp2", "temp2")
    #varnames_in <- c("aprt", "aprt")
    #varnames_in <- c("tsurf", "tsurf")
    #varnames_in <- c("srad0d", "srad0d")
    #varnames_in <- c("albedo", "albedo")
    #varnames_in <- c("wisoaprt_d", "wisoaprt_d")
    #varnames_in <- c("wisoaprt_d_post", "wisoaprt_d_post")
    varnames_in <- c("lm_wisoaprt_d_post_as_time_slope", "lm_wisoaprt_d_post_as_time_slope")
    levs <- c(2, 2)
    #varnames_in <- c("tsurfaprt", "tsurfaprt")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_temp2", "lm_wisoaprt_d_sellevel_2_as_temp2")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_ptemp", "lm_wisoaprt_d_sellevel_2_as_ptemp")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_tsurf", "lm_wisoaprt_d_sellevel_2_as_tsurf")
    #varnames_in <- c("lm_wisoaprt_d_sellevel_2_as_ptsurf", "lm_wisoaprt_d_sellevel_2_as_ptsurf")
    #varnames_in <- c("lm_temp2_as_time_slope", "lm_temp2_as_time_slope")
    #varnames_in <- c("lm_tsurf_as_time_slope", "lm_tsurf_as_time_slope")
    #varnames_in <- c("lm_aprt_as_time_slope", "lm_aprt_as_time_slope")
    #varnames_in <- c("lm_wind10_as_time_slope", "lm_wind10_as_time_slope")
    #varnames_in <- c("c204_ICEARE_GLO", "c204_ICEARE_GLO")
    #varnames_in <- c("c205_ICEVOL_GLO", "c205_ICEVOL_GLO")
    #varnames_in <- c("c204_ICEARE_GLO", "c204_ICEARE_GLO")
    #varnames_in <- c("c64_ICEARE_ARC", "c64_ICEARE_ARC")
    #varnames_in <- c("c65_ICEVOL_ARC", "c65_ICEVOL_ARC")
    #varnames_in <- c("c46_HFL_GIN", "c46_HFL_GIN")
    #varnames_in <- c("c47_WFL_GIN", "c47_WFL_GIN")
    #varnames_in <- c("c44_ICEARE_GIN", "c44_ICEARE_GIN")
    #varnames_in <- c("c45_ICEVOL_GIN", "c45_ICEVOL_GIN")
    #varnames_in <- c("c86_HFL_LAB", "c86_HFL_LAB")
    #varnames_in <- c("c87_WFL_LAB", "c87_WFL_LAB")
    #varnames_in <- c("c84_ICEARE_LAB", "c84_ICEARE_LAB")
    #varnames_in <- c("c85_ICEVOL_LAB", "c85_ICEVOL_LAB")
    #varnames_in <- c("c145_ICEVOL_SO", "c145_ICEVOL_SO")
    #varnames_in <- c("c145_ICEVOL_SO", "c145_ICEVOL_SO")
    #varnames_in <- rep("amoc", t=2)
    #codes <- rep(101, t=2)
    #varnames_in <- c("zmld", "zmld")
    #varnames_in <- c("quv", "quv")
    #varnames_in <- c("quv_direction", "quv_direction")
    #levsf <- c("_int1000-100hPa", "_int1000-100hPa")
    #varnames_out_samedims <- "quv"
    #names_legend_samedims <- c("qu", "qv")
    #modes <- rep("select", t=2)
    modes <- rep("yearsum", t=2)
    #modes <- rep("seassum", t=2)
    #areas <- rep("moc45to60N", t=2)
    #levs <- rep("-285to-2180m", t=2)
    #areas <- rep("moc30to60N", t=2)
    #levs <- rep("-285to-2180m", t=2)
    #areas <- rep("moc26.5N", t=2)
    #areas <- rep("moc50N", t=2)
    #levs <- rep("-0to-5420m", t=2)
    #areas <- c("LSeaSouthmld", "LSeaSouthmld")
    #areas <- c("GINmld", "GINmld")
    #areas <- c("weddelmld", "weddelmld")
    #areas <- c("ladoga_remapnn", "ladoga_remapnn")
    #areas <- c("shuchye_remapnn", "shuchye_remapnn")
    #areas <- c("levinson-lessing_remapnn", "levinson-lessing_remapnn")
    #areas <- c("taymyr_remapnn", "taymyr_remapnn")
    #areas <- c("emanda_remapnn", "emanda_remapnn")
    #areas <- c("kotokel_remapnn", "kotokel_remapnn")
    #areas <- c("elgygytgyn_remapnn", "elgygytgyn_remapnn")
    #areas <- c("two-jurts_remapnn", "two-jurts_remapnn")
    seasonsf <- rep("annual", t=2)
    #seasonsf <- rep("yearsum", t=2)
    #seasonsf <- rep("seassum", t=2)
    #seasonsp <- rep("DJF", t=2)
    #seasonsp <- rep("MAM", t=2)
    #seasonsp <- rep("JJA", t=2)
    #seasonsp <- rep("SON", t=2)
    #seasonsp <- rep("Mar", t=2)
    #seasonsp <- rep("Apr", t=2)
    #seasonsp <- rep("Jun", t=2)
    #seasonsp <- rep("Sep", t=2)
    #seasonsp <- rep("Dec", t=2)
    #n_mas <- c(1, 1)
    #n_mas <- c(30, 10)
    #n_mas <- c(90, 30)
    #n_mas <- c(175, 30)
    #n_mas <- c(200, 20)
    n_mas <- c(250, 50)
    #n_mas <- c(30*12, 10*12)
    #n_mas <- c(250*12, 50*12) # jan-dec
    #n_mas <- c(120*12, 20*12) # seasons
    #remove_mean_froms <- c(-179, 0)
    #remove_mean_tos <- c(-179, 0)
    regboxes <- lapply(vector("list", l=2), base::append, list(regbox="NAsiberia"))

} else if (F) { # two vars of qu, qv, quv
    prefixes <- c("cosmos-aso-wiso_Hol-Tx10_wiso_mm", "cosmos-aso-wiso_Hol-Tx10_wiso_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-Tx10_main_mm_plev", "cosmos-aso-wiso_Hol-Tx10_main_mm_plev")
    models <- c("echam5", "echam5")
    names_short <- c("Hol-Tx10", "Hol-Tx10")
    names_legend <- c("Ladoga Hol-Tx10", "Ladoga Hol-Tx10")
    #names_legend <- c("Ladoga Hol-Tx10", "LadogaLand Hol-Tx10")
    fromsf <- c("0001", "0001") # Hol-Tx10
    tosf <- c("7001", "7001") # Hol-Tx10
    new_origins <- c(-7000, -7000) # Hol-Tx10
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- c(120, 120)
    modes <- c("select", "select")
    #modes <- c("yearsum", "yearsum")
    #seasonsf <- c("yearsum", "yearsum")
    #seasonsp <- "Feb" # cdo's default season timestamps: Feb, May, Aug, Nov
    #seasonsp <- "May"
    #seasonsp <- "Jun"
    #seasonsp <- "Aug"
    #seasonsp <- "Nov"
    #seasonsp <- "Dec"
    varnames_in <- c("aprl", "aprc")
    varnames_out_samedims <- c("aprl_vs_aprc")
    names_legend_samedims <- c("aprl", "aprc")
    #varnames_in <- c("wisoaprt_d_post", "wisoaprt_d_post")
    #levs <- c(2, 2)
    #varnames_in <- c("qu", "qv")
    #levsf <- c("_int1000-100hPa", "_int1000-100hPa")
    #varnames_out_samedims <- "quv"
    #names_legend_samedims <- c("qu", "qv")
    areas <- c("ladoga_remapnn", "ladoga_remapnn")
    #areas <- c("ladoga_remapnn", "ladogaLand_remapnn")

} else if (T) { # positive/negative north pacific index NPI in Hol-T
    models <- c("echam5", "echam5")
    prefixes <- c("cosmos-aso-wiso_Hol-T_main_mm", "cosmos-aso-wiso_Hol-T_main_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "cosmos-aso-wiso_Hol-T_wiso_mm")
    names_short <- c("Hol-T_above1.5sd", "Hol-T_below1.5sd")
    fromsf <- c("0004", "0004")
    tosf <- c(7000, 7000)
    new_origins <- c(-6996, -6996)
    time_ref <- 1950
    #varnames_in <- c("psl_gt_1.5_sd_NPI", "psl_lt_1.5_sd_NPI")
    varnames_in <- c("temp2_gt_1.5_sd_NPI", "temp2_lt_1.5_sd_NPI")
    #varnames_in <- c("aprt_gt_1.5_sd_NPI", "aprt_lt_1.5_sd_NPI")
    #varnames_in <- c("wind10_gt_1.5_sd_NPI", "wind10_lt_1.5_sd_NPI")
    #varnames_in <- c("wisoaprt_d_post_gt_1.5_sd_NPI", "wisoaprt_d_post_lt_1.5_sd_NPI")
    #levs <- c(2, 2)
    modes <- c("select", "select")
    #modes <- c("timmean", "timmean")
    #modes <- c("yearsum", "yearsum")
    seasonsf <- c("annual", "annual")
    #seasonsf <- c("NDJFMmean", "NDJFMmean")
    #seasonsf <- c("yearsum", "yearsum")
    regboxes <- lapply(vector("list", l=2), base::append, list(regbox="NAsiberia"))

} else if (F) { # recT106erai vs recT127era5
    models <- c("echam5", "echam6")
    prefixes <- c("echam5_recT106erai_wiso", "echam6_recT127era5_wiso")
    names_short <- c("recT106erai", "recT127era5")
    names_legend <- c("T106 ERA-I", "T127 ERA5")
    fromsf <- c(1958, 1979)
    tosf <- c(2019, 2019)
    n_mas <- rep(36, t=2)
    varnames_in <- rep("temp2", t=2)
    #varnames_in <- rep("tsurf", t=2)
    #varnames_in <- rep("aprt", t=2)
    modes <- rep("select", t=2)
    #areas <- rep("ladoga_remapnn", t=2)
    #areas <- rep("shuchye_remapnn", t=2)
    #areas <- rep("levinson-lessing_remapnn", t=2)
    #areas <- rep("taymyr_remapnn", t=2)
    #areas <- rep("emanda_remapnn", t=2)
    #areas <- rep("elgygytgyn_remapnn", t=2)
    areas <- rep("two-jurts_remapnn", t=2)
    #areas <- rep("kotokel_remapnn", t=2)

# =====================================
# 3 settings
} else if (F) { # awi-esm-1-1-lr deck anomlies vs pi
    models <- rep("echam6", t=3)
    #models <- rep("fesom", t=3)
    if (F) {
        prefixes <- c("historical_minus_piControl_echam6_echammon_awi-esm-1-1-lr",
                      "1percCO2_minus_piControl_echam6_echammon_awi-esm-1-1-lr",
                      "4CO2_minus_piControl_echam6_echammon_awi-esm-1-1-lr")
        names_short <- c("hist", "1pctCO2", "abrupt-4xCO2") 
    } else if (F) {
        prefixes <- c("awi-esm-1-1-lr_historical_minus_piControl_monthly_mean",
                      "awi-esm-1-1-lr_1percCO2_minus_piControl_monthly_mean",
                      "awi-esm-1-1-lr_4CO2_minus_piControl_monthly_mean")
        names_short <- c("hist", "1pctCO2", "abrupt-4xCO2") 
    }
    if (F) {
        varnames_in <- rep("temp2", t=3)
    } else if (F) {   
        varnames_in <- rep("tos", t=3)
        postpaths <- paste0(workpath, "/post/", models, "/regular_grid/ltm/", mode, "/", varnames_in)
        reg_dxs <- reg_dys <- rep("0.250", t=3)
    }
    fromsf <- c(1985, 2070, 2070)
    tosf <- c(2014, 2099, 2099)
    if (F) {
        names_legend <- c("historical", 
                          eval(substitute(expression(paste("1%CO"[2])))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2])))))
    } else if (F) {
        names_legend <- c(paste0("historical ", fromsf[1], "-", tosf[1], " mean minus piControl"),
                          #paste0("historical ", fromsp[2], "-", tosp[2], " mean"),
                          eval(substitute(expression(paste("1%CO"[2], " last 30 years mean minus piControl")))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " last 30 years mean minus piControl")))))
    }

} else if (F) { # Hol-7 vs Hol-T with vs without orbital acceleration
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=3)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm", t=3)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_grb_code_15_remapcon2_r120x101_gt_0.15_times_area", t=3)
    #prefixes <- c("cosmos-aso-wiso_Hol-7_wiso_mm", 
    #              "cosmos-aso-wiso_Hol-T_wiso_mm", 
    #              "cosmos-aso-wiso_Hol-Tx10_wiso_mm") 
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", 
    #              "Hol-T_stschuett_echam5_wiso",
    #              "Hol-T_echam5_wiso")
    #prefixes <- c("cosmos-aso-wiso_Hol-7_fort_75_monmean",
    #              "cosmos-aso-wiso_Hol-T_fort_75_monmean",
    #              "cosmos-aso-wiso_Hol-Tx10_fort_75_monmean")
    #prefixes <- c("cosmos-aso-wiso_Hol-7_timeser_ext",
    #              "cosmos-aso-wiso_Hol-T_timeser_ext",
    #              "cosmos-aso-wiso_Hol-Tx10_timeser_ext")
    #prefixes <- c("cosmos-aso-wiso_Hol-7_grb_code_15_remapcon2_r120x101_gt_0.15_times_area", # sicmomo
    #              "cosmos-aso-wiso_Hol-T_grb_code_15_remapcon2_r120x101_gt_0.15_times_area",
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_15_remapcon2_r120x101_gt_0.15_times_area")
    #prefixes <- c("cosmos-aso-wiso_Hol-7_grb_code_183_remapcon2_r120x101", # zmld
    #              "cosmos-aso-wiso_Hol-T_grb_code_183_remapcon2_r120x101",
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_183_remapcon2_r120x101")
    #prefixes <- rep("cosmos-aso-wiso_mpiom1_Hol-T_grb_code_15_remapcon2_r120x101", t=3) # sicmomo
    models <- rep("echam5", t=3)
    #models <- rep("mpiom1", t=3)
    #names_short <- rep("Hol-Tx10", t=3)
    #names_short <- rep("Hol-T", t=3)
    names_short <- c("Hol-7", "Hol-T", "Hol-Tx10")
    #names_short <- c("ch", "st", "me_w_st")
    #names_legend <- names_short
    #names_legend <- c("7k ctrl", "transient", "transient x10")
    #names_legend <- c("COSMOS 7k", "COSMOS transient", "COSMOS transient x10")
    #names_legend <- c("Mar", "Sep", "Annual")
    names_legend <- c("Dec", "Jun", "Annual")
    #cols <- c(3, 2, 1)
    #fromsf <- rep("0001", t=3) # hol-tx10
    fromsf <- rep("0004", t=3) # hol-t
    #fromsf <- c("0800", "0004", "0001") # hol-7 complete, hol-t, hol-tx10
    #fromsf <- c("2791", "0004", "0001") # hol-7 last 110 years, hol-tx10, hol-t
    #fromsf <- c("2800", "0004", "0001") # hol-7 last 101 years, hol-tx10, hol-t
    #fromsf <- c("0004", "0004", "0004") # ch, st, ch_w_st
    #tosf <- c("2900", "7000", "7001") # hol-7, hol-t, hol-tx10
    #tosf <- c("7000", "7000", "7000") # ch, st, ch_w_st
    #tosf <- rep("7001", t=3) # hol-tx10
    tosf <- rep("7000", t=3) # hol-t
    #new_origins <- rep(-7000, t=3) # hol-tx10
    new_origins <- rep(-6996, t=3) # hol-t
    #new_origins <- c(-9101, -6996, -7000) # hol-7 complete, hol-t, hol-tx10
    #new_origins <- c(-7110, -6996, -7000) # hol-7 last 110 years, hol-t, hol-tx10
    #new_origins <- c(-7101, -6996, -7000) # hol-7 last 101 years, hol-t, hol-tx10
    #new_origins <- c(-6996, -6996, -6996) # ch, st, ch_w_st
    time_ref <- 1950 # any string, e.g. "BP", or number
    remove_mean_froms <- rep(-6996, t=3)
    remove_mean_tos <- rep(-6996, t=3)
    #add_linear_trend <- c(F, T, T)
    #fromsp <- c(-7200, NA, NA)
    #fromsp <- c(-7010, -6996, -7000) # zoom: 7k control/7k transient transition 
    #tosp <- c(-7001, -6980, -6980)
    #fromsp <- c(-7030, -6996, -7000) # zoom: 7k control/7k transient transition 
    #tosp <- c(-7001, -6900, -6900)
    #seasonsf <- rep("annual", t=3)
    #seasonsf <- rep("yearsum", t=3)
    #seasonsf <- rep("seassum", t=3) # cdo default: date is middle of season
    #seasonsf <- c("annual", NA, NA)
    #seasonsf <- c("Mar", "Sep", "annual")
    #seasonsf <- c(NA, NA, "annual")
    seasonsf <- c("Dec", "Jun", "annual")
    #seasonsp <- rep("DJF", t=3)
    #seasonsp <- rep("MAM", t=3)
    #seasonsp <- rep("JJA", t=3)
    #seasonsp <- rep("SON", t=3)
    #seasonsp <- rep("Jan", t=3)
    #seasonsp <- rep("Feb", t=3)
    #seasonsp <- rep("Mar", t=3)
    #seasonsp <- rep("Jun", t=3)
    #seasonsp <- rep("Sep", t=3)
    #seasonsp <- c(NA, "Mar", "Sep")
    #seasonsp <- c("Dec", "Jun", NA)
    #varnames_in <- rep("temp2", t=3)
    #varnames_in <- rep("aprt", t=3)
    #varnames_in <- c("aprt", "aprl", "aprc")
    #varnames_out_samedims <- "aprt"
    #names_legend_samedims <- c("aprt (total)", "aprl (large-scale)", "aprc (convection)")
    #varnames_in <- rep("evap", t=3)
    #varnames_in <- rep("pe", t=3)
    #varnames_in <- rep("ws", t=3)
    #varnames_in <- rep("wisoaprt_d", t=3)
    #varnames_in <- rep("wisoaprt_d_post", t=3)
    #varnames_in <- rep("wisoevap_d_post", t=3)
    #varnames_in <- rep("wisope_d_post", t=3)
    #varnames_in <- rep("c204_ICEARE_GLO", t=3)
    #varnames_in <- rep("c64_ICEARE_ARC", t=3)
    #varnames_in <- rep("c144_ICEARE_SO", t=3)
    #varnames_in <- rep("SICOMO", t=3)
    #varnames_in <- rep("lm_SICOMO_as_time_slope", t=3)
    #varnames_in <- rep("zmld", t=3)
    #varnames_in <- rep("amoc", t=3)
    #codes <- rep(101, t=3)
    varnames_in <- rep("srad0d", t=3)
    #modes <- rep("select", t=3)
    #modes <- rep("fldmean", t=3)
    #modes <- rep("fldsum", t=3)
    #modes <- rep("yearsum", t=3)
    #modes <- rep("seassum", t=3)
    modes <- rep("zonmean", t=3)
    #areas <- rep("moc26.5N", t=3)
    #levs <- rep("-0to-5420m", t=3)
    #areas <- rep("northern_hemisphere", t=3)
    #areas <- rep("southern_hemisphere", t=3)
    #areas <- rep("weddelmld", t=3)
    #areas <- rep("ladoga_remapnn", t=3)
    #areas <- rep("shuchye_remapnn", t=3)
    #areas <- rep("levinson-lessing_remapnn", t=3)
    #areas <- rep("taymyr_remapnn", t=3)
    #areas <- rep("emanda_remapnn", t=3)
    #areas <- rep("elgygytgyn_remapnn", t=3)
    #areas <- rep("two-jurts_remapnn", t=3)
    #areas <- rep("kotokel_remapnn", t=3)
    #levs <- rep(2, t=3)
    n_mas <- rep(100, t=3)
    #n_mas <- rep(150, t=3)
    #n_mas <- c(30, 3*30, 30)
    #n_mas <- c(90, 3*90, 90)
    #n_mas <- rep(120, t=3)
    #n_mas <- c(120, 360, 120)
    #n_mas <- rep(360, t=3)
    #n_mas <- c(90, 3*150, 150) # compare seasons
    #n_mas <- c(1000, 3*500, 500) # Hol-7 mean
    #n_mas <- c(1000, 6*500, 3*500) # Hol-7 mean
    #n_mas <- c(1200, 12000, 1200)
    regboxes <- lapply(vector("list", l=3), base::append, list(regbox="NAsiberia"))

} else if (F) { # three vars of qu, qv, quv
    models <- c("echam5", "echam5", "echam5")
    prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm", t=3)
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_main_mm_plev", t=3)
    names_short <- rep("Hol-T", t=3)
    #names_short <- rep("Hol-Tx10", t=3)
    #names_legend <- rep("Ladoga Hol-Tx10", t=3)
    #fromsf <- rep("0001", t=3) # Hol-Tx10
    fromsf <- rep("0004", t=3) # Hol-T
    tosf <- rep("7000", t=3) # Hol-T
    #tosf <- rep("7001", t=3) # Hol-Tx10
    new_origins <- rep(-6996, t=3) # Hol-Tx10
    #new_origins <- rep(-7000, t=3) # Hol-Tx10
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- c(120, 120, 120)
    seasonsf <- rep("annual", t=3)
    #seasonsp <- "Feb" # cdo's default season timestamps: Feb, May, Aug, Nov
    #seasonsp <- "May"
    #seasonsp <- "Jun"
    #seasonsp <- "Aug"
    #seasonsp <- "Nov"
    #seasonsp <- "Dec"
    varnames_in <- c("wind10_gt_1.5_sd_NPI", "u10_gt_1.5_sd_NPI", "v10_gt_1.5_sd_NPI")
    varnames_uv <- list("wind10"=c(u="u10", v="v10"))
    #varnames_in <- c("qu", "qv", "quv")
    #levsf <- c("_int1000-100hPa", "_int1000-100hPa", "_int1000-100hPa")
    #varnames_out_samedims <- "quv"
    #names_legend_samedims <- c("qu", "qv", "quv")
    #modes <- rep("select", t=3)
    modes <- rep("timmean", t=3)
    #areas <- rep("ladoga_remapnn", t=3)
    regboxes <- lapply(vector("list", l=3), base::append, list(regbox="NAsiberia"))

} else if (F) { # echam restart issue
    models <- rep("echam6", t=3)
    prefixes <- paste0("awi-cm-1-1-lr_historical_", c("6hr", "day", "mon"))
    names_short <- c("6hr", "day", "mon")
    fromsf <- rep(1850, t=3)
    tosf <- rep(1851, t=3)
    names_legend <- paste0("temp2 ", names_short)
    varnames_in <- rep("temp2", t=3)
    modes <- rep("fldmean", t=3)

# =====================================
# 4 settings
} else if (F) { # awi-esm-1-1-lr deck
    models <- rep("echam6", t=4)
    #models <- rep("fesom", t=4)
    if (F) { # awi-cm-1-1-lr
        prefixes <- paste0("awi-cm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("awi-cm-1-1-lr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (F) { # awi-esm-1-1-lr
        prefixes <- paste0("awi-esm-1-1-lr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("awi-esm-1-1-lr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (F) { # awi-esm-1-2-lr
        prefixes <- paste0("awi-esm-1-2-lr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("awi-esm-1-2-lr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (F) { # awi-cm-1-1-mr
        prefixes <- paste0("awi-cm-1-1-mr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("awi-cm-1-1-mr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (F) { # mpi-esm1-2-lr
        prefixes <- paste0("mpi-esm1-2-lr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("mpi-esm1-2-lr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    } else if (T) { # mpi-esm1-2-hr
        prefixes <- paste0("mpi-esm1-2-hr_", c("piControl", "historical", "1percCO2", "4CO2"))
        names_short <- paste0("mpi-esm1-2-hr_", c("piControl", "hist", "1pctCO2", "abrupt-4xCO2")) 
    }
    text_cols <- c("black", "#E41A1C", "#377EB8", "#1B9E77")
    scatterpchs <- c(4, 16, 16, 16)
    #varnames_in <- rep("temp2", t=4)
    #codes <- c(167, "", "", "")
    #varnames_in <- rep("tas", t=4)
    #varnames_in <- rep("srad0", t=4)
    varnames_in <- rep("toa_imbalance", t=4)
    #varnames_in <- rep("tau_aero_550", t=4)
    #codes <- c(11, "", "", "")
    #varnames_in <- rep("srad0d", t=4)
    #codes <- c(184, "", "", "")
    #varnames_in <- rep("MOCw", t=4)
    #areas <- rep("NA", t=4)
    #depths <- rep("0-5900", t=4)
    #moc_lats <- c(26.5, 41)
    #varnames_in <- rep("siarean", t=4)
    #areas <- rep("arctic", t=4)
    #varnames_in <- rep("mlotst", t=4)
    #varnames_in <- rep("thetao", t=4)
    #varnames_in <- rep("so", t=4)
    #varnames_in <- rep("potdens", t=4)
    #areas <- rep("LSstolpe18", t=4)
    #areas <- rep("GIN2", t=4)
    #depths <- rep("0-5900", t=4)
    #depth_fromsp <- rep(-3500, t=4)
    #varnames_in <- rep("tos", t=4)
    #postpaths <- paste0(workpath, "/post/", models, "/regular_grid/ltm/", mode, "/", varnames_in)
    #reg_dxs <- reg_dys <- rep("0.250", t=4)
    if (T) { # transient deck first 150 years
        if (F) { # awi-esm-1-1-lr
            if (F) { # pi before deck
                #fromsf <- c(1855, 1850, 1850, 1850) # PI-CTRL5 wrong labels
                #tosf <- c(1954, 2014, 2099, 2099) # PI-CTRL5 wrong labels
                fromsf <- c(1842, 1850, 1850, 1850) # PI-CTRL5 correct labels
                tosf <- c(1941, 2014, 2099, 2099) # PI-CTRL5 correct labels
                new_origins <- c(1750, NA, NA, NA) # plot pi before historical on time axis
                tosp <- c(NA, NA, rep(1999, t=2))
            } else if (T) { # pi and deck
                fromsf <- c(1942, 1850, 1850, 1850) # PI-CTRL6
                tosf <- c(2091, 2014, 2099, 2099)
                new_origins <- c(1850, NA, NA, NA)
                #tosp <- rep(1999, t=4) # ECS
                tosp <- rep(1989, t=4) # TCR
            }
        } else if (F) { # awi-esm-1-2-lr lars
            fromsf <- c(1046, 1850, 1850, 1850)
            tosf <- c(1195, 2014, 1999, 1999)
            new_origins <- c(1850, NA, NA, NA)
            tosp <- c(NA, 1999, NA, NA)
        } else if (F) { # awi-cm-1-1-mr
            fromsf <- c(2650, 1850, 1850, 1850)
            tosf <- c(2799, 2014, 1999, 1999)
            new_origins <- c(1850, NA, NA, NA)
            #tosp <- c(NA, 1999, NA, NA) # ECS
            tosp <- rep(1989, t=4) # TCR
        } else if (T) { # mpi-esm1-2-lr and mpi-esm1-2-hr
            fromsf <- c(1850, 1850, 1850, 1850)
            tosf <- c(1999, 2014, 1999, 1999)
            #tosp <- c(NA, 1999, NA, NA) ECS
            tosp <- rep(1989, t=4) # TCR
        }
    } else if (F) { # tranient pi last 30
        fromsf <- c(1912, 1850, 1850, 1850)
        tosf <- c(1941, 2014, 2099, 2099)
        new_origins <- c(1912-91, NA, NA, NA)
        fromsp <- c(1849-29, 1850, 1850, 1850)
        tosp <- c(1849, 2014, 2099, 2099)
    } else if (F) { # ltm last 30
        fromsf <- c(1912, 1985, 2070, 2070)
        tosf <- c(1941, 2014, 2099, 2099)
    }
    #remove_mean_froms <- c(1849, 1850, 1850, 1850)
    #remove_mean_tos <- remove_mean_froms
    add_linear_trend <- c(F, F, F, T)
    #add_linear_trend <- c(F, F, T, T)
    modes <- rep("fldmean", t=4)
    seasonsf <- rep("annual", t=4)
    #seasonsp <- rep("JFM", t=4) 
    #seasonsp <- rep("Mar", t=4)
    #seasonsp <- rep("Jul", t=4)
    #n_mas <- rep(60, t=4)
    #n_mas <- rep(36, t=4)
    #n_mas <- rep(1, t=4)
    if (T) {
        names_legend <- c("piControl", 
                          "historical", 
                          eval(substitute(expression(paste("1%CO"[2])))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2])))))
    } else if (F) {
        names_legend <- c("piControl last 30 years mean",
                          #"piControl last 100 years mean",
                          paste0("historical ", fromsf[2], "-", tosf[2], " mean"),
                          #paste0("historical ", fromsp[2], "-", tosp[2], " mean"),
                          eval(substitute(expression(paste("1%CO"[2], " last 30 years mean")))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " last 30 years mean")))))
    } else if (F) {
        names_legend <- c(paste0("piControl ", seasonsp[1], " ", fromsp[1], "-", tosp[1]),
                          paste0("historical ", seasonsp[2], " ", fromsp[2], "-", tosp[2]),
                          eval(substitute(expression(paste("1%CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsp[3], from=fromsp[3], to=tosp[3]))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsp[4], from=fromsp[4], to=tosp[4]))))
    }
    
} else if (F) { # awi-esm-1-1-lr deck and lgm
    if (F) {
        prefixes <- c("historical_echam6_echammon_awi-esm-1-1-lr",
                      "1percCO2_echam6_echammon_awi-esm-1-1-lr",
                      "4CO2_echam6_echammon_awi-esm-1-1-lr",
                      "MM_01.01_echam_awi-esm-1-1-lr_lgm")
    } else if (F) {
        prefixes <- c("historical_echam6_echam_awi-esm-1-1-lr",
                      "1percCO2_echam6_echam_awi-esm-1-1-lr",
                      "4CO2_echam6_echam_awi-esm-1-1-lr",
                      "MM_01.01_echam_awi-esm-1-1-lr_lgm")
    }
    models <- rep("echam6", t=4)
    names_short <- c("hist", "1pctCO2", "abrupt-4xCO2", "lgm") 
    fromsf <- c(1850, 1850, 1850, 3537)
    #fromsf <- c(rep(1985, t=3), 3843)
    #fromsf <- c(1985, 2070, 2070, 3843)
    #tosf <- c(rep(2014, t=3), 3872)
    tosf <- c(2014, 2099, 2099, 3872)
    seasonsf <- rep("Jan-Dec", t=4)
    if (F) {
        names_legend <- c("historical", 
                          eval(substitute(expression(paste("1%CO"[2])))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2])))),
                          "LGM")
    } else if (F) {
        names_legend <- c(paste0("historical ", seasonsf[1], " ", fromsf[1], "-", tosf[1]),
                          eval(substitute(expression(paste("1%CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsf[2], from=fromsf[2], to=tosf[2]))),
                          eval(substitute(expression(paste("abrupt-4" %*% "CO"[2], " ", season, " ", from, "-", to)),
                                          list(season=seasonsf[3], from=fromsf[3], to=tosf[3]))),
                          paste0("LGM ", seasonsf[4], " ", fromsf[4], "-", tosf[4]))
    }
    n_mas <- rep(60, t=4)
    #varnames_in <- rep("temp2", t=4)
    varnames_in <- rep("srad0", t=4)
    
} else if (F) { # compare cdo remap* Hol-T*
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=4)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=4)
    #prefixes <- c("cosmos-aso-wiso_Hol-T_wiso_mm", "Hol-T_stschuett_echam5_wiso",
    #              "cosmos-aso-wiso_Hol-Tx10_wiso_mm", "cosmos-aso-wiso_Hol-Tx10_wiso_mm")
    #prefixes <- c("cosmos-aso-wiso_Hol-Tx10_timeser_ext", 
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_15_remapcon2_r120x101_gt_0.15_times_area",
    #              "cosmos-aso-wiso_Hol-Tx10_timeser_ext",
    #              "cosmos-aso-wiso_Hol-Tx10_grb_code_15_remapcon2_r120x101_gt_0.15_times_area")
    #prefixes <- c(rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=2), rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=2))
    models <- rep("echam5", t=4)
    #models <- rep("mpiom1", t=4)
    #names_short <- rep("Hol-Tx10", t=4)
    names_short <- rep("Hol-T", t=4)
    #names_short <- c("Hol-T_direct", "Hol-T_post", "Hol-Tx10_direct", "Hol-Tx10_post")
    #names_short <- c("c64_ICEARE_ARC", "NH", "c144_ICEARE_SO", "SH")
    #names_short <- c(rep("Hol-T", t=2), rep("Hol-Tx10", t=2))
    #names_legend <- names_short
    #names_legend <- c("Hol-T_direct", "Hol-T_post", "Hol-Tx10_direct", "Hol-Tx10_post")
    #names_legend <- c("nn", "bil", "bic", "dis")
    #names_legend <- c("Hol-T Ladoga", "Hol-T LadogaLand", "Hol-Tx10 Ladoga", "Hol-Tx10 LadogaLand")
    names_legend <- c("DJF", "MAM", "JJA", "SON")
    #fromsf <- rep("0001", t=4) # beginning of Hol-Tx10
    fromsf <- rep("0004", t=4) # beginning of Hol-T
    #fromsf <- c("0004", "0004", "0001", "0001")
    #tosf <- rep("6821", t=4)
    tosf <- rep("7000", t=4) # end of Hol-T
    #tosf <- rep("7001", t=4) # end of Hol-Tx10
    #tosf <- c("7000", "7000", "7001", "7001")
    #seasonsf <- rep("annual", t=4)
    #seasonsf <- c("Jan-Dec", "annual", "Jan-Dec", "annual")
    #seasonsf <- rep("yearsum", t=4)
    #seasonsf <- rep("seassum", t=4)
    #seasonsp <- rep("Jul", t=4)
    #seasonsp <- rep("Jan", t=4)
    seasonsp <- c("DJF", "MAM", "JJA", "SON")
    cols <- c(DJF="blue", MAM="darkgreen", JJA="red", SON="brown")
    remove_mean_froms <- rep(-6996, t=4)
    remove_mean_tos <- rep(-6996, t=4)
    #new_origins <- rep(-6996, t=4)
    #new_origins <- rep(-7000, t=4)
    #new_origins <- c(-6996, -6996, -7000, -7000)
    new_origins <- rep(-6996, t=4)
    time_ref <- 1950 # any string, e.g. "BP", or number
    #n_mas <- rep(30, t=4)
    n_mas <- rep(90, t=4)
    #n_mas <- rep(120, t=4)
    #n_mas <- c(100*12, 10*10, 10*12, 10)
    #varnames_in <- rep("temp2", t=4)
    #varnames_in <- rep("tsurf", t=4)
    #varnames_in <- rep("aprt", t=4)
    #varnames_in <- rep("aprl", t=4)
    #varnames_in <- rep("aprc", t=4)
    #varnames_in <- rep("aprs", t=4)
    #varnames_in <- rep("tsurfaprt", t=4)
    #varnames_in <- c("wisoaprt_d", "wisoaprt_d", "wisoaprt_d", "wisoaprt_d_post")
    #varnames_in <- rep("wisoaprt_d_post", t=4)
    #levs <- c(2, 2, 2, 2)
    #varnames_in <- c("c64_ICEARE_ARC", "SICOMO", "c144_ICEARE_SO", "SICOMO")
    modes <- rep("select", t=4)
    #modes <- c("select", "fldsum", "select", "fldsum") 
    #modes <- rep("yearsum", t=4)
    #modes <- rep("seassum", t=4)
    #varnames_out_samedims <- "SICOMO"
    #names_legend_samedims <- names_legend
    #cols_samedims <- 1:4 
    #ltys_samedims <- rep(1, t=4)
    #areas <- c("global", "northern_hemisphere", "global", "southern_hemisphere")
    #areas <- c("ladoga_remapnn", "ladoga_remapbil", "ladoga_remapbic", "ladoga_remapdis")
    #areas <- c("shuchye_remapnn", "shuchye_remapbil", "shuchye_remapbic", "shuchye_remapdis")
    #areas <- c("levinson-lessing_remapnn", "levinson-lessing_remapbil", "levinson-lessing_remapbic", "levinson-lessing_remapdis")
    #areas <- c("taymyr_remapnn", "taymyr_remapbil", "taymyr_remapbic", "taymyr_remapdis")
    #areas <- c("emanda_remapnn", "emanda_remapbil", "emanda_remapbic", "emanda_remapdis")
    #areas <- c("elgygytgyn_remapnn", "elgygytgyn_remapbil", "elgygytgyn_remapbic", "elgygytgyn_remapdis")
    #areas <- rep(c("ladoga_remapnn", "ladogaLand_remapnn"), t=2)
    areas <- rep("ladoga_remapnn", t=4)

} else if (F) { # awi-esm-1-1-lr cold/warm atmosphere restart problem
    models <- rep("echam6", t=4)
    prefixes <- c("awi-esm-1-1-lr_piControl_477_ollie_echam6",
                  "awi-esm-1-1-lr_midHolocene_warm_echam6",
                  "awi-esm-1-1-lr_midHolocene_cold_echam6",
                  "awi-esm-1-1-lr_piControl_mistral_esm_echam6")
    names_short <- c("PIollie", "MHwarm", "MHcold", "PImistral")
    fromsf <- c(2700, 3106, 3123, 1842)
    tosf <- c(3249, 3205, 3166, 1941)
    new_origins <- c(1, 551, 551, 551)
    #fromsp <- c(546, rep(NA, t=3))
    #tosp <- c(NA, rep(555, t=3))
    n_mas <- rep(36, t=4)
    varnames_in <- c("temp2", "tas", "temp2", "tas")
    modes <- rep("fldmean", t=4)
    varnames_out_samedims <- "temp2"
    names_legend_samedims <- names_short
    cols_samedims <- 1:4 
    ltys_samedims <- rep(1, t=4)
    areas <- rep("global", t=4)

} else if (F) { # echam restart issue
    models <- rep("echam6", t=4)
    prefixes <- paste0("awi-cm-1-1-lr_historical_", c("3hr", "6hr", "day", "mon"))
    names_short <- c("3hr", "6hr", "day", "mon")
    #prefixes <- paste0("awi-esm-1-1-lr_historical_", c("3hr", "6hrPlev", "day", "Amon"))
    #names_short <- c("3hr", "6hrPlev", "day", "Amon")
    fromsf <- rep(1850, t=4)
    tosf <- rep(1851, t=4)
    #names_legend <- paste0("st ", names_short)
    #varnames_in <- rep("st", t=4)
    #levs <- rep(47, t=4)
    names_legend <- paste0("lsp ", names_short)
    varnames_in <- rep("lsp", t=4)
    #names_legend <- paste0("tas ", names_short)
    #varnames_in <- rep("tas", t=4)
    modes <- rep("fldmean", t=4)

# ==================================================
## 5 settings 
} else if (F) { # compare Hol-T* seasons and annual
    models <- rep("echam5", t=5)
    #models <- rep("mpiom1", t=5)
    #models <- rep("jsbach", t=5)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm", t=5)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=5)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm_plev", t=5)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_grb", t=5)
    #prefixes <- rep("cosmos-aso-wiso_Hol-T_veg_mm", t=5)
    names_short <- rep("Hol-T", t=5)
    names_legend <- c("DJF", "MAM", "JJA", "SON", "Annual")
    #fromsf <- rep("0001", t=5) # beginning of Hol-Tx10
    #fromsf <- rep("0004", t=5) # beginning of Hol-T
    #fromsf <- c("0004", "0004", "0001", "0001")
    fromsf <- rep("6971", t=5) # last 30 years of Hol-T
    #tosf <- rep("6821", t=5)
    tosf <- rep("7000", t=5) # end of Hol-T
    #tosf <- rep("7001", t=5) # end of Hol-Tx10
    #tosf <- c("7000", "7000", "7001", "7001")
    #seasonsf <- rep("annual", t=5)
    #seasonsf <- c("Jan-Dec", "annual", "Jan-Dec", "annual")
    #seasonsf <- rep("yearsum", t=5)
    #seasonsf <- rep("seassum", t=5)
    seasonsf <- c("DJF", "MAM", "JJA", "SON", "Jan-Dec")
    #seasonsf <- c("DJF", "MAM", "JJA", "SON", "annual")
    #seasonsf <- c("DJFmean", "MAMmean", "JJAmean", "SONmean", "annual")
    #seasonsf <- c("yearsum", rep("seassum", t=4))
    #seasonsp <- rep("Jul", t=5)
    #seasonsp <- rep("Jan", t=5)
    #seasonsp <- c("annual", "DJF", "MAM", "JJA", "SON")
    #seasonsp <- c("yearmean", "DJF", "MAM", "JJA", "SON")
    #seasonsp <- c("yearsum", "DJF", "MAM", "JJA", "SON")
    #remove_mean_froms <- rep(-6996, t=5)
    #remove_mean_tos <- rep(-6996, t=5)
    #new_origins <- rep(-6996, t=5)
    #new_origins <- rep(-7000, t=5)
    #new_origins <- c(-6996, -6996, -7000, -7000)
    new_origins <- rep(-6996, t=5)
    time_ref <- 1950 # any string, e.g. "BP", or number
    #n_mas <- rep(30, t=5)
    n_mas <- c(rep(30, t=4), 4*30)
    #n_mas <- rep(30, t=5)
    #n_mas <- c(4*90, rep(90, t=5))
    #n_mas <- rep(120, t=5)
    #n_mas <- c(100*12, 10*10, 10*12, 10)
    varnames_in <- rep("temp2", t=5)
    #varnames_in <- rep("tsurf", t=5)
    #varnames_in <- rep("tsurfaprt", t=5)
    #varnames_in <- rep("aprt", t=5)
    #varnames_in <- rep("aprl", t=5)
    #varnames_in <- rep("aprc", t=5)
    #varnames_in <- rep("aprs", t=5)
    #varnames_in <- rep("evap", t=5)
    #varnames_in <- rep("pe", t=5)
    #varnames_in <- rep("quv", t=5)
    #varnames_in <- rep("quv_direction", t=5)
    #varnames_in <- rep("lm_quv_as_time_slope", t=5)
    #varnames_uv <- rep(list("lm_quv_as_time_slope"=c(u="lm_qu_as_time_slope", v="lm_qv_as_time_slope")), t=5)
    #varnames_out_samedims <- "lm_quv_as_time_slope"
    #levsf <- rep("_int1000-100hPa", t=5)
    #varnames_in <- rep("wisoaprt_d_post", t=5)
    #varnames_in <- rep("lm_wisoaprt_d_post_as_time_slope", t=5)
    #levs <- rep(2, t=5)
    #varnames_in <- rep("lm_temp2_as_time_slope", t=5)
    #varnames_in <- rep("lm_tsurf_as_time_slope", t=5)
    #varnames_in <- rep("lm_aprt_as_time_slope", t=5)
    #varnames_in <- rep("lm_aps_as_time_slope", t=5)
    #varnames_in <- rep("lm_psl_as_time_slope", t=5)
    #varnames_in <- rep("lm_THO_as_time_slope", t=5)
    #codes <- rep(2, t=5)
    #levs <- rep(6, t=5)
    #varnames_in <- rep("lm_aprt_as_time_slope", t=5)
    #varnames_in <- rep("lm_wind10_as_time_slope", t=5)
    #varnames_uv <- rep(list("lm_wind10_as_time_slope"=c(u="lm_u10_as_time_slope", v="lm_v10_as_time_slope")), t=5) # for quiver
    #varnames_out_samedims <- "lm_wind10_as_time_slope"
    #names_legend_samedims <- 
    #varnames_in <- rep("lm_act_fpc_as_time_slope", t=5)
    #codes <- rep(31, t=5)
    #levsf <- rep("_sum1-4lev", t=5)
    modes <- rep("timmean", t=5)
    #modes <- rep("select", t=5)
    #modes <- c("select", "fldsum", "select", "fldsum") 
    #modes <- rep("yearsum", t=5)
    #modes <- rep("seassum", t=5)
    #modes <- rep("vertsum", t=5)
    #modes <- c(rep("seassum", t=4), "yearsum")
    #varnames_out_samedims <- "SICOMO"
    #names_legend_samedims <- names_legend
    #cols <- c(DJF="blue", MAM="darkgreen", JJA="red", SON="brown", "black")
    #cols_samedims <- 1:5
    #ltys_samedims <- rep(1, t=5)
    #areas <- rep("global_remapcon2_r3600x1800", t=5)
    #areas <- rep("ladoga_remapnn", t=5)
    #areas <- rep("shuchye_remapnn", t=5)
    #areas <- rep("levinson-lessing_remapnn", t=5)
    #areas <- rep("taymyr_remapnn", t=5)
    #areas <- rep("emanda_remapnn", t=5)
    #areas <- rep("elgygytgyn_remapnn", t=5)
    #areas <- rep("two-jurts_remapnn", t=5)
    #areas <- rep("kotokel_remapnn", t=5)
    #regboxes <- lapply(vector("list", l=5), base::append, list(regbox="N30-90"))
    regboxes <- lapply(vector("list", l=5), base::append, list(regbox="NAsiberia"))

# ==================================================
# 6 settings
} else if (F) { # compare PLOT lakes
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=6)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=6)
    models <- rep("echam5", t=6)
    #names_short <- rep("Hol-Tx10", t=6)
    names_short <- rep("Hol-T", t=6)
    names_legend <- c("Ladoga", "Shuchye", "Levinson-Lessing", "Taymyr", "Emanda", "Elgygytgyn")
    #fromsf <- rep("0001", t=6) # beginning of chunk 1
    fromsf <- rep("0004", t=6)
    tosf <- rep("6821", t=6)
    #tosf <- rep("7001", t=6) # end of chunk 3
    #seasonsp <- rep("Jul", t=6)
    seasonsp <- rep("Jan", t=6)
    new_origins <- rep(-6996, t=6)
    #new_origins <- rep(-7000, t=6)
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- rep(30, t=6)
    #n_mas <- rep(120, t=6)
    #varnames_in <- rep("temp2", t=6)
    varnames_in <- rep("aprt", t=6)
    areas <- c("ladoga_remapnn", "shuchye_remapnn", "levinson-lessing_remapnn", "taymyr_remapnn", "emanda_remapnn", "elgygytgyn_remapnn")

} else if (F) { # compare Hol-T* time series
    #prefixes <- rep("cosmos-aso-wiso_Hol-Tx10_fort_75", t=6)
    prefixes <- c(rep("cosmos-aso-wiso_Hol-T_wiso_mm", t=3), rep("cosmos-aso-wiso_Hol-Tx10_wiso_mm", t=3))
    models <- rep("echam5", t=6)
    #models <- rep("mpiom1", t=6)
    names_short <- c(rep("Hol-T", t=3), rep("Hol-Tx10", t=3))
    names_legend <- names_short
    fromsf <- c(rep("0004", t=3), rep("0001", t=3))
    tosf <- c(rep("7000", t=3), rep("7001", t=3))
    new_origins <- c(rep(-6996, t=3), rep(-7000, t=3))
    time_ref <- 1950 # any string, e.g. "BP", or number
    seasonsp <- rep("JJA", t=6)
    #n_mas <- rep(30, t=6)
    #n_mas <- rep(120, t=6)
    n_mas <- c(rep(10*12, t=3), rep(5*12, t=3))
    #n_mas <- c(rep(100*12, t=3), rep(20*12, t=3))
    modes <- rep("select", t=6)
    #varnames_in <- rep("amoc", t=6)
    #codes <- rep(101, t=6)
    #areas <- c("moc45to60N", "moc30to60N", "moc50N", "moc45to60N", "moc30to60N", "moc26.5N")
    #levs <- c("-285to-2180m", "-285to-2180m", "-0to-5420m", "-0to-5420m", "-0to-5420m", "-0to-5420m")
    #names_legend <- paste0(areas, " ", levs)
    varnames_in <- rep(c("aprt", "aprl", "aprc"), t=2)
    areas <- rep("ladoga_remapnn", t=6)
    varnames_out_samedims <- "aprt"
    names_legend_samedims <- c("Hol-T aprt (total)", "Hol-T aprl (large-scale)", "Hol-T aprc (convection)",
                               "Hol-Tx10 aprt (total)", "Hol-Tx10 aprl (large-scale)", "Hol-Tx10 aprc (convection)")
    ltys_samedims <- c(rep(1, t=3), rep(2, t=3))
    cols_samedims <- rep(c("black", "#E41A1C", "#377EB8"), t=2)

} else if (F) { # positive/negative north pacific index NPI in Hol-T
    models <- rep("echam5", t=6)
    prefixes <- rep("cosmos-aso-wiso_Hol-T_main_mm", t=6)
    names_short <- c(rep("Hol-T_above1.5sd", t=3), rep("Hol-T_below1.5sd", t=3))
    fromsf <- rep("0004", t=6)
    tosf <- rep(7000, t=6)
    new_origins <- rep(-6996, t=6)
    time_ref <- 1950
    varnames_in <- c("wind10_gt_1.5_sd_NPI", "u10_gt_1.5_sd_NPI", "v10_gt_1.5_sd_NPI",
                     "wind10_lt_1.5_sd_NPI", "u10_lt_1.5_sd_NPI", "v10_lt_1.5_sd_NPI")
    varnames_uv <- list(list(uv=1, u=2, v=3),
                        list(uv=4, u=5, v=6))
    varnames_out_samedims <- "wind10_u10_v10"
    modes <- rep("timmean", t=6)
    seasonsf <- rep("annual", t=6)
    #regboxes <- lapply(vector("list", l=6), base::append, list(regbox="NAsiberia"))

# ==================================================
# 7 settings
} else if (F) { # compare pi/mh cold/warm atmosphere restart problem
    models <- rep("echam6", t=7)
    prefixes <- c("awi-esm-1-1-lr_pi477_ollie", # temp2 2700 to 3249 -> 2051 to 2600
                  "awi-esm-1-1-lr_piControl_g3bid", # temp2 2701 to 2999 -> 2051 to 2349
                  "awi-esm-1-1-lr_piControl", # tas 1842 to 1941 -> 2350 to 2449
                  "awi-esm-1-1-lr_mh477_ollie", # temp2 2623 to 2657
                  "awi-esm-1-1-lr_mh_new_mistral", # temp2 2624 to 3001
                  "awi-esm-1-1-lr_midHolocene", # tas 3106 to 3205
                  "awi-esm-1-1-lr_mh_cold_mistral") # temp2 3105 to 3207
    names_short <- c("pi477", "piControl_spinup", "piControl", "mh477", "mh_new", "midHolocene", "mh_cold")
    fromsf <- c(2700, 2701, 1842, 2623, 2624, 3106, 3105)
    tosf <- c(3249, 2999, 1941, 2657, 3001, 3205, 3207)
    new_origins <- c(2051, 2051, 2350, NA, NA, NA, NA)
    #fromsp <- c(546, rep(NA, t=3))
    #tosp <- c(NA, rep(555, t=3))
    n_mas <- rep(36, t=7)
    varnames_in <- c("temp2", "temp2", "tas", "temp2", "temp2", "tas", "temp2")
    modes <- rep("fldmean", t=7)
    varnames_out_samedims <- "temp2"
    names_legend_samedims <- c("pi477 (2700 to 3249)", "piControl_spinup (2701 to 2999)", "piControl (1842 to 1941)",
                               "mh477 (2623 to 2657)", "mh_new (2624 to 3001)", "midHolocene (3106 to 3205)",
                               "mh_cold (3105 to 3207)")
    cols_samedims <- 1:7
    ltys_samedims <- rep(1, t=7)

# ==================================================
# 8 settings
} else if (F) { # hol-tx10 vs hol-t
    prefixes <- c(rep("cosmos-aso-wiso_Hol-Tx10_timeser_ext", t=4), 
                  rep("cosmos-aso-wiso_Hol-T_timeser_ext", t=4))
    models <- rep("mpiom1", t=8)
    names_short <- c(rep("Hol-Tx10", t=4), rep("Hol-T", t=4))
    names_legend <- names_short
    fromsf <- c(rep("0001", t=4), rep("0004", t=4))
    tosf <- c(rep("7001", t=4), rep("7000", t=4))
    new_origins <- c(rep(-7000, t=4), rep(-6996, t=4)) 
    time_ref <- 1950 # any string, e.g. "BP", or number
    n_mas <- c(rep(120, t=4), rep(1200, t=4))
    cols <- c(rep(1, t=4), rep(2, t=4))
    #varnames_in <- rep(c("c208_SST_GLO", "c210_T200_GLO", "c212_T700_GLO", "c214_T2200_GLO"), t=2)
    #varnames_in <- rep(c("c128_SST_ATL", "c130_T200_ATL", "c132_T700_ATL", "c134_T2200_ATL"), t=2)
    #varnames_in <- rep(c("cSST_GIN", "c50_T200_GIN", "c52_T700_GIN", "c54_T2200_GIN"), t=2)
    #varnames_in <- rep(c("c88_SST_LAB", "c90_T200_LAB", "c92_T700_LAB", "c94_T2200_LAB"), t=2)
    #varnames_out_samedims <- "thetao"
    varnames_in <- rep(c("c209_SSS_GLO", "c211_S200_GLO", "c213_S700_GLO", "c215_S2200_GLO"), t=2)
    #varnames_in <- rep(c("c129_SSS_ATL", "c131_S200_ATL", "c133_S700_ATL", "c135_S2200_ATL"), t=2)
    #varnames_in <- rep(c("c49_SSS_GIN", "c51_S200_GIN", "c53_S700_GIN", "c55_S2200_GIN"), t=2)
    #varnames_in <- rep(c("c89_SSS_LAB", "c91_S200_LAB", "c93_S700_LAB", "c95_S2200_LAB"), t=2)
    varnames_out_samedims <- "so"
    names_legend_samedims <- paste0(names_short, rep(paste0(" ", c("surf", "200m", "700m", "2200m")), t=2))
    cols_samedims <- c(1:4, 1:4)
    ltys_samedims <- c(rep(2, t=4), rep(1, t=4))

} else if (F) { # compare pi/mh cold/warm atmosphere restart problem
    models <- rep("echam6", t=8)
    prefixes <- c("awi-esm-1-1-lr_pi477_ollie", # temp2 2700 to 3249 -> 2051 to 2600
                  "awi-esm-1-1-lr_piControl_g3bid", # temp2 2701 to 2999 -> 2051 to 2349
                  "awi-esm-1-1-lr_piControl", # tas 1842 to 1941 -> old: 2350 to 2449; new: 3000 to 3099 
                  "awi-esm-1-1-lr_mh477_ollie", # temp2 2623 to 2657 -> 2700 to 2734
                  "awi-esm-1-1-lr_mh_new_mistral", # temp2 2624 to 3001 -> 2701 to 3078
                  "awi-esm-1-1-lr_midHolocene", # tas 3106 to 3205 -> 3183 to 3282 
                  "awi-esm-1-1-lr_mh_cold_mistral", # temp2 3123 to 3266 -> 3200 to 3343
                  "awi-esm-1-1-lr_mat_0013") # temp2 1855 to 1968; new: 3100 to 3250
    names_short <- c("pi477", "piControl_spinup", "piControl", "mh477", "mh_new", "midHolocene", "mh_cold", "mat_0013")
    fromsf <-      c(2700, 2701, 1842, 2623, 2624, 3106, 3123, 1855)
    tosf <-        c(3249, 2999, 1941, 2657, 3001, 3205, 3266, 1968)
    new_origins <- c(NA,   NA,   3000, 2700, 2701, 3183, 3200, 3000)
    #fromsp <- c(546, rep(NA, t=3))
    #tosp <- c(NA, rep(555, t=3))
    n_mas <- rep(36, t=8)
    codes <- c("", "", "", "", "", "", "", 167)
    varnames_in <- c("temp2", "temp2", "tas", "temp2", "temp2", "tas", "temp2", "temp2")
    modes <- rep("fldmean", t=8)
    varnames_out_samedims <- "temp2"
    names_legend_samedims <- c("1) pi477", "2) piControl_spinup", "3) piControl", "4) mh477", 
                               "5) mh_new", "6) midHolocene", "7) mh_cold", "8) mat_0013")
    cols_samedims <- 1:8
    ltys_samedims <- rep(1, t=8)

} # which settings

