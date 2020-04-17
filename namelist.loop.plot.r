# this namelist is used by `plot_loop_echam.r`

# cols: model settings per run
# rows: specifications for nrow plot_echam.r runs

# =====================================
# 3 settings
if (T) { 
    # Hol-7 vs Hol-T with vs without orbital acceleration with
    # 7 areas (plot lakes) a 3 vars (wisoaprt, evap, pe) a 5 seasons (an, djf, mam, jja, son) 
    # -> 7*3*5 = 105 plots
    nareas <- 7; nvars <- 3; nseas <- 5
    nruns <- nareas*nvars*nseas
    prefixes <- matrix(c("cosmos-aso-wiso_echam5_Hol-7_wiso_mm", 
                         "cosmos-aso-wiso_echam5_Hol-T_wiso_mm", 
                         "cosmos-aso-wiso_echam5_Hol-Tx10_wiso_mm"), nruns, 3, byrow=T)
    models <- matrix(rep("echam5", t=3), nruns, 3, byrow=T)
    names_short <- matrix(c("Hol-7", "Hol-T", "Hol-Tx10"), nruns, 3, byrow=T)
    names_legend <- names_short
    names_legend <- matrix(c("COSMOS 7k", "COSMOS transient", "COSMOS transient x10"), nruns, 3, byrow=T)
    fromsf <- matrix(c("2800", "0004", "0001"), nruns, 3, byrow=T) # hol-7 last 101 years, hol-tx10, hol-t
    tosf <- matrix(c("2900", "7000", "7001"), nruns, 3, byrow=T) # hol-7, hol-tx10, hol-t
    new_origins <- matrix(c(-7101, -6996, -7000), nruns, 3, byrow=T) # hol-7 last 101 years, hol-tx10, hol-t
    time_frequencies <- matrix(rep("annual", t=3), nruns, 3, byrow=T)
    time_ref <- 1950 # any string, e.g. "BP", or number
    seasonsf <- replicate(rep(rep(c("yearsum", rep("seassum", t=4)), t=nvars), t=nareas), n=3)
    seasonsp <- replicate(rep(rep(c(NA, "DJF", "MAM", "JJA", "SON"), t=nvars), t=nareas), n=3)
    varnames_in <- replicate(rep(rep(c("wisoaprt_d_post", "wisoevap_d_post", "wisope_d_post"), e=nseas), t=nareas), n=3)
    levs <- matrix(rep(2, t=3), nruns, 3, byrow=T)
    modes <- replicate(rep(rep(c("yearsum", rep("seassum", t=4)), t=nvars), t=nareas), n=3)
    areas <- replicate(c(rep("ladoga_remapnn", t=nseas*nvars),
                         rep("shuchye_remapnn", t=nseas*nvars),
                         rep("levinson-lessing_remapnn", t=nseas*nvars),
                         rep("emanda_remapnn", t=nseas*nvars),
                         rep("elgygytgyn_remapnn", t=nseas*nvars),
                         rep("two-yurts_remapnn", t=nseas*nvars),
                         rep("kotokel_remapnn", t=nseas*nvars)), n=3)
    n_mas <- matrix(c(90, 3*150, 150), nruns, 3, byrow=T) # seasons

} # which settings

