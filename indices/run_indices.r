# r

rm(list=ls()); graphics.off()
source("index_functions.r")

index <- NULL

# calc north pacific index
if (F) {
    if (T) f <- ""
    if (file.exists(f)) {
        stop("update")
        message("run north_pacific_index(DJF_slp_fldmean_detrend=", f, ") ...")
        index <- siberian_high_index(DJF_slp_fldmean_detrend=f, varname="psl")
    } else {
        stop("f = ", f, " does not exist")
    }
} # calc north pacific high index

# calc siberian high index
if (T) {
    if (T) f <- "/ace/user/cdanek/post/echam5/fldmean/psl/cosmos-aso-wiso_Hol-T_main_mm_echam5_fldmean_psl_minus_trend_E80-120_N40-65_DJFmean_0004-7000.nc"
    if (file.exists(f)) {
        message("run siberian_high_index(DJF_slp_fldmean_detrend=", f, ") ...")
        index <- siberian_high_index(DJF_slp_fldmean_detrend=f, varname="psl")
    } else {
        stop("f = ", f, " does not exist")
    }
} # calc siberian high index


# plot result of called functions (if any)
if (!is.null(index)) {

    # histogram of index
    plotname <- paste0(index$index_fname, "_", index$outname, "_hist.png")
    message("\nplot ", plotname, " ...")
    png(plotname, width=1600, height=1600, res=400)
    index_hist <- hist(index$index, plot=F)
    hist(index$index, xaxt="n", yaxt="n",
         xlab=index$index_names_hort, ylab="Count", main=NA)
    title(index$index_name, cex.main=0.75)
    axis(1, at=index_hist$breaks)
    axis(2, at=pretty(index_hist$counts, n=10), las=2)
    dev.off()

    # barplot of index
    plotname <- paste0(index$index_fname, "_", index$outname, "_ts.png")
    message("plot ", plotname, " ...")
    png(plotname, width=2666, height=1600, res=400)
    plot(index$index, t="n", col="gray", lwd=0.5, yaxt="n",
         xlab="index", ylab=paste0(index$index_name_short, " [std dev]"))
    title(index$index_name, cex.main=0.75)
    axis(2, at=pretty(index$index, n=8), las=2)
    pos_col <- col2rgba("red", 0.33); neg_col <- col2rgba("blue", 0.33)
    pos_inds <- which(index$index >= 0); neg_inds <- which(index$index < 0)
    fac <- 0.5
    dt <- diff(seq_along(index$index))[1]
    rect(pos_inds - fac*dt, 0,
         pos_inds + fac*dt, index$index[pos_inds],
         col=pos_col, border=NA)
    rect(neg_inds - fac*dt, 0,
         neg_inds + fac*dt, index$index[neg_inds],
         col=neg_col, border=NA)
    abline(h=0)
    # add running mean
    n_mas <- c(10, 20, 30, 50, 100)
    mas <- vector("list", l=length(n_mas))
    for (i in seq_along(n_mas)) {
        mas[[i]] <- stats::filter(index$index, filter=rep(1/n_mas[i], t=n_mas[i]))
        lines(mas[[i]], col=i+1, lty=i+1)
    }
    # add values below/above `sd_thr` std devs.
    if (length(index$sd_plus_inds) > 0) {
        points(index$sd_plus_inds, index$index[index$sd_plus_inds], col=pos_col)
    }
    if (length(index$sd_minus_inds) > 0) {
        points(index$sd_minus_inds, index$index[index$sd_minus_inds], col=neg_col)
    }
    legend("topleft", paste0(n_mas, " pt run mean"), 
           col=seq_along(n_mas)+1, lty=seq_along(n_mas)+1,
           bty="n", x.intersp=0.2, cex=0.5, ncol=length(n_mas))
    legend("bottomleft", index$f,
           col="black", lty=NA, pch=NA, lwd=0.5, x.intersp=-1.5, bty="n", cex=0.35)
    dev.off()

} # if !is.null(index)

