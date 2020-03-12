# r

mpiom_calc_moc <- function(varname="amoc", mpiom_model_res="GR30L40", mpiom_reg_res="r360x180") {

    if (missing(varname) ||
        !any(varname == c("gmoc", "amoc"))) {
        stop("mpiom_calc_moc(): choose \"gmoc\" (global MOC; code 100 from fort.75) or ",
             "\"amoc\" (Atlantic MOC; code 101 from fort.75)")
    }


} # mpiom_calc_moc function


