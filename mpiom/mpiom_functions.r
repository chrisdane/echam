# r

correct_mpiom_partabn <- function(partabn_file) {

    if (missing(partabn_file) || !is.character(partabn_file)) {
        stop("provide `partabn_file=\"/path/to/mpiom_partabn\"`")
    }
    
    partabn <- readLines(partabn_file)

    # "  NAME=c1_PSIGULF" --> "  out_name=c1_PSIGULF"
    # -> keep indents! 
    name_inds <- which(grepl("  NAME=", partabn))
    if (length(name_inds) > 0) {
        for (i in seq_along(name_inds)) {
            laste_equal_sign_character_ind <- regexpr("\\=[^\\=]*$", partabn[name_inds[i]])
            given_name <- substr(partabn[name_inds[i]], 
                                 laste_equal_sign_character_ind+1, 
                                 nchar(partabn[name_inds[i]]))
            message(i, "/", length(name_inds), ": \"", partabn[name_inds[i]], "\" --> ", appendLF=F)
            partabn[name_inds[i]] <- paste0("  out_name=", given_name) 
            message("\"", partabn[name_inds[i]], "\"")
        }
    }

    # "  CODE=1" --> "  name=var12"
    # -> keep indents! 
    code_inds <- which(grepl("  CODE=", partabn))
    if (length(code_inds) > 0) {
        for (i in seq_along(code_inds)) {
            laste_equal_sign_character_ind <- regexpr("\\=[^\\=]*$", partabn[code_inds[i]])
            given_code_number <- substr(partabn[code_inds[i]], 
                                        laste_equal_sign_character_ind+1, 
                                        nchar(partabn[code_inds[i]]))
            message(i, "/", length(code_inds), ": \"", partabn[code_inds[i]], "\" --> ", appendLF=F)
            partabn[code_inds[i]] <- paste0("  name=var", given_code_number) 
            message("\"", partabn[code_inds[i]], "\"")
        }
    }

    fout <- paste0(dirname(partabn_file), "/", 
                   tools::file_path_sans_ext(basename(partabn_file)),
                   "_corrected.txt")
    message("\nsave \"", fout, "\" ...")
    write(partabn, file=fout)

} # correct_mpiom_partabn


mpiom_calc_moc <- function(varname="amoc", mpiom_model_res="GR30L40", mpiom_reg_res="r360x180") {

    if (missing(varname) ||
        !any(varname == c("gmoc", "amoc"))) {
        stop("mpiom_calc_moc(): choose \"gmoc\" (global MOC; code 100 from fort.75) or ",
             "\"amoc\" (Atlantic MOC; code 101 from fort.75)")
    }


} # mpiom_calc_moc function


