# 

fs <- c("/work/ba1103/a270073/post/ACCESS-CM2/timmean/siconc/ACCESS-CM2_ssp585_r1i1p1f1_ACCESS-CM2_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/ACCESS-ESM1-5/timmean/siconc/ACCESS-ESM1-5_ssp585_r1i1p1f1_ACCESS-ESM1-5_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/BCC-CSM2-MR/timmean/siconc/BCC-CSM2-MR_ssp585_r1i1p1f1_BCC-CSM2-MR_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/CAMS-CSM1-0/timmean/siconc/CAMS-CSM1-0_ssp585_r1i1p1f1_CAMS-CSM1-0_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/CanESM5-CanOE/timmean/siconc/CanESM5-CanOE_ssp585_r1i1p2f1_CanESM5-CanOE_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/CanESM5/timmean/siconc/CanESM5_ssp585_r1i1p1f1_CanESM5_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/CESM2/timmean/siconc/CESM2_ssp585_r4i1p1f1_CESM2_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/CESM2-WACCM/timmean/siconc/CESM2-WACCM_ssp585_r1i1p1f1_CESM2-WACCM_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/CMCC-CM2-SR5/timmean/siconc/CMCC-CM2-SR5_ssp585_r1i1p1f1_CMCC-CM2-SR5_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/CMCC-ESM2/timmean/siconc/CMCC-ESM2_ssp585_r1i1p1f1_CMCC-ESM2_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/CNRM-CM6-1-HR/timmean/siconc/CNRM-CM6-1-HR_ssp585_r1i1p1f2_CNRM-CM6-1-HR_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/CNRM-CM6-1/timmean/siconc/CNRM-CM6-1_ssp585_r1i1p1f2_CNRM-CM6-1_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/E3SM-1-1/timmean/siconc/E3SM-1-1_ssp585_r1i1p1f1_E3SM-1-1_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/EC-Earth3-CC/timmean/siconc/EC-Earth3-CC_ssp585_r1i1p1f1_EC-Earth3-CC_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/EC-Earth3/timmean/siconc/EC-Earth3_ssp585_r1i1p1f1_EC-Earth3_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/EC-Earth3-Veg-LR/timmean/siconc/EC-Earth3-Veg-LR_ssp585_r1i1p1f1_EC-Earth3-Veg-LR_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/EC-Earth3-Veg/timmean/siconc/EC-Earth3-Veg_ssp585_r1i1p1f1_EC-Earth3-Veg_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/FGOALS-f3-L/timmean/siconc/FGOALS-f3-L_ssp585_r1i1p1f1_FGOALS-f3-L_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/FGOALS-g3/timmean/siconc/FGOALS-g3_ssp585_r1i1p1f1_FGOALS-g3_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/FIO-ESM-2-0/timmean/siconc/FIO-ESM-2-0_ssp585_r1i1p1f1_FIO-ESM-2-0_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/GFDL-CM4/timmean/siconc/GFDL-CM4_ssp585_r1i1p1f1_GFDL-CM4_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/GFDL-ESM4/timmean/siconc/GFDL-ESM4_ssp585_r1i1p1f1_GFDL-ESM4_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/HadGEM3-GC31-LL/timmean/siconc/HadGEM3-GC31-LL_ssp585_r1i1p1f3_HadGEM3-GC31-LL_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/HadGEM3-GC31-MM/timmean/siconc/HadGEM3-GC31-MM_ssp585_r1i1p1f3_HadGEM3-GC31-MM_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/INM-CM4-8/timmean/siconc/INM-CM4-8_ssp585_r1i1p1f1_INM-CM4-8_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/INM-CM5-0/timmean/siconc/INM-CM5-0_ssp585_r1i1p1f1_INM-CM5-0_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/IPSL-CM6A-LR/timmean/siconc/IPSL-CM6A-LR_ssp585_r1i1p1f1_IPSL-CM6A-LR_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/KIOST-ESM/timmean/siconc/KIOST-ESM_ssp585_r1i1p1f1_KIOST-ESM_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/MIROC6/timmean/siconc/MIROC6_ssp585_r1i1p1f1_MIROC6_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/MIROC-ES2L/timmean/siconc/MIROC-ES2L_ssp585_r1i1p1f2_MIROC-ES2L_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/MPI-ESM1-2-HR/timmean/siconc/MPI-ESM1-2-HR_ssp585_r1i1p1f1_MPI-ESM1-2-HR_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/MPI-ESM1-2-LR/timmean/siconc/MPI-ESM1-2-LR_ssp585_r1i1p1f1_MPI-ESM1-2-LR_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/MRI-ESM2-0/timmean/siconc/MRI-ESM2-0_ssp585_r1i1p1f1_MRI-ESM2-0_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/NESM3/timmean/siconc/NESM3_ssp585_r1i1p1f1_NESM3_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/NorESM2-LM/timmean/siconc/NorESM2-LM_ssp585_r1i1p1f1_NorESM2-LM_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/NorESM2-MM/timmean/siconc/NorESM2-MM_ssp585_r1i1p1f1_NorESM2-MM_timmean_siconc_global_Jan-Dec_2015-2015.nc",
"/work/ba1103/a270073/post/UKESM1-0-LL/timmean/siconc/UKESM1-0-LL_ssp585_r1i1p1f2_UKESM1-0-LL_timmean_siconc_global_Jan-Dec_2015-2015.nc")

for (fi in seq_along(fs)) {
    message("*************************************************************************************\n", fi, ": ", fs[fi])
    cmd <- paste0("cdo griddes ", fs[fi], " | head -30")
    system(cmd)
    cmd <- paste0("cdo griddes ", fs[fi], " | grep xvals | head -30")
    system(cmd)
    cmd <- paste0("cdo griddes ", fs[fi], " | grep yvals | head -30")
    system(cmd)
    userchoice <- readline(prompt="Continue? [enter]")
}


