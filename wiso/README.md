# wiso

In wiso, `nwiso` water isotopologues (water molecules with different isotopic compositions) are calculated and saved in the `level` dimension of `wiso*` variables in `*wiso*` files:
No | Water isotopologue | Molecular weight | Latex notation
---|--------------------|------------------|---------------
1 | <sup>1</sup>H<sub>2</sub><sup>16</sup>O = H<sub>2</sub><sup>16</sup>O | 18 | `${}^{1}$H$_{2}{}^{16}$O` = `H$_{2}{}^{16}$O`
2 | <sup>1</sup>H<sub>2</sub><sup>18</sup>O = H<sub>2</sub><sup>18</sup>O | 20 | `${}^{1}$H$_{2}{}^{18}$O` = `H$_{2}{}^{18}$O`
3 | <sup>1</sup>H<sup>2</sup>H<sup>16</sup>O = HD<sup>16</sup>O | 19 | `${}^{1}$H${}^{2}$H${}^{16}$O` = `HD$^{16}$O`
4\* | <sup>1</sup>H<sub>2</sub><sup>17</sup>O = H<sub>2</sub><sup>17</sup>O | 19 | `${}^{1}$H$_{2}{}^{17}$O` = `H$_{2}{}^{17}$O`

<sup>\*H<sub>2</sub><sup>17</sup>O not tested yet</sup>

From these, the abundances of the stable hydrogen and oxygen isotopes with respect to SMOV, `δ` (in permil), are calculated following the general formular
```
δ* = [ (heavy/light)_sample / (heavy/light)_standard - 1 ] * 1000,
```
e.g.
```
δ18O = [ (18O/16O)_sample / (18O/16O)_standard - 1 ] * 1000,
δD   = [ (  D/H  )_sample / (  D/H  )_standard - 1 ] * 1000.
```

These calculations are done either during the model run via `/ace/user/paleo/utils.ace/cosmos-wiso/echam5/calc_wiso_monmean_d.cosmos-aso.sh`:
```bash
# convert 2m-temperature and surface temperature from K to C)
${cdo} -s -f nc -monmean -subc,273.15  -selcode,167 $IN temp2

# convert  precipitation, evaporation and runoff fields from kg/m**2s to mm/month
# (conversion factor: 3600*24*365/12*1000/1000 = 2.628e6)
${cdo} -s -f nc -monmean -mulc,2.628e6 -selcode,142 $IN aprl

# calculate total precipitation
${cdo} -s -f nc -chvar,aprl,aprt -chcode,142,260 -add aprl aprc aprt

# get related water isotope fields and reservoirs
${cdo} -s -f nc -monmean -mulc,2.628e6 -selcode,53 $IN_WISO wisoaprl

# calculate total precipitation
${cdo} -s -f nc -chvar,wisoaprl,wisoaprt -chcode,53,50 -add wisoaprl wisoaprc wisoaprt

# set ECHAM defaults fields to 0. if flux fields (reservoirs) are less than 0.05 mm/month (0.05 mm)
${cdo} -s -f nc -setmisstoc,0. -ifthen -gec,0.05 aprt aprt dummy; mv dummy aprt

# set water isotope fields to 0. if ECHAM default fields (reservoirs) are zero
${cdo} -s -f nc -setmisstoc,0. -ifthen -nec,0. aprt wisoaprt dummy; mv dummy wisoaprt

# calculate delta values of all water isotope fields (reference standard: SMOW) 
# - the SMOW values have to be stored in the file SMOW_FAC (with the correct grid size & order of isotope values!)
${cdo} -s -f nc -chvar,wisoaprt,wisoaprt_d -chcode,50,10 -mulc,1000 -subc,1. -div -div wisoaprt aprt ${SMOW_FAC} wisoaprt_d

# precipitation-weighted temperature:
cdo -s -f nc -chvar,temp2,ptemp -chcode,167,170 -div -mul -yearsum temp2 -yearsum aprt -yearsum aprt ptemp.yearmean
```
or postprocessed via `https://gitlab.awi.de/paleodyn/model-analysis/blob/master/previous_scripts/ANALYSIS_calc_wiso_echam5_yearmean.sh`:
```bash
cdo -s -f nc -chvar,wisoaprt,wisoaprt_d -chcode,50,10 -mulc,1000. -subc,1. -div -div yearsum wisoaprt -yearsum aprt $SMOW_FAC_file wisoaprt_d.yearmean
```
The latter calculation yields precipitation-weighted δ values (through the `yearsum`).

The natural isotope distributions according to SMOW (the standard) are saved in the correspoding `level` dimension of the `SMOW.FAC.*.nc` files (see also `znat` or `tnat` in `setwiso.f90`):
1. H<sub>2</sub><sup>16</sup>O: `1.0`
2. H<sub>2</sub><sup>18</sup>O: `20/18 * 2005.2 * 1e-4 = 0.2228`
3. HD<sup>16</sup>O: `19/18 * 2 * 155.76 * 1e-3 = 0.3288267`
4. H<sub>2</sub><sup>17</sup>O: `379.9 * 1e-3 = 0.3799` (not yet included in `SMOW.FAC.*.nc`)

For H<sub>2</sub><sup>16</sup>O, the `1.0` yields δ<sup>16</sup>O = 0, since <sup>16</sup>O is already to light isotope. The actual SMOW standards for δ<sup>18</sup>O and δD are `2005.2 * 1e-6 = 0.0020052` and `155.76 * 1e-6 = 0.00015576`, respectively (Tab. 2.5 of [Sharp 2017](https://digitalrepository.unm.edu/unm_oer/1/)). However, the wiso model simulates water isotopologues, and not the oxygen and hydrogen isotopes directly. Hence, the SMOW values need to be multiplied by the mass ratios of the heavy to light water isotopologues:
1. H<sub>2</sub><sup>18</sup>O/H<sub>2</sub><sup>16</sup>O = 20/18
2. HD<sup>16</sup>O/H<sub>2</sub><sup>16</sup>O = 19/18

In addition, due to the symmetry of the HD<sup>16</sup>O water molecule, the factor `2` is needed, since both isotopomers (=isotopologues with element order changed) HD<sup>16</sup>O and DH<sup>16</sup>O are possible. The remaining factors are applied to yield numbers which are not too small to work with.

