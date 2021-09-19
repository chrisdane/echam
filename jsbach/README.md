# jsbach

# land cover types or PFTs

- subgridscale land cover types in fraction of grid cell, i.e. no geographic location within gridcell
- either PFT or glacier
- jsbach readme 1.3 basic concepts; equations 1.1 to 1.12:

```
# model variables:
cover_fract(lon,lat,lev,time) = Land Cover Fraction [] not with respect to cell area but vegetation area
veg_ratio_max(lon,lat,time) = Maximum Vegetation Fraction []
area_cell_m2(lon,lat) = cell area [m2]

# eq. 1.1
                       V_veg = A * veg_max
=> area_veg_m2(lon,lat,time) = area_cell_m2(lon,lat) * veg_ratio_max(lon,lat,time)

# eq. 1.5
                                   f = c * veg_max
=> cover_fract_box(lon,lat,lev,time) = cover_fract(lon,lat,lev,time) * veg_ratio_max(lon,lat,time)

# todo:
jsbach_nc=/mnt/lustre02/work/ba1103/a270094/AWIESM/test/input/jsbach/jsbach.nc
cover_fract_nc="/work/ba1103/a270073/post/jsbach/select/cover_fract/awi-esm-1-1-lr_kh800_piControl_og_jsbach_select_cover_fract_global_Jan-Dec_1950-1951.nc"
veg_ratio_max_nc="/work/ba1103/a270073/post/jsbach/select/veg_ratio_max/awi-esm-1-1-lr_kh800_piControl_og_jsbach_select_veg_ratio_max_global_Jan-Dec_1950-1951.nc"
cdo -subc,1 -setrtomiss,3,100 -setrtomiss,0,1 -selvar,cover_type jsbach.nc cover_type_mask.nc
```

## landcover types: lctlib

from Table C.21 from jsbach docu p. 193:
```
#  1: tropical broadleaf evergreen 
#  2: tropical broadleaf deciduous
#  3: extra-tropical evergreen
#  4: extra-tropical deciduous
#  5: raingreen shrubs
#  6: deciduous shrubs
#  7: C3 grass
#  8: C4 grass
#  9: C3 pasture
# 10: C4 pasture
# 11: C3/C4 crop
```

variable `cover_type(lon,lat,ntiles=11)` from `input/jsbach.nc`:
```
| tile | cover_type | lctlib PFT |
| -------|-------------|--------|
| 1 | 2 | Tropical evergreen trees | 
| 2 | 3 | Tropical deciduous trees |
| 3 | 4 | Extra-tropical evergreen trees |
| 4 | 5 | Extra-tropical deciduous trees |
| 5 | 10 | Raingreen shrubs |
| 6 | 11 | Deciduous shrubs |
| 7 | 12 | C3 grass |
| 8 | 13 | C4 grass |
| 9 | 15 | C3 pasture |
| 10 | 16 | C4 pasture |
| 11 | 20 | C3 crop |
```

from `${echam.model_dir}/lctlib_nlct21.def`:
```
#  1: Glacier                     
#  2: Tropical evergreen trees                     
#  3: Tropical deciduous trees                     
#  4: Extra-tropical evergreen trees                     
#  5: Extra-tropical deciduous trees                     
#  6: Temperate broadleaf evergreen trees                     
#  7: Temperate broadleaf deciduous trees                     
#  8: Coniferous evergreen trees                     
#  9: Coniferous deciduous trees                     
# 10: Raingreen shrubs                     
# 11: Deciduous shrubs                     
# 12: C3 grass                     
# 13: C4 grass                     
# 14: Pasture                     
# 15: C3 pasture                     
# 16: C4 pasture                     
# 17: Tundra                     
# 18: Swamp                     
# 19: Crops                     
# 20: C3 crop                     
# 21: C4 crop                     
NLCT 21    # number of landcover types
LctNumber 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
#LandcoverClass (bare soil: 0; glacier: 1; lake: 2; natural forest: 3; natural grassland: 4; other natural vegetation: 5; crops: 6; pastures: 7)                     
LandcoverClass 1 3 3 3 3 3 3 3 3 5 5 4 4 7 7 7 5 5 6 6 6
```

from `lctlib_nlct11.def`:
```
#  1: Tropical evergreen trees
#  2: Tropical deciduous trees
#  3: Extra-tropocal evergreen trees
#  4: Extra-tropical deciduous trees
#  5: Raingreen shrubs
#  6: Cold shrubs
#  7: C3 grass
#  8: C4 grass
#  9: Crops
# 10: Pasture
# 11: Glacier
```

# convert

```
cdo -f nc copy -setcodetab,Hol-T_jsbach.codes Hol-T_jsbach_290301.grb Hol-T_jsbach_290301.grb.nc
```

