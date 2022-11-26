set more off
clear all

if c(username)=="research" { //insert username
cd "/Users/research/Desktop/valeska-honors-new/" // insert root path
}

cd "data-clean/tract-zonal-statistics-clean/2019"

append using `: dir . files "*.dta"'

/*
//ssc install fs
fs "*.dta"
append using `r(files)'
*/

save "/Users/research/Desktop/valeska-honors-new/data-clean/Monthly/2019_Wide.dta", replace

rename CPM2_5_V5GL02_HPM25_NA_201901_2 pm_25_201901
rename CPM2_5_V5GL02_HPM25_NA_201902_2 pm_25_201902
rename CPM2_5_V5GL02_HPM25_NA_201903_2 pm_25_201903
rename CPM2_5_V5GL02_HPM25_NA_201904_2 pm_25_201904
rename CPM2_5_V5GL02_HPM25_NA_201905_2 pm_25_201905
rename CPM2_5_V5GL02_HPM25_NA_201906_2 pm_25_201906
rename CPM2_5_V5GL02_HPM25_NA_201907_2 pm_25_201907
rename CPM2_5_V5GL02_HPM25_NA_201908_2 pm_25_201908
rename CPM2_5_V5GL02_HPM25_NA_201909_2 pm_25_201909
rename CPM2_5_V5GL02_HPM25_NA_201910_2 pm_25_201910
rename CPM2_5_V5GL02_HPM25_NA_201911_2 pm_25_201911
rename CPM2_5_V5GL02_HPM25_NA_201912_2 pm_25_201912
duplicates drop

reshape long pm_25_, i(GEOID_1) j(ym)
save "/Users/research/Desktop/valeska-honors-new/data-clean/Monthly/2019_Long.dta", replace

tostring ym, gen(str_ym) 
gen year = substr(str_ym, 1,4)
gen month = substr(str_ym, 5,2)

replace pm_25_ = pm_25_ / 100
drop ym str_ym
reshape wide pm_25_, i(GEOID_1 year) j(month) string
destring year, replace


save "/Users/research/Desktop/valeska-honors-new/data-clean/panel_2019.dta", replace

set more off
if c(username)=="research" { //insert username
cd "/Users/research/Desktop/valeska-honors-new/" // insert root path
}
cd "data-clean/tract-zonal-statistics"
fs "*.dta" display `SigmaMax'
