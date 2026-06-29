clear
set more off

cd "C:\Users\ASUS-CISDI20\Dropbox\CTFK 2026\smoking-SKI-SUSENAS\Data"

//** SUSENAS 2023

use "D:\dataset\BPS\Susenas\Susenas Maret 2023 - KOR\kor23_ind_1.dta", replace

clonevar no_art = R401
clonevar informan = R410
clonevar age = R407

gen self_fill = .
 replace self_fill = 1 if no_art == informan
 replace self_fill = 0 if self_fill == .
 
gen pop = 1
gen FWT_ceil = ceil(FWT)
gen FWT_int = int(FWT_ceil)

collapse (sum) pop self_fill [fw=FWT_int], by(age)

gen self_fill_prop = self_fill/pop

export excel using "SUSENAS self fill.xlsx", firstrow(variables) replace

//** SKI 2023
clear

use "D:\dataset\BPS\SKI 2023\CISDI\ski_INDIVIDU.dta", clear