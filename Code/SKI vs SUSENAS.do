clear
set more off

**---SUSENAS 2023--**

use "C:\Users\ASUS\OneDrive\DATASET BUAT OLAH-OLAH\BPS\Susenas\Susenas Maret 2023 - KOR\kor23_ind_1.dta", replace

** Kode Kabupaten
gen str2 r101_str = string(R101, "%02.0f")
gen str2 r102_str = string(R102, "%02.0f")

* Gabungkan menjadi kode kabupaten/kota
gen str4 kodewil = r101_str + r102_str

** Urban/Rural
clonevar urban = R105 
	replace urban = 0 if urban == 2
label define urb 1 "Urban" 0 "Rural"
label values urban urb

** Jenis Kelamin
clonevar jenis_kelamin = R506 
label define lab_jk 1 "Laki-laki" 2 "Perempuan"

drop if jenis_kelamin > 2

** Usia
clonevar usia = R407 
keep if usia >= 10 & usia <= 75

** Weight
gen FWT_int = ceil(FWT)
replace FWT_int = int(FWT_int)

bysort URUT: gen hitung_rt = (_n == 1)

** Rokok
gen rokok_tembakau = 0
	replace rokok_tembakau = 1 if inlist(R1207, 1, 2)
gen rokok_elektrik = 0
	replace rokok_elektrik = 1 if inlist(R1206, 1, 2)
gen rokok_all = 0
	replace rokok_elektrik = 1 if rokok_tembakau == 1 & rokok_elektrik == 1

** pop
gen pop = 1
	
** collapse
collapse (sum) rokok_tembakau rokok_elektrik rokok_all pop hitung_rt [fw=FWT_int], by(urban jenis_kelamin usia kodewil)

** id
gen source = 1

save "C:\Users\ASUS\OneDrive\CISDI - LOCAL DRIVE\SKI vs SUSENAS\SUSENAS_PREV.dta", replace

**---SKI 2023--**
use "C:\Users\ASUS\OneDrive\DATASET BUAT OLAH-OLAH\BPS\SKI 2023\CISDI\ski_INDIVIDU.dta", clear

** Kode Kabupaten
gen str2 b1r1_str = string(B1R1, "%02.0f")
gen str2 b1r2_str = string(B1R2, "%02.0f")

* Gabungkan menjadi kode kabupaten/kota
gen str4 kodewil = b1r1_str + b1r2_str

** Urban/Rural
clonevar urban = B1R5 
	replace urban = 0 if urban == 2
label define urb 1 "Urban" 0 "Rural"
label values urban urb

** Jenis Kelamin
clonevar jenis_kelamin = B4K4
label define lab_jk 1 "Laki-laki" 2 "Perempuan"

drop if jenis_kelamin > 2

** Usia
clonevar usia = B4K7THN
keep if usia >= 10 & usia <= 75

** Weight
gen FWT_int = ceil(w_final)
replace FWT_int = int(FWT_int)

bysort IDRT: gen hitung_rt = (_n == 1)

** Rokok
gen rokok_all = 0
	replace rokok_all = 1 if inlist(G16, 1, 2)
gen rokok_tembakau = 0
	replace rokok_tembakau = 1 if rokok_all == 1 & (G14A == 1 | G14B == 1 | G14C == 3 | G14E == 4)
gen rokok_elektrik = 0
	replace rokok_elektrik = 1 if rokok_all == 1 & (G14D == 1)

** pop
gen pop = 1

** collapse
collapse (sum) rokok_tembakau rokok_elektrik rokok_all pop hitung_rt [fw=FWT_int], by(urban jenis_kelamin usia kodewil)

** id
gen source = 2

save "C:\Users\ASUS\OneDrive\CISDI - LOCAL DRIVE\SKI vs SUSENAS\SKI_PREV.dta", replace

append using "C:\Users\ASUS\OneDrive\CISDI - LOCAL DRIVE\SKI vs SUSENAS\SUSENAS_PREV.dta"

label define src 1 "SUSENAS" 2 "SKI"
label values source src

save "C:\Users\ASUS\OneDrive\CISDI - LOCAL DRIVE\SKI vs SUSENAS\SUSENAS_SKI_AGR.dta", replace
