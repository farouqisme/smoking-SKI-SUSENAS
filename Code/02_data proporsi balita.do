clear
set more off

**---SKI 2023--**
use "C:\Users\ASUS\OneDrive\DATASET BUAT OLAH-OLAH\BPS\SKI 2023\CISDI\ski_INDIVIDU.dta", clear

** Kode Kabupaten
gen str2 b1r1_str = string(B1R1, "%02.0f")
gen str2 b1r2_str = string(B1R2, "%02.0f")

* Gabungkan menjadi kode kabupaten/kota
gen str4 kodewil = b1r1_str + b1r2_str
clonevar kodeprov = b1r1_str

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
clonevar balita = B4K7BLN
replace balita = 0 if balita == 0
replace balita = 1 if balita > 0

** Weight
gen FWT_int = ceil(w_final)
replace FWT_int = int(FWT_int)

bysort IDRT: gen hitung_rt = (_n == 1)

** pop
gen pop = 1

collapse (sum) pop balita hitung_rt [fw=FWT_int], by(kodewil kodeprov)
gen balita_pop_perc = balita/pop
gen balita_rt_perc = balita/hitung_rt

drop pop hitung_rt

save "D:\smoking-SKI-SUSENAS\Data\proporsi balita.dta", replace

** ganti kodewil SKI menjadi kodewil SUSENAS 2023
use "D:\smoking-SKI-SUSENAS\Data\relasi kode kabupaten susenas ski.dta", clear

rename kodewil_ski kodewil

merge 1:m kodewil using "D:\smoking-SKI-SUSENAS\Data\proporsi balita.dta"

drop _merge kodewil_susenas kodewil kodeprov

rename kodewil_susenaspre2022 kodewil

save "D:\smoking-SKI-SUSENAS\Data\proporsi balita.dta", replace

