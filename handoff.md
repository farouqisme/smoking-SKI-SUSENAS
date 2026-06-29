# Handoff — Analisis Perokok SUSENAS vs SKI

**Tanggal:** 29 Juni 2026
**Author:** Muhammad Akmal Farouqi (CISDI)
**Repo:** https://github.com/farouqisme/smoking-SKI-SUSENAS
**Branch aktif:** `main` (semua pekerjaan sudah di-merge)

---

## Apa yang sudah selesai

### Analisis utama (SUSENAS 2023 vs SKI 2023)
Seluruh pipeline analisis dari EDA hingga inferensial sudah selesai dan ada di `Code/`:

| Script | Status | Output |
|--------|--------|--------|
| `01_EDA_table.R` | ✅ Final | Tabel distribusi populasi & perokok |
| `02_EDA_map.R` | ✅ Final | 12 peta rasio prevalensi per kab/kota (di `Output/Maps/`) |
| `02_EDA_viz.R` | ✅ Final | Visualisasi distribusi umur, rasio kelengkapan |
| `03a_inferential (individual).R` | ✅ Final | Logit individual — OR perokok SKI vs SUSENAS |
| `03b_inferential (kabkota).R` | ✅ Final | OLS agregat kab/kota |
| `04_sample completeness check.R` | ✅ Final | Rasio kelengkapan demografi 514 kab/kota |
| `05_prep podes data.R` | ✅ Final | Prep variabel kontrol aksesibilitas faskes |
| `06_CI with PSU STRATA.R` | ✅ Final | CI 95% dengan desain PSU + strata (paket `survey`) |

### Analisis tren 2015–2024 (untuk coalition/advokasi)
| Script | Status | Output |
|--------|--------|--------|
| `07_additional data cleaning for coalition guidance.R` | ✅ Final | 3 PNG di `Output/`, angka headline di komentar script |

**Angka headline (usia 10+, SUSENAS):**

| Indikator | 2015 | 2024 |
|-----------|------|------|
| Prevalensi perokok tembakau | 26,6% | 26,3% |
| Jumlah perokok tembakau | 55.385.816 (~55,4 jt) | 62.005.937 (~62,0 jt) |
| Prevalensi laki-laki | 52,1% | 51,6% |
| Prevalensi perempuan | 1,1% | 0,9% |
| Jumlah perokok laki-laki | 54.199.773 (~54,2 jt) | 60.913.008 (~60,9 jt) |
| Jumlah perokok perempuan | 1.186.043 (~1,2 jt) | 1.092.929 (~1,1 jt) |

> Pesan kunci: prevalensi sedikit turun, tetapi jumlah absolut naik **+6,6 juta** akibat pertumbuhan penduduk.

### Visualisasi output (di `Output/`)

| File | Deskripsi |
|------|-----------|
| `prev-perokok-umur-2015-2024.png` | Lineplot prevalensi per usia 10–65+, 2015 vs 2024 |
| `jumlah-perokok-umur-2015-2024.png` | Lineplot jumlah perokok (ribu orang) per usia |
| `prev-perokok-pulau-2015-2024.png` | Barplot prevalensi per pulau besar + label + ↑↓ |
| `map-{merokok,tembakau,elektrik}-{all,10-18,26-30,51-55}.png` | 12 peta rasio prevalensi SUSENAS vs SKI |

### Temuan kunci analisis SUSENAS vs SKI 2023
1. Selisih prevalensi perokok tembakau secara nasional: **3,35 pp** (SKI lebih tinggi)
2. Perbedaan terkonsentrasi pada usia **<20 tahun dan >40 tahun** (pola U-terbalik)
3. Individu SKI **1,13× lebih mungkin** terklasifikasi sebagai perokok tembakau vs SUSENAS (p < 0,05)
4. Perokok elektrik: individu SKI **50,1% lebih rendah** probabilitasnya vs SUSENAS
5. SUSENAS lebih unggul dalam kelengkapan demografi: min. 54,6% (Buton Selatan) vs SKI 17,5% (Puncak)

---

## Struktur repo

```
smoking-SKI-SUSENAS/
├── Code/          14 script R + Stata, bernomor urut analisis
├── Data/          File data olahan kecil (xlsx referensi); file mentah .dta/.csv di-ignore
├── Output/        PNG, DOCX, XLSX hasil analisis
├── README.md      Dokumentasi proyek lengkap
└── handoff.md     File ini
```

---

## Data yang dibutuhkan (tidak ada di repo)

File-file ini terlalu besar dan di-ignore via `.gitignore`. Harus tersedia lokal di path yang tertera di masing-masing script:

| File | Path (di script) | Ukuran |
|------|-----------------|--------|
| `susenas15mar_ki.dta` | `D:/dataset/BPS/Susenas/Susenas Maret 2015 - KOR/` | ~besar |
| `ssn202403_kor_ind1.dbf` | `D:/dataset/BPS/Susenas/Susenas Maret 2024 - KOR/` | ~besar |
| `kor23_ind_1.dta` | `D:/dataset/BPS/Susenas/Susenas Maret 2023 - KOR/` | ~besar |
| `ski_INDIVIDU.dta` | `D:/dataset/BPS/SKI 2023/CISDI/` | ~besar |
| Shapefile Indonesia kab/kota | `C:/Users/ASUS/OneDrive/DATASET .../idn_adm_bps...shp` | — |

> **Catatan:** Path di atas adalah path lokal penulis. Jika menjalankan di mesin lain, sesuaikan semua baris `import`/`st_read` di masing-masing script.

---

## Hal yang belum / bisa dilanjutkan

- [ ] **PAF (Population Attributable Fraction)** — `Data/by age jk for PAF (2015).xlsx` sudah disiapkan sebagai input, tapi script perhitungan PAF belum ada
- [ ] **Analisis subgroup lebih lanjut** — tren 2015–2024 baru pada level nasional dan pulau besar; belum per provinsi atau urban/rural
- [ ] **Validasi angka dengan bobot kompleks (PSU/strata)** — script 07 menggunakan weighted sum sederhana (bukan `svymean`); untuk CI yang akurat gunakan pola di script 06
- [ ] **Visualisasi jenis kelamin × umur** — jumlah perokok laki-laki vs perempuan per usia belum divisualisasikan
- [ ] **Update laporan Word** — `[EXT] Report SUSENAS vs SKI.docx` di folder Downloads belum diupdate dengan temuan tren 2015–2024

---

## Environment

- **R:** 4.5.2 (`C:\Program Files\R\R-4.5.2\bin\Rscript.exe`)
- **Paket R:** tidyverse, rio, survey, ggplot2, sf, patchwork, gt, janitor, scales, ggspatial
- **Stata:** diperlukan untuk script `.do` (versi 14+)
- **OS:** Windows 11
