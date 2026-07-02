# Analisis Distribusi Perokok: SUSENAS vs SKI

Repositori ini berisi seluruh kode analisis untuk studi perbandingan distribusi perokok antara **SUSENAS** (Badan Pusat Statistik) dan **SKI** (Kementerian Kesehatan), serta analisis tren prevalensi perokok tembakau **2015–2024** menggunakan SUSENAS.

Laporan lengkap tersedia melalui tautan yang dicantumkan dalam dokumen resmi proyek.

---

## Latar Belakang

Prevalensi perokok antara SUSENAS 2023 dan SKI 2023 menunjukkan perbedaan yang signifikan, terutama pada kelompok usia muda (10–18 tahun) dan usia dewasa-tua (>40 tahun). Perbedaan ini berpotensi muncul akibat perbedaan desain sampling antara kedua survei. Di sisi lain, penggunaan dua data yang berbeda di berbagai instansi pemerintah — Kemenpora menggunakan SUSENAS untuk Indeks Pembangunan Pemuda, sementara Kemenkes menggunakan SKI untuk renstra — membuat harmonisasi data menjadi krusial untuk keperluan advokasi pengendalian tembakau.

---

## Pertanyaan Penelitian

1. Apakah terdapat perbedaan distribusi populasi antara data SUSENAS dan SKI pada beragam kategori demografi?
2. Apakah terdapat perbedaan signifikan antara prevalensi perokok, perokok tembakau, dan perokok elektrik pada data SUSENAS dan SKI? Apa faktor determinan yang memengaruhi perbedaan tersebut?

---

## Data

| Sumber | Tahun | Keterangan |
|--------|-------|------------|
| SUSENAS KOR (BPS) | 2015, 2023, 2024 | Data individu usia 10+ |
| SKI (Kemenkes) | 2023 | Data individu usia 10–75 |
| PODES (BPS) | 2021 | Data fasilitas kesehatan kab/kota |

> **Catatan:** File data mentah (`.dta`, `.csv`) tidak disimpan di repositori karena ukurannya melebihi 10 MB. Jalur impor data perlu disesuaikan pada baris `import` di masing-masing script.

---

## Struktur Folder

```
smoking-SKI-SUSENAS/
├── Code/          # Script R dan Stata, bernomor sesuai urutan analisis
├── Data/          # File data olahan (file mentah di-ignore)
├── Output/        # Visualisasi (PNG) dan tabel hasil (XLSX, DOCX)
└── README.md
```

### Deskripsi Script (Code/)

| Script | Bahasa | Fungsi |
|--------|--------|--------|
| `01_EDA_table.R` | R | Tabel deskriptif distribusi populasi dan perokok |
| `01_data level individu.do` | Stata | Persiapan data tingkat individu |
| `01_data level region.do` | Stata | Persiapan data tingkat wilayah |
| `02_EDA_map.R` | R | Peta rasio prevalensi SUSENAS vs SKI per kab/kota |
| `02_EDA_viz.R` | R | Visualisasi EDA distribusi perokok |
| `02_data populasi untuk komparasi.do` | Stata | Data populasi komparasi |
| `02_data proporsi balita.do` | Stata | Proporsi rumah tangga dengan balita |
| `03a_inferential (individual).R` | R | Regresi logistik tingkat individu |
| `03b_inferential (kabkota).R` | R | Regresi OLS tingkat kab/kota |
| `04_sample completeness check.R` | R | Rasio kelengkapan demografi per kab/kota |
| `05_prep podes data.R` | R | Persiapan data PODES untuk variabel kontrol |
| `06_CI with PSU STRATA.R` | R | Estimasi prevalensi dengan CI 95% mempertimbangkan desain sampling (PSU + strata) |
| `06_identifikasi sumber informan.do` | Stata | Identifikasi sumber responden jawaban survei |
| `07_additional data cleaning for coalition guidance.R` | R | Data cleaning + visualisasi tren perokok tembakau 2015–2024 untuk kebutuhan advokasi |

---

## Cara Menjalankan

**Prerequisites:** R 4.x dengan paket berikut:

```r
install.packages(c("tidyverse", "rio", "survey", "ggplot2", "sf",
                   "patchwork", "gt", "janitor", "scales", "ggspatial"))
```

Untuk script Stata (`.do`), diperlukan Stata 14+.

Jalankan script berurutan: `01` → `02` → `03` → `04` → `05` → `06` → `07`. Sesuaikan jalur impor data di baris `import` setiap script dengan direktori lokal Anda.

---

## Temuan Utama

### Tren 2015–2024 (SUSENAS, usia 10+)

| Indikator | 2015 | 2024 |
|-----------|------|------|
| Prevalensi perokok tembakau | 26,6% | 26,3% |
| Jumlah perokok tembakau | ~55,4 juta | ~62,0 juta |
| Prevalensi laki-laki | 52,1% | 51,6% |
| Prevalensi perempuan | 1,1% | 0,9% |

> Meskipun prevalensi sedikit turun, jumlah absolut perokok meningkat ~6,6 juta akibat pertumbuhan penduduk.

#### Perubahan jumlah perokok per umur tunggal (10–75)

Dua grafik pengembangan (per umur tunggal, tanpa kelompok umur) memisahkan efek perubahan pada tiap usia:

- **Net perubahan jumlah absolut** (2024 − 2015): total **+6,44 juta** perokok pada rentang 10–75 tahun.
- **Growth agregat 10–75:** **+11,8%**.
- Kenaikan **terkonsentrasi pada usia ≥40 tahun** (net +6,71 juta; growth +27,1%), sedangkan usia <20 tahun nyaris datar (net +29 ribu; +1,3%).
- Terdapat **penurunan** jumlah perokok pada usia sekitar **25–35 tahun** (terdalam di usia 35: −239 ribu; usia 29: −206 ribu).
- Kenaikan bersih terbesar pada usia paruh baya (usia 53: +503 ribu; usia 48: +468 ribu; usia 43: +441 ribu).

> Data pendukung per umur tersedia di `Data/perokok absolut per umur 2015-2024.xlsx` (kolom `usia`, `perokok_2015`, `perokok_2024`) untuk perhitungan lanjutan.

#### Growth jumlah perokok absolut per pulau (usia 10+)

Semua pulau mengalami **kenaikan** jumlah perokok absolut 2015→2024: Maluku **+26,2%**, Bali-Nusra **+21,0%**, Sumatera **+12,3%**, Kalimantan **+11,6%**, Jawa **+11,2%**, Sulawesi **+9,8%**, Papua **+7,5%**. Secara absolut kenaikan terbesar tetap di Jawa (~32,8 → ~36,5 juta) dan Sumatera. (Grafik: `Output/growth-perokok-pulau-2015-2024.png`.)

### Perbandingan SUSENAS vs SKI 2023

- Perbedaan prevalensi perokok tembakau antar survei mencapai **3,35 pp** secara nasional.
- Perbedaan terkonsentrasi pada usia **<20 tahun** dan **>40 tahun** (pola U-terbalik).
- SUSENAS memiliki kelengkapan demografi lebih tinggi di 303 dari 514 kab/kota; nilai minimum kelengkapan SUSENAS (54,6%) jauh di atas SKI (17,5%).
- Hasil regresi logistik: individu SKI **1,13× lebih mungkin** terklasifikasi sebagai perokok tembakau dibanding SUSENAS (signifikan pada p < 0,05), tetapi **50% lebih rendah** untuk perokok elektrik.

---

## Penulis

**Muhammad Akmal Farouqi**
Center for Indonesia's Strategic Development Initiatives (CISDI)

---

## Lisensi

MIT License © 2026 Muhammad Akmal Farouqi
