# Dashboard Statistik Terpadu - SOVI Analysis

## Deskripsi Dashboard

Dashboard Statistik Terpadu adalah aplikasi web interaktif yang dikembangkan menggunakan R Shiny untuk analisis data SOVI (Social Vulnerability Index) secara komprehensif. Dashboard ini menyediakan berbagai fitur analisis statistik mulai dari eksplorasi data dasar hingga analisis regresi yang kompleks.

## Data yang Digunakan

- **Dataset Utama**: SOVI (Social Vulnerability Index) Data
- **Sumber**: Scientific Data Journal - Nature
- **URL Data**: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv
- **URL Matrik Penimbang**: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv
- **URL Metadata**: https://www.sciencedirect.com/science/article/pii/S2352340921010180

## Fitur Dashboard

### 1. Beranda
- Informasi lengkap tentang dashboard
- Metadata dataset SOVI
- Penjelasan fitur-fitur yang tersedia
- Manual pengguna

### 2. Manajemen Data
- Upload file CSV atau load data default dari URL
- Preview dan ringkasan data
- **Transformasi variabel kontinyu ke kategorik:**
  - Kategorisasi berdasarkan kuantil
  - Kategorisasi custom dengan breakpoint manual
  - Transformasi logaritma, square root, standardisasi
- **Interpretasi** untuk setiap metode transformasi
- Download data yang sudah ditransformasi

### 3. Eksplorasi Data

#### 3a. Statistik Deskriptif
- Mean, median, standar deviasi, min, max, skewness, kurtosis
- Analisis berdasarkan kelompok
- **Interpretasi lengkap** untuk setiap output

#### 3b. Visualisasi
- Scatter plot, box plot, histogram, correlation matrix, bar chart, density plot
- Visualisasi interaktif dengan plotly
- **Interpretasi** untuk setiap jenis visualisasi
- Download dalam format JPG dan PDF

#### 3c. Pemetaan
- Heat map, choropleth, point map dengan Leaflet
- Peta interaktif dengan koordinat geografis
- **Interpretasi pola spasial**

### 4. Uji Asumsi Data
- **Uji Normalitas**: Shapiro-Wilk test / Anderson-Darling
- **Uji Homogenitas**: Levene's test
- **Interpretasi statistik** untuk setiap uji
- Visualisasi Q-Q plot dan histogram

### 5. Statistik Inferensia

#### 5a. Uji Rata-rata
- One sample t-test
- Two sample t-test
- Paired t-test
- **Interpretasi hasil** dengan keputusan H₀/H₁

#### 5b. Uji Proporsi & Varians
- Uji proporsi 1 dan 2 sampel
- Uji varians 1 dan 2 sampel
- **Interpretasi hasil** dengan kesimpulan statistik

#### 5c. ANOVA
- One-way ANOVA
- Two-way ANOVA dengan/tanpa interaksi
- Post-hoc test (Tukey HSD)
- **Interpretasi komprehensif** hasil ANOVA

### 6. Regresi Linear Berganda
- Model regresi dengan multiple predictors
- **Uji asumsi regresi**: Normalitas residual, homoskedastisitas, multikolinearitas
- **Diagnostik model**: Cook's distance, leverage points
- Plot diagnostik lengkap
- **Interpretasi model** dengan R-squared, signifikansi

### 7. Metadata
- Informasi lengkap dataset SOVI
- Deskripsi variabel-variabel
- Metodologi dan aplikasi
- Sitasi artikel ilmiah

## Instalasi dan Penggunaan

### Prasyarat
```r
# Install packages yang diperlukan
install.packages(c("shiny", "shinydashboard", "DT", "plotly", "ggplot2", 
                   "corrplot", "leaflet", "htmlwidgets", "knitr", "rmarkdown", 
                   "openxlsx", "VIM", "mice", "nortest", "car", "broom", 
                   "dplyr", "gridExtra", "sf", "maps", "moments"))
```

### Menjalankan Dashboard
```r
# Method 1: Direct run
shiny::runApp('dashboard_statistik_terpadu.R')

# Method 2: Dengan pengaturan host dan port
shiny::runApp('dashboard_statistik_terpadu.R', host='0.0.0.0', port=8080)
```

### Akses Dashboard
Setelah dijalankan, dashboard akan terbuka di browser pada alamat:
- Local: http://localhost:8080
- Network: http://[your-ip]:8080

## Fitur Teknis

- **Platform**: R Shiny
- **Package Utama**: shiny, ggplot2, plotly, DT, leaflet
- **Format Output**: PDF, Word, Excel, JPG
- **Responsif**: Ya, dapat diakses di desktop dan mobile
- **Data Loading**: Otomatis dari URL dengan fallback
- **Error Handling**: Robust error handling
- **Single File**: Seluruh dashboard dalam satu file R

## Cara Penggunaan Dashboard

1. **Mulai dari Beranda**: Baca informasi dan metadata
2. **Manajemen Data**: Load data SOVI atau upload data sendiri
3. **Eksplorasi Data**: Lakukan analisis deskriptif dan visualisasi
4. **Uji Asumsi**: Pastikan data memenuhi asumsi statistik
5. **Statistik Inferensia**: Jalankan uji hipotesis sesuai kebutuhan
6. **Regresi Linear**: Analisis hubungan antar variabel
7. **Download Hasil**: Semua output dapat didownload

## Output yang Dapat Didownload

- **Gambar**: JPG format untuk visualisasi
- **Laporan**: PDF/Word format untuk hasil analisis
- **Data**: CSV/Excel format untuk data yang sudah diproses
- **Manual**: PDF format untuk panduan pengguna

## Interpretasi Otomatis

Setiap analisis dilengkapi dengan interpretasi otomatis yang mencakup:
- Penjelasan hasil statistik
- Kesimpulan uji hipotesis
- Rekomendasi tindak lanjut
- Konteks praktis dari hasil analisis

## Dukungan dan Bantuan

Jika mengalami kendala:
1. Periksa koneksi internet untuk loading data dari URL
2. Pastikan semua package R telah terinstall
3. Gunakan data sample jika URL tidak tersedia
4. Download manual pengguna dari menu Beranda

## Lisensi

Dashboard ini dikembangkan untuk keperluan edukasi dan penelitian. Data SOVI bersumber dari artikel ilmiah yang dipublikasikan di Scientific Data Journal - Nature.

## Sitasi

Jika menggunakan dashboard ini untuk penelitian, mohon sitasi artikel original:

Flanagan, B.E., Hallisey, E.J., Adams, E. et al. A county-level dataset for informing the United States social vulnerability to environmental hazards. Sci Data 8, 290 (2021). https://doi.org/10.1038/s41597-021-01080-w