# Dashboard Analisis Data Statistik Komprehensif

## Deskripsi
Dashboard R Shiny ini menyediakan analisis data statistik yang komprehensif dengan antarmuka yang user-friendly. Dashboard ini dirancang untuk memudahkan pengguna dalam melakukan berbagai analisis statistik tanpa perlu menulis kode R secara manual.

## Fitur Utama

### 1. Beranda
- **Metadata Dashboard**: Informasi lengkap tentang fitur-fitur yang tersedia
- **Informasi Teknis**: Versi, library yang digunakan, dan panduan penggunaan
- **Tampilan Data**: Menampilkan data yang telah diunggah dalam format tabel interaktif
- **Download**: Ringkasan data dalam format TXT

### 2. Manajemen Data
- **Kategorisasi Data**: Mengubah data kontinyu menjadi kategorik
- **Metode Kategorisasi**: 
  - Quantile: Berdasarkan kuartil
  - Equal Width: Berdasarkan lebar interval yang sama
- **Interpretasi**: Penjelasan hasil kategorisasi
- **Download**: Data hasil kategorisasi dalam format CSV

### 3. Eksplorasi Data
- **Statistik Deskriptif**: Mean, median, modus, standar deviasi, dll.
- **Visualisasi Data**:
  - Histogram dengan kurva densitas
  - Boxplot (individual atau berdasarkan grup)
  - Density plot
  - Scatter plot dengan garis regresi
- **Tabel Frekuensi**: Distribusi data kategorik atau binning data numerik
- **Peta Interaktif**: Visualisasi data spasial (jika tersedia koordinat)
- **Interpretasi**: Penjelasan distribusi dan variabilitas data
- **Download**: Plot (JPG) dan statistik deskriptif (PDF)

### 4. Uji Asumsi Data
- **Uji Normalitas**: 
  - Shapiro-Wilk test (n ≤ 5000)
  - Kolmogorov-Smirnov test (n > 5000)
- **Uji Homogenitas**: Levene's test untuk kesamaan varians
- **Interpretasi**: Penjelasan hasil uji dengan rekomendasi
- **Download**: Hasil uji individual atau gabungan (PDF)

### 5. Statistik Inferensia

#### 5.1 Uji Beda Rata-rata
- **Uji T Satu Sampel**: Membandingkan rata-rata sampel dengan nilai hipotesis
- **Uji T Dua Sampel**: Membandingkan rata-rata dua kelompok independen
- **Opsi**: Asumsi varians sama atau berbeda
- **Interpretasi**: Kesimpulan statistik dengan konteks praktis
- **Download**: Hasil uji dalam format TXT

#### 5.2 Uji Proporsi & Varians
- **Uji Proporsi Satu Sampel**: Membandingkan proporsi sampel dengan proporsi hipotesis
- **Uji Proporsi Dua Sampel**: Membandingkan proporsi dua kelompok
- **Uji Varians Satu Sampel**: Chi-square test untuk varians
- **Uji Varians Dua Sampel**: F-test untuk perbandingan varians
- **Interpretasi**: Penjelasan hasil dengan signifikansi praktis

#### 5.3 ANOVA (Analysis of Variance)
- **ANOVA Satu Arah**: Membandingkan rata-rata lebih dari 2 kelompok
- **ANOVA Dua Arah**: Analisis dengan dua faktor (dengan/tanpa interaksi)
- **Interpretasi**: Penjelasan F-statistic dan post-hoc analysis
- **Download**: Hasil ANOVA dalam format TXT

### 6. Regresi Linear Berganda
- **Model Building**: Membangun model dengan multiple predictors
- **Ringkasan Model**: R-squared, adjusted R-squared, F-statistic
- **Plot Diagnostik**: Residual plots, Q-Q plot, leverage plot
- **Uji Asumsi Regresi**:
  - Normalitas residual
  - Autokorelasi (Durbin-Watson)
  - Multikolinearitas (VIF)
- **Prediksi**: Tabel prediksi vs aktual dengan residual
- **Interpretasi**: Penjelasan koefisien dan asumsi model
- **Download**: Model (PDF), plot diagnostik (JPG), prediksi (CSV)

## Instalasi dan Penggunaan

### Prasyarat
Pastikan R dan RStudio sudah terinstal, serta package berikut:

```r
# Install packages yang diperlukan
install.packages(c(
  "shiny", "shinydashboard", "dplyr", "ggplot2", "DT", "readr",
  "car", "rstatix", "rmarkdown", "leaflet", "plotly", "corrplot",
  "knitr", "gridExtra", "flextable", "officer", "openxlsx", "lmtest"
))
```

### Cara Menjalankan
1. Buka RStudio
2. Set working directory ke folder yang berisi file dashboard
3. Jalankan perintah:
```r
rmarkdown::run("dashboard.rmd")
```

### Cara Menggunakan
1. **Upload Data**: Klik "Browse" dan pilih file CSV Anda
2. **Pilih Menu**: Navigasi menggunakan sidebar menu
3. **Analisis**: Pilih variabel dan parameter yang diinginkan
4. **Interpretasi**: Baca hasil dan interpretasi yang disediakan
5. **Download**: Unduh hasil dalam format yang diinginkan

## Format Data
Dashboard ini mendukung file CSV dengan ketentuan:
- Baris pertama berisi nama kolom/variabel
- Data numerik untuk analisis kuantitatif
- Data kategorik/faktor untuk analisis kualitatif
- Koordinat (latitude, longitude) untuk visualisasi peta (opsional)

### Contoh Struktur Data
```csv
id,nama,jenis_kelamin,usia,tinggi_badan,berat_badan,pendidikan,gaji,latitude,longitude
1,Ahmad,Laki-laki,25,170,65,S1,8000000,-6.2088,106.8456
2,Siti,Perempuan,30,160,55,S2,15000000,-6.1754,106.8272
...
```

## Fitur Download
Setiap menu menyediakan opsi download:
- **JPG**: Grafik dan visualisasi
- **PDF**: Laporan dan statistik
- **TXT**: Hasil uji statistik
- **CSV**: Data dan prediksi
- **Word**: Laporan komprehensif (dalam pengembangan)

## Tips Penggunaan
1. **Persiapan Data**: Pastikan data bersih dan tidak ada missing values yang berlebihan
2. **Pemilihan Variabel**: Pilih variabel yang sesuai dengan jenis analisis
3. **Interpretasi**: Selalu baca interpretasi yang disediakan untuk memahami hasil
4. **Asumsi**: Lakukan uji asumsi sebelum analisis inferensial
5. **Dokumentasi**: Gunakan fitur download untuk dokumentasi hasil

## Troubleshooting
- **Error Upload**: Pastikan file berformat CSV dengan encoding UTF-8
- **Variabel Tidak Muncul**: Periksa tipe data (numerik/kategorik)
- **Plot Tidak Muncul**: Klik tombol "Generate Plot" atau refresh browser
- **Download Gagal**: Pastikan tidak ada karakter khusus dalam nama variabel

## Pengembangan Selanjutnya
- [ ] Export ke Word document
- [ ] Analisis time series
- [ ] Machine learning algorithms
- [ ] Interactive plotly charts
- [ ] Database connectivity
- [ ] User authentication

## Kontribusi
Untuk kontribusi dan saran perbaikan, silakan hubungi pengembang atau buat issue di repository ini.

## Lisensi
Dashboard ini dikembangkan untuk tujuan edukasi dan penelitian. Penggunaan komersial memerlukan izin khusus.

---
**Versi**: 2.0  
**Terakhir Diperbarui**: 2024  
**Pengembang**: Tim Analisis Data Statistik