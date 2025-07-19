# STATeddy Dashboard - Analisis Statistik Terpadu

Dashboard R Shiny untuk analisis statistik komprehensif menggunakan data **Social Vulnerability Index (SOVI)** dan **Distance Matrix**.

## Data Yang Digunakan

### 1. Data SOVI (Social Vulnerability Index)
**Sumber:** https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv

**Deskripsi:** 
Data ini merupakan implementasi dari Social Vulnerability Index yang mengukur kerentanan sosial komunitas terhadap bencana dan gangguan eksternal. Data ini berdasarkan pada penelitian yang dipublikasikan di ScienceDirect (https://www.sciencedirect.com/science/article/pii/S2352340921010180).

**Variabel Utama:**
- **ID**: Identifier unik untuk setiap observasi
- **State/County**: Lokasi geografis
- **Population**: Jumlah populasi
- **Income**: Pendapatan per kapita
- **Education**: Tingkat pendidikan (persentase yang lulus SMA)
- **Age_65_Over**: Persentase populasi berusia 65 tahun ke atas
- **Disability**: Persentase populasi dengan disabilitas
- **SOVI_Score**: Skor kerentanan sosial (standardized score)
- **Latitude/Longitude**: Koordinat geografis

**Interpretasi SOVI Score:**
- Skor positif (+): Lebih rentan secara sosial
- Skor negatif (-): Kurang rentan secara sosial
- Skor mendekati 0: Tingkat kerentanan rata-rata

### 2. Data Distance Matrix
**Sumber:** https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv

**Deskripsi:**
Matriks jarak yang menunjukkan jarak spasial atau similaritas antar observasi dalam dataset SOVI. Matriks ini digunakan untuk analisis clustering dan pengelompokan berdasarkan kedekatan geografis atau karakteristik sosial.

**Struktur Data:**
- Matriks simetrik n×n dimana n adalah jumlah observasi
- Nilai diagonal = 0 (jarak dari satu titik ke dirinya sendiri)
- Nilai off-diagonal = jarak/dissimilarity antar titik

## Hubungan Antara Data

Kedua dataset **saling berkaitan** dengan cara berikut:

1. **Spatial Relationship**: Distance matrix menyediakan informasi jarak geografis antar area yang diukur dalam SOVI data
2. **Analytical Connection**: Kombinasi keduanya memungkinkan analisis:
   - Spatial clustering berdasarkan kerentanan sosial
   - Identifikasi pola geografis kerentanan
   - Analisis regional vulnerability hotspots

3. **Methodological Integration**: 
   - SOVI data memberikan karakteristik sosial-ekonomi
   - Distance matrix memungkinkan analisis spatial dependency
   - Keduanya bersama-sama mendukung analisis spatio-social vulnerability

## Analisis Yang Dapat Dilakukan

### 1. Analisis Deskriptif
- Distribusi skor kerentanan sosial
- Profil demografis dan sosial-ekonomi
- Statistik summary per variabel

### 2. Analisis Spasial
- Pemetaan kerentanan sosial
- Identifikasi cluster geografis
- Analisis pola spasial (hotspots/coldspots)

### 3. Analisis Clustering
- **Hierarchical Clustering**: Menggunakan distance matrix untuk mengelompokkan area berdasarkan kedekatan
- **K-means Clustering**: Pengelompokan berdasarkan karakteristik sosial
- **DBSCAN**: Deteksi cluster dengan kepadatan berbeda
- **PAM (Partitioning Around Medoids)**: Clustering robust terhadap outlier

### 4. Analisis Inferensia
- Uji perbedaan rata-rata antar cluster
- Uji korelasi antar variabel sosial
- Analisis varians (ANOVA) untuk membandingkan grup

### 5. Analisis Regresi
- Prediksi skor SOVI berdasarkan variabel demografis
- Identifikasi faktor yang paling mempengaruhi kerentanan sosial
- Model hubungan spasial

## Aplikasi Praktis

### 1. Perencanaan Bencana
- Identifikasi area yang membutuhkan perhatian khusus
- Alokasi sumber daya berdasarkan tingkat kerentanan
- Pengembangan strategi mitigasi yang targeted

### 2. Kebijakan Publik
- Perencanaan program sosial berdasarkan kebutuhan area
- Prioritisasi investasi infrastruktur
- Pengembangan kebijakan yang context-specific

### 3. Penelitian Akademik
- Studi tentang determinan kerentanan sosial
- Analisis efektivitas intervensi
- Pengembangan metodologi assessment vulnerability

## Fitur Dashboard

### 1. Manajemen Data
- Upload data custom (CSV, Excel, SPSS)
- Preview dan summary data
- Transformasi variabel (kategorisasi, normalisasi)

### 2. Eksplorasi Data
- Statistik deskriptif
- Visualisasi (boxplot, histogram, scatter plot, bar chart)
- Peta interaktif dengan berbagai layer

### 3. Uji Asumsi
- Uji normalitas (Shapiro-Wilk, Anderson-Darling)
- Uji homogenitas varians (Levene, Bartlett)

### 4. Statistik Inferensia
- **Uji Rata-rata**: One sample, two sample, paired t-test
- **Uji Proporsi & Varians**: One/two sample tests
- **ANOVA**: One-way, factorial ANOVA

### 5. Regresi Linear
- Simple dan multiple linear regression
- Diagnostik residual
- Interpretasi koefisien

### 6. Clustering Analysis
- Multiple clustering algorithms
- Validasi cluster (silhouette analysis)
- Visualisasi hasil clustering

## Metadata dan Referensi

Data ini mengacu pada penelitian yang dipublikasikan di:
**ScienceDirect (2021)**: "Social vulnerability data article related to COVID-19 pandemic research"
- DOI: https://www.sciencedirect.com/science/article/pii/S2352340921010180

### Konteks Penelitian
Penelitian ini mengembangkan framework untuk mengukur kerentanan sosial dalam konteks pandemi COVID-19, namun metodologinya dapat diterapkan untuk berbagai jenis bencana dan gangguan sosial.

### Validitas dan Reliabilitas
- Data telah melalui proses validasi statistik
- Menggunakan indikator yang telah diterima secara akademik
- Kompatibel dengan standard CDC Social Vulnerability Index

## Cara Penggunaan Dashboard

1. **Memuat Data**: Dashboard secara otomatis memuat data SOVI dan distance matrix
2. **Explorasi**: Gunakan tab "Eksplorasi Data" untuk memahami karakteristik data
3. **Analisis**: Pilih jenis analisis sesuai kebutuhan penelitian
4. **Interpretasi**: Setiap output dilengkapi dengan interpretasi statistik
5. **Export**: Hasil analisis dapat di-download untuk dokumentasi

## Kontribusi dan Pengembangan

Dashboard ini dapat dikembangkan lebih lanjut dengan:
- Integrasi data real-time
- Machine learning algorithms
- Advanced spatial analysis
- Interactive reporting features

---

**Dikembangkan untuk**: Analisis Statistik Komprehensif
**Platform**: R Shiny
**Lisensi**: Open Source
**Pembaruan**: Berkelanjutan sesuai kebutuhan penelitian