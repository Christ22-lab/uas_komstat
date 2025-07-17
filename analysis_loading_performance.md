# Analisis Performa Loading Dashboard SOVI

## Identifikasi Masalah Loading Lambat

### 1. **Ukuran Data yang Diunduh**

Berdasarkan analisis, ukuran data yang diunduh adalah:
- **SOVI Data**: 91,855 bytes (~90 KB)
- **Distance Matrix**: 4,409,022 bytes (~4.2 MB)

**Total download**: ~4.3 MB per session

### 2. **Penyebab Loading Lambat**

#### A. **Distance Matrix Berukuran Besar (4.2 MB)**
- File distance.csv berisi matriks jarak 511x511 
- Setiap cell berisi nilai numerik dengan presisi tinggi
- Membutuhkan waktu unduh dan parsing yang signifikan

#### B. **Loading Sinkron di Startup**
```r
observe({
  # Load SOVI data
  values$sovi_data <- load_sovi_data()
  values$distance_data <- load_distance_data()  # ← Ini yang lambat
  
  # Update UI elements...
})
```

#### C. **Tidak Ada Caching**
- Data diunduh ulang setiap kali dashboard dimuat
- Tidak ada mekanisme penyimpanan lokal

#### D. **Parsing CSV yang Intensif**
- Distance matrix dengan 511 kolom memerlukan parsing yang kompleks
- `read_csv()` harus menganalisis struktur data yang besar

### 3. **Analisis Penggunaan Distance Matrix**

Setelah memeriksa kode, distance matrix hanya digunakan untuk:
- Export data (`export_distance`)
- Tidak digunakan untuk analisis utama dashboard

## Solusi Optimasi

### 1. **Lazy Loading Distance Matrix**
```r
# Ubah dari loading otomatis menjadi on-demand
load_distance_data_lazy <- function() {
  if (is.null(values$distance_data)) {
    showNotification("Loading distance matrix...", type = "message")
    values$distance_data <- load_distance_data()
    showNotification("Distance matrix loaded!", type = "message")
  }
  return(values$distance_data)
}
```

### 2. **Caching dengan RDS**
```r
# Fungsi dengan caching
load_data_with_cache <- function(url, cache_file, load_func) {
  if (file.exists(cache_file)) {
    # Load dari cache
    data <- readRDS(cache_file)
  } else {
    # Download dan simpan ke cache
    data <- load_func(url)
    if (!is.null(data)) {
      saveRDS(data, cache_file)
    }
  }
  return(data)
}
```

### 3. **Parallel Loading**
```r
# Load data secara paralel
library(future)
plan(multisession)

# Async loading
sovi_future <- future({
  load_sovi_data()
})

distance_future <- future({
  load_distance_data()
})

# Resolve ketika dibutuhkan
values$sovi_data <- value(sovi_future)
```

### 4. **Compression dan Format Optimasi**
```r
# Gunakan format yang lebih efisien
load_distance_data_compressed <- function() {
  tryCatch({
    url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
    
    # Download dengan kompresi
    temp_file <- tempfile(fileext = ".csv")
    download.file(url, temp_file, mode = "wb")
    
    # Load dengan fread (lebih cepat)
    data <- fread(temp_file)
    unlink(temp_file)
    
    return(data)
  }, error = function(e) {
    return(NULL)
  })
}
```

### 5. **Progressive Loading dengan Progress Bar**
```r
# Loading dengan progress indicator
load_data_with_progress <- function() {
  withProgress(message = 'Loading data...', value = 0, {
    
    incProgress(0.3, detail = "Loading SOVI data...")
    values$sovi_data <- load_sovi_data()
    
    incProgress(0.6, detail = "Loading distance matrix...")
    values$distance_data <- load_distance_data()
    
    incProgress(1, detail = "Complete!")
  })
}
```

## Implementasi Solusi Terbaik

### Rekomendasi Utama:

1. **Hapus loading distance matrix dari startup**
2. **Implementasi lazy loading untuk distance matrix**
3. **Tambahkan caching untuk kedua dataset**
4. **Gunakan progress indicator**

### Kode Optimasi:

```r
# Di pustaka_fungsi.R
load_sovi_data_cached <- function() {
  cache_file <- "cache/sovi_data.rds"
  
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  
  data <- load_sovi_data()
  if (!is.null(data)) {
    dir.create("cache", showWarnings = FALSE)
    saveRDS(data, cache_file)
  }
  return(data)
}

# Di server function
observe({
  # Hanya load SOVI data (cepat)
  values$sovi_data <- load_sovi_data_cached()
  
  # Distance matrix di-load on-demand
  values$distance_data <- NULL
  
  # Update UI...
})

# Load distance matrix hanya ketika dibutuhkan
output$export_distance <- downloadHandler(
  filename = function() { paste0("distance_matrix_", Sys.Date(), ".csv") },
  content = function(file) {
    if (is.null(values$distance_data)) {
      showNotification("Loading distance matrix...", type = "message")
      values$distance_data <- load_distance_data()
    }
    write.csv(values$distance_data, file, row.names = FALSE)
  }
)
```

## Hasil yang Diharapkan

Dengan implementasi solusi ini:
- **Startup time**: Dari ~10-15 detik menjadi ~2-3 detik
- **Memory usage**: Berkurang ~4.2 MB saat startup
- **User experience**: Loading yang lebih responsif
- **Caching**: Loading berikutnya akan lebih cepat

## Kesimpulan

Loading lambat terutama disebabkan oleh:
1. **Distance matrix berukuran 4.2 MB** yang di-load saat startup
2. **Tidak ada caching mechanism**
3. **Sinkronous loading** tanpa progress indicator

Solusi terbaik adalah **lazy loading** dengan **caching** untuk mengoptimalkan performa dashboard.