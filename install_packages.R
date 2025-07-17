# Script untuk menginstal semua package yang diperlukan untuk Dashboard Analisis Data Statistik
# Jalankan script ini sebelum menjalankan dashboard

# Fungsi untuk menginstal package jika belum terinstal
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Menginstal package yang belum terinstal:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE)
  } else {
    cat("Semua package sudah terinstal.\n")
  }
}

# Daftar package yang diperlukan
required_packages <- c(
  # Core Shiny packages
  "shiny",
  "shinydashboard",
  
  # Data manipulation
  "dplyr",
  "readr",
  
  # Visualization
  "ggplot2",
  "plotly",
  "leaflet",
  "corrplot",
  "gridExtra",
  
  # Tables
  "DT",
  
  # Statistical analysis
  "car",      # Levene's test, VIF
  "rstatix",  # Statistical tests
  "lmtest",   # Durbin-Watson test
  
  # Reporting
  "rmarkdown",
  "knitr",
  "flextable",
  "officer",
  "openxlsx"
)

# Menginstal package
cat("=== INSTALASI PACKAGE DASHBOARD ANALISIS DATA STATISTIK ===\n")
cat("Memeriksa dan menginstal package yang diperlukan...\n\n")

tryCatch({
  install_if_missing(required_packages)
  
  cat("\n=== VERIFIKASI INSTALASI ===\n")
  # Verifikasi instalasi
  for(pkg in required_packages) {
    if(pkg %in% installed.packages()[,"Package"]) {
      cat("✓", pkg, "- Terinstal\n")
    } else {
      cat("✗", pkg, "- GAGAL TERINSTAL\n")
    }
  }
  
  cat("\n=== TESTING LOAD PACKAGES ===\n")
  # Test load packages
  failed_packages <- c()
  for(pkg in required_packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
      cat("✓", pkg, "- Berhasil dimuat\n")
    }, error = function(e) {
      cat("✗", pkg, "- Gagal dimuat:", e$message, "\n")
      failed_packages <<- c(failed_packages, pkg)
    })
  }
  
  if(length(failed_packages) == 0) {
    cat("\n🎉 SEMUA PACKAGE BERHASIL TERINSTAL DAN DIMUAT!\n")
    cat("Dashboard siap dijalankan dengan perintah:\n")
    cat("rmarkdown::run('dashboard.rmd')\n")
  } else {
    cat("\n⚠️  BEBERAPA PACKAGE GAGAL DIMUAT:\n")
    cat(paste(failed_packages, collapse = ", "), "\n")
    cat("Silakan instal manual dengan: install.packages(c('", paste(failed_packages, collapse = "', '"), "'))\n")
  }
  
}, error = function(e) {
  cat("ERROR dalam instalasi:", e$message, "\n")
  cat("Silakan coba instal manual atau periksa koneksi internet.\n")
})

cat("\n=== INFORMASI SISTEM ===\n")
cat("R Version:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("Locale:", Sys.getlocale(), "\n")

cat("\n=== SELESAI ===\n")