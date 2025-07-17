# Script untuk menjalankan Dashboard Analisis Data Statistik
# Pastikan semua package sudah terinstal dengan menjalankan install_packages.R terlebih dahulu

cat("=== DASHBOARD ANALISIS DATA STATISTIK ===\n")
cat("Memulai dashboard...\n\n")

# Periksa apakah file dashboard ada
if (!file.exists("dashboard.rmd")) {
  stop("File dashboard.rmd tidak ditemukan! Pastikan Anda berada di direktori yang benar.")
}

# Periksa apakah file pustaka_fungsi.R ada
if (!file.exists("pustaka_fungsi.R")) {
  stop("File pustaka_fungsi.R tidak ditemukan! File ini diperlukan untuk menjalankan dashboard.")
}

# Load library yang diperlukan
required_packages <- c("rmarkdown", "shiny")
for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    stop(paste("Package", pkg, "tidak terinstal. Jalankan install_packages.R terlebih dahulu."))
  }
}

cat("✓ File dashboard ditemukan\n")
cat("✓ File pustaka fungsi ditemukan\n")
cat("✓ Package utama sudah dimuat\n\n")

cat("🚀 Menjalankan dashboard...\n")
cat("Dashboard akan terbuka di browser Anda.\n")
cat("Jika tidak terbuka otomatis, buka browser dan akses alamat yang ditampilkan.\n\n")

cat("Untuk menghentikan dashboard, tekan Ctrl+C atau ESC di console R.\n")
cat("========================================\n\n")

# Jalankan dashboard
tryCatch({
  rmarkdown::run("dashboard.rmd")
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  cat("Pastikan:\n")
  cat("1. Semua package sudah terinstal (jalankan install_packages.R)\n")
  cat("2. File dashboard.rmd dan pustaka_fungsi.R ada di direktori yang sama\n")
  cat("3. Tidak ada error syntax dalam kode\n")
})