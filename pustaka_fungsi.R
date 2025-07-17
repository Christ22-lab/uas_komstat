# Nama file: pustaka_fungsi.R

# Muat semua library yang dibutuhkan
# Pastikan semua ini sudah di-install dengan install.packages()
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(car)      # Untuk Levene's Test
library(rstatix)  # Untuk kemudahan beberapa uji statistik
library(rmarkdown)

# -------------------------------------------------------------------
# FUNGSI-FUNGSI ANALISIS
# -------------------------------------------------------------------

# Fungsi untuk Uji Normalitas Shapiro-Wilk
lakukan_uji_normalitas <- function(data_vector) {
  if (length(na.omit(data_vector)) > 2) {
    shapiro.test(data_vector)
  } else {
    "Data tidak cukup untuk uji normalitas."
  }
}

# Fungsi untuk Interpretasi Uji Normalitas
interpretasi_normalitas <- function(shapiro_test_result) {
  if (is.character(shapiro_test_result)) return(shapiro_test_result)
  
  p_value <- shapiro_test_result$p.value
  if (p_value > 0.05) {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") > 0.05. Data diasumsikan berdistribusi normal.")
  } else {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") <= 0.05. Data tidak berdistribusi normal.")
  }
}

# Fungsi untuk Uji Homogenitas Levene
lakukan_uji_homogenitas <- function(data, formula) {
  leveneTest(formula, data = data)
}

# Fungsi untuk Interpretasi Uji Homogenitas
interpretasi_homogenitas <- function(levene_test_result) {
  p_value <- levene_test_result$`Pr(>F)`[1]
  if (p_value > 0.05) {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") > 0.05. Varian antar kelompok adalah homogen (sama).")
  } else {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") <= 0.05. Varian antar kelompok tidak homogen (berbeda).")
  }
}

# Fungsi untuk Uji T 1-Sampel
lakukan_uji_t_1sampel <- function(data_vector, mu) {
  t.test(data_vector, mu = mu)
}

# Fungsi untuk Interpretasi Uji T 1-Sampel
interpretasi_t_1sampel <- function(t_test_result, mu) {
  p_value <- t_test_result$p.value
  mean_sample <- t_test_result$estimate
  if (p_value > 0.05) {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") > 0.05. Tidak ada perbedaan signifikan antara rata-rata sampel (", round(mean_sample, 2), ") dengan nilai hipotesis (", mu, ").")
  } else {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") <= 0.05. Terdapat perbedaan signifikan antara rata-rata sampel (", round(mean_sample, 2), ") dengan nilai hipotesis (", mu, ").")
  }
}

# Fungsi untuk Regresi Linear Berganda
lakukan_regresi <- function(data, formula) {
  summary(lm(formula, data = data))
}

# Fungsi untuk membuat plot diagnostik regresi
plot_diagnostik_regresi <- function(model) {
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1)) # Reset layout
}