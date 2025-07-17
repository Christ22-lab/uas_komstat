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
library(leaflet)  # Untuk peta
library(plotly)   # Untuk plot interaktif
library(corrplot) # Untuk korelasi
library(knitr)    # Untuk laporan
library(gridExtra) # Untuk multiple plots
library(flextable) # Untuk tabel yang bagus
library(officer)   # Untuk export word
library(openxlsx)  # Untuk export excel

# -------------------------------------------------------------------
# FUNGSI-FUNGSI MANAJEMEN DATA
# -------------------------------------------------------------------

# Fungsi untuk kategorisasi data kontinyu
kategorisasi_data <- function(data_vector, n_kategori = 3, metode = "quantile") {
  if (metode == "quantile") {
    cut(data_vector, breaks = quantile(data_vector, probs = seq(0, 1, 1/n_kategori), na.rm = TRUE), 
        include.lowest = TRUE, labels = paste0("Kategori_", 1:n_kategori))
  } else if (metode == "equal") {
    cut(data_vector, breaks = n_kategori, labels = paste0("Kategori_", 1:n_kategori))
  }
}

# Fungsi interpretasi kategorisasi
interpretasi_kategorisasi <- function(data_vector, kategori_result, metode) {
  n_obs <- length(data_vector)
  n_kategori <- length(levels(kategori_result))
  
  paste0("Interpretasi: Data kontinyu dengan ", n_obs, " observasi telah dikategorikan menjadi ", 
         n_kategori, " kategori menggunakan metode ", metode, ". ",
         "Distribusi: ", paste(table(kategori_result), collapse = ", "))
}

# -------------------------------------------------------------------
# FUNGSI-FUNGSI ANALISIS STATISTIK
# -------------------------------------------------------------------

# Fungsi untuk Uji Normalitas Shapiro-Wilk
lakukan_uji_normalitas <- function(data_vector) {
  if (length(na.omit(data_vector)) > 2 && length(na.omit(data_vector)) <= 5000) {
    shapiro.test(data_vector)
  } else if (length(na.omit(data_vector)) > 5000) {
    ks.test(data_vector, "pnorm", mean(data_vector, na.rm = TRUE), sd(data_vector, na.rm = TRUE))
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

# Fungsi untuk Uji T 2-Sampel
lakukan_uji_t_2sampel <- function(data, formula, var_equal = TRUE) {
  t.test(formula, data = data, var.equal = var_equal)
}

# Fungsi untuk Interpretasi Uji T 2-Sampel
interpretasi_t_2sampel <- function(t_test_result) {
  p_value <- t_test_result$p.value
  mean_diff <- diff(t_test_result$estimate)
  if (p_value > 0.05) {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") > 0.05. Tidak ada perbedaan signifikan antara rata-rata kedua kelompok.")
  } else {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") <= 0.05. Terdapat perbedaan signifikan antara rata-rata kedua kelompok. Selisih rata-rata: ", round(mean_diff, 2))
  }
}

# Fungsi untuk Uji Proporsi 1-Sampel
lakukan_uji_proporsi_1sampel <- function(x, n, p0) {
  prop.test(x, n, p = p0)
}

# Fungsi untuk Interpretasi Uji Proporsi 1-Sampel
interpretasi_proporsi_1sampel <- function(prop_test_result, p0) {
  p_value <- prop_test_result$p.value
  prop_sample <- prop_test_result$estimate
  if (p_value > 0.05) {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") > 0.05. Tidak ada perbedaan signifikan antara proporsi sampel (", round(prop_sample, 3), ") dengan proporsi hipotesis (", p0, ").")
  } else {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") <= 0.05. Terdapat perbedaan signifikan antara proporsi sampel (", round(prop_sample, 3), ") dengan proporsi hipotesis (", p0, ").")
  }
}

# Fungsi untuk Uji Proporsi 2-Sampel
lakukan_uji_proporsi_2sampel <- function(x1, n1, x2, n2) {
  prop.test(c(x1, x2), c(n1, n2))
}

# Fungsi untuk Interpretasi Uji Proporsi 2-Sampel
interpretasi_proporsi_2sampel <- function(prop_test_result) {
  p_value <- prop_test_result$p.value
  if (p_value > 0.05) {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") > 0.05. Tidak ada perbedaan signifikan antara proporsi kedua kelompok.")
  } else {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") <= 0.05. Terdapat perbedaan signifikan antara proporsi kedua kelompok.")
  }
}

# Fungsi untuk Uji Varians 1-Sampel (Chi-square test)
lakukan_uji_varians_1sampel <- function(data_vector, sigma2_0) {
  n <- length(data_vector)
  s2 <- var(data_vector, na.rm = TRUE)
  chi_stat <- (n - 1) * s2 / sigma2_0
  p_value <- 2 * min(pchisq(chi_stat, df = n - 1), 1 - pchisq(chi_stat, df = n - 1))
  
  list(
    statistic = chi_stat,
    p.value = p_value,
    sample_var = s2,
    df = n - 1,
    method = "Chi-square test for variance"
  )
}

# Fungsi untuk Interpretasi Uji Varians 1-Sampel
interpretasi_varians_1sampel <- function(var_test_result, sigma2_0) {
  p_value <- var_test_result$p.value
  sample_var <- var_test_result$sample_var
  if (p_value > 0.05) {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") > 0.05. Tidak ada perbedaan signifikan antara varians sampel (", round(sample_var, 4), ") dengan varians hipotesis (", sigma2_0, ").")
  } else {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") <= 0.05. Terdapat perbedaan signifikan antara varians sampel (", round(sample_var, 4), ") dengan varians hipotesis (", sigma2_0, ").")
  }
}

# Fungsi untuk Uji Varians 2-Sampel (F-test)
lakukan_uji_varians_2sampel <- function(data, formula) {
  var.test(formula, data = data)
}

# Fungsi untuk Interpretasi Uji Varians 2-Sampel
interpretasi_varians_2sampel <- function(var_test_result) {
  p_value <- var_test_result$p.value
  f_ratio <- var_test_result$statistic
  if (p_value > 0.05) {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") > 0.05. Tidak ada perbedaan signifikan antara varians kedua kelompok. F-ratio: ", round(f_ratio, 4))
  } else {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") <= 0.05. Terdapat perbedaan signifikan antara varians kedua kelompok. F-ratio: ", round(f_ratio, 4))
  }
}

# Fungsi untuk ANOVA 1-Arah
lakukan_anova_1arah <- function(data, formula) {
  model <- aov(formula, data = data)
  summary(model)
}

# Fungsi untuk Interpretasi ANOVA 1-Arah
interpretasi_anova_1arah <- function(anova_result) {
  p_value <- anova_result[[1]]$`Pr(>F)`[1]
  f_value <- anova_result[[1]]$`F value`[1]
  if (p_value > 0.05) {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") > 0.05. Tidak ada perbedaan signifikan antara rata-rata kelompok. F-value: ", round(f_value, 4))
  } else {
    paste0("Interpretasi: P-value (", round(p_value, 4), ") <= 0.05. Terdapat perbedaan signifikan antara rata-rata kelompok. F-value: ", round(f_value, 4), ". Diperlukan uji post-hoc untuk mengetahui kelompok mana yang berbeda.")
  }
}

# Fungsi untuk ANOVA 2-Arah
lakukan_anova_2arah <- function(data, formula) {
  model <- aov(formula, data = data)
  summary(model)
}

# Fungsi untuk Interpretasi ANOVA 2-Arah
interpretasi_anova_2arah <- function(anova_result) {
  hasil <- anova_result[[1]]
  interpretasi <- c()
  
  for (i in 1:(nrow(hasil) - 1)) {
    p_value <- hasil$`Pr(>F)`[i]
    f_value <- hasil$`F value`[i]
    faktor <- rownames(hasil)[i]
    
    if (p_value > 0.05) {
      interpretasi <- c(interpretasi, paste0(faktor, ": P-value (", round(p_value, 4), ") > 0.05. Tidak signifikan."))
    } else {
      interpretasi <- c(interpretasi, paste0(faktor, ": P-value (", round(p_value, 4), ") <= 0.05. Signifikan. F-value: ", round(f_value, 4)))
    }
  }
  
  paste(interpretasi, collapse = "\n")
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

# Fungsi untuk interpretasi regresi
interpretasi_regresi <- function(model_summary) {
  r_squared <- model_summary$r.squared
  adj_r_squared <- model_summary$adj.r.squared
  f_stat <- model_summary$fstatistic
  p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  
  interpretasi <- paste0(
    "Interpretasi Model Regresi:\n",
    "R-squared: ", round(r_squared, 4), " (", round(r_squared * 100, 2), "% variasi Y dijelaskan oleh model)\n",
    "Adjusted R-squared: ", round(adj_r_squared, 4), "\n",
    "F-statistic: ", round(f_stat[1], 4), " dengan p-value: ", round(p_value, 4), "\n",
    if (p_value < 0.05) "Model secara keseluruhan signifikan." else "Model secara keseluruhan tidak signifikan."
  )
  
  return(interpretasi)
}

# -------------------------------------------------------------------
# FUNGSI-FUNGSI VISUALISASI
# -------------------------------------------------------------------

# Fungsi untuk membuat peta sederhana (contoh)
buat_peta_sederhana <- function(data, lat_col, lon_col, value_col) {
  if (all(c(lat_col, lon_col, value_col) %in% names(data))) {
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~get(lon_col),
        lat = ~get(lat_col),
        radius = ~get(value_col) * 10,
        popup = ~paste("Nilai:", get(value_col))
      )
  } else {
    return("Kolom yang diperlukan tidak ditemukan dalam data")
  }
}

# -------------------------------------------------------------------
# FUNGSI-FUNGSI EXPORT
# -------------------------------------------------------------------

# Fungsi untuk membuat laporan lengkap
buat_laporan_lengkap <- function(data, analisis_list, judul = "Laporan Analisis Data") {
  # Implementasi untuk membuat laporan lengkap
  # Akan dikembangkan lebih lanjut
  return("Laporan sedang dipersiapkan...")
}