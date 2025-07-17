# ==============================================================================
# PUSTAKA FUNGSI DASHBOARD ANALISIS DATA STATISTIK MODERN
# ==============================================================================
# Versi: 3.0
# Dikembangkan untuk analisis data SOVI (Social Vulnerability Index)
# Data source: https://github.com/bmlmcmc/naspaclust
# ==============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinycssloaders)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(readr)
  library(car)
  library(rstatix)
  library(leaflet)
  library(corrplot)
  library(RColorBrewer)
  library(viridis)
  library(htmltools)
  library(htmlwidgets)
  library(knitr)
  library(gridExtra)
  library(scales)
  library(reshape2)
  library(cluster)
  library(factoextra)
  library(FactoMineR)
  library(data.table)
})

# ==============================================================================
# LIBRARY LOADING
# ==============================================================================

# Suppress startup messages
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(leaflet)
  library(corrplot)
  library(VIM)
  library(mice)
  library(psych)
  library(car)
  library(nortest)
  library(lmtest)
  library(broom)
  library(knitr)
  library(kableExtra)
  library(RColorBrewer)
  library(viridis)
  library(htmltools)
  library(htmlwidgets)
  library(scales)
  library(reshape2)
  library(cluster)
  library(factoextra)
  library(FactoMineR)
  library(data.table)
})

# ==============================================================================
# FUNGSI AKSES DATA OPTIMIZED
# ==============================================================================

# Fungsi untuk mengunduh data SOVI dengan caching
load_sovi_data_cached <- function() {
  cache_file <- "cache/sovi_data.rds"
  
  # Buat folder cache jika belum ada
  if (!dir.exists("cache")) {
    dir.create("cache", showWarnings = FALSE)
  }
  
  # Cek apakah cache ada dan masih fresh (kurang dari 24 jam)
  if (file.exists(cache_file)) {
    file_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours")
    if (file_age < 24) {
      return(readRDS(cache_file))
    }
  }
  
  # Load data dari URL
  data <- load_sovi_data()
  
  # Simpan ke cache jika berhasil
  if (!is.null(data)) {
    saveRDS(data, cache_file)
  }
  
  return(data)
}

# Fungsi untuk mengunduh data SOVI (original)
load_sovi_data <- function() {
  tryCatch({
    url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
    data <- read_csv(url, show_col_types = FALSE)
    
    # Bersihkan nama kolom
    names(data) <- gsub("\\s+", "_", names(data))
    
    return(data)
  }, error = function(e) {
    return(NULL)
  })
}

# Fungsi untuk mengunduh data jarak dengan lazy loading
load_distance_data_lazy <- function() {
  cache_file <- "cache/distance_data.rds"
  
  # Buat folder cache jika belum ada
  if (!dir.exists("cache")) {
    dir.create("cache", showWarnings = FALSE)
  }
  
  # Cek cache terlebih dahulu
  if (file.exists(cache_file)) {
    file_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours")
    if (file_age < 24) {
      return(readRDS(cache_file))
    }
  }
  
  # Load data dari URL dengan optimasi
  data <- load_distance_data_optimized()
  
  # Simpan ke cache jika berhasil
  if (!is.null(data)) {
    saveRDS(data, cache_file)
  }
  
  return(data)
}

# Fungsi untuk mengunduh data jarak dengan optimasi
load_distance_data_optimized <- function() {
  tryCatch({
    url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
    
    # Download ke file temporary
    temp_file <- tempfile(fileext = ".csv")
    download.file(url, temp_file, mode = "wb", quiet = TRUE)
    
    # Load dengan fread (lebih cepat dari read_csv)
    data <- fread(temp_file)
    
    # Hapus file temporary
    unlink(temp_file)
    
    return(data)
  }, error = function(e) {
    return(NULL)
  })
}

# Fungsi untuk mengunduh data jarak (original - fallback)
load_distance_data <- function() {
  tryCatch({
    url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
    data <- read_csv(url, show_col_types = FALSE)
    return(data)
  }, error = function(e) {
    return(NULL)
  })
}

# Fungsi untuk loading data dengan progress indicator
load_data_with_progress <- function(session = NULL) {
  if (!is.null(session)) {
    withProgress(message = 'Loading data...', value = 0, {
      incProgress(0.5, detail = "Loading SOVI data...")
      sovi_data <- load_sovi_data_cached()
      
      incProgress(1, detail = "Complete!")
      return(sovi_data)
    })
  } else {
    return(load_sovi_data_cached())
  }
}

# ==============================================================================
# FUNGSI METADATA
# ==============================================================================

# Fungsi untuk mendapatkan metadata
get_metadata_info <- function() {
  return(list(
    title = "Social Vulnerability Index (SOVI) Dataset",
    source = "https://www.sciencedirect.com/science/article/pii/S2352340921010180",
    description = "Dataset contains social vulnerability indicators for Indonesian districts",
    variables = list(
      "DISTRICTCODE" = "District identification code",
      "CHILDREN" = "Percentage of children population",
      "FEMALE" = "Percentage of female population", 
      "ELDERLY" = "Percentage of elderly population",
      "FHEAD" = "Percentage of female-headed households",
      "FAMILYSIZE" = "Average family size",
      "NOELECTRIC" = "Percentage without electricity access",
      "NOWATER" = "Percentage without clean water access",
      "RENT" = "Percentage of rented housing",
      "POOR" = "Percentage of poor households",
      "UNEMP" = "Unemployment rate",
      "DEPEND" = "Dependency ratio",
      "DISAB" = "Percentage of disabled population",
      "POVERTY" = "Poverty index",
      "POPULATION" = "Total population",
      "AREA" = "Area in square kilometers",
      "DENSITY" = "Population density"
    ),
    last_updated = "2021",
    total_districts = 511,
    data_size = list(
      sovi = "~90 KB",
      distance = "~4.2 MB"
    )
  ))
}

# ==============================================================================
# FUNGSI CACHE MANAGEMENT
# ==============================================================================

# Fungsi untuk membersihkan cache
clear_cache <- function() {
  if (dir.exists("cache")) {
    files <- list.files("cache", full.names = TRUE)
    file.remove(files)
    return(TRUE)
  }
  return(FALSE)
}

# Fungsi untuk cek status cache
check_cache_status <- function() {
  if (!dir.exists("cache")) {
    return(list(
      sovi_cached = FALSE,
      distance_cached = FALSE,
      cache_size = 0
    ))
  }
  
  sovi_cache <- file.exists("cache/sovi_data.rds")
  distance_cache <- file.exists("cache/distance_data.rds")
  
  cache_files <- list.files("cache", full.names = TRUE)
  cache_size <- sum(file.info(cache_files)$size, na.rm = TRUE)
  
  return(list(
    sovi_cached = sovi_cache,
    distance_cached = distance_cache,
    cache_size = cache_size
  ))
}

# ==============================================================================
# FUNGSI ANALISIS STATISTIK LANJUTAN
# ==============================================================================

# Fungsi untuk analisis komponen utama (PCA)
perform_pca <- function(data, scale = TRUE) {
  numeric_data <- data %>% select_if(is.numeric)
  if(ncol(numeric_data) < 2) return(NULL)
  
  tryCatch({
    pca_result <- PCA(numeric_data, scale.unit = scale, graph = FALSE)
    return(pca_result)
  }, error = function(e) {
    return(NULL)
  })
}

# Fungsi untuk analisis cluster
perform_clustering <- function(data, k = 3, method = "kmeans") {
  numeric_data <- data %>% select_if(is.numeric)
  if(nrow(numeric_data) < k) return(NULL)
  
  tryCatch({
    if(method == "kmeans") {
      set.seed(123)
      result <- kmeans(scale(numeric_data), centers = k, nstart = 25)
      return(result)
    } else if(method == "hierarchical") {
      dist_matrix <- dist(scale(numeric_data))
      hc <- hclust(dist_matrix, method = "ward.D2")
      clusters <- cutree(hc, k = k)
      return(list(cluster = clusters, hclust = hc))
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Fungsi untuk analisis korelasi
perform_correlation <- function(data, method = "pearson") {
  numeric_data <- data %>% select_if(is.numeric)
  if(ncol(numeric_data) < 2) return(NULL)
  
  tryCatch({
    cor_matrix <- cor(numeric_data, use = "complete.obs", method = method)
    return(cor_matrix)
  }, error = function(e) {
    return(NULL)
  })
}

# ==============================================================================
# FUNGSI VISUALISASI MODERN
# ==============================================================================

# Tema ggplot2 modern
theme_modern <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#f0f0f0", size = 0.5),
      panel.grid.minor = element_line(color = "#f8f8f8", size = 0.3),
      text = element_text(family = "Arial", color = "#333333"),
      plot.title = element_text(size = 16, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 12, color = "#666666", margin = margin(b = 15)),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 11, face = "bold"),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# Fungsi untuk membuat plot korelasi modern
create_correlation_plot <- function(cor_matrix) {
  if(is.null(cor_matrix)) return(NULL)
  
  # Konversi ke format long
  cor_df <- as.data.frame(as.table(cor_matrix))
  names(cor_df) <- c("Var1", "Var2", "Correlation")
  
  p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "#d73027", high = "#1a9850", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Correlation") +
    theme_modern() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank()) +
    labs(title = "Correlation Matrix Heatmap",
         subtitle = "Pearson correlation coefficients between variables") +
    coord_fixed()
  
  return(p)
}

# Fungsi untuk membuat plot PCA modern
create_pca_plot <- function(pca_result, data) {
  if(is.null(pca_result)) return(NULL)
  
  # Biplot
  p1 <- fviz_pca_biplot(pca_result, 
                        col.ind = "contrib", 
                        col.var = "contrib",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE,
                        title = "PCA Biplot",
                        subtitle = "Variables and observations projection") +
    theme_modern()
  
  return(p1)
}

# Fungsi untuk membuat plot cluster modern
create_cluster_plot <- function(cluster_result, data) {
  if(is.null(cluster_result)) return(NULL)
  
  numeric_data <- data %>% select_if(is.numeric)
  
  if("cluster" %in% names(cluster_result)) {
    # K-means
    p <- fviz_cluster(cluster_result, data = numeric_data,
                      palette = viridis_discrete(),
                      geom = "point",
                      ellipse.type = "convex",
                      ggtheme = theme_modern(),
                      title = "K-means Clustering Results",
                      subtitle = "Clusters visualization in PCA space")
  } else {
    # Hierarchical
    p <- fviz_dend(cluster_result$hclust, k = length(unique(cluster_result$cluster)),
                   color_labels_by_k = TRUE,
                   palette = viridis_discrete(),
                   main = "Hierarchical Clustering Dendrogram",
                   ggtheme = theme_modern())
  }
  
  return(p)
}

# ==============================================================================
# FUNGSI STATISTIK DESKRIPTIF LANJUTAN
# ==============================================================================

# Fungsi untuk statistik deskriptif komprehensif
comprehensive_summary <- function(data) {
  numeric_data <- data %>% select_if(is.numeric)
  
  summary_stats <- numeric_data %>%
    summarise_all(list(
      Mean = ~mean(., na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE),
      Q1 = ~quantile(., 0.25, na.rm = TRUE),
      Q3 = ~quantile(., 0.75, na.rm = TRUE),
      IQR = ~IQR(., na.rm = TRUE),
      Skewness = ~e1071::skewness(., na.rm = TRUE),
      Kurtosis = ~e1071::kurtosis(., na.rm = TRUE),
      Missing = ~sum(is.na(.))
    ))
  
  # Transpose untuk tampilan yang lebih baik
  summary_long <- summary_stats %>%
    pivot_longer(everything(), names_to = "Variable_Stat", values_to = "Value") %>%
    separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_(?=[^_]*$)") %>%
    pivot_wider(names_from = Statistic, values_from = Value)
  
  return(summary_long)
}

# ==============================================================================
# FUNGSI UJI STATISTIK LANJUTAN
# ==============================================================================

# Fungsi untuk uji normalitas multivariat
multivariate_normality_test <- function(data) {
  numeric_data <- data %>% select_if(is.numeric)
  if(ncol(numeric_data) < 2) return(NULL)
  
  tryCatch({
    # Mardia's test
    result <- mvnormtest::mshapiro.test(t(as.matrix(numeric_data)))
    return(result)
  }, error = function(e) {
    return(NULL)
  })
}

# Fungsi untuk MANOVA
perform_manova <- function(data, dependent_vars, independent_var) {
  tryCatch({
    formula_str <- paste("cbind(", paste(dependent_vars, collapse = ", "), ") ~", independent_var)
    manova_result <- manova(as.formula(formula_str), data = data)
    return(summary(manova_result))
  }, error = function(e) {
    return(NULL)
  })
}

# ==============================================================================
# FUNGSI UTILITAS
# ==============================================================================

# Fungsi untuk format angka
format_number <- function(x, digits = 2) {
  if(is.numeric(x)) {
    return(format(round(x, digits), nsmall = digits, big.mark = ","))
  }
  return(x)
}

# Fungsi untuk membuat tabel yang cantik
create_beautiful_table <- function(data, caption = NULL) {
  dt <- datatable(data,
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    scrollY = "400px",
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ),
                  class = 'cell-border stripe hover',
                  caption = caption,
                  filter = 'top',
                  extensions = 'Buttons') %>%
    formatRound(columns = sapply(data, is.numeric), digits = 3)
  
  return(dt)
}

# Fungsi untuk membuat info box modern
create_info_box <- function(title, value, subtitle = NULL, icon = "info-circle", color = "blue") {
  valueBox(
    value = value,
    subtitle = HTML(paste0("<b>", title, "</b><br>", subtitle)),
    icon = icon(icon),
    color = color,
    width = NULL
  )
}

# ==============================================================================
# FUNGSI CSS DAN STYLING
# ==============================================================================

# CSS untuk dashboard modern
get_modern_css <- function() {
  return(tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      
      /* Global Styles */
      body {
        font-family: 'Inter', sans-serif !important;
        background-color: #f8fafc !important;
      }
      
      /* Header Styles */
      .main-header .navbar {
        background-color: #1e293b !important;
        border: none !important;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
      }
      
      .main-header .navbar-brand {
        color: #ffffff !important;
        font-weight: 600 !important;
        font-size: 18px !important;
      }
      
      /* Sidebar Styles */
      .main-sidebar {
        background-color: #ffffff !important;
        box-shadow: 2px 0 4px rgba(0,0,0,0.1) !important;
      }
      
      .sidebar-menu > li > a {
        color: #475569 !important;
        font-weight: 500 !important;
        padding: 12px 20px !important;
        border-radius: 8px !important;
        margin: 4px 12px !important;
        transition: all 0.3s ease !important;
      }
      
      .sidebar-menu > li > a:hover {
        background-color: #f1f5f9 !important;
        color: #1e293b !important;
      }
      
      .sidebar-menu > li.active > a {
        background-color: #3b82f6 !important;
        color: #ffffff !important;
      }
      
      .sidebar-menu .treeview-menu > li > a {
        color: #64748b !important;
        padding-left: 40px !important;
      }
      
      /* Content Styles */
      .content-wrapper {
        background-color: #f8fafc !important;
      }
      
      .content-header h1 {
        color: #1e293b !important;
        font-weight: 600 !important;
        margin-bottom: 20px !important;
      }
      
      /* Box Styles */
      .box {
        border-radius: 12px !important;
        border: none !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1) !important;
        margin-bottom: 20px !important;
      }
      
      .box-header {
        background-color: transparent !important;
        border-bottom: 1px solid #e2e8f0 !important;
        padding: 20px !important;
      }
      
      .box-title {
        color: #1e293b !important;
        font-weight: 600 !important;
        font-size: 16px !important;
      }
      
      .box-body {
        padding: 20px !important;
      }
      
      /* Button Styles */
      .btn {
        border-radius: 8px !important;
        font-weight: 500 !important;
        padding: 8px 16px !important;
        transition: all 0.3s ease !important;
      }
      
      .btn-primary {
        background-color: #3b82f6 !important;
        border-color: #3b82f6 !important;
      }
      
      .btn-primary:hover {
        background-color: #2563eb !important;
        border-color: #2563eb !important;
      }
      
      .btn-success {
        background-color: #10b981 !important;
        border-color: #10b981 !important;
      }
      
      .btn-success:hover {
        background-color: #059669 !important;
        border-color: #059669 !important;
      }
      
      .btn-info {
        background-color: #06b6d4 !important;
        border-color: #06b6d4 !important;
      }
      
      .btn-info:hover {
        background-color: #0891b2 !important;
        border-color: #0891b2 !important;
      }
      
      .btn-warning {
        background-color: #f59e0b !important;
        border-color: #f59e0b !important;
      }
      
      .btn-warning:hover {
        background-color: #d97706 !important;
        border-color: #d97706 !important;
      }
      
      /* Input Styles */
      .form-control {
        border-radius: 8px !important;
        border: 1px solid #d1d5db !important;
        padding: 8px 12px !important;
        transition: border-color 0.3s ease !important;
      }
      
      .form-control:focus {
        border-color: #3b82f6 !important;
        box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1) !important;
      }
      
      /* Value Box Styles */
      .small-box {
        border-radius: 12px !important;
        margin-bottom: 20px !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1) !important;
      }
      
      .small-box h3 {
        font-weight: 700 !important;
        font-size: 28px !important;
      }
      
      .small-box p {
        font-weight: 500 !important;
      }
      
      /* DataTable Styles */
      .dataTables_wrapper {
        font-family: 'Inter', sans-serif !important;
      }
      
      .dataTables_wrapper .dataTables_length select,
      .dataTables_wrapper .dataTables_filter input {
        border-radius: 6px !important;
        border: 1px solid #d1d5db !important;
        padding: 4px 8px !important;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        border-radius: 6px !important;
        margin: 0 2px !important;
      }
      
      /* Tab Styles */
      .nav-tabs-custom .nav-tabs {
        border-bottom: 2px solid #e2e8f0 !important;
      }
      
      .nav-tabs-custom .nav-tabs li a {
        color: #64748b !important;
        font-weight: 500 !important;
        border-radius: 8px 8px 0 0 !important;
        padding: 12px 20px !important;
      }
      
      .nav-tabs-custom .nav-tabs li.active a {
        background-color: #3b82f6 !important;
        color: #ffffff !important;
        border-color: #3b82f6 !important;
      }
      
      /* Loading Spinner */
      .spinner {
        border: 4px solid #f3f3f3;
        border-top: 4px solid #3b82f6;
        border-radius: 50%;
        width: 40px;
        height: 40px;
        animation: spin 1s linear infinite;
        margin: 20px auto;
      }
      
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .box-header {
          padding: 15px !important;
        }
        
        .box-body {
          padding: 15px !important;
        }
        
        .sidebar-menu > li > a {
          padding: 10px 15px !important;
          margin: 2px 8px !important;
        }
      }
      
      /* Custom Scrollbar */
      ::-webkit-scrollbar {
        width: 8px;
      }
      
      ::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb {
        background: #cbd5e1;
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: #94a3b8;
      }
    "))
  ))
}

# ==============================================================================
# FUNGSI INTERPRETASI HASIL
# ==============================================================================

# Fungsi interpretasi untuk PCA
interpret_pca <- function(pca_result) {
  if(is.null(pca_result)) return("PCA tidak dapat dilakukan.")
  
  # Ambil eigenvalues
  eigenvalues <- pca_result$eig[,1]
  variance_explained <- pca_result$eig[,2]
  
  # Interpretasi
  interpretation <- paste0(
    "INTERPRETASI ANALISIS KOMPONEN UTAMA (PCA):\n",
    "==========================================\n\n",
    "1. Komponen Utama:\n",
    "   - PC1 menjelaskan ", round(variance_explained[1], 2), "% dari total varians\n",
    "   - PC2 menjelaskan ", round(variance_explained[2], 2), "% dari total varians\n",
    "   - Kumulatif PC1 dan PC2: ", round(sum(variance_explained[1:2]), 2), "%\n\n",
    "2. Eigenvalues:\n",
    "   - PC1: ", round(eigenvalues[1], 3), "\n",
    "   - PC2: ", round(eigenvalues[2], 3), "\n\n",
    "3. Kesimpulan:\n",
    if(variance_explained[1] > 30) {
      "   - PC1 menjelaskan proporsi varians yang cukup besar\n"
    } else {
      "   - PC1 menjelaskan proporsi varians yang relatif kecil\n"
    },
    if(sum(variance_explained[1:2]) > 60) {
      "   - Dua komponen pertama sudah menjelaskan sebagian besar varians\n"
    } else {
      "   - Diperlukan lebih banyak komponen untuk menjelaskan varians\n"
    },
    "   - Analisis PCA membantu mereduksi dimensi data dengan tetap mempertahankan informasi penting"
  )
  
  return(interpretation)
}

# Fungsi interpretasi untuk clustering
interpret_clustering <- function(cluster_result, k) {
  if(is.null(cluster_result)) return("Clustering tidak dapat dilakukan.")
  
  if("cluster" %in% names(cluster_result)) {
    # K-means
    cluster_sizes <- table(cluster_result$cluster)
    wss <- cluster_result$tot.withinss
    bss <- cluster_result$betweenss
    
    interpretation <- paste0(
      "INTERPRETASI ANALISIS CLUSTER (K-MEANS):\n",
      "=======================================\n\n",
      "1. Jumlah Cluster: ", k, "\n\n",
      "2. Ukuran Cluster:\n",
      paste(sapply(1:k, function(i) paste0("   - Cluster ", i, ": ", cluster_sizes[i], " observasi")), collapse = "\n"), "\n\n",
      "3. Kualitas Clustering:\n",
      "   - Within-cluster sum of squares: ", round(wss, 2), "\n",
      "   - Between-cluster sum of squares: ", round(bss, 2), "\n",
      "   - Total sum of squares: ", round(wss + bss, 2), "\n\n",
      "4. Kesimpulan:\n",
      if(bss/(wss + bss) > 0.7) {
        "   - Kualitas clustering sangat baik (cluster terpisah dengan jelas)\n"
      } else if(bss/(wss + bss) > 0.5) {
        "   - Kualitas clustering cukup baik\n"
      } else {
        "   - Kualitas clustering perlu diperbaiki (cluster kurang terpisah)\n"
      },
      "   - Setiap cluster menunjukkan pola karakteristik yang berbeda\n",
      "   - Hasil dapat digunakan untuk segmentasi dan pengambilan keputusan"
    )
  } else {
    # Hierarchical
    interpretation <- paste0(
      "INTERPRETASI ANALISIS CLUSTER (HIERARCHICAL):\n",
      "============================================\n\n",
      "1. Jumlah Cluster: ", k, "\n\n",
      "2. Metode: Ward's method dengan euclidean distance\n\n",
      "3. Interpretasi Dendrogram:\n",
      "   - Tinggi cabang menunjukkan jarak antar cluster\n",
      "   - Cabang yang lebih rendah menunjukkan cluster yang lebih mirip\n",
      "   - Pengelompokan dilakukan secara hierarkis\n\n",
      "4. Kesimpulan:\n",
      "   - Clustering hierarkis memberikan gambaran struktur data\n",
      "   - Dapat membantu menentukan jumlah cluster optimal\n",
      "   - Hasil menunjukkan hubungan kemiripan antar observasi"
    )
  }
  
  return(interpretation)
}

# ==============================================================================
# FUNGSI EKSPOR DATA
# ==============================================================================

# Fungsi untuk ekspor hasil analisis
export_analysis_results <- function(data, analysis_type, results) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("analysis_", analysis_type, "_", timestamp, ".csv")
  
  tryCatch({
    write.csv(results, filename, row.names = FALSE)
    return(filename)
  }, error = function(e) {
    return(NULL)
  })
}

# ==============================================================================
# FUNGSI VALIDASI DATA
# ==============================================================================

# Fungsi untuk validasi data
validate_data <- function(data) {
  if(is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "Data kosong atau tidak valid"))
  }
  
  numeric_cols <- sum(sapply(data, is.numeric))
  if(numeric_cols < 2) {
    return(list(valid = FALSE, message = "Data harus memiliki minimal 2 kolom numerik"))
  }
  
  missing_percentage <- sum(is.na(data)) / (nrow(data) * ncol(data)) * 100
  if(missing_percentage > 50) {
    return(list(valid = FALSE, message = "Terlalu banyak data yang hilang (>50%)"))
  }
  
  return(list(valid = TRUE, message = "Data valid untuk analisis"))
}

# ==============================================================================
# FUNGSI UTILITAS TAMBAHAN
# ==============================================================================

# Fungsi untuk membuat summary card
create_summary_card <- function(title, value, icon, color = "primary") {
  div(class = paste0("card border-", color),
      div(class = "card-body",
          div(class = "d-flex align-items-center",
              div(class = paste0("text-", color, " mr-3"),
                  icon(icon, class = "fa-2x")
              ),
              div(
                h4(class = "card-title mb-0", value),
                p(class = "card-text text-muted", title)
              )
          )
      )
  )
}

# Fungsi untuk progress bar
create_progress_bar <- function(value, max_value = 100, color = "primary") {
  percentage <- (value / max_value) * 100
  
  div(class = "progress mb-3",
      div(class = paste0("progress-bar bg-", color),
          role = "progressbar",
          style = paste0("width: ", percentage, "%"),
          `aria-valuenow` = value,
          `aria-valuemin` = 0,
          `aria-valuemax` = max_value,
          paste0(round(percentage, 1), "%")
      )
  )
}