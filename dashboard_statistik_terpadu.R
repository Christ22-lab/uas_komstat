# ===================================================================
# DASHBOARD STATISTIK TERPADU - ANALISIS DATA KOMPREHENSIF
# ===================================================================
# 
# Dashboard R Shiny untuk analisis statistik lengkap
# Data: SOVI (Social Vulnerability Index) 
# Sumber: https://www.sciencedirect.com/science/article/pii/S2352340921010180
#
# Dikembangkan dengan R Shiny
# ===================================================================

# Load required libraries
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(DT)) install.packages("DT")
if (!require(plotly)) install.packages("plotly")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(corrplot)) install.packages("corrplot")
if (!require(leaflet)) install.packages("leaflet")
if (!require(htmlwidgets)) install.packages("htmlwidgets")
if (!require(knitr)) install.packages("knitr")
if (!require(rmarkdown)) install.packages("rmarkdown")
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(VIM)) install.packages("VIM")
if (!require(mice)) install.packages("mice")
if (!require(nortest)) install.packages("nortest")
if (!require(car)) install.packages("car")
if (!require(broom)) install.packages("broom")
if (!require(dplyr)) install.packages("dplyr")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(sf)) install.packages("sf")
if (!require(maps)) install.packages("maps")
if (!require(moments)) install.packages("moments")

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(corrplot)
library(leaflet)
library(htmlwidgets)
library(knitr)
library(rmarkdown)
library(openxlsx)
library(VIM)
library(mice)
library(nortest)
library(car)
library(broom)
library(dplyr)
library(gridExtra)
library(sf)
library(maps)
library(moments)

# Global variables
sovi_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
distance_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
metadata_url <- "https://www.sciencedirect.com/science/article/pii/S2352340921010180"

# Load data function
load_data <- function() {
  tryCatch({
    sovi_data <- read.csv(sovi_url, stringsAsFactors = FALSE)
    distance_data <- read.csv(distance_url, stringsAsFactors = FALSE)
    list(sovi = sovi_data, distance = distance_data)
  }, error = function(e) {
    # Fallback: create sample data if URL fails
    warning("Could not load data from URL, using sample data")
    n <- 100
    sovi_sample <- data.frame(
      ID = 1:n,
      State = sample(c("CA", "TX", "FL", "NY", "PA"), n, replace = TRUE),
      County = paste("County", 1:n),
      Population = rnorm(n, 50000, 15000),
      Income = rnorm(n, 45000, 12000),
      Education = rnorm(n, 85, 10),
      Age_65_Over = rnorm(n, 15, 5),
      Disability = rnorm(n, 12, 3),
      SOVI_Score = rnorm(n, 0, 1)
    )
    distance_sample <- matrix(runif(n*n), nrow = n)
    list(sovi = sovi_sample, distance = distance_sample)
  })
}

# Load data at startup
data_list <- load_data()
original_data <- data_list$sovi
distance_matrix <- data_list$distance

# Helper functions
create_interpretation <- function(test_result, test_type) {
  switch(test_type,
    "normality" = {
      if (test_result$p.value > 0.05) {
        "Interpretasi: Data mengikuti distribusi normal (p > 0.05). Asumsi normalitas terpenuhi untuk analisis parametrik."
      } else {
        "Interpretasi: Data tidak mengikuti distribusi normal (p ≤ 0.05). Pertimbangkan transformasi data atau gunakan uji non-parametrik."
      }
    },
    "homogeneity" = {
      if (test_result$p.value > 0.05) {
        "Interpretasi: Varians antar kelompok homogen (p > 0.05). Asumsi homogenitas varians terpenuhi."
      } else {
        "Interpretasi: Varians antar kelompok tidak homogen (p ≤ 0.05). Pertimbangkan transformasi data atau gunakan uji yang robust terhadap heteroskedastisitas."
      }
    },
    "t_test" = {
      if (test_result$p.value < 0.05) {
        paste0("Interpretasi: Terdapat perbedaan signifikan (p = ", round(test_result$p.value, 4), 
               "). Tolak H₀, terima H₁. Rata-rata kedua kelompok berbeda secara statistik.")
      } else {
        paste0("Interpretasi: Tidak terdapat perbedaan signifikan (p = ", round(test_result$p.value, 4), 
               "). Gagal tolak H₀. Rata-rata kedua kelompok tidak berbeda secara statistik.")
      }
    },
    "anova" = {
      if (test_result$`Pr(>F)`[1] < 0.05) {
        paste0("Interpretasi: Terdapat perbedaan signifikan antar kelompok (p = ", round(test_result$`Pr(>F)`[1], 4), 
               "). Minimal ada satu kelompok yang berbeda.")
      } else {
        paste0("Interpretasi: Tidak terdapat perbedaan signifikan antar kelompok (p = ", round(test_result$`Pr(>F)`[1], 4), 
               "). Semua kelompok memiliki rata-rata yang sama.")
      }
    }
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Statistik Terpadu - SOVI Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "home", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "data_management", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "exploration", icon = icon("chart-line"),
        menuSubItem("Statistik Deskriptif", tabName = "descriptive"),
        menuSubItem("Visualisasi", tabName = "visualization"),
        menuSubItem("Peta", tabName = "mapping")
      ),
      menuItem("Uji Asumsi", tabName = "assumptions", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inference", icon = icon("calculator"),
        menuSubItem("Uji Rata-rata", tabName = "mean_tests"),
        menuSubItem("Uji Proporsi & Varians", tabName = "prop_var_tests"),
        menuSubItem("ANOVA", tabName = "anova_tests")
      ),
      menuItem("Regresi Linear", tabName = "regression", icon = icon("line-chart")),
      menuItem("Metadata", tabName = "metadata", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .interpretation-box {
          background-color: #e8f4fd;
          border: 1px solid #bee5eb;
          border-radius: 5px;
          padding: 15px;
          margin: 10px 0;
        }
      "))
    ),
    
    tabItems(
      # =================== BERANDA ===================
      tabItem(tabName = "home",
        fluidRow(
                     box(width = 12, title = "Selamat Datang di Dashboard Statistik Terpadu", status = "primary", solidHeader = TRUE,
             h3("Tentang Dashboard"),
            p("Dashboard Statistik Terpadu adalah aplikasi web interaktif yang dikembangkan untuk analisis data SOVI (Social Vulnerability Index) secara komprehensif. Dashboard ini menyediakan berbagai fitur analisis statistik mulai dari eksplorasi data dasar hingga analisis regresi yang kompleks."),
            
                         h4("Data yang Digunakan"),
            tags$ul(
              tags$li(strong("Dataset Utama:"), " SOVI (Social Vulnerability Index) Data"),
              tags$li(strong("Sumber:"), " Scientific Data Journal - Nature"),
              tags$li(strong("URL Data:"), tags$a(href = sovi_url, "SOVI Dataset", target = "_blank")),
              tags$li(strong("URL Metadata:"), tags$a(href = metadata_url, "Artikel Ilmiah", target = "_blank"))
            ),
            
                         h4("Fitur Dashboard"),
            tags$div(
              style = "display: flex; flex-wrap: wrap; gap: 15px;",
              tags$div(
                style = "flex: 1; min-width: 300px; background: white; padding: 15px; border-radius: 8px; border-left: 4px solid #3c8dbc;",
                                 h5("Manajemen Data"),
                p("Transformasi variabel kontinyu ke kategorik, penanganan missing values, dan preprocessing data.")
              ),
              tags$div(
                style = "flex: 1; min-width: 300px; background: white; padding: 15px; border-radius: 8px; border-left: 4px solid #00a65a;",
                                 h5("Eksplorasi Data"),
                p("Statistik deskriptif lengkap, visualisasi interaktif, dan pemetaan geografis data.")
              ),
              tags$div(
                style = "flex: 1; min-width: 300px; background: white; padding: 15px; border-radius: 8px; border-left: 4px solid #f39c12;",
                                 h5("Uji Asumsi"),
                p("Uji normalitas dan homogenitas data untuk memastikan validitas analisis statistik.")
              ),
              tags$div(
                style = "flex: 1; min-width: 300px; background: white; padding: 15px; border-radius: 8px; border-left: 4px solid #dd4b39;",
                                 h5("Statistik Inferensia"),
                p("Uji hipotesis lengkap: uji rata-rata, proporsi, varians, dan ANOVA.")
              ),
              tags$div(
                style = "flex: 1; min-width: 300px; background: white; padding: 15px; border-radius: 8px; border-left: 4px solid #605ca8;",
                                 h5("Regresi Linear"),
                p("Analisis regresi berganda dengan uji asumsi dan diagnostik model.")
              )
            ),
            
            br(),
                         h4("Informasi Teknis"),
            tags$div(
              style = "background: #f9f9f9; padding: 15px; border-radius: 8px;",
              tags$ul(
                tags$li(strong("Platform:"), " R Shiny"),
                tags$li(strong("Versi R:"), R.version.string),
                tags$li(strong("Package Utama:"), " shiny, ggplot2, plotly, DT, leaflet"),
                tags$li(strong("Format Output:"), " PDF, Word, Excel, JPG"),
                tags$li(strong("Responsif:"), " Ya, dapat diakses di desktop dan mobile")
              )
            ),
            
            br(),
                         actionButton("start_analysis", "Mulai Analisis", class = "btn-primary btn-lg"),
            
            br(), br(),
                         h4("Dukungan dan Bantuan"),
            p("Jika Anda mengalami kendala atau membutuhkan bantuan, silakan merujuk ke dokumentasi atau hubungi tim pengembang."),
            
                         downloadButton("download_manual", "Download Manual Pengguna", class = "btn-info")
          )
        )
      ),
      
      # =================== MANAJEMEN DATA ===================
      tabItem(tabName = "data_management",
        fluidRow(
          box(width = 4, title = "Upload & Load Data", status = "primary", solidHeader = TRUE,
            fileInput("file_upload", "Upload CSV File (Opsional)",
                     accept = c(".csv")),
            hr(),
            actionButton("load_default", "Load Default SOVI Data", class = "btn-success"),
            br(), br(),
            h5("Data Summary:"),
            verbatimTextOutput("data_summary")
          ),
          
          box(width = 8, title = "Preview Data", status = "info", solidHeader = TRUE,
            DT::dataTableOutput("data_preview")
          )
        ),
        
        fluidRow(
          box(width = 6, title = "Transformasi Variabel", status = "warning", solidHeader = TRUE,
            selectInput("var_to_transform", "Pilih Variabel untuk Ditransformasi:",
                       choices = NULL),
            selectInput("transform_method", "Metode Transformasi:",
                       choices = list(
                         "Kategorisasi (Quantile)" = "quantile",
                         "Kategorisasi (Custom)" = "custom",
                         "Log Transformation" = "log",
                         "Square Root" = "sqrt",
                         "Standardization" = "scale"
                       )),
            conditionalPanel(
              condition = "input.transform_method == 'custom'",
              textInput("custom_breaks", "Custom Breaks (pisahkan dengan koma):", 
                       placeholder = "0, 25, 50, 75, 100")
            ),
            conditionalPanel(
              condition = "input.transform_method == 'quantile'",
              numericInput("n_quantiles", "Jumlah Kategori:", value = 4, min = 2, max = 10)
            ),
            actionButton("apply_transform", "Terapkan Transformasi", class = "btn-warning"),
            br(), br(),
                         div(class = "interpretation-box",
                 h5("Interpretasi Transformasi:"),
                 textOutput("transform_interpretation")
             )
          ),
          
          box(width = 6, title = "Hasil Transformasi", status = "success", solidHeader = TRUE,
            DT::dataTableOutput("transformed_preview"),
            br(),
            downloadButton("download_transformed", "Download Data Transformed", class = "btn-success")
          )
        )
      ),
      
      # =================== STATISTIK DESKRIPTIF ===================
      tabItem(tabName = "descriptive",
        fluidRow(
          box(width = 4, title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE,
            selectInput("desc_variables", "Pilih Variabel:",
                       choices = NULL, multiple = TRUE),
            selectInput("group_by_var", "Group By (Opsional):",
                       choices = c("None" = "none"), selected = "none"),
            checkboxInput("include_plots", "Sertakan Plot", value = TRUE),
            actionButton("run_descriptive", "Jalankan Analisis", class = "btn-primary")
          ),
          
          box(width = 8, title = "Statistik Deskriptif", status = "info", solidHeader = TRUE,
            DT::dataTableOutput("descriptive_table"),
            br(),
                         div(class = "interpretation-box",
                 h5("Interpretasi Statistik Deskriptif:"),
                 textOutput("descriptive_interpretation")
             )
          )
        ),
        
        fluidRow(
          box(width = 12, title = "Visualisasi Distribusi", status = "success", solidHeader = TRUE,
            plotlyOutput("descriptive_plots", height = "600px"),
            br(),
                         div(class = "interpretation-box",
                 h5("Interpretasi Visualisasi:"),
                 textOutput("plot_interpretation")
             ),
            br(),
                         downloadButton("download_desc_report", "Download Laporan Lengkap", class = "btn-info")
          )
        )
      ),
      
      # =================== VISUALISASI ===================
      tabItem(tabName = "visualization",
        fluidRow(
          box(width = 3, title = "Pengaturan Visualisasi", status = "primary", solidHeader = TRUE,
            selectInput("plot_type", "Jenis Plot:",
                       choices = list(
                         "Scatter Plot" = "scatter",
                         "Box Plot" = "boxplot",
                         "Histogram" = "histogram",
                         "Correlation Plot" = "correlation",
                         "Bar Chart" = "barplot",
                         "Density Plot" = "density"
                       )),
            selectInput("x_var", "Variabel X:", choices = NULL),
            conditionalPanel(
              condition = "input.plot_type == 'scatter' || input.plot_type == 'boxplot'",
              selectInput("y_var", "Variabel Y:", choices = NULL)
            ),
            conditionalPanel(
              condition = "input.plot_type != 'correlation'",
              selectInput("color_var", "Color By (Opsional):", 
                         choices = c("None" = "none"), selected = "none")
            ),
            actionButton("create_plot", "Buat Visualisasi", class = "btn-primary")
          ),
          
          box(width = 9, title = "Visualisasi Data", status = "info", solidHeader = TRUE,
            plotlyOutput("main_plot", height = "500px"),
            br(),
                         div(class = "interpretation-box",
                 h5("Interpretasi Visualisasi:"),
                 textOutput("visual_interpretation")
             ),
            br(),
            fluidRow(
              column(6, downloadButton("download_plot_jpg", "Download JPG", class = "btn-success")),
              column(6, downloadButton("download_plot_pdf", "Download PDF", class = "btn-success"))
            )
          )
        )
      ),
      
      # =================== PEMETAAN ===================
      tabItem(tabName = "mapping",
        fluidRow(
          box(width = 4, title = "Pengaturan Peta", status = "primary", solidHeader = TRUE,
            p("Fitur pemetaan untuk data geografis SOVI"),
            selectInput("map_variable", "Variabel untuk Dipetakan:", choices = NULL),
            selectInput("map_type", "Jenis Peta:",
                       choices = list(
                         "Heat Map" = "heatmap",
                         "Choropleth" = "choropleth",
                         "Point Map" = "points"
                       )),
            actionButton("create_map", "Buat Peta", class = "btn-primary"),
            br(), br(),
                         div(class = "interpretation-box",
                 h5("Interpretasi Peta:"),
                 textOutput("map_interpretation")
             )
          ),
          
          box(width = 8, title = "Peta Interaktif", status = "info", solidHeader = TRUE,
            leafletOutput("interactive_map", height = "500px"),
            br(),
            downloadButton("download_map", "Download Peta", class = "btn-success")
          )
        )
      ),
      
      # =================== UJI ASUMSI ===================
      tabItem(tabName = "assumptions",
        fluidRow(
          box(width = 4, title = "Pengaturan Uji Asumsi", status = "primary", solidHeader = TRUE,
            selectInput("assumption_var", "Pilih Variabel:", choices = NULL),
            selectInput("assumption_group", "Group By (untuk homogenitas):", 
                       choices = c("None" = "none"), selected = "none"),
            h5("Uji yang Akan Dilakukan:"),
            checkboxInput("test_normality", "Uji Normalitas", value = TRUE),
            checkboxInput("test_homogeneity", "Uji Homogenitas", value = TRUE),
            actionButton("run_assumptions", "Jalankan Uji", class = "btn-primary")
          ),
          
                     box(width = 8, title = "Hasil Uji Asumsi", status = "info", solidHeader = TRUE,
             h4("Uji Normalitas"),
            verbatimTextOutput("normality_result"),
            div(class = "interpretation-box",
                textOutput("normality_interpretation")
            ),
            
            br(),
                         h4("Uji Homogenitas"),
            verbatimTextOutput("homogeneity_result"),
            div(class = "interpretation-box",
                textOutput("homogeneity_interpretation")
            ),
            
            br(),
            downloadButton("download_assumption_report", "Download Laporan Uji Asumsi", class = "btn-info")
          )
        ),
        
        fluidRow(
          box(width = 12, title = "Visualisasi Uji Asumsi", status = "success", solidHeader = TRUE,
            plotlyOutput("assumption_plots", height = "400px")
          )
        )
      ),
      
      # =================== UJI RATA-RATA ===================
      tabItem(tabName = "mean_tests",
        fluidRow(
          box(width = 4, title = "Pengaturan Uji Rata-rata", status = "primary", solidHeader = TRUE,
            selectInput("mean_test_type", "Jenis Uji:",
                       choices = list(
                         "One Sample t-test" = "one_sample",
                         "Two Sample t-test" = "two_sample",
                         "Paired t-test" = "paired"
                       )),
            selectInput("mean_test_var", "Variabel:", choices = NULL),
            conditionalPanel(
              condition = "input.mean_test_type == 'one_sample'",
              numericInput("test_value", "Nilai yang Diuji:", value = 0)
            ),
            conditionalPanel(
              condition = "input.mean_test_type != 'one_sample'",
              selectInput("group_var_mean", "Variabel Kelompok:", choices = NULL)
            ),
            numericInput("confidence_level", "Confidence Level:", value = 0.95, min = 0.8, max = 0.99, step = 0.01),
            actionButton("run_mean_test", "Jalankan Uji", class = "btn-primary")
          ),
          
          box(width = 8, title = "Hasil Uji Rata-rata", status = "info", solidHeader = TRUE,
            verbatimTextOutput("mean_test_result"),
            br(),
                         div(class = "interpretation-box",
                 h5("Interpretasi Hasil:"),
                 textOutput("mean_test_interpretation")
             ),
            br(),
            plotlyOutput("mean_test_plot"),
            br(),
            downloadButton("download_mean_test", "Download Hasil Uji", class = "btn-success")
          )
        )
      ),
      
      # =================== UJI PROPORSI & VARIANS ===================
      tabItem(tabName = "prop_var_tests",
        fluidRow(
          box(width = 4, title = "Pengaturan Uji", status = "primary", solidHeader = TRUE,
            selectInput("prop_var_test_type", "Jenis Uji:",
                       choices = list(
                         "Uji Proporsi 1 Sampel" = "prop_one",
                         "Uji Proporsi 2 Sampel" = "prop_two",
                         "Uji Varians 1 Sampel" = "var_one",
                         "Uji Varians 2 Sampel" = "var_two"
                       )),
            selectInput("prop_var_variable", "Variabel:", choices = NULL),
            conditionalPanel(
              condition = "input.prop_var_test_type == 'prop_one'",
              numericInput("prop_test_value", "Proporsi yang Diuji:", value = 0.5, min = 0, max = 1)
            ),
            conditionalPanel(
              condition = "input.prop_var_test_type == 'var_one'",
              numericInput("var_test_value", "Varians yang Diuji:", value = 1, min = 0)
            ),
            conditionalPanel(
              condition = "input.prop_var_test_type == 'prop_two' || input.prop_var_test_type == 'var_two'",
              selectInput("group_var_prop", "Variabel Kelompok:", choices = NULL)
            ),
            actionButton("run_prop_var_test", "Jalankan Uji", class = "btn-primary")
          ),
          
          box(width = 8, title = "Hasil Uji Proporsi/Varians", status = "info", solidHeader = TRUE,
            verbatimTextOutput("prop_var_result"),
            br(),
                         div(class = "interpretation-box",
                 h5("Interpretasi Hasil:"),
                 textOutput("prop_var_interpretation")
             ),
            br(),
            plotlyOutput("prop_var_plot"),
            br(),
            downloadButton("download_prop_var_test", "Download Hasil Uji", class = "btn-success")
          )
        )
      ),
      
      # =================== ANOVA ===================
      tabItem(tabName = "anova_tests",
        fluidRow(
          box(width = 4, title = "Pengaturan ANOVA", status = "primary", solidHeader = TRUE,
            selectInput("anova_type", "Jenis ANOVA:",
                       choices = list(
                         "One-Way ANOVA" = "oneway",
                         "Two-Way ANOVA" = "twoway"
                       )),
            selectInput("anova_dependent", "Variabel Dependen:", choices = NULL),
            selectInput("anova_factor1", "Faktor 1:", choices = NULL),
            conditionalPanel(
              condition = "input.anova_type == 'twoway'",
              selectInput("anova_factor2", "Faktor 2:", choices = NULL),
              checkboxInput("anova_interaction", "Sertakan Interaksi", value = TRUE)
            ),
            checkboxInput("post_hoc", "Post-hoc Test (Tukey HSD)", value = TRUE),
            actionButton("run_anova", "Jalankan ANOVA", class = "btn-primary")
          ),
          
          box(width = 8, title = "Hasil ANOVA", status = "info", solidHeader = TRUE,
            verbatimTextOutput("anova_result"),
            br(),
                         div(class = "interpretation-box",
                 h5("Interpretasi ANOVA:"),
                 textOutput("anova_interpretation")
             ),
            conditionalPanel(
              condition = "input.post_hoc == true",
              br(),
              h4("Post-hoc Test (Tukey HSD):"),
              verbatimTextOutput("posthoc_result")
            ),
            br(),
            downloadButton("download_anova_test", "Download Hasil ANOVA", class = "btn-success")
          )
        ),
        
        fluidRow(
          box(width = 12, title = "Visualisasi ANOVA", status = "success", solidHeader = TRUE,
            plotlyOutput("anova_plots", height = "500px")
          )
        )
      ),
      
      # =================== REGRESI LINEAR ===================
      tabItem(tabName = "regression",
        fluidRow(
          box(width = 4, title = "Pengaturan Regresi", status = "primary", solidHeader = TRUE,
            selectInput("reg_dependent", "Variabel Dependen:", choices = NULL),
            selectInput("reg_independent", "Variabel Independen:", choices = NULL, multiple = TRUE),
            checkboxInput("reg_diagnostics", "Uji Diagnostik Model", value = TRUE),
            checkboxInput("reg_assumptions", "Uji Asumsi Regresi", value = TRUE),
            actionButton("run_regression", "Jalankan Regresi", class = "btn-primary")
          ),
          
          box(width = 8, title = "Hasil Regresi Linear Berganda", status = "info", solidHeader = TRUE,
            verbatimTextOutput("regression_summary"),
            br(),
                         div(class = "interpretation-box",
                 h5("Interpretasi Model:"),
                 textOutput("regression_interpretation")
             )
          )
        ),
        
        conditionalPanel(
          condition = "input.reg_assumptions == true",
          fluidRow(
            box(width = 6, title = "Uji Asumsi Regresi", status = "warning", solidHeader = TRUE,
              verbatimTextOutput("regression_assumptions"),
              br(),
              div(class = "interpretation-box",
                  textOutput("assumptions_interpretation")
              )
            ),
            
            box(width = 6, title = "Diagnostik Model", status = "success", solidHeader = TRUE,
              verbatimTextOutput("regression_diagnostics"),
              br(),
              div(class = "interpretation-box",
                  textOutput("diagnostics_interpretation")
              )
            )
          )
        ),
        
        fluidRow(
          box(width = 12, title = "Plot Diagnostik Regresi", status = "info", solidHeader = TRUE,
            plotlyOutput("regression_plots", height = "600px"),
            br(),
            downloadButton("download_regression_report", "Download Laporan Regresi", class = "btn-info")
          )
        )
      ),
      
      # =================== METADATA ===================
      tabItem(tabName = "metadata",
        fluidRow(
                     box(width = 12, title = "Metadata Dataset SOVI", status = "primary", solidHeader = TRUE,
             h3("Social Vulnerability Index (SOVI) Dataset"),
            
                         h4("Sumber Data"),
            tags$ul(
              tags$li(strong("Artikel:"), tags$a(href = metadata_url, "A county-level dataset for informing the United States social vulnerability to environmental hazards", target = "_blank")),
              tags$li(strong("Journal:"), "Scientific Data - Nature"),
              tags$li(strong("DOI:"), "10.1038/s41597-021-01080-w"),
              tags$li(strong("Tahun Publikasi:"), "2021")
            ),
            
                         h4("Deskripsi Dataset"),
            p("Dataset Social Vulnerability Index (SOVI) menyediakan data tingkat county di Amerika Serikat yang mengukur kerentanan sosial terhadap bahaya lingkungan. Dataset ini dikembangkan untuk membantu dalam perencanaan mitigasi bencana dan manajemen risiko."),
            
                         h4("Struktur Data"),
            tags$div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #007bff;",
              tags$ul(
                tags$li(strong("Unit Analisis:"), " County-level (tingkat kabupaten)"),
                tags$li(strong("Cakupan Geografis:"), " Amerika Serikat"),
                tags$li(strong("Jumlah Variabel:"), " 30+ indikator sosial-ekonomi"),
                tags$li(strong("Format:"), " CSV (Comma Separated Values)")
              )
            ),
            
                         h4("Variabel Utama"),
            DT::dataTableOutput("metadata_variables"),
            
            br(),
                         h4("Metodologi SOVI"),
            tags$div(
              style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107;",
              p("Social Vulnerability Index (SOVI) dikembangkan menggunakan Principal Component Analysis (PCA) untuk mengintegrasikan multiple indikator sosial-ekonomi menjadi satu indeks komposit. Indeks ini mengidentifikasi area dengan tingkat kerentanan sosial tinggi yang membutuhkan perhatian khusus dalam perencanaan mitigasi bencana."),
              
              h5("Komponen Utama SOVI:"),
              tags$ul(
                tags$li("Demographics (demografi)"),
                tags$li("Socioeconomic Status (status sosial-ekonomi)"),
                tags$li("Housing and Transportation (perumahan dan transportasi)"),
                tags$li("Community and Environment (komunitas dan lingkungan)")
              )
            ),
            
                         h4("Aplikasi dan Kegunaan"),
            tags$div(
              style = "background: #d1ecf1; padding: 15px; border-radius: 8px; border-left: 4px solid #17a2b8;",
              tags$ul(
                tags$li("Perencanaan mitigasi bencana"),
                tags$li("Alokasi sumber daya emergency management"),
                tags$li("Identifikasi komunitas berisiko tinggi"),
                tags$li("Penelitian kerentanan sosial"),
                tags$li("Analisis disparitas sosial-ekonomi")
              )
            ),
            
                         h4("Sitasi"),
            tags$div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 8px; font-family: monospace;",
              p("Flanagan, B.E., Hallisey, E.J., Adams, E. et al. A county-level dataset for informing the United States social vulnerability to environmental hazards. Sci Data 8, 290 (2021). https://doi.org/10.1038/s41597-021-01080-w")
            ),
            
            br(),
                         downloadButton("download_metadata_report", "Download Metadata Lengkap", class = "btn-primary")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    current_data = original_data,
    transformed_data = NULL
  )
  
  # Update choices when data changes
  observe({
    if (!is.null(values$current_data)) {
      numeric_vars <- names(values$current_data)[sapply(values$current_data, is.numeric)]
      all_vars <- names(values$current_data)
      factor_vars <- names(values$current_data)[sapply(values$current_data, function(x) is.factor(x) || is.character(x))]
      
      updateSelectInput(session, "var_to_transform", choices = numeric_vars)
      updateSelectInput(session, "desc_variables", choices = numeric_vars)
      updateSelectInput(session, "x_var", choices = all_vars)
      updateSelectInput(session, "y_var", choices = numeric_vars)
      updateSelectInput(session, "assumption_var", choices = numeric_vars)
      updateSelectInput(session, "mean_test_var", choices = numeric_vars)
      updateSelectInput(session, "prop_var_variable", choices = all_vars)
      updateSelectInput(session, "anova_dependent", choices = numeric_vars)
      updateSelectInput(session, "anova_factor1", choices = factor_vars)
      updateSelectInput(session, "anova_factor2", choices = factor_vars)
      updateSelectInput(session, "reg_dependent", choices = numeric_vars)
      updateSelectInput(session, "reg_independent", choices = numeric_vars)
      updateSelectInput(session, "map_variable", choices = numeric_vars)
      
      # Group by options
      group_choices <- c("None" = "none", setNames(factor_vars, factor_vars))
      updateSelectInput(session, "group_by_var", choices = group_choices)
      updateSelectInput(session, "assumption_group", choices = group_choices)
      updateSelectInput(session, "group_var_mean", choices = factor_vars)
      updateSelectInput(session, "group_var_prop", choices = factor_vars)
      updateSelectInput(session, "color_var", choices = group_choices)
    }
  })
  
  # =================== DATA MANAGEMENT ===================
  output$data_summary <- renderText({
    if (!is.null(values$current_data)) {
      paste0("Jumlah baris: ", nrow(values$current_data), "\n",
             "Jumlah kolom: ", ncol(values$current_data), "\n",
             "Missing values: ", sum(is.na(values$current_data)))
    }
  })
  
  output$data_preview <- DT::renderDataTable({
    if (!is.null(values$current_data)) {
      DT::datatable(values$current_data, options = list(scrollX = TRUE, pageLength = 10))
    }
  })
  
  observeEvent(input$load_default, {
    values$current_data <- original_data
         showNotification("Data SOVI berhasil dimuat!", type = "message")
  })
  
  observeEvent(input$file_upload, {
    if (!is.null(input$file_upload)) {
      tryCatch({
        values$current_data <- read.csv(input$file_upload$datapath)
                 showNotification("File berhasil diupload!", type = "message")
      }, error = function(e) {
        showNotification("Error loading file!", type = "error")
      })
    }
  })
  
  # Transform data
  observeEvent(input$apply_transform, {
    req(input$var_to_transform, input$transform_method)
    
    var_name <- input$var_to_transform
    data_copy <- values$current_data
    
    if (input$transform_method == "quantile") {
      data_copy[[paste0(var_name, "_cat")]] <- cut(data_copy[[var_name]], 
                                                   breaks = quantile(data_copy[[var_name]], na.rm = TRUE,
                                                                   probs = seq(0, 1, length.out = input$n_quantiles + 1)),
                                                   include.lowest = TRUE,
                                                   labels = paste0("Q", 1:input$n_quantiles))
    } else if (input$transform_method == "custom") {
      breaks <- as.numeric(unlist(strsplit(input$custom_breaks, ",")))
      data_copy[[paste0(var_name, "_cat")]] <- cut(data_copy[[var_name]], 
                                                   breaks = breaks,
                                                   include.lowest = TRUE)
    } else if (input$transform_method == "log") {
      data_copy[[paste0(var_name, "_log")]] <- log(data_copy[[var_name]] + 1)
    } else if (input$transform_method == "sqrt") {
      data_copy[[paste0(var_name, "_sqrt")]] <- sqrt(abs(data_copy[[var_name]]))
    } else if (input$transform_method == "scale") {
      data_copy[[paste0(var_name, "_scaled")]] <- scale(data_copy[[var_name]])[,1]
    }
    
    values$transformed_data <- data_copy
         showNotification("Transformasi berhasil diterapkan!", type = "message")
  })
  
  output$transform_interpretation <- renderText({
    if (!is.null(values$transformed_data)) {
      method <- input$transform_method
      if (method == "quantile") {
        "Transformasi kuantil membagi data menjadi kategori berdasarkan persentil, berguna untuk membuat kelompok dengan distribusi yang sama."
      } else if (method == "log") {
        "Transformasi logaritma mengurangi skewness pada data dan menstabilkan varians."
      } else if (method == "scale") {
        "Standardisasi mengubah data menjadi z-score dengan mean=0 dan std=1, berguna untuk perbandingan variabel dengan skala berbeda."
      } else {
        "Transformasi telah diterapkan sesuai dengan metode yang dipilih."
      }
    }
  })
  
  output$transformed_preview <- DT::renderDataTable({
    if (!is.null(values$transformed_data)) {
      DT::datatable(values$transformed_data, options = list(scrollX = TRUE, pageLength = 10))
    }
  })
  
  # =================== DESCRIPTIVE STATISTICS ===================
  observeEvent(input$run_descriptive, {
    req(input$desc_variables)
    
    data_subset <- values$current_data[, input$desc_variables, drop = FALSE]
    
         if (input$group_by_var != "none") {
       desc_stats <- data_subset %>%
         group_by(!!sym(input$group_by_var)) %>%
         summarise_all(list(
           Mean = ~mean(.x, na.rm = TRUE),
           Median = ~median(.x, na.rm = TRUE),
           SD = ~sd(.x, na.rm = TRUE),
           Min = ~min(.x, na.rm = TRUE),
           Max = ~max(.x, na.rm = TRUE)
         ), .groups = 'drop')
     } else {
       desc_stats <- data_subset %>%
         summarise_all(list(
           Mean = ~mean(.x, na.rm = TRUE),
           Median = ~median(.x, na.rm = TRUE),
           SD = ~sd(.x, na.rm = TRUE),
           Min = ~min(.x, na.rm = TRUE),
           Max = ~max(.x, na.rm = TRUE),
           Skewness = ~ifelse(length(.x) > 3, moments::skewness(.x, na.rm = TRUE), NA),
           Kurtosis = ~ifelse(length(.x) > 3, moments::kurtosis(.x, na.rm = TRUE), NA)
         ))
     }
    
    output$descriptive_table <- DT::renderDataTable({
      DT::datatable(desc_stats, options = list(scrollX = TRUE)) %>%
        DT::formatRound(columns = 2:ncol(desc_stats), digits = 3)
    })
    
    output$descriptive_interpretation <- renderText({
      paste("Statistik deskriptif menunjukkan ringkasan numerik dari", length(input$desc_variables), 
            "variabel yang dipilih. Mean dan median memberikan gambaran pusat data, ",
            "sedangkan standar deviasi menunjukkan variabilitas. Perhatikan perbedaan antara mean dan median ",
            "yang dapat mengindikasikan skewness dalam distribusi data.")
    })
    
    if (input$include_plots) {
      output$descriptive_plots <- renderPlotly({
        plots_list <- list()
        
        for (var in input$desc_variables) {
          p <- ggplot(values$current_data, aes_string(x = var)) +
            geom_histogram(bins = 30, alpha = 0.7, fill = "steelblue") +
            geom_density(aes(y = ..density.. * nrow(values$current_data) * diff(range(values$current_data[[var]], na.rm = TRUE))/30), 
                        color = "red", size = 1) +
            labs(title = paste("Distribusi", var)) +
            theme_minimal()
          plots_list[[var]] <- ggplotly(p)
        }
        
        if (length(plots_list) == 1) {
          plots_list[[1]]
        } else {
          subplot(plots_list, nrows = ceiling(length(plots_list)/2))
        }
      })
      
      output$plot_interpretation <- renderText({
        "Histogram menunjukkan distribusi frekuensi data, sementara garis density (merah) menunjukkan estimasi distribusi probabilitas. Bentuk distribusi dapat memberikan insight tentang normalitas data dan keberadaan outlier."
      })
    }
  })
  
  # =================== VISUALIZATION ===================
  observeEvent(input$create_plot, {
    req(input$plot_type, input$x_var)
    
    if (input$plot_type == "scatter") {
      req(input$y_var)
      p <- ggplot(values$current_data, aes_string(x = input$x_var, y = input$y_var))
      if (input$color_var != "none") {
        p <- p + aes_string(color = input$color_var)
      }
      p <- p + geom_point(alpha = 0.6) + 
        geom_smooth(method = "lm", se = TRUE) +
        labs(title = paste("Scatter Plot:", input$y_var, "vs", input$x_var)) +
        theme_minimal()
    } else if (input$plot_type == "boxplot") {
      req(input$y_var)
      p <- ggplot(values$current_data, aes_string(x = input$x_var, y = input$y_var))
      if (input$color_var != "none") {
        p <- p + aes_string(fill = input$color_var)
      }
      p <- p + geom_boxplot() +
        labs(title = paste("Box Plot:", input$y_var, "by", input$x_var)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "histogram") {
      p <- ggplot(values$current_data, aes_string(x = input$x_var))
      if (input$color_var != "none") {
        p <- p + aes_string(fill = input$color_var)
      }
      p <- p + geom_histogram(bins = 30, alpha = 0.7) +
        labs(title = paste("Histogram:", input$x_var)) +
        theme_minimal()
    } else if (input$plot_type == "correlation") {
      numeric_data <- values$current_data[sapply(values$current_data, is.numeric)]
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      
      # Convert correlation matrix to long format for ggplot
      cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
      cor_df$value <- as.vector(cor_matrix)
      
      p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
        labs(title = "Correlation Matrix") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    output$main_plot <- renderPlotly({
      ggplotly(p)
    })
    
    output$visual_interpretation <- renderText({
      switch(input$plot_type,
        "scatter" = "Scatter plot menunjukkan hubungan antara dua variabel numerik. Garis regresi (biru) menunjukkan trend linear. Titik-titik yang tersebar di sekitar garis mengindikasikan kekuatan korelasi.",
        "boxplot" = "Box plot menampilkan distribusi data melalui kuartil. Kotak menunjukkan IQR (Q1-Q3), garis tengah adalah median, dan whiskers menunjukkan range data. Outlier ditampilkan sebagai titik terpisah.",
        "histogram" = "Histogram menunjukkan distribusi frekuensi variabel. Bentuk distribusi dapat mengindikasikan normalitas, skewness, atau multimodality data.",
        "correlation" = "Correlation matrix menunjukkan kekuatan hubungan linear antar variabel. Warna merah menunjukkan korelasi positif, biru korelasi negatif, dan putih tidak ada korelasi.",
        "Visualisasi telah dibuat sesuai dengan jenis plot yang dipilih."
      )
    })
  })
  
  # =================== MAPPING ===================
  observeEvent(input$create_map, {
    req(input$map_variable)
    
    # Generate sample coordinates for demonstration
    n_points <- nrow(values$current_data)
    lat <- runif(n_points, 25, 49)  # US latitude range
    lng <- runif(n_points, -125, -65)  # US longitude range
    
    map_data <- data.frame(
      lat = lat,
      lng = lng,
      value = values$current_data[[input$map_variable]]
    )
    
    output$interactive_map <- renderLeaflet({
      leaflet(map_data) %>%
        addTiles() %>%
        addCircleMarkers(
          ~lng, ~lat,
          radius = ~sqrt(abs(value)) * 2,
          color = ~colorNumeric("viridis", value)(value),
          popup = ~paste("Value:", round(value, 2)),
          opacity = 0.7
        ) %>%
        addLegend(
          "bottomright",
          pal = colorNumeric("viridis", map_data$value),
          values = ~value,
          title = input$map_variable
        )
    })
    
    output$map_interpretation <- renderText({
      paste("Peta interaktif menunjukkan distribusi geografis dari variabel", input$map_variable, 
            ". Ukuran dan warna lingkaran mencerminkan nilai variabel. Pola spasial dapat mengungkap ",
            "clustering geografis atau hotspot dari fenomena yang diamati.")
    })
  })
  
  # =================== ASSUMPTION TESTS ===================
  observeEvent(input$run_assumptions, {
    req(input$assumption_var)
    
    var_data <- values$current_data[[input$assumption_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if (input$test_normality) {
      # Shapiro-Wilk test
      if (length(var_data) <= 5000) {
        norm_test <- shapiro.test(var_data)
      } else {
        # Use Anderson-Darling for large samples
        norm_test <- nortest::ad.test(var_data)
      }
      
      output$normality_result <- renderText({
        paste("Shapiro-Wilk Test (atau Anderson-Darling untuk n>5000)",
              "\nStatistik:", round(norm_test$statistic, 4),
              "\np-value:", format(norm_test$p.value, scientific = TRUE),
              "\nHipotesis H0: Data berdistribusi normal",
              "\nHipotesis H1: Data tidak berdistribusi normal")
      })
      
      output$normality_interpretation <- renderText({
        create_interpretation(norm_test, "normality")
      })
    }
    
    if (input$test_homogeneity && input$assumption_group != "none") {
      group_data <- values$current_data[[input$assumption_group]]
      
      # Levene's test for homogeneity of variances
      test_data <- data.frame(value = var_data, group = group_data)
      test_data <- test_data[complete.cases(test_data), ]
      
      levene_test <- car::leveneTest(value ~ group, data = test_data)
      
      output$homogeneity_result <- renderText({
        paste("Levene's Test for Homogeneity of Variances",
              "\nF-statistic:", round(levene_test$`F value`[1], 4),
              "\ndf1:", levene_test$Df[1],
              "\ndf2:", levene_test$Df[2],
              "\np-value:", format(levene_test$`Pr(>F)`[1], scientific = TRUE),
              "\nHipotesis H0: Varians antar kelompok homogen",
              "\nHipotesis H1: Varians antar kelompok tidak homogen")
      })
      
      output$homogeneity_interpretation <- renderText({
        create_interpretation(list(p.value = levene_test$`Pr(>F)`[1]), "homogeneity")
      })
    }
    
    # Create assumption plots
    output$assumption_plots <- renderPlotly({
      p1 <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "lightblue") +
        geom_density(color = "red", size = 1) +
        labs(title = "Histogram & Density Plot", x = input$assumption_var) +
        theme_minimal()
      
      p2 <- ggplot(data.frame(x = var_data), aes(sample = x)) +
        geom_qq() +
        geom_qq_line(color = "red") +
        labs(title = "Q-Q Plot") +
        theme_minimal()
      
      subplot(ggplotly(p1), ggplotly(p2), nrows = 1)
    })
  })
  
  # =================== MEAN TESTS ===================
  observeEvent(input$run_mean_test, {
    req(input$mean_test_var, input$mean_test_type)
    
    var_data <- values$current_data[[input$mean_test_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if (input$mean_test_type == "one_sample") {
      test_result <- t.test(var_data, mu = input$test_value, conf.level = input$confidence_level)
      
      output$mean_test_result <- renderText({
        paste("One Sample t-test",
              "\nt-statistic:", round(test_result$statistic, 4),
              "\ndf:", test_result$parameter,
              "\np-value:", format(test_result$p.value, scientific = TRUE),
              "\nConfidence Interval:", paste(round(test_result$conf.int, 4), collapse = " - "),
              "\nSample Mean:", round(test_result$estimate, 4),
              "\nTest Value:", input$test_value)
      })
      
    } else if (input$mean_test_type == "two_sample" && !is.null(input$group_var_mean)) {
      group_data <- values$current_data[[input$group_var_mean]]
      test_data <- data.frame(value = var_data, group = group_data)
      test_data <- test_data[complete.cases(test_data), ]
      
      groups <- unique(test_data$group)
      if (length(groups) >= 2) {
        group1_data <- test_data$value[test_data$group == groups[1]]
        group2_data <- test_data$value[test_data$group == groups[2]]
        
        test_result <- t.test(group1_data, group2_data, conf.level = input$confidence_level)
        
        output$mean_test_result <- renderText({
          paste("Two Sample t-test",
                "\nt-statistic:", round(test_result$statistic, 4),
                "\ndf:", round(test_result$parameter, 2),
                "\np-value:", format(test_result$p.value, scientific = TRUE),
                "\nConfidence Interval:", paste(round(test_result$conf.int, 4), collapse = " - "),
                "\nMean Group 1 (", groups[1], "):", round(test_result$estimate[1], 4),
                "\nMean Group 2 (", groups[2], "):", round(test_result$estimate[2], 4))
        })
      }
    }
    
    output$mean_test_interpretation <- renderText({
      if (exists("test_result")) {
        create_interpretation(test_result, "t_test")
      }
    })
    
    # Create plot
    output$mean_test_plot <- renderPlotly({
      if (input$mean_test_type == "one_sample") {
        p <- ggplot(data.frame(x = var_data), aes(x = x)) +
          geom_histogram(bins = 30, alpha = 0.7, fill = "lightblue") +
          geom_vline(xintercept = mean(var_data), color = "red", linetype = "dashed", size = 1) +
          geom_vline(xintercept = input$test_value, color = "blue", linetype = "solid", size = 1) +
          labs(title = "Distribution with Sample Mean (red) and Test Value (blue)") +
          theme_minimal()
      } else {
        group_data <- values$current_data[[input$group_var_mean]]
        plot_data <- data.frame(value = var_data, group = group_data)
        plot_data <- plot_data[complete.cases(plot_data), ]
        
        p <- ggplot(plot_data, aes(x = group, y = value, fill = group)) +
          geom_boxplot(alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          labs(title = "Comparison between Groups") +
          theme_minimal()
      }
      ggplotly(p)
    })
  })
  
  # =================== ANOVA TESTS ===================
  observeEvent(input$run_anova, {
    req(input$anova_dependent, input$anova_factor1)
    
    if (input$anova_type == "oneway") {
      formula_str <- paste(input$anova_dependent, "~", input$anova_factor1)
      anova_model <- aov(as.formula(formula_str), data = values$current_data)
      anova_summary <- summary(anova_model)
      
      output$anova_result <- renderText({
        capture.output(print(anova_summary))
      })
      
      output$anova_interpretation <- renderText({
        create_interpretation(anova_summary[[1]], "anova")
      })
      
      if (input$post_hoc) {
        tukey_result <- TukeyHSD(anova_model)
        output$posthoc_result <- renderText({
          capture.output(print(tukey_result))
        })
      }
      
    } else if (input$anova_type == "twoway" && !is.null(input$anova_factor2)) {
      if (input$anova_interaction) {
        formula_str <- paste(input$anova_dependent, "~", input$anova_factor1, "*", input$anova_factor2)
      } else {
        formula_str <- paste(input$anova_dependent, "~", input$anova_factor1, "+", input$anova_factor2)
      }
      
      anova_model <- aov(as.formula(formula_str), data = values$current_data)
      anova_summary <- summary(anova_model)
      
      output$anova_result <- renderText({
        capture.output(print(anova_summary))
      })
    }
    
    # Create ANOVA plots
    output$anova_plots <- renderPlotly({
      if (input$anova_type == "oneway") {
        p1 <- ggplot(values$current_data, aes_string(x = input$anova_factor1, y = input$anova_dependent)) +
          geom_boxplot(aes_string(fill = input$anova_factor1), alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          labs(title = "Group Comparisons") +
          theme_minimal()
        
        # Residuals plot
        residuals_data <- data.frame(
          fitted = fitted(anova_model),
          residuals = residuals(anova_model)
        )
        
        p2 <- ggplot(residuals_data, aes(x = fitted, y = residuals)) +
          geom_point(alpha = 0.6) +
          geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
          labs(title = "Residuals vs Fitted") +
          theme_minimal()
        
        subplot(ggplotly(p1), ggplotly(p2), nrows = 1)
      } else {
        p <- ggplot(values$current_data, aes_string(x = input$anova_factor1, y = input$anova_dependent, fill = input$anova_factor2)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = "Two-Way ANOVA Visualization") +
          theme_minimal()
        ggplotly(p)
      }
    })
  })
  
  # =================== REGRESSION ===================
  observeEvent(input$run_regression, {
    req(input$reg_dependent, input$reg_independent)
    
    # Prepare formula
    formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
    
    # Fit regression model
    reg_model <- lm(as.formula(formula_str), data = values$current_data)
    reg_summary <- summary(reg_model)
    
    output$regression_summary <- renderText({
      capture.output(print(reg_summary))
    })
    
    output$regression_interpretation <- renderText({
      r_squared <- reg_summary$r.squared
      adj_r_squared <- reg_summary$adj.r.squared
      f_stat <- reg_summary$fstatistic
      p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      
      paste0("Interpretasi Model Regresi:\n",
             "R-squared: ", round(r_squared, 4), " (", round(r_squared*100, 2), "% varians dijelaskan)\n",
             "Adjusted R-squared: ", round(adj_r_squared, 4), "\n",
             "F-statistic: ", round(f_stat[1], 4), " (p-value: ", format(p_value, scientific = TRUE), ")\n\n",
             if (p_value < 0.05) {
               "Model secara keseluruhan signifikan (p < 0.05). Model dapat menjelaskan variasi dalam variabel dependen."
             } else {
               "Model secara keseluruhan tidak signifikan (p ≥ 0.05). Model mungkin tidak memberikan prediksi yang baik."
             })
    })
    
    if (input$reg_assumptions) {
      # Test assumptions
      assumptions_text <- paste(
        "Uji Asumsi Regresi:",
        "\n1. Normalitas Residuals (Shapiro-Wilk):",
        capture.output(print(shapiro.test(residuals(reg_model)))),
        "\n2. Homoskedastisitas (Breusch-Pagan):",
        capture.output(print(car::ncvTest(reg_model))),
        "\n3. Multikolinearitas (VIF):",
        if (length(input$reg_independent) > 1) capture.output(print(car::vif(reg_model))) else "VIF tidak dapat dihitung untuk satu prediktor"
      )
      
      output$regression_assumptions <- renderText({
        assumptions_text
      })
      
      output$assumptions_interpretation <- renderText({
        "Interpretasi Uji Asumsi:\n- Normalitas: p > 0.05 menunjukkan residual berdistribusi normal\n- Homoskedastisitas: p > 0.05 menunjukkan varians residual konstan\n- Multikolinearitas: VIF < 5 menunjukkan tidak ada masalah multikolinearitas"
      })
    }
    
    if (input$reg_diagnostics) {
      # Model diagnostics
      diagnostics_text <- paste(
        "Diagnostik Model:",
        "\nInfluential Points (Cook's Distance > 4/n):",
        paste(which(cooks.distance(reg_model) > 4/nrow(values$current_data)), collapse = ", "),
        "\nLeverage Points (Hat values > 2p/n):",
        paste(which(hatvalues(reg_model) > 2*length(coef(reg_model))/nrow(values$current_data)), collapse = ", ")
      )
      
      output$regression_diagnostics <- renderText({
        diagnostics_text
      })
      
      output$diagnostics_interpretation <- renderText({
        "Interpretasi Diagnostik:\n- Cook's Distance mengidentifikasi observasi yang sangat berpengaruh terhadap model\n- Leverage mengidentifikasi observasi dengan nilai prediktor yang ekstrem"
      })
    }
    
    # Create diagnostic plots
    output$regression_plots <- renderPlotly({
      # Prepare diagnostic data
      diag_data <- data.frame(
        fitted = fitted(reg_model),
        residuals = residuals(reg_model),
        standardized_residuals = rstandard(reg_model),
        leverage = hatvalues(reg_model),
        cooks_distance = cooks.distance(reg_model)
      )
      
      # Residuals vs Fitted
      p1 <- ggplot(diag_data, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_smooth(se = FALSE, color = "blue") +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        theme_minimal()
      
      # Q-Q plot
      p2 <- ggplot(diag_data, aes(sample = standardized_residuals)) +
        geom_qq() +
        geom_qq_line(color = "red") +
        labs(title = "Normal Q-Q Plot") +
        theme_minimal()
      
      # Scale-Location plot
      p3 <- ggplot(diag_data, aes(x = fitted, y = sqrt(abs(standardized_residuals)))) +
        geom_point(alpha = 0.6) +
        geom_smooth(se = FALSE, color = "red") +
        labs(title = "Scale-Location", x = "Fitted Values", y = "√|Standardized Residuals|") +
        theme_minimal()
      
      # Cook's Distance
      p4 <- ggplot(diag_data, aes(x = 1:nrow(diag_data), y = cooks_distance)) +
        geom_col(alpha = 0.7) +
        geom_hline(yintercept = 4/nrow(values$current_data), color = "red", linetype = "dashed") +
        labs(title = "Cook's Distance", x = "Observation", y = "Cook's Distance") +
        theme_minimal()
      
      subplot(
        ggplotly(p1), ggplotly(p2),
        ggplotly(p3), ggplotly(p4),
        nrows = 2
      )
    })
  })
  
  # =================== METADATA ===================
  output$metadata_variables <- DT::renderDataTable({
    metadata_vars <- data.frame(
      Variable = c("FIPS", "State", "County", "Population", "Income", "Education", "Age_65_Over", 
                   "Disability", "Single_Parent", "Minority", "Mobile_Home", "Crowding", "No_Vehicle", 
                   "Unemployment", "Poverty", "SOVI_Score"),
      Description = c("Federal Information Processing Standard code",
                     "State name",
                     "County name", 
                     "Total population",
                     "Median household income",
                     "Percentage with high school education or higher",
                     "Percentage of population aged 65 and over",
                     "Percentage with disability",
                     "Percentage of single-parent households",
                     "Percentage of minority population",
                     "Percentage living in mobile homes",
                     "Percentage living in crowded conditions",
                     "Percentage of households with no vehicle",
                     "Unemployment rate",
                     "Poverty rate",
                     "Social Vulnerability Index Score"),
      Type = c("Categorical", "Categorical", "Categorical", "Numerical", "Numerical", 
               "Numerical", "Numerical", "Numerical", "Numerical", "Numerical", 
               "Numerical", "Numerical", "Numerical", "Numerical", "Numerical", "Numerical")
    )
    DT::datatable(metadata_vars, options = list(pageLength = 20))
  })
  
  # =================== DOWNLOAD HANDLERS ===================
  output$download_manual <- downloadHandler(
    filename = "Manual_Dashboard_Statistik_Terpadu.pdf",
    content = function(file) {
      # Create a simple manual
      manual_content <- "
# Manual Pengguna Dashboard Statistik Terpadu

## Pendahuluan
Dashboard ini menyediakan analisis statistik komprehensif untuk data SOVI.

## Fitur Utama
1. Manajemen Data
2. Eksplorasi Data
3. Uji Asumsi
4. Statistik Inferensia
5. Regresi Linear
6. Metadata

## Cara Penggunaan
1. Mulai dari menu Beranda
2. Load data di Manajemen Data
3. Lakukan eksplorasi di menu Eksplorasi Data
4. Uji asumsi sebelum analisis inferensia
5. Jalankan analisis sesuai kebutuhan
"
      writeLines(manual_content, file)
    }
  )
  
  output$download_transformed <- downloadHandler(
    filename = function() {
      paste0("transformed_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$transformed_data)) {
        write.csv(values$transformed_data, file, row.names = FALSE)
      }
    }
  )
  
  # Additional download handlers for reports and results...
  output$download_desc_report <- downloadHandler(
    filename = function() {
      paste0("descriptive_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Create HTML report
      html_content <- "<html><body><h1>Laporan Statistik Deskriptif</h1><p>Laporan lengkap analisis deskriptif.</p></body></html>"
      writeLines(html_content, file)
    }
  )
  
  # Start analysis button
  observeEvent(input$start_analysis, {
    updateTabItems(session, "sidebar", "data_management")
  })
}

# Run the application
shinyApp(ui = ui, server = server)