library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyWidgets)
library(car)
library(officer)
library(flextable)
library(leaflet)
library(nortest)
library(lmtest)
library(plotly)
library(readr)
library(webshot) # Diperlukan untuk mengunduh plot/peta
library(htmlwidgets)

# Instalasi phantomjs jika belum ada untuk webshot
if (is.null(webshot:::find_phantom())) {
  webshot::install_phantomjs()
}


# URL untuk data
sovi_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
distance_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
metadata_url <- "https://www.sciencedirect.com/science/article/pii/S2352340921010180"

# UI
ui <- dashboardPage(
  dashboardHeader(title = "InsightStat Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "data_mgmt", icon = icon("table")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", tabName = "uji_asumsi", icon = icon("check")),
      menuItem("Statistik Inferensia", icon = icon("calculator"),
               menuSubItem("Uji Beda Rata-rata", tabName = "uji_beda"),
               menuSubItem("Uji Proporsi & Variansi", tabName = "uji_prop_var"),
               menuSubItem("ANOVA", tabName = "anova")),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Beranda
      tabItem(tabName = "beranda",
              fluidRow(
                box(title = "Selamat Datang di InsightStat Dashboard", width = 12, status = "primary", solidHeader = TRUE,
                    h3("Metadata dan Informasi Dashboard"),
                    p("InsightStat Dashboard adalah alat analisis statistik interaktif untuk mengelola, mengeksplorasi, dan menganalisis data kerentanan sosial berdasarkan Social Vulnerability Index (SoVI)."),
                    p("Versi: 1.0.1 | Dibuat oleh: Tim Analisis Data | Tanggal: 17 Juli 2025"),
                    p("Sumber Data: ", a("sovi_data.csv", href = sovi_url), " dan ", a("distance.csv", href = distance_url)),
                    p("Metadata: Artikel tentang Social Vulnerability Index (SoVI) oleh Nasution et al. (2022): ",
                      a("The origin and diffusion of the Social Vulnerability Index (SoVI)", href = metadata_url),
                      " (ScienceDirect). Ringkasan: Artikel ini menjelaskan perkembangan SoVI, indeks untuk mengukur kerentanan sosial terhadap bencana, dengan fokus pada ketimpangan sosial dan spasial. SoVI digunakan untuk menargetkan area yang rentan secara sosial untuk perencanaan pemulihan bencana."),
                    p("Fitur: Manajemen data, eksplorasi data, uji asumsi, statistik inferensia, dan regresi linear berganda dengan output yang dapat diunduh dalam format JPG dan Word."),
                    downloadButton("download_beranda", "Unduh Informasi Beranda (Word)")
                )
              )
      ),
      # Manajemen Data
      tabItem(tabName = "data_mgmt",
              fluidRow(
                box(title = "Manajemen Data", width = 12, status = "primary", solidHeader = TRUE,
                    fileInput("file_upload", "Unggah File CSV (Opsional, default: sovi_data.csv)", accept = ".csv"),
                    selectInput("var_cont", "Pilih Variabel Kontinu", choices = NULL),
                    numericInput("num_bins", "Jumlah Kategori", value = 3, min = 2, max = 10),
                    actionButton("categorize", "Kategorikan Data"),
                    DTOutput("data_table"),
                    verbatimTextOutput("data_mgmt_summary"),
                    downloadButton("download_data_mgmt", "Unduh Hasil Manajemen Data (Word)")
                )
              )
      ),
      # Eksplorasi Data
      tabItem(tabName = "eksplorasi",
              fluidRow(
                box(title = "Eksplorasi Data", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("var_explore", "Pilih Variabel untuk Eksplorasi", choices = NULL),
                    radioButtons("plot_type", "Tipe Visualisasi", choices = c("Histogram", "Boxplot", "Peta", "Matriks Jarak")),
                    uiOutput("explore_output_ui"), # UI dinamis untuk plot atau tabel
                    verbatimTextOutput("explore_summary"),
                    downloadButton("download_explore_plot", "Unduh Plot (JPG)"),
                    downloadButton("download_explore_summary", "Unduh Ringkasan (Word)")
                )
              )
      ),
      # Uji Asumsi
      tabItem(tabName = "uji_asumsi",
              fluidRow(
                box(title = "Uji Asumsi", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("var_norm", "Pilih Variabel untuk Uji Normalitas", choices = NULL),
                    actionButton("run_normality", "Jalankan Uji Normalitas"),
                    verbatimTextOutput("normality_result"),
                    hr(),
                    selectInput("var_homo", "Pilih Variabel untuk Uji Homogenitas", choices = NULL),
                    selectInput("group_homo", "Pilih Variabel Grouping", choices = NULL),
                    actionButton("run_homogeneity", "Jalankan Uji Homogenitas"),
                    verbatimTextOutput("homogeneity_result"),
                    downloadButton("download_assumption", "Unduh Hasil Uji Asumsi (Word)")
                )
              )
      ),
      # Uji Beda Rata-rata
      tabItem(tabName = "uji_beda",
              fluidRow(
                box(title = "Uji Beda Rata-rata", width = 12, status = "primary", solidHeader = TRUE,
                    radioButtons("mean_test_type", "Tipe Uji", choices = c("Uji-t Satu Sampel" = "one_sample", "Uji-t Dua Sampel" = "two_sample"), inline = TRUE),
                    selectInput("var_mean", "Pilih Variabel", choices = NULL),
                    # UI kondisional untuk grup
                    conditionalPanel(
                      condition = "input.mean_test_type == 'two_sample'",
                      selectInput("group_mean", "Pilih Variabel Grouping", choices = NULL)
                    ),
                    actionButton("run_mean_test", "Jalankan Uji"),
                    verbatimTextOutput("mean_test_result"),
                    downloadButton("download_mean_test", "Unduh Hasil Uji Beda (Word)")
                )
              )
      ),
      # Uji Proporsi & Variansi
      tabItem(tabName = "uji_prop_var",
              fluidRow(
                box(title = "Uji Proporsi & Variansi", width = 12, status = "primary", solidHeader = TRUE,
                    radioButtons("prop_var_test", "Tipe Uji", choices = c("Uji Proporsi", "Uji Variansi (F-test dua sampel)"), inline = TRUE),
                    selectInput("var_prop_var", "Pilih Variabel", choices = NULL),
                    selectInput("group_prop_var", "Pilih Variabel Grouping", choices = NULL),
                    actionButton("run_prop_var_test", "Jalankan Uji"),
                    verbatimTextOutput("prop_var_result"),
                    downloadButton("download_prop_var", "Unduh Hasil Uji Proporsi/Variansi (Word)")
                )
              )
      ),
      # ANOVA
      tabItem(tabName = "anova",
              fluidRow(
                box(title = "ANOVA", width = 12, status = "primary", solidHeader = TRUE,
                    radioButtons("anova_type", "Tipe ANOVA", choices = c("Satu Arah (One-way)" = "one_way", "Dua Arah (Two-way)" = "two_way"), inline = TRUE),
                    selectInput("var_anova", "Pilih Variabel Respon", choices = NULL),
                    selectInput("group_anova1", "Pilih Faktor 1", choices = NULL),
                    # UI kondisional untuk faktor kedua
                    conditionalPanel(
                      condition = "input.anova_type == 'two_way'",
                      selectInput("group_anova2", "Pilih Faktor 2", choices = NULL)
                    ),
                    actionButton("run_anova", "Jalankan Uji"),
                    verbatimTextOutput("anova_result"),
                    downloadButton("download_anova", "Unduh Hasil ANOVA (Word)")
                )
              )
      ),
      # Regresi Linear Berganda
      tabItem(tabName = "regresi",
              fluidRow(
                box(title = "Regresi Linear Berganda", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("dep_var", "Pilih Variabel Dependen", choices = NULL),
                    selectInput("indep_vars", "Pilih Variabel Independen", choices = NULL, multiple = TRUE),
                    actionButton("run_regression", "Jalankan Regresi"),
                    verbatimTextOutput("regression_result"),
                    downloadButton("download_regression", "Unduh Hasil Regresi (Word)")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # === DATA REACTIVES ===
  
  data <- reactive({
    if (!is.null(input$file_upload)) {
      read_csv(input$file_upload$datapath, show_col_types = FALSE)
    } else {
      req(sovi_url)
      read_csv(sovi_url, show_col_types = FALSE)
    }
  })
  
  distance_data <- reactive({
    req(distance_url)
    read_csv(distance_url, show_col_types = FALSE)
  })
  
  # === UPDATE SELECT INPUTS OBSERVER ===
  
  observe({
    df <- data()
    req(df)
    var_names <- names(df)
    
    # Pisahkan variabel numerik dan kategorikal/faktor
    var_numeric <- var_names[sapply(df, is.numeric)]
    var_factor <- var_names[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    # Update inputs based on variable type
    updateSelectInput(session, "var_cont", choices = var_numeric)
    updateSelectInput(session, "var_explore", choices = var_names)
    updateSelectInput(session, "var_norm", choices = var_numeric)
    updateSelectInput(session, "var_homo", choices = var_numeric)
    updateSelectInput(session, "group_homo", choices = var_factor)
    updateSelectInput(session, "var_mean", choices = var_numeric)
    updateSelectInput(session, "group_mean", choices = var_factor)
    updateSelectInput(session, "var_prop_var", choices = var_numeric)
    updateSelectInput(session, "group_prop_var", choices = var_factor)
    updateSelectInput(session, "var_anova", choices = var_numeric)
    updateSelectInput(session, "group_anova1", choices = var_factor)
    updateSelectInput(session, "group_anova2", choices = var_factor)
    updateSelectInput(session, "dep_var", choices = var_numeric)
    # PERBAIKAN: Hapus argumen `multiple = TRUE` dari `updateSelectInput`
    updateSelectInput(session, "indep_vars", choices = var_numeric)
  })
  
  # === HELPER FUNCTIONS ===
  
  create_word_doc <- function(content, title) {
    doc <- read_docx() %>%
      body_add_par(title, style = "heading 1") %>%
      body_add_par(content, style = "Normal")
    return(doc)
  }
  
  # === BERANDA (HOME) ===
  
  output$download_beranda <- downloadHandler(
    filename = "informasi_beranda.docx",
    content = function(file) {
      content <- paste(
        "InsightStat Dashboard\n",
        "Versi: 1.0.1\n",
        "Dibuat oleh: Tim Analisis Data\n",
        "Tanggal: 17 Juli 2025\n",
        "Sumber Data: ", sovi_url, " dan ", distance_url, "\n",
        "Metadata: Artikel tentang Social Vulnerability Index (SoVI) oleh Nasution et al. (2022): ", metadata_url,
        "\nRingkasan: Artikel ini menjelaskan perkembangan SoVI, indeks untuk mengukur kerentanan sosial terhadap bencana, dengan fokus pada ketimpangan sosial dan spasial. SoVI digunakan untuk menargetkan area yang rentan secara sosial untuk perencanaan pemulihan bencana.\n",
        "Fitur: Manajemen data, eksplorasi data, uji asumsi, statistik inferensia, dan regresi linear berganda dengan output yang dapat diunduh dalam format JPG dan Word."
      )
      doc <- create_word_doc(content, "Informasi Beranda")
      print(doc, target = file)
    }
  )
  
  # === MANAJEMEN DATA ===
  
  data_categorized <- eventReactive(input$categorize, {
    req(data(), input$var_cont, input$num_bins)
    df <- data()
    var <- input$var_cont
    bins <- input$num_bins
    
    new_col_name <- paste0(var, "_cat")
    breaks <- seq(min(df[[var]], na.rm = TRUE), max(df[[var]], na.rm = TRUE), length.out = bins + 1)
    labels <- paste("Kategori", 1:bins)
    df[[new_col_name]] <- cut(df[[var]], breaks = breaks, labels = labels, include.lowest = TRUE)
    df
  })
  
  output$data_table <- renderDT({
    datatable(data_categorized(), options = list(scrollX = TRUE))
  })
  
  data_mgmt_summary_text <- eventReactive(input$categorize, {
    req(data_categorized(), input$var_cont, input$num_bins)
    df <- data_categorized()
    var <- input$var_cont
    new_var <- paste0(var, "_cat")
    
    summary_table <- table(df[[new_var]])
    paste("Variabel", var, "telah dikategorikan menjadi", input$num_bins, "kategori dalam kolom baru '", new_var, "'.\n",
          "Distribusi frekuensi kategori baru adalah:\n",
          paste(capture.output(print(summary_table)), collapse = "\n"))
  })
  
  output$data_mgmt_summary <- renderText({
    data_mgmt_summary_text()
  })
  
  output$download_data_mgmt <- downloadHandler(
    filename = "manajemen_data.docx",
    content = function(file) {
      req(data_mgmt_summary_text())
      doc <- create_word_doc(data_mgmt_summary_text(), "Hasil Manajemen Data")
      print(doc, target = file)
    }
  )
  
  # === EKSPLORASI DATA ===
  
  explore_plot_object <- reactive({
    req(data(), input$var_explore, input$plot_type)
    df <- data()
    var <- input$var_explore
    plot_type <- input$plot_type
    
    if (plot_type == "Histogram" && is.numeric(df[[var]])) {
      p <- ggplot(df, aes_string(x = var)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.8) +
        theme_minimal() + labs(title = paste("Histogram dari", var))
      ggplotly(p)
    } else if (plot_type == "Boxplot" && is.numeric(df[[var]])) {
      p <- ggplot(df, aes_string(y = var)) +
        geom_boxplot(fill = "skyblue", alpha = 0.8) +
        theme_minimal() + labs(title = paste("Boxplot dari", var))
      ggplotly(p)
    } else if (plot_type == "Peta") {
      if (all(c("latitude", "longitude") %in% names(df))) {
        leaflet(df) %>%
          addTiles() %>%
          addCircles(lng = ~longitude, lat = ~latitude, radius = 40000, 
                     popup = ~paste(DISTRICTCODE, "<br>", var, ":", df[[var]]),
                     color = "navy")
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  
  output$explore_output_ui <- renderUI({
    if (input$plot_type %in% c("Histogram", "Boxplot")) {
      plotlyOutput("explore_plotly")
    } else if (input$plot_type == "Peta") {
      leafletOutput("explore_leaflet")
    } else if (input$plot_type == "Matriks Jarak") {
      DTOutput("distance_table")
    }
  })
  
  output$explore_plotly <- renderPlotly({ explore_plot_object() })
  output$explore_leaflet <- renderLeaflet({ explore_plot_object() })
  output$distance_table <- renderDT({ datatable(distance_data(), caption = "Matriks Jarak Antar Wilayah", options = list(scrollX = TRUE)) })
  
  explore_summary_text <- reactive({
    req(data(), input$var_explore)
    df <- data()
    var <- input$var_explore
    if (is.numeric(df[[var]])) {
      stats <- summary(df[[var]])
      paste("Statistik deskriptif untuk", var, ":\n",
            paste(capture.output(print(stats)), collapse = "\n"),
            "\nInterpretasi: Variabel ini memiliki rentang dari", min(df[[var]], na.rm = TRUE),
            "hingga", max(df[[var]], na.rm = TRUE), ". Nilai rata-rata adalah",
            round(mean(df[[var]], na.rm = TRUE), 2), "dan median adalah", median(df[[var]], na.rm = TRUE), ".")
    } else {
      stats <- table(df[[var]])
      paste("Frekuensi kategori untuk", var, ":\n",
            paste(capture.output(print(stats)), collapse = "\n"))
    }
  })
  
  output$explore_summary <- renderText({
    explore_summary_text()
  })
  
  output$download_explore_plot <- downloadHandler(
    filename = function() { paste0("plot_", input$plot_type, "_", input$var_explore, ".jpeg") },
    content = function(file) {
      p <- explore_plot_object()
      if (!is.null(p)) {
        # PENYEMPURNAAN: Gunakan webshot untuk semua plot interaktif
        temp_file <- "temp.html"
        saveWidget(p, temp_file, selfcontained = TRUE)
        webshot(temp_file, file = file, delay = 2, vwidth = 900, vheight = 700)
      }
    }
  )
  
  output$download_explore_summary <- downloadHandler(
    filename = "ringkasan_eksplorasi.docx",
    content = function(file) {
      content <- explore_summary_text()
      if (input$plot_type == "Matriks Jarak") {
        content <- paste(content, "\n\nMatriks Jarak:\nMatriks jarak dari distance.csv menunjukkan jarak spasial antar lokasi, yang dapat digunakan untuk analisis spasial seperti clustering atau pemetaan kerentanan.", collapse = "\n")
      }
      doc <- create_word_doc(content, "Ringkasan Eksplorasi Data")
      print(doc, target = file)
    }
  )
  
  # === UJI ASUMSI ===
  
  normality_result_text <- eventReactive(input$run_normality, {
    req(data(), input$var_norm)
    shapiro <- shapiro.test(data()[[input$var_norm]])
    paste("Hasil Uji Normalitas (Shapiro-Wilk) untuk variabel '", input$var_norm, "':\n",
          paste(capture.output(print(shapiro)), collapse = "\n"),
          "\nInterpretasi: Jika p-value < 0.05, data tidak terdistribusi normal. Jika p-value >= 0.05, tidak ada bukti untuk menolak asumsi normalitas.")
  })
  
  output$normality_result <- renderText({ normality_result_text() })
  
  homogeneity_result_text <- eventReactive(input$run_homogeneity, {
    req(data(), input$var_homo, input$group_homo)
    if (input$group_homo == "") return("Pilih variabel grouping untuk uji homogenitas.")
    levene <- leveneTest(as.formula(paste(input$var_homo, "~", input$group_homo)), data = data())
    paste("Hasil Uji Homogenitas (Levene's Test):\n",
          "Variabel dependen:", input$var_homo, "| Faktor:", input$group_homo, "\n",
          paste(capture.output(print(levene)), collapse = "\n"),
          "\nInterpretasi: Jika p-value < 0.05, variansi antar kelompok tidak homogen. Jika p-value >= 0.05, asumsi homogenitas variansi terpenuhi.")
  })
  
  output$homogeneity_result <- renderText({ homogeneity_result_text() })
  
  output$download_assumption <- downloadHandler(
    filename = "hasil_uji_asumsi.docx",
    content = function(file) {
      content <- paste(normality_result_text(), "\n\n", homogeneity_result_text(), collapse = "\n")
      doc <- create_word_doc(content, "Hasil Uji Asumsi")
      print(doc, target = file)
    }
  )
  
  # === UJI BEDA RATA-RATA ===
  
  mean_test_result_text <- eventReactive(input$run_mean_test, {
    req(data(), input$var_mean)
    var <- input$var_mean
    
    if (input$mean_test_type == "one_sample") {
      t_test <- t.test(data()[[var]], mu = 0) # Uji terhadap mu=0 sebagai default
      paste("Hasil Uji-t Satu Sampel untuk '", var, "':\n",
            paste(capture.output(print(t_test)), collapse = "\n"),
            "\nInterpretasi: Jika p-value < 0.05, rata-rata populasi secara signifikan berbeda dari 0.")
    } else {
      req(input$group_mean)
      group <- input$group_mean
      t_test <- t.test(as.formula(paste(var, "~", group)), data = data())
      paste("Hasil Uji-t Dua Sampel:\n",
            "Variabel:", var, "| Grup:", group, "\n",
            paste(capture.output(print(t_test)), collapse = "\n"),
            "\nInterpretasi: Jika p-value < 0.05, terdapat perbedaan rata-rata yang signifikan antara dua kelompok.")
    }
  })
  
  output$mean_test_result <- renderText({ mean_test_result_text() })
  
  output$download_mean_test <- downloadHandler(
    filename = "hasil_uji_beda_rata_rata.docx",
    content = function(file) {
      doc <- create_word_doc(mean_test_result_text(), "Hasil Uji Beda Rata-rata")
      print(doc, target = file)
    }
  )
  
  # === UJI PROPORSI & VARIANSI ===
  
  prop_var_result_text <- eventReactive(input$run_prop_var_test, {
    req(data(), input$var_prop_var, input$group_prop_var)
    var <- input$var_prop_var
    group <- input$group_prop_var
    
    if (input$prop_var_test == "Uji Proporsi") {
      # Asumsi uji proporsi chi-square pada tabel kontingensi
      tab <- table(data()[[group]], data()[[var]])
      prop_test <- prop.test(tab)
      paste("Hasil Uji Proporsi (Chi-square) untuk '", var, "' berdasarkan '", group, "':\n",
            paste(capture.output(print(prop_test)), collapse = "\n"),
            "\nInterpretasi: Jika p-value < 0.05, terdapat perbedaan proporsi yang signifikan antar kelompok.")
    } else { # Uji Variansi F-test
      var_test <- var.test(as.formula(paste(var, "~", group)), data = data())
      paste("Hasil Uji Variansi Dua Sampel (F-test):\n",
            "Variabel:", var, "| Grup:", group, "\n",
            paste(capture.output(print(var_test)), collapse = "\n"),
            "\nInterpretasi: Jika p-value < 0.05, variansi antar kelompok berbeda secara signifikan.")
    }
  })
  
  output$prop_var_result <- renderText({ prop_var_result_text() })
  
  output$download_prop_var <- downloadHandler(
    filename = "hasil_uji_prop_var.docx",
    content = function(file) {
      doc <- create_word_doc(prop_var_result_text(), "Hasil Uji Proporsi & Variansi")
      print(doc, target = file)
    }
  )
  
  # === ANOVA ===
  
  anova_result_text <- eventReactive(input$run_anova, {
    req(data(), input$var_anova, input$group_anova1)
    
    if (input$anova_type == "one_way") {
      formula <- as.formula(paste(input$var_anova, "~", input$group_anova1))
      anova_model <- aov(formula, data = data())
      paste("Hasil ANOVA Satu Arah (One-Way):\n",
            paste(capture.output(summary(anova_model)), collapse = "\n"),
            "\nInterpretasi: Jika p-value (Pr(>F)) < 0.05, terdapat perbedaan rata-rata yang signifikan di antara kelompok.")
    } else {
      req(input$group_anova2)
      formula <- as.formula(paste(input$var_anova, "~", input$group_anova1, "*", input$group_anova2))
      anova_model <- aov(formula, data = data())
      paste("Hasil ANOVA Dua Arah (Two-Way):\n",
            paste(capture.output(summary(anova_model)), collapse = "\n"),
            "\nInterpretasi: Jika p-value suatu faktor < 0.05, faktor tersebut berpengaruh signifikan. Jika p-value interaksi < 0.05, efek satu faktor bergantung pada level faktor lainnya.")
    }
  })
  
  output$anova_result <- renderText({ anova_result_text() })
  
  output$download_anova <- downloadHandler(
    filename = "hasil_anova.docx",
    content = function(file) {
      doc <- create_word_doc(anova_result_text(), "Hasil ANOVA")
      print(doc, target = file)
    }
  )
  
  # === REGRESI LINEAR BERGANDA ===
  
  regression_result_text <- eventReactive(input$run_regression, {
    req(data(), input$dep_var, input$indep_vars)
    
    formula <- as.formula(paste(input$dep_var, "~", paste(input$indep_vars, collapse = "+")))
    model <- lm(formula, data = data())
    
    # Uji asumsi regresi
    shapiro <- shapiro.test(residuals(model))
    dw_test <- dwtest(model)
    bp_test <- bptest(model)
    
    paste(
      "Hasil Regresi Linear Berganda:\n",
      paste(capture.output(summary(model)), collapse = "\n"),
      "\n\n--- UJI ASUMSI MODEL REGRESI ---\n",
      "1. Normalitas Residual (Shapiro-Wilk):\n",
      paste(capture.output(print(shapiro)), collapse = "\n"),
      "   Interpretasi: Jika p-value < 0.05, residual tidak terdistribusi normal.\n",
      "2. Autokorelasi (Durbin-Watson):\n",
      paste(capture.output(print(dw_test)), collapse = "\n"),
      "   Interpretasi: Nilai DW mendekati 2 menunjukkan tidak ada autokorelasi. Nilai mendekati 0 atau 4 menunjukkan adanya autokorelasi.\n",
      "3. Homoskedastisitas (Breusch-Pagan):\n",
      paste(capture.output(print(bp_test)), collapse = "\n"),
      "   Interpretasi: Jika p-value < 0.05, terjadi heteroskedastisitas (variansi residual tidak konstan).\n",
      "\n--- INTERPRETASI MODEL ---\n",
      "Koefisien yang signifikan (Pr(>|t|) < 0.05) menunjukkan variabel independen berpengaruh signifikan terhadap variabel dependen. Adjusted R-squared menunjukkan proporsi variansi variabel dependen yang dapat dijelaskan oleh model."
    )
  })
  
  output$regression_result <- renderText({ regression_result_text() })
  
  output$download_regression <- downloadHandler(
    filename = "hasil_regresi.docx",
    content = function(file) {
      doc <- create_word_doc(regression_result_text(), "Hasil Regresi Linear Berganda")
      print(doc, target = file)
    }
  )
}

# Run app
shinyApp(ui, server)