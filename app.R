# app.R
# =====================================================================
# LIBRARIES
# =====================================================================
library(shiny)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(DT)
library(scales)
library(shinycssloaders)
library(RColorBrewer)
library(openxlsx)
library(rmarkdown)
library(colourpicker)
library(shinyAce)
library(svglite)

# Use public access (no login) for public sheets
gs4_deauth()

# =====================================================================
# Custom Theme Function
# =====================================================================
custom_theme <- function(font_family, font_size, font_color) {
  theme_minimal(base_family = font_family, base_size = font_size) +
    theme(
      text = element_text(color = font_color),
      axis.text = element_text(color = font_color),
      axis.title = element_text(color = font_color),
      plot.title = element_text(color = font_color, size = font_size + 2, face = "bold")
    )
}

# =====================================================================
# UI
# =====================================================================
ui <- fluidPage(
  titlePanel("{ Dynamic Google Sheet Dashboard }"),
  sidebarLayout(
    sidebarPanel(
      helpText("Paste a public Google Sheet link (Share ➜ Anyone with the link = Viewer)."),
      textInput("sheet_url", "🔗 Google Sheet URL:",
                value = "https://docs.google.com/spreadsheets/d/1Hzcm-CxYlh6BueA4bbARkklxDycIHnxYDIdzuFl3ODQ/edit?gid=0#gid=0"),
      actionButton("load_data", "📥 Load Sheet"),
      tags$hr(),
      # Visualization chooser (always visible)
      selectInput(
        "viz_choice", "📊 Choose Visualization:",
        choices = c(
          # Tables
          "📄 Tables: Raw Data"                = "tbl_raw",
          "🧮 Tables: Summary by Category"     = "tbl_summary",
          "🔢 Tables: Frequency of Category"   = "tbl_freq",
          "🔗 Tables: Correlation Matrix"      = "tbl_corr",
          # One-category charts
          "🥧 Pie (count by Category)"         = "pie",
          "📊 Bar (sum Value by Category)"     = "bar",
          "⚪ Dot (sum Value by Category)"     = "dot",
          "🍭 Lollipop (sum Value by Category)"= "lollipop",
          "🔢 Histogram (Value)"               = "hist",
          "🌫️ Density (Value)"                = "density",
          "📦 Boxplot (Value ~ Category)"      = "box",
          "🎻 Violin (Value ~ Category)"       = "violin",
          "📈 Line (sum Value by Category)"    = "line",
          "🟢 Area (sum Value by Category)"    = "area",
          # Two-category / relationships
          "📚 Grouped Bar (Category × Subcat)" = "grouped",
          "🧱 Stacked Bar (Category × Subcat)" = "stacked",
          "🔥 Heatmap (Category × Subcat)"     = "heatmap",
          "🔴 Scatter (X vs Y)"                = "scatter",
          "🔵 Bubble (X vs Y, size = Value)"   = "bubble",
          "🌐 2D Density (X vs Y)"             = "density2d",
          "🏔️ Ridgeline (Value ~ Category)"   = "ridge"
        ), selected = "tbl_raw"
      ),
      tags$hr(),
      # Font customization
      selectInput("font_choice", "Choose Font:",
                  choices = c("Arial", "Times New Roman", "Courier New", "Georgia", "Verdana")),
      sliderInput("font_size", "Font Size:", min = 10, max = 40, value = 14),
      colourInput("font_color", "Font Color:", value = "black"),
      tags$hr(),
      # Dynamic selectors (populated after sheet load)
      uiOutput("selectors"),
      tags$hr(),
      # Downloads
      downloadButton("download_csv", "⬇️ Download CSV"),
      downloadButton("download_excel", "⬇️ Download Excel"),
      downloadButton("download_pdf", "⬇️ Download PDF"),
      downloadButton("download_svg", "⬇️ Download SVG"),
      tags$hr(),
      helpText("View source code in the 'View Code' tab (read-only).")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualizer", uiOutput("viz_panel") %>% withSpinner(color = "#4E79A7", type = 6)),
        tabPanel("Data Table", DTOutput("table")),      # simple full-table tab
        tabPanel("Summary", htmlOutput("summary")),     # styled summary
        tabPanel("View Code", aceEditor(
          outputId = "code_display",
          mode = "r",
          theme = "chrome",
          readOnly = TRUE,
          height = "700px"
        ))
      )
    )
  )
)

# =====================================================================
# SERVER
# =====================================================================
server <- function(input, output, session) {
  # reactive dataset
  data_rv <- reactiveVal(NULL)
  
  default_or_first <- function(choices, preferred) {
    if (length(choices) == 0) return(NULL)
    pref <- preferred[preferred %in% choices]
    if (length(pref)) pref[1] else choices[1]
  }
  
  # load Google Sheet (robust)
  observeEvent(input$load_data, {
    req(input$sheet_url)
    withProgress(message = "Loading Google Sheet...", value = 0.1, {
      df <- NULL
      try({ df <- suppressMessages(read_sheet(input$sheet_url)) }, silent = TRUE)
      if (is.null(df)) {
        sheet_id <- gsub(".*?/d/([^/]+)(/.*|$)", "\\1", input$sheet_url)
        try({ df <- suppressMessages(read_sheet(sheet_id)) }, silent = TRUE)
      }
      if (is.null(df)) {
        showNotification("⚠️ Could not load the sheet. Check URL & sharing (Anyone with link = Viewer).", type = "error", duration = 7)
        return(invisible(NULL))
      }
      data_rv(df)
      incProgress(0.9, detail = "Parsing data...")
      Sys.sleep(0.2)
      showNotification("✅ Sheet loaded!", type = "message", duration = 3)
      
      # update selectors with sensible defaults
      cats <- names(df)
      # detect numeric-like columns (coerce safe)
      nums <- names(df)[sapply(df, function(x) suppressWarnings(any(!is.na(as.numeric(x)))))]
      
      updateSelectInput(session, "category_col", choices = cats, selected = default_or_first(cats, c("Group","group","Category","CATEGORY")))
      updateSelectInput(session, "value_col", choices = nums, selected = default_or_first(nums, c("Value","value","A","a")))
      updateSelectInput(session, "subcategory_col", choices = c("— none —", cats), selected = "— none —")
      x_default <- default_or_first(nums, c("X","x","A","a"))
      y_default <- default_or_first(nums[nums != x_default], c("Y","y","B","b"))
      updateSelectInput(session, "x_num", choices = nums, selected = x_default)
      updateSelectInput(session, "y_num", choices = nums, selected = y_default)
    })
  })
  
  # helpers: columns sets
  num_cols <- reactive({
    req(data_rv())
    names(data_rv())[sapply(data_rv(), function(x) suppressWarnings(any(!is.na(as.numeric(x)))))]
  })
  cat_cols <- reactive({ req(data_rv()); names(data_rv()) })
  
  # dynamic selectors UI (shown after sheet load)
  output$selectors <- renderUI({
    req(data_rv())
    tagList(
      selectInput("category_col", "Category column:", choices = cat_cols()),
      selectInput("value_col", "Numeric value column:", choices = num_cols()),
      selectInput("subcategory_col", "Subcategory (optional):", choices = c("— none —", cat_cols()), selected = "— none —"),
      selectInput("x_num", "X (numeric):", choices = num_cols()),
      selectInput("y_num", "Y (numeric):", choices = num_cols())
    )
  })
  
  # cleaned data
  cleaned_data <- reactive({ req(data_rv()); data_rv() })
  
  # aggregations
  agg_one_cat <- reactive({
    req(cleaned_data(), input$category_col)
    df <- cleaned_data()
    # try numeric value, otherwise count
    if (!is.null(input$value_col) && input$value_col %in% names(df)) {
      v <- suppressWarnings(as.numeric(df[[input$value_col]]))
      if (any(!is.na(v))) {
        return(df %>% mutate(.val = v) %>% group_by(.data[[input$category_col]]) %>%
                 summarise(value = sum(.val, na.rm = TRUE), .groups = "drop") %>% arrange(desc(value)))
      }
    }
    df %>% filter(!is.na(.data[[input$category_col]])) %>% count(.data[[input$category_col]], name = "value") %>% arrange(desc(value))
  })
  
  agg_two_cat <- reactive({
    req(cleaned_data(), input$category_col)
    validate(need(input$subcategory_col != "— none —", "Please pick a Subcategory column for this visualization."))
    df <- cleaned_data()
    v <- if (!is.null(input$value_col) && input$value_col %in% names(df)) suppressWarnings(as.numeric(df[[input$value_col]])) else NA_real_
    df %>% mutate(.val = v) %>%
      group_by(.data[[input$category_col]], .data[[input$subcategory_col]]) %>%
      summarise(value = if (any(!is.na(.val))) sum(.val, na.rm = TRUE) else dplyr::n(), .groups = "drop")
  })
  
  corr_mat <- reactive({
    req(cleaned_data())
    nums <- cleaned_data() %>% mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>% select(where(~ any(!is.na(.))))
    validate(need(ncol(nums) >= 2, "Need at least two numeric columns to compute a correlation matrix."))
    as.data.frame(round(cor(nums, use = "pairwise.complete.obs"), 3))
  })
  
  # decide whether to show table or plot container
  output$viz_panel <- renderUI({
    req(input$viz_choice)
    if (grepl("^tbl_", input$viz_choice)) {
      DTOutput("table_out")
    } else {
      plotOutput("plot_out", height = "560px")
    }
  })
  
  # Tables
  output$table_out <- renderDT({
    req(cleaned_data(), input$viz_choice)
    if (input$viz_choice == "tbl_raw") {
      datatable(cleaned_data(), options = list(pageLength = 12, scrollX = TRUE))
    } else if (input$viz_choice == "tbl_summary") {
      req(input$category_col)
      datatable(
        cleaned_data() %>%
          { df <- .;
          has_val <- !is.null(input$value_col) && input$value_col %in% names(df);
          if (has_val) df <- df %>% mutate(.val = suppressWarnings(as.numeric(df[[input$value_col]])));
          df %>% group_by(.data[[input$category_col]]) %>%
              summarise(n = n(),
                        sum = if (has_val && any(!is.na(.val))) sum(.val, na.rm = TRUE) else NA_real_,
                        mean = if (has_val && any(!is.na(.val))) mean(.val, na.rm = TRUE) else NA_real_,
                        median = if (has_val && any(!is.na(.val))) median(.val, na.rm = TRUE) else NA_real_,
                        sd = if (has_val && any(!is.na(.val))) sd(.val, na.rm = TRUE) else NA_real_,
                        .groups = "drop") %>% arrange(desc(n))
          },
        options = list(pageLength = 12, scrollX = TRUE)
      )
    } else if (input$viz_choice == "tbl_freq") {
      req(input$category_col)
      freq <- cleaned_data() %>% count(.data[[input$category_col]], name = "Frequency") %>% arrange(desc(Frequency))
      datatable(freq, options = list(pageLength = 12, scrollX = TRUE))
    } else if (input$viz_choice == "tbl_corr") {
      datatable(corr_mat(), options = list(pageLength = 12, scrollX = TRUE))
    }
  })
  
  # Summary (styled)
  output$summary <- renderUI({
    req(cleaned_data())
    summary_text <- capture.output(summary(cleaned_data()))
    htmltools::tags$pre(
      style = paste0("font-family:", input$font_choice, "; font-size:", input$font_size, "px; color:", input$font_color, ";"),
      paste(summary_text, collapse = "\n")
    )
  })
  
  # Plot builder (centralized)
  make_plot <- reactive({
    req(cleaned_data(), input$viz_choice)
    df <- cleaned_data()
    choice <- input$viz_choice
    pal_fill <- scale_fill_brewer(palette = "Set3")
    pal_col <- scale_color_brewer(palette = "Dark2")
    p <- NULL
    
    # One-category / aggregated:
    if (choice == "pie") {
      pd <- agg_one_cat()
      validate(need(nrow(pd) > 0, "No non-missing values in the selected Category."))
      p <- ggplot(pd, aes(x = "", y = value, fill = .data[[input$category_col]])) + geom_col(width = 1, color = "white") + coord_polar("y") + pal_fill + labs(title = paste("Distribution of", input$category_col), fill = input$category_col) + theme_void()
    } else if (choice == "bar") {
      dfb <- agg_one_cat()
      p <- ggplot(dfb, aes(x = reorder(.data[[input$category_col]], -value), y = value, fill = .data[[input$category_col]])) + geom_col(show.legend = FALSE) + pal_fill + labs(title = paste("Total", ifelse(!is.null(input$value_col), input$value_col, "Count"), "by", input$category_col), x = input$category_col, y = ifelse(!is.null(input$value_col), input$value_col, "Count")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (choice == "dot") {
      dfb <- agg_one_cat()
      p <- ggplot(dfb, aes(x = reorder(.data[[input$category_col]], -value), y = value, color = .data[[input$category_col]])) + geom_point(size = 5, alpha = 0.9, show.legend = FALSE) + pal_col + labs(title = paste("Dot Plot of", ifelse(!is.null(input$value_col), input$value_col, "Count"), "by", input$category_col), x = input$category_col, y = ifelse(!is.null(input$value_col), input$value_col, "Count")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (choice == "lollipop") {
      dfb <- agg_one_cat()
      p <- ggplot(dfb, aes(x = reorder(.data[[input$category_col]], value), y = value, color = .data[[input$category_col]])) + geom_segment(aes(xend = reorder(.data[[input$category_col]], value), y = 0, yend = value), linewidth = 1) + geom_point(size = 4) + pal_col + coord_flip() + labs(title = paste("Lollipop:", ifelse(!is.null(input$value_col), input$value_col, "Count"), "by", input$category_col), x = input$category_col, y = ifelse(!is.null(input$value_col), input$value_col, "Count")) + theme_minimal() + theme(legend.position = "none")
    } else if (choice == "hist") {
      req(input$value_col)
      dfh <- df %>% mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>% drop_na(.val)
      validate(need(nrow(dfh) > 0, "No numeric values found for the selected Value column."))
      p <- ggplot(dfh, aes(x = .val)) + geom_histogram(bins = 30, fill = "#5DA5DA", color = "white") + labs(title = paste("Histogram of", input$value_col), x = input$value_col, y = "Count") + theme_minimal()
    } else if (choice == "density") {
      req(input$value_col)
      dfd <- df %>% mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>% drop_na(.val)
      validate(need(nrow(dfd) > 0, "No numeric values found for the selected Value column."))
      p <- ggplot(dfd, aes(x = .val)) + geom_density(fill = "#B276B2", alpha = 0.5) + labs(title = paste("Density of", input$value_col), x = input$value_col, y = "Density") + theme_minimal()
    } else if (choice == "box") {
      req(input$category_col, input$value_col)
      db <- df %>% mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>% drop_na(.val)
      p <- ggplot(db, aes(x = .data[[input$category_col]], y = .val, fill = .data[[input$category_col]])) + geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) + pal_fill + labs(title = paste("Boxplot of", input$value_col, "by", input$category_col), x = input$category_col, y = input$value_col) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (choice == "violin") {
      req(input$category_col, input$value_col)
      dv <- df %>% mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>% drop_na(.val)
      p <- ggplot(dv, aes(x = .data[[input$category_col]], y = .val, fill = .data[[input$category_col]])) + geom_violin(show.legend = FALSE, alpha = 0.7, trim = FALSE) + pal_fill + labs(title = paste("Violin of", input$value_col, "by", input$category_col), x = input$category_col, y = input$value_col) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (choice == "line") {
      dl <- agg_one_cat() %>% mutate(.cat = .data[[input$category_col]])
      p <- ggplot(dl, aes(x = factor(.cat, levels = .cat), y = value, group = 1)) + geom_line(linewidth = 1, color = "#4E79A7") + geom_point(size = 3, color = "#4E79A7") + labs(title = paste("Line (ordered by value):", ifelse(!is.null(input$value_col), input$value_col, "Count"), "by", input$category_col), x = input$category_col, y = ifelse(!is.null(input$value_col), input$value_col, "Count")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (choice == "area") {
      da <- agg_one_cat() %>% mutate(.cat = .data[[input$category_col]])
      p <- ggplot(da, aes(x = factor(.cat, levels = .cat), y = value, group = 1)) + geom_area(alpha = 0.6, fill = "#59A14F") + geom_point(size = 3, color = "#59A14F") + labs(title = paste("Area (ordered by value):", ifelse(!is.null(input$value_col), input$value_col, "Count"), "by", input$category_col), x = input$category_col, y = ifelse(!is.null(input$value_col), input$value_col, "Count")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (choice == "grouped") {
      req(agg_two_cat())
      p <- ggplot(agg_two_cat(), aes(x = .data[[input$category_col]], y = value, fill = .data[[input$subcategory_col]])) + geom_col(position = position_dodge(width = 0.75)) + pal_fill + labs(title = paste("Grouped Bar:", input$category_col, "×", input$subcategory_col), x = input$category_col, y = paste("Total", ifelse(!is.null(input$value_col), input$value_col, "Count")), fill = input$subcategory_col) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (choice == "stacked") {
      req(agg_two_cat())
      p <- ggplot(agg_two_cat(), aes(x = .data[[input$category_col]], y = value, fill = .data[[input$subcategory_col]])) + geom_col() + pal_fill + labs(title = paste("Stacked Bar:", input$category_col, "×", input$subcategory_col), x = input$category_col, y = paste("Total", ifelse(!is.null(input$value_col), input$value_col, "Count")), fill = input$subcategory_col) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (choice == "heatmap") {
      req(agg_two_cat())
      p <- ggplot(agg_two_cat(), aes(x = .data[[input$subcategory_col]], y = .data[[input$category_col]], fill = value)) + geom_tile(color = "white") + scale_fill_gradient(low = "skyblue", high = "darkblue", labels = label_number_si()) + labs(title = paste("Heatmap:", input$category_col, "×", input$subcategory_col), x = input$subcategory_col, y = input$category_col, fill = paste("Σ", ifelse(!is.null(input$value_col), input$value_col, "Count"))) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (choice == "scatter") {
      req(input$x_num, input$y_num)
      pts <- df %>% mutate(.x = suppressWarnings(as.numeric(.data[[input$x_num]])), .y = suppressWarnings(as.numeric(.data[[input$y_num]]))) %>% drop_na(.x, .y)
      validate(need(nrow(pts) > 0, "Select numeric X and Y columns."))
      p <- ggplot(pts, aes(x = .x, y = .y)) + geom_point(color = "#E15759", alpha = 0.8) + labs(title = paste("Scatter:", input$x_num, "vs", input$y_num), x = input$x_num, y = input$y_num) + theme_minimal()
    } else if (choice == "bubble") {
      req(input$x_num, input$y_num, input$value_col)
      pts <- df %>% mutate(.x = suppressWarnings(as.numeric(.data[[input$x_num]])), .y = suppressWarnings(as.numeric(.data[[input$y_num]])), .size = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>% drop_na(.x, .y, .size)
      validate(need(nrow(pts) > 0, "Pick numeric X, Y, and Value columns."))
      p <- ggplot(pts, aes(x = .x, y = .y, size = .size)) + geom_point(alpha = 0.55, color = "#76B7B2") + scale_size_area(max_size = 16) + labs(title = paste("Bubble:", input$x_num, "vs", input$y_num, "| size =", input$value_col), x = input$x_num, y = input$y_num, size = input$value_col) + theme_minimal()
    } else if (choice == "density2d") {
      req(input$x_num, input$y_num)
      pts <- df %>% mutate(.x = suppressWarnings(as.numeric(.data[[input$x_num]])), .y = suppressWarnings(as.numeric(.data[[input$y_num]]))) %>% drop_na(.x, .y)
      validate(need(nrow(pts) > 0, "Pick numeric X and Y columns."))
      p <- ggplot(pts, aes(x = .x, y = .y)) + geom_point(alpha = 0.25) + geom_density_2d_filled(alpha = 0.6) + labs(title = paste("2D Density:", input$x_num, "vs", input$y_num), x = input$x_num, y = input$y_num) + theme_minimal()
    } else if (choice == "ridge") {
      req(input$value_col, input$category_col)
      dr <- df %>% mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>% drop_na(.val)
      validate(need(nrow(dr) > 0, "Pick a numeric Value column and a Category column."))
      p <- ggplot(dr, aes(x = .val, y = .data[[input$category_col]], fill = .data[[input$category_col]])) + ggridges::geom_density_ridges(alpha = 0.75, show.legend = FALSE, rel_min_height = 0.01) + pal_fill + labs(title = paste("Ridgeline of", input$value_col, "by", input$category_col), x = input$value_col, y = input$category_col) + theme_minimal()
    }
    
    if (!is.null(p)) p + custom_theme(input$font_choice, input$font_size, input$font_color) else NULL
  })
  
  # Plot rendering (uses make_plot)
  output$plot_out <- renderPlot({ req(make_plot()); make_plot() })
  
  # Full table tab (raw)
  output$table <- renderDT({ datatable(cleaned_data(), options = list(pageLength = 12, scrollX = TRUE)) })
  
  # Downloads: CSV, Excel, PDF
  output$download_csv <- downloadHandler(
    filename = function() paste0("dataset-", Sys.Date(), ".csv"),
    content = function(file) write.csv(cleaned_data(), file, row.names = FALSE)
  )
  output$download_excel <- downloadHandler(
    filename = function() paste0("dataset-", Sys.Date(), ".xlsx"),
    content = function(file) openxlsx::write.xlsx(cleaned_data(), file)
  )
  output$download_pdf <- downloadHandler(
    filename = function() paste0("dataset-", Sys.Date(), ".pdf"),
    content = function(file) {
      tmp <- tempfile(fileext = ".Rmd")
      writeLines(c(
        "---",
        "title: \"Dataset Export\"",
        "output: pdf_document",
        "---",
        "",
        "```{r, echo=FALSE}",
        "knitr::kable(head(cleaned_data(), 50))",
        "```"
      ), tmp)
      rmarkdown::render(tmp, output_file = file, quiet = TRUE)
    }
  )
  # SVG (reuses make_plot)
  output$download_svg <- downloadHandler(
    filename = function() paste0("plot-", Sys.Date(), ".svg"),
    content = function(file) {
      req(make_plot())
      ggsave(filename = file, plot = make_plot(), device = "svg", width = 10, height = 7)
    }
  )
  
  # ---------------- View Code (Ace) - read-only ----------------
  observe({
    # try to read this file; if not available show message
    code_text <- tryCatch(paste(readLines("app.R"), collapse = "\n"), error = function(e) "app.R not found on server.")
    updateAceEditor(session, "code_display", value = code_text)
  })
}

# =====================================================================
# APP
# =====================================================================
shinyApp(ui, server)
