# app.R
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

# Use public access (no login) for public sheets
gs4_deauth()

# ---------------- UI ----------------
ui <- fluidPage(
  titlePanel("{ Dynamic Google Sheet Dashboard }"),
  sidebarLayout(
    sidebarPanel(
      helpText("Paste a public Google Sheet link (Share ➜ Anyone with the link = Viewer)."),
      textInput(
        "sheet_url", "🔗 Google Sheet URL:",
        value = "https://docs.google.com/spreadsheets/d/1Hzcm-CxYlh6BueA4bbARkklxDycIHnxYDIdzuFl3ODQ/edit?pli=1&gid=0#gid=0"
      ),
      actionButton("load_data", "📥 Load Sheet"),
      tags$hr(),
      
      # Visualization chooser (always visible)
      selectInput(
        "viz_choice", "📊 Choose Visualization:",
        choices = c(
          # Tables
          "📄 Tables: Raw Data" = "tbl_raw",
          "🧮 Tables: Summary by Category" = "tbl_summary",
          "🔢 Tables: Frequency of Category" = "tbl_freq",
          "🔗 Tables: Correlation Matrix (numeric)" = "tbl_corr",
          # One-category charts (category + optional numeric)
          "🥧 Charts: Pie (count by Category)" = "pie",
          "📊 Charts: Bar (sum Value by Category)" = "bar",
          "⚪ Charts: Dot (sum Value by Category)" = "dot",
          "🍭 Charts: Lollipop (sum Value by Category)" = "lollipop",
          "🔢 Charts: Histogram (Value)" = "hist",
          "🌫️ Charts: Density (Value)" = "density",
          "📦 Charts: Boxplot (Value ~ Category)" = "box",
          "🎻 Charts: Violin (Value ~ Category)" = "violin",
          "📈 Charts: Line (sum Value by Category)" = "line",
          "🟢 Charts: Area (sum Value by Category)" = "area",
          # Two-category / relationships
          "📚 Charts: Grouped Bar (Category × Subcategory)" = "grouped",
          "🧱 Charts: Stacked Bar (Category × Subcategory)" = "stacked",
          "🔥 Charts: Heatmap (Category × Subcategory, sum Value)" = "heatmap",
          "🔴 Charts: Scatter (X vs Y)" = "scatter",
          "🔵 Charts: Bubble (X vs Y, size = Value)" = "bubble",
          # New
          "🌐 Charts: 2D Density (X vs Y)" = "density2d",
          "🏔️ Charts: Ridgeline (Value ~ Category)" = "ridge"
        ),
        selected = "tbl_raw"
      ),
      
      tags$hr(),
      # Dynamic selectors appear AFTER data is loaded
      uiOutput("selectors")
    ),
    mainPanel(
      # Spinner overlay while rendering
      uiOutput("viz_panel") %>% withSpinner(color = "#4E79A7", type = 6)
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  data_rv <- reactiveVal(NULL)
  
  # Helper to pick a default if present, else first available
  default_or_first <- function(choices, preferred) {
    if (length(choices) == 0) return(NULL)
    pref <- preferred[preferred %in% choices]
    if (length(pref)) pref[1] else choices[1]
  }
  
  # Load sheet with progress + robust URL handling
  observeEvent(input$load_data, {
    req(input$sheet_url)
    withProgress(message = "Loading Google Sheet...", value = 0.1, {
      df <- NULL
      try({
        df <- suppressMessages(read_sheet(input$sheet_url))
      }, silent = TRUE)
      
      if (is.null(df)) {
        # try extracting the /d/<id> and reading by id
        sheet_id <- gsub(".*?/d/([^/]+)(/.*|$)", "\\1", input$sheet_url)
        try({
          df <- suppressMessages(read_sheet(sheet_id))
        }, silent = TRUE)
      }
      
      if (is.null(df)) {
        showNotification(
          "⚠️ Could not load the sheet. Check the URL & sharing (Anyone with link = Viewer).",
          type = "error", duration = 7
        )
        return(invisible(NULL))
      }
      
      incProgress(0.6, detail = "Parsing data...")
      Sys.sleep(0.2)
      data_rv(df)
      incProgress(0.25, detail = "Done")
      showNotification("✅ Sheet loaded!", type = "message", duration = 3)
      
      # After load, set sensible defaults (Group/A/B if present)
      cats <- names(df)
      nums <- names(df)[sapply(df, function(x) suppressWarnings(any(!is.na(as.numeric(x)))))]
      
      updateSelectInput(session, "category_col",
                        choices = cats,
                        selected = default_or_first(cats, c("Group","group","CATEGORY","Category")))
      updateSelectInput(session, "value_col",
                        choices = nums,
                        selected = default_or_first(nums, c("A","a","Value","value")))
      updateSelectInput(session, "subcategory_col",
                        choices = c("— none —", cats),
                        selected = "— none —")
      # X/Y for scatter/bubble/2D density
      x_default <- default_or_first(nums, c("A","a","X","x"))
      y_default <- default_or_first(nums[nums != x_default], c("B","b","Y","y"))
      updateSelectInput(session, "x_num", choices = nums, selected = x_default)
      updateSelectInput(session, "y_num", choices = nums, selected = y_default)
    })
  })
  
  # Helpers for column type sets
  num_cols <- reactive({
    req(data_rv())
    nms <- names(data_rv())
    keep <- sapply(data_rv(), function(x) suppressWarnings(any(!is.na(as.numeric(x)))))
    nms[keep]
  })
  cat_cols <- reactive({
    req(data_rv())
    names(data_rv())
  })
  
  # Selectors (appear after data is available)
  output$selectors <- renderUI({
    req(data_rv())
    tagList(
      selectInput("category_col", "Category column:", choices = cat_cols()),
      selectInput("value_col", "Numeric value column:", choices = num_cols()),
      selectInput("subcategory_col", "Subcategory column (optional):",
                  choices = c("— none —", cat_cols()), selected = "— none —"),
      selectInput("x_num", "X (numeric, for Scatter/Bubble/2D Density):",
                  choices = num_cols()),
      selectInput("y_num", "Y (numeric, for Scatter/Bubble/2D Density):",
                  choices = num_cols())
    )
  })
  
  # Cleaned data
  cleaned_data <- reactive({
    req(data_rv())
    data_rv()
  })
  
  # Aggregations
  agg_one_cat <- reactive({
    req(cleaned_data(), input$category_col)
    # Try to use numeric value; if none selected or non-numeric -> fall back to counts
    if (!is.null(input$value_col) && input$value_col %in% names(cleaned_data())) {
      v <- suppressWarnings(as.numeric(cleaned_data()[[input$value_col]]))
      if (any(!is.na(v))) {
        return(
          cleaned_data() %>%
            mutate(.val = v) %>%
            group_by(.data[[input$category_col]]) %>%
            summarise(value = sum(.val, na.rm = TRUE), .groups = "drop") %>%
            drop_na() %>%
            arrange(desc(value))
        )
      }
    }
    # Fallback: counts per category
    cleaned_data() %>%
      filter(!is.na(.data[[input$category_col]])) %>%
      count(.data[[input$category_col]], name = "value") %>%
      arrange(desc(value))
  })
  
  agg_two_cat <- reactive({
    req(cleaned_data(), input$category_col)
    validate(need(input$subcategory_col != "— none —",
                  "Please pick a Subcategory column for this visualization."))
    v <- if (!is.null(input$value_col) && input$value_col %in% names(cleaned_data()))
      suppressWarnings(as.numeric(cleaned_data()[[input$value_col]])) else NA_real_
    cleaned_data() %>%
      mutate(.val = v) %>%
      group_by(.data[[input$category_col]], .data[[input$subcategory_col]]) %>%
      summarise(value = if (any(!is.na(.val))) sum(.val, na.rm = TRUE) else dplyr::n(), .groups = "drop") %>%
      drop_na()
  })
  
  corr_mat <- reactive({
    req(cleaned_data())
    nums <- cleaned_data() %>%
      mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
      select(where(~ any(!is.na(.))))
    validate(need(ncol(nums) >= 2, "Need at least two numeric columns to compute a correlation matrix."))
    as.data.frame(round(cor(nums, use = "pairwise.complete.obs"), 3))
  })
  
  # Decide table vs plot container
  output$viz_panel <- renderUI({
    req(input$viz_choice)
    if (grepl("^tbl_", input$viz_choice)) {
      DTOutput("table_out")
    } else {
      plotOutput("plot_out", height = "560px")
    }
  })
  
  # ------------- Tables -------------
  output$table_out <- renderDT({
    req(cleaned_data(), input$viz_choice)
    if (input$viz_choice == "tbl_raw") {
      datatable(cleaned_data(), options = list(pageLength = 12, scrollX = TRUE))
    } else if (input$viz_choice == "tbl_summary") {
      req(input$category_col)
      has_val <- !is.null(input$value_col) && input$value_col %in% names(cleaned_data())
      df <- cleaned_data()
      if (has_val) {
        v <- suppressWarnings(as.numeric(df[[input$value_col]]))
        df <- df %>% mutate(.val = v)
      }
      tbl <- df %>%
        group_by(.data[[input$category_col]]) %>%
        summarise(
          n = n(),
          sum = if (has_val && any(!is.na(.val))) sum(.val, na.rm = TRUE) else NA_real_,
          mean = if (has_val && any(!is.na(.val))) mean(.val, na.rm = TRUE) else NA_real_,
          median = if (has_val && any(!is.na(.val))) median(.val, na.rm = TRUE) else NA_real_,
          sd = if (has_val && any(!is.na(.val))) sd(.val, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) %>% arrange(desc(n))
      datatable(tbl, options = list(pageLength = 12, scrollX = TRUE))
    } else if (input$viz_choice == "tbl_freq") {
      req(input$category_col)
      tbl <- cleaned_data() %>%
        count(.data[[input$category_col]]) %>%
        arrange(desc(n))
      datatable(tbl, options = list(pageLength = 12, scrollX = TRUE))
    } else if (input$viz_choice == "tbl_corr") {
      datatable(corr_mat(), options = list(pageLength = 12, scrollX = TRUE))
    }
  })
  
  # ------------- Plots -------------
  output$plot_out <- renderPlot({
    req(cleaned_data(), input$viz_choice)
    
    pal_fill <- scale_fill_brewer(palette = "Set3")
    pal_col  <- scale_color_brewer(palette = "Dark2")
    
    if (input$viz_choice == "pie") {
      req(input$category_col)
      pie_data <- cleaned_data() %>%
        filter(!is.na(.data[[input$category_col]])) %>%
        count(category = as.character(.data[[input$category_col]]))
      validate(need(nrow(pie_data) > 0, "No non-missing values in the selected Category."))
      ggplot(pie_data, aes(x = "", y = n, fill = category)) +
        geom_col(width = 1, color = "white") +
        coord_polar("y") + pal_fill +
        labs(title = paste("🥧 Distribution of", input$category_col), fill = input$category_col) +
        theme_void()
      
    } else if (input$viz_choice == "bar") {
      df <- agg_one_cat()
      ggplot(df, aes(x = reorder(!!sym(input$category_col), -value),
                     y = value, fill = !!sym(input$category_col))) +
        geom_col(show.legend = FALSE) + pal_fill +
        labs(title = paste("📊 Total", ifelse(!is.null(input$value_col), input$value_col, "Count"),
                           "by", input$category_col),
             x = input$category_col, y = ifelse(!is.null(input$value_col), input$value_col, "Count")) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$viz_choice == "dot") {
      df <- agg_one_cat()
      ggplot(df, aes(x = reorder(!!sym(input$category_col), -value),
                     y = value, color = !!sym(input$category_col))) +
        geom_point(size = 5, alpha = 0.9, show.legend = FALSE) + pal_col +
        labs(title = paste("⚪ Dot Plot of",
                           ifelse(!is.null(input$value_col), input$value_col, "Count"),
                           "by", input$category_col),
             x = input$category_col, y = ifelse(!is.null(input$value_col), input$value_col, "Count")) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$viz_choice == "lollipop") {
      df <- agg_one_cat()
      ggplot(df, aes(x = reorder(!!sym(input$category_col), value),
                     y = value, color = !!sym(input$category_col))) +
        geom_segment(aes(xend = reorder(!!sym(input$category_col), value),
                         y = 0, yend = value), linewidth = 1) +
        geom_point(size = 4) + pal_col +
        coord_flip() +
        labs(title = paste("🍭 Lollipop:",
                           ifelse(!is.null(input$value_col), input$value_col, "Count"),
                           "by", input$category_col),
             x = input$category_col,
             y = ifelse(!is.null(input$value_col), input$value_col, "Count")) +
        theme_minimal() + theme(legend.position = "none")
      
    } else if (input$viz_choice == "hist") {
      req(input$value_col)
      df <- cleaned_data() %>%
        mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>%
        drop_na(.val)
      validate(need(nrow(df) > 0, "No numeric values found for the selected Value column."))
      ggplot(df, aes(x = .val)) +
        geom_histogram(bins = 30, fill = "#5DA5DA", color = "white") +
        labs(title = paste("🔢 Histogram of", input$value_col), x = input$value_col, y = "Count") +
        theme_minimal()
      
    } else if (input$viz_choice == "density") {
      req(input$value_col)
      df <- cleaned_data() %>%
        mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>%
        drop_na(.val)
      validate(need(nrow(df) > 0, "No numeric values found for the selected Value column."))
      ggplot(df, aes(x = .val)) +
        geom_density(fill = "#B276B2", alpha = 0.5) +
        labs(title = paste("🌫️ Density of", input$value_col), x = input$value_col, y = "Density") +
        theme_minimal()
      
    } else if (input$viz_choice == "box") {
      req(input$category_col, input$value_col)
      df <- cleaned_data() %>%
        mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>%
        drop_na(.val)
      ggplot(df, aes(x = !!sym(input$category_col), y = .val, fill = !!sym(input$category_col))) +
        geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) + pal_fill +
        labs(title = paste("📦 Boxplot of", input$value_col, "by", input$category_col),
             x = input$category_col, y = input$value_col) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$viz_choice == "violin") {
      req(input$category_col, input$value_col)
      df <- cleaned_data() %>%
        mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>%
        drop_na(.val)
      ggplot(df, aes(x = !!sym(input$category_col), y = .val, fill = !!sym(input$category_col))) +
        geom_violin(show.legend = FALSE, alpha = 0.7, trim = FALSE) + pal_fill +
        labs(title = paste("🎻 Violin of", input$value_col, "by", input$category_col),
             x = input$category_col, y = input$value_col) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$viz_choice == "line") {
      df <- agg_one_cat() %>% mutate(.cat = !!sym(input$category_col))
      ggplot(df, aes(x = factor(.cat, levels = .cat), y = value, group = 1)) +
        geom_line(linewidth = 1, color = "#4E79A7") +
        geom_point(size = 3, color = "#4E79A7") +
        labs(title = paste("📈 Line (ordered by value):",
                           ifelse(!is.null(input$value_col), input$value_col, "Count"),
                           "by", input$category_col),
             x = input$category_col, y = ifelse(!is.null(input$value_col), input$value_col, "Count")) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$viz_choice == "area") {
      df <- agg_one_cat() %>% mutate(.cat = !!sym(input$category_col))
      ggplot(df, aes(x = factor(.cat, levels = .cat), y = value, group = 1)) +
        geom_area(alpha = 0.6, fill = "#59A14F") +
        geom_point(size = 3, color = "#59A14F") +
        labs(title = paste("🟢 Area (ordered by value):",
                           ifelse(!is.null(input$value_col), input$value_col, "Count"),
                           "by", input$category_col),
             x = input$category_col, y = ifelse(!is.null(input$value_col), input$value_col, "Count")) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$viz_choice == "grouped") {
      df <- agg_two_cat()
      ggplot(df, aes(x = !!sym(input$category_col), y = value, fill = !!sym(input$subcategory_col))) +
        geom_col(position = position_dodge(width = 0.75)) +
        pal_fill +
        labs(title = paste("📚 Grouped Bar:", input$category_col, "×", input$subcategory_col),
             x = input$category_col, y = paste("Total", ifelse(!is.null(input$value_col), input$value_col, "Count")),
             fill = input$subcategory_col) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$viz_choice == "stacked") {
      df <- agg_two_cat()
      ggplot(df, aes(x = !!sym(input$category_col), y = value, fill = !!sym(input$subcategory_col))) +
        geom_col() + pal_fill +
        labs(title = paste("🧱 Stacked Bar:", input$category_col, "×", input$subcategory_col),
             x = input$category_col, y = paste("Total", ifelse(!is.null(input$value_col), input$value_col, "Count")),
             fill = input$subcategory_col) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$viz_choice == "heatmap") {
      df <- agg_two_cat()
      ggplot(df, aes(x = !!sym(input$subcategory_col), y = !!sym(input$category_col), fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "skyblue", high = "darkblue", labels = label_number_si()) +
        labs(title = paste("🔥 Heatmap:", input$category_col, "×", input$subcategory_col),
             x = input$subcategory_col, y = input$category_col, fill = paste("Σ", ifelse(!is.null(input$value_col), input$value_col, "Count"))) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$viz_choice == "scatter") {
      req(input$x_num, input$y_num)
      df <- cleaned_data() %>%
        mutate(.x = suppressWarnings(as.numeric(.data[[input$x_num]])),
               .y = suppressWarnings(as.numeric(.data[[input$y_num]]))) %>%
        drop_na(.x, .y)
      validate(need(nrow(df) > 0, "Select numeric X and Y columns."))
      ggplot(df, aes(x = .x, y = .y)) +
        geom_point(color = "#E15759", alpha = 0.8) +
        labs(title = paste("🔴 Scatter:", input$x_num, "vs", input$y_num),
             x = input$x_num, y = input$y_num) +
        theme_minimal()
      
    } else if (input$viz_choice == "bubble") {
      req(input$x_num, input$y_num, input$value_col)
      df <- cleaned_data() %>%
        mutate(.x = suppressWarnings(as.numeric(.data[[input$x_num]])),
               .y = suppressWarnings(as.numeric(.data[[input$y_num]])),
               .size = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>%
        drop_na(.x, .y, .size)
      validate(need(nrow(df) > 0, "Pick numeric X, Y, and Value columns."))
      ggplot(df, aes(x = .x, y = .y, size = .size)) +
        geom_point(alpha = 0.55, color = "#76B7B2") +
        scale_size_area(max_size = 16) +
        labs(title = paste("🔵 Bubble:", input$x_num, "vs", input$y_num, "| size =", input$value_col),
             x = input$x_num, y = input$y_num, size = input$value_col) +
        theme_minimal()
      
    } else if (input$viz_choice == "density2d") {
      req(input$x_num, input$y_num)
      df <- cleaned_data() %>%
        mutate(.x = suppressWarnings(as.numeric(.data[[input$x_num]])),
               .y = suppressWarnings(as.numeric(.data[[input$y_num]]))) %>%
        drop_na(.x, .y)
      validate(need(nrow(df) > 0, "Pick numeric X and Y columns."))
      ggplot(df, aes(x = .x, y = .y)) +
        geom_point(alpha = 0.25) +
        geom_density_2d_filled(alpha = 0.6) +
        labs(title = paste("🌐 2D Density:", input$x_num, "vs", input$y_num),
             x = input$x_num, y = input$y_num) +
        theme_minimal()
      
    } else if (input$viz_choice == "ridge") {
      req(input$value_col, input$category_col)
      df <- cleaned_data() %>%
        mutate(.val = suppressWarnings(as.numeric(.data[[input$value_col]]))) %>%
        drop_na(.val)
      validate(need(nrow(df) > 0, "Pick a numeric Value column and a Category column."))
      ggplot(df, aes(x = .val, y = !!sym(input$category_col), fill = !!sym(input$category_col))) +
        geom_density_ridges(alpha = 0.75, show.legend = FALSE, rel_min_height = 0.01) + pal_fill +
        labs(title = paste("🏔️ Ridgeline of", input$value_col, "by", input$category_col),
             x = input$value_col, y = input$category_col) +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)
