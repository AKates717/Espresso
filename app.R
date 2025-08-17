# app.R — Espresso Dial‑In Logger & Dialing Helper
# Packages ----
# install.packages(c("shiny", "tidyverse", "DT", "openxlsx", "bslib"))
library(shiny)
library(tidyverse)
library(DT)
library(openxlsx)
library(bslib)

# ==== Config =====
DATA_FILE <- "espresso_shots.xlsx"  # saved in the app working dir by default

# Ensure workbook exists with proper header
ensure_workbook <- function(path = DATA_FILE) {
  if (!file.exists(path)) {
    df <- tibble(
      timestamp = as.POSIXct(character()),
      bean = character(),
      roaster = character(),
      origin = character(),
      roast_date = as.Date(character()),
      days_off_roast = numeric(),
      dose_g = numeric(),
      yield_g = numeric(),
      ratio = numeric(),
      time_s = numeric(),
      grinder_model = character(),
      grind_macro = numeric(),
      grind_micro = numeric(),
      temp_c = numeric(),
      preinf_bar = numeric(),
      preinf_s = numeric(),
      pressure_profile = character(),
      notes = character(),
      outcome = factor(levels = c("sour", "balanced", "bitter", "hollow", "astringent")),
      rating_10 = numeric()
    )
    wb <- createWorkbook()
    addWorksheet(wb, "shots")
    writeData(wb, "shots", df)
    saveWorkbook(wb, path, overwrite = TRUE)
  }
}

read_shots <- function(path = DATA_FILE) {
  ensure_workbook(path)
  suppressWarnings(
    read.xlsx(path, sheet = "shots", detectDates = TRUE) |>
      as_tibble() |>
      mutate(
        timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"),
        roast_date = as.Date(roast_date),
        days_off_roast = as.numeric(days_off_roast),
        ratio = if_else(is.na(ratio) & !is.na(yield_g) & !is.na(dose_g) & dose_g > 0, yield_g / dose_g, ratio),
        outcome = factor(outcome, levels = c("sour", "balanced", "bitter", "hollow", "astringent"))
      )
  )
}

append_shot <- function(new_row, path = DATA_FILE) {
  df <- read_shots(path)
  df2 <- bind_rows(df, new_row)
  wb <- createWorkbook()
  addWorksheet(wb, "shots")
  writeData(wb, "shots", df2)
  saveWorkbook(wb, path, overwrite = TRUE)
}

# Simple recommender: median of best past shots for same bean (or global)
recommend_from_history <- function(df, bean, grinder_model = NULL) {
  df <- df |>
    filter(!is.na(rating_10)) |>
    arrange(desc(rating_10))
  
  pool <- df
  if (!is.null(bean) && nzchar(bean)) pool <- pool |>
    filter(str_to_lower(bean) == str_to_lower(.data$bean))
  
  # If still empty, relax to roaster/origin similarities in future—here fallback to global
  if (nrow(pool) == 0) pool <- df
  
  top <- pool |>
    filter(rating_10 >= quantile(rating_10, probs = 0.75, na.rm = TRUE))
  if (nrow(top) == 0) top <- pool
  
  rec <- top |>
    summarise(
      dose_g = round(median(dose_g, na.rm = TRUE), 1),
      ratio = round(median(ratio, na.rm = TRUE), 2),
      yield_g = round(median(yield_g, na.rm = TRUE), 1),
      time_s = round(median(time_s, na.rm = TRUE), 0),
      temp_c = round(median(temp_c, na.rm = TRUE), 1),
      preinf_bar = round(median(preinf_bar, na.rm = TRUE), 1),
      preinf_s = round(median(preinf_s, na.rm = TRUE), 0),
      grind_macro = round(median(grind_macro, na.rm = TRUE), 0),
      grind_micro = round(median(grind_micro, na.rm = TRUE), 0)
    )
  
  # Fill sane baselines if NA
  defaults <- tibble(
    dose_g = 18, ratio = 2.0, yield_g = 36, time_s = 28,
    temp_c = 94, preinf_bar = 2.5, preinf_s = 6,
    grind_macro = NA, grind_micro = NA
  )
  
  rec <- defaults |>
    modify2(rec, ~ ifelse(is.na(.y), .x, .y)) |>
    as_tibble()
  
  rec
}

# ===== UI =====
ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Espresso Dial‑In Logger & Helper"),
  layout_sidebar(
    sidebar = sidebar(
      h4("Log a Shot"),
      textInput("bean", "Bean (e.g., Gesha Peru)", placeholder = "Gesha Peru"),
      textInput("roaster", "Roaster", placeholder = "Roaster name"),
      textInput("origin", "Origin", placeholder = "Country/Region"),
      dateInput("roast_date", "Roast date", value = NA),
      numericInput("dose_g", "Dose (g)", value = 18, step = 0.1, min = 0),
      numericInput("yield_g", "Yield (g)", value = 36, step = 0.1, min = 0),
      numericInput("time_s", "Shot time (s)", value = 28, step = 1, min = 0),
      radioButtons("grinder_model", "Grinder", choices = c("Baratza Sette 270", "Other"), selected = "Baratza Sette 270"),
      numericInput("grind_macro", "Sette Macro (0–31)", value = 10, step = 1, min = 0, max = 31),
      numericInput("grind_micro", "Sette Micro (0–9)", value = 5, step = 1, min = 0, max = 9),
      numericInput("temp_c", "Brew temp (°C)", value = 94, step = 0.1),
      numericInput("preinf_bar", "Pre‑inf pressure (bar)", value = 2.5, step = 0.1),
      numericInput("preinf_s", "Pre‑inf time (s)", value = 6, step = 1),
      textInput("pressure_profile", "Pressure profile", placeholder = "e.g., 9→6 bar decline"),
      selectInput("outcome", "Outcome", choices = c("", "sour", "balanced", "bitter", "hollow", "astringent"), selected = ""),
      sliderInput("rating_10", "Rating (1–10)", min = 1, max = 10, value = 7, step = 1),
      textAreaInput("notes", "Notes", placeholder = "Tasting / channeling / puck prep etc.", height = "100px"),
      actionButton("save", "Save shot", class = "btn-primary"),
      hr(),
      h4("Dial‑In Helper"),
      selectizeInput("bean_filter", "Filter by bean", choices = NULL, multiple = FALSE, options = list(create = TRUE, placeholder = "Start typing…")),
      actionButton("recommend", "Get recommendation", class = "btn-success"),
      hr(),
      h5("Data file:"),
      verbatimTextOutput("filepath")
    ),
    card(
      card_header("Recommended Starting Point"),
      uiOutput("rec_ui"),
      card_body(
        fluidRow(
          column(6, plotOutput("plot_rating_by_date", height = 260)),
          column(6, plotOutput("plot_ratio_vs_rating", height = 260))
        )
      ),
      hr(),
      card_header("Shot History"),
      DTOutput("table")
    )
  )
)

# ===== Server =====
server <- function(input, output, session) {
  ensure_workbook(DATA_FILE)
  
  # Reactive dataset read fresh on demand
  shots <- reactiveVal(read_shots(DATA_FILE))
  
  observe({
    # populate bean filter choices
    beans <- shots() |> distinct(bean) |> arrange(bean) |> pull(bean) |> discard(is.na) |> unique()
    updateSelectizeInput(session, "bean_filter", choices = beans, server = TRUE)
  })
  
  output$filepath <- renderText(normalizePath(DATA_FILE))
  
  # Save handler
  observeEvent(input$save, {
    req(input$dose_g, input$yield_g, input$time_s)
    df <- shots()
    
    days_off <- if (!is.null(input$roast_date) && !is.na(input$roast_date)) as.numeric(Sys.Date() - input$roast_date) else NA_real_
    
    new_row <- tibble(
      timestamp = Sys.time(),
      bean = str_squish(input$bean),
      roaster = str_squish(input$roaster),
      origin = str_squish(input$origin),
      roast_date = as.Date(input$roast_date),
      days_off_roast = days_off,
      dose_g = as.numeric(input$dose_g),
      yield_g = as.numeric(input$yield_g),
      ratio = ifelse(isTRUE(input$dose_g > 0), as.numeric(input$yield_g) / as.numeric(input$dose_g), NA_real_),
      time_s = as.numeric(input$time_s),
      grinder_model = input$grinder_model,
      grind_macro = as.numeric(input$grind_macro),
      grind_micro = as.numeric(input$grind_micro),
      temp_c = as.numeric(input$temp_c),
      preinf_bar = as.numeric(input$preinf_bar),
      preinf_s = as.numeric(input$preinf_s),
      pressure_profile = input$pressure_profile,
      notes = input$notes,
      outcome = ifelse(nzchar(input$outcome), input$outcome, NA_character_),
      rating_10 = as.numeric(input$rating_10)
    )
    
    append_shot(new_row)
    shots(read_shots(DATA_FILE))
    showNotification("Shot saved!", type = "message")
  })
  
  # Recommendation logic
  rec_data <- eventReactive(input$recommend, {
    df <- shots()
    recommend_from_history(df, input$bean_filter)
  }, ignoreInit = TRUE)
  
  output$rec_ui <- renderUI({
    req(rec_data())
    rec <- rec_data()
    
    tagList(
      fluidRow(
        column(4, strong("Dose (g)"), div(rec$dose_g)),
        column(4, strong("Ratio (out)"), div(rec$ratio)),
        column(4, strong("Yield (g)"), div(rec$yield_g))
      ),
      br(),
      fluidRow(
        column(4, strong("Time (s)"), div(rec$time_s)),
        column(4, strong("Temp (°C)"), div(rec$temp_c)),
        column(4, strong("Pre‑inf (bar · s)"), div(paste0(rec$preinf_bar, " · ", rec$preinf_s)))
      ),
      br(),
      fluidRow(
        column(6, strong("Sette Macro"), div(ifelse(is.na(rec$grind_macro), "(set by taste)", rec$grind_macro))),
        column(6, strong("Sette Micro"), div(ifelse(is.na(rec$grind_micro), "(set by taste)", rec$grind_micro)))
      ),
      tags$em("Tip: If shots taste sour, grind finer or raise temp 0.5–1°C; if bitter/astringent, grind coarser or drop temp 0.5–1°C.")
    )
  })
  
  # Plots
  output$plot_rating_by_date <- renderPlot({
    df <- shots()
    validate(need(nrow(df) > 0, "No data yet"))
    df |>
      arrange(timestamp) |>
      mutate(date = as.Date(timestamp)) |>
      ggplot(aes(date, rating_10)) +
      geom_point() +
      geom_line() +
      labs(x = "Date", y = "Rating (1–10)", title = "Ratings over time") +
      ylim(1, 10)
  })
  
  output$plot_ratio_vs_rating <- renderPlot({
    df <- shots()
    validate(need(nrow(df) > 0, "No data yet"))
    ggplot(df, aes(ratio, rating_10)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE) +
      labs(x = "Brew ratio (out/in)", y = "Rating (1–10)", title = "Brew ratio vs rating")
  })
  
  # Table
  output$table <- renderDT({
    df <- shots() |>
      arrange(desc(timestamp))
    datatable(
      df,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
