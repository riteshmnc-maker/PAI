local_lib <- file.path(getwd(), "R_libs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = FALSE), .libPaths()))
}

required_packages <- c("shiny", "readxl", "dplyr", "DT", "bslib", "htmltools")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Run install.packages(c(",
    paste(sprintf("'%s'", missing_packages), collapse = ", "),
    "), lib = 'R_libs') from this folder.",
    call. = FALSE
  )
}

cache_dir <- file.path(getwd(), ".r-cache", "sass")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
options(sass.cache = cache_dir)

library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(bslib)
library(htmltools)

excel_file <- "Theme wise PAI Score of Gram Panchayat (Autosaved).xlsx"
sheet_name <- "PAI Score master"

score_columns <- c("Overall PDI Score", paste("Theme", 1:9))
district_column <- "District"
block_column <- "Block"
gp_column <- "GP"
gp_code_column <- "GP LGD Code"

load_pai_data <- function(path = excel_file) {
  if (!file.exists(path)) {
    stop("Could not find the PAI workbook: ", path, call. = FALSE)
  }

  raw_data <- read_excel(
    path,
    sheet = sheet_name,
    skip = 1,
    col_types = c("text", "text", "text", "text", rep("numeric", length(score_columns)))
  )

  expected_columns <- c(district_column, block_column, gp_column, gp_code_column, score_columns)
  missing_columns <- setdiff(expected_columns, names(raw_data))
  if (length(missing_columns) > 0) {
    stop(
      "The PAI sheet is missing expected column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  raw_data %>%
    select(all_of(expected_columns)) %>%
    mutate(
      across(all_of(c(district_column, block_column, gp_column, gp_code_column)), as.character),
      across(all_of(score_columns), as.numeric)
    ) %>%
    filter(
      !is.na(.data[[district_column]]),
      !is.na(.data[[block_column]]),
      !is.na(.data[[gp_column]])
    )
}

summarise_scores <- function(data, group_columns) {
  data %>%
    group_by(across(all_of(group_columns))) %>%
    summarise(
      `GP Count` = n(),
      across(all_of(score_columns), ~ round(mean(.x, na.rm = TRUE), 2)),
      .groups = "drop"
    ) %>%
    arrange(desc(`Overall PDI Score`))
}

format_gp_scores <- function(data) {
  data %>%
    select(all_of(c(district_column, block_column, gp_column, gp_code_column, score_columns))) %>%
    mutate(across(all_of(score_columns), ~ round(.x, 2))) %>%
    arrange(desc(`Overall PDI Score`))
}

add_pdi_tiers <- function(data) {
  scores <- data[["Overall PDI Score"]]
  valid_scores <- scores[!is.na(scores)]

  if (length(valid_scores) == 0) {
    data$Performance <- "middle"
    return(data)
  }

  cutoffs <- quantile(valid_scores, probs = c(1 / 3, 2 / 3), na.rm = TRUE, names = FALSE, type = 7)
  data$Performance <- case_when(
    is.na(scores) ~ "middle",
    scores <= cutoffs[[1]] ~ "bottom",
    scores <= cutoffs[[2]] ~ "middle",
    TRUE ~ "top"
  )
  data
}

datatable_with_tiers <- function(data) {
  table_data <- add_pdi_tiers(data)
  performance_column <- match("Performance", names(table_data)) - 1
  visible_columns <- setdiff(names(table_data), "Performance")

  datatable(
    table_data,
    rownames = FALSE,
    filter = "top",
    extensions = c("Buttons", "Scroller"),
    options = list(
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
      pageLength = 25,
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 520,
      scroller = TRUE,
      order = list(list(match("Overall PDI Score", names(table_data)) - 1, "desc")),
      columnDefs = list(list(targets = performance_column, visible = FALSE)),
      rowCallback = JS(sprintf(
        "function(row, data) {
          var tier = data[%d];
          var colors = {
            bottom: { bg: '#fee2e2', border: '#dc2626' },
            middle: { bg: '#ffedd5', border: '#f97316' },
            top: { bg: '#dcfce7', border: '#16a34a' }
          };
          if (colors[tier]) {
            $('td', row).css('background-color', colors[tier].bg);
            $('td:first', row).css('border-left', '6px solid ' + colors[tier].border);
          }
        }",
        performance_column
      ))
    ),
    class = "stripe hover compact nowrap",
  colnames = names(table_data)
  ) %>%
    formatRound(columns = score_columns, digits = 2)
}

app_data <- load_pai_data()

ui <- page_navbar(
  title = "PAI Performance Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1f6f5b"
  ),
  header = tagList(
    tags$style(HTML("
      body { background: #f7faf8; font-family: Inter, Segoe UI, Arial, sans-serif; }
      .navbar { box-shadow: 0 2px 12px rgba(15, 23, 42, 0.08); }
      .summary-band {
        display: grid;
        grid-template-columns: repeat(4, minmax(150px, 1fr));
        gap: 14px;
        margin: 18px 0 14px;
      }
      .metric {
        background: #ffffff;
        border: 1px solid #dbe7df;
        border-radius: 8px;
        padding: 14px 16px;
      }
      .metric-label {
        color: #475569;
        font-size: 0.82rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0;
      }
      .metric-value {
        color: #0f172a;
        font-size: 1.65rem;
        font-weight: 750;
        line-height: 1.1;
        margin-top: 6px;
      }
      .control-row {
        display: grid;
        grid-template-columns: repeat(2, minmax(220px, 320px));
        gap: 14px;
        align-items: end;
        margin: 12px 0 18px;
      }
      .legend {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        color: #334155;
        font-size: 0.92rem;
        margin: 0 0 12px;
      }
      .legend span {
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }
      .swatch {
        width: 14px;
        height: 14px;
        border-radius: 3px;
        border: 1px solid rgba(15, 23, 42, 0.15);
      }
      .swatch.red { background: #fee2e2; }
      .swatch.orange { background: #ffedd5; }
      .swatch.green { background: #dcfce7; }
      .dataTables_wrapper { background: #ffffff; border: 1px solid #dbe7df; border-radius: 8px; padding: 12px; }
      table.dataTable tbody td { vertical-align: middle; }
      @media (max-width: 760px) {
        .summary-band { grid-template-columns: repeat(2, minmax(120px, 1fr)); }
        .control-row { grid-template-columns: 1fr; }
      }
    ")),
    div(
      class = "container-fluid",
      div(
        class = "summary-band",
        div(class = "metric", div(class = "metric-label", "Districts"), div(class = "metric-value", textOutput("district_count", inline = TRUE))),
        div(class = "metric", div(class = "metric-label", "Blocks"), div(class = "metric-value", textOutput("block_count", inline = TRUE))),
        div(class = "metric", div(class = "metric-label", "Gram Panchayats"), div(class = "metric-value", textOutput("gp_count", inline = TRUE))),
        div(class = "metric", div(class = "metric-label", "Average Overall PDI"), div(class = "metric-value", textOutput("overall_average", inline = TRUE)))
      ),
      div(
        class = "legend",
        span(tags$i(class = "swatch red"), "Bottom third of the current table"),
        span(tags$i(class = "swatch orange"), "Middle third of the current table"),
        span(tags$i(class = "swatch green"), "Top third of the current table")
      )
    )
  ),
  nav_panel(
    "District Table",
    div(
      class = "container-fluid",
      DTOutput("district_table")
    )
  ),
  nav_panel(
    "Block Table",
    div(
      class = "container-fluid",
      div(
        class = "control-row",
        selectInput("block_district", "District", choices = character(0), selected = "All districts")
      ),
      DTOutput("block_table")
    )
  ),
  nav_panel(
    "GP Table",
    div(
      class = "container-fluid",
      div(
        class = "control-row",
        selectInput("gp_district", "District", choices = character(0), selected = "All districts"),
        selectInput("gp_block", "Block", choices = character(0), selected = "All blocks")
      ),
      DTOutput("gp_table")
    )
  )
)

server <- function(input, output, session) {
  district_choices <- c("All districts", sort(unique(app_data[[district_column]])))

  updateSelectInput(session, "block_district", choices = district_choices, selected = "All districts")
  updateSelectInput(session, "gp_district", choices = district_choices, selected = "All districts")
  updateSelectInput(session, "gp_block", choices = "All blocks", selected = "All blocks")

  observeEvent(input$gp_district, {
    block_source <- app_data
    if (!is.null(input$gp_district) && input$gp_district != "All districts") {
      block_source <- filter(block_source, .data[[district_column]] == input$gp_district)
    }

    updateSelectInput(
      session,
      "gp_block",
      choices = c("All blocks", sort(unique(block_source[[block_column]]))),
      selected = "All blocks"
    )
  }, ignoreNULL = FALSE)

  output$district_count <- renderText(format(length(unique(app_data[[district_column]])), big.mark = ","))
  output$block_count <- renderText(format(nrow(distinct(app_data, .data[[district_column]], .data[[block_column]])), big.mark = ","))
  output$gp_count <- renderText(format(nrow(app_data), big.mark = ","))
  output$overall_average <- renderText(sprintf("%.2f", mean(app_data[["Overall PDI Score"]], na.rm = TRUE)))

  district_scores <- reactive({
    summarise_scores(app_data, district_column)
  })

  block_scores <- reactive({
    block_data <- app_data
    if (!is.null(input$block_district) && input$block_district != "All districts") {
      block_data <- filter(block_data, .data[[district_column]] == input$block_district)
    }
    summarise_scores(block_data, c(district_column, block_column))
  })

  gp_scores <- reactive({
    gp_data <- app_data
    if (!is.null(input$gp_district) && input$gp_district != "All districts") {
      gp_data <- filter(gp_data, .data[[district_column]] == input$gp_district)
    }
    if (!is.null(input$gp_block) && input$gp_block != "All blocks") {
      gp_data <- filter(gp_data, .data[[block_column]] == input$gp_block)
    }
    format_gp_scores(gp_data)
  })

  output$district_table <- renderDT({
    datatable_with_tiers(district_scores())
  }, server = TRUE)

  output$block_table <- renderDT({
    datatable_with_tiers(block_scores())
  }, server = TRUE)

  output$gp_table <- renderDT({
    datatable_with_tiers(gp_scores())
  }, server = TRUE)
}

shinyApp(ui, server)
