library(shiny) 
library(readr)
library(dplyr)
library(dbplyr)
library(rlang)
library(RSQLite)

ui <- fluidPage(
  
  titlePanel("dplyr → SQL Translator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "schema_file",
        "Upload Schema CSV",
        accept = c(".csv")
      ),
      
      selectInput(
        "dialect",
        "SQL Dialect",
        choices = c(
          "SQL Server" = "mssql",
          "PostgreSQL" = "postgres",
          "SQLite" = "sqlite",
          "Oracle" = "oracle"
        ),
        selected = "mssql"
      ),
      
      textAreaInput(
        "dplyr_code",
        "Enter dplyr code using `tbl` as your table:",
        value = "tbl %>% filter(row_number() <= 10)",
        rows = 10,
        width = "100%"
      ),
      
      actionButton("run_btn", "Generate SQL")
    ),
    
    mainPanel(
      h4("Generated SQL:"),
      verbatimTextOutput("sql_output")
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Utility function: Choose the right simulated backend ----
  get_backend <- function(dialect) {
    switch(
      dialect,
      "mssql" = simulate_mssql(),
      "postgres" = simulate_postgres(),
      "sqlite" = simulate_sqlite(),
      "oracle" = simulate_oracle(),
      simulate_mssql()
    )
  }
  
  # ---- Reactive schema loader ----
  schema_tbl <- reactive({
    req(input$schema_file)
    read_csv(input$schema_file$datapath, show_col_types = FALSE)
  })
  
  # ---- SQL generator ----
  result_sql <- eventReactive(input$run_btn, {
    req(schema_tbl())
    req(input$dplyr_code)
    
    # Create a lazy table using the schema and chosen SQL dialect
    backend <- get_backend(input$dialect)
    tbl <- tbl_lazy(schema_tbl(), con = backend)
    
    # Parse dplyr code
    expr <- tryCatch(
      parse_expr(input$dplyr_code),
      error = function(e) return(e)
    )
    
    if (inherits(expr, "error")) {
      return(paste("Error parsing code:", expr$message))
    }
    
    # Evaluate user code with tbl available
    env <- list(tbl = tbl, .data = tbl)
    
    out <- tryCatch(
      eval(expr, envir = env),
      error = function(e) return(e)
    )
    
    if (inherits(out, "error")) {
      return(paste("Error evaluating code:", out$message))
    }
    
    # Generate SQL
    tryCatch(
      sql_render(out),
      error = function(e) paste("Error generating SQL:", e$message)
    )
  })
  
  # ---- Send SQL to UI ----
  output$sql_output <- renderText({
    result_sql()
  })
}

shinyApp(ui, server)
