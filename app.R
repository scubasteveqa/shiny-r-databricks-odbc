library(shiny)
library(bslib)
library(DBI)
library(odbc)
library(connectapi)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)

# ── Data helpers ──────────────────────────────────────────────────────────────

fetch_data <- function(access_token) {
  conn <- dbConnect(
    odbc::databricks(),
    httpPath = Sys.getenv("DATABRICKS_PATH"),
    authMech = 11,
    auth_flow = 0,
    auth_accesstoken = access_token
  )
  on.exit(dbDisconnect(conn))

  dbGetQuery(conn, "
    SELECT
      t.dateTime,
      t.product,
      t.quantity,
      t.totalPrice,
      c.continent,
      c.country,
      f.name AS franchise_name
    FROM samples.bakehouse.sales_transactions t
    JOIN samples.bakehouse.sales_customers c
      ON t.customerID = c.customerID
    JOIN samples.bakehouse.sales_franchises f
      ON t.franchiseID = f.franchiseID
  ")
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Bakehouse Franchise Dashboard (odbc::databricks)",
  sidebar = sidebar(
    width = 260,
    h3("Bakehouse Dashboard"),
    p(
      "Franchise sales analytics powered by ",
      code("samples.bakehouse"),
      style = "color: #6c757d; font-size: 0.85rem; margin: 0;"
    ),
    actionButton("load_data", "Refresh Data", class = "btn-primary w-100"),
    tags$script(HTML(
      "setTimeout(function() { document.getElementById('load_data').click(); }, 500);"
    )),
    selectInput("continent", "Continent", choices = "All", selected = "All"),
    selectInput("franchise", "Franchise", choices = "All", selected = "All")
  ),

  layout_columns(
    value_box("Total Revenue", textOutput("total_revenue"), theme = "primary"),
    value_box("Total Orders", textOutput("total_orders"), theme = "info"),
    value_box("Avg Order Value", textOutput("avg_order"), theme = "success"),
    value_box("Franchises", textOutput("franchise_count"), theme = "warning"),
    col_widths = c(3, 3, 3, 3)
  ),
  layout_columns(
    card(card_header("Revenue by Franchise"), plotlyOutput("chart_franchise_revenue")),
    card(card_header("Revenue by Continent"), plotlyOutput("chart_continent")),
    col_widths = c(6, 6)
  ),
  layout_columns(
    card(card_header("Top Products by Revenue"), plotlyOutput("chart_products")),
    card(card_header("Monthly Revenue Trend"), plotlyOutput("chart_trend")),
    col_widths = c(6, 6)
  ),
  card(
    card_header("Transaction Data"),
    DTOutput("sales_table")
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  raw_data <- reactiveVal(NULL)

  observeEvent(input$load_data, {
    tryCatch({
      session_token <- session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN
      if (is.null(session_token) || session_token == "") {
        showNotification("No session token found. Deploy this app on Posit Connect.",
                         type = "error")
        return()
      }

      client <- connectapi::connect()
      credentials <- connectapi::get_oauth_credentials(client, session_token)
      access_token <- credentials$access_token

      df <- fetch_data(access_token)
      names(df) <- tolower(names(df))
      df$datetime <- as.POSIXct(df$datetime)
      df$month <- format(df$datetime, "%Y-%m")

      raw_data(df)

      continents <- c("All", sort(unique(df$continent)))
      franchises <- c("All", sort(unique(df$franchise_name)))
      updateSelectInput(session, "continent", choices = continents, selected = "All")
      updateSelectInput(session, "franchise", choices = franchises, selected = "All")

      showNotification(paste("Loaded", nrow(df), "rows from Databricks"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 10)
    })
  })

  filtered_data <- reactive({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    if (input$continent != "All") {
      df <- df[df$continent == input$continent, ]
    }
    if (input$franchise != "All") {
      df <- df[df$franchise_name == input$franchise, ]
    }
    df
  })

  output$total_revenue <- renderText({
    df <- filtered_data()
    if (is.null(df)) return("--")
    paste0("$", formatC(sum(df$totalprice), format = "f", digits = 2, big.mark = ","))
  })

  output$total_orders <- renderText({
    df <- filtered_data()
    if (is.null(df)) return("--")
    formatC(nrow(df), format = "d", big.mark = ",")
  })

  output$avg_order <- renderText({
    df <- filtered_data()
    if (is.null(df) || nrow(df) == 0) return("--")
    paste0("$", formatC(mean(df$totalprice), format = "f", digits = 2, big.mark = ","))
  })

  output$franchise_count <- renderText({
    df <- filtered_data()
    if (is.null(df)) return("--")
    as.character(length(unique(df$franchise_name)))
  })

  output$chart_franchise_revenue <- renderPlotly({
    df <- filtered_data()
    if (is.null(df)) return(plot_ly() |> layout(title = "Loading..."))
    agg <- df |>
      group_by(franchise_name) |>
      summarise(revenue = sum(totalprice), .groups = "drop") |>
      arrange(revenue)
    agg$franchise_name <- factor(agg$franchise_name, levels = agg$franchise_name)
    plot_ly(agg, x = ~revenue, y = ~franchise_name, type = "bar", orientation = "h") |>
      layout(xaxis = list(title = "Revenue ($)"), yaxis = list(title = "Franchise"))
  })

  output$chart_continent <- renderPlotly({
    df <- filtered_data()
    if (is.null(df)) return(plot_ly() |> layout(title = "Loading..."))
    agg <- df |>
      group_by(continent) |>
      summarise(revenue = sum(totalprice), .groups = "drop")
    plot_ly(agg, labels = ~continent, values = ~revenue, type = "pie") |>
      layout(showlegend = TRUE)
  })

  output$chart_products <- renderPlotly({
    df <- filtered_data()
    if (is.null(df)) return(plot_ly() |> layout(title = "Loading..."))
    agg <- df |>
      group_by(product) |>
      summarise(revenue = sum(totalprice), .groups = "drop") |>
      arrange(desc(revenue)) |>
      head(15)
    agg$product <- factor(agg$product, levels = rev(agg$product))
    plot_ly(agg, x = ~product, y = ~revenue, type = "bar") |>
      layout(xaxis = list(title = "Product"), yaxis = list(title = "Revenue ($)"))
  })

  output$chart_trend <- renderPlotly({
    df <- filtered_data()
    if (is.null(df)) return(plot_ly() |> layout(title = "Loading..."))
    agg <- df |>
      group_by(month) |>
      summarise(revenue = sum(totalprice), .groups = "drop") |>
      arrange(month)
    plot_ly(agg, x = ~month, y = ~revenue, type = "scatter", mode = "lines+markers") |>
      layout(xaxis = list(title = "Month"), yaxis = list(title = "Revenue ($)"))
  })

  output$sales_table <- renderDT({
    df <- filtered_data()
    if (is.null(df)) return(datatable(data.frame()))
    display <- df |>
      select(datetime, franchise_name, product, quantity, totalprice, continent, country) |>
      mutate(datetime = format(datetime, "%Y-%m-%d %H:%M"))
    names(display) <- c("Date", "Franchise", "Product", "Qty", "Total", "Continent", "Country")
    datatable(display, filter = "top", options = list(pageLength = 15))
  })
}

shinyApp(ui, server)
