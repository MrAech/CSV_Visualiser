library(shiny)
library(readr)
library(plotly)

geom_fun <- function(plot, color = NULL, size = NULL) {
  if (!is.null(color)) {
    plot <- plot %>% layout(colorway = color)
  }
  
  if (!is.null(size)) {
    plot <- plot %>% add_trace(marker = list(size = size), inherit = FALSE)
  }
  
  plot
}

ui <- fluidPage(
  titlePanel("CSV Plotter"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values,text/plain',
                  '.csv')
      ),
      uiOutput("x_var_select"),
      uiOutput("y_var_select"),
      selectInput("plot_type", "Plot Type", choices = c("Scatter Plot", "Line Plot", "Bar Plot", "Histogram", "Box Plot")),
      selectInput("color", "Color", choices = c("Blue" = "blue", "Red" = "red", "Green" = "green")),
      numericInput("size", "Size", value = 10, min = 1, max = 10),
      actionButton("plotButton", "Plot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Main", verbatimTextOutput("info"), tableOutput("csv_content")),
        tabPanel("Plot", plotlyOutput("plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath)
  })
  
  # Renders the CSV content in the "Main" tab
  output$csv_content <- renderTable({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  # Generates the x and y variable select inputs
  output$x_var_select <- renderUI({
    req(dataset())
    selectInput("x_var", "X Variable", choices = names(dataset()))
  })
  
  output$y_var_select <- renderUI({
    req(dataset())
    selectInput("y_var", "Y Variable", choices = names(dataset()))
  })
  
  # Observes the plot button click 
  observeEvent(input$plotButton, {
    req(input$x_var, input$y_var)
    
    # Parse customization inputs
    color <- input$color
    size <- input$size
    
    # Generates the plot using plotly
    output$plot <- renderPlotly({
      data <- dataset()
      plot_type <- input$plot_type
      
      if (plot_type == "Scatter Plot") {
        p <- plot_ly(data, x = ~get(input$x_var), y = ~get(input$y_var), type = "scatter", mode = 'markers')
        
        # Customizes marker attributes
        p <- geom_fun(p, color = color, size = size)
        
      } else if (plot_type == "Line Plot") {
        p <- plot_ly(data, x = ~get(input$x_var), y = ~get(input$y_var), type = 'scatter', mode = 'lines')
      } else if (plot_type == "Bar Plot") {
        p <- plot_ly(data, x = ~get(input$x_var), y = ~get(input$y_var), type = 'bar')
      } else if (plot_type == "Histogram") {
        p <- plot_ly(data, x = ~get(input$x_var), type = 'histogram')
      } else if (plot_type == "Box Plot") {
        p <- plot_ly(data, y = ~get(input$y_var), type = 'box')
      }
      
      p <- p %>%
        layout(title = paste(plot_type, "of Selected Variables"),
               xaxis = list(title = input$x_var),
               yaxis = list(title = input$y_var))
      
      p
    })
    
    output$info <- renderText("Plot is displayed in the 'Plot' tab.")
  })
}

shinyApp(ui = ui, server = server)
