library(shiny)
library(bslib)
library(readxl)
library(DT)
library(dplyr)
library(GWalkR)
library(shinyjs)
options(shiny.maxRequestSize = 10 * 1024^2)
ui <-  page_navbar(
    title = "Breast Cancer Data Viewer",
    id = 'nav',
    theme = bs_theme(preset = "cosmo"),
    fillable = 'Drilldown',
    nav_panel('Uploaded data',
        layout_sidebar(
      sidebar = card(
        fileInput("file", "Upload Excel or RDS File",
                  accept = c(
                    ".rds",
                    ".xls",
                    ".xlsx"
                  )
        ),
        uiOutput('xlsx_options'),
        actionButton('submit','Submit'),
        actionButton('clear','Clear'),
        helpText("Please upload an Excel file (.xls or .xlsx)")
      ),
      gwalkrOutput('interactive_panel')
    )
)
)


server <- function(input, output,session) {
  # Reactive expression to read the Excel file
  data <- reactiveVal(NULL)
  
  observeEvent(input$submit,{
    req(input$file)
    
    file <- input$file$datapath
    file_ext <- tools::file_ext(input$file$name)
    
    tryCatch({
      data(switch(file_ext,
                  "xlsx" = {
                    req(input$header_row)
                    read_excel(file, skip = as.numeric(input$header_row) - 1)
                  },
                  "RDS" = readRDS(file),
                  stop("Unsupported file type")
      ))
    }, error = function(e) {
      showNotification("Error reading the file. Please make sure it's a valid Excel or RDS file.", type = "error")
      data(NULL)
    })
  })
  
  output$xlsx_options <- renderUI({
    req(input$file)
    if(grepl('\\.xlsx$',input$file$name)){
      numericInput('header_row','Row number for column names',value = 1, min = 1)
    }
  })
  
  observeEvent(input$clear,{
    data(NULL)
    unlink(input$file$datapath)
    reset(id = '')
    updateNumericInput(session,'header_row',value = 1)
  })
  
  # observeEvent(data(),{
  #   req(input$file)
  #   freezeReactiveValue(input,'sites')
  #   choices <- unique(data()$site)
  #   updateCheckboxGroupInput(session,
  #                            'sites',
  #                            choices = choices, 
  #                            selected = choices)
  # 
  # })
  filtering_variables <- reactive({
    req(input$file)
    setdiff(names(data()),c('site','year'))
  })
  
  # observeEvent(filtering_variables(),{
  #   updateSelectizeInput(session = session,
  #                     'selected_vars1',
  #                     choices = filtering_variables())
  # })
  
  # output$orig_data <- renderUI({
  #   data()|>
  #     filter(row_number() < 6)|>
  #     DT::datatable(rownames=FALSE,
  #                   options = list(scrollX = TRUE, 
  #                                  scrollY = TRUE,
  #                                  pageLength = 10, 
  #                                  dom = 'ti',
  #                     columnDefs = list(
  #                                       list(className = 'dt-center', 
  #                                            targets = '_all')
  #                                                 )),
  #                   filter = list(position = 'top', clear = FALSE)
  #     )
  # })
   
  # observeEvent(input$file,{
  #    updateVarSelectizeInput(session = session,
  #                        'selected_vars1',
  #                        data = data(),
  #                        selected = names(data())[1:5]
  #                        )
  # })


  # filtered_data <- reactive({
  #  tmp <- data() |>
  #     select(site,!!!input$selected_vars1)|>
  #     filter(site %in% input$sites)
  #  if(nrow(tmp) > 1000) {
  #    tmp <- tmp[sample(nrow(tmp),size = 500),]
  #    }
  #   
  #  tmp
  # })
  # 
  # output$filtered_data <- renderUI({
  #     req(filtered_data())
  #     filtered_data()|>
  #        DT::datatable(rownames=FALSE,
  #                    options = list(scrollX = TRUE,
  #                                   scrollY = TRUE,
  #                                   pageLength = 5,
  #                                   lengthMenu = c(5,10,25,50),
  #                                   dom = 'ltip'
  #                                   #columnDefs = list(
  #                                    # list(className = 'dt-center',
  #                                     #     targets = '_all')
  #                                   ),
  #                    filter = list(position = 'top', clear = FALSE)
  #      )
  # 
  # })
  output$interactive_panel <- renderGwalkr({
    req(data())
    gwalkr(data())
  }
  )
 }
shinyApp(ui,server)