library(shiny)
library(bslib)
library(readxl)
library(DT)
library(dplyr)
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
        helpText("Please upload an Excel file (.xls or .xlsx)")
      ),
      card(
        card_header ( 'A glance at the uploaded dataset'),
        card_body(
          uiOutput('orig_data')
        )
      )
      )
    ),
    nav_panel('Drilldown',
      layout_sidebar(
        sidebar = list(
          card(
            card_header('Select a subset of sites'),
            card_body(checkboxGroupInput(
        'sites', # input reference name
        label = '',
        choices = NULL, # gets filled when upload a dataset
        selected = NULL,
        inline = TRUE)
            ) # end card body
          ), #end card
      card(
        card_header('Year'),
        card_body(
          checkboxGroupInput('year',
                   label = '',
                   selected = NULL, 
                   choices = list('Baseline'='0','Year 1'='1'),
                   inline = TRUE)
                   ) #end card body
          ), # end card
          varSelectizeInput('selected_vars1',label = 'Select variables to view',data = NULL,multiple = TRUE)
      ),
    card(
      card_header('Filtered data shown here'),
      card_body(uiOutput('filtered_data'))
    )
    )
    )
)


server <- function(input, output,session) {
  # Reactive expression to read the Excel file
  data <- reactive({
    req(input$file)
    
    # Read the file
    tryCatch(
      {
        ext <- tools::file_ext(input$file$name)
        switch(ext,
               RDS = readRDS(input$file$datapath),
               xlsx = read_excel(input$file$datapath),
               validate("Invalid file; Please upload a .xlsx or .rds file")
        )
    },
      error = function(e) {
        stop("Error reading the file. Please make sure it's a valid Excel or RDS file.")
      }
    )
  })
  observeEvent(data(),{
    req(input$file)
    freezeReactiveValue(input,'sites')
    choices <- unique(data()$site)
    updateCheckboxGroupInput(session,
                             'sites',
                             choices = choices, 
                             selected = choices)

  })
  filtering_variables <- reactive({
    req(input$file)
    setdiff(names(data()),c('site','year'))
  })
  
  # observeEvent(filtering_variables(),{
  #   updateSelectizeInput(session = session,
  #                     'selected_vars1',
  #                     choices = filtering_variables())
  # })
  
  output$orig_data <- renderUI({
    data()|>
      filter(row_number() < 6)|>
      DT::datatable(rownames=FALSE,
                    options = list(scrollX = TRUE, 
                                   scrollY = TRUE,
                                   pageLength = 10, 
                                   dom = 'ti',
                      columnDefs = list(
                                        list(className = 'dt-center', 
                                             targets = '_all')
                                                  )),
                    filter = list(position = 'top', clear = FALSE)
      )
  })
   
  observeEvent(input$file,{
     updateVarSelectizeInput(session = session,
                         'selected_vars1',
                         data = data(),
                         selected = names(data())[1:5]
                         )
  })


  filtered_data <- reactive({
   tmp <- data() |>
      select(site,!!!input$selected_vars1)|>
      filter(site %in% input$sites)
   if(nrow(tmp) > 1000) {
     tmp <- tmp[sample(nrow(tmp),size = 500),]
     }
    
   tmp
  })

  output$filtered_data <- renderUI({
      req(filtered_data())
      filtered_data()|>
         DT::datatable(rownames=FALSE,
                     options = list(scrollX = TRUE,
                                    scrollY = TRUE,
                                    pageLength = 5,
                                    lengthMenu = c(5,10,25,50),
                                    dom = 'ltip'
                                    #columnDefs = list(
                                     # list(className = 'dt-center',
                                      #     targets = '_all')
                                    ),
                     filter = list(position = 'top', clear = FALSE)
       )

  })
 }
shinyApp(ui,server)