BEING_SOURCED_FROM_SOMEWHERE = T
source("global.R")
source("./R/scoper.R")


# Define UI for data upload app ----

ui = dashboardPagePlus(
    header = dashboardHeaderPlus(
        title = "ST Scoping Dashboard",
        fixed = TRUE,
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears",
        left_menu = tagList(
            dropdownBlock(
                id = "mydropdown",
                title = "About",
                icon = icon("sliders")
            ),
            dropdownBlock(
                id = "mydropdown2",
                title = "Help",
                icon = icon("question-circle"),
                actionButton("btn","Take Tour", icon("question-circle"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
                actionButton(inputId='ab1', label="Learn More", 
                             icon = icon("th"), 
                             onclick ="window.open('https://www.youtube.com/watch?v=H6wl-EyhXl0', '_blank')")
            )
        ),
        dropdownMenu(
            icon = icon("envelope"))
    ),
    sidebar = dashboardSidebar(
        sidebarMenu(tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#222d32
                    }
                    .box.box-solid.box-primary{
                    border-bottom-color:#222d32;
                    border-left-color:#222d32;
                    border-right-color:#222d32;
                    border-top-color:#222d32;
                    background:#222d32
                    }")),
                    menuItem("Map Dashboard", tabName = "map_dashboard", icon = icon("map"), 
                             startExpanded = T,
                             boxPlus(collapsed = T,
                                     title = "Filters", 
                                     closable = FALSE, 
                                     width = 40,
                                     status = "primary", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     prettyCheckboxGroup(
                                         inputId = "checkgroup2",
                                         label = "Click me!",
                                         thick = TRUE,
                                         choices = click_index,
                                         animation = "pulse",
                                         status = "info"
                                     ),
                                     selectInput(inputId = 'groups', label = 'Choose Deliverables', 
                                                 listter,
                                                 multiple = T),
                                     # uiOutput("variables")
                                     )),
                    menuItem("Data Center", tabName = "data_center", icon = icon("table")),
                    menuItem("Data Centerrr", tabName = "data_centerrr", icon = icon("table"))
                    
        )
    ),
    body = dashboardBody(
        # fluidRow(
        tabItems(
            tabItem("map_dashboard", 
                    mapviewOutput("mapSelected", height = 900),
                    setShadow(class = "dropdown-menu")
            ),tabItem("data_center",
                      fluidRow(
                          infoBox(width = 3,
                                  "Number of Processes", "XXXXXX", icon = icon("th-list"),
                                  color = "blue", fill=TRUE
                          ), 
                          infoBox(width = 3,
                                  "Workflow Deliverables", "Bing bang", icon = icon("tasks"),
                                  color = "blue", fill=TRUE
                          ), 
                          infoBox(width = 3,
                                  "Workflow Milestones", "Bing bong", icon = icon("alarm", lib = "glyphicon"),
                                  color = "blue", fill=TRUE
                          ),
                          infoBox(width = 3,
                                  "Workflow Milestones", downloadBttn(
                                      outputId = "downloadData",
                                      style = "bordered",
                                      color = "primary"
                                  ), icon = icon("download",),
                                  color = "blue", fill=TRUE
                          )
                      ),
                      infoBox(width = 3,
                              "Number of Processes", textOutput("text"), icon = icon("th-list"),
                              color = "blue", fill=TRUE
                      )
            ),tabItem("data_centerrr",leafletOutput("mapSelected_leaflet"))
            
        ),
        rightsidebar = rightSidebar(width = 500, 
                                    rightSidebarTabContent(
                                        id = 1,
                                        icon = "desktop",
                                        title = "Tab 1",
                                        active = TRUE,
                                        sliderInput(
                                            "obs", 
                                            "Number of observations:",
                                            min = 0, max = 1000, value = 500
                                        )
                                    ),
                                    rightSidebarTabContent(
                                        id = 2,
                                        title = "Tab 2",
                                        textInput("caption", "Caption", "Data Summary")
                                    ),
                                    rightSidebarTabContent(
                                        id = 3,
                                        title = "Tab 3",
                                        icon = "paint-brush",
                                        numericInput("obs", "Observations:", 10, min = 1, max = 100)
                                    ))
        
    )
)

#server====================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================

# Define server logic to read selected file ----
server <- function(input, output, session) {
    
    #utility======================================================================
    #=============================================================================
    #=============================================================================
    # observeEvent(input$btn,
    #              introjs(session))
    
    #shows html popup at start of application
    observeEvent("", {
        showModal(modalDialog(
            "Hello friend",
            size = "l",
            easyClose = TRUE,
        ))
    })
    #utility======================================================================
    #=============================================================================
    #=============================================================================
    output$mapSelected <- renderMapview({
        # mmap
        tmp = breweries %>%  mapview()
    })
    
    output$mapSelected_leaflet <- renderLeaflet({
        tmp = breweries %>%  mapview()
        tmp@map
    })
    
    # output$variables <- renderUI({
    #     req(input$geocode_method)
    #     if(input$geocode_method == "With Addresses"){
    #         textInput("gecocode", label, value = "", width = NULL, placeholder = NULL)
    #     } else {
    #         textInput("gecocode", "Caption:", "Data Summary")
    #     }
    # })
    
    output$text = renderText({
        input$checkgroup2 %>%  print()
        input$checkgroup2 
    })
    
    
}



# Run the app ----
shinyApp(ui, server)

