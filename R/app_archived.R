# BEING_SOURCED_FROM_SOMEWHERE = T
source("global.R")
source("./R/scoper.R")

library(dashboardthemes)


# Define UI for data upload app ----
library(shinydashboard)

ui <- dashboardPagePlus(
    
    header = dashboardHeaderPlus(title = span(img(src = "dig_logo.png", height = "60%"), 
                                              "GeocodR Dashboard",
                                              style="font-size:30px"),
                                 
                                 
                                 left_menu = tagList(
                                     dropdownBlock(
                                         id = "mydropdown",
                                         title = "Dropdown 1",
                                         icon = icon("info-circle"),
                                         sliderInput(
                                             inputId = "n",
                                             label = "Number of observations",
                                             min = 10, max = 100, value = 30
                                         )))
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
                    menuItem("Scope Filters", tabName = "map_dahsboard", icon = icon("map"), 
                             startExpanded = F,
                             br(),
                             boxPlus(collapsed = T,
                                     title = "Census Filters", 
                                     closable = FALSE, 
                                     width = 40,
                                     status = "primary", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     selectInput(inputId = 'groups', label = 'Choose Deliverables', 
                                                 listter,
                                                 multiple = T),
                                     uiOutput("variables")),
                             boxPlus(collapsed = T,
                                     title = "Geocode Filters", 
                                     closable = FALSE, 
                                     width = "60",
                                     status = "primary", 
                                     solidHeader = TRUE, 
                                     collapsible = TRUE,
                                     selectInput("geocode_method", "Geocode Method:",
                                                 c("With Addresses",
                                                   "With Coordinates")), 
                                     sliderInput("aa", "US Pop. HHincome:",
                                                 min = 0, max = 1000,
                                                 value = 500), 
                                     uiOutput("variables")),
                             
                             menuSubItem('Something Here', tabName = "ci_imports", icon = icon('import', lib = 'glyphicon'))
                    ),
                    menuItem("Data Center", tabName = "data_center", icon = icon("table")),
                    tags$hr(style="border-color: purple;")
                    
        )
    ),
    # sidebar = dashboardSidebar(
    #     # selectInput(inputId = 'groups', label = 'Choose variables', 
    #     #             choices = del_main$Title, 
    #     #             multiple = TRUE, selectize = TRUE),
    #     selectInput(inputId = 'groups', label = 'Choose Deliverables', 
    #                 listter,
    #                 multiple = T),
    #     uiOutput("variables")
    # ),
    dashboardBody(
        shinyDashboardThemes(
            theme = "grey_dark"
        ),
        # Boxes need to be put in a row (or column)
        
        fluidRow(
            infoBox(width = 3,
                    "Number of Processes", textOutput("process_count"), icon = icon("th-list"),
                    color = "blue", fill=TRUE
            ), 
            infoBox(width = 3,
                    "Workflow Deliverables", textOutput("deliverable_count"), icon = icon("tasks"),
                    color = "blue", fill=TRUE
            ), 
            infoBox(width = 3,
                    "Workflow Milestones", textOutput("milestone_count"), icon = icon("alarm", lib = "glyphicon"),
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
        fluidRow(
            tabBox(
                tabPanel("Process List", dataTableOutput("del_main")),
                tabPanel("Deliverable List",dataTableOutput("del_del")),
                tabPanel("Milestone List",dataTableOutput("del_mile"))),
            box(status = "primary",
                title = "Selections",
                background = "light-blue",
                
                box(width = 6, 
                    title = "Deliverables", 
                    dataTableOutput("deliverable_table")),
                box(width = 6,
                    title = "Milestones",
                    dataTableOutput("milestone_table")))
        )
    )
)
# data.table(name = "Fred", age = 50, anniversary = as.Date("1991-10-12")) %>%  
#     mutate(nu = str_glue("My name is {name}, my age next year is {age + 1}, "))
# 
# del_main[2,] %>% 
#     .[,.(Title, Description)] %>% 
#     .[,`:=`(Description_nu = str_glue('<a href="#" onclick="alert({Description});">Click for Description</a>'))]
# 
# # single braces can be inserted by doubling them
# str_glue("My name is {name}, not {{name}}.")
# 
# # You can also used named arguments
# str_glue(
#     "My name is {name}, ",
#     "and my age next year is {age + 1}.",
#     name = "Joe",
#     age = 40
# )
# 
# # `str_glue_data()` is useful in data pipelines
# mtcars %>% str_glue_data("{rownames(.)} has {hp} hp")






server <- function(input, output) {
    output$del_main = renderDataTable({
        del_main %>% 
            .[,.(Title, Description)] %>% 
            mutate(Description_nu = str_glue('<a href="#" onclick="alert(\'{Description}\');">Click for Description</a>')) %>%
            # .[,`:=`(Description_nu = str_glue('<a href="#" onclick="alert({Description});">Click for Description</a>'))] %>%
            datatable(options = list(pageLength = 900, scrollY = "600px"), escape = F)
    })
    
    output$del_del = renderDataTable({
        del_deliverable %>% 
            .[,.(class, Title, Description)] %>% 
            datatable(options = list(pageLength = 900, scrollY = "600px"))
    })
    
    output$del_mile = renderDataTable({
        del_milestone %>% 
            .[,.(class, Title, Description)] %>% 
            datatable(options = list(pageLength = 900, scrollY = "600px"))
    })
    
    index_codes = reactive({
        del_group[Title %in% input$groups, Code]
    })
    
    output$process_count =  renderText({
        index_codes() %>% 
            length()
    })
    
    output$deliverable_table =  renderDataTable({
        del_deliverable[gsub('.{3}$', '', Code)  %in% index_codes()] %>% 
            .[, .(Code, Title)] %>% 
            datatable(options = list(pageLength = 900, scrollY = "250px"))
    })
    
    output$deliverable_count =  renderText({
        del_deliverable[gsub('.{3}$', '', Code)  %in% index_codes()] %>% 
            nrow()
    })
    
    output$milestone_table =  renderDataTable({
        del_milestone[gsub('.{3}$', '', Code)  %in% index_codes()] %>% 
            .[, .(Code, Title)] %>% 
            datatable(options = list(pageLength = 900, scrollY = "250px"))
    })
    
    output$milestone_count =  renderText({
        del_milestone[gsub('.{3}$', '', Code)  %in% index_codes()] %>% 
            nrow()
    })
    
    output$variables <- renderUI({
        index_codes = del_main[Title %in% input$groups, Code]
        
        lapply(index_codes, function(x) {
            list(selectInput(paste0("dynamic", x), x,
                             choices = del_group[class %in% index_codes, Title] %>% 
                                 as.factor(), 
                             multiple = TRUE))
        })
    })
    
    download_process = reactive({
        del[gsub('.{3}$', '', Code)  %in% index_codes()]
    })
    
    output$downloadData <- downloadHandler(
        # data = del[gsub('.{3}$', '', Code)  %in% index_codes()],
        
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(download_process(), file)
        }
    )
}

shinyApp(ui, server)


