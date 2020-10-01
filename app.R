BEING_SOURCED_FROM_SOMEWHERE = T
source("global.R")
source("./R/scoper.R")


box_styling = ".box.box-solid.box-primary>.box-header {
color:#fff;background:#222d32}
.box.box-solid.box-primary{
border-bottom-color:#222d32; border-left-color:#222d32; 
border-right-color:#222d32;border-top-color:#222d32;
background:#222d32}"


# box_tag = 

# Define UI for data upload app ----


ui <- dashboardPagePlus(
    
    header = dashboardHeaderPlus(title = span(#img(src = "dig_logo.png", height = "60%"), 
        "ST Scoping Tool",
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
                )))),
    sidebar = dashboardSidebar(width = 280,
                               sidebarMenu(tags$style(HTML(box_styling)),
                                           id = "tabs",
                                           menuItem("ScopeDashboard", tabName = "scope_db", 
                                                    icon = icon("toolbox"))
                                           ,
                                           conditionalPanel(condition = "input.tabs == 'scope_db'",
                                                            boxPlus(collapsed = T, closable = F,
                                                                    title = "Phase Select\n(All that apply)", status = "primary",
                                                                    solidHeader = T, collapsible = T,
                                                                    prettyCheckboxGroup(
                                                                        inputId = "phase_check",
                                                                        label = "Relevant Design Phases",
                                                                        thick = TRUE,
                                                                        choices = unique(phase$input_index),
                                                                        animation = "pulse",
                                                                        status = "info")
                                                            # ),
                                           #                  boxPlus(collapsed = T, closable = F,
                                           #                          title = "Filters", status = "primary", 
                                           #                          solidHeader = T, collapsible = T,
                                           #                          prettyCheckboxGroup(
                                           #                              inputId = "checkgroup2",
                                           #                              label = "Relevant Design Phases",
                                           #                              thick = TRUE,
                                           #                              choices = c("yolo", "hella"),
                                           #                              animation = "pulse",
                                           #                              status = "info")
                                                            )
                                           )
                                           ,
                                           menuItem("DataCenter", tabName = "data_center", icon = icon("table"))
                               )
    ),
    
    body = dashboardBody(
        # shinyDashboardThemes(
        #     theme = "grey_dark"
        # ), 
        tabItems(
            tabItem(tabName = "scope_db", 
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
                        infoBox(width = 3, "Workflow Milestones", downloadBttn(
                                    outputId = "downloadData", style = "bordered",
                                    color = "primary"
                                ), icon = icon("download",),
                        )
                    ),
                    fluidRow(column(width = 6,
                           boxPlus(closable = F,
                                   dataTableOutput("phases")), 
                           boxPlus(closable = F)
                           ), 
                    column(width = 6))
                    
            ),
            tabItem(tabName = "data_center", 
                    infoBox(width = 3,
                            "Number of Processes", textOutput("process_count"), icon = icon("th-list"),
                            color = "blue", fill=TRUE
                    ))
        )

    )
)

server <- function(input, output, session) {
    
    output$phases = renderDataTable({
        phase %>%  
            datatable(escape = F)
        
    })
    
    output$deliverables_potetnial = renderDataTable({
        # observe({
        index = phase %>% .[input_index %in% input$phase_check, Milestone]
        print(index)
        deliverables %>% .[Milestone %in% index] %>%  
            select(-Milestone_Percent) %>%  
            datatable()
        
    })
    
    # output$del_main = renderDataTable({
    #     del_main %>% 
    #         .[,.(Title, Description)] %>% 
    #         mutate(Description = str_glue('<a href="#" onclick="alert(\'{Description}\');">Click for Description</a>')) %>%
    #         # .[,`:=`(Description_nu = str_glue('<a href="#" onclick="alert({Description});">Click for Description</a>'))] %>%
    #         datatable(options = list(pageLength = 900, scrollY = "600px"), escape = F)
    # })
    # 
    # output$del_del = renderDataTable({
    #     del_deliverable %>% 
    #         .[,.(class, Title, Description)] %>% 
    #         datatable(options = list(pageLength = 900, scrollY = "600px"))
    # })
    # 
    # output$del_mile = renderDataTable({
    #     del_milestone %>% 
    #         .[,.(class, Title, Description)] %>% 
    #         datatable(options = list(pageLength = 900, scrollY = "600px"))
    # })
    # 
    # index_codes = reactive({
    #     del_group[Title %in% input$groups, Code]
    # })
    # 
    # output$process_count =  renderText({
    #     index_codes() %>% 
    #         length()
    # })
    # 
    # output$deliverable_table =  renderDataTable({
    #     del_deliverable[gsub('.{3}$', '', Code)  %in% index_codes()] %>% 
    #         .[, .(Code, Title)] %>% 
    #         datatable(options = list(pageLength = 900, scrollY = "250px"))
    # })
    # 
    # output$deliverable_count =  renderText({
    #     del_deliverable[gsub('.{3}$', '', Code)  %in% index_codes()] %>% 
    #         nrow()
    # })
    # 
    # output$milestone_table =  renderDataTable({
    #     del_milestone[gsub('.{3}$', '', Code)  %in% index_codes()] %>% 
    #         .[, .(Code, Title)] %>% 
    #         datatable(options = list(pageLength = 900, scrollY = "250px"))
    # })
    # 
    # output$milestone_count =  renderText({
    #     del_milestone[gsub('.{3}$', '', Code)  %in% index_codes()] %>% 
    #         nrow()
    # })
    # 
    # output$variables <- renderUI({
    #     index_codes = del_main[Title %in% input$groups, Code]
    #     
    #     lapply(index_codes, function(x) {
    #         list(selectInput(paste0("dynamic", x), x,
    #                          choices = del_group[class %in% index_codes, Title] %>% 
    #                              as.factor(), 
    #                          multiple = TRUE))
    #     })
    # })
    # 
    # download_process = reactive({
    #     del[gsub('.{3}$', '', Code)  %in% index_codes()]
    # })
    # 
    # output$downloadData <- downloadHandler(
    #     # data = del[gsub('.{3}$', '', Code)  %in% index_codes()],
    #     
    #     filename = function() {
    #         paste("data-", Sys.Date(), ".csv", sep="")
    #     },
    #     content = function(file) {
    #         write.csv(download_process(), file)
    #     }
    # )
    
    
}

shinyApp(ui, server)



