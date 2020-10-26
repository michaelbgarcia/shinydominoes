# app_master.R

install.packages("reactable")
devtools::install_github("michaelbgarcia/shinydominoes", ref = "main", upgrade = "always")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(reactable)
library(shinydominoes)

host = "https://opendatasciencelab.jnj.com"
api_key = Sys.getenv("DOMINO_USER_API_KEY")
owner_name = Sys.getenv("DOMINO_PROJECT_OWNER")
owner_id = get_owner_id(api_key, host)
project_this = Sys.getenv("DOMINO_PROJECT_NAME")
project_this_id = get_project_id(owner_name = owner_name, 
                                 project_name = project_this, 
                                 api_key = api_key, 
                                 host = host)


# Get hardware Tiers

hardware_tbl = projects_get_hardware_tbl(project_this_id, api_key, host)
hardware_list = hardware_tbl$id %>% as.list() %>% set_names(hardware_tbl$name)

ui = navbarPage(
  title = project_this,
  theme = shinytheme("flatly"),
  useShinydashboard(),
  useShinyjs(),
  setBackgroundColor(color = "ghostwhite"),
  selected = "Main",
  tabPanel("Main",
           sidebarLayout(
             sidebarPanel = sidebarPanel(
               width = 2,
               p('Start an app instance or visit an existing instance on the table to your right.'),
               br(),
               p("Apps are automatically set to expire at a given time. Set the amount of time, in hours,
                 the app should persist."),
               br(),
               br(),
               numericInput(inputId = "hrs_expire", 
                            label = "Set Project Expiration (Hrs.)",
                            value = 4,
                            min = 1,
                            max = 8,
                            step = 1),
               selectInput(inputId = "hardware_select",
                           label = "Select Hardware Tier",
                           choices = hardware_list,
                           selected = "medium-k8s"),
               hr(),
               actionButton(inputId = "start_app",
                            label = "Start a New App Instance",
                            style = "color:white",
                            class = "btn-primary")
             ),
             mainPanel = mainPanel(
               shinydashboard::box(title = "Existing Applications", width = 12,
                                   disabled(
                                     actionButton(inputId = "project_delete",
                                                  label = "Delete App Instance",
                                                  style = "color:white",
                                                  class = "btn-danger")
                                   ),
                                   
                                   br(),
                                   br(),
                                   reactableOutput("table")
               )
             )
           )
           
  )
)


server <- function(input, output, session) {
  
  reac_data = reactiveValues(app_url_view = NULL,
                             project_tbl = NULL)
  reac_table = reactiveValues(rows_selected = NA,
                              updated = 0)
  
  # Initialize reactable
  output$table <- renderReactable({
    # req(reac_data$project_tbl)
    # reactable_project_tbl(reac_data$project_tbl, reac_table$rows_selected)
    # Initialize tibble
    my_tbl = tibble(id = character(0), name = character(0),
                    app_status = character(0), app_url = character(0), time_left = character(0))
    reactable(my_tbl,
              selection = "multiple",
              paginationType = "simple",
              #defaultSelected = reac_table$rows_selected,
              columns = list(
                id = colDef(name = "Project ID"),
                name = colDef(name = "Project Name"),
                app_status = colDef(name = "Status"),
                app_url = colDef(
                  name = "App",
                  cell = function(value) {
                    url = value
                    tags$a(href = url, target = "_blank", "View app")
                  }
                ),
                time_left = colDef(name = "Time Remaining")
              )
    )
  })
  
  observe({
    invalidateLater(1000)
    isolate({
      # Refresh tbl
      reac_data$project_tbl = refresh_main_tbl(project_name = project_this, api_key = api_key, host = host)
      updateReactable("table", data = reac_data$project_tbl, selected = reac_table$rows_selected)
    })
  })
  

observeEvent(input$start_app, priority = 5, {
  withProgress(message = 'Starting a new application instance', value = 1, {
    
    app_init(project_name = project_this, 
             project_id = project_this_id, 
             owner_name = owner_name, 
             owner_id = owner_id, 
             hardware_tier_id = input$hardware_select, 
             hrs_expire = input$hrs_expire,
             api_key = api_key, 
             host = host)
    
    # Clear table rows
    reac_table$rows_selected = NA
    
    # Refresh tbl
    reac_data$project_tbl = refresh_main_tbl(project_name = project_this, api_key = api_key, host = host)
    updateReactable("table", data = reac_data$project_tbl, selected = reac_table$rows_selected)

  })
  
})


observe(priority = -99,{
  reac_table$rows_selected = getReactableState("table", "selected")
  shinyjs::toggleState("project_delete", isTruthy(reac_table$rows_selected))
})


observe({
  shinyjs::toggleState("project_delete", isTruthy(reac_table$rows_selected))
})

observeEvent(input$project_delete, priority = 5, {
  
  req(reac_table$rows_selected)
  
  withProgress(message = 'Deleting selected apps', value = 1, {
    
    table_selected_names = reac_data$project_tbl %>%
      slice(reac_table$rows_selected) %>%
      pull(name)
    
    table_selected_names %>% map(~project_delete(owner_name = owner_name,
                                                 project_name = .,
                                                 api_key = api_key,
                                                 host = host))
    
    #reac_timer$start_app = reac_timer$start_app + 1
    # Clear table rows
    reac_table$rows_selected = NA
    
    # Refresh tbl
    reac_data$project_tbl = refresh_main_tbl(project_name = project_this, api_key = api_key, host = host)
    updateReactable("table", data = reac_data$project_tbl, selected = reac_table$rows_selected)
    
  })
  
})

}

shinyApp(ui, server)
