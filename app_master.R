# app_master.R

# install.packages("reactable")
# devtools::install_github("michaelbgarcia/shinydominoes", ref = "main")

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

  # reac_timer = reactiveValues(tick = 0,
  #                             initialize_app = 0,
  #                             start_app = 0,
  #                             stop_app = 0)


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
      refresh_project_tbl()
    })
  })
  
  refresh_project_tbl = function() {
    reac_data$project_tbl = projects_get(api_key, host) %>%
      content() %>% {
        tibble(
          id = map(., pluck("id")) %>% flatten_chr,
          name = map(., pluck("name")) %>% flatten_chr,
          app_status = as.character(NA),
          app_url= as.character(NA),
          time_left = as.integer(NA)
        )
      } %>%
      bind_rows() %>%
      filter(grepl(paste(project_this,"childproject",sep = "_"),name)) %>%
      {
        if(nrow(.) > 0) {
          rowwise(.) %>%
            mutate(app_status = app_get_details(id,api_key, host)$status,
                   app_url = paste0(host,url_view_app(app_id = app_get_details(id,api_key, host)$id)),
                   time_left = 
                     projects_get_details(id, api_key, host) %>%
                     content() %>%
                     pluck("description") %>%
                     gsub(pattern = "Project expires: ", replacement = "", x = .) %>%
                     as.POSIXct(.) %>%
                     difftime(.,Sys.time(), units="secs") %>%
                     .POSIXct(xx = .) %>%
                     format(., "%H:%M:%S")
            ) %>%
            ungroup()
        } else {
          .
        }
      }
    
    updateReactable("table", data = reac_data$project_tbl, selected = reac_table$rows_selected)
    # isolate({
    #   reac_table$updated = reac_table$updated + 1
    # })
  }

  observeEvent(input$start_app, priority = 5, {
    withProgress(message = 'Starting a new application instance', value = 1, {

      # Hash for project & app
      hash = as.integer(Sys.time())

      # Create new project name
      project_name_new = project_create_name(project_name = project_this, hash = hash)

      # Copy Project
      project_id_new = project_copy(owner_id = owner_id,
                                    project_id = project_this_id,
                                    project_name_new = project_name_new,
                                    api_key = api_key, host = host) %>%
        content() %>% pluck("id")

      # #Upload app.sh
      project_file_upload_shell(owner_name, project_name_new, api_key, host)

      # Create new app name
      app_name_new = app_create_name(project_name_new = project_name_new)

      # Create app
      app_id_new = app_create(app_name = app_name_new,
                              project_id = project_id_new,
                              api_key = api_key, host = host) %>%
        content() %>% pluck("id")

      # Start app
      app_start(app_id = app_id_new, api_key = api_key, host = host)

      # Schedule project deletion
      project_file_upload_delete(owner_name, project_name_new, api_key, host)
      schedulerun_create(owner_name, project_name_new, project_id_new, input$hrs_expire, api_key, host)

      #reac_timer$start_app = reac_timer$start_app + 1
      
      # Clear table rows
      reac_table$rows_selected = NA
      
      # Refresh tbl
      refresh_project_tbl()
      
      

    })

  })

  
  observe(priority = -99,{
    reac_table$rows_selected = getReactableState("table", "selected")
    shinyjs::toggleState("project_delete", isTruthy(reac_table$rows_selected))
  })
  
  
  observe({
    shinyjs::toggleState("project_delete", isTruthy(reac_table$rows_selected))
  })
  
  # observeEvent(reac_table$updated, {
  #   updateReactable("table", data = reac_data$project_tbl, selected = reac_table$rows_selected)
  # })

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
      refresh_project_tbl()
      
    })

  })

}

shinyApp(ui, server)
