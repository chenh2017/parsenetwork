#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  # header =========================================
  header <- shinydashboardPlus::dashboardHeader(
    title = tags$p(img(src="www/parse-logo.png", width = "30px"), span("NLP & Codified Network")),
    leftUi = headerUI("btn"),
    titleWidth = "350px", controlbarIcon = shiny::icon("cogs"))
  
  # sidebar =========================================
  sidebar <- dashboardSidebar(
    sidebarUI("side"),
    collapsed = TRUE,
    width = "350px",
    minified = FALSE
  )
  
  # body ============================================
  body <- shinydashboard::dashboardBody(
    windowSizeUI("win"),
    # useSweetAlert(),
    rintrojs::introjsUI(),
    # uiOutput("winsize")
    # textOutput("centernodes"),
    uiOutput("ui_network"),
    # bs NodeInfo =====================================
    bsModal(
      id = "selectednode", title = "Clicked node info:", trigger = FALSE,
      size = "large",
      uiOutput("clicked_node_title"),
      fluidRow(
        column(6,
               htmlOutput("clicked_node_info")
        ),
        column(6,
               sliderInput(
                 inputId = "cutoff_ind", 
                 label = "Range of cosine similarity:",
                 min = 0.1, value = c(0.2,1), max = 1,
                 step = 0.01
               )
        )
      ),
      br(),
      tabsetPanel(id = "tabs_nodeinfo",
                  tabPanel(title = "Table of connected nodes",
                           # tbClickedUI("tb1")
                           uiOutput("clicked_node_table")
                  ),
                  tabPanel(title = "Sunburst plot",
                           br(),
                           div(uiOutput("ui_sun"),align="center")
                  ),
                  tabPanel(title = "Circular plot",
                           br(),
                           uiOutput("circularplot")
                  ),
                  tabPanel(title = "More details",
                           br(),
                           uiOutput("ui_details")
                  )
      )
    )
  )
  
  # controlbar =========================================
  controlbar <- shinydashboardPlus::dashboardControlbar(
    width = 450,
    id = "controlbar",
    skin = "light",
    shinydashboardPlus::controlbarMenu(
      id = "controlbarMenu",
      shinydashboardPlus::controlbarItem(
        # "Threshold",
        "Filter Nodes",
        uiOutput("ui_filter")
        # h4("Range of cosine similarity"),
        # sliderInput(
        #   inputId = "filter_cos",
        #   label = "Filter nodes by cosine similarity",
        #   min = 0.1, max = 1,
        #   value = c(0.1, 1),
        #   step = 0.01
        # ),
        # hr(),
        # tags$b(h4("Categories", aligh = "center")),
        # shinyWidgets::pickerInput(
        #   inputId = "filter_category",
        #   label = "Filter nodes by category:",
        #   choices = NULL,
        #   selected = NULL,
        #   options = list(
        #     `actions-box` = TRUE),
        #   multiple = TRUE
        # )
      ),
      shinydashboardPlus::controlbarItem(
        "Network",
        checkboxInput("hide_labels", "Hide the labels", value = FALSE),
        h4(tags$b("customize color")),
        div(uiOutput("ui_color"), style="height: 450px; overflow-y: scroll;"),
        h4(tags$b("customize shape")),
        uiOutput("ui_shape")
      )
    )
  )
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 

    shinydashboardPlus::dashboardPage(
      header,
      sidebar,
      body,
      controlbar,
      title = NULL
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'parsenetwork'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

