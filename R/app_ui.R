#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  # header ==========================================
  header <- shinydashboardPlus::dashboardHeader(
    title = img(src="www/parse-logo.png", width = "30px"),
    leftUi = tagList(
      h4("PARSE Network", class = "title_text"),
      downloadButton("downloadData",
                     " Download",
                     icon = icon("download"),
                     class = "btn btn-primary header-button",
                     width = "100px",
                     style = "padding: 6px;",
                     title = "The cosine similarity of current network."
      ),
      bookmarkButton(
        label = "Bookmark", id = "bookmark",
        class = "btn btn-primary header-button"
      ),
      actionButton("instruct", " About",
                   icon = icon("book"),
                   class = "btn btn-primary header-button",
                   width = "100px",
                   style = "padding: 6px 20px 6px 20px;",
                   title = "The introduction of the app."
      ),
      actionButton("help", " Help",
                   icon = icon("question"),
                   class = "btn btn-primary header-button",
                   width = "100px",
                   style = "padding: 6px 20px 6px 20px;",
                   title = "The introduction tour."
      )
    ),
    titleWidth = "50px")
  # sidebar =========================================
  sidebar <- shinydashboardPlus::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Data", tabName = "Data", icon = icon("table")),
      shinydashboard::menuItem("Network", tabName = "Network", icon = icon("share-alt"),
               selected=TRUE),
      id = "sidebarmenu"
    ),
    collapsed = FALSE,
    width = "120px",
    minified = TRUE
  )
  
  # body ============================================
  body <- shinydashboard::dashboardBody(
    windowSizeUI("win"),
    # includeCSS("www/style.css"),
    # useSweetAlert(),
    # shinybrowser::detect(),
    
    rintrojs::introjsUI(),
    
    # bs NodeInfo =====================================
    shinyBS::bsModal(
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
      # hr(),
      tabsetPanel(id = "tabs_nodeinfo",
                  tabPanel(title = "Sunburst plot",
                           br(),
                           # fluidRow(column(6,
                           #                 sliderTextInput("changeline","max Text length on each line (set as 99 if not breaking lines:)",
                           #                                 choices = c(5,10,15,20,25,99),selected = 10,grid=TRUE,width = "100%"),
                           #                 pickerInput(
                           #                   inputId = "rotatelabel",
                           #                   label = "The orientation of text inside sectors",
                           #                   choices = c("radial", "tangential")
                           #                 )
                           # ),
                           # column(6,sliderInput("scale_sungh","Graph height:", 
                           #                      min=500,max=1000,value=750,width = "100%"))
                           # ),
                           
                           div(uiOutput("ui_sun"),align="center")
                  ),
                  tabPanel(title = "Circular plot",
                           br(),
                           uiOutput("circularplot")
                  ),
                  tabPanel(title = "Table of connected nodes",
                           uiOutput("clicked_node_table")
                  ),
                  tabPanel(title = "More details",
                           br(),
                           uiOutput("ui_details")
                  )
      )
    ),
    shinyBS::bsModal(
      id = "instruction", title = "Instruction", trigger = "instruct",
      size = "large",
      includeMarkdown(app_sys("app/doc/documentation.md"))
    ),
    
    # content ============================================
    shinydashboard::tabItems(
      # Tab data  ==============================================================
      shinydashboard::tabItem(tabName = "Data",
              box(id = "input_tb",
                  title = "Input box",
                  width = 5,
                  fluidRow(
                    column(width = 3,
                           div(
                             shinyWidgets::prettyToggle(
                               inputId = "btn_fixed",
                               label_on = "Precise match", 
                               label_off = "Fuzzy match",
                               value = TRUE,
                               status_off = "primary"
                             ), align = "center", style = "margin-top: 8px;"
                           )),
                    conditionalPanel("input.btn_fixed == 1",
                                     column(width = 9,
                                            selectizeInput(inputId = "searchbox1", 
                                                           label = NULL,
                                                           choices = NULL, 
                                                           selected = NULL, 
                                                           multiple = TRUE,
                                                           
                                                           options = list(
                                                             create = FALSE,
                                                             placeholder = 'cancer',
                                                             maxItems = 3,
                                                             onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                             onType = I("function (str) {if (str === '') {this.close();}}")
                                                           )))
                    ),
                    conditionalPanel("input.btn_fixed == 0",
                                     column(width = 9,
                                            shinyWidgets::searchInput(
                                              inputId = "searchbox2",
                                              placeholder = "cancer",
                                              value = NULL,
                                              btnSearch = icon("search"),
                                              width = "100%"
                                            )
                                     ))),
                  
                  
                  uiOutput("ui_input"),
                  hr(),
                  checkboxGroupInput("inCheckboxGroup1", "0 NLP node(s) Selected:"),
                  checkboxGroupInput("inCheckboxGroup2", "0 Codified node(s) Selected:"),
                  fluidRow(
                    column(6,
                           div(
                             actionButton(
                               inputId = "deselect",
                               label = "Deselect", 
                               icon = icon("undo"),
                               color = "lightgrey"
                             ), align = "center")),
                    column(6,
                           div(
                             actionButton(
                               inputId = "gobutton",
                               label = "Submit", 
                               icon = icon("check"),
                               color = "green"
                             ), align = "center"))
                  )
              ),
              conditionalPanel(condition = "(!is.null(input.inCheckboxGroup1)) | (!is.null(input.inCheckboxGroup2))",
                               
                               box(id = "output_tb",
                                   title = "Output box",
                                   width = 7,
                                   uiOutput("out_table"))
              )
      ),
      # Tab network ============================================================
      shinydashboard::tabItem(tabName = "Network",
              uiOutput("network")
              
      )
    )
  )
  
  # controlbar =========================================
  controlbar <- shinydashboardPlus::dashboardControlbar(width = 300,
    id = "controlbar",
    shinydashboardPlus::controlbarMenu(
      id = "controlbarMenu",
      shinydashboardPlus::controlbarItem(
        # "Threshold",
        "Filter Nodes",
        h4("Range of cosine similarity"),
        uiOutput("slider_cos"),
        hr(),
        tags$b(h4("Categories", aligh = "center")),
        shinyWidgets::awesomeCheckboxGroup(
          inputId = "filter_category",
          label = "Filter nodes by category:", 
          choices = NULL,
          selected = NULL
        ),
        fluidRow(
          column(6,
                 div(
                   actionButton(
                     inputId = "btn_deselect_category",
                     label = "Deselect", 
                     icon = icon("times"),
                     color = "lightgrey"
                   ), align = "center")),
          column(6,
                 div(
                   actionButton(
                     inputId = "btn_selectall_category",
                     label = "Select All", 
                     icon = icon("check"),
                     color = "green"
                   ), align = "center"))
        )
      ),
      shinydashboardPlus::controlbarItem(
        "Network",
        checkboxInput("hide_labels", "Hide the labels", value = TRUE),
        uiOutput("ui_color")
      )
    ),
    skin = "light"
  )
  
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    
    shinydashboardPlus::dashboardPage(
      header,
      sidebar,
      body,
      controlbar = controlbar,
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

