#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import rintrojs
#' @import shinydashboardPlus
#' @import visNetwork
#' @noRd
#' 
app_server <- function(db){
  server <- function(input, output, session) {
    shinyhelper::observe_helpers()
    
    ## data  ===================
    
    # db <- "/home/hui/Packages/biomedicalnet/data-raw/cui.db"
    # db <- "/home/hui/Project/parse_network/data-raw/BCH/v2_20220707/bch_v20220707.db"
    ids <- getData("ids", db)
    dict.combine <- getData("node", db)
    if(sum(!grepl("\\w", dict.combine$group, perl = TRUE)) > 0){
      dict.combine$group[!grepl("\\w", dict.combine$group, perl = TRUE)] <- "Unknown"
    }
    
    tb_input <- dplyr::left_join(ids[,"id", drop = FALSE], dict.combine[, c("id", "term")], by = "id")
    attrs <- readRDS("/home/hui/Packages/biomedicalnet/data-raw/attrs.rds")
    ColorsNet <- readRDS("/home/hui/Packages/biomedicalnet/data-raw/colors_group.rds")
    cui_sy <- getData("synonyms", db)
    node_num_cutoff = 500
    
    winsize <- windowSizeServer("win")
    # data <- data.frame(a = 1:5, b = 6:10)
    steps <- data.table::fread("/home/hui/Project/CUInetwork/doc/steps.tsv")
    headerServer("btn", steps[, -1], app_sys("app/doc/documentation.md"), df_edges_cutted())
    
    directed = FALSE
    
    # if(directed){
    #   load("~/Packages/biomedicalnet/data-raw/test_directed.RData")
    # } else {
    #   load("~/Packages/biomedicalnet/data-raw/test_bch.RData")
    # }
    
    
    # center_nodes ====
    center_nodes <- sidebarServer("side", tb_input, type = 2, 
                                  # selected = c(2, 4, 8, 12),
                                  selected = c(4, 5),
                                  # init_nodes = c("C0003873", "C0409637", "PheCode:714.1", "PheCode:714.2"),
                                  init_nodes = c("PheCode:714.1", "PheCode:714.2"),
                                  synonyms = cui_sy, server = TRUE)
    
    output$centernodes <- renderText({
      center_nodes()
    })
    
    name_input <- reactive({
      paste(gsub("[^\\w]", "_", center_nodes(), perl = TRUE), collapse = "_")
    })
    
    observeEvent(center_nodes(), {
      if(!is.null(center_nodes())){
        max_nodes <- min(length(center_nodes()) * 100, nrow(df_edges_center()))
        thr_cos <- floor(sort(df_edges_center()$cos, decreasing = TRUE)[max_nodes]*100)/100
        max = ceiling(max(df_edges_center()$cos)*100)/100
        min = floor(min(df_edges_center()$cos)*100)/100
    
        df <- df_edges_center()[df_edges_center()$cos >= thr_cos, ]
        categories <- sort(unique(df$category))
        
        if(input$controlbarMenu == "Network"){
          updateControlbarMenu(session = session, id = "controlbarMenu", selected = "Filter Nodes")
        }
        print("====min====max====value====")
        print(max)
        print(min)
        print(c(thr_cos, max))
        if(is.na(thr_cos)){
          thr_cos <- min
        }
        output$ui_filter <- renderUI({
          tagList(
            sliderInput(
              inputId = paste0(name_input(), "-filter_cos"),
              label = "Filter nodes by cosine similarity",
              min = min, max = max,
              value = c(thr_cos, max),
              step = 0.01
            ),
            hr(),
            shinyWidgets::pickerInput(
              inputId = paste0(name_input(), "-filter_category"),
              label = "Filter nodes by category:",
              choices = categories,
              selected = categories,
              options = list(
                `actions-box` = TRUE),
              multiple = TRUE
            )
          )
          })
        } else {
        output$slider_cos <- renderUI({""})
      }
    })
    
    categories <- reactive({
      req(input[[paste0(name_input(), "-filter_cos")]])
      thr_cos <- input[[paste0(name_input(), "-filter_cos")]]
      df <- df_edges_center()[df_edges_center()$cos >= thr_cos, ]
      sort(unique(df$category))
    })
    
    observeEvent(categories(), {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = paste0(name_input(), "-filter_category"),
        label = "Filter nodes by category:",
        choices = categories(),
        selected = categories(),
        options = list(
          `actions-box` = TRUE)
      )
    })
    
    df_edges_cutted <- reactive({
      req(input[[paste0(name_input(), "-filter_cos")]])
      req(input[[paste0(name_input(), "-filter_category")]])
      # print("df_edges_cutted")
      # print(nrow(df_edges_center()))
      df <- df_edges_center()[df_edges_center()$cos >= input[[paste0(name_input(), "-filter_cos")]][1], ]
      print(nrow(df))
      # print(nrow(df[df$category %in% input$filter_category, ]))
      df[df$category %in% input[[paste0(name_input(), "-filter_category")]], ]
    })
    
    df_edges_center <- reactive({
      # df <- df_edges[df_edges$from %in% center_nodes() | df_edges$to  %in% center_nodes(),]
      # df$category = dict.combine$category[match(df$to, dict.combine$id)]
      # df$category[!df$from %in% center_nodes()] = dict.combine$category[match(df$from[!df$from %in% center_nodes()], dict.combine$id)]
      # print("df_edges_center")
      # print(nrow(df))
      # 
      # 
      # 
      # df <- df[!duplicated(df),]
      # if(!directed){
      #   df$ends <- paste0(df$from, ";",df$to)
      #   df$ends <- sapply(df$ends, function(x){
      #     paste(sort(strsplit(x, ";", fixed = T)[[1]]), collapse = ";")
      #   })
      # 
      #   df <- df[!duplicated(df$ends), ]
      #   df <- df[, -5]
      # }
      # print(nrow(df))
      # df
      print("df_edges")
      req(center_nodes())
      print(center_nodes())
      df <- Reduce(rbind, lapply(center_nodes(), getCosFromDB, db))
      df$category <- dict.combine$category[match(df$to, dict.combine$id)]
      print(table(df$category))
      df
    })
    
    # observeEvent(center_nodes(), {
    #   max_nodes <- min(length(center_nodes()) * 100, nrow(df_edges_center))
    #   thr_cos <- floor(sort(df_edges_center()$cos, decreasing = TRUE)[max_nodes]*100)/100
    #   max = ceiling(max(df_edges_center()$cos)*100)/100
    #   min = floor(min(df_edges_center()$cos)*100)/100
    #   print("updateSliderInput")
    #   print(c(max, min, thr_cos, max_nodes))
    #   updateSliderInput(inputId = "filter_cos",
    #                     max = max,
    #                     min = min,
    #                     value = c(thr_cos, max))
    # })
    
    # filter category ===============================
    
    # categories <- reactive({
    #   print("categories")
    #   df <- df_edges_center()[df_edges_center()$cos >= input$filter_cos[1], ]
    #   print(nrow(df))
    #   sort(unique(df$category))
    # })
    # 
    # observe({
    #   print("updatePickerInput")
    #   print(categories())
    #   shinyWidgets::updatePickerInput(
    #     session = session,
    #     inputId = "filter_category",
    #     label = "Filter nodes by category:", 
    #     choices = categories(),
    #     selected = categories()
    #   )
    # })
    
    ## df_edges_cut ====
    # df_edges_cutted <- reactive({
    #   print("df_edges_cutted")
    #   print(nrow(df_edges_center()))
    #   df <- df_edges_center()[df_edges_center()$cos >= input$filter_cos[1], ]
    #   print(nrow(df))
    #   print(nrow(df[df$category %in% input$filter_category, ]))
    #   df[df$category %in% input$filter_category, ]
    # })
    
    
    ## network  ====================================
      output$ui_network <- renderUI({
        req(winsize()[2])
        # req(input$filter_cos)
        # req(input$filter_category)
        if(length(center_nodes()) > 0 & (nrow(df_edges_cutted()) > 0)){
          print("ui_network")
          print(nrow(df_edges_cutted()))
          print(center_nodes())
          shinycssloaders::withSpinner(
            visNetworkOutput("network",
                             height =  paste0((winsize()[2]-100),"px")),
            type = 6
          )
        } else {
          h3("Try select some rows and click on submit")
        }
      })
      
      output$network <- renderVisNetwork({
        print("********************network**************")
        # myconfirmation = input$myconfirmation
        print(center_nodes())
        if(length(center_nodes()) > 0 | (isTruthy(df_edges_cutted()))){
          req(picked_colors())
          plot_network(center_nodes(), df_edges_cutted(), 
                       dict.combine, attrs, picked_colors(), c(input$shape1, input$shape2),
                       hide_labels = FALSE, 
                       directed = directed,
                       node_num_cutoff = 500, 
                       layout = "layout_nicely")
        }
      })
    
    
    
    
    
    selected_id = reactive({
      print("selected_id")
      input$current_node_id$nodes[[1]]
    })
    
    # node info ====
    observeEvent(input$current_node_id, {
      if (!is.null(selected_id())){
        toggleModal(session, "selectednode", toggle = "open")
        df <- getCosFromDB(selected_id(), db)
        v_cos = df$cos
        print("openBS_nodeinfo")
        print(length(v_cos))
        t_cos = sort(v_cos, decreasing = TRUE)[min(length(v_cos), 100)]
        ifelse(is.na(t_cos), min(v_cos), t_cos)
        updateSliderInput(
          inputId = "cutoff_ind",
          # label = "Filter edges by cosine similarity (above):",
          min = 0.1, value = c(t_cos,1), max = 1,
          step = 0.01
        )
      }
    })
    
    # Clicked node text
    output$clicked_node_title <- renderUI({
      h3(dict.combine$term[match(selected_id(), dict.combine$id)])
    })
    output$clicked_node_info <- renderUI({
      print("clicked_node_info")
      clickedNodeText(selected_id(), dict.combine)
    })
  
    ## df plots  ===================================
    df_plots <- reactive({
      print("df_plots")
      if (!is.null(selected_id())){
        df <- getCosFromDB(selected_id(), db)
        df[df$cos >= input$cutoff_ind[1],]
        # df_edges[df_edges$from == selected_id() | df_edges$to == selected_id(), ]
      }
    })
  
    ## sunburst =======================================
    output$ui_sun <- renderUI({
      if(nrow(df_plots()) > 0){
        shinycssloaders::withSpinner(
          plotly::plotlyOutput("sun",width="auto",
                               height="750px"), type = 6
        )} else {
          ""
        }
    })
  
    output$sun <- plotly::renderPlotly({
      print("sunburst")
      sunburstPlotly(selected_id(), df_plots(),
                     # input$changeline, input$rotatelabel, input$scale_sungh,
                     dict.combine)
    })
    
    ## circular plot  =======================================
    ColorsCirc <- data.frame("group"=c("ACTI","CHEM","PheCode","DISO",
                                       "Drug","Lab","RXNORM","LOINC",
                                       "PHEN","PHYS","PROC","CCS"),
                             "color"=c("#85FA00","#0DA7FB","#FBD51C","#FBD51C",
                                       "#D700FE","#00FDD7","#D700FE","#00FDD7",
                                       "#FC844B","#E9DBF8","#FD0D2E","#FD0D2E"))
    
    output$circularplot <- renderUI({
      if(nrow(df_plots()) > 0){
        shinycssloaders::withSpinner(
          plotOutput("circular", width = "100%",
                     height = paste0(max(700, winsize()[2] - 450),"px")), type = 6
        )} else {
          ""
        }
    })
    output$circular <- renderPlot({
      print("circular")
      node_now = selected_id()
      circularBar(df_plots(), dict.combine, ColorsCirc)
    })
    
    # table of clicked node  ================================
    # tbClickedServer("tb1", df_plots(), paste0(winsize()[2] - 400,"px"), dict.combine)
    
    output$clicked_node_table <- renderUI({
      if(nrow(df_plots()) > 0){
        shinycssloaders::withSpinner(
          reactable::reactableOutput("tb_clicked_node", width = "100%",
                                     height = paste0(winsize()[2] - 400,"px")), type = 6
        )} else {
          ""
        }
    })
    
    df_clicked_node <- reactive({
      df <- df_plots()
      colnames(df) <- c("center_nodes", "connected_nodes", "cosine_similarity")
      df <- left_join(df,
                      dict.combine[, c("id", "term", "category")],
                      by = c("connected_nodes" = "id"))
      df$cosine_similarity <- round(df$cosine_similarity, 3)
      df[order(df$cosine_similarity, decreasing = TRUE), c(1, 2, 5, 4, 3)]
    })
    
    
    output$tb_clicked_node <- reactable::renderReactable(
      reactable::reactable({ df_clicked_node() },
                           groupBy = c("center_nodes"),
                           columns = list(
                             cosine_similarity = reactable::colDef(name = "cosine similarity"),
                             connected_nodes = reactable::colDef(
                               minWidth = 250,
                               name = "connected_nodes / term",
                               # Show species under character names
                               cell = function(value, index) {
                                 term <- df_clicked_node()$term[index]
                                 term <- if (!is.na(term)) term else "Unknown"
                                 div(
                                   div(style = list(fontWeight = 600), value),
                                   div(style = list(fontSize = 12), term)
                                 )
                               }
                             ),
                             term = reactable::colDef(show = FALSE)
                           ),
                           bordered = TRUE,
                           defaultExpanded = TRUE,
                           pagination = FALSE
      )
    )
    
    
    
    ## ui details  ===================================================
    output$ui_details <- renderUI({
      outdiv <- tagList()
      tbs <- getData("details", db)
      tbs <- rbind(tbs, data.frame(tname = "synonyms", title = "Synonyms", note = "Synonyms"))
      print(tbs)
      apply(tbs, 1, function(x){
        tname = x[1]
        title = x[2]
        helps = ifelse(!is.null(x) & length(x) == 3, x[3], "")
        print(tname)
        df <- getData(tname, db)
        if(tname == "rollup"){
          colnames(df)[1] <- "id"
        }
        if(selected_id() %in% df$id){
          if(tname == "rollup"){
            sy <- cui_sy
          } else{
            sy <- NULL
          }
          outdiv <<- detailsTab(tname, df[df$id == selected_id(),], title, outdiv, selected_id(), sy, output, helps)
        }
      })
      outdiv
    })
    
    rollup <- getData("rollup", db)
    rollup_selected <- reactive({
      rollup[rollup$cui_final == selected_id(), ]
    })
    
    output$rollup_tb_details <- reactable::renderReactable(reactable::reactable({
      rollup_selected()[, -1]
    },
    bordered = TRUE,
    details = function(index) {
      if(!is.null(cui_sy)){
        cui <- rollup_selected()$cui[index]
        print("details for table")
        print(cui)
        if(cui %in% cui_sy$id){
          htmltools::div(
            "Synonyms of ", rollup_selected()$cui[index], ":",
            htmltools::tags$pre(paste(sort(cui_sy$synonyms[cui_sy$id == rollup_selected()$cui[index]]), collapse = "\n")),
            width = "300px"
          )
        }
      }
    },
    pagination = FALSE)
    )
    
    # color picker  =================================
    colors<-c("#FBD51C","#D700FE","#00FDD7","#FD0D2E","#FC844B","#E9DBF8","#0DA7FB","#85FA00",
              "#FF0DC1","#C7ED9B","#D2A27F","#FF96EB","#D82668","#45E3FD","#0D9600","#7AB9A8",
              "#B9ADFC","#FD8C9D","#63495A","#D2EF22","#AD70FB","#26FD8D","#B800B9","#8D880D",
              "#0047BB","#972A16","#D292AC","#006581","#85224D","#32511C","#6D2A95","#352EFE")
    output$ui_color <- renderUI({
      n <- 1
      colors_group <- NULL
      print("ui_color")
      print(ColorsNet$group)
      print(unique(dict.combine$group))
      lapply(sort(unique(dict.combine$group)), function(x){
        if(!x %in% ColorsNet$group){
          c <- sample(setdiff(colors, c(ColorsNet$color.background, colors_group$color.background)), 1)
          colors_group <<- rbind(colors_group, data.frame("group"=x,
                                                          "color.background"=c))
        } else {
          colors_group <<- rbind(colors_group, ColorsNet[ColorsNet$group == x,])
        }
        column(6, colorpickerUI(x, colors_group$color.background[colors_group$group == x]))
      })
    })
    
    picked_colors <- reactive({
      updateControlbarMenu("controlbarMenu", selected = "Network")
      c <- sapply(sort(unique(dict.combine$group)), colorpickerServer)
      req(c[[1]])
      print("picked_colors")
      print(c)
      data.frame("group" = names(c), "color.background" = c)
    })
    
    # shape picker ====
    output$ui_shape <- renderUI({
      tagList(column(6, 
        shinyWidgets::pickerInput(
          inputId = "shape1",
          label = attrs$attr_nodes_type$type[1], 
          choices = c("square", "triangle", "box", "circle", "dot", "star",
                      "ellipse", "database", "text", "diamond"),
          selected = attrs$attr_nodes_type$shape[1],
          choicesOpt = list(
            icon = c("fa fa-square", 
                     "fa fa-mountain", 
                     "fa fa-square-plus", 
                     "fa fa-circle-info", 
                     "fa fa-circle", 
                     "fa fa-star",
                     "fa fa-circle-question", 
                     "fa fa-database", 
                     "fa fa-font", 
                     "fa diamond")),
          options = list(
            `icon-base` = "")
        )),column(6, 
        shinyWidgets::pickerInput(
          inputId = "shape2",
          label = attrs$attr_nodes_type$type[2], 
          choices = c("square", "triangle", "box", "circle", "dot", "star",
                      "ellipse", "database", "text", "diamond"), 
          selected = attrs$attr_nodes_type$shape[2],
          choicesOpt = list(
            icon = c("fa fa-square", 
                     "fa fa-mountain", 
                     "fa fa-square-plus", 
                     "fa fa-circle-info", 
                     "fa fa-circle", 
                     "fa fa-star",
                     "fa fa-circle-question", 
                     "fa fa-database", 
                     "fa fa-font", 
                     "fa diamond")),
          options = list(
            `icon-base` = "")
        )))
    })
  
  }
return(server)
}