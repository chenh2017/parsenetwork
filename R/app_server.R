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
app_server <- function(db_path, directed){
  server <- function(input, output, session) {
    
    shinyhelper::observe_helpers()
    
    ## data  ===================
    
    db <- db_path
    ids <- getData("ids", db)
    dict.combine <- getData("node", db)
    input_table <- dplyr::left_join(ids[,"id", drop = FALSE], dict.combine[, c("id", "term", "category")], by = "id")

    node_num_cutoff = 500
    
    ## Note ================
    
    showNotification("Click 'Help' button to open step-by-step instructions.",
                     duration = 3, type = "warning"
    )
    
    observeEvent(input$help, {
      rintrojs::introjs(session,
              options = list(
                steps = steps[, -1],
                showBullets = FALSE
              )
      )
    })
    
    # input  =====================================================================
    # maxHeight <- reactive({shinybrowser::get_height()})
    
    winsize <- windowSizeServer("win")
    
    input_rows <- reactive({
      print("getReactableState_table")
      reactable::getReactableState("tb_input", "selected")
    })
    s1 = reactive({input$inCheckboxGroup1})
    s2 = reactive({input$inCheckboxGroup2})
    thr_cos_pop = reactive({input$cutoff_ind[1]})
    filter_cap = reactive({
      print("filter_cap")
      print(input$filter_category)
      input$filter_category
    })
    
    selected_id = reactive({
      print("selected_id")
      input$current_node_id$nodes[[1]]
    })
    
    
    ## search box 1 ============================
    
    terms <- unique(tolower(input_table$term))
    observeEvent(input$btn_fixed, {
      updateSelectizeInput(session, 'searchbox1', choices = terms, server = TRUE)
    }, ignoreNULL = FALSE)
    
    
    ## input table ==============================
    
    output$ui_input <- renderUI({
      if((!is.null(df_input()))){
        reactable::reactableOutput("tb_input")
      }
    })
    
    df_input <- reactive({
      print("df_input")
      if(input$btn_fixed & (isTruthy(input$searchbox1))){
        input_table[tolower(input_table$term) == tolower(input$searchbox1),]
      } else if(isTruthy(input$searchbox2)){
        input_table[grepl(input$searchbox2, input_table$term, ignore.case = TRUE),]
      } else {
        input_table[grepl("cancer", input_table$term, ignore.case = TRUE),]
      }
    })
    
    input_selected <- reactive({
      if(input_Expanded() & (nrow(df_input()) < 6)){
        1:nrow(df_input())
      } else {
        NULL
      }
    })
    
    input_Expanded <- reactive({
      (isTruthy(df_input()) & nrow(df_input()) < 10) | (length(unique(df_input()$category)) == 1 )
    })
    
    output$tb_input <- reactable::renderReactable(
      reactable::reactable({ 
        df_input()[, c("id", "term", "category")]
      }, 
      groupBy = "category",
      columns = list(
        category = reactable::colDef(
          width = 200
        ),
        id = reactable::colDef(
          name = "id / term",
          cell = function(value, index) {
            term <- df_input()$term[index]
            term <- if (!is.na(term)) term else "Unknown"
            div(
              div(style = list(fontWeight = 600), value),
              div(style = list(fontSize = 12), term)
            )
          }
        ),
        term = reactable::colDef(show = FALSE)
      ),
      # searchable = TRUE,
      selection = "multiple", onClick = "select",
      defaultSelected = input_selected(),
      defaultExpanded = input_Expanded(),
      pagination = FALSE,
      height = winsize()[2]-450,
      theme = reactable::reactableTheme(
        rowSelectedStyle = list(backgroundColor = "#eee", 
                                boxShadow = "inset 2px 0 0 0 #ffa62d")
      )
      ))
    
    ## center nodes ==============================
    center_nodes <- eventReactive(input$gobutton, {
      print("center_nodes")
      # if (is.null(c(s1(), s2()))){
      #   c("C0008947", "LOINC:11006-4")
      # } else {
        c(s1(), s2())
      # }
    }, ignoreNULL = FALSE)
    
    
    
    # df_edges =======================================
    df_edges <- reactive({
      print("df_edges")
      req(center_nodes())
      print(center_nodes())
      Reduce(rbind, lapply(center_nodes(), getCosFromDB, ids, db))
    })
    
    
    # silders cos  ================================================================
    observeEvent(center_nodes(), {
      print("slider Input")
      if(!is.null(center_nodes())){
        thr_cos = df_edges() %>%
          dplyr::group_by(.data$from) %>%
          dplyr::summarize(thr_cos = ifelse(is.na(sort(.data$cos, decreasing = TRUE)[100]), min(abs(.data$cos)), sort(abs(.data$cos), decreasing = TRUE)[100]),
                           max = max(abs(.data$cos)))
        thr_cos = thr_cos[match(center_nodes(), thr_cos$from), ]
        
        print(thr_cos)
        print(input$controlbarMenu)
        
        if(input$controlbarMenu == "Network"){
          updateControlbarMenu(session = session, id = "controlbarMenu", selected = "Filter Nodes")
        }
        
        output$slider_cos <- renderUI({
          lapply(1:length(center_nodes()), function(i) {
            print(paste0("cos_", gsub(":", "_", center_nodes()[i])))
            sliderInput(
              inputId = paste0("cos_", gsub(":", "_", center_nodes()[i])),
              label = paste0(center_nodes()[i],":"),
              min = 0.1, max = abs(thr_cos$max[i]),
              value = c(abs(thr_cos$thr_cos[i]), abs(thr_cos$max[i])),
              step = 0.01
            )
          })
        })
      } else {
        output$slider_cos <- renderUI({""})
      }
      
    })
    
    list_cos <- reactive({
      sapply(1:length(center_nodes()), function(i){
        input[[paste0("cos_", gsub(":", "_", center_nodes()[i]))]][1]
      })
    })
    
    
    thr_cos <- reactive({
      # if(sum(sapply(list_cos(), is.null)) == 0){
        print("thr_cos")
        print(list_cos())
        list_cos() 
      # } else {
      #   c(0.2, 0.1)
      # }
    })
    
    
    # df_edges_cutted ====================================
    
    df_edges_cutted_cos <- reactive({
      print("df_edges_cutted_cos")
      if(sum(sapply(list_cos(), is.null)) == 0){
        Reduce(rbind, lapply(1:length(center_nodes()), function(i){
          df <- df_edges()[df_edges()$from == center_nodes()[i], ]
          print(paste(center_nodes()[i], "========="))
          print(nrow(df))
          df <- left_join(df, dict.combine[, c("id", "category")], by = c("to" = "id"))
          df[abs(df$cos) >= thr_cos()[i], ]
        }))
      }
    })
    
    df_edges_cutted <- reactive({
      print("df_edges_cutted")
      if(isTruthy(df_edges_cutted_cos()) & isTruthy(filter_cap())){
        Reduce(rbind, lapply(1:length(center_nodes()), function(i){
          df <- df_edges_cutted_cos()[df_edges_cutted_cos()$from == center_nodes()[i], ]
          print(paste(center_nodes()[i], "========="))
          print(nrow(df))
          df <- df[df$category %in% filter_cap(), 1:3]
          print(nrow(df))
          df
        }))
      }
    })
    
    
    # output table ===========================================
    
    
    df_output <- reactive({
      if (isTruthy(df_edges_cutted())){
        df_edges_cutted1 <- df_edges_cutted()
        colnames(df_edges_cutted1) <- c("center_nodes", "connected_nodes", "cosine_similarity")
        df <- left_join(df_edges_cutted1,
                        dict.combine[, c("id", "term", "category")],
                        by = c("connected_nodes" = "id"))
        print("df_output")
        # print(head(df))
        df[, c(1, 5, 2, 4, 3)]
      }
    })
    
    
    
    
    
    ## network  ====================================
    output$network <- renderUI({
      if((isTruthy(df_edges_cutted()))){
        shinycssloaders::withSpinner(
          visNetwork::visNetworkOutput("network_proxy_nodes",
                           height =  paste0((winsize()[2]-90),"px")),
          type = 6
        )
      }
    })
    output$network_proxy_nodes <- visNetwork::renderVisNetwork({
      print("********************network**************")
      req(picked_colors())
      myconfirmation = input$myconfirmation
      plot_network(df_edges_cutted(), input$hide_labels,
                   500,
                   myconfirmation, 1, 1,
                   dict.combine, attrs, picked_colors(), directed)
    })
    
    
    # Node info ==================================================================
    
    
    
    
    openBS_nodeinfo <- reactive({
      print("openBS_nodeinfo")
      if (!is.null(selected_id())){
        toggleModal(session, "selectednode", toggle = "open")
        df <- getCosFromDB(selected_id(), ids, db)
        v_cos = df$cos
        print("openBS_nodeinfo")
        print(length(v_cos))
        t_cos = sort(v_cos, decreasing = TRUE)[100]
        ifelse(is.na(t_cos), min(v_cos), t_cos)
        updateSliderInput(
          inputId = "cutoff_ind",
          # label = "Filter edges by cosine similarity (above):",
          min = 0.1, value = c(t_cos,1), max = 1,
          step = 0.01
        )
      }
    })
    
    observeEvent(input$current_node_id, {
      openBS_nodeinfo()
    })
    
    # Clicked node text
    output$clicked_node_title <- renderUI({
      h3(getDesc(selected_id()))
    })
    output$clicked_node_info <- renderUI({
      print("clicked_node_info")
      clickedNodeText(selected_id(), dict.combine)
    })
    
    ## df plots  ===================================
    df_plots <- reactive({
      print("df_plots")
      if (!is.null(selected_id())){
        df <- getCosFromDB(selected_id(), ids, db)
        df[df$cos >= thr_cos_pop(),]
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
      circularStatic(df_plots(), dict.combine, ColorsCirc)
    })
    
    
    # table of clicked node  ================================
    
    output$clicked_node_table <- renderUI({
      print("clicked_node_table")
      if(nrow(df_plots()) > 0){
        shinycssloaders::withSpinner(
          reactable::reactableOutput("tb_clicked_node", width = "100%",
                          height = paste0(winsize()[2] - 400,"px")), type = 6
        )} else {
          ""
        }
    })
    
    output$tb_clicked_node <- reactable::renderReactable(reactable::reactable({
      print("tb_clicked_node")
      df_edges_cutted <- df_plots()
      colnames(df_edges_cutted) <- c("center_nodes", "connected_nodes", "cosine_similarity")
      df <- left_join(df_edges_cutted,
                      dict.combine[, c("id", "term", "category")],
                      by = c("connected_nodes" = "id"))
      df$cosine_similarity <- round(df$cosine_similarity, 3)
      df[, c(5, 2, 4, 3)]
    },
    groupBy = c("category"),
    columns = list(
      cosine_similarity = reactable::colDef(name = "cosine similarity"),
      connected_nodes = reactable::colDef(
        minWidth = 250,
        name = "connected_nodes / term",
        # Show species under character names
        cell = function(value, index) {
          term <- df$term[index]
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
    
    observeEvent(input$deselect, {
      print("input$deselect")
      reactable::updateReactable(outputId = "tb_input", selected = NA, session = session)
    }
    )
    
    ##  more tab   =================================================
    
    cui_sy <- reactive({
      getData("synonyms", db)
    })
    
    details <- reactive({
      getData("details", db, id = selected_id())
    })
    
    # observeEvent(selected_id(), {
    #   if (isTruthy(details()) & nrow(details())>0) {
    #     write_rds(details(), "test/test_details.rds")
    #     showTab(inputId = "tabs_nodeinfo", target = "More details")
    #   } else {
    #     hideTab(inputId = "tabs_nodeinfo", target = "More details")
    #   }
    # })
    
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
        if(selected_id() %in% df$id){
          if(tname == "rollup"){
            sy <- cui_sy()
            # print(df[df$id == selected_id(),])
          } else{
            sy <- NULL
          }
          outdiv <<- detailsServer(tname, df[df$id == selected_id(),], title, outdiv, selected_id(), sy, output, helps)
        }
      })
      outdiv
    })
    
    
    # output$ui_details <- renderUI({
    #   outdiv <- tagList()
    #   for (title in unique(details()$title)) {
    #     print("ui_details")
    #     if(nrow(df_plots()) > 0){
    #       outdiv <- tagList(outdiv,
    #         h4(),
    #         shinycssloaders::withSpinner(
    #           reactableOutput("tb_details", width = "100%",
    #                           height = paste0(winsize()[2] - 400,"px")), type = 6
    #         ))
    #       } else {
    #         ""
    #       }
    #   }
    #   if (selected_id() %in% cui_sy()$id) {
    #     sy_info <- sort(cui_sy()$synonyms[cui_sy()$id == selected_id()])
    #     if (length(outdiv) > 0){
    #       outdiv <- tagList(outdiv, br())
    #     }
    #     outdiv <- tagList(outdiv,
    #       box_info(title = paste0("Synonyms of ", getDesc(selected_id()), ":"),
    #                info = tags$ul(
    #                  lapply(sy_info, function(x){ tags$li(x) })
    #                ),
    #                height = winsize()[2] - 400)
    #     )
    #   }
    #   outdiv
    # })
    
    
    # df_details <- reactive({
    #   df <- t(data.frame(strsplit(a, "|", fixed = TRUE)))
    #   colnames(df) <- strsplit(b, "|", fixed = TRUE)[[1]]
    # })
    
    # output$tb_details <- renderReactable(reactable({
    #   print("tb_rollup")
    #   df_rollup()
    # },
    #   bordered = TRUE,
    # details = function(index) {
    #   cui <- df_rollup()$cui[index]
    #   if(cui %in% cui_sy()$id ){
    #     print("*****************rollup********************")
    #     print(cui)
    #     htmltools::div(
    #       "Synonyms of ", df_rollup()$cui[index], ":",
    #       htmltools::tags$pre(paste(sort(cui_sy()$synonyms[cui_sy()$id == df_rollup()$cui[index]]), collapse = "\n"))
    #     )
    #   }
    # 
    #   },
    #   pagination = FALSE
    #   )
    # )
    
    
    
    ## Update checkboxinput if refreshing===========================================
    observeEvent(input$deselect, {
      x <- character(0)
      updateCheckboxGroupInput(session, "inCheckboxGroup1",
                               "0 NLP node(s) Selected",
                               choices = x,
                               selected = x)
      updateCheckboxGroupInput(session, "inCheckboxGroup2",
                               "0 Codified node(s) Selected",
                               choices = x,
                               selected = x)
      
    })
    
    ## Update checkboxinput based on selected rows in table=========================
    observe({
      print("checkboxUpdateBySelectedRows")
      checkboxUpdateBySelectedRows(input_rows(), df_input(), session)
    })
    
    
    
    ## downloading table================================================
    output$downloadData <- WriteData(df_output())
    
    getDesc <- function(x){
      dict.combine$term[match(x, dict.combine$id)]
    }
    
    # filter category ===============================
    
    categories <- reactive({
      print("categories")
      print(unique(df_edges_cutted_cos()$category))
      sort(unique(df_edges_cutted_cos()$category))
    })
    
    observe({
      # req(categories())
      print("updateAwesomeCheckboxGroup")
      print(categories())
      shinyWidgets::updateAwesomeCheckboxGroup(
        session = session,
        inputId = "filter_category",
        label = "Filter nodes by category:",
        choices = categories(),
        selected = categories())
    })
    
    observeEvent(input$btn_deselect_category, {
      shinyWidgets::updateAwesomeCheckboxGroup(
        session = session,
        inputId = "filter_category",
        choices = levels(dict.combine$category),
        selected = NULL)
    })
    
    observeEvent(input$btn_selectall_category, {
      shinyWidgets::updateAwesomeCheckboxGroup(
        session = session,
        inputId = "filter_category",
        choices = levels(dict.combine$category),
        selected = levels(dict.combine$category))
    })
    
    
    # bookmark ======================================
    
    observeEvent(input$bookmark, {
      # print(attr_nodes())
      session$doBookmark()
    })
    
    # color picker  =================================
    
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
    

  }
  return(server)
}
