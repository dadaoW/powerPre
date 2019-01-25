#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("DT")
#install.packages("rvest")
#install.packages("shinyAce")
#install.packages("ggthemr")
#devtools::install_github('cttobin/ggthemr')
#install.packages("shinythemes")
#install.packages("shinydashboard")
#install.packages("shinycssloaders")
#install.packages("leaflet")
#install.packages("shinyjs")
#install.packages("weatherr")
#install.packages("RMySQL")

library(shiny)
library(DT)
library(rvest)
library(plyr)
library(shinyAce)
library(shiny)
library(ggplot2)
library(ggthemr)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)
library(shinyjs)
library(weatherr)
library(RMySQL)

Logged = FALSE
my_username <- "test"
my_password <- "test"

ui2 <- dashboardPage(
  
  dashboardHeader(title="Caini-Deying"),
  dashboardSidebar
  (
    sidebarMenu
    (
      menuItem("table", icon = icon("th"), tabName = "table"),
      menuItem("predict", icon = icon("flag",lib = "glyphicon"), tabName = "predict"),
      menuItem("feedback", tabName = "feedback", icon = icon("pencil",lib = "glyphicon")),
      menuItem("info", icon = icon("header",lib = "glyphicon"), tabName = "info")
    )
  ),
  dashboardBody
  (
    fluidRow
    (
      tabItems
      (
        tabItem
        (
          tabName = "table",
          tabsetPanel(
            type = "tab",id = "tabsetPanel_data",
            tabPanel(
              "数据预览",
              sidebarLayout
              (
                #position = "right",
                sidebarPanel
                (
                  #h3("数据预览"),
                  actionButton(inputId = "data_look",label = "预览最新数据")
                  ,br()
                  ,br()
                  ,downloadButton("downloadData2", "下载")
                  ,br()
                  ,br()
                  ,bookmarkButton(id = "bookmark","bookmark")
                ),
                mainPanel(
                  DT::dataTableOutput('data_table_look') %>% withSpinner(type=4)
                )
              )            
            )
          ,tabPanel(
            "数据上传",
            sidebarLayout
            (
              #position = "right",
              sidebarPanel
              (
                h3("上传数据"),
                fileInput(inputId = "file",label = h4("请选择上传文件路径")),
                actionButton(inputId = "upload_action",label = "开始上传并显示最后六条记录")
              ),
              mainPanel(
                #DT::dataTableOutput('data_table') %>% withSpinner(type=4)
                uiOutput("tb")
              )
            )            
          )
          ,tabPanel(
            "数据追加",
            sidebarLayout
            (
              sidebarPanel
              (
                
                h3("追加数据"),
                fileInput(inputId = "file_add",label = h4("请选择追加文件路径")),
                actionButton(inputId = "upload_action_add",label = "开始上传并显示最后六条记录")
              ),
              mainPanel(
                #DT::dataTableOutput('data_table') %>% withSpinner(type=4)
                uiOutput("tb_added")
              )
            )            
          )
          ,tabPanel(
            "模型训练",
            sidebarLayout
            (
              sidebarPanel
              (
                h3("模型训练")
                ,br()
                ,actionButton(inputId = "existed_data_use",label = "用已存数据训练模型")
                ,br()
                ,br()
                ,actionButton(inputId = "local_data_use",label = "用本地数据训练模型")
              ),
              mainPanel(
                #DT::dataTableOutput('data_table')%>% withSpinner(type=4)
                #textOutput()
                
              )
            )            
          )
        )
        ),
        
        tabItem
        (tabName = "predict",
                tabsetPanel
                (
                  type = "tab",id = "tabsetPanel",
                  tabPanel
                  (
                  "Panel1",
                  sidebarLayout
                    (
                     sidebarPanel
                      (
                       textInput("reload_channels", h4("换料通道"), value = "")
                       ,textInput("yewei_avg", label=h4("平均液位△"),value = "")
                       #,actionButton(inputId = "clear_the_matrix",label = "Clear")
                       ,actionButton(inputId = "predict_action",label = "Apply")
                       #,radioButtons('style', 'Progress bar style', c('notification', 'old'))
                       
                      )
                     ,mainPanel
                      (
                       wellPanel
                        (
                         #DT::dataTableOutput('table4') %>% withSpinner(type=4)
                         #?column()
                         #?fluidRow()
                         titlePanel(title = h4("14区域液位值")),
                         fluidRow(column(width = 2
                                    ,textInput("3", label = "区域3", value = ""),offset = 4,style='padding:0px;'
                                    #,tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                                    )
                                  ,column(width = 2
                                     ,textInput("10", label = "区域10", value = "")
                                     ,offset = 0,style='padding:0px;'))
                         ,fluidRow(column(width = 2
                                     ,textInput("1", label = "区域1", value = ""),offset = 1,style='padding:0px;')
                                  ,column(width = 2
                                     ,textInput("8", label = "区域8", value = "")
                                     ,offset = 0,style='padding:0px;')
                                  ,column(width = 2
                                     ,textInput("6", label = "区域6", value = ""),offset = 2,style='padding:0px;')
                                  ,column(width = 2
                                     ,textInput("13", label = "区域13", value = "")
                                     ,offset = 0,style='padding:0px;'))
                         ,fluidRow(column(width = 2
                                     ,textInput("4", label = "区域4", value = ""),offset = 4,style='padding:0px;')
                                  ,column(width = 2
                                     ,textInput("11", label = "区域11", value = "")
                                     ,offset = 0,style='padding:0px;'))
                         ,fluidRow(column(width = 2
                                     ,textInput("2", label = "区域2", value = ""),offset = 1,style='padding:0px;')
                                  ,column(width = 2
                                     ,textInput("9", label = "区域9", value = "")
                                     ,offset = 0,style='padding:0px;')
                                  ,column(width = 2
                                     ,textInput("7", label = "区域7", value = ""),offset = 2,style='padding:0px;')
                                  ,column(width = 2
                                     ,textInput("14", label = "区域14", value = "")
                                     ,offset = 0,style='padding:0px;'))
                         ,fluidRow(column(width = 2
                                     ,textInput("5", label = "区域5", value = ""),offset = 4,style='padding:0px;')
                                  ,column(width = 2
                                     ,textInput("12", label = "区域12", value = "")
                                     ,offset = 0,style='padding:0px;'))
                        ),
                       wellPanel
                        (
                         titlePanel(title = h4("14区域液位变化值")),
                         #?titlePanel()
                         #DT::dataTableOutput('table4')%>% withSpinner(type=4)
                         #?column()
                         #?textInput()
                         #?fluidRow()#最多12个列
                         fluidRow(column(width = 2
                                    ,textInput("3", label = "区域3", value = ""),offset = 4,style='padding:0px;')
                                  ,column(width = 2
                                    ,textInput("10", label = "区域10", value = "")
                                    ,offset = 0,style='padding:0px;'))
                         ,fluidRow(column(width = 2
                                    ,textInput("1", label = "区域1", value = ""),offset = 1,style='padding:0px;')
                                  ,column(width = 2
                                    ,textInput("8", label = "区域8", value = "")
                                    ,offset = 0,style='padding:0px;')
                                  ,column(width = 2
                                    ,textInput("6", label = "区域6", value = ""),offset = 2,style='padding:0px;')
                                  ,column(width = 2
                                    ,textInput("13", label = "区域13", value = "")
                                    ,offset = 0,style='padding:0px;'))
                         ,fluidRow(column(width = 2
                                    ,textInput("4", label = "区域4", value = ""),offset = 4,style='padding:0px;')
                                  ,column(width = 2
                                    ,textInput("11", label = "区域11", value = "")
                                    ,offset = 0,style='padding:0px;'))
                         ,fluidRow(column(width = 2
                                    ,textInput("2", label = "区域2", value = ""),offset = 1,style='padding:0px;')
                                  ,column(width = 2
                                    ,textInput("9", label = "区域9", value = "")
                                    ,offset = 0,style='padding:0px;')
                                  ,column(width = 2
                                    ,textInput("7", label = "区域7", value = ""),offset = 2,style='padding:0px;')
                                  ,column(width = 2
                                    ,textInput("14", label = "区域14", value = "")
                                    ,offset = 0,style='padding:0px;'))
                         ,fluidRow(column(width = 2
                                    ,textInput("5", label = "区域5", value = ""),offset = 4,style='padding:0px;')
                                  ,column(width = 2
                                    ,textInput("12", label = "区域12", value = "")
                                    ,offset = 0,style='padding:0px;'))
                       )
                      )
                     )
                  ),
             tabPanel("Panel2", 
               
               wellPanel(
               textOutput('done'),
               DT::dataTableOutput('table4')%>% withSpinner(type=4))
           ),
            tabPanel("Panel3",
                     sidebarLayout(
                       sidebarPanel(
                         textInput("reload_channels2", h3("reload_channels2"), value = "")
                         ,actionButton(inputId = "predict_action2",label = "Apply")
                         #,downloadButton("downloadData", "Download")
                       ),
                       mainPanel(
                         DT::dataTableOutput('table5')%>% withSpinner(type=4)
                       )
                     )
                     ),
            tabPanel("Panel4",
                     sidebarLayout(
                       sidebarPanel(
                         #textInput("reload_channels2", h3("reload_channels2"), value = "")
                         #,
                         actionButton(inputId = "predict_action3",label = "预测")
                         ,br()
                         ,br()
                         ,downloadButton("downloadData", label = "结果下载")
                         #,downloadButton("downloadData", "Download")
                       ),
                       mainPanel(
                         DT::dataTableOutput('table6')%>% withSpinner(type=4)
                       )
                     )
            )
         )
        ),
        
        
        
        

        tabItem(tabName = "feedback",
                sidebarLayout(
                  sidebarPanel(
                    h5("Thanks for giving us valuable feedback"),
                    actionButton("reset", "Reset text"),
                    actionButton("submit", "submit")
                  ),
                  
                  mainPanel(
                    aceEditor("ace",fontSize = 20,autoComplete = "enabled",theme="ambiance")
                  )
                )
        ),
        
        tabItem(tabName = "info",
                sidebarLayout(
                  sidebarPanel("information",
                               h5("maintainer: Caini WangDeying"),
                               h5("version: 0.0.1"),
                               h6("contact me:18810381036"),
                               a("wangdy9217@163.com")
                               
                  ),
                  mainPanel(
                    
                    img(src="F:/RWD/shiny3/caini_20190123103629.png",width=400,height=200),
                    br(),
                    h2("Haide",aign="center"),
                    h3("a simple weather application powered by shiny",align="left"),
                    h5("technology used: ",","
                       , a("R",href="https://www.r-project.org/"),","
                       , a("shiny",href="http://shiny.rstudio.com/") ,",", "Crawler" ,","
                       , a("ggplot2",href="http://ggplot2.org/")),
                    h5("library used:", a("shiny",href="http://shiny.rstudio.com/") 
                       , a("DT")
                       , a("ggplot2",href="http://ggplot2.org/")
                       , a("ggthemr",href="https://cran.r-project.org/web/packages/ggthemes/index.html")
                       , a("plyr",href="https://cran.r-project.org/web/packages/plyr/index.html")
                       , a("rvest",href="https://cran.r-project.org/web/packages/rvest/index.html")
                       , a("weatherR",href="https://cran.r-project.org/web/packages/weatherr/index.html"))
                    ,includeMarkdown("log.md")
                  )                    
                )
        )
      ))
  )
)

server <- function(input, output,session) {
  options(shiny.maxRequestSize=100*1024^2)

  data <- reactive({
    input$upload_action
    file1 <- isolate(input$file)
    if(is.null(file1)){return()}
    else{
      #read.csv(file1$datapath)
      DataUpdate(data_source_path=file1$datapath
                     ,newdata_path=FALSE,dwnld_data = FALSE,save_rdata = FALSE)
      data_1022
      }
  })
  
  output$data_table <- DT::renderDataTable(
    DT::datatable({
    if(is.null(data())){return()} 
    else{tail(data())[,1:31]}
    },
    #下面的options还有待完善。
   options = list(columnDefs = list(list(className = 'dt-center',targets = 0:30),
                                    list(width=2,targets = 0:30)),
                  pageLength = 25,
                  fixedHeader.header = TRUE,
                  scrollX = TRUE,
                  autoWidth = TRUE,
                  scrollY = 250
                  ,paging = FALSE
                  ,fixedColumns.leftColumns = 1
                  ,searching = FALSE
                  ,filtering = FALSE
                  ,ordering = FALSE
                  ,scroller.loadingIndicator = TRUE
   )
    )
  )
  output$tb <- renderUI({
    if(is.null(data())){}
    else DT::dataTableOutput('data_table')%>% withSpinner(type=4)
  })
  
  data_add <- reactive({
    input$upload_action_add
    file2 <- isolate(input$file_add)
    if(is.null(file2)){return()}
    else{
      #read.csv(file1$datapath)
      DataUpdate(data_source_path=FALSE
                 ,newdata_path=file2$datapath,dwnld_data = FALSE,save_rdata = FALSE)
      data_1022
    }
  })
  
  output$data_table_added <- DT::renderDataTable(
    DT::datatable({
      if(is.null(data_add())){return()}
      else{tail(data_add())[,1:31]}
    },
    #下面的options还有待完善。
    options = list(columnDefs = list(list(className = 'dt-center',targets = 0:30),
                                     list(width=2,targets = 0:30)),
                   pageLength = 25,
                   fixedHeader.header = TRUE,
                   scrollX = TRUE,
                   autoWidth = TRUE,
                   scrollY = 250
                   ,paging = FALSE
                   ,fixedColumns.leftColumns = 1
                   ,searching = FALSE
                   ,filtering = FALSE
                   ,ordering = FALSE
                   ,scroller.loadingIndicator = TRUE
    )
    )
  )
  output$tb_added <- renderUI({
    if(is.null(data_add())){}
    else DT::dataTableOutput('data_table_added')%>% withSpinner(type=4)
  })
  
  
  output$data_table_look <- DT::renderDataTable(DT::datatable({
    #"scrollX": true
    input$data_look
    #style = isolate(input$style)
    data_1022[,1:31]
  },
  #下面的options还有待完善。
  options = list(columnDefs = list(list(className = 'dt-center',targets = 0:22),
                                   list(width=2,targets = 0:22)),
                 pageLength = 25,
                 fixedHeader.header = TRUE,
                 scrollX = TRUE,
                 autoWidth = TRUE,
                 scrollY = 300
                 ,paging = FALSE
                 ,fixedColumns.leftColumns = 1
                 ,searching = FALSE
                 ,filtering = FALSE
                 ,ordering = FALSE
                 ,scroller.loadingIndicator = TRUE
  )
  ))
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      data <- data_1022
      write.csv(data,con,row.names = TRUE)
    }
  )

  
  
  
  
  output$table4 <- DT::renderDataTable(DT::datatable({
    input$predict_action
    #style = isolate(input$style)

    if(isolate(input$reload_channels) == ""){
      power_distribution_matrix}
    else{
     #output$op_info <- start_info
    #rm(power_distribution_matrix)
    powerpre <- PowerPre(fpd = "5068.7-2",reload_channels=isolate(input$reload_channels),yewei_avg=FALSE
             ,yewei_14=FALSE,yewei_delta=FALSE
             ,dwnld_power = FALSE,save_rdata = FALSE)
    
    power_distribution_matrix
    #output$op_info <- end_info
    }
  },
  #下面的options还有待完善。
  options = list(columnDefs = list(list(className = 'dt-center',targets = 0:22),
                                     list(width=2,targets = 0:22)),
                   pageLength = 25,
                   fixedHeader.header = TRUE,
                   scrollX = TRUE,
                   autoWidth = TRUE,
                   scrollY = 500
                   ,paging = FALSE
                   ,fixedColumns.leftColumns = 1
                   ,searching = FALSE
                   ,filtering = FALSE
                   ,ordering = FALSE
                   ,scroller.loadingIndicator = TRUE
                   )
  ))
  
  output$table5 <- DT::renderDataTable(DT::datatable({
    #"scrollX": true
    input$predict_action2
    
    if(isolate(input$reload_channels) == ""){
      power_distribution_matrix}
    else{
      #output$op_info <- start_info
      #rm(power_distribution_matrix)
      powerpre <- PowerPre(fpd = "5068.7-2"
                           ,reload_channels=isolate(input$reload_channels2)
                           ,yewei_avg=FALSE
                           ,yewei_14=FALSE,yewei_delta=FALSE
                           ,dwnld_power = FALSE,save_rdata = FALSE)
      power_distribution_matrix
      #output$op_info <- end_info
    }
  },
  #下面的options还有待完善。
  options = list(columnDefs = list(list(className = 'dt-center',targets = 0:22),
                                   list(width=2,targets = 0:22)),
                 pageLength = 25,
                 fixedHeader.header = TRUE,
                 scrollX = TRUE,
                 autoWidth = TRUE,
                 scrollY = 500
                 ,paging = FALSE
                 ,fixedColumns.leftColumns = 1
                 ,searching = FALSE
                 ,filtering = FALSE
                 ,ordering = FALSE
  )
  ))
  
  output$table6 <- DT::renderDataTable(DT::datatable({
    #"scrollX": true
    input$predict_action3
    
    if(isolate(input$reload_channels) == ""){
      power_distribution_matrix}
    else{
      #output$op_info <- start_info
      #rm(power_distribution_matrix)
      powerpre <- PowerPre(fpd = "5068.7-2"
                           ,reload_channels=isolate(input$reload_channels)
                           ,yewei_avg=FALSE
                           ,yewei_14=FALSE,yewei_delta=FALSE
                           ,dwnld_power = FALSE,save_rdata = FALSE)
      power_distribution_matrix
      #output$op_info <- end_info
    }
  },
  #下面的options还有待完善。
  options = list(columnDefs = list(list(className = 'dt-center',targets = 0:22),
                                   list(width=2,targets = 0:22)),
                 pageLength = 25,
                 fixedHeader.header = TRUE,
                 scrollX = TRUE,
                 autoWidth = TRUE,
                 scrollY = 500
                 ,paging = FALSE
                 ,fixedColumns.leftColumns = 1
                 ,searching = FALSE
                 ,filtering = FALSE
                 ,ordering = FALSE
  )
  ))
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      data <- power_distribution_matrix
      write.csv(data,con,row.names = TRUE)
    }
  )
  
  observeEvent(input$predict_action,{
    updateTabsetPanel(session, "tabsetPanel", selected = "Panel2")}
  )
  
  
  #observeEvent(input$clear_the_matrix,{
  #  if(exists("power_distribution_matrix")){
  #    output$op_info <- "this clear reaction"
  #    
  #  updateTabsetPanel(session, "tabsetPanel", selected = "Panel2")}}
  #)
  
  
  
  
  
  


  
  observeEvent(input$bookmark, {
    session$doBookmark()
  })

  observeEvent(input$map_marker_click,{
    print("observed map_marker_click")
    p <- input$map_marker_click
    print(p)
    str <- paste(p$lat," , ",p$lng)
    print(str)
    output$geo <- renderText({ 
      str
    })
    output$wea <- renderText({
      data <- locationforecast(p$lat, p$lng)
      as.character(data$temperature[1])
    })
  })
  observeEvent(input$map_click,{
    print("observed map_marker_click")
    p <- input$map_click
    print(p)
    str <- paste(p$lat," , ",p$lng)
    print(str)
    output$geo <- renderText({ 
      str
    })
    output$wea <- renderText({
      data <- locationforecast(p$lat, p$lng)
      as.character(data$temperature[1])
    })
  }
  )
  observeEvent(input$reset, {
    updateAceEditor(session, "ace", value = "")
    
  })
  observeEvent(input$submit,{
    str = as.character(input$ace)
    print(str)
  })
  
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
      }     
    }
  })
  output$dataInfo <- renderPrint({
    if (values$authenticated) "OK!!!!!"
    else "You are NOT authenticated"
  })
}

enableBookmarking(store = "url")
app <- shinyApp(ui = ui2, server = server)
