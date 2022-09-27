## ui.R ##
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggtips)
library(plotly)
library(stringr)
##########################################################################
message <- dropdownMenu(type = "messages",
                        messageItem(
                          from = "Sales Dept",
                          message = "Sales are steady this month."
                        ),
                        messageItem(
                          from = "New User",
                          message = "How do I register?",
                          icon = icon("question"),
                          time = "13:45"
                        ),
                        messageItem(
                          from = "Support",
                          message = "The new server is ready.",
                          icon = icon("life-ring"),
                          time = "2014-12-01"
                        )
)

notification <- dropdownMenu(type = "notifications",
                             notificationItem(
                               text = "5 new users today",
                               icon("users")
                             ),
                             notificationItem(
                               text = "12 items delivered",
                               icon("truck"),
                               status = "success"
                             ),
                             notificationItem(
                               text = "Server load at 86%",
                               icon = icon("exclamation-triangle"),
                               status = "warning"
                             )
)

task <- dropdownMenu(type = "tasks", badgeStatus = "success",
                     taskItem(value = 90, color = "green",
                              "Documentation"
                     ),
                     taskItem(value = 17, color = "aqua",
                              "Project X"
                     ),
                     taskItem(value = 75, color = "yellow",
                              "Server deployment"
                     ),
                     taskItem(value = 80, color = "red",
                              "Overall project"
                     )
)

##########################################################################
header <- dashboardHeader(message, notification, task, 
                          title = "Shiny Dashboard Practice", titleWidth = "15%")



sidebar <- dashboardSidebar(
  sidebarMenu(width = "15%",
    # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                   label = "Search..."),
    # htmlOutput("text"),
    menuItem("Dashboard", tabName = "dashboard",
             icon = icon("dashboard")),
    menuItem("Company Info", tabName = "companyinfo",
             icon = icon("circle-info")),
    menuItem("Charts", tabName = "charts",
             icon = icon("chart-column")
             ),
    hr(),
    sliderInput("slider1", "Slider input:", 0, 1, 0.5),
    selectInput("comp_variable", "Company Variable:",
                c("Job title" = "job_title",
                  "Company Location" = "company_location",
                  "Company Size" = "company_size"))#,
    # "sliderInput change histogram's ",br() ,
    # "binwidth  with Normalize Data",
    # br(),br(),
    # "selectInput change plots in",br() ,
    # "Company Info"
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              # A static valueBox
              valueBox(dim(table(data$work_year)), "Years", 
                       icon = icon("calendar"), color = "light-blue"),
              valueBox(dim(table(data$employment_type)), "Employment Type",
                       icon = icon("keyboard"), color = "blue"),
              
              # Dynamic valueBoxes
              valueBoxOutput("ExperienceLevelBox"),
              
            ),
            
            
            fluidRow(
              box(
                selectInput("variable", "Variable:",
                            c("Employment Type" = "employment_type",
                              "Employment Level" = "experience_level")),
                title = tagList(shiny::icon("server"), "Choose Variable"),
                height = 580, width = 2, solidHeader = FALSE, 
                status = "primary",
                sliderInput("slider", "Slider input:", 1, 607, 300),
                br(),hr(),
                "You can choose varibale to change datatables and plots",br(),br(),br(),
                "Slider Input can change rows of dataset, so you can decide how many data in right plot",
                br(),br()
              ),
              
              tabBox(
                title = tagList(shiny::icon("database"), "Data"),
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",width = 6, height = 580,
                selected = "Normalize Data",
                tabPanel("Row Data",plotlyOutput("bar_data", height = 480, width = "100%")),
                tabPanel("Z Score",plotlyOutput("zscore", height = 480, width = "100%")),
                tabPanel("Normalize Data",plotlyOutput("bar_std", height = 480, width = "100%"))
              ),
              column(4,
                     dataTableOutput('table')
              ),
            ),
            fluidRow(
              box(
                title = tagList(shiny::icon("chart-column"), "Bar Plot"), 
                solidHeader = F,
                collapsible = TRUE, status = "primary",
                plotOutput("bar1", height = 400), width = 7
              ),
              box(
                title = tagList(shiny::icon("chart-pie"), "Years"), 
                solidHeader = F,
                collapsible = TRUE, status = "primary",
                plotOutput("donut1", height = 400), width = 5
              ),
            ),
            fluidRow(
              box(
                title = tagList(shiny::icon("chart-column"), "Bar Plot"),
                solidHeader = TRUE,
                collapsible = TRUE, status = "danger",
                plotOutput("bar2", height = 350), width = 7
              ),
              box(
                title = tagList(shiny::icon("chart-pie"), "Employment Type/Experience Eevel"), 
                solidHeader = TRUE,
                collapsible = TRUE, status = "danger",
                plotOutput("pd", height = 350), width = 5
              )
            ),
            
            fluidRow(
              box(
                title =  tagList(shiny::icon("chart-line"), "Line Plot"), 
                solidHeader = TRUE,
                collapsible = TRUE, status = "primary",
                plotOutput("plot1", height = 350), width = 7
              ),
              box(
                title =  tagList(shiny::icon("chart-pie"), "Employment Type")
                , solidHeader = TRUE,
                collapsible = TRUE, status = "success",
                plotOutput("donut2", height = 350), width = 5
              ),
              
              box(
                title = tagList(shiny::icon("box"), "Employment Type")
                , solidHeader = TRUE,
                collapsible = TRUE, status = "primary",
                plotlyOutput("plot2", height = 400), width = 7
              ),
              box(
                title =  tagList(shiny::icon("chart-pie"), "Experience Eevel"),
                solidHeader = TRUE,
                collapsible = TRUE, status = "success",
                plotOutput("donut3", height = 400), width = 5
              )
            ),
            
    ),
    
    tabItem(tabName = "companyinfo",
            fluidRow(
              # A static valueBox
              infoBox(dim(table(data$job_title)), "Job Title",
                       icon = icon("clipboard"), color = "green", fill=TRUE),
              infoBox(dim(table(data$company_size)), "Company Size",
                       icon = icon("folder-plus"), color = "olive", fill=TRUE),
              infoBox(dim(table(data$company_location)), "Company Location",
                       icon = icon("location-dot"), color = "teal", fill=TRUE)
            ),
            
            fluidRow(
              leafletOutput("mymap"),
            ),
            br(),hr(),
            fluidRow(
              box(
                title = tagList(shiny::icon("chart-column"), "Box Plot"),
                collapsible = TRUE, status = "primary",
                plotOutput("bar3", height = 400), width = 6
              ),
              tabBox(
                title = tagList(shiny::icon("chart-pie"),"Pie/Donut Plot"),
                tabPanel(
                  "Piechart",
                  plotOutput("pie_donut", height = 400)
                  ),
                tabPanel(
                  "Donutchart",
                  plotOutput("donut_pie", height = 400)
                ),
                width = 6
              )
            ),
         ),
    tabItem(tabName = "charts",
            mainPanel(
              tabsetPanel(
                tabPanel("Iris 2D scatters Plot", 
                         fluidPage(
                           titlePanel("Iris 2D Plot"),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("xvariable", "X Variable:",
                                           choices = names(iris)),
                               selectInput("yvariable", "Y Variable:",
                                           choices = names(iris), selected = "Sepal.Width")
                             ),
                             mainPanel(width = 8,
                                       plotlyOutput("myPlot", width = "100%", height = 500)
                             )
                           )
                         )
                      ),
                tabPanel("Iris 3D scatters Plot", 
                         fluidPage(
                           titlePanel("Iris 3D Plot"),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("xvariable1", "X Variable:", width = "100%",
                                           choices = names(iris), selected = NULL),
                               selectInput("yvariable1", "Y Variable:", width = "100%",
                                           choices = names(iris), selected = "Sepal.Width"),
                               selectInput("zvariable1", "Z Variable:", width = "100%",
                                           choices = names(iris), selected = "Petal.Length"),
                             ),
                             mainPanel(width = 8,
                                       plotlyOutput("plot3d", height = 700)
                             )
                           )
                         )
                ),
                
                tabPanel("Cumulative Lines Animation", 
                         fluidPage(
                           titlePanel("Txhousing Cumulative Lines Animation"),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("city1", "City1:", width = "100%",
                                           choices = city$Var1, selected = "Abilene"),
                               selectInput("city2", "City2:", width = "100%",
                                           choices = city$Var1, selected = "Bay Area")
                             ),
                             mainPanel(width = 8,
                                       plotlyOutput("animation", height = 600)
                             )
                           )
                         )
                      ),

                 )
             )
    )
  )
)

#ui
ui <- dashboardPage(header, sidebar, body)