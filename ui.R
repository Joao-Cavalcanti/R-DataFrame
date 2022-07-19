

header <- dashboardHeader(title = "Projeto de Estatística")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
    menuItem('Comparando Ações', tabName = 'comp', icon = icon('chart-bar'))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'm',
            fluidRow(
              box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                  selectInput('stock', 'Ação', stock_list, multiple=FALSE),
                  dateRangeInput("dates", 
                                 "Intervalo",
                                 end = max_time,
                                 start = min_time,
                                 min  = min_time,
                                 max  = max_time),
                  textOutput("DateRange"),
                  actionButton('go', 'Submeter')
              )
            ),
            fluidRow(
              box(title = "Informações sobre a ação", width = 12, solidHeader = TRUE,
                  DTOutput('info'),
                  DTOutput('info1')
              )
            ),
            fluidRow(
              box(title = "Série de Preços", width = 12, solidHeader = TRUE,
                  plotOutput('sh'),
                  plotOutput('sh1'),
                  plotOutput('sh2')
              )
            ),
    ),
    tabItem(tabName = 'comp',
            fluidRow(
              box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                  selectizeInput(
                    "q1",
                    label = "Ações",
                    choices = stock_list,
                    multiple = TRUE,
                    options = list(maxItems = 2)
                  ),
                  dateRangeInput("dates1", 
                                 "Intervalo",
                                 end = max_time,
                                 start = min_time,
                                 min  = min_time,
                                 max  = max_time),
                  textOutput("DateRange1"),
                  actionButton('go1', 'Submeter'),
                  )),
              fluidRow(
                box(title = "Valor de correlação", width = 12, solidHeader = TRUE,
                    DTOutput('info2')
                )
              ),
            fluidRow(
              box(title = "Série de Preços com duas variáveis", width = 12, solidHeader = TRUE,
                  plotOutput('sh3'),
                  plotOutput('sh4'),
                  plotOutput('sh5')
              )
            )
                  
                  
              
              
                      
    )
  )
)

ui <- dashboardPage(
  skin = 'blue',
  header, sidebar, body)
