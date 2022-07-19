find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
# Define server logic required to draw a histogram
server <- function(input, output) {
  ################### INPUT ####################
  select_stock <- eventReactive(input$go, {
    
    stock_name <- input$stock
    twin <- input$true_date
    
    df_stock <- master_df %>% 
      filter(Name == stock_name) 
    ## FALTA -> FILTRAR O DF POR DATA!!
    
    return(df_stock)
  })
  
  select_stock1 <- eventReactive(input$go1, {
    
    stock_names <- input$q1
    twin <- input$true_date
    
    df_stock <- master_df %>% 
      filter(Name == stock_names[1] | Name == stock_names[2]) 
    ## FALTA -> FILTRAR O DF POR DATA!!
    
    return(df_stock)
  })
  
  output$DateRange <- renderText({
    # make sure end date later than start date
    validate(
      need(input$dates[2] > input$dates[1], "Datas escolhidas estão com a ordem errada ou foram o mesmo dia."
      )
    )
    
    # make sure greater than 2 week difference
    validate(
      need(difftime(input$dates[2], input$dates[1], "days") > 1, "Intervalo muito pequeno, escolhar no mínimo um intervalo de 2 dias."
      )
    )
    
    paste("O intervalo de tempo tem", 
          difftime(input$dates[2], input$dates[1], units="days"),
          "days")
  })

  output$DateRange1 <- renderText({
    # make sure end date later than start date
    validate(
      need(input$dates1[2] > input$dates1[1], "Datas escolhidas estão com a ordem errada ou foram o mesmo dia."
      )
    )
    
    # make sure greater than 2 week difference
    validate(
      need(difftime(input$dates1[2], input$dates1[1], "days") > 1, "Intervalo muito pequeno, escolhar no mínimo um intervalo de 2 dias."
      )
    )
    
    paste("O intervalo de tempo tem", 
          difftime(input$dates1[2], input$dates1[1], units="days"),
          "days")
  })
  
  
  output$timedate_comp <- renderUI({
    
    stock_name <- input$stock_comp
  
    df <- master_df %>% 
      filter(Name %in% stock_name)
    
    maxmin_time <- df %>% 
      group_by(Name) %>% 
      summarise(MD = min(Date)) %>% 
      .$MD %>% 
      max()
    
    minmax_time <- df %>% 
      group_by(Index) %>% 
      summarise(MD = max(Date)) %>% 
      .$MD %>% 
      min()
    
    min_time <- maxmin_time
    max_time <- minmax_time
    
    dateRangeInput("true_date_comp", "Período de análise",
                   end = max_time,
                   start = min_time,
                   min    = min_time,
                   max    = max_time,
                   format = "dd/mm/yy",
                   separator = " - ",
                   language='pt-BR')
  })
  
  ################ OUTPUT #####################
  Info_DataTable <- eventReactive(input$go,{
    df <- select_stock()
    Stock <- input$stock
    Dates_data <- input$dates
    
    df <- df[df$Date > Dates_data[1] & df$Date < Dates_data[2],]
    
    Valor_Mínimo <- min(df %>% select(Close))
    Valor_Máximo <- max(df %>% select(Close))
    mean <- df %>% select(Close) %>% colMeans()
    Media <- mean[[1]]
  
    
    med <- t((df %>% filter(Name == Stock)) %>% select(Close))
    Mediana <- median(med)
    
    standard_dev <- t((df %>% filter(Name == Stock)) %>% select(Close))
    Desvio_Padrão <- sd(standard_dev)
    
    df_tb <-  data.frame(Valor_Mínimo, Valor_Máximo, Stock, Media, Mediana, Desvio_Padrão)
    
    df_tb <- as.data.frame(t(df_tb))

    
    # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
    # tb <- tb %>% 
    #     rename('Informações' = nms,
    #            'Valores' = V2)
    # 
    return(df_tb)
  })
  
  Info_DataTable1 <- eventReactive(input$go,{
    df <- select_stock()
    Stock <- input$stock
    Dates_data <- input$dates
    
    df <- df[df$Date > Dates_data[1] & df$Date < Dates_data[2],]
    
    
   
    mode <- t((df %>% filter(Name == Stock)) %>% select(Close))
    Moda <- find_mode(mode)
    
    df_tb <-  data.frame(Moda)
    
    df_tb <- as.data.frame(t(df_tb))
    
    
    # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
    # tb <- tb %>% 
    #     rename('Informações' = nms,
    #            'Valores' = V2)
    # 
    return(df_tb)
  })
  
  Info_DataTable2 <- eventReactive(input$go1,{
    df <- master_df
    Stock <- input$q1
    Dates_data <- input$dates1
    
    df <- df[df$Date > Dates_data[1] & df$Date < Dates_data[2],]
    
    data1 <- df %>% filter(Name == Stock[1]) %>% select(Close)
    data2 <- df %>% filter(Name == Stock[2]) %>% select(Close)
    Correlação <- cor(data1, data2)
    
    df_tb <-  data.frame(Correlação)
    
    df_tb <- as.data.frame(t(df_tb))
    
    
    # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
    # tb <- tb %>% 
    #     rename('Informações' = nms,
    #            'Valores' = V2)
    # 
    return(df_tb)
  })
  
  output$info <- renderDT({
    Info_DataTable() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  output$info1 <- renderDT({
    Info_DataTable1() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  output$info2 <- renderDT({
    Info_DataTable2() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  
  output$sh <- renderPlot({
    # All the inputs
    df <- select_stock()
    Dates_data <- input$dates
    df <- df[df$Date > Dates_data[1] & df$Date < Dates_data[2],]
    
    aux <- df$Close %>% na.omit() %>% as.numeric()
    aux1 <- min(aux)
    aux2 <- max(aux)
    
    df$Date <- ymd(df$Date)
    a <- df %>% 
      ggplot(aes(Date, Close, group=1)) +
      geom_path() +
      ylab('Preço da Ação em $') +
      coord_cartesian(ylim = c(aux1, aux2)) +
      theme_bw() +
      scale_x_date(date_labels = "%Y-%m-%d")
    
    a
  })
  
  output$sh1 <- renderPlot({
    
    df <- select_stock()
    Dates_data <- input$dates
    df <- df[df$Date > Dates_data[1] & df$Date < Dates_data[2],]
    
    df$Date <- ymd(df$Date)
    a <- df %>%
      ggplot( aes(x=df$Date, y=df$Close, option="A")) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, alpha=0.9) +
      geom_jitter(color="black", size=0.4, alpha=0.9) +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) +
      ggtitle("BoxPlot para Análise dos Preços") +
      xlab("")
    a
      
    
  })
  
  output$sh2 <- renderPlot({
    
    df <- select_stock()
    Dates_data <- input$dates
    df <- df[df$Date > Dates_data[1] & df$Date < Dates_data[2],]
    
    p <- ggplot(df, aes(x=df$Close)) + 
      geom_histogram()+
      ggtitle("Histograma dos preços") +
      xlab("")
    p
    
  })
  
  output$sh3 <- renderPlot({
    select_stock1()
    Dates_data <- input$dates1
    df <- master_df 
    df <- df[df$Date > Dates_data[1] & df$Date < Dates_data[2],]
    
    Stock <- input$q1
    
    data1 <- df %>% filter(Name == Stock[1]) %>% select(Close)
    data2 <- df %>% filter(Name == Stock[2]) %>% select(Close)
    
    df_tb <-  data.frame(df$Date, data1, data2)
    
    pl <- ggplot(data = df_tb, aes(x = df_tb$df.Date))
    pl <- pl + geom_line(aes(y = df_tb$Close, colour = 'red', label = Stock[1]))
    pl <- pl + geom_line(aes(y = df_tb$Close.1, colour = 'blue', label = Stock[2]))
    pl <- pl + theme_classic()
    pl <- pl + scale_color_manual(name ="variable"
                                  , labels= c(Stock[2], Stock[1])
                                  , values = c("blue", "red"))
    pl
    
  })
  
  output$sh4 <- renderPlot({
    select_stock1()
    Dates_data <- input$dates1
    df <- master_df 
    df <- df[df$Date > Dates_data[1] & df$Date < Dates_data[2],]
    
    Stock <- input$q1
    
    
    
    data1 <- df %>% filter(Name == Stock[1])
    data2 <- df %>% filter(Name == Stock[2])
    
    mean1 <- data1 %>% select(Close) %>% colMeans()
    Media1 <- mean1[[1]]
    
    mean2 <- data2 %>% select(Close) %>% colMeans()
    Media2 <- mean2[[1]]
    
    Médias = c(Media1, Media2)
    name = c(Stock[1], Stock[2])
    df_tb <-  data.frame(Médias, name)
    
    p <- ggplot(df_tb, aes(x=name, y=Médias)) + 
      geom_bar(stat = "identity")
    p
    
  })
  
  output$sh5 <- renderPlot({
    select_stock1()
    Dates_data <- input$dates1
    df <- master_df 
    df <- df[df$Date > Dates_data[1] & df$Date < Dates_data[2],]
    
    Stock <- input$q1
    
    data1 <- df %>% filter(Name == Stock[1]) %>% select(Close)
    data2 <- df %>% filter(Name == Stock[2]) %>% select(Close)
    
    df_tb <-  data.frame(df$Date, data1, data2)
    
    pl <- ggplot(data = df_tb, aes(x = df_tb$df.Date))
    pl <- pl + geom_point(aes(y = df_tb$Close, colour = 'red'))
    pl <- pl + geom_point(aes(y = df_tb$Close.1, colour = 'blue'))
    pl <- pl + theme_classic()
    pl <- pl + scale_color_manual(name ="variable"
                                  , labels= c(Stock[2], Stock[1])
                                  , values = c("blue", "red"))
    pl
    
  })
  
  
  
  
  
}