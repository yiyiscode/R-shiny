## server.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(webr)
library(moonBook)
library(leaflet)
library(ggtips)
library(plotly)

###########################################################################
#server
server <- function(input, output, session){
  newdata <- reactive({
    mydata <- data.frame(table(data[, input$comp_variable]))
    colnames(mydata) <- c("class", "Freq")
    mydata$Prop <- prop.table(mydata$Freq)
    mydata$p <- round(mydata$Freq / sum(mydata$Freq) * 100, 1)
    mydata <- mydata %>%
      arrange(desc(class)) %>%
      mutate(lab.ypos = cumsum(mydata$p) - 0.5*(mydata$p))
    mydata <- mydata[order(-mydata$Freq), ]
    return(mydata[1:8, ])
  })
  
  output$ExperienceLevelBox <- renderValueBox(
    valueBox(
      dim(table(data$experience_level)), "Experience Level",
      icon = icon("users"), color = "navy"
    )
  )
  
  output$bar_data <- renderPlotly({
    p <- ggplot(data[1:input$slider, ], aes(x = X, y = salary_in_usd)) +
         geom_bar(stat="identity", fill="steelblue")+
         labs(x="No")
    p <- ggplotly(p)
    mytext <- paste("NO: ", data$X, "\n" , 
                    "Salary in usd: ",data$salary_in_usd, sep="")    
    pp <- plotly_build(p)   
    style(pp, text=mytext, hoverinfo = "text", traces = c(1, 2))
  })
  
  output$zscore <- renderPlotly({
    p <- plot_ly(std_data[1:input$slider, ],
                 x = ~X,
                 y = ~salary_in_usd,
                 color = ~greater_than_mean,
                 colors = c("#ff9933", "#33cccc"),
                 type = 'bar',
                 hovertemplate = paste('<b>No:</b>: %{x}<br>',
                                       '<i>Z Score:</i>: %{y:.2f}'))
      p <- p%>% layout(legend = list(title=list(text='<b> Greater Than Mean </b>')))
    p
  })
  
  output$bar_std <- renderPlotly({
    p <- ggplot(std_data[1:input$slider, ], aes(x = salary_in_usd)) +
      geom_histogram(aes(y=..density..), fill="steelblue", binwidth = input$slider1)+
      geom_density(alpha=0.2, fill="green")+
      geom_line(data=normaldata ,aes(x=x, y=y), color="red",  linetype="dashed")
    ggplotly(p)
  })
  
  
  output$bar1 <- renderPlot(
    ggplot(df2, aes_string(x = "Group.1", y = "x", fill = "Group.2")) +
      geom_bar(stat="identity", position="dodge") +
      scale_fill_brewer(palette="Paired")+
      geom_text(aes(label=round(x)), vjust=1.4, color="white",
                position = position_dodge(0.9)) 
  )
  
  output$bar2 <- renderPlot(
    ggplot(df1, aes_string(x = "Group.1", y = "x", fill = "Group.2")) +
      geom_bar(stat="identity", position="dodge") +
      scale_fill_brewer(palette="Paired")+
      geom_text(aes(label=round(x)), vjust=1.4, color="white",
                position = position_dodge(0.9)) 
  )
  
  output$pd <- renderPlot(
    PieDonut(pd, aes(experience_level, employment_type, count = Freq))
  )
  
  output$donut1 <- renderPlot(
    ggplot(year_data, aes(x = 2, y = year_prop, fill = class)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0)+
      geom_text(aes(y = lab.ypos, label = year_prop), color = "white")+
      scale_fill_brewer(palette="Paired") +
      theme_void()+
      xlim(0.5, 2.5)
  )
  
  output$donut2 <- renderPlot(
    ggplot(type_data, aes(x = 2, y = type_prop, fill = class)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0)+
      geom_text(aes(y = lab.ypos[2], label = type_prop[2]), color = "white")+
      scale_fill_brewer(palette="Paired") +
      theme_void()+
      xlim(0.5, 2.5)
  )
  
  output$donut3 <- renderPlot(
    ggplot(level_data, aes(x = 2, y = level_prop, fill = class)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0)+
      geom_text(aes(y = lab.ypos, label = level_prop), color = "white")+
      scale_fill_brewer(palette="Paired") +
      theme_void()+
      xlim(0.5, 2.5)
  )
  
  output$plot1 <- renderPlot(
    ggplot(data.frame(aggregate(data$salary_in_usd, 
                                list(data$work_year, 
                                     data[, input$variable]), 
                                FUN=mean)),
           aes(x=Group.1, y=x, color=Group.2))+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks=seq(2020, 2022, 1))+
      labs(x = "Year", color = input$variable)
    
  )
  
  output$table <- renderDataTable(
  data[, c("work_year",input$variable,"salary_in_usd", "remote_ratio")],
  options = list(
   pageLength = 10,
   initComplete = I("function(settings, json) {alert('Done.');}")
    )
  )
  
  output$plot2 <- renderPlotly({
    fig <- plot_ly(data,
                   x = ~salary_in_usd,
                   color = ~get(input$variable),
                   colors = "RdBu",
                   type = "box")%>%
      layout(title = paste("Box Plot of", input$variable),
             legend = list(title=list(text=paste0('<b>',
                                                  input$variable,
                                                  '</b>'))))
    
    fig
  })
  ##########################################################################
  output$bar3 <- renderPlot(
    ggplot(newdata(), aes(x = reorder(class, Freq), y = Freq))+
      geom_bar(stat="identity", fill="#006666")+
      geom_text(aes(label=Freq), hjust=1.6, color="white", size=4)+
      coord_flip()+
      labs(x=input$comp_variable)
  )

  output$pie_donut <- renderPlot(
    ggplot(newdata(), aes(x = "", y = Freq, fill=paste(class, paste0("(", round(Prop*100),"%)"))))+
      geom_bar(width = 1, stat="identity")+
      coord_polar("y", start=0)+
      scale_fill_brewer(palette="BrBG") +
      labs(fill=input$comp_variable, y="", x="")+
      theme_void() +
      geom_text(aes(label = paste0(round(Prop*100), "%")),
                position = position_stack(vjust = 0.5), color="#001a33")+
      labs(fill="Class")
    )
  
  output$donut_pie <- renderPlot(
    ggplot(newdata(), aes(x = 2, y = Freq, fill = paste(class, paste0("(", p,"%)")))) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0)+
      #geom_text(aes(y = lab.ypos, label = round(Prop*100, 1)), color = "white")+
      scale_fill_brewer(palette="BrBG") +
      theme_void()+
      xlim(0.5, 2.5)+
      labs(fill="Class")
  )
  ############################################################
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      # addProviderTiles(providers$Stamen.TonerLite,
      #                  options = providerTileOptions(noWrap = TRUE)
      # ) %>%
      addTiles() %>%
      addMarkers(data = data, ~long, ~lat)
  })
#####################################################################
  output$myPlot <- renderPlotly({
    p <- plot_ly(iris, x = ~get(input$xvariable), 
                 y = ~get(input$yvariable),
                 colors = c("#ff6666","#0099cc","#339966"),
                 symbol = ~Species,
                 type = 'scatter', 
                 mode = 'markers', 
                 color = iris$Species,
                 size = 2,
                 hovertemplate = paste(input$xvariable, ':%{x:.2f}<br>',
                                       input$yvariable, ':%{y:.2f}'),
                 width = 1000)
    p <- p %>% layout(xaxis = list(title = input$xvariable), 
                      yaxis = list(title = input$yvariable), 
                      legend = list(title=list(text='<b> Species of Iris </b>'))
                      )
    p
  })
  
  output$animation <- renderPlotly({
    accumulate_by <- function(dat, var) {
      var <- lazyeval::f_eval(var, dat)
      lvls <- plotly:::getLevels(var)
      dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
      })
      dplyr::bind_rows(dats)
    }
    
    df <- txhousing 
    fig <- df %>%
      filter(year > 2005, city %in% c(input$city1, input$city2))
    fig <- fig %>% accumulate_by(~date)
    
    
    fig <- fig %>%
      plot_ly(
        x = ~date, 
        y = ~median,
        split = ~city,
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F),
        fill = 'tonexty',
        width = 1000
      )
    fig <- fig %>% layout(
      xaxis = list(
        title = "Date",
        zeroline = F
      ),
      yaxis = list(
        title = "Median",
        zeroline = F,
        tickprefix = "$"
      )
    ) 
    fig <- fig %>% animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE
    )
    fig <- fig %>% animation_slider(
      currentvalue = list(
        prefix = "Year "
      )
    )
    fig <- fig %>% animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
    
    fig
  })
  
  output$plot3d <- renderPlotly({
    p <- plot_ly(iris, 
                 x = ~get(input$xvariable1), 
                 y = ~get(input$yvariable1), 
                 z=~get(input$zvariable1), 
                 colors = c("#ff6666","#0099cc","#339966"),
                 type = 'scatter3d', 
                 mode = 'markers', 
                 color = iris$Species,
                 width = 1000)%>%
      layout(plot_bgcolor='#e5ecf6', 
             scene = list(xaxis = list(title = input$xvariable1),
                          yaxis = list(title = input$yvariable1),
                          zaxis = list(title = input$zvariable1)),
             legend = list(title=list(text='<b> Species of Iris </b>')))
    p
  })
  #############################################################################
  
}


#shinyApp(ui, server)
