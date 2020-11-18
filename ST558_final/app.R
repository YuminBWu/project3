library(shiny)
library(rtweet)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(dplyr)
library(randomForest)

ntw = 2000
## search for 2000 tweets using the rstats hashtag
rt <- search_tweets(
  "#rstats", n = ntw, include_rts = FALSE
)

## preview tweets data
rt

clplot = NA

## preview users data
users_data(rt)

p1<-ts_plot(rt)
p2<-ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# tmls <- get_timelines(c("CNN","BBCWorld","FoxNews"), n = 3200)
tml_cnn = search_tweets(q=c("CNN"), n = ntw, include_rts = FALSE)
tml_cnn$ch = 'CNN'
tml_bbc = search_tweets(q=c("BBCWorld"),n = ntw, include_rts = FALSE)
tml_bbc$ch = 'BBCWorld'
tml_fox = search_tweets(q=c("FoxNews"), n = ntw, include_rts = FALSE)
tml_fox$ch = 'FoxNews'
tmls = rbind(tml_cnn,tml_bbc,tml_fox)

ui <- dashboardPage(
  
  dashboardHeader(title = "Collecting Twitter Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data information", tabName = "page1", icon = icon("info")),
      menuItem("Summary", tabName = "page2", icon = icon("chart-bar")),
      menuItem("Clustering", tabName = "page3", icon = icon("project-diagram")),
      menuItem("Modeling", tabName = "page4", icon = icon("laptop")),
      menuItem("Save data", tabName = "page5", icon = icon("save"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "page1",
              h2("This is a shiny program about the dataset of",strong("tweets"),"."), 
              p("The app can provide you with summary of the dataset. This dataset comes from the", a("rtweet package",href="https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html"),
                ". The data set is about twitter such as the number of retweet, the number of favourite, the number of followers, the number of friends, etc."),
              p("The users can visualize the data and can perform linear regression and random forest to investigate the relationship between tweet counts."),
              withMathJax(),
              helpText('The linear regression is based on the following model: $$y=\\mu + \\beta x + \\epsilon$$'),
              helpText('where $$\\epsilon \\sim N(0,\\sigma^2) $$')
      ),
      
      # Second tab content
      tabItem(tabName = "page2",
              fluidRow(
                box(radioButtons("radio", "Selection",   
                                 c("friends count vs retweet count"="friends count vs retweet count",
                                   "favourites count vs friends count"="favourites count vs friends count"
                                 )),
                    plotlyOutput("distPlot")),
                
                box(selectInput("organization", "organization",  unique(tmls$ch),selected = "BBCWorld"),
               
                plotOutput("frequency")),
                
                box(sliderInput("size", "Size of Points on Graph",
                                min = 10, max = 30, value = 5, step = 1)
                    
                )
               
              )
      ),
      tabItem(tabName='page3',
               fluidRow(
                 downloadButton("downloadcl", "Download"),
                 box(selectInput("unsupervised_learning", "Unsupervised Learning",  c("principal components analysis","clustering"),selected = "principal components analysis")),
                 box(plotOutput(outputId="unsupervised"))
                 
               )),
      tabItem(tabName='page4',
              fluidRow(
                box(radioButtons("radiom", "Models",   
                                 choices=c("Linear regression",
                                   "Random forest"),
                                 selected="Linear regression")),
                box(selectInput("depv", "Dependent Variable",   
                                 c('favorite_count'='favorite_count',
                                   'retweet_count'='retweet_count')),
                    uiOutput("ui")),
                #box(
                #    selectInput("indv", "Predictors",   
                #                 c("followers_count"="followers_count",
                #                   "friends_count"="friends_count"))
                #  ),
                
                box(tableOutput("table")),
                
                conditionalPanel(
                  condition = "input.radiom == 'Linear regression'",
                  box(
                  textInput('inputt',"Input the value of the predictor",value='0'),
                  textOutput('predicted'))
                )
                
              )
              ),
      tabItem(tabName='page5',
              fluidRow(
                downloadButton("downloadData", "Download"),
                tabsetPanel(
                  type = "tabs",
                  # summary tab
                  tabPanel(
                    "  Select language",
                    uiOutput("lang"),
                    shiny::dataTableOutput("merged")
                  )
                )
              ))
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output,session) {

  pl = 
    reactive({
      if(input$unsupervised_learning=="principal components analysis"){PCs<-prcomp(select(tmls,favorite_count,retweet_count),scale=TRUE)
  PCs
  biplot(PCs,xlabs=rep(".", nrow(tmls)),cex=1.2)
  }
  else{ 
    if(input$unsupervised_learning=="clustering"){hierClust<-hclust(dist(data.frame(tmls$favorite_count,tmls$retweet_count)))
    hierClust
    plot(hierClust,xlab = "")
    }
    
  }
    })

  
  output$unsupervised<- renderPlot({
    
    if(input$unsupervised_learning=="principal components analysis"){PCs<-prcomp(select(tmls,favorite_count,retweet_count),scale=TRUE)
    PCs
    clplot = biplot(PCs,xlabs=rep(".", nrow(tmls)),cex=1.2)
    clplot
    }
    else{ 
      if(input$unsupervised_learning=="clustering"){hierClust<-hclust(dist(data.frame(tmls$favorite_count,tmls$retweet_count)))
      hierClust
      clplot = plot(hierClust,xlab = "")
      clplot
      }

    }
    #pl()

  })
  
  output$downloadcl <- downloadHandler(
    filename = "clustering.png",
    content = function(file) {
      png(file)
      pl()
      dev.off()
    }
  )
  
  
  
  output$frequency <- renderPlot({
 
    if(input$organization=='CNN'){
        tmls %>%
        dplyr::filter(created_at > "2017-10-29") %>%
        dplyr::filter(ch == "CNN") %>%
        # dplyr::group_by(screen_name) %>%
        ts_plot("60 secs", trim = 1L) +
        ggplot2::geom_point(size = input$size) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "bottom",
          plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = "Frequency of Twitter statuses posted by news organization",
          subtitle = "Twitter status (tweet) counts aggregated by one minute from the latest 2000 tweets",
          caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )
      
      
    } else {
      if(input$organization=='BBCWorld')
      {
        tmls %>%
          dplyr::filter(created_at > "2017-10-29") %>%
          dplyr::filter(ch == "BBCWorld") %>%
          # dplyr::group_by(screen_name) %>%
          ts_plot("60 secs", trim = 1L) +
          ggplot2::geom_point(size = input$size) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.title = ggplot2::element_blank(),
            legend.position = "bottom",
            plot.title = ggplot2::element_text(face = "bold")) +
          ggplot2::labs(
            x = NULL, y = NULL,
            title = "Frequency of Twitter statuses posted by news organization",
            subtitle = "Twitter status (tweet) counts aggregated by one minute from the latest 2000 tweets",
            caption = "\nSource: Data collected from Twitter's REST API via rtweet"
          )
      }else{
        if(input$organization=='FoxNews')
          {
        
         tmls %>%
          dplyr::filter(created_at > "2017-10-29") %>%
            dplyr::filter(ch == "FoxNews") %>%
          # dplyr::group_by(screen_name) %>%
          ts_plot("60 secs", trim = 1L) +
          ggplot2::geom_point(size = input$size) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.title = ggplot2::element_blank(),
            legend.position = "bottom",
            plot.title = ggplot2::element_text(face = "bold")) +
          ggplot2::labs(
            x = NULL, y = NULL,
            title = "Frequency of Twitter statuses posted by news organization",
            subtitle = "Twitter status (tweet) counts aggregated by one minute from the latest 2000 tweets",
            caption = "\nSource: Data collected from Twitter's REST API via rtweet"
          )}
      }
      
    }
    
  })
   
  observe({
    
    if(input$organization=='BBCWorld'){
      val <- input$size
      updateSliderInput(session, "size", 
                        min = 1, max = 3, value = val, step = 1)
    } else {
      if(input$organization=='CNN')
      {
        val <- input$size
        updateSliderInput(session, "size", 
                          min = 1, max = 20, value = val, step = 1)
      }else{
      val <- input$size
      updateSliderInput(session, "size", 
                        min = 1, max = 10, value = val, step = 1)
      }
    }
  })
  
  
  
     output$distPlot <- renderPlotly({
       if(input$radio=='friends count vs retweet count')
       {
         # p1
         plot_ly(rt, x = ~friends_count, y = ~retweet_count) # %>%
           #filter(city %in% input$cities) %>%
           #group_by(city)
         }else{
         if(input$radio=='favourites count vs friends count')
         {
           # p2
           plot_ly(rt, x = ~friends_count, y = ~favourites_count)
          }
         }
       })
     
     output$table = renderTable({
       if(input$radiom=='Linear regression')
       {
         dv = input$depv
         pv = input$indv
         re = lm(as.formula(paste0(dv,'~',pv)),rt)
         t = as.data.frame(summary(re)$coef)
         t$Variable = rownames(summary(re)$coef)
         t
       }else{
         dv = input$depv
         re = randomForest(as.formula(paste0(dv,' ~ friends_count+followers_count+listed_count+is_retweet')), data = rt, method = "rf",nbagg = 200,importance=TRUE)
         t = as.data.frame(re$importance)
         t$Variable = rownames(t)
         t
       }
     })
     
     
     output$predicted = renderText({
       dv = input$depv
       pv = input$indv
       re = lm(as.formula(paste0(dv,'~',pv)),rt)
       pr = as.numeric(input$inputt)
       pdf = data.frame(pv = pr)
       colnames(pdf) = pv
       paste0("The predicted value of ", dv, " is ",round(predict(re, pdf)),".")
     })
     
     
     output$lang<-renderUI({
       selectInput("lang", label = h4("Choose language"),
                   choices = rt$lang ,selected = 1
       )
     })
     
     output$merged <- shiny::renderDataTable({
       rt %>%
         filter(lang == input$lang) 
     })
     
     output$downloadData <- downloadHandler(
       filename = "subset.csv",
       content = function(file) {
         write.csv(apply(rt[which(rt$lang==input$lang),],2,as.character), file)
       }
     )
     
     output$ui <- renderUI({
       switch(input$radiom,
       'Linear regression'=selectInput("indv", "Predictors",   
                                    c("followers_count"="followers_count",
                                      "friends_count"="friends_count"))
       )
     })
}
   


# Run the application 
shinyApp(ui = ui, server = server) 

