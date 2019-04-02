## Dual Histograms, scatterplot, Interaction Plot, t-Test, Effect Size (d) ##
library(readr) # to read data
library(ggplot2) # plotting
library(dplyr)
#library(magrittr)
library(psych)
library(DescTools)
library(effsize) # effect sizes
library(shiny)
options(scipen=999)

ui <- 
  fluidPage(
    titlePanel('Quick Statistics'),
    fluidRow(
      column(3, uiOutput('var1')),
      column(3, uiOutput('var2')),
      column(3, uiOutput('cat1')),
      column(3, uiOutput('cat2'))
    ),
    sidebarLayout(
    sidebarPanel(
        h5('Upload dataset to conduct analyses.'),
        hr(),
        fileInput('csvfile', 'Choose CSV File',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        checkboxInput('header', 'Has Header?', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons("disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head")),
      mainPanel(
        tabsetPanel(
        tabPanel("Data",
                 tableOutput("contents")),
        tabPanel('Table',
                 mainPanel(
                   dataTableOutput('data')
                 )),
        tabPanel('Descriptives',
                 verbatimTextOutput('despsyc'),
                 verbatimTextOutput('desctools')),
        tabPanel("Histograms", 
                 h2("Histograms for X and Y"),
                 splitLayout(cellWidths = c("50%", "50%"), 
                             plotOutput("firstHist"), 
                             plotOutput("secondHist")),
                 column(4, offset=4,
                        p('Note: Vertical bar indicates mean'), br(),
                        sliderInput("n_breaks", "Number of bins:", min = 0, max = 25, value = 10)
                 )), # two charts one row
        tabPanel('Interaction', h2("Interaction Plot"),h5('For First Categorical Variable'),
                 plotOutput("interaction")),
        tabPanel("Scatterplot", h2("Scatterplot"), p('Plots X and Y variables chosen above'),
                 p('Density indicated by shading of points (darker values = more dense)'),
                 plotOutput("firstPlot")),
        tabPanel("Multiplot",
                 h2("Multiplot Matrix"),
                 p("Plots all numeric variables in dataset."),
                 plotOutput('multiplot')),
        tabPanel("t-Tests",      
                 h2("Key summary statistics"),
                 p("The observed p value :"),
                 verbatimTextOutput("tpvalue"),
                 p("The observed t test statistic :"),
                 verbatimTextOutput('tvalue'),
                 p("The lower confidence interval :"),
                 verbatimTextOutput('cilow'),
                 p("The upper confidence interval :"),
                 verbatimTextOutput("ciup"),
                 p("Cohen's d Effect Size:"),
                 verbatimTextOutput('effsize')),
        tabPanel("ANOVA",
                 verbatimTextOutput('anova'),
                 verbatimTextOutput('aci')),
        tabPanel("ANCOVA 1 Cov",
                 verbatimTextOutput('ancova'),
                 verbatimTextOutput('ancovaci')),
        tabPanel("ANCOVA 2 Cov",
                 verbatimTextOutput('ancova2cov'),
                 verbatimTextOutput('ancova2ci')),
        tabPanel('About',
                 br(),
                 strong('Code'),
                 p('The code for this web application is available at',
                   a('GitHub.', href='https://github.com/mizumot/bs', target="_blank")),
                 p('If you want to run this code on your computer (in a local R session), run the code below:',
                   br(),
                   code('library(shiny)'),br(),
                   code('runGitHub("bs","mizumot")')
                 ),
                 br(),
                 strong('Author'),
                 p(a("Derek J. Hanson,", href="http://derekjhanson.com", target="_blank"),br(),
                   'Ph.D. Candidate',br(),
                   'Educational Psychology',br(),
                   'University of Texas at Austin')))
      )))

server <- function(input, output) {
  
  # new data file
  datasetInput <- reactive({
    data <- input$csvfile
    if (is.null(input$csvfile))
      return(mtcars)
    read.csv(data$datapath, header=input$header, sep=input$sep, 
             stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  output$contents <- renderTable(
    if(input$disp == "head") {
      return(head(datasetInput()))
    }
    else {
      return(datasetInput())
    }
  )

  output$var1 = renderUI({
    selectInput("var1", "Variable 1 (x):", choices = colnames(select_if(datasetInput(), is.numeric)))
  })
  output$var2 = renderUI({
    selectInput("var2", "Variable 2 (y):", choices = colnames(select_if(datasetInput(), is.numeric)))
  })
  output$cat1 = renderUI({
    selectInput("cat1", "Categorical Variable 1:", choices=colnames(select_if(datasetInput(), is.character)))
  })
  output$cat2 = renderUI({
    selectInput("cat2", "Categorical Variable 2:", choices=colnames(select_if(datasetInput(), is.character)))
  })
    
  # data
  output$data = renderDataTable({
    datasetInput()
  })
  
  # descriptives
  output$despsyc = renderPrint({
    x = select_if(datasetInput(), is.numeric)
    describe(x)
  })
  
  output$desctools = renderPrint({
    x = select_if(datasetInput(), is.numeric)
    Desc(x)
  })

  # histograms
  df1 <- reactive({ 
    df1 <- datasetInput() %>% select(input$var1)
  })
  
  df2 <- reactive({ 
    df2 <- datasetInput() %>% select(input$var2)
  })  
  
  output$firstHist <- renderPlot({ 
    dfb <- df1()
    mu <- datasetInput() %>% summarise(avg=mean(dfb[[1]]))
    ggplot(datasetInput(), aes(x=dfb[[1]])) +
      geom_histogram(position="identity", alpha=0.7, fill="skyblue",
                     bins=as.numeric(input$n_breaks))+
      geom_vline(data=mu, aes(xintercept=mu[[1]]))+
      labs(x=colnames(dfb))
  })
  
  output$secondHist <- renderPlot({ 
    dfb <- df2()
    mu <- datasetInput() %>% summarise(avg=mean(dfb[[1]]))
    ggplot(datasetInput(), aes(x=dfb[[1]])) +
      geom_histogram(position="identity", alpha=0.7, fill="skyblue",
                     bins=as.numeric(input$n_breaks))+
      geom_vline(data=mu, aes(xintercept=mu[[1]]))+
      labs(x=colnames(dfb))
  })
  
  # scatterplot
  df <- reactive({ 
    df <- datasetInput() %>% select(input$var1, input$var2)
  })
  
  output$firstPlot <- renderPlot({ 
    vars <- df()
    # calculate bivariate density for each point, along with principal component value
    library(fields)
    var = colnames(vars[1])
    var2 = colnames(vars[2])
    myvars = c(var, var2)
    #as.data.frame(data[,myvars])
    vars$density <- fields::interp.surface(
      MASS::kde2d(vars[[1]], vars[[2]]), as.data.frame(datasetInput()[,myvars]))
    vars$pc <- predict(prcomp(~vars[[1]]+vars[[2]], vars))[,1]
    
    ggplot(vars, aes(vars[[1]], vars[[2]], color = pc, alpha = 1/density)) +
      geom_point(show.legend = FALSE) +
      theme_minimal() +
      scale_color_gradient(low = "#32aeff", high = "#f2aeff") +
      scale_alpha(range = c(.5, .8)) +
      labs(x=colnames(vars[1]), y=colnames(vars[2]))
  })    
  
  # crazy correlation matrix  
  output$multiplot = renderPlot({
    library("PerformanceAnalytics")
    corrs = (select_if(datasetInput(), is.numeric))
    corrs = corrs[,]
    chart.Correlation(corrs, histogram=TRUE)
  })
  
  # interaction plot
  df3 <- reactive({ 
    df3 <- datasetInput() %>% select(input$var1, input$var2, input$cat1)
  })
  df4 <- reactive({ 
    df4 <- datasetInput() %>% select(input$var1, input$var2, input$cat1, input$cat2)
  })
  
  output$interaction = renderPlot({
    inter = df3()
    ggplot(datasetInput(), aes(inter[[1]], inter[[2]], colour=inter[[3]])) +  
      geom_smooth(method='lm', fill=NA, inherit.aes = TRUE) +
      labs(x=colnames(inter[1]), y=colnames(inter[2]), colour=colnames(inter[3]))
  })    
  
  # t-tests
  tdata1 = reactive({ 
    tdata1 = datasetInput() %>% select(input$var1)
  })
  
  tdata2 = reactive({ 
    tdata2 = datasetInput() %>% select(input$var2)
  })
  
  output$tpvalue = reactive({
    t1 = t.test(tdata1()[[1]], tdata2()[[1]], var.equal=TRUE, na.action=na.omit)
    {return(t1$p.value)}
  })
  
  output$tvalue = reactive({
    t1 = t.test(tdata1()[[1]], tdata2()[[1]], var.equal=TRUE, na.action=na.omit)
    {return(t1$statistic[[1]])}
  })
  
  output$cilow = reactive({
    t1 = t.test(tdata1()[[1]], tdata2()[[1]], var.equal=TRUE, na.action=na.omit)
    {return(t1$conf.int[[1]])}
  })
  
  output$ciup = reactive({
    t1 = t.test(tdata1()[[1]], tdata2()[[1]], var.equal=TRUE, na.action=na.omit)
    {return(t1$conf.int[[2]])}
  })
  
  output$effsize = reactive({
    d = cohen.d(tdata1()[[1]], tdata2()[[1]], na.rm=TRUE)
    {return(d$estimate[[1]])}
  })
  
  # ANOVA
  output$anova = renderPrint({
    vars <- df()
    summary(aov(vars[[1]]~vars[[2]]))
  })
  
  output$aci = renderPrint({
    vars = df()
    confint(aov(vars[[1]]~vars[[2]]))
  })
  
  # ANCOVA
  output$ancova = renderPrint({
    inter = df3()
    summary(aov(inter[[1]]~inter[[2]]+inter[[3]]))
  })
  
  output$ancovaci = renderPrint({
    inter = df3()
    confint(aov(inter[[1]]~inter[[2]]+inter[[3]]))
  })
  
  output$ancova2cov = renderPrint({
    inter = df4()
    summary(aov(inter[[1]]~inter[[2]]+inter[[3]]+inter[[4]]))
  })
  
  output$ancova2ci = renderPrint({
    inter = df4()
    confint(aov(inter[[1]]~inter[[2]]+inter[[3]]+inter[[4]]))
  })
  
}

shinyApp(ui = ui, server = server)

#setwd("C:/Users/Wyndows/Documents/Shiny")
#library(rsconnect)
deployApp()
