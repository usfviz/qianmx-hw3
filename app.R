#setwd("~/Desktop/Visualization/hw/hw3")
if (!require("plotly")) {install.packages("plotly")}
if (!require("GGally")) {install.packages("GGally")}
library(plotly)
library(GGally)

# load data
facebook <- read.table('dataset_Facebook.csv',sep=';',header = TRUE, stringsAsFactors = FALSE)

# Bubble Plot
slope <- 2.666051223553066e-05
facebook$size <- sqrt(facebook$Total.Interactions * slope)
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070')

# Scatter Matrix Plot
colnames(facebook)[8:12] <- c('Reach','Impressions','Engaged.Users','Consumers','Consumptions')
makePairs <- function(data) 
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}

# Parallel Plot
facebook1 <- facebook[,c("Post.Month","Post.Weekday","Post.Hour","Paid","Type")]
facebook1$Paid <- as.factor(facebook1$Paid)
facebook1 <- na.omit(facebook1)
# =============================================Shiny=============================================
# UI
ui <- fluidPage(
  headerPanel('HW3-Multivariate'),
  
  conditionalPanel(
    condition = "input.conditionedPanels==2",
    sidebarPanel(
    radioButtons(
      inputId="radio",
      label="Variable Selection Type:",
      choices=list(
        "All",
        "Manual Select"
      ),
      selected="All"),
    position = 'left'),
    
    conditionalPanel(
      condition = "input.radio != 'All'",
      checkboxGroupInput(
        'show_vars', 
        'Select Variables(>1):',
        choices=c(
          "Lifetime Reach" = "Reach",
          "Lifetime Impressions" = "Impressions",
          "Lifetime Engaged Users" = "Engaged.Users",
          "Lifetime Consumers" = "Consumers",
          "Lifetime Consumptions" = "Consumptions"
        ), 
        selected = "carat"
      )
    ),
    position = 'right'
  ),
  
  conditionalPanel(
    condition = "input.conditionedPanels==3",
    selectInput("type", "Post Type", 
              choices = list("Link" = "Link", "Photo" = "Photo",
                             "Status" = "Status", "Video"="Video"), selected = 1)
    ),

  mainPanel( tabsetPanel(
    tabPanel("bubble", plotlyOutput("bubble"),value=1),
    tabPanel("scattermatrix", plotOutput("scatterplot"),value = 2),
    tabPanel("parallelplot", plotOutput("parallelplot"),value=3)
    , id = "conditionedPanels"
    )
  )
)

# Server
server <- function(input, output) {
  
  # bubble
  output$bubble <-
    renderPlotly({
      plot_ly(facebook, x = facebook$like, y = facebook$share, 
              color = facebook$Type, 
              size = facebook$Total.Interactions, 
              colors = colors,
              type = 'scatter', 
              mode = 'markers',
              marker = list(symbol = 'circle', sizemode = 'diameter',
                            line = list(width = 2, color = '#FFFFFF')))%>%
        layout(title = 'Bubble Chart - Facebook Data',
               xaxis = list(title = 'Like',
                            gridcolor = 'rgb(255, 255, 255)',
                            #range = c(2.003297660701705, 5.191505530708712),
                            type = 'log',
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwidth = 2),
               yaxis = list(title = 'Share',
                            gridcolor = 'rgb(255, 255, 255)',
                            #range = c(36.12621671352166, 91.72921793264332),
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwith = 2),
               paper_bgcolor = 'rgb(243, 243, 243)',
               plot_bgcolor = 'rgb(243, 243, 243)')
    })
  
  # scatterplot
  df <- reactive({
    if (input$radio=='All'){df <- facebook[,c('Reach','Impressions','Engaged.Users','Consumers','Consumptions'),drop=FALSE]}
    else{
      df <- facebook[,input$show_vars,drop=FALSE]
      validate(
        need(ncol(df) >=2, "Please select at least 2 variables")
      )
    }
    return(df)
  })
  
  gg1 <- reactive({makePairs(df())})
  mega_data <- reactive({
    data.frame(gg1()$all, Type=rep(facebook$Type, length=nrow(gg1()$all)))
  })
  
  output$scatterplot <- renderPlot({
    ggplot(mega_data(), aes_string(x = "x", y = "y")) +
      facet_grid(xvar ~ yvar, scales = "free") +
      geom_point(aes(colour=Type), na.rm = TRUE, alpha=0.8) +
      stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)),
                   data = gg1()$densities, position = "identity",
                   colour = "grey20", geom = "line")
    
  })
  
  # ParallelPlot
  facebook2 <- reactive({
    df <- facebook1[which(facebook1$Type == input$type),]
    return(df)
  })
  
  output$parallelplot <- renderPlot({
    ggparcoord(facebook2(), columns = 1:3, groupColumn = "Paid", scale = 'globalminmax')
  })
  
}

shinyApp(ui = ui, server = server)






