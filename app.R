#Project 3 - Anaya Malik

library(shiny)
library(tidyverse)
library(rsconnect)

BestUSCities2 <- read.csv('BestUSCities2.csv') #read in the csv file


ui <- fluidPage(
  
  
  # Application title
  titlePanel("Best Cities in the U.S - Project 3"),
  
  #image
  img(src = "https://th.bing.com/th/id/R.d627c9a1bf9177b8dd4406483b3e8e9c?rik=yL0%2bWPrrNOpWnQ&riu=http%3a%2f%2fupload.wikimedia.org%2fwikipedia%2fcommons%2f5%2f5f%2fNew_York_City_skyline_banner.jpg&ehk=HPYmKfrHCQJBo6%2b0pNEmtiVgZw7LLunKSln8jpmjtNk%3d&risl=1&pid=ImgRaw&r=0", width = "1000px", height = "150px"),
  

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #Select box for variable:
      selectInput("selectvar", label = h3("Choose a variable"), 
                  choices=list("Median Monthly Rent"=1, "Average Annual Salary"=2, "Median Age" =3, "Unemployment Rate" = 4, "State" = 5), 
                  selected = 1),
      
      #Select box for variable:
      selectInput("selectvar2", label = h3("Choose a Color"), 
                  choices=list("Light Pink"=1, "Light Yellow"=2, "Light Blue"=3), 
                  selected = 1),
      
      sliderInput("x_range",
                  "x_range:",
                  min = 502,
                  max = 1940,
                  value = c(502, 1940)),
      
      # Show a plot of the generated distribution
      
      #option to show mean
      checkboxInput("checkbox1", label="Display mean", value=FALSE),
      
      #option to show sd
      checkboxInput("checkbox2", label="Display standard deviation", value=FALSE),
      
      #option to show table
      checkboxInput("checkbox3", label="Display table", value=FALSE),
      
      verbatimTextOutput("displayText")
      
      
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      p('Mean:'),
      fluidRow(column(5, verbatimTextOutput("mean"))),
      p('Standard deviation:'),
      fluidRow(column(5, verbatimTextOutput("sd"))),
      p('Table:'),
      fluidRow(column(5, verbatimTextOutput("table"))),
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output){
  
  output$displayText <- renderPrint({
    cat("skyline header picture - Bing. (2023). Bing. https://www.bing.com/images/search?view=detailV2&ccid=1ifJob%2bR&id=E7E75DD3D821B7EFF4CD9D56EA34EBFA583EBDC8&thid=OIP.1ifJob-Rd7jdRAZIOz6OnAHaBE&mediaurl=https%3a%2f%2fth.bing.com%2fth%2fid%2fR.d627c9a1bf9177b8dd4406483b3e8e9c%3frik%3dyL0%252bWPrrNOpWnQ%26riu%3dhttp%253a%252f%252fupload.wikimedia.org%252fwikipedia%252fcommons%252f5%252f5f%252fNew_York_City_skyline_banner.jpg%26ehk%3dHPYmKfrHCQJBo6%252b0pNEmtiVgZw7LLunKSln8jpmjtNk%253d%26risl%3d1%26pid%3dImgRaw%26r%3d0&exph=808&expw=5616&q=skyline+header+picture&simid=608021224861747509&FORM=IRPRST&ck=58018A76DFFF74B964990B30129982B6&selectedIndex=7&ajaxhist=0&ajaxserp=0")
  })
  
  output$distPlot <- renderPlot({
    #Median Monthly Rent
    if(input$selectvar == 1 & input$selectvar2 == 1){
      filtered <- BestUSCities2 %>%
        filter(Median.Monthly.Rent > input$x_range[1]) %>%
        filter(Median.Monthly.Rent < input$x_range[2])
      
      hist(filtered$Median.Monthly.Rent, main='Median Monthly Rent Frequency Graph',xlab='Monthly Monthly Rent', col = 'lightpink')
    }
    
    if(input$selectvar == 1 & input$selectvar2 == 2){
      filtered <- BestUSCities2 %>%
        filter(Median.Monthly.Rent > input$x_range[1]) %>%
        filter(Median.Monthly.Rent < input$x_range[2])
      
      hist(filtered$Median.Monthly.Rent, main='Median Monthly Rent Frequency Graph',xlab='Monthly Monthly Rent', col = 'lightyellow')
    }
  
    if(input$selectvar == 1 & input$selectvar2 == 3){
      filtered <- BestUSCities2 %>%
        filter(Median.Monthly.Rent > input$x_range[1]) %>%
        filter(Median.Monthly.Rent < input$x_range[2])
      
      hist(filtered$Median.Monthly.Rent, main='Median Monthly Rent Frequency Graph',xlab='Monthly Monthly Rent', col = 'lightblue')
    }
    
    #Average Annual Salary
    if(input$selectvar == 2 & input$selectvar2 == 1){
      hist(BestUSCities2$Average.Annual.Salary, main='Average Annual Salary Frequency Graph',xlab='Average Annual Salary',col = 'lightpink')
    }
    
    if(input$selectvar == 2 & input$selectvar2 == 2){
      hist(BestUSCities2$Average.Annual.Salary, main='Average Annual Salary Frequency Graph',xlab='Average Annual Salary',col = 'lightyellow')
    }
    
    if(input$selectvar == 2 & input$selectvar2 == 3){
      hist(BestUSCities2$Average.Annual.Salary, main='Average Annual Salary Frequency Graph',xlab='Average Annual Salary',col = 'lightblue')
    }
    
    #Median Age
    if(input$selectvar == 3 & input$selectvar2 == 1){
      hist(BestUSCities2$Median.Age, main='Median Age Frequency Graph',xlab='Median Age Annual Salary',col = 'lightpink' )
    }
    
    if(input$selectvar == 3 & input$selectvar2 == 2){
      hist(BestUSCities2$Median.Age, main='Median Age Frequency Graph',xlab='Median Age Annual Salary',col = 'lightyellow' )
    }
    
    if(input$selectvar == 3 & input$selectvar2 == 3){
      hist(BestUSCities2$Median.Age, main='Median Age Frequency Graph',xlab='Median Age Annual Salary',col = 'lightblue' )
    }
    
    #Unemployment Rate
    if(input$selectvar == 4 & input$selectvar2 == 1){
      hist(BestUSCities2$Unemployment.Rate, main='Unemployment Rate Frequency Graph',xlab='Unemployment Rate Annual Salary',col = 'lightpink')
    }
    
    if(input$selectvar == 4 & input$selectvar2 == 2){
      hist(BestUSCities2$Unemployment.Rate, main='Unemployment Rate Frequency Graph',xlab='Unemployment Rate Annual Salary',col = 'lightyellow')
    }
    
    if(input$selectvar == 4 & input$selectvar2 == 3){
      hist(BestUSCities2$Unemployment.Rate, main='Unemployment Rate Frequency Graph',xlab='Unemployment Rate Annual Salary',col = 'lightblue')
    }
    
    #States
    if(input$selectvar == 5 & input$selectvar2 == 1){
      barplot(table(BestUSCities2$State), main='State Frequency Bar Graph',xlab='States',col = 'lightpink')
      table(BestUSCities2$State)
    }
    
    if(input$selectvar == 5 & input$selectvar2 == 2){
      barplot(table(BestUSCities2$State), main='State Frequency Bar Graph',xlab='States',col = 'lightyellow')
      table(BestUSCities2$States)
    }
    
    
    if(input$selectvar == 5 & input$selectvar2 == 3){
      barplot(table(BestUSCities2$State), main='State Frequency Bar Graph',xlab='States',col = 'lightblue')
      table(BestUSCities2$States)
    }
    
    #Display mean if selected
    output$mean <- renderPrint({ 
      if(input$checkbox1 == TRUE & input$selectvar == 1){
        mean(BestUSCities2$Median.Monthly.Rent, na.rm=TRUE)}
      
      else if(input$checkbox1 == TRUE & input$selectvar == 2) {
        mean(BestUSCities2$Average.Annual.Salary, na.rm=TRUE)}
      
      else if(input$checkbox1 == TRUE & input$selectvar == 3){
        mean(BestUSCities2$Median.Age, na.rm=TRUE)}
      
      else if(input$checkbox1 == TRUE & input$selectvar == 4){
        mean(BestUSCities2$Unemployment.Rate, na.rm=TRUE)}

    })
    

    #Display sd if selected
    output$sd <- renderPrint({ 
      if(input$checkbox2 == TRUE & input$selectvar == 1){
        sd(BestUSCities2$Median.Monthly.Rent, na.rm=TRUE)}
      
      else if(input$checkbox2 == TRUE & input$selectvar == 2){
        sd(BestUSCities2$Average.Annual.Salary, na.rm=TRUE)}
      
      else if(input$checkbox2 == TRUE & input$selectvar == 3){
        sd(BestUSCities2$Median.Age, na.rm=TRUE)}
      
      else if(input$checkbox2 == TRUE & input$selectvar == 4){
        sd(BestUSCities2$Unemployment.Rate, na.rm=TRUE)}

    })
    
    #Display table if selected
    output$table <- renderPrint({
      if(input$checkbox3 == TRUE & input$selectvar == 5){
        table(BestUSCities2$State)}
    })
    
  })
  
}


shinyApp(ui = ui, server = server)



