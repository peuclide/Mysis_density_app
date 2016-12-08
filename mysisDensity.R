
library(ggplot2)
library(plyr)
setwd("~/Documents/OneDrive/Documents/UVM/UVM Mysis/Mysis decline/Shiny")
#mysis <- read.csv("MysisDensityShiny.csv", header = T)
mysis <- read.csv("MysisDensity12-8-16.csv")
mysis$ave.den.per.m2 <- as.numeric(as.character(mysis$ave.den.per.m2))

#make summarySE funciton

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
str(mysis)





msum <- summarySE(mysis, measurevar="ave.den.per.m2", groupvars=c("Lake","yr.sampled"))
pd <- position_dodge(0.1) 
ggplot( msum[msum$Lake %in% c("Ontario", "Michigan"),], aes(yr.sampled, 
  log(ave.den.per.m2), colour = Lake, group = Lake))+
    geom_errorbar(aes(ymin=ave.den.per.m2-ci, ymax=ave.den.per.m2+ci), colour="black", width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3)

ggplot( mysis[mysis$season == "spring",], aes(Lake, ave.den.per.m2))+
 geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Plant growth with\ndifferent treatments")


ggplot( mysis[mysis$yr.sampled == 1995,], aes(Lake, ave.den.per.m2))+
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(2000)

plot(log(mysis$ave.den.per.m2)~mysis$max.depth)

ggplot(mysis, aes(x=min.depth, y=log(ave.den.per.m2))) + geom_point()



####shiny app start
library(shiny)

ui <- fluidPage(

    titlePanel("Mysis Density in North America"),
    

      
      sliderInput(inputId = "date", 
                         label = "Choose vairable", 
                         value = 1995,
                         min = 1965, max = 2015),
             
             textInput(inputId = "Title",
                       label = "choose a title",
                       value = "boxplots!!!"),
             
             plotOutput("hist"),
             
             
             selectInput(inputId = "season", 
                         label = "choose a season", 
                         choices = c("spring", "summer", "fall", "winter")),
             
             plotOutput("boxplot1"),
             fluidRow(
               column(3,
                      checkboxGroupInput(inputId = "lakes",
                                         label = "Select one or more lake",
                                         choices = unique(as.character(mysis$Lake)))
                      
               ),
               
               column(9,
                      plotOutput("line1")
                      
               )
               
             ),
             fluidRow(
               conditionalPanel(
                 'input.dataset === "diamonds"',
                 checkboxGroupInput('show_vars', 'Columns in diamonds to show:',
                                    names(diamonds), selected = names(diamonds))
               )
               
             ),
    
    fluidRow(
      column(2,
             checkboxGroupInput("var_names", 'Columns in data to show in table:', 
                                names(mysis), selected = names(mysis))
    ),

      column(10,
              dataTableOutput('mytable1')
            )
             
     
)
)
      
      
      
    
  
  
  
                
                
server <- function(input, output) {
 
  
#make a boxplots of density based on year sampled 
    
  output$hist <- renderPlot({ggplot( mysis[mysis$yr.sampled == input$date,], aes(Lake, ave.den.per.m2))+
      geom_boxplot(fill = "#56B4E9") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle(input$Title) 
  }) 
  
  
  output$boxplot1 <- renderPlot({ggplot( mysis[mysis$season == input$season,], aes(Lake, ave.den.per.m2))+
    geom_boxplot(fill = "#56B4E9") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(input$season)
})  
  
  
  data <- reactive({
    lake <- subset(mysis, Lake %in% c(input$lakes))
    
    ddply(lake, .(yr.sampled, Lake), summarise, mean_den = mean(ave.den.per.m2))
    
    
  })
  
  
  
  output$line1 <- renderPlot({
    
    ggplot(data(), aes(yr.sampled, mean_den, group = Lake, color = Lake)) + 
      geom_point()+
      geom_line() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      xlab("") +
      ylab(bquote('Mean Density ('*ind/m^2*')'))+
      ggtitle("")
    
  })
   
  data2 <- reactive({
    mysis[,c(input$var_names)]
    
    
  })
  output$mytable1 <- renderDataTable({
    data2()
  })
  
}

shinyApp(ui = ui, server = server)

