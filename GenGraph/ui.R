library(shinydashboard)
library(ggvis)
library(shiny)
library(dplyr)
library(ggplot2) 
library(dbConnect)
library(shinyjs)
library(lazyeval)
library(shinyAce)
library(knitr)
library(tidyr)
library(corrplot)

 #dm <- dropdownMenu(type="messages")
  mm <- dropdownMenu(type="notifications")
  tm <- dropdownMenu(type="tasks")

  sm <- sidebarMenu(id="tabs", 
  menuItem( 
    text="Set Up",startExpanded = TRUE,icon=icon("dashboard"),
    menuSubItem(text="Plot",tabName="Plot",icon=icon("bar-chart-o")),
    menuSubItem(text="Other Variables",icon=icon("th"),tabName="Variables"),
    menuSubItem(text="NPlots",tabName="NPlots",icon=icon("line-chart")),
    menuSubItem(text="Operator",tabName="Operator",icon=icon("users")),
    menuSubItem(text="Selection",tabName="Selection",icon=icon("align-justify")),
    menuSubItem(text="Transformation",tabName="Transformation"),
    menuSubItem(text="View",tabName="View"),
    menuSubItem(text="Guidelines Tool",icon=icon("user"),tabName="Semantic")   
  )
 )  

ui <- dashboardPage(
  dashboardHeader(title = Sys.Date(),mm,tm),
  dashboardSidebar(sm,
   
    tabItems(
    tabItem(
      tabName="Plot",

      fluidPage(
       useShinyjs(),
       fluidRow(
         selectInput("gVariable", "Plot:",
                  choices = names(dat.moodle1),
                  multiple = TRUE,
                  selected = "Points")
    
           
    
       )
      )
    ),
    tabItem(
      tabName="Variables",

      fluidPage(
       useShinyjs(),
       fluidRow(
        uiOutput("oSource")
       )
      )
    ), 
    tabItem(
       tabName="NPlots",

      fluidPage(
       useShinyjs(),
       fluidRow(
         sliderInput("number","Number of plots", value=1,min=1,max=20)
     ,
      
         sliderInput("plotActive","Plot Active", value=1,min=1,max=20)
     
       )
      )
    ),
    
    tabItem(
      tabName="Operator",

      fluidPage(
       useShinyjs(),
       fluidRow(
        
      selectInput("filter1Variable", "Operators:",
                  c("n","Join","Union"),
                  selected = "1")                
       )
      )
    ),
    tabItem(
      tabName="Transformation",

      fluidPage(
       useShinyjs(),
       fluidRow(
        uiOutput("tSource")
       )
      )
    ), 
     tabItem(
      tabName="View",

      fluidPage(
       useShinyjs(),
       fluidRow(
        
      selectInput("filter3Variable", "View:",
                  c("Summarize","Table"),
                  selected = "1")                
       )
      )
    ), 
    tabItem(
      tabName="Selection",

      fluidPage(
       useShinyjs(),
       fluidRow(
         selectInput("filterVariable", "Selection:",
                  c("n","Filter","Group+","Editor"),
                  multiple = TRUE,  
                  selected = "n")
    
       )
      )
    ),
    
    tabItem(
      tabName="Semantic",

      fluidPage(
       useShinyjs(),
       fluidRow(
         selectInput("filter4Variable", "Guidelines:",
                  c("n","Logaritmic X","Logaritmic Y","Remove Legend Color","Remove Legend Size","Remove Legend Stroke","Remove Legend Shape","zero in X","zero in Y","Rate of Change",
                    "Banking 45","Aspect Ratio Auto"),
                  multiple = TRUE,  
                  selected = "n")
    
       )
      )
    )  
   ) 
       
                  
  ),
  dashboardBody(

     

     fluidRow(
     useShinyjs(),
     box(width = 12,collapsible=TRUE,
     box(width = 3,
     uiOutput("dNumber")), 
     box(width = 3,
     uiOutput("dSource")),
     box(width = 3,
     uiOutput("xSource"))),     
     

     
     box(width = 3,collapsible=TRUE,  
     box(width = 6,uiOutput("xAxes")),
     box(width = 6, 
     uiOutput("yAxes"))),
     
     box(width = 6,collapsible=TRUE, 
     box(width = 3,
     uiOutput("Size")),
     box(width = 3,
     uiOutput("Color")),
     box(width = 2,
     uiOutput("Stroke")), 
     box(width = 2,
     uiOutput("Shape")),
     box(width = 2,
     uiOutput("Text"))),
     
     box(width = 3,collapsible=TRUE, 
     box(width = 5,
     uiOutput("fSource")),
     box(width = 5,
     uiOutput("gSource")))),
        
      
     fluidRow(
      useShinyjs(),
      box(width = 2,
       splitLayout(cellWidths = c("100%"),
       uiOutput("p_ui"))),

      box(width = 7,
       splitLayout(cellWidths = c("100%"),
        uiOutput("plots"))),

      box(width = 3,
       splitLayout(cellWidths = c("100%"),
        uiOutput('mytabs')))),

      fluidRow(
      useShinyjs(), 
      box(width = 10,collapsible=TRUE,
       plotOutput("plot2"))),
      

     fluidRow(
        bootstrapPage(
        div( 
        class="container-fluid",
        div(class="row-fluid",
        div(class="span6",             
         aceEditor("ace",value='')
         ,actionButton("eval", "Update")
        )
        ,
        div(class="span6",            
           htmlOutput("knitDoc")
        )
    
        ))
      )
     )
    ) 
   
    )