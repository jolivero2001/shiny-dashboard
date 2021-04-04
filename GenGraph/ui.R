library(shinydashboard)
library(ggvis)
library(shiny)
library(dplyr)
library(ggplot2) 
library(DBI)
library(shinyjs)
library(lazyeval)
library(shinyAce)
library(knitr)
library(tidyr)
library(corrplot)
library(ggraph)

 #dm <- dropdownMenu(type="messages")
  mm <- dropdownMenu(type="notifications")
  tm <- dropdownMenu(type="tasks")

  sm <- sidebarMenu(id="tabs", 
  menuItem( 
    text="Set Up",startExpanded = TRUE,icon=icon("dashboard"),
    menuSubItem(text="Plots",tabName="Plots",icon=icon("bar-chart-o")),
    menuSubItem(text="Analytics",tabName="Analytics",icon=icon("bar-chart-o")),
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
      tabName="Plots",

      fluidPage(
       useShinyjs(),
       fluidRow(
         selectInput("gVariable", "Plots:",
                  choices = names(dat.moodle1),
                  multiple = TRUE,
                  selected = "Points")
    
           
    
       )
      )
    ),

    tabItem(
      tabName="Analytics",

      fluidPage(
       useShinyjs(),
       fluidRow(
         selectInput("aVariable", "Analytics:",
                  c("n","Regresion","1way Anova","2way Anova","Correlation","Segmentation"),
                  multiple= FALSE, 
                  selected = "1")   
    
           
    
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
                  c("Table","Summarize"),
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
                  c("n","Logaritmic X","Logaritmic Y","Banking 45"),
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
       
      box(width = 20 ,collapsible=TRUE,
       splitLayout(cellArgs = list(style = "padding: 10px"),
      
      uiOutput("Size"),
      uiOutput("Color"),

      uiOutput("Stroke"), 
      uiOutput("Shape"),

      uiOutput("Text"),

      
      uiOutput("fSource"),
      uiOutput("gSource")
  

     )
     )),

     
     fluidRow(
      useShinyjs(),
      
      box(width = 8,collapsible=TRUE,
      splitLayout(cellWidths = c("100%"),
      tabsetPanel(id="tabs2",
                  type = "pills",
                  tabPanel("Plots", uiOutput("plots")),
                  tabPanel("Analytics", uiOutput("plots2")),
                  tabPanel("Data", uiOutput("summary"))
                  #tabPanel("Editor", htmlOutput("knitDoc"))
                  #tabPanel("Controls",uiOutput("p_ui"))
                   
        )
       )),

                     
      box(width = 4,
      splitLayout(cellArgs = list(style = "padding: 10px"), 
      uiOutput("xAxes"),
      uiOutput("yAxes"),
      uiOutput("Facetx"),
      uiOutput("Factor2")
      )),
      
      box(width = 4,collapsible=TRUE,
        bootstrapPage(
        div( 
        class="container-fluid",
        div(class="row-fluid",
        div(class="span6",             
         aceEditor("ace",mode="markdown",value='')
         ,actionButton("eval", "Update")
        )
        #,
        #div(class="span6",            
        #   htmlOutput("knitDoc")
        #)
    
        ))
      )
     )),

     fluidRow(
      useShinyjs(),

      box(width = 2,
      splitLayout(cellArgs = list(style = "padding: 10px"), 
      uiOutput("p_ui"))),

       
      box(width = 2,
      splitLayout(cellArgs = list(style = "padding: 10px"),
      uiOutput("p_uif")))
     )   
      

     
    ) 
   
    )