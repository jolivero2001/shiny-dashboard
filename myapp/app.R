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
#library(car)



args <- commandArgs(TRUE)
profile <- args[1]

dat.source  <- read.delim("dataSource.csv",  header = TRUE, sep = ";")
dat.analytics  <- read.delim("analytics.csv",  header = TRUE, sep = ";")
dat.moodle  <- read.delim("ANOVA.csv", header = TRUE, sep = ";",na.strings=c(""))
dat1.moodle  <- read.delim("ANOVA.csv", header = TRUE, sep = ";",na.strings=c(""))
#dat.moodle  <- read.delim("cursos.csv", header = TRUE, sep = ";")
dat.moodle1 <- read.delim("Plots.csv", header = TRUE, sep = ";",na.strings=c(""))
#dat.anova <- read.delim("ANOVA.csv",header=TRUE,sep =";",na.strings=c(""))
plotDataHis <- list(20)
#datsumm <- ggvis()

listTitle <- matrix(NA,nrow=10,ncol=6,byrow=TRUE)

conn <- dbConnect(drv = RMySQL::MySQL(),dbname = "moodle",host = "localhost",
    username = "root",password = "root")
    on.exit(dbDisconnect(conn), add = TRUE)

clean_plot <-function(output,nPlot)
{

  if (nPlot == 1)
  {
   output$plots <- renderUI({
      print("")
   })

   output$summary = renderPrint({
      print("")
   })

  } else if (nPlot == 2)
  {
   output$plots2 <- renderPlot({
      print("")
   })

   output$summary2 = renderPrint({
      print("")
   })

  } else if (nPlot == 3)
  {
   output$plots3 <- renderPlot({
      print("")
   })

   output$summary3 = renderPrint({
      print("")
   })

  }   
}

   
layer_uned <- function(v,layer,tsize,tfill,vdata)  
  {
    xVar <- vdata$x
    xVar<-na.omit(xVar) 
    yVar <- vdata$y
    yVar<-na.omit(yVar) 

    if (layer == 'Points')
    {
      v %>% layer_f(function(v) {            
       v %>% layer_points(fill=~factor(long),size=~h)       
      }) %>% 
      add_legend("fill",title=tfill) %>% 
      add_legend("size", orient="left",title=tsize) 
      
     
    } else if (layer == 'Histogram')
    {
      v %>% layer_f(function(v) {
         if (is.numeric(xVar))
        {
         v %>% layer_histograms()
          
        } else
        {
         if (is.numeric(yVar))
         {  
          v %>% layer_bars()
         } else
         {
          v %>% layer_points() 
         }  
        }
      }) 
           
    } else if (layer == 'KDE')
    {
     v %>% layer_f(function(v) {
      
      if (length(xVar) < 2 || !is.numeric(xVar))
      {
         v %>% layer_points(fill=~factor(long),size=~h) 
      } else 
      {
      
       v %>%layer_densities(adjust = input_slider(.1, 2, value = 1, step = .1, label = "Bandwidth adjustment"),
        kernel = input_select(
        c("Gaussian" = "gaussian",
          "Epanechnikov" = "epanechnikov",
          "Rectangular" = "rectangular",
          "Triangular" = "triangular",
          "Biweight" = "biweight",
          "Cosine" = "cosine",
          "Optcosine" = "optcosine"),
        label = "Kernel"))       
      }
     })
  
    } else if (layer == 'Boxplot' || layer == 'Anova')
    {
     v %>% layer_f(function(v) {
      if (!is.numeric(yVar) )
      {
       v %>% layer_points(fill=~factor(long),size=~h) 
      } else
      {
       v %>%layer_points() %>% layer_boxplots()
      }
     })
 
    } else if (layer == 'Modeling')
    {
      

      v %>% layer_f(function(v) {
      mod <- try(loess.smooth(xVar, yVar)) 
      if(inherits(mod, "try-error"))
      
      {
       v %>% layer_points(fill=~factor(long),size=~h)  
      }  else
      {     
      
              #layer_smooths(span = input_slider(.8, 3, value = 1, step = .5))
       #v  %>% compute_model_prediction(y ~ x,span = input_slider(.8, 8, value = 1, step = .5),family="gaussian") %>% 
       #   layer_paths(~pred_, ~resp_)  
       v  %>%  layer_smooths(span = input_slider(.8, 8, value = 1, step = .5)) 
          
      }
      })
 
    } else if (layer == 'Regresion')
    {
      v %>% layer_f(function(v) {


      mod <- try(loess.smooth(xVar,yVar))
      
      if(!inherits(mod, "try-error")){
        
          v%>%layer_points(size := input_slider(10, 310, label = "Point size")) %>% 
              #layer_model_predictions(model = "lm", stroke := "red", fill := "red") 
         compute_model_prediction(y ~ x,model = "lm", span = input_slider(.8, 8, value = 1, step = .5),family="gaussian") %>% 
          layer_paths(~pred_, ~resp_)
         
       } else
       {
         
          v %>% layer_points(fill=~factor(long),size=~h) 
         
        }
       

      })
 
    } else if (layer == 'Regresion.')
    {
      v %>% layer_f(function(v) {
       mod <- try(loess.smooth(xVar,yVar))
      
      if(!inherits(mod, "try-error")){
        
          v%>%layer_points() %>% 
              layer_model_predictions(model = "lm",stroke="red") %>% 
              layer_smooths(span = input_slider(.8, 8, value = 1, step = .5)) 
       } else
        {
         
          v %>% layer_points(fill=~factor(long),size=~h) 
         
        }
     
      })
 
    } else if (layer == 'Paths')
    {
      v %>% layer_f(function(v) {
      v%>%layer_paths()  
      })
 
    }
     
        
  }

  #dm <- dropdownMenu(type="messages")
  mm <- dropdownMenu(type="notifications")
  tm <- dropdownMenu(type="tasks")

  sm <- sidebarMenu(
  
  menuItem( 
    text="Set Up",
    menuSubItem(text="Plot",tabName="Plot"),
    menuSubItem(text="NPlots",tabName="NPlots"),
    menuSubItem(text="Operator",tabName="Operator"),
    menuSubItem(text="Agregation",tabName="Agregation")
     
  )
 )  

####runApp(
shinyApp(
  ui = dashboardPage(
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
                  selected = 1)
    
           
    
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
                  selected = "n") 
           
    
       )
      )
    ),
    tabItem(
      tabName="Agregation",

      fluidPage(
       useShinyjs(),
       fluidRow(
         selectInput("filterVariable", "Agregation:",
                  c("n","Filter","Group+","Editor"),
                  selected = "n")
    
       )
      )
    ) 
   ), 
     
     
      selectInput("dVariable", "Data Source:",
                  choices = names(dat.source),
                  selected = 1)   
    ,
      conditionalPanel(
             condition = "input.user.other == true",
             fileInput('Source', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain'))
       ), 
      conditionalPanel(
             condition = "input.user.moodle == true",
              selectInput("MoodleData", "Table",
              list("mdl_user","mdl_course","mdl_survey","mdl_chat"))
             
       ) ,
       
      
      selectInput("d1Variable", "Data Source1:",
                  choices = names(dat.source),
                  selected = 1)   
    ,
      
      

     conditionalPanel(
             condition = "input.user.other == true",
              fileInput('Source1', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain'))
       ) 
      ,


      conditionalPanel(
             condition = "input.user.moodle == true",
              selectInput("MoodleData1", "Table1",
              list("mdl_user","mdl_course","mdl_survey","mdl_chat"))
             
       ) ,
     
      selectInput("AnVariable", "Analytics:",
                  choices = names(dat.analytics),
                  selected = 1) 
    ,
      conditionalPanel(
             condition = "input.user.Anova == true",
              selectInput("AnovaAfter", "After",
              list("Tukey"))
       )   
                   
                  
  ),
  dashboardBody(

   

     fluidRow(
     useShinyjs(),
     box(width = 2,
     selectInput("xVariable", "Axe X Variable:",
                  choices = names(dat.moodle),                                    
                  selected = 1))
     ,
     box(width = 2,     
      selectInput("yVariable", "Axe Y Variable:",
                  choices = names(dat.moodle),
                  selected = 1))
      ,  
      

      box(width = 2,       
          
      selectInput("zVariable", "Color Variable:",
                  choices = names(dat.moodle),
                  selected = 1))
     ,

      box(width = 2,
      selectInput("hVariable", "Size Variable:",
                  choices = names(dat.moodle),
                 selected = 1))
     
    ,
      
      box(width = 2,
      selectInput("jVariable", "Facet Row (j):",
                  choices = names(dat.moodle),
                  selected = 1))
     ,
      box(width = 2,
      selectInput("kVariable", "Facet Col (k):",
                  choices = names(dat.moodle),
                 selected = 1))
     
     )
     ,   
      fluidRow(
     useShinyjs(),
     box(width = 2,
     selectInput("x1Variable", "Axe X Variable:",
                  choices = names(dat1.moodle),                                    
                  selected = 1)),
     box(width = 2,     
      selectInput("y1Variable", "Axe Y Variable:",
                  choices = names(dat1.moodle),
                  selected = 1))
     
     
      
      )
     
     ,   
     fluidRow(
      useShinyjs(),
      box(width = 2,
       splitLayout(cellWidths = c("100%"),
       uiOutput("p_ui"))),
      box(width = 6,
       splitLayout(cellWidths = c("100%"),
       tabsetPanel(type = "tabs",
                  tabPanel("Plots", uiOutput("plots")),
                  tabPanel("Plot-2", plotOutput("plots2")),
                  tabPanel("EDITOR", plotOutput("plots3"))
        )
       )),
                  
       box(width = 4,
       splitLayout(cellWidths = c("100%"),
       tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Summary-2", verbatimTextOutput("summary2")),
                  tabPanel("Editor", verbatimTextOutput("summary3")),  
                  tabPanel("Data", tableOutput("table"))
       )))  
       
      
    ),
          fluidRow(
       bootstrapPage(
        div(
        class="container-fluid",
        div(class="row-fluid",
        div(class="span6", 
            
            aceEditor("ace",mode="markdown",value=' 
            ```{r} 
                  output$summary3 = renderPrint({
                  print(plotData())
                  }) 
                  output$plots2 <- renderPlot({
                  if (is.numeric(plotData()$x))
                  {
                   hist(plotData()$x)
                  } 
                  })

            ``` 
            '),
            actionButton("eval", "Update")
          ),
        div(class="span6",
            
           htmlOutput("knitDoc"))
    
  ))
      )
)
     
   
   )
  ),
    
  server= function(input, output,session) 
  {

   output$header <- renderUI({
    h3(Sys.Date())
  })
   
  dataSource <- reactive ({

  #hide(id="MoodleData1") 
  #hide(id="Source1")
   
  if (input$dVariable != "moodle")
  { 
   hide(id="MoodleData") 
   
   if (input$dVariable == 'other')
   {
     show(id="Source")
     
     dataFile <- input$Source$datapath
     if (!is.null(dataFile)) 
     {
      dat.moodle <- read.delim(dataFile, header = TRUE, sep = ";",na.strings=c("")) 
     }
     
    
   } else
   {
     
     hide(id="Source")  
     dat.moodle <- read.delim(input$dVariable, header = TRUE, sep = ";",na.strings=c(""))
     
   } 
  } else
  {
   show(id="MoodleData")  
   hide(id="Source")  
   
   dat.moodle <- dbReadTable(conn = conn,  name = input$MoodleData)
  } 
 
  if ( input$filter1Variable =="Join" )
   {
     dat.moodle <- merge(dat.moodle,dataSource1(),all=TRUE)
   }
 
   summary(dat.moodle)
   dat.moodle
})

  dataSource1 <- reactive ({
  
  if (input$d1Variable != "moodle")
  {
   hide(id="MoodleData1") 
   if (input$d1Variable == 'other')
   {
     show(id="Source1")
     
     dataFile <- input$Source1$datapath
     if (!is.null(dataFile)) 
     {
      dat1.moodle <- read.delim(dataFile, header = TRUE, sep = ";",na.strings=c(""))
     }
       
   
   } else
   { 
    hide(id="Source1")   
    dat1.moodle <- read.delim(input$d1Variable, header = TRUE, sep = ";",na.strings=c(""))
    
   }
  } else
  {
   show(id="MoodleData1")
   hide(id="Source1") 
   dat1.moodle <- dbReadTable(conn = conn,  name = input$MoodleData1)
  } 
  
  
   #summary(dat1.moodle)
   dat1.moodle
})
   
  plotData <- reactive({  
  
 if(any(input$xVariable %in% names(dataSource())) & any(input$yVariable %in% names(dataSource())) 
  & any(input$zVariable %in% names(dataSource())) & any(input$hVariable %in% names(dataSource())) 
  & any(input$jVariable %in% names(dataSource()))
  & any(input$kVariable %in% names(dataSource()))) 
  #& any(input$fVariable %in% names(dataSource())))
  #& any(input$ffactor   %in% names(dataSource())))
   {   
        
    #df <- dat.moodle[,c(input$xVariable,input$yVariable,input$zVariable,input$hVariable,input$jVariable,input$kVariable,input$ffactor)]
    df <- subset(dataSource(), select = c(input$xVariable,input$yVariable,input$zVariable,input$hVariable,input$jVariable,
                                          input$kVariable))
    names(df) <- c("x","y","z","h","j","k")
    df$long = as.character(paste0(df$z," ",input$zVariable))
    df$plotA <- input$number
    df$gPlot <- input$gVariable
   
   if ( input$filter1Variable =="Join" )
   {
     df <- merge(df,plotData1(),all=TRUE)
   } else if ( input$filter1Variable =="Union" )
   {
     df <- Reduce(function(...) merge(...), list(df, plotData1()))
   }
    
    return(df)
   }
  }) 

  plotData1 <- reactive({  
  
 if(any(input$x1Variable %in% names(dataSource1())) & any(input$y1Variable %in% names(dataSource1()))) 
  
   {   
        
   
    df1 <- subset(dataSource1(), select = c(input$x1Variable,input$y1Variable))
    names(df1) <- c("x1","y1")
    
   
    return(df1)
   }
  })

  
 

  get_plot_output_list <- function(input_n) 
  {
   plot_output_list <-  lapply(1:input_n, function(index) {
         
         plotname <- paste("plot", input_n-(index-1), sep="")
        
           
             ggvisOutput(plotname)
         
              
       })
   
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
        
return(plot_output_list)
}

output$plots <- renderUI({get_plot_output_list(20)}) 

observe ({
input$number

 
updateSliderInput(session,"plotActive",value=input$number)
})             


observe ({
if (input$filterVariable == 'Editor')
{
 output$knitDoc <- renderUI({
    input$eval
    return(isolate(HTML(knit2html(text = input$ace, fragment.only = TRUE, quiet = TRUE))))
  })  
} else
{
 output$knitDoc <- renderUI({return(NULL)})
}

})

 observe ({
    
   ###notificationItem(input$gVariable)
  
   input$dVariable 
    
   #updateSelectInput(session,"filterVariable",selected="n")
   

   updateSelectInput(session,"xVariable",choices=names(dataSource()))
   updateSelectInput(session,"yVariable",choices=names(dataSource()))
   updateSelectInput(session,"zVariable",choices=names(dataSource()))
   updateSelectInput(session,"hVariable",choices=names(dataSource()))
   updateSelectInput(session,"jVariable",choices=names(dataSource()))
   updateSelectInput(session,"kVariable",choices=names(dataSource())) 
   #updateSelectInput(session,"x1Variable",choices=names(dataSource()))
   #updateSelectInput(session,"y1Variable",choices=names(dataSource())) 
})

 observe ({  
   input$d1Variable 
   #updateSelectInput(session,"filterVariable",selected="n")
   

   updateSelectInput(session,"x1Variable",choices=names(dataSource1()))
   updateSelectInput(session,"y1Variable",choices=names(dataSource1())) 

})
 
#observe ({  
#   input$number
   
   # updateSelectInput(session,"filterVariable",selected="n")

    
#})


observe ({

 if (input$filter1Variable == "Join" || input$filter1Variable == "Union") 
 {
   #hide(id = "p_ui", anim = TRUE)
   show(id="d1Variable")
   show(id="x1Variable")
   show(id="y1Variable")

 } else
 {
   hide(id="d1Variable")
   hide(id="x1Variable")
   hide(id="y1Variable")
 }  

  
 if (input$filterVariable == "Editor") 
 {
   show(id="ace")
   show(id="eval")

 } else
 {
   clean_plot(output,3)
   hide(id="ace")
   hide(id="eval")
 } 
  
 if (input$filterVariable == "n") 
 { 
    #hide(id = "p_ui", anim = TRUE)
    
    
    updateTextInput(session, "jVariable", 
                    label = "Facet Row (j):")
    updateTextInput(session, "kVariable", 
                    label = "Facet Col (k):") 

 } else 
 {
    
     
    
    updateTextInput(session, "jVariable", 
                    label = "Filter by:")
    updateTextInput(session, "kVariable", 
                    label = "Filter by:") 
 }  

  if (input$filterVariable == "Filter" || input$gVariable == "Modeling" 
    || input$filterVariable == 'Group+' || input$gVariable == "Regresion" 
    || input$gVariable =="Regresion." || input$gVariable == "KDE")
 {
    show(id="p_ui")
 } else
 {
    hide(id = "p_ui", anim = TRUE)
 }
   

 if (input$gVariable == "Facet.Grid" || input$gVariable == "Facet.Wrap" || input$filterVariable == "Filter") 
 {
   show(id="jVariable")
   show(id="kVariable")

 } else if (input$filterVariable == 'Group+') 
 {
     show(id="jVariable")
     updateTextInput(session, "jVariable", 
                    label = "Group by:") 
     hide(id = "kVariable", anim = TRUE)
   
 } else 
 {  
   hide(id = "jVariable", anim = TRUE)    
   hide(id = "kVariable", anim = TRUE)
 } 

 

  if (input$gVariable == "Points")
  {
    #show(id="filterVariable")
    show(id="zVariable")
    show(id="hVariable")
    
  } else
  {
    #hide(id="filterVariable")
    hide(id="zVariable") 
    hide(id="hVariable") 
  }  

   if (input$AnVariable == "Anova")
  {
    updateTextInput(session, "xVariable", 
                    label = "factor")
    updateTextInput(session, "yVariable", 
                    label = "muestra")
    #show(id="ffactor")
     show("AnovaAfter") 
  } else
  {
    hide("AnovaAfter")
    updateTextInput(session, "xVariable", 
                    label = "Axe X Variable:")
    updateTextInput(session, "yVariable", 
                    label = "Axe y Variable:")
    #hide(id="ffactor") 
  }   

 if(is.null(plotData())) return



 for (i in 1:input$number) { 
   
 
 local({  
  my_i <- i
  graph <- input$gVariable
  
  
  plotname <- paste("plot",my_i,sep="") 

  
  listTitle[my_i,1]<-input$xVariable
  listTitle[my_i,2]<-input$yVariable 
  listTitle[my_i,3]<-input$zVariable
  listTitle[my_i,4]<-input$hVariable
  listTitle[my_i,5]<-input$jVariable 
  listTitle[my_i,6]<-input$kVariable 

  #keys <- left_right(0,1,step=0.5)
  

  if (input$gVariable != "Facet.Grid" && input$gVariable != "Facet.Wrap")
  {
   if (input$plotActive == my_i || input$plotActive == 1)
   {
    if(!is.null(plotData()))
    {
     plotDataHis[[my_i]] <- plotData()
     
     if (input$filterVariable == "Filter") 
     {  
      datsumm <-reactive({plotDataHis[[my_i]] %>% ggvis(x=~x,y=~y) %>% 
              filter(j >= eval(input_slider(min=min(as.numeric(plotDataHis[[my_i]]$j)),max=max(as.numeric(plotDataHis[[my_i]]$j)),value=min(as.numeric(plotDataHis[[my_i]]$j)),step=1,label=listTitle[my_i,5]))) %>% 
              filter(j <= eval(input_slider(min=min(as.numeric(plotDataHis[[my_i]]$j)),max=max(as.numeric(plotDataHis[[my_i]]$j)),value=max(as.numeric(plotDataHis[[my_i]]$j)),step=1,label=listTitle[my_i,5]))) %>% 
              filter(k %in% eval(input_select(choices=unique(plotDataHis[[my_i]]$k),multiple=TRUE,selected=plotDataHis[[my_i]]$k,label=listTitle[my_i,6]))) %>%  
              ##scale_numeric("y",trans=eval(input_select(c("linear","log"),label="Scale"))) %>% 
              layer_uned(graph,listTitle[my_i,3],listTitle[my_i,4],plotDataHis[[my_i]]) %>%                  		
              add_axis("x", title = listTitle[my_i,1]) %>%
              add_axis("y", title = listTitle[my_i,2])  
              #add_legend("fill",title=listTitle[my_i,3]) %>% add_legend("size", orient="left",title=listTitle[my_i,4]) %>%                             
	      #add_tooltip(function(data){data$resp_}, "hover") %>%
              #add_tooltip(function(data ){paste0(my_i, data$plotA)},"click") %>%
              #handle_click(clickFunc)  
                
            }) 

            datsumm %>% bind_shiny(plotname,controls_id="p_ui")

     } else if (input$filterVariable == "Group+")
     {
              datsumm <-reactive({plotDataHis[[my_i]] %>% ggvis(x=~x,y=~y) %>% 
              group_by(j) %>%
              #auto_group(exclude=c("x", "y")) %>%  
              #layer_points(fill=~factor(long),size=~h) %>%
              #scale_numeric("y",trans=input_select(c("linear","log"),label="Scale")) %>%
              layer_uned(graph,listTitle[my_i,3],listTitle[my_i,4],plotDataHis[[my_i]]) %>% 
              filter(j %in% eval(input_select(choices=unique(plotDataHis[[my_i]]$j),multiple=TRUE,selected=plotDataHis[[my_i]]$j,label=listTitle[my_i,5]))) %>% 
              layer_paths(stroke=~factor(h)) %>% 
              #layer_model_predictions(model = "lm") %>% 
              #add_legend("fill",title=listTitle[my_i,3]) %>% add_legend("size", orient="left",title=listTitle[my_i,4]) %>%                 		
              add_axis("x", title = listTitle[my_i,1]) %>% 
              add_axis("y", title = listTitle[my_i,2]) 
            }) %>% bind_shiny(plotname,controls_id="p_ui")
     
              datsumm %>% bind_shiny(plotname,controls_id="p_ui")
     } else 
     {
             ## reactive({ plotDataHis[[my_i]] %>% ggvis(x=~x,y=~y,opacity := 0) %>%  
              #layer_uned(input$gVariable) %>%
             ## hide_axis("x") %>%
             ##  hide_axis("y")   
            ##}) %>% bind_shiny(plotname)

             datsumm <- reactive({plotDataHis[[my_i]] %>% ggvis(x=~x,y=~y) %>%  
              #layer_uned(input$gVariable) %>% 
              #layer_points() %>%                  		
              add_axis("x", title = listTitle[my_i,1]) %>%
              add_axis("y", title = listTitle[my_i,2]) %>%
              #scale_numeric("y",trans=eval(input_select(c("linear","log"),selected="linear",label="Scale"))) %>%
              #scale_numeric("y",trans="log") %>%
              layer_uned(graph,listTitle[my_i,3],listTitle[my_i,4],plotDataHis[[my_i]]) 
              #add_legend("fill",title=listTitle[my_i,3]) %>% 
              #add_legend("size", orient="left",title=listTitle[my_i,4])                              
	      #add_tooltip(function(data){data$resp_}, "hover") 
              #add_tooltip(function(data ){paste0(my_i, data$plotA)},"click")
              #handle_click(clickFunc)   
            }) 
            datsumm %>% bind_shiny(plotname,controls_id="p_ui")
     }
     
    }  
   }
   
   
 } else if (input$gVariable == "Facet.Grid")
    {
    if(!is.null(plotData())){
    updateSliderInput(session,"plotActive",value=1)
    updateSliderInput(session,"number",value=1) 
    
    
      my_i <- 1
      plotname <- paste("plots",my_i,sep="")           
      plotDataHis[[my_i]] <- plotData()  
        
      output$plots2 <- renderPlot({
        sp <- ggplot(plotDataHis[[my_i]],aes(x,y)) + geom_point() + geom_line(aes(x, z))+xlab(input$xVariable) + ylab(input$yVariable)       
        sp <- sp + facet_grid(j ~ k,labeller=label_both) 
        print(sp)           
      })            
     } 
   } else if (input$gVariable == "Facet.Wrap")
   {
    if(!is.null(plotData())){
     updateSliderInput(session,"plotActive",value=1)
     updateSliderInput(session,"number",value=1) 
    
    
      my_i <- 1
      plotname <- paste("plots",my_i,sep="")           
      plotDataHis[[my_i]] <- plotData()
         
      output$plots2 <- renderPlot({
        sp <- ggplot(plotDataHis[[my_i]],aes(x,y)) + geom_point() + geom_line(aes(x, z))+xlab(input$xVariable) + ylab(input$yVariable)       
        sp <- sp + facet_wrap(j ~ k,labeller=label_both) 
        print(sp)           
      })            
    }
  }



  if (input$gVariable != 'Facet.Wrap' && input$gVariable != 'Facet.Grid')
  {
   
    
   output$summary <- renderPrint({
                  
                   
                    print(summary(datsumm()$cur_data()$x))
                    print(summary(datsumm()$cur_data()$y))
                   
                  
            })
  }
  output$table <- renderTable(datsumm()$cur_data()) 

# Analytics by default

   if (input$AnVariable == "Regresion" || input$AnVariable == "Regresion.")
   {
     if(!is.null(plotData())){
      #plotDataHis[[my_i]] <- datsumm()$cur_data()
      if (is.numeric(datsumm()$cur_data()$x))
      {
       axis.x = scale(datsumm()$cur_data()$x, center=TRUE, scale=FALSE)
       mod1 = lm(span=0.8,formula = datsumm()$cur_data()$y ~ axis.x, data = datsumm()$cur_data())
       mod2 = lm(span=0.8,formula = log(datsumm()$cur_data()$y) ~ axis.x, data = datsumm()$cur_data())  
       output$summary2 = renderPrint({
         print(mod1)
         print(mod2)
      
     })
     output$plots2 <- renderPlot({
      print("")
     })
     }    
      
     } 
   } 
   
   if (input$AnVariable == "Anova")
   {
     if(!is.null(plotData()))
     {
      #plotDataHis[[my_i]] <- plotData()
      #plotDataHis[[my_i]] <- datsumm()$cur_data() 
      output$plots2 <- renderPlot({
        out.ANOVA <- aov(y ~ x, data = datsumm()$cur_data())
        out.TUKEY <- TukeyHSD(out.ANOVA)
        delivery.hsd = data.frame(out.TUKEY$x)
        delivery.hsd$Comparison = row.names(delivery.hsd)

        sp<-ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
            geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
            coord_flip()   
        print(sp)

        
       }) 

      
       
        output$summary = renderPrint({
           out.ANOVA <- summary(aov(y ~ x, data = datsumm()$cur_data()))
           print(out.ANOVA)
        })


       if (input$AnovaAfter == 'Tukey')
       {
         
        output$summary2 = renderPrint({
           out.ANOVA <- aov(y ~ x, data = datsumm()$cur_data())
           out.TUKEY <- TukeyHSD(out.ANOVA)
           print(out.TUKEY)

           delivery.hsd = data.frame(out.TUKEY$x)
           delivery.hsd$Comparison = row.names(delivery.hsd)

           sp<-ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
            geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
            coord_flip()   
            print(sp)

        })
       } 
        
      }
      
     } else if (input$AnVariable == "Segmentacion")
     {
       if(!is.null(datsumm()$cur_data()))
       {
         #plotDataHis[[my_i]]$x <- datsumm()$cur_data()$x
         #plotDataHis[[my_i]]$y <- datsumm()$cur_data()$y

         clusters <- reactive({
         kmeans(datsumm()$cur_data(), 3)
         })
         output$summary2 = renderPrint({
          print(clusters()$cluster)
         }) 
         output$plots2 <- renderPlot({
         palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
         "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

         par(mar = c(5.1, 4.1, 0, 1))
         plot(datsumm()$cur_data() ,
          col = clusters()$cluster,
          pch = 20, cex = 3)
          points(clusters()$centers, pch = 4, cex = 4, lwd = 4)    
    
         })
       }
         
     }
     
  })
 
 }
                  
})

})
###)
#,launch.browser=TRUE)