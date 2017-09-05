library(shinydashboard)
library(ggvis)
library(shiny)
library(dplyr)
library(ggplot2)
library(dbConnect)
library(shinyjs)
library(lazyeval)
#library(car)


args <- commandArgs(TRUE)
argu1 <- args[1]
argu2 <- args[2]
profile <- args[3]

dat.source  <- read.delim("dataSource.csv",  header = TRUE, sep = ";")
dat.analytics  <- read.delim("analytics.csv",  header = TRUE, sep = ";")
dat.moodle  <- read.delim("a2moddleV12.csv", header = TRUE, sep = ";")
#dat.moodle  <- read.delim("cursos.csv", header = TRUE, sep = ";")
dat.moodle1 <- read.delim("a2moddleV13.csv", header = TRUE, sep = ";")
#dat.anova <- read.delim("ANOVA.csv",header=TRUE,sep =";")
plotDataHis <- list(20)


listTitle <- matrix(NA,nrow=10,ncol=6,byrow=TRUE)

conn <- dbConnect(drv = RMySQL::MySQL(),dbname = "moodle",host = "localhost",
    username = "root",password = "root")
    on.exit(dbDisconnect(conn), add = TRUE)
    
layer_uned <- function(v,layer,tsize,tfill)  
  {
    
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
      v %>% layer_histograms()
      }) 
           
    } else if (layer == 'KDE')
    {
      v %>% layer_f(function(v) {
      v %>%layer_densities()
      })
 
    } else if (layer == 'Boxplot' || layer == 'Anova')
    {
      v %>% layer_f(function(v) {
      v %>%layer_points() %>% layer_boxplots()
      })
 
    } else if (layer == 'Modeling')
    {
      v %>% layer_f(function(v) {
      v %>%layer_points() %>% 
              layer_smooths(span = input_slider(0.5, 1, 0.5, 0.05))
      })
 
    } else if (layer == 'Regresion')
    {
      v %>% layer_f(function(v) {
      v%>%layer_points() %>% 
              layer_model_predictions(model = "lm",stroke="red") 
      })
 
    } else if (layer == 'Regresion.')
    {
      v %>% layer_f(function(v) {
      v %>%layer_points() %>% 
              layer_model_predictions(model = "lm",stroke="red") %>% 
              layer_smooths(span = input_slider(0.5, 1, 0.5, 0.05)) 
      })
 
    } else if (layer == 'Paths')
    {
      v %>% layer_f(function(v) {
      v%>%layer_paths()  
      })
 
    }
     
        
  }

runApp(
shinyApp(
  ui = dashboardPage(
   
  dashboardHeader(title = profile),
  dashboardSidebar(
    
     
      sliderInput("number","Number of plots", value=1,min=1,max=20)
     ,
      
      sliderInput("plotActive","Plot Active", value=1,min=1,max=20)
           
    ,
     
     
      selectInput("gVariable", "Graphs:",
                  choices = names(dat.moodle1),
                  selected = 1)
    ,
      selectInput("dVariable", "Data Source:",
                  choices = names(dat.source),
                  selected = 1)   
    ,
      
      conditionalPanel(
             condition = "input.user.moodle == true",
              selectInput("MoodleData", "Table",
              list("mdl_user","mdl_course","mdl_survey","mdl_chat"))
       ) ,
       conditionalPanel(
             condition = "input.other == true",
              textInput("Source","Enter the Source File","ANOVA.csv")
       ) 
    ,
      selectInput("AnVariable", "Analytics:",
                  choices = names(dat.analytics),
                  selected = 1) 
    ,
      conditionalPanel(
             condition = "input.user.Anova == true",
              selectInput("AnovaAfter", "After",
              list("Summary","Tukey","Levene"))
       )   
                   
    ,              
      selectInput("filterVariable", "Agregation:",
                  c("n","Filter","Group","Group+"),
                  selected = "n")
    #,

    #selectInput("fVariable", "by:",
    #              choices = names(dat.moodle),
    #              selected = 1)
                  
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
     #,       
     # box(width = 2,
     # selectInput("ffactor", "Factor:",
     #             choices = names(dat.moodle),
     #            selected = 1))
     ),   
     fluidRow(
      useShinyjs(),
      box(width = 2,
       splitLayout(cellWidths = c("100%"),
       uiOutput("p_ui"))),
      box(width = 10,
       splitLayout(cellWidths = c("60%","20%","20%"),
       uiOutput("plots"),           
       plotOutput("plots1"),
       verbatimTextOutput("summary"))
      )
 
     
    )  
  
   
   )
  ),
    
  server= function(input, output,session) 
  {
 
  dataSource <- reactive ({
  
  if (input$dVariable != "moodle")
  {
   hide(id="MoodleData")  
   dat.moodle <- read.delim(input$dVariable, header = TRUE, sep = ";")
  } else
  {
   show(id="MoodleData")
   
   dat.moodle <- dbReadTable(conn = conn,  name = input$MoodleData)
  } 
  
   summary(dat.moodle)
   dat.moodle
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
    return(df)
   }
  })

  plotData1 <- reactive({  
  
 if(any(input$xVariable %in% names(dataSource())) & any(input$yVariable %in% names(dataSource())))
   {   
        
    #df <- dat.moodle[,c(input$xVariable,input$yVariable,input$zVariable,input$hVariable,input$jVariable,input$kVariable,input$ffactor)]
    df1 <- subset(dataSource(), select = c(input$xVariable,input$yVariable))
    names(df1) <- c("x","y")
    return(df1)
   }
  })
 
 

  get_plot_output_list <- function(input_n) 
  {
   
   plot_output_list <-  lapply(1:input_n, function(index) {
         
         plotname <- paste("plot", input_n-(index-1), sep="")
        
          #if (input$gVariable != "Points.Facet")
          #{ 
             ggvisOutput(plotname)
          #} 
          #else
          #{
           #plotOutput(output[[plotname]])
           
          #}
              
       })
   
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
        
return(plot_output_list)
}

             
output$plots <- renderUI({get_plot_output_list(20)})  
#output$plots1 <- renderUI(plotOutput("plots1"))


 observe ({
    
   input$dVariable 
   #updateSelectInput(session,"filterVariable",selected="n")
   

   updateSelectInput(session,"xVariable",choices=names(dataSource()))
   updateSelectInput(session,"yVariable",choices=names(dataSource()))
   updateSelectInput(session,"zVariable",choices=names(dataSource()))
   updateSelectInput(session,"hVariable",choices=names(dataSource()))
   updateSelectInput(session,"jVariable",choices=names(dataSource()))
   updateSelectInput(session,"kVariable",choices=names(dataSource())) 
   #updateSelectInput(session,"fVariable",choices=names(dataSource()))
   #updateSelectInput(session,"ffactor",choices=names(dataSource()))
 
   #updateSelectInput(session,"fVariable",selected=1)
    

})
 
observe ({  
   input$number
   
   # updateSelectInput(session,"filterVariable",selected="n")
   updateSliderInput(session,"plotActive",value=input$number)
    
#})


#observe ({
  
 if (input$filterVariable == "n") 
 {
    hide(id = "p_ui", anim = TRUE)
    
    
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

 if (input$filterVariable == "Filter" || input$gVariable == "Modeling" || input$filterVariable == 'Group+' )
 {
    show(id="p_ui")
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

 

 if (input$gVariable == "Facet.Grid" || input$gVariable == "Facet.Wrap" || input$AnVariable =="Anova" || input$AnVariable =="Segmentacion") 
 {
   
   show(id="plots1")
   
 } else 
 {  
   
   hide(id = "plots1",    anim = TRUE)
 } 
 

  if (input$gVariable == "Points")
  {
    show(id="filterVariable")
    #show(id="fVariable")
    
  } else
  {
    hide(id="filterVariable")
    #hide(id="fVariable") 
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
   #if (input$plotActive == my_i) 
   {
    if(!is.null(plotData()))
    {
     plotDataHis[[my_i]] <- plotData()
     if (input$filterVariable == "Filter") 
     {  
      reactive({ plotDataHis[[my_i]] %>% ggvis(x=~x,y=~y) %>% 
              filter(j >= eval(input_slider(min=min(plotDataHis[[my_i]]$j),max=max(plotDataHis[[my_i]]$j),value=min(plotDataHis[[my_i]]$j),step=1,label=listTitle[my_i,5]))) %>% 
              filter(j <= eval(input_slider(min=min(plotDataHis[[my_i]]$j),max=max(plotDataHis[[my_i]]$j),value=max(plotDataHis[[my_i]]$j),step=1,label=listTitle[my_i,5]))) %>% 
              filter(k %in% eval(input_select(choices=unique(as.character(plotDataHis[[my_i]]$k)),multiple=TRUE,label=listTitle[my_i,6]))) %>%  
              layer_uned(graph,listTitle[my_i,3],listTitle[my_i,4]) %>%                  		
              add_axis("x", title = listTitle[my_i,1]) %>%
              add_axis("y", title = listTitle[my_i,2])  
              #add_legend("fill",title=listTitle[my_i,3]) %>% add_legend("size", orient="left",title=listTitle[my_i,4]) %>%                             
	      #add_tooltip(function(data){data$resp_}, "hover") %>%
              #add_tooltip(function(data ){paste0(my_i, data$plotA)},"click") %>%
              #handle_click(clickFunc)   
            }) %>% bind_shiny(plotname,controls_id="p_ui")

     } else if (input$filterVariable == "Group+")
     {
              reactive({plotDataHis[[my_i]] %>% ggvis(x=~x,y=~y) %>% 
              group_by(j) %>%   
              #filter(z >= eval(input_slider(min=0,max=max(plotDataHis[[my_i]]$z),value=0,step=1,label=listTitle[my_i,3]))) %>% 
              #filter(z <= eval(input_slider(min=0,max=max(plotDataHis[[my_i]]$z),value=max(plotDataHis[[my_i]]$z),step=1,label=listTitle[my_i,3]))) %>%  
              #filter(h %in% eval(input_select(choices=unique(as.character(plotDataHis[[my_i]]$h)),multiple=TRUE,label=listTitle[my_i,4]))) %>%  
              layer_points(fill=~factor(long),size=~h) %>%
              filter(j %in% eval(input_select(choices=unique(as.character(plotDataHis[[my_i]]$j)),multiple=TRUE,label=listTitle[my_i,5]))) %>% 
              layer_paths(stroke=~factor(h)) %>% 
              #layer_model_predictions(model = "lm") %>% 
              #add_legend("fill",title=listTitle[my_i,3]) %>% add_legend("size", orient="left",title=listTitle[my_i,4]) %>%                 		
              add_axis("x", title = listTitle[my_i,1]) %>%
              add_axis("y", title = listTitle[my_i,2]) 
            }) %>% bind_shiny(plotname,controls_id="p_ui")
     

     } else if (input$filterVariable == "n")
     {
             ## reactive({ plotDataHis[[my_i]] %>% ggvis(x=~x,y=~y,opacity := 0) %>%  
              #layer_uned(input$gVariable) %>%
             ## hide_axis("x") %>%
             ##  hide_axis("y")   
            ##}) %>% bind_shiny(plotname)

              reactive({ plotDataHis[[my_i]] %>% ggvis(x=~x,y=~y) %>%  
              #layer_uned(input$gVariable) %>% 
              #layer_points() %>%                  		
              add_axis("x", title = listTitle[my_i,1]) %>%
              add_axis("y", title = listTitle[my_i,2]) %>%
              layer_uned(graph,listTitle[my_i,3],listTitle[my_i,4]) 
              #add_legend("fill",title=listTitle[my_i,3]) %>% 
              #add_legend("size", orient="left",title=listTitle[my_i,4])                              
	      #add_tooltip(function(data){data$resp_}, "hover") 
              #add_tooltip(function(data ){paste0(my_i, data$plotA)},"click")
              #handle_click(clickFunc)   
            }) %>% bind_shiny(plotname,controls_id="p_ui")
            
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
      output$plots1 <- renderPlot({
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
      output$plots1 <- renderPlot({
        sp <- ggplot(plotDataHis[[my_i]],aes(x,y)) + geom_point() + geom_line(aes(x, z))+xlab(input$xVariable) + ylab(input$yVariable)       
        sp <- sp + facet_wrap(j ~ k,labeller=label_both) 
        print(sp)           
      })            
    }
  }
# Analytics
   
   if (input$AnVariable == "Anova")
   {
     if(!is.null(plotData())){
      plotDataHis[[my_i]] <- plotData()
    
      output$plots1 <- renderPlot({
        out.ANOVA <- aov(y ~ x, data = plotDataHis[[my_i]])
        out.TUKEY <- TukeyHSD(out.ANOVA)
        delivery.hsd = data.frame(out.TUKEY$x)
        delivery.hsd$Comparison = row.names(delivery.hsd)

        sp<-ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
            geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
            coord_flip()   
        print(sp)

        
       }) 

      
       if (input$AnovaAfter == 'Summary')
       {
        output$summary = renderPrint({
           out.ANOVA <- summary(aov(y ~ x, data = plotDataHis[[my_i]]))
           print(out.ANOVA)
        })
       } else if (input$AnovaAfter == 'Tukey')
       {
         
        output$summary = renderPrint({
           out.ANOVA <- aov(y ~ x, data = plotDataHis[[my_i]])
           out.TUKEY <- TukeyHSD(out.ANOVA)
           print(out.TUKEY)

           delivery.hsd = data.frame(out.TUKEY$x)
           delivery.hsd$Comparison = row.names(delivery.hsd)

           sp<-ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
            geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
            coord_flip()   
            print(sp)

        })
       } else if (input$AnovaAfter == 'Levene')
       {
          
        output$summary = renderPrint({
           out.Levene <- leveneTest(y ~ x)
           print(out.Levene)
        })
       } 
        
      }
      
      } else if (input$AnVariable == "Segmentacion")
      {
       if(!is.null(plotData1()))
       {
         plotDataHis[[my_i]] <- plotData1()

         clusters <- reactive({
         kmeans(plotData1(), 3)
         })
         output$summary = renderPrint({
          print(clusters()$cluster)
         }) 
         output$plots1 <- renderPlot({
         palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
         "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

         par(mar = c(5.1, 4.1, 0, 1))
         plot(plotData1(),
          col = clusters()$cluster,
          pch = 20, cex = 3)
          points(clusters()$centers, pch = 4, cex = 4, lwd = 4)    
    
         })
       }
         
   }
     
 })
 
 }
               
})

}),launch.browser=TRUE)