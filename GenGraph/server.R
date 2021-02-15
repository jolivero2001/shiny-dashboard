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
library(reshape2)  
library(ggraph)
library(data.tree)
library(manipulate)

  source("functions.R")
  source("functionsRender.R")
  
 

 server <- function(input, output,session) 
{

  ###domains <- reactiveValues(x = c(NA,NA),y = c(NA,NA))
    
 ####lb <- linked_brush(keys = NULL, "blue")  

 ranges <- reactiveValues(x = NULL, y = NULL)
 
 observeEvent(input$plots_dblclick, {
    brush <- input$plots_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

 
 observe ({
   RenderDynamicObjects(input,output,session)
   
 })  
 
 observe ({
   req(input$gVariable)
   updateSliderInput(session,"plotActive",value=input$number)
 })

 observe ({
   req(input$gVariable)
   if (any(input$gVariable == 'Correlation'))
   {
    updateTabItems(session,"tabs",selected="Variable")
   }
})

 observe ({
   controlViewObjects(input,output,session)
 })

 observe ({  

 req(input$gVariable)
  

 vsize <- 0
 vshape <- 0
 line<-NULL 
 line<-paste0(line,"\n")
 

 for (i in 1:input$number) { 
 
 local({ 
  
  my_i <- i   
  
  plotname <- paste("plot",my_i,sep="")
 
  if (input$plotActive == my_i)
  {
   
    
    plot <- paste0("plotname<-","'",sep="") 
    plot <- paste0(plot,plotname,sep="")
    plot <- paste0(plot,"'",sep="") 
   
    
    for (indLayer in 1:length(input$gVariable))
    {
        level<-paste0("indLayer<-",indLayer) 


##
        line<-paste0(line,level,"\n")
##
        graph <- input$gVariable[indLayer]
       
        dataS <-dataSourceGeneric(indLayer,1,input)
##
        line<-paste0(line,'dataS <-dataSourceGeneric(indLayer,1,input)','\n')
##
        if ( input$filter1Variable =="Join" )
        {
          dataS <- merge(dataS,dataSourceGeneric(indLayer,2,input),all=TRUE)       
        } else if ( input$filter1Variable =="Union" )
        { 
         dataS <- Reduce(function(...) merge(...), list(dataS, dataSourceGeneric(indLayer,2,input)))   
        } 

        if (!is.analytics(input$gVariable[indLayer]))
        {   
         vdata<-plotDataGeneric(input[[paste0("x",indLayer)]],input[[paste0("y",indLayer)]],input[[paste0("f",indLayer)]],input[[paste0("g",indLayer)]],input[[paste0("Color",indLayer)]],input[[paste0("Size",indLayer)]],input[[paste0("Stroke",indLayer)]],input[[paste0("Shape",indLayer)]],input[[paste0("Text",indLayer)]],input,dataS,indLayer)
##
         line<-paste0(line,'vdata<-plotDataGeneric(input[[paste0("x",indLayer)]],input[[paste0("y",indLayer)]],input[[paste0("f",indLayer)]],input[[paste0("g",indLayer)]],input[[paste0("Color",indLayer)]],input[[paste0("Size",indLayer)]],input[[paste0("Stroke",indLayer)]],input[[paste0("Shape",indLayer)]],input[[paste0("Text",indLayer)]],input,dataS,indLayer)','\n')
##      
        }
       if(!is.null(vdata))
       { 
                                    
        if (indLayer == 1)
        {       
         datsumm <- ggplot()    
         
        } else if (indLayer == 99999)
        {
         ####datsumm <- datsumm %>% add_props(inherit=FALSE)
         line<- paste0(line,"datsumm <- datsumm %>% add_props(inherit=FALSE)","\n")
# control countables
         if (is.numeric(vDataF[[indLayer-1]]$x) && !is.numeric(vdata$x))
         {
          vdata$x <- as.numeric(as.character(vdata$x))  
           
          line <- paste0(line,'vdata$x <- as.numeric(vdata$x)','\n')          
         } else if (!is.numeric(vDataF[[indLayer-1]]$x) && is.numeric(vdata$x))
         {
          vdata$x <- as.character(vdata$x)
          line <- paste0(line,'vdata$x <- as.character(vdata$x)','\n') 
         }
       
         if (is.numeric(vDataF[[indLayer-1]]$y) && !is.numeric(vdata$y))
         {
          vdata$y <- as.numeric(as.character(vdata$y)) 
          line <- paste0(line,'vdata$y <- as.numeric(vdata$y)','\n')         
         } else if (!is.numeric(vDataF[[indLayer-1]]$y) && is.numeric(vdata$y))
         {
          vdata$y <- as.character(vdata$y)
          line <- paste0(line,'vdata$y <- as.character(vdata$y)','\n')
         }
             

         ####datsumm <- datsumm %>% add_data(vdata)
         line <- paste0(line,'datsumm <- datsumm %>% add_data(vdata)','\n') 
        }

        
                  
        nSel <- 0
        if (any(input$filterVariable == "Filter")) 
        {   
         nSel <- length(input[[paste0("f",indLayer)]])    
        } 

        nSelg <-0  
        if (any(input$filterVariable == "Group+"))
        {
         nSelg <- length(input[[paste0("g",indLayer)]])      
        }  
             
     
        
                                        
     
        if (nSel > 0)
        {  
         
          
         for (index in 1:nSel)   
         { 
          vartemp <- paste0("f",indLayer)
          if (index > 1)
          {
           vartemp <- paste0(vartemp,index)
          } 
          vartemp1 <- paste0("ff",index) 
       
          if (!is.null(input[[vartemp1]]))
          {
           
           clabel <- paste0(input[[vartemp]][index],"-")                           
           cols <- vdata %>% names() 
                                
           filcol <- cols[index+4]                                              
           filters <- quote(eval(as.name(filcol)) %in% input[[vartemp1]])
           #vdata  <- appl_filter(vdata,filters,index,input,indLayer)
           #sfilter_ <- rlang::parse_expr(filters) 
           vdata <- vdata %>% dplyr::filter(!!filters)
           
          }                                                                    
         }

        }

       
       
       if (any(input[[paste0("oVariable",indLayer)]] == "Size"))  
       {
        
        tsize=input[[paste0("Size",indLayer)]]
        datsumm <- datsumm + aes(size=Size)
       }
        
       if (any(input[[paste0("oVariable",indLayer)]] == "Shape")) 
       {
        
        datsumm <- datsumm + aes(shape=Shape)
           
       }

      
       if (any(input[[paste0("oVariable",indLayer)]] == "Color")) 
       {
       
         datsumm <- datsumm + aes(color=Color)   
          
        } 
     
       if (any(input[[paste0("oVariable",indLayer)]] == "Stroke")) 
       {
      
        datsumm <- datsumm + aes(stroke=Stroke)
       }       
       
       
       if (any(input[[paste0("filter2Variable",indLayer)]]  == "Count"))
       {
         datsumm <- datsumm %>% compute_count(~x)
         #datsumm <- datsumm %>% compute_count(~y)
       } 
 
    
   

      if (input$gVariable[indLayer] == 'Points')
      {
        datsumm <- datsumm + geom_point(data=vdata,aes(x = x,y=y),stat="identity",position="identity",alpha=input$alpha.val.points,size=input$size.points)  
        datsumm <- datsumm+ xlab(input[[paste0("x",indLayer)]]) + ylab(input[[paste0("y",indLayer)]]) 
       if (any(input$aVariable == 'Regresion'))
       {
        datsumm <- datsumm + geom_smooth(data=vdata,aes(x = x,y=y),method="lm")
       }  
       #if (input$checkJitter)
       #{
       # datsumm <- datsumm + geom_jitter(alpha = 0.3, color = "tomato")
       #}       
       if (nSelg > 0) 
       {                   
           
           datsumm <- datsumm + geom_path(data=vdata,aes(x=x,y=y,group=g))                       
            
                          
                
        }        
      } else if (input$gVariable[indLayer] == 'Boxplot') 
      {
        if (any(input$aVariable=='2way Anova'))
        {
         vdata$xFacet <- interaction(vdata$x, vdata$Facet)
         datsumm <- datsumm + geom_boxplot(data=vdata,aes(x = xFacet,y=y),alpha=input$alpha.val.Box)
        } else
        {
         
         datsumm <- datsumm + geom_boxplot(data=vdata,aes(x = x,y=y),alpha=input$alpha.val.Box)
        }
        datsumm <- datsumm+ xlab(input[[paste0("x",indLayer)]]) + ylab(input[[paste0("y",indLayer)]])
      } else if (input$gVariable[indLayer] == 'Violin')
      {
        datsumm <- datsumm + geom_violin(data=vdata,aes(x = x,y=y),alpha=input$alpha.val.Violin) 
        datsumm <- datsumm+ xlab(input[[paste0("x",indLayer)]]) + ylab(input[[paste0("y",indLayer)]])
      } else if (input$gVariable[indLayer] == 'Regresion')
      {
        datsumm <- datsumm + geom_point(data=vdata,aes(x = x,y=y),stat="identity",position="identity",alpha=input$alpha.val.points,size=input$size.points) 
        datsumm <- datsumm + geom_smooth(data=vdata,aes(x = x,y=y),method="lm") 
        datsumm <- datsumm+ xlab(input[[paste0("x",indLayer)]]) + ylab(input[[paste0("y",indLayer)]])
      } else if (input$gVariable[indLayer] == "Histogram") 
      {
        
        datsumm <- datsumm + geom_histogram(data=vdata,aes(x = x),stat='count',bins= input$bins.val,alpha=input$alpha.val.his)
        datsumm <- datsumm + xlab(input[[paste0("x",indLayer)]])
        if (input$checkInter)
        {
          datsumm <- datsumm + geom_vline(xintercept = mean(vdata$x), color = "red")
        } 
      } else if (input$gVariable[indLayer] == "Density")
      {
        datsumm <- datsumm + geom_density2d(data=vdata,aes(x = x, y=y),h=0.5,bins=60,alpha=input$alpha.val.density)
      
      } else if (substr(input$gVariable[indLayer],1,5) == "Lines")
      {
        datsumm <- datsumm + geom_line(data=vdata,aes(x = x,y=y,linetype=g),color=input$lColor,size=input$size.lines)
        datsumm <- datsumm+ xlab(input[[paste0("x",indLayer)]]) + ylab(input[[paste0("y",indLayer)]])

      } else if  (substr(input$gVariable[indLayer],1,5) == "Polar")
      {
        datsumm <- datsumm + geom_bar(data=vdata,aes(x = x,y=y),stat="identity") + coord_polar()
        datsumm <- datsumm+ xlab("") + ylab("")
      } else if (input$gVariable[indLayer] == 'Facet.Wrap')
      {
         datsumm <- datsumm + facet_wrap( ~Facet)
      } else if (input$gVariable[indLayer] == 'Facet.Grid')
      {
         datsumm <- datsumm + facet_grid( ~Facet)
      } 
      
       
      
      datsumm <- datsumm + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)                 	     
      
      ###p <- switch(input$ggplot_scaletype,
      ###    normal =
      ###      pc,
      ###    reverse =
      ###      pc + scale_x_reverse() + scale_y_reverse(),
      ###    log10 =
      ###      pc + scale_x_log10() + scale_y_log10(),
      ###    log2 =
      ###      pc + scale_x_continuous(trans = scales::log2_trans()) +
      ###           scale_y_continuous(trans = scales::log2_trans()),
      ###    log10_trans =
      ###      pc + coord_trans(x = "log10", y = "log10"),
      ###    log2_trans =
      ###      pc + coord_trans(x = "log2", y = "log2"),
      ###    coord_cartesian =
      ###      pc + coord_cartesian(xlim = c(2,4), ylim = c(0,50)),
      ###    coord_flip =
      ###      pc + coord_flip(),
      ###    coord_fixed =
      ###      pc + coord_fixed(),
      ###    coord_polar =
      ###      pc + coord_polar(),
          # Discrete x, continuous y
      ###    x_factor =
      ###      pc,
          # Datetime x, Date y
      ###    datetime =
      ###      pc
      ###  )       


      if (any(input$filter4Variable == "Logaritmic X"))
      {
         datsumm <- datsumm + scale_x_log10()
      } else if (any(input$filter4Variable == "Logaritmic Y"))
      {
         datsumm <- datsumm + scale_y_log10()
      } else if (any(input$filter4Variable == "zero in X") && indLayer == 1)
      {
         datsumm <- datsumm %>% scale_numeric("x",domain=c(0,NA))
         line <- paste0(line,'datsumm <- datsumm %>% scale_numeric("x",domain=c(0,NA))','\n')
      } else if (any(input$filter4Variable == "zero in Y") && indLayer == 1)
      {
         datsumm <- datsumm %>% scale_numeric("y",domain=c(NA,0))
         line <- paste0(line,'datsumm <- datsumm %>% scale_numeric("y",domain=c(NA,0))','\n')
      } else if (any(input$filter4Variable == "Banking 45"))
      {
         ratio <- bank_slopes(vdata$x, vdata$y,method="as")
         datsumm <- datsumm + coord_fixed(ratio = ratio)
        
      } else if (any(input$filter4Variable == "Aspect Ratio Auto"))
      {
         datsumm <- datsumm %>% set_options(width="auto",height="auto",resizable=TRUE)
         line <- paste0(line,'datsumm <- datsumm %>% set_options(width="auto", height="auto", resizable=TRUE)','\n')
      } 
             
      plotDataHis[[indLayer]] <- datsumm
      vDataF[[indLayer]] <- vdata
      
     }

    

       
 
     } #  second for 
      
      # Show PLot and Table/Summary
       
      if (!is.null(vdata))
      {

        plotDataHis[[my_i]] <- datsumm
        output[[plotname]]  <- renderPlot({ 
         #tabsetPanel(                       
             #tabPanel("Plots",
              # renderPlot({
                 datsumm
           #})  
## ))
        })        
       
        
        ###print(vDataF)    
        updateDataTable(input,output,vDataF,datsumm)
        updateAnalisys(graph,input,output,datsumm,vDataF,indLayer,session,line)
        
        
        
    #}       
       } 
     
    
    } else
    {
       
       #
    }  
 # end if control  
    
# Analytics by default
   
   
  }) #local

  } #first for
  
                
 }) # end observer
 


  } #end server 