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
library(reshape2)  


  source("functions.R")
  source("functionsRender.R")
  
 

 server <- function(input, output,session) 
{

  domains <- reactiveValues(x = c(NA,NA),y = c(NA,NA))
    
 lb <- linked_brush(keys = NULL, "blue")  

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
   req(input$gVariable)
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
   
  if (input$plotActive == my_i)
  {
   
    plotname <- paste("plot",my_i,sep="")
    plot <- paste0("plotname<-","'",sep="") 
    plot <- paste0(plot,plotname,sep="")
    plot <- paste0(plot,"'",sep="") 
    line<-paste0(line,plot,"\n")
    
    for (indLayer in 1:length(input$gVariable))
    {
        level<-paste0("indLayer<-",indLayer) 
        line<-paste0(line,level,"\n")
        graph <- input$gVariable[indLayer]
        line<-paste0(line,"graph <- input$gVariable[indLayer]","\n")
        dataS <-dataSourceGeneric(indLayer,1,input)
        line<-paste0(line,'dataS <-dataSourceGeneric(indLayer,1,input)','\n')
        if ( input$filter1Variable =="Join" )
        {
          dataS <- merge(dataS,dataSourceGeneric(indLayer,2,input),all=TRUE)       
        } else if ( input$filter1Variable =="Union" )
        { 
         dataS <- Reduce(function(...) merge(...), list(dataS, dataSourceGeneric(indLayer,2,input)))   
        } 
           
        vdata<-plotDataGeneric(input[[paste0("x",indLayer)]],input[[paste0("y",indLayer)]],input[[paste0("f",indLayer)]],input[[paste0("g",indLayer)]],input[[paste0("Color",indLayer)]],input[[paste0("Size",indLayer)]],input[[paste0("Stroke",indLayer)]],input[[paste0("Shape",indLayer)]],input[[paste0("Text",indLayer)]],input,dataS,indLayer)

        line<-paste0(line,'vdata<-plotDataGeneric(input[[paste0("x",indLayer)]],input[[paste0("y",indLayer)]],input[[paste0("f",indLayer)]],input[[paste0("g",indLayer)]],input[[paste0("Color",indLayer)]],input[[paste0("Size",indLayer)]],input[[paste0("Stroke",indLayer)]],input[[paste0("Shape",indLayer)]],input[[paste0("Text",indLayer)]],input,dataS,indLayer)','\n')

       if(!is.null(vdata))
       {                               
        if (indLayer == 1)
        {
         line<-paste0(line,'datsumm <- list(1) ','\n') 
         datsumm <-  vdata  
         line<-paste0(line,'datsumm <- vdata','\n')       
         datsumm <- datsumm %>%  ggvis(fill.brush:='red') 
         line<-paste0(line,"datsumm <- datsumm %>%  ggvis(fill.brush:='red',na.rm=TRUE)","\n") 
             
         
        } else
        {
         datsumm <- datsumm %>% add_props(inherit=FALSE)
         line<- paste0(line,"datsumm <- datsumm %>% add_props(inherit=FALSE)","\n")
# control countables
         if (is.numeric(vDataF[[indLayer-1]]$x) && !is.numeric(vdata$x))
         {
          vdata$x <- as.numeric(vdata$x)  
          line <- paste0(line,'vdata$x <- as.numeric(vdata$x)','\n')          
         } else if (!is.numeric(vDataF[[indLayer-1]]$x) && is.numeric(vdata$x))
         {
          vdata$x <- as.character(vdata$x)
          line <- paste0(line,'vdata$x <- as.character(vdata$x)','\n') 
         }
       
         if (is.numeric(vDataF[[indLayer-1]]$y) && !is.numeric(vdata$y))
         {
          vdata$y <- as.numeric(vdata$y) 
          line <- paste0(line,'vdata$y <- as.numeric(vdata$y)','\n')         
         } else if (!is.numeric(vDataF[[indLayer-1]]$y) && is.numeric(vdata$y))
         {
          vdata$y <- as.character(vdata$y)
          line <- paste0(line,'vdata$y <- as.character(vdata$y)','\n')
         }
             

         datsumm <- datsumm %>% add_data(vdata)
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
             
     
        if (nSelg > 0) 
        {  
        
         for (index in 1:nSelg)
         {                   
           levelg<-paste0("index<-",index) 
           line<-paste0(line,levelg,"\n") 
           clabel  <- input[[paste0("g",indLayer)]][index]             
           datsumm <- datsumm %>% group_by(g)                       
           line <- paste0(line,'datsumm <- datsumm %>% group_by(g)','\n')    
                          
         }
               
        }                                   
     
        if (nSel > 0)
        {  
         
          
         for (index in 1:nSel)   
         {  
           vartemp <- paste0("f",indLayer)
           clabel <- paste0(input[[vartemp]][index],"-")
           clabel2 <- paste0(clabel,vartemp) 
           levelf<-paste0("index<-",index) 
           line<-paste0(line,levelf,"\n") 
           line <- paste0(line,'clabel2 <- paste0(paste0(input[[paste0("f",indLayer)]][index],"-"),paste0("f",indLayer))','\n')                          
           cols <- vdata %>% names() 
           line <- paste0(line,'cols <- vdata %>% names()','\n')                      
           filcol <- cols[index+2] 
           line <- paste0(line,'filcol <- cols[index+2]','\n')  
           #line <- paste0(line,'vdata %>% names()[index+2]','\n')
                                              
           datsumm   <-  datsumm %>% filter(UQ(as.name(filcol)) %in% eval(input_select(choices=unique(vdata[[index+2]]),multiple=TRUE,selected=vdata[[index+2]],label=clabel2)))
           line <- paste0(line,'datsumm <- datsumm %>% filter(UQ(as.name(filcol)) %in% eval(input_select(choices=unique(vdata[[index+2]]),multiple=TRUE,selected=vdata[[index+2]],label=clabel2)))','\n')               
                                                             
         }

        }

       
       
       if (any(input[[paste0("oVariable",indLayer)]] == "Size") && has_Size(graph)) 
       {
        
        tsize=input[[paste0("Size",indLayer)]]
        datsumm <- datsumm %>% group_by(Size)
        line <- paste0(line,'datsumm <- datsumm %>% group_by(Size)','\n') 
        datsumm <- datsumm %>% add_props(size=~Size)
        line <- paste0(line,'datsumm <- datsumm %>% add_props(fill=~factor(Size))','\n')
        if (any(input$filter4Variable != 'Remove Legend Size')) 
        {  
         datsumm <- datsumm %>% add_legend("size",title=tsize)
         line <- paste0(line,'datsumm <- datsumm %>% add_legend("size",title=input[[paste0("Size",indLayer)]])','\n')   
        } else
        {
         datsumm <- datsumm %>% hide_legend("size")
         line <- paste0(line,'datsumm <- datsumm %>% hide_legend("size")','\n')
        } 
       } 
  
       if (any(input[[paste0("oVariable",indLayer)]] == "Shape") && has_Shape(graph)) 

       {
        
        tshape=input[[paste0("Shape",indLayer)]]  
        datsumm <- datsumm %>% add_props(shape=~factor(Shape))
        line <- paste0(line,'datsumm <- datsumm %>% add_props(shape=~factor(Shape))','\n') 
        if (any(input$filter4Variable != 'Remove Legend Shape')) 
        {  
         datsumm <- datsumm %>% add_legend("shape",orient='left',title=tshape)
         line <- paste0(line,'datsumm <- datsumm %>% add_legend("shape",orient="left",title=input[[paste0("Shape",indLayer)]])','\n')    
        } else
        {
         datsumm <- datsumm %>% hide_legend("shape")
         line <- paste0(line,'datsumm <- datsumm %>% hide_legend("shape")','\n')
        }   
        
           
       }

      
       if (any(input[[paste0("oVariable",indLayer)]] == "Color")  && has_Color(graph))
       {
        #nColor <-length(input[[paste0("Color",indLayer)]])
       
         
         tfill=input[[paste0("Color",indLayer)]]
         
         datsumm <- datsumm %>% group_by(Color) 
         line <- paste0(line,'datsumm <- datsumm %>% group_by(Color)','\n') 
         datsumm <- datsumm %>% add_props(fill=~factor(Color)) 
         line <- paste0(line,'datsumm <- datsumm %>% add_props(fill=~factor(Color))','\n') 
          
         if (any(input$filter4Variable == 'Remove Legend Color')) 
         {  
            datsumm <- datsumm %>% hide_legend("fill") 
            line <- paste0(line,'datsumm <- datsumm %>% hide_legend("fill")','\n')   
           } else
           {
                 
             datsumm <- datsumm %>% add_legend("fill",orient='left',title=tfill) 
             line <- paste0(line,'datsumm <- datsumm %>% add_legend("fill",orient="left",title=input[[paste0("Color",indLayer)]])','\n')   
          }          
          
        } 
     
       if (any(input[[paste0("oVariable",indLayer)]] == "Stroke") && has_Stroke(graph))
       {
        tfill=input[[paste0("Stroke",indLayer)]] 
        
        datsumm <- datsumm %>% add_props(stroke=~factor(Stroke),strokeWidth:=input_slider(1,5,value=2,step=1,label='Width')) 
        line <- paste0(line,'datsumm <- datsumm %>% add_props(stroke=~factor(Stroke),strokeWidth:=input_slider(1,5,value=2,step=1,label="Width"))','\n')
         datsumm <- datsumm %>% group_by(Stroke) 
         line <- paste0(line,'datsumm <- datsumm %>% group_by(Stroke)','\n') 
         if (any(input$filter4Variable == 'Remove Legend Stroke')) 
         {  
          datsumm <- datsumm %>% hide_legend("stroke") 
          line <- paste0(line,'datsumm <- datsumm %>% hide_legend("stroke")','\n')     
         } else
         {
          datsumm <- datsumm %>% add_legend("stroke",orient='right',title=tfill)
          line <- paste0(line,'datsumm <- datsumm %>% add_legend("stroke",orient="right",title=input[[paste0("Stroke",indLayer)]])','\n')  
         }
       }       
       
       
       if (any(input[[paste0("filter2Variable",indLayer)]]  == "Count"))
       {
         datsumm <- datsumm %>% compute_count(~x)
         #datsumm <- datsumm %>% compute_count(~y)
       } 
 
    
     zoom_brush = function(items, session, page_loc, plot_loc, ...) {
       show_tooltip(session, page_loc$r + 5, page_loc$t, html = nrow(items))
       }
       
     

        
       #datsumm_new <- reactive({
       #print('pasa')
       # print(domains[["x"]][1]) 
        ##domains[["x"]][1] <-0
            ##domains[["x"]][2] <-7500

    #   if (anyNA(domains$x))
     #   datsumm
     #  else
     #  {
        
      #  datsumm[datsumm$x >= domains[["x"]][1] & datsumm$x <= domains[["x"]][2]]
      #  print('pasa2')
        
       #} 
       #})
       
      
      listTitle[my_i,1] <- input[[paste0("x",indLayer)]]    
      listTitle[my_i,2] <- input[[paste0("y",indLayer)]]

       
       
      datsumm <- datsumm %>% layer_generic(graph,vdata,indLayer,input,session) %>%                         	     
      set_options(width="auto",height="auto",resizable=TRUE) %>% 
      
      handle_brush(function(items, session, page_loc, plot_loc, ...) {show_tooltip(session, page_loc$r + 5, page_loc$t, html = nrow(items))  }) %>% add_tooltip(tooltip_hover,"hover")   
       
      line <- paste0(line,'datsumm <- datsumm %>% layer_generic(graph,vdata,indLayer,input,session) %>%set_options(width="auto",height="auto",resizable=TRUE) %>% handle_brush(function(items, session, page_loc, plot_loc, ...) {show_tooltip(session, page_loc$r + 5, page_loc$t, html = nrow(items))}) %>% add_tooltip(tooltip_hover,"hover")','\n')

      datsumm <- datsumm %>% add_axis("x",title = listTitle[my_i,1]) %>%
                             add_axis("y",title = listTitle[my_i,2]) 
                                  
      line <- paste0(line,'datsumm <- datsumm %>% add_axis("x",title = input[[paste0("x",indLayer)]] ) %>%
                             add_axis("y",title = input[[paste0("y",indLayer)]] )','\n')
 
      if (any(input$filter4Variable == "Logaritmic X") && indLayer == 1)
      {
         datsumm <- datsumm %>% scale_numeric("x",trans="log", expand=0)
         line <- paste0(line,'datsumm <- datsumm %>% scale_numeric("x",trans="log", expand=0)','\n')
      } else if (any(input$filter4Variable == "Logaritmic Y") && indLayer == 1)
      {
         datsumm <- datsumm %>% scale_numeric("y",trans="log", expand=0)
         line <- paste0(line,'datsumm <- datsumm %>% scale_numeric("y",trans="log", expand=0)','\n') 
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
         datsumm <- datsumm %>% set_options(width=400, height=600, keep_aspect=TRUE)
         line <- paste0(line,'datsumm <- datsumm %>% set_options(width=400, height=600, keep_aspect=TRUE)','\n')
      } else if (any(input$filter4Variable == "Aspect Ratio Auto"))
      {
         datsumm <- datsumm %>% set_options(width="auto",height="auto",resizable=TRUE)
         line <- paste0(line,'datsumm <- datsumm %>% set_options(width="auto", height="auto", resizable=TRUE)','\n')
      } 
             
      plotDataHis[[indLayer]] <- datsumm
      vDataF[[indLayer]] <- vdata
      line <- paste0(line,'vDataF[[indLayer]] <- vdata ','\n')
     }

    

       

     } #  second for 
      
      # Show PLot and Table/Summary
  
      if (!is.null(vdata))
      {

        
        datsumm %>% bind_shiny(plotname,controls_id="p_ui")  
    
        line<-paste0(line,'datsumm %>% bind_shiny(plotname,controls_id="p_ui")','\n')       
        updateDataTable(input,output,vDataF,datsumm)
        
        line<-paste0(line,"updateDataTable(input,output,vDataF,datsumm)",'\n')
        updateAnalisys(graph,input,output,datsumm,dataS,indLayer)
        
        line<-paste0(line,"updateAnalisys(graph,input,output,datsumm,dataS,indLayer)",'\n') 
         
        #if (input$ace == "")
        #{  
        updateAceEditor(session,'ace',mode='r',value=line)
         
        #}
        
       
    #}       
       } 
     
    
    } 
 # end if control  
    
# Analytics by default
   
   
  }) #local

  } #first for
  
                
 }) # end observer
 


  } #end server 