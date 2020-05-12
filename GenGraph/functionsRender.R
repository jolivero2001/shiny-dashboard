  tooltip_hover <- function(data) {
      if (is.null(data)) return(NULL)
      paste0("x: ", data$x, "<br>", "y: ",data$y)
    }
 
RenderDynamicObjects = function(input,output,session)
{

 output$header <- renderUI({
    h3(Sys.Date()) 
   })

    
   output$plots <- renderUI({ 
     
     do.call(tabsetPanel,
       lapply(1:20, function(j) {
           
          
           plotname <- paste("plot",j, sep="")  
           tabPanel(plotname,ggvisOutput(plotname))
           
           
        })
       )                     
                
     })

   
  observe({

  output$oSource <- renderUI({ 
    lapply(1:length(input$gVariable) , function(i)
     { 
      
      if (is.null(input[[paste0("oVariable",i)]]))
  
      { 
       if (input$gVariable[i] == 'Correlation')
       {
        updateTabItems(session,"tabs",selected="Variables")
        var <- cbind("Color")
        selectInput(inputId=paste0("oVariable",i),label=paste0("Other Variables ",i),c("","Size","Color","Shape","Stroke","Text"),multiple=TRUE,selected=var) 
        
       }
       else
       {
        selectInput(inputId=paste0("oVariable",i),label=paste0("Other Variables ",i),c("","Size","Color","Shape","Stroke","Text"),multiple=TRUE,selected="") 
       }
      } else
      {
       itemSelected<-input[[paste0("oVariable",i)]]
       selectInput(inputId=paste0("oVariable",i),label=paste0("Other Variables ",i),c("","Size","Color","Shape","Stroke","Text"),multiple=TRUE,selected=itemSelected) 
      }
      
                      
    }) 
 
  })
 })
  

  output$tSource <- renderUI({
    lapply(1:length(input$gVariable) , function(i)
     { 
       if (is.null(input[[paste0("filter2Variable",i)]]))
      { 
       selectInput(inputId=paste0("filter2Variable",i),label=paste0("Transformation ",i),c("n","Gathering","Jittering","Scale-X","Scale-Y"),multiple=TRUE,selected="") 
      } else
      {
       itemSelected<-input[[paste0("filter2Variable",i)]]
       selectInput(inputId=paste0("filter2Variable",i),label=paste0("Transformation ",i),c("n","Gathering","Jittering","Scale-X","Scale-Y"),multiple=TRUE,selected=itemSelected) 
      }     
                      
    }) 
 
  })

  output$dSource <- renderUI({
    
    req(input[[paste0("dNumber",1)]])
    if (length(input$gVariable) == 0)
    {
     nSource=1
    }else
    {
     nSource=length(input$gVariable)
    }   
    lapply(1:nSource, function(i)
     { 
      lapply( 1:input[[paste0("dNumber",i)]],function(ind) 
      {

       ndat <-paste0("dat",i)
       ndat <-paste0(ndat,ind)

       
       
       selectInput(inputId=ndat,label=paste0("Data:",i),choices = names(dat.source))
        
     
      })             
     
    }) 
      
  })
  
  output$xSource <- renderUI({
    #req(input$gVariable)
    if (length(input$gVariable) == 0)
    {
     nSource=1
    }else
    {
     nSource=length(input$gVariable)
    }    
    req(input[[paste0("dNumber",1)]])
    lapply(1:nSource, function(i)
    { 
     lapply( 1:input[[paste0("dNumber",i)]],function(ind) 
      { 
        nSource <-paste0("Source",i)
        nSource <-paste0(nSource,ind)

        nData <-paste0("MoodleData",i)
        nData <-paste0(nData,ind) 
        
        ndat <-paste0("dat",i)
        ndat <-paste0(ndat,ind)
        
        req(input[[ndat]])
  
        if (input[[ndat]] == "other")
        {
          fileInput(nSource,'Choose CSV file',accept=c('text/csv', 'text/comma-separated-values,text/plain'))
        } else if (input[[ndat]] == "moodle")
        { 
          selectInput(nData, "Table",
              list("mdl_user","mdl_course","mdl_survey","mdl_chat"),selected=1)
        }      
       
          
      })       
        })
  })

  output$x1Source <- renderUI({
    req(input[[paste0("dNumber",1)]])
    lapply(1:length(input$gVariable), function(i)
    { 
     lapply( 1:input[[paste0("dNumber",i)]],function(ind) 
      { 
        nSource <-paste0("Source",i)
        nSource <-paste0(nSource,ind)

        nData <-paste0("MoodleData",i)
        nData <-paste0(nData,ind) 
        
        ndat <-paste0("dat",i)
         ndat <-paste0(ndat,ind)
        
        req(input[[ndat]])
  
         if (input[[ndat]] == "moodle")
        { 
          selectInput(nData, "Table",
              list("mdl_user","mdl_course","mdl_survey","mdl_chat"),selected=1)
        }      
       
          
      })       
    })  
  })

observe ({
  output$xAxes <- renderUI({   
    lapply(1:length(input$gVariable), function(i)
    { 
     dat.moodle <- dataSourceGeneric(i,1,input) 
      if (!is.null(dat.moodle))
      {
      if ( input$filter1Variable =="Join" )
      {
       dat.moodle <- merge(dat.moodle,dataSourceGeneric(i,2,input),all=TRUE)
      } else if ( input$filter1Variable =="Union" )
      {  
       dat.moodle <- Reduce(function(...) merge(...), list(dat.moodle, dataSourceGeneric(i,2,input)))   
      }  
     if (is.null(input[[paste0("x",i)]]))
     { 
       
      
       selectInput(inputId=paste0("x",i),label=paste0("x",i),choices = names(dat.moodle)) 
      } 
      else
      {
       itemSelected<-input[[paste0("x",i)]]
        selectInput(inputId=paste0("x",i),label=paste0("x",i),choices = names(dat.moodle),selected=itemSelected) 
      }
     }
    }) 
   })
})

observe ({
  output$yAxes <- renderUI({
    lapply(1:length(input$gVariable), function(i)
    {
      dat.moodle <- dataSourceGeneric(i,1,input)
      if (!is.null(dat.moodle))
      { 
      if ( input$filter1Variable =="Join" )
      {
       dat.moodle <- merge(dat.moodle,dataSourceGeneric(i,2,input),all=TRUE)
      } else if ( input$filter1Variable =="Union" )
      {  
       dat.moodle <- Reduce(function(...) merge(...), list(dat.moodle, dataSourceGeneric(i,2,input)))   
      }
      if (is.null(input[[paste0("y",i)]]))
      { 
       if (input$gVariable[i] == 'Correlation')
       {
        selectInput(inputId=paste0("y",i),label=paste0("y",i),choices = names(dat.moodle),selected="Var2") 
       } else
       {  
        selectInput(inputId=paste0("y",i),label=paste0("y",i),choices = names(dat.moodle))
       } 
      } else
      {
       if (input$gVariable[i] == 'Correlation')
       {
        itemSelected<-input[[paste0("y",i)]]
        selectInput(inputId=paste0("y",i),label=paste0("y",i),choices = names(dat.moodle),selected='Var2')
       } 
       else
       {
        itemSelected<-input[[paste0("y",i)]]
        selectInput(inputId=paste0("y",i),label=paste0("y",i),choices = names(dat.moodle),selected=itemSelected) 
        
       }
      }
     }  
    }) 
  })

})

observe ({
  output$fSource <- renderUI({
   #if (any(input$filterVariable == "Filter")) 
   #{    
    lapply(1:length(input$gVariable), function(i)
    {
      selectInput(inputId=paste0("f",i),label=paste0("f",i),multiple = TRUE,choices = names(dataSourceGeneric(i,1,input)),selected=names(dataSourceGeneric(i,1,input)[1]))  
    })
   #} 
  })
})

observe ({
  output$gSource <- renderUI({
    
    lapply(1:length(input$gVariable), function(i)
    {
      selectInput(inputId=paste0("g",i),label=paste0("g",i),multiple = TRUE,choices = names(dataSourceGeneric(i,1,input)),selected=names(dataSourceGeneric(i,1,input)[1]))  
    }) 
  })
  

})
 

observe ({
 
if (any(input$filterVariable == 'Editor'))
{
 output$knitDoc <- renderUI({
    input$eval
    #textAce <-paste0("```{r echo=FALSE, message=FALSE}","\n")
    #textAce <-paste0(textAce,"knitr::opts_chunk$set(fig.width=5,fig.height=4)","\n") 
    #textAce <-paste0(textAce,"library(ggvis)","\n")
    #textAce <-paste0(textAce,"library(dplyr)","\n")
    #textAce <-paste0(textAce,"library(knitr)","\n")   
    #textAce <-paste0(textAce,"```","\n") 
    #textAce <-paste0("```{r,echo=FALSE}","\n")
    textAce <-paste0(input$ace,"\n")
    #textAce <-paste0(textAce,"```")
    #return(HTML(knit2html(text = isolate(textAce), fragment.only = TRUE, quiet = FALSE)))
    return(eval(parse(text = isolate(textAce))))
  })   
} else
{ 
 output$knitDoc <- renderUI({return(NULL)})
}

})




  
####Other Variables####
 

observe ({

  output$notif<-renderUI({
            dropdownMenu(type = "notifications", badgeStatus = "success"
                 
                 )                 
           })

})
observe ({

    if (length(input$gVariable) == 0)
    {
     nSource=1
    }else
    {
     nSource=length(input$gVariable)
    }   
 
    output$dNumber <- renderUI({
      
       lapply(1:nSource, function(i)
      { 
            
         sliderInput(inputId=paste0("dNumber",i),"Source", value=1,min=1,max=20)
      
       })
        
     }) 

})

observe ({
    
     output$Color <- renderUI({        
       lapply(1:length(input$gVariable), function(i)
      { 
        dat.moodle <- dataSourceGeneric(i,1,input) 
        if (!is.null(dat.moodle))
        {
         if (any(input[[paste0("oVariable",i)]] == "Color"))
         {      
          if (is.null(input[[paste0("Color",i)]])) 
          { 
           if (input$gVariable[i] == 'Correlation') 
           {
            selectInput(inputId=paste0("Color",i),label=paste0("Color",i),choices = names(dat.moodle),selected="value") 
           } else
           {  
            
            selectInput(inputId=paste0("Color",i),label=paste0("Color",i),choices = names(dat.moodle)) 
           }
          } else
          {
            
            itemSelected<-input[[paste0("Color",i)]]
            
            selectInput(inputId=paste0("Color",i),label=paste0("Color",i),choices = names(dat.moodle),selected=itemSelected) 
           
          }
         } else 
         {
          selectInput(inputId=paste0("Colorx",i),label=paste0("Color",i),choices =" ") 
         }
        }
       }) 
       })  

})  

observe ({
      output$Size <- renderUI({        
       lapply(1:length(input$gVariable), function(i)
      { 
        dat.moodle <- dataSourceGeneric(i,1,input)
        if (!is.null(dat.moodle))
        {  
        if (any(input[[paste0("oVariable",i)]] == "Size"))
        {      
          if (is.null(input[[paste0("Size",i)]]))
          { 
           if (input$gVariable[i] == 'Correlation')
           {
             selectInput(inputId=paste0("Size",i),label=paste0("Size",i),choices = names(dat.moodle),selected="value") 
           } else
           {    
            selectInput(inputId=paste0("Size",i),label=paste0("Size",i),choices = names(dat.moodle))
           } 
          } else
          {
            itemSelected<-input[[paste0("Size",i)]]
            selectInput(inputId=paste0("Size",i),label=paste0("Size",i),choices = names(dat.moodle),selected=itemSelected) 
          }
        } else 
        {
          selectInput(inputId=paste0("Sizex",i),label=paste0("Size",i),choices ="") 
        }
        }
       })
       })  

})     
       output$Shape <- renderUI({        
       lapply(1:length(input$gVariable), function(i)
      { 
        dat.moodle <- dataSourceGeneric(i,1,input) 
        if (!is.null(dat.moodle))
        {
        if (any(input[[paste0("oVariable",i)]] == "Shape"))
        {      
          if (is.null(input[[paste0("Shape",i)]]))
          {  
            selectInput(inputId=paste0("Shape",i),label=paste0("Shape",i),choices = names(dat.moodle)) 
          } else
          {
            itemSelected<-input[[paste0("Shape",i)]]
            selectInput(inputId=paste0("Shape",i),label=paste0("Shape",i),choices = names(dat.moodle),selected=itemSelected) 
          }
        } else 
        {
          selectInput(inputId=paste0("Shapex",i),label=paste0("Shape",i),choices ="") 
        }
        }
       })
       })    
      
       output$Stroke <- renderUI({        
       lapply(1:length(input$gVariable), function(i)
      { 
        dat.moodle <- dataSourceGeneric(i,1,input) 
        if (!is.null(dat.moodle))
        { 
        if (any(input[[paste0("oVariable",i)]] == "Stroke"))
        {      
          if (is.null(input[[paste0("Stroke",i)]]))
          {  
            selectInput(inputId=paste0("Stroke",i),label=paste0("Stroke",i),choices = names(dat.moodle)) 
          } else
          {
            itemSelected<-input[[paste0("Stroke",i)]]
            selectInput(inputId=paste0("Stroke",i),label=paste0("Stroke",i),choices = names(dat.moodle),selected=itemSelected) 
          }
        } else 
        {
          selectInput(inputId=paste0("Strokex",i),label=paste0("Stroke",i),choices ="") 
        }
        }
       })
       })  

      output$Text  <- renderUI({
       
       lapply(1:length(input$gVariable), function(i)
      { 
         dat.moodle <- dataSourceGeneric(i,1,input)
          
          if ( input$filter1Variable =="Join" )
          {
           dat.moodle <- merge(dat.moodle,dataSourceGeneric(i,2,input),all=TRUE)
          } else if ( input$filter1Variable =="Union" )
          {  
           dat.moodle <- Reduce(function(...) merge(...), list(dat.moodle, dataSourceGeneric(i,2,input)))   
          }    
        if (!is.null(dat.moodle))
        { 
        if (any(input[[paste0("oVariable",i)]] == "Text") || any(input$gVariable[[i]] == "Text"))
        {      
          
           selectInput(inputId=paste0("Text",i),label=paste0("Texting",i),choices = names(dat.moodle)) 
        } else 
        {
          selectInput(inputId=paste0("Textx",i),label=paste0("Text",i),choices ="") 
        }
       }
       })
      })          
      
      
} 

updateDataTable = function(input,output,vDataF,datsumm)
{
       
       vDataT <- vDataF
       req(input$gVariable)
       output$mytabs <- renderUI({
         do.call(tabsetPanel,
          lapply(1:length(input$gVariable), function(j) {
           dat.moodle <- dataSource(j,1,input)                
           names(vDataF[[j]])[1]<-input[[paste0("x",j)]]
           names(vDataF[[j]])[2]<-input[[paste0("y",j)]]
           
           vDataF[[j]]$f1 <- NULL
           vDataF[[j]]$g <- NULL
           vDataF[[j]]$Color <- NULL
           vDataF[[j]]$Size <- NULL
           vDataF[[j]]$Stroke <- NULL
           vDataF[[j]]$Shape <- NULL
           vDataF[[j]]$Text <- NULL

           if (input$filter3Variable == "Table")
           {  
            tabPanel(input$gVariable[j],
                    
            tabPanel(input$gVariable[j],
                    renderDataTable(vDataF[[j]],
             options = list(
                pageLength = 5))))
            } else if (input$filter3Variable == "Summarize")
            {
             tabPanel(input$gVariable[j],
                    
             tabPanel(input$gVariable[j],
                    renderPrint({
                   if (input$gVariable[j] == "Correlation")
                   {
                    corvdata<-cor(dat.moodle[sapply(dat.moodle,is.numeric)]) 
                    print(corvdata) 
                   }else if (input$gVariable[j] == "Anova")
                   {
                     val <- input[[paste0("x",j)]]
                     val <- paste0(val,"~")
                     val <- paste0(val,input[[paste0("y",j)]])
                      
                     out.ANOVA <- summary(aov(formula=y~x,data = datsumm$cur_data()))
                     print(out.ANOVA) 
                   }else if (input$gVariable[j] == "Segmentation")
                   { 
                     clusters <- reactive({
                                  kmeans(datsumm$cur_data(), 3) 
                     })
                     print(clusters()$cluster)
                    
                   }else
                   { 
                    print(summary(dat.moodle))
                   }
              })))
            } 
            
           
          }))
          
           
            
        

         })

         
         
}

updateAnalisys = function(graph,input,output,datsumm,dataS,indLayer)
{
         
         if (graph == 'Anova')
         {
          
          output$plot2 <- renderPlot({
           out.ANOVA <- aov(y ~ x, data = datsumm$cur_data())
           out.TUKEY <- TukeyHSD(out.ANOVA)
           delivery.hsd = data.frame(out.TUKEY$x)
           delivery.hsd$Comparison = row.names(delivery.hsd)

           sp<-ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
            geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
            coord_flip()
   
           print(sp)
          })
         } else if (graph =='Segmentation')
         {
          clusters <- reactive({
                 kmeans(dataS[1:3], 3)
          }) 
          
          output$plot2 <- renderPlot({
          palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
         "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999") )

          par(mar = c(5.1, 4.1, 0, 1))
          
          plot(dataS[1:3],
          col = clusters()$cluster,
          pch = 20, cex = 3)


          points(clusters()$centers, pch=4, cex = 4, lwd = 4)
          
          write.csv(dataS,'./output/data.csv')
          write.csv(clusters()$cluster,'./output/cluster.csv')
          
                
         })
         values<-reactiveValues()
         values[["numvotes"]] <- 1
         output$notif<-renderUI({
            dropdownMenu(type = "notifications", badgeStatus = "warning",
                 notificationItem(icon = icon("ok", lib = "glyphicon"), status = "danger",
                                  "Check ./output/data.csv && ./output/data.csv"
                 )                 )
           })
        } else if (any(input[[paste0("oVariable",indLayer)]] == "Color"))
        {
          
          output$plot2 <- renderPlot({
          if (graph == 'Histogram')
          {
           sp <- ggplot(datsumm$cur_data(),aes(x)) + geom_histogram() + xlab(input[[paste0("x",indLayer)]])
          } else
          {
           sp <- ggplot(datsumm$cur_data(),aes(x,y)) + geom_point() + xlab(input[[paste0("x",indLayer)]]) + ylab(input[[paste0("y",indLayer)]])
          }       
          sp <- sp + facet_wrap( ~ Color) 
          print(sp)           
         })            
        } 
}       