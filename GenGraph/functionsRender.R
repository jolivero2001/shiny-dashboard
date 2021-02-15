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
     
     ##do.call(tabsetPanel,
       plot_list <- lapply(1:input$number, function(j) {
               
           plotname <- paste("plot",j, sep="")  
           #tabPanel(plotname,plotOutput(plotname))
           plotOutput(plotname,dblclick = "plots_dblclick",brush = brushOpts(id = "plots_brush",resetOnNew = TRUE))
           
           
        })
       #)                     
       do.call(tagList, plot_list)         
     })
   
  observe({

  output$oSource <- renderUI({ 
    lapply(1:length(input$gVariable) , function(i)
     { 
      
      if (is.null(input[[paste0("oVariable",i)]]))
  
      { 
       
        selectInput(inputId=paste0("oVariable",i),label=paste0("Other Variables ",i),c("","Size","Color","Shape","Stroke","Text"),multiple=TRUE,selected="") 
       
      } else
      {
       itemSelected<-input[[paste0("oVariable",i)]]
       selectInput(inputId=paste0("oVariable",i),label=paste0("Other Variables ",i),c("","Size","Color","Shape","Stroke","Text"),multiple=TRUE,selected=itemSelected) 
      }
      
                      
    }) 
 
  })

  

  output$tSource <- renderUI({
    lapply(1:length(input$gVariable) , function(i)
     { 
       if (is.null(input[[paste0("filter2Variable",i)]]))
      { 
       selectInput(inputId=paste0("filter2Variable",i),label=paste0("Transformation ",i),c("n","Gathering","Scale-X","Scale-Y"),multiple=TRUE,selected="") 
      } else
      {
       itemSelected<-input[[paste0("filter2Variable",i)]]
       selectInput(inputId=paste0("filter2Variable",i),label=paste0("Transformation ",i),c("n","Gathering","Scale-X","Scale-Y"),multiple=TRUE,selected=itemSelected) 
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
              choices=names(dat.tables),selected=1)
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
              choices=names(dat.tables),selected=1)
        }      
       
          
      })       
    })  
  })
})

observe ({
  output$xAxes <- renderUI({
      
    lapply(1:length(input$gVariable), function(i)
    { 
     dat.moodle <- dataSourceGeneric(i,1,input) 
      if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
      {
      if (input$filter1Variable =="Join" )
      {
       dat.moodle <- merge(dat.moodle,dataSourceGeneric(i,2,input),all=TRUE)
      } else if ( input$filter1Variable =="Union" )
      {  
       dat.moodle <- Reduce(function(...) merge(...), list(dat.moodle, dataSourceGeneric(i,2,input)))   
      } 
     xlabel<- paste0("x",i)
     controlLabels(input,xlabel,xlabel1) 
     if (is.null(input[[paste0("x",i)]]))
     { 
       
      
       selectInput(inputId=paste0("x",i),label=paste0(xlabel,input$gVariable[i]),choices = names(dat.moodle)) 
      } 
      else
      {
       itemSelected<-input[[paste0("x",i)]]
        selectInput(inputId=paste0("x",i),label=paste0(xlabel,input$gVariable[i]),choices = names(dat.moodle),selected=itemSelected) 
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
      if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
      { 
      if ( input$filter1Variable =="Join" )
      {
       dat.moodle <- merge(dat.moodle,dataSourceGeneric(i,2,input),all=TRUE)
      } else if ( input$filter1Variable =="Union" )
      {  
       dat.moodle <- Reduce(function(...) merge(...), list(dat.moodle, dataSourceGeneric(i,2,input)))   
      }
      ylabel<- paste0("y",i) 
      if (is.null(input[[paste0("y",i)]]))
      { 
       if (input$gVariable[i] == 'Correlation')
       {
        selectInput(inputId=paste0("y",i),label=paste0(ylabel,input$gVariable[i]),choices = names(dat.moodle),selected="Var2") 
       } else
       {  
        selectInput(inputId=paste0("y",i),label=paste0(ylabel,input$gVariable[i]),choices = names(dat.moodle))
       } 
      } else
      {
       if (input$gVariable[i] == 'Correlation')
       {
        itemSelected<-input[[paste0("y",i)]]
        selectInput(inputId=paste0("y",i),label=paste0(ylabel,input$gVariable[i]),choices = names(dat.moodle),selected='Var2')
       } 
       else
       {
        itemSelected<-input[[paste0("y",i)]]
        selectInput(inputId=paste0("y",i),label=paste0(ylabel,input$gVariable[i]),choices = names(dat.moodle),selected=itemSelected) 
        
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
     dat.moodle <- dataSourceGeneric(i,1,input)
     if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
     { 
      selectInput(inputId=paste0("f",i),label=paste0("filtering",i),multiple = TRUE,choices = names(dataSourceGeneric(i,1,input)),selected=names(dataSourceGeneric(i,1,input)[1]))
     }   
    })
   #} 
  })
})

observe ({
  output$gSource <- renderUI({
    
    lapply(1:length(input$gVariable), function(i)
    {
     dat.moodle <- dataSourceGeneric(i,1,input) 
     if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
     { 
       selectInput(inputId=paste0("g",i),label=paste0("grouping",i),multiple = TRUE,choices = names(dataSourceGeneric(i,1,input)),selected=names(dataSourceGeneric(i,1,input)[1]))  
     }
    }) 
  })
  

})
 

observe ({
  output$Facetx <- renderUI({
     
     if (any(substr(input$gVariable,1,5) == 'Facet'))
     {
      lapply(1:length(input$gVariable), function(i)
      {
       if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
       { 
        #if (is.null(input[[paste0("Facet",i)]]))
        #{        
          selectInput(inputId=paste0("Facet",i),label="Facet",multiple = TRUE,choices = names(dataSourceGeneric(1,1,input)),selected=names           (dataSourceGeneric(i ,1,input)[1]))
        #} else
        #{
        #  itemSelected<-input[[paste0("Facet",i)]]
        #  selectInput(inputId=paste0("Facet",i),label="Facet",multiple = TRUE,choices = names(dataSourceGeneric#(1,1,input)),selected=itemSelected) 
        #}
      }
     })
    }
  })
})

observe ({
  output$Factor2 <- renderUI({
     
     if (any(input$aVariable == '2way Anova'))
     {
      #lapply(1:length(input$gVariable), function(i)
      #{
      # if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
      # { 
        #if (is.null(input[[paste0("Facet",i)]]))
        #{        
          selectInput(inputId="Factor2",label="Factor 2",multiple = TRUE,choices = names(dataSourceGeneric(1,1,input)),selected=names(dataSourceGeneric(1,1,input)[1]))
        #} else
        #{
        #  itemSelected<-input[[paste0("Facet",i)]]
        #  selectInput(inputId=paste0("Facet",i),label="Facet",multiple = TRUE,choices = names(dataSourceGeneric#(1,1,input)),selected=itemSelected) 
        #}
      #}
     #})
    }
  })
  
}) 

observe ({
 
if (any(input$filterVariable == 'Editor'))
{
 output$knitDoc <- renderUI({
    textAce <-paste0("```{r echo=FALSE, message=FALSE}","\n")
    textAce <-paste0(textAce,"knitr::opts_chunk$set(fig.width=5,fig.height=4)","\n") 
    textAce <-paste0(textAce,"library(ggvis)","\n")
    textAce <-paste0(textAce,"library(dplyr)","\n")
    textAce <-paste0(textAce,"library(knitr)","\n")   
    #textAce <-paste0(textAce,"```","\n") 
    #textAce <-paste0("```{r,echo=FALSE}","\n")
    textAce <-paste0(textAce,input$ace,"\n")
    textAce <-paste0(textAce,"```","\n")
    #return(knit2html(text = isolate(textAce), fragment.only = TRUE, quiet = FALSE))
    return(HTML(knit2html(text = isolate(textAce), fragment.only = TRUE, quiet = FALSE)))
    #return(eval(parse(text = isolate(textAce))))
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

   req(input$gVariable)
     
    output$p_uif <- renderUI({
     lapply(1:length(input$gVariable), function(indLayer)
     {
      if (!is.analytics(input$gVariable[indLayer])) 
      { 
      if (any(input$filterVariable == "Filter"))  
      { 
        nSel <- length(input[[paste0("f",indLayer)]])    
      }
      if (nSel > 0)
      { 
       Flist <- lapply(1:nSel, function(index)  
       {   
        vartemp <- paste0("f",indLayer)
        if (index > 1)
        {
          #vartemp <- paste0(vartemp,index)
        } 
        vartemp1 <- paste0("ff",index)
        clabel <- paste0(input[[vartemp]][index]," filter")
        #clabel2 <- paste0(clabel,vartemp) 
                 
        vdatas <- dataSourceGeneric(indLayer,1,input)                                          
        vdatax<-plotDataGeneric(input[[paste0("x",indLayer)]],input[[paste0("y",indLayer)]],input[[paste0("f",indLayer)]],input[[paste0("g",indLayer)]],input[[paste0("Color",indLayer)]],input[[paste0("Size",indLayer)]],input[[paste0("Stroke",indLayer)]],input[[paste0("Shape",indLayer)]],input[[paste0("Text",indLayer)]],input,vdatas,indLayer)                                      

        selectInput(inputId=vartemp1,choices=unique(vdatax[[index+4]]),multiple=TRUE,selected=unique(vdatax[[index+4]]),label=clabel)
       })
       do.call(tagList, Flist) 
      }
      }
     })
   })      
})


observe ({   

   req(input$gVariable)
     
    output$p_ui <- renderUI({
     lapply(1:length(input$gVariable), function(i)
     { 
      if (input$gVariable[i] == "Histogram")
       {      
         bins<-sliderInput(inputId="bins.val","bins", value=1,min=1,max=20)
         alpha.his<-sliderInput(inputId="alpha.val.his","alpha.his",value = .5, min=.1, max=1, step = .5)
         checkInter <- checkboxInput(inputId="checkInter","Intercep",value=FALSE)
         lists<-list(bins=bins,alpha.his=alpha.his,checkInter=checkInter)
          
         do.call(tagList, lists) 
  
      } else if (substr(input$gVariable[i],1,6)  == "Points") 
      {
         alpha.points<-sliderInput(inputId="alpha.val.points","alpha.points",value = 1, min=.1, max=1, step = .5)
         size.points<-sliderInput(inputId="size.points","size.points",value = 2, min=2, max=5, step = .5)
         checkJitter <- checkboxInput(inputId="checkJitter","Jitter",value=FALSE)
         lists<-list(alpha.points=alpha.points,size.points=size.points,checkJitter=checkJitter) 
         do.call(tagList, lists) 
   
      
        
      }else if (input$gVariable[i]  == "Density")
      {
         sliderInput(inputId="alpha.val.density","alpha.density",value = 1, min=.1, max=1, step = .5)
  
      } else if (input$gVariable[i] == "Boxplot")   
      { 
         sliderInput(inputId="alpha.val.Box","alpha.boxplot",value = 1, min=.1, max=1, step = .5) 
 
      } else if (input$gVariable[i] == "Violin")   
      { 
         sliderInput(inputId="alpha.val.Violin","alpha.violin",value = 1, min=.1, max=1, step = .5) 
 
      } else if (substr(input$gVariable[i],1,5)  == "Lines")
      {
        #lineType<-selectInput( inputId="lineType",label="LineType",c("solid","dashed","dotted"),selected="solid")
        lColor<-selectInput(inputId="lColor",label="Color Line",c("red","green","blue","black"),selected="black")
        size.lines<-sliderInput(inputId="size.lines","size.lines",value = 1, min=1, max=5, step = .5) 
        lists<-list(lColor=lColor,size.lines=size.lines) 
        do.call(tagList, lists)    
      } else 
      {
        return(NULL)
      }  
   
     })
 }) 

})
 
observe ({
    
     output$Color <- renderUI({        
       lapply(1:length(input$gVariable), function(i)
      {
        dat.moodle <- dataSourceGeneric(i,1,input) 
        if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
        {
         if (any(input[[paste0("oVariable",i)]] == "Color"))
         {      
          if (is.null(input[[paste0("Color",i)]])) 
          { 
           
            
            selectInput(inputId=paste0("Color",i),label=paste0("Color ",input$gVariable[i]),choices = names(dat.moodle)) 
           
          } else
          {
            
            itemSelected<-input[[paste0("Color",i)]]
            
            selectInput(inputId=paste0("Color",i),label=paste0("Color ",input$gVariable[i]),choices = names(dat.moodle),selected=itemSelected) 
           
          }
         } else 
         {
          selectInput(inputId=paste0("Color",i),label=paste0("Color ",input$gVariable[i]),choices =" ") 
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
        if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
        {  
        if (any(input[[paste0("oVariable",i)]] == "Size"))
        {      
          if (is.null(input[[paste0("Size",i)]]))
          { 
           
            selectInput(inputId=paste0("Size",i),label=paste0("Size ",input$gVariable[i]),choices = names(dat.moodle))
            
          } else
          {
            itemSelected<-input[[paste0("Size",i)]]
            selectInput(inputId=paste0("Size",i),label=paste0("Size ",input$gVariable[i]),choices = names(dat.moodle),selected=itemSelected) 
          }
        } else 
        {
          selectInput(inputId=paste0("Size",i),label=paste0("Size ",input$gVariable[i]),choices ="") 
        }
        }
       })
       })  

})     
       output$Shape <- renderUI({        
       lapply(1:length(input$gVariable), function(i)
      { 
        dat.moodle <- dataSourceGeneric(i,1,input) 
        if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
        {
        if (any(input[[paste0("oVariable",i)]] == "Shape"))
        {      
          if (is.null(input[[paste0("Shape",i)]]))
          {  
            selectInput(inputId=paste0("Shape",i),label=paste0("Shape ",input$gVariable[i]),choices = names(dat.moodle)) 
          } else
          {
            itemSelected<-input[[paste0("Shape",i)]]
            selectInput(inputId=paste0("Shape",i),label=paste0("Shape ",input$gVariable[i]),choices = names(dat.moodle),selected=itemSelected) 
          }
        } else 
        {
          selectInput(inputId=paste0("Shape",i),label=paste0("Shape ",input$gVariable[i]),choices ="") 
        }
        }
       })
       })    
      
       output$Stroke <- renderUI({        
       lapply(1:length(input$gVariable), function(i)
      { 
        dat.moodle <- dataSourceGeneric(i,1,input) 
        if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
        { 
        if (any(input[[paste0("oVariable",i)]] == "Stroke"))
        {      
          if (is.null(input[[paste0("Stroke",i)]]))
          {  
            selectInput(inputId=paste0("Stroke",i),label=paste0("Stroke ",input$gVariable[i]),choices = names(dat.moodle)) 
          } else
          {
            itemSelected<-input[[paste0("Stroke",i)]]
            selectInput(inputId=paste0("Stroke",i),label=paste0("Stroke ",input$gVariable[i]),choices = names(dat.moodle),selected=itemSelected) 
          }
        } else 
        { 
          selectInput(inputId=paste0("Stroke",i),label=paste0("Stroke ",input$gVariable[i]),choices ="") 
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
        if (!is.null(dat.moodle) && !is.analytics(input$gVariable[i]))
        { 
        if (any(input[[paste0("oVariable",i)]] == "Text") || any(input$gVariable[[i]] == "Text"))
        {      
          
           selectInput(inputId=paste0("Text",i),label=paste0("Texting ",input$gVariable[i]),choices = names(dat.moodle)) 
        } else 
        {
          selectInput(inputId=paste0("Textx",i),label=paste0("Text ",input$gVariable[i]),choices ="") 
        }
       }
       })
      })          
      
      
} 

updateDataTable = function(input,output,vDataF,datsumm)
{
       
       vDataT <- vDataF
        req(input$gVariable)
        
       output$summary <- renderUI({
         do.call(tabsetPanel,
          lapply(1:length(input$gVariable), function(j) {
           dat.moodle <- dataSource(j,1,input)
                    
           names(vDataF[[j]])[1]<-input[[paste0("x",j)]]
           names(vDataF[[j]])[2]<-input[[paste0("y",j)]]
           #vDataF[[j]]$Facet <- NULL
           #vDataF[[j]]$Factor2<- NULL
           #vDataF[[j]]$f <- NULL
           #vDataF[[j]]$g <- NULL
            
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
                   
                    print(summary(dat.moodle))
                   
              })))
            } 
            
           
          }))
          
           
            
        

         })

         
         
}

regresionEditor<-function(line)
{
  line<-paste0(line,'out.lm <- lm( y ~ x, data = vdata)','\n')
  line<-paste0(line,'print(summary(out.lm))','\n') 
  #line<-paste0(line,'out.ANOVA <- anova(out.lm)','\n')
  #line<-paste0(line,'print(out.lm)','\n') 
  line<-paste0(line, 'f<-summary(out.lm)$fstatistic','\n')  
  line<-paste0(line,'if (f[1] > 1) print("Model fits properly")','\n') 
  line<-paste0(line,'p <- pf(f[1],f[2],f[3],lower.tail=F)','\n')
  line<-paste0(line,'attributes(p)<-NULL','\n')  
  line<-paste0(line,'if (p < 0.05)  print("HO is rejected and")','\n')   
  line<-paste0(line,'r<-summary(out.lm)$r.squared','\n')
  line<-paste0(line,'if (r < 1)  print("")','\n')
 
  line<-paste0(line,'print("Normality")','\n')  
  line<-paste0(line,'shapiro.test(rstudent(out.lm))','\n')
  line<-paste0(line,'out.ANOVA <- aov(y ~ x, data = vdata)','\n')
  line<-paste0(line,'ggplot(out.ANOVA, aes(sample=out.ANOVA$residuals)) + stat_qq(size = 2, color = "blue") +                      stat_qq_line(size=1.25,color="red")','\n') 
  line<-paste0(line,'print("Homocedasticity")','\n')
  line<-paste0(line,'bptest(out.lm)','\n')
  line<-paste0(line,'plot(out.lm$residuals)','\n')
  return(line)  
}


updateAnalisys = function(graph,input,output,datsumm,dataS,indLayer,session,line)
{
       ###req(input$gVariable)
                         

       output$plots2 <- renderUI({
        #do.call(tabsetPanel,
         #lapply(1:length(input$gVariable), function(j) {
         lapply(1:1, function(j) { 
          
        #for (j in 1:length(input$gVariable))
        #{ 
         dat.moodle <- dataSource(j,1,input)
         if (any(input$aVariable == 'Regresion') && input$gVariable[j] == 'Points')
         {
          line<-regresionEditor(line)
          updateAceEditor(session,'ace',value=line)  
          tabsetPanel(
             tabPanel("Regresion Summary",
               renderPrint({
                out.ANOVA <- lm( y ~ x, data = dataS[[j]] )
                summary( out.ANOVA )
              })),

             tabPanel("Regresion Multiple Summary",
               renderPrint({
                out.ANOVA <- lm( y ~ ., data = dataS[[j]] )
                summary( out.ANOVA )
              })),
             
             tabPanel("Analisys ANOVA",
               renderPrint({ 
                  out.ANOVA <- lm( y ~ x, data = dataS[[j]] )
                  anova(out.ANOVA)
              })),

             tabPanel("Summary Normality Test",
               renderPrint({ 
                  out.ANOVA <- lm( y ~ x, data = dataS[[j]] )
                  out.ANOVAR<-anova(out.ANOVA)
                  shapiro.test(rstudent( out.ANOVA))
              })),
           
             tabPanel("Normality Test",
               renderPlot({ 
                  out.ANOVA <- aov(y ~ x, data = dataS[[j]])
                  g1<-qqnorm(out.ANOVA$residuals) 
                  g2<-qqline(out.ANOVA$residuals)
                  ggplot(dataS[[j]], aes(sample=y)) +
                       stat_qq(size = 2, color = "blue") + 
                       stat_qq_line(size=1.25, color="red") 
              })),

              tabPanel("Summary Homocedasticity",
                 renderPrint({
                   out.ANOVA <- lm( y ~ x, data = dataS[[j]] )
                   bptest(out.ANOVA)
              })),


              tabPanel("Homocedasticity",
                 renderPlot({
                    out.ANOVA <- lm( y ~ x, data = dataS[[j]] )
                   out.ANOVAR<-anova(out.ANOVA)
                   plot(out.ANOVA$residuals)
              })),
              
              tabPanel("Report",
                 renderUI({
                   textAce <-paste0("```{r echo=FALSE, message=FALSE}","\n")
                   textAce <-paste0(textAce,"knitr::opts_chunk$set(fig.width=5,fig.height=4)","\n") 
                   textAce <-paste0(textAce,"library(ggvis)","\n")
                   textAce <-paste0(textAce,"library(dplyr)","\n")
                   textAce <-paste0(textAce,"library(knitr)","\n")   
    #textAce <-paste0(textAce,"```","\n") 
    #tex tAce <-paste0("```{r,echo=FALSE}","\n")
    textAce <-paste0(textAce,input$ace,"\n")
                   textAce <-paste0(textAce,"```","\n")
    #return(knit2html(text = isolate(textAce), fragment.only = TRUE, quiet = FALSE))
                   return(HTML(knit2html(text = isolate(textAce), fragment.only = TRUE, quiet = FALSE)))
                 })
                 ),

              tabPanel(" ",
               renderPrint({
                
                print(" ")
              }))
          )
         } else if (any(input$aVariable == '1way Anova') && (input$gVariable[j] == 'Boxplot' || input$gVariable[j] == 'Violin' || input$gVariable[j] == 'Points'))
         {
          
            
            tabsetPanel( 

             tabPanel("Tree",
               renderPrint({
                dat.moodle$pathString <- do.call(paste,c(dat.moodle,sep="/"))
                df.igraph <- data.tree::as.Node(dat.moodle)        
                plot(df.igraph)
                print(df.igraph)
                 
              })),

             tabPanel("1way Anova",
               renderPrint({
                out.ANOVA <- summary(aov(formula=y ~ x,data = dataS[[j]]))
                print(out.ANOVA)
              })),

              tabPanel("Summary-Tukey",
               renderPrint({
                out.ANOVA <- aov(y ~ x, data = dataS[[j]])
                out.TUKEY <- TukeyHSD(out.ANOVA) 
                print(out.TUKEY)
              })),          
    
             tabPanel("Tukey",
               renderPlot({
                 out.ANOVA <- aov(y ~ x, data = dataS[[j]])
                 out.TUKEY <- TukeyHSD(out.ANOVA)
                 delivery.hsd = data.frame(out.TUKEY$x)
                 delivery.hsd$Comparison = row.names(delivery.hsd)
                 #plot(delivery.hsd,las=1 , col="brown")
 
                 sp<-ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
                     geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
                      coord_flip()
   
                      print(sp)
              })),
 
                   
              tabPanel("Summary Normality Test",
               renderPrint({ 
                  out.ANOVA <- aov(formula=y ~ x,data = dataS[[j]])
                  shapiro.test(rstudent( out.ANOVA ))
              })),

                    
              tabPanel("Normality Test",
               renderPlot({ 
                  out.ANOVA <- aov(y ~ x, data = dataS[[j]])
                  g1<-qqnorm(out.ANOVA$residuals) 
                  g2<-qqline(out.ANOVA$residuals)
                  ggplot(dataS[[j]], aes(sample=y)) +
                       stat_qq(size = 2, color = "blue") + 
                       stat_qq_line(size=1.25, color="red") 
              })),
              

              #tabPanel("Histogram",
              # renderPlot({              
              #  out.ANOVA <- aov(y ~ x, data = dataS[[j]])
              #   qplot(out.ANOVA$residuals,
              #         geom = "histogram",
              #         bins = 10)

               #})),

              tabPanel("Summary Homocedasticity",
                 renderPrint({
                   out.ANOVA <- aov( y ~ x, data = dataS[[j]] )
                   bptest(out.ANOVA)
              })),


              tabPanel("Homocedasticity",
                 renderPlot({
                   out.ANOVA <- aov(y ~ x, data = dataS[[j]])
                   plot(out.ANOVA$residuals)
              })),
              
              

              tabPanel(" ",
               renderPrint({
                
                print(" ")
              }))

            )
           } else if (any(input$aVariable == '2way Anova') && (input$gVariable[j] == 'Boxplot' || input$gVariable[j] == 'Violin' || input$gVariable[j] == 'Points'))
         {
          
            
            tabsetPanel( 

             tabPanel("Tree",
               renderPrint({
                dat.moodle$pathString <- do.call(paste,c(dat.moodle,sep="/"))
                df.igraph <- data.tree::as.Node(dat.moodle)        
                plot(df.igraph)
                print(df.igraph)
                 
              })),

             tabPanel("2way Anova",
               renderPrint({
                out.ANOVA <- summary(aov(formula=y ~ x + Factor2 + x:Factor2,data = dataS[[j]]))
                print(out.ANOVA)
              })),

              tabPanel("Summary-Tukey First FActor",
               renderPrint({
                out.ANOVA <- aov(y ~ x + Factor2 + x:Factor2, data = dataS[[j]])
                out.TUKEY <- TukeyHSD(out.ANOVA, which="x") 
                print(out.TUKEY)
              })),          
    
             tabPanel("Tukey",
               renderPlot({
                 out.ANOVA <- aov(y ~ x + Factor2 + x:Factor2, data = dataS[[j]])
                 out.TUKEY <- TukeyHSD(out.ANOVA,which="x")
                 delivery.hsd = data.frame(out.TUKEY$x)
                 delivery.hsd$Comparison = row.names(delivery.hsd)
                 #plot(delivery.hsd,las=1 , col="brown")
 
                 sp<-ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
                     geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
                      coord_flip()
   
                      print(sp)
              })),
 
              tabPanel("Summary-Tukey Second FActor",
               renderPrint({
                out.ANOVA <- aov(y ~ x + Factor2, data = dataS[[j]])
                out.TUKEY <- TukeyHSD(out.ANOVA,which="Factor2") 
                print(out.TUKEY)
              })),          
    
             tabPanel("Tukey",
               renderPlot({
                 out.ANOVA <- aov(y ~ x + Factor2 , data = dataS[[j]])
                 out.TUKEY <- TukeyHSD(out.ANOVA,which="Factor2")
                 delivery.hsd = data.frame(out.TUKEY$Facet)
                 delivery.hsd$Comparison = row.names(delivery.hsd)
                 #plot(delivery.hsd,las=1 , col="brown")
 
                 sp<-ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
                     geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
                      coord_flip()
   
                      print(sp)
              })),

                   
              tabPanel("Summary Normality Test",
               renderPrint({ 
                  out.ANOVA <- aov(formula=y ~ x,data = dataS[[j]])
                  shapiro.test(rstudent( out.ANOVA ))
              })),

                    
              tabPanel("Normality Test",
               renderPlot({ 
                  out.ANOVA <- aov(y ~ x, data = dataS[[j]])
                  g1<-qqnorm(out.ANOVA$residuals) 
                  g2<-qqline(out.ANOVA$residuals)
                  ggplot(dataS[[j]], aes(sample=y)) +
                       stat_qq(size = 2, color = "blue") + 
                       stat_qq_line(size=1.25, color="red") 
              })),
              

              #tabPanel("Histogram",
              # renderPlot({              
              #  out.ANOVA <- aov(y ~ x, data = dataS[[j]])
              #   qplot(out.ANOVA$residuals,
              #         geom = "histogram",
              #         bins = 10)

               #})),

              tabPanel("Summary Homocedasticity",
                 renderPrint({
                   out.ANOVA <- aov( y ~ x, data = dataS[[j]] )
                   bptest(out.ANOVA)
              })),


              tabPanel("Homocedasticity",
                 renderPlot({
                   out.ANOVA <- aov(y ~ x, data = dataS[[j]])
                   plot(out.ANOVA$residuals)
              })),
              
              

              tabPanel(" ",
               renderPrint({
                
                print(" ")
              }))

            )
           } else if (any(input$aVariable =='Segmentation') && input$gVariable[j] == 'Points')
           {
            clusters <- reactive({
                 kmeans(dataS[[j]], input$nCluster)
            }) 
            tabsetPanel(                             
             tabPanel("Segmentacion",
              renderPlot({
                 palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                        "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999") )

                 par(mar = c(5.1, 4.1, 0, 1))
           
                plot(dataS[[j]][1:2],
                col = clusters()$cluster,
                pch = 20, cex = 3) 


                points(clusters()$centers, pch=4, cex = 4, lwd = 4)
          
                write.csv(dataS,'./output/data.csv')
                write.csv(clusters()$cluster,'./output/cluster.csv')
          
                
            })))
         
            ###values<-reactiveValues()
            ###values[["numvotes"]] <- 1
            ###output$notif<-renderUI({
             ###dropdownMenu(type = "notifications", badgeStatus = "warning",
                 ###notificationItem(icon = icon("ok", lib = "glyphicon"), status = "danger",
                    ####              "Check ./output/data.csv && ./output/data.csv"
                 ###)                 )
            ###})
        } else if (any(substr(input$aVariable,1,5) =='Facetxxx') && input$gVariable[j] == 'Points')
        {
            tabsetPanel(                       
             tabPanel(input$gVariable[j],
               renderPlot({
                if (input$gFacet == 'Histogram')
                {
                 sp <- ggplot(dataS[[j]],aes(x)) + geom_histogram() + xlab(input[[paste0("x",j)]])
                } else if (input$gFacet == 'Points')
                {
                 print(dataS[[j]]) 
                 sp <- ggplot(dataS[[j]],aes(x,y)) + geom_point() + xlab(input[[paste0("x",j)]]) + ylab(input[[paste0("y",j)]])
                } else if (input$gFacet == 'Boxplot')
                {
                 sp <- ggplot(dataS[[j]],aes(x,y)) + geom_boxplot() + xlab(input[[paste0("x",j)]]) + ylab(input[[paste0("y",j)]]) 
                 sp <- sp + geom_jitter(alpha = 0.3, color = "tomato")
                } else if (input$gFacet == 'Regresion')
                {
                 sp <- ggplot(dataS[[j]],aes(x,y)) + geom_smooth() + xlab(input[[paste0("x",j)]]) + ylab(input[[paste0("y",j)]])
                } else if (input$gFacet == 'Line')
                {
                 sp <- ggplot(dataS[[j]],aes(x,y)) + geom_rect() + xlab(input[[paste0("x",j)]]) + ylab(input[[paste0("y",j)]])
                } else if (input$gFacet == 'Violin')
                {
                 sp <- ggplot(dataS[[j]],aes(x,y)) + geom_violin() + xlab(input[[paste0("x",j)]]) + ylab(input[[paste0("y",j)]]) + labs(title="Iris") 
###+ theme_elegante()
                 sp <- sp + geom_jitter(alpha = 0.3, color = "tomato")
                } 

                if (input$gVariable[j] == 'Facet.Wrap')
                {                            
                 sp <- sp + facet_wrap( ~Facet)
                } else
                {
                 sp <- sp + facet_grid( ~Facet)
                }   

                print(sp) 
      
              })),

              tabPanel(" ",
               renderPrint({
                
                print("")
              }))

           )
        } else if (input$gVariable[j] == 'Test.Fiabilidad')
        {
          print(as.data.frame(t(dat.moodle))) 
          print(alpha(as.data.frame(t(dat.moodle)))$total$std.alpha)
                                 
        } else if (input$gVariable[j] == 'Heatmap')
        {
         tabsetPanel(                       
             tabPanel("Heatmap",
               renderPlot({
                sp <- ggplot(datsumm$cur_data(),aes(x,y)) + geom_tile(aes(fill=Facet)) + scale_fill_gradient2() 
                heatmap(sp)
          })))
        } else if (any(input$aVariable =='Correlation') && input$gVariable[j] == 'Points')
        {
          tabsetPanel(   
             tabPanel("Correlation",
               renderPlot({
                 corvdata<-cor(dat.moodle[sapply(dat.moodle,is.numeric)])
                 melted_data <- melt(corvdata) 
                 sp <- ggplot(data = melted_data, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                   midpoint = 0, limit = c(-1,1), space = "Lab", 
                  name="Pearson\nCorrelation") +
                  theme_minimal()+ 
                  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                         size = 12, hjust = 1))+
                       coord_fixed()
                  print(sp)         
              })),

                   
              tabPanel("Summary-Correlation",
               renderPrint({ 
                  corvdata<-cor(dat.moodle[sapply(dat.moodle,is.numeric)]) 
                    print(corvdata) 
              })),

              tabPanel(" ",
               renderPrint({
                
                print("")
              }))
          ) 
        } else
        {

          # tabsetPanel(                                            
           #  tabPanel("Summary",
            #   renderPrint({
             #    print(summary(dat.moodle))
                 
             #})))
           
        }
     })

            
      
     
    }) 
}       