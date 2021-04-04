has_Stroke <- function(graph)
{
  has_Stroke <- TRUE
  if (graph == 'Points' || graph == 'Boxplot')
  {
    has_Stroke <- FALSE
  } 
  return(has_Stroke)
}

has_Color <- function(graph)
{
  has_Color <- TRUE
  if (graph == 'Histogram' || graph == 'Regresion.loess' || graph == 'Boxplot' || graph == 'Regresion.lm')
  {
    has_Color <- FALSE
  } 
  return(has_Color)
}

has_Size <- function(graph)
{
  has_Size <- TRUE
  if (graph == 'Histogram' || graph == 'Paths' || graph == 'Regresion.loess' || graph == 'Boxplot' || graph =='Text' || graph=='Lines' || graph=='BarChar' || graph=='Regresion.lm')
  {
    has_Size <-FALSE
  } 
  return(has_Size) 
}

has_Shape <- function(graph)
{
  has_Shape <- TRUE
  if (graph == 'Histogram' || graph == 'Paths' || graph == 'Regresion.loess' || graph == 'Boxplot' || graph=='Lines' || graph=='BarChar')
  {
    has_Shape <-FALSE
  } 
  return(has_Shape)  

}


is.analytics <- function(graph)
{
  is.analytics <- FALSE 
  if (graph == 'Correlation' || graph == 'Segmentation' || graph == '1way Anova' || graph == '2way Anova' || graph =='Facet.Wrap' || graph =='Facet.Grid' || graph =='Regresion')
  {
   is.analytics <- TRUE
  } 
  return(is.analytics)
}


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

drawPlot <- function(v,plot,input,data,index)
{
 
 v %>% layer_f(function(v)
 {

  if (plot == 'Histogram')
 {
   v %>% ggplot(data,aes(x)) + geom_histogram() + xlab(input[[paste0("x",index)]])
 } else if (plot == 'Points')
 {
   v %>% ggplot(data,aes(x,y)) + geom_point() + xlab(input[[paste0("x",index)]]) + ylab(input[[paste0("y",index)]])
 } else if (plot == 'Boxplot')
 {
   v %>% ggplot(data,aes(x,y)) + geom_boxplot() + xlab(input[[paste0("x",index)]]) + ylab(input[[paste0("y",index)]]) 
   sp <- sp + geom_jitter(alpha = 0.3, color = "tomato")
 } else if (plot == 'Regresion')
 {
    v %>% ggplot(data,aes(x,y)) + geom_smooth() + xlab(input[[paste0("x",index)]]) + ylab(input[[paste0("y",index)]])
 } else if (plot == 'Line')
 {
    v %>% ggplot(data,aes(x,y)) + geom_rect() + xlab(input[[paste0("x",index)]]) + ylab(input[[paste0("y",index)]])
 } else if (plot == 'Violin')
 {
    v %>% ggplot(data,aes(x,y)) + geom_violin() + xlab(input[[paste0("x",index)]]) + ylab(input[[paste0("y",index)]]) + labs(title="Iris") 
    sp <- sp + geom_jitter(alpha = 0.3, color = "tomato")
 } 

  return(sp)
 }) 
}

   
layer_generic <- function(v,layer,vdata,vDataF,indLayer,input,session)  
  {
    
    xVar <- vdata$x
    xVar<-na.omit(xVar) 
    yVar <- vdata$y
    yVar<-na.omit(yVar)

    
    
    if (substr(layer,1,6) == 'Points' || layer == 'Segmentation' || layer == 'Facet.Wrap'|| layer == 'Facet.Grid' || layer == 'Heatmap' || layer =='Correlation')
    {
     
     
      v %>% layer_f(function(v) {   
             
         if (is.numeric(vdata$x) && is.numeric(vdata$y))
         {
          #v %>% add_props(inherit=FALSE) %>% layer_points(x = prop("x", ~x),y = prop("y", ~y),fill.brush:='red')
          v %>% layer_points(x = prop("x", ~x),y = prop("y", ~y),fill.brush:='red',opacity:=input_slider(.1, 1, value = 1, step = .5, label = "alpha-Points"))
         } else
         {
          v %>% layer_points(x = prop("x", ~x),y = prop("y", ~y),fill.brush:='red',opacity:=input_slider(.1, 1, value = 1, step = .5, label = "alpha-Points"))
         } 
               
      }) 
     
    } else if (layer == 'BarChar')
    {
     
      v %>% layer_f(function(v) {
        
        

        if (is.numeric(yVar))
        {           
         v %>% add_props(~x,~y) %>% layer_bars()
        } else
        {
          v %>% add_props(~x,~y) %>% layer_points() 
        }
  
      }) 
     
    } else if (substr(layer,1,9) == 'Histogram')
    {
     if ((indLayer > 1 && is.numeric(vDataF[[indLayer-1]]$y)) || indLayer == 1)
     {       
      v %>% layer_f(function(v) {
        if (is.numeric(xVar))
        {
          
            
         ###v %>% add_props(inherit=FALSE) %>% add_data(vdata) %>% add_props(~x) %>% layer_histograms(width=input_slider(.01, 2, value = .01, step = .1, label = "Binwidth adjustment",fill:="#E74C3C")
       
         v %>%layer_histograms(x=prop("x", ~x),width=input_slider(.1, 2, value = .1, step = .1, label = "Binwidth histogram"),fill:=input_select(c("red","yellow","green","brown","blue","orange","black","white"),label='Color-histogram'))   
                  
        } else
        {
        if (is.numeric(yVar))
         {  
          v %>% add_props(~x,~y) %>% layer_bars()
         } else 
         {
          v %>% add_props(~x,~y) %>% layer_points() 
         }  
        }
      }) 
     } else
     {
       if (is.numeric(yVar))
         {  
          v %>% add_props(~x,~y) %>% layer_bars()
         } else 
         {
          v %>% add_props(~x,~y) %>% layer_points() 
         }  
     }
      
    } else if (layer == 'KDE')
    {
     v %>% layer_f(function(v) {
      
      if (length(xVar) < 2 || !is.numeric(xVar))
      {
         v %>%add_props(~x,~y) %>% layer_points() 
      } else 
      {
      
       v %>% add_props(~x,~y) %>% layer_densities(adjust = input_slider(.1, 2, value = 1, step = .1, label = "Bandwidth KDE"),
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
       v %>%add_props(~x,~y) %>% layer_points() 
      } else
      {
       v %>% add_props(~x,~y) %>% layer_boxplots(fill:='white',coef = input_slider(1.5, 2.5, value = 1.5, step = .5, label = "Coef-boxplot"))
#,width #=input_slider(0.5, 1, value = 0.5, step = 0.1, label = "Width"))
      }
     })
     
    } else if (layer == 'Regresion.loess')
    {
      

      v %>% layer_f(function(v) {
      mod <- try(loess.smooth(xVar, yVar)) 
      if(inherits(mod, "try-error"))
      
        {
       v %>%add_props(~x,~y) %>% layer_points()  
      }  else
      {      
       v %>% add_props(~x,~y) %>% layer_smooths(span = input_slider(.8, 8, value = 1, step = .5,label='span'))           
      }
      })
 
    } else if (layer == 'Regresion.lm')
    {
      v %>% layer_f(function(v) {
 

      mod <- try(loess.smooth(xVar,yVar))
      
      if(!inherits(mod, "try-error")){
        
          v %>%add_props(~x,~y) %>% layer_points(size := input_slider(10, 310, label = "Point size")) %>% 
              #layer_model_predictions(model = "lm", stroke := "red", fill := "red") 
         compute_model_prediction(y ~ x,model = "lm", span = input_slider(.8, 8, value = 1, step = .5,label='span'),family="gaussian") %>% 
          layer_paths(~pred_, ~resp_)
         
       } else
       {
         
          v %>%add_props(~x,~y) %>% layer_points() 
         
        }
       

      })  
 
    } else if (layer == 'Regresion.')
    {
      v %>% layer_f(function(v) {
       mod <- try(loess.smooth(xVar,yVar))
      
      if(!inherits(mod, "try-error")){
        
          v %>%add_props(~x,~y) %>% layer_points() %>% 
              layer_model_predictions(model = "lm",stroke="red") %>% 
              layer_smooths(span = input_slider(.8, 8, value = 1, step = .5,label='span')) 
       } else
        {
         
          v %>%add_props(~x,~y) %>% layer_points() 
          
        }
     
      })
 
    } else if (substr(layer,1,5) == 'Paths')
    {
      v %>% layer_f(function(v) {
        v %>%layer_paths(x = prop("x", ~x),y = prop("y", ~y),stroke := input_select(
        c("red","brown")))  
      })
 
    } else if (layer == 'Lines')
    {
      v %>% layer_f(function(v) {
     
       #v %>% add_props(inherit=FALSE) %>% add_props(~x,~y) %>% layer_lines() 

      v %>% add_props(~x,~y) %>% layer_lines() 

      })
    } else if (layer == 'Text')
    {
     v %>% layer_f(function(v) {
      v %>%layer_text(x = prop("x", ~x, scale = "xcenter"),
        y = prop("y", ~y, scale = "ycenter"),text:=~Text, fontSize := 10)
     })
    } else if (layer == 'Correlation')
    {
      v %>% layer_f(function(v) {   
             
        
          v %>% layer_rects(x = prop("x", ~x),y = prop("y", ~y),width = band(), height = band())
         
               
      }) 
      
       
    } 
  }

dataSourceGeneric = function(indLayer,Level,input)
   {
     ndat<-paste0("dat",indLayer)
     ndat<-paste0(ndat,Level)
     nSource <-paste0("Source",indLayer)
     nSource <-paste0(nSource,Level)  
     
     nData <-paste0("MoodleData",indLayer)
     nData <-paste0(nData,Level) 
     dat.moodle<-NULL
    if(!is.null(input[[ndat]])) 
#&& !is.null(input[[nSource]] && !is.null(input[[nSource]])) 
    { 
     if (input[[ndat]] == 'other')
     {
       
      dataFile <- input[[nSource]]$datapath
      
      if (!is.null(dataFile)) 
      {
       dat.moodle <- read.delim(dataFile, header = TRUE, sep = ",",na.strings=c(""),stringsAsFactors=FALSE) 
       
       
      } else
      { 
          nSource <-paste0("Source",1)
          nSource <-paste0(nSource,Level)  
          dataFile <- input[[nSource]]$datapath
          if (!is.null(dataFile))
          {
           dat.moodle  <- read.delim(dataFile, header = TRUE, sep = ",",na.strings=c(""),stringsAsFactors=FALSE)
          }
        
        
      }
     } 

            
     if (input[[ndat]] == 'moodle' && !is.null(input[[nData]]))
     {
      
      #conn <- dbConnect(drv = RMySQL::MySQL(),dbname = "bitnami_moodle",host = "localhost",
       #                username = "root")
      ###on.exit(dbDisconnect(conn), add = TRUE)
      
      if (input[[nData]] == 'StudentsbyCourse')
      { 
        dat.moodle <- dbGetQuery(conn,Query1)
      } else if (input[[nData]] == 'GradesbyStudent')
      {
        dat.moodle <- dbGetQuery(conn,Query2)
      } else if (input[[nData]] == 'Survey') 
      {
        dat.moodle <- dbGetQuery(conn,Query3)
      } else
      {      
        dat.moodle <- dbReadTable(conn = conn,  name = input[[nData]])
      } 
      #dbDisconnect(conn)
     } 
   
   } else
   {
      
        
          #if (!is.null(dataFile))
          #{
           #dat.moodle  <- read.delim("dataFile", header = TRUE, sep = ",",na.strings=c("")) 
          #}
       
        
       ##dat.moodle  <- read.delim("UCBAdmissions.csv", header = TRUE, sep = ",",na.strings=c(""))
   } 
     
    
  
    if (any(input[[paste0("filter2Variable",indLayer)]] == "Gathering") && !is.null(dat.moodle))
    {
      if (!is.character(dat.moodle$x)) 
      {
        dat.moodle <- gather(dat.moodle, x, y) 
        
       
      } 
    } 

    if (any(input[["gVariable"]] == "Correlation") && !is.null(dat.moodle))
    {
          ###corvdata<-cor(dat.moodle[sapply(dat.moodle,is.numeric)])
          ###dat.moodle <- melt(corvdata)           
    }

    if (is.numeric(dat.moodle$y) && any(input[[paste0("filter2Variable",indLayer)]] == "Scale-Y"))
    { 
     #par(mar=c(3.1, 4.1, 1.1, 2.1))     
     dat.moodle$y <- c(scale(dat.moodle$y))
    }    
     
    if (is.numeric(dat.moodle$y) && any(input[[paste0("filter2Variable",indLayer)]] == "Scale-X"))
    { 
     #par(mar=c(3.1, 4.1, 1.1, 2.1))     
     dat.moodle$x <- c(scale(dat.moodle$x))
    }   
    dat.moodle
    
   }
   
  

dataSource = function(indLayer,Level,input)
   {
     ndat<-paste0("dat",indLayer)
     ndat<-paste0(ndat,Level)
     nSource <-paste0("Source",indLayer)
     nSource <-paste0(nSource,Level)  
     
     nData <-paste0("MoodleData",indLayer)
     nData <-paste0(nData,Level) 
     dat.moodle<-NULL
    if(!is.null(input[[ndat]])) 
#&& !is.null(input[[nSource]] && !is.null(input[[nSource]])) 
    { 
     if (input[[ndat]] == 'other')
     {
       
      dataFile <- input[[nSource]]$datapath
      
      if (!is.null(dataFile)) 
      {
       dat.moodle <- read.delim(dataFile, header = TRUE, sep = ",",na.strings=c(""),stringsAsFactors=FALSE) 
       
       
      } else
      { 
          nSource <-paste0("Source",1)
          nSource <-paste0(nSource,Level)  
          dataFile <- input[[nSource]]$datapath
          if (!is.null(dataFile))
          {
           dat.moodle  <- read.delim(dataFile, header = TRUE, sep = ",",na.strings=c(""),stringsAsFactors=FALSE)
          }
        
        
      }
     } 

            
     if (input[[ndat]] == 'moodle' && !is.null(input[[nData]]))
     {
      
      #conn <- dbConnect(drv = RMySQL::MySQL(),dbname = "bitnami_moodle",host = "localhost",
       #                username = "root")
      #on.exit(dbDisconnect(conn), add = TRUE)
     if (input[[nData]] == 'StudentsbyCourse')
      { 
        dat.moodle <- dbGetQuery(conn,Query1)
      } else if (input[[nData]] == 'GradesbyStudent')
      {
        dat.moodle <- dbGetQuery(conn,Query2)
      } else if (input[[nData]] == 'Survey')
      {
        dat.moodle <- dbGetQuery(conn,Query3) 
      } else
      {      
        dat.moodle <- dbReadTable(conn = conn,  name = input[[nData]])
      }
      #dbDisconnect(conn)
     } 
   
   } else
   {
      
        
           #if (!is.null(dataFile))
          #{
           #dat.moodle  <- read.delim("dataFile", header = TRUE, sep = ",",na.strings=c("")) 
          #}
       
        
       ##dat.moodle  <- read.delim("UCBAdmissions.csv", header = TRUE, sep = ",",na.strings=c(""))
   } 
     
    
  
    
    dat.moodle
    
  }

  dataHeaderGeneric = function(indLayer,Level,input)
   {
     ndat<-paste0("dat",indLayer)
     ndat<-paste0(ndat,Level)
     nSource <-paste0("Source",indLayer)
     nSource <-paste0(nSource,Level)  
     
     nData <-paste0("MoodleData",indLayer)
     nData <-paste0(nData,Level) 

    if(!is.null(input[[ndat]])) 
#&& !is.null(input[[nSource]] && !is.null(input[[nSource]])) 
    { 
     if (input[[ndat]] == 'other')
     {
       
      dataFile <- input[[nSource]]$datapath
      
      if (!is.null(dataFile)) 
      {
       dat.moodle <- read.table(dataFile, nrows=1,header = TRUE, sep = ",",na.strings=c(""),as.is=T) 
       
      } else
      { 
          nSource <-paste0("Source",1)
          nSource <-paste0(nSource,Level)  
          dataFile <- input[[nSource]]$datapath
          if (!is.null(dataFile))
          {
           dat.moodle <- read.table(dataFile, nrows=1,header = TRUE, sep = ",",na.strings=c(""),as.is=T)
          }
        
        
      }
     } 

            
     if (input[[ndat]] == 'moodle' && !is.null(input[[nData]]))
       {
      
      #conn <- dbConnect(drv = RMySQL::MySQL(),dbname = "bitnami_moodle",host = "localhost",
       #                username = "root")
       #on.exit(dbDisconnect(conn), add = TRUE)
      
      if (input[[nData]] == 'StudentsbyCourse')
      { 
        dat.moodle <- dbGetQuery(conn,Query1)
      } else if (input[[nData]] == 'GradesbyStudent')
      {
        dat.moodle <- dbGetQuery(conn,Query2)
      } else
      {      
        dat.moodle <- dbReadTable(conn = conn,  name = input[[nData]])
      }
      #dbDisconnect(conn)
     } 
   
   } else
   {
      
        
          #if (!is.null(dataFile))
          #{
           #dat.moodle  <- read.delim("dataFile", header = TRUE, sep = ",",na.strings=c("")) 
          #}
       
        
       ##dat.moodle  <- read.delim("UCBAdmissions.csv", header = TRUE, sep = ",",na.strings=c(""))
   } 
    if (any(input[[paste0("filter2Variable",indLayer)]] == "Gathering") && !is.null(dat.moodle))
    {
         if (!is.character(dat.moodle$x) )
         { 
          dat.moodle <- gather(dat.moodle, x, y) 
          par(mar=c(3.1, 4.1, 1.1, 2.1))
          if (!is.character(dat.moodle$y))
          {
           dat.moodle$y <- c(scale(dat.moodle$y))
           
          }
         } 
    } 


    if (any(input[["gVariable"]] == "Correlation") && !is.null(dat.moodle))
    {
          #corvdata<-cor(dat.moodle[sapply(dat.moodle,is.numeric)])
            #dat.moodle <- melt(corvdata)           
    } 

   
       
    dat.moodle
   } 
   
 
 ####Main DataFrame##### 
 
 plotDataGeneric = function(inputx,inputy,inputf,inputgr,inputColor,inputSize,inputStroke,inputShape,inputText,input,dataS,indLayer)
 { 
   
  inputFacet <- input[[paste0("Facet",indLayer)]]
  if (is.null(inputFacet))
  {
   if (indLayer == 1)
   { 
     inputFacet <- inputx
   } else
   {
    ###inputFacet <- input[[paste0("Facet",indLayer-1)]]
    inputFacet <- inputx
   } 
  } 

  inputFactor2 <- input[["Factor2"]]
  if (is.null(inputFactor2))
  {
   inputFactor2 <- inputx
  } 

  if(any(inputx %in% names(dataS)) & any(inputy %in% names(dataS)) & any(inputFacet %in% names(dataS)) & any(inputf %in% names(dataS)) & any(inputgr %in% names(dataS)) & any(inputFactor2 %in% names(dataS)) ) 
   {   
        
    
    df <- subset(dataS, select = c(inputx,inputy,inputFacet,inputFactor2,inputf,inputgr))
       
    
    names(df) <- c("x","y","Facet","Factor2","f","g")

    if(any(inputf %in% names(dataS)))
    { 
        
     nSel <- length(inputf) 
     vartemp<-paste0("f",indLayer)
     names(df)[5] <-vartemp  
     if (nSel > 1)
     {
      for (index in 2:nSel) 
      { 
       vartemp<-paste0("f",indLayer)
       names(df)[4+index] <-paste0(vartemp,index)
      } 
      names(df)[4+nSel+1] <-"g"
     } 
    }
     
    if(any(inputgr %in% names(dataS))) 
    { 
        
     nSelg <- length(inputgr) 

     if (nSelg > 1)
     {
      for (index in 2:nSelg) 
      { 
       names(df)[4 + nSel + index] <-paste0("g",index)
      } 
       
    } 
   }

    ind<-0
    
    
    if(any(inputColor %in% names(dataS)))
    { 
      
     if (any(input[[paste0("oVariable",indLayer)]] == 'Color'))
     {
      
        
       df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputColor))
       
       names(df)[4+nSel+nSelg+ind+1]<-'Color'
       ind<-ind+1 
      }
     } else if (is.null(input[[paste0("oVariable",indLayer)]]) || is.null(inputColor) || !any(input[[paste0("oVariable",indLayer)]] == "Color"))
     {
      df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputx))  
      names(df)[4+nSel+nSelg+ind+1]<-'Color'
      ind<-ind+1       
     }
    

    if(any(inputSize %in% names(dataS)))
    { 
     if (any(input[[paste0("oVariable",indLayer)]] == 'Size'))
     {
        
      df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputSize))
      names(df)[4+nSel+nSelg+ind+1]<-'Size' 
      ind<-ind+1
     }
    } else if (is.null(input[[paste0("oVariable",indLayer)]]) || is.null(inputSize) || !any(input[[paste0("oVariable",indLayer)]] == "Size"))
     {
      
      df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputx))  
      names(df)[4+nSel+nSelg+ind+1]<-'Size'
      ind<-ind+1       
      }

   if(any(inputShape %in% names(dataS)))
    {
     if (any(input[[paste0("oVariable",indLayer)]] == "Shape"))
     {
       
      df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputShape))
      names(df)[4+nSel+nSelg+ind+1]<-'Shape' 
      ind<-ind+1
     }
    } else if (is.null(input[[paste0("oVariable",indLayer)]]) || is.null(inputShape) || !any(input[[paste0("oVariable",indLayer)]] == "Shape"))
     {
      
      df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputx))  
      names(df)[4+nSel+nSelg+ind+1]<-'Shape'
      ind<-ind+1       
      }

   if(any(inputStroke %in% names(dataS)))
    {
     if (any(input[[paste0("oVariable",indLayer)]] == "Stroke"))
     {
        
       df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputStroke))
      names(df)[4+nSel+nSelg+ind+1]<-'Stroke' 
      ind<-ind+1
     }
    } else if (is.null(input[[paste0("oVariable",indLayer)]]) || is.null(inputStroke) || !any(input[[paste0("oVariable",indLayer)]] == "Stroke"))
     {
      
      df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputx))  
      names(df)[4+nSel+nSelg+ind+1]<-'Stroke'
      ind<-ind+1       
      } 

   if(any(inputText %in% names(dataS)))
    {
     if (any(input[[paste0("oVariable",indLayer)]] == "Texting") || any(input$gVariable == "Text"))
      {
      df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputText))
      names(df)[4+nSel+nSelg+ind+1]<-'Text' 
      ind<-ind+1
     }
    }  else if (is.null(input[[paste0("oVariable",indLayer)]]) || is.null(inputText) || !any(input[[paste0("oVariable",indLayer)]] == "Text"))
    {
      
      df[4+nSel+nSelg+ind+1] <- subset(dataS, select = c(inputx))  
      names(df)[4+nSel+nSelg+ind+1]<-'Text'
      ind<-ind+1       
    } 

    jx=0
    jy=0

    if (input$checkJitter)  
    { 
      if (is.numeric(df$x) && is.numeric(df$y))
      {
       jx<-rnorm(length(df$x),mean=0,sd=.2)    
       jy<-rnorm(length(df$y),mean=0,sd=.06)  
       df$x <- df$x + jx
       df$y <- df$y + jy 
      }          
    }
   
    print(indLayer) 
    print(df)
   
    return(df)
    
    
   }
  }  

 appl_filter = function(data,sfilter,index,input,indLayer)
 {
  sfilter_ <- rlang::parse_expr(sfilter)
  message("filter statement: `", sfilter_, "`.")
  cols <- data %>% names()                                
  filcol <- cols[index+3]
  vartemp1 <- paste0("ff",indLayer)
  print('pasa')
  data <- data %>%
    dplyr::filter(!!sfilter_)

  return(data)
 }  

 controlLabels = function(input,label,label1)
 {
  if (any(input$aVariable == "1way Anova")) 
  {
   label <- "factor 1"
      
  } else if (any(input$aVariable == "2way Anova")) 
  {
   
                    label <- "factor 1" 
                    label1<- "factor 2"        
  } else if (any(substr(input$gVariable,1,5) == "Facet"))
  {
   label1 <- "Facet"    
  }     
 } 

 controlViewObjects = function(input,output,session)
 {

   if (any(input$filterVariable =='n'))
   {
    hide(id="grVariable")
    hide(id="fVariable")
   }
  
   if (any(input$filterVariable =='Filter'))
  {

   show(id="Filter")
   show(id="fVariable")   
  } else
  {
   hide(id="Filter")
   hide(id="fVariable")  
  }
 
  if (any(input$filterVariable =='Group+'))
  {

   show(id="Group+")
   show(id="grVariable")   
  }
  
  
 if (input$tabs2 == "Analytics") 
 {
   show(id="ace")
   show(id="eval")

 } else
 {
   ##clean_plot(output,3)
   hide(id="ace")
  hide(id="eval")
 } 
   
 
  if (any(input$filterVariable == "Filter") || any(input$gVariable == "Regresion.loess") || any(input$gVariable == "Histogram")
    || any(input$filterVariable == 'Group+') || any(input$gVariable == "Regresion.lm") 
    || any(input$gVariable =="Regresion.") || any(input$gVariable == "KDE")|| any(input$gVariable == "Paths") || any(input$gVariable == "Paths.1") || any(input$gVariable == "BarChar") || any(input$gVariable == "Points") || any(input$gVariable == "Boxplot"))
  {
    show(id="p_ui")
  } else
  {
    hide(id = "p_ui", anim = TRUE)
  }

   if (any(input$aVariable == "1way Anova")) 
  {
    updateSelectInput(session, "x1", 
                    label = "factor 1")
    hide(id="Facet")
      
  } else if (any(input$aVariable == "2way Anova")) 
  {
    show(id="Facet")
    updateSelectInput(session, "x1", 
                    label = "factor 1")
    updateSelectInput(session, "Facet", 
                    label = "factor 2")        
  } else if (any(substr(input$gVariable,1,5) == "Facet"))
  {
   updateSelectInput(session, "Facet", 
                    label = "Facet")
   show(id="Facet")  
    
    
  } else
  {
    hide(id="Facet")
  }    

 } 
     