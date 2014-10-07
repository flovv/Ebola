library(shiny)

library(plyr)
library(XML)
library(RCurl)

library(reshape2)
library(ggplot2)

library(minpack.lm)

#library(gtools)

## for the map version!
#library(googleVis)

options("scipen"=10, "digits"=4)


getColNamesOld <- function(){
  
  #cNames <- c("Date","Cases" , "Deaths",  "Cases"  ,"Deaths"  ,"Cases",	"Deaths",	"Cases"	,"Deaths"	,"Cases",	"Deaths",	"Cases",	"Deaths","Cases",  "Deaths", "Refs")
  countries <- c("Total",  "Guinea",	"Liberia",	"Sierra_Leone",	"Nigeria",	"Senegal")
  
  #################################
  
  ## create Colnames!
  re <- c()
  for (i in countries){
    re <- c(re, paste(i,"Cases", sep="_"), paste(i,"Deaths", sep="_"))
    
  }
  return(re)
  
}


getColNames <- function(){
  
  cNames <- c("Date","Cases" , "Deaths",  "Cases"  ,"Deaths"	,"Cases",	"Deaths",	"Cases"	,"Deaths"	,"Cases",	"Deaths",	"Cases",	"Deaths","Cases",  "Deaths", "Refs")
  countries <- c("Total",  "Guinea",	"Liberia",	"Sierra_Leone",	"Nigeria",	"Senegal", "US")
  
  #################################
  
  ## create Colnames!
  re <- c()
  for (i in countries){
    re <- c(re, paste(i,"Cases", sep="_"), paste(i,"Deaths", sep="_"))
    
  }
  return(re)
  
}


getWikiDataV2 <- function(){
  
  if(!exists("content")){

    theurl <- "http://en.wikipedia.org/wiki/Ebola_virus_epidemic_in_West_Africa"
    print("grap Wiki data!")
    tables <- readHTMLTable(theurl)
    #n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
    
    listNames <- names(sapply(tables, names))
    index <- grep("Ebola cases and deaths", listNames)[1]
    
    content <- as.data.frame(tables[index]) #tables$`Ebola cases and deaths by country and by date – 1 August to most recent WHO update`
    #`Ebola cases and deaths by country and by date - 1 August to present.`
    
    ### get old data!
    #index2 <- grep("Ebola cases and deaths", listNames)[2]
    #content2 <- as.data.frame(tables[index2])
    
#     content2 <- tables$`Archived Ebola cases and deaths by country – 22 March to 30 July`
#     content2 <- content2[2:nrow(content2),]
#     re <- getColNamesOld()    
#     cNames <- c("Date", re, "refs")
#     colnames(content2) <- cNames
    
    
    
    #colnames(content) <- t(content[1,])
    content <- content[2:nrow(content),]
    
    re <- getColNames()
    
    cNames <- c("Date", re, "refs")
    colnames(content) <- cNames
    
    ####
   #content<- smartbind(content, content2)
    
    ####################################
    #str(content)
    ####
    # change numbers!
    ####  
    for(i in re){
      content[,i] <- gsub(",", "", content[,i], fixed = TRUE)
      content[,i] <- as.numeric(content[,i])
    }
    
    ##################################################
    Sys.setlocale("LC_TIME", "English")
    content$Date <- gsub("Sept", "Sep", content$Date, fixed = TRUE)  
    content$date <- as.Date(content$Date, "%d %b %Y")
    
    ############
    content$Day  <- content$date-min(content$date)
  }
  return(content)
  
}

getWikiData <- function(){

  
  if(!exists("content")){
  theurl <- "http://en.wikipedia.org/wiki/Ebola_virus_epidemic_in_West_Africa"
  print("grap Wiki data!")
  webpage <- getURL(theurl)
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  
  pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
  
  
  # Extract table header and contents
  results <- xpathSApply(pagetree, "//*/table[@class='wikitable']/tr/td", xmlValue)
  
  # Convert character vector to dataframe
  
  ## need to be changed if countries are added! 
  content <- as.data.frame(matrix(results, ncol=16 ,byrow = TRUE),stringsAsFactors=FALSE)

  re <- getColNames()
  
  cNames <- c("Date", re, "refs")
  colnames(content) <- cNames
  
  ####################################
  #str(content)
  ####
  # change numbers!
  ####  
  for(i in re){
    content[,i] <- gsub(",", "", content[,i], fixed = TRUE)
    content[,i] <- as.numeric(content[,i])
  }
  
  ##################################################
  Sys.setlocale("LC_TIME", "English")
  content$Date <- gsub("Sept", "Sep", content$Date, fixed = TRUE)  
  content$date <- as.Date(content$Date, "%d %b %Y")
  
  ############
  content$Day  <- content$date-min(content$date)
  
  }
  
  return(content)
  
}







content <<- getWikiDataV2()
# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("EbolaOutbreak2014", '.csv', sep='') },
    content = function(file) {
      write.csv(content, file)
    }
  )
  
  ### MAP
#   output$map <- renderGvis({
#     
#     countries2 <- c("Guinea",  "Liberia",  "Sierra_Leone",	"Nigeria",	"Senegal")
#     ## create Colnames!
#     re <- c()
#     
#     for (i in countries2){
#       re <- c(re,  paste(i,"Deaths", sep="_"))
#       
#     }
#     
#     plot <- as.data.frame(t(content[1, re]))
#     colnames(plot) <- c("Deaths")
#     plot$Country <- countries2
#     
#     Geo <- gvisGeoMap(plot, locationvar="Country", numvar="Deaths", 
#                       options=list(height=370, dataMode='regions', region='002'))
#     
#     return(Geo) 
#   })

  # Basic!
  output$facet <- renderPlot({

    re <- getColNames()
    
    mm<-melt(content, measure.vars = c(re),id.vars = c("date"), na.rm=T )
    
    ##############################################################################
  p2 <-  ggplot(mm, aes(y=value, x=date,  group=variable, color=variable, shape=variable)) + 
      geom_smooth(method=loess)+geom_point(size=3)+
      scale_x_date(name="Month") +
      scale_y_continuous(name="Number of Cases") +
      ggtitle('Cases by Country') + facet_wrap(~variable)
   
  print(p2)
  })
  
  # Basic!
  output$basic1 <- renderPlot({
    
    re <- getColNames()
    content <- getWikiData()    
    mm<-melt(content, measure.vars = c("Total_Cases", "Total_Deaths"),id.vars = c("date"), na.rm=T )
    
    ##############################################################################
   p1<- ggplot(mm, aes(y=value, x=date,  group=variable, color=variable)) + 
     geom_smooth(method=loess)+geom_point(size=3)+
     scale_x_date(name="Month") +
     scale_y_continuous(name="Absolute Number") +
     ggtitle('Total registered Cases and Deaths') 
    
   print(p1)
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(content)
  })
  
  output$regression <- renderPrint({
    summary(lm(log(Total_Cases)~ as.numeric(Day), data= content))
    #summary(content)
  })
 
  
  output$saturation <- renderPrint({
    ## weibull! function F(x) = 1 - \mathrm{e}^{-(\lambda \cdot x)^k}
    content$day <- as.numeric(content$Day)
    ctr= nls.control(maxiter = 150, tol = 1e-03, minFactor = 1/102,  printEval = FALSE, warnOnly = FALSE)
    nlm1 <- nlsLM(Total_Cases ~ a - exp(-(b*day)^c) ,control = ctr, start = list( a = 1.3, b = 15, c=2), data= content)
    summary(nlm1)
  })
  
  ######################
  output$saturation_gombertz <- renderPrint({
    ## gompertz function y(t)=ae^{be^{ct}}
    content$day <- as.numeric(content$Day)
    ctr= nls.control(maxiter = 150, tol = 1e-05, minFactor = 1/102,  printEval = FALSE, warnOnly = FALSE)
    nlm1 <- nlsLM(Total_Cases ~ a * exp(b*exp(c*day)) ,control = ctr, start = list( a = 10, b = 150, c=2), data= content)
    summary(nlm1)
  })
  
  output$saturation_old <- renderPrint({
    
    content$day <- as.numeric(content$Day)
    nlm1 <- nlsLM(Total_Cases ~ 1 / (1 + b * exp(-a * day )) , start = list( a = 1, b = 1.3), data= content)
    summary(nlm1)
  })
  
  output$linear <- renderPrint({
    
    summary(lm(Total_Cases ~ as.numeric(Day), data= content))
  })
  
  
  ########### prediction!
  output$prediction <- renderPlot({
    content$day <- as.numeric(content$Day)
    ## three models!
    linear <- lm(Total_Cases ~ day, data= content)
    growth <- lm(log(Total_Cases)~ day, data= content)
    saturation <- nlsLM(Total_Cases ~ 1 / (1 + b * exp(-a * day )) , start = list( a = 1, b = 1.3), data= content)

    start= max(content$day)    
    dd <- seq(from= start, to=start+30, 1)
    ll <- predict(linear, newdata = list(day=seq(from= start, to=start+30, 1)))
    lg <- exp(predict(growth, newdata = list(day=seq(from= start, to=start+30, 1))))
    ls <- predict(saturation,newdata = list(day=seq(from= start, to=start+30, 1)))
    
    o <- as.data.frame(cbind(dd, ll, lg, ls))
    colnames(o) <- c("day", "linear", "growth", "saturation")
    
    o <- as.data.frame(cbind(dd, ll, lg))
    colnames(o) <- c("day", "linear", "growth")
    
    mm<- melt(o, id.vars = "day")
    
    p2 <-  ggplot(mm, aes(y=value, x=day,  group=variable, color=variable, shape=variable)) + 
      geom_smooth(method=loess)+geom_point(size=3)+
      #scale_x_date(name="Days in Future") +
      scale_y_continuous(name="Total Number of Cases") +
      ggtitle("Prediction with linear and growth model.") + facet_wrap(~variable, scales = "free_y")
    
    print(p2)
    
  })
  
  # Generate an HTML table view of the data
  output$table <- renderDataTable({
    re <- getColNames()
    data.frame(x=content[,c("Date", re)])
  })
  
})