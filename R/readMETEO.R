load("./data/estaciones.rda")
load("./data/varMETEO.rda")
load("./data/varMETEO10.rda")


readMeteo <- function(data,est,var,date1,date2){  # llave funcion inicio
  
  type <- "Inv"
  
if(length(est)==1){  

if(as.Date(date1,format="%d/%m/%Y")<as.Date(date2,format="%d/%m/%Y")) { # llave if inicio date
  
  if(!is.na(match(est,estaciones[,1]))){
  
  switch(data,"diezminutal"={
    if(!any(is.na(match(var,eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1])))==T){
      if(length(var)==1){ # llave if inicio var
    
           if(difftime(as.Date(date2,format="%d/%m/%Y"),as.Date(date1,format="%d/%m/%Y"),units="days")>110){ # llave if inicio difftime
      
                  dates <- format.Date(c(seq(as.Date(date1,format="%d/%m/%Y"),as.Date(date2,format="%d/%m/%Y"),100),as.Date(date2,format="%d/%m/%Y")),format="%d/%m/%Y")
      
                   temp <- NULL
      
                   for (i in 1:(length(dates)-1)){ #llave for inicio 
        
                    date1 <- dates[i]
                    date2 <- dates[i+1]
        
                    url <- paste("http://www2.meteogalicia.es/galego/observacion/estacions/DatosHistoricosTaboas_dezminutal",type,".asp?est=",estaciones[est==estaciones[,1],2],"&param=",
                     ifelse(length(var)>1,
                            paste(eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,2],collapse=","),
                            paste(eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,2],sep="",collapse="")),"&data1=",date1,"&data2=",date2,"&red=102&tiporede=automaticas&idprov=",estaciones[est==estaciones[,1],3],sep="",collapse="")
                    ifelse(identical(readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,],character(0))==F,
                    temp[[i]] <- readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,],
                    stop("No data. Problems with dates."))
                    
                   }
      
      
                    temp <- do.call("rbind", temp)
                    temp[,2] <- as.numeric(gsub(",",".",temp[,2]))
                    names(temp) <- c("Fecha",eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,1])
                    table <- temp
                    row.names(table) <- 1:nrow(table)
                    return(table)
            } # llave if cierre difftime
        else{ # llave else inicio difftime
      
                    url <- paste("http://www2.meteogalicia.es/galego/observacion/estacions/DatosHistoricosTaboas_dezminutal",type,".asp?est=",estaciones[est==estaciones[,1],2],"&param=",
                    ifelse(length(var)>1,
                          paste(eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,2],collapse=","),
                          paste(eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,2],sep="",collapse="")),"&data1=",date1,"&data2=",date2,"&red=102&tiporede=automaticas&idprov=",estaciones[est==estaciones[,1],3],sep="",collapse="")
                    if(identical(readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,],character(0))==F){
                    temp <- NULL
                    temp <- readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,]
                    temp[,2] <- as.numeric(gsub(",",".",temp[,2]))
                    names(temp) <- c("Fecha",eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,1])
                    table <- temp
                    row.names(table) <- 1:nrow(table)
                    return(table)
                    }
                    else{
                      stop("No data. Problems with dates.")
                    }
              } # llave else cierre difftime
  

 
        } # llave if cierre var
      
        else { # llave else inicio var
    
    
    
            if(difftime(as.Date(date2,format="%d/%m/%Y"),as.Date(date1,format="%d/%m/%Y"),units="days")>110){ # llave if inicio difftime2
      
                      dates <- format.Date(c(seq(as.Date(date1,format="%d/%m/%Y"),as.Date(date2,format="%d/%m/%Y"),100),as.Date(date2,format="%d/%m/%Y")),format="%d/%m/%Y")
      
                      temp <- NULL
      
                        for (i in 1:(length(dates)-1)){ # llave for inicio difftime2
        
                               date1 <- dates[i]
                               date2 <- dates[i+1]
        
                              url <- paste("http://www2.meteogalicia.es/galego/observacion/estacions/DatosHistoricosTaboas_dezminutal",type,".asp?est=",estaciones[est==estaciones[,1],2],"&param=",
                                      ifelse(length(var)>1,
                                            paste(eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,2],collapse=","),
                                            paste(eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,2],sep="",collapse="")),"&data1=",date1,"&data2=",date2,"&red=102&tiporede=automaticas&idprov=",estaciones[est==estaciones[,1],3],sep="",collapse="")
                                if(identical(readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,],character(0))==F){
                              temp[[i]] <- readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,]
                                }
                               else{
                                 stop("No data. Problems with dates.")
                               }
                            } #llave for cierre difftime2
      
      
                          temp <- do.call("rbind", temp)
                          temp[,2:ncol(temp)] <- apply(temp[,2:ncol(temp)],2,gsub,pattern=",",replacement=".")
                          temp[,2:ncol(temp)] <- apply(temp[,2:ncol(temp)],2,as.numeric)
                          names(temp) <- c("Fecha",eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,1])
                          table <- temp
                          row.names(table) <- 1:nrow(table)
                          return(table)
                      } # llave if cierre difftime2
            
            
                        else{ # llave else inicio difftime2
      
                                  url <- paste("http://www2.meteogalicia.es/galego/observacion/estacions/DatosHistoricosTaboas_dezminutal",type,".asp?est=",estaciones[est==estaciones[,1],2],"&param=",
                                               ifelse(length(var)>1,
                                                      paste(eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,2],collapse=","),
                                                      paste(eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,2],sep="",collapse="")),"&data1=",date1,"&data2=",date2,"&red=102&tiporede=automaticas&idprov=",estaciones[est==estaciones[,1],3],sep="",collapse="")
                                  if(identical(readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,],character(0))==F){
                                  temp <- NULL
                                  temp <- readHTMLTable(url,stringsAsFactors=F,dec = ",")[[1]][-1:-2,]
                                  temp[,2:ncol(temp)] <- apply(temp[,2:ncol(temp)],2,gsub,pattern=",",replacement=".")
                                  temp[,2:ncol(temp)] <- apply(temp[,2:ncol(temp)],2,as.numeric)
                                  names(temp) <- c("Fecha",eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[eval(parse(text=paste("varMETEO10$",est,collapse="",sep="")))[,1]%in%var,1])
                                  table <- temp
                                  row.names(table) <- 1:nrow(table)
                                  return(table)
                                  }
                                  else{
                                    stop("No data. Problems with dates.")
                                  }
                                } #llave else cierre difftime2
    
      
                            } #llave else cierre var
    }
    else{
      stop("Submitted variable name not matching.")
    }
                      },"diario"={     
                        if(!any(is.na(match(var,varMETEO[,1])))==T){
                      temp <- NULL
    
        
                      url <- paste("http://www2.meteogalicia.es/galego/observacion/estacions/DatosHistoricosTaboas_diario",type,".asp?est=",estaciones[est==estaciones[,1],2],"&param=",
                                   ifelse(length(var)>1,
                                          paste(eval(parse(text=paste("varMETEO",collapse="",sep="")))[eval(parse(text=paste("varMETEO",collapse="",sep="")))[,1]%in%var,2],collapse=","),
                                          paste(eval(parse(text=paste("varMETEO",collapse="",sep="")))[eval(parse(text=paste("varMETEO",collapse="",sep="")))[,1]%in%var,2],sep="",collapse="")),"&data1=",date1,"&data2=",date2,"&red=102&tiporede=automaticas&idprov=",estaciones[est==estaciones[,1],3],sep="",collapse="")
                      if(identical(readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,],character(0))==F){
                      temp <- readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,]
                      }
                      else{
                        stop("No data. Problems with dates.")
                      }
    
                      ifelse(length(var)>1,temp[,2:ncol(temp)] <- apply(temp[,2:ncol(temp)],2,gsub,pattern=",",replacement="."),temp[,2] <- gsub(",",".",temp[,2]))
                      
                      ifelse(length(var)>1,temp[,2:ncol(temp)] <- apply(temp[,2:ncol(temp)],2,as.numeric),temp[,2] <- as.numeric(temp[,2]))
                      
                      names(temp) <- c("Fecha",eval(parse(text=paste("varMETEO",collapse="",sep="")))[eval(parse(text=paste("varMETEO",collapse="",sep="")))[,1]%in%var,1])
                      table <- temp
                      row.names(table) <- 1:nrow(table)
                      return(table)
                        }
                        else{
                          
                          stop("Submitted variable name not matching.")
                        }
              },"mensual"={   
                if(!any(is.na(match(var,varMETEO[,1])))==T){
                      temp <- NULL
                      
                                            
                      url <- paste("http://www2.meteogalicia.es/galego/observacion/estacions/DatosHistoricosTaboas_mensual",type,".asp?est=",estaciones[est==estaciones[,1],2],"&param=",
                                   ifelse(length(var)>1,
                                          paste(eval(parse(text=paste("varMETEO",collapse="",sep="")))[eval(parse(text=paste("varMETEO",collapse="",sep="")))[,1]%in%var,2],collapse=","),
                                          paste(eval(parse(text=paste("varMETEO",collapse="",sep="")))[eval(parse(text=paste("varMETEO",collapse="",sep="")))[,1]%in%var,2],sep="",collapse="")),"&data1=",date1,"&data2=",date2,"&red=102&tiporede=automaticas&idprov=",estaciones[est==estaciones[,1],3],sep="",collapse="")
                      if(identical(readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,],character(0))==F){
                    
                      temp <- readHTMLTable(url,stringsAsFactors=F)[[1]][-1:-2,]
                      
                      }
                      else{
                        stop("No data. Problems with dates.")
                      }
                      ifelse(length(var)>1,temp[,2:ncol(temp)] <- apply(temp[,2:ncol(temp)],2,gsub,pattern=",",replacement="."),temp[,2] <- gsub(",",".",temp[,2]))
                      
                      ifelse(length(var)>1,temp[,2:ncol(temp)] <- apply(temp[,2:ncol(temp)],2,as.numeric),temp[,2] <- as.numeric(temp[,2]))
                      
                      names(temp) <- c("Fecha",eval(parse(text=paste("varMETEO",collapse="",sep="")))[eval(parse(text=paste("varMETEO",collapse="",sep="")))[,1]%in%var,1])
                      table <- temp
                      row.names(table) <- 1:nrow(table)
                      return(table)
                }
                else{
                  stop("Submitted variable name not matching.")
                }
                },stop("Types of data: diezminutal (ten-minute),diario (daily) o mensual (monthly)")) #llave if cierre mensual
      
  }
  else{
    
    stop("Submitted name of weather station not matching.")
  }
    } #llave if cierre date

else{ #llave else inicio date
  
  stop("Start date is greater than the end date.")
} #llave else cierre date

}


else{
stop("You can only download data for one weather station at the same time. For several stations use a for-loop.")
}
}






