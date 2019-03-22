output$cum_data<-renderTable({
  
  cum_data<-
  if(!is.null(vals$data) & input$type_cum_data=="Proportion"){
    
    cbind(vals$data[,!colnames(vals$data)%in%c("xi", "ni")],
          lapply(vals$data[,colnames(vals$data)%in%c("xi", "ni")], cumfunc))
    
  }else if(!is.null(vals$data) & input$type_cum_data=="Mean"){
    
    cbind(vals$data[,!colnames(vals$data)%in%c("m1i", "m2i", "sd1i", "sd2i", "n1i", "n2i")],
          lapply(vals$data[,colnames(vals$data)%in%c("m1i", "m2i", "sd1i", "sd2i", "n1i", "n2i")], cumfunc))
    
  }else if(!is.null(vals$data) & input$type_cum_data=="Two proportions (2X2)"){
    
    cbind(vals$data[,!colnames(vals$data)%in%c("ai", "ci", "n1i", "n2i")],
          lapply(vals$data[,colnames(vals$data)%in%c("ai", "ci", "n1i", "n2i")], cumfunc))
  }
  
  cum_data
  
})

