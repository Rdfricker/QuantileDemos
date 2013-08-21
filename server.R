library(shiny)

shinyServer(function(input,output){

# Probability Density Function Plots	
  output$pdf_plot <- renderPlot({
  	if (input$typeDist == "uniform") {	
      x <- 0:20 # dummy inputs
      y <- rep(-99,length(x)) # dummy inputs
      max_height <- 1/(input$theta2-input$theta1)
      plot(x,y,type="n",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(input$min_y_unif,input$max_y_unif),ylim=c(0,1.04*max_height))
      lines(c(input$theta1,input$theta2),c(1/(input$theta2-input$theta1),1/(input$theta2-input$theta1)),lwd=3,col="blue")
      points(input$theta1,1/(input$theta2-input$theta1),pch=19,col="blue")
      points(input$theta2,1/(input$theta2-input$theta1),pch=19,col="blue")
      lines(c(input$min_y_unif-1,input$theta1),c(0,0),lwd=3,col="blue")
      lines(c(input$theta2,input$max_y_unif+1),c(0,0),lwd=3,col="blue")   
            
      phi <- input$p*(input$theta2-input$theta1)+input$theta1
      polygon(c(input$theta1,input$theta1,phi,phi),c(0,1/(input$theta2-input$theta1),1/(input$theta2-input$theta1),0),col="yellow")
      mtext(bquote(phi==.(phi)),side=1,padj=1,cex=2,at=phi)
      mtext(bquote(Pr(Y<=phi)==.(input$p)),,side=3,padj=0,cex=2,at=(input$min_y_normal+input$max_y_normal)/2)
      
      lines(c(input$theta1,input$theta2),c(1/(input$theta2-input$theta1),1/(input$theta2-input$theta1)),lwd=3,col="blue")
      points(input$theta1,1/(input$theta2-input$theta1),pch=19,col="blue")
      points(input$theta2,1/(input$theta2-input$theta1),pch=19,col="blue")
      lines(c(input$min_y_unif-1,input$theta1),c(0,0),lwd=3,col="blue")
      lines(c(input$theta2,input$max_y_unif+1),c(0,0),lwd=3,col="blue")         
      }  
      
    if (input$typeDist == "normal") {	  
      x <- seq(input$min_y_normal,input$max_y_normal,0.01)
      y <- dnorm(x,input$mean,input$sd)
      max_height <- dnorm(input$mean,input$mean,input$sd)
      plot(x,y,type="l",xlab=expression(y),ylab=expression(f(y)),xlim=c(input$min_y_normal,input$max_y_normal),ylim=c(0,1.04*max_height),lwd=3,col="blue",xaxs="i",yaxs="i")   
      
      new_x <- x[x < qnorm(input$p,input$mean,input$sd)]
      new_y <- y[x < qnorm(input$p,input$mean,input$sd)]
      phi <- qnorm(input$p,input$mean,input$sd)
      polygon(c(phi,min(phi,input$min_y_normal),new_x,phi),c(0,0,new_y,dnorm(phi,input$mean,input$sd)),col="yellow")
      mtext(bquote(phi==.(phi)),side=1,padj=1,cex=2,at=phi)
      mtext(bquote(Pr(Y<=phi)==.(input$p)),,side=3,padj=0,cex=2,at=(input$min_y_normal+input$max_y_normal)/2)
      
      lines(x,y,,lwd=3,col="blue")
      }   
      
    if (input$typeDist == "gamma") {	
      x <- seq(0,input$max_y_gamma,0.01)
      y <- dgamma(x,input$alpha_gamma,1/input$beta_gamma)
      max_height <- max(dgamma(x,input$alpha_gamma,1/input$beta_gamma))
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(0,input$max_y_gamma),ylim=c(0,1.04*max_height),lwd=3,col="blue",xaxs="i",yaxs="i")  
            
      new_x <- x[x < qgamma(input$p,input$alpha_gamma,1/input$beta_gamma)]
      new_y <- y[x < qgamma(input$p,input$alpha_gamma,1/input$beta_gamma)]
      phi <- qgamma(input$p,input$alpha_gamma,1/input$beta_gamma)
      polygon(c(phi,min(phi,0),new_x,phi),c(0,0,new_y,dgamma(phi,input$alpha_gamma,1/input$beta_gamma)),col="yellow")
      mtext(bquote(phi==.(phi)),side=1,padj=1,cex=2,at=phi)
      mtext(bquote(Pr(Y<=phi)==.(input$p)),,side=3,padj=0,cex=2,at=input$max_y_gamma/2)
      
      
      lines(x,y,lwd=3,col="blue") 
            }   
         
    if (input$typeDist == "chisq") {	
      x <- seq(0,input$max_y_chisq,0.01)
      y <- dchisq(x,input$df)
      if(input$df==1){max_height <- 4}
      if(input$df>1){max_height <- max(dchisq(x,input$df))}
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(0,input$max_y_chisq),ylim=c(0,1.04*max_height),lwd=3,col="blue",xaxs="i",yaxs="i")       
                  
      new_x <- x[x < qchisq(input$p,input$df)]
      new_y <- y[x < qchisq(input$p,input$df)]
      phi <- qchisq(input$p,input$df)
      if(input$df==1) polygon(c(0,phi,phi,rev(new_x)),c(0,0,dchisq(phi,1),rev(new_y)),col="yellow")      
      if(input$df>1) polygon(c(phi,min(phi,0),new_x,phi),c(0,0,new_y,dchisq(phi,input$df)),col="yellow")
      mtext(bquote(phi==.(phi)),side=1,padj=1,cex=2,at=phi)
      mtext(bquote(Pr(Y<=phi)==.(input$p)),,side=3,padj=0,cex=2,at=input$max_y_chisq/2)
      
      lines(x,y,lwd=3,col="blue")       
      }    
            
    if (input$typeDist == "exponential") {	  
      x <- seq(0,input$max_y_exp,0.01)
      y <- dexp(x,1/input$beta_exp)
      max_height <- max(dexp(x,1/input$beta_exp))
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(0,input$max_y_exp),ylim=c(0,1.04*max_height),lwd=3,col="blue",xaxs="i",yaxs="i")  
                  
      new_x <- x[x < qexp(input$p,1/input$beta_exp)]
      new_y <- y[x < qexp(input$p,1/input$beta_exp)]
      phi <- qexp(input$p,1/input$beta_exp)
      polygon(c(phi,min(phi,0),new_x,phi),c(0,0,new_y,dexp(phi,1/input$beta_exp)),col="yellow")
      mtext(bquote(phi==.(phi)),side=1,padj=1,cex=2,at=phi)
      mtext(bquote(Pr(Y<=phi)==.(input$p)),,side=3,padj=0,cex=2,at=input$max_y_exp/2)
      
      lines(x,y,lwd=3,col="blue")  
      }   
      
    if (input$typeDist == "beta") {	  
      x <- seq(0,1,0.01)
      y <- dbeta(x,input$alpha_beta,input$beta_beta)
      max_height <- max(dbeta(x,input$alpha_beta,input$beta_beta))
      plot(x,y,type="l",main="",xlab=expression(y),ylab=expression(f(y)),xlim=c(0,1),ylim=c(0,1.04*max_height),lwd=3,col="blue",xaxs="i",yaxs="i")   
                        
      new_x <- x[x < qbeta(input$p,input$alpha_beta,input$beta_beta)]
      new_y <- y[x < qbeta(input$p,input$alpha_beta,input$beta_beta)]
      phi <- qbeta(input$p,input$alpha_beta,input$beta_beta)
      polygon(c(phi,min(phi,0),new_x,phi),c(0,0,new_y,dbeta(phi,input$alpha_beta,input$beta_beta)),col="yellow")
      mtext(bquote(phi==.(phi)),side=1,padj=1,cex=2,at=phi)
      mtext(bquote(Pr(Y<=phi)==.(input$p)),,side=3,padj=0,cex=2,at=0.5)
      
      lines(x,y,lwd=3,col="blue")   

      }   
  })
  
  
})


