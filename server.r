library(shiny)
library(gtools)

solve24 <- function(vals=c(9, 9, 9, 9),
                    goal=24,
                    ops=c("+", "-", "*", "/")) {
  
  val.perms <- as.data.frame(t(
    permutations(length(vals), length(vals))))
  
  nop <- length(vals)-1
  op.perms <- as.data.frame(t(
    do.call(expand.grid,
            replicate(nop, list(ops)))))
  
  ord.perms <- as.data.frame(t(
    do.call(expand.grid,
            replicate(n <- nop, 1:((n <<- n-1)+1)))))
  #  ord.perms[[3]]<-c(2,1,2)
  
  for (val.perm in val.perms)
    for (op.perm in op.perms)
      for (ord.perm in ord.perms)
      {
        expr <- as.list(vals[val.perm])
        for (i in 1:nop) {
          expr[[ ord.perm[i] ]] <- call(as.character(op.perm[i]),
                                        expr[[ ord.perm[i]   ]],
                                        expr[[ ord.perm[i]+1 ]])
          expr <- expr[ -(ord.perm[i]+1) ]
        }
        if (isTRUE(all.equal(eval(expr[[1]]), goal))) return(expr[[1]])
      }
  
  return("No Solution")
}

rand<-function(){
  ran<-c("NA","NA","NA","NA")
  while (ran[1]==ran[2] | ran[1]==ran[3] | ran[1]==ran[4] | ran[2]==ran[3] | ran[2]==ran[4] | ran[3]==ran[4]){
    rannum<-sample(1:10, 4, replace=T)
    rancol<-sample(c("s","h","c","d"),4,replace=T)
    for (i in 1:4){
      ran[i]<-paste(rancol[i],rannum[i],".png",sep="")
    }
  }
  return(ran)
}

shinyServer(function(input, output){

  v<-reactiveValues(draw1="NA",draw2="NA",draw3="NA",draw4="NA",draw=c("NA","NA","NA","NA"))
  observeEvent(input$random,{
    v$draw<-rand()
    v$draw1<-v$draw[1]
    v$draw2<-v$draw[2]
    v$draw3<-v$draw[3]
    v$draw4<-v$draw[4]
  })
  
  observeEvent(input$random,{
    output$image1 <- renderImage({
      list(src = paste("~/stat/cards_png/",v$draw1,sep=""),
           width = 142,
           height = 192)
    }, deleteFile = FALSE)
    
    output$image2 <- renderImage({
      list(src = paste("~/stat/cards_png/",v$draw2,sep=""),
           width = 142,
           height = 192)
    }, deleteFile = FALSE)
    
    output$image3 <- renderImage({
      list(src = paste("~/stat/cards_png/",v$draw3,sep=""),
           width = 142,
           height = 192)
    }, deleteFile = FALSE)
    
    output$image4 <- renderImage({
      list(src = paste("~/stat/cards_png/",v$draw4,sep=""),
           width = 142,
           height = 192)
    }, deleteFile = FALSE)
  })

  output$text1 <- renderText({
    if (input$do==0) return()
    paste("You have selected ",input$card1,", ",input$card2,", ",input$card3,", ",input$card4,".",sep="")
  })
  output$text2 <- renderPrint({
    if (input$do==0) return(cat("No Input"))
    solu<-solve24(c(as.numeric(input$card1),as.numeric(input$card2),as.numeric(input$card3),as.numeric(input$card4)))
    if (solu!="No Solution") {
      print(solu)
    }
    else cat(solu)
  })
})