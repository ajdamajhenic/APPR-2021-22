library(shiny)
library(dplyr)
library(ggplot2)


shinyServer(function(input, output) {
  
  output$tabela<- DT::renderDataTable({
    
    tabela3
  })
  
  output$b<- renderPlot({
    
    tabela__1 <- tabela11 %>% filter(statisticna_regija == input$regija)
    
    print(ggplot(data=tabela__1, aes(x = leto, y = delez), group=1) + geom_line(col="#339999") + geom_point(col="#339999") + 
            ylab("Delež brezposelnosti") + xlab("Leto")) + scale_x_continuous(breaks = seq(2008, 2021, by=1), limits = c(2008,2021)) + theme_minimal()+
      theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
  })
  output$drzava<- renderPlot({
    
    tabela__2 <- s1_shiny %>% filter(drzava == input$drzava & izobrazba==input$izobrazba) 
    
    print(ggplot(data=tabela__2, aes(x = leto, y = delez, group=1)) + geom_line(color="#CC0033") + geom_point(color="#CC0033")+
            ylab("Povprečna stopnja brezposelnosti") + xlab("Leto")) + theme_minimal() +
      theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
  })
  output$drzava<- renderPlot({
    
    tabela__3 <- s2_shiny %>% filter(drzava == input$drzava & izobrazba==input$izobrazba) 
    
    print(ggplot(data=tabela__3, aes(x = leto, y = delez, group=1)) + geom_line(color="#CC0033") + geom_point(color="#CC0033")+
            ylab("Povprečna stopnja brezposelnosti") + xlab("Leto")) + theme_minimal() +
      theme(axis.title = element_text(size=13,face="bold"), axis.text = element_text(size=10), axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))
  })
})
