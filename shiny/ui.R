library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Analiza brezposelnosti"),
  
  tabsetPanel(
    tabPanel("Tabela brezposelnosti po regijah",
             DT::dataTableOutput("tabela")),
    
  
  tabPanel("Brezposelnost po regijah",
           sidebarPanel(
             
             selectInput("regija", label = "Izberite regijo:",
                         choices=(sort(unique(tabela11$statisticna_regija))))),
           
           mainPanel(plotOutput("b"))),

  tabPanel("Povprečen delež brezposelnih moških",
           sidebarPanel(
             
             selectInput("drzava", label = "Izberite državo:",
                         choices=(sort(unique(s1_shiny$drzava))))),
           sidebarPanel(
           selectInput("izobrazba", label = "Izberite izobrazbo:",
                       choices=(sort(unique(s1_shiny$izobrazba))))),
           mainPanel(plotOutput("drzava"))),
  tabPanel("Povprečen delež brezposelnih žensk",
           sidebarPanel(
             
             selectInput("drzava", label = "Izberite državo:",
                         choices=(sort(unique(s2_shiny$drzava))))),
           sidebarPanel(
             selectInput("izobrazba", label = "Izberite izobrazbo:",
                         choices=(sort(unique(s2_shiny$izobrazba))))),
           mainPanel(plotOutput("drzava"))),
  uiOutput("izborTabPanel"))
))
