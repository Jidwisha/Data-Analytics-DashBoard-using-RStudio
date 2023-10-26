library(shinydashboard)
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(bslib)

ds= read_csv("Transactions datasheet .csv")
ds= ds %>% group_by(hr,pmt,mid,pg,sub_type)%>% summarise(
  t_sum = sum(t),
  s_sum = sum(success),
  SR = s_sum*100/t_sum)
ui <- dashboardPage(
  dashboardHeader(title = "Transaction dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Payment Method", tabName = "PayM"),
      menuItem("Payment Gateway", tabName = "PayG"),
      menuItem("Time-Series Graphs with mid-dimension", tabName = "tsg",
               menuSubItem("Payment_Method",tabName = "mt"),
               menuSubItem("Payment_Gateway",tabName = "gt"),
               menuSubItem("sub_type",tabName = "st")),
      menuItem("Table",tabName = "tb")
    )
  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "PayM",
            fluidPage(
              selectInput("Method", label = "Method", choices = (c(unique(ds[,"pmt"])))),
              plotOutput("Graph1"),
              textOutput("x1")
            )
    ),
    tabItem(tabName = "PayG",
            fluidPage(
              selectInput("Gateway", label = "Gateway", choices = (c(unique(ds[,"pg"])))),
              plotOutput("Graph2"),
              textOutput("x2")
            )
    ),
            tabItem(tabName = "mt",
            fluidPage(
              plotOutput("Graph3")
            )),tabItem(tabName = "gt",
                          fluidPage(
                          plotOutput("Graph4"))),
            tabItem(tabName = "st",
                    fluidPage(
                      plotOutput("Graph5"))),
    tabItem(tabName = "tb",
            fluidPage(
              tableOutput("t1")
            ))
    )))
server <- function(input, output) {
  output$Graph1 <- renderPlot(ggplot(ds %>% filter(pmt==input$Method)) + geom_line(aes(hr,SR)))
  output$x1 <- renderText(input$Method)
  
  output$Graph2 <-renderPlot(ggplot(ds %>% filter(pg==input$Gateway)) + geom_line(aes(hr,SR)))
  output$x2 <- renderText(input$Gateway)
  
  
  output$Graph3 <-renderPlot(ggplot(ds, aes(hr,SR,color="red")) +
                               geom_line(aes(color=pmt)) + facet_wrap(~mid,nrow=4))
  
  output$Graph4 <-renderPlot(ggplot(ds, aes(hr,SR,color="blue")) +
                               geom_line(aes(color=pg)) + facet_wrap(~mid,nrow=4))
  
  output$Graph5 <-renderPlot(ggplot(ds, aes(hr,SR,color="black")) +
                               geom_line(aes(color=sub_type)) + facet_wrap(~mid,nrow=4))
  
  output$t1 <- renderTable(ds)
  
}

shinyApp(ui, server)
