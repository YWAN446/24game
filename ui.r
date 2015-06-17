library(shiny)

shinyUI(fluidPage(
  titlePanel(" 24 game"),
  sidebarLayout(
    sidebarPanel(
      h3("Select Four Numbers"),
      selectInput("card1", label=h4("Card 1"),
                  choices=list("NA"=NA,"A"=1,"2"=2,"3"=3,"4"=4,"5"=5,
                               "6"=6,"7"=7,"8"=8,"9"=9,"10"=10),selected=NA),
      selectInput("card2", label=h4("Card 2"),
                  choices=list("NA"=NA,"A"=1,"2"=2,"3"=3,"4"=4,"5"=5,
                               "6"=6,"7"=7,"8"=8,"9"=9,"10"=10),selected=NA),
      selectInput("card3", label=h4("Card 3"),
                  choices=list("NA"=NA,"A"=1,"2"=2,"3"=3,"4"=4,"5"=5,
                               "6"=6,"7"=7,"8"=8,"9"=9,"10"=10),selected=NA),
      selectInput("card4", label=h4("Card 4"),
                  choices=list("NA"=NA,"A"=1,"2"=2,"3"=3,"4"=4,"5"=5,
                               "6"=6,"7"=7,"8"=8,"9"=9,"10"=10),selected=NA),
      br(),
      # generate four random cards
      actionButton("random",label="Draw Four Cards"),
      
      br(),
      br(),
      actionButton("do",label="Solution"),
      width=4
      ),
    mainPanel(
      h2("Cards Drawn"),
      imageOutput("image1",inline=TRUE),
      imageOutput("image2",inline=TRUE),
      imageOutput("image3",inline=TRUE),
      imageOutput("image4",inline=TRUE),
      br(),
      h2("Solution"),
      h4(textOutput("text1")),
      h4(textOutput("text2"))
      )
    )
  ))