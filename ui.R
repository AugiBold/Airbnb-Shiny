library(shiny)

shinyUI(fluidPage(
  navbarPage("Airbnb Washington, D.C. Listings Analysis",
             tabPanel("About",
                      mainPanel(
                        h2("What is this Project about?"),
                        verbatimTextOutput("txtout0"), # txtout0 is generated from the server
                        img(src='39.png', align = "left"),
                        img(src='8wards.png', align = "right")
                        
                      )),
             
             tabPanel("Summary",
                      sidebarPanel(
                        sliderInput("Listing_Price_Range", "Please Select a Price Range: ", min=50, max=1000, value=500, step=50) 
                        ), #sidebarPanel - Listing_Price_Range
                      mainPanel(
                        h2("Price Distribution of Listings"),
                        verbatimTextOutput("txtout1"), # txtout1 is generated from the server 
                        plotOutput("Price_Distribution"),
                        
                        
                        h2("Which Room type are the most listings?"),
                        plotOutput("Room_type"), 
                        
                        
                        h2("What does the Price distribution look like by Room type"),
                        plotOutput("Price_Density")
                        ) #mainPanel 
                      ), # Summary tabPanel 
             
             tabPanel("Analysis",
                      mainPanel(
                        h2("What is the Average Price of listings per Neighborhood Group(Ward)?"),
                        plotOutput("Ward_Avg"), 
                        verbatimTextOutput("txtout2"), # txtout2 is generated from the server 
                        
                        
                        h2("How much % does each Ward make up of the Market Size?"),
                        plotOutput("Market_Size"),
                        verbatimTextOutput("txtout3"), # txtout3 is generated from the server
                        
                        
                        h2("How much % does each Ward contribute to the Reviews written to Date?"),
                        plotOutput("Reviews"),
                        verbatimTextOutput("txtout4"), # txtout4 is generated from the server
                        
                        
                        h2("How about for Reviews written in the last 12 months?"),
                        plotOutput("Reviews_ltm"),
                        verbatimTextOutput("txtout5") # txtout5 is generated from the server
                      
                      ) # mainPanel 
                      
             ), # Analysis tabPanel 
             
             tabPanel("Results",
                      sidebarPanel(
                        selectInput("Ward", "Please Select a Neighborhood Group(Ward): ", 
                           choices=c("Ward 1", "Ward 2", "Ward 3", "Ward 4", "Ward 5", "Ward 6", "Ward 7", "Ward 8"))
                        ), #sidebarPanel - Ward 
                      mainPanel(
                        h2("Average Listing Price by Neighborhood"),
                        verbatimTextOutput("txtout6"), # txtout6 is generated from the server
                        plotOutput("Neighborhood"),
                        
                        h2("Likelihood of Demand by Neighborhood Group(Ward)"),
                        verbatimTextOutput("txtout7"), # txtout7 is generated from the server
                        plotOutput("Likelihood"), 
                        verbatimTextOutput("txtout8") # txtout8 is generated from the server
                        
                        ), #mainPanel
                      ) #  Results tabPanel 
             
             ) # navbarPage 
  ) # fluidPage 
)    



