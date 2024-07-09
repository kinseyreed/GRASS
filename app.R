#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#July9 2024 vers

library(shiny)
library(shinydashboard)
library(dplyr)


# Load the data
practices_data <- read.csv("PracticesCalc.csv")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "EQIP Incentive Payment Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Incentive Calculator", tabName = "calculator", icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "calculator",
        fluidRow(
          box(
            title = "Select Incentive Types and Practices",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("incentiveTypeUI"),
            uiOutput("practiceUI"),
            uiOutput("itemsUI"),
            actionButton("add", "Add Items", class = "btn btn-primary"),
            br(),
            br(),
            uiOutput("removePracticeUI"),
            actionButton("remove", "Remove Selected Practice", class = "btn btn-danger"),
            br(),
            br(),
            actionButton("calculate", "Calculate Total", class = "btn btn-success"),
            br(),
            br(),
            tableOutput("selectedItemsTable")
          ),
          box(
            title = "Total Payment Breakdown",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            tableOutput("totalPaymentTable")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store selected items
  selectedItems <- reactiveVal(data.frame(IncentiveType = character(), 
                                          Practice = character(), 
                                          Item = character(), 
                                          Rate = numeric(), 
                                          Unit = character(), 
                                          Amount = numeric(), 
                                          stringsAsFactors = FALSE))
  
  # Dynamic UI for selecting Incentive Types
  output$incentiveTypeUI <- renderUI({
    checkboxGroupInput("incentiveType", "Select Incentive Type:", 
                       choices = unique(practices_data$IncentiveType))
  })
  
  # Dynamic UI for selecting Practices
  output$practiceUI <- renderUI({
    req(input$incentiveType)
    selectInput("practice", "Select Practice:", 
                choices = unique(practices_data %>% 
                                   filter(IncentiveType %in% input$incentiveType) %>% 
                                   pull(Practice)), multiple = TRUE)
  })
  
  # Dynamic UI for selecting Items
  output$itemsUI <- renderUI({
    req(input$practice)
    
    lapply(input$practice, function(practice) {
      items <- practices_data %>% 
        filter(IncentiveType %in% input$incentiveType, Practice == practice)
      
      wellPanel(
        h4(practice),
        lapply(1:nrow(items), function(i) {
          item <- items[i, ]
          tagList(
            checkboxInput(paste0("select_", practice, "_", i), item$Item, value = FALSE),
            if(item$IncentiveType %in% c("Hosting Field Day", "Grazing School")) {
              NULL
            } else {
              conditionalPanel(
                condition = paste0("input.select_", practice, "_", i, " == true"),
                if (item$Max > 100) {
                  tagList(
                    textInput(paste0("amount_", practice, "_", i), 
                              paste0("Amount (", item$Unit, "):"), 
                              value = item$Min),
                    helpText(paste("Range:", item$Min, "-", item$Max, item$Unit))
                  )
                } else {
                  sliderInput(paste0("amount_", practice, "_", i), 
                              paste0("Amount (", item$Unit, "):"), 
                              min = item$Min, 
                              max = item$Max, 
                              value = item$Min)
                }
              )
            }
          )
        })
      )
    })
  })
  
  # Dynamic UI for removing selected practices
  output$removePracticeUI <- renderUI({
    req(selectedItems())
    selectInput("removePractice", "Select Practice to Remove:", 
                choices = unique(selectedItems()$Practice), multiple = TRUE)
  })
  
  # Add selected items and their amounts to the reactive value
  observeEvent(input$add, {
    req(input$practice)
    for (practice in input$practice) {
      items <- practices_data %>% 
        filter(IncentiveType %in% input$incentiveType, Practice == practice)
      
      for (i in 1:nrow(items)) {
        if (input[[paste0("select_", practice, "_", i)]]) {
          item <- items[i, ]
          amount <- if(item$IncentiveType %in% c("Hosting Field Day", "Grazing School")) 1 else as.numeric(input[[paste0("amount_", practice, "_", i)]])
          new_row <- data.frame(IncentiveType = item$IncentiveType,
                                Practice = item$Practice,
                                Item = item$Item,
                                Rate = item$Rate,
                                Unit = item$Unit,
                                Amount = amount,
                                stringsAsFactors = FALSE)
          
          selectedItems(rbind(selectedItems(), new_row))
        }
      }
    }
  })
  
  # Remove selected practices from the reactive value
  observeEvent(input$remove, {
    req(input$removePractice)
    selectedItems(selectedItems() %>% filter(!Practice %in% input$removePractice))
  })
  
  # Render the table of selected items
  output$selectedItemsTable <- renderTable({
    req(selectedItems())
    selectedItems()
  }, striped = TRUE, hover = TRUE, align = "l")
  
  # Calculate the total payments
  calculatePayment <- reactive({
    req(input$calculate)
    selected_items <- selectedItems()
    
    payments <- selected_items %>% 
      rowwise() %>% 
      mutate(Payment = Amount * Rate) %>% 
      ungroup()
    
    payments
  })
  
  # Display the result as a summary table
  output$totalPaymentTable <- renderTable({
    req(calculatePayment())
    payments <- calculatePayment() %>% 
      group_by(IncentiveType, Practice, Item, Unit) %>% 
      summarise(Total_Payment = sum(Payment), .groups = 'drop') %>% 
      arrange(IncentiveType, Practice, Item)
    
    total_sum <- sum(payments$Total_Payment)
    payments <- rbind(payments, data.frame(IncentiveType = "", Practice = "", Item = "Total", Unit = "", Total_Payment = total_sum))
    
    payments
  }, striped = TRUE, hover = TRUE, align = "l")
}
