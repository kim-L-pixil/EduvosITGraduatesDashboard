library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(jsonlite)

data <- read.csv("C:/Users/nozib/OneDrive/Documents/graduate_survey.csv")

#Data Cleaning
data <- my_data %>% 
  select(Campus, StudyField............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................., Role, ProgLang, Databases, WebFramework, Platform, AISearch, AITool) %>%
  mutate(across(everything(), ~replace_na(.x, "Unknown")))
  
 #User Interface
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Eduvos IT Graduates Survey Analysis"), 
  sidebarLayout(
    sidebarPanel(
      selectInput("studyField", "Select Study Field:", choices = unique(data$StudyField.............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................)),
      selectInput("role", "Select Role:", choices = unique(data$Role))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Industry Analysis", plotOutput("industryPlot")),
        tabPanel("Programming Languages", plotOutput("progLangPlot")),
        tabPanel("Databases", plotOutput("dbPlot")),
        tabPanel("Platforms", plotOutput("platformPlot")),
        tabPanel("Web Frameworks", plotOutput("webFrameworkPlot")),
        tabPanel("AI Tools", plotOutput("aiToolPlot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    my_data %>% 
      filter(StudyField............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................. == input$StudyField............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................., Role == input$Role)
  })
  
  output$progLangPlot <- renderPlot({
    prog_lang_count <- filtered_data() %>% 
      separate_rows(ProgLang, sep = ",") %>% 
      count(ProgLang, sort = TRUE)
    
    ggplot(prog_lang_count, aes(x = reorder(ProgLang, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Top Programming Languages",
        x = "Programming Language",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$dbPlot <- renderPlot({
    db_count <- filtered_data() %>% 
      separate_rows(Databases, sep = ",") %>% 
      count(Databases, sort = TRUE)
    
    ggplot(db_count, aes(x = reorder(Databases, n), y = n)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      labs(
        title = "Top Databases",
        x = "Database",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$webFrameworkPlot <- renderPlot({
    web_framework_count <- filtered_data() %>% 
      separate_rows(WebFramework, sep = ",") %>% 
      count(WebFramework, sort = TRUE)
    
    ggplot(web_framework_count, aes(x = reorder(WebFramework, n), y = n)) +
      geom_bar(stat = "identity", fill = "purple") +
      coord_flip() +
      labs(
        title = "Top Web Frameworks",
        x = "Web Framework",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$platformPlot <- renderPlot({
    platform_count <- filtered_data() %>% 
      separate_rows(Platform, sep = ",") %>% 
      count(Platform, sort = TRUE)
    
    ggplot(platform_count, aes(x = reorder(Platform, n), y = n)) +
      geom_bar(stat = "identity", fill = "green") +
      coord_flip() +
      labs(
        title = "Top Platforms",
        x = "Platform",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$aiToolPlot <- renderPlot({
    ai_tool_count <- filtered_data() %>% 
      separate_rows(AITool, sep = ",") %>% 
      count(AITool, sort = TRUE)
    
    ggplot(ai_tool_count, aes(x = reorder(AITool, n), y = n)) +
      geom_bar(stat = "identity", fill = "blue") +
      coord_flip() +
      labs(
        title = "Top AI Tools",
        x = "AI Tool",
        y = "Count"
      ) +
      theme_minimal()
  })
}
  
# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    data %>% 
      filter(StudyField............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................. == input$StudyField............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................., Role == input$Role)
  })
  
  output$industryPlot <- renderPlot({
    industry_data <- filtered_data() %>% 
      separate_rows(Industry, sep = ",") %>% 
      count(Industry, sort = TRUE)
    
    ggplot(industry_data, aes(x = reorder(Industry, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Top Industries",
        x = "Industry",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$progLangPlot <- renderPlot({
    prog_lang_count <- filtered_data() %>% 
      separate_rows(ProgLang, sep = ",") %>% 
      count(ProgLang, sort = TRUE)
    
    ggplot(prog_lang_count, aes(x = reorder(ProgLang, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Top Programming Languages",
        x = "Programming Language",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$dbPlot <- renderPlot({
    db_count <- filtered_data() %>% 
      separate_rows(Databases, sep = ",") %>% 
      count(Databases, sort = TRUE)
    
    ggplot(db_count, aes(x = reorder(Databases, n), y = n)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      labs(
        title = "Top Databases",
        x = "Database",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$webFrameworkPlot <- renderPlot({
    web_framework_count <- filtered_data() %>% 
      separate_rows(WebFramework, sep = ",") %>% 
      count(WebFramework, sort = TRUE)
    
    ggplot(web_framework_count, aes(x = reorder(WebFramework, n), y = n)) +
      geom_bar(stat = "identity", fill = "purple") +
      coord_flip() +
      labs(
        title = "Top Web Frameworks",
        x = "Web Framework",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$platformPlot <- renderPlot({
    platform_count <- filtered_data() %>% 
      separate_rows(Platform, sep = ",") %>% 
      count(Platform, sort = TRUE)
    
    ggplot(platform_count, aes(x = reorder(Platform, n), y = n)) +
      geom_bar(stat = "identity", fill = "green") +
      coord_flip() +
      labs(
        title = "Top Platforms",
        x = "Platform",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$aiToolPlot <- renderPlot({
    ai_tool_count <- filtered_data() %>% 
      separate_rows(AITool, sep = ",") %>% 
      count(AITool, sort = TRUE)
    
    ggplot(ai_tool_count, aes(x = reorder(AITool, n), y = n)) +
      geom_bar(stat = "identity", fill = "blue") +
      coord_flip() +
      labs(
        title = "Top AI Tools",
        x = "AI Tool",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$dataTable <- renderTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)
  

  
  
  
  
  
  
  
  
  