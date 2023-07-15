library(tidyverse)
library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)
library(fmsb)
library(plotly)

joined_table <- read_xlsx("joined_table.xlsx")
dashboard <- read_xlsx("dashboard.xlsx")
avg_movements <- read.xlsx("avg_movements.xlsx")

joined_table <- joined_table %>%
  mutate(Pitch_Type = case_when(
    Pitch_Type == "FF" ~ "Fourseam Fastball",
    Pitch_Type == "SI" ~ "Sinker",
    Pitch_Type == "FC" ~ "Cutter",
    Pitch_Type %in% c("SW", "SL", "SV") ~ "Slider",
    Pitch_Type == "CU" ~ "Curveball",
    Pitch_Type == "FS" ~ "Splitter",
    Pitch_Type == "CH" ~ "Changeup",
    TRUE ~ Pitch_Type
  )) %>%
  arrange(full_name) %>%
  select(full_name, Pitch_Type, Pitches, plus, MPH, `H-Mov`, `V-Mov`, RPM, gyro, VAA, wOBA, Usage., Hard.) # Add wOBA variable

dashboard <- dashboard %>% 
  mutate(Pitch_Type = case_when(
    Pitch_Type == "FF" ~ "Fourseam Fastball",
    Pitch_Type == "SI" ~ "Sinker",
    Pitch_Type == "FC" ~ "Cutter",
    Pitch_Type == "SL" ~ "Slider",
    Pitch_Type == "SW" ~ "Sweeper",
    Pitch_Type == "SV" ~ "Slurve",
    Pitch_Type == "CU" ~ "Curveball",
    Pitch_Type == "FS" ~ "Splitter",
    Pitch_Type == "CH" ~ "Changeup",
    TRUE ~ Pitch_Type
  ))

avg_movements <- avg_movements %>% 
  mutate(Pitch_Type = case_when(
    Pitch_Type == "FF" ~ "Fourseam Fastball",
    Pitch_Type == "SI" ~ "Sinker",
    Pitch_Type == "FC" ~ "Cutter",
    Pitch_Type == "SL" ~ "Slider",
    Pitch_Type == "SW" ~ "Sweeper",
    Pitch_Type == "SV" ~ "Slurve",
    Pitch_Type == "CU" ~ "Curveball",
    Pitch_Type == "FS" ~ "Splitter",
    Pitch_Type == "CH" ~ "Changeup",
    TRUE ~ Pitch_Type
  ))

ui <- fluidPage(
  titlePanel("2023 Unique+ Dashboard"),
  theme = shinythemes::shinytheme("yeti"),
  sidebarLayout(
    sidebarPanel(
      # Search input for pitcher name
      selectizeInput('name',
                     'Select or Type Pitcher:',
                     choices = c("", unique(joined_table$full_name)),
                     multiple = FALSE,
                     options = list(
                       placeholder = 'Enter pitcher name',
                       create = TRUE
                     )
      ),
      checkboxGroupInput('Pitch_Type',
                         'Select Pitch Types:',
                         choices = c('All', 'Fourseam Fastball', 'Sinker', 'Cutter', 'Slider', 'Curveball', 'Changeup', 'Splitter'),
                         selected = 'All')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Leaderboard", DT::DTOutput("leaders")),
        tabPanel("Unique+ Plots",
                 plotOutput("wobaPlot"),
                 tableOutput("table"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  searched_pitcher <- reactive({
    input$name
  })
  
  # Filtered table based on pitcher name and selected pitch types
  filtered_table <- reactive({
    if (!is.null(input$name) && input$name != "") {
      # Filter based on search name
      joined_table %>% filter(full_name == input$name)
    } else {
      # No filter
      joined_table
    }
  })

  

  
  output$leaders <- DT::renderDT({
    if (input$Pitch_Type == "All" || is.null(input$Pitch_Type) || length(input$Pitch_Type) == 0) {
      # If "All" or no pitch types selected, show all pitches
      filtered_table() %>%
        group_by(full_name, Pitch_Type) %>%
        summarise(Pitches = sum(Pitches, na.rm = TRUE),
                  `Unique+` = round(mean(plus, na.rm = TRUE), 0),
                  Velocity = round(mean(MPH, na.rm = TRUE), 1),
                  `Horizontal Mvt.` = round(mean(`H-Mov`, na.rm = TRUE), 2),
                  `Vertical Mvt.` = round(mean(`V-Mov`, na.rm = TRUE), 2),
                  RPM = round(mean(RPM, na.rm = TRUE), 2),
                  Gyro_Spin = mean(gyro, na.rm = TRUE),
                  VAA = round(mean(VAA, na.rm = TRUE), 2),
                  wOBA = round(mean(wOBA, na.rm = TRUE),2),
                  `Usage.` = round(sum(`Usage.`, na.rm = TRUE),2)) %>% 
        arrange(desc(`Unique+`))
    } else {
      # Filter based on selected pitch types
      filtered_table() %>%
        filter(Pitch_Type %in% input$Pitch_Type) %>%
        group_by(full_name, Pitch_Type) %>%
        summarise(Pitches = sum(Pitches, na.rm = TRUE),
                  `Unique+` = round(mean(plus, na.rm = TRUE), 0),
                  Velocity = round(mean(MPH, na.rm = TRUE), 1),
                  `Horizontal Mvt.` = round(mean(`H-Mov`, na.rm = TRUE), 2),
                  `Vertical Mvt.` = round(mean(`V-Mov`, na.rm = TRUE), 2),
                  RPM = round(mean(RPM, na.rm = TRUE), 2),
                  Gyro_Spin = mean(gyro, na.rm = TRUE),
                  VAA = round(mean(VAA, na.rm = TRUE), 2),
                  wOBA = round(mean(wOBA, na.rm = TRUE),2),
                  `Usage.` = round(sum(`Usage.`, na.rm = TRUE),2)) %>% 
        arrange(desc(`Unique+`))
    }
  })
  
  output$wobaPlot <- renderPlot({
    selected_pitcher <- input$name
    
    if (is.null(selected_pitcher) || selected_pitcher == "") {
      # No pitcher selected
      p <- ggplot() +
        labs(title = "Select a Pitcher") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      theme_void()
    } else {
      filtered_dashboard <- dashboard %>%
        filter(full_name == selected_pitcher)
      
      available_pitch_types <- filtered_dashboard$Pitch_Type
      
      avg_movements_filtered <- avg_movements %>%
        filter(hand == filtered_dashboard$hand,
               Pitch_Type %in% available_pitch_types) %>%
        select(avg_hmov, avg_vmov, Pitch_Type)
      
      p <- ggplot() +
        geom_point(data = filtered_dashboard, aes(x = `H-Mov`, y = `V-Mov`, color = Pitch_Type),
                   shape = 16, size = 20) +
        geom_point(data = avg_movements_filtered, aes(x = avg_hmov, y = avg_vmov, color = Pitch_Type),
                   size = 20, shape = 1) +
        xlim(-22, 22) +
        ylim(-22, 22) +
        labs(
          title = paste(selected_pitcher, "Movement Plot"),
          subtitle = "Open Circles Are League Average Movement", 
          y = "Induced Vertical Break",
          x = "Horizontal Break",
          fill = "Pitch Type"
        ) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 25),
              plot.subtitle = element_text(hjust = 0.5, size = 15))
    }
    
    # Print the plot with the subtitle
    print(p)
  })
  
  
  
  output$table <- renderTable({
    filtered_table() %>%
      group_by(Pitch_Type) %>%
      summarise(`Unique+` = sprintf("%.0f", round(mean(plus), 0)),
                `Usage Percentage` = sprintf("%.0f", round(100 * sum(`Usage.`, na.rm = TRUE), 0))) %>% 
      mutate(`Usage Percentage` = paste0(`Usage Percentage`, "%")) %>%
      arrange(desc(as.numeric(gsub("%", "", `Unique+`))))
  })
}

shinyApp(ui = ui, server = server)
