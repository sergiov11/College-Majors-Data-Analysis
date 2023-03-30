library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(scales)
library(bslib)

major_df <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/college-majors/recent-grads.csv", stringsAsFactors = FALSE)

# sue's code
majors_gender <- major_df %>% group_by(Major_category) %>%summarize(Total= sum(Total, na.rm = TRUE),Men= sum(Men, na.rm = TRUE),Women=sum(Women, na.rm = TRUE))

man_major <- major_df %>% group_by(Major_category) %>% summarize(total = sum(Men, na.rm = T))%>% mutate(gender = rep("Men",time=length(majors_gender$Major_category)))


women_major <- major_df %>% group_by(Major_category) %>% summarize(total = sum(Women, na.rm = T)) %>% mutate(gender = rep("Women",time=length(majors_gender$Major_category)))

list_df <- rbind(man_major, women_major)

# Sergio's code
low_wage_majors <- major_df %>% group_by(Major_category) %>% summarize(Total = sum(Total, na.rm = TRUE), Low_wage_jobs = sum(Low_wage_jobs, na.rm = TRUE))


my_theme <- bs_theme(
  bg = "black",
  fg = "blue",
  primary = "white"
)

my_theme <- bs_theme_update(my_theme, bootswatch = "yeti")

intro_page <- tabPanel (
  "Introduction",
  htmlOutput("summary")
  
)


# Intro page
intro_page <- tabPanel (
  "Introduction",
  htmlOutput("Intro")
)



sergio_page <- tabPanel(
  # Application title
  "Number of Low Wage Jobs By Major Category",
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "user_selection",
                  label = "Major Categories",
                  choices = low_wage_majors$Major_category,
                  selected = "Interdisciplinary",
                  multiple = TRUE)),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput(outputId = "sergios_plot"),
      p(""),
      p("Our team decided to to a graph based on the major categories with the most low wage jobs because it can be useful information for someone going into college who is looking to major into something they might like but also make good money. Although it is important to clarify the fact that one should follow their passion in college, for some students economic stability is important for them and their families and we want to make information like that more available for everybody.")
    )
  )
  
)

dylan_page <- tabPanel(
  # Application title
  "Major Categories",
  
  sidebarLayout(
    sidebarPanel(
      select_widget_type <-
        selectInput(
          inputId = "Type",
          label = "Choose an option to compare:",
          choices = c("Total_Population", "Employed_Population", "Median_Income", "Employment_Rate"),
          selectize = TRUE,
          selected = "Total_Population"
        ),
      
      select_widget_color <-
        selectInput(
          inputId = "Color",
          label = "Choose a color:",
          choices = c("red", "blue", "green", "cyan", "purple", "gold", "orange", "magenta"),
          selectize = TRUE,
          selected = "magenta"
        )
    ),
  
  
  # Show the plot
  mainPanel(
    plotlyOutput("Major_Plot"),
    p(""),
    p("One of our teams questions we wanted to explore was how different majors and their populations stacked up with each other. Here we have a bar chart with the Major Categories, with 4 options to choose to compare majors; Total Population, Employed Population, Median Income, and Employment Rate. There is also an option to choose a color for the bars. Looking at the Median Income chart, it is interesting to see Engineering Majors making a lot more money on average than the other majors, and Psychology & Social Work having the lowest median income. Another interesting thing is that the employement rate for all majors are pretty similar, with Agriculture having the highest Employment Rate. This makes sense, as Agriculture is a trade that is very important and doesn't have a lot of interest.")
  )
  )
)


sue_page <- tabPanel(
  # Application title
  "Majors & Gender",
  
  sidebarLayout(
    sidebarPanel(
      select_widget <-
        selectInput(
          inputId = "major_selection",
          label = "Majors",
          choices = list_df$Major_category,
          selectize = TRUE,
          multiple = TRUE,
          selected = "Agriculture & Natural Resources"
        ),
      radio_widget <- radioButtons(
        inputId = "gender_selection",
        label = "Gender",
        choices = list("MEN" = "Men", "WOMEN" = "Women"),
        selected = "Men"
      )),
    
    mainPanel(
      plotlyOutput("major_gender"),
      p(),
      p("One of our team's main questions is what are the gender demographics for the majors. Therefore, we made it as a stacked bar graph to visualize both the gender demographics and how many people in total are in the majors. Apperently, it is obvious that business has the largest number of students and the most equal gender ratio among the majors, whereas Education, Health and Engineering were the majors with unequal gender ratio.")
    )
  )
  
)

conclusion_page <- tabPanel (
  "Conclusion",
  htmlOutput("Conclusion")
)

ui <- navbarPage(
  theme = my_theme,
  "College Majors & Success",
  intro_page,
  sergio_page,
  dylan_page,
  sue_page,
  conclusion_page
  
  
)