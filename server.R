library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(scales)
library(bslib)

major_df <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/college-majors/recent-grads.csv", stringsAsFactors = FALSE)

server <- function(input, output) {
  
  
  output$Intro <- renderUI({
    p1 <- paste("Welcome to the \"Recent Grads\" Data Analysis Shiny App, created by our team. Our goal is to understand the connection between college majors and career success by analyzing the Recent Grads dataset.
Our app consists of three pages, each offering unique insights.The first page focuses on major categories with the most low-wage jobs, providing valuable information for those considering college and their future careers.
The second page features a graph that displays the employment rate, median income, and total population of selected Major Categories. The last page showcases a graph analyzing gender demographics and the total population in each Major. 
It's important to note that the Recent Grads dataset has limitations such as a limited sample size, potential errors or inconsistencies, and only including graduates from the US. These limitations should be considered when interpreting the results.
To sum it up, understanding the relationship between college majors and career success is crucial in making informed education and career decisions. Our app provides valuable insights to help guide individuals toward success and satisfaction in the workforce. Let's dive into the data")
    
    
    p2 <- paste("<br/><br/><img src='https://media.istockphoto.com/id/186879365/photo/cost-of-high-education-illustration.jpg?s=612x612&w=0&k=20&c=XPeuBtEGEC-Cg8MWHRK0T4qYm2tGMjH6TofR75gxKPI=' height='200' width='200'><br/><br/>")
    
    p3 <- paste("Sources: Casselman, B. (n.d.). Recent-Grads [Data set]. FiveThirtyEight. Retrieved March 8, 2023, from https://github.com/fivethirtyeight/data/blob/master/college-majors/recent-grads.csv 
    
                
        Image: Cost of high education [Illustration]. (n.d.). iStock. Retrieved March 8, 2023, from https://www.istockphoto.com/photo/cost-of-high-education-illustration-gm186879365-19086029")
    
    p4 <- paste("<br/><br/><img src='https://img95.lovepik.com/photo/40108/3289.gif_wh300.gif' height='200' width='200'><br/><br/>")
    
    HTML(paste(p1, p2, p4, "<br/><br/><i style='font-size:12px;'>", p3, "</i>", sep = ''))
  
    })
  
  
  output$sergios_plot <- renderPlotly({
    
    low_wage_majors <- major_df %>% group_by(Major_category) %>% summarize(Total = sum(Total, na.rm = TRUE), Low_wage_jobs = sum(Low_wage_jobs, na.rm = TRUE))
    
    majors_wage_df <- low_wage_majors %>% filter(Major_category %in% input$user_selection)
    
    # Sergio's Plot
    sergios_plot <- ggplot(data = majors_wage_df) + 
      geom_point(aes(x = Low_wage_jobs, 
                     y = Total, 
                     color = Major_category,
                     text = paste("Major:", Major_category, "
                                  Number of low wage jobs: ", Low_wage_jobs,
                                  " 
                                  Total:", Total))) + 
      labs(title = "Major Category & Number of Low Wage Jobs", 
           x = "Number of Low Wage Jobs per Major Category", 
           y = "Total number of Jobs",
           color = "Major Category") +
      scale_color_brewer(palette = "Set1") +
      scale_x_continuous(labels = label_number_si()) +
      scale_y_continuous(labels = label_number_si())
    
    return(ggplotly(sergios_plot, tooltip = c("text")))
    
  })
  
  
  output$Major_Plot <- renderPlotly({
    # Initialize Table with all Options
    majors <- major_df %>% group_by(Major_category) %>% summarize(Total_Population = sum(Total, na.rm = TRUE), Employed_Population = sum(Employed, na.rm = TRUE), Median_Income = mean(Median, na.rm = TRUE)) %>% mutate(Employment_Rate = (Employed_Population / Total_Population))
    
    # Condense Table down to Selected Column
    majors <- majors %>% select(Major_category, Selected_Column = input$Type)
    
    
    major_plot <- ggplot(data = majors, aes(x = Selected_Column, y = Major_category, text = paste("Value:", Selected_Column, "Major:", Major_category))) + geom_bar(stat = "identity", fill = input$Color) +scale_x_continuous(labels = label_number_si())+ labs(title = "Comparing different Major Categories:", x = input$Type, y = "Majors")

    major_plot <- ggplotly(major_plot, tooltip = c("text"))
    
    return(major_plot)
  })
  
  #sue
  output$major_gender <- renderPlotly({
    majors_gender <- major_df %>% group_by(Major_category) %>%summarize(Total= sum(Total, na.rm = TRUE),Men= sum(Men, na.rm = TRUE),Women=sum(Women, na.rm = TRUE))
    
    man_major <- major_df %>% group_by(Major_category) %>% summarize(total = sum(Men, na.rm = T))%>% mutate(gender = rep("Men",time=length(majors_gender$Major_category)))
    
    
    women_major <- major_df %>% group_by(Major_category) %>% summarize(total = sum(Women, na.rm = T)) %>% mutate(gender = rep("Women",time=length(majors_gender$Major_category)))
    
    list_df <- rbind(man_major, women_major)
    
    filtered_df <- list_df %>%filter(gender == input$gender_selection) %>%filter(Major_category %in% input$major_selection)
    
    major_gender<- ggplot(filtered_df, aes(x = Major_category, y = total, fill= Major_category,text = paste("Major:", Major_category,"Total:",total))) + labs(title = "Major Category & Gender", x = "Major Categories", y = "Total", fill = "Major") + geom_bar(stat = "identity", position = "stack")+coord_flip()+ scale_y_continuous(labels = label_number_si())
    
    return(ggplotly(major_gender, tooltip = c("text")))

  })
 
  
  

    output$Conclusion <- renderUI({
      
      p1 <- paste("In this \"Recent Grads\" Data Analysis Shiny App, we analyzed the correlation between college majors and career success. Our goal was to uncover valuable insights and trends from the \"Recent Grads\" dataset.

The first page analyzed low-wage jobs in Major Categories and revealed that more low-wage jobs are associated with more total jobs in a Major Category. The Business Major had a high number of both low-wage and total jobs. This highlights the importance of considering job opportunities and low-wage jobs when choosing a major.

The second page compared different Major Categories and found that Engineering Majors had a higher median income compared to other majors. This provides valuable information for individuals making educational and career decisions.

The third page analyzed gender demographics and the total population in each Major. The Business Major had the largest total population and an equal representation of men and women. This highlights the importance of considering both the total population and gender representation in a particular Major Category.

Our analysis revealed several important insights and trends regarding the correlation between a person's success in the work industry and their college major. These takeaways have broader implications for individuals making career decisions, as they can help individuals make informed decisions about their education and future careers, leading to greater success and fulfillment in the workforce. Our Shiny R app provides a valuable resource for individuals looking to understand the data trends and patterns in the \"Recent Grads\" dataset.")
      
      
      
      HTML(paste(p1, sep = '<br/>'))
      
    })
    
  
}