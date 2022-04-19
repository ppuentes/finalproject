#Libraries

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(plotly)
library(gganimate)
library(ggthemes)
library(rsconnect)
library(scales)


#Reading the dataset 

health <- read_csv("heart_2020_cleaned.csv")

#Data
bmi_groups <- health %>% 
  clean_names() %>% 
  mutate(bmi_group=case_when(bmi<18.5~"Underweight",
                             bmi>=18.5 & bmi<=24.9~"Normal Weight",
                             bmi>=25 & bmi<=29.9~"Overweight",
                             bmi>=30 & bmi<=34.9~"Obesity Class I",
                             bmi>=35 & bmi<=35.9~"Obesity Class II",
                             bmi>=36~"Extreme Obesity"))

ui <- fluidPage(
  numericInput(inputId = "weight",
               label = "Weight (Kg)",
               value = "50"),
  numericInput(inputId = "height",
               label = "Height (Cm)",
               value = "170"),
  selectInput(inputId = "sex",
               label = "Sex",
               choices = list(Female = "Female", Male = "Male")),
  selectInput(inputId = "age_cat",
               label = "Age Category",
               choices = bmi_groups %>% 
                 arrange(age_category) %>% 
                 distinct(age_category) %>% 
                 pull(age_category),
               multiple=FALSE),
  selectInput(inputId = "phys_act",
               label = "Physical Activity",
               choices = list(Yes = "Yes", No = "No")),
  submitButton(text="Ready!"),
  plotOutput(outputId = "Percentage_plot")
  )


server<-function(input,output){
 
  output$Percentage_plot <- renderPlot({
   
    
    weight <- input$weight
    height <- input$height
    
    dataset <- tibble(weight,
                      height) %>%
      mutate(BMI_val = weight/((height/100)^2),
             bmi_cat = case_when(BMI_val<18.5~"Underweight",
                                 BMI_val>=18.5 & BMI_val<=24.9~"Normal Weight",
                                 BMI_val>=25 & BMI_val<=29.9~"Overweight",
                                 BMI_val>=30 & BMI_val<=34.9~"Obesity Class I",
                                 BMI_val>=35 & BMI_val<=35.9~"Obesity Class II",
                                 BMI_val>=36~"Extreme Obesity"))
    user_bmi <- dataset$bmi_cat
    
    app_data <- bmi_groups %>%
      filter(bmi_group == user_bmi,
             sex == input$sex,
             age_category == input$age_cat,
             physical_activity == input$phys_act) %>% 
      group_by(heart_disease) %>% 
      mutate(prop= n()/sum(n()))
  
    
    bar_distribution <- app_data%>%
      #summarize(heart_disease_prevalence =  (sum(heart_disease == "Yes")/n())*100,
                #no_heart_disease = (sum(heart_disease == "No")/n())*100) %>%
      ggplot()+
      geom_bar(aes(x=1, fill = heart_disease), position = "stack")+
      labs(title= "Demographics of Similar Characteristics to Those Input and the Proportion of Heart Disease Prevalence")+
      theme_clean()
    
    pie <- bar_distribution +
      coord_polar("y", start= 0)+
      scale_fill_brewer(palette="Blues")+
      #labs(x= " ", y = " ")+
      theme_void()#+
      geom_text(aes(x= 1, y= prop, label= " "))
    
    pie
  })
}

shinyApp(ui=ui, server=server)