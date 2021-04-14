
#Importing Libraries
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(googleway)
library(ggplot2)
library(ggmap)

#Importing dataset 
data <- dataset
countries <- data$Country


#Importing predicted models(diabetics and Blood Pressure)


#R shiny UI

ui<-tagList(
    tags$style("html,body{background-color: white;}
                .container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
                #myimg{
                    width:100%;
                }
               @media screen and (min-width: 1800px){
                .container{
                    width: 1800px;
                    
                }
               }"),
    tags$div(class="container",
             
             dashboardPage(
                 
                 
                 
                 
                 #Title
                 dashboardHeader(title = 'DIABETES & BP', titleWidth = 300),
                 
                 #Sidebars
                 dashboardSidebar(width = 300,
                                  sidebarMenu(menuItem("Home Page", tabName = "Home", icon = icon('home')),
                                              menuItem("Diabetes prediction", tabName = "diapred", icon = icon('syringe')),
                                              menuItem("Blood Pressure prediction", tabName = 'bppred', icon = icon('tint')))),
                 #Tabs layout
                 dashboardBody(tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
                               #Home Page contents
                               tabItems(tabItem('Home',
                                                #Content in Home page
                                                h3("Welcome to Analysis and Modelling of Diabetes and Raised Blood pressure Around the World"),
                                                h3("Select a option from Side Bars")),
                                        
                                        
                                        #Diabetics Page content
                                        tabItem('diapred',
                                                #Content in Diabetic prediction
                                                box(title = "Enter your data", solidHeader = TRUE,status='primary', width = 12,height=NULL,
                                                    splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                                
                                                                radioButtons("gender", "Enter your gender?", choices = c("male", "female")),
                                                                div(),
                                                                numericInput("Year", "Enter Year",1996),
                                                                div(),
                                                                numericInput("Obesity", "Obesity value",50),
                                                                div(),
                                                                numericInput("Blood_pressure", "Enter your BP",1.7),
                                                                div(),
                                                                selectInput("Country", "Select Country", choices = countries),
                                                                actionButton('Pred_d','Predict', icon = icon('search'),class = "btn-primary",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                                
                                                #display the predicted results
                                                box(title = 'Prediction result', status = 'success', solidHeader = TRUE, width = 4, height = NULL,
                                                    div(h5(' your Diabetic Prevalence:')),
                                                    verbatimTextOutput("diabetic_pred_value", placeholder = TRUE),
                                                    div(h5('Your risk factor')),
                                                    verbatimTextOutput("risk_val", placeholder = TRUE),
                                                    div(h5('Total count in your region')),
                                                    verbatimTextOutput("count_val", placeholder = TRUE)),
                                                
                                                #Line graph of diabetics vs year(F/M)
                                                box(status = 'primary',title='Output plots',width=8,
                                                    splitLayout(cellWidths = c('50%', '50%')),
                                                    box(status = 'primary', title = 'Line graph plot'
                                                        ,plotOutput('line_diabetic')#----plot(line graph)
                                                    ),
                                                    
                                                    #Histogram plot of diabetics 
                                                    box(status = 'primary', title = 'Frequency graph plot'
                                                        #plotOutput('histo_diabetic')----plot(freq graph)
                                                        
                                                    ))# end of output plots
                                                
                                                
                                                
                                        ),
                                        #Blood Pressure Page content
                                        tabItem('bppred',
                                                #Content in Blood Pressure prediction
                                                box(title = "Enter your data", solidHeader = TRUE,status='primary', width = 12,
                                                    splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                                
                                                                radioButtons("gender", "Enter your gender?", choices = c("male", "female")),
                                                                numericInput("age", "Enter your age",0),
                                                                numericInput("Mean_BMI", "What is your Mean_BMI value",50),
                                                                numericInput("height", "What is your height (Meters) ?",1.7),
                                                                numericInput("weight", "What is your weigth (kg) ?",40),
                                                                numericInput("location", "Enter location",1.7),
                                                                actionButton('Pred','Predict', icon = icon('search'),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                                )),
                                                
                                                #display the predicted results
                                                box(title = 'Prediction result', status = 'success', solidHeader = TRUE, width = 4, height = 260,
                                                    div(h5(' your Blood Pressure Prevalence:')),
                                                    verbatimTextOutput("bloodpressure_pred_value", placeholder = TRUE),
                                                    div(h5('Your risk factor')),
                                                    verbatimTextOutput("risk_val", placeholder = TRUE),
                                                    div(h5('Total count in your region')),
                                                    verbatimTextOutput("count_val", placeholder = TRUE)),
                                                
                                                #Line graph of blood pressure vs year(F/M)
                                                box(status = 'primary',title='Output plots',width=8,
                                                    splitLayout(cellWidths = c('50%', '50%')),
                                                    box(status = 'primary', title = 'Line graph plot'
                                                        #plotOutput('line_bp')----plot(line graph)
                                                    ),
                                                    
                                                    #Histogram plot of blood pressure 
                                                    box(status = 'primary', title = 'Frequency graph plot'
                                                        #plotOutput('histo_bp')----plot(freq graph)
                                                        
                                                    ))# end of output plots
                                                
                                        ) )# end of tab items
                               
                               
                               
                               
                 )# end of dashboard body
                 
                 
             )))
    





#Importing Model
model_dia <- readRDS(file = 'model_1.rda')
new_val_dia=tibble(Sex=c('Female','Male') , Year=c(1981),
                   LogObesity=c(0.6),Prevalence_raised_blood_pressure=c(0.13),Country=c('Bahamas'))
diabetic_predict = predict.lm(model_dia, new_val_dia)










server <- function(input, output,session){
    
    
    
    
    observeEvent(input$Pred_d,{
        showModal(modalDialog(
            title = "Important message",
            "This is an important message!"
        ))
    })
}
    
    
    
   
    
    



shinyApp(ui, server)