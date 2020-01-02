library(shiny)
library(tidyr)
library(dplyr)
library(data.table)
library(caret)
library(C50)
library(shinythemes)
library(RGtk2)
library(gWidgets)
library(tcltk2)
library(ROSE)
library(rsconnect)

ui<-fluidPage(
  titlePanel("prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num","Area_Service",1),
      numericInput("num1","Hospital.County",1),
      numericInput("num2","Hospital.Id",1),
      numericInput("num3","Age",1),
      numericInput("num4","Gender",1),
      numericInput("num5","Cultural_group",1),
      numericInput("num6","ethnicity",1),
      numericInput("num7","Admission_type",1),
      numericInput("num8","ccs_diagnosis_code",1),
      numericInput("num9","apr_mdc_description",1),
      numericInput("num10","Code_illness",1),
      numericInput("num11","Mortality.risk",1),
      numericInput("num12","Surg_Description",1),
      numericInput("num13","Payment_typology_1",1),
      numericInput("num14","Emergency.dept_yes.No",1),
      numericInput("num15","Tot_charg",1),
      numericInput("num16","Tot_cost",1),
      numericInput("num17","ratio_of_total_costs_to_total_charges",1)

      
    ),
    mainPanel(
      tableOutput("distplot")
    
    )
  )
)
server<-function(input,output) {
  output$distplot<-renderTable({
    
    #Insurance.Dataset.<-read.csv("Macintosh Sierra/Users/user/Downloads/Insurance Dataset.csv")
    Insurance <- read.csv("C:/Users/piyush/Downloads/Insurance Dataset .csv")
    
    # generate bins based on input$bins from ui.R
    View(Insurance)
    summary(Insurance)
    is.na(Insurance)
    Insurance[Insurance==""] <- NA
    View(Insurance)
    install.packages("imputeMissings")
    library(imputeMissings)
    imputeMissings::compute(Insurance, object = NULL, method = "median/mode", flag = FALSE)
    is.na(Insurance)
    colSums(is.na(Insurance))
    newdata <- imputeMissings::impute(Insurance,flag = FALSE)
    Insurance[Insurance=="NA"] <- newdata
    colSums(is.na(newdata))
    View(newdata)
    summary(newdata)
    attach(newdata)
    
    newdata$Area_Service <- as.numeric(newdata$Area_Service)
    newdata$Cultural_group <- as.numeric(newdata$Cultural_group)
    newdata$Admission_type <- as.numeric(newdata$Admission_type)
    newdata$Mortality.risk <- as.numeric(newdata$Mortality.risk)
    newdata$Payment_typology_1 <- as.numeric(newdata$Payment_typology_1)
    newdata$Age <- as.numeric(newdata$Age)
    newdata$Gender <- as.numeric(newdata$Gender)
    newdata$Surg_Description <- as.numeric(newdata$Surg_Description)
    newdata$Emergency.dept_yes.No <- as.numeric(newdata$Emergency.dept_yes.No)
    newdata$Hospital.County <- as.numeric(newdata$Hospital.County)
    newdata$ethnicity <- as.numeric(newdata$ethnicity)
    newdata$Result <- as.numeric(newdata$Result)
    
    
    ########MOdel Building#########
    
    #########Dropped columns########
    newdata[, c("Hospital.Name","Certificate_num","zip_code_3_digits","Days_spend_hsptl",
                "year_discharge","ccs_diagnosis_description","Home.or.self.care.","apr_drg_description","apr_mdc_description","ccs_procedure_code","ccs_procedure_description",
                "Description_illness","payment_typology_2","payment_typology_3",
                "Weight_baby","Abortion")] <- list(NULL)
    View(newdata)
    
    
    
    library(caTools)
    
    split <- sample.split(newdata$Result,SplitRatio = 0.7)
    train <- subset(newdata, split==TRUE)
    test <- subset(newdata, split==FALSE)
    ### samplings
    
    #for under sampling
    data_balanced_under <- ovun.sample(Result ~ ., data = train, method = "under", p=0.5, seed = 1)$data
    table(data_balanced_under$Result)
    
    #for over sampling
    data_balanced_over <- ovun.sample(Result ~ ., data = train, method = "over", p=0.5, seed = 1)$data
    table(data_balanced_under$Result)
    
    #for both over and under sampling
    data_balanced_both <- ovun.sample(Result~., data = train, method = "both", p=0.5, seed = 1)$data
    table(data_balanced_under$Result)
    
    ####Naïve Bayes###
       #model.Insurance <- lm(Result~.,data=Insurance)
    library(e1071)
    model <- naiveBayes(train$Result~.,data = train[,-18])
    pred <- predict(model,test[,-18])
    mean(pred==test[,18])
    table(pred)
    table(test[,18])
    library(caret)
    confusionMatrix(pred,test$Result)
    precision(pred,test$Result)
    recall(pred,test$Result)
    
    
    #MODEL.Insurance<-lm(Result~.,data=Cars)
    nw=data.frame(Area_Service=input$num,Hospital.County=input$num1,Hospital.Id=input$num2,
                  Age=input$num3,Gender=input$num4,Cultural_group=input$num5,
                  ethnicity=input$num6,Admission_type=input$num7,ccs_diagnosis_code=input$num8,
                  apr_mdc_description=input$num9,Code_illness=input$num10,Mortality.risk=input$num11,
                  Surg_Description=input$num12,Payment_typology_1=input$num13,
                  Emergency.dept_yes.No=input$num14,Tot_charg=input$num15,
                  Tot_cost=input$num16,ratio_of_total_costs_to_total_charges=input$num17)
    w=predict(model,nw)
    w
  })
}
shinyApp(ui=ui,server = server)
