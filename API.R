library(plumber)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(knitr)
library(caret)
library(lattice)

df <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

df <- df %>%
  mutate(
    Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("No", "Yes")),
    HighBP = factor(HighBP, levels = c(0, 1), labels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c(0, 1), labels = c("No", "Yes")),
    CholCheck = factor(CholCheck, levels = c(0, 1), labels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c(0, 1), labels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c(0, 1), labels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0, 1), labels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("No", "Yes")),
    Fruits = factor(Fruits, levels = c(0, 1), labels = c("No", "Yes")),
    Veggies = factor(Veggies, levels = c(0, 1), labels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("No", "Yes")),
    AnyHealthcare = factor(AnyHealthcare, levels = c(0, 1), labels = c("No", "Yes")),
    NoDocbcCost = factor(NoDocbcCost, levels = c(0, 1), labels = c("No", "Yes")),
    GenHlth = factor(GenHlth, levels = 1:5, labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    MentHlth = as.integer(MentHlth),
    PhysHlth = as.integer(PhysHlth),
    DiffWalk = factor(DiffWalk, levels = c(0, 1), labels = c("No", "Yes")),
    Sex = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Age = factor(Age, levels = 1:13, labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
    Education = factor(Education, levels = 1:6, labels = c("Never attended", "Grades 1-8", "Grades 9-11", "Grade 12 or GED", "College 1-3 years", "College 4 years or more")),
    Income = factor(Income, levels = 1:8, labels = c("<$10,000", "$10,000-$15,000", "$15,000-$20,000", "$20,000-$25,000", "$25,000-$35,000", "$35,000-$50,000", "$50,000-$75,000", ">$75,000"))
  )


log_reg_model1 <- glm(Diabetes_binary ~ ., data = df, family = "binomial")



# Predict diabetes risk
#* @param HighBP The blood pressure level
#* @param HighChol The cholesterol level
#* @param CholCheck Whether cholesterol was checked
#* @param Smoker Smoking status
#* @param Stroke History of stroke
#* @param HeartDiseaseorAttack History of heart disease or attack
#* @param PhysActivity Physical activity status
#* @param Fruits Fruit consumption status
#* @param Veggies Vegetable consumption status
#* @param HvyAlcoholConsump Heavy alcohol consumption status
#* @param AnyHealthcare Healthcare access status
#* @param NoDocbcCost Cost of doctor's visit
#* @param GenHlth General health status
#* @param MentHlth Mental health days
#* @param PhysHlth Physical health days
#* @param DiffWalk Difficulty walking
#* @param Sex Gender
#* @param Age Age group
#* @param Education Education level
#* @param Income Income level
#* @get /Apred
function(HighBP = "No", HighChol = "No", CholCheck = "No", Smoker = "No", Stroke = "No", HeartDiseaseorAttack = "No",
         PhysActivity = "No", Fruits = "No", Veggies = "No", HvyAlcoholConsump = "No", AnyHealthcare = "No",
         NoDocbcCost = "No", GenHlth = "Excellent", MentHlth = 0, PhysHlth = 0, DiffWalk = "No", Sex = "Female",
         Age = "18-24", BMI = "12" , Education = "Never attended", Income = "<$10,000") {
  
  new_data <- data.frame(
    HighBP = factor(HighBP, levels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c("No", "Yes")),
    CholCheck = factor(CholCheck, levels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c("No", "Yes")),
    Fruits = factor(Fruits, levels = c("No", "Yes")),
    Veggies = factor(Veggies, levels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c("No", "Yes")),
    AnyHealthcare = factor(AnyHealthcare, levels = c("No", "Yes")),
    NoDocbcCost = factor(NoDocbcCost, levels = c("No", "Yes")),
    GenHlth = factor(GenHlth, levels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    MentHlth = as.integer(MentHlth),
    PhysHlth = as.integer(PhysHlth),
    BMI = as.integer(BMI),
    DiffWalk = factor(DiffWalk, levels = c("No", "Yes")),
    Sex = factor(Sex, levels = c("Female", "Male")),
    Age = factor(Age, levels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
    Education = factor(Education, levels = c("Never attended", "Grades 1-8", "Grades 9-11", "Grade 12 or GED", "College 1-3 years", "College 4 years or more")),
    Income = factor(Income, levels = c("<$10,000", "$10,000-$15,000", "$15,000-$20,000", "$20,000-$25,000", "$25,000-$35,000", "$35,000-$50,000", "$50,000-$75,000", ">$75,000"))
  )
  
  prediction <- predict(log_reg_model1, new_data, type = "prob")
  return(prediction)
}

#  Info endpoint
#* @get /info
function() {
  list(
    name = "Cheng Chen",
    github_url = "https://github.com/Ncsueric/Final-Project"
  )
}

# Example calls to the API
# Example 1: http://127.0.0.1:8000/Apred?HighBP=Yes&Smoker=No
# Example 2: http://127.0.0.1:8000/Apred?PhysActivity=Yes&Fruits=No
# Example 3: http://127.0.0.1:8000/Apred?Income=$35,000-$50,000
