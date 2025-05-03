### Julian Sahagun and Peter Bizoukas
### STAT 43000
### Analysis of Gym Dataset (Final Project)

# GymData = read.csv("C:\\Users\\PGBiz\\OneDrive\\Desktop\\spring 2025\\app stats\\project\\GymDataAnalysis\\gym_members_exercise_tracking.csv")
GymData = read.csv("C:\\1Courses\\STAT430\\Project\\GymDataAnalysis\\gym_members_exercise_tracking.csv") # Julian's Laptop
# GymData = read.csv("D:\\2school\\STAT430\\GymDataAnalysis\\gym_members_exercise_tracking.csv") # Julian's PC
head(GymData)
attach(GymData)

# Exploring the dataset
boxplot(Calories_Burned ~ Experience_Level, col=c("lightgreen","lightyellow","pink"), main="Calories Burned by Experience Level", xlab="Experience Level (1=Novice, 3=Experienced)",ylab="Calories Burned")

library(ggplot2)

# how does experience level affect calories burned?
ggplot(GymData, aes(x = factor(Experience_Level), y = Calories_Burned)) +
  geom_boxplot(fill = "#1f77b4", color = "black", width = 0.6) +
  labs(
    title = "Calories Burned by Experience Level",
    subtitle = "Do more experienced gym members burn more calories?",
    x = "Experience Level (1 = Novice, 3 = Experienced)",
    y = "Calories Burned"
  ) +
  theme_light(base_size = 14)
# the plot shows a trend of more calories burned given higher experience
# use AOV to test whether there is a significant difference between the groups
calPerExp = aov(Calories_Burned~factor(Experience_Level))
summary(calPerExp)
# p-value very small -> reject null hypothesis -> sig diff in mean calories burned at different experience levels
TukeyHSD(calPerExp)
plot(TukeyHSD(calPerExp))
# significant difference of calories burned in all levels of experience


boxplot(Session_Duration ~ Workout_Frequency)

ggplot(GymData, aes(x = factor(Workout_Frequency), y = Session_Duration)) +
  geom_boxplot(fill = "#ff7f0e", color = "black", width = 0.6) +
  labs(
    title = "Session Duration by Workout Frequency",
    subtitle = "Are more frequent gym-goers spending longer per session?",
    x = "Workout Frequency (days/week)",
    y = "Session Duration (hours)"
  ) +
  theme_light(base_size = 14)

boxplot(BMI ~ Workout_Frequency)
ggplot(GymData, aes(x = factor(Workout_Frequency), y = BMI)) +
  geom_boxplot(fill = "#2ca02c", color = "black", width = 0.6) +
  labs(
    title = "BMI by Workout Frequency",
    subtitle = "Does BMI differ based on how often members work out?",
    x = "Workout Frequency (days/week)",
    y = "BMI"
  ) +
  theme_light(base_size = 14)

#TODO: test multicollinearity with calories burned and session duration

# Objectives

# Calories Burned  
# Model selection using bidirectional Stepwise elimination
library(MASS)
calModel = lm(Calories_Burned ~ Age + Gender + Weight + Height + Avg_BPM + Session_Duration + Workout_Type + Fat_Percentage + Water_Intake + Workout_Frequency + Experience_Level + BMI)
stepAIC(calModel,direction="both")
calModel2 = lm(formula = Calories_Burned ~ Age + Gender + Weight + Height + 
     Avg_BPM + Session_Duration + BMI)
summary(calModel2)

# use alpha=0.01 
calModel3 = lm(Calories_Burned ~ Age + Gender + Avg_BPM + Session_Duration)
summary(calModel3)
AIC(calModel)
AIC(calModel2)
AIC(calModel3) 
# similar AIC values 
# We will use calModel3 because it has the least parameters


# Gender prediction given workout details
GenderBin = ifelse(Gender == "Male", 1, 0) # Male = 1, Female = 0
genderModel = glm(GenderBin ~ Avg_BPM + Session_Duration + Calories_Burned + Workout_Type + Workout_Frequency + Experience_Level, family="binomial")
summary(genderModel)


# Predictions using our models

predict(calModel3, data.frame(Age = 22, Gender = "Male", Avg_BPM=142,Session_Duration=.8245), type = "resp") # 49m 47s workout, actual 511, predicted: 684.0528

predict(calModel3, data.frame(Age=22, Gender="Male", Avg_BPM=142, Session_Duration=1.4), type="resp")