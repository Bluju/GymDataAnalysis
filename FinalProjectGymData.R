### Julian Sahagun and Peter Bizoukas
### STAT 43000
### Analysis of Gym Dataset (Final Project)

# GymData = read.csv("C:\\Users\\PGBiz\\OneDrive\\Desktop\\spring 2025\\app stats\\project\\GymDataAnalysis\\gym_members_exercise_tracking.csv")
GymData = read.csv("D:\\2school\\STAT430\\GymDataAnalysis\\gym_members_exercise_tracking.csv")
head(GymData)
attach(GymData)

# Exploring the dataset
boxplot(Calories_Burned ~ Experience_Level, col=c("lightgreen","lightyellow","pink"), main="Calories Burned by Experience Level", xlab="Experience Level (1=Novice, 3=Experienced)",ylab="Calories Burned")

library(ggplot2)
ggplot(GymData, aes(x = factor(Experience_Level), y = Calories_Burned)) +
  geom_boxplot(fill = "#1f77b4", color = "black", width = 0.6) +
  labs(
    title = "Calories Burned by Experience Level",
    subtitle = "Do more experienced gym members burn more calories?",
    x = "Experience Level (1 = Novice, 3 = Experienced)",
    y = "Calories Burned"
  ) +
  theme_light(base_size = 14)

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


# Objectives

# Calories Burned given Exp. level, Workout Freq., and Workout Type
calModel = lm(Calories_Burned ~ Experience_Level + Workout_Frequency + Workout_Type)
summary(calModel)
## Experience Level is significant
## Workout Frequency and Workout Type are not significant


calModel2 = lm(Calories_Burned ~ Age + Gender + Fat_Percentage + Session_Duration + Experience_Level + BMI)
summary(calModel2)
## Age, Gender, and Session Duration are significant
## Fat_Percentage, Experience level, and BMI are not significant

calModel3 = lm(Calories_Burned ~ Experience_Level + Age + Gender + Session_Duration)
summary(calModel3)
## Experience Level is no longer significant
## Significant values are: Age, Gender, and Session Duration

calModel4 = lm(Calories_Burned ~ Age + Gender + Session_Duration)
summary(calModel4)

# Gender prediction given workout details
GenderBin = ifelse(Gender == "Male", 1, 0) # Male = 1, Female = 0
genderModel = glm(GenderBin ~ Avg_BPM + Session_Duration + Calories_Burned + Workout_Type + Workout_Frequency + Experience_Level, family="binomial")
summary(genderModel)
