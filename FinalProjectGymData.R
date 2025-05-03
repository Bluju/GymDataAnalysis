### Julian Sahagun and Peter Bizoukas
### STAT 43000
### Analysis of Gym Dataset (Final Project)

# GymData = read.csv("C:\\Users\\PGBiz\\OneDrive\\Desktop\\spring 2025\\app stats\\project\\GymDataAnalysis\\gym_members_exercise_tracking.csv")
# GymData = read.csv("C:\\1Courses\\STAT430\\Project\\GymDataAnalysis\\gym_members_exercise_tracking.csv") # Julian's Laptop
GymData = read.csv("D:\\2school\\STAT430\\GymDataAnalysis\\gym_members_exercise_tracking.csv") # Julian's PC
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

# Predictions using our models

# Gender prediction given workout details
GenderBin = ifelse(Gender == "Male", 1, 0) # Male = 1, Female = 0
genderModel = glm(GenderBin ~ Avg_BPM + Session_Duration + Workout_Type + Workout_Frequency + Experience_Level + Calories_Burned, family="binomial")
stepAIC(genderModel)
genderModel2 = glm(GenderBin~ Avg_BPM + Session_Duration + Calories_Burned, family="binomial")
summary(genderModel2)
genderModel3 = glm(GenderBin~ Avg_BPM + Session_Duration + Calories_Burned + Workout_Type, family="binomial")
summary(genderModel3)
plot(genderModel3, which = 1)




# predict calories
predict(calModel3, data.frame(Age = 22, Gender = "Male", Avg_BPM=142,Session_Duration=.8245), type = "resp") # 49m 47s workout, actual 511, predicted: 684.0528

predict(calModel3, data.frame(Age=22, Gender="Male", Avg_BPM=142, Session_Duration=1.4), type="resp") # Train App says: 849, our model says 1096.544


#predict gender
predict(genderModel3, (data.frame(Avg_BPM=142, Session_Duration=1.4, Calories_Burned=1096.544, Workout_Type="Strength")), type = "resp") # Actual: Male
predict(genderModel3, (data.frame(Avg_BPM=156, Session_Duration=1.59, Calories_Burned=1116, Workout_Type="HIIT")), type="resp") # Actual: Female

#Residual analysis of calModel3
#residual vs. fitted
plot(calModel3$fitted.values, calModel3$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
plot(calModel3, 1)

plot(calModel3, which=1)

#### Should probably boxcox calmodel3
library(MASS)
boxcox_result <- boxcox(calModel3, lambda = seq(-2, 2, 0.1))



######## I asked chatgpt, and it did this, it the boxcox didnt fix much, but the quad did? 

lambda_opt = boxcox_result$x[which.max(boxcox_result$y)]
lambda_opt
Calories_Transformed <- (Calories_Burned^lambda_opt - 1) / lambda_opt
calModel_bc = lm(Calories_Transformed ~ Age + Gender + Avg_BPM + Session_Duration)
summary(calModel_bc)
plot(calModel_bc, which = 1)
predict(calModel_bc, data.frame(Age=22, Gender="Male", Avg_BPM=142, Session_Duration=1.4), type="resp") # Train App says: 849, our model says 1096.544


########MASS####################################################






#qq plot
qqnorm(residuals(calModel3), main = "Normal Q-Q")
qqline(residuals(calModel3), col = "red")

hist(residuals(calModel3), breaks = 20, col = "lightblue",
     main = "Histogram of Residuals", xlab = "Residuals")

plot(calModel3, which = 3)
plot(calModel3, which = 4)  # Cook’s distance
plot(calModel3, which = 5)  # Residuals vs leverage


# Visuals
# Correlation heatmap
library(corrplot)
corrplot(cor(GymData[,c("Age","Weight","BMI","Avg_BPM","Calories_Burned","Session_Duration")]), 
         method="color", type="upper", order="hclust", 
         addCoef.col = "black", tl.col="black", tl.srt=45)

# Actual vs. Predicted plot
predictions <- predict(calModel3)
library(ggplot2)
ggplot(data.frame(actual=GymData$Calories_Burned, predicted=predictions), 
       aes(x=predicted, y=actual)) +
  geom_point(alpha=0.5) +
  geom_abline(intercept=0, slope=1, color="red") +
  labs(title="Actual vs. Predicted Calories Burned",
       x="Predicted Values", y="Actual Values") +
  theme_minimal()

# Experience level interaction with session duration
ggplot(GymData, aes(x=Session_Duration, y=Calories_Burned, color=factor(Experience_Level))) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm") +
  labs(title="Effect of Session Duration on Calories Burned by Experience Level",
       color="Experience Level") +
  theme_light()



ggplot(pred_df, aes(x=predicted, fill=factor(actual))) +
  geom_histogram(alpha=0.5, position="identity", bins=30) +
  scale_fill_manual(values=c("pink", "blue"), 
                    labels=c("Female", "Male"), name="Actual Gender") +
  labs(title="Distribution of Predicted Probabilities by Gender",
       x="Predicted Probability of Male", y="Count") +
  theme_minimal()


# Create confusion matrix
library(ggplot2)

# Convert to data frame
conf_mat <- table(
  Predicted = factor(pred_class, levels=c(0,1), labels=c("Female", "Male")),
  Actual = factor(GenderBin, levels=c(0,1), labels=c("Female", "Male"))
)
conf_df <- as.data.frame(conf_mat)
names(conf_df) <- c("Predicted", "Actual", "Frequency")

# Create heatmap
ggplot(conf_df, aes(x=Actual, y=Predicted, fill=Frequency)) +
  geom_tile() +
  geom_text(aes(label=Frequency), color="white", size=12) +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  labs(title="Confusion Matrix", fill="Count") +
  theme_minimal()



library(popbio)
log_model_bpm <- glm(GenderBin ~ Avg_BPM, data = GymData, family = "binomial")
logi.hist.plot(independ = GymData$Avg_BPM,
               depend = GenderBin,
               boxp = FALSE,
               type = "hist",
               col = "lightblue")
