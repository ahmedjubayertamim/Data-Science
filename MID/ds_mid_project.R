setwd("D:\\Courses\\10th Semester\\DataScience\\Project")
df <- read.csv("D:/Courses/10th Semester/DataScience/Project/dataset.csv")
View(df)
unique(df$gender)
unique(df$smoking_history)
df$gender[df$gender == "NA" | df$gender == ""] <- NA
df$smoking_history[df$smoking_history == "NA" | df$smoking_history == "No Info"                 | df$smoking_history == ""] <- NA
any(is.na(df)) 
sum(is.na(df))
colSums(is.na(df))

colors <- colorRampPalette(c("lightblue","darkblue"))(10)
par(mar = c(10, 4, 4, 2))
barplot(colSums(is.na(df)),
        main = "Missing Values per Column",
        xlab = "",
        ylab = "Count",
        col = colors,
        las = 2,
        cex.names = 0.8)


#Handle missing values: 
numeric_cols <- sapply(df, is.numeric)
for (col in names(df)[numeric_cols]) 
{
  df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
}


cat_cols <- sapply(df, is.factor) | sapply(df, is.character)
for (col in names(df)[cat_cols]) 
{
  mode_value <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
  df[[col]][is.na(df[[col]])] <- mode_value
}

Q1 <- quantile(df$age, 0.25, na.rm = TRUE)
Q3 <- quantile(df$age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1 
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR 
df[df$age < lower_bound | df$age > upper_bound, ]


boxplot(df$age, main = "Age Outliers Boxplot", col = "lightblue")


boxplot(df[, numeric_cols], main = "Outliers in Numeric Columns", col = "lightblue")



df <- df[!(df$age < lower_bound | df$age > upper_bound), ]

  median_age <- median(df$age, na.rm = TRUE)
df$age[df$age < lower_bound | df$age > upper_bound] <- median_age

df$ageGroup <- cut(df$age,
                   breaks = c(0, 18, 60, 120),
                   labels = c("Child", "Adult", "Senior"),
                   right = FALSE)


df$BMI_Category <- cut(df$bmi,
                       breaks = c(0, 18.5, 25, 30, 100),
                       labels = c("Underweight", "Normal", "Overweight", "Obese"),
                       right = FALSE)


df$Gender_numeric <- as.numeric(as.factor(df$gender))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

duplicated(df)
df[duplicated(df), ]
df <- df[!duplicated(df), ]
   
sum(duplicated(df))

df_filtered <- df[df$gender == "Female" & df$bmi> 25, ]
df[df$Age < 0 | df$Age > 120, ]

	table(df$diabetes)
str(df)

df$gender <- as.factor(df$gender)
df$smoking_history <- as.factor(df$smoking_history)
df$diabetes <- as.factor(df$diabetes)
library(ROSE)
set.seed(123)
df_balanced <- ROSE(diabetes ~ ., data = df)$data
table(df_balanced$diabetes)

set.seed(123)
split_index <- sample(1:nrow(df_balanced), 0.7 * nrow(df_balanced))
train_data <- df_balanced[split_index, ]
test_data  <- df_balanced[-split_index, ]

dim(train_data)
dim(test_data)


library(dplyr)
num_vars <- c("age", "bmi", "HbA1c_level", "blood_glucose_level")
descriptive_stats <- df_balanced %>%
  group_by(diabetes) %>%
  summarise(
    count = n(),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE),
    sd_bmi = sd(bmi, na.rm = TRUE),
    mean_HbA1c = mean(HbA1c_level, na.rm = TRUE),
    sd_HbA1c = sd(HbA1c_level, na.rm = TRUE),
    mean_glucose = mean(blood_glucose_level, na.rm = TRUE),
    sd_glucose = sd(blood_glucose_level, na.rm = TRUE)
  )
library(dplyr)
num_vars <- c("age", "bmi", "HbA1c_level", "blood_glucose_level")
descriptive_stats <- df_balanced %>%
  group_by(diabetes) %>%
  summarise(
    count = n(),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE),
    sd_bmi = sd(bmi, na.rm = TRUE),
    mean_HbA1c = mean(HbA1c_level, na.rm = TRUE),
    sd_HbA1c = sd(HbA1c_level, na.rm = TRUE),
    mean_glucose = mean(blood_glucose_level, na.rm = TRUE),
    sd_glucose = sd(blood_glucose_level, na.rm = TRUE)
  )

descriptive_stats

tapply(df_balanced$bmi, df_balanced$diabetes, mean)
	t.test(bmi ~ diabetes, data = df_balanced)
	
	library(dplyr)
	df_balanced %>%
	  group_by(gender) %>%
	  summarise(
	    count = n(),
	    mean_BMI = mean(bmi, na.rm = TRUE),
	    sd_BMI = sd(bmi, na.rm = TRUE),
	    var_BMI = var(bmi, na.rm = TRUE),
	    IQR_BMI = IQR(bmi, na.rm = TRUE)
	  )
	
	par(mar = c(5, 4, 4, 2))  # bottom, left, top, right
	boxplot(bmi ~ gender, data = df_balanced,
	        col = c("lightblue", "pink"),
	        main = "Variation of BMI Across Gender",
	        xlab = "Gender",
	        ylab = "BMI"
	)
	
	