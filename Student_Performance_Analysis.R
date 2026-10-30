
# ==========================================
# PROJECT: STUDENT PERFORMANCE ANALYSIS
# LANGUAGE: R
# ==========================================

# 1. Install and load required packages

packages <- c("ggplot2", "dplyr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(ggplot2)
library(dplyr)

# 2. Create Student Dataset

set.seed(123)

students <- data.frame(
  Student_ID = 1:20,
  Student_Name = paste("Student", 1:20),
  Attendance = sample(55:100, 20, replace = TRUE),
  Maths = sample(35:100, 20, replace = TRUE),
  Science = sample(35:100, 20, replace = TRUE),
  English = sample(35:100, 20, replace = TRUE),
  Computer = sample(35:100, 20, replace = TRUE)
)

# 3. Calculate Total, Average, Grade and Result

students <- students %>%
  mutate(
    Total = Maths + Science + English + Computer,
    Average = round(Total / 4, 2),
    Grade = case_when(
      Average >= 90 ~ "A+",
      Average >= 80 ~ "A",
      Average >= 70 ~ "B",
      Average >= 60 ~ "C",
      Average >= 50 ~ "D",
      TRUE ~ "F"
    ),
    Result = ifelse(
      Maths >= 33 & Science >= 33 &
        English >= 33 & Computer >= 33,
      "Pass", "Fail"
    )
  )

# 4. Display Complete Dataset

cat("\n===== STUDENT DATASET =====\n")
print(students)

# 5. Overall Performance Summary

cat("\n===== PERFORMANCE SUMMARY =====\n")

cat("Total Students:", nrow(students), "\n")
cat("Average Marks:", round(mean(students$Average), 2), "\n")
cat("Highest Average:", max(students$Average), "\n")
cat("Lowest Average:", min(students$Average), "\n")

pass_percentage <- mean(students$Result == "Pass") * 100
cat("Pass Percentage:", round(pass_percentage, 2), "%\n")

# 6. Find Topper

topper <- students %>%
  filter(Average == max(Average))

cat("\n===== TOPPER =====\n")
print(topper[, c("Student_Name", "Average", "Grade")])

# 7. Subject-wise Average Marks

subject_average <- data.frame(
  Subject = c("Maths", "Science", "English", "Computer"),
  Average_Marks = c(
    mean(students$Maths),
    mean(students$Science),
    mean(students$English),
    mean(students$Computer)
  )
)

cat("\n===== SUBJECT-WISE AVERAGE =====\n")
print(subject_average)

# 8. Graph 1: Student Average Marks

p1 <- ggplot(students,
             aes(x = Student_Name, y = Average)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Student Average Marks",
    x = "Students",
    y = "Average Marks"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 90, hjust = 1, size = 7
  ))

print(p1)

# 9. Graph 2: Subject-wise Performance

p2 <- ggplot(subject_average,
             aes(x = Subject, y = Average_Marks)) +
  geom_col(fill = "seagreen") +
  labs(
    title = "Subject-wise Average Marks",
    x = "Subject",
    y = "Average Marks"
  ) +
  theme_minimal()

print(p2)

# 10. Graph 3: Pass vs Fail

result_count <- students %>%
  count(Result)

p3 <- ggplot(result_count,
             aes(x = Result, y = n, fill = Result)) +
  geom_col() +
  labs(
    title = "Pass and Fail Analysis",
    x = "Result",
    y = "Number of Students"
  ) +
  theme_minimal()

print(p3)

# 11. Graph 4: Attendance vs Average Marks

p4 <- ggplot(students,
             aes(x = Attendance, y = Average)) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", se = FALSE,
              color = "navy") +
  labs(
    title = "Attendance vs Student Performance",
    x = "Attendance (%)",
    y = "Average Marks"
  ) +
  theme_minimal()

print(p4)

# 12. Correlation Analysis

correlation <- cor(
  students$Attendance,
  students$Average
)

cat("\n===== ATTENDANCE CORRELATION =====\n")
cat("Correlation coefficient:",
    round(correlation, 3), "\n")

# 13. Grade Distribution

grade_count <- students %>%
  count(Grade)

p5 <- ggplot(grade_count,
             aes(x = Grade, y = n, fill = Grade)) +
  geom_col() +
  labs(
    title = "Grade Distribution",
    x = "Grade",
    y = "Number of Students"
  ) +
  theme_minimal()

print(p5)

# 14. Save Dataset and Summary to CSV Files

write.csv(
  students,
  "student_performance_data.csv",
  row.names = FALSE
)

write.csv(
  subject_average,
  "subject_wise_summary.csv",
  row.names = FALSE
)

# 15. Final Conclusion

cat("\n===== PROJECT CONCLUSION =====\n")

if (pass_percentage >= 75) {
  cat("Overall student performance is satisfactory.\n")
} else {
  cat("There is scope for improvement in student performance.\n")
}

cat("Analysis completed successfully!\n")
cat("CSV files saved in your working directory.\n")

analyze_student <- function(name, maths, science,
                            english, computer, attendance) {
  
  total <- maths + science + english + computer
  average <- total / 4
  
  grade <- if (average >= 90) {
    "A+"
  } else if (average >= 80) {
    "A"
  } else if (average >= 70) {
    "B"
  } else if (average >= 60) {
    "C"
  } else if (average >= 50) {
    "D"
  } else {
    "F"
  }
  
  result <- if (all(c(maths, science, english, computer) >= 33)) {
    "Pass"
  } else {
    "Fail"
  }
  
  output <- data.frame(
    Student_Name = name,
    Maths = maths,
    Science = science,
    English = english,
    Computer = computer,
    Attendance = attendance,
    Total = total,
    Average = round(average, 2),
    Grade = grade,
    Result = result
  )
  
  print(output)
}



