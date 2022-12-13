library(ggplot2)

#########################
# Make sure all datasets 
# are loaded in R session 
#########################

x <- c("Overall score" , 
       "Academic Reputation",
       "Employee Reputation", 
       "Faculty Student Ratio" ,
       "Citations per Faculty", 
       "International Faculty Ratio" , 
       "International Student Ratio")
colors <- c("Overall score" = "red",
            "Academic Reputation" = "blue", 
            "Employee Reputation" = "green",
            "Faculty Student Ratio" = "violet",
            "Citations per Faculty" = "orange",
            "International Faculty Ratio" = "dark green",
            "International Student Ratio" = "cyan")

###########################
#Scatter plot for 2020
###########################
p1 <- ggplot(QS_Rank_2020[1:499, ], aes(x = 1:499)) +
  geom_point(aes(y = Score, color = x[1]), size = 1) +
  geom_point(aes(y = Academic.Reputation, color = x[2]), size = 1) +
  geom_point(aes(y = Employer.Reputation, color = x[3]), size = 1) +
  geom_point(aes(y = Faculty.Student.Ratio, color = x[4]), size = 1) +
  geom_point(aes(y = Citations.per.Faculty, color = x[5]), size = 1) +
  geom_point(aes(y = International.Faculty.Ratio, color = x[6]), size = 1) +
  geom_point(aes(y = International.Students.Ratio, color = x[7]), size = 1) +
  xlab("Ranking") +
  labs(title = "Scatter plots for all scores for Year 2020") +
  scale_color_manual(guide = "legend",
                     name = "Scores",
                     #breaks = c("Overall score" , 
                     #        "Academic Reputation",
                     #       "Employee Reputation", 
                     #      "Faculty Student Ratio" ,
                     #     "Citations per Faculty", 
                     #    "International Faculty Ratio" , 
                     #   "International Student Ratio"),
                     values = colors) +
  theme_minimal();p1

###########################
#Scatter plot for 2021
###########################
p1 <- ggplot(QS_Rank_2021[1:499, ], aes(x = 1:499)) +
  geom_point(aes(y = Score, color = x[1]), size = 1) +
  geom_point(aes(y = Academic.Reputation, color = x[2]), size = 1) +
  geom_point(aes(y = Employer.Reputation, color = x[3]), size = 1) +
  geom_point(aes(y = Faculty.Student.Ratio, color = x[4]), size = 1) +
  geom_point(aes(y = Citations.per.Faculty, color = x[5]), size = 1) +
  geom_point(aes(y = International.Faculty.Ratio, color = x[6]), size = 1) +
  geom_point(aes(y = International.Students.Ratio, color = x[7]), size = 1) +
  xlab("Ranking") +
  labs(title = "Scatter plots for all scores for Year 2023") +
  scale_color_manual(guide = "legend",
                     name = "Scores",
                     #breaks = c("Overall score" , 
                     #        "Academic Reputation",
                     #       "Employee Reputation", 
                     #      "Faculty Student Ratio" ,
                     #     "Citations per Faculty", 
                     #    "International Faculty Ratio" , 
                     #   "International Student Ratio"),
                     values = colors) +
  theme_minimal()

###########################
#Scatter plot for 2022
###########################
p1 <- ggplot(QS_Rank_2022[1:499, ], aes(x = 1:499)) +
  geom_point(aes(y = Score, color = x[1]), size = 1) +
  geom_point(aes(y = Academic.Reputation, color = x[2]), size = 1) +
  geom_point(aes(y = Employer.Reputation, color = x[3]), size = 1) +
  geom_point(aes(y = Faculty.Student.Ratio, color = x[4]), size = 1) +
  geom_point(aes(y = Citations.per.Faculty, color = x[5]), size = 1) +
  geom_point(aes(y = International.Faculty.Ratio, color = x[6]), size = 1) +
  geom_point(aes(y = International.Students.Ratio, color = x[7]), size = 1) +
  xlab("Ranking") +
  labs(title = "Scatter plots for all scores for Year 2022") +
  scale_color_manual(guide = "legend",
                     name = "Scores",
                     #breaks = c("Overall score" , 
                     #        "Academic Reputation",
                     #       "Employee Reputation", 
                     #      "Faculty Student Ratio" ,
                     #     "Citations per Faculty", 
                     #    "International Faculty Ratio" , 
                     #   "International Student Ratio"),
                     values = colors) +
  theme_minimal()

###########################
#Scatter plot for 2023
###########################
p1 <- ggplot(QS_Rank_2023[1:499, ], aes(x = 1:499)) +
  geom_point(aes(y = Score, color = x[1]), size = 1) +
  geom_point(aes(y = Academic.Reputation, color = x[2]), size = 1) +
  geom_point(aes(y = Employer.Reputation, color = x[3]), size = 1) +
  geom_point(aes(y = Faculty.Student.Ratio, color = x[4]), size = 1) +
  geom_point(aes(y = Citations.per.Faculty, color = x[5]), size = 1) +
  geom_point(aes(y = International.Faculty.Ratio, color = x[6]), size = 1) +
  geom_point(aes(y = International.Students.Ratio, color = x[7]), size = 1) +
  xlab("Ranking") +
  labs(title = "Scatter plots for all scores for Year 2023") +
  scale_color_manual(guide = "legend",
                     name = "Scores",
                     #breaks = c("Overall score" , 
                     #        "Academic Reputation",
                     #       "Employee Reputation", 
                     #      "Faculty Student Ratio" ,
                     #     "Citations per Faculty", 
                     #    "International Faculty Ratio" , 
                     #   "International Student Ratio"),
                     values = colors) +
  theme_minimal()