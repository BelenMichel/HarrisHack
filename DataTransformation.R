
require(tidyverse)

students = read.csv(file="~/studentInfo.csv", 
                     header=TRUE, sep=",", stringsAsFactors = FALSE) %>% 
  mutate(disability = if_else(disability == "N",0,1),
         gender = if_else(gender == "M",0,1)) %>% 
  mutate(assessment_preference = sample(0:1, 32593, rep = TRUE),
         enjoy_participation = sample(0:1, 32593, rep = TRUE),
         enjoy_challenges = sample(0:1, 32593, rep = TRUE),
         abstract_thought = sample(0:1, 32593, rep = TRUE),
         practical_problems = sample(0:1, 32593, rep = TRUE),
         logical_person = sample(0:1, 32593, rep = TRUE),
         extrovert = sample(0:1, 32593, rep = TRUE),
         sensor = sample(0:1, 32593, rep = TRUE),
         thinker = sample(0:1, 32593, rep = TRUE),
         judger = sample(0:1, 32593, rep = TRUE),
         visual_learner = sample(0:1, 32593, rep = TRUE), 
         auditory_learner = sample(0:1, 32593, rep = TRUE),
         practical_learner = sample(0:1, 32593, rep = TRUE),
         reading_learner = sample(0:1, 32593, rep = TRUE)) 

assessments = read.csv(file="~/assessments.csv", 
                       header=TRUE, sep=",", stringsAsFactors = FALSE)

student_assessments = read.csv(file="~/studentAssessment.csv", 
                               header=TRUE, sep=",", stringsAsFactors = FALSE)

student_courses = 
  assessments %>% 
  inner_join(student_assessments, by = c("id_assessment")) %>%  
  inner_join(students, by = c("id_student","code_module","code_presentation"))

set.seed(20)
studentCluster = kmeans(students[, c(4,11,14:27)], 10, nstart = 20)

students["studentCluster"] = studentCluster$cluster
  
student_courses = 
  student_courses %>% 
  mutate(intellectual_challenge = round(rnorm(173912, mean = 4, sd = 1),0),
         intellectual_challenge = if_else(intellectual_challenge > 5,5,intellectual_challenge),
         intellectual_challenge = if_else(intellectual_challenge < 1,1,intellectual_challenge),
         understood_purpose = round(rnorm(173912, mean = 4, sd = 1),0),
         understood_purpose = if_else(understood_purpose > 5,5,understood_purpose),
         understood_purpose = if_else(understood_purpose < 1,1,understood_purpose),
         understood_standards = round(rnorm(173912, mean = 4, sd = 1),0),
         understood_standards = if_else(understood_standards > 5,5,understood_standards),
         understood_standards = if_else(understood_standards < 1, 1,understood_standards),
         valuable_class_time = round(rnorm(173912, mean = 4, sd = 1),0),
         valuable_class_time = if_else(valuable_class_time > 5,5,valuable_class_time),
         valuable_class_time = if_else(valuable_class_time < 1, 1, valuable_class_time),
         received_useful_feedback = round(rnorm(173912, mean = 4, sd = 1),0),
         received_useful_feedback = if_else(received_useful_feedback > 5,5,received_useful_feedback),
         received_useful_feedback = if_else(received_useful_feedback < 1, 1, received_useful_feedback),
         fair_evaluations = round(rnorm(173912, mean = 4, sd = 1),0),
         fair_evaluations = if_else(fair_evaluations > 5,5,fair_evaluations),
         fair_evaluations = if_else(fair_evaluations < 1, 1, fair_evaluations),
         felt_respected = round(rnorm(173912, mean = 4, sd = 1),0),
         felt_respected = if_else(felt_respected > 5,5,felt_respected),
         felt_respected = if_else(felt_respected < 1, 1, felt_respected),
         overall_excellent_course = round(rnorm(173912, mean = 4, sd = 1),0),
         overall_excellent_course = if_else(overall_excellent_course > 5,5,overall_excellent_course),
         overall_excellent_course = if_else(overall_excellent_course < 1, 1, overall_excellent_course)) %>% 
  mutate(course_id = str_c(code_module," - ",code_presentation)) 























