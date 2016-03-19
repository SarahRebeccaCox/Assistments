#######
#SETUP#
#######


data <- read.csv("skill_builder_data_corrected.csv")

names(data)


#replace blanks with a value in first_action column
for (i in 1:nrow(data)){
  if (is.na(data$first_action[i]==TRUE)){
    data$first_action[i] <- 3 #3 means student looked at problem but then did nothing
  }
}


data$hint_percent <- data$hint_count/data$hint_total

#drop immediately useless columns
data <- subset(data, select=-c(order_id,
                     position, #"position" doesn't exist in codebook
                     type, #"type" is the same in every row
                     skill_name, #skill_id and skill_name are identical
                     answer_id, #only exists for multiple choice, some blanks mean incorrect mult choice answers though.
                                #also probably not relevant
                     answer_text, #probably not relevant
                     bottom_hint, #not necessary with hint_percent
                     base_sequence_id,
                     hint_count,
                     hint_total,
                     overlap_time
                     )) 


write.csv(data,"cleanerData.csv")
#############################
#convert all data to numeric#
#############################

#first, transform all factors to actual factors
data.numbered <- transform(data, 
                                assignment_id = factor(assignment_id),
                                user_id = factor(user_id),
                                assistment_id = factor(assistment_id),
                                problem_id = factor(problem_id),
                                tutor_mode = factor(as.numeric(factor(tutor_mode))),
                                answer_type = factor(as.numeric(factor(answer_type))),
                                sequence_id = factor(sequence_id),
                                student_class_id = factor(student_class_id),
                                base_sequence_id = factor(base_sequence_id),
                                skill_id = factor(skill_id),
                                teacher_id = factor(teacher_id),
                                school_id = factor(school_id),
                                template_id = factor(template_id),
                                first_action = factor(first_action),
                                opportunity_original = as.numeric(opportunity_original)
)

#then create indicator vars for each factor
numbered <p model.matrix(~., data=data.numbered)[,-1]
#R crash :(



####thoughts
#multilevel logistic regression?
library(nlme)

#lfa.mod.1 <- lme(fixed = LFA~Group+Emotion,
#                 random=~1|Patient,
#                 data=data, 
#                 na.action=na.omit,
#                 method="ML")


### OR use all vars to predict how long it takes a student to master a skill
### that is, predict max(opportunity) for each student/skill

##sort: by student, by skill, max opportunity
###################################################################################################



#get success rate for each student
library(plyr)
test <- ddply(box.and.whisker,
              .(assignment_id,user_id),
              summarize,
              percent_correct=sum(correct)/length(user_id),
              number=length(user_id))

#a start but not able to get quite what I think I want




######DATA EXPLANATION AT https://sites.google.com/site/assistmentsdata/how-to-interpret