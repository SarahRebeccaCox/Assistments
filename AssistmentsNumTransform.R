data <- read.csv("skill_builder_data_corrected.csv")

names(data)

#replace blanks with a value in first_action column
for (i in 1:nrow(data)){
  if (is.na(data$first_action[i]==TRUE)){
    data$first_action[i] <- 3 #3 means student looked at problem but then did nothing
  }
}

#same for bottom_hint column
for (i in 1:nrow(data)){
  if (is.na(data$bottom_hint[i]==TRUE)){
    data$bottom_hint[i] <- 2 #2 means student did not ask for final hint
  }
}

#drop immediately useless columns

data <- subset(data, select=-c(order_id,
                     position, #"position" doesn't exist in codebook
                     type, #"type" is the same in every row
                     skill_id, #skill_id and skill_name are identical
                     answer_id, #only exists for multiple choice, some blanks mean incorrect mult choice answers though.
                                #also probably not relevant
                     answer_text #probably not relevant
                     )) 

#this may not have been the best choice
###################################################################################################
data.numbered <- transform(data, 
                                assignment_id = factor(as.numeric(factor(assignment_id))),
                                user_id = factor(as.numeric(factor(user_id))),
                                assistment_id = factor(as.numeric(factor(assistment_id))),
                                problem_id = factor(as.numeric(factor(problem_id))),
                                tutor_mode = factor(as.numeric(factor(tutor_mode))),
                                answer_type = factor(as.numeric(factor(answer_type))),
                                sequence_id = factor(sequence_id),
                                student_class_id = factor(as.numeric(factor(student_class_id))),
                                base_sequence_id = factor(base_sequence_id),
                                skill_name = factor(as.numeric(factor(skill_name))),
                                teacher_id = factor(as.numeric(factor(teacher_id))),
                                school_id = factor(as.numeric(factor(school_id))),
                                template_id = factor(as.numeric(factor(template_id))),
                                first_action = factor(as.numeric(factor(first_action))),
                                bottom_hint =factor( as.numeric(factor(bottom_hint)))
                             
)
###################################################################################################

data.numbered$hint_percent <- data$hint_count/data$hint_total


#is it better to have a bunch of binary vars for each level of every column?

#DV should be a measure of performance, of course. maybe predicting how well a student performs in one skill as a function of performance in other skills?
#still figuring out how to reshape the data to properly do this


#only box and whisker skill data
box.and.whisker <- data[data$skill_name=="Box and Whisker",]

#get success rate for each student
library(plyr)
test <- ddply(box.and.whisker,.(assignment_id,user_id),summarize,percent_correct=sum(correct)/length(user_id),number=length(user_id))


#####WORKING ON REFORMATTING DATA TO GET SOMETHING USEABLE FOR PREDICTION.
######TRYING TO PREDICT PERCENT OF QUESTIONS CORRECT
######DATA EXPLANATION AT https://sites.google.com/site/assistmentsdata/how-to-interpret