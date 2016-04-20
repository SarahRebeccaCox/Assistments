data <- read.csv("cleanerData.csv")
sort(data)
library(ggplot2)

length(unique(data$skill_id))



split.by.skill <- function(skill.id){
  assign("x",data[data$skill_id==skill.id,])
  x <- x[is.na(x$user_id)==FALSE,]
  #x <- x[,c(3,7)]
  x <- x[order(x$user_id),]
  return(x)
}


#####This function outputs plots that show how many questions
#####A student answers incorrectly between each correct answer
#####Until they "master" the skill or quit
#####Students who only have correct answers are not shown in the plots
learning.curve <- function(skill.id){
  
  x <- assign(paste0("skill",as.character(skill.id)),split.by.skill(skill.id)) #skill-specific dataframe
  
  #create new data frame for visualizing
  learning.curve.df <- data.frame(matrix(c(0,0,0,0,0,0),2,3))
  names(learning.curve.df) <- c("Student.ID","Attempts.to.Correct","Try.Number")
  learning.curve.df$Time <- 0 # time to response on first problem of set
  learning.curve.df$Class.ID <- 0 # student class id
  learning.curve.df$Teacher.ID <- 0 # teacher id
  learning.curve.df$School.ID <- 0 # school id
  learning.curve.df$First.Action <- 0 # first action on first problem of set
  
  learn.curve.df.row <- 1 #initialize row
  for (i in 1:nrow(x)){ 
    
    #FOR THE FIRST ROW
    if (i == 1){ 
      count <- 0
      next
    }
    
    #FOR NEW USERS
    if (x$user_id[i] != x$user_id[i-1]){ #if it's a new user
      count <- 0 #reset the count
      if (x$correct[i]==0){ #if they got the question wrong
        count <- count+1 #add one to their attempt count
      }
      if (x$correct[i]==1){ #when they get it right 
        if (count != 0){ #if it's not the second or third in a row 
          learning.curve.df[learn.curve.df.row,1] <- x$user_id[i] #fill in user id 
          learning.curve.df[learn.curve.df.row,2] <- count #record the count
           
          learning.curve.df$Time[learn.curve.df.row] <- x$ms_first_response[i]
          learning.curve.df$Class.ID[learn.curve.df.row] <- x$student_class_id[i]
          learning.curve.df$Teacher.ID[learn.curve.df.row] <- x$teacher_id[i]
          learning.curve.df$School.ID[learn.curve.df.row] <- x$school_id[i]
          learning.curve.df$First.Action[learn.curve.df.row] <- x$first_action[i]
          
          
          learn.curve.df.row <- learn.curve.df.row + 1 #move to the next recording row
        }
        count <- 0 #reset the count
      }
    } #end of loop
    
    #FOR RETURNING USERS
    if (x$user_id[i] == x$user_id[i-1]){ #if it's the same user
      if (x$correct[i]==0){ #if they got the question wrong
        count <- count+1 #add one to their attempt count
      }
      if (x$correct[i]==1){ #when they get it right 
        if (count != 0){ #if it's not the second or third in a row 
          learning.curve.df[learn.curve.df.row,1] <- x$user_id[i] #fill in user id 
          learning.curve.df[learn.curve.df.row,2] <- count #record the count
          
          learning.curve.df$Time[learn.curve.df.row] <- x$ms_first_response[i]
          learning.curve.df$Class.ID[learn.curve.df.row] <- x$student_class_id[i]
          learning.curve.df$Teacher.ID[learn.curve.df.row] <- x$teacher_id[i]
          learning.curve.df$School.ID[learn.curve.df.row] <- x$school_id[i]
          learning.curve.df$First.Action[learn.curve.df.row] <- x$first_action[i]
          
          learn.curve.df.row <- learn.curve.df.row + 1 #move to the next recording row
        }
        count <- 0 #reset the count
      }
    }
  }
  
  #filling in try number
  for (j in 1:nrow(learning.curve.df)){
    if (j == 1){ #starter exception
      count <- 1
      learning.curve.df$Try.Number[j] <- count
      next
    }
    if (learning.curve.df$Student.ID[j] != learning.curve.df$Student.ID[j-1]){ #if new user
      count <- 1 #reset count
      learning.curve.df$Try.Number[j] <- count #fill in row
    }
    if (learning.curve.df$Student.ID[j] == learning.curve.df$Student.ID[j-1]){ #if returning user
      count <- count + 1 #add to count
      learning.curve.df$Try.Number[j] <- count #fill in row
    }
  }
  

  ###############
  #MEAN CORRECTN#
  ###############
  
  
  learning.curve.df$Mean <- 0

  
    
    
  for (i in 1:nrow(learning.curve.df)){ 
    
    #FOR THE FIRST ROW
    if (i == 1){ 
      sum <- learning.curve.df$Attempts.to.Correct[i]
      n <- 1
      learning.curve.df$Mean[i] <- sum/n
      
      
      next
    }
    
    #FOR NEW USERS
    if (learning.curve.df$Student.ID[i] != learning.curve.df$Student.ID[i-1]){ #if it's a new user
      sum <- learning.curve.df$Attempts.to.Correct[i]
      n <- 1 #reset the count
      learning.curve.df$Mean[i] <- sum/n
      }

    
  #FOR RETURNING USERS
  if (learning.curve.df$Student.ID[i] == learning.curve.df$Student.ID[i-1]){ #if it's a returning user
    sum <- sum + learning.curve.df$Attempts.to.Correct[i]
    n <- n + 1 
    learning.curve.df$Mean[i] <- sum/n
    }
  } #end of loop
  
  learning.curve.df$Class.ID <- as.factor(learning.curve.df$Class.ID)
  learning.curve.df$Teacher.ID <- as.factor(learning.curve.df$Teacher.ID)
  learning.curve.df$School.ID <- as.factor(learning.curve.df$School.ID)
  learning.curve.df$First.Action <- as.factor(learning.curve.df$First.Action)
 ###############
 #VISUALIZATION#
 ###############
 
  plot2 <- ggplot(learning.curve.df, aes(Try.Number, Mean,colour=Student.ID)) + 
    geom_line(aes(group = Student.ID)) + 
    geom_point() + 
    labs(title=paste0("Skill ",as.character(skill.id),", Mean Smoothed")) +
    labs(x="Try Number") +
    labs(y="Questions Wrong Before a Correct")
 
  return(list(df=learning.curve.df,plot=plot2))
       
}



unique(data$skill_id) #see the list of skill ID's

skill1 <- learning.curve(1)
skill2 <- learning.curve(2)
skill4 <- learning.curve(4)
skill5 <- learning.curve(5)
skill8 <- learning.curve(8)

##### Time and First Action are recorded each time a student asnwers a question correctly.
##### Not all information about time to answer questiosn and first action is contained in this dataset

#########################################
#FURTHER RESHAPING, ONE LINE PER STUDENT#
#########################################

data.for.glm <- function(dataframe){
  
  #####INITIALIZE A DATAFRAME
  
  glm.df <- data.frame(matrix(0,length(unique(dataframe$Student.ID)),ncol(dataframe)))
  names(glm.df) <- names(dataframe)
  names(glm.df)[9] <- "Sum.of.Attempts"
  
  glm.df.row <- 1 #initialize row
  for (i in 1:nrow(dataframe)){ 
    
    #FOR THE FIRST ROW
    if (i == 1){ 
      glm.df$Student.ID[glm.df.row] <- dataframe$Student.ID[i]
      glm.df$Class.ID[glm.df.row] <- dataframe$Class.ID[i]
      glm.df$Teacher.ID[glm.df.row] <- dataframe$Teacher.ID[i]
      glm.df$School.ID[glm.df.row] <- dataframe$School.ID[i]
      glm.df$Time[glm.df.row] <- dataframe$Time[i]
      glm.df$First.Action[glm.df.row] <- dataframe$First.Action[i]
      sum <- dataframe$Attempts.to.Correct[i]
      
      glm.df.row <- glm.df.row + 1
      next
    }
    
    #FOR NEW USERS
    if (dataframe$Student.ID[i] != dataframe$Student.ID[i-1]){ #if it's a new user
      
      #record sum for student above
      glm.df$Sum.of.Attempts[glm.df.row-1] <- sum
      
      #reset the sum
      sum <- dataframe$Attempts.to.Correct[i]
      
      #add other identifying information
      glm.df$Student.ID[glm.df.row] <- dataframe$Student.ID[i]
      glm.df$Class.ID[glm.df.row] <- dataframe$Class.ID[i]
      glm.df$Teacher.ID[glm.df.row] <- dataframe$Teacher.ID[i]
      glm.df$School.ID[glm.df.row] <- dataframe$School.ID[i]
      glm.df$Time[glm.df.row] <- dataframe$Time[i]
      glm.df$First.Action[glm.df.row] <- dataframe$First.Action[i]
      
     
      
      #IF LAST ROW
      if (i == nrow(dataframe)){
        glm.df$Sum.of.Attempts[glm.df.row] <- sum
      }
      
      glm.df.row <- glm.df.row + 1
    } #end of loop
    
    #FOR RETURNING USERS
    if (dataframe$Student.ID[i] == dataframe$Student.ID[i-1]){ #if it's the same user
      
      #add to sum
      sum <- sum + dataframe$Attempts.to.Correct[i]
      
      #IF LAST ROW
      if (i == nrow(dataframe)){
        glm.df$Sum.of.Attempts[glm.df.row] <- sum
      }      
    }
  } 
  return(glm.df)
}



a <- skill1$df
b <- data.for.glm(a)



