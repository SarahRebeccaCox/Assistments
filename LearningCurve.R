data <- read.csv("cleanerData.csv")
sort(data)

length(unique(data$skill_id))



split.by.skill <- function(skill.id){
  assign("x",data[data$skill_id==skill.id,])
  x <- x[is.na(x$user_id)==FALSE,]
  x <- x[,c(3,7)]
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
  

  ###Visualization

  plot <- ggplot(learning.curve.df, aes(Try.Number, Attempts.to.Correct,colour=Student.ID)) + 
    geom_line(aes(group = Student.ID)) + 
    geom_point() + 
    labs(title=paste0("Skill ",as.character(skill.id))) +
    labs(x="Try Number") +
    labs(y="Questions Wrong Before a Correct")
  
  return(plot)

}



unique(data$skill_id) #see the list of skill ID's

learning.curve(1) #plot for skill 1
learning.curve(2) #etc
learning.curve(4)





