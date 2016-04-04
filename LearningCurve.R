data <- read.csv("cleanerData.csv")
sort(data)

data2 <- data[with(data, order(skill_id, user_id)), ]
data2[10000:10100,]


skill1 <- data[data$skill_id==1,]
skill1 <- skill1[is.na(skill1$user_id)==FALSE,]
dim(skill1)

skill1 <- skill1[with(skill1, order(user_id)),]
skill1.vis <- skill1[,c(3,7)]

#create new data frame for visualizing
learning.curve.df <- data.frame(matrix(c(0,0,0,0,0,0),2,3))
names(learning.curve.df) <- c("Student.ID","Attempts.to.Correct","Try.Number")
learning.curve.df[,1] <- skill1$user_id[1] #initiate


########THIS DATAFRAME WILL EXCLUDE STUDENTS WHO DO NOT ANSWER ANY QUESTION INCORRECTLY
learn.curve.df.row <- 3 #initialize row
for (i in 3:nrow(skill1)){ #just manually do 1 and 2
  
  #FOR NEW USERS
  if (skill1$user_id[i] != skill1$user_id[i-1]){ #if it's a new user
    count <- 0 #reset the count
    if (skill1$correct[i]==0){ #if they got the question wrong
      count <- count+1 #add one to their attempt count
    }
    if (skill1$correct[i]==1){ #when they get it right 
      if (count != 0){ #if it's not the second or third in a row 
        learning.curve.df[learn.curve.df.row,1] <- skill1$user_id[i] #fill in user id 
        learning.curve.df[learn.curve.df.row,2] <- count #record the count
        learn.curve.df.row <- learn.curve.df.row + 1 #move to the next recording row
      }
      count <- 0 #reset the count
    }
  } #end of loop
  
  #FOR RETURNING USERS
  if (skill1$user_id[i] == skill1$user_id[i-1]){ #if it's the same user
    if (skill1$correct[i]==0){ #if they got the question wrong
      count <- count+1 #add one to their attempt count
    }
    if (skill1$correct[i]==1){ #when they get it right 
      if (count != 0){ #if it's not the second or third in a row 
        learning.curve.df[learn.curve.df.row,1] <- skill1$user_id[i] #fill in user id 
        learning.curve.df[learn.curve.df.row,2] <- count #record the count
        learn.curve.df.row <- learn.curve.df.row + 1 #move to the next recording row
      }
      count <- 0 #reset the count
    }
  }
}


#filling in try number
for (i in 1:nrow(learning.curve.df)){
  if (i == 1){ #starter exception
    count <- 1
    learning.curve.df$Try.Number[i] <- count
    next
  }
  if (learning.curve.df$Student.ID[i] != learning.curve.df$Student.ID[i-1]){ #if new user
    count <- 1 #reset count
    learning.curve.df$Try.Number[i] <- count #fill in row
  }
  if (learning.curve.df$Student.ID[i] == learning.curve.df$Student.ID[i-1]){ #if returning user
    count <- count + 1 #add to count
    learning.curve.df$Try.Number[i] <- count #fill in row
  }
}
##########
#PLOTTING#
##########

library(ggplot2)

skill1.plot <- ggplot(learning.curve.df, aes(Try.Number, Attempts.to.Correct,colour=Student.ID)) + 
  geom_line(aes(group = Student.ID)) + 
  geom_point() + 
  labs(title="Skill 1") +
  labs(x="Try Number") +
  labs(y="Questions Wrong Before a Correct")





