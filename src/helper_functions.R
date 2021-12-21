replacing_category <- function(){
  for(i in 1:259){
    if(plants$category[i] == "flower" || plants$category[i]== "fruit"){
      print(i)
    }  
    else{
      plants$category[i] <- "else"
    }
    
  }
  View(plants)
  write.csv(plants, file = "new_plants")
  }

replacing_space <- function(){
  for(i in 1:259){
    if(grepl("c",plants$required_space[i], fixed=TRUE)){
      plants$required_space[i] <- paste(plants$required_space[i], "m", sep = "")
    }
    if(grepl(".",plants$required_space[i], fixed=TRUE)){
      plants$required_space[i] <- paste(plants$required_space[i], "m", sep = "")
    }
  }
  View(plants)
  write.csv(plants, file = "new_plants_data")
  
}
