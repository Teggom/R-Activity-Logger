# This script will record activity within a specified folder and produce a
# Masterlog detailing changes made. This is entirly for the benefit of the fire 
# stream "Sustainability Economics" and is in no way an attempt to cause or create
# harm to any persons or devices.

# Stephen Kozak 8/28/2017

setwd("~/../Desktop/TestScriptFolder/Attempt 1 - Setting up Base Copy/")
Root_Dir <- getwd()
Master_Directory_List <- c()
Legend_store <- c()

## Functions
insert_into_vector <- function(vector = c(), position = 0, element = NULL){
  the_return <- c()
  if(position > length(vector)){
    return(c(vector, element))
  } else if(position == 1){
    return(c(element, vector))
  } else {
    top_half <- vector[1:(position-1)]
    bottom_half <- vector[position:length(vector)]
    return(c(top_half, element, bottom_half))
  }
}


create_master_directory <- function(directory = getwd()){
  d <- directory
  dir_list <- dir()
  for(x in dir_list){
    a <<- c(a, paste(d,"/",x, sep = ""))
    setwd(d)
    tryCatch({
      setwd(x)
      create_master_directory(directory = getwd())
      }, 
      error = function(x){})
  }
  setwd(d)
}

directory_printer <- function(directory = getwd(), curry = ""){
  d <- directory
  dir_list <- dir()
  for(x in dir_list){
    Master_Directory_List <<- c(Master_Directory_List, paste(d,"/",x, sep = ""))
    setwd(d)
    print(paste(curry, x))
    tryCatch({
      setwd(x)
      directory_printer(directory = getwd(), curry = paste(curry, "-----", sep = "|")
      )}, 
      error = function(x){})
    tryCatch({
      if(grep("passwd|password|access|securetty", x)){
        tryCatch({
         # print(paste("########", readLines(x), sep = ""))
        } ,error = function(x){})
    }},error=function(x){}) 
  }
  setwd(d)
}
directory_legend <- function(directory = getwd()){
  d <- directory
  dir_list <- dir()
  for(x in dir_list){
    Legend_store <<- c(Legend_store, paste(d,"/",x, sep = ""))
    setwd(d)
    tryCatch({
      setwd(x)
      directory_store(directory = getwd())
      }, 
      error = function(x){})
  }
  setwd(d)
  
}


#read.table("File", sep = "\n")

#Run this if it is the first time ever running this program
First_Time_Run <- function(directory = getwd()){
  file.create("MasterLog.txt")
  file.create("CurrentStateLog.txt")
  file.create("ErrorLog.txt")
  Master_Directory_List <<- c()
  directory_printer(directory = Root_Dir)
  write(c("-----"), file = "ErrorLog.txt")
  for(x in Master_Directory_List){                      #No yeah this needs to change, this is slow to the order of O(x^3) or more. 
    if(file.size("CurrentStateLog.txt")!=0){
      tryCatch({
        File_Data <- read.table("CurrentStateLog.txt", sep = "\n")
        File_Data <- as.vector(File_Data$V1)
        write_text <- ""
        write_text <- paste(file.mtime(x), x, sep = " | ")
        write(c(File_Data, write_text, sep = "\n"), file = "CurrentStateLog.txt")
        },
        error = function(y){
          write(c(as.vector(read.table("ErrorLog.txt",sep="\n")$V1),paste(Sys.time(),"|",x)), file="ErrorLog.txt")
        }
      )
    } else {
      tryCatch({
        write_text <- ""
        write_text <- paste(file.mtime(x), x, sep = " | ")
        write(c(write_text, sep = ""), file = "CurrentStateLog.txt")
        },
        error = function(y){
          write(c(as.vector(read.table("ErrorLog.txt",sep="\n")$V1),paste(Sys.time(),"|",x)), file="ErrorLog.txt")
        }
      )
    }
  }
}


#Length -1 => forever
#time_interval => seconds between checkups on the system
Peep_On_Activities <- function(length = -1, time_interval = 600, Main_Dir = Root_Dir){
  setwd(Root_Dir)
  run_without_end = function(){if(length==-1){return(TRUE)}else{return(FALSE)}}
  if(time_interval<=0)quit(status = 1)
  if(!file.exists("ErrorLog.txt") %in% dir()){
    First_Time_Run()
  }
  time_counter <- time_interval
  number_of_runs <- 0
  while(number_of_runs!=length){
    number_of_runs= number_of_runs+1
    start_time <- Sys.time();
    current_time <- Sys.time();
    while((current_time-start_time)<time_interval){
      Sys.sleep(1)
      current_time<- Sys.time();
      print(paste(round(as.numeric(current_time)-as.numeric(start_time),digits = 0), "Seconds Have Passed: looking for", time_interval, "Seconds!"))
    }
    print("Running Search...")
    #now that I am here, I must execute a search to check changes
    State_Vector <- as.vector(read.table("CurrentStateLog.txt", sep = "\n")$V1)
    d <- Root_Dir
    Master_Directory_List <- c()
    a <<- c()
    create_master_directory(directory = getwd())
    Master_Directory_List <- a
    dir_list <- gsub("^.*\\|\\s", "", Master_Directory_List)
    to_add <- c()
    replace <- c()
    exists_vector <- gsub("^.*\\| ", "", State_Vector)
    for(x in exists_vector){
      if(!(TRUE %in% grepl(paste(x, "$", sep = ""), dir_list))){
        to_add <- c(to_add, paste(Sys.time(), "Item_Was_Deleted", x, sep = " | "))
      }
    }
    for(x in dir_list){
      if(!grepl("ErrorLog.txt", x) && !grepl("CurrentStateLog.txt", x) && !grepl("MasterLog.txt", x)){
        if(!(TRUE %in% grepl(x, State_Vector))){
          to_add <- c(to_add, paste(file.mtime(x), "Item_Was_Created", x, sep = " | ")) # these are new files
        } else {# runs assuming times do not match
          correct_item <- State_Vector[grepl(paste(x, "$", sep = ""), State_Vector)]#x mnust sometimes have have time
          correct_time <- as.POSIXct(strptime(correct_item, "%Y-%m-%d %H:%M:%S"))
          current_time <- file.mtime(x)
          if(current_time != correct_time){
            to_add <- c(to_add, paste(file.mtime(x), "Item_Was_Changed", x, sep = " | "))
          }
        }
        replace <- c(replace, paste(file.mtime(x), x, sep = " | "))
      }
    }
    
    
    #adds the new files to the current
  
    #updates current with the new file change states
    
    #Sorts the currently new changes
    
    write(c(replace), sep = "\n", file = "CurrentStateLog.txt")
    Sys.sleep(4)
    final_time_ordered <- c()
    while(length(to_add)>0){
      greatest <- as.POSIXct(gsub(" \\| .*$", "", to_add[1]))
      pos <- 1
      for(x in 1:length(to_add)){
        test <- as.POSIXct(gsub(" \\| .*$", "", to_add[x]))
        if(test<greatest){
          greatest<-test
          pos <- x
        }
      }
      final_time_ordered <- c(final_time_ordered, to_add[pos])
      if(pos == 1 && length(to_add)==1){to_add <- c()}
      else if(pos == 1){to_add <- to_add[2:length(to_add)]}
      else if(pos == length(to_add)){to_add <- to_add[1:(length(to_add)-1)]}
      else {to_add <- c(to_add[1:(pos-1)], to_add[(pos+1):length(to_add)])}
    }
    
    setwd(Root_Dir)
    if(length(final_time_ordered)==0){
      final_time_ordered <- c(paste(Sys.time(), "Nothing Changed", sep = " | "))
    }
    if(file.size("MasterLog.txt")!=0){
      Master_Vector <- as.vector(read.table("MasterLog.txt", sep = "\n")$V1)
      Master_Vector <- c(Master_Vector, " ", " ", final_time_ordered)
      write(c(Master_Vector), sep = "\n", file = "MasterLog.txt")
    } else {
      write(c(final_time_ordered), sep = "\n", file = "MasterLog.txt")
    }
    #write(c(State_Vector), sep = "\n", file = "CurrentStateLog.txt")
    
    
  }  
}









