##

#Author: Markos Viggiato

##

library("jsonlite")

android_dataset <- read.table("final_dataset_desktop.txt", sep = ',', stringsAsFactors = FALSE, header = TRUE)
repos_info <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("repo_name", "commits")
colnames(repos_info) <- x

for (i in 1:length(android_dataset[,1])){
      fullname <- android_dataset[i,1]
      print(fullname)
      current_page <- 1
      continue <- TRUE
      commits <- 0
      while(continue){
            data <- fromJSON(paste0("https://api.github.com/repos/",fullname,"/commits?per_page=100&page=",current_page ,"&since=2016-10-01&until=2018-10-01&access_token=6386cfcdda76c69372b01965b798111a1954589c"))
            print(current_page)
            if(length(data$sha) == 0)
                  break;
            if(length(data$sha) < 100){
                  continue <- FALSE
            }
            else{
                  current_page <- current_page + 1
            }
            commits <- commits + length(data$sha)
            
      }
      repos_info <- rbind(repos_info, cbind(fullname, commits))
}
