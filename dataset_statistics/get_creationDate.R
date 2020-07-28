##

#Author: Markos Viggiato

##

library(jsonlite)

desktop_dataset <- read.table("loc_contrib_desktop.txt", sep = ',', stringsAsFactors = FALSE, header = TRUE)

repos_info_desktop <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("repo_name", "date")
colnames(repos_info_desktop) <- x

for (i in 1:length(desktop_dataset[,1])){
      print(desktop_dataset[i,1])
      repo <- fromJSON(paste0("https://api.github.com/repos/",desktop_dataset[i,1],"?access_token=6386cfcdda76c69372b01965b798111a1954589c"))
      repos_info_desktop <- rbind(repos_info_desktop, cbind(desktop_dataset[i,1], repo$created_at))
}

repos_info_desktop <- repos_info_desktop[order(as.Date(repos_info_desktop$V2, format="%Y-%m-%d")),]
repos_info_desktop_final <- repos_info_desktop[1:207,]

repos_info_android <- repos_info_android[order(as.Date(repos_info_android$V2, format="%Y-%m-%d")),]
repos_info_android_final <- repos_info_android[1:306,]

current_dataset_android <- read.table("loc_contrib_android.txt", sep = ',', header = TRUE)

final_dataset_android <-data.frame(matrix(ncol = 3, nrow = 0))
x <- c("repo_name", "sloc", "contributors")
colnames(final_dataset_android) <- x

for(i in 1:length(repos_info_android_final$V1)){
      for(j in 1:length(current_dataset_android$repo)){
            if(repos_info_android_final[i,1] == current_dataset_android[j,1]){
                  final_dataset_android <- rbind(final_dataset_android, current_dataset_android[j,])
            }
      }
}

current_dataset_desktop <- read.table("loc_contrib_desktop.txt", sep = ',', header = TRUE)

final_dataset_desktop <-data.frame(matrix(ncol = 3, nrow = 0))
x <- c("repo_name", "sloc", "contributors")
colnames(final_dataset_desktop) <- x

for(i in 1:length(repos_info_desktop_final$V1)){
      for(j in 1:length(current_dataset_desktop$repo)){
            if(repos_info_desktop_final[i,1] == current_dataset_desktop[j,1]){
                  final_dataset_desktop <- rbind(final_dataset_desktop, current_dataset_desktop[j,])
            }
      }
}



write.table(final_dataset_desktop, "final_dataset_desktop.txt", row.names = FALSE, sep = ',')
write.table(final_dataset_android, "final_dataset_android.txt", row.names = FALSE, sep = ',')
