#Author: Markos Viggiato


library("stringr")
android <- read.table("final_dataset_android.txt",  header = TRUE, stringsAsFactors = FALSE)
desktop <- read.table("final_dataset_desktop.txt", sep = ',', header = TRUE, stringsAsFactors = FALSE)

dir_android <- as.data.frame(list.files(path = paste0(getwd(),"/commit_history/android")), stringsAsFactors = FALSE)
dir_desktop <- as.data.frame(list.files(path = paste0(getwd(),"/commit_history/desktop")), stringsAsFactors = FALSE)


for(i in 1:length(dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`)){
      for(j in 1:length(android$repo)){
            nameToLookFor <- strsplit(dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`[i],".txt")
            if ( !is.na(str_extract(android$repo[j], paste0("/",nameToLookFor, "$"))) ){
                  android <- android[-j,]
                  break
            }
      }
}

for(i in 1:length(dir_desktop$`list.files(path = paste0(getwd(), "/commit_history/desktop"))`)){
      for(j in 1:length(desktop$repo)){
            nameToLookFor <- strsplit(dir_desktop$`list.files(path = paste0(getwd(), "/commit_history/desktop"))`[i],".txt")
            if ( !is.na(str_extract(desktop$repo[j], paste0("/",nameToLookFor, "$"))) ){
                  desktop <- desktop[-j,]
                  break
            }
      }
}


# read commit history files
dir_android <- as.data.frame(list.files(path = paste0(getwd(),"/commit_history/android")), stringsAsFactors = FALSE)
dir_desktop <- as.data.frame(list.files(path = paste0(getwd(),"/commit_history/desktop")), stringsAsFactors = FALSE)

j<-0
for(i in 1:length(dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`)){
      data <- read.table(paste0(getwd(),"/commit_history/android/", dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`[i]), header = TRUE, sep = ',')
      if(nrow(data) < 24){
            print(dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`[i])
            j <- j+1
      }
}
print(j)
