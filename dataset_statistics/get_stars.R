##

#Author: Markos Viggiato

##

library("jsonlite")

android <- read.table("final_dataset_android.txt", header = TRUE, sep = ',')
desktop <- read.table("final_dataset_desktop.txt", header = TRUE, sep = ',')

android_stars <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("repo_name", "stars")
colnames(android_stars) <- x

for(i in 1:length(android$repo)){
      if(grepl("Bearded-Hen/Android-BootBootstrap", android$repo[i]))
            next
      data <- fromJSON(paste0("https://api.github.com/repos/", android$repo[i],"?access_token=a7e5acbd23f087e6c3f683969cd8404a048742b6"))
      repo_name <- as.character(android$repo[i])
      stars <- data$stargazers_count
      android_stars <- rbind(android_stars, cbind(repo_name, stars))
}

desktop_stars <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("repo_name", "stars")
colnames(desktop_stars) <- x

for(i in 1:length(desktop$repo)){
      data <- fromJSON(paste0("https://api.github.com/repos/", desktop$repo[i],"?access_token=a7e5acbd23f087e6c3f683969cd8404a048742b6"))
      repo_name <- desktop$repo[i]
      stars <- data$stargazers_count
      desktop_stars <- rbind(desktop_stars, cbind(repo_name, stars))
}
desktop_stars[,1] <- as.character(desktop$repo)

write.table(android_stars, "stars_android.txt", sep = ',', row.names = FALSE)
write.table(desktop_stars, "stars_desktop.txt", sep = ',', row.names = FALSE)
