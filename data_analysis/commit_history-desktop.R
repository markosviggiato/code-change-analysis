##########
# Author: Markos Viggiato
# October 21st, 2018
##########

library("jsonlite")

android_dataset <- read.table("final_dataset_desktop-read.txt", sep = ',', stringsAsFactors = FALSE, header = TRUE)
repos_info <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("repo_name", "number_contr", "SLOC", "number_pullR", "numbe_issues")
colnames(repos_info) <- x

access_token <- character(length = 4)
# access_token array removed: include here the current access tokens

index_access_token <- 1
current_access_token <- access_token[index_access_token]

for (i in 1:length(android_dataset[,1])){
      
      commit_info <- data.frame(matrix(ncol = 9, nrow = 0))
      x <- c("sha", "date_commit", "number_changed_files", "added_loc", "deleted_loc", "total_loc", "source_code", "build", "test")
      colnames(commit_info) <- x
      
      commit_error <- data.frame(matrix(ncol = 2, nrow = 0))
      x <- c("sha", "error_msg")
      colnames(commit_error) <- x
      
      fullname <- android_dataset[i,1]
      sloc <- android_dataset[i,2]
      number_contr <- android_dataset[i,3]
      print(fullname)
      repo_issues <- fromJSON(paste0("https://api.github.com/repos/",fullname,"?", current_access_token))
      number_issues <- repo_issues$open_issues
      
      current_page <- 1
      continue <- TRUE
      number_pullR <- 0
      while(continue){
            pulls_info <- fromJSON(paste0("https://api.github.com/repos/",fullname,"/pulls?per_page=100&page=",current_page ,"&", current_access_token))
            if(length(pulls_info$id) == 0)
                  break;
            if(length(pulls_info$id) < 100){
                  continue <- FALSE
            }
            else{
                  current_page <- current_page + 1
            }
            number_pullR <- number_pullR + length(pulls_info$id)
      }
      repos_info <- rbind(repos_info, cbind(fullname, number_contr, sloc, number_pullR, number_issues))
      
      current_page <- 1
      continue <- TRUE
      while(continue){
            commits <- fromJSON(paste0("https://api.github.com/repos/",fullname,"/commits?per_page=100&page=", current_page, "&since=2016-10-01&until=2018-10-01&", current_access_token))
            print(current_page)
            print(current_access_token)
            print(length(commits$sha))
            if(length(commits$sha) == 0)
                  break;
            if(length(commits$sha) < 100){
                  continue <- FALSE
            }
            else{
                  current_page <- current_page + 1
            }
            if(current_page == 30){
                  index_access_token <- index_access_token + 1
                  if(index_access_token > 4)
                        index_access_token <- 1
                  current_access_token <- access_token[index_access_token]
            }


            for(sha in commits$sha){
                  print(sha)
                  Sys.sleep(1)
                  one_commit <- tryCatch({

                    fromJSON(paste0("https://api.github.com/repos/",fullname,"/commits/",sha,"?", current_access_token))
                    }, warning = function(w) {
                      print(w)
                    }, error = function(e) {
                      print(e)
                    }, finally = {
                    })

                  if(!is.null(one_commit$message)){
                    commit_error <- rbind(commit_error, cbind(sha, one_commit$message))
                    next
                  }

                  sha <- one_commit$sha
                  date_commit <- one_commit$commit$committer$date
                  changed_files <- one_commit$files

                  source_code <- 0
                  build <- 0
                  test <- 0
                  number_changed_files <- 0
                  added_loc <- 0
                  deleted_loc <- 0
                  total_loc <- 0
                  if( !is.null(changed_files$filename )){
                    # check extensions of changed files
                    for(file  in  1:length(changed_files$filename)){
                          if( grepl(".java", changed_files$filename[file]) ){
                                source_code <- 1
                                number_changed_files <- number_changed_files + 1
                                added_loc <- added_loc + changed_files$additions[file]
                                deleted_loc <- deleted_loc + changed_files$deletions[file]
                                total_loc <- total_loc + changed_files$changes[file]
                          }
                          else{
                                if( grepl("build.xml", changed_files$filename[file]) || grepl("build.gradle", changed_files$filename[file]) || grepl("pom.xml", changed_files$filename[file]) )
                                      build <- 1
                                else{
                                      if( grepl("test", changed_files$filename[file]) )
                                            test <- 1
                                }
                          }
                    }
                   }


                  commit_info <- rbind(commit_info, cbind(sha, date_commit, number_changed_files, added_loc, deleted_loc, total_loc, source_code, build, test))
            }
      }

      write.table(commit_info, paste0("commit_history/desktop/", strsplit(fullname, "/")[[1]][2], ".txt"), row.names = FALSE, sep = ',')
      if( dim(commit_error)[1] != 0 )
        write.table(commit_error, paste0("commit_history/desktop/", strsplit(fullname, "/")[[1]][2], "_ERROR.txt"), row.names = FALSE, sep = ',')
}
write.table(repos_info, "commit_history/repos_info_desktop.txt", row.names = FALSE, sep = ',')




