##########
# Author: Markos Viggiato
##########

################## MULTIPLE LINEAR REGRESSION #######################
library("effsize")
library("mctest")
library("ggplot2")

################## Load data #######################
dir_android <- as.data.frame(list.files(path = paste0(getwd(),"/commit_history/android")), stringsAsFactors = FALSE)
dir_desktop <- as.data.frame(list.files(path = paste0(getwd(),"/commit_history/desktop")), stringsAsFactors = FALSE)


# android
number_commits_android <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("repo_name", "number_commits")
colnames(number_commits_android) <- x

for(i in 1:length(dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`)){
      
      data <- read.table(paste0(getwd(),"/commit_history/android/", dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`[i]), header = TRUE, sep = ',')
      if(nrow(data) >= 24){
            repo_name <- dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`[i]
            number_commits <- as.numeric(as.character(round(nrow(data)/24,2)))
            number_commits_android <- rbind(number_commits_android, cbind(repo_name, number_commits))
      }
      else
            print(dir_android$`list.files(path = paste0(getwd(), "/commit_history/android"))`[i])
}


# desktop
number_commits_desktop <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("repo_name", "number_commits")
colnames(number_commits_desktop) <- x

for(i in 1:length(dir_desktop$`list.files(path = paste0(getwd(), "/commit_history/desktop"))`)){
      
      data <- read.table(paste0(getwd(),"/commit_history/desktop/", dir_desktop$`list.files(path = paste0(getwd(), "/commit_history/desktop"))`[i]), header = TRUE, sep = ',')
      if(nrow(data) >= 24){
            repo_name <- dir_desktop$`list.files(path = paste0(getwd(), "/commit_history/desktop"))`[i]
            number_commits <- as.numeric(as.character(round(nrow(data)/24,2)))
            number_commits_desktop <- rbind(number_commits_desktop, cbind(repo_name, number_commits))
      }
}



################## WMW test and Cliff's Delta #######################

wilcox.test(as.numeric(number_commits_android$number_commits), as.numeric(number_commits_desktop$number_commits), correct = FALSE)
effsize::cliff.delta(as.numeric(number_commits_android$number_commits),
            as.numeric(number_commits_desktop$number_commits),
            conf.level=.95,  use.unbiased=TRUE, use.normal=FALSE,  return.dm=FALSE)

par(mfrow=c(1,2))
par(cex.lab=1.5) # is for y-axis
par(cex.axis=1.5) # is for x-axis
boxplot(as.numeric(number_commits_android$number_commits), main='Mobile', ylim = c(0, 175), cex.main=2, ylab="Number of commits per month")
boxplot(as.numeric(number_commits_desktop$number_commits), main='Non-mobile', ylim = c(0, 175), cex.main=2, ylab="Number of commits per month")



dataset_android <- read.table("commit_history/repos_info_android.txt", sep = ',', header = TRUE, stringsAsFactors = FALSE)
dataset_desktop <- read.table("commit_history/repos_info_desktop.txt", sep = ',', header = TRUE, stringsAsFactors = FALSE)

############################ ANDROID ############################

commits_per_month_android <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("repo_name", "number_commits")
colnames(commits_per_month_android) <- x

for(i in 1:length(dataset_android$fullname)){
      name_to_lookFor <- strsplit(dataset_android[i,1], '/')[[1]][[2]]
      print(name_to_lookFor)
      
      all_commits <- read.table(paste0("commit_history/android/", name_to_lookFor, ".txt"), sep = ',', header = TRUE, stringsAsFactors = FALSE)
      number_commits <- nrow(all_commits)
      print(number_commits/24)
      commits_per_month_android <- rbind(commits_per_month_android, cbind(repo_name=dataset_android[i,1], commits_per_month=as.numeric(round(number_commits/24, 2))))
}
print(length(commits_per_month_android$commits_per_month))
dataset_android <- cbind(dataset_android, commits_per_month=commits_per_month_android$commits_per_month)
dataset_android <- dataset_android[dataset_android$commits_per_month!=0,]

print(length(dataset_android$fullname))



############################ DESKTOP ############################

commits_per_month_desktop <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("repo_name", "number_commits")
colnames(commits_per_month_desktop) <- x

for(i in 1:length(dataset_desktop$fullname)){
      name_to_lookFor <- strsplit(dataset_desktop[i,1], '/')[[1]][[2]]
      print(name_to_lookFor)
      
      all_commits <- read.table(paste0("commit_history/desktop/", name_to_lookFor, ".txt"), sep = ',', header = TRUE, stringsAsFactors = FALSE)
      number_commits <- nrow(all_commits)
      print(number_commits/24)
      commits_per_month_desktop <- rbind(commits_per_month_desktop, cbind(repo_name=dataset_desktop[i,1], commits_per_month=as.numeric(round(number_commits/24, 2))))
}
print(length(commits_per_month_desktop$commits_per_month))
dataset_desktop <- cbind(dataset_desktop, commits_per_month=commits_per_month_desktop$commits_per_month)
dataset_desktop <- dataset_desktop[dataset_desktop$commits_per_month!=0,]

print(length(dataset_desktop$fullname))

# add indicator variable (binary indicate whether repo has AndroidManifest.xml or not)
dataset_android <- cbind(dataset_android, indicator=rep(1, nrow(dataset_android)))
dataset_desktop <- cbind(dataset_desktop, indicator=rep(0, nrow(dataset_desktop)))


# full dataset
dataset_full <- rbind(dataset_android, dataset_desktop)


fit_control <- lm(log(as.numeric(dataset_full$commits_per_month)) ~ dataset_full$number_contr + dataset_full$sloc + 
                        dataset_full$number_pullR + dataset_full$number_issues, data=dataset_full)
summary(fit_control) # show results

# include Indicator variable in the model
fit_full <- lm(log(as.numeric(dataset_full$commits_per_month)) ~ dataset_full$indicator + dataset_full$number_contr + 
                     dataset_full$sloc + dataset_full$number_pullR + dataset_full$number_issues, data=dataset_full)
summary(fit_full) # show results


par(mfrow=c(2,2))

# check for residuals in QQ plot
plot(fit_full)

# pair-wise correlation
for(i in 2:ncol(dataset_full)-1){
      dataset_full[,i] <- as.numeric(dataset_full[,i])
}


X <- data.frame(matrix(ncol = 4, nrow = 0))

X<-dataset_full[,2:5]
x <- c("nCont", "sloc", "nPR", "nIssues")
colnames(X) <- x

X$nCont <- log(as.numeric(X$number_contr)+0.01)
X$sloc <- log(as.numeric(X$sloc)+0.01)
X$nPR <- log(as.numeric(X$number_pullR)+0.01)
X$nIssues <- log(as.numeric(X$number_issues)+0.01)

library(GGally)
ggpairs(X)


# check VIF values
mctest::omcdiag(as.matrix(as.numeric(X)),dataset_full$commits_per_month)
mctest::imcdiag(X,dataset_full$commits_per_month)


