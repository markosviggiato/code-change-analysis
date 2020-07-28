##########
# Author: Markos Viggiato
##########

# read dataset
dataset_android <- read.table("commit_history/repos_summary_info_android.txt", sep = ',', header = TRUE, stringsAsFactors = FALSE)
dataset_desktop <- read.table("commit_history/repos_summary_info_desktop-final.txt", sep = ',', header = TRUE, stringsAsFactors = FALSE)



############################ ANDROID#############################

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




############################ DESKTOP #############################

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

################# some statistics of independent variables and response variable #############################

# sloc ~ number of commits per month (frequency)
scatter.smooth(x=dataset_full$sloc, y=dataset_full$commits_per_month, main="commits ~ sloc", xlim = c(0,10000))  # scatterplot

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(dataset_full$sloc, outline = FALSE)
boxplot(as.numeric(dataset_full$commits_per_month))

par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(dataset_full$sloc), main="Density Plot: sloc", ylab="Frequency")  # density plot for 'sloc'
plot(density(as.numeric(dataset_full$commits_per_month)), main="Density Plot: commits_freq", ylab="Frequency")

# number of contributors ~ number of commits per month (frequency)
scatter.smooth(x=dataset_full$number_contr, y=dataset_full$commits_per_month, main="commits ~ contrib", xlim = c(0,100))  # scatterplot

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(dataset_full$number_contr, outline = FALSE)
boxplot(as.numeric(dataset_full$commits_per_month))

par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(dataset_full$number_contr), main="Density Plot: contrib", ylab="Frequency")  
plot(density(as.numeric(dataset_full$commits_per_month)), main="Density Plot: commits_freq", ylab="Frequency")


# number of pull request ~ number of commits per month (frequency)
scatter.smooth(x=dataset_full$number_pullR, y=dataset_full$commits_per_month, main="commits ~ pullR", xlim = c(0,100))  # scatterplot

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(dataset_full$number_pullR, outline = FALSE)
boxplot(as.numeric(dataset_full$commits_per_month))

par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(dataset_full$number_pullR), main="Density Plot: pullR", ylab="Frequency")  
plot(density(as.numeric(dataset_full$commits_per_month)), main="Density Plot: commits_freq", ylab="Frequency")

# number of issues ~ number of commits per month (frequency)
scatter.smooth(x=dataset_full$number_issues, y=dataset_full$commits_per_month, main="commits ~ issues", xlim = c(0,500))  # scatterplot

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(dataset_full$number_issues, outline = FALSE)
boxplot(as.numeric(dataset_full$commits_per_month))

par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(dataset_full$number_issues), main="Density Plot: issues", ylab="Frequency")  
plot(density(as.numeric(dataset_full$commits_per_month)), main="Density Plot: commits_freq", ylab="Frequency")


################## MULTIPLE LINEAR REGRESSION #######################

fit <- lm(as.numeric(dataset_full$commits_per_month) ~ dataset_full$number_contr + dataset_full$sloc + dataset_full$number_pullR + dataset_full$number_issues, data=dataset_full)
summary(fit) # show results

# include Indicator variable in the model
fit <- lm(as.numeric(dataset_full$commits_per_month) ~ dataset_full$indicator + dataset_full$number_contr + dataset_full$sloc + dataset_full$number_pullR + dataset_full$number_issues, data=dataset_full)
summary(fit) # show results
