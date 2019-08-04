source("util.R")

loadData <- function() {
  survey <- read.qualtrics("data/review.csv")
  attempts <- read.csv("data/python_problems_anon.csv", header=T)
  
  survey <- survey[survey$user_id %in% attempts$user_id,]
  attempts <- attempts[order(attempts$id),]
  attempts$has_best_score <- attempts$has_best_score == "t"
}


analyzeRatings <- function() {
  #Overall positive ratings
  hist(survey$Q25)
  mean(is.na(survey$Q25))
  
  # Plot helpfulness as a function of showing the Compare-Contrast prompt
  ggplot(data=survey, aes(y=Q25, x=as.factor(showCC))) + geom_boxplot()
  ggplot(data=survey, aes(y=Q25, x=as.factor(showBlanks))) + geom_boxplot()
  
  condCompare(survey$Q25, survey$showBlanks==1)
  condCompare(survey$Q25, survey$showCC==1)
  
  ddply(survey, c("showBlanks", "showCC"), summarize, meanRating=safeMean(Q25))
  
  # No significant effect of condition on perceived usefulness
  # (Technically not ok - non-independent ratings from the same student)
  summary(aov(Q25 ~ showCC * showBlanks, data=survey))
}

analyzeAttempts <- function(survey, attempts, problemID) {
  performance <- summarizeAttemtps(attempts, problemID, problemID-1)
  survey$last_problem_id <- survey$problem_id
  merge(survey[,c("user_id", "last_problem_id", "showCC", "showBlanks", "Q25")], performance, by=c("user_id", "last_problem_id"))
}

analyzeAllAttempts <- function() {
  test <- rbind(
    analyzeAttempts(survey, attempts, 148),
    analyzeAttempts(survey, attempts, 150),
    analyzeAttempts(survey, attempts, 152)
  )
  
  anova(lm(firstCorrect ~ showCC * showBlanks + as.factor(problem_id), data=test))
  summary(lm(firstCorrect ~ showCC * showBlanks + as.factor(problem_id), data=test))
  fisher.test(xor(test$showCC, test$showBlanks), test$firstCorrect)
  summary(glm(firstCorrect ~ showCC * showBlanks + as.factor(problem_id), data=test, family=binomial()))
  
  ddply(test, c("showCC", "showBlanks"), summarize, percFirstCorrect=mean(firstCorrect), medAttempts=median(nAttempts))
}

summarizeAttemtps <- function(attempts, problem_id, last_problem_id) {
  problemAttempts <- attempts[attempts$problem_id == problem_id,]
  df <- NA
  for (user_id in unique(problemAttempts$user_id)) {
    userAttempts <- problemAttempts[problemAttempts$user_id == user_id,]
    df <- rbind(df, data.frame(
      problem_id = problem_id,
      last_problem_id = last_problem_id,
      user_id = user_id,
      nAttempts = nrow(userAttempts),
      firstCorrect = first(userAttempts$has_best_score),
      everCorrect = any(userAttempts$has_best_score)
    ))
  }
  df <- df[-1,]
  return (df)
} 
