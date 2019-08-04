source("util.R")

loadData <- function() {
  surveyReview <- read.qualtrics("data/review.csv")
  surveyWk10 <- read.qualtrics("data/week10.csv")
  attempts <- read.csv("data/python_problems_anon.csv", header=T)
  
  surveyReview <- surveyReview[surveyReview$user_id %in% attempts$user_id,]
  surveyWk10 <- surveyWk10[surveyWk10$user_id %in% attempts$user_id,]
  
  attempts <- attempts[order(attempts$id),]
  attempts$has_best_score <- attempts$has_best_score == "t"
}

# Run me line by line
analyzeAllWk10 <- function() {
  test <- analyzeAttempts(surveyWk10, attempts, 63, 61)
  test$attemptsRank <- rank(test$nAttempts)
  summary(lm(nAttempts ~ showCC * showBlanks, data=test))
  summary(lm(attemptsRank ~ showCC * showBlanks, data=test))
  summary(lm(Q25 ~ showCC * showBlanks, data=test))
}

# Run me line by line
analyzeAllReview <- function() {
  test <- rbind(
    analyzeAttempts(surveyReview, attempts, 148, 147),
    analyzeAttempts(surveyReview, attempts, 150, 149),
    analyzeAttempts(surveyReview, attempts, 152, 151)
  )
  
  ddply(test, c("showCC", "showBlanks"), summarize, percFirstCorrect=mean(firstCorrect), medAttempts=median(nAttempts))
  anova(lm(firstCorrect ~ showCC * showBlanks + as.factor(problem_id), data=test))
  summary(lm(firstCorrect ~ showCC * showBlanks + as.factor(problem_id), data=test))
  fisher.test(xor(test$showCC, test$showBlanks), test$firstCorrect)
  summary(glm(firstCorrect ~ showCC * showBlanks + as.factor(problem_id), data=test, family=binomial()))
  
  
  anova(lm(Q25 ~ showCC * showBlanks + as.factor(problem_id), data=test))
}


analyzeRatings <- function(survey) {
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

analyzeAttempts <- function(survey, attempts, problemID, last_problem_id) {
  performance <- summarizeAttemtps(attempts, problemID, last_problem_id)
  survey$last_problem_id <- survey$problem_id
  merge(survey[,c("user_id", "last_problem_id", "showCC", "showBlanks", "Q25")], performance, by=c("user_id", "last_problem_id"))
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
