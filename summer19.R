source("util.R")

# Run me line by line
loadData <- function() {
  surveyReview <- read.qualtrics("data/review.csv")
  surveyWk10 <- read.qualtrics("data/week10.csv")
  attempts <- read.csv("data/python_problems_anon_aug7.csv", header=T)
  consent <- read.csv("data/consent_info.csv", header=T)
  consented <- consent$user_id[consent$consentrecode==1]
  
  attempts <- attempts[attempts$user_id %in% consented,]
  surveyReview <- surveyReview[surveyReview$user_id %in% attempts$user_id,]
  surveyReview <- cleanSurvey(surveyReview)
  surveyWk10 <- surveyWk10[surveyWk10$user_id %in% attempts$user_id,]
  surveyWk10 <- cleanSurvey(surveyWk10)
  
  attempts$showCC <- ifelse(attempts$text == "", NA, grepl("showCC=1", attempts$text, fixed=T))
  attempts$showBlanks <- ifelse(attempts$text == "", NA, grepl("showBlanks=1", attempts$text, fixed=T))
  
  attempts$timestamp <- as.POSIXct(strptime(sub(":([0-9]{2})$", "\\1", as.character(attempts$timestamp)), "%Y-%m-%d %H:%M:%OS%z"))
  
  attempts <- attempts[order(attempts$timestamp),]
  attempts$has_best_score <- attempts$score == attempts$max_score
}

cleanSurvey <- function(survey) {
  survey <- survey[order(survey$StartDate),]
  survey <- survey[!duplicated(survey[,c("user_id", "problem_id")]),]
  survey
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
  review <- rbind(
    analyzeAttempts(surveyReview, attempts, 148, 147),
    analyzeAttempts(surveyReview, attempts, 150, 149),
    analyzeAttempts(surveyReview, attempts, 152, 151)
  )
  
  ggplot(review, aes(nAttempts)) + geom_histogram() + facet_wrap(~problem_id)
  
  mean((review$showCC.x == review$showCC.y)[!is.na(review$showCC.x) & !is.na(review$showCC.y)])
  
  review <- review[!is.na(review$showCC) & !is.na(review$showBlanks),]
  review <- review[!is.na(review$showCC) & !is.na(review$showBlanks),]
  
  # remove
  # review <- review[!is.na(review$showCC.x) & !is.na(review$showBlanks.x),]
  
  ddply(review, c("showCC", "showBlanks"), summarize, percFirstCorrect=mean(firstCorrect), medAttempts=median(nAttempts))
  anova(lm(firstCorrect ~ showCC * showBlanks + as.factor(problem_id), data=review))
  summary(lm(firstCorrect ~ showCC * showBlanks + as.factor(problem_id), data=review))
  fisher.test(xor(review$showCC, review$showBlanks), review$firstCorrect)
  summary(glm(firstCorrect ~ showCC * showBlanks + as.factor(problem_id), data=review, family=binomial()))
  
  fisher.test(review$showBlanks[!review$showCC], review$firstCorrect[!review$showCC])
  
  review$cond <- paste0(review$showCC, review$showBlanks)
  ggplot(review, aes(y=0+firstCorrect, x=cond)) + stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  ) + facet_wrap(~ problem_id) + scale_y_continuous(limits=c(0,1))
    
  stats <- ddply(review, c("showCC", "showBlanks", "problem_id"), summarize, n=length(showCC),
                 percFirstCorrect=mean(firstCorrect), medAttempts=median(nAttempts))
  stats$se <- se.prop(stats$percFirstCorrect, stats$n)
  ggplot(stats, aes(x=showCC, fill=showBlanks==1, y=percFirstCorrect)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percFirstCorrect-se, ymax=percFirstCorrect+se), width=0.25, position = position_dodge(width = 1)) +
    facet_grid(problem_id ~ .) + scale_y_continuous(limits=c(0,1))
  
  anova(lm(Q25 ~ showCC * showBlanks + as.factor(problem_id), data=review))
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
  merged <- merge(survey[,c("user_id", "last_problem_id", "showCC", "showBlanks", "Q25")], performance, 
                  by=c("user_id", "last_problem_id"), all.y=T)
  merged$showCC <- ifNA(merged$showCC.x, merged$showCC.y)
  merged$showBlanks <- ifNA(merged$showBlanks.x, merged$showBlanks.y)
  return (merged)
}

summarizeAttemtps <- function(attempts, problem_id, last_problem_id) {
  problemAttempts <- attempts[attempts$problem_id == problem_id,]
  lastProblemAttempts <- attempts[attempts$problem_id == last_problem_id,]
  df <- NA
  for (user_id in unique(problemAttempts$user_id)) {
    userAttempts <- problemAttempts[problemAttempts$user_id == user_id,]
    userLastAttempts <- lastProblemAttempts[lastProblemAttempts$user_id == user_id,]
    df <- rbind(df, data.frame(
      problem_id = problem_id,
      last_problem_id = last_problem_id,
      user_id = user_id,
      # TODO: Is first best? Also, may not work if they never got the prior one right
      # (but then they didn't have a condition, so it doesn't matter...)
      showCC = first(userLastAttempts$showCC[!is.na(userLastAttempts$showCC) & userLastAttempts$has_best_score]),
      showBlanks = first(userLastAttempts$showBlanks[!is.na(userLastAttempts$showBlanks) & userLastAttempts$has_best_score]),
      nAttempts = nrow(userAttempts),
      firstCorrect = first(userAttempts$has_best_score),
      everCorrect = any(userAttempts$has_best_score)
    ))
  }
  df <- df[-1,]
  return (df)
} 
