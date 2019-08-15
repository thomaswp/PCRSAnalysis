source("util.R")

library(data.table)

# Run me line by line
loadData <- function() {
  surveyReview <- read.qualtrics("data/review.csv")
  surveyWk10 <- read.qualtrics("data/week10.csv")
  attempts <- read.csv("data/python_problems_anon_aug7.csv", header=T)
  consent <- read.csv("data/consent_info.csv", header=T)
  consented <- consent$user_id[consent$consentrecode==1]
  
  # Remove test users
  consented <- consented[!(consented %in% c(
    "1595da56e6817bd9334a91f32202378e168516e9efbbdcfaf32280ee90f94ae4a67a89bdff5dc5563ad955c882aee46d00bf14076f5f662b6fd3cbff15a46834",
    "cc1713b969a02f2b21bccc5eaeca7e7688ef84835c46d31de79f805bef043dae1ac8a6a9b44857d14817de11c5b4346de7b73b790e68722a5740d7689f8e24f5",
    "2e06db433ac102c55abf50f5cdb292c3311034530fb547458e99a527e322c24f1197b4636e8d601969e74425aa4b1ff1c5fe09c33cb35d890d28d18d4883fa0b"
  ))]
  
  attempts <- attempts[attempts$user_id %in% consented,]
  surveyReview <- surveyReview[surveyReview$user_id %in% attempts$user_id,]
  # Qualtrics timezone is off by 2 hours... :P
  surveyReview$StartDate <- surveyReview$StartDate + 2*60*60
  surveyReview$EndDate <- surveyReview$EndDate + 2*60*60
  # surveyReview <- cleanSurvey(surveyReview)
  surveyWk10 <- surveyWk10[surveyWk10$user_id %in% attempts$user_id,]
  surveyWk10$StartDate <- surveyWk10$StartDate + 2*60*60
  surveyWk10$EndDate <- surveyWk10$EndDate + 2*60*60
  # surveyWk10 <- cleanSurvey(surveyWk10)
  
  attempts$showCC <- ifelse(attempts$text == "", NA, grepl("showCC=1", attempts$text, fixed=T))
  attempts$showBlanks <- ifelse(attempts$text == "", NA, grepl("showBlanks=1", attempts$text, fixed=T))
  attempts$hasURL <- !is.na(attempts$showCC) & !is.na(attempts$showBlanks)
  
  attempts$timestamp <- as.POSIXct(strptime(sub(":([0-9]{2})$", "\\1", as.character(attempts$timestamp)), "%Y-%m-%d %H:%M:%OS%z"))
  
  attempts <- attempts[order(attempts$timestamp),]
  attempts$is_correct <- attempts$score == attempts$max_score
  
  problemNames <- data.frame(
    problem_id=c(147, 148, 149, 150, 151, 152),
    problemName=c("2A_Find_outliers", "2B_Get_short_heights", "3A_Discount", "3B_Discount", "1A_Average_height", "1B_Average_even")
  )
}

mergeSurveyAttempts <- function(attempts, survey, problem_id) {
  pAttempts <- attempts[attempts$problem_id == problem_id & attempts$is_correct,] # & attempts$hasURL,]
  qSubmissions <- survey[survey$problem_id == problem_id,]
  
  z <- lapply(intersect(pAttempts$user_id,qSubmissions$user_id), function(id) {
    d1 <- subset(qSubmissions, user_id==id)
    d2 <- subset(pAttempts, user_id==id)
    
    print(min(d1$StartDate - d2$timestamp[1]))
    
    # For each survey attempt, find the closest earlier submission attempt
    d1$indices <- sapply(d1$StartDate,function(d) which.min(abs(d2$timestamp[d2$timestamp < d] - d)))
    d2$indices <- 1:nrow(d2)
    
    # Take the first one that matches
    # TODO: May need to sort the matches by timestamp
    merged <- merge(d1,d2,by=c('user_id','indices'))
    merged <- merged[order(merged$timestamp),]
    merged[1,]
  })
  
  z2 <- do.call(rbind,z)
  z2$indices <- NULL
  
  print(mean((z2$showBlanks.x == z2$showBlanks.y)[!is.na(z2$showBlanks.y)]))
  print(mean((z2$showCC.x == z2$showCC.y)[!is.na(z2$showCC.y)]))
  
  z2$showCC <- z2$showCC.x
  z2$showBlanks <- z2$showBlanks.x
  z2$survey <- T
  
  noSubmit <- attempts[attempts$problem_id == problem_id & attempts$is_correct & !(attempts$user_id %in% z2$user_id),]
  noSubmit <- noSubmit[order(noSubmit$timestamp),]
  noSubmit <- noSubmit[!duplicated(noSubmit$user_id),]  
  noSubmit$survey <- F
  
  correct <- rbind.fill(z2, noSubmit)
  correct <- correct[,c("user_id", "problem_id", "showCC", "showBlanks", "Q25", "Q38", "Q41", "Q43")]
  correct$problem_id <- problem_id
  correct$survey <- !is.na(correct$Q25)
  correct
}

cleanSurvey <- function(survey) {
  survey <- survey[order(survey$StartDate),]
  survey <- survey[!duplicated(survey[,c("user_id", "problem_id")]),]
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
  
  # Merge with problem names
  review <- merge(review, problemNames, all.x=T)
  # Remove those with no known condition
  review <- review[!is.na(review$showCC) & !is.na(review$showBlanks),]
  # 99% of students attempted the problem, so probably easier to remove them
  mean(review$attempted)
  review <- review[review$attempted,]
  review$cond <- paste0(review$showCC, review$showBlanks)
  
  ggplot(review, aes(nAttempts)) + geom_histogram() + facet_wrap(~problemName)
  ggplot(review, aes(timeRevising)) + geom_histogram() + facet_wrap(~problemName)
  ggplot(review, aes(firstScore)) + geom_histogram() + facet_wrap(~problemName)
  
  # Only run to remove all those who didn't submit survey (treatment not ITT effect)
  #review <- review[review$survey,]
  
  ddply(review, c("showCC", "showBlanks"), summarize, percFirstCorrect=mean(firstCorrect), medAttempts=median(nAttempts))
  anova(lm(firstCorrect ~ showCC * showBlanks + problemName, data=review))
  summary(lm(firstCorrect ~ showCC * showBlanks + problemName, data=review))
  fisher.test(xor(review$showCC, review$showBlanks), review$firstCorrect)
  summary(glm(firstCorrect ~ showCC * showBlanks + problemName, data=review, family=binomial()))
  
  summary(glm(firstCorrect ~ problemName * sign(showBlanks+showCC), data=review, family=binomial()))
  
  fisher.test(review$showBlanks[!review$showCC], review$firstCorrect[!review$showCC])
  
  
  ggplot(review, aes(y=0+firstCorrect, x=cond)) + stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  ) + facet_wrap(~ problemName) + scale_y_continuous(limits=c(0,1))
  
  ggplot(review, aes(y=timeRevising, x=cond)) + geom_boxplot() + 
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  ) + facet_wrap(~ problemName)
  
  ggplot(review, aes(y=firstScore, x=cond)) + geom_boxplot() + 
    stat_summary(
      geom = "point",
      fun.y = "mean",
      col = "black",
      size = 3,
      shape = 24,
      fill = "red"
    ) + facet_wrap(~ problemName)
    
  stats <- ddply(review, c("showCC", "showBlanks", "problemName"), summarize, n=length(showCC),
                 percFirstCorrect=mean(firstCorrect), percEverCorrect=mean(everCorrect),
                 meanTimeRevising=mean(timeRevising), seTimeRevising=se(timeRevising),
                 medAttempts=median(nAttempts))
  stats$seFC <- se.prop(stats$percFirstCorrect, stats$n)
  stats$seEC <- se.prop(stats$percEverCorrect, stats$n)
  
  ggplot(stats, aes(x=showCC, fill=showBlanks==1, y=percFirstCorrect)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percFirstCorrect-seFC, ymax=percFirstCorrect+seFC), width=0.25, position = position_dodge(width = 1)) +
    facet_grid(problemName ~ .) + scale_y_continuous(limits=c(0,1))
  
  ggplot(stats, aes(x=showCC, fill=showBlanks==1, y=meanTimeRevising)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percFirstCorrect-seFC, ymax=percFirstCorrect+seFC), width=0.25, position = position_dodge(width = 1)) +
    facet_grid(problemName ~ .) + scale_y_continuous(limits=c(0,1))
  
  anova(lm(Q25 ~ showCC * showBlanks + as.factor(problemName), data=review))
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

analyzeAttempts <- function(survey, attempts, problem_id, last_problem_id) {
  lastProblemSubmissions <- mergeSurveyAttempts(attempts, survey, last_problem_id)
  performance <- summarizeAttemtps(attempts, problem_id)
  performance$last_problem_id <- last_problem_id
  lastProblemSubmissions$last_problem_id <- lastProblemSubmissions$problem_id
  lastProblemSubmissions$problem_id <- NULL
  merged <- merge(lastProblemSubmissions, performance, 
                  by=c("user_id", "last_problem_id"), all.x=T)
  merged$attempted <- !is.na(merged$problem_id)
  return (merged)
}

summarizeAttemtps <- function(attempts, problem_id) {
  problemAttempts <- attempts[attempts$problem_id == problem_id,]
  df <- NA
  for (user_id in unique(problemAttempts$user_id)) {
    userAttempts <- problemAttempts[problemAttempts$user_id == user_id,]
    
    df <- rbind(df, data.frame(
      problem_id = problem_id,
      user_id = user_id,
      nAttempts = nrow(userAttempts),
      firstCorrect = first(userAttempts$is_correct),
      firstScore = first(userAttempts$score),
      everCorrect = any(userAttempts$is_correct),
      timeRevising = max(userAttempts$timestamp) - min(userAttempts$timestamp)
    ))
  }
  df <- df[-1,]
  return (df)
} 
