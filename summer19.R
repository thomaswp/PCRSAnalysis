source("util.R")

library(data.table)
library(car)

options(contrasts = c("contr.sum","contr.poly"))

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
    problem_id=c(147, 148, 149, 150, 151, 152, 42, 61, 63),
    problemName=c("2A_Find_outliers", "2B_Get_short_heights", "3A_Discount", "3B_Discount", "1A_Average_height", "1B_Average_even",
                  "1A_List_of_indexes", "2A_Bubble_bubble", "2B_Inserting_into_list")
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
  correct <- correct[,intersect(names(correct), names(survey))]
                                #c("user_id", "problem_id", "showCC", "showBlanks", "Q25", "Q38", "Q41", "Q43"))]
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
  wk10A <- analyzeAttempts(surveyWk10, attempts, 63, 42)
  wk10A$showCC42 <- wk10A$showCC
  wk10A$showBlanks42 <- wk10A$showBlanks
  wk10B <- analyzeAttempts(surveyWk10, attempts, 63, 61)
  
  wk10 <- merge(wk10A[,c("user_id", "showCC42", "showBlanks42")], wk10B, all.y=T)
  
  # Merge with problem names
  wk10 <- merge(wk10, problemNames, all.x=T)
  # Remove those with no known condition
  wk10 <- wk10[!is.na(wk10$showCC) & !is.na(wk10$showBlanks),]
  # 98% of students attempted the problem, so probably easier to remove them
  mean(wk10$attempted)
  wk10 <- wk10[wk10$attempted,]
}
 
drawPlots <- function() {
  
  # Choose a dataset - Run one but not both of these lines
  dataset <- wk10
  dataset <- review
  
  dataset$showBlanks <- dataset$showBlanks == 1
  dataset$showCC <- dataset$showCC == 1
  dataset$cond <- paste0(ifelse(dataset$showCC==0, "no_cc", "cc"), "/", ifelse(dataset$showBlanks==0, "no_blanks", "blanks"))
  dataset$cond <- ordered(dataset$cond, c("no_cc/no_blanks", "no_cc/blanks", "cc/no_blanks", "cc/blanks"))
  
  timeStats <- ddply(dataset, "problem_id", summarize, mTime = mean(timeWorking, na.rm=T), sdTime = sd(timeWorking, na.rm=T),
                                                       medLastTime = median(lastTimeRevising))
  dataset <- merge(dataset, timeStats)
  dataset$normTW <- (dataset$timeWorking - dataset$mTime) / dataset$sdTime
  hist(dataset$normTW)
  dataset$lastTop50 <- dataset$lastTimeRevising <= dataset$medLastTime
  
  ggplot(dataset, aes(nAttempts)) + geom_histogram() + facet_wrap(~problemName)
  ddply(dataset, "problemName", summarize, medAttempts=median(nAttempts), meanAttempts=mean(nAttempts), medTR = median(timeRevising))
  
  ggplot(dataset, aes(y=normTW, x=cond)) + geom_boxplot() + 
    stat_summary(geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4) +
    ggtitle("Time Revising by Condition")
  
  ggplot(dataset, aes(y=normTW, x=cond)) + geom_boxplot() + 
    stat_summary(geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4) +
    facet_grid(problemName ~ .) +
    ggtitle("Time Revising by Condition")
  
  # For testing how Anova works
  # m1 <- lm(timeRevising ~ showCC * showBlanks, data=dataset)
  # m2 <- lm(timeRevising ~ showCC * showBlanks + problem_id, data=dataset)
  # anova(m1, m2)
  
  kruskal.test(dataset$normTR, dataset$cond)
  kruskal.test(dataset$timeRevising, dataset$cond)
  summary(lm(timeRevising ~ showCC * showBlanks + problem_id, data=dataset))
  summary(lm(normTR ~ showCC * showBlanks, data=dataset))
  # Report this
  Anova(lm(normTW ~ showCC * showBlanks, data=dataset),type = 3)
  raov(normTR ~ showCC * showBlanks, data=dataset)
  condCompare(dataset$normTR, dataset$showBlanks, test=t.test)
  condCompare(dataset$normTR, dataset$showBlanks, filter=!dataset$showCC, test=t.test)
  condCompare(dataset$normTR, dataset$showBlanks, filter=dataset$showCC)
  
  ggplot(dataset, aes(y=timeRevising, x=cond)) + geom_boxplot() + 
    stat_summary(geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4) +
    ggtitle("Time Revising by Condition")
  
  Anova(lm(rating ~ showCC * showBlanks, data=dataset),type = 3)
  raov(rating ~ showCC * showBlanks, data=dataset)
  
  ggplot(dataset, aes(y=rating, x=cond)) + geom_boxplot() + 
    stat_summary(geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4) +
    ggtitle("Rating")
  
  
  chisq.test(dataset$cond, dataset$survey)
  chisq.test(dataset[,c("survey", "showCC", "showBlanks")])
  chisq.test(dataset$showCC, dataset$survey)
  mean(dataset$survey[dataset$showCC])
  mean(dataset$survey[!dataset$showCC])
  chisq.test(dataset$showBlanks, dataset$survey)
  mean(dataset$survey[dataset$showBlanks])
  mean(dataset$survey[!dataset$showBlanks])
  
  
  statsComb <- ddply(dataset, c("showCC", "showBlanks"), summarize, n=length(showCC),
                 percFirstCorrect=mean(firstCorrect), seFC=se.prop(percFirstCorrect, n),
                 percEverCorrect=mean(everCorrect),seFE=se.prop(percEverCorrect, n),
                 percSubmitted=mean(survey), seSV=se.prop(percSubmitted, n),
                 medAttempts=median(nAttempts),
                 corSurveyFirstCorrect=cor(firstCorrect,survey),
                 fisherSurveyFirstCorrect=fisher.test(firstCorrect,survey)$p.value)

  ggplot(statsComb, aes(x=showCC, fill=showBlanks==1, y=percSubmitted)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percSubmitted-seSV, ymax=percSubmitted+seSV), width=0.25, position = position_dodge(width = 1)) +
    scale_y_continuous(limits=c(0,1)) +
    ggtitle("Percent Completed Survey")
  
  
  Anova(lm(firstScore ~ showCC * showBlanks, data=dataset),type = 3)
  Anova(lm(rating ~ showCC * showBlanks, data=dataset),type = 3)
  chisq.test(survey ~ cond, dataset[dataset$cond != "no_cc/no_blanks",])
  
  condCompare(dataset$firstScore*25, dataset$showBlanks, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=!dataset$showCC, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=!dataset$showCC)
  condCompare(dataset$firstScore*25, dataset$showCC, filter=!dataset$showBlanks, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showCC, filter=!dataset$showBlanks)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=dataset$showCC, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=dataset$showCC)
  condCompare(dataset$firstScore*25, dataset$showCC, filter=dataset$showBlanks)
  compareStats(dataset$firstScore[dataset$cond == "no_cc/no_blanks"]*25, dataset$firstScore[dataset$cond == "cc/blanks"]*25, test=t.test)
  
  ddply(dataset, c("showCC", "showBlanks"), summarize, mTests=mean(firstScore*25), sdTests=sd(firstScore*25),
                                                       mRating=mean(rating, na.rm=T), sdRating=sd(rating, na.rm=T),
                                                       pComplete=mean(survey)*100)
  
  stat_box_data <- function(y) {
    return( 
      data.frame(
        y = 0,
        label = paste('n =', length(y))
      )
    )
  }
  ggplot(dataset, aes(y=firstScore/4, x=showCC, fill=showBlanks==1)) + geom_boxplot(position="dodge") + 
    stat_summary(geom = "point", fun.y = "mean", col = "black", size = 1, shape = 1, position = position_dodge(width = 0.8)) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4, position = position_dodge(width = 0.8)) +
    # stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9, position = position_dodge(width = 0.8)),
    scale_y_continuous(labels=scales::percent, name = "Tests Passed") +
    scale_x_discrete(name="Explicit Prompts") +
    scale_fill_manual(name="Implicit\nPrompts", values=twoColors) +
    #facet_grid(problemName ~ .) +
    theme_bw() +
    ggtitle("Test Passed on First Attempt")
  
  
  
  
  stats <- ddply(dataset, c("showCC", "showBlanks", "problemName"), summarize, n=length(showCC),
                 percFirstCorrect=mean(firstCorrect), seFC=se.prop(percFirstCorrect, n),
                 percEverCorrect=mean(everCorrect),seFE=se.prop(percEverCorrect, n),
                 percSubmitted=mean(survey), seSV=se.prop(percSubmitted, n),
                 medAttempts=median(nAttempts),
                 corSurveyFirstCorrect=cor(firstCorrect,survey),
                 fisherSurveyFirstCorrect=fisher.test(firstCorrect,survey)$p.value)
  
  ggplot(stats, aes(x=showCC, fill=showBlanks==1, y=percFirstCorrect)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percFirstCorrect-seFC, ymax=percFirstCorrect+seFC), width=0.25, position = position_dodge(width = 1)) +
    facet_grid(problemName ~ .) + scale_y_continuous(limits=c(0,1)) +
    ggtitle("Percent Correct of First Try")
  
  ggplot(stats, aes(x=showCC, fill=showBlanks==1, y=percSubmitted)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percSubmitted-seSV, ymax=percSubmitted+seSV), width=0.25, position = position_dodge(width = 1)) +
    facet_grid(problemName ~ .) + scale_y_continuous(limits=c(0,1)) +
    ggtitle("Percent Completed Survey")
  

  
  
  statsSurvey <- ddply(dataset[dataset$survey,], c("showCC", "showBlanks", "problemName"), summarize, n=length(showCC),
                 percFirstCorrect=mean(firstCorrect), seFC=se.prop(percFirstCorrect, n),
                 percEverCorrect=mean(everCorrect),seFE=se.prop(percEverCorrect, n),
                 medAttempts=median(nAttempts))
  
  ggplot(statsSurvey, aes(x=showCC, fill=showBlanks==1, y=percFirstCorrect)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percFirstCorrect-seFC, ymax=percFirstCorrect+seFC), width=0.25, position = position_dodge(width = 1)) +
    facet_grid(problemName ~ .) + scale_y_continuous(limits=c(0,1)) +
    ggtitle("Percent Correct of First Try (submitted survey only)")
  
  statsAll <- ddply(dataset, c("showCC", "showBlanks"), summarize, n=length(showCC),
                 percFirstCorrect=mean(firstCorrect), seFC=se.prop(percFirstCorrect, n),
                 percEverCorrect=mean(everCorrect),seFE=se.prop(percEverCorrect, n),
                 percSubmitted=mean(survey), seSV=se.prop(percSubmitted, n),
                 medAttempts=median(nAttempts),
                 corSurveyFirstCorrect=cor(firstCorrect,survey),
                 fisherSurveyFirstCorrect=fisher.test(firstCorrect,survey)$p.value)
  
  ggplot(statsAll, aes(x=showCC, fill=showBlanks==1, y=percFirstCorrect)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percFirstCorrect-seFC, ymax=percFirstCorrect+seFC), width=0.25, position = position_dodge(width = 1)) +
    ggtitle("Percent Correct of First Try")
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
  review$rating <- review$Q25
  mean(is.na(review$timeWorking))
  review <- review[!is.na(review$timeWorking),]
  
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
  ) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4) +
  facet_wrap(~ problemName)
  
  ggplot(review, aes(y=firstScore, x=cond)) + geom_boxplot() + 
    stat_summary(
      geom = "point",
      fun.y = "mean",
      col = "black",
      size = 3,
      shape = 24,
      fill = "red"
    ) + stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4) +
    facet_wrap(~ problemName)
    
  stats <- ddply(review, c("showCC", "showBlanks", "problemName"), summarize, n=length(showCC),
                 percFirstCorrect=mean(firstCorrect), seFC=se.prop(percFirstCorrect, n),
                 percEverCorrect=mean(everCorrect),seFE=se.prop(percEverCorrect, n),
                 percSubmitted=mean(survey), seSV=se.prop(percSubmitted, n),
                 medAttempts=median(nAttempts))
  
  ggplot(stats, aes(x=showCC, fill=showBlanks==1, y=percFirstCorrect)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percFirstCorrect-seFC, ymax=percFirstCorrect+seFC), width=0.25, position = position_dodge(width = 1)) +
    facet_grid(problemName ~ .) + scale_y_continuous(limits=c(0,1))
  
  ggplot(stats, aes(x=showCC, fill=showBlanks==1, y=percSubmitted)) + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label=paste0("n=",n)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(ymin=percSubmitted-seSV, ymax=percSubmitted+seSV), width=0.25, position = position_dodge(width = 1)) +
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
  
  
  performance <- summarizeAttemtps(attempts, problem_id)
  performance$last_problem_id <- last_problem_id
  
  priorPerformance <- summarizeAttemtps(attempts, last_problem_id)
  priorPerformance$last_problem_id <- priorPerformance$problem_id
  priorPerformance$lastFirstCorrect <- priorPerformance$firstCorrect
  priorPerformance$lastTimeRevising <- priorPerformance$timeRevising
  priorPerformance$lastTimeStopped <- priorPerformance$timeStopped
  performance <- merge(performance, priorPerformance[,c("user_id", "last_problem_id", "lastFirstCorrect", "lastTimeRevising", "lastTimeStopped")], all.x=T)
  performance$timeFirstSubmit <- as.numeric(performance$timeStarted - performance$lastTimeStopped, units="mins")
  performance$timeWorking <- performance$timeRevising + ifelse(performance$timeFirstSubmit < 15, performance$timeFirstSubmit, 3)
  performance$timeWorking <- ifelse(performance$timeWorking <= 0, NA, performance$timeWorking)
  
  lastProblemSubmissions <- mergeSurveyAttempts(attempts, survey, last_problem_id)
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
      timeRevising = workingTime(userAttempts$timestamp),
      timeStopped = max(userAttempts$timestamp),
      timeStarted = min(userAttempts$timestamp)
    ))
  }
  df <- df[-1,]
  return (df)
}

workingTime <- function(times, maxSep = 3) {
  if (length(times) < 2) return(0)
  total = 0
  for (i in 2:length(times)) {
    diff <- as.numeric(times[i] - times[i-1], units="mins")
    total <- total + min(diff, maxSep)
  }
  return(total)
}
