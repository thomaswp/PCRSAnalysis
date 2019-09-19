source("util.R")

library(data.table)
library(car)
library(lme4)
library(lmerTest)

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
  
  p147Regex <- "[a-zA-Z_-]+\\s*=\\s*\\[\\]\\s*for\\s+[a-zA-Z_-]+\\s+in\\s+data:\\s+if\\s+[a-zA-Z_-]+\\s+[<>]\\s+[a-zA-Z_-]+:\\s+[a-zA-Z_-]+\\.append\\([a-zA-Z_-]+\\)\\s+return\\s+[a-zA-Z_-]+"
  p149Regex <- "for\\s+[a-zA-Z_-]+\\s+in\\s+range\\(len\\(item_prices\\)\\):\\s+item_prices\\[[a-zA-Z_-]+\\]\\s*=\\s*max\\(+item_prices\\[[a-zA-Z_-]+\\]\\s*\\/\\s*divider\\)?\\s*-\\s*dollar_discount\\)*,\\s*[0-9.]+\\)"
  p151Regex <- "[a-zA-Z_-]+\\s*=\\s*0\\s+[a-zA-Z_-]+\\s*=\\s*0\\s+for\\s+[a-zA-Z_-]+\\s+in\\s+heights:\\s+if\\s+[0a-zA-Z_-]+\\s*[><=]+\\s[0a-zA-Z_-]+:\\s+[a-zA-Z_-]+\\s*\\+=\\s+[a-zA-Z_-]+\\s+[1a-zA-Z_-]+\\s*\\+=\\s*[1a-zA-Z_-]+\\s+return\\s+[a-zA-Z_-]+\\s*\\/\\s*[a-zA-Z_-]+"
  
  p147Sol <-
"def find_outliers(data: List[int], outlier_threshold: int) -> List[int]:
    
  outliers = []
  
  for value in data:
    if value > outlier_threshold:
      outliers.append(value)
  
  return outliers"
  p149Sol <- 
"def discount(item_prices: List[float], divider: float, dollar_discount: float) -> None:
    for i in range(len(item_prices)):
        item_prices[i] = max((item_prices[i] / divider) -
        dollar_discount, 1)"
  p151Sol <- 
"def average_height(heights : List[int]) -> float:

    total = 0
    count = 0
    for height in heights:
        if height >= 0:
            total += height
            count += 1

    return total / count"
  solutions <- data.frame(last_problem_id=c(147, 149, 151), solution=c(p147Sol, p149Sol, p151Sol), regex=c(p147Regex, p149Regex, p151Regex))
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
  dataset$surveyTimeCapped <- pmin(dataset$surveyTime, 300)/60
  dataset$diff <- sapply(1:nrow(dataset), function(i) stringMatch(dataset$solution[i], dataset$lastCode[i], normalize="NO") / nchar(as.character(dataset$solution[i])))
  dataset$match <- sapply(1:nrow(dataset), function(i) grepl(dataset$regex[i], dataset$lastCode[i]))
  dataset$match149 <- grepl(p149Regex, dataset$lastCode)
  dataset$match150 <- grepl("min", dataset$code) & !grepl("if", dataset$code)
  
  timeStats <- ddply(dataset, "problem_id", summarize, mTime = mean(timeWorking, na.rm=T), sdTime = sd(timeWorking, na.rm=T),
                                                       medLastTime = median(lastTimeRevising))
  dataset <- merge(dataset, timeStats)
  dataset$normTW <- (dataset$timeWorking - dataset$mTime) / dataset$sdTime
  hist(dataset$normTW)
  dataset$lastTop50 <- dataset$lastTimeRevising <= dataset$medLastTime
  
  
  Anova(lm(rating ~ showCC * showBlanks, data=dataset), type=3)
  Anova(lmer(rating ~ showCC * showBlanks + (1|user_id), data=dataset), type=3)
  
  ggplot(dataset, aes(y=rating, x=showCC, fill=showBlanks==1)) + geom_boxplot(position="dodge") + 
    stat_summary(geom = "point", fun.y = "mean", col = "black", size = 1, shape = 1, position = position_dodge(width = 0.8)) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4, position = position_dodge(width = 0.8)) +
    # stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9, position = position_dodge(width = 0.8)),
    scale_y_continuous(labels=scales::percent, name = "Tests Passed") +
    scale_x_discrete(name="Explicit Prompts") +
    scale_fill_manual(name="Implicit\nPrompts", values=twoColors) +
    #facet_grid(problemName ~ .) +
    theme_bw() +
    ggtitle("Rating")
  
  Anova(lm(firstScore ~ showCC * showBlanks, data=dataset), type=3)
  Anova(lmer(firstScore ~ showCC * showBlanks + (1|user_id), data=dataset), type=3)
  
  condCompare(dataset$firstScore*25, dataset$showBlanks, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=!dataset$showCC, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=!dataset$showCC)
  condCompare(dataset$firstScore*25, dataset$showCC, filter=!dataset$showBlanks, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showCC, filter=!dataset$showBlanks)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=dataset$showCC, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=dataset$showCC)
  condCompare(dataset$firstScore*25, dataset$showCC, filter=dataset$showBlanks)
  compareStats(dataset$firstScore[dataset$cond == "no_cc/no_blanks"]*25, dataset$firstScore[dataset$cond == "cc/blanks"]*25, test=t.test)
  
  Anova(lmer(surveyTimeCapped ~ showCC * showBlanks + (1|user_id), data=dataset), type=3)
  
  Anova(lm(surveyTimeCapped ~ showCC * showBlanks, data=dataset), type=3)
  ddply(dataset, "cond", summarize, mTime=mean(surveyTimeCapped, na.rm=T), sdTime=sd(surveyTimeCapped, na.rm=T))
  
  chisq.test(dataset$cond, dataset$survey)
  chisq.test(dataset$showCC, dataset$survey)
  chisq.test(dataset$showBlanks, dataset$survey)
  
  
  ggplot(filt, aes(y=firstScore/4, x=showCC, fill=showBlanks==1)) + geom_boxplot(position="dodge") + 
    stat_summary(geom = "point", fun.y = "mean", col = "black", size = 1, shape = 1, position = position_dodge(width = 0.8)) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4, position = position_dodge(width = 0.8)) +
    # stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9, position = position_dodge(width = 0.8)),
    scale_y_continuous(labels=scales::percent, name = "Tests Passed") +
    scale_x_discrete(name="Compare Prompts") +
    scale_fill_manual(name="Explain\nPrompts", values=twoColors) +
    #facet_grid(problemName ~ .) +
    theme_bw() +
    ggtitle("Test Passed on First Attempt")
  
  
  
  stat_box_data <- function(x) {
    return (function(y) {
      return( 
        data.frame(
          y = 1.15 * max(x, na.rm=T),
          label = paste0('n=', length(y))
        )
      )
    })
  }
  ggplot(dataset, aes(y=surveyTimeCapped, x=showCC, fill=showBlanks==1)) + geom_boxplot(position="dodge") + 
    stat_summary(geom = "point", fun.y = "mean", col = "black", size = 1, shape = 1, position = position_dodge(width = 0.75)) + 
    stat_summary(fun.data = stat_box_data(dataset$surveyTimeCapped), geom = "text", hjust = 0.5, vjust = 0.9, position = position_dodge(width = 0.8)) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width=0.4, position = position_dodge(width = 0.75), color="#000000") +
    scale_y_continuous(name = "Time (m)", breaks = c(1, 2, 3, 4, 5)) +
    scale_x_discrete(name="Compare Prompts") +
    scale_fill_manual(name="Explain\nPrompts", values=twoColors) +
    #facet_grid(problemName ~ .) +
    theme_bw() +
    ggtitle("Time on Intervention")
  Anova(lm(surveyTimeCapped ~ showCC * showBlanks, data=dataset), type=3)
  ddply(dataset, "cond", summarize, mTime=mean(surveyTimeCapped, na.rm=T), sdTime=sd(surveyTimeCapped, na.rm=T))
  
  
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
  mean(dataset$survey[dataset$showCC])
  mean(dataset$survey[!dataset$showCC])
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
  
  tbl <- dataset[dataset$cond != "no_cc/no_blanks", c("survey", "cond")]
  chisq.test(tbl$survey, tbl$cond)
  
  filt <- dataset[dataset$problem_id != 150,]
  
  kruskal.test(filt$firstScore, filt$cond)
  summary(aov(filt$firstScore ~ filt$cond))
  Anova(lm(firstScore ~ showCC * showBlanks, data=filt),type = 3)
  summary(lmer(firstScore ~ showCC * showBlanks + (1|user_id), data=filt),type = 3)
  summary(lmer(firstScore ~ cond + (1|user_id), data=filt),type = 3)
  
  condCompare(dataset$firstScore*25, dataset$showBlanks, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=!dataset$showCC, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=!dataset$showCC)
  condCompare(dataset$firstScore*25, dataset$showCC, filter=!dataset$showBlanks, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showCC, filter=!dataset$showBlanks)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=dataset$showCC, test=t.test)
  condCompare(dataset$firstScore*25, dataset$showBlanks, filter=dataset$showCC)
  condCompare(dataset$firstScore*25, dataset$showCC, filter=dataset$showBlanks)
  compareStats(dataset$firstScore[dataset$cond == "no_cc/no_blanks"]*25, dataset$firstScore[dataset$cond == "cc/blanks"]*25, test=t.test)
  compareStats(dataset$firstScore[dataset$cond == "no_cc/blanks"]*25, dataset$firstScore[dataset$cond == "cc/no_blanks"]*25, test=t.test)
  
  ddply(dataset, c("showCC", "showBlanks"), summarize, mTests=mean(firstScore*25), sdTests=sd(firstScore*25),
                                                       mRating=mean(rating, na.rm=T), sdRating=sd(rating, na.rm=T),
                                                       pComplete=mean(survey)*100)
  

  
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
  
  # Merge with problem names and solutions
  review <- merge(review, problemNames, all.x=T)
  review <- merge(review, solutions)
  # Remove those with no known condition
  review <- review[!is.na(review$showCC) & !is.na(review$showBlanks),]
  # 99% of students attempted the problem, so probably easier to remove them
  mean(review$attempted)
  review <- review[review$attempted,]
  review$rating <- review$Q25
  mean(is.na(review$timeWorking))
  review <- review[!is.na(review$timeWorking),]
  review$surveyTime <- ifNA(review$Q29_Page.Submit, ifNA(review$Q30_Page.Submit, review$Q61_Page.Submit))
  review$Q56[!is.na(review$Q56) & review$Q56 == ""] <- NA
  review$Q38[!is.na(review$Q38) & review$Q38 == ""] <- NA
  review$Q23[!is.na(review$Q23) & review$Q23 == ""] <- NA
  review$comparison <- ifNA(as.character(review$Q56), ifNA(as.character(review$Q38), as.character(review$Q23)))
  
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
  priorPerformance$lastCode <- priorPerformance$code
  performance <- merge(performance, priorPerformance[,c("user_id", "last_problem_id", "lastFirstCorrect", "lastTimeRevising", "lastTimeStopped", "lastCode")], all.x=T)
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
      timeStarted = min(userAttempts$timestamp),
      code = last(userAttempts$submission)
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

stringMatch <-
  function(string.1, string.2, normalize=c('YES', 'NO'), penalty = 1, case.sensitive = FALSE) {
    normalize <- toupper(normalize)
    normalize <- match.arg(normalize)
    if(case.sensitive == FALSE) {   
      string.1 <- tolower(string.1)
      string.2 <- tolower(string.2)
    }
    string.1 <- as.character(string.1)
    string.2 <- as.character(string.2)
    s1 <- strsplit(string.1,split="\\s+")[[1]]
    s2 <- strsplit(string.2,split="\\s+")[[1]]
    n <- length(s1)
    m <- length(s2)
    d <- matrix(0, nrow=n+1, ncol=m+1)
    d[,1] <- 0:n
    d[1,] <- 0:m
    d[1,1] <- 0
    for (i in 2:(n+1)) {
      for (j in 2:(m+1)) {
        if (s1[i-1] == s2[j-1]) cost <- 0 else cost <- penalty
        d[i,j] <- min(d[i-1,j] + 1, # insertion
                      d[i,j-1] + 1, # deletion
                      d[i-1,j-1] + cost) # substitution
      }
    }
    switch(normalize, YES = 1-d[n+1,m+1]/max(n,m), # normalize to [0,1]
           NO = d[n+1,m+1]) # Return edit distance
  }
