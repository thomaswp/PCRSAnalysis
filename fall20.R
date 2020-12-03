source("util.R")
library(plyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ggpubr)
library(rbin)
library(car)
library(stringr)


GetData <- function(){
  folder = "data/fall2020/"
  consenters <- read.csv(paste0(folder, "pcrs_20201116/consenters.csv")) # 1415 consenters
  (length(unique(consenters$student_id)))
  allAttempts <- read.csv(paste0(folder, "pcrs_20201116/pyandexpt.csv")) # 37052
  (length(unique(allAttempts$user_id))) #total number of students: 2126
  
  ######## filter students ############
  ####################################
  allAttempts <- allAttempts[allAttempts$hashed_id %in% consenters$student_id,]
  ### Length of all students
  (length(unique(allAttempts$user_id))) # 1414 students
  ## just double check
  (length(unique(allAttempts$hashed_id))) # 1414
}

# all analysis is here. You need to GetData first.
dataAnalysis <- function(){
  length(unique(allAttempts$problem_id)) # 90 unique problems.

  # just get attempts of experiment problems 
  attempts = allAttempts[allAttempts$problem_id==39 | allAttempts$problem_id==59 | allAttempts$problem_id==64
                         | allAttempts$problem_id==56 | allAttempts$problem_id==62 | allAttempts$problem_id == 183
                         | allAttempts$problem_id==87, ] #87 is week7 problem [every_second_line]
  length(unique(attempts$user_id)) #1367
  
  # Samiha is just curious to know how many students solved each problem in the study
  attempts39 = allAttempts[allAttempts$problem_id==39,]
  length(unique(attempts39$user_id)) # 1335 solved problem 39, [average daily temo]
  attempts56 = allAttempts[allAttempts$problem_id==56,]
  length(unique(attempts56$user_id)) # 1201 solved problem 56, [reverse look up (2) dict]
  attempts59 = allAttempts[allAttempts$problem_id==59,]
  length(unique(attempts59$user_id)) # 1314 solved problem 59, [only evens] 
  attempts64 = allAttempts[allAttempts$problem_id==64,]
  length(unique(attempts64$user_id)) # 1213 solved problem 64, [reverse look up (2) lists]
  attempts62 = allAttempts[allAttempts$problem_id==62,]
  length(unique(attempts62$user_id)) # 1189 solved problem 62, [Food Quantities]
  attempts183 = allAttempts[allAttempts$problem_id==183,]
  length(unique(attempts183$user_id)) # 1317 solved problem 62, [Food Quantities]
  
  table(attempts$problem_id) 
  
  allAttempts$timestamp <-as.POSIXct(allAttempts$timestamp, format="%Y-%m-%d %H:%M:%S")
  getTimeOfStudy = attempts
  # # The estimated time of the start of the study "2020-10-17 18:48:44"
  getTimeOfStudy = getTimeOfStudy[order(getTimeOfStudy$timestamp), ]
  attempts = attempts[order(attempts$timestamp), ]
  attempts = attempts[attempts$timestamp>= "2020-10-17 16:08:56", ] # to remove 3 attempts of master (not a student) in july
  
  allPriorAttempts = allAttempts[!allAttempts$problem_id %in% attempts$problem_id, ]
  (length(unique(allPriorAttempts$user_id[allPriorAttempts$timestamp < "2020-10-17 16:08:56"]))
    /length(unique(allPriorAttempts$user_id)))
  
  # to make sure prior attempts took place prior to the study
  allPriorAttempts = allPriorAttempts[allPriorAttempts$timestamp< "2020-10-17 16:08:56", ] 
  length(unique(allPriorAttempts$problem_id)) #  we got 75 problems
  length(unique(allPriorAttempts$user_id)) # 1412 unique user.
  length(unique(allAttempts$user_id)) # 1414 unique user.
  #The two students difference is because they did prior problems but in a time after the study,
  #so they were excluded because attempts happened in problems other than the study problems at time
  #of the study should not be priorAttempts
  
  allPriorAttempts$correct <- allPriorAttempts$score == allPriorAttempts$max_score # why we consider only prior problems where students got max score?
  start = TRUE
  for (problemID in unique(allPriorAttempts$problem_id)) {
    problem_attempts <- estimatePriorAttempts(allPriorAttempts, problemID)
    if(start==TRUE){
      priorAttempts = problem_attempts
      start = FALSE
    }else{
      priorAttempts = rbind(priorAttempts, problem_attempts)
    }
  }
  (length(unique(priorAttempts$user_id))) #1412 student
  meanAttempts <- ddply(priorAttempts, c("problem_id"), summarize, mAttempts = mean(nAttempts), sdAttempt=sd(nAttempts))
  meanAttempts = meanAttempts[meanAttempts$sd!=0, ]
  priorAttempts <- merge(priorAttempts, meanAttempts)
  priorAttempts$zAttempts <- (priorAttempts$nAttempts - priorAttempts$mAttempts) / priorAttempts$sdAttempt
  priorKnowledge <- ddply(priorAttempts, "user_id", summarize, mz = mean(zAttempts), sdz=sd(zAttempts))
  mean(priorKnowledge$mz) #0.0057
  mean(priorKnowledge$sdz) #0.7953
  priorKnowledge$highPK <- priorKnowledge$mz < median(priorKnowledge$mz)
  table(priorKnowledge$highPK) #(we got 706 in each group)
  priorKnowledge <- priorKnowledge[priorKnowledge$user_id %in% attempts$user_id,]
 # priorKnowledgeNotIn <- priorKnowledge[!priorKnowledge$user_id %in% attempts$user_id,]
  priorKnowledge$pkRank <- rank(priorKnowledge$mz)
  length(unique(priorKnowledge$user_id)) #1366 students when only having same students in the prior attempts and attempts
  length(unique(attempts$user_id)) #1366
  # merge takes the matching users only between priorAttempts and all users
  attempts <- merge(attempts, priorKnowledge) 
  length(unique(attempts$user_id)) #(1366)
  length(unique(attempts$user_id[attempts$highPK])) #683
  length(unique(attempts$user_id[!attempts$highPK])) #683
  
  #if feedback = null this means they were not assigned to a condition
  attempts$had_feedback <- attempts$feedback_text != ""
  (length(unique(attempts$user_id)))
  #False if showHints  = false, NA if feedback_text = "". It shows if students were in the exp group or not.
  attempts$had_hints <- ifelse(attempts$had_feedback, grepl("'showHints': True", attempts$feedback_text, fixed=T), NA)
  attempts$reverse <- ifelse(attempts$had_feedback, grepl("'reversedShow': True", attempts$feedback_text, fixed=T), NA)
  # this show that a student had hint
  attempts$had_old_hints <- ifelse(attempts$had_feedback, grepl("'hadCodeSuggestion': True", attempts$feedback_text, fixed=T), NA)
  attempts$had_skeletonHints <- ifelse(attempts$had_feedback, grepl("'hadSkeletonHints': True", attempts$feedback_text, fixed=T), NA)
  
  # Reverse was False for the first 2 problems, but True for 5 and 6, so if you had hints when reverse was false, you were in the early group
  attempts$early <- ifelse(attempts$had_feedback, attempts$had_hints != attempts$reverse, NA)
  attempts$correct <- attempts$score == attempts$max_score
  attempts$suggest <- ifelse(attempts$had_old_hints, grepl("'suggest': True", attempts$feedback_text, fixed=T), NA)
  attempts$hLevelInt <- ifelse(attempts$had_old_hints, grepl("'highlightLevelInt': 2", attempts$feedback_text, fixed=T), NA)
  attempts$showMissing <- ifelse(attempts$had_old_hints, grepl("'showMissing': True", attempts$feedback_text, fixed=T), NA)
  attempts$showSelfExplain <- ifelse(attempts$had_hints, grepl("'showSelfExplain': True", attempts$feedback_text, fixed=T), NA)
  (length(unique(attempts$user_id[attempts$early==TRUE]))) #(using new date: 654)
  (length(unique(attempts$user_id[attempts$early==FALSE]))) #(using new date: 706)
  prettyAttempts = attempts[, c("user_id", "timestamp", "score", "problem_id", "name.2", "feedback_text",
                                "max_score", "mz", "highPK", "had_feedback", "had_hints", "had_old_hints", "had_skeletonHints", "reverse", "early",
                                "correct", "showMissing", "showSelfExplain")]
  prettyAttempts = prettyAttempts[order(prettyAttempts$problem_id), ]
  prettyAttempts = prettyAttempts[order(prettyAttempts$user_id), ]
  #looks like there are 2 students who were both in the early and late groups
  #write.csv(prettyAttempts, "prettyAttempts.csv")
  prettyAttempts2 = prettyAttempts[!is.na(prettyAttempts$early),]
  #write.csv(prettyAttempts2, "prettyAttempts2.csv")
  
  ## calculate all parameters for each problem for each student
  attempts = attempts[order(attempts$user_id), ]
  attempts$timestamp <-as.POSIXct(attempts$timestamp, format="%Y-%m-%d %H:%M:%S")
  attempts = attempts[order(attempts$timestamp), ]
  attempts$bestScore = ifelse(attempts$has_best_score=="t", TRUE, FALSE)
  #39 (1A with hints Early), 56 (hints for late), 59 (with hints, Early), 
  #62 (2nd assessment), 64 (hints for late), 183 (1B, first assessment)
  
  problem1_attempts <- estimateParameters(attempts, 39) # Week 6
  problem2_attempts <- estimateParameters(attempts, 56)  # Week 8
  problem3_attempts <- estimateParameters(attempts, 59) # Week 6
  problem4_attempts <- estimateParameters(attempts, 62) # -- assessment Week 8
  problem5_attempts <- estimateParameters(attempts, 64) #Week 8
  problem6_attempts <- estimateParameters(attempts, 183) #Week 6 assessment
  problem7_attempts <- estimateParameters(attempts, 87) #Week 7 problem
  
  attemptsTime = rbind(problem1_attempts, problem2_attempts, problem3_attempts, problem4_attempts,
                       problem5_attempts, problem6_attempts, problem7_attempts)
  length(unique(attemptsTime$user_id)) #1366
  attemptsTime <- merge(attemptsTime, priorKnowledge)
  
  students <- ddply(attempts, "user_id", summarize,
                    anonID = first(hashed_id),
                    n = length(had_feedback),
                    pFeedback = mean(had_feedback),
                    pHints = mean(had_hints[had_feedback]),
                    early = if (pFeedback == 0) NA else any(early, na.rm=T),
                    suggest = any(suggest, na.rm=TRUE),
                    had_old_hints = any(had_old_hints, na.rm=TRUE),
                    skeletonHints = any(had_skeletonHints, na.rm=TRUE),
                    hLevelInt = any(hLevelInt, na.rm=TRUE),
                    showMissing = any(showMissing, na.rm=TRUE),
                    showSelfExplain = any(showSelfExplain, na.rm=TRUE)
  )
  byStudentWTime <- merge(students, attemptsTime) #(7567)
  (length(unique(byStudentWTime$user_id))) #1366 , 1379 with w7 problem
  # 0.5% of students had early NA
  length(unique(byStudentWTime$user_id[is.na(byStudentWTime$early)]))/length(unique(byStudentWTime$user_id))
  (mean(byStudentWTime$pCorrect[byStudentWTime$pCorrect>0]))
  byStudentWTime <- byStudentWTime[!is.na(byStudentWTime$early),] #(7548) , 1358 students
  ##### length of students in the early group:  (653)
  (length(unique(byStudentWTime$user_id[byStudentWTime$early==TRUE])))
  ##### length of students in the late group: (705)
  (length(unique(byStudentWTime$user_id[byStudentWTime$early==FALSE])))
  
  ##########################
  byStudentWTime$mLnTime <- log(byStudentWTime$timeRevising + 1)
  #not sure if the exp here is calculated correctly
  byStudentWTime$exp <- byStudentWTime$early == (byStudentWTime$problem_id == 39 | byStudentWTime$problem_id == 59)
  byStudentWTime$firstCorrect <- byStudentWTime$pCorrect == 1
  byStudentWTime$problem_id_nom <- as.factor(byStudentWTime$problem_id)
  byStudentWTime$isAssessment = ifelse(byStudentWTime$problem_id==62 | byStudentWTime$problem_id==183, TRUE, FALSE)
  byStudentWTime$week = ifelse(byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59, 0, 1)
  byStudentWTime$week = ifelse(byStudentWTime$problem_id==62 | byStudentWTime$problem_id==183, 2, byStudentWTime$week)
  byStudentWTime$gaveUp = ifelse(byStudentWTime$pCorrect==0, TRUE, FALSE)
  byStudentWTime$nHints = byStudentWTime$nSkeletonHints+byStudentWTime$nOldHints
  byStudentWTime$percSkeleton = byStudentWTime$nSkeletonHints/byStudentWTime$nHints
  byStudentWTime$percOldHints = byStudentWTime$nOldHints/byStudentWTime$nHints
  #the number of attempts correlates strongly (and significant) with the total \textit{time} a student spent on the problem
  cor.test(byStudentWTime$nAttempts, byStudentWTime$timeRevising, method = "spearman")
  
  byStudent39 <- byStudentWTime[byStudentWTime$problem_id == 39,]
  byStudent59 <- byStudentWTime[byStudentWTime$problem_id == 59,]
  byStudent183 <- byStudentWTime[byStudentWTime$problem_id == 183,]
  byStudent56 <- byStudentWTime[byStudentWTime$problem_id == 56,]
  byStudent64 <- byStudentWTime[byStudentWTime$problem_id == 64,]
  byStudent62 <- byStudentWTime[byStudentWTime$problem_id == 62,]
  byStudent87 <- byStudentWTime[byStudentWTime$problem_id == 87,]
  
  
  condCompare((byStudent64$pCorrect == 1) + 0, byStudent64$highPK, filter=byStudent64$early)
  condCompare((byStudent64$pCorrect == 1) + 0, byStudent64$highPK, filter=!byStudent64$early)
  
  condCompare((byStudent62$pCorrect == 1) + 0, byStudent62$highPK, filter=byStudent62$early)
  condCompare((byStudent62$pCorrect == 1) + 0, byStudent62$highPK, filter=!byStudent62$early)
  # significant difference in firstCorrect between high and low PK
  chisq.test(byStudent39$firstCorrect, byStudent39$highPK)
  ## high PK = 664, low PK: 666
  (length(unique(byStudent39$user_id[!byStudent39$highPK])))
  # a significant difference in total attempts ( p = 0.00)
  condCompare(byStudent39$nAttempts, byStudent39$highPK) 
  condCompare(byStudent39$nAttempts, byStudent39$early) #it was less but not significant
  
  ## check the difference between the impact of different hint types, not sure if this is a good comparison
  condCompare(byStudent39$nAttempts, byStudent39$skeletonHints) #significant, with skeletons they had more attempts
  condCompare(byStudent39$nAttempts, byStudent39$had_old_hints) #significant, with hints they had more attempts
  
  # And a small but significant correlation
  cor.test(byStudent39$nAttempts, byStudent39$mz, method = "spearman")
  # No significant difference in # of highPK students in each group
  chisq.test(byStudent39$early, byStudent39$highPK)
  # No significant different in mz score either, in either the high or lowPK groups
  condCompare(byStudent39$mz, byStudent39$early)
  ggplot(byStudent39, aes(y=mz,x=early)) + geom_boxplot() + facet_wrap(~ highPK)
  ggplot(byStudent39, aes(y=nAttempts,x=early)) + geom_boxplot() + facet_wrap(~ highPK)
  
  condCompare(byStudent39$mz, byStudent39$early, filter=!byStudent39$highPK)
  # No overall difference between conditions in % first correct on first problem
  chisq.test(byStudent39$firstCorrect, byStudent39$early)
  # No significant differences in firstCorrect between the two groups ( p =1)
  chisq.test(byStudent39$firstCorrect[byStudent39$highPK], byStudent39$early[byStudent39$highPK])
  chisq.test(byStudent39$firstCorrect[!byStudent39$highPK], byStudent39$early[!byStudent39$highPK]) 
  table(byStudentWTime$early[byStudentWTime$problem_id==39])
  
  
  byStudentWTime$highPK1 = ifelse(byStudentWTime$highPK==TRUE, 1, 0)
  byStudentWTime$early1 = ifelse(byStudentWTime$early==TRUE, 1, 0)
  an = aov(nAttempts ~ highPK1 + early1 + highPK1 * early1 , data=byStudentWTime[byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59,])
  summary(an)
  summary(lmer(nAttempts ~ highPK1 + early1 + highPK1 * early1  + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59,]))
  
  byStudentWTime$problem_name = " "
  byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==39, "39 - 1 Practice", byStudentWTime$problem_name)
  byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==59, "59 - 2 Practice", byStudentWTime$problem_name)
  byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==183, "183 - 1 Assessment", byStudentWTime$problem_name)
  byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==87, "87 - week 7", byStudentWTime$problem_name)
  byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==64, "64 - 3 Practice", byStudentWTime$problem_name)
  byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==56, "56 - 4 Practice", byStudentWTime$problem_name)
  byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==62, "62 - 2 Assessment", byStudentWTime$problem_name)
  byStudentWTime$problem_name <- factor(byStudentWTime$problem_name, levels = c("39 - 1 Practice","59 - 2 Practice", "183 - 1 Assessment", "87 - week 7", "64 - 3 Practice", "56 - 4 Practice", "62 - 2 Assessment"))
  
  ggplot(byStudentWTime, aes(y=nAttempts, x=early)) + geom_boxplot() + geom_violin(width=0.2) + scale_x_discrete(labels = c("Late", "Early")) + facet_wrap(~ problem_name)
  
  ggplot(byStudentWTime, aes(timeRevising)) + geom_histogram() + facet_wrap(~ problem_name) + scale_x_continuous(limits=c(0,10))
  ggplot(byStudentWTime, aes(log(timeRevising+1))) + geom_histogram() + facet_wrap(~ problem_name) + scale_x_continuous(limits=c(0,10))
  #less but not significant [avg daily]
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==39)
  # early: 639 and they spent significantly less attempts [only evens]
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==59)
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==183)
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==87) #week7
  #early: 584, marginally significant [reverse lookup]
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==56)
  # less but not not significant
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==64)
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==62)
  
  condCompare(byStudentWTime$timeRevising, byStudentWTime$early, filter = byStudentWTime$problem_id==39)
  #significant
  condCompare(byStudentWTime$timeRevising, byStudentWTime$early, filter = byStudentWTime$problem_id==59)
  condCompare(byStudentWTime$timeRevising, byStudentWTime$early, filter = byStudentWTime$problem_id==183)
  #significant
  condCompare(byStudentWTime$timeRevising, byStudentWTime$early, filter = byStudentWTime$problem_id==56)
  #significant
  condCompare(byStudentWTime$timeRevising, byStudentWTime$early, filter = byStudentWTime$problem_id==64)
  condCompare(byStudentWTime$timeRevising, byStudentWTime$early, filter = byStudentWTime$problem_id==62)
  
  (length((byStudentWTime$user_id[byStudentWTime$early & ( byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59)])))
  
  ## students almost always got the problem correct \textit{eventually} (96.76\% of the time)
  (mean(byStudentWTime$pCorrect > 0))
  
  #===================================================================================
  # Look at students "giving up" - do we see evidence that hints help students persist,
  # but then give up later without hints?
  
  #15 students gave up in experimental, and 18 students gave up in the control 
  (length(unique(byStudent39$user_id[byStudent39$early & byStudent39$gaveUp])))
  (length(unique(byStudent39$user_id[byStudent39$problem_id==39 & !byStudent39$early & byStudent39$gaveUp])))
  chisq.test(byStudent39$gaveUp, byStudent39$early) # p = 0.86 
  
  #29/76 students gave up in experimental, and 47/76 students gave up in the control 
  (length(unique(byStudent59$user_id[byStudent59$early & byStudent59$gaveUp])))
  (length(unique(byStudent59$user_id[!byStudent59$early & byStudent59$gaveUp])))
  chisq.test(byStudent59$gaveUp, byStudent59$early) # p = 0.07 [marginally significant] 
  
  #16/28 students gave up in experimental, and 12/28 students gave up in the control  
  (length(unique(byStudent183$user_id[byStudent183$early & byStudent183$gaveUp])))
  (length(unique(byStudent183$user_id[!byStudent183$early & byStudent183$gaveUp])))
  chisq.test(byStudent183$gaveUp, byStudent183$early) # p = 0.4 
  
  #14/28 students gave up in the group who do not have hint, and 12/28 students gave up in the group who had hints  
  (length(unique(byStudent64$user_id[byStudent64$early & byStudent64$gaveUp])))
  (length(unique(byStudent64$user_id[!byStudent64$early & byStudent64$gaveUp])))
  chisq.test(byStudent64$gaveUp, byStudent64$early) # p = 0.7
  
  #13/20 students gave up in the group who do not have hint, and 7/20 students gave up in the group who had hints  
  (length(unique(byStudent56$user_id[byStudent56$early & byStudent56$gaveUp])))
  (length(unique(byStudent56$user_id[!byStudent56$early & byStudent56$gaveUp])))
  chisq.test(byStudent56$gaveUp, byStudent56$early) # p = 0.2
  
  #22/63 students gave up in the group who do not have hints in W8, and 41/63 students gave up in the group who had hints in W8 
  (length(unique(byStudent62$user_id[byStudent62$early & byStudent62$gaveUp])))
  (length(unique(byStudent62$user_id[!byStudent62$early & byStudent62$gaveUp])))
  chisq.test(byStudent62$gaveUp, byStudent62$early) # p = 0.03
  
  #===================================================================================
  #Try to factor out students who did not get the problem correct on their first try.
  #For each problem, what % of students didn't get right on the first try
  
  byStudentIFT = byStudentWTime[byStudentWTime$pCorrect<1, ]
  # problem 39: 28.49%
  (1-(length(unique(byStudent39$user_id[byStudent39$pCorrect<1])))/((length(unique(byStudent39$user_id)))))*100
  # problem 59: 14.43%
  (1-(length(unique(byStudent59$user_id[byStudent59$pCorrect<1])))/((length(unique(byStudent59$user_id)))))*100
  # problem 183: 34%
  (1-(length(unique(byStudent183$user_id[byStudent183$pCorrect<1])))/((length(unique(byStudent183$user_id)))))*100
  # problem 64: 18.68%
  (1-(length(unique(byStudent64$user_id[byStudent64$pCorrect<1])))/((length(unique(byStudent64$user_id)))))*100
  # problem 56: 40.32%
  (1-(length(unique(byStudent56$user_id[byStudent56$pCorrect<1])))/((length(unique(byStudent56$user_id)))))*100
  # problem 62: 24.75%
  (1-(length(unique(byStudent62$user_id[byStudent62$pCorrect<1])))/((length(unique(byStudent62$user_id)))))*100
  
  #Of those, what percent of the total hints were skeleton hints
  #Or, calculate this for each student, and then plot the distribution of # of code hints and # of skeleton hints
  
  #===================================================================================
  
  
  #===================================================================================
  # Replicating RQ1 analysis (AIED'20)======
  #=========================================
  (length(unique(byStudentWTime$user_id[byStudentWTime$early==TRUE]))) # 653
  (length(unique(byStudentWTime$user_id[byStudentWTime$early==FALSE]))) # 705
  # Table 1 in the paper
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 39)
  # length of students who completed this problem eventually
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==39 & byStudentWTime$early==TRUE])))
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==39 & byStudentWTime$early==TRUE & byStudentWTime$pCorrect>0])))
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==39 & byStudentWTime$early==FALSE & byStudentWTime$pCorrect>0])))
  
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 59)
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==59 & byStudentWTime$early==TRUE])))
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==59 & byStudentWTime$early==TRUE & byStudentWTime$pCorrect>0])))
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==59 & byStudentWTime$early==FALSE & byStudentWTime$pCorrect>0])))
  
  
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 183)
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==183 & byStudentWTime$early==TRUE & byStudentWTime$pCorrect>0])))
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==183 & byStudentWTime$early==FALSE & byStudentWTime$pCorrect>0])))
  
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 64)
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==64 & byStudentWTime$early==TRUE & byStudentWTime$pCorrect>0])))
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==64 & byStudentWTime$early==FALSE & byStudentWTime$pCorrect>0])))
  
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 56)
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==56 & byStudentWTime$early==TRUE & byStudentWTime$pCorrect>0])))
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==56 & byStudentWTime$early==FALSE & byStudentWTime$pCorrect>0])))
  
  condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 62)
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==62 & byStudentWTime$early==TRUE & byStudentWTime$pCorrect>0])))
  (length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==62 & byStudentWTime$early==FALSE & byStudentWTime$pCorrect>0])))
  
  
  timeStats <- ddply(byStudentWTime, c("problem_id", "early"), summarize, 
                     nCorrect=sum(pCorrect > 0),
                     mTime=mean(timeRevising), medTime=median(timeRevising), seTime=se(timeRevising),
                     mLnTime=mean(log(timeRevising+1)), seLnTime=se(log(timeRevising+1)),
                     pFirstCorrect=mean(timeRevising==0))
  timeStats
  # End RQ1 ======
  
  condCompare(log(byStudentWTime$timeRevising+1), byStudentWTime$early, filter=byStudentWTime$problem_id==39, test=t.test)
  condCompare(byStudentWTime$timeRevising==0, byStudentWTime$early, filter=byStudentWTime$problem_id==39, test=fisher.test)
  
  condCompare(byStudent39$nAttempts, byStudent39$early, filter=!byStudent39$highPK)
  condCompare(byStudent39$nAttempts, byStudent39$early, filter=byStudent39$highPK)
  condCompare(byStudent39$timeRevising, byStudent39$early, filter=!byStudent39$highPK)
  condCompare(byStudent39$timeRevising, byStudent39$early, filter=byStudent39$highPK)
  condCompare(byStudent56$nAttempts, byStudent56$early, filter=byStudent56$highPK)
  
  
  # Replicating RQ2 analysis (AIED'20)======
  #=========================================
  pkStats <- ddply(byStudentWTime, c("problem_id", "early", "highPK"), summarize,
                   n=length(problem_id),
                   mTime=mean(timeRevising), medTime=median(timeRevising), seTime=se(timeRevising),
                   mLnTime=mean(log(timeRevising+1)), seLnTime=se(log(timeRevising+1)),
                   mAttempts=mean(nAttempts), seAttempts=se(nAttempts),
                   pFirstCorrect=mean(firstCorrect),
                   seFirstCorrect = se.prop(pFirstCorrect, n))
  
  pkStats$Prior_Knowledge = ifelse(pkStats$highPK==TRUE, "High", "Low")
  
  P39Plot = ggplot(pkStats[pkStats$problem_id == 39,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) + 
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank()
    )
  
  P59Plot = ggplot(pkStats[pkStats$problem_id == 59,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank()
    )
  
  P183Plot = ggplot(pkStats[pkStats$problem_id == 183,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
  P64Plot = ggplot(pkStats[pkStats$problem_id == 64,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
    theme(legend.position = "none") +  scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
  P56Plot = ggplot(pkStats[pkStats$problem_id == 56,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early")) +
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
  P62Plot = ggplot(pkStats[pkStats$problem_id == 62,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+ 
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
  figure = ggarrange(P39Plot, P59Plot, P183Plot, P64Plot, P56Plot, P62Plot,
                     labels = c("1Practice", "2Practice", "1Assessment", "3Practice", "4Practice", "2Assessment"),
                     ncol = 3, nrow = 2,
                     common.legend = TRUE,
                     font.label = list(size = 9, face = "bold", color ="blue"),
                     vjust = 1)
  
  annotate_figure(figure,
                  bottom = text_grob("Average number of attempts", color = "blue", face = "bold", size = 10)
  )
  
  ##### plots for time
  ###################
  P39PlotT = ggplot(pkStats[pkStats$problem_id == 39,], aes(y=mTime, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mTime-seTime, ymax=mTime+seTime)) + 
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank()
    )
  plot(P39PlotT)
  
  P59PlotT = ggplot(pkStats[pkStats$problem_id == 59,], aes(y=mTime, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mTime-seTime, ymax=mTime+seTime)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank()
    )
  #plot(P59PlotT)
  
  P183PlotT = ggplot(pkStats[pkStats$problem_id == 183,], aes(y=mTime, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mTime-seTime, ymax=mTime+seTime)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  #plot(P180PlotT)
  
  P64PlotT = ggplot(pkStats[pkStats$problem_id == 64,], aes(y=mTime, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mTime-seTime, ymax=mTime+seTime)) +
    theme(legend.position = "none") +  scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  #plot(P175Plot)
  
  P56PlotT = ggplot(pkStats[pkStats$problem_id == 56,], aes(y=mTime, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mTime-seTime, ymax=mTime+seTime)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early")) +
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
  P62PlotT = ggplot(pkStats[pkStats$problem_id == 62,], aes(y=mTime, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
    geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mTime-seTime, ymax=mTime+seTime)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+ 
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
  figureT = ggarrange(P39PlotT, P59PlotT, P183PlotT, P64PlotT, P56PlotT, P62PlotT,
                     labels = c("1Practice", "2Practice", "1Assessment", "3Practice", "4Practice", "2Assessment"),
                     ncol = 3, nrow = 2,
                     common.legend = TRUE,
                     font.label = list(size = 9, face = "bold", color ="blue"),
                     vjust = 1)
  
  annotate_figure(figureT,
                  bottom = text_grob("Average Revising Time", color = "blue", face = "bold", size = 10)
  )
  
  # End RQ2 =====
  
  byStudentWTime$problemID = 1
  byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 59, 2, byStudentWTime$problemID)
  byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 183, 3, byStudentWTime$problemID)
  byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 64, 4, byStudentWTime$problemID)
  byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 56, 5, byStudentWTime$problemID)
  byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 62, 6, byStudentWTime$problemID)
  
  # Not considering the interaction, there's a significant of PK
  anova(m1 <- lmer(nAttempts ~ exp + highPK + problem_id_nom + (1 | user_id), data=byStudentWTime[ byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59 |  byStudentWTime$problem_id==183 | byStudentWTime$problem_id==62 | byStudentWTime$problem_id==56 | byStudentWTime$problem_id==64 ,]), type="III")
  #There is a significant interaction effect between experimental and high prior knowledge
  anova(m2 <- lmer(nAttempts ~ exp * highPK + problem_id_nom + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59 |  byStudentWTime$problem_id==183 | byStudentWTime$problem_id==62 | byStudentWTime$problem_id==56 | byStudentWTime$problem_id==64 ,]), type="III")
  # But the interaction model is significantly better
  anova(m1, m2)
  
  ## Additional models using week as a factor.
  anova(lmer(nAttempts ~ problemID + week + highPK + exp*highPK + exp*week + (1|user_id), data= byStudentWTime), type="III")
  anova(lmer(nAttempts ~ problemID + week + highPK + exp*highPK + exp*week + (1|user_id), data= byStudentWTime[byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59 | byStudentWTime$problem_id==64 | byStudentWTime$problem_id==56, ]))
  anova(lmer(nAttempts ~ problemID + week + exp*week + (1|user_id), data= byStudentWTime[byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59 | byStudentWTime$problem_id==64 | byStudentWTime$problem_id==56, ]))
  
  # Replicating discussion analysis (AIED'20)======
  #=========================================
  byStudent39 <- byStudentWTime[byStudentWTime$problem_id == 39,]
  condCompare((byStudent39$pCorrect == 1) + 0, byStudent39$highPK, filter=byStudent39$early)
  condCompare((byStudent39$pCorrect == 1) + 0, byStudent39$highPK, filter=!byStudent39$early)
  
  condCompare((byStudent39$pCorrect == 1) + 0, byStudent39$early, filter=!byStudent39$highPK)
  condCompare((byStudent39$pCorrect == 1) + 0, byStudent39$early, filter=byStudent39$highPK)
  condCompare(byStudent39$nAttempts, byStudent39$early, filter=!byStudent39$highPK)
  condCompare(byStudent39$nAttempts, byStudent39$early, filter=byStudent39$highPK)
  
  condCompare((byStudent39$pCorrect == 1) + 0, byStudent39$highPK, filter=!byStudent39$early)
  condCompare((byStudent59$pCorrect == 1) + 0, byStudent59$highPK, filter=byStudent59$early)
  condCompare((byStudent59$pCorrect == 1) + 0, byStudent59$highPK, filter=!byStudent59$early)
  condCompare((byStudent183$pCorrect == 1) + 0, byStudent183$highPK, filter=byStudent183$early)
  condCompare((byStudent183$pCorrect == 1) + 0, byStudent183$highPK, filter=!byStudent183$early)
  condCompare((byStudent56$pCorrect == 1) + 0, byStudent56$highPK, filter=byStudent56$early)
  condCompare((byStudent56$pCorrect == 1) + 0, byStudent56$highPK, filter=!byStudent56$early)
  condCompare((byStudent64$pCorrect == 1) + 0, byStudent64$highPK, filter=byStudent64$early)
  condCompare((byStudent64$pCorrect == 1) + 0, byStudent64$highPK, filter=!byStudent64$early)
  condCompare((byStudent62$pCorrect == 1) + 0, byStudent62$highPK, filter=byStudent62$early)
  condCompare((byStudent62$pCorrect == 1) + 0, byStudent62$highPK, filter=!byStudent62$early)
  
  
  #### Plots for pFirstCorrect
  ############################
  P39Plot2 = ggplot(pkStats[pkStats$problem_id == 39,], aes(y=pFirstCorrect, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank()
    )
  
  
  P59Plot2 = ggplot(pkStats[pkStats$problem_id == 59,], aes(y=pFirstCorrect, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank()
    )
  
  #plot(P183Plot)
  P183Plot2 = ggplot(pkStats[pkStats$problem_id == 183,], aes(y=pFirstCorrect, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  #plot(P174Plot)
  
  P64Plot2 = ggplot(pkStats[pkStats$problem_id == 64,], aes(y=pFirstCorrect, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
    theme(legend.position = "none") +  scale_x_discrete(labels = c("Late", "Early"))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  #plot(P175Plot)
  
  P56Plot2 = ggplot(pkStats[pkStats$problem_id == 56,], aes(y=pFirstCorrect, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early")) +
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
  P62Plot2 = ggplot(pkStats[pkStats$problem_id == 62,], aes(y=pFirstCorrect, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
    theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+ 
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
  figure = ggarrange(P39Plot2, P59Plot2, P183Plot2, P64Plot2, P56Plot2, P62Plot2,
                     labels = c("1Practice", "2Practice", "1Assessment", "3Practice", "4Practice", "2Assessment"),
                     ncol = 3, nrow = 2,
                     common.legend = TRUE,
                     font.label = list(size = 9, face = "bold", color ="blue"),
                     vjust = 1)
  
  annotate_figure(figure,
                  bottom = text_grob("Average pCorrect", color = "blue", face = "bold", size = 10)
  )
  
  
  Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 39,]), type="III")
  Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 59,]), type="III")
  Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 183,]), type="III")
  Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 64,]), type="III")
  Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 56,]), type="III")
  Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 62,]), type="III")
  
}

## helping functions
workingTime <- function(times, maxSep = 3) { #times is attempts of a user of a given problem id
  if (length(times) < 2) return(0)
  total = 0
  for (i in 2:length(times)) {
    diff <- as.numeric(times[i] - times[i-1], units="mins")
    total <- total + min(diff, maxSep)
  }
  return(total)
}

estimateParameters <- function(attempts, problem_id){
  problemAttempts <- attempts[attempts$problem_id == problem_id,]
  timePerProblem <- NA
  for (user_id in unique(problemAttempts$user_id)) {
    userAttempts <- problemAttempts[problemAttempts$user_id == user_id,]
    userAttempts <- userAttempts[order(userAttempts$timestamp), ]
    # Find the last attempt before they've gotten it right
    lastValidAttempt <- min(c(which(userAttempts$correct), nrow(userAttempts)))
    # Keep only through that attempt (i.e. ignore attempts after correct)
    userAttempts <- userAttempts[1:lastValidAttempt,]
    timePerProblem <- rbind(timePerProblem, data.frame(
      problem_id = problem_id,
      user_id = user_id,
      nAttempts = nrow(userAttempts),
      timeRevising = workingTime(userAttempts$timestamp),
      timeStopped = max(userAttempts$timestamp),
      timeStarted = min(userAttempts$timestamp),
      pCorrect = mean(userAttempts$correct),
      had_hints = any(userAttempts$had_hints[userAttempts$had_feedback]),
      had_selfExplain = any(userAttempts$showSelfExplain[userAttempts$showSelfExplain]),
      had_feedback = any(userAttempts$had_feedback),
      nInCorrect = sum(userAttempts$correct==FALSE),
      mBestScore = mean(userAttempts$bestScore),
      nSkeletonHints = length(userAttempts$user_id[userAttempts$had_feedback==TRUE & userAttempts$had_skeletonHints==TRUE]),
      nOldHints = length(userAttempts$user_id[userAttempts$had_feedback==TRUE & userAttempts$had_old_hints==TRUE])
    ))
  }
  timePerProblem <- timePerProblem[-1,]
  return (timePerProblem)
}

estimatePriorAttempts <- function(attempts, problem_id){
  problemAttempts <- attempts[attempts$problem_id == problem_id,]
  timePerProblem <- NA
  for (user_id in unique(problemAttempts$user_id)) {
    userAttempts <- problemAttempts[problemAttempts$user_id == user_id,]
    userAttempts <- userAttempts[order(userAttempts$timestamp), ]
    # Find the last attempt before they've gotten it right
    lastValidAttempt <- min(c(which(userAttempts$correct), nrow(userAttempts)))
    # Keep only through that attempt (i.e. ignore attempts after correct)
    userAttempts <- userAttempts[1:lastValidAttempt,]
    timePerProblem <- rbind(timePerProblem, data.frame(
      problem_id = problem_id,
      user_id = user_id,
      nAttempts = nrow(userAttempts)
    ))
  }
  timePerProblem <- timePerProblem[-1,]
  return (timePerProblem)
}





