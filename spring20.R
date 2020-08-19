source("util.R")
library(plyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ggpubr)
library(rbin)
library(car)



GetData <- function(){
  consentersExp <- read.csv("data/spring2020/pcrs_20200514/consenters.csv") # 111 consenters
  consentersNonExp <- read.csv("data/spring2020/pcrs_exp20200514/consenters.csv") # 627 consenters
  allAttempts_nonExp <- read.csv("data/spring2020/pcrs_20200514/pyandexpt.csv") # non-Exp 37052
  allAttempts_Exp <- read.csv("data/spring2020/pcrs_exp20200514/pyandexpt.csv") # Exp 289203
  
  
  ########## merge consenters #########
  #####################################
  (length(unique(consentersExp$student_id)))
  (length(unique(consentersNonExp$student_id)))
  common_consenters <- consentersNonExp[consentersNonExp$student_id %in% consentersExp$student_id, ] # 15 
  consenters = rbind(consentersExp, consentersNonExp) #738
  (length(unique(consenters$student_id))) # 723 unique consenter
  
  ######## merge students ############
  ####################################
  allAttempts_nonExp <- allAttempts_nonExp[allAttempts_nonExp$hashed_id %in% consenters$student_id,]
  ### Length of all students in the nonExp
  (length(unique(allAttempts_nonExp$user_id))) # 188 students
  
  allAttempts_Exp <- allAttempts_Exp[allAttempts_Exp$hashed_id %in% consenters$student_id,]
  ### Length of all students in the Exp
  (length(unique(allAttempts_Exp$user_id))) # 682 students
  
  commonStudents = allAttempts_nonExp[allAttempts_nonExp$hashed_id %in% allAttempts_Exp$hashed_id, ]
  (length(unique(commonStudents$hashed_id))) # 147
  missingStudents = allAttempts_Exp[!allAttempts_Exp$hashed_id %in% allAttempts_nonExp$hashed_id , ]
  (length(unique(missingStudents$user_id))) # 535 students in the Exp and not in the nonExp.
  
  # to bind both Experimental and nonExperimental, I will add the two students who were not in the nonExp.
  # I did not merge them all because of the duplicate rows, so this way is to avoid duplicates.
  allAttempts = rbind(missingStudents, allAttempts_nonExp)
  (length(unique(allAttempts$hashed_id))) # 723
}

#Use this function if you want to do analysis for Review data, skip it otherwise
ReviewAnalysis <- function(){
  allAttempts_review = read.csv("data/spring2020/pcrs_v220200514/pyandexpt.csv")
  consenters_review = read.csv("data/spring2020/pcrs_v220200514/consenters.csv") #702
  ########## merge consenters #########
  #####################################
  (length(unique(allAttempts_review$user_id))) # 987
  (length(unique(consenters_review$student_id))) #702
  #529
  common_consenters <- consenters[consenters$student_id %in% consenters_review$student_id, ] # 15 
  consenters = rbind(consenters_review, consenters) #738
  (length(unique(consenters$student_id))) # 908 unique consenter
  
  ######## merge students ############
  ####################################
  allAttempts_review <- allAttempts_review[allAttempts_review$hashed_id %in% consenters$student_id,]
  ### Length of all students in the nonExp
  (length(unique(allAttempts_review$user_id))) # 839 students
  length(unique(allAttempts_review$problem_id)) # 134 unique problems.
  
  allAttempts = allAttempts_review
  
}

length(unique(allAttempts$problem_id)) # 117 unique problems.

# just get attempts of experiment problems 
attempts = allAttempts[allAttempts$problem_id==39 | allAttempts$problem_id==56 | allAttempts$problem_id==59
                       | allAttempts$problem_id==62 | allAttempts$problem_id==64 | allAttempts$problem_id==180, ]


(length(unique(attempts$problem_id))) # 6 problems
### Length of students who attempted the 6 problems in the study: 563
(length(unique(attempts$user_id))) # 563 user. in the review: 801
# problems: 39 (1A with hints Early), 59 (with hints, Early), 180 (1B, first assessment)
#           56 (hints for late), 64 (hints for late), 62 (2nd assessment)
table(attempts$problem_id) 

allAttempts$timestamp <-as.POSIXct(allAttempts$timestamp, format="%Y-%m-%d %H:%M:%S")
getTimeOfStudy = attempts
# There is only student who started solving the study problems 1 month and 6 days days earlier than others: 15883 2020-01-12 18:48:44
# # The estimated time of the start of the study "2020-01-12 18:48:44"
getTimeOfStudy = getTimeOfStudy[order(getTimeOfStudy$timestamp), ]
allPriorAttempts = allAttempts[!allAttempts$problem_id %in% attempts$problem_id, ]
(length(unique(allPriorAttempts$user_id[allPriorAttempts$timestamp < "2020-02-22 12:29:05"]))/length(unique(allPriorAttempts$user_id)))
#when removing that student, data makes sense now. The start time is 2/22/20
allPriorAttempts = allPriorAttempts[allPriorAttempts$timestamp < "2020-02-22 12:29:05", ]
length(unique(allPriorAttempts$problem_id)) #  using the new date we got 83 problems
length(unique(allPriorAttempts$user_id)) # 257 unique user. using the new date we got 650 students
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

meanAttempts <- ddply(priorAttempts, c("problem_id"), summarize, mAttempts = mean(nAttempts), sdAttempt=sd(nAttempts))
meanAttempts = meanAttempts[meanAttempts$sd!=0, ]
priorAttempts <- merge(priorAttempts, meanAttempts)
priorAttempts$zAttempts <- (priorAttempts$nAttempts - priorAttempts$mAttempts) / priorAttempts$sdAttempt
priorKnowledge <- ddply(priorAttempts, "user_id", summarize, mz = mean(zAttempts), sdz=sd(zAttempts))
mean(priorKnowledge$mz) #0.0027
mean(priorKnowledge$sdz) #NA
priorKnowledge$highPK <- priorKnowledge$mz < median(priorKnowledge$mz)
table(priorKnowledge$highPK) #(using new date, we got 325 in each group)
priorKnowledge <- priorKnowledge[priorKnowledge$user_id %in% attempts$user_id,]
priorKnowledge$pkRank <- rank(priorKnowledge$mz)
length(unique(priorKnowledge$user_id)) #(using new date: 552)
length(unique(attempts$user_id)) #563
# merge takes the matching users only between priorAttempts and all users
attempts <- merge(attempts, priorKnowledge) 
length(unique(attempts$user_id)) #(using new date: 552)

## just to verify
#################
priorAttempts <- priorAttempts[priorAttempts$user_id %in% attempts$user_id,]
length(unique(priorAttempts$problem_id)) # There are 81 problems
priorAttempts2 <- ddply(priorAttempts, "user_id", summarize, nProblems = length(problem_id))
summary(priorAttempts2$nProblems)
hist(priorAttempts2$nProblems)


#if feedback = null this means they were not assigned to a condition
attempts$had_feedback <- attempts$feedback_text != ""
(length(unique(attempts$user_id)))
#False if showHints  = false, NA if feedback_text = ""
attempts$had_hints <- ifelse(attempts$had_feedback, grepl("'showHints': True", attempts$feedback_text, fixed=T), NA)
attempts$reverse <- ifelse(attempts$had_feedback, grepl("'reversedShow': True", attempts$feedback_text, fixed=T), NA)
attempts$saw_hint <- ifelse(attempts$had_feedback, grepl("'hadCodeSuggestion': True", attempts$feedback_text, fixed=T), NA)
# Reverse was False for the first 2 problems, but True for 5 and 6, so if you had hints when reverse was false, you were in the early group
attempts$early <- ifelse(attempts$had_feedback, attempts$had_hints != attempts$reverse, NA)
attempts$correct <- attempts$score == attempts$max_score
attempts$suggest <- ifelse(attempts$had_hints, grepl("'suggest': True", attempts$feedback_text, fixed=T), NA)
attempts$hLevelInt <- ifelse(attempts$had_hints, grepl("'highlightLevelInt': 2", attempts$feedback_text, fixed=T), NA)
attempts$showMissing <- ifelse(attempts$had_hints, grepl("'showMissing': True", attempts$feedback_text, fixed=T), NA)
attempts$showSelfExplain <- ifelse(attempts$had_hints, grepl("'showSelfExplain': True", attempts$feedback_text, fixed=T), NA)
(length(unique(attempts$user_id[attempts$early==TRUE]))) #(using new date: 238)
(length(unique(attempts$user_id[attempts$early==FALSE]))) #(using new date: 233)
prettyAttempts = attempts[, c("user_id", "timestamp", "score", "problem_id", "name.2", "feedback_text",
                              "max_score", "mz", "highPK", "had_feedback", "had_hints", "reverse", "early",
                              "correct", "showMissing", "showSelfExplain")]
prettyAttempts = prettyAttempts[order(prettyAttempts$problem_id), ]
prettyAttempts = prettyAttempts[order(prettyAttempts$user_id), ]
#looks like there are 2 students who were both in the early and late groups
write.csv(prettyAttempts, "prettyAttempts.csv")

## calculate all parameters for each problem for each student
attempts = attempts[order(attempts$user_id), ]
attempts$timestamp <-as.POSIXct(attempts$timestamp, format="%Y-%m-%d %H:%M:%S")
attempts = attempts[order(attempts$timestamp), ]
attempts$bestScore = ifelse(attempts$has_best_score=="t", TRUE, FALSE)
#39 (1A with hints Early), 56 (hints for late), 59 (with hints, Early), 
#62 (2nd assessment), 64 (hints for late), 180 (1B, first assessment)

problem1_attempts <- estimateParameters(attempts, 39) # Week 7
problem2_attempts <- estimateParameters(attempts, 56)  # Week 8
problem3_attempts <- estimateParameters(attempts, 59) # Week 7
problem4_attempts <- estimateParameters(attempts, 62) # -- not mentioned
problem5_attempts <- estimateParameters(attempts, 64) #Week 8
problem6_attempts <- estimateParameters(attempts, 180) # --

attemptsTime = rbind(problem1_attempts, problem2_attempts, problem3_attempts, problem4_attempts, problem5_attempts, problem6_attempts, problem7_attempts)
length(unique(attemptsTime$user_id))
attemptsTime <- merge(attemptsTime, priorKnowledge)

students <- ddply(attempts, "user_id", summarize,
                  anonID = first(hashed_id),
                  n = length(had_feedback),
                  pFeedback = mean(had_feedback),
                  pHints = mean(had_hints[had_feedback]),
                  early = if (pFeedback == 0) NA else any(early, na.rm=T),
                  suggest = any(suggest, na.rm=TRUE),
                  hLevelInt = any(hLevelInt, na.rm=TRUE),
                  showMissing = any(showMissing, na.rm=TRUE),
                  showSelfExplain = any(showSelfExplain, na.rm=TRUE)
                  )
byStudentWTime <- merge(students, attemptsTime) #(new date: 2872)
# 17.2% of students had early NA
length(unique(byStudentWTime$user_id[is.na(byStudentWTime$early)]))/length(unique(byStudentWTime$user_id))
(mean(byStudentWTime$pCorrect[byStudentWTime$pCorrect>0]))
byStudentWTime <- byStudentWTime[!is.na(byStudentWTime$early),] #(2441)
##### length of students in the early group:  (237)
(length(unique(byStudentWTime$user_id[byStudentWTime$early==TRUE])))
##### length of students in the late group: (220)
(length(unique(byStudentWTime$user_id[byStudentWTime$early==FALSE])))
##########################
byStudentWTime$mLnTime <- log(byStudentWTime$timeRevising + 1)
byStudentWTime$exp <- byStudentWTime$early == (byStudentWTime$problem_id == 39 | byStudentWTime$problem_id == 59)
byStudentWTime$firstCorrect <- byStudentWTime$pCorrect == 1
byStudentWTime$problem_id_nom <- as.factor(byStudentWTime$problem_id)
byStudentWTime$isAssessment = ifelse(byStudentWTime$problem_id==62 | byStudentWTime$problem_id==180, TRUE, FALSE)
byStudentWTime$week = ifelse(byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59, 0, 1)
byStudentWTime$week = ifelse(byStudentWTime$problem_id==62 | byStudentWTime$problem_id==180, 2, byStudentWTime$week)

#the number of attempts correlates strongly (and significant) with the total \textit{time} a student spent on the problem
cor.test(byStudentWTime$nAttempts, byStudentWTime$timeRevising, method = "spearman")

byStudent39 <- byStudentWTime[byStudentWTime$problem_id == 39,]
# Oddly, no difference in firstCorrect between high and low PK
chisq.test(byStudent39$firstCorrect, byStudent39$highPK)
## high PK = 216, low PK: 211
(length(unique(byStudent39$user_id[!byStudent39$highPK])))
# a significant difference in total attempts ( p = 0.00)
condCompare(byStudent39$nAttempts, byStudent39$highPK) 
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
# marginally significant
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
byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==180, "180 - 1 Assessment", byStudentWTime$problem_name)
byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==64, "64 - 3 Practice", byStudentWTime$problem_name)
byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==56, "56 - 4 Practice", byStudentWTime$problem_name)
byStudentWTime$problem_name = ifelse(byStudentWTime$problem_id==62, "62 - 2 Assessment", byStudentWTime$problem_name)
byStudentWTime$problem_name <- factor(byStudentWTime$problem_name, levels = c("39 - 1 Practice","59 - 2 Practice", "180 - 1 Assessment", "64 - 3 Practice", "56 - 4 Practice", "62 - 2 Assessment"))

ggplot(byStudentWTime, aes(y=nAttempts, x=early)) + geom_boxplot() + geom_violin(width=0.2) + scale_x_discrete(labels = c("Late", "Early")) + facet_wrap(~ problem_name)

ggplot(byStudentWTime, aes(timeRevising)) + geom_histogram() + facet_wrap(~ problem_name) + scale_x_continuous(limits=c(0,10))
ggplot(byStudentWTime, aes(log(timeRevising+1))) + geom_histogram() + facet_wrap(~ problem_name) + scale_x_continuous(limits=c(0,10))
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==39)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==59)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==180)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==56)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==64)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id==62)

(length((byStudentWTime$user_id[byStudentWTime$early & ( byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59)])))

## students almost always got the problem correct \textit{eventually} (96.77\% of the time)
(mean(byStudentWTime$pCorrect > 0))

# Replicating RQ1 analysis (AIED'20)======
#=========================================
(length(unique(byStudentWTime$user_id[byStudentWTime$early==TRUE]))) # 237
(length(unique(byStudentWTime$user_id[byStudentWTime$early==FALSE]))) # 220
# Table 1 in the paper
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 39)
# length of students who completed this problem eventually
(length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==39 & byStudentWTime$early==TRUE & byStudentWTime$pCorrect>0])))
(length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==39 & byStudentWTime$early==FALSE & byStudentWTime$pCorrect>0])))

condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 59)
(length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==59 & byStudentWTime$early==TRUE])))
(length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==59 & byStudentWTime$early==TRUE & byStudentWTime$pCorrect>0])))
(length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==59 & byStudentWTime$early==FALSE & byStudentWTime$pCorrect>0])))


condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 180)
(length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==180 & byStudentWTime$early==TRUE & byStudentWTime$pCorrect>0])))
(length(unique(byStudentWTime$user_id[byStudentWTime$problem_id==180 & byStudentWTime$early==FALSE & byStudentWTime$pCorrect>0])))

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

ggplot(timeStats, aes(x=problem_id, y=mTime, color=early)) + 
   geom_line(position=position_dodge(width=0.2)) + 
   geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mTime-seTime, ymax=mTime+seTime))
ggplot(timeStats, aes(x=problem_id, y=mLnTime, color=early)) + 
   geom_line(position=position_dodge(width=0.2)) + 
   geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mLnTime-seLnTime, ymax=mLnTime+seLnTime))
ggplot(timeStats, aes(x=problem_id, y=pFirstCorrect, color=early)) + 
   geom_line()

timeStats$isAssessment <- c(F, F, T, T, F, F, T, T)[timeStats$problem_id - 172 + 1]
timeStats$problemGroup <- c(0, 1, 0, 1, 2, 3, 2, 3)[timeStats$problem_id - 172 + 1]

ggplot(timeStats, aes(x=isAssessment+0, y=mLnTime, color=early)) + 
   geom_line(position=position_dodge(width=0.2)) + 
   geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mLnTime-seLnTime, ymax=mLnTime+seLnTime)) +
   facet_wrap(~ problemGroup, scales = "free")
ggplot(timeStats, aes(x=isAssessment+0, y=pFirstCorrect, color=early)) + 
   geom_line() + 
   facet_wrap(~ problemGroup, scales = "free")

condCompare(log(byStudentWTime$timeRevising+1), byStudentWTime$early, filter=byStudentWTime$problem_id==176, test=t.test)
condCompare(byStudentWTime$timeRevising==0, byStudentWTime$early, filter=byStudentWTime$problem_id==176, test=fisher.test)

noIntervention <- byStudentWTime$user_id[byStudentWTime$problem_id == 172 & byStudentWTime$firstCorrect]


condCompare(byStudent39$nAttempts, byStudent39$early, filter=!byStudent39$highPK)
condCompare(byStudent39$nAttempts, byStudent39$early, filter=byStudent39$highPK)
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

P39Plot = ggplot(pkStats[pkStats$problem_id == 39,], aes(y=pFirstCorrect, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) + 
  theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
  theme(axis.title.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.ticks.y=element_blank()
      )
plot(P39Plot)

P59Plot = ggplot(pkStats[pkStats$problem_id == 59,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
  theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()
  )

#plot(P173Plot)
P180Plot = ggplot(pkStats[pkStats$problem_id == 180,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
  theme(legend.position = "none") + scale_x_discrete(labels = c("Late", "Early"))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
#plot(P174Plot)

P64Plot = ggplot(pkStats[pkStats$problem_id == 64,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
  theme(legend.position = "none") +  scale_x_discrete(labels = c("Late", "Early"))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
#plot(P175Plot)

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

figure = ggarrange(P39Plot, P59Plot, P180Plot, P64Plot, P56Plot, P62Plot,
          labels = c("1Practice", "2Practice", "1Assessment", "3Practice", "4Practice", "2Assessment"),
          ncol = 3, nrow = 2,
          common.legend = TRUE,
          font.label = list(size = 9, face = "bold", color ="blue"),
          vjust = 1)

annotate_figure(figure,
                bottom = text_grob("Average number of attempts", color = "blue", face = "bold", size = 10)
)
# End RQ2 =====

byStudentWTime$problemID = 1
byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 59, 2, byStudentWTime$problemID)
byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 180, 3, byStudentWTime$problemID)
byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 64, 4, byStudentWTime$problemID)
byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 56, 5, byStudentWTime$problemID)
byStudentWTime$problemID = ifelse(byStudentWTime$problem_id == 62, 6, byStudentWTime$problemID)

# Not considering the interaction, there's a significant of PK
anova(m1 <- lmer(nAttempts ~ exp + highPK + problem_id_nom + (1 | user_id), data=byStudentWTime[ byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59 |  byStudentWTime$problem_id==180 | byStudentWTime$problem_id==52 | byStudentWTime$problem_id==56 | byStudentWTime$problem_id==64 ,]), type="III")
#There is a significant interaction effect between experimental and high prior knowledge
anova(m2 <- lmer(nAttempts ~ exp * highPK + problem_id_nom + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id==39 | byStudentWTime$problem_id==59 |  byStudentWTime$problem_id==180 | byStudentWTime$problem_id==52 | byStudentWTime$problem_id==56 | byStudentWTime$problem_id==64 ,]), type="III")
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

byStudent59 <- byStudentWTime[byStudentWTime$problem_id == 59,]
condCompare((byStudent59$pCorrect == 1) + 0, byStudent59$highPK, filter=byStudent59$early)
condCompare((byStudent59$pCorrect == 1) + 0, byStudent59$highPK, filter=!byStudent59$early)

byStudent180 <- byStudentWTime[byStudentWTime$problem_id == 180,]
condCompare((byStudent180$pCorrect == 1) + 0, byStudent180$highPK, filter=byStudent180$early)
condCompare((byStudent180$pCorrect == 1) + 0, byStudent180$highPK, filter=!byStudent180$early)

byStudent56 <- byStudentWTime[byStudentWTime$problem_id == 56,]
condCompare((byStudent56$pCorrect == 1) + 0, byStudent56$highPK, filter=byStudent56$early)
condCompare((byStudent56$pCorrect == 1) + 0, byStudent56$highPK, filter=!byStudent56$early)

byStudent64 <- byStudentWTime[byStudentWTime$problem_id == 64,]
condCompare((byStudent64$pCorrect == 1) + 0, byStudent64$highPK, filter=byStudent64$early)
condCompare((byStudent64$pCorrect == 1) + 0, byStudent64$highPK, filter=!byStudent64$early)

byStudent62 <- byStudentWTime[byStudentWTime$problem_id == 62,]
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

#plot(P173Plot)
P180Plot2 = ggplot(pkStats[pkStats$problem_id == 180,], aes(y=pFirstCorrect, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
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

figure = ggarrange(P39Plot2, P59Plot2, P180Plot2, P64Plot2, P56Plot2, P62Plot2,
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
Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 180,]), type="III")
Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 64,]), type="III")
Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 56,]), type="III")
Anova(lm(nAttempts ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 62,]), type="III")

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
      mBestScore = mean(userAttempts$bestScore)
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





