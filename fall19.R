source("util.R")
library(plyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ggpubr)



allAttempts <- read.csv("data/pyandexpt.csv")
consenters <- read.csv("data/consenters.csv")
survey <- read.csv("data/survey.csv") # survey data can be linked to attempts data by : anonId and hashed_id

attempts <- allAttempts[allAttempts$quest_id == 49,]
attempts <- attempts[attempts$user_id %in% consenters$student_id,]

#if feedback = null this means they were not assigned to a condition
attempts$had_feedback <- attempts$feedback_text != ""
attempts$had_hints <- ifelse(attempts$had_feedback, grepl("'showHints': True", attempts$feedback_text, fixed=T), NA)
attempts$reverse <- ifelse(attempts$had_feedback, grepl("'reversedShow': True", attempts$feedback_text, fixed=T), NA)
attempts$early <- ifelse(attempts$had_feedback, attempts$had_hints == attempts$reverse, NA)
attempts$correct <- attempts$score == attempts$max_score
attempts$suggest <- ifelse(attempts$had_hints, grepl("'suggest': True", attempts$feedback_text, fixed=T), NA)
attempts$hLevelInt <- ifelse(attempts$had_hints, grepl("'highlightLevelInt': 2", attempts$feedback_text, fixed=T), NA)
attempts$showMissing <- ifelse(attempts$had_hints, grepl("'showMissing': True", attempts$feedback_text, fixed=T), NA)

## calculate all parameters for each problem for each student
attempts = attempts[order(attempts$user_id), ]
attempts$timestamp <-as.POSIXct(attempts$timestamp, format="%Y-%m-%d %H:%M:%S")
attempts = attempts[order(attempts$timestamp), ] # looks like students were doing the attempts in 2-4 minutes
problem1_attempts <- estimateParameters(attempts, 172)
problem1_attempts = problem1_attempts[order(problem1_attempts$user_id), ]
problem2_attempts <- estimateParameters(attempts, 173)
problem3_attempts <- estimateParameters(attempts, 174)
problem4_attempts <- estimateParameters(attempts, 175)
problem5_attempts <- estimateParameters(attempts, 176)
problem6_attempts <- estimateParameters(attempts, 177)
problem7_attempts <- estimateParameters(attempts, 178)
problem8_attempts <- estimateParameters(attempts, 179)
attemptsTime = rbind(problem1_attempts, problem2_attempts, problem3_attempts, problem4_attempts, problem5_attempts, problem6_attempts, problem7_attempts, problem8_attempts)

# add hints factors to students dataframe since the hint factor should be for the student, not for specific problem. We already know what are the problems with hints.
students <- ddply(attempts, "user_id", summarize,
                  anonID = first(hashed_id),
                  n = length(had_feedback),
                  pFeedback = mean(had_feedback),
                  pHints = mean(had_hints[had_feedback]),
                  early = if (pFeedback == 0) NA else any(early, na.rm=T),
                  suggest = any(suggest, na.rm=TRUE),
                  hLevelInt = any(hLevelInt, na.rm=TRUE),
                  showMissing = any(showMissing, na.rm=TRUE)
                  )

mean(students$pFeedback > 0)
# 119 students in the Early condition, and 124 in the False
table(students$early)

byStudentWTime <- merge(students, attemptsTime)
byStudentWTime <- byStudentWTime[!is.na(byStudentWTime$early),]
ggplot(byStudentWTime, aes(y=nAttempts, x=early)) + geom_boxplot() + geom_violin(width=0.2) + facet_wrap(~ problem_id)

ggplot(byStudentWTime, aes(timeRevising)) + geom_histogram() + facet_wrap(~ problem_id) + scale_x_continuous(limits=c(0,10))
ggplot(byStudentWTime, aes(log(timeRevising+1))) + geom_histogram() + facet_wrap(~ problem_id) + scale_x_continuous(limits=c(0,10))

timeStats <- ddply(byStudentWTime, c("problem_id", "early"), summarize, 
                   mTime=mean(timeRevising), medTime=median(timeRevising), seTime=se(timeRevising),
                   mLnTime=mean(log(timeRevising+1)), seLnTime=se(log(timeRevising+1)),
                   pFirstCorrect=mean(timeRevising==0))
timeStats

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


# pFirst correct means 1/number of attempts , so the smaller the higher the attempts they did
ddply(byStudentWTime, c("problem_id", "early"), summarize, pFirstCorrect=mean(nAttempts == 1), n=length(nAttempts), meanTime = mean(timeRevising))


ddply(attempts, c("problem_id"), summarize, 
      pFeedback = mean(feedback_text == ""), 
      pCorrect = mean(correct),
      pNoFBUncomp = mean(feedback_text == "" & score == 0) / pFeedback,
      pHints = mean(grepl("'showHints': True", feedback_text, fixed=T)) / (1-pFeedback))


#ddply(attempts, c("user_id"), summarize, pLogging = mean(feedback_text == ""))

# remove student who got the first problem right on their first try
studentsP1 = byStudentWTime[byStudentWTime$problem_id==172, ]
studentsP1 = studentsP1[order(studentsP1$user_id), ]
# 240 rows

# note: I can't just remove students who had 1 attempt because I found some students got it right at the first attempt and took another attempt but make it false.
students_correct_firstTry = studentsP1[studentsP1$pCorrect==1, ] 
#108 students got the first problem correct on the first try
(length(unique(students_correct_firstTry$user_id)))
byStudent_2 = byStudentWTime[!byStudentWTime$user_id %in% students_correct_firstTry$user_id, ]
byStudent_2 = byStudent_2[order(byStudent_2$user_id), ]
#just to verify
length(byStudent_2$user_id[byStudent_2$pCorrect==1 & byStudent_2$problem_id==172])
ggplot(byStudent_2, aes(y=nAttempts, x=early)) + geom_boxplot() + geom_violin(width=0.2) + facet_wrap(~ problem_id)
byStudent_22 = ddply(byStudent_2, c("problem_id", "early"), summarize, pFirstCorrect=mean(nAttempts == 1), n=length(nAttempts))

# students attempts for only the first 4 problems, before the late group takes hints
byStudent_2_P1_4 = byStudent_2[byStudent_2$problem_id==172 | byStudent_2$problem_id==173 | byStudent_2$problem_id==174 | byStudent_2$problem_id==175, ]
ggplot(byStudent_2_P1_4, aes(y=nAttempts, x=early)) + geom_boxplot() + geom_violin(width=0.2) + facet_wrap(~ problem_id)

#####################################
### differences for every problem pair
#####################################
## Problem 172
byStudent_2_172 = byStudent_2[byStudent_2$problem_id==172, ]
condCompare(byStudent_2_172$pCorrect, byStudent_2_172$early)
condCompare(byStudent_2_172$nInCorrect, byStudent_2_172$early)
condCompare(byStudent_2_172$timeRevising, byStudent_2_172$early)
# Early condition = 64 students
length(unique(byStudent_2_172$user_id[byStudent_2_172$early==TRUE]))

#Problem 173
byStudent_2_173 = byStudent_2[byStudent_2$problem_id==173, ]
condCompare(byStudent_2_173$pCorrect, byStudent_2_173$early)
condCompare(byStudent_2_173$pFirstCorrect, byStudent_2_173$early)
condCompare(byStudent_2_173$nInCorrect, byStudent_2_173$early)
condCompare(byStudent_2_173$timeRevising, byStudent_2_173$early)
# Early condition = 63 students
length(unique(byStudent_2_173$user_id[byStudent_2_173$early==TRUE]))

#Problem 174
byStudent_2_174 = byStudent_2[byStudent_2$problem_id==174, ]
condCompare(byStudent_2_174$pCorrect, byStudent_2_174$early)
condCompare(byStudent_2_174$pFirstCorrect, byStudent_2_174$early)
condCompare(byStudent_2_174$nInCorrect, byStudent_2_174$early)
condCompare(byStudent_2_174$timeRevising, byStudent_2_174$early)
# Early condition = 63 students
length(unique(byStudent_2_174$user_id[byStudent_2_174$early==TRUE]))

#Problem 175
byStudent_2_175 = byStudent_2[byStudent_2$problem_id==175, ]
condCompare(byStudent_2_175$pCorrect, byStudent_2_175$early)
condCompare(byStudent_2_175$pFirstCorrect, byStudent_2_175$early)
fisher.test(byStudent_2_175$pFirstCorrect, byStudent_2_175$early)
condCompare(byStudent_2_175$nInCorrect, byStudent_2_175$early)
condCompare(byStudent_2_175$timeRevising, byStudent_2_175$early)
# Early condition = 58 students
length(unique(byStudent_2_175$user_id[byStudent_2_175$early==TRUE]))

#Problem 176
byStudent_2_176 = byStudent_2[byStudent_2$problem_id==176, ]
condCompare(byStudent_2_176$pCorrect, byStudent_2_176$early)
condCompare(byStudent_2_176$nAttempts, byStudent_2_176$early) #marginal significance
condCompare(byStudent_2_176$nInCorrect, byStudent_2_176$early)
condCompare(byStudent_2_176$timeRevising, byStudent_2_176$early) #marginal significance
fisher.test(byStudent_2_176$pFirstCorrect, byStudent_2_176$early)
# Early condition = 56 students, marginal significance, 
#this shows evidence that students who received hints spent significantly more time revising attempts
length(unique(byStudent_2_176$user_id[byStudent_2_176$early==TRUE]))

#Problem 177
byStudent_2_177 = byStudent_2[byStudent_2$problem_id==177, ]
condCompare(byStudent_2_177$nAttempts, byStudent_2_177$early)
condCompare(byStudent_2_177$pCorrect, byStudent_2_177$early)
condCompare(byStudent_2_177$pFirstCorrect, byStudent_2_177$early)
condCompare(byStudent_2_177$nInCorrect, byStudent_2_177$early)
condCompare(byStudent_2_177$timeRevising, byStudent_2_177$early)
# Early condition = 56 students
length(unique(byStudent_2_177$user_id[byStudent_2_177$early==TRUE]))

# students who attempted problems 5,6,7,8
byStudentsP5_8 = byStudent_2[byStudent_2$problem_id==176 |  byStudent_2$problem_id==177 | byStudent_2$problem_id==178 | byStudent_2$problem_id==179, ] 
## get students who attempted problem 176
studentsP5 = byStudentsP5_8[byStudentsP5_8$problem_id==176, ]
studentsP5_2_firstTry = studentsP5[studentsP5$pCorrect==1, ]
#28 students got the first problem correct on the first try
(length(unique(studentsP5_2_firstTry$user_id)))

#then remove these 28 students as well from the data but only from the last 4 problems
#734 rows. byStudent_3 is without students who got the first try right and those on the late condition who got the first attempt of problem 5 right
byStudent_3 = byStudent_2[!byStudent_2$user_id %in% studentsP5_2_firstTry$user_id, ]
byStudent_2_P5_8 = byStudent_3[byStudent_3$problem_id==176 | byStudent_3$problem_id==177 | byStudent_3$problem_id==178 | byStudent_3$problem_id==179, ]

ggplot(byStudent_2_P5_8, aes(y=nAttempts, x=early)) + geom_boxplot() + geom_violin(width=0.2) + facet_wrap(~ problem_id)
byStudent_23 = ddply(byStudent_2_P5_8, c("problem_id", "early"), summarize, pFirstCorrect=mean(nAttempts == 1), n=length(nAttempts))

# Then find differences between problems 
# problem 176
byStudent_2_P176 = byStudent_2_P5_8[byStudent_2_P5_8$problem_id==176, ]
condCompare(byStudent_2_P176$pCorrect, byStudent_2_P176$early)
# Early condition = 40
(length(unique(byStudent_2_P176$user_id[byStudent_2_P176$early==TRUE])))
condCompare(byStudent_2_P176$pFirstCorrect, byStudent_2_P176$early)
condCompare(byStudent_2_P176$nInCorrect, byStudent_2_P176$early)

# problem 177
byStudent_2_P177 = byStudent_2_P5_8[byStudent_2_P5_8$problem_id==177, ]
condCompare(byStudent_2_P177$pCorrect, byStudent_2_P177$early)
# Early condition = 40
(length(unique(byStudent_2_P177$user_id[byStudent_2_P177$early==TRUE])))
condCompare(byStudent_2_P177$pFirstCorrect, byStudent_2_P177$early)
condCompare(byStudent_2_P177$nInCorrect, byStudent_2_P177$early)

# problem 178
byStudent_2_P178 = byStudent_2_P5_8[byStudent_2_P5_8$problem_id==178, ]
condCompare(byStudent_2_P178$pCorrect, byStudent_2_P178$early)
# Early condition = 35
(length(unique(byStudent_2_P178$user_id[byStudent_2_P178$early==TRUE])))
condCompare(byStudent_2_P178$pFirstCorrect, byStudent_2_P178$early)

# problem 179
byStudent_2_P179 = byStudent_2_P5_8[byStudent_2_P5_8$problem_id==179, ]
condCompare(byStudent_2_P179$pCorrect, byStudent_2_P179$early)
# Early condition = 40
(length(unique(byStudent_2_P179$user_id[byStudent_2_P179$early==TRUE])))
condCompare(byStudent_2_P179$pFirstCorrect, byStudent_2_P179$early)

###########################################################
### prepare dataframe for linear model (isHints, priorProblemsHints, problemsSinceLastHint)
##########################################################

lm_byStudent = byStudent_2
lm_byStudent$problem_id = as.character(lm_byStudent$problem_id)

## does the problem has hints or not. 1 for yes, and 0 for no
lm_byStudent$isHints = 0
lm_byStudent$isHints = ifelse(lm_byStudent$early==TRUE & (lm_byStudent$problem_id==172 | lm_byStudent$problem_id==173), 1, lm_byStudent$isHints)
lm_byStudent$isHints = ifelse(lm_byStudent$early==FALSE & (lm_byStudent$problem_id==176 | lm_byStudent$problem_id==177), 1, lm_byStudent$isHints)

## How many prior problems have you had hints
lm_byStudent$priorProblemsHints = 0
lm_byStudent$priorProblemsHints = ifelse(lm_byStudent$early==TRUE & lm_byStudent$problem_id==173, 1, lm_byStudent$priorProblemsHints)
lm_byStudent$priorProblemsHints = ifelse(lm_byStudent$early==TRUE & !(lm_byStudent$problem_id==172 | lm_byStudent$problem_id==173), 2, lm_byStudent$priorProblemsHints)
lm_byStudent$priorProblemsHints = ifelse(lm_byStudent$early==FALSE & lm_byStudent$problem_id==177, 1, lm_byStudent$priorProblemsHints)
lm_byStudent$priorProblemsHints = ifelse(lm_byStudent$early==FALSE & (lm_byStudent$problem_id==178 | lm_byStudent$problem_id==179), 2, lm_byStudent$priorProblemsHints)

## how many problems since the last hint
lm_byStudent$problemsSinceLastHint = 0
lm_byStudent$problemsSinceLastHint = ifelse(lm_byStudent$early==TRUE & (lm_byStudent$problem_id==173 | lm_byStudent$problem_id==174), 1, lm_byStudent$problemsSinceLastHint)
lm_byStudent$problemsSinceLastHint = ifelse(lm_byStudent$early==TRUE & lm_byStudent$problem_id==175, 2, lm_byStudent$problemsSinceLastHint)
lm_byStudent$problemsSinceLastHint = ifelse(lm_byStudent$early==TRUE & lm_byStudent$problem_id==176, 3, lm_byStudent$problemsSinceLastHint)
lm_byStudent$problemsSinceLastHint = ifelse(lm_byStudent$early==TRUE & lm_byStudent$problem_id==177, 4, lm_byStudent$problemsSinceLastHint)
lm_byStudent$problemsSinceLastHint = ifelse(lm_byStudent$early==TRUE & lm_byStudent$problem_id==178, 5, lm_byStudent$problemsSinceLastHint)
lm_byStudent$problemsSinceLastHint = ifelse(lm_byStudent$early==TRUE & lm_byStudent$problem_id==179, 6, lm_byStudent$problemsSinceLastHint)

lm_byStudent$problemsSinceLastHint = ifelse(lm_byStudent$early==FALSE & (lm_byStudent$problem_id==177 | lm_byStudent$problem_id==178), 1, lm_byStudent$problemsSinceLastHint)
lm_byStudent$problemsSinceLastHint = ifelse(lm_byStudent$early==FALSE & lm_byStudent$problem_id==179, 2, lm_byStudent$problemsSinceLastHint)

lm_byStudent$earlyCond = ifelse(lm_byStudent$early==TRUE, 1, 0)

######################
## rating the problems difficulty instead of using 8 problems in the model
####################
lm_byStudent$problem_id_ranked = "0"
lm_byStudent$problem_id_ranked = ifelse((lm_byStudent$problem_id=="172" | lm_byStudent$problem_id=="174"), "1", lm_byStudent$problem_id_ranked)
lm_byStudent$problem_id_ranked = ifelse((lm_byStudent$problem_id=="173" | lm_byStudent$problem_id=="175"), "2", lm_byStudent$problem_id_ranked)

lm_byStudent$problem_id_ranked = ifelse((lm_byStudent$problem_id=="176" | lm_byStudent$problem_id=="178"), "3", lm_byStudent$problem_id_ranked)
lm_byStudent$problem_id_ranked = ifelse((lm_byStudent$problem_id=="177" | lm_byStudent$problem_id=="179"), "4", lm_byStudent$problem_id_ranked)

#### start the models#####
#########################
model13 = lmer(formula = nAttempts ~ earlyCond + problem_id_ranked + isHints + problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model13)

model5 = lmer(formula = timeRevising ~ earlyCond + problem_id_ranked + isHints + problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model5)

model6 = lmer(formula = timeRevising ~ problem_id_ranked + isHints + priorProblemsHints + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model6)

model8 = lmer(formula = timeRevising ~ problem_id_ranked + isHints + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model8)

model9 = lmer(formula = timeRevising ~ isHints + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model9)

model14 = lmer(formula = nAttempts ~ earlyCond + problem_id_ranked + isHints + problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model14)


#####################################################################
##### analyzing hints factors: suggest, showMissing, highlightLevelInt
######################################################################
#prepare data for the model
lm_byStudent$suggest2 = ifelse(lm_byStudent$suggest==TRUE, 1, 0)
lm_byStudent$hLevelInt2= ifelse(lm_byStudent$hLevelInt==TRUE, 1, 0)
lm_byStudent$showMissing2 = ifelse(lm_byStudent$showMissing==TRUE, 1, 0)

modelFAll = lmer(formula = nAttempts ~ suggest2 + hLevelInt2 + showMissing2 + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelFAll)

modelFAll3 = lmer(formula = nInCorrect ~ suggest2 + hLevelInt2 + showMissing2 + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelFAll3)

modelFAll3 = lmer(formula = timeRevising ~ suggest2 + hLevelInt2 + showMissing2 + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelFAll3)

modelFAll2 = lmer(formula = timeRevising ~ suggest2 + hLevelInt2 + showMissing2 + problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelFAll2)

modelFAll2 = lmer(formula = nInCorrect ~ suggest2 + hLevelInt2 + showMissing2 + problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelFAll2)

modelFAll3 = lmer(formula = timeRevising ~ suggest2 + hLevelInt2 + showMissing2 + problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelFAll3)

modelF2 = lmer(formula = nInCorrect ~ suggest2 + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelF2)

modelF2 = lmer(formula = timeRevising ~ suggest2 + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelF2)

modelF3 = lmer(formula = timeRevising ~ hLevelInt2 + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelF3)

modelF4 = lmer(formula = timeRevising ~ showMissing2 + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelF4)

modelF5 = lmer(formula = nInCorrect ~ suggest2 + problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelF5)

modelF6 = lmer(formula = nInCorrect ~ problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelF6)

# non of these are significant
lm_byStudent_172_2 = lm_byStudent[lm_byStudent$problem_id==172, ]
condCompare(lm_byStudent_172_2$nAttempts, lm_byStudent_172_2$suggest)
condCompare(lm_byStudent_172_2$nAttempts, lm_byStudent_172_2$showMissing)
condCompare(lm_byStudent_172_2$nAttempts, lm_byStudent_172_2$hLevelInt)

#not significant
condCompare(lm_byStudent_172_2$timeRevising, lm_byStudent_172_2$suggest)
condCompare(lm_byStudent_172_2$timeRevising, lm_byStudent_172_2$showMissing)
condCompare(lm_byStudent_172_2$timeRevising, lm_byStudent_172_2$hLevelInt)

lm_byStudent_173_2 = lm_byStudent[lm_byStudent$problem_id==173, ]
condCompare(lm_byStudent_173_2$nAttempts, lm_byStudent_173_2$suggest)
condCompare(lm_byStudent_173_2$nAttempts, lm_byStudent_173_2$showMissing)
condCompare(lm_byStudent_173_2$nAttempts, lm_byStudent_173_2$hLevelInt)

#not significant
condCompare(lm_byStudent_173_2$timeRevising, lm_byStudent_173_2$suggest)
condCompare(lm_byStudent_173_2$timeRevising, lm_byStudent_173_2$showMissing)
condCompare(lm_byStudent_173_2$timeRevising, lm_byStudent_173_2$hLevelInt)

lm_byStudent_175_2 = lm_byStudent[lm_byStudent$problem_id==175, ]
condCompare(lm_byStudent_175_2$nAttempts, lm_byStudent_175_2$suggest)
condCompare(lm_byStudent_175_2$nAttempts, lm_byStudent_175_2$showMissing)
condCompare(lm_byStudent_175_2$nAttempts, lm_byStudent_175_2$hLevelInt) #marginal significance

#not significant
condCompare(lm_byStudent_175_2$timeRevising, lm_byStudent_175_2$suggest)
condCompare(lm_byStudent_175_2$timeRevising, lm_byStudent_175_2$showMissing)
condCompare(lm_byStudent_175_2$timeRevising, lm_byStudent_175_2$hLevelInt) #marginal significance

lm_byStudent_176_2 = lm_byStudent[lm_byStudent$problem_id==176, ]
condCompare(lm_byStudent_176_2$nAttempts, lm_byStudent_176_2$suggest)
condCompare(lm_byStudent_176_2$nAttempts, lm_byStudent_176_2$showMissing)
condCompare(lm_byStudent_176_2$nAttempts, lm_byStudent_176_2$hLevelInt)

#not significant
condCompare(lm_byStudent_176_2$timeRevising, lm_byStudent_176_2$suggest)
condCompare(lm_byStudent_176_2$timeRevising, lm_byStudent_176_2$showMissing)
condCompare(lm_byStudent_176_2$timeRevising, lm_byStudent_176_2$hLevelInt)

#### create a model for each problem
lm_byStudent_179 = lm_byStudent[lm_byStudent$problem_id=="179", ]
# early: 52, late: 52 . x: suggest = TRUE
length(unique(lm_byStudent_179$user_id[lm_byStudent_179$suggest==TRUE]))
condCompare(lm_byStudent_179$nAttempts, lm_byStudent_179$suggest) #significant
condCompare(lm_byStudent_179$nAttempts, lm_byStudent_179$showMissing)
condCompare(lm_byStudent_179$nAttempts, lm_byStudent_179$hLevelInt)
condCompare(lm_byStudent_179$pFirstCorrect, lm_byStudent_179$suggest) #significant
condCompare(lm_byStudent_179$nInCorrect, lm_byStudent_179$suggest) #significant

####################
####### paper plots
###################

earlyCondModel = ddply(lm_byStudent, c("problem_id", "early"), summarise, avgRTime = mean(timeRevising), n= length(user_id))
ggplot(earlyCondModel, aes(x=problem_id, y=avgRTime, group= early, color= early )) + geom_line()

#################################
## split the 2x2x2 hint conditions S: for suggest, M: for showMissing, H: for highlight
####################################
lm_byStudent$hintFactor = 0
lm_byStudent$hintFactor = ifelse(lm_byStudent$suggest==TRUE & lm_byStudent$showMissing==TRUE & lm_byStudent$hLevelInt==TRUE, "SMH", lm_byStudent$hintFactor)
lm_byStudent$hintFactor = ifelse(lm_byStudent$suggest==TRUE & lm_byStudent$showMissing==TRUE & lm_byStudent$hLevelInt==FALSE, "SM", lm_byStudent$hintFactor)
lm_byStudent$hintFactor = ifelse(lm_byStudent$suggest==TRUE & lm_byStudent$showMissing==FALSE & lm_byStudent$hLevelInt==TRUE, "SH", lm_byStudent$hintFactor)
lm_byStudent$hintFactor = ifelse(lm_byStudent$suggest==TRUE & lm_byStudent$showMissing==FALSE & lm_byStudent$hLevelInt==FALSE, "S", lm_byStudent$hintFactor)
lm_byStudent$hintFactor = ifelse(lm_byStudent$suggest==FALSE & lm_byStudent$showMissing==TRUE & lm_byStudent$hLevelInt==TRUE, "MH", lm_byStudent$hintFactor)
lm_byStudent$hintFactor = ifelse(lm_byStudent$suggest==FALSE & lm_byStudent$showMissing==TRUE & lm_byStudent$hLevelInt==FALSE, "M", lm_byStudent$hintFactor)
lm_byStudent$hintFactor = ifelse(lm_byStudent$suggest==FALSE & lm_byStudent$showMissing==FALSE & lm_byStudent$hLevelInt==TRUE, "H", lm_byStudent$hintFactor)
lm_byStudent$hintFactor = ifelse(lm_byStudent$suggest==FALSE & lm_byStudent$showMissing==FALSE & lm_byStudent$hLevelInt==FALSE, "Ctrl", lm_byStudent$hintFactor)

## Just to verify that students do not overlap accross hint conditions
studentsFactor1 = lm_byStudent[lm_byStudent$hintFactor=="SMH", ]
studentsFactor2 = lm_byStudent[lm_byStudent$hintFactor=="SM", ]
studentsFactor3 = lm_byStudent[lm_byStudent$hintFactor=="SH", ]
studentsFactor4 = lm_byStudent[lm_byStudent$hintFactor=="S", ]
studentsFactor5 = lm_byStudent[lm_byStudent$hintFactor=="MH", ]
studentsFactor6 = lm_byStudent[lm_byStudent$hintFactor=="M", ]
studentsFactor7 = lm_byStudent[lm_byStudent$hintFactor=="H", ]
studentsFactor8 = lm_byStudent[lm_byStudent$hintFactor=="Ctrl", ]
allStudentsWAllFactors = rbind(studentsFactor1, studentsFactor2, studentsFactor3, studentsFactor4, studentsFactor5, studentsFactor6, studentsFactor7, studentsFactor8)
################################################################

### Make a new dataframe ############
hintCondModel = ddply(lm_byStudent, c("problem_id", "hintFactor"), summarise, avgRTime = mean(timeRevising), n= length(user_id))
ggplot(hintCondModel, aes(x=problem_id, y=avgRTime, group= hintFactor, color= hintFactor )) + geom_line()

#### prepare a dataframe for Suggest/Early
##########################################
lm_byStudent$suggestEarly = 0
lm_byStudent$suggestEarly = ifelse(lm_byStudent$suggest==TRUE & lm_byStudent$early==TRUE, "suggestEarly", lm_byStudent$suggestEarly)
lm_byStudent$suggestEarly = ifelse(lm_byStudent$suggest==TRUE & lm_byStudent$early==FALSE, "suggestLate", lm_byStudent$suggestEarly)
lm_byStudent$suggestEarly = ifelse(lm_byStudent$suggest==FALSE & lm_byStudent$early==TRUE, "!suggestEarly", lm_byStudent$suggestEarly)
lm_byStudent$suggestEarly = ifelse(lm_byStudent$suggest==FALSE & lm_byStudent$early==FALSE, "!suggestLate", lm_byStudent$suggestEarly)

hintSuggestModel = ddply(lm_byStudent, c("problem_id", "suggestEarly"), summarise, avgRTime = mean(timeRevising), n= length(user_id))
ggplot(hintSuggestModel, aes(x=problem_id, y=avgRTime, group= suggestEarly, color= suggestEarly )) + geom_line()

#### prepare a dataframe for ShowMissing/Early
##########################################
lm_byStudent$showMissingEarly = 0
lm_byStudent$showMissingEarly = ifelse(lm_byStudent$showMissing==TRUE & lm_byStudent$early==TRUE, "shMissingEarly", lm_byStudent$showMissingEarly)
lm_byStudent$showMissingEarly = ifelse(lm_byStudent$showMissing==TRUE & lm_byStudent$early==FALSE, "shMissingLate", lm_byStudent$showMissingEarly)
lm_byStudent$showMissingEarly = ifelse(lm_byStudent$showMissing==FALSE & lm_byStudent$early==TRUE, "!shMissingEarly", lm_byStudent$showMissingEarly)
lm_byStudent$showMissingEarly = ifelse(lm_byStudent$showMissing==FALSE & lm_byStudent$early==FALSE, "!shMissingLate", lm_byStudent$showMissingEarly)

hintshowMissingModel = ddply(lm_byStudent, c("problem_id", "showMissingEarly"), summarise, avgRTime = mean(timeRevising), n= length(user_id))
ggplot(hintshowMissingModel, aes(x=problem_id, y=avgRTime, group= showMissingEarly, color= showMissingEarly )) + geom_line()

#### prepare a dataframe for Highlight/Early
##########################################
lm_byStudent$highlightEarly = 0
lm_byStudent$highlightEarly = ifelse(lm_byStudent$hLevelInt==TRUE & lm_byStudent$early==TRUE, "highlightEarly", lm_byStudent$highlightEarly)
lm_byStudent$highlightEarly = ifelse(lm_byStudent$hLevelInt==TRUE & lm_byStudent$early==FALSE, "highlightLate", lm_byStudent$highlightEarly)
lm_byStudent$highlightEarly = ifelse(lm_byStudent$hLevelInt==FALSE & lm_byStudent$early==TRUE, "!highlightEarly", lm_byStudent$highlightEarly)
lm_byStudent$highlightEarly = ifelse(lm_byStudent$hLevelInt==FALSE & lm_byStudent$early==FALSE, "!highlightLate", lm_byStudent$highlightEarly)

hintHighlightgModel = ddply(lm_byStudent, c("problem_id", "highlightEarly"), summarise, avgRTime = mean(timeRevising), n= length(user_id))
ggplot(hintHighlightgModel, aes(x=problem_id, y=avgRTime, group= highlightEarly, color= highlightEarly )) + geom_line()


### performance/learning plots#########
##########################################
lm_byStudent_Prob172_174 = lm_byStudent[lm_byStudent$problem_id=="172" | lm_byStudent$problem_id=="174", ]
lm_byStudent_Prob172_174_Early = ddply(lm_byStudent_Prob172_174, c("problem_id", "early"), summarise, avgRTime = mean(timeRevising), n = length(user_id))
pair1 = ggplot(lm_byStudent_Prob172_174_Early, aes(x=problem_id, y=avgRTime, group= early, color= early)) + geom_line() + 
  scale_x_discrete(labels=c("Performance", "Learning")) + xlab("172-174") 

lm_byStudent_Prob173_175 = lm_byStudent[lm_byStudent$problem_id=="173" | lm_byStudent$problem_id=="175", ]
lm_byStudent_Prob173_175_Early = ddply(lm_byStudent_Prob173_175, c("problem_id", "early"), summarise, avgRTime = mean(timeRevising), n = length(user_id))
pair2 = ggplot(lm_byStudent_Prob173_175_Early, aes(x=problem_id, y=avgRTime, group= early, color= early)) + geom_line() + 
  scale_x_discrete(labels=c("Performance", "Learning")) + xlab("173-175") 

lm_byStudent_Prob176_178 = lm_byStudent[lm_byStudent$problem_id=="176" | lm_byStudent$problem_id=="178", ]
lm_byStudent_Prob176_178_Early = ddply(lm_byStudent_Prob176_178, c("problem_id", "early"), summarise, avgRTime = mean(timeRevising), n = length(user_id))
pair3 = ggplot(lm_byStudent_Prob176_178_Early, aes(x=problem_id, y=avgRTime, group= early, color= early)) + geom_line() + 
  scale_x_discrete(labels=c("Performance", "Learning")) + xlab("176-178") 

lm_byStudent_Prob177_179 = lm_byStudent[lm_byStudent$problem_id=="177" | lm_byStudent$problem_id=="179", ]
lm_byStudent_Prob177_179_Early = ddply(lm_byStudent_Prob177_179, c("problem_id", "early"), summarise, avgRTime = mean(timeRevising), n = length(user_id))
pair4 = ggplot(lm_byStudent_Prob177_179_Early, aes(x=problem_id, y=avgRTime, group= early, color= early)) + geom_line() + 
  scale_x_discrete(labels=c("Performance", "Learning")) + xlab("177-179") 

ggarrange(pair1, pair2, pair3, pair4,
          labels = c("1", "2", "3", "4"),
          ncol = 2, nrow = 2,
          common.legend = TRUE)

############################################
####### survey data ####################
########################################
# 1- filter data
surveyFiltered = survey[survey$anonid %in% lm_byStudent$anonID, ]
length(unique(lm_byStudent$user_id))
surveyFiltered$anonID = surveyFiltered$anonid
lm_byStudent_WSurvey = lm_byStudent[lm_byStudent$anonID %in% surveyFiltered$anonid, ]
lm_byStudent_WSurvey = merge(lm_byStudent_WSurvey, surveyFiltered, by = "anonID")

## only 98 students from those who exists in the attempts have taken the survey. also there is no duplicates

#Q186: Rate the amount of prior programming experience from 1 to 9
# From the results below, looks like it has no interaction effect
summary(lm_byStudent_WSurvey$Q186)
lm_byStudent_WSurvey$Q186_2 = ifelse(is.na(lm_byStudent_WSurvey$Q186), -1, lm_byStudent_WSurvey$Q186) # those who said NA, I gave them rating of -1
summary(lm_byStudent_WSurvey$Q186_2)
table(lm_byStudent_WSurvey$Q186_2)
lm_byStudent_WSurvey_PK = lm_byStudent_WSurvey[lm_byStudent_WSurvey$Q186_2>-1, ]
summary(lm_byStudent_WSurvey_PK$Q186_2)
lm_byStudent_WSurvey_PK$priorKn = ifelse(lm_byStudent_WSurvey_PK$Q186_2>5, 1, 0)
modelS1 = lmer(formula = timeRevising ~  priorKn + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
summary(modelS1)

modelS2 = lmer(formula = timeRevising ~  priorKn + suggest2 + showMissing2 + hLevelInt2 + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
summary(modelS2)

modelS3 = lmer(formula = timeRevising ~  priorKn + problem_id_ranked + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
summary(modelS3)

modelS4 = lmer(formula = timeRevising ~  priorKn + problem_id_ranked + isHints + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
summary(modelS4)

modelS5 = lmer(formula = nInCorrect ~  priorKn + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
summary(modelS5)

modelS6 = lmer(formula = nAttempts ~  priorKn + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
summary(modelS6)

# Q184: how well do you want to do in this course
# the results show it has no effect
lm_byStudent_WSurvey$motiv = 0
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="I want to be the best student in this class", 14, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="I want to be one of top 5 students in this class", 13, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="A or better", 12, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="A- or better", 11, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="B+ or better", 10, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="B or better", 9, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="B- or better", 8, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="C+ or better", 7, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="C or better", 6, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="C- or better", 5, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="D+ or better", 4, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="D or better", 3, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="D- or better", 2, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv = ifelse(lm_byStudent_WSurvey$Q184=="I really don't care", 1, lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv

summary(lm_byStudent_WSurvey$motiv) #median is 11
hist(lm_byStudent_WSurvey$motiv)
lm_byStudent_WSurvey$motiv2 = ifelse(lm_byStudent_WSurvey$motiv>11, 1, 0)
lm_byStudent_WSurvey_Motiv = lm_byStudent_WSurvey[lm_byStudent_WSurvey$motiv>0, ]
modelM1 = lmer(formula = timeRevising ~  motiv2 + (1 | user_id), data = lm_byStudent_WSurvey_Motiv, REML=FALSE)
summary(modelM1)

modelM2 = lmer(formula = timeRevising ~ motiv2 +suggest2 + showMissing2 + hLevelInt2 + (1 | user_id), data = lm_byStudent_WSurvey_Motiv, REML=FALSE)
summary(modelM2)

modelM3 = lmer(formula = timeRevising ~  priorKn + motiv2+ problem_id_ranked + (1 | user_id), data = lm_byStudent_WSurvey_Motiv, REML=FALSE)
summary(modelM3)

modelM4 = lmer(formula = nAttempts ~  motiv2 + (1 | user_id), data = lm_byStudent_WSurvey_Motiv, REML=FALSE)
summary(modelM4)

modelM5 = lmer(formula = nInCorrect ~  motiv2 + (1 | user_id), data = lm_byStudent_WSurvey_Motiv, REML=FALSE)
summary(modelM5)

modelM6 = lmer(formula = nInCorrect ~  priorKn + motiv2 + (1 | user_id), data = lm_byStudent_WSurvey_Motiv, REML=FALSE)
summary(modelM6)

#################################################################
## Q180: If there were more problems on PCRS I would be very likely to do them
#There is marginal significance in some models######
################################################################
lm_byStudent_WSurvey$Q180_2 = 0
lm_byStudent_WSurvey$Q180_2 = ifelse(lm_byStudent_WSurvey$Q180=="Strongly disagree", 1, lm_byStudent_WSurvey$Q180_2)
lm_byStudent_WSurvey$Q180_2 = ifelse(lm_byStudent_WSurvey$Q180=="Mostly disagree", 2, lm_byStudent_WSurvey$Q180_2)
lm_byStudent_WSurvey$Q180_2 = ifelse(lm_byStudent_WSurvey$Q180=="Somewhat disagree", 3, lm_byStudent_WSurvey$Q180_2)
lm_byStudent_WSurvey$Q180_2 = ifelse(lm_byStudent_WSurvey$Q180=="Slightly disagree", 4, lm_byStudent_WSurvey$Q180_2)
lm_byStudent_WSurvey$Q180_2 = ifelse(lm_byStudent_WSurvey$Q180=="Neither agree nor disagree", 5, lm_byStudent_WSurvey$Q180_2)
lm_byStudent_WSurvey$Q180_2 = ifelse(lm_byStudent_WSurvey$Q180=="Slightly Agree", 6, lm_byStudent_WSurvey$Q180_2)
lm_byStudent_WSurvey$Q180_2 = ifelse(lm_byStudent_WSurvey$Q180=="Somewhat agree", 7, lm_byStudent_WSurvey$Q180_2)
lm_byStudent_WSurvey$Q180_2 = ifelse(lm_byStudent_WSurvey$Q180=="Mostly agree", 8, lm_byStudent_WSurvey$Q180_2)
lm_byStudent_WSurvey$Q180_2 = ifelse(lm_byStudent_WSurvey$Q180=="Strongly agree", 9, lm_byStudent_WSurvey$Q180_2)
summary(lm_byStudent_WSurvey$Q180_2)
lm_byStudent_WSurvey$Q180_2_2 = ifelse(lm_byStudent_WSurvey$Q180_2>7, 1, 0)
lm_byStudent_WSurvey_Q180 = lm_byStudent_WSurvey[lm_byStudent_WSurvey$Q180_2>0, ]
summary(lm_byStudent_WSurvey_Q180$Q180_2)

modelM7 = lmer(formula = nInCorrect ~  Q180_2_2 + (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM7)

modelM11 = lmer(formula = nAttempts ~  Q180_2_2 + (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM11)

#marginal significance
modelM7 = lmer(formula = timeRevising ~  Q180_2_2 + (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM7)

#marginal significance
modelM8 = lmer(formula = timeRevising ~  Q180_2_2 + problem_id_ranked+ (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM8)

modelM9 = lmer(formula = timeRevising ~  Q180_2_2 + isHints+ (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM9)

#marginal significance
modelM10 = lmer(formula = timeRevising ~  Q180_2_2 + suggest2 + showMissing2 + hLevelInt2 + (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM10)

###########################################
#### Q176: I tend to guess quickly on PCRS problems until I get the answer rather than try to solve 
#### the problem before I choose the answer 
#### It's significance in most models
###############################################
lm_byStudent_WSurvey$Q176_2 = 0
lm_byStudent_WSurvey$Q176_2 = ifelse(lm_byStudent_WSurvey$Q176=="Strongly disagree", 1, lm_byStudent_WSurvey$Q176_2)
lm_byStudent_WSurvey$Q176_2 = ifelse(lm_byStudent_WSurvey$Q176=="Mostly disagree", 2, lm_byStudent_WSurvey$Q176_2)
lm_byStudent_WSurvey$Q176_2 = ifelse(lm_byStudent_WSurvey$Q176=="Somewhat disagree", 3, lm_byStudent_WSurvey$Q176_2)
lm_byStudent_WSurvey$Q176_2 = ifelse(lm_byStudent_WSurvey$Q176=="Slightly disagree", 4, lm_byStudent_WSurvey$Q176_2)
lm_byStudent_WSurvey$Q176_2 = ifelse(lm_byStudent_WSurvey$Q176=="Neither agree nor disagree", 5, lm_byStudent_WSurvey$Q176_2)
lm_byStudent_WSurvey$Q176_2 = ifelse(lm_byStudent_WSurvey$Q176=="Slightly Agree", 6, lm_byStudent_WSurvey$Q176_2)
lm_byStudent_WSurvey$Q176_2 = ifelse(lm_byStudent_WSurvey$Q176=="Somewhat agree", 7, lm_byStudent_WSurvey$Q176_2)
lm_byStudent_WSurvey$Q176_2 = ifelse(lm_byStudent_WSurvey$Q176=="Mostly agree", 8, lm_byStudent_WSurvey$Q176_2)
lm_byStudent_WSurvey$Q176_2 = ifelse(lm_byStudent_WSurvey$Q176=="Strongly agree", 9, lm_byStudent_WSurvey$Q176_2)
summary(lm_byStudent_WSurvey$Q176_2)
lm_byStudent_WSurvey_Q176 = lm_byStudent_WSurvey[lm_byStudent_WSurvey$Q176_2>0, ]
summary(lm_byStudent_WSurvey_Q176$Q176_2)
table(lm_byStudent_WSurvey_Q176$Q176_2)
lm_byStudent_WSurvey_Q176$Q176_2_2 = ifelse(lm_byStudent_WSurvey_Q176$Q176_2>5, 1, 0)

# significant. This means students who prefer to keep guessing, had more incorrect attempts
modelM7 = lmer(formula = nInCorrect ~  Q176_2_2 + (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM7)

#marginal significance
modelM11 = lmer(formula = nAttempts ~  Q176_2_2 + (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM11)

#marginal significance
modelM7 = lmer(formula = timeRevising ~  Q176_2_2 + (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM7)

# significant
modelM8 = lmer(formula = timeRevising ~  Q176_2_2 + problem_id_ranked+ (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM8)

#marginal significant
modelM9 = lmer(formula = timeRevising ~  Q176_2_2 + isHints+ (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM9)

#marginal significance
modelM10 = lmer(formula = timeRevising ~  Q176_2_2 + suggest2 + showMissing2 + hLevelInt2 + (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM10)


################################
#### Q175: I don't tend to work on homework until just before it is to be handed in.
## all the results show it is not significant
###########################################
lm_byStudent_WSurvey$Q175_2 = 0
lm_byStudent_WSurvey$Q175_2 = ifelse(lm_byStudent_WSurvey$Q175=="Strongly disagree", 1, lm_byStudent_WSurvey$Q175_2)
lm_byStudent_WSurvey$Q175_2 = ifelse(lm_byStudent_WSurvey$Q175=="Mostly disagree", 2, lm_byStudent_WSurvey$Q175_2)
lm_byStudent_WSurvey$Q175_2 = ifelse(lm_byStudent_WSurvey$Q175=="Somewhat disagree", 3, lm_byStudent_WSurvey$Q175_2)
lm_byStudent_WSurvey$Q175_2 = ifelse(lm_byStudent_WSurvey$Q175=="Slightly disagree", 4, lm_byStudent_WSurvey$Q175_2)
lm_byStudent_WSurvey$Q175_2 = ifelse(lm_byStudent_WSurvey$Q175=="Neither agree nor disagree", 5, lm_byStudent_WSurvey$Q175_2)
lm_byStudent_WSurvey$Q175_2 = ifelse(lm_byStudent_WSurvey$Q175=="Slightly Agree", 6, lm_byStudent_WSurvey$Q175_2)
lm_byStudent_WSurvey$Q175_2 = ifelse(lm_byStudent_WSurvey$Q175=="Somewhat agree", 7, lm_byStudent_WSurvey$Q175_2)
lm_byStudent_WSurvey$Q175_2 = ifelse(lm_byStudent_WSurvey$Q175=="Mostly agree", 8, lm_byStudent_WSurvey$Q175_2)
lm_byStudent_WSurvey$Q175_2 = ifelse(lm_byStudent_WSurvey$Q175=="Strongly agree", 9, lm_byStudent_WSurvey$Q175_2)
summary(lm_byStudent_WSurvey$Q175_2)
lm_byStudent_WSurvey_Q175 = lm_byStudent_WSurvey[lm_byStudent_WSurvey$Q175_2>0, ]
summary(lm_byStudent_WSurvey_Q175$Q175_2)
table(lm_byStudent_WSurvey_Q175$Q175_2)
lm_byStudent_WSurvey_Q175$Q175_2_2 = ifelse(lm_byStudent_WSurvey_Q175$Q175_2>4, 1, 0)
table(lm_byStudent_WSurvey_Q175$Q175_2_2)

modelM7 = lmer(formula = nInCorrect ~  Q175_2_2 + (1 | user_id), data = lm_byStudent_WSurvey_Q175, REML=FALSE)
summary(modelM7)


modelM11 = lmer(formula = nAttempts ~  Q175_2_2 + (1 | user_id), data = lm_byStudent_WSurvey_Q175, REML=FALSE)
summary(modelM11)


modelM7 = lmer(formula = timeRevising ~  Q175_2_2 + (1 | user_id), data = lm_byStudent_WSurvey_Q175, REML=FALSE)
summary(modelM7)


modelM8 = lmer(formula = timeRevising ~  Q175_2_2 + problem_id_ranked+ (1 | user_id), data = lm_byStudent_WSurvey_Q175, REML=FALSE)
summary(modelM8)

modelM9 = lmer(formula = timeRevising ~  Q175_2_2 + isHints+ (1 | user_id), data = lm_byStudent_WSurvey_Q175, REML=FALSE)
summary(modelM9)

modelM10 = lmer(formula = timeRevising ~  Q175_2_2 + suggest2 + showMissing2 + hLevelInt2 + (1 | user_id), data = lm_byStudent_WSurvey_Q175, REML=FALSE)
summary(modelM10)


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
    userAttempts = userAttempts[order(userAttempts$timestamp), ]
    timePerProblem <- rbind(timePerProblem, data.frame(
      problem_id = problem_id,
      user_id = user_id,
      nAttempts = nrow(userAttempts),
      timeRevising = workingTime(userAttempts$timestamp),
      timeStopped = max(userAttempts$timestamp),
      timeStarted = min(userAttempts$timestamp),
      pCorrect = mean(userAttempts$correct),
      had_hints = any(userAttempts$had_hints[userAttempts$had_feedback]),
      had_feedback = any(userAttempts$had_feedback),
      nInCorrect = sum(userAttempts$correct==FALSE)
    ))
  }
  timePerProblem <- timePerProblem[-1,]
  return (timePerProblem)
}





