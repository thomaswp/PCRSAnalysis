source("util.R")
library(plyr)
library(ggplot2)
library(lme4) # load library
library(lmerTest)



allAttempts <- read.csv("data/pyandexpt.csv")
consenters <- read.csv("data/consenters.csv")


attempts <- allAttempts[allAttempts$quest_id == 49,]
attempts <- attempts[attempts$user_id %in% consenters$student_id,]

#if feedback = null this means they were not assigned to a condition
attempts$had_feedback <- attempts$feedback_text != ""
attempts$had_hints <- ifelse(attempts$had_feedback, grepl("'showHints': True", attempts$feedback_text, fixed=T), NA)
attempts$reverse <- ifelse(attempts$had_feedback, grepl("'reversedShow': True", attempts$feedback_text, fixed=T), NA)
attempts$early <- ifelse(attempts$had_feedback, attempts$had_hints == attempts$reverse, NA)
attempts$correct <- attempts$score == attempts$max_score

# if showHints is False, then it is useless to check these factors. but I need to keep the condition
# for the other problems

attempts$suggest <- ifelse(attempts$had_hints, grepl("'suggest': True", attempts$feedback_text, fixed=T), NA)
attempts$hLevelInt <- ifelse(attempts$had_hints, grepl("'highlightLevelInt': 2", attempts$feedback_text, fixed=T) | grepl("'highlightLevelInt': 1", attempts$feedback_text, fixed=T), NA)
attempts$showMissing <- ifelse(attempts$had_hints, grepl("'showMissing': True", attempts$feedback_text, fixed=T), NA)

# number of incorrect attempts

byStudent <- ddply(attempts, c("problem_id", "user_id"), summarize, 
                   pCorrect = mean(correct),
                   had_hints = any(had_hints[had_feedback]),
                   had_feedback = any(had_feedback),
                   nAttempts = length(correct),
                   pFirstCorrect=mean(nAttempts == 1), # actually this means the student have only 1 attempt.
                   nInCorrect = sum(correct==FALSE))

ddply(byStudent, "problem_id", summarize,
      n = length(had_feedback),
      pFeedback = mean(had_feedback),
      pHints = mean(had_hints[had_feedback]))


# add hints factors to students dataframe since the factor should be for the student, not for specific problem. We already know what are the problems with hints.
students <- ddply(attempts, "user_id", summarize,
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


byStudent <- merge(students, byStudent)
byStudent <- byStudent[!is.na(byStudent$early),]
ggplot(byStudent, aes(y=nAttempts, x=early)) + geom_boxplot() + geom_violin(width=0.2) + facet_wrap(~ problem_id)

# pFirst correct means 1/number of attempts , so the smaller the higher the attempts they did
ddply(byStudent, c("problem_id", "early"), summarize, pFirstCorrect=mean(nAttempts == 1), n=length(nAttempts))

ddply(attempts, c("problem_id"), summarize, 
      pFeedback = mean(feedback_text == ""), 
      pCorrect = mean(correct),
      pNoFBUncomp = mean(feedback_text == "" & score == 0) / pFeedback,
      pHints = mean(grepl("'showHints': True", feedback_text, fixed=T)) / (1-pFeedback))


#ddply(attempts, c("user_id"), summarize, pLogging = mean(feedback_text == ""))

#Samiha's:
# remove student who got the first problem right on their first try
studentsP1 = byStudent[byStudent$problem_id==172, ]
studentsP1 = studentsP1[order(studentsP1$user_id), ]
# 240 rows
# note: I can just remove students who had 1 attempt because I found some students got it right at the first attempt and took another attempt but make it false.

#108 rows
students_correct_firstTry = studentsP1[studentsP1$pCorrect==1, ]
#108 students got the first problem correct on the first try
(length(unique(students_correct_firstTry$user_id)))
byStudent_2 = byStudent[!byStudent$user_id %in% students_correct_firstTry$user_id, ]
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
byStudent_2_172 = byStudent_2[byStudent_2$problem_id==172, ]
condCompare(byStudent_2_172$pCorrect, byStudent_2_172$early)
condCompare(byStudent_2_172$nInCorrect, byStudent_2_172$early)
# Early condition = 64 students
length(unique(byStudent_2_172$user_id[byStudent_2_172$early==TRUE]))

byStudent_2_173 = byStudent_2[byStudent_2$problem_id==173, ]
condCompare(byStudent_2_173$pCorrect, byStudent_2_173$early)
condCompare(byStudent_2_173$pFirstCorrect, byStudent_2_173$early)
condCompare(byStudent_2_173$nInCorrect, byStudent_2_173$early)
# Early condition = 63 students
length(unique(byStudent_2_173$user_id[byStudent_2_173$early==TRUE]))

byStudent_2_174 = byStudent_2[byStudent_2$problem_id==174, ]
condCompare(byStudent_2_174$pCorrect, byStudent_2_174$early)
condCompare(byStudent_2_174$pFirstCorrect, byStudent_2_174$early)
condCompare(byStudent_2_174$nInCorrect, byStudent_2_174$early)
# Early condition = 63 students
length(unique(byStudent_2_174$user_id[byStudent_2_174$early==TRUE]))

byStudent_2_175 = byStudent_2[byStudent_2$problem_id==175, ]
condCompare(byStudent_2_175$pCorrect, byStudent_2_175$early)
condCompare(byStudent_2_175$pFirstCorrect, byStudent_2_175$early)
fisher.test(byStudent_2_175$pFirstCorrect, byStudent_2_175$early)
condCompare(byStudent_2_175$nInCorrect, byStudent_2_175$early)
# Early condition = 58 students
length(unique(byStudent_2_175$user_id[byStudent_2_175$early==TRUE]))

byStudent_2_176 = byStudent_2[byStudent_2$problem_id==176, ]
condCompare(byStudent_2_176$pCorrect, byStudent_2_176$early)
condCompare(byStudent_2_176$pFirstCorrect, byStudent_2_176$early)
condCompare(byStudent_2_176$nInCorrect, byStudent_2_176$early)
fisher.test(byStudent_2_176$pFirstCorrect, byStudent_2_176$early)
# Early condition = 58 students
length(unique(byStudent_2_176$user_id[byStudent_2_176$early==TRUE]))

byStudent_2_177 = byStudent_2[byStudent_2$problem_id==177, ]
condCompare(byStudent_2_177$pCorrect, byStudent_2_177$early)
condCompare(byStudent_2_177$pFirstCorrect, byStudent_2_177$early)
condCompare(byStudent_2_177$nInCorrect, byStudent_2_177$early)
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

##################
### prepare dataframe for linear model (isHints, priorProblemsHints, problemsSinceLastHint)
###################

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
anovaModel = aov(nAttempts ~ early * problem_id * problemsSinceLastHint * priorProblemsHints, data=lm_byStudent)
summary(anovaModel)

model1 = lmer(formula = nAttempts ~ early * problem_id * isHints * priorProblemsHints + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model1)

model1 = lmer(formula = nAttempts ~ early * isHints * problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model1)

######################
## rating the problems difficulty instead of using 8 problems in the model
####################
lm_byStudent$problem_id_ranked = "0"
lm_byStudent$problem_id_ranked = ifelse((lm_byStudent$problem_id=="172" | lm_byStudent$problem_id=="174"), "1", lm_byStudent$problem_id_ranked)
lm_byStudent$problem_id_ranked = ifelse((lm_byStudent$problem_id=="173" | lm_byStudent$problem_id=="175"), "2", lm_byStudent$problem_id_ranked)

lm_byStudent$problem_id_ranked = ifelse((lm_byStudent$problem_id=="176" | lm_byStudent$problem_id=="178"), "3", lm_byStudent$problem_id_ranked)
lm_byStudent$problem_id_ranked = ifelse((lm_byStudent$problem_id=="177" | lm_byStudent$problem_id=="179"), "4", lm_byStudent$problem_id_ranked)

model13 = lmer(formula = pFirstCorrect ~ earlyCond * problem_id_ranked * isHints * problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model13)

model14 = lmer(formula = nAttempts ~ earlyCond * problem_id_ranked * isHints * problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model14)

model15 = lmer(formula = nInCorrect ~ problem_id_ranked + isHints + priorProblemsHints + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model15)

model15 = lmer(formula = nInCorrect ~ problem_id_ranked + isHints + priorProblemsHints + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model15)

model15 = lmer(formula = nInCorrect ~ problem_id_ranked + isHints + priorProblemsHints + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(model15)

##########################
##### analyzing hints factors
###########################
## why highlightLevelInt is always true? is this correct?
attempts2Factors = attempts

# note that if a student is assigned to a condition, they will remain in this condition.
byStudentFactors <- ddply(attempts2Factors, c("problem_id", "user_id"), summarize, 
                   pCorrect = mean(correct),
                   had_hints = any(had_hints[had_feedback]),
                   had_feedback = any(had_feedback),
                   nAttempts = length(correct),
                   pFirstCorrect=mean(nAttempts == 1),
                   nInCorrect = sum(correct==FALSE))


byStudentFactors <- merge(students, byStudentFactors)
byStudentFactors <- byStudentFactors[!is.na(byStudentFactors$early),]
#just to verify that number of students in the early and late conditions is the same.
test4 = ddply(byStudentFactors, c("user_id"), summarize, n = length(user_id), isEarly = any(early))
table(test4$isEarly)

#prepare data for the model
byStudentFactors$suggest2 = ifelse(byStudentFactors$suggest==TRUE, 1, 0)
byStudentFactors$hLevelInt2= ifelse(byStudentFactors$hLevelInt==TRUE, 1, 0)
byStudentFactors$showMissing2 = ifelse(byStudentFactors$showMissing==TRUE, 1, 0)
# Remove students who got the first problem correct on the first try
# 957 
byStudentFactors2 = byStudentFactors[!byStudentFactors$user_id %in% students_correct_firstTry$user_id, ]
byStudentFactors2 = byStudentFactors2[order(byStudentFactors2$user_id), ]
#just to verify
length(byStudentFactors2$user_id[byStudentFactors2$pCorrect==1 & byStudentFactors2$problem_id==172])

byStudentFactors2$problem_id = as.character(byStudentFactors2$problem_id)
byStudentFactors2$problem_id_ranked = "0"
byStudentFactors2$problem_id_ranked = ifelse((byStudentFactors2$problem_id=="172" | byStudentFactors2$problem_id=="174"), "1", byStudentFactors2$problem_id_ranked)
byStudentFactors2$problem_id_ranked = ifelse((byStudentFactors2$problem_id=="173" | byStudentFactors2$problem_id=="175"), "2", byStudentFactors2$problem_id_ranked)

byStudentFactors2$problem_id_ranked = ifelse((byStudentFactors2$problem_id=="176" | byStudentFactors2$problem_id=="178"), "3", byStudentFactors2$problem_id_ranked)
byStudentFactors2$problem_id_ranked = ifelse((byStudentFactors2$problem_id=="177" | byStudentFactors2$problem_id=="179"), "4", byStudentFactors2$problem_id_ranked)

## prepare the model
# problems ranks only has an effect
modelFAll = lmer(formula = nAttempts ~ suggest2 + hLevelInt2 + showMissing2 + problem_id_ranked + (1 | user_id), data = byStudentFactors2, REML=FALSE)
summary(modelFAll)

modelFAll2 = lmer(formula = nInCorrect ~ suggest2 + hLevelInt2 + showMissing2 + problem_id_ranked + (1 | user_id), data = byStudentFactors2, REML=FALSE)
summary(modelFAll2)

modelFAll3 = lmer(formula = pFirstCorrect ~ suggest2 + hLevelInt2 + showMissing2 + problem_id_ranked + (1 | user_id), data = byStudentFactors2, REML=FALSE)
summary(modelFAll3)

modelF2 = lmer(formula = nInCorrect ~ suggest2 + (1 | user_id), data = byStudentFactors2, REML=FALSE)
summary(modelF2)

modelF3 = lmer(formula = pFirstCorrect ~ hLevelInt2 + (1 | user_id), data = byStudentFactors2, REML=FALSE)
summary(modelF3)

modelF4 = lmer(formula = pFirstCorrect ~ showMissing2 + (1 | user_id), data = byStudentFactors2, REML=FALSE)
summary(modelF4)

modelF5 = lmer(formula = nInCorrect ~ suggest2 + problem_id_ranked + (1 | user_id), data = byStudentFactors2, REML=FALSE)
summary(modelF5)

modelF6 = lmer(formula = nInCorrect ~ problem_id_ranked + (1 | user_id), data = byStudentFactors2, REML=FALSE)
summary(modelF6)

# non of these are significant
condCompare(byStudentFactors2$nAttempts, byStudentFactors2$suggest)
condCompare(byStudentFactors2$nAttempts, byStudentFactors2$showMissing)
condCompare(byStudentFactors2$nAttempts, byStudentFactors2$hLevelInt)

condCompare(byStudentFactors2$pFirstCorrect, byStudentFactors2$suggest)
condCompare(byStudentFactors2$pFirstCorrect, byStudentFactors2$showMissing)
condCompare(byStudentFactors2$pFirstCorrect, byStudentFactors2$hLevelInt)

condCompare(byStudentFactors2$nInCorrect, byStudentFactors2$suggest)
condCompare(byStudentFactors2$nInCorrect, byStudentFactors2$showMissing)
condCompare(byStudentFactors2$nInCorrect, byStudentFactors2$hLevelInt)

ddply(byStudentFactors2, c("problem_id", "suggest"), summarize, pFirstCorrect=mean(nAttempts == 1), n=length(nAttempts))

#### create a model for each problem
byStudentFactors2_172 = byStudentFactors2[byStudentFactors2$problem_id=="172", ]
# early: 64, late: 68
length(unique(byStudentFactors2_172$user_id[byStudentFactors2_172$early==FALSE]))
condCompare(byStudentFactors2_172$nAttempts, byStudentFactors2_172$suggest)
condCompare(byStudentFactors2_172$nAttempts, byStudentFactors2_172$showMissing)
condCompare(byStudentFactors2_172$nAttempts, byStudentFactors2_172$hLevelInt)

byStudentFactors2_179 = byStudentFactors2[byStudentFactors2$problem_id=="179", ]
# early: 52, late: 52 . x: suggest = TRUE
length(unique(byStudentFactors2_179$user_id[byStudentFactors2_179$suggest==TRUE]))
condCompare(byStudentFactors2_179$nAttempts, byStudentFactors2_179$suggest) #significant
condCompare(byStudentFactors2_179$nAttempts, byStudentFactors2_179$showMissing)
condCompare(byStudentFactors2_179$nAttempts, byStudentFactors2_179$hLevelInt)

condCompare(byStudentFactors2_179$pFirstCorrect, byStudentFactors2_179$suggest) #significant
condCompare(byStudentFactors2_179$nInCorrect, byStudentFactors2_179$suggest) #significant

