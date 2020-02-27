source("util.R")
library(plyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ggpubr)
library(rbin)
library(car)


consenters <- read.csv("data/consenters.csv")
allAttempts <- read.csv("data/pyandexpt.csv")
### Length of all students: 1055
(length(unique(allAttempts$user_id)))
### Length of Consenters: 401
(length(unique(consenters$student_id)))

allAttempts <- allAttempts[allAttempts$user_id %in% consenters$student_id,]
survey <- read.csv("data/survey.csv") # survey data can be linked to attempts data by : anonId and hashed_id


allAttempts$timestamp <-as.POSIXct(allAttempts$timestamp, format="%Y-%m-%d %H:%M:%S")
getTimeOfStudy = allAttempts[allAttempts$problem_id>=172 & allAttempts$problem_id<=179, ]
getTimeOfStudy = getTimeOfStudy[order(getTimeOfStudy$timestamp), ] # The estimated time of the start of the study "2019-12-12 09:35:41 EST"
allPriorAttempts = allAttempts[allAttempts$problem_id<172 & allAttempts$timestamp<"2019-12-12 09:35:41", ]
length(unique(allPriorAttempts$problem_id)) # 71 unique problem
allPriorAttempts$correct <- allPriorAttempts$score == allPriorAttempts$max_score
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


attempts <- allAttempts[allAttempts$quest_id == 49,]
### Length of students who attempted the 8 problems in the study: 248
(length(unique(attempts$user_id)))

# Old way of counting prior attempts
# priorAttempts <- ddply(allPriorAttempts, c("user_id", "problem_id"), summarize, nAttempts=length(user_id)) # 25629

meanAttempts <- ddply(priorAttempts, c("problem_id"), summarize, mAttempts = mean(nAttempts), sdAttempt=sd(nAttempts), medAttempts = median(nAttempts))
meanAttempts = meanAttempts[meanAttempts$sd!=0, ]
mergedPriorAttempts <- merge(priorAttempts, meanAttempts)
mergedPriorAttempts$goodPerf <- mergedPriorAttempts$nAttempts <= mergedPriorAttempts$medAttempts
mergedPriorAttempts$zAttempts <- (mergedPriorAttempts$nAttempts - mergedPriorAttempts$mAttempts) / mergedPriorAttempts$sdAttempt
priorKnowledge <- ddply(mergedPriorAttempts, "user_id", summarize, mz = mean(zAttempts), sdz=sd(zAttempts), pGood=mean(goodPerf))
mean(priorKnowledge$mz)
mean(priorKnowledge$sdz)
priorKnowledge$oldHighPK <- priorKnowledge$mz < median(priorKnowledge$mz)
priorKnowledge$highPK <- priorKnowledge$pGood > median(priorKnowledge$pGood)
table(priorKnowledge$highPK)
priorKnowledge <- priorKnowledge[priorKnowledge$user_id %in% attempts$user_id,]
priorKnowledge$pkRank <- rank(priorKnowledge$pGood)
table(priorKnowledge$highPK, priorKnowledge$oldHighPK)

## just to verify
#################
mergedPriorAttempts <- mergedPriorAttempts[mergedPriorAttempts$user_id %in% attempts$user_id,]
table(priorKnowledge$highPK)
length(unique(mergedPriorAttempts$problem_id)) # There is 71 problems, but after excluding problem 55, they are 70
priorAttempts2 <- ddply(mergedPriorAttempts, "user_id", summarize, nProblems = length(problem_id))
summary(priorAttempts2$nProblems)
hist(priorAttempts2$nProblems)


#if feedback = null this means they were not assigned to a condition
attempts <- merge(attempts, priorKnowledge)
attempts$had_feedback <- attempts$feedback_text != ""
attempts$had_hints <- ifelse(attempts$had_feedback, grepl("'showHints': True", attempts$feedback_text, fixed=T), NA)
attempts$reverse <- ifelse(attempts$had_feedback, grepl("'reversedShow': True", attempts$feedback_text, fixed=T), NA)
attempts$saw_hint <- ifelse(attempts$had_feedback, grepl("'hadCodeSuggestion': True", attempts$feedback_text, fixed=T), NA)
# Reverse was False for the first 2 problems, but True for 5 and 6, so if you had hints when reverse was false, you were in the early group
attempts$early <- ifelse(attempts$had_feedback, attempts$had_hints != attempts$reverse, NA)
attempts$correct <- attempts$score == attempts$max_score
attempts$suggest <- ifelse(attempts$had_hints, grepl("'suggest': True", attempts$feedback_text, fixed=T), NA)
attempts$hLevelInt <- ifelse(attempts$had_hints, grepl("'highlightLevelInt': 2", attempts$feedback_text, fixed=T), NA)
attempts$showMissing <- ifelse(attempts$had_hints, grepl("'showMissing': True", attempts$feedback_text, fixed=T), NA)
# table(attempts$had_hints, attempts$saw_hint+0) # Verify hints were only seen when shown
# table(attempts$early, attempts$saw_hint+0, attempts$problem_id) # Verify hints were only seen when they should be

## calculate all parameters for each problem for each student
attempts = attempts[order(attempts$user_id), ]
attempts$timestamp <-as.POSIXct(attempts$timestamp, format="%Y-%m-%d %H:%M:%S")
attempts = attempts[order(attempts$timestamp), ] # looks like students were doing the attempts in 2-4 minutes
attempts$bestScore = ifelse(attempts$has_best_score=="t", TRUE, FALSE)

problem1_attempts <- estimateParameters(attempts, 172)
problem2_attempts <- estimateParameters(attempts, 173)
problem3_attempts <- estimateParameters(attempts, 174)
problem4_attempts <- estimateParameters(attempts, 175)
problem5_attempts <- estimateParameters(attempts, 176)
problem6_attempts <- estimateParameters(attempts, 177)
problem7_attempts <- estimateParameters(attempts, 178)
problem8_attempts <- estimateParameters(attempts, 179)
attemptsTime = rbind(problem1_attempts, problem2_attempts, problem3_attempts, problem4_attempts, problem5_attempts, problem6_attempts, problem7_attempts, problem8_attempts)

attemptsTime <- merge(attemptsTime, priorKnowledge)

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
byStudentWTime <- merge(students, attemptsTime)
# 2% of students had early NA
length(unique(byStudentWTime$user_id[is.na(byStudentWTime$early)]))/length(unique(byStudentWTime$user_id))
(mean(byStudentWTime$pCorrect[byStudentWTime$pCorrect>0]))
byStudentWTime <- byStudentWTime[!is.na(byStudentWTime$early),] #1731
##### length of students in the early group: 119
(length(unique(byStudentWTime$user_id[byStudentWTime$early==TRUE])))
##### length of students in the late group: 124
(length(unique(byStudentWTime$user_id[byStudentWTime$early==FALSE])))
##########################
byStudentWTime$mLnTime <- log(byStudentWTime$timeRevising + 1)
byStudentWTime$lnAttempts <- log(byStudentWTime$nAttempts)
byStudentWTime$exp <- byStudentWTime$early == (byStudentWTime$problem_id <= 175)
byStudentWTime$firstCorrect <- byStudentWTime$pCorrect == 1
byStudentWTime$problem_id_nom <- as.factor(byStudentWTime$problem_id)
byStudentWTime$isAssessment <- c(F, F, T, T, F, F, T, T)[byStudentWTime$problem_id - 172 + 1]
byStudentWTime$problemGroup <- c(0, 1, 0, 1, 2, 3, 2, 3)[byStudentWTime$problem_id - 172 + 1]
#byStudentWTime$nAttemptsUncapped <- byStudentWTime$nAttempts
#byStudentWTime$nAttempts <- pmin(byStudentWTime$nAttempts, 10)

#### (ddply is not WORKING) Number of students in each condition who saw problems 176 or 177 before completing part one (172-175)
attemptsOrder = ddply(byStudentWTime, ("user_id"), summarize, 
                      maxTime4Probs = max(timeStopped[problem_id<176]),
                      minTime4Probs = min(timeStarted[(problem_id==176 | problem_id==177)]),
                      isHighPK = first(highPK), isEarly = first(early))

attemptsOrder$isOrdered <- ifNA(attemptsOrder$maxTime4Probs < attemptsOrder$minTime4Probs, T)
table(attemptsOrder$isOrdered, attemptsOrder$isHighPK)
table(attemptsOrder$isOrdered, attemptsOrder$isEarly)

# Don't remove the students - just report it instead
# byStudentWTime = byStudentWTime[byStudentWTime$isOrdered == TRUE, ]

### check students who did problem 175 before 173, and those who did problem 174 before 172
byStudentWTime$isAssessmentFirst =  FALSE
users172 = byStudentWTime[byStudentWTime$problem_id==172, ]
users173 = byStudentWTime[byStudentWTime$problem_id==173, ]
users174 = byStudentWTime[byStudentWTime$problem_id==174, ]
users175 = byStudentWTime[byStudentWTime$problem_id==175, ]
for (user_id in unique(byStudentWTime$user_id)) {
  # I have to make sure that this students attempted both problems 1A and 2A, and similarly for 1B and 2B
  if(user_id %in% users172$user_id & user_id %in% users174$user_id){
    byStudentWTime$isAssessmentFirst[byStudentWTime$user_id==user_id]= ifelse((byStudentWTime$timeStopped[byStudentWTime$problem_id==172 & byStudentWTime$user_id==user_id]) > 
                                                                                (byStudentWTime$timeStopped[byStudentWTime$problem_id==174 & byStudentWTime$user_id==user_id]), TRUE, byStudentWTime$isAssessmentFirst[byStudentWTime$user_id==user_id])
  }
  if(user_id %in% users173$user_id & user_id %in% users175$user_id){
    byStudentWTime$isAssessmentFirst[byStudentWTime$user_id==user_id]= ifelse((byStudentWTime$timeStopped[byStudentWTime$problem_id==173 & byStudentWTime$user_id==user_id]) > 
                                                                                (byStudentWTime$timeStopped[byStudentWTime$problem_id==175 & byStudentWTime$user_id==user_id]), TRUE, byStudentWTime$isAssessmentFirst[byStudentWTime$user_id==user_id])
  }
}
# 1 student other than those 
length(unique(byStudentWTime$user_id[byStudentWTime$isAssessmentFirst==TRUE])) # they are 2 and 1 is already included in those 13
# This student is in the control condition
length(unique(byStudentWTime$user_id[byStudentWTime$isAssessmentFirst==TRUE & byStudentWTime$early==TRUE]))
byStudentWTime = byStudentWTime[byStudentWTime$isAssessmentFirst == FALSE, ]
length(unique(byStudentWTime$user_id)) # 229 students after removing 14 students in total.
##########################

#the number of attempts correlates strongly with the total \textit{time} a student spent on the problem
cor.test(byStudentWTime$nAttempts, byStudentWTime$timeRevising, method = "spearman")

byStudent172 <- byStudentWTime[byStudentWTime$problem_id == 172,]
# Oddly, no difference in firstCorrect between high and low PK
chisq.test(byStudent172$firstCorrect, byStudent172$highPK)
# But a significant difference in total attempts
condCompare(byStudent172$nAttempts, byStudent172$highPK)
# And a significant (but small) correlation
cor.test(byStudent172$nAttempts, byStudent172$mz, method = "spearman")
# No significant difference in # of highPK students in each group
chisq.test(byStudent172$early, byStudent172$highPK)
# No significant different in mz score either, in either the high or lowPK groups
condCompare(byStudent172$mz, byStudent172$early)
ggplot(byStudent172, aes(y=mz,x=early)) + geom_boxplot() + facet_wrap(~ highPK)
condCompare(byStudent172$mz, byStudent172$early, filter=!byStudent172$highPK)
# No overall difference between conditions in % first correct on first problem
chisq.test(byStudent172$firstCorrect, byStudent172$early)
# No significant differences in firstCorrect between the two groups (though its darn close for lowPK students...)
chisq.test(byStudent172$firstCorrect[byStudent172$highPK], byStudent172$early[byStudent172$highPK])
chisq.test(byStudent172$firstCorrect[!byStudent172$highPK], byStudent172$early[!byStudent172$highPK]) # marginal significance between lowPK with and without hints

table(byStudentWTime$early[byStudentWTime$problem_id==172])


byStudentWTime$highPK1 = ifelse(byStudentWTime$highPK==TRUE, 1, 0)
byStudentWTime$early1 = ifelse(byStudentWTime$early==TRUE, 1, 0)
an = aov(nAttempts ~ highPK1 * early1 , data=byStudentWTime[byStudentWTime$problem_id<174,])
summary(an)
summary(lmer(nAttempts ~ early * highPK + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id < 174,]))

ggplot(byStudentWTime, aes(y=nAttempts, x=early)) + geom_boxplot() + geom_violin(width=0.2) + facet_wrap(~ problem_id)

ggplot(byStudentWTime, aes(timeRevising)) + geom_histogram() + facet_wrap(~ problem_id) + scale_x_continuous(limits=c(0,10))
ggplot(byStudentWTime, aes(log(timeRevising+1))) + geom_histogram() + facet_wrap(~ problem_id) + scale_x_continuous(limits=c(0,10))
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter = byStudentWTime$problem_id <176)


#scatter plot
ggplot(byStudentWTime[byStudentWTime$problem_id<176,], aes(y=rankAttempts, x=pGood, group=early, color=early)) + geom_point(size= 0.3) + geom_smooth(span=0.3, method=lm) + facet_wrap(~ problem_id, scales = "free")
ggplot(byStudentWTime[byStudentWTime$problem_id<176,], aes(y=log(nAttempts), x=pGood, group=early, color=early)) + geom_point(size= 0.3) + geom_smooth(span=0.3, method=lm) + facet_wrap(~ problem_id, scales = "free")

## students almost always got the problem correct \textit{eventually} (96.77\% of the time)
(mean(byStudentWTime$pCorrect > 0))


# RQ1 ======
(length(unique(byStudentWTime$user_id[byStudentWTime$early==TRUE & byStudentWTime$problem_id < 176]))) # 112
(length(unique(byStudentWTime$user_id[byStudentWTime$early==FALSE & byStudentWTime$problem_id < 176]))) # 118

# Table 1 in the paper
# Number of attempts
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 172)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 173)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 174)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 175)

# Total number in each condition
ddply(byStudentWTime[byStudentWTime$problem_id < 176,], "early", summarize, n=length(unique(user_id)))
timeStats <- ddply(byStudentWTime, c("problem_id", "early"), summarize, 
                   nCorrect=sum(pCorrect > 0),
                   mTime=mean(timeRevising), medTime=median(timeRevising), seTime=se(timeRevising),
                   mLnTime=mean(log(timeRevising+1)), seLnTime=se(log(timeRevising+1)),
                   pFirstCorrect=mean(timeRevising==0))
# Number of students who completed problems correctly eventually
timeStats[1:8,1:3]

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



# RQ2 ======
cors <- ddply(byStudentWTime, c("problem_id", "early"), summarize, corPK=cor(nAttempts, pGood, method="spearman"), corP=cor.test(nAttempts, pGood, method="spearman")$p.value)
cors$pcode <- symnum(cors$corP, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
cors

pkStats <- ddply(byStudentWTime, c("problem_id", "early", "highPK"), summarize,
                 n=length(problem_id),
                 mTime=mean(timeRevising), medTime=median(timeRevising), seTime=se(timeRevising),
                 mLnTime=mean(log(timeRevising+1)), seLnTime=se(log(timeRevising+1)),
                 mAttempts=mean(nAttempts), seAttempts=se(nAttempts),
                 mRAttempts=mean(rankAttempts), seRAttempts=se(rankAttempts),
                 mLnAttempts=mean(log(nAttempts)), seLnAttempts=se(log(nAttempts)),
                 medAttempts=median(nAttempts), iqrAttempts=IQR(nAttempts),
                 pFirstCorrect=mean(firstCorrect),
                 seFirstCorrect = se.prop(pFirstCorrect, n))

ggplot(pkStats[pkStats$problem_id < 176,], aes(x=early, y=mRAttempts, linetype=highPK, group=highPK)) +
  geom_line(position=position_dodge(width=0.2)) + 
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mRAttempts-seRAttempts, ymax=mRAttempts+seRAttempts)) +
  facet_wrap(~ problem_id,scales = "free", ncol=2) + ylab("Average number of attempts") + xlab("") + scale_x_discrete(labels=c("no hints", "hints"))
ggplot(pkStats[pkStats$problem_id < 176,], aes(x=early, y=mLnAttempts, linetype=highPK, group=highPK)) +
  geom_line(position=position_dodge(width=0.2)) + 
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mLnAttempts-seLnAttempts, ymax=mLnAttempts+seLnAttempts)) +
  facet_wrap(~ problem_id,scales = "free", ncol=2) + ylab("Average number of attempts") + xlab("") + scale_x_discrete(labels=c("no hints", "hints"))

pkStats$Prior_Knowledge = ifelse(pkStats$highPK==TRUE, "High", "Low")
ggplot(pkStats[pkStats$problem_id < 176,], aes(x=early, y=mAttempts, linetype=Prior_Knowledge, group=Prior_Knowledge))+ scale_fill_discrete(name = "Prior Knowledge", labels = c("Low", "High")) +
  geom_line(position=position_dodge(width=0.2)) + 
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
  facet_wrap(~ problem_id,scales = "free", ncol=2) + ylab("Average number of attempts") + xlab("") + scale_x_discrete(labels=c("no hints", "hints")) 

P172Plot = ggplot(pkStats, aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) +  geom_line(position=position_dodge(width=0.2)) +ylab("Average number of attempts")+ geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) + theme(legend.position = "none")
P172Plot = ggplot(pkStats[pkStats$problem_id == 172,], aes(x=early, y=mAttempts, linetype=Prior_Knowledge, group=Prior_Knowledge))+ 
  scale_x_discrete(labels = c("Control", "Hints"))+
  geom_line(position=position_dodge(width=0.2)) + 
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) + ylab("Average number of attempts") + xlab("") + scale_x_discrete(labels=c("no hints", "hints")) 

P172Plot = ggplot(pkStats[pkStats$problem_id == 172,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +  scale_x_discrete(labels = c("Control", "Hints"))+ 
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.ticks.y=element_blank()
      )
plot(P172Plot)
P173Plot = ggplot(pkStats[pkStats$problem_id == 173,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
  theme(legend.position = "none") + scale_x_discrete(labels = c("Control", "Hints"))+ 
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()
  )

plot(P173Plot)
P174Plot = ggplot(pkStats[pkStats$problem_id == 174,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
  theme(legend.position = "none") + scale_x_discrete(labels = c("Control", "Hints"))+ 
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
plot(P174Plot)
P175Plot = ggplot(pkStats[pkStats$problem_id == 175,], aes(y=mAttempts, x=early, linetype=Prior_Knowledge, group= Prior_Knowledge)) + geom_line(position=position_dodge(width=0.2)) +  scale_fill_discrete(name = "groupCond", labels = c("Control", "Exp"))+
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
  theme(legend.position = "none") + scale_x_discrete(labels = c("Control", "Hints"))+ 
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
plot(P175Plot)
figure = ggarrange(P172Plot, P173Plot, P174Plot, P175Plot,
          labels = c("1A", "2A", "1B", "2B"),
          ncol = 4, nrow = 1,
          common.legend = TRUE,
          font.label = list(size = 12, face = "bold", color ="blue"),
          vjust = 1)
annotate_figure(figure,
                bottom = text_grob("Average number of attempts", color = "blue", face = "bold", size = 14)
)

# Not considering the interaction, there's a significant of PK
anova(m1 <- lmer(nAttempts ~ exp + highPK + problem_id_nom + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id < 176,]), type="III")
#There is a significant interaction effect between experimenrtal and high prior knowledge
anova(m2 <- lmer(nAttempts ~ exp * highPK + problem_id_nom + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id < 176,]), type="III")
# But the interaction model is significantly better
anova(m1, m2)
# Residuals aren't anywhere near normal
qqnorm(residuals(m2))
shapiro.test(residuals(m2))

anova(m3 <- lmer(rankAttempts ~ exp * highPK + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id < 176,]), type="III")
# More balanced, but still not normal
qqnorm(residuals(m3))
shapiro.test(residuals(m3))

anova(m4 <- lmer(log(nAttempts) ~ exp * highPK + problem_id_nom + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id < 176,]), type="III")
qqnorm(residuals(m4))
shapiro.test(residuals(m4))

# Discussion
byStudent172 <- byStudentWTime[byStudentWTime$problem_id == 172,]
condCompare((byStudent172$pCorrect == 1) + 0, byStudent172$highPK, filter=byStudent172$early)
condCompare((byStudent172$pCorrect == 1) + 0, byStudent172$highPK, filter=!byStudent172$early)

# End RQ2 =====

# RQ3 ======
fisher.test(matrix(c(1, 26, 8, 19), nrow=2))

# End RQ3 ====

Anova(lm(log(nAttempts) ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 172,]), type="III")
Anova(lm(log(nAttempts) ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 173,]), type="III")
Anova(lm(log(nAttempts) ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 174,]), type="III")
Anova(lm(log(nAttempts) ~ exp * highPK, data=byStudentWTime[byStudentWTime$problem_id == 175,]), type="III")

pkStats$isAssessment <- c(F, F, T, T, F, F, T, T)[pkStats$problem_id - 172 + 1]
pkStats$problemGroup <- c(0, 1, 0, 1, 2, 3, 2, 3)[pkStats$problem_id - 172 + 1]
pkStats$exp <- pkStats$early == (pkStats$problem_id <= 175)


ggplot(byStudentWTime[byStudentWTime$problem_id < 176,], aes(x=early, y=rankAttempts, color=highPK)) +#, linetype=highPK)) + 
  geom_boxplot(position = position_dodge(width=0.3), width=0.2) +
  stat_summary(geom = "point", fun.y = "mean", size = 3, shape = 24, position=position_dodge(width=0.3)) +
  #geom_line(position=position_dodge(width=0.2)) + 
  #geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
  facet_wrap(~ problem_id, scales = "free", ncol=2) +
  theme_bw()

ggplot(pkStats, aes(x=early, y=mLnTime, linetype=highPK, group=highPK)) + 
   geom_line(position=position_dodge(width=0.2)) + 
   geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mLnTime-seLnTime, ymax=mLnTime+seLnTime)) +
   facet_wrap(~ problem_id, scales = "free", ncol=2)
ggplot(pkStats, aes(x=early, y=mAttempts, linetype=highPK, group=highPK)) + 
   geom_line(position=position_dodge(width=0.2)) + 
   geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
   facet_wrap(~ problem_id, scales = "free", ncol=2)
ggplot(pkStats, aes(x=early, y=pFirstCorrect, linetype=highPK, group=highPK)) + 
  geom_line(position=position_dodge(width=0.2)) + 
  geom_errorbar(position=position_dodge(width=0.2), aes(ymin=pFirstCorrect-seFirstCorrect, ymax=pFirstCorrect+seFirstCorrect)) +
  facet_wrap(~ problem_id, scales = "free", ncol=2)
ggplot(pkStats, aes(x=problem_id, y=mLnTime, color=early, linetype=highPK)) + 
   geom_line(position=position_dodge(width=0.2), size=1) + 
   geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mLnTime-seLnTime, ymax=mLnTime+seLnTime))


summary(lmer(timeRevising ~ exp * highPK + problem_id_nom + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id < 176,]))
# There not a significant effect of having hints for low PK students overall
summary(lmer(nAttempts ~ exp + problem_id + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id < 176 & !byStudentWTime$highPK,]))


# Over all problems, definitely an effect of PK on performance
summary(lmer(nAttempts ~ mz + problem_id_nom + (1 | user_id), data=byStudentWTime))
# Similarly strong over the first four problems
summary(lmer(nAttempts ~ mz + problem_id_nom + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id < 176,]))
condCompare(byStudentWTime$nAttempts, byStudentWTime$highPK)
# However, there is only a marginally significant main effect for PK if we consider the no-hint group
anova(lmer(nAttempts ~ highPK + problem_id_nom + (1 | user_id), data=byStudentWTime[byStudentWTime$problem_id < 176 & !byStudentWTime$early,]))

# 172: Significant difference between high and low for no-hints, but not hints
condCompare(byStudentWTime$nAttempts, byStudentWTime$highPK, filter=byStudentWTime$problem_id == 172 & byStudentWTime$early)
condCompare(byStudentWTime$nAttempts, byStudentWTime$highPK, filter=byStudentWTime$problem_id == 172 & !byStudentWTime$early)
# 172: No significant differences between hints and no hints in either group, but both have a small (NS) effect in opposite directions
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 172 & byStudentWTime$highPK)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 172 & !byStudentWTime$highPK)

# 173: Significant difference between high and low for no-hints, but not hints
condCompare(byStudentWTime$nAttempts, byStudentWTime$highPK, filter=byStudentWTime$problem_id == 173 & byStudentWTime$early)
condCompare(byStudentWTime$nAttempts, byStudentWTime$highPK, filter=byStudentWTime$problem_id == 173 & !byStudentWTime$early)

# 174: Non-significant difference (med effect size) between high and low for no-hints, but not hints
condCompare(byStudentWTime$nAttempts, byStudentWTime$highPK, filter=byStudentWTime$problem_id == 174 & byStudentWTime$early)
condCompare(byStudentWTime$nAttempts, byStudentWTime$highPK, filter=byStudentWTime$problem_id == 174 & !byStudentWTime$early)
# 174: No significant differences between hints and no hints in either group, but both have a small (NS) effect in opposite directions
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 174 & byStudentWTime$highPK)
condCompare(byStudentWTime$nAttempts, byStudentWTime$early, filter=byStudentWTime$problem_id == 174 & !byStudentWTime$highPK)

# 175: Non-significant difference (med effect size) between high and low for no-hints, but not hints
condCompare(byStudentWTime$nAttempts, byStudentWTime$highPK, filter=byStudentWTime$problem_id == 174 & byStudentWTime$early)
condCompare(byStudentWTime$nAttempts, byStudentWTime$highPK, filter=byStudentWTime$problem_id == 174 & !byStudentWTime$early)


ggplot(pkStats[pkStats$problem_id<=175, ], aes(x=exp, y=mLnTime, linetype=highPK, group=highPK)) + 
  geom_line(position=position_dodge(width=0.2)) + scale_x_discrete(labels=c("no hints", "hints"))+ geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mLnTime-seLnTime, ymax=mLnTime+seLnTime)) +
  facet_wrap(~ problem_id, scales = "free")

ggplot(pkStats[pkStats$problem_id<=175, ], aes(x=exp, y=mAttempts, linetype=highPK, group=highPK)) + 
  geom_line(position=position_dodge(width=0.2)) + scale_x_discrete(labels=c("no hints", "hints"))+ geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mAttempts-seAttempts, ymax=mAttempts+seAttempts)) +
  facet_wrap(~ problem_id, scales = "free")
# pFirst correct means 1/number of attempts , so the smaller the higher the attempts they did
ddply(byStudentWTime, c("problem_id", "early"), summarize, pFirstCorrect=mean(nAttempts == 1), n=length(nAttempts), meanTime = mean(timeRevising))


ggplot(pkStats[pkStats$problem_id==172, ], aes(x=exp, y=mLnTime, linetype=highPK, group=highPK)) + 
  geom_line(position=position_dodge(width=0.2)) + xlab("problem 172") + scale_x_discrete(labels=c("no hints", "hints"))+ geom_errorbar(position=position_dodge(width=0.2), aes(ymin=mLnTime-seLnTime, ymax=mLnTime+seLnTime))

#Check for low PK students in the first problem, and see the difference in hints (whether they had hints or not)
pkStatsLow172 = byStudentWTime[byStudentWTime$highPK==FALSE & byStudentWTime$problem_id==172, ]
condCompare(pkStatsLow172$timeRevising, pkStatsLow172$early)
condCompare(pkStatsLow172$nAttempts, pkStatsLow172$early)

pkStatsHigh172 = byStudentWTime[byStudentWTime$highPK==TRUE & byStudentWTime$problem_id==172, ]
condCompare(pkStatsHigh172$timeRevising, pkStatsHigh172$early)
condCompare(pkStatsHigh172$nAttempts, pkStatsHigh172$early)

pkStatsNoHints172 = byStudentWTime[byStudentWTime$early==FALSE & byStudentWTime$problem_id==172, ]
condCompare(pkStatsNoHints172$timeRevising, pkStatsNoHints172$highPK) # this is signifcant
condCompare(pkStatsNoHints172$nAttempts, pkStatsNoHints172$highPK) #this is significant

pkStatsLow173 = byStudentWTime[byStudentWTime$highPK==FALSE & byStudentWTime$problem_id==173, ]
condCompare(pkStatsLow173$timeRevising, pkStatsLow173$early)
condCompare(pkStatsLow173$nAttempts, pkStatsLow173$early) # this is significant

pkStatsNoHints173 = byStudentWTime[byStudentWTime$early==FALSE & byStudentWTime$problem_id==173, ]
condCompare(pkStatsNoHints173$timeRevising, pkStatsNoHints173$highPK) # this is signifcant
condCompare(pkStatsNoHints173$nAttempts, pkStatsNoHints173$highPK) #this is significant


ddply(attempts, c("problem_id"), summarize, 
      pFeedback = mean(feedback_text == ""), 
      pCorrect = mean(correct),
      pNoFBUncomp = mean(feedback_text == "" & score == 0) / pFeedback,
      pHints = mean(grepl("'showHints': True", feedback_text, fixed=T)) / (1-pFeedback))

### check dropouts between problems
studentProblems = ddply(byStudentWTime, c("user_id"), summarise, nProblems=length(user_id), early = first(early), highPK = first(highPK))
condCompare(studentProblems$nProblems, studentProblems$highPK) # significant
condCompare(studentProblems$nProblems, studentProblems$early) #not significant
table(studentProblems$nProblems)
# 28.8% dropped out
length(studentProblems$user_id[studentProblems$nProblems<8])/length(studentProblems$user_id)
# 27.73% of early group dropped out
length(studentProblems$user_id[studentProblems$nProblems<8 & studentProblems$early==TRUE])/length(studentProblems$user_id[studentProblems$early==TRUE])
# 29.8% of late group dropped out
length(studentProblems$user_id[studentProblems$nProblems<8 & studentProblems$early==FALSE])/length(studentProblems$user_id[studentProblems$early==FALSE])
# 23.13% of students with high PK dropped out
length(studentProblems$user_id[studentProblems$nProblems<8 & studentProblems$highPK==TRUE])/length(studentProblems$user_id[studentProblems$highPK==TRUE])
# 35.77% of students with low PK dropped out
length(studentProblems$user_id[studentProblems$nProblems<8 & studentProblems$highPK==FALSE])/length(studentProblems$user_id[studentProblems$highPK==FALSE])

hist(studentProblems$nProblems)

# trying to get the the order of problems
attempts$problem_id = as.numeric(attempts$problem_id)
attempts = attempts[order(attempts$timestamp), ]
attempts <- attempts[!is.na(attempts$early),]
attempts_2 = ddply(attempts, c("user_id", "problem_id"), summarise, firstStartTime = min(timestamp), lastTime = max(timestamp), n = length(user_id), early=first(early), highPK=first(highPK))
attempts_2 = attempts_2[order(attempts_2$user_id), ]
#Number of students in each condition who saw problems 176 or 177 before completing part one (172-175)
length(unique(attempts_2$user_id)) # 243
attempts_2$unOrdered = FALSE


notInOrder = attempts_2[attempts]
attempts_2$inOrder = FALSE
attempts_2 = attempts_2[attempts_2$problem_id<178, ]

for (user_id in unique(attempts_2$user_id)) {
  userAttempts <- attempts_2[attempts_2$user_id == user_id,]
  userAttempts = userAttempts[order(userAttempts$lastTime), ]
  inOrder = TRUE
  item = userAttempts$problem_id[1]
    for (i in 2:length(userAttempts)){
      if(!is.na(userAttempts$problem_id[i])){
        if (userAttempts$problem_id[i]>=item){
          item = userAttempts$problem_id[i]
        }else {
          inOrder = FALSE
        }
      }
  }
  attempts_2$inOrder[attempts_2$user_id==user_id] = inOrder
}
attempts_2 = ddply(attempts_2, c("user_id"), summarise, early=first(early), highPK=first(highPK), inOrder=first(inOrder))
summary(attempts_2$inOrder) # 15.6% of students did it out of order
# 22.01% of low PK did it out of order 
length(attempts_2$user_id[attempts_2$inOrder==FALSE & attempts_2$highPK==FALSE])/length(attempts_2$user_id[attempts_2$highPK==FALSE])
# 10.44% of high PK did it out of order
length(attempts_2$user_id[attempts_2$inOrder==FALSE & attempts_2$highPK==TRUE])/length(attempts_2$user_id[attempts_2$highPK==TRUE])
# 16.80% in early condition did it out of order
length(attempts_2$user_id[attempts_2$inOrder==FALSE & attempts_2$early==TRUE])/length(attempts_2$user_id[attempts_2$early==TRUE])
# 14.5% in late condition did it out of order
length(attempts_2$user_id[attempts_2$inOrder==FALSE & attempts_2$early==FALSE])/length(attempts_2$user_id[attempts_2$early==FALSE])


## only for the first 4 problems
studentProblems = ddply(byStudentWTime[byStudentWTime$problem_id<175, ], c("user_id"), summarise, nProblems=length(user_id), early = first(early), highPK = first(highPK))
length(studentProblems$user_id[studentProblems$nProblems<8]) ## 28.8%

### prepare a small dataframe where students have the same code submitted, and it is incorrect
caseStudyDF = attempts[attempts$problem_id==172 & attempts$bestScore==FALSE & attempts$score<4, ]
#caseStudyDF <- caseStudyDF[!is.na(caseStudyDF$early),] 
caseStudyDF2= ddply(caseStudyDF, c("user_id", "submission"), summarise, nDuplicates=length(user_id), score = first(score), early = first(early), highPK = first(highPK))
caseStudyDF2 = caseStudyDF2[order(caseStudyDF2$user_id), ]
caseStudyDF2_HPK = caseStudyDF2[caseStudyDF2$highPK==TRUE, ]
caseStudyDF2_LPK = caseStudyDF2[caseStudyDF2$highPK==FALSE, ]
write.csv(caseStudyDF2_LPK, "caseStudyDF2_LPK.csv")
write.csv(caseStudyDF2_HPK, "caseStudyDF2_HPK.csv")
write.csv(caseStudyDF2, "caseStudyDF2.csv")
# I found 2 cases with the same incorrect code
case1 = attempts[attempts$user_id==12899, ]
case1 = case1[, c("user_id", "problem_id", "early", "highPK", "submission", "feedback_text", "score", "max_score","timestamp")]
case2 = attempts[attempts$user_id==13004, ]
case2 = case2[, c("user_id", "problem_id", "early", "highPK", "submission", "feedback_text", "score", "max_score","timestamp")]

case1 = case1[order(case1$timestamp), ]
#cases = cases[cases$problem_id==172, ]
case2 = case2[order(case2$timestamp), ]
# estimate time between hints for these students
case1$timeDiff = 0
case2$timeDiff = 0
for (i in 2:length(case1)) {
    case1$timeDiff[i] =  as.numeric(case1$timestamp[i] - case1$timestamp[i-1], units="mins")
}

cases = cases[, c("user_id", "problem_id", "early", "highPK", "submission", "feedback_text", "score", "max_score","timestamp")]

test2 = ddply(cases, c("user_id", "problem_id"), summarise, n = length(user_id))
write.csv(cases, "cases.csv")

#may need to make sure that students in the analysis are those who did all the 8 problems.
studentsW8Problems = ddply(byStudentWTime, c("user_id"), summarise, nProblems=length(unique(problem_id)), early = first(early), highPK = first(highPK))
studentsWN8Problems = studentsW8Problems[studentsW8Problems$nProblems<8, ]
byStudentWTime=byStudentWTime[!byStudentWTime$user_id %in% studentsWN8Problems$user_id, ]

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
condCompare(byStudent_2_172$nAttempts, byStudent_2_172$early)
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

lm_byStudent2 = byStudent_2
lm_byStudent = byStudentWTime
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
summary(lmer(formula = nAttempts ~ earlyCond + problem_id_ranked + isHints + problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE))

summary(lmer(formula = timeRevising ~ earlyCond + problem_id_ranked + isHints + problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE))

summary(lmer(formula = timeRevising ~ problem_id_ranked + isHints + priorProblemsHints + (1 | user_id), data = lm_byStudent, REML=FALSE))

summary(lmer(formula = timeRevising ~ problem_id_ranked + isHints + (1 | user_id), data = lm_byStudent, REML=FALSE))

summary(lmer(formula = timeRevising ~ isHints + (1 | user_id), data = lm_byStudent, REML=FALSE))

summary(lmer(formula = nAttempts ~ earlyCond + problem_id_ranked + isHints + problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE))
summary(lmer(formula = nAttempts ~ earlyCond * problemsSinceLastHint + (1 | user_id), data = lm_byStudent, REML=FALSE))


#####################################################################
##### analyzing hints factors: suggest, showMissing, highlightLevelInt
######################################################################
#prepare data for the model
lm_byStudent$suggest2 = ifelse(lm_byStudent$suggest==TRUE, 1, 0)
lm_byStudent$hLevelInt2= ifelse(lm_byStudent$hLevelInt==TRUE, 1, 0)
lm_byStudent$showMissing2 = ifelse(lm_byStudent$showMissing==TRUE, 1, 0)

modelFAll = lmer(formula = nAttempts ~ suggest2 + hLevelInt2 + showMissing2 + (1 | user_id), data = lm_byStudent, REML=FALSE)
summary(modelFAll)

summary(lmer(formula = nInCorrect ~ suggest2 + hLevelInt2 + showMissing2 + (1 | user_id), data = lm_byStudent, REML=FALSE))

summary(lmer(formula = timeRevising ~ suggest2 + hLevelInt2 + showMissing2 + (1 | user_id), data = lm_byStudent, REML=FALSE))
summary(lmer(formula = timeRevising ~ suggest2 + hLevelInt2 + showMissing2 + problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE))
summary(lmer(formula = nInCorrect ~ suggest2 + hLevelInt2 + showMissing2 + problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE))

summary(lmer(formula = timeRevising ~ suggest2 + hLevelInt2 + showMissing2 + problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE))
summary(lmer(formula = nInCorrect ~ suggest2 + (1 | user_id), data = lm_byStudent, REML=FALSE))
summary(lmer(formula = timeRevising ~ suggest2 + (1 | user_id), data = lm_byStudent, REML=FALSE))
summary(lmer(formula = timeRevising ~ hLevelInt2 + (1 | user_id), data = lm_byStudent, REML=FALSE))
summary(lmer(formula = timeRevising ~ showMissing2 + (1 | user_id), data = lm_byStudent, REML=FALSE))
summary(lmer(formula = nInCorrect ~ suggest2 + problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE))
summary(lmer(formula = nInCorrect ~ problem_id_ranked + (1 | user_id), data = lm_byStudent, REML=FALSE))

# non of these are significant
condCompare(lm_byStudent$nAttempts, lm_byStudent$suggest, wilcox.test, filter=(lm_byStudent$problem_id==172))
condCompare(lm_byStudent$nAttempts, lm_byStudent$showMissing, wilcox.test, filter=(lm_byStudent$problem_id==172))
condCompare(lm_byStudent$nAttempts, lm_byStudent$hLevelInt, wilcox.test, filter=(lm_byStudent$problem_id==172))

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
length(unique(lm_byStudent$user_id)) # 243
surveyFiltered$anonID = surveyFiltered$anonid
lm_byStudent_WSurvey = lm_byStudent[lm_byStudent$anonID %in% surveyFiltered$anonid, ]
lm_byStudent_WSurvey = merge(lm_byStudent_WSurvey, surveyFiltered, by = "anonID")
length(unique(lm_byStudent_WSurvey$user_id)) # 181
pretty_survey = lm_byStudent_WSurvey[, c("user_id", "early", "highPK", "nAttempts", "Q138", "Q140", "Q142", "Q144_4", "Q144_5", "Q146_4", "Q146_5", "Q129")]
pretty_survey_summarized = ddply(pretty_survey, c("user_id"), summarise, early = first(early), 
                      highPK = first(highPK),
                      mAttempts = mean(nAttempts),
                      Q138 = first(Q138),
                      Q140 = first(Q140),
                      Q142 = first(Q142),
                      Q144_4 = first(Q144_4),
                      Q144_5 = first(Q144_5),
                      Q146_4 = first(Q146_4),
                      Q146_5 = first(Q146_5),
                      Q129 = first(Q129))
pretty_survey_summarized = pretty_survey_summarized[order(pretty_survey_summarized$highPK), ]
write.csv(pretty_survey_summarized, "pretty_survey_summarized.csv")
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

modelS3 = lmer(formula = timeRevising ~  priorKn * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
summary(modelS3)

modelS4 = lmer(formula = timeRevising ~  priorKn * isHints + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
summary(modelS4)

modelS5 = lmer(formula = nInCorrect ~  priorKn * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
summary(modelS5)

modelS6 = lmer(formula = nAttempts ~  priorKn * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_PK, REML=FALSE)
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

modelM3 = lmer(formula = timeRevising ~  motiv2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Motiv, REML=FALSE)
summary(modelM3)

modelM4 = lmer(formula = nAttempts ~  motiv2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Motiv, REML=FALSE)
summary(modelM4)

modelM5 = lmer(formula = nInCorrect ~  motiv2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Motiv, REML=FALSE)
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

modelM7 = lmer(formula = nInCorrect ~  Q180_2_2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM7)

modelM11 = lmer(formula = nAttempts ~  Q180_2_2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM11)

modelM7 = lmer(formula = timeRevising ~  Q180_2_2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM7)

modelM8 = lmer(formula = timeRevising ~  Q180_2_2 + problem_id_ranked+ (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM8)

modelM10 = lmer(formula = timeRevising ~  Q180_2_2 + suggest2 + showMissing2 + hLevelInt2 + (1 | user_id), data = lm_byStudent_WSurvey_Q180, REML=FALSE)
summary(modelM10)

###########################################
#### Q176: I tend to guess quickly on PCRS problems until I get the answer rather than try to solve 
#### the problem before I choose the answer 
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

modelM7 = lmer(formula = nInCorrect ~  Q176_2_2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM7)

modelM11 = lmer(formula = nAttempts ~  Q176_2_2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM11)

modelM7 = lmer(formula = timeRevising ~  Q176_2_2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM7)

modelM8 = lmer(formula = timeRevising ~  Q176_2_2 + problem_id_ranked+ (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
summary(modelM8)

modelM9 = lmer(formula = timeRevising ~  Q176_2_2 * isHints+ (1 | user_id), data = lm_byStudent_WSurvey_Q176, REML=FALSE)
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

modelM7 = lmer(formula = nInCorrect ~  Q175_2_2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Q175, REML=FALSE)
summary(modelM7)


modelM11 = lmer(formula = nAttempts ~  Q175_2_2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Q175, REML=FALSE)
summary(modelM11)


modelM7 = lmer(formula = timeRevising ~  Q175_2_2 * earlyCond + (1 | user_id), data = lm_byStudent_WSurvey_Q175, REML=FALSE)
summary(modelM7)


############ read survey data - open ended questions ##########
###############################################################
surveyQual = read.csv("data/survey_questionTitles.csv")
surveyQual = surveyQual[order(surveyQual$StartDate), ]

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
      had_feedback = any(userAttempts$had_feedback),
      nInCorrect = sum(userAttempts$correct==FALSE),
      mBestScore = mean(userAttempts$bestScore)
    ))
  }
  timePerProblem <- timePerProblem[-1,]
  timePerProblem$rankAttempts <- rank(timePerProblem$nAttempts)
  return (timePerProblem)
}

estimatePriorAttempts <- function(attempts, problem_id){
  problemAttempts <- attempts[attempts$problem_id == problem_id,]
  problemAttempts <- problemAttempts[order(problemAttempts$timestamp), ]
  
  timePerProblem <- NA
  for (user_id in unique(problemAttempts$user_id)) {
    userAttempts <- problemAttempts[problemAttempts$user_id == user_id,]
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





