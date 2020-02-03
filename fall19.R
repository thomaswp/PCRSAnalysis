
library(plyr)
library(ggplot2)

allAttempts <- read.csv("data/F19/pyandexpt.csv")
consenters <- read.csv("data/F19/consenters.csv")


attempts <- allAttempts[allAttempts$quest_id == 49,]
attempts <- attempts[attempts$user_id %in% consenters$student_id,]

attempts$had_feedback <- attempts$feedback_text != ""
attempts$had_hints <- ifelse(attempts$had_feedback, grepl("'showHints': True", attempts$feedback_text, fixed=T), NA)
attempts$reverse <- ifelse(attempts$had_feedback, grepl("'reversedShow': True", attempts$feedback_text, fixed=T), NA)
attempts$early <- ifelse(attempts$had_feedback, attempts$had_hints == attempts$reverse, NA)
attempts$correct <- attempts$score == attempts$max_score

byStudent <- ddply(attempts, c("problem_id", "user_id"), summarize, 
                   pCorrect = mean(correct),
                   had_hints = any(had_hints[had_feedback]),
                   had_feedback = any(had_feedback),
                   nAttempts = length(correct))


ddply(byStudent, "problem_id", summarize,
      n = length(had_feedback),
      pFeedback = mean(had_feedback),
      pHints = mean(had_hints[had_feedback]))

students <- ddply(attempts, "user_id", summarize,
                  n = length(had_feedback),
                  pFeedback = mean(had_feedback),
                  pHints = mean(had_hints[had_feedback]),
                  early = if (pFeedback == 0) NA else any(early, na.rm=T))
mean(students$pFeedback > 0)
table(students$early)


byStudent <- merge(students, byStudent)
byStudent <- byStudent[!is.na(byStudent$early),]
ggplot(byStudent, aes(y=nAttempts, x=early)) + geom_boxplot() + geom_violin(width=0.2) + facet_wrap(~ problem_id)
ddply(byStudent, c("problem_id", "early"), summarize, pFirstCorrect=mean(nAttempts == 1), n=length(nAttempts))


ddply(attempts, c("problem_id"), summarize, 
      pFeedback = mean(feedback_text == ""), 
      pCorrect = mean(correct),
      pNoFBUncomp = mean(feedback_text == "" & score == 0) / pFeedback,
      pHints = mean(grepl("'showHints': True", feedback_text, fixed=T)) / (1-pFeedback))


#ddply(attempts, c("user_id"), summarize, pLogging = mean(feedback_text == ""))
