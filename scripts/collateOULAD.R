library (magrittr)
library (dplyr)
library (purrr)
library (tidyr)

#
# Read in data
#

print ("Reading assessments.csv")
assessments <- read.csv ("data/OU-Analyse/assessments.csv")

print ("Reading courses.csv")
courses <- read.csv ("data/OU-Analyse/courses.csv")

print ("Reading vle.csv")
vle <- read.csv ("data/OU-Analyse/vle.csv")

print ("Reading studentAssessment.csv")
studentAssessment <- read.csv ("data/OU-Analyse/studentAssessment.csv")

print ("Reading studentInfo.csv")
studentInfo <- read.csv ("data/OU-Analyse/studentInfo.csv")

print ("Reading studentVle.csv")
studentVle <- read.csv ("data/OU-Analyse/studentVle.csv")

#
# Combine assessment with course data
#

print ("Combine asssessment with course data")
assessmentsClipped <- assessments %>% 
    group_by (code_module, code_presentation) %>%
    mutate (week = date / 7) %>%
    arrange (date) %>%
    group_modify (~mutate (.x, assessmentNo = 1:nrow (.x))) %>%
    mutate (assessmentLabel = paste ("assessment", assessmentNo))

assessmentsWithCourses <- courses %>%
    mutate (assessmentLabel = "end") %>%
    rename (date = module_presentation_length) %>%
    mutate (week = date / 7) %>%
    bind_rows (assessmentsClipped)

print ("Save assessmentsWithCourses")
saveRDS (assessmentsWithCourses, "data/modified/assessmentsWithCourses.Rds")

#
# Widen student VLE data
#
# vle.csv is a file with a list of sites, the module and presentation to 
# which they relate, and the activity type, as well as week_from and week_to 
# (not filled out for all sites)
#
# studentVle.csv is a file with a list of students and where they clicked on a 
# given date. 
# There are duplicates date/sites, so it's not clear how these are calculated.

# First, merge these two by site, module and presentation so that we have access to the activity type
print ("Merge studentVle and vle into a single data-set, studentVleAnnotated, which annotates studentVle with site activity type")
studentVleAnnotated <- merge (studentVle, vle
                              , by = c('id_site', 'code_module', 'code_presentation'))

print ("Save merged studentVle")
saveRDS (studentVleAnnotated, "data/modified/studentVleAnnotated.Rds")

print ("Sum duplicate dates")
studentVleDaily <- studentVleAnnotated %>%
    group_by (code_module, code_presentation, id_student, date, activity_type) %>%
    summarise (sum_click = sum (sum_click)) %>%
    ungroup

studentVleDailyAll <- studentVleDaily %>%
    group_by (code_module, code_presentation, id_student, date) %>%
    summarise (sum_click     = sum (sum_click)
             , activity_type = "all") %>%
    ungroup

studentVleDaily %<>% bind_rows (studentVleDaily, studentVleDailyAll)

print ("Save studentVleDaily")
saveRDS (studentVleDaily, "data/modified/studentVleDaily.Rds")
  
print ("Sum dates by week")
studentVleWeekly <- studentVleDaily %>%
    ungroup %>%
    mutate (weekNo = if_else (date < 0, 0, ceiling (date/7))) %>%
    group_by (code_module, code_presentation, id_student, weekNo, activity_type) %>%
    summarise (sum_click = sum (sum_click))

print ("Save studentVleWeekly")
saveRDS (studentVleWeekly, "data/modified/studentVleWeekly.Rds")

print ("Pivot wider to record clicks in a given week as column")
studentVleWider <- studentVleWeekly %>%
  pivot_wider (names_from  = activity_type
             , values_from = sum_click) %>%
  pivot_wider (names_from = weekNo, values_from = all:htmlactivity, names_sep = "W") %>%
  mutate (across (matches ("[a-z]*W[0-9]*"), ~replace_na (., 0)))

print ("Save studentVleWider")
saveRDS (studentVleWider, "data/modified/studentVleWider.Rds")

#
# Calculate submission status by merging assessments with courses and student 
# assessment data
#

print ("Merge assessments with courses")
assessmentsExcerpt <- assessments %>% 
  mutate (dueDate = `date`) %>%
  select (code_module, code_presentation, id_assessment, dueDate) %>%
  merge (., courses, by=c('code_module', 'code_presentation')) 

print ("Merge studentAssessment and assessmentsExcerpt, and fill out whether a student made a submission for a given assessment")
studentAssessmentAnnotated <- merge (studentAssessment, assessmentsExcerpt
                                     , by='id_assessment') %>%
  arrange (id_student , code_module, code_presentation, dueDate) %>%
  group_by (code_module, code_presentation, id_student) %>%
  mutate (dueDate = if_else (condition = is.na (dueDate)
                           , true  = module_presentation_length
                           , false = dueDate)) %>%
  arrange (id_student, code_module, code_presentation, dueDate)

print ("Record whether an assessment was late, on time, transferred credit from previous sitting, or not made at all")
studentAssessmentAnnotated %<>%
  mutate (SubmissionLeeway = pmap_dbl (list (dueDate, date_submitted), `-`)) %>%
  mutate (SubmissionStatus = case_when (
    is.na (score)                            ~ "no submission"
  , SubmissionLeeway < 0  & (is_banked == 0) ~ "late"
  , SubmissionLeeway >= 0 & (is_banked == 0) ~ "on time"
  , (is_banked == 1)                         ~ "transferred credit"
  , is.na (SubmissionLeeway)                 ~ "no submission"
  , TRUE                                     ~ "no submission")) %>%
  mutate (SubmissionStatus = as.factor (SubmissionStatus)) %>%
  mutate (SubmissionStatus = relevel (SubmissionStatus, ref = "on time"))

print ("Record score as a quintile")
tmpScoreQ <- cut (studentAssessmentAnnotated$score
                , quantile (studentAssessmentAnnotated$score, probs = 0:4/4, na.rm = TRUE)
                , include.lowest = TRUE
                , labels = c("0-65%", "66-80%", "81-90%", "91-100%")) %>%
  as.character %>%
  replace_na ("no submission") %>%
  factor (., levels = c("91-100%", "81-90%", "66-80%", "0-65%", "no submission"))
studentAssessmentAnnotated$scoreQ = tmpScoreQ

print ("Save studentAssessmentAnnotated")
saveRDS (studentAssessmentAnnotated, "data/modified/studentAssessmentAnnotated.Rds")

#
# Re-code student info
#

print ("Re-code studentInfo")
studentInfoAnnotated <- studentInfo %>%
  mutate (imd_band   = if_else (imd_band == "10-20", "10-20%", imd_band)) %>%
  mutate (imdModified = ifelse (region == "Scotland" | region == "Ireland" | imd_band == ""
                                , yes = "other"
                                , no  = imd_band)) %>%
  mutate (region            = as.factor (region)
        , highest_education = as.factor (highest_education)
        , imdModified       = factor (imdModified, levels = c("90-100%", "80-90%"
                                                            , "70-80%" , "60-70%"
                                                            , "50-60%" , "40-50%"
                                                            , "30-40%" , "20-30%"
                                                            , "10-20%" , "0-10%"
                                                            , "other")
                                    , ordered = FALSE)) %>%
  mutate (region            = relevel (region, ref = "London Region"))

print ("Saving studentInfoAnnotated")
saveRDS (studentInfoAnnotated, "data/modified/studentInfoAnnotated.Rds")

#
# Re-code student assessment data to record score in the 1st/2nd/3rd assignment
#

second <- function (x) nth (x, 2L)
third  <- function (x) nth (x, 3L)

studentAssessmentInitial <- studentAssessmentAnnotated %>%
    select (code_module, code_presentation, id_student, id_assessment
          , dueDate, score, scoreQ, SubmissionLeeway, SubmissionStatus) %>%
    arrange  (id_student, code_module, code_presentation, dueDate) %>%
    group_by (id_student, code_module, code_presentation) %>%
    summarise (Score1st          = first  (score)
             , ScoreQ1st         = first  (scoreQ)
             , Score2nd          = second (score)
             , ScoreQ2nd         = second (scoreQ)
             , Score3rd          = third  (score)
             , ScoreQ3rd         = third  (scoreQ)
             , SubLeeway1st      = first  (SubmissionLeeway)
             , SubLeeway2nd      = second (SubmissionLeeway)
             , SubLeeway3rd      = third  (SubmissionLeeway)
             , SubStatus1st      = first  (SubmissionStatus)
             , SubStatus2nd      = second (SubmissionStatus)
             , SubStatus3rd      = third  (SubmissionStatus)) %>%
  mutate (across (starts_with ("SubStatus"), ~replace_na ("no submission")))

print ("Save student asssessment data recording score")
saveRDS (studentAssessmentInitial, "data/modified/studentAssessmentInitial.Rds")

#
# Merge student asssessment data and student info, and then merge interaction 
# with VLE data
#

print ("Merge studentAssessmentInitial and studentInfoAnnotated as studentInfoWeekly")
studentInfoWeekly <- merge (studentInfoAnnotated, studentAssessmentInitial
                          , by = c('code_module', 'code_presentation', 'id_student')) 

print ("Merge weekly clicks into studentInfoWeekly")
studentInfoWeekly %<>% merge (., studentVleWider
                            , by = c('code_module', 'code_presentation', 'id_student'))

print ("Save studentInfoWeekly")
saveRDS (studentInfoWeekly, "data/modified/studentInfoWeekly.Rds")