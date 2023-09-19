library (shiny)
library (datasets)
library (ggplot2)
library (magrittr)
library (dplyr)
library (ggmosaic)
library (plotly)
library (stringr)

print ("Load VLE data by day")
studentVleDaily  = readRDS ("../data/modified/studentVleDaily.Rds") %>%
    group_by (code_module, code_presentation, date, activity_type) %>%
    summarise (mean   = mean (sum_click, na.rm=T)
             , q1     = quantile (sum_click, .25, na.rm=T)
             , q3     = quantile (sum_click, .75, na.rm=T)
             , sd     = sd (sum_click, na.rm=T)
             , median = median (sum_click, na.rm=T))

print ("Load VLE data by week")
studentVleWeekly = readRDS ("../data/modified/studentVleWeekly.Rds") %>%
    group_by (code_module, code_presentation, weekNo, activity_type) %>%
    summarise (mean   = mean (sum_click, na.rm=T)
             , q1     = quantile (sum_click, .25, na.rm=T)
             , q3     = quantile (sum_click, .75, na.rm=T)
             , sd     = sd (sum_click, na.rm=T)
             , median = median (sum_click, na.rm=T))

print ("Load student info")
studentInfoWeekly = readRDS ("../data/modified/studentInfoWeekly.Rds")
  
print ("Load course/assessment data")
assessmentsWithCourses = readRDS ("../data/modified/assessmentsWithCourses.Rds")

varsRaw <- c(
    "code_module", "code_presentation", "id_student"
  , "gender", "region", "highest_education", "imd_band", "age_band"
  , "num_of_prev_attempts", "studied_credits", "disability"
  , "final_result"
  , "imdModified", "FirstClick", "LastClick"
  , "Score1st", "Score2nd", "Score3rd"
  , "ScoreQ1st", "ScoreQ2nd", "ScoreQ3rd"
  , "SubStatus1st", "SubStatus2nd", "SubStatus3rd")
varsPretty <- c(
    "Module", "Presentation", "Student ID"
  , "Gender", "Region", "Highest qualification prior to O.U.", "IMD band", "Age band"
  , "Number of previous attempts", "Number of credits taken", "Disabled"
  , "Final result"
  , "IMD band (excl. Scotland/Ireland)", "Date of first click", "Date of last click"
  , "Score in 1st assignment", "Score in 2nd assignment", "Score in 3rd assignment"
  , "Score in 1st assignment (quintile)", "Score in 2nd assignment (quintile)", "Score in 3rd assignment (quintile)"
  , "Status of 1st assignment", "Status of 2nd assignment", "Status of 3rd assignment")
varsTable <- data.frame (raw = varsRaw, pretty = varsPretty)

lookupVars <- function (xs) {
    helper = function (x) {
        if (x[[2]] == 0)
            return (paste ("Clicks on", x[[1]], "pages prior to module start"))
        else
            return (paste ("Clicks on", x[[1]], "pages during week", x[[2]]))
    }
    if (str_ends (xs, "W[0-9]*")) {
        activitiesRaw    = str_split (xs, 'W')
        activitiesPretty = sapply (activitiesRaw, helper)
        return (activitiesPretty)
    }
    else
        return (varsTable[varsTable$raw %in% xs,]$pretty)
}

ui <- fluidPage(
    titlePanel("Open University Learning Analytics data-set")
  , sidebarPanel (
        selectInput ("selectMode"
                       , "Mode"
                       , choices = list ("Interactions with online material" = "clicks"
                                       , "Course/assessment outcomes"        = "outcomes")
                       , selected = "clicks")
      , conditionalPanel (
            condition = "input.selectMode == 'clicks'"
          , selectInput ("selectPresentation"
                       , "Presentation"
                       , choices = list ("January 2013" = "2013B"
                                       , "October 2013" = "2013J"
                                       , "January 2014" = "2014B"
                                       , "October 2014" = "2014J")
                       , selected = "2013J")
          , selectInput ("selectModule"
                       , "Module"
                       , choices  = "AAA"
                       , selected = "AAA")
          , selectInput ("selectActivity"
                       , "Activity"
                       , choices  = "all"
                       , multiple = TRUE
                       , selected = "all")
          , selectInput ("selectAggregate"
                       , "Aggregate"
                       , choices  = list ("mean", "median")
                       , selected = "mean")
          , selectInput ("selectTimeSeries"
                       , "Time series"
                       , choices  = list ("day", "week")
                       , selected = "week")
          , sliderInput ("selectTimeSeriesRange"
                       , "Date range"
                       , min = 0, max = 39, value = c(0, 12))
          , checkboxInput ("showPopDist"
                         , "Show population distribution"
                         , value = F)
      )
      , conditionalPanel (
            condition = "input.selectMode == 'outcomes'"
          , selectInput ("selectOutcome"
                           , "Outcome"
                           , choices = list ("Final result"                        = "final_result"
                                           , "Score in 1st assignment"             = "Score1st"
                                           , "Score in 2nd assignment"             = "Score2nd"
                                           , "Score in 2nd assignment"             = "Score3rd"
                                           , "Score in 1st assignment (quintile)"  = "ScoreQ1st"
                                           , "Score in 2nd assignment (quintile)"  = "ScoreQ2nd"
                                           , "Score in 3rd assignment (quintile)"  = "ScoreQ3rd"
                                           , "Submission status of 1st assignment" = "SubStatus1st"
                                           , "Submission status of 2nd assignment" = "SubStatus2nd"
                                           , "Submission status of 3rd assignment" = "SubStatus3rd"))
          , selectInput ("selectPredictor"
                       , "Predictor"
                       , choices  = "gender"
                       , selected = "gender"))
  )
  , mainPanel (tags$style(type = "text/css"
                        , ".shiny-output-error { visibility: hidden; }"
                        , ".shiny-output-error:before { visibility: hidden; }")
             , plotlyOutput (outputId = "plotArea"))
)

server <- function(input, output, session) {
    observeEvent (input$selectPresentation, {
        updateSelectInput (session, input = "selectModule"
                         , choices = { studentVleWeekly %>% 
                             filter (code_presentation == input$selectPresentation) %>%
                             pull (code_module) %>% unique () })
    })
    observeEvent (input$selectModule, {
        updateSelectInput (session, input = "selectActivity"
                         , choices = { studentVleWeekly %>%
                             filter (code_presentation == input$selectPresentation
                                   , code_module       == input$selectModule) %>%
                             pull (activity_type) %>% unique () }
                         , selected = "all")
    })
    observe ({
        combinedTS = list (module=input$selectModule, presentation=input$selectPresentation, timeSeries=input$selectTimeSeries)
        updateSliderInput (session, input = "selectTimeSeriesRange"
                         , min = {
                              if (combinedTS$timeSeries == "day") {
                                  studentVleDaily %>%
                                      filter (code_presentation == combinedTS$presentation
                                            , code_module       == combinedTS$module) %>%
                                      pull (date) %>% min () 
                              } else {
                                  studentVleWeekly %>%
                                      filter (code_presentation == combinedTS$presentation
                                            , code_module       == combinedTS$module) %>%
                                      pull (weekNo) %>% min () 
                              }
                          }
                        , max = {
                             if (combinedTS$timeSeries == "day") {
                                 studentVleDaily %>%
                                     filter (code_presentation == combinedTS$presentation
                                           , code_module       == combinedTS$module) %>%
                                     pull (date) %>% max ()
                             } else {
                                studentVleWeekly %>%
                                    filter (code_presentation == combinedTS$presentation
                                          , code_module       == combinedTS$module) %>%
                                    pull (weekNo) %>% max ()
                            }
                         }
                       , value = {
                            if (combinedTS$timeSeries == "day") c(0, 83)
                            else c(0, 12)
                       })
    })
    
    observeEvent (input$selectOutcome, {
        demographics = list ("gender", "region", "highest_education", "imdModified"
                           , "age_band", "num_of_prev_attempts", "studied_credits", "disability")
        scoreOutcomesW1 = list ("Score1st", "ScoreQ1st", "SubStatus1st")
        scoreOutcomesW4 = list ("Score2nd", "ScoreQ2nd", "SubStatus2nd")
        scoreOutcomesW7 = list ("Score3rd", "ScoreQ3rd", "SubStatus3rd")
      
        if (input$selectOutcome %in% scoreOutcomesW1) {
            clickPredictors = c("allW0", "allW1")
            availPredictors = c(demographics, clickPredictors)
            namedPredictors = lapply (availPredictors, lookupVars)
            names (availPredictors) = namedPredictors
            
            updateSelectInput (session
                             , input    = "selectPredictor"
                             , choices  = availPredictors
                             , selected = "gender")
        }
        else if (input$selectOutcome %in% scoreOutcomesW4) {
            clickPredictors = paste ("allW", 0:4, sep='')
            availPredictors = c(demographics, scoreOutcomesW1, clickPredictors)
            namedPredictors = lapply (availPredictors, lookupVars)
            names (availPredictors) = namedPredictors
          
            updateSelectInput (session
                             , input    = "selectPredictor"
                             , choices  = availPredictors
                             , selected = "gender")
        }
        else if (input$selectOutcome %in% scoreOutcomesW7) {
            clickPredictors = paste ("allW", 0:7, sep='')
            availPredictors = c(demographics
                                      , scoreOutcomesW1, scoreOutcomesW4
                                      , clickPredictors)
            namedPredictors = lapply (availPredictors, lookupVars)
            names (availPredictors) = namedPredictors
          
            updateSelectInput (session
                             , input    = "selectPredictor"
                             , choices  = availPredictors
                             , selected = "gender")
        }
        else if (input$selectOutcome == "final_result") {
            clickPredictors = paste ("allW", 0:12, sep='')
            availPredictors = c(demographics
                              , scoreOutcomesW1, scoreOutcomesW4, scoreOutcomesW7
                              , clickPredictors)
            namedPredictors = lapply (availPredictors, lookupVars)
            names (availPredictors) = namedPredictors
          
            updateSelectInput (session
                             , input    = "selectPredictor"
                             , choices  = availPredictors
                             , selected = "gender")
        }
        else
            updateSelectInput (session
                             , input    = "selectPredictor"
                             , choices  = demographics
                             , selected = "gender")
    })
    
    studentInfoWeeklyR = reactive (studentInfoWeekly)
    
    studentVleDailyR = reactive ({
        studentVleDaily %>%
            filter (activity_type %in% input$selectActivity) %>%
            filter (code_presentation == input$selectPresentation
                  , code_module       == input$selectModule
                  , date >= input$selectTimeSeriesRange[[1]]
                  , date <= input$selectTimeSeriesRange[[2]]) %>%
            ungroup %>%
            group_by (code_module, code_presentation, activity_type) %>%
            arrange (desc (date)) %>% mutate (label = if_else (row_number () == 1, activity_type, ""))
    })
    studentVleWeeklyR = reactive ({
        studentVleWeekly %>%
            filter (activity_type %in% input$selectActivity) %>%
            filter (code_presentation == input$selectPresentation
                  , code_module       == input$selectModule
                  , weekNo >= input$selectTimeSeriesRange[[1]]
                  , weekNo <= input$selectTimeSeriesRange[[2]]) %>%
            ungroup %>%
            group_by (code_module, code_presentation, activity_type) %>%
            arrange (desc (weekNo)) %>% mutate (label = if_else (row_number () == 1, activity_type, ""))
    })
    
    assessmentsR = reactive ({
        if (input$selectTimeSeries == "day") {
            assessmentsWithCourses %>%
                filter (code_presentation == input$selectPresentation
                      , code_module       == input$selectModule
                      , date >= input$selectTimeSeriesRange[[1]]
                      , date <= input$selectTimeSeriesRange[[2]])
        } else if (input$selectTimeSeries == "week") {
            assessmentsWithCourses %>%
                filter (code_presentation == input$selectPresentation
                      , code_module       == input$selectModule
                      , week >= input$selectTimeSeriesRange[[1]]
                      , week <= input$selectTimeSeriesRange[[2]])
        }
    })
    
    output$plotArea <- renderPlotly ({
        if (input$selectMode == "clicks") {
            if (input$selectTimeSeries == "week" && nrow (studentVleWeeklyR ()) != 0) {
                if (input$selectAggregate == "mean") {
                    p = ggplot (data = studentVleWeeklyR ()
                              , mapping = aes (x = weekNo, y = mean
                                             , ymin = mean - sd, ymax = mean + sd
                                             , colour = activity_type)) +
                        geom_line () + #geom_point () +
                        geom_text (aes (label = label), nudge_x = 1) +
                        labs (title = "Clicks week by week") + 
                        xlab ("Week number") + 
                        ylab ("Mean of clicks") +
                        theme (legend.position = "none") +
                        geom_vline (data = assessmentsR ()
                                  , aes (xintercept = week, ymin = 0, ymax = 0)) +
                        geom_text (data = assessmentsR ()
                                 , aes (x = week, y = -1, label = assessmentLabel
                                      , colour = "black", ymin = 0, ymax = 0)
                                 , nudge_y = -1)
                }
                else if (input$selectAggregate == "median") {
                    p = ggplot (data = studentVleWeeklyR ()
                              , mapping = aes (x = weekNo, y = median
                                             , ymin = q1, ymax = q3
                                             , colour = activity_type)) +
                        geom_line () + #geom_point () + 
                        geom_text (aes (label = label), nudge_x = 1) +
                        labs (title = "Clicks week by week") + 
                        xlab ("Week number") + 
                        ylab ("Median of clicks") +
                        theme (legend.position = "none") +
                        geom_vline (data = assessmentsR ()
                                  , aes (xintercept=week, ymin=0, ymax=0)) +
                        geom_text (data = assessmentsR ()
                                 , aes (x = week, y = -1, label = assessmentLabel
                                      , colour = "black", ymin = 0, ymax = 0)
                                 , nudge_y = -1)
                }
            }
            else if (input$selectTimeSeries == "day" && nrow (studentVleDailyR ()) != 0) {
                if (input$selectAggregate == "mean") {
                    p = ggplot (data = studentVleDailyR ()
                              , mapping = aes (x = date, y = mean
                                             , ymin = mean - sd, ymax = mean + sd
                                             , colour = activity_type)) +
                        geom_line () + #geom_point () +
                        geom_text (aes (label = label), nudge_x = 1) +
                        labs (title = "Clicks day by day") + 
                        xlab ("Date") + 
                        ylab ("Mean of clicks") +
                        theme (legend.position = "none") +
                        geom_vline (data = assessmentsR ()
                                  , aes (xintercept = date, ymin = 0, ymax = 0)) +
                        geom_text (data = assessmentsR ()
                                 , aes (x = date, y = -1, label = assessmentLabel
                                      , colour = "black", ymin = 0, ymax = 0)
                                 , nudge_y = -1)
                  }
                else if (input$selectAggregate == "median") {
                  p = ggplot (data = studentVleDailyR ()
                              , mapping = aes (x = date, y = median
                                             , ymin = q1, ymax = q3
                                             , colour = activity_type)) +
                    geom_line () + #geom_point () + 
                    geom_text (aes (label = label), nudge_x = 1) +
                    labs (title = "Clicks day by day") + 
                    xlab ("Date") + 
                    ylab ("Median of clicks") +
                    theme (legend.position = "none") +
                    geom_vline (data = assessmentsR ()
                              , aes (xintercept=date, ymin=0, ymax=0)) +
                    geom_text (data = assessmentsR ()
                             , aes (x = date, y = -1, label = assessmentLabel
                                  , colour = "black", ymin = 0, ymax = 0)
                             , nudge_y = -1)
                }
            }
            if (input$showPopDist)
                pp = p + geom_ribbon (alpha = 0.3)
            else 
                pp = p   
            suppressWarnings (ggplotly (pp) %>% layout (font = list (family = "Arial")))
        }
        else if (input$selectMode == "outcomes") {
            isCatPred = function (x)
                x %in% c ("gender", "region", "highest_education"
                        , "imdModified", "age_band", "disability") ||
                any (startsWith (x, c("SubStatus", "ScoreQ")))
            isCatOut = function (x)
                x == "final_result" || any (startsWith (x, c("SubStatus", "ScoreQ")))
          
            if (isCatPred (input$selectPredictor) && isCatOut (input$selectOutcome)) {
                p = ggplot (data = studentInfoWeeklyR ()) +
                    geom_mosaic (mapping = aes (x = product (!!sym (input$selectPredictor)), fill = !!(sym (input$selectOutcome)))) +
                    theme (axis.text.x = element_text (angle = 45, hjust = 1))
                ggplotly (p) %>% layout (font = list (family = "Arial")
                                       , xaxis = list (title = lookupVars (input$selectPredictor))
                                       , yaxis = list (title = lookupVars (input$selectOutcome)))
            }
            else if (!isCatPred (input$selectPredictor) && isCatOut (input$selectOutcome)) {
                p = ggplot (data = studentInfoWeeklyR ()
                          , mapping = aes_string (y = input$selectPredictor, x = input$selectOutcome)) +
                    geom_boxplot () + coord_flip () + # Apparent bug in ggplotly()
                    theme (axis.text.x = element_text (angle = 45, hjust = 1)) +
                    ylab (lookupVars (input$selectPredictor)) + xlab (lookupVars (input$selectOutcome))
                ggplotly (p) %>% layout (font = list (family = "Arial"))
            }
            else if (isCatPred (input$selectPredictor) && !isCatOut (input$selectOutcome)) {
                p = ggplot (data = studentInfoWeeklyR ()
                          , mapping = aes_string (x = input$selectPredictor, y = input$selectOutcome)) +
                    geom_boxplot () +
                    theme (axis.text.x = element_text (angle = 45, hjust = 1)) +
                    xlab (lookupVars (input$selectPredictor)) + ylab (lookupVars (input$selectOutcome))
                ggplotly (p) %>% layout (font = list (family = "Arial"))
            }
            
            else if (!isCatPred (input$selectPredictor) && !isCatOut (input$selectOutcome)) {
                p = ggplot (data = studentInfoWeeklyR ()
                          , mapping = aes_string (x = input$selectPredictor, y = input$selectOutcome)) +
                    geom_point () +
                    theme (axis.text.x = element_text (angle = 45, hjust = 1))  +
                    xlab (lookupVars (input$selectPredictor)) + ylab (lookupVars (input$selectOutcome))
                ggplotly (p) %>% layout (font = list (family = "Arial"))
            }
        }
    })
}

shinyApp(ui = ui, server = server)
