library(taskscheduleR)

taskscheduler_create(taskname = "my_r_job", 
                     rscript = "C:/Users/이정빈/Desktop/practice_r/analysis_document.R", 
                     schedule = "DAILY", 
                     starttime = "09:00", 
                     modifier = 1)