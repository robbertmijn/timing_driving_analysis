source("functions_preprocessing_driving_timing.R")
path <- "../experiment/data/exp1nocc/"

demo_dat <- process_demodat(path)
timing_dat <- process_timingdat(path)
driving_dat <- process_drivingdat(path, timing_dat)

save(timing_dat, driving_dat, demo_dat, file = paste0(format(Sys.time(), "%Y%m%d%H%M%S_timing_driving.rdata")))
