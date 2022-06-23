library(data.table)
library(ggplot2)
theme_set(theme_classic())
library(scales)
library(lme4)
library(emmeans)

process_demodat <- function(path){
  # process demographic data from pp_list
  pp_list <- fread(paste0(path, "/pp_list.csv"))
  files <- Sys.glob(paste0(path, "/subject*"))
  dat <- NULL
  for(f in files){
    cat(f, "\n")
    dat <- rbind(dat, fread(f, nrows = 1))
  }
  return(dat)
}

process_timingdat <- function(path){
  # process timing data from .txt files
  pp_list <- fread(paste0(path, "/pp_list.csv"))
  files <- Sys.glob(paste0(path, "/Timing*"))
  narrow_list <- fread(paste0(path, "/narrow_onsets.csv"))
  
  dat <- NULL
  for(f in files){
    cat(f, "\n")
    tempdat <- fread(f, skip = 16)
    tempdat <- merge(tempdat, narrow_list[, .(TrialNr = trial, narrow)], by = "TrialNr")
    dat <- rbind(dat, tempdat , fill = T)
  }
  setnames(dat, c("TrialNr", "D_ResponseTime", "D_OnsetTime", "D_Score", "D_CueTime"), 
           c("trial", "rt", "onsettime", "total_score", "t_cue"))
  dat[, onsettime := as.integer(onsettime)]
  dat[, offsettime := onsettime + 10]
  dat[, t_narrow := t_cue - onsettime]
  dat[, t_response := t_cue + rt]
  dat[, t_bias := rt - 10]
  dat <- merge(dat, pp_list[, .(BlockNr, pp_id)])
  dat[, outlier := ifelse(rt %between% c(median(rt) - 3 * mad(rt), median(rt) + 3 * mad(rt)), 0, 1), by = pp_id]
  dat[, BlockNr := as.integer(BlockNr)]
  dat[, block := as.integer(ifelse(BlockNr %% 2 == 0, 2, 1))]
  dat[, trial := trial + 56 * (block - 1)]
  return(dat)
}

process_drivingdat <- function(path, tdat){
  
  # Process driving data from dat files
  # REMEMBER TO FIX HEADERS MANUALLY :-(
  
  pp_list <- fread(paste0(path, "/pp_list.csv"))
  files <- Sys.glob(paste0(path, "/*.dat"))
  narrow_list <- fread(paste0(path, "/narrow_onsets.csv"))
  onsets <- narrow_list$m_abs_narrow_onset
  
  dat <- NULL
  for(df in files){
    cat(df, "\n")
    Bnr <- as.integer(strsplit(df, "_")[[1]][2])
    tempdat <- fread(df)
    setnames(tempdat, make.names(names = names(tempdat), unique=TRUE))
    tempdat[, BlockNr := Bnr]
    tempdat[, velocity := velocity * 3.6]
    
    # t_onset keeps track of time elapsed since start of narrow (or "intersection")
    for(i in 1:length(onsets)){
      # get time points at which narrows start
      t <- tempdat[which.min(abs(tempdat$xpos - onsets[i])), time]
      # mark samples relative to the start of the narrows
      tempdat[time %between% c(t - 6, t + 14), t_onset := time - t]
      tempdat[time %between% c(t - 6, t + 14), narrow_id := i]
    }
    
    # Add whether there actually was a physical narrow
    tempdat <- merge(tempdat, narrow_list[, .(narrow_id = trial, narrow)], by = "narrow_id", all = T)
    
    # t_response keeps track of time elapsed since response
    t_responses <- tdat[BlockNr == Bnr, t_response]
    for(i in 1:length(t_responses)){
      t <- t_responses[i]
      tempdat[time %between% c(t - 6, t + 14), t_response := time - t]
      tempdat[time %between% c(t - 6, t + 14), response_id := i]
    }
    setkey(tempdat, time)
    
    dat <- rbind(dat, tempdat)
  }
  dat <- merge(dat, pp_list[, .(BlockNr, pp_id)])
  dat[, t_onset := round(t_onset, digits = 1)]
  dat[, t_response := round(t_response, digits = 1)]
  dat[, time := round(time, digits = 1)]
  dat[, experiment := tail(strsplit(path, "/")[[1]], 1)]
  dat[, is_narrow := ifelse(dfrint < 83.4 & narrow == T, T, F)]
  dat[, block := ifelse(BlockNr %% 2 == 0, 2, 1)]
  dat[, trial := narrow_id + 56 * (block - 1)]
  dat <- merge(dat, 
               dat[t_onset %between% c(-5, -3), .(baseline = mean(velocity)), by = .(pp_id, trial)],
               by = c("pp_id", "trial"))
  dat[, velocity.bl := velocity - baseline]
  return(dat)
}