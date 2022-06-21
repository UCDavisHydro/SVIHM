
#-------------------------------------------------------------------------------------------------#

calc_split_sample_stats <- function(dates, sim, obs, split_date, FUNs, FUN_names) {
  if (length(dates) != length(sim) | length(sim) != length(obs)) {
    stop('Arrays for dates, sim, and obs all must be the same length.')
  }
  if (!typeof(FUNs)=='list') { FUNs <- list(FUNs)}
  i <- 1
  res <- lapply(FUNs, function(f) {
    fname <- FUN_names[i]
    all <- f(sim, obs)
    pre_stat <- f(sim[dates < split_date], obs[dates < split_date])
    pst_stat <- f(sim[dates >= split_date], obs[dates >= split_date])
    i <<- i + 1
    return(data.frame('Statistic' = fname, 'All' = all, 'Pre' = pre_stat, 'Post' = pst_stat))
  })
  res <- do.call(rbind, res)
  return(res)
}

#-------------------------------------------------------------------------------------------------#

calc_split_sample_stats.grouped <- function(dates, sim, obs, groups, split_date, FUNs, FUN_names) {
  res <- list()
  for (grp in unique(groups)) {
    grpd <- dates[groups == grp]
    grps <- sim[groups == grp]
    grpo <- obs[groups == grp]
    grpr <- calc_split_sample_stats(grpd, grps, grpo, split_date, FUNs, FUN_names)
    grpr$Group <- grp
    res <- append(res, list(grpr))
  }
  res <- do.call(rbind, res)
  return(res)
}

#-------------------------------------------------------------------------------------------------#

print.split_sample_stats <- function(sampledf, group, side) {
  strout <- paste(group,'-')
  grpdf <- sampledf[sampledf$Group == group,]
  for (stat in grpdf$Statistic) {
    strout <- paste0(strout, '  ', stat, ' = ', round(grpdf[grpdf$Statistic == stat, side],3))
  }
  return(strout)
}

#-------------------------------------------------------------------------------------------------#
