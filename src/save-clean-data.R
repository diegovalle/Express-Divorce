
saveFile <- function(df, fileName) {
  write.csv(df, file.path("clean-data", fileName), row.names = FALSE)
}

saveFile(div.df, "divorces-federal-district.csv")
saveFile(marriages.df, "marriages-federal-district.csv")
saveFile(marriage.duration.df, "marriage-duration-federal-district.csv")
saveFile(marriage.duration, "marriage-duration-all-mx.csv")
saveFile(div.df.state, "divorces-federal-district-by-marriage-state.csv")
