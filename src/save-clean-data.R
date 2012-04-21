
saveFile <- function(df, fileName, bz = FALSE) {
  if(!bz)
    write.csv(df, file.path("clean-data", fileName), row.names = FALSE)
  else
    write.csv(df, bzfile(file.path("clean-data", fileName)), row.names = FALSE)
}

saveFile(div.df, "divorces-federal-district.csv")
saveFile(marriages.df, "marriages-federal-district.csv")
saveFile(marriage.duration.df, "marriage-duration-federal-district.csv")
saveFile(marriage.duration, "marriage-duration-all-mx.csv")
saveFile(div.df.state, "divorces-federal-district-by-marriage-state.csv")
saveFile(marriage.duration.state, "marriage-duration-by-state.csv.bz2", bz = TRUE)
