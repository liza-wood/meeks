make.duplicate.for2 <- function(df, name1, name2, finalname){
  duplicated.df <- data.table()
  for (i in df$row){
    if(length(df$row[df$row == i]) == 2 && 
       df$match.name[df$row == i] == 
       c(name1, name2)){
      new.matches <- c(finalname, finalname)
      row <- i
    }  else{
      new.matches <- df$match.name[df$row == i]
      row <- i
    }
    dup.df <- data.table(new.matches, row)
    duplicated.df <- rbind(dup.df, duplicated.df, fill = T)
    assign("duplicated.df", duplicated.df, envir = .GlobalEnv)
  }
}

clean.for.next.input <- function(){
  print(nrow(duplicated.df))
  duplicated.df <- unique(duplicated.df)
  print(nrow(duplicated.df))
  exact.match.dup <- left_join(exact.agency.match.u, duplicated.df, by = "row")
  print(nrow(exact.match.dup))
  exact.match.dup <- select(exact.match.dup, -match.name)
  exact.match.dup.u <- unique(exact.match.dup)
  print(nrow(exact.match.dup.u))
  doubles <- exact.match.dup.u %>% group_by(row) %>% count() %>% filter(n > 1) %>% select(-n)
  print(nrow(doubles))
  doubles.df <- exact.match.dup.u %>% filter(row %in% doubles$row) # 1302
  print(nrow(doubles.df))
  exact.agency.match <- rename(exact.match.dup.u, "match.name" = "new.matches")
  exact.agency.match.u <- unique(exact.agency.match)
  print(nrow(exact.agency.match.u)) 
  assign("duplicated.df", duplicated.df, envir = .GlobalEnv)
  assign("exact.match.dup", exact.match.dup, envir = .GlobalEnv)
  assign("exact.match.dup.u", exact.match.dup.u, envir = .GlobalEnv)
  assign("doubles", doubles, envir = .GlobalEnv)
  assign("doubles.df", doubles.df, envir = .GlobalEnv)
  assign("exact.agency.match", exact.agency.match, envir = .GlobalEnv)
  assign("exact.agency.match.u", exact.agency.match.u, envir = .GlobalEnv)
}

make.duplicate.for3 <- function(df, name1, name2, name3, finalname){
  duplicated.df <- data.table()
  for (i in df$row){
    if(length(df$row[df$row == i]) == 3 && 
       df$match.name[df$row == i] == 
       c(name1, name2, name3)){
      new.matches <- c(finalname, finalname, finalname)
      row <- i
    }  else{
      new.matches <- df$match.name[df$row == i]
      row <- i
    }
    dup.df <- data.table(new.matches, row)
    duplicated.df <- rbind(dup.df, duplicated.df, fill = T)
    assign("duplicated.df", duplicated.df, envir = .GlobalEnv)
  }
}

make.duplicate.for5 <- function(df, name1, name2, name3, name4, name5, finalname){
  duplicated.df <- data.table()
  for (i in df$row){
    if(length(df$row[df$row == i]) == 5 && 
       df$match.name[df$row == i] == 
       c(name1, name2, name3, name4, name5)){
      new.matches <- c(finalname, finalname, finalname, finalname, finalname)
      row <- i
    }  else{
      new.matches <- df$match.name[df$row == i]
      row <- i
    }
    dup.df <- data.table(new.matches, row)
    duplicated.df <- rbind(dup.df, duplicated.df, fill = T)
    assign("duplicated.df", duplicated.df, envir = .GlobalEnv)
  }
}

