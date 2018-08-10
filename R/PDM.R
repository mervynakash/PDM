#' @title Imputes missing values
#' @description Imputes missing values in the data frame for both categorical and numerical variables.
#' @param df,threshold
#' @return df
#' @export pdm_impute

pdm_impute <- function(df,threshold = 20){

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  miss_col <- colSums(is.na(df))

  miss_col <- (miss_col[miss_col > 0])

  miss_col <- miss_col[order(miss_col)]

  miss_perc <- round(miss_col/nrow(df)*100,2)
  col_perc <- miss_perc[miss_perc > threshold]
  miss_col_names <- names(col_perc)

  df[,c(miss_col_names)] <- NULL

  miss_col <- miss_col[!names(miss_col) %in% miss_col_names]

  # str(df)

  fact_col <- sapply(df, is.factor)

  fact <- names(fact_col[fact_col])
  num <- names(fact_col[!fact_col])

  no_unique <- sapply(df[,c(num)], function(x){length(unique(x))})

  continuous_col <- names(no_unique[no_unique > 20])

  factor_col <- names(no_unique[no_unique <= 20])

  factor_col <- c(factor_col, fact)

  miss_col_names <- names(miss_col)

  miss_val <- c()

  miss_val <- ifelse(miss_col_names %in% factor_col, c(miss_val,1), c(miss_val,2))
  names(miss_val) <- miss_col_names

  miss_val_cont <- names(miss_val[miss_val == 2])

  miss_val_fact <- names(miss_val[miss_val == 1])


  if(length(miss_val_cont) > 1){
    sw <- sapply(df[,c(miss_val_cont)], norm_test)
  } else {
    sw_test <- shapiro.test(df[,c(miss_val_cont)])
    sw <- sw_test$p.value
  }

  impute_factor <- ifelse(sw > 0.05, 1, 2)


  cat_df <- df[,c(factor_col)]

  if(is.data.frame(cat_df)){
    freq_diff_cat <- sapply(cat_df[which(!is.na(cat_df)),], optimal_table)
    freq_diff_cat <- freq_diff_cat[order(freq_diff_cat)]
    optimal_col <- names(freq_diff_cat[1])
  } else if (is.vector(cat_df)){
    optimal_col <- factor_col
  } else if(is.null(cat_df)){
    optimal_col <- NULL
  }

  miss_val_dupl <- miss_val

  df[,c(factor_col)] <- data.frame(sapply(df[,c(factor_col)], as.factor))

  # df2 <- df
  # df <- df2

  ID <- df$Id
  df$Id <- NULL

  if(optimal_col %in% names(miss_val)){
    df <- categorical_impute(df, optimal_col, colnames(df)[!colnames(df) %in% c(names(miss_val_dupl),optimal_col)])
    miss_val <- miss_val[!names(miss_val) %in% optimal_col]
  }


  k <- 1
  for(i in 1:length(miss_val)){
    if(miss_val[i] == 2){
      print(names(miss_val[i]))
      df <- continuous_impute(df,miss_val[i],optimal_col,impute_factor[k])
      miss_val_dupl <- miss_val_dupl[!names(miss_val_dupl) %in% names(miss_val[i])]
      k = k + 1
    } else {
      print(names(miss_val[i]))
      df <- categorical_impute(df,names(miss_val[i]), colnames(df)[!colnames(df) %in% names(miss_val_dupl)])
      miss_val_dupl <- miss_val_dupl[!names(miss_val_dupl) %in% names(miss_val[i])]
    }
  }

  non_cont <- factor_col[!factor_col %in% fact]

  # new_df <- df
  # df <- new_df

  df[,non_cont] <- data.frame(sapply(df[,non_cont], function(x){as.numeric(as.character(x))}))
  df$Id <- ID

  return(df)
}



categorical_impute <- function(df,target, predictors){

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  form <- as.formula(paste(target,".",sep = "~"))
  if(length(predictors) == 0){
    train <- df[which(!is.na(df[,target])),]
    test <- df[which(is.na(df[,target])),]
  } else {
    train <- df[which(!is.na(df[,target])),c(predictors,target)]
    test <- df[which(is.na(df[,target])),c(predictors)]
  }
  model <- rpart::rpart(form,train,control = rpart.control(cp = 0))
  cptable <- as.data.frame(model$cptable)
  cpval <- cptable$CP[which(cptable$nsplit < ncol(train) & cptable$xerror == min(cptable$xerror))]
  model_new <- rpart::prune(model, cp = cpval)
  pred <- predict(model_new, test, type = "class")
  df[which(is.na(df[,target])),target] <- pred
  return(df)
}


continuous_impute <- function(df, miss, optimal, impute_factor){

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  if(is.null(optimal)){
    if(impute_factor == 1){
      df[which(is.na(names(miss))),names(miss)] <- mean(df[,names(miss)],na.rm = T)
    } else {
      df[which(is.na(names(miss))),names(miss)] <- median(df[,names(miss)],na.rm = T)
    }
  } else {
    if(impute_factor == 1){
      freq_tab <- as.data.frame(aggregate(df[,names(miss)],by = list(df[,optimal]), mean, na.rm = T))
      colnames(freq_tab) <- c("Optimal","Missing")
      for(j in which(is.na(df[,names(miss)]))){
        optimal_freq_val <- df[j,optimal]
        df[j,names(miss)] <- freq_tab$Missing[which(freq_tab$Optimal == optimal_freq_val)]
      }
    } else {
      freq_tab <- as.data.frame(aggregate(df[,names(miss)],by = list(df[,optimal]), median, na.rm = T))
      colnames(freq_tab) <- c("Optimal","Missing")
      for(j in which(is.na(df[,names(miss)]))){
        optimal_freq_val <- df[j,optimal]
        df[j,names(miss)] <- freq_tab$Missing[which(freq_tab$Optimal == optimal_freq_val)]
      }
    }
  }
  return(df)
}

norm_test <- function(x){

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  sw <- shapiro.test(x)
  return(sw$p.value)
}

optimal_table <- function(x){

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  tab <- table(x)
  freq_tab <- round(tab/sum(tab),5)
  max_tab <- max(freq_tab)
  min_tab <- min(freq_tab)
  diff_tab <- max_tab - min_tab
  return(diff_tab)
}

