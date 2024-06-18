####Importing libraries -----------
packages <-
  c(
    "dplyr",
    "readxl",
    "rjson",
    "haven",
    "tools",
    "matlib",
    "ggplot2",
    "tidyr",
    "grid",
    "gridExtra",
    "jsonlite"
  )
# Loop through each package and check if it's installed
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    # If it's not installed, install it
    install.packages(pkg)
  }
  # Load the package
  library(pkg, character.only = TRUE)
}


#Taking the path of the file from the user and extracting the extension------
new_file <-
  readline(prompt = "Enter the path of file ,Note that we seprate between each directory by /: ")
new_file <- as.character(new_file)
file_ext <-  file_ext(new_file)


###Confidence Intervals on the Regression Coefficients ------------
CI_Beta <- function(X, Y, Beta, C, p, n, MSE) {
  #take alpha from the user
  alpha <-
    readline(prompt = "Enter the significance level for the Regression Coefficients interval : ")
  alpha <- as.numeric(alpha)
  
  #take the index of the β in the matrix (ii)from the user
  indexR <- readline(prompt = "Enter the Row index : ")
  indexR <- as.numeric(indexR)
  
  
  #calc t value from the table
  t_table = qt(1 - (alpha / 2), n - p, lower.tail = TRUE)
  
  #select the desired β and  it's opposite C
  βHat = Beta[indexR, 1]
  
  #Cmatrix=C
  CHat = C[indexR, indexR]
  
  #print the data
  lowerBound <- βHat - (t_table * sqrt(MSE * CHat))
  upperBound <- βHat + (t_table * sqrt(MSE * CHat))
  
  β_interval <- paste(lowerBound, upperBound, sep = " , ")
  print(β_interval)
  
}






#CI Estimation of the Mean Response--------
CI_MR <- function(X, Y, p, n, MSE, C) {
  #take alpha from the user
  alpha <-
    readline(prompt = "Enter the significance level for the Mean Response interval : ")
  alpha <- as.numeric(alpha)
  
  #calc t value from the table
  t_table = qt(1 - (alpha / 2), n - p, lower.tail = TRUE)
  
  #take the x node vector from the user
  nrows <- readline(prompt = "Enter the number of rows for X node: ")
  
  
  # Read the values from the console as a single vector
  values <- scan("", what = numeric(), n = as.integer(nrows))
  
  # Convert the vector to a matrix with 1 column
  Xnode <- matrix(values, nrow = as.integer(nrows), ncol = 1)
  
  
  #calc the data which are under the root
  XnodeT <- as.matrix(t(Xnode))
  val_under_sqrt = MSE %*% (XnodeT %*% C %*% Xnode)
  
  YHat = XnodeT %*% Beta
  
  
  #print the data
  lowerBound <- YHat - (t_table * sqrt(val_under_sqrt))
  upperBound <- YHat + (t_table * sqrt(val_under_sqrt))
  MR_interval <- paste(lowerBound, upperBound, sep = " , ")
  print(MR_interval)
  
  
}




#CI Estimation of new observation--------
CI_NO <- function(X, Y, p, n, MSE, C) {
  #take alpha from the user
  alpha <-
    readline(prompt = "Enter the significance level for the Mean Response interval : ")
  alpha <- as.numeric(alpha)
  
  #take the x node vector from the user
  nrows <- readline(prompt = "Enter the number of rows for X nod: ")
  

  # Read the values from the console as a single vector
  values <- scan("", what = numeric(), n = as.integer(nrows))
  
  # Convert the vector to a matrix with 1 column
  Xnode <- matrix(values, nrow = as.integer(nrows), ncol = 1)
  
  #calc t value from the table
  t_table = qt(1 - (alpha / 2), n - p, lower.tail = TRUE)
  
  
  #calc the data which are under the root
  XnodeT <- as.matrix(t(Xnode))
  val_under_sqrt = MSE %*% (1 + (XnodeT %*% C %*% Xnode))
   
  YHat = XnodeT %*% Beta
  #print the data
  lowerBound <- YHat - (t_table * sqrt(val_under_sqrt))
  upperBound <- YHat + (t_table * sqrt(val_under_sqrt))
  New_Obs_interval <- paste(lowerBound, upperBound, sep = " , ")
  print(New_Obs_interval)
}





# Main multiple linear regression function--------
MLR <- function(df) {
  # Extracting dimensions of the data frame
  rows_dim <- dim(df)[1]
  col_dim <- dim(df)[2]
  
  # Extracting the target variable (Y) Independent
  print(names(df))
  Y_column_name <-
    readline(prompt = "Please enter the target (Y) column name: ")
  Y <- select(df, c(Y_column_name))
  Y <- Y[, Y_column_name]
  
  
  # Extracting all independent variables (X1..Xn) from the file and storing it in a matrix
  X_vars_names <-
    readline(prompt =
               " Plz enter names of independent variables(regressors):
               \n Warning: split variables by \'-\' ")
  X_vars_names <-
    strsplit(x = X_vars_names, split = '-', fixed = TRUE)[[1]]
  X_matrix <- select(df, c(X_vars_names))
  X = X_matrix[, X_vars_names]
  # Calculating degree of freedom , sample size and number of independent variables
  n <- dim(df)[1]
  k <- dim(X_matrix)[2]
  DF_total <- n - 1
  p <- k + 1
  DF_error <- n - p
  DF_reg <- k
  
  # adding a column of 1s to the matrix of independent variables
  X <- as.matrix(cbind(rep(1, times = rows_dim), X_matrix))
  
  # visualizing the relation between the dependent and independent variables
  par(mfrow = c(2, 1))
  plots <- list()
  for (i in 1:k) {
    q1 = qplot(y = Y, x = X[, i])
    plots[[i]] <- q1
  }
  
  # Calculating Y_bar
  Y_bar <- mean(Y)

  
  # Calculating X transpose
  X_trans <- t(X)
  
  # Calculating X transpose*X
  X_transX <- as.matrix(X_trans %*% X)
  typeof(X_transX)
  # Calculating inverse of ( X transpose*X )
  
  C <- as.matrix(matlib::inv(X_transX))
  # Calculating the Hat function
  hat <- X %*% C %*% X_trans
  
  # Calculating the beta matrix
  Beta <- C %*% X_trans %*% Y
  # Calculating Y_hat
  Y_hat <- X %*% Beta
  
  # Calculating errors
  e <- (Y - Y_hat)
  
  
  #visualization of normality
  ggplot(data = as.data.frame(e), aes(x = e, fill = 'red')) +
    geom_density(alpha = 0.6) +
    theme_bw() + labs(
      title = "Errors Normality test",
      subtitle = "If the dependent variable is normally distributed for a fixed set of predictor values, then the residual values
should be normally distributed with a mean of 0."
    )
  # Calculating sum of squares for error, total and regression
  SSE <- t(e) %*% e
  SST = sum((Y - Y_bar) ^ 2)
  SSR = SST - SSE
  # Calculating MSR and MSE
  MSR <- SSR / DF_reg
  MSE <- as.numeric(SSE / DF_error)
  # Calculating f statistic
  F_stat <- MSR / MSE
  #displaying  the data 
  tabData <- data.frame(
    Statistic <-
      c(
        "DF_total",
        "p",
        "DF_error",
        "DF_reg",
        "Ybar",
        "SSE",
        "SSR",
        "SST",
        "F_stat"
      ),
    
    value_of_stat <-
      c(
        DF_total,
        p,
        DF_error,
        DF_reg,
        Y_bar,
        SSE,
        SSR,
        SST,
        F_stat
      )
  )
  View(tabData)
  # Calculating f from the distribution table to do so we need to get alpha from the user
  alpha <- readline(prompt = "Enter the significance level for F test: ")
  alpha <- as.numeric(alpha)
  f_tabulated <- qf(1 - alpha, DF_reg, DF_error)
  
  # Conclusion of the hypothesis
  if (F_stat > f_tabulated) {
    print(
      "F > f_tabulated , ∴ we reject the H0 , in other words there is at least one of   βi isn't equal zero"
    )
  } else if (F_stat < f_tabulated) {
    print(
      "F < f_tabulated , ∴ we don't reject the H0, , in other words all the values of βi is equal zero"
    )
  }
  # Standardizing errors of our model ,
  # For example it can be used to compare it with other models errors
  standardized_errors = c()
  for (i in 1:n) {
    standardized_errors[i] = e[i] / sqrt(MSE)
    i <- +1
  }
  
  
  # Student error standardization , Studentized residuals
  studentized_errors = c()
  for (i in 1:n) {
    studentized_errors[i] = e[i] / sqrt((1 - hat[i, i]) * MSE)
    i = +1
  }
  plot(studentized_errors)
  
  # ANOVA table of the model
  anova_table <-
    data.frame(
      Source.Of.variation <- c("Between Groups", "Error", "Total"),
      sum_of_squares <- c(SSR, SSE, SST),
      degrees_of_freedom <-
        c(DF_reg, DF_error, DF_total),
      mean_squares <- c(MSR, MSE, "-"),
      F_value <-
        c(MSR / MSE, "-", "-")
    )
  # calculating the variance of Beta
  Beta_variance <- C * MSE
  
  #calculating the R square
  R_sqr <- 1 - (SSE / SST)
  View(anova_table)
  
  
  .GlobalEnv$X <- X
  .GlobalEnv$Y <- Y
  .GlobalEnv$Beta <- Beta
  .GlobalEnv$Bet_variance
  .GlobalEnv$C <- C
  .GlobalEnv$p <- p
  .GlobalEnv$n <- n
  
  CI_Beta(X, Y, Beta, C, p, n, MSE)
  CI_MR(X, Y, p, n, MSE, C)
  CI_NO(X, Y, p, n, MSE, C) 


  print("Program finished!")
}


#Identifying the type of the file then passing it to the SLR function as a df-------
if (file.exists(new_file)) {
  print("File imported successfully ")
  
  if (file_ext == "csv") {
    csv_file <- read.csv(new_file)
    df <- data.frame(csv_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in excel Sheet")
    }
    
  } else if (file_ext == "xlsx" ||
             file_ext == "xls") {
    # here we have two conditions because a csv file can have either of these two formats
    excel_file <- read_excel(new_file)
    df <- data.frame(excel_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in excel Sheet")
    }
    
  } else if (file_ext == "json") {
    json_file = rjson::fromJSON(file = new_file)
    df = as.data.frame(json_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in  Sheet")
    }
    
  } else if (file_ext == "sas7bdat") {
    sas_file <- read_sas(new_file)
    df = data.frame(sas_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in  Sheet")
    }
    
  } else if (file_ext == "sav") {
    sas_file <- read_spss(new_file)
    df = data.dta(sas_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in  Sheet")
    }
  } else if (file_ext == "dta") {
    sas_file <- read_sas(new_file)
    df = data.dta(sas_file)
    if (ncol(df)==2) {
      SLR(df)
    } else if (ncol(df)>2) {
      MLR(df)
    } else{
      print("Error in  Sheet")
    }
  }
  
  
} else{
  print("Invalid file path !")
}
