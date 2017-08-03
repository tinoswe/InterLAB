#' Check size of input measurements
#'
#' Takes in any numeric array and prints out the size of the array.
#' @param x A numeric array
#' @return sz the size of the array
#' @export
array_size <- function(x){
  return(length(x))
}

#' Get z-score
#'
#' Takes in a single number, the mean and the sigma and calculates the z-score
#' @param x A number
#' @param mu Mean
#' @param sigma Sigma
#' @return z The z-score
#' @export
z_score <- function(x, mu, sigma){
  z <- (x-mu)/sigma
  return(z)
}

#' Make scatterplot with errorbars
#'
#' Takes in an array of means, an array of labels and an array of uncertainties and makes scatterplot with errorbars
#' @param x_arrs An array of numbers
#' @param x_labs An array of x-labels
#' @param x_u An array of uncertainties
#' @param fldr Folder name where to save the chart
#' @export
get_scatterplot <- function(x_arrs,
                            x_labs,
                            x_u,
                            fname,
                            fldr){

  png(filename=paste(fldr,fname,sep="/"),
      width = 10,
      height = 8,
      units = 'in',
      res = 300)

  x_i <- seq(1,
             length(x_arrs),
             by=1)

  d = data.frame(
    x  = x_i,
    y  = x_arrs,
    xl = x_labs,
    sd = x_u)


  plot(d$x,
       d$y,
       pch=20,
       col="black",
       xlim=c(0,length(d$x)+1),
       ylim=c(mean(d$y) - 4*(sd(d$y)),
              mean(d$y) + 4*(sd(d$y))),
       #bty="n",
       axes="F",
       xlab = "",
       ylab = "")

  axis(1,
       at = d$x,
       labels = d$xl)

  axis(2)

  arrows(d$x,
         d$y - 0.5*d$sd,
         d$x,
         d$y + 0.5*d$sd,
         length=0.02,
         angle=90,
         code=3)

  dev.off()

}

#' Get list of files in folder
#'
#' Takes in the folder name and gets all Excel files in it
#' @param folder Folder name
#' @return flist List of files in folder
#' @export
get_files_from_folder <- function(folder){
  flist <- list.files(folder,
                      pattern = "*.xlsx")
  return(flist)
}

#' Read xlsx files and store data
#'
#' Stores all data into appropriate R object
#' @return flist List of files in folder
#' @export
make_data_frame <- function(){

  library(readxl)

  folder_path <- "/Users/olivo.martino/Desktop/DatiEsempio"
  flist <- get_files_from_folder(folder_path)

  df <- data.frame(lab = character(),
                   cal1 = as.numeric(),
                   cal2 = as.numeric(),
                   cal3 = as.numeric(),
                   cal4 = as.numeric(),
                   f_bpa_1 = as.numeric(),
                   f_bpa_2 = as.numeric(),
                   v_bpa_1 = as.numeric(),
                   v_bpa_2 = as.numeric(),
                   stringsAsFactors = FALSE)
  i <- 1
  for (f in flist){

    f_path <- paste(folder_path,
                    f,
                    sep="/")

    labname <- read_excel(f_path,
                          range = "E12",
                          col_names = c("lname"))
    df[i,1] <- labname$lname

    cal_vals <- read_excel(f_path,
                           range = "E18:H18",
                           col_names = c("c1","c2","c3","c4"))

    df[i,2] <- cal_vals$c1
    df[i,3] <- cal_vals$c2
    df[i,4] <- cal_vals$c3
    df[i,5] <- cal_vals$c4

    fish_bpa_1 <- read_excel(f_path,
                           range = "E22",
                           col_names = c("fish_bpa_1"))
    df[i,6] <- fish_bpa_1$fish_bpa_1

    fish_bpa_2 <- read_excel(f_path,
                             range = "G22",
                             col_names = c("fish_bpa_2"))
    df[i,7] <- fish_bpa_2$fish_bpa_2

    vegs_bpa_1 <- read_excel(f_path,
                             range = "E26",
                             col_names = c("vegs_bpa_1"))
    df[i,8] <- vegs_bpa_1$vegs_bpa_1

    vegs_bpa_2 <- read_excel(f_path,
                             range = "G26",
                             col_names = c("vegs_bpa_2"))
    df[i,9] <- vegs_bpa_2$vegs_bpa_2


    i <- i +1
  }

  return(df)
}

#' Get simple mu and sigma
#'
#' Takes in an array of numbers and returns mu and sigma
#' @param arr_in An array of numbers
#' @return arr_out An two elements array: mu and sigma
#' @export
get_mu_sigma <- function(arr_in){

  mu <- median(arr_in)
  sigma <- sd(arr_in)
  arr_out <- c(mu, sigma)
  return(arr_out)
}

#' Makes bar plot
#'
#'takes in variable name and makes barplot
#' @param df frame
#' @param var_name var name as string
#' @export
make_bar_plot <- function(df, var_name){

  #folder where to save charts
  out_folder <- "/Users/olivo.martino/Desktop/DatiEsempio"

  png(filename=paste(out_folder,
                     paste(var_name,".png",sep=""),
                     sep="/"),
      width = 10,
      height = 8,
      units = 'in',
      res = 300)

  df <- df[,c("lab",var_name)]

  zs <- z_score(x=df[,var_name],
                mu=median(df[,var_name]),
                sigma=sd(df[,var_name]))
  df[,"zs"] <- zs

  df <- df[order(df[3]),]

  barplot(df[,"zs"],
          names.arg = df[,"lab"],
          ylim=c(-3,3),
          space=2)
  title(main=var_name,
        ylab = "z-score")
  abline(h=0,
         col="black")
  dev.off()
  }


#' Makes it all
#'
#' Does all
#' @export
make_all <- function(){

  #read input and build dataframe
  df <- make_data_frame()

  #barplots of columns z-scores
  make_bar_plot(df,"cal1")
  make_bar_plot(df,"cal2")
  make_bar_plot(df,"cal3")
  make_bar_plot(df,"cal4")
  make_bar_plot(df,"f_bpa_1")
  make_bar_plot(df,"f_bpa_2")
  make_bar_plot(df,"v_bpa_1")
  make_bar_plot(df,"v_bpa_2")

  #get_scatterplot(x_arrs = df$cal1,
  #                x_labs = df$lab,
  #                x_u = sqrt(df$cal1)/12,
  #                fname = "test.png",
  #                fldr = out_folder)

}

#install.packages("gridExtra")
#library(gridExtra)
#out_folder <- "/Users/olivo.martino/Desktop/DatiEsempio"
#pdf(paste(out_folder,"data_output.pdf",sep="/"),
#    height=11,
#    width=8.5)
#grid.table(df)
#dev.off()
