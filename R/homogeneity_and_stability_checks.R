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
                            fldr){
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
read_data <- function(){

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
