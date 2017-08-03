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
