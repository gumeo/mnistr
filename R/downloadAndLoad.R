#' Download the mnist data
#'
#' \code{download_mnist} downloads the mnist dataset into \code{./mnist}.
#'
#' The function creates the \code{mnist} directory in the specified directory \code{where},
#' downloads the dataset and unzips it. Use the function \code{\link{load_mnist}}
#' afterwards to load the dataset into the workspace. The dataset is downloaded
#' from \url{http://yann.lecun.com/exdb/mnist/}. Check out the webpage for
#' performance of various classifiers. It may depend on your platform whether
#' the dataset is successfully unzipped.
#'
#' @param where Path to where the dataset should be saved. Default is current
#'   working directory.
#'
#' @return Returns \code{TRUE} if successful.
#'
#' @examples
#'
#' \dontrun{
#' download_mnist('somePathToStore/')
#' load_mnist('samePathAsAbove/')
#' }
#' @export
download_mnist <- function(where = "./"){
  # Create the directory
  where <- paste(where,"mnist",sep='')
  dir.create(where)
  # Download the files
  trainImgs <- paste(where,'/train-images-idx3-ubyte.gz',sep='')
  download.file('http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz',
                destfile = trainImgs)
  trainLabs <- paste(where,'/train-labels-idx1-ubyte.gz',sep='')
  download.file('http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz',
                destfile = trainLabs)
  testImgs <- paste(where,'/t10k-images-idx3-ubyte.gz',sep='')
  download.file('http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz',
                destfile = testImgs)
  testLabs <- paste(where,'/t10k-labels-idx1-ubyte.gz',sep='')
  download.file('http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz',
                destfile = testLabs)
  # Unzip the downloaded files
  R.utils::gunzip(trainImgs)
  R.utils::gunzip(trainLabs)
  R.utils::gunzip(testImgs)
  R.utils::gunzip(testLabs)
  print(paste('Mnist dataset has been downloaded to: ', where))
  return(TRUE)
}

#' Load the mnist dataset into the workspace
#'
#' \code{load_mnist} Loads the mnist dataset as the lists trainSet and testSet into
#' the current workspace.
#'
#' Assuming that you have downloaded the dataset with the \code{\link{download_mnist}}
#' function, the \code{load_mnist} function loads the data into the lists trainSet and testSet.
#' Both lists have three elements named n, x and y. n is the number of samples, x is the
#' 28 by 28 images in rows and y is the label of the images. This function is inspired by
#' the gist: brendan o'connor - gist.github.com/39760 - anyall.org
#'
#' @param where Path to the folder containing the mnist directory. Default assumes
#'   that the current working directory contains the mnist folder.
#'
#' @return Returns \code{TRUE} if successful.
#'
#' @examples
#'
#' \dontrun{
#' download_mnist('somePathToStore/')
#' load_mnist('samePathAsAbove/')
#' # Check the number of elements in training set
#' trainSet$n
#' # Plot a random digit
#' (ggDigit(trainSet$x[sample(1:trainSet$n,1),]))
#' }
#' @export
load_mnist <- function(where = './') {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  if(!file.exists(paste(where,'mnist',sep=''))){
    stop(paste('There is no mnist directory in:',where))
  }
  trainSet <<- load_image_file('./mnist/train-images-idx3-ubyte')
  testSet <<- load_image_file('./mnist/t10k-images-idx3-ubyte')

  trainSet$y <<- load_label_file('./mnist/train-labels-idx1-ubyte')
  testSet$y <<- load_label_file('./mnist/t10k-labels-idx1-ubyte')
}
