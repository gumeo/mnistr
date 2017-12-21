#' Convert factor to a dummy/indicator matrix
#'
#' \code{class.ind} take a factor vector of length $n$ with $k$ levels and
#'                  outputs an $n\times k$ indicator matrix.  
#'
#' @param cl factor vector
#'
#' @return Appropriate indicator matrix.
#'
#' @examples
#' # Random factor data
#' dat <- factor(sample(1:3,100, replace=TRUE))
#' # Get the ggplot object
#' indicatorMatrix <- class.ind(dat)
#'
#' @export
class.ind <- function(cl) {
  Ik=diag(length(levels(cl)))
  x=Ik[as.numeric(cl),]
  dimnames(x) <- list(names(cl), levels(cl))
  x
}

#' Sigmoid activation functon
#'
#' \code{sigmoid} can take a scalar, vector or matrix and output
#'                elementwise application of the sigmoid nonlinearity/
#'                squashing function. There is an additional boolean
#'                flag for calculating the derivative. The deriv argument
#'                is needed for activation functions.  
#'
#' @param X numeric scalar, vector or matrix
#' @param deriv boolean indicating whether we should evaluate the function
#'              or the derivative at the input.
#'
#' @return Same format as the input \code{X}.
#'
#' @examples
#' sigmoid(-5)
#' sigmoid(0)
#' sigmoid(5)
#' sigmoid(0,deriv=TRUE)
sigmoid <- function(X,deriv=FALSE){
  if(deriv==FALSE){
    return(1/(1+exp(-X)))
  }else{
    return(sigmoid(X)*(1 - sigmoid(X)))
  }
}

#' Rectified Linear Unit activation functon
#'
#' \code{sigmoid} can take a scalar, vector or matrix and output
#'                elementwise application of the reLU nonlinearity/
#'                squashing function. There is an additional boolean
#'                flag for calculating the derivative. The deriv argument
#'                is needed for activation functions.  
#'
#' @param X numeric scalar, vector or matrix
#' @param deriv boolean indicating whether we should evaluate the function
#'              or the derivative at the input.
#'
#' @return Same format as the input \code{X}.
#'
#' @examples
#' reLU(-5)
#' reLU(0)
#' reLU(5)
#' reLU(1,deriv=TRUE)
reLU <- function(X,deriv=FALSE){
  if(deriv==FALSE){
    X[X<0] <- 0 
    return(X)
  }else{
    X[X<0] <- 0
    X[X>0] <- 1
    return(X)
  }
}

#' Softmax activation functon
#'
#' \code{sigmoid} takes a vector and applies a softmax to it for 
#'                transforming it into probabilities. This is 
#'                normally used for an output layer in a neural
#'                network for doing classification.
#'
#' @param X numeric vector or matrix (for minibatches)
#'
#' @return Same format as the input \code{X}.
#'
#' @examples
#' softmax(matrix(1:10,5,2))
#' softmax(matrix(1:10,2,5))
softmax <- function(X){
  Z <- rowSums(exp(X))
  X <- exp(t(X))%*%diag(1/Z)
  return(t(X))
}

#' Matrix closure for neural network layers
#'
#' \code{matrixInLayer} getter and setter functions for a matrix
#'
#' @param init boolean for whether we need to initialize values.
#' @param rS number of rows.
#' @param cS number of columns.
#' @param initPos boolean to indicate whether values need to be initialized as positive.
#' @param initScale scalar to truncate initialized values closer to zero.
#'
#' @return environment with the functions \code{setter} and \code{getter}
#'
#' @examples
#' testFun <- matrixInLayer(TRUE,10,10,TRUE,10)
#'
#' @keywords internal
matrixInLayer <- function(init = FALSE, rS, cS, initPos = FALSE, initScale = 100){
  intMat <- matrix(0, nrow=rS, ncol=cS)
  if(init == TRUE){
    intMat <- matrix(rnorm(rS*cS)/initScale,nrow = rS,ncol = cS)
    if(initPos == TRUE){
      intMat <- abs(intMat)
    }
  }
  getter <- function(){
    return(intMat)
  }
  setter <- function(nMat){
    intMat <<- nMat
  }
  return(list2env(list(setter = setter, getter=getter)))
}

#' Fully connected layer for neural network
#'
#' \code{Layer} encapsulates all the data needed for a fully connected layer.
#'
#' @param activation function for neural network. Must be able to do elementwise
#'                   calculations on a matrix and have the parameter \code{deriv},
#'                   which is a boolean indicating whether the derivative should be
#'                   calculated
#' @param minibatchSize Number of samples used for estimating the gradient
#' @param sizeP vector of two values, number of inputs to this layer and number
#'              of outputs from this layer, ignoring bias values.
#' @param is_input boolean indicating whether this is an input
#' @param is_output boolean indicating whether this is output
#' @param initPos boolean indicating whether weights should be initialized as positive
#' @param initScale scalar for initialising wieghts, e.g. if it is 100, then
#'                  the randomly sampled initial weights are scaled by 1/100.
#'
#' @return environment with the functions to set all the internal matricies and
#'         a function to forward propagate through the layer.
#'
#' @examples
#' testLayer <- Layer(mnistr::reLU, 3, c(10,10),FALSE,FALSE,TRUE,1000)
#' testLayer$W$getter() # Check random weights
Layer <- function(activation, minibatchSize,sizeP,is_input=FALSE,is_output=FALSE, initPos, initScale){
  # Matrix holding the output values
  Z <- matrixInLayer(FALSE,minibatchSize,sizeP[1])
  
  # Outgoing Weights
  W <- matrixInLayer(TRUE,sizeP[1],sizeP[2],initPos=initPos, initScale=initScale)
  
  # Input to this layer
  S <- matrixInLayer(FALSE,minibatchSize,sizeP[1])
  
  # Deltas for this layer
  D <- matrixInLayer(FALSE,minibatchSize,sizeP[1])
  
  # Matrix holding derivatives of the activation function
  Fp <- matrixInLayer(FALSE,sizeP[1],minibatchSize)
  
  # Propagate minibatch through this layer
  forward_propagate <- function(){
    if(is_input == TRUE){
      return(Z$getter()%*%W$getter())
    }
    Z$setter(activation(S$getter()))
    if(is_output == TRUE){
      return(Z$getter())
    }else{
      # Add bias for the hidden layer
      Z$setter(cbind(Z$getter(),rep(1,nrow(Z$getter()))))
      Fp$setter(t(activation(S$getter(),deriv = TRUE))) 
      #print(Fp$getter())
      return(Z$getter()%*%W$getter())
    }
  }
  
  # Return a list of these functions
  myEnv <- list2env(list(forward_propagate=forward_propagate, S = S,
                         D = D, Fp = Fp, W = W, Z = Z))
  class(myEnv) <- 'Layer'
  return(myEnv)
}

#' Multi Layer Percepteron
#'
#' \code{mlp} is a function that generates an MLP, for which you can train on data.
#'
#' @param structNet Vector inidcating the sizes of the nodes in the network. E.g.
#'                  \code{c(100,70,40,10)} would be a network with 100 input nodes,
#'                  2 hidden layers with 70 and 40 neurons respectively, and an 
#'                  output layer with 10 neurons.
#' @param minibatchSize Number of samples used for estimating the gradient
#' @param activation function for neural network. Must be able to do elementwise
#'                   calculations on a matrix and have the parameter \code{deriv},
#'                   which is a boolean indicating whether the derivative should be
#'                   calculated
#' @param initPos boolean indicating whether weights should be initialized as positive
#' @param initScale scalar for initialising wieghts, e.g. if it is 100, then
#'                  the randomly sampled initial weights are scaled by 1/100.
#'
#' @return environment with the functions to train the network, forwards propagate and
#'         a list with all the layers. The forward propage function can be used to do
#'         predictions.
#'
#' @examples
#' testMLP <- mlp(c(10,10,10),5,mnistr::reLU,TRUE,1000)
#' testLayer$W$getter() # Check random weights
mlp <- function(structNet, minibatchSize,activation, initPos =FALSE, initScale=100){
  num_layers <- length(structNet)
  #Create the network
  layers <- list()
  for(i in 1:length(structNet)){
    if(i == 1){#inp layer
      layers[[i]] <- mnistr::Layer(activation, minibatchSize, c(structNet[1]+1,structNet[2]),is_input=TRUE,initPos = initPos,initScale=initScale)
    }else if(i == length(structNet)){
      layers[[i]] <- mnistr::Layer(softmax, minibatchSize, c(structNet[num_layers],structNet[num_layers]),is_output=TRUE,initPos = initPos,initScale=initScale)
    }else{
      layers[[i]] <- mnistr::Layer(activation, minibatchSize, c(structNet[i]+1,structNet[i+1]),initPos = initPos,initScale=initScale)
    }
  }
  
  print('Layers have been initialized!')
  
  forward_prop <- function(dataBatch){
    # Add bias to the input
    layers[[1]]$Z$setter(cbind(dataBatch,rep(1,nrow(dataBatch))))
    for(i in 1:(num_layers-1)){
      layers[[i+1]]$S$setter(layers[[i]]$forward_propagate())
    }
    return(layers[[num_layers]]$forward_propagate())
  }
  
  backwards_prop <- function(yhat,labels){
    layers[[num_layers]]$D$setter(t(yhat-labels))
    for(i in (num_layers-1):2){
      W_nobias <- layers[[i]]$W$getter()
      W_nobias <- W_nobias[1:(nrow(W_nobias)-1),]
      mat <- layers[[i]]$Fp$getter()
      layers[[i]]$D$setter((W_nobias%*%layers[[i+1]]$D$getter())*mat)
    }
  }
  
  update_weights <- function(eta){
    for(i in 1:(num_layers-1)){
      W_grad <- -eta*t(layers[[i+1]]$D$getter()%*%layers[[i]]$Z$getter())
      layers[[i]]$W$setter(layers[[i]]$W$getter()+W_grad)
    }
  }
  
  # Labels here as dummy matrix
  train <- function(trainData, trainLabels, num_epochs, eta, cvSplit = 0.3){
    cvInds <- sample(1:nrow(trainData),round(cvSplit*nrow(trainData)))
    cvData <- trainData[cvInds,]
    cvLabels <- trainLabels[cvInds,]
    trainData <- trainData[-cvInds,]
    trainLabels <- trainLabels[-cvInds,]
    extraCV <- length(cvInds)%%minibatchSize
    if(extraCV > 0){
      extraCV <- minibatchSize - extraCV
    }
    extraCV <- sample(1:nrow(cvData),extraCV)
    cvData <- rbind(cvData,cvData[extraCV,])
    cvLabels <- rbind(cvLabels,cvLabels[extraCV,])
    k <- 0
    for(i in 1:num_epochs){
      print(paste('epoch number: ',i))
      inds <- sample(1:nrow(trainData),nrow(trainData)) # Permutate data
      extra <- length(inds)%%minibatchSize # Resample to make final batch correct size
      if(extra > 0){
        extra <- minibatchSize - extra
      }
      inds <- c(inds,sample(1:nrow(trainData),extra))
      numIter <- length(inds)/minibatchSize
      for(j in 1:numIter){
        batchInds <- ((j-1)*minibatchSize):(j*minibatchSize)
        tDat <- trainData[inds[batchInds],]
        tLab <- trainLabels[inds[batchInds],]
        preds <- forward_prop(tDat)
        backwards_prop(preds,tLab)
        update_weights(eta = eta)
      }
      # Calculate accuracy on CV set
      diff <- c()
      numCV <- nrow(cvData)/minibatchSize
      for(j in 1:numCV){
        batchInds <- ((j-1)*minibatchSize):(j*minibatchSize)
        # Forward prop
        cvDat <- cvData[batchInds,]
        cvLab <- cvLabels[batchInds,]
        preds <- forward_prop(cvDat)
        diff <- c(diff,max.col(preds)-max.col(cvLab))
      }
      acc <- sum(diff==0)/length(diff)
      print(paste('Accuracy for epoch',i,'is:',acc))
    }
  }
  
  # Implement predict function
  
  myEnv <- list2env(list(network=layers,
                         forward_prop=forward_prop,
                         train = train))
  class(myEnv) <- 'mlp'
  return(myEnv)
}