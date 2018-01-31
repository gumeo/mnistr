# mnistr

This package serves the purpose of making it easy to get started with the mnist data set in R. This is currently **work in progress** and I plan to work on this in my free-time. The main goals of this package are:

- Download mnist from R into specified or current directory
- Load mnist into R 
- Basic plotting functions for digits
- Simple neural network to train on the data

If the download function cannot unzip the data, then you have to do it manually.

Future plans include:

- Extending the neural network implementation
- Add other algorithms implemented in R and Rcpp

## Installation

I might submit the package to CRAN later, until then, install with:

```
devtools::install_github('gumeo/mnistr')
```

## How to use?

If there is any relevant content for this package, I am more likely to put it on my [blog](gumeo.github.io).

In particular checkout [this post](https://gumeo.github.io/post/part-2-deep-learning-with-closures-in-r/)!

Here is a visualization I made with this package, showing first layer in an MLP during training:
<p align="center">
    <img src="./inst/twitterAni.gif" width="640"\>
</p>

