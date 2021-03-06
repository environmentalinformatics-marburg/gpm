% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compIndexFolds.R
\name{compIndexFolds}
\alias{compIndexFolds}
\title{Create testing and training folds for each response variable following a 
grouping by index strategy.}
\usage{
compIndexFolds(x, selector, nbr = 1)
}
\arguments{
\item{x}{An object of class gpm or data.frame}

\item{selector}{The column name of the selector variable. Only 
relevant if use_selector is TRUE.}

\item{response}{The column name(s) of the response variable(s)}

\item{resamples}{The list of the resamples containing the individual row 
numbers (resulting from function \code{\link{resamplingsByVariable}})}

\item{p}{The fraction of each sample to be used for model training 
(default 0.75)}

\item{use_selector}{Use the selector variable for splitting the samples into
training or testing (default FALSE).}
}
\value{
A nested list with training and testing samples for each of the n
resamplings.
}
\description{
TODO
Split a data set into testing and training samples by leaving selectors out.
If more than one response (i.e. dependent) variable is supplied, a different set of
testing/training pairs is created for each of them. The training/testing
samples leave one or more selectors completely out. Selectors can be something
like individual locations, a certain time step or a combination of both. 
Basically, it can be anything since the selector variable is defined by the
user and hence it can be compiled in such a way that it leaves out whatever
the user wants.
}
\details{
The split into training and testing samples is realized by using 
the caret::createDataPartition function which preserves the frequency
distribution of the individual response variable(s).
}
\examples{
\dontrun{
#Not run
}

}
\references{
The function uses functions from:
 Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, 
 Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton Kenkel, 
 the R Core Team, Michael Benesty, Reynald Lescarbeau, Andrew Ziem, 
 Luca Scrucca, Yuan Tang and Can Candan. (2016). caret: Classification and 
 Regression Training. https://CRAN.R-project.org/package=caret
}
\seealso{
\code{\link{resamplingsByVariable}} for creating n resamplings from
the original dataset.
}
