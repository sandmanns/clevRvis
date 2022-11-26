\name{exploreTrees}
\alias{exploreTrees}
\title{
Explore alternative trees
}
\description{
clevRvis allows to explore alternative trees, i.e. alternative parental
relations. Taking information on the CCFs and available time points as input,
all possible parental relations are investigated and checked for validity.
}
\usage{
exploreTrees(fracTable, timepoints)
}
\arguments{
\item{fracTable}{
A numeric matrix containing tumor fraction estimates for all clones at all
time points.
}
\item{timepoints}{
A numeric vector specifying the timepoints for each column of the matrix.
}
}
\details{
To create a seaObject, the basis for all plotting functions in clevRvis, a
fracTable, a timepoints vector and a parents vector are required. clevRvis
provides an approach to determine all valid parental relations on the basis
of the information provided in fracTable and the timepoints vector. Thereby,
alternative trees can be explored.

To optimize run-time, the analysis is devided into 3 step procedure:

1) Possible parents are determined. If clone 1 has at any measured time point a
lower CCF compared to clone 2, then clone 1 cannot be clone 2's parent.

2) Possible branched dependent evolution is investigated. If clone 2 can only
develope from clone 1, the difference in CCFs for clone 1 and clone 2 is
calculated. Every remaining clone with a CCF larger than the difference
cannot develop from clone 1.

3) All remaining, possible parental relations are determined. An extensive
validity check is performed using clevRvis (validity check when creating a
seaObject). A maximum of 20,000 parental relations is investigated.


}
\value{
A list of numeric vectors containing valid parental relations, apt to explore
alternative trees.
}
\author{
Sarah Sandmann <sarah.sandmann@uni-muenster.de>
}


\examples{
timepoints <- c(0,30,75,150)
fracTable <- matrix(
            c( 100, 45,  0,  0,
                20,  0,  0,  0,
                30,  0, 20,  5,
                98,  0, 55, 40),
            ncol=length(timepoints))

trees <- exploreTrees(fracTable, timepoints)
}
