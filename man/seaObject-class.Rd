\docType{class}
\name{seaObject-class}
\alias{seaObject-class}
\alias{show}
\alias{show,seaObject-method}
\alias{ytop}
\alias{ytop,seaObject-method}
\alias{ytop<-}
\alias{ytop<-,seaObject-method}
\alias{ybtm}
\alias{ybtm,seaObject-method}
\alias{ybtm<-}
\alias{ybtm<-,seaObject-method}
\alias{xpos}
\alias{xpos,seaObject-method}
\alias{xpos<-}
\alias{xpos<-,seaObject-method}
\alias{col}
\alias{col,seaObject-method}
\alias{col<-}
\alias{col<-,seaObject-method}
\alias{timepoints}
\alias{timepoints,seaObject-method}
\alias{timepoints<-}
\alias{timepoints<-,seaObject-method}
\alias{fracTable}
\alias{fracTable,seaObject-method}
\alias{fracTable<-}
\alias{fracTable<-,seaObject-method}
\alias{parents}
\alias{parents,seaObject-method}
\alias{parents<-}
\alias{parents<-,seaObject-method}
\alias{nestLevels}
\alias{nestLevels,seaObject-method}
\alias{nestLevels<-}
\alias{nestLevels<-,seaObject-method}
\alias{cloneFamily}
\alias{cloneFamily,seaObject-method}
\alias{cloneFamily<-}
\alias{cloneFamily<-,seaObject-method}
\alias{cloneLabels}
\alias{cloneLabels,seaObject-method}
\alias{cloneLabels<-}
\alias{cloneLabels<-,seaObject-method}
\alias{defaultLabels}
\alias{defaultLabels,seaObject-method}
\alias{defaultLabels<-}
\alias{defaultLabels<-,seaObject-method}
\alias{originTimepoint}
\alias{originTimepoint,seaObject-method}
\alias{originTimepoint<-}
\alias{originTimepoint<-,seaObject-method}
\title{
Class seaObject
}
\description{
Represents a seaObject class, containing all necessary input to generate shark
plots, dolphin plots and plaice plots
}
\section{Methods}{
    \describe{
    \item{show}{
    \code{show(seaObject)}: summary of the seaObject.
    }
    \item{ytop}{
    \code{ytop(seaObject)}: get value of ytop.
    }
    \item{ytop<-}{
    \code{ytop(seaObject)<-}: assign value to ytop.
    }
    \item{ybtm}{
    \code{ybtm(seaObject)}: get value of ybtm.
    }
    \item{ybtm<-}{
    \code{ybtm(seaObject)<-}: assign value to ybtm.
    }
    \item{xpos(x)}{
    \code{xpos(seaObject)}: get value of xpos
    }
    \item{xpos(x)<-}{
    \code{xpos(seaObject)}: assign value to xpos
    }
    \item{col(x)}{
    \code{col(seaObject)}: get value of col
    }
    \item{col(x)<-}{
    \code{col(seaObject)}: assign value to col
    }
    \item{timepoints(x)}{
    \code{timepoints(seaObject)}: get value of timepoints
    }
    \item{timepoints(x)<-}{
    \code{timepoints(seaObject)}: assign value to timepoints
    }
    \item{fracTable(x)}{
    \code{fracTable(seaObject)}: get value of fracTable
    }
    \item{fracTable(x)<-}{
    \code{fracTable(seaObject)}: assign value to fracTable
    }
    \item{parents(x)}{
    \code{parents(seaObject)}: get value of parents
    }
    \item{parents(x)<-}{
    \code{parents(seaObject)}: assign value to parents
    }
    \item{nestLevels(x)}{
    \code{nestLevels(seaObject)}: get value of nestLevels
    }
    \item{nestLevels(x)<-}{
    \code{nestLevels(seaObject)}: assign value to nestLevels
    }
    \item{cloneFamily(x)}{
    \code{cloneFamily(seaObject)}: get value of cloneFamily
    }
    \item{cloneFamily(x)<-}{
    \code{cloneFamily(seaObject)}: assign value to cloneFamily
    }
    \item{cloneLabels(x)}{
    \code{cloneLabels(seaObject)}: get value of cloneLabels
    }
    \item{cloneLabels(x)<-}{
    \code{cloneLabels(seaObject)}: assign value to cloneLabels
    }
    \item{defaultLabels(x)}{
    \code{defaultLabels(seaObject)}: get value of defaultLabels
    }
    \item{defaultLabels(x)<-}{
    \code{defaultLabels(seaObject)}: assign value to defaultLabels
    }
    \item{originTimepoint(x)}{
    \code{originTimepoint(seaObject)}: get value of originTimepoint
    }
    \item{originTimepoint(x)<-}{
    \code{originTimepoint(seaObject)}: assign value to originTimepoint
    }
}
}
\author{
Sarah Sandmann <sarah.sandmann@uni-muenster.de>
}
\examples{
timepoints <- c(0,30,75,150)
fracTable <- matrix(
            c( 100, 45, 00, 00,
                02, 00, 00, 00,
                03, 00, 02, 01,
                98, 00, 95, 40),
            ncol=length(timepoints))
parents <- c(0,1,1,3)
seaObject <- createSeaObject(fracTable = fracTable,
                            parents = parents,
                            timepoints = timepoints)
timepoints(seaObject)
timepoints(seaObject) <- c(0,20,75,150)
}
