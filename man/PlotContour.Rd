\name{PlotContour}
\alias{PlotContour}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PlotContour
}
\description{
This function plots a contour line with a user-defined probability and tightness of fit on an existing plot.
}
\usage{
PlotContour(Data, Probability, BandWidthX,BandWidthY,Colour)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
  An n by 2 matrix of data points around which the contour will be drawn.
  }
  \item{Probability}{
  The proportion of points which should be within the contour line drawn.
  }
  \item{BandWidthX}{
  The tightness of fit of the contour line along the x-axis. This value will be proportional to the scale of the axis.
  }
  \item{BandWidthY}{
  The tightness of fit of the contour line along the y-axis. This value will be proportional to the scale of the axis.
  }
  \item{Colour}{
  Colour of the line to be drawn.
  }
}
\author{
Olivia Cheronet
}
\examples{
#Plot a regular scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#Plot a contour line encompassing 75% of the points
PlotContour(iris[,1:2],0.75,0.5,0.25,"red")

}


