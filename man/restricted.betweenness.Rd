\name{restricted.betweenness}
\alias{restricted.betweenness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Calculates betweenness using only paths from the source vertices to the sink vertices
}
\usage{
restricted.betweenness(g, from = V(g), to = V(g), parallel = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
  igraph object
}
  \item{from}{
source vertices
}
  \item{to}{
sink vertices
}
  \item{parallel}{
whether to use registered parallel backend
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data.frame with the restricted betweenness
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Eric Kramer <ericransomkramer@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
    library("igraph")
    g = barabasi.game(100)
    restricted.betweenness(g, from=1:50, to=51:100)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
