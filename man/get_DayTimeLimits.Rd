\name{get_DayTimeLimits}
\alias{get_DayTimeLimits}

\title{
Estimating the timing of sunrise, sunset, astronomical and nautical twilight events
}

\description{
Estimating the timing of sunrise, sunset, astronomical and nautical twilight events in \code{POSIXct}-format based on geolocations and a similar time vector. The function is a simplified call of the \link[suntools]{sunriset} and \link[suntools]{crepuscule} functions of the suntools package, which use algorithms provided by NOAA.
}

\usage{
get_DayTimeLimits(pos)
}

\arguments{
\item{pos}{A \link{data.frame} with columns \code{datetime} (\code{POSIXct}), \code{Lon}, and \code{Lat}.}
}

\value{
The input \link{data.frame} \code{pos} extended by the time vectors \code{sunrise}, \code{sunset}, \code{dawn.naut}, \code{dawn.ast}, \code{dusk.naut}, and \code{dusk.ast}.
}

\references{
Meeus, J. (1991) Astronomical Algorithms. Willmann-Bell, Inc.
}

\seealso{
\link[suntools]{sunriset}, \link[suntools]{crepuscule}, \link{classify_DayTime}
}

\author{
Robert K. Bauer
}

\examples{
#### Example 1: estimate current times of dawn, sunrise, dusk and sunset in Mainz, Germany
pos <- data.frame(Lat = 50, Lon = 8.2667)
pos$datetime <- strptime(Sys.Date(),"\%Y-\%m-\%d")
get_DayTimeLimits(pos)

#### Example 1b: classify current time of the day in Mainz, Germany
classify_DayTime(get_DayTimeLimits(pos))
}

