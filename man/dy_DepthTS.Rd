\name{dy_DepthTS}
\alias{dy_DepthTS}
\alias{dy_TS}

\title{
Plot time series data via the dygraphs interactive interface
}

\description{
Plot time series data (e.g. depth or temperature time series from archival tags) via the dygraphs interactive interface.
}

\usage{
dy_TS(ts_df, y="Depth", xlim, ylim, 
      ylab=y, xlab, main,
      ID, ID_label="Serial", 
      plot_DayTimePeriods=TRUE, twilight.set="ast", 
      color="darkblue",
      doRangeSelector=TRUE, drawPoints=FALSE, pointSize=2, tz="UTC", ...)

dy_DepthTS(ts_df, y="Depth", xlim, ylim, 
           ylab=y, xlab, main,
           ID, ID_label="Serial", 
           plot_DayTimePeriods=TRUE, twilight.set="ast", 
           color="darkblue",
           doRangeSelector=TRUE, drawPoints=FALSE, pointSize=2, tz="UTC", ...)
}

\arguments{
\item{ts_df}{\link{data.frame} holding the time series data to be plotted, including the x-vector 'datetime' (\code{POSIXct}, UTC) and the numeric y-vector whose label is defined by \code{y}.}

\item{y}{Character label of the time series vector to be plotted (default "Depth").}

\item{xlim}{X-axis limits (x1, x2) of the plot (default range(ts_df$datetime), but can be specified in \code{empty.plot_TS}).}

\item{ylim}{Y-axis limits (default range(ts_df[[y]]), but can be specified in \code{empty.plot_TS}).}

\item{ylab, xlab}{Axis labels.}

\item{main}{Main title (default "Tag ID").}

\item{ID, ID_label}{Tag ID and column label (default "Serial").}

\item{plot_DayTimePeriods, twilight.set}{Whether day-time periods ("Night", "Dawn", "Day", "Dusk") should be shaded. If \code{plot_DayTimePeriods=TRUE}, twilight columns are required (or will be estimated via \link{get_DayTimeLimits}). \code{twilight.set} selects "ast" (astronomical) vs "naut" (nautical) twilight.}

\item{color}{Line color (default "darkblue").}

\item{doRangeSelector}{Whether to add dygraph interactive range selector (default TRUE).}

\item{drawPoints, pointSize}{Whether to draw points at sampling times and their size.}

\item{tz}{Time zone for the plot (default "UTC").}

\item{\dots}{Additional arguments passed to \link[dygraphs]{dygraph}. Further arguments can be passed via \link[dygraphs]{dyOptions}.}
}

\seealso{\link{plot_TS}, \link{plot_DepthTempTS}, \link[dygraphs]{dygraph}, \link[dygraphs]{dyOptions}}

\value{
An interactive dygraph plot object that can be altered further using \link[dygraphs]{dyOptions}.
}

\author{
Robert K. Bauer
}

\examples{
### load sample depth and temperature time series data from miniPAT
# ts_file <- system.file("example_files/104659-Series.csv", package="RchivalTag")
# ts_df <- read_TS(ts_file)
# ts_df$Serial <- ts_df$DeployID
# head(ts_df)

## plot depth-time series data
# dy_DepthTS(ts_df)

## add missing Lon, Lat info for day/night shading
# ts_df$Lon <- 5; ts_df$Lat <- 43
# dy_DepthTS(ts_df)

## same figure with plot_DepthTS
# plot_DepthTS(ts_df, plot_DayTimePeriods=TRUE)

## further arguments
# dy_DepthTS(ts_df, xlim=unique(ts_df$date)[2:3], plot_DayTimePeriods=FALSE)

## add further options via dyOptions
# dg <- dy_DepthTS(ts_df, xlim=unique(ts_df$date)[2:3], plot_DayTimePeriods=FALSE, drawPoints=TRUE)
# dg <- dyOptions(dg, drawGrid=FALSE)
# dg
}

