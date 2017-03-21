

\name{ts2histos}
\alias{ts2histos}
\title{
convert depth and temperature time series data to discrete Time-at-Depth and Time-at-Temperature data (histogram data)
}
\description{
convert depth and temperature time series data to discrete Time-at-Depth (TaD) and Time-at-Temperature (TaT) data (histogram data) at user-defined breakpoints
}
\details{
Time-at-Depth and Time-at-Temperature fequencies (histograms) are a standard data product of archival tags (incl. tag models TDR-Mk9, PAT-Mk10 and miniPAT by \href{http://wildlifecomputers.com/}{Wildlife Computers}) that allow to assess habitat preferences of tagged animals.  It can be likewise generated from transmitted or recovered time series data sets, which is the purpose of this function. 

However, different depth and temperature bin breaks are often used during different deployment programs, which makes a later comparitive analysis of TaT and TaD data difficult. For such cases, the functions \link{combine_histos} and \link{merge_histos} can be applied to merge TaT and TaD frequencies based on common bin breaks of different tags.

To visualize Time-at-Temperature (TaT) and Time-at-Depth (TaD) data, please see \link{hist_tat} and \link{hist_tad}, respectively.
}

\usage{
ts2histos(ts_df, tad_breaks=NULL, tat_breaks=NULL, split_by=NULL, aggregate_by="Ptt")
}
 

\arguments{
\item{ts_df}{
dataframe of depth time series data. Obligatory columns are the numeric vector "Depth", "date" (of class \link{Date}) and "Serial". \code{split.by} defines an optional vector to consider (e.g. \code{day.period}).
}

\item{tad_breaks, tat_breaks}{
a numeric vector, defining the depth and/or temperature breakpoints of the histogram cells.
}

\item{split_by}{
Name of the logical vector by which TaD/TaT data shall be splitted (e.g. day.time).
}

\item{aggregate_by}{
character vector defining the columns by which the tagging data should be aggregated. Should contain columns that identify tags (e.g. Serial, Ptt, DeployID) the date and/or day time period (to seperate records from night, day, dawn and dusk see \link{classify_DayTime}). Default values are: date, Day and Ptt.
}

}



\seealso{\link{read_histos}, \link{hist_tad}, \link{merge_histos}} 


\author{
Robert K. Bauer
}

\examples{
### load sample depth and temperature time series data from miniPAT:
ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
ts_df <- read.table(ts_file, header = TRUE, sep = ",")
head(ts_df)

tad_breaks <- c(0, 2, 5, 10, 20, 50, 100, 200, 300, 400, 600, 2000)
tat_breaks <- c(10,12,15,17,18,19,20,21,22,23,24,27)


## example 1a) convert only DepthTS data to daily TAD frequencies:
ts2histos(ts_df, tad_breaks = tad_breaks)
hist_tad(ts_df, bin_breaks = tad_breaks)
hist_tad(ts_df, bin_breaks = tad_breaks, do_mid.ticks = FALSE)

## convert 1b) only TemperatureTS data to daily TAT frequencies:
tat <- ts2histos(ts_df, tat_breaks = tat_breaks)
hist_tat(ts_df, bin_breaks = tat_breaks, do_mid.ticks = FALSE)
hist_tat(tat$TAT$merged, do_mid.ticks = FALSE)

## convert 1c) DepthTS & TemperatureTS data to daily TAD & TAT frequencies:
ts2histos(ts_df, tad_breaks = tad_breaks, tat_breaks = tat_breaks)

## example 2) rebin daily TAD frequencies:
tad <- ts2histos(ts_df, tad_breaks = tad_breaks)
tad2 <- rebin_histos(hist_list = tad, tad_breaks = tad_breaks[c(1:3,6:12)])
par(mfrow=c(2,2))
hist_tad(tad, do_mid.ticks = FALSE) ## example for multiple individuals
hist_tad(tad$TAD$merged, do_mid.ticks = FALSE)
hist_tad(tad$TAD$merged, bin_breaks = tad_breaks[c(1:3,6:12)]) ## from inside hist_tad



}

