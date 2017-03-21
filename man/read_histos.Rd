

\name{read_histos}
\alias{read_histos}
\title{
reads a TAD/TAT-histogram file from archival tags
}
\description{
reads or posttreats a manually loaded standard histogram data file, containing Time-at-Depth (TAD) and Time-at-Temperature (TAT) frequency data, from archival tags by \href{http://wildlifecomputers.com/}{Wildlife Computers}.
}
\usage{
read_histos(hist_file)
}
 

\arguments{
\item{hist_file}{
character string indicating the name of a standard \href{http://wildlifecomputers.com/}{Wildlife Computers} file to read or the \link{data.frame} of a manually loaded histogram data file. The Date-vector of the file is expected to be or the format "\%H:\%M:\%S \%d-\%b-\%Y, tz='UTC'". The combination of the columns \code{DeployID}, \code{Ptt} and \code{Serial} is assumed to provide an unique key to distinguish data from indiviudal tags.
}


}

\details{
This function reads or posttreats a manually loaded standard Wildlife Computers histogram file including Time-at-Depth (TAD) and Time-at-Temperature (TAT) frequency data. In the post-treatment, the histogram data is split in lists of TAD and TAT per individual (see below). Thus processed data from several histogram files (or similarly processed time series data) can be combined using the function \link{combine_histos}. Merging of histogram data from several tags, based on similar or user-specified TAD and TAT-\code{bin_breaks}, can be done by applying function \link{merge_histos}. To generate TAD/TAT histogram data from depth and temperature time series data, see \link{ts2histos}.
}

\value{A list-of-lists containing the loaded histogram data. Lists of TAD and TAT data are distinguished at the first nesting level. Further sublists contain the \code{bin_breaks} and \link{data.frame}s of the histogram data per tag (ID).
Tag IDs are constructed based on the columns \code{DeployID}, \code{Ptt} and \code{Serial} keys (e.g. DeployID.101_Ptt.102525).

$ TAD:List\cr
  ..$ ID1  : List of 2\cr
  .. ..$ bin_breaks: num\cr 
  .. ..$ df        : data.frame \cr
$ TAT:List\cr 
  ..$ ID1  : List of 2\cr
  .. ..$ bin_breaks: num\cr 
  .. ..$ df        : data.frame \cr 
  ..$ ID2  : List of 2\cr
  ...

}

\seealso{\link{ts2histos}, \link{combine_histos}, \link{merge_histos}, \link{hist_tad}, \link{hist_tat}} 


\author{
Robert K. Bauer
}

\examples{
## example 1) read, merge and plot TAD frequency data from several files:
## part I - read histogram data from two files:
hist_dat_1 <- read_histos(system.file("example_files/104659-Histos.csv",package="RchivalTag"))
hist_dat_2 <- read_histos(system.file("example_files/104659b-Histos.csv",package="RchivalTag"))
## note the second list is based on the same data (tag), but on different bin_breaks

## part II - combine TAD/TAT frecuency data from seperate files in one list:
hist_dat_combined <- combine_histos(hist_dat_1, hist_dat_2)
par(mfrow=c(2,1))
hist_tad(hist_dat_combined)
hist_tat(hist_dat_combined)

## part III - force merge TAD/TAT frecuency data from seperate files 
# in one list, by applying common bin_breaks:
hist_dat_merged <- merge_histos(hist_dat_combined,force_merge = TRUE)
hist_tad(hist_dat_merged)
hist_tat(hist_dat_merged)

## part IV - plot merged data:
hist_tad(hist_dat_merged) # of all tags
unique(hist_dat_merged$TAD$merged$df$DeployID) ## list unique tags in merged list
hist_tad(hist_dat_merged, select_id = "15P1019b", select_from = 'DeployID') # of one tag

## part V - unmerge data:
unmerge_histos(hist_dat_merged)
}

