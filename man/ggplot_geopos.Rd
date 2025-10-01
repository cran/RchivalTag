\name{ggplot_geopos}
\alias{ggplot_geopos}
\alias{get_geopos}

\title{
Reads and plots geolocation estimates derived from archival tagging data
}

\description{
Geolocations provided via CSV or data frames are plotted as line or scatter plots.  
If netCDF (.nc) or KMZ files from the \href{https://my.wildlifecomputers.com/}{Wildlife Computers portal} are selected, a \link[sp]{SpatialPolygonsDataFrame} is created and surface probability maps are illustrated.  
The netCDF transformation follows the R-code in the \href{https://static.wildlifecomputers.com/manuals/Location-Processing-User-Guide.pdf}{Location Processing User Guide}.  
KMZ files already include contour lines of the 50, 95, and 99\% likelihood areas, which are extracted and transformed to a \link[sp]{SpatialPolygonsDataFrame}.
}

\usage{
ggplot_geopos(x, ggobj, xlim, ylim, zlim, standard_year=FALSE, 
              full_year=standard_year, date_format, lang_format="en", tz="UTC", 
              Breaks, cb.title, cb.date_format, cbpos, cb.height = 10, cb.xlab = "",
              cb.reverse=FALSE, pal.reverse=cb.reverse, prob_lim=.75, color_by="date", 
              pal, alpha=70, type="p",
              main ,lwd=1, size=2, shape=19, verbose= FALSE, ...)

get_geopos(x, xlim, ylim, date_format, lang_format="en", tz="UTC", 
           proj4string, prob_lim=.5, verbose=TRUE)
}

\arguments{
\item{x}{\link{data.frame} containing horizontal position records, or a path/filename of \code{.csv}, \code{.kmz}, \code{.kml}, or \code{.nc}-files.}

\item{ggobj}{A \link[ggplot2]{ggplot} object (optional base plot).}

\item{xlim, ylim}{Numeric vectors defining the limits of the x- and y-axes.}

\item{zlim, Breaks, standard_year, full_year}{Date range and breaks of the colorbar. If \code{standard_year = TRUE}, positions are standardized yearly and tick breaks are ignored. If \code{full_year = TRUE}, the color scale covers all months from January to December.}

\item{date_format, lang_format, tz}{
character strings indicating the date format, language format and the corresponding time zone, defined by the vectors Date and Time (by default: date_format="\%d-\%b-\%Y \%H:\%M:\%S", lang_format="en", tz='UTC')
If formatting fails, please check as well the input language format, defined by \code{lang_format} (and use abbrviations such as "en" for English,"es" for Spanish, "fr" for French, etc.) as well.
}

\item{proj4string}{Coordinate reference system (CRS; \link[raster]{projection}).}

\item{cb.title}{Title of the colorbar (default "Date").}

\item{cb.date_format}{Date format of the colorbar ticks (default "\%Y-\%m-\%d").}

\item{cbpos, cb.xlab, cb.height}{Position, x-axis label, and height of the colorbar.}

\item{prob_lim}{For kmz, kml, or netCDF (.nc) files, defines the probability surface limit in \%. Default 50\%. Valid KMZ/KML values: 50, 95, 99.}

\item{color_by}{Column or vector used for coloring geolocations (default "date").}

\item{pal}{Color map used for polygon (.nc) or scatter plots. See \link[oceanmap]{cmap} for pre-installed color maps.}

\item{cb.reverse, pal.reverse}{Inverse order of ticks and colormap of colorscale.}

\item{alpha}{Transparency of polygons or dots in percent (default 70).}

\item{type}{Plot type: "p" for points (default), "l" for lines, "b" for both.}

\item{size, shape}{Point size and type (default 2 and 19 for solid dots).}

\item{lwd}{Line width.}

\item{\dots}{Additional arguments passed to \link[oceanmap]{ggplotmap}.}

\item{main}{Overall plot title.}

\item{verbose}{Whether file names should be printed when loading geolocation files (default TRUE for \code{get_geopos()}, FALSE for \code{ggplot_geopos()}).}
}

\seealso{
\link{leaflet_geopos}, \link{ggplotly_geopos}, \link[oceanmap]{ggplotmap}, 
\link[sp]{SpatialPolygonsDataFrame}
}

\author{
Robert K. Bauer
}

\examples{
# Example 1a: Line plot from CSV
# library(oceanmap)
# csv_file <- system.file("example_files/15P1019-104659-1-GPE3.csv", package="RchivalTag")
# pos <- get_geopos(csv_file)
# ggobj <- ggplot_geopos(pos)
# ggobj
# ggplotly_geopos(ggobj)

# Example 1b: Scatter plot on existing landmask
# ggobj <- oceanmap::ggplotmap("lion", grid.res=5)
# ggobj4 <- ggplot_geopos(csv_file, ggobj)
# ggplotly_geopos(ggobj4)

# Example 2: Probability surfaces from netCDF
# nc_file <- system.file("example_files/15P1019-104659-1-GPE3.nc", package="RchivalTag")
# ggobj6 <- ggplot_geopos(nc_file)
# ggplotly_geopos(ggobj6)

# Example 3: Probability surfaces from KMZ
# kmz_file <- system.file("example_files/15P1019-104659-1-GPE3.kmz", package="RchivalTag")
# ggobj7 <- ggplot_geopos(kmz_file)
# ggplotly_geopos(ggobj7)
}

