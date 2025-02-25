% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_LS.R
\name{generate_LS}
\alias{generate_LS}
\title{Generate L-systems object}
\usage{
generate_LS(axiom, rules, angle, len = 1, l_factor = 1, n_gen)
}
\arguments{
\item{axiom}{starting string}

\item{rules}{replacement rules}

\item{angle}{direction to next point}

\item{len}{initial length, defaults to 1}

\item{l_factor}{length factor per branch level, defaults to 1}

\item{n_gen}{number of generations}
}
\value{
Segments of L-Systems at axes origin. Each with segment's coordinates, direction, id, level, function placeholder
}
\description{
Generate L-System object by first generating an action string from axiom & rules, then the object itself
and then translate the object to the axes origins.
}
