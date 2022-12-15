#' @title Sample data that can be analysed using the Dynameta shiny app
#' 
#' @description An example dataset containing data collected for a GLiTRS meta-analysis
#' investigating the effect of insecticide application on Odonata abundance. 
#' This can be analysed using the Dynameta shiny app.
#' 
#' @format A data frame with 47 rows and 46 variables:
#' \describe{
#'   \item{\code{Paper_ID}}{The paper from which the comparison was drawn. (character)}
#'   \item{\code{Included}}{The exclusion or inclusion status of the paper. (character)}
#'   \item{\code{Observation_ID}}{The comparison of that row, which should be unique to each row. (character)}
#'   \item{\code{Author}}{The full list of authors of the paper. (character)}
#'   \item{\code{Year}}{The year of publication of the paper. (integer)}
#'   \item{\code{Title}}{The paper title. (character)}
#'   \item{\code{Journal}}{The journal the paper was published in. (character)}
#'   \item{\code{DOI}}{The DOI of the paper. (character)}
#'   \item{\code{URL}}{The URL of the paper. (character)}
#'   \item{\code{Language}}{The language of the paper. (character)}
#'   \item{\code{Database}}{The database from which the paper was found. (character)}
#'   \item{\code{Latitude}}{The latitude coordinate of the observation in that row. (double)}
#'   \item{\code{Longitude}}{The longitude coordinate of the observation in that row. (double)}
#'   \item{\code{Country}}{The country in which the observation in that row is located. (character)}
#'   \item{\code{Taxa_level}}{The taxonomic level that is common to all of the biodiversity recorded in that row. (character)}
#'   \item{\code{Taxa_name}}{The name of the taxonomic level that is common to all of the biodiversity recorded in that row. (character)}
#'   \item{\code{Order}}{The taxonomic order of the biodiversity measured in that row, for both the treatment and control. (character)}
#'   \item{\code{Family}}{The taxonomic family of the biodiversity measured in that row, for both the treatment and control. (character)}
#'   \item{\code{Genus}}{The taxonomic genus of the biodiversity measured in that row, for both the treatment and control. (character)}
#'   \item{\code{Binomial}}{The Latin binomial of the species in that row, if it has been recorded. (character)}
#'   \item{\code{Life_history_stage}}{The life-history stage of the taxa in that row. (character)}
#'   \item{\code{Experimental_year_start}}{The year in which data collection started. (integer)}
#'   \item{\code{Experimental_year_end}}{The year in which data collection ended. (integer)}
#'   \item{\code{Sampling_method}}{The sampling method used to sample the biodiversity measure. (character)}
#'   \item{\code{Biodiversity_metric}}{The metric of biodiversity measured. Biodiversity_metric should typically be one of “Abundance”, “Richness”, or “Biomass”. (character)}
#'   \item{\code{Unit}}{The more specific unit of measure for the biodiversity recorded in that row. (character)}
#'   \item{\code{IUCN_threat_category_1}}{Broadest level of IUCN threat. (character)}
#'   \item{\code{IUCN_threat_category_2}}{Secondary level of IUCN threat. (character)}
#'   \item{\code{IUCN_threat_category_3}}{Tertiary level of IUCN threat. (character)}
#'   \item{\code{Treatment}}{Name of the threat measured in that row (e.g. insecticide). (character)}
#'   \item{\code{Treatment_quantity}}{The quantity of treatment reported by the authors for that observation. (double)}
#'   \item{\code{Treatment_quantity_unit}}{The unit of measure for the numeric value in Treatment_quantity. (character)}
#'   \item{\code{Control}}{Name of the control measured in that row (e.g. no insecticide). (character)}
#'   \item{\code{Extracted_from}}{The figure or table from which the data in that row was extracted. (character)}
#'   \item{\code{Evidence_type}}{The type of evidence for the data in that row. Type of evidence should be either “Experimental” or “Quasi-experimental”. (character)}
#'   \item{\code{Treatment_N}}{The number of treatment sites from which the mean and error values were drawn. (integer)}
#'   \item{\code{Treatment_Mean}}{The mean biodiversity value across all treatment sites for that comparison. (double)}
#'   \item{\code{Treatment_error}}{The raw treatment error value reported by the authors for that observation. (double)}
#'   \item{\code{Treatment_error_type}}{The type of error recorded in Treatment_error. Typically this will be one of Standard error, Standard deviation, or 95% confidence interval. (character)}
#'   \item{\code{Control_N}}{The number of control sites from which the mean and error values were drawn. (integer)}
#'   \item{\code{Control_Mean}}{The mean biodiversity value across all control sites for that comparison. (double)}
#'   \item{\code{Control_error}}{The raw control error value reported by the authors for that observation. (double)}
#'   \item{\code{Control_error_type}}{The type of error recorded on Control_error. Typically this will be one of “Standard error”, “Standard deviation”, or “95% confidence interval”. (character)}
#'   \item{\code{Contributor_name}}{Name of the contributor for that row. (character)}
#'   \item{\code{Search_date}}{Date on which the final optimised search is carried out. (date)}
#'   \item{\code{Notes}}{This field should be used for any relevant descriptions that don’t fall within core data, can’t be categorised into clearly defined factors in the associated metadata, or provide explanations for the associated metadata. (character)} 
#'}
#'
#'@usage data(sample_data)
"sample_data"