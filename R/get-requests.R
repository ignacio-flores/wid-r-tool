#' @title Get variables associated to a list of area codes
#'
#' @author Thomas Blanchet
#'
#' @description Perform the GET request to the server to retrieve all
#' variables for a list of area codes.
#'
#' @param areas List of area codes.
#' @param sixlet Six-letter code for which to fetch variables.
#'
#' @importFrom httr GET add_headers content
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON

get_variables_areas <- function(areas, sixlet = "all") {
    # Concatenate area codes
    query_areas <- paste(areas, collapse = ",")

    # Perform request
    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "countries-available-variables?countries=", query_areas, "&variables=", sixlet
    )
    response_request <- GET(url, add_headers("x-api-key" = base64encode(api_key)))
    response_content <- content(response_request, as = "text", encoding = "UTF-8")
    response_json <- fromJSON(response_content, simplifyVector = FALSE)
    if (length(response_json) == 1) {
        response_json <- response_json[[1]]
    }

    response_table <- data.frame()
    for (variable in names(response_json)) {
        json_variable <- response_json[[variable]]
        for (country in names(json_variable)) {
            json_country <- json_variable[[country]]
            df_country <- data.frame()
            for (i in json_country) {
                df_country <- rbind(df_country, data.frame(
                    percentile = i[[1]],
                    age = i[[2]],
                    pop = i[[3]],
                    stringsAsFactors = FALSE
                ))
            }
            df_country$variable <- variable
            df_country$country <- country

            response_table <- rbind(response_table, df_country)
        }
    }

    return(response_table)
}

#' @title Get data associated to a list of variables
#'
#' @author Thomas Blanchet
#'
#' @description Perform GET request to the server to retrieve data associated
#' to a list of variables.
#'
#' @param areas List of area codes.
#' @param variables List of variables, of the form: \code{"xxxxxx_pXXpYY_999_i"}
#' @param no_extrapolation Logical: should interpolated/extrapolated years be
#' included or not?
#'
#' @importFrom httr GET add_headers content
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON

get_data_variables <- function(areas, variables, no_extrapolation = FALSE) {
    # Concatenate area codes, variables
    query_areas <- paste(areas, collapse = ",")
    query_variables <- paste(variables, collapse = ",")

    # Perform request
    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "countries-variables?countries=", query_areas,
        "&variables=", query_variables, "&years=all"
    )
    response_request <- GET(url, add_headers("x-api-key" = base64encode(api_key)))
    response_content <- content(response_request, as = "text", encoding = "UTF-8")
    response_json <- fromJSON(response_content, simplifyVector = FALSE)

    response_table <- data.frame()
    for (variable in names(response_json)) {
        json_variable <- response_json[[variable]]
        for (json_country in json_variable) {
            # Extract country
            country <- names(json_country)
            # Extract data
            df_data <- data.frame()
            for (i in json_country[[1]]$values) {
                df_data <- rbind(df_data, data.frame(
                    indicator = variable,
                    country = country,
                    year = i[[1]],
                    value = i[[2]],
                    stringsAsFactors = FALSE
                ))
            }
            # Extract metadata
            json_meta <- json_country[[1]]$meta

            if (no_extrapolation) {
                # Periods of extrapolated data
                extrapol_brackets <- json_meta$extrapolation
                if (!is.null(extrapol_brackets)) {
                    if (!is.na(extrapol_brackets) & extrapol_brackets != "") {
                        extrapol_brackets <- fromJSON(extrapol_brackets)

                        # Data points to be included
                        data_points <- json_meta$data_points
                        if (!is.null(data_points)) {
                            if (!is.na(data_points) & data_points != "") {
                                data_points <- fromJSON(data_points)
                            } else {
                                data_points <- NULL
                            }
                        }

                        # List of year to exclude because they are extrapolations
                        to_exclude <- NULL
                        for (i in 1:nrow(extrapol_brackets)) {
                            exclude_range <- seq(
                                from = as.integer(extrapol_brackets[i, 1]) + 1,
                                to = as.integer(extrapol_brackets[i, 2])
                            )
                            to_exclude <- c(to_exclude, exclude_range)
                        }
                        to_exclude <- as.character(to_exclude)
                        to_exclude <- to_exclude[!(to_exclude %in% data_points)]

                        # Remove extrapolations from the data
                        df_data <- df_data[!(df_data$year %in% to_exclude), ]
                    }
                }
            }

            response_table <- rbind(response_table, df_data)
        }
    }

    return(as.data.frame(response_table))
}

#' @title Get metadata associated to a list of variables
#'
#' @author Thomas Blanchet
#'
#' @description Perform GET request to the server to retrieve metadata
#' associated to a list of variables.
#'
#' @param areas List of area codes.
#' @param variables List of variables, of the form: \code{"xxxxxx_pXXpYY_999_i"}
#'
#' @importFrom httr GET add_headers content
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON

get_metadata_variables <- function(areas, variables, report_missing = TRUE) {
    # Concatenate area codes, variables
    query_areas <- paste(areas, collapse = ",")
    query_variables <- paste(variables, collapse = ",")

    # Perform request
    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "countries-variables-metadata?countries=", query_areas,
        "&variables=", query_variables)
    response_request <- GET(url, add_headers("x-api-key" = base64encode(api_key)))
    response_content <- content(response_request, as = "text", encoding = "UTF-8")
    response_json <- fromJSON(response_content, simplifyVector = FALSE)

    response_table <- data.frame()
    response_json <- response_json[[1]]$metadata_func

    missing_metadata <- list()

    for (json_variable in response_json) {
        # Extract variable name
        variable <- names(json_variable)
        # Extract the various metadata
        json_name  <- json_variable[[variable]][[1]][[1]]
        json_type  <- json_variable[[variable]][[2]][[1]]
        json_pop   <- json_variable[[variable]][[3]][[1]]
        json_age   <- json_variable[[variable]][[4]][[1]]
        json_units <- json_variable[[variable]][[5]][[1]]
        json_notes <- json_variable[[variable]][[6]][[1]]
        # The item "unit" (5th position) is always filled, so we use it to
        # loop over the different countries
        for (meta_country in json_units) {
            meta_note <- NULL
            for (note in json_notes[[1]][[1]]) {
                if (note$alpha2 == meta_country$country) {
                    meta_note <- note
                }
            }
            meta <- data.frame(variable = variable, stringsAsFactors = FALSE)

            meta$unit     <- meta_country$metadata$unit
            meta$unitname <- meta_country$metadata$unit_name

            meta$shortname    <- json_name$shortname
            meta$shortdes     <- json_name$simpledes
            meta$technicaldes <- json_name$technicaldes

            meta$shorttype <- json_type$shortdes
            meta$longtype  <- json_type$longtype

            meta$shortpop <- json_pop$shortdes
            meta$pop      <- json_pop$longdes

            meta$shortage <- json_age$shortname
            meta$age      <- json_age$fullname

            meta$country     <- meta_country$country
            meta$countryname <- meta_country$country_name

            # Handle potential missing metadata by setting them to NA
            meta$method     <- if (!is.null(meta_note$method)) meta_note$method else NA
            meta$source     <- if (!is.null(meta_note$source)) meta_note$source else NA
            meta$quality    <- if (!is.null(meta_note$data_quality)) meta_note$data_quality else NA
            meta$imputation <- if (!is.null(meta_note$imputation)) meta_note$imputation else NA


            # Identify missing fields
            missing_fields <- names(meta)[sapply(meta, function(x) all(is.na(x)))]

            if (length(missing_fields) > 0) {
                if (!(variable %in% names(missing_metadata))) {
                    missing_metadata[[variable]] <- list(all_missing = c(), partial_missing = list())
                }

                if (length(missing_fields) == length(meta)) {
                    # All metadata is missing for this country
                    missing_metadata[[variable]]$all_missing <- c(missing_metadata[[variable]]$all_missing, meta$country)
                } else {
                    # Only some fields are missing
                    missing_metadata[[variable]]$partial_missing[[meta$country]] <- missing_fields
                }
            }

            response_table <- rbind(response_table, meta)
        }
    }

    # Show message if missing metadata was detected
    if (length(missing_metadata) > 0 && report_missing == FALSE) {
        message("Some metadata fields are missing for certain countries or variables.")
        message("Run `get_metadata_variables(..., report_missing = TRUE)` to see details.")
    }

    # Print organized missing metadata summary if the option is enabled
    if (report_missing == TRUE && length(missing_metadata) > 0) {
        message("Missing Metadata Summary:\n")
        for (var in names(missing_metadata)) {
            cat("Variable:", var, "\n")
            if (length(missing_metadata[[var]]$all_missing) > 0) {
                cat("  All metadata missing for:", paste(missing_metadata[[var]]$all_missing, collapse = ", "), "\n")
            }
            if (length(missing_metadata[[var]]$partial_missing) > 0) {
                cat("  Partial metadata missing:\n")
                for (country in names(missing_metadata[[var]]$partial_missing)) {
                    cat("    -", country, ":", paste(missing_metadata[[var]]$partial_missing[[country]], collapse = ", "), "\n")
                }
            }
        }
        cat("\n")
    }

    # # Show message if missing metadata was detected
    # if (length(missing_metadata) > 0 && !report_missing) {
    #     message("Some metadata fields are missing for certain countries or variables.")
    #     message("Run `get_metadata_variables(..., report_missing = TRUE)` to see details.")
    # }
    #
    # # Print missing metadata summary if the option is enabled
    # if (report_missing && length(missing_metadata) > 0) {
    #     print("Missing metadata detected:")
    #     print(missing_metadata)
    # }

    # Clarify meaning of 'imputation'
    response_table$imputation[response_table$imputation == "region"]    <- "regional imputation"
    response_table$imputation[response_table$imputation == "survey"]    <- "adjusted surveys"
    response_table$imputation[response_table$imputation == "tax"]       <- "surveys and tax data"
    response_table$imputation[response_table$imputation == "full"]      <- "surveys and tax microdata"
    response_table$imputation[response_table$imputation == "rescaling"] <- "rescaled fiscal income"

    return(response_table)
}

