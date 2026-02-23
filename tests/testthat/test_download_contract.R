context("download_wid API contract")

mock_variables_response <- function() {
    data.frame(
        percentile = "p99p100",
        age = "992",
        pop = "j",
        variable = "sptinc",
        country = "FR",
        stringsAsFactors = FALSE
    )
}

mock_data_response <- function() {
    data.frame(
        indicator = c("sptinc_p99p100_992_j", "sptinc_p99p100_992_j"),
        country = c("FR", "FR"),
        year = c("2000", "2001"),
        value = c(1, 2),
        stringsAsFactors = FALSE
    )
}

test_that("metadata mode accepts partial metadata values", {
    partial_metadata <- data.frame(
        variable = "sptinc_p99p100_992_j",
        unit = NA_character_,
        unitname = NA_character_,
        shortname = "Top 1% share",
        shortdes = "share",
        technicaldes = "technical",
        shorttype = "share",
        longtype = "share",
        shortpop = "equal-split adults",
        pop = "equal-split adults",
        shortage = "adult",
        age = "20+",
        country = "FR",
        countryname = "France",
        method = NA_character_,
        source = NA_character_,
        quality = NA_character_,
        imputation = NA_character_,
        stringsAsFactors = FALSE
    )

    testthat::local_mocked_bindings(
        get_variables_areas = function(areas, sixlet = "all") {
            mock_variables_response()
        },
        get_data_variables = function(areas, variables, no_extrapolation = FALSE) {
            mock_data_response()
        },
        get_metadata_variables = function(areas, variables, report_missing = TRUE, collected_metadata = NULL) {
            list(response_table = partial_metadata, collected_metadata = list())
        },
        .package = "wid"
    )

    out <- download_wid(
        areas = "FR",
        indicators = "sptinc",
        perc = "p99p100",
        ages = "992",
        pop = "j",
        metadata = TRUE
    )

    expect_equal(nrow(out), 2)
    expect_true(all(out$countryname == "France"))
    expect_true(all(is.na(out$quality)))
    expect_true(all(is.na(out$imputation)))
})

test_that("metadata mode requires metadata schema columns", {
    broken_metadata <- data.frame(
        variable = "sptinc_p99p100_992_j",
        country = "FR",
        stringsAsFactors = FALSE
    )

    testthat::local_mocked_bindings(
        get_variables_areas = function(areas, sixlet = "all") {
            mock_variables_response()
        },
        get_data_variables = function(areas, variables, no_extrapolation = FALSE) {
            mock_data_response()
        },
        get_metadata_variables = function(areas, variables, report_missing = TRUE, collected_metadata = NULL) {
            list(response_table = broken_metadata, collected_metadata = list())
        },
        .package = "wid"
    )

    expect_error(
        download_wid(
            areas = "FR",
            indicators = "sptinc",
            perc = "p99p100",
            ages = "992",
            pop = "j",
            metadata = TRUE
        ),
        regexp = "undefined columns selected",
        fixed = TRUE
    )
})

test_that("include_extrapolations forwards no_extrapolation flag", {
    observed_flags <- logical()

    testthat::local_mocked_bindings(
        get_variables_areas = function(areas, sixlet = "all") {
            mock_variables_response()
        },
        get_data_variables = function(areas, variables, no_extrapolation = FALSE) {
            observed_flags <<- c(observed_flags, no_extrapolation)
            mock_data_response()
        },
        .package = "wid"
    )

    download_wid(
        areas = "FR",
        indicators = "sptinc",
        perc = "p99p100",
        ages = "992",
        pop = "j",
        include_extrapolations = TRUE
    )

    download_wid(
        areas = "FR",
        indicators = "sptinc",
        perc = "p99p100",
        ages = "992",
        pop = "j",
        include_extrapolations = FALSE
    )

    expect_equal(observed_flags, c(FALSE, TRUE))
})

test_that("exclude extrapolations path does not require meta in data payload", {
    fake_response <- list(
        sptinc_p99p100_992_j = list(
            list(FR = list(values = list(
                list("2000", 1),
                list("2001", 2)
            )))
        )
    )

    testthat::local_mocked_bindings(
        GET = function(...) NULL,
        content = function(...) "ignored",
        fromJSON = function(...) fake_response,
        .package = "wid"
    )

    out <- get_data_variables("FR", "sptinc_p99p100_992_j", no_extrapolation = TRUE)
    expect_equal(nrow(out), 2)
    expect_true(all(out$indicator == "sptinc_p99p100_992_j"))
    expect_true(all(out$country == "FR"))
})
