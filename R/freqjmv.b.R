
# This file is a generated template, your changes will not be overwritten

freqjmvClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "freqjmvClass",
    inherit = freqjmvBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            if (self$options$vector %in% names(self$data)) {

                self$results$text$setContent(
                    rosetta::freq(self$data[, self$options$vector])$dat
                );
            }

        })
)
