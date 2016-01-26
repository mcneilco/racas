#' ModelFit
#'
#' @field drc_function function. 
#' @field paramNames character. 
#' @field categorization_function function. 
#' @field get_reported_parameters function. 
#' @field apply_limits function. 
#' @field default_fit_settings list. 
#' @field simple_to_advanced_fittings_function function. 
#' @field model_equation_img character. 
#'
#' @export
#'
ModelFit <- setRefClass(Class = "ModelFit",
                        fields = list(
                          drc_function = "function",
                          paramNames = "character",
                          categorization_function = "function",
                          get_reported_parameters = "function",
                          apply_limits = "function",
                          default_fit_settings = "list",
                          simple_to_advanced_fittings_function = "function",
                          model_equation_img = "character",
                          sortOptions = "list",
                          get_curve_attributes = "function",
                          get_saved_fitted_parameters = "function",
                          curveid_query = "character",
                          experiment_query = "character",
                          raw_results_persistence_path = "character",
                          typeMap = "data.table"
                        )
)

ll4 <- ModelFit$new()

kifit <- ModelFit$new()

mm2 <- ModelFit$new()
