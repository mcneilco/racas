#' Title
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
                          get_curve_attributes = "function"
                        )
)

ll4 <- ModelFit$new(drc_function = drc::LL.4, 
                    paramNames = c("slope", "min", "max", "ec50"), 
                    categorization_function = categorize.LL4, 
                    get_reported_parameters = get_reported_parameters.LL4,
                    apply_limits = apply_limits.LL4,
                    default_fit_settings = get_default_fit_settings("4 parameter D-R"),
                    simple_to_advanced_fittings_function = updateFitSettings.LL4,
                    model_equation_img = get_text_file_contents(system.file(file.path("rmd","equations"), "ll4.txt", package = "racas")),
                    sortOptions = sortOptions.LL4,
                    get_curve_attributes = get_curve_attributes.LL4
)

kifit <- ModelFit$new(drc_function = ki_fct.5, 
                      paramNames = c("min", "max", "ki", "ligandConc", "kd"), 
                      categorization_function = categorize.ki, 
                      get_reported_parameters = get_reported_parameters.ki,
                      apply_limits = apply_limits.ki,
                      default_fit_settings = get_default_fit_settings("Ki Fit"),
                      simple_to_advanced_fittings_function = updateFitSettings.Ki,
                      model_equation_img = get_text_file_contents(system.file(file.path("rmd","equations"), "ki.txt", package = "racas")),
                      sortOptions = sortOptions.ki,
                      get_curve_attributes = get_curve_attributes.ki
)

mm2 <- ModelFit$new(drc_function = drc::MM.2, 
                    paramNames = c("max", "kd"), 
                    categorization_function = categorize.MM2, 
                    get_reported_parameters = get_reported_parameters.MM2,
                    apply_limits = apply_limits.MM2,
                    default_fit_settings = get_default_fit_settings("MM.2"),
                    simple_to_advanced_fittings_function = updateFitSettings.MM2
                    
)
