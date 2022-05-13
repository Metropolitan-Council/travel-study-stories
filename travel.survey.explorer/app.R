# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
travel.survey.explorer::run_app() # add parameters here (if any)


# rsconnect::deployApp(appDir = ".",
#                      account = "metrotransitmn",
#                      server = "shinyapps.io",
#                      appName = "travel-study-stories",
#                      appId = 6298674,
#                      launch.browser = function(url) {
#                        message("Deployment completed: ", url)     },
#                      lint = FALSE,
#                      metadata = list(asMultiple = FALSE,
#                                      asStatic = FALSE,
#                                      ignoredFiles = "dev/01_start.R|dev/02_dev.R|dev/03_deploy.R|dev/run_dev.R|LICENSE|LICENSE.md|README.md|README.Rmd|tests/spelling.R|tests/testthat/test-create_one_way_table.R|tests/testthat/test-create_two_way_table.R|tests/testthat/test-golem-recommended.R|tests/testthat.R|data-raw"),      logLevel = "verbose")
