
pwd
Rscript -e "rsconnect::setAccountInfo(name='metrotransitmn', token='${SHINYAPPSIO_TOKEN}', secret='${SHINYAPPSIO_SECRET}')"
Rscript -e "rsconnect::deployApp(appDir = '.', appId = 6297984,  account = 'metrotransitmn', server = 'shinyapps.io', appName = 'travel-survey-explorer', appTitle = 'travel-survey-explorer', lint = FALSE, metadata = list(asMultiple = FALSE, asStatic = FALSE, ignoredFiles = 'dev/01_start.R|dev/02_dev.R|dev/03_deploy.R|dev/run_dev.R|LICENSE|LICENSE.md|README.md|README.Rmd|tests/spelling.R|tests/testthat/test-create_one_way_table.R|tests/testthat/test-create_two_way_table.R|tests/testthat.R|data-raw'), logLevel = 'verbose')"
