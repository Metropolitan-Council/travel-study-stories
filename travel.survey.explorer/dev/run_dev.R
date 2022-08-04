# Set options here
options(golem.app.prod = FALSE,
        scipen = 999999) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# travel.survey.explorer::render_guides()

# Run the application
run_app()
