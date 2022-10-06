script_name=$0
script_full_path=$(dirname "$0")
cd $script_full_path

Rscript -e 'library(methods); shiny::runApp(appDir = getwd(), launch.browser = TRUE)'
