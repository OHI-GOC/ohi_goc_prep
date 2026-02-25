library(rsconnect)

# set account info
rsconnect::setAccountInfo(name='sophiamanos713',
                          token='903C42E9825CCC51A5ED64DBCAFB8117',
                          secret='I6s1bOSQgNOmtvmixHPCClehLi+romHnx0hMSIYi')

# Define your app directory                      
app_dir <- "/path/to/your/fishing_effort_yearly_shiny"

# Deploy with explicit file specification
rsconnect::deployApp(
  appDir = app_dir,
  appFiles = c(
    "app.R",
    list.files(file.path(app_dir, "data"), 
               full.names = TRUE, 
               recursive = TRUE)
  )
)