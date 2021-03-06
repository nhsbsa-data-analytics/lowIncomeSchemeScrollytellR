# NHS Low Income Scheme Scrollytell

This R package has been developed by NHS Business Services Authority Data Science team to use a combination of analytics, user research and customer survey data we explore whether the [NHS Low Income Scheme](https://www.nhsbsa.nhs.uk/nhs-low-income-scheme) in England reaches its intended audience and meets user needs. In this article we consider take-up of the scheme.

We have used the `golem` framework to developed this scrollytelling article as a `shiny` dashboard taking inspiration from:

* [Example `golem` apps](https://github.com/ThinkR-open/golem)
* [ONS Exploring local income deprivation scrollytelling article](https://www.ons.gov.uk/visualisations/dvc1371/#/E07000223)
* [`statistiekcbs/scrollytell` R package](https://github.com/statistiekcbs/scrollytell)
* [Metropolitan Council scrollytelling dashboard](https://github.com/Metropolitan-Council/service.allocation.viz)
* [Clinical Development Unit Data Science Team dashboards](https://github.com/CDU-data-science-team)

The package is structured as follows:

```
lowIncomeSchemeScrollytellR
├── .gitignore                              # Currently ignoring all `data` files
├── .Renviron                               # Credential file (create from the template example.Renviron)
├── .Rbuildignore                           # Golem file
├── app.R                                   # Golem file
├── data                                    # Data for the dashboard (accessible via lowIncomeSchemeScrollytellR::{name})
├── data-raw                                # Various scripts to produce `data` files
├── DESCRIPTION                             # Metadata of package
├── dev                                     # Golem files
│   ├── 01_start.R                          # Golem file (use to set up golem framework)
│   ├── 02_dev.R                            # Golem file (use to develop package)
│   ├── 03_deploy.R                         # Golem file (use to deploy package)
│   └── run_dev.R                           # Golem file (use to test development of package)
├── example.Renviron                        # Template credential file required for data-raw scripts
├── inst                                    # Installed files...
│   ├── app                                 # ... for the app...
│   │   └── www                             # ... made available at runtime
│   │       ├── colours.css                 # Define colour palette of NHS identity
│   │       ├── logo.jpg                    # NHS logo
│   │       ├── mod_{n}_*.md                # Markdown text for module
│   │       └── style.css                   # CSS to defining the styling of the dashboard
│   └── golem-config.yml                    # Golem file
├── nhs-low-income-scheme-scrollytell.Rproj # R Project file
├── LICENSE                                 # Apache
├── man                                     # Automatically generated documentation by roxygen2
├── NAMESPACE                               # Automatically generated documentation by roxygen2
├── R                                       # R code for the dashboard
│   ├── _disable_auotload.R                 # Golem file
│   ├── app_config.R                        # Golem file
│   ├── app_server.R                        # Server component
│   ├── app_ui.R                            # UI component
│   ├── golem_*.R                           # Golem file
│   ├── mod_{n}_*.R                         # Module (usually a container of scrollytell)
│   ├── run_app.R                           # Golem file
│   ├── utils_helpers.R                     # NHSBSA highcharter theme
│   └── utils-pipe.R                        # %>% operator
├── README.md                               # Brief overview of the package
```
