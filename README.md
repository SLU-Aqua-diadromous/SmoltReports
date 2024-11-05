# SmoltReports

This package contains Rmarkdown templates that can make it easy to make simple
but human readable reports from data from the smolt traps. Summaries of both
the raw data from the traps and estimates of smolt production can be produced by
the templates.

# Templates

Templates are available from `Rstudio` -\> `File` -\> `New file` -\> `Rmarkdown...` -\> `From template`

1.  Simple smolt estmates (No BlackBox data)

2.  Smolt estmates (with BlackBox data)

3.  Yearly smolt trap report

You can also save a copy of a template with R-code.
```r
rmarkdown::available_templates(package = "SmoltReports")

rmarkdown::draft(FILENAME,
  template = "smolt-estimates-blackbox",
  package = "SmoltReports", edit = FALSE)
```

The template will use data files in the same folder as the template is saved in.
Functions defined in this package are used by the templates to read this data.
If all input files that a template expects exists no modification to the file 
should be necessary, you only need to `render` the file to produce the report.

You can render the files either by clicking the `Knit`-button in Rstudio or
using the `rmarkdown::render()` function.
