
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `soles` package

<!-- badges: start -->

<!-- badges: end -->

This package provides utility functions to help automate teaching and
education related tasks at the University of Sydney. Most of them have
been made for my own use, but the functions should be useful to anyone
teaching at the University of Sydney.

**Note: This package is for USYD staff only.** Manual data exports from
University systems (Canvas, Gradescope, etc.) are **required** to use
package functions. The package does not download these sensitive files
for you.

## Why `soles`?

A small number of functions will *only* make sense if you are a member
of the School of Life and Environmental Sciences (SOLES). For example,
the way we compile markbooks for the Board of Examiners meetings may not
be the same across the University.

## Todo

- [ ] Add shiny package to parse extensions

<!-- 
Here are the key functions, with detailed examples below:
&#10;- **`uos()`**: scrapes a unit of study site to get information about the unit* of study. Useful to quickly check assessment due dates and other information. Importantly, this information can be used to automate other functions in this package.
- **`cr_db()`**: combines Canvas, Gradescope, UoS Coordinator and Disability Academic Plans data to create a tidy data frame of students and their information. I use this as a starting point for many other functions in this package. With the database I can do things like:
  - bulk apply special considerations (extensions)
  - check for borderline students
  - check attendance 
  - perform detailed analyses and visualisations rapidly (a *key* reason for using R instead of point and click systems such as the University's painfully *slow* SRES)
- **`cr_marksheet()`**: creates a marksheet for the Board of Examiners meetings. Automates many tedious tasks such as checking for missing marks, pending special considerations and more when applying grades. To use this function you will probably use the following functions:
  - `ms_assessments()`: select assessments that contribute towards the final grade.
  - `ms_weights()`: declare assessment weights to calculate the final grade.
  - `ms_manual()`: perform manual adjustments -- useful for borderline students or students with special considerations.
- **`cr_seams_section()`**: creates a list of students to be added as a Section in SEAMS. This is useful for bulk Canvas actions such as adding students to a group or extending due dates for all students on similar academic plans. -->
