# CRAN Submission Comments - symptomcheckR Version 0.1.1

## Resubmission

Thanks for the quick response to our submission. I corrected all points raised - please find my answers to your points below.

### Answers to Victoria Wimmer

> If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) \<doi:...\> authors (year) \<arXiv:...\> authors (year, ISBN:...) or if those are not available: \<https:...\> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")

Thanks for making us aware of that. I added the reference in the correct format now.

> \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("\# Not run:") as a warning for the user. Does not seem necessary. Please unwrap the examples if they are executable in \< 5 sec, or replace \dontrun{} with \donttest{}.

Thanks for letting us know. I deleted '\dontrun{}' so the example should be executable now.

Thanks for taking the time to re-review our submission.

Best wishes,

Marvin Kopka

## Initial Submission

### Test Environments

-   Local R installation, R 4.3.1, macOS 14.1.1
-   win-builder (devel and release)
-   rhub check_for_cran

### R CMD check results

0 errors \| 0 warnings \| 3 notes

-   checking CRAN incoming feasibility ... NOTE Maintainer: 'Marvin Kopka [marvin.kopka\@tu-berlin.de](mailto:marvin.kopka@tu-berlin.de){.email}' New submission
-   checking for non-standard things in the check directory ... NOTE Found the following files/directories: ''NULL''
    -   I have cleaned up the package directory and believe any non-standard files previously present have been removed.
-   checking for detritus in the temp directory ... NOTE Found the following files/directories: 'lastMiKTeXException':
    -   The note regarding 'lastMiKTeXException' is likely to be from the PDF documentation building process. This file does not affect the functionality of the package.

Thank you for taking the time to review my package.

Best regards,\
Marvin Kopka\
[marvin.kopka\@tu-berlin.de](mailto:marvin.kopka@tu-berlin.de){.email}
