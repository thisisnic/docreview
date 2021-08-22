# docreview 0.1.0

## New features
* Added a check for if vignette images have alt text
* Added a vignette explaining docreview config files

## Improvements
* All checks now run before results are rendered, so if there's an error, it's clearer
* Default config file has been updated to remove redundant lines and unnecessary components in their hierarchy
* Checks are now skipped if they are missing from the config file (previously resulted in an error)
* The check for exports without examples now only checks function with number of arguments greater than or equal to a specified value (default is 1)

