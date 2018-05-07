This is the resubmission of the package 'webr'

## Test environments
* local OS X install, R 3.4.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

## Reviewer comments

2018-04-18 Prof Swetlana Herbrandt:

1. Thanks, please omit the redundant "with R" in your title.

2. Please write the title in title case:
Data and Functions for Web-Based Analysis

3. We are missing Tommaso Martino <todoslogos@@gmail.com> in the Authors field. Please add all authors and copyright holders in the Authors@R field with the appropriate roles.

4. Please add the ISBN to your reference in your description text.

5. Please elaborate in your Description what functionality your package provide.

## Submission comments:

1. I omitted the redundant "with R" in title.
2. I wrote the title in title case.
3. I added Tommaso Martino in the Authors filed.
4. I added the ISBN in my description field.
5. I added a explanation of main function 'plot.htest' in Description field.

## Automatic check results of last submission

2018-4-23

Windows check:
** checking whether the package can be loaded ... ERROR
Loading this package had a fatal error status code 1
Loading log:
Error: package or namespace load failed for 'webr' in library.dynam(lib, package, package.lib):
 DLL 'tibble' not found: maybe not installed for this architecture?

No strong reverse dependencies to be checked.
 
## Submission Comments  

2018-4-23

I have never used ‘tibble’ in my package ‘webr’.
I think the rejection is a false positive.

## Reviewer Comments

2018-5-5 by Uwe Ligges

Thanks for your note, can you please resubmit the package, looks like this was a hicc up in our submission system.

## Reviewer comment

2018-5-7 by Swetlana Herbrandt

Thanks, please add a blank after "Korean,":
Korean, ISBN

Please do not enclose function names in quotation marks:
plot.htest()

Please fix and resubmit.

## Submission Comments  

I added a blank after "Korean,": Korean, ISBN

I deleted quotation marks: plot.htest()
