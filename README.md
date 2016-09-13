# IndOR

To install and load the package in R

```ruby
library(devtools)
install_github("MathieuEmily/IndOR")
library(IndOR)
```

Function IndOR takes as input a matrix M, where M is a 9x2 table. The first column is made by counts for the nine genotypes in cases and the second column stores counts for controls.

# Example:

If you observe the following counts in cases:


|       |  AA | Aa | aa |
|------|:-----|:-----|:----|
| BB	| 349 | 299 | 64 |
| Bb	| 466	 | 399 | 86 |
| bb   | 155 | 133 | 49 |

   AA	Aa	aa

BB	349	299	64

Bb	466	399	86

bb	155	133	49

and in controls:

   AA	Aa	aa

BB	353	302	65

Bb	470	403	86

bb	157	134	29

then in R you can build the following contingency table:

```ruby
Cases <- c(349,299,64,466,399,86,155,133,49)
Controls <- c(353,302,65,470,403,56,157,134,29)
M <- cbind(Cases,Controls)
```

By typing 
```ruby
IndOR(M)
```
you can get the following response:
```ruby
$statistic
     [,1]
[1,]  3.7

$p.value
     [,1]
[1,] 0.45
```
