# NoP stats

A simple script to pull chapters and collect statistics for The Nature of Predators by /u/SpacePaladin15 on Reddit.

I am very new to Clojure and FP in general and I wrote the bulk of this at 5AM so my code is probably pretty bad. 

## Installation
You can clone the repo and build it with leiningen yourself if you want to make changes, or go to the releases and download a pre-made jar.

Either way, you are going to need the Java runtime installed, which hopefully should be the case for most computers.

## Usage
The simplest way to use to program is to download and run `nopstats-1.4.2-standalone.jar` from the releases tab. The program will try to connect to the reddit API, and it will also generate a file called `output.txt` that will contain all of the relevant stats the script collects.

If successful, it will create several files in the local directory:

|File|Content|
|-|-|
|`output.txt`| Stats readout for each chapter plus a summary at the end |
|`perspectives.csv`|A comma separated list of the perspectives thus far in order|
|`totals.csv`|A comma separated list of word counts for the chapters in order|
|`chapters/[character]/The Nature of Predators [n].md`|Chapter text sorted by perspective in Markdown format|
|`chapters/[character]/The Nature of Predators [n].html`|Chapter text sorted by perspective in HTML format|
|`omnibus.html` | A continuous collection of all NoP chapters |


To run the script from the terminal:

    $ java -jar nopstats-1.4.2-standalone.jar [ags]
|Argument|Effect|
|-|-|
|`-md` | Only output in markdown format |
|`-html` | Only output in HTML format |

## TODO
- [ ] Generate chart graphics for word counts
- [ ] Generate pie chart for perspectives
- [ ] Generate a timeline graphic
- [ ] Add more command line args
- [x] Support more than 100 chapters (Reddit API)