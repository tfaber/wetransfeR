[![Travis-CI Build Status](https://travis-ci.org/tfaber/wetransfeR.svg?branch=master)](https://travis-ci.org/tfaber/wetransfeR)

## An R package to send (large) files

This package builds on the wetransfer open API (https://developers.wetransfer.com/) which allows users to send large files (of any kind; up to 2GB per file). To be able to use the package you need to obtain a (free) api key at the forementioned link.

## Installation

You can install the development version from Github:

```
library(devtools)
install_github("tfaber/wetransfeR")
library(wetransfeR)
```

The package leverages on `httr`, `jsonlite` package functions.

## Usage

Using this package you can use the `send_wetransfer()` function and include a reference to the path where your files are stored, your api key, a name and (optional) description of the transfer:

```
send_wetransfer("path/to/files","somekey","my_first_transfer","some nice files")
```


## Plans

This is the very first version of the package, there are multiple improvements/ extensions planned:

- add options for asynchronous evaluation using the future package
- allow for transfers of URL objects (see https://developers.wetransfer.com/documentation)

Please let me know if I can make improvements in the code!
