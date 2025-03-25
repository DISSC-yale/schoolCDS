# School Common Data Set

An R package to collect, process, and work with school Common Data Set (CDS) data,
as defined by the [Common Data Set Initiative](https://commondataset.org/).

## Data

The current version of the extract dataset is available at [docs/cds_data.csv](docs/cds_data.csv),
and a version is included in the package as `cds_data`.

## Installation

Download R from [r-project.org](https://www.r-project.org), then install the package from an R console:

```R
# install.packages("remotes")
remotes::install_github("DISSC-yale/schoolCDS")
```

And load the package:

```R
library(schoolCDS)
```

## Alternate OCR Method

The [process_mistral.py](process_mistral.py) script uses the [Mistral API](https://docs.mistral.ai/api/#tag/ocr)
to extract text from the PDF reports, and save results to [reports/mistral](reports/mistral/):

```sh
python process_mistral.py
```

This is an experimental alternative to using the texts directly, or other OCR methods.
