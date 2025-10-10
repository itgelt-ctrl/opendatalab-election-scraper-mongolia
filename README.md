# opendatalab-election-scraper-mongolia
## Mongolia Parliamentary Election Data Scraper

This repository contains an R script to scrape **candidate-level** data from [OpenDataLab.mn](https://opendatalab.mn) for Mongolia's parliamentary elections (1992–2024).

## Features
- Scrapes candidate, district, and election year data in R. 

## Usage

Run the script in R. 

Required packages:
`install.packages(c(
  "rvest",
  "dplyr",
  "xml2",
  "stringr",
  "tibble",
  "purrr",
  "readr"
))`


## Output

Data is saved in `data/` as a CSV file.


## Contributing

Pull requests are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.


## Contact

For questions or support, open an issue or contact [@itgelt-ctrl](https://github.com/itgelt-ctrl).

## License

This project is licensed under the MIT License with the Commons Clause.  
**Commercial use is prohibited without explicit permission.**  
See [LICENSE](LICENSE) for full details.
