# aviation-budget

Analysis pipeline for a survey experiment on aviation short-term decaronisation options, examining willingness to pay (WTP) for flying tickets including a sustainable aviation surcharge and willingness to change (WTC) flying behaviour across three countries (US, Switzerland, China) under different design principles, which allocate the burden based on different distributive justice principles.

## Setup

Requires [conda](https://docs.conda.io/en/latest/miniconda.html). Create and activate the environment:

```bash
conda env create -f environment.yaml
conda activate aviation-budget
```

## Running the pipeline

Place the raw survey data files in `raw-data/`, then run:

```bash
snakemake --cores 2
```

All cleaned data goes to `data/` and all outputs (plots, tables) go to `output/`.

To also render the sample exploration HTML report:

```bash
snakemake --cores 2 --config html_reports=true
```

## Configuring plot aesthetics

Edit `config.yaml` to adjust text size, point size, line widths, and colour scheme across all plots at once.
