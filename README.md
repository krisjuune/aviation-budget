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

## Data collection

The `data-collection/` folder contains scripts used to build the survey.

- **`wtp-tickets.py`** — generates the surcharge values shown in the WTP ticket scenarios for each country and treatment arm
- **`ch_calc_quota.py`** — calculates demographic quotas for the Swiss sample
- **`check-demo.py`** — checks that collected demographic distributions match targets
- **`power-analysis.R`** — power analysis for sample size determination
- **`sythetic-data.py`** — generates synthetic data for piloting
- **`functions/pre-analysis.R`** — helper functions

### experiment-qualtrics

JavaScript and HTML snippets embedded directly in the Qualtrics survey. Each file corresponds to one survey page or question block:

| File | Purpose |
|------|---------|
| `wtp_intro.js` | Randomly assigns a flight scenario (short/long route) and sets up route-specific ticket costs for the WTP question |
| `wtp_ticket_values_us/ch/cn.js` | Calculates and injects the treatment-specific surcharge amount shown on the ticket, using respondent income, treatment arm, and reduction level |
| `wtp_ticket_display.html` | HTML template for the flight ticket mock-up shown to respondents |
| `wtp_ticket_image.js` | Displays the ticket image |
| `wtp_treatment_msg.js` | Injects a one-sentence description of the surcharge allocation rule matching the respondent's treatment arm |
| `wtp_treatment_image.js` | Displays the treatment-specific infographic for the WTP module |
| `wtc_treatment.js` | Displays the treatment-specific infographic for the WTC (budget) module |
| `display_nr_flights.js` | Echoes back the respondent's self-reported planned flights as a recall check |
| `summary_que_wtc[_mobile].js/.html` | Displays a summary infographic before the WTC question, adapted for desktop and mobile |
| `summary_que_wtp[_mobile].js/.html` | Displays a summary infographic before the WTP question, adapted for desktop and mobile |

## Configuring plot aesthetics

Edit `config.yaml` to adjust text size, point size, line widths, and colour scheme across all plots at once.
