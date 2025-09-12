# Decision analytics in R for logistics and customer insights.

**This project is split into two complementary parts:**

1. **Part 1: Robot selection & fleet allocation** — choose the best robot prototype and allocate 30 robots across store types under budget and technician constraints.  
2. **Part 2: Customer revenue analysis** — analyze demographics and behavior to identify factors that drive revenue.  

## ⚙️ Tech Stack
- **Language:** R (4.x)
- **Data manipulation & visualization:** tidyverse (dplyr, ggplot2, readr)
- **Decision analysis:** lpSolve (linear/goal programming), ompr (optional)
- **Modelling:** base R (lm), broom for tidy regression outputs
- **Reporting:** rmarkdown / knitr, renv for reproducibility

---

## 📑 Part 1 — Robot selection & fleet allocation

### Problem
A company is planning a one-month trial of autonomous robots for last-mile delivery.  
Objective: **select the best robot prototype** from four options and **allocate 30 robots** across grocery, clothing, and sports stores while meeting constraints:

- Total budget: **£250,000**  
- Technician availability: **≤ 250 hours/week**  
- Minimum robots per store: **5**  

### Methods
- **TOPSIS (multi-criteria decision-making):** rank prototypes on carrying capacity, battery size, speed, cost, and reliability.  
- **Integer goal programming:** allocate robots to stores under budget and technician-hour constraints.  

### Results
- **Best prototype:** **Deviant** (highest TOPSIS score).  
- **Final allocation:** **17 grocery / 5 clothing / 8 sports** (30 robots).  
- **Constraints satisfied:** £250,000 budget fully used, 245 technician hours per week.

---

## 📑 Part 2 — Customer revenue analysis

### Problem
To evaluate the trial’s impact, customer data (400 records) is analyzed to understand what factors influence revenue.  
Variables include:  
- Estimated Age  
- Time on Site  
- Seen Voucher (0/1)  
- Estimated Income  
- Advertisement Channel (1=Leaflet, 2=Social Media, 3=Search Engine, 4=Influencer)  
- Revenue (dependent variable)  

### Methods
- **Exploratory Data Analysis (EDA):** average revenue by income bin, voucher status, and ad channel.  
- **Multiple Linear Regression:** model revenue as a function of demographics and marketing channel.  
- **Diagnostics:** R², MSE, normalized MSE, predicted vs actual plots.  

### Results
- **Positive revenue drivers:** voucher exposure, estimated income, and influencer advertising channel.  
- **Model performance:** low mean squared error and meaningful explanatory power.  

---

## ⚙️ Tech Stack


---
