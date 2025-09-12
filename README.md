**Data-driven decision support in R:**  
1. Select the best robot prototype and allocate a 30-robot fleet within budget and technician constraints.  
2. Analyze customer data to identify factors driving revenue.  

![demo gif](demo/demo.gif)

---

## 🚀 Project Overview
This project demonstrates how decision analysis and predictive modeling can guide real-world logistics and marketing strategy.  

- **Part A (Fleet allocation):** Use **multi-criteria decision-making** and **integer programming** to choose the optimal delivery robot and allocate 30 robots across store types.  
- **Part B (Customer analysis):** Apply **exploratory data analysis** and **multiple linear regression** on customer demographics to uncover drivers of spending.  

---

## 📊 Key Results
- **Fleet allocation:**  
  - Selected robot prototype: **Deviant** (highest TOPSIS score).  
  - Final allocation: **17 grocery / 5 clothing / 8 sports**.  
  - Budget used: **£250,000**; technician hours: within 250-hour weekly limit.  

- **Customer revenue analysis:**  
  - Dataset: 400 customers (age, time on site, voucher, income, ad channel, revenue).  
  - Key positive drivers of revenue: **voucher exposure, estimated income, influencer advertising channel**.  
  - Model achieved good fit with low mean squared error.  

---

## 🛠 Methods
### Part A — Fleet decision & allocation
- **TOPSIS** (Technique for Order Preference by Similarity to Ideal Solution) to rank robot prototypes on carrying capacity, battery, speed, cost, and reliability.  
- **Goal Programming / Integer LP** to allocate robots across store types under constraints (budget, technician hours, minimum robots per store).  

### Part B — Customer revenue analysis
- **Exploratory Data Analysis (EDA):** revenue trends by income, ad channel, voucher status.  
- **Multiple Linear Regression (MLR):** model revenue as a function of age, time on site, voucher, income, and ad channel.  
- **Model evaluation:** R², MSE, normalized MSE, predicted vs actual plots.  

---

## 📂 Repo Structure
