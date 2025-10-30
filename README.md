# 🤖 Data Analysis for Optimising Resource Allocation in Autonomous Robot Trial

This project focuses on data-driven decision analytics to support the selection and allocation of autonomous delivery robots using Excel.

---

## 🧩 Objective

A company plans a one-month trial of autonomous delivery robots.  
The goal is to **select the best robot prototype** and **allocate 30 robots** across three store types — grocery, clothing, and sports — while meeting operational constraints.

---

## ⚙️ Methodology

### Technique for Order of Preference by Similarity to Ideal Solution (TOPSIS)

Implemented entirely in **Microsoft Excel**.

TOPSIS was applied to rank robot prototypes based on multiple performance criteria:

- Carrying Capacity  
- Battery Size  
- Speed  
- Cost  
- Reliability  

### Steps Followed

1. **Normalization** of criteria values  
2. **Weighted Normalized Matrix** calculation  
3. **Identification of Positive and Negative Ideal Solutions (PIS & NIS)**  
4. **Distance Calculation** from PIS and NIS  
5. **Relative Closeness (TOPSIS Score)** computation and ranking  

---

## 🧮 Results

- **Best Prototype:** **Deviant** (highest TOPSIS score)  
- **Optimal Allocation:**  
  - Grocery Stores — 17 robots  
  - Clothing Stores — 5 robots  
  - Sports Stores — 8 robots  

### Constraints
- **Total Budget:** £250,000 (fully utilized)  
- **Technician Availability:** 245 hours/week (within 250-hour limit)  
- **Minimum Robots per Store:** 5  

✅ All constraints satisfied.

---

## 🛠️ Tools Used

- **Microsoft Excel** — for all decision modelling, TOPSIS ranking, and allocation analysis  
- **Techniques:** Multi-Criteria Decision Making (TOPSIS)  

---

## 🧾 Summary

This project demonstrates the use of Excel-based analytical techniques to:
- Evaluate multiple alternatives using TOPSIS  
- Make optimal allocation decisions under budgetary and manpower constraints  

The **Deviant** prototype emerged as the most suitable robot for deployment, achieving full utilisation of resources and compliance with all constraints.

