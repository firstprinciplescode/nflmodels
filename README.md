# nflmodels

# 🏈 NFL Data & Analysis Repository

This repository serves as a **centralized hub** for all of my NFL data scraping, transformation, and modeling efforts. The code here handles **data collection, processing, and statistical modeling** across various aspects of football analytics.

---

## 📂 Repository Structure (Planned)
As the repository grows, files will be organized into the following structure:

nfl-data-repo

- initial_data_processing/
- qbgrp_def/
- big_data_frames/
- functions/
- README.md  

## 🛠️ Setup & Dependencies
This project is built using **Python (for scraping) and R (for data analysis & modeling).**

This repository will continue to evolve with new data, analyses, and insights to enhance NFL performance evaluation.

---

## 📌 Repository Overview
This repository is organized into several **key components**, each focusing on different aspects of **NFL predictive analytics**:

## 📂 initial_data_processing/

### **1️⃣ Data Scraping - PFF API Scraper (Python)**
- A **Python scraper** to pull data from the **PFF API**.
- Collects propietary information from PFF such as information on quarterback pressure and rushing statistics.

### **3️⃣ Data Manipulation (R) - Play-by-Play Processing**
- **Preprocessing play-by-play data** before the creation of machine learning models.
- Includes **play-by-play integration with personnel information**.
- **Metrics predicted include:**
  - **Completion percentage**
  - **Likelihood of a TD before/after the snap**
  - **Likelihood of a pressure**
  - **Expected yards per carry**
  - Other key game-level metrics.
 
## 📂 qbgrp_def/

### **4️⃣ Clustering QB, Defense Performance**
- Creating **data frames to cluster**:
  - QB + team + season (`qbgrp_ssn`) performance.
  - Defensive season (`def_ssn`) performance.
- Clusters based on **good vs. bad performances** in:
  - Passing depth statistics
  - Time in pocket statistics
  - Play-action vs. non-play-action statistics
  - Blitz statistics
  - Pressure statistics
- **Weight derivation** to measure similarity across performances between qbgrp_ssn, def_ssn.

## 📂 big_data_frames/

### **5️⃣ IDs Hub**
- **Construction of a unique player/team ID system** to **standardize joins** across multiple datasets.
- Ensures **consistent merging** of data from different sources.

### **6️⃣ Core Data Frames - The Foundation for Advanced Analysis**
- **Rushing, receiving, and QB stats** data frames.
- **Clustering** of rushers, receivers into groups based on categories including
  - receiver pre-snap alignment
  - receiver route depth
  - receiver target depth
  - rusher situation
  - rusher zone vs gap
  - rusher usage
- Merging **various statistics** into a **cohesive dataset**.
- These serve as the **base for predictive models & other transformations**.

## 📂 functions/

### **7️⃣ Advanced Statistical Functions**
- Functions to:
  - **Find similar players** (e.g., WRs, RBs) based on performance profiles.
  - **Calculate key statistical insights** in **comparable game situations** for statistics like completion percentage, scramble percentage, sack rate, etc..

---
