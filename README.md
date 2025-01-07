# airline-ucg
Dashboard for understanding how topics drive satisfaction in airlines

## 1. TODO

### 1.1. Task 1 - Data extraction

> *Dorian*

Focus on the following airlines:
- [AirFrance](https://www.tripadvisor.com/Airline_Review-d8729003-Reviews-Air-France) 
- [Qatar Airways](https://www.tripadvisor.com/Airline_Review-d8729134-Reviews-Qatar-Airways)
- [Emirates](https://www.tripadvisor.com/Airline_Review-d8729069-Reviews-or520-Emirates)
- [Japan Airlines](https://www.tripadvisor.com/Airline_Review-d8729095-Reviews-Japan-Airlines-JAL)
- [British Airways](https://www.tripadvisor.com/Airline_Review-d8729039-Reviews-British-Airways)
- [Korean Air](https://www.tripadvisor.com/Airline_Review-d8729105-Reviews-or1030-Korean-Air)
- [Asiana Airlines](https://www.tripadvisor.com/Airline_Review-d8729024-Reviews-Asiana-Airlines)
- [Swiss International Air Lines](https://www.tripadvisor.com/Airline_Review-d8729160-Reviews-Swiss-International-Air-Lines-SWISS)
- [Turkish Airlines](https://www.tripadvisor.com/Airline_Review-d8729174-Reviews-Turkish-Airlines)
- [Lufthansa](https://www.tripadvisor.com/Airline_Review-d8729113-Reviews-Lufthansa)
- [Iberia](https://www.tripadvisor.com/Airline_Review-d8729089-Reviews-Iberia)
- [Ryanair](https://www.tripadvisor.com/Airline_Review-d8729141-Reviews-Ryanair)


The reviews are in the format `json` and `csv` and can be found in the following folders:
- [`crawling/data_csv`](crawling/data_csv)
- [`crawling/data_json`](crawling/data_json)

### 1.2. Task 2 - Cleaning Data 
> *Dorian* & *Auriane*

Data cleaned, saved as CSV files in the following folder :
- ['cleaning'](cleaning)
  
The cleaning operations made are :
- delete lines where review text or rating is empty
- replace the missing values in numerical column with the mean
- make sure date columns are in datetime
- delete lines where travel date or review date is empty
- delete duplicates
- delete lines where rating is not between 0 and 5

### 1.3. Task 3 - Clustering based on topic (Understand how topics drive satisfaction)
> *Julien*

Code is available here : 
[Clustering_based_on_topic.ipynb](https://github.com/dorianb04/airline-ucg/blob/main/Clustering_based_on_topic.ipynb) 

- Satisfaction graphs
- Wordclouds
- Interactive wordcloud

### 1.4. Task 4 - Dashboard using shiny package in R(sentiment per airline /destination)
> *Kiran* & *Tonghan*

## 2. Installation
First, clone the repository:
```bash
$ git clone https://github.com/dorianb04/airline-ucg
$ cd airline-ucg
```
