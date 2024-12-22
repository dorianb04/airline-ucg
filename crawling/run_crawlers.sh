#!/bin/bash

# Array of URLs
urls=(
    "https://www.tripadvisor.com/Airline_Review-d8729141-Reviews-Ryanair"
    "https://www.tripadvisor.com/Airline_Review-d8729003-Reviews-Air-France"
    "https://www.tripadvisor.com/Airline_Review-d8729134-Reviews-Qatar-Airways"
    "https://www.tripadvisor.com/Airline_Review-d8729069-Reviews-or520-Emirates"
    "https://www.tripadvisor.com/Airline_Review-d8729095-Reviews-Japan-Airlines-JAL"
    "https://www.tripadvisor.com/Airline_Review-d8729039-Reviews-British-Airways"
    "https://www.tripadvisor.com/Airline_Review-d8729105-Reviews-or1030-Korean-Air"
    "https://www.tripadvisor.com/Airline_Review-d8729024-Reviews-Asiana-Airlines"
    "https://www.tripadvisor.com/Airline_Review-d8729160-Reviews-Swiss-International-Air-Lines-SWISS"
    "https://www.tripadvisor.com/Airline_Review-d8729174-Reviews-Turkish-Airlines",
    "https://www.tripadvisor.com/Airline_Review-d8729113-Reviews-Lufthansa",
    "https://www.tripadvisor.com/Airline_Review-d8729089-Reviews-Iberia",
    
)

# Launch crawler for each URL in parallel with delay
for url in "${urls[@]}"; do
    python ./crawler.py --url "$url" --max_pages 5 &
    sleep 20
done

# Wait for all background processes to complete
wait

echo "All crawlers have finished"