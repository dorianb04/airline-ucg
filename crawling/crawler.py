from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from bs4 import BeautifulSoup
import time
import pandas as pd
import logging
import argparse
import re
import json
import os

# Suppress Selenium logging
logging.getLogger('selenium').setLevel(logging.ERROR)

def extract_review_content(review):
    """
    Extracts structured content from a review HTML element.
    This function parses a BeautifulSoup review element from TripAdvisor to extract various
    review attributes like title, route information, ratings, and review text.
    Args:
        review (bs4.element.Tag): A BeautifulSoup Tag object representing the review HTML element
    Returns:
        dict: A dictionary containing the following review information:
            - title (str): The review title
            - departure (str): Departure location
            - destination (str): Destination location  
            - type_of_route (str): Type of route/flight
            - review_text (str): Main review content
            - travel_date (str): Date of travel
            - review_date (str): Date review was written
            - rating (float): Overall rating (out of 5)
            - legroom_rating (float): Rating for legroom
            - seat_comfort_rating (float): Rating for seat comfort
            - entertainment_rating (float): Rating for in-flight entertainment
            - customer_service_rating (float): Rating for customer service
            - value_rating (float): Rating for value for money
            - cleanliness_rating (float): Rating for cleanliness
            - check_in_rating (float): Rating for check-in/boarding
            - food_rating (float): Rating for food/beverage
        Returns None if there is an error during extraction.
    Raises:
        No explicit raises, exceptions are caught and handled internally
    """
    
    # Initialize variables
    title = route = review_text = travel_date = review_date = rating = None
    detailed_ratings = {}
    
    try:
        # First try to find title in current div
        title_div = review.find('div', class_='uuBRH')
        if not title_div:
            # If not found, try to find in nested tuuww div
            nested_review = review.find('div', class_='tuuww')
            if nested_review:
                title_div = nested_review.find('div', class_='uuBRH')
        title = title_div.text.strip() if title_div else None
        
        # Extract route info
        route_spans = review.find_all('span', class_='thpSa')
        if route_spans:
            route = ' - '.join([span.text.strip() for span in route_spans]).split(' - ')
            departure = route[0].strip()
            destination = route[1].strip()
            type_of_route = route[2].strip()

        else:
            departure = None
            destination = None
            type_of_route = None

        
        # Extract review text
        text_span = review.find('span', class_='JguWG')
        review_text = text_span.text.strip() if text_span else None
        
        # Extract travel date
        travel_date = None
        date_text = review.find_all('div', class_=['biGQs', '_P', 'pZUbB', 'hmDzD'])
        for i in date_text:
            if 'Date of travel' in i.text:
                travel_date = i.text.split(':')[-1].strip()
                break

        # Extract review date
        review_date_div = review.find('div', class_='ncFvv')
        if not review_date_div:
            # Try to find in parent div
            review_date_div = review.parent.find('div', class_='fiHDc')
            if review_date_div:
                review_date_div = review_date_div.find('div', class_='ncFvv')
        review_date = review_date_div.text.replace('Written', '').strip() if review_date_div else None
        
        # Extract rating - first try current div
        rating_svg = review.find('svg', class_='UctUV')
        # If not found, try parent div
        if not rating_svg:
            parent_div = review.parent.find('svg', class_='UctUV')
            if parent_div:
                rating_svg = parent_div
        
        if rating_svg and rating_svg.find('title'):
            rating_text = rating_svg.find('title').text
            rating = float(rating_text.split('of')[0].strip())
        
        # Extract detailed ratings
        ratings_div = review.find('div', class_='JxiyB')
        if not ratings_div:
            # Try to find in parent
            ratings_div = review.parent.find('div', class_='JxiyB')
        
        if ratings_div:
            rating_items = ratings_div.find_all('div', class_='msVPq')
            for item in rating_items:
                category_div = item.find('div', class_='osNWb')
                if category_div:
                    category = category_div.text.strip()
                    svg = item.find('svg', class_='UctUV')
                    if svg and svg.find('title'):
                        rating_text = svg.find('title').text
                        rating_value = float(rating_text.split('of')[0].strip())
                        detailed_ratings[category] = rating_value
        
        return {
            'title': title,
            'departure': departure,
            'destination': destination,
            'type_of_route': type_of_route,
            'review_text': review_text,
            'travel_date': travel_date,
            'review_date': review_date,
            'rating': rating,
            'legroom_rating': detailed_ratings.get('Legroom', None),
            'seat_comfort_rating': detailed_ratings.get('Seat comfort', None),
            'entertainment_rating': detailed_ratings.get('In-flight Entertainment', None),
            'customer_service_rating': detailed_ratings.get('Customer service', None),
            'value_rating': detailed_ratings.get('Value for money', None),
            'cleanliness_rating': detailed_ratings.get('Cleanliness', None),
            'check_in_rating': detailed_ratings.get('Check-in and boarding', None),
            'food_rating': detailed_ratings.get('Food and Beverage', None)
        }
        
    except Exception as e:
        print(f"Error extracting review content: {e}")
        return None

def get_reviews(url, max_pages=10):
    """
    Scrapes reviews from a TripAdvisor airline review page using Selenium WebDriver.
    This function automates the process of collecting review data from TripAdvisor airline reviews,
    handling pagination and saving results both incrementally and as a final CSV file.
    Args:
        url (str): The TripAdvisor URL for the airline reviews page
        max_pages (int, optional): Maximum number of pages to scrape. Defaults to 10.
    Returns:
        pandas.DataFrame: DataFrame containing the scraped reviews data with columns for review content,
                         or None if an error occurs during scraping.
    The function:
    - Sets up a Chrome WebDriver with anti-bot detection options
    - Extracts airline name from URL for file naming
    - Iteratively scrapes review content from each page
    - Saves reviews incrementally to a text file
    - Creates a final CSV file with all reviews
    - Handles pagination automatically
    Notes:
        - Requires Chrome WebDriver to be installed
        - Uses time delays to avoid detection
        - Creates both .txt and .csv output files
        - File names are based on the airline name extracted from URL
    Raises:
        Various exceptions may be raised during web scraping, but are caught and handled internally.
    """
    

    options = Options()
    options.add_argument('--disable-blink-features=AutomationControlled')
    options.add_argument('--start-maximized')
    options.add_argument('--ignore-certificate-errors')
    options.add_argument('--ignore-ssl-errors')
    options.add_argument('--log-level=3')
    options.add_experimental_option('excludeSwitches', ["enable-automation", "enable-logging"])
    options.add_experimental_option('useAutomationExtension', False)
    
    airline_name = re.sub(r'.*Reviews-', '', url).lower()

    driver = None
    reviews_data = []
    
    try:
        driver = webdriver.Chrome(options=options)
        driver.execute_script("Object.defineProperty(navigator, 'webdriver', {get: () => undefined})")
        
        driver.get(url)
        time.sleep(10)  # Wait for initial load
        
        page = 1
        while page <= max_pages:
            print(f"Scraping page {page}...")
            soup = BeautifulSoup(driver.page_source, 'html.parser')

            if page == 1:
                with open(f'page_{page}.html', 'w', encoding='utf-8') as f:
                    f.write(soup.prettify())

            # Find reviews in both structures
            review_divs = soup.find_all('div', class_=['lwGaE'])
            
            if not review_divs:
                print("No reviews found on page")
            else:
                print(f"Found {len(review_divs)} reviews on page {page}")
                
                for review in review_divs:
                    try:
                        review_data = extract_review_content(review)
                        if review_data:
                            reviews_data.append(review_data)
                            with open(f'data_temp/{airline_name}.txt', 'a', encoding='utf-8') as f:
                                f.write(str(review_data)+"\n", f)
                    except Exception as e:
                        print(f"Error parsing review: {e}")
                        continue
            
            # Try to find and click next button
            try:
                next_button = driver.find_element('css selector', 'button[aria-label="Next page"]')
                if not next_button.is_enabled():
                    print("Reached last page")
                    break
                next_button.click()
                time.sleep(3)
                page += 1
            except Exception as e:
                print(f"Could not find next button or reached last page: {e}")
                break
        
        # Save the final data as a json file
        with open(f'data_json/{airline_name}_reviews.json', 'w', encoding='utf-8') as f:
            json.dump(reviews_data, f, indent=4)

        # Convert to DataFrame and save
        df = pd.DataFrame(reviews_data)
        df.to_csv(f'data_csv/{airline_name}_reviews.csv', index=False)

        print(f"Scraped {len(reviews_data)} reviews")
        return df
        
    except Exception as e:
        print(f"An error occurred: {e}")
        return None
    finally:
        if driver:
            try:
                driver.quit()
            except Exception:
                pass
        
        os.remove(f'data_temp/{airline_name}.txt')

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Scrape airline reviews from TripAdvisor')
    parser.add_argument('--url', type=str, required=True, help='TripAdvisor URL to scrape')
    parser.add_argument('--max_pages', type=int, default=10, help='Maximum number of pages to scrape (default: 10)')

    args = parser.parse_args()

    if not args.url:
        print("Please provide a valid TripAdvisor airline reviews URL")
        exit(1)
    
    reviews_df = get_reviews(args.url, args.max_pages)
    if reviews_df is not None:
        print("\nSample of reviews:")
        print(reviews_df.head())