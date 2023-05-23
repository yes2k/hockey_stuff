import hockey_scraper

for year in range(2021, 2023, 1):
    hockey_scraper.scrape_seasons([year], True, docs_dir="data")
