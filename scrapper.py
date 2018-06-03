from calendar import month_name
from csv import DictWriter
from datetime import datetime
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from unidecode import unidecode
import os
import pandas
import re

import pdb
#pdb.set_trace()

DATA_LEN = 16
MONTH_DAY_LEN = 2


def main() -> None:
  feds = {
      "America",
      "Africa",
      "Asia",
      "Europe",
      "Oceania"}
  fieldnames = [
      "HomeTeam",
      "AwayTeam",
      "Date",
      "Contest", 
      "HomeGoals",
      "AwayGoals",
      "HomeGoalsFull",
      "AwayGoalsFull",
      "ExtraTimePossible",
      "HomeAdvantage"]
  filename = "matches-scrapped.csv"
  url_home = "https://www.eloratings.net"
  options = Options()
  options.set_headless(headless=True)
  teams = get_teams(feds, url_home, options)

  with open(filename, "w") as csvfile:
    writer = DictWriter(csvfile, delimiter=",", fieldnames=fieldnames, 
        lineterminator="\n")
    writer.writeheader()
    get_games_for_all_teams(teams, url_home, fieldnames, options, writer)

  df = pandas.read_csv(filename, encoding="iso-8859-1", header=0,
      sep=",")
  df.drop_duplicates(subset=None, inplace=True)
  df.to_csv(filename, index=False)

def get_browser(options: "Options") -> "Webdriver":
  return webdriver.Firefox(firefox_options=options)

def get_teams(feds: set, url_home: str, options: "Options") -> set:
  pattern = re.compile("^handleLink\(\'(.+)\'\); return false;$")
  teams = set()

  for fed in feds:
    url_fed = os.path.join(url_home, fed)
    browser = get_browser(options)
    print("Read {}".format(url_fed))
    browser.get(url_fed)
    elements = browser.find_elements_by_xpath("//div/a[text()]")

    for element in elements:
      attribute_onclick = element.get_attribute("onclick")
      match = pattern.match(attribute_onclick)

      if not match:
        print("Can't find team name in {}".format(attribute_onclick))
        continue
       
      teams.add(match.group(1))
    
    browser.close()  

  return teams

def get_games_for_all_teams(teams: set, url_home: str, fieldnames: list,
    options: "Options", writer: "DictWriter") -> None:
  for team in teams:
    get_games(team, url_home, fieldnames, options, writer)

def get_games(team: set, url_home: str, fieldnames: list, options: "Options",
    writer: "DictWriter") -> None:
  url_team = os.path.join(url_home, team)
  browser = get_browser(options)
  print("Read {}".format(url_team))
  browser.get(url_team)
  elements_even = browser.find_elements_by_xpath(
      "//div[@class='ui-widget-content slick-row even']")
  elements_odd = browser.find_elements_by_xpath(
      "//div[@class='ui-widget-content slick-row odd']")
  elements = elements_even + elements_odd

  for element in elements:
    get_game(element, fieldnames, writer)

def get_game(element: "FirefoxWebElement", fieldnames: list,
    writer: "DictWriter") -> None:
  data = element.text.split("\n")

  if len(data) < DATA_LEN:
    print("Missing fields in {}".format(data))
    return
  elif len(data[0].split(" ")) < MONTH_DAY_LEN:
    print("Missing day in {}".format(data))
    return

  date = datetime.strptime(data[0] + " " + data[1], "%B %d %Y").date()
  date_str = str(date)
  home_team = unidecode(data[2])
  away_team = unidecode(data[3])
  home_goals = int(data[4])
  away_goals = int(data[5])
  contest = unidecode(data[6])
  home_advantage = int(home_team in unidecode(data[7]))
  row = {
      fieldnames[0]: home_team,
      fieldnames[1]: away_team,
      fieldnames[2]: date_str,
      fieldnames[3]: contest,
      fieldnames[4]: home_goals,
      fieldnames[5]: away_goals,
      fieldnames[6]: home_goals,
      fieldnames[7]: away_goals,
      fieldnames[8]: 0,
      fieldnames[9]: home_advantage}
  writer.writerow(row)

def write_to_file(contents: str, filename: str) -> None:
  with open(filename, "wb") as f:
    f.write(contents)


if __name__ == "__main__":
  main()
