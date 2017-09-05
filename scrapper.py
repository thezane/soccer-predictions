from calendar import month_name
from csv import writer
from datetime import date
from io import BytesIO
from lxml import etree
import urllib.parse
import urllib.request
import pdb
#pdb.set_trace()

def get_teams(url, data, headers) -> None:
  print("Getting teams at: {}".format(url))
  req = urllib.request.Request(url, data, headers)

  with urllib.request.urlopen(req) as response:
    fed_page = response.read()
    parser = etree.HTMLParser()
    tree = etree.parse(BytesIO(fed_page), parser)
    teams = tree.xpath(
        "//table/tr/td[text()='rank']" + \
        "/parent::tr/parent::table//a/@href")
    return ["http://www.eloratings.net/{}".format(team)
        for team in teams]

def get_games(url, data, headers) -> None:
  print("Getting games at: {}".format(url))
  req = urllib.request.Request(url, data, headers)

  with urllib.request.urlopen(req) as response:
    team_page = response.read()
    parser = etree.HTMLParser()
    tree = etree.parse(BytesIO(team_page), parser)
    element_games = tree.xpath("//tr[@class='nh']")
   
    for element_game in element_games:
      get_game(element_game)

def get_game(element_game: "ElementTree") -> None:
  month_day_str_list = element_game[0].text.split()
  month_str = month_day_str_list[0]

  if len(month_day_str_list) == 1 and month_str in month_to_int:
    print("Game is missing day:")
    print(etree.tostring(element_game, pretty_print=True))
    day = 1
  elif len(month_day_str_list) != 2:
    print("Game is missing month and day:")
    print(etree.tostring(element_game, pretty_print=True))
    return
  else:
    day = int(month_day_str_list[1])

  month = month_to_int[month_str]
  year = int(element_game[0][0].tail)
  date_str = str(date(year, month, day))
  home_team_name = element_game[1].text
  away_team_name = element_game[1][0].tail
  home_goals = int(element_game[2].text)
  away_goals = int(element_game[2][0].tail)
  contest = element_game[3].text
  home_advantage = home_team_name in element_game[3][0].tail


if __name__ == "__main__":
  month_to_int = {k: v for v, k in enumerate(month_name)}
  user_agent = "Mozilla/5.0 (Windows NT 6.1; Win64; x64)"
  values = {"language" : "Python"}
  headers = {"User-Agent" : user_agent}
  data = urllib.parse.urlencode(values)
  data = data.encode("ascii")
  urls_feds = [
      "europe",
      "america",
      "africa",
      "asia",
      "oceania"]

  for url_fed in urls_feds:
    urls_teams = get_teams(
        "http://www.eloratings.net/{}.html".format(url_fed),
        data, headers)

    for url_team in urls_teams:
      get_games(url_team, data, headers)
