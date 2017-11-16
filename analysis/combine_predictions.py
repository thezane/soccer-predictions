import csv

IN_ROOT = "../output/"
in_file_names = (
"odms-matches-bivpois-odmiter1.csv",
"odms-matches-bivpois.csv",
"odms-matches-odmiter1.csv",
"odms-matches-softmax-diff.csv",
"odms-matches-softmax-geomdiff-odmiter1.csv",
"odms-matches-softmax-geomdiff.csv",
"odms-matches-softmax-nonmov-odmiter1.csv",
"odms-matches-softmax-nonmov.csv",
"odms-matches-softmax-odmiter1.csv",
"odms-matches-softmax.csv",
"odms-matches.csv",
)

OUT_ROOT = "./output/"

csv_header = ("HomeTeam","AwayTeam","Date","Contest","HomeGoals","AwayGoals","HomeAdvantage","HomeStrAgg","AwayStrAgg","HomeStrAggNext","AwayStrAggNext","HomeWin","Tie","AwayWin","SSE")
row_len = len(csv_header)
m = {csv_header[i]:i for i in range(len(csv_header))}

homewin = []

for filenum in range(len(in_file_names)):
	filename = in_file_names[filenum]
	with open(IN_ROOT + filename,'r', encoding='windows-1252') as csvfile:
		reader = csv.reader(csvfile, delimiter=',', quotechar='"')
		this_csv_header = None
		datarow_ind = 0
		for row in reader:
			assert len(row) == row_len
			if not this_csv_header:
				# just to check that they all have the expected header
				this_csv_header = row
				assert tuple(this_csv_header) == csv_header
			else:
				if datarow_ind > len(homewin) - 1:
					homewin.append([])
				homewin[datarow_ind].append(row[m["HomeWin"]])
				datarow_ind += 1

	with open(OUT_ROOT + "HomeWin.csv", 'w', encoding='windows-1252') as homewin_out_file:
		homewin_out_file.writelines((",".join(row) + "\n" for row in homewin))



