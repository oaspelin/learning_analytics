import os, time, pickle, csv
from tabulate import tabulate
from termcolor import colored
from CustomFeatures import CalculateFeatures

#-----------------------------------------------#
# Function that indents an entire block of text #
#-----------------------------------------------#
def indent(text, prefix, predicate=None):
    if predicate is None:
        def predicate(line):
            return line.strip()
    def prefixed_lines():
        for line in text.splitlines(True):
            yield (prefix + line if predicate(line) else line)
    return ''.join(prefixed_lines())
# END OF FUNCTION

#--------------------------------------#
# Function that pretty-prints data set #
#--------------------------------------#
def PrintDataSet(DataSet):
	
	# Initialize effective row er (rows that have custom features)
	EffectiveRowCounter = 0

	# Loop through all problems
	for ProblemID in DataSet.keys():

		# Print ProblemID with a small pause
		print colored('\n'+'-'*32+'\n\tProblem Number '+str(ProblemID)+'\n'+'-'*32, 'red')
		time.sleep(1)

		# Loop through all students
		for UserID in DataSet[ProblemID].keys():

			# Print UserID
			print colored('\n'+'-'*52+'\nStudent ID: '+UserID+'\n'+'-'*52, 'magenta')

			# Loop through all problem (re-)submissions
			for SubmissionNumber in DataSet[ProblemID][UserID].keys():

				# Print submission number
				print colored('\n\t'+'-'*20+'\n\tSubmission Number '+str(SubmissionNumber)+'\n\t'+'-'*20, 'blue')

				# Print time stamp
				print '\t\t TimeStamp      '+str(DataSet[ProblemID][UserID][SubmissionNumber]['TimeStamp'])
				if SubmissionNumber>0:
					print '\t\t TimeSinceLast  '+str(DataSet[ProblemID][UserID][SubmissionNumber]['TimeSinceLast'])

				# Print video and forum events since last submission
				if SubmissionNumber>0:

					# Get number of video and forum events
					NVideoEvents = DataSet[ProblemID][UserID][SubmissionNumber]['NVideoEvents']
					NForumEvents = DataSet[ProblemID][UserID][SubmissionNumber]['NForumEvents']

					# Print number of video and forum events
					print '\t\t NVideoEvents   '+str(NVideoEvents)
					print '\t\t NForumEvents   '+str(NForumEvents)

					# Print table with video events if they exist
					if NVideoEvents>0:
						print '\n\t\t VideoEvents (since last submission) : '
						print indent(tabulate(zip(*[
							DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents']['TimeStamp'],
							DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents']['EventType'],
							DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents']['VideoID'],
							DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents']['CurrentTime'],
							DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents']['OldTime'],
							DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents']['NewTime'],
							DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents']['SeekType'],
							DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents']['OldSpeed'],
							DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents']['NewSpeed']]),
							headers=['TimeStamp', 'EventType', 'VideoID', 'CurrentTime', 'OldTime', 'NewTime', 'SeekType', 'OldSpeed', 'NewSpeed'], tablefmt="rst"), prefix='\t\t\t\t')

					# Print table with forum events if they exist
					if NForumEvents>0:
						print '\n\t\t ForumEvents (since last submission) : '
						print indent(tabulate(zip(*[
							DataSet[ProblemID][UserID][SubmissionNumber]['ForumEvents']['TimeStamp'],
							DataSet[ProblemID][UserID][SubmissionNumber]['ForumEvents']['EventType'],
							DataSet[ProblemID][UserID][SubmissionNumber]['ForumEvents']['PostType'],
							DataSet[ProblemID][UserID][SubmissionNumber]['ForumEvents']['PostID'],
							DataSet[ProblemID][UserID][SubmissionNumber]['ForumEvents']['PostLength']]),
							headers=['TimeStamp', 'EventType', 'PostType', 'PostID', 'PostLength'], tablefmt="rst"), prefix='\t\t\t\t')

				# Print extra features
				if SubmissionNumber>0:
					# Get feature names
					FeatureNames = DataSet[ProblemID][UserID][SubmissionNumber]['Features'].keys()
					if len(FeatureNames)>0:
						# Count as 1 row
						EffectiveRowCounter += 1
						# Print features
						print colored('\n\t\t Features :', 'cyan')
						for FeatureName in FeatureNames:
							print colored('\t\t\t\t%s : %d' % (FeatureName, DataSet[ProblemID][UserID][SubmissionNumber]['Features'][FeatureName]), 'cyan')
					
				# Print (re-)submission grade and grade difference
				print '\n\t\t Grade          '+str(DataSet[ProblemID][UserID][SubmissionNumber]['Grade'])
				if SubmissionNumber>0:
					GradeDiff = DataSet[ProblemID][UserID][SubmissionNumber]['GradeDiff']
					if GradeDiff>0:
						GradeDiffColor = 'green'
					elif GradeDiff<0:
						GradeDiffColor = 'red'
					else:
						GradeDiffColor = 'grey'
					print colored('\t\t GradeDiff      '+str(GradeDiff), GradeDiffColor)

	# Print end of dataset notice, and number of effective rows 
	print '\nEnd of data set. Number of rows with custom features is %d.\n' % EffectiveRowCounter

# END OF FUNCTION

#--------------------------------------------------------#
# Function that appends new features to (re-)submissions #
#--------------------------------------------------------#
def AppendFeatures(DataSet):

	# Loop through all problems
	for ProblemID in DataSet.keys():

		# Loop through all students
		for UserID in DataSet[ProblemID].keys():

			# Loop through all problem (re-)submissions
			for SubmissionNumber in DataSet[ProblemID][UserID].keys():

				# First submission or re-submission?
				if SubmissionNumber>0:

					# Get number of video and forum events
					NVideoEvents = DataSet[ProblemID][UserID][SubmissionNumber]['NVideoEvents']
					NForumEvents = DataSet[ProblemID][UserID][SubmissionNumber]['NForumEvents']

					# Calculate features for video events
					if NVideoEvents>0 and NForumEvents==0:
						Features = CalculateFeatures(VideoEvents=DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents'])
						DataSet[ProblemID][UserID][SubmissionNumber]['Features'].update(Features)
					elif NVideoEvents==0 and NForumEvents>0:
						Features = CalculateFeatures(ForumEvents=DataSet[ProblemID][UserID][SubmissionNumber]['ForumEvents'])
						DataSet[ProblemID][UserID][SubmissionNumber]['Features'].update(Features)
					elif NVideoEvents>0 and NForumEvents>0:
						Features = CalculateFeatures(VideoEvents=DataSet[ProblemID][UserID][SubmissionNumber]['VideoEvents'], ForumEvents=DataSet[ProblemID][UserID][SubmissionNumber]['ForumEvents'])
						DataSet[ProblemID][UserID][SubmissionNumber]['Features'].update(Features)

	return DataSet
# END OF FUNCTION

#------------------------------------------------------#
# Function that exports data set as table and CSV File #
#------------------------------------------------------#
def ExportAsCSV(DataSet):

	# Print title
	print '\nOutput Table (with custom features):\n'

	# Determine number of custom features and get their names
	FeatureNamesSet = set()
	for ProblemID in DataSet.keys():
		for UserID in DataSet[ProblemID].keys():
			for SubmissionNumber in DataSet[ProblemID][UserID].keys():
				if SubmissionNumber>0:
					FeatureNamesSet.update(DataSet[ProblemID][UserID][SubmissionNumber]['Features'].keys())
	ListOfFeatureNames = list(FeatureNamesSet)

	# Initialize table
	Table = []

	# Loop through all problems
	for ProblemID in DataSet.keys():

		# Loop through all students
		for UserID in DataSet[ProblemID].keys():

			# Loop through all problem (re-)submissions
			for SubmissionNumber in DataSet[ProblemID][UserID].keys():
				
				# Initialize row values
				[TimeStamp, TimeSinceLast, Grade, GradeDiff, NVideoEvents, NForumEvents] = 6*[None]

				# Get time stamps
				TimeStamp = DataSet[ProblemID][UserID][SubmissionNumber]['TimeStamp']
				if SubmissionNumber>0:
					TimeSinceLast = DataSet[ProblemID][UserID][SubmissionNumber]['TimeSinceLast']

				# Get number of video and forum events
				if SubmissionNumber>0:
					NVideoEvents = DataSet[ProblemID][UserID][SubmissionNumber]['NVideoEvents']
					NForumEvents = DataSet[ProblemID][UserID][SubmissionNumber]['NForumEvents']
					
				# Get grade and grade difference
				Grade = DataSet[ProblemID][UserID][SubmissionNumber]['Grade']
				if SubmissionNumber>0:
					GradeDiff = DataSet[ProblemID][UserID][SubmissionNumber]['GradeDiff']
				
				# Build basic row
				Row = [ProblemID, UserID, SubmissionNumber, TimeStamp, TimeSinceLast, Grade, GradeDiff, NVideoEvents, NForumEvents]
				
				# Add custom feature to row
				if SubmissionNumber>0:
					for FeatureName in ListOfFeatureNames:
						if FeatureName in DataSet[ProblemID][UserID][SubmissionNumber]['Features'].keys():
							Row += [DataSet[ProblemID][UserID][SubmissionNumber]['Features'][FeatureName]]
						else:
							Row += [None]
				else:
					for FeatureName in ListOfFeatureNames:
						Row += [None]

				# Add row to table (if there are video or forum events)
				#if NVideoEvents is None and NForumEvents is None:
				Table += [Row]
				#else:
				#	if (Row[7]+Row[8])>0 or SubmissionNumber==len(DataSet[ProblemID][UserID].keys()):
				#		Table += [Row]

	# Build list of output table's column names
	ColumnNames = ['ProblemID', 'UserID', 'SubmissionNumber', 'TimeStamp', 'TimeSinceLast', 'Grade', 'GradeDiff', 'NVideoEvents', 'NForumEvents']
	ColumnNames += ListOfFeatureNames
	
	# Print sample of output table
	#print tabulate(Table[0:160], headers=ColumnNames, tablefmt="fancy_grid")
	#print '(this is a sample of the output table)\n'

	# Save table into CSV file
	with open("../OutputTable2.csv", "w") as f:
		f.write(','.join(ColumnNames)+'\n')
		writer = csv.writer(f)
		writer.writerows(Table)

	# Print success message
	print colored('Success! Table with %d rows saved to file: ./OutputTable.csv\n' % len(Table), 'green')

# END OF FUNCTION


#===============================#
#          MAIN SCRIPT          #
#===============================#

# Get input dataset filepath
FilePath = '../dataset.pickle'

# Load dataset into python dictionary
with open(FilePath, 'rb') as handle:
	DataSet = pickle.load(handle)

# Add custom features to data set
DataSet = AppendFeatures(DataSet)

# Pretty-print updated data set
# PrintDataSet(DataSet)

# Key press pause
os.system('read -s -n 1 -p "Data set will now be saved to CSV file. Press any key to continue..." | echo ""')

# Save data set into CSV file
ExportAsCSV(DataSet)

