#----------------------------------------#
# Function that computes custom features #
#----------------------------------------#
def CalculateFeatures(VideoEvents=[], ForumEvents=[]):

	# Initialize features dict
	Features = {}

	# Features for video events
	if len(VideoEvents)>0:

		# Calculate custom features
		# Keys: TimeStamp, EventType, VideoID, CurrentTime, OldTime, NewTime, SeekType, OldSpeed, NewSpeed
		TimeStamps = VideoEvents['TimeStamp']
		TimeStampDiffs = [x[0]-x[1] for x in zip(TimeStamps[1:],TimeStamps[:-1])]
		DurationOfVideoActivity = TimeStamps[-1] - TimeStamps[0]
		AverageVideoTimeDiffs = sum(TimeStampDiffs)/max(1,len(TimeStampDiffs))
		
		# Append features to dictionary
		Features.update({
			'DurationOfVideoActivity' : DurationOfVideoActivity,
			'AverageVideoTimeDiffs' : AverageVideoTimeDiffs
		})

	# Features for forum events
	if len(ForumEvents)>0:

		# Calculate custom features
		# Keys: TimeStamp, EventType, PostType, PostID, PostLength
		EventTypes = ForumEvents['EventType']
		NumberOfThreadViews = EventTypes.count('Forum.Thread.View')

		# Append features to dictionary
		Features.update({
			'NumberOfThreadViews' : NumberOfThreadViews
		})

	return Features