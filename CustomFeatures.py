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

		VideoTypes = VideoEvents['EventType']

		NumberOfVideoUnique = float(len(set(VideoEvents['VideoID'])))
		
		NumberOfVideoPlaysPerVideo = VideoTypes.count('Video.Play')/NumberOfVideoUnique
		NumberOfVideoSeekPerVideo = VideoTypes.count('Video.Seek')/NumberOfVideoUnique
		#NumberOfVideoPausePerVideo = VideoTypes.count('Video.Pause')/NumberOfVideoUnique

		NumberOfVideoDownload = VideoTypes.count('Video.Download')

		# Append features to dictionary
		Features.update({
			'DurationOfVideoActivity' : DurationOfVideoActivity,
			'AverageVideoTimeDiffs' : AverageVideoTimeDiffs,
			'NumberOfVideoUnique' : NumberOfVideoUnique,
			'NumberOfVideoPlaysPerVideo' : NumberOfVideoPlaysPerVideo,
			'NumberOfVideoSeekPerVideo' : NumberOfVideoSeekPerVideo,
			#'NumberOfVideoPausePerVideo' : NumberOfVideoPausePerVideo,
			'NumberOfVideoDownload' : NumberOfVideoDownload
		})

	# Features for forum events
	if len(ForumEvents)>0:

		# Calculate custom features
		# Keys: TimeStamp, EventType, PostType, PostID, PostLength
		EventTypes = ForumEvents['EventType']
		
		NumberOfThreadViews = EventTypes.count('Forum.Thread.View')
		NumberOfThreadSubscribe = EventTypes.count('Forum.Thread.Subscribe')
		
		NumberOfPostUpvotes = EventTypes.count('Forum.Post.Upvote')
		NumberOfPostDownvotes = EventTypes.count('Forum.Post.Downvote')
		NumberOfCommentUpvotes = EventTypes.count('Forum.Comment.Upvote')
		NumberOfCommentDownvotes = EventTypes.count('Forum.Comment.Downvote')

		NumberOfForumVotes =  NumberOfPostUpvotes + NumberOfPostDownvotes + NumberOfCommentUpvotes + NumberOfCommentDownvotes
		
		NumberOfThreadLaunch = EventTypes.count('Forum.Thread.Launch')
		NumberOfThreadPostOn = EventTypes.count('Forum.Thread.PostOn')
		NumberOfPostCommentOn = EventTypes.count('Forum.Post.CommentOn')
		
		a = 3
		b = 2
		g = 1
		
		ForumActivity = a * NumberOfThreadLaunch + b * NumberOfThreadPostOn + g * NumberOfPostCommentOn
		
		# Append features to dictionary
		Features.update({
			'NumberOfThreadViews' : NumberOfThreadViews,
			'NumberOfThreadSubscribe' : NumberOfThreadSubscribe,
			'NumberOfForumVotes' : NumberOfForumVotes,
			'ForumActivity' : ForumActivity
		})

	return Features