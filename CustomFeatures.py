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

		NumberOfVideoPlay = VideoTypes.count('Video.Play')
		NumberOfVideoSeek = VideoTypes.count('Video.Seek')

		NumberOfVideoDownload = VideoTypes.count('Video.Download')


		# is currently not adding any value, consider changing this derived feature
		VideoScore = 0.25 * NumberOfVideoUnique + \
		1 * NumberOfVideoPlay + \
		0.2 * NumberOfVideoSeek + \
		0.25 * NumberOfVideoDownload

		# Append features to dictionary
		Features.update({
			'DurationOfVideoActivity' : DurationOfVideoActivity,
			'AverageVideoTimeDiffs' : AverageVideoTimeDiffs,
			'VideoScore' : VideoScore
		})

	# Features for forum events
	if len(ForumEvents)>0:

		# Calculate custom features
		# Keys: TimeStamp, EventType, PostType, PostID, PostLength
		TimeStamps = ForumEvents['TimeStamp']
		TimeStampDiffs = [x[0]-x[1] for x in zip(TimeStamps[1:],TimeStamps[:-1])]
		AverageForumTimeDiffs = sum(TimeStampDiffs)/max(1,len(TimeStampDiffs))

		EventTypes = ForumEvents['EventType']

		NumberOfThreadViews = EventTypes.count('Forum.Thread.View')
		NumberOfThreadSubscribe = EventTypes.count('Forum.ThreadSubscribe')

		NumberOfPostUpvotes = EventTypes.count('Forum.Post.Upvote')
		NumberOfPostDownvotes = EventTypes.count('Forum.Post.Downvote')
		NumberOfCommentUpvotes = EventTypes.count('Forum.Comment.Upvote')
		NumberOfCommentDownvotes = EventTypes.count('Forum.Comment.Downvote')

		NumberOfForumVotes =  NumberOfPostUpvotes + NumberOfPostDownvotes + NumberOfCommentUpvotes + NumberOfCommentDownvotes

		NumberOfThreadLaunch = EventTypes.count('Forum.Thread.Launch')
		NumberOfThreadPostOn = EventTypes.count('Forum.Thread.PostOn')
		NumberOfPostCommentOn = EventTypes.count('Forum.Post.CommentOn')

		ForumScore = 1.5 * NumberOfThreadSubscribe + \
		3 * NumberOfThreadLaunch + \
		2 * NumberOfThreadPostOn + \
		2 * NumberOfPostCommentOn + \
		0.5 * NumberOfForumVotes + \
		0.25 * NumberOfThreadViews

		# Append features to dictionary
		Features.update({
			'AverageForumTimeDiffs' : AverageForumTimeDiffs,
			'ForumScore' : ForumScore
		})

	return Features
