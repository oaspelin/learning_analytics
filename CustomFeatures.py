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

		TotalNumberOfVideoEvents = float(NumberOfVideoPlay + \
									NumberOfVideoSeek + \
									NumberOfVideoDownload)
		VideoUniquePerTotalVideoEvent=float(NumberOfVideoUnique/len(VideoEvents))

		VideoPlayScore = 0
		VideoSeekScore = 0
		VideoDownloadScore = 0

		if TotalNumberOfVideoEvents > 0:
			VideoPlayScore = NumberOfVideoPlay/TotalNumberOfVideoEvents
			VideoSeekScore = NumberOfVideoSeek/TotalNumberOfVideoEvents
			VideoDownloadScore = NumberOfVideoDownload//TotalNumberOfVideoEvents

		# Append features to dictionary
		Features.update({
			'DurationOfVideoActivity' : DurationOfVideoActivity,
			'AverageVideoTimeDiffs' : AverageVideoTimeDiffs,
			'NumberOfVideoUnique' : NumberOfVideoUnique,

			'VideoUniquePerTotalVideoEvent': VideoUniquePerTotalVideoEvent,

			'VideoPlayScore' : VideoPlayScore,
			'VideoSeekScore' : VideoSeekScore,
			'VideoDownloadScore' : VideoDownloadScore,
			'VideoEventCountScore' : float(len(VideoEvents))/(len(VideoEvents) + len(ForumEvents)),

			'NumberOfVideoPlay' : NumberOfVideoPlay,
			'NumberOfVideoSeek' : NumberOfVideoSeek,
			'NumberOfVideoDownload' : NumberOfVideoDownload

		})

	# Features for forum events
	if len(ForumEvents)>0:

		# Calculate custom features
		# Keys: TimeStamp, EventType, PostType, PostID, PostLength
		TimeStamps = ForumEvents['TimeStamp']
		TimeStampDiffs = [x[0]-x[1] for x in zip(TimeStamps[1:],TimeStamps[:-1])]
		AverageForumTimeDiffs = sum(TimeStampDiffs)/max(1,len(TimeStampDiffs))

		EventTypes = ForumEvents['EventType']

		NumberOfThreadView = EventTypes.count('Forum.Thread.View')
		NumberOfThreadSubscribe = EventTypes.count('Forum.ThreadSubscribe')

		NumberOfPostUpvotes = EventTypes.count('Forum.Post.Upvote')
		NumberOfPostDownvotes = EventTypes.count('Forum.Post.Downvote')
		NumberOfCommentUpvotes = EventTypes.count('Forum.Comment.Upvote')
		NumberOfCommentDownvotes = EventTypes.count('Forum.Comment.Downvote')

		NumberOfForumVote =  (NumberOfPostUpvotes + NumberOfPostDownvotes + NumberOfCommentUpvotes + NumberOfCommentDownvotes)

		NumberOfThreadLaunch = EventTypes.count('Forum.Thread.Launch')
		NumberOfThreadPostOn = EventTypes.count('Forum.Thread.PostOn')
		NumberOfPostCommentOn = EventTypes.count('Forum.Post.CommentOn')

		TotalNumberOfForumEvents = float(NumberOfThreadView + \
									NumberOfThreadSubscribe + \
									NumberOfForumVote + \
									NumberOfThreadLaunch + \
									NumberOfThreadPostOn + \
									NumberOfPostCommentOn)

		ThreadViewScore = 0
		ThreadSubscribeScore = 0
		ThreadLaunchScore = 0
		ThreadPostOnScore = 0
		PostCommentOnScore = 0
		ForumVoteScore = 0

		if TotalNumberOfForumEvents > 0:
			ThreadViewScore = NumberOfThreadView/TotalNumberOfForumEvents
			ThreadSubscribeScore = NumberOfThreadSubscribe/TotalNumberOfForumEvents
			ThreadLaunchScore = NumberOfThreadLaunch/TotalNumberOfForumEvents
			ThreadPostOnScore = NumberOfThreadPostOn/TotalNumberOfForumEvents
			PostCommentOnScore = NumberOfPostCommentOn/TotalNumberOfForumEvents
			ForumVoteScore = NumberOfForumVote/TotalNumberOfForumEvents

		# Append features to dictionary
		Features.update({
			'AverageForumTimeDiffs' : AverageForumTimeDiffs,

			'ThreadViewScore' : ThreadViewScore,
			'ThreadSubscribeScore' : ThreadSubscribeScore,
			'ThreadLaunchScore' : ThreadLaunchScore,
			'ThreadPostOnScore' : ThreadPostOnScore,
			'PostCommentOnScore' : PostCommentOnScore,
			'ForumVoteScore' : ForumVoteScore,
			'ForumEventCountScore' : float(len(ForumEvents))/(len(VideoEvents) + len(ForumEvents)),

			'NumberOfThreadView' : NumberOfThreadView,
			'NumberOfThreadSubscribe' : NumberOfThreadSubscribe,
			'NumberOfThreadLaunch' : NumberOfThreadLaunch,
			'NumberOfThreadPostOn' : NumberOfThreadPostOn,
			'NumberOfPostCommentOn' : NumberOfPostCommentOn,
			'NumberOfForumVote' : NumberOfForumVote

		})

	return Features
