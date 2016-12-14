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

		# is currently not adding any value, consider changing this derived feature
		# VideoScore = 0.25 * NumberOfVideoUnique + \
		# 1 * NumberOfVideoPlay + \
		# 0.2 * NumberOfVideoSeek + \
		# 0.25 * NumberOfVideoDownload

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

			'VideoPlayScore' : VideoPlayScore,
			'VideoSeekScore' : VideoSeekScore,
			'VideoDownloadScore' : VideoDownloadScore,

			'VideoEventCountScore' : float(len(VideoEvents))/(len(VideoEvents) + len(ForumEvents))
			# 'VideoScore' : VideoScore,
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

		# ForumScore = 1.5 * NumberOfThreadSubscribe + \
		# 3 * NumberOfThreadLaunch + \
		# 2 * NumberOfThreadPostOn + \
		# 2 * NumberOfPostCommentOn + \
		# 0.5 * NumberOfForumVotes + \
		# 0.25 * NumberOfThreadViews

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

			'ForumEventCountScore' : float(len(ForumEvents))/(len(VideoEvents) + len(ForumEvents))
			# 'ForumScore' : ForumScore
		})





	return Features
