#----------------------------------------#
# Function that computes custom features #
#----------------------------------------#
def CalculateFeatures(VideoEvents=[], ForumEvents=[]):

	# Initialize features dict
	Features = {}
		
	NumberOfVideoUnique =0
	NumberOfVideoPlay =0
	NumberOfVideoSeek =0
	NumberOfVideoDownload =0

	TotalNumberOfVideoEvents=0
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
									NumberOfVideoDownload+\
									NumberOfVideoUnique)
		VideoUniquePerTotalNumberVideoEvent=float(NumberOfVideoUnique/len(VideoEvents))


		# Append features to dictionary
		Features.update({
			'DurationOfVideoActivity' : DurationOfVideoActivity,
			'AverageVideoTimeDiffs' : AverageVideoTimeDiffs,
		
			'VideoUniquePerTotalNumberVideoEvent': VideoUniquePerTotalNumberVideoEvent,

			'VideoEventCountScore' : float(len(VideoEvents))/(len(VideoEvents) + len(ForumEvents)),
			'NumberOfVideoUnique' : NumberOfVideoUnique,
			'NumberOfVideoPlay' : NumberOfVideoPlay,
			'NumberOfVideoSeek' : NumberOfVideoSeek,
			'NumberOfVideoDownload' : NumberOfVideoDownload

		})


	NumberOfThreadView = 0
	NumberOfThreadSubscribe = 0
	NumberOfPostUpvotes = 0
	NumberOfPostDownvotes = 0
	NumberOfCommentUpvotes = 0
	NumberOfCommentDownvotes = 0

	NumberOfForumVote = 0

	NumberOfThreadLaunch = 0
	NumberOfThreadPostOn = 0
	NumberOfPostCommentOn = 0
	NumberOfForumLoad = 0
	TotalNumberOfForumEvents=0
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
		NumberOfForumLoad = EventTypes.count('Forum.Load')

		TotalNumberOfForumEvents = float(NumberOfThreadView + \
									NumberOfThreadSubscribe + \
									NumberOfForumVote + \
									NumberOfThreadLaunch + \
									NumberOfThreadPostOn + \
									NumberOfPostCommentOn+\
									NumberOfForumLoad)


		# Append features to dictionary
		Features.update({
			'AverageForumTimeDiffs' : AverageForumTimeDiffs,

			'ForumEventCountScore' : float(len(ForumEvents))/(len(VideoEvents) + len(ForumEvents)),

			'NumberOfThreadView' : NumberOfThreadView,
			'NumberOfThreadSubscribe' : NumberOfThreadSubscribe,
			'NumberOfThreadLaunch' : NumberOfThreadLaunch,
			'NumberOfThreadPostOn' : NumberOfThreadPostOn,
			'NumberOfPostCommentOn' : NumberOfPostCommentOn,
			'NumberOfForumVote' : NumberOfForumVote,
			'NumberOfForumLoad' : NumberOfForumLoad
		})

	if TotalNumberOfForumEvents>0:
		Features.update({
			'ThreadViewScore': NumberOfThreadView/float(TotalNumberOfForumEvents+TotalNumberOfVideoEvents),
			'ThreadSubscribeScore': NumberOfThreadSubscribe/float(TotalNumberOfForumEvents+TotalNumberOfVideoEvents),
			'ThreadLaunchScore': NumberOfThreadLaunch/float(TotalNumberOfForumEvents+TotalNumberOfVideoEvents),
			'ThreadPostOnScore': NumberOfThreadPostOn/float(TotalNumberOfForumEvents+TotalNumberOfVideoEvents),
			'PostCommentOnScore': NumberOfPostCommentOn/float(TotalNumberOfForumEvents+TotalNumberOfVideoEvents),
			'ForumVoteScore': NumberOfForumVote/float(TotalNumberOfForumEvents+TotalNumberOfVideoEvents),
			'ForumLoadScore': NumberOfForumLoad/float(TotalNumberOfForumEvents+TotalNumberOfVideoEvents)
		})
	if TotalNumberOfVideoEvents>0:
		Features.update({
			'VideoUniqueScore' : NumberOfVideoUnique/float(TotalNumberOfVideoEvents+TotalNumberOfForumEvents),
			'VideoPlayScore' : NumberOfVideoPlay/float(TotalNumberOfVideoEvents+TotalNumberOfForumEvents),
			'VideoSeekScore' : NumberOfVideoSeek/float(TotalNumberOfVideoEvents+TotalNumberOfForumEvents),
			'VideoDownloadScore' : NumberOfVideoDownload/float(TotalNumberOfVideoEvents+TotalNumberOfForumEvents)

		})	

	return Features
