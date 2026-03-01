package lila.openingPractice

import reactivemongo.api.bson.Macros.Annotations.Key

// Practice mode for opening practice
enum PracticeMode:
  case Learning
  case Drilling

// Status of a line in the learning process
enum LineStatus:
  case NotStarted
  case Learning
  case Learned
  case Mastered
  def isLearned = this == Learned || this == Mastered
  def isMastered = this == Mastered

// Statistics for drilling mode
case class DrillingStats(
    currentStreak: Int = 0,
    bestStreak: Int = 0,
    totalDrills: Int = 0
):
  def afterCorrectDrill: DrillingStats =
    val newStreak = currentStreak + 1
    copy(
      currentStreak = newStreak,
      bestStreak = math.max(bestStreak, newStreak),
      totalDrills = totalDrills + 1
    )

  def afterMistake: DrillingStats =
    copy(currentStreak = 0, totalDrills = totalDrills + 1)

// Progress on a single opening line
case class LineProgress(
    status: LineStatus = LineStatus.NotStarted,
    attempts: Int = 0,
    mistakes: Int = 0,
    lastMistakeAt: Option[Instant] = None,
    drilling: DrillingStats = DrillingStats()
):
  def withAttempt: LineProgress =
    copy(attempts = attempts + 1)

  def withMistake: LineProgress =
    copy(
      mistakes = mistakes + 1,
      lastMistakeAt = Some(nowInstant)
    )

  def isLearned: Boolean = status.isLearned
  def isMastered: Boolean = status.isMastered

// User's progress across all opening lines
case class UserOpeningProgress(
    @Key("_id") id: UserId,
    lines: Map[OpeningLineId, LineProgress] = Map.empty,
    currentMode: PracticeMode = PracticeMode.Learning,
    createdAt: Instant,
    updatedAt: Instant
):
  def withLineProgress(lineId: OpeningLineId, progress: LineProgress): UserOpeningProgress =
    copy(
      lines = lines.updated(lineId, progress),
      updatedAt = nowInstant
    )

  def withMode(mode: PracticeMode): UserOpeningProgress =
    copy(currentMode = mode, updatedAt = nowInstant)

  def learnedLines: Set[OpeningLineId] =
    lines.filter(_._2.isLearned).keySet

  def masteredLines: Set[OpeningLineId] =
    lines.filter(_._2.isMastered).keySet

  def completionPercent(totalLines: Int): Int =
    if totalLines == 0 then 0
    else (learnedLines.size * 100) / totalLines

object UserOpeningProgress:
  def empty(userId: UserId): UserOpeningProgress =
    UserOpeningProgress(
      id = userId,
      lines = Map.empty,
      currentMode = PracticeMode.Learning,
      createdAt = nowInstant,
      updatedAt = nowInstant
    )
