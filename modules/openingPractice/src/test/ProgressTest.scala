package lila.openingPractice

import play.api.libs.json.*
import reactivemongo.api.bson.*

class ProgressTest extends munit.FunSuite:

  import BsonHandlers.{ given }
  import OpeningPracticeJson.{ given }

  val testUserId = UserId("testUser")
  val lineId1 = OpeningLineId("line1")
  val lineId2 = OpeningLineId("line2")
  val testInstant = nowInstant

  // Shared fixture: progress with one line per status
  val allStatusProgress = UserOpeningProgress(
    id = testUserId,
    lines = Map(
      OpeningLineId("line1") -> LineProgress(status = LineStatus.NotStarted),
      OpeningLineId("line2") -> LineProgress(status = LineStatus.Learning),
      OpeningLineId("line3") -> LineProgress(status = LineStatus.Learned),
      OpeningLineId("line4") -> LineProgress(status = LineStatus.Mastered)
    ),
    currentMode = PracticeMode.Learning,
    createdAt = testInstant,
    updatedAt = testInstant
  )

  // PracticeMode BSON

  test("PracticeMode.Learning BSON round-trip"):
    val mode = PracticeMode.Learning
    val bson = summon[BSONHandler[PracticeMode]].writeTry(mode).get
    assertEquals(bson, BSONString("learning"))
    val restored = summon[BSONHandler[PracticeMode]].readTry(bson).get
    assertEquals(restored, mode)

  test("PracticeMode.Drilling BSON round-trip"):
    val mode = PracticeMode.Drilling
    val bson = summon[BSONHandler[PracticeMode]].writeTry(mode).get
    assertEquals(bson, BSONString("drilling"))
    val restored = summon[BSONHandler[PracticeMode]].readTry(bson).get
    assertEquals(restored, mode)

  test("PracticeMode BSON rejects invalid values"):
    val result = summon[BSONHandler[PracticeMode]].readTry(BSONString("invalid"))
    assert(result.isFailure, "Should fail on invalid practice mode")

  // LineStatus BSON

  test("LineStatus.NotStarted BSON round-trip"):
    val status = LineStatus.NotStarted
    val bson = summon[BSONHandler[LineStatus]].writeTry(status).get
    assertEquals(bson, BSONString("notStarted"))
    val restored = summon[BSONHandler[LineStatus]].readTry(bson).get
    assertEquals(restored, status)

  test("LineStatus.Learning BSON round-trip"):
    val status = LineStatus.Learning
    val bson = summon[BSONHandler[LineStatus]].writeTry(status).get
    assertEquals(bson, BSONString("learning"))
    val restored = summon[BSONHandler[LineStatus]].readTry(bson).get
    assertEquals(restored, status)

  test("LineStatus.Learned BSON round-trip"):
    val status = LineStatus.Learned
    val bson = summon[BSONHandler[LineStatus]].writeTry(status).get
    assertEquals(bson, BSONString("learned"))
    val restored = summon[BSONHandler[LineStatus]].readTry(bson).get
    assertEquals(restored, status)

  test("LineStatus.Mastered BSON round-trip"):
    val status = LineStatus.Mastered
    val bson = summon[BSONHandler[LineStatus]].writeTry(status).get
    assertEquals(bson, BSONString("mastered"))
    val restored = summon[BSONHandler[LineStatus]].readTry(bson).get
    assertEquals(restored, status)

  test("LineStatus BSON rejects invalid values"):
    val result = summon[BSONHandler[LineStatus]].readTry(BSONString("invalid"))
    assert(result.isFailure, "Should fail on invalid line status")

  // DrillingStats BSON and domain logic

  test("DrillingStats BSON round-trip"):
    val stats = DrillingStats(currentStreak = 5, bestStreak = 10, totalDrills = 50)
    val bson = summon[BSONDocumentHandler[DrillingStats]].writeTry(stats).get
    val restored = summon[BSONDocumentHandler[DrillingStats]].readTry(bson).get
    assertEquals(restored, stats)

  test("DrillingStats empty state BSON round-trip"):
    val stats = DrillingStats()
    val bson = summon[BSONDocumentHandler[DrillingStats]].writeTry(stats).get
    val restored = summon[BSONDocumentHandler[DrillingStats]].readTry(bson).get
    assertEquals(restored, stats)

  test("DrillingStats.afterCorrectDrill increments streak and total"):
    val stats = DrillingStats(currentStreak = 5, bestStreak = 8, totalDrills = 20)
    val updated = stats.afterCorrectDrill
    assertEquals(updated.currentStreak, 6)
    assertEquals(updated.bestStreak, 8)
    assertEquals(updated.totalDrills, 21)

  test("DrillingStats.afterCorrectDrill updates best streak"):
    val stats = DrillingStats(currentStreak = 8, bestStreak = 8, totalDrills = 20)
    val updated = stats.afterCorrectDrill
    assertEquals(updated.currentStreak, 9)
    assertEquals(updated.bestStreak, 9)

  test("DrillingStats.afterMistake resets current streak"):
    val stats = DrillingStats(currentStreak = 5, bestStreak = 10, totalDrills = 20)
    val updated = stats.afterMistake
    assertEquals(updated.currentStreak, 0)
    assertEquals(updated.bestStreak, 10)
    assertEquals(updated.totalDrills, 21)

  // LineProgress BSON and domain logic

  test("LineProgress BSON round-trip"):
    val progress = LineProgress(
      status = LineStatus.Learning,
      attempts = 5,
      mistakes = 2,
      lastMistakeAt = Some(testInstant),
      drilling = DrillingStats(currentStreak = 3, bestStreak = 5, totalDrills = 10)
    )
    val bson = summon[BSONDocumentHandler[LineProgress]].writeTry(progress).get
    val restored = summon[BSONDocumentHandler[LineProgress]].readTry(bson).get
    assertEquals(restored.status, progress.status)
    assertEquals(restored.attempts, progress.attempts)
    assertEquals(restored.mistakes, progress.mistakes)
    assertEquals(restored.lastMistakeAt.map(_.toMillis), progress.lastMistakeAt.map(_.toMillis))
    assertEquals(restored.drilling, progress.drilling)

  test("LineProgress empty state BSON round-trip"):
    val progress = LineProgress()
    val bson = summon[BSONDocumentHandler[LineProgress]].writeTry(progress).get
    val restored = summon[BSONDocumentHandler[LineProgress]].readTry(bson).get
    assertEquals(restored, progress)

  test("LineProgress.isLearned returns false for NotStarted"):
    assertEquals(LineProgress(status = LineStatus.NotStarted).isLearned, false)

  test("LineProgress.isLearned returns false for Learning"):
    assertEquals(LineProgress(status = LineStatus.Learning).isLearned, false)

  test("LineProgress.isLearned returns true for Learned"):
    assertEquals(LineProgress(status = LineStatus.Learned).isLearned, true)

  test("LineProgress.isLearned returns true for Mastered"):
    assertEquals(LineProgress(status = LineStatus.Mastered).isLearned, true)

  test("LineProgress.isMastered returns true only for Mastered"):
    assertEquals(LineProgress(status = LineStatus.NotStarted).isMastered, false)
    assertEquals(LineProgress(status = LineStatus.Learning).isMastered, false)
    assertEquals(LineProgress(status = LineStatus.Learned).isMastered, false)
    assertEquals(LineProgress(status = LineStatus.Mastered).isMastered, true)

  test("LineProgress.withAttempt increments attempts"):
    val progress = LineProgress(attempts = 5)
    assertEquals(progress.withAttempt.attempts, 6)

  test("LineProgress.withMistake increments mistakes and sets timestamp"):
    val progress = LineProgress(mistakes = 2, lastMistakeAt = None)
    val updated = progress.withMistake
    assertEquals(updated.mistakes, 3)
    assert(updated.lastMistakeAt.isDefined, "lastMistakeAt should be set")

  // UserOpeningProgress BSON and domain logic

  test("UserOpeningProgress BSON round-trip"):
    val progress = UserOpeningProgress(
      id = testUserId,
      lines = Map(
        lineId1 -> LineProgress(status = LineStatus.Learned, attempts = 10),
        lineId2 -> LineProgress(status = LineStatus.Learning, attempts = 5)
      ),
      currentMode = PracticeMode.Drilling,
      createdAt = testInstant,
      updatedAt = testInstant
    )
    val bson = summon[BSONDocumentHandler[UserOpeningProgress]].writeTry(progress).get
    val restored = summon[BSONDocumentHandler[UserOpeningProgress]].readTry(bson).get
    assertEquals(restored.id, progress.id)
    assertEquals(restored.lines, progress.lines)
    assertEquals(restored.currentMode, progress.currentMode)
    assertEquals(restored.createdAt.toMillis, progress.createdAt.toMillis)
    assertEquals(restored.updatedAt.toMillis, progress.updatedAt.toMillis)

  test("UserOpeningProgress empty state BSON round-trip"):
    val progress = UserOpeningProgress.empty(testUserId)
    val bson = summon[BSONDocumentHandler[UserOpeningProgress]].writeTry(progress).get
    val restored = summon[BSONDocumentHandler[UserOpeningProgress]].readTry(bson).get
    assertEquals(restored.id, testUserId)
    assertEquals(restored.lines, Map.empty)
    assertEquals(restored.currentMode, PracticeMode.Learning)

  test("UserOpeningProgress.empty creates default state"):
    val progress = UserOpeningProgress.empty(testUserId)
    assertEquals(progress.id, testUserId)
    assertEquals(progress.lines, Map.empty)
    assertEquals(progress.currentMode, PracticeMode.Learning)
    assert(progress.createdAt != null)
    assert(progress.updatedAt != null)

  test("UserOpeningProgress.withLineProgress adds new line"):
    val progress = UserOpeningProgress.empty(testUserId)
    val lineProgress = LineProgress(status = LineStatus.Learning, attempts = 5)
    val updated = progress.withLineProgress(lineId1, lineProgress)
    assertEquals(updated.lines.get(lineId1), Some(lineProgress))

  test("UserOpeningProgress.withLineProgress updates existing line"):
    val initial = LineProgress(status = LineStatus.Learning, attempts = 5)
    val progress = UserOpeningProgress.empty(testUserId).withLineProgress(lineId1, initial)
    val updated = LineProgress(status = LineStatus.Learned, attempts = 10)
    val result = progress.withLineProgress(lineId1, updated)
    assertEquals(result.lines.get(lineId1), Some(updated))

  test("UserOpeningProgress.withMode changes practice mode"):
    val progress = UserOpeningProgress.empty(testUserId)
    val updated = progress.withMode(PracticeMode.Drilling)
    assertEquals(updated.currentMode, PracticeMode.Drilling)

  test("UserOpeningProgress.learnedLines returns only learned and mastered"):
    val learned = allStatusProgress.learnedLines
    assertEquals(learned.size, 2)
    assert(learned.contains(OpeningLineId("line3")))
    assert(learned.contains(OpeningLineId("line4")))

  test("UserOpeningProgress.masteredLines returns only mastered"):
    val mastered = allStatusProgress.masteredLines
    assertEquals(mastered.size, 1)
    assert(mastered.contains(OpeningLineId("line4")))

  test("UserOpeningProgress.completionPercent calculates correctly"):
    val progress = UserOpeningProgress(
      id = testUserId,
      lines = Map(
        OpeningLineId("line1") -> LineProgress(status = LineStatus.Learned),
        OpeningLineId("line2") -> LineProgress(status = LineStatus.Learning),
        OpeningLineId("line3") -> LineProgress(status = LineStatus.Mastered)
      ),
      currentMode = PracticeMode.Learning,
      createdAt = testInstant,
      updatedAt = testInstant
    )
    // 2 learned out of 5 total = 40%
    assertEquals(progress.completionPercent(5), 40)

  test("UserOpeningProgress.completionPercent handles zero total"):
    assertEquals(UserOpeningProgress.empty(testUserId).completionPercent(0), 0)

  test("UserOpeningProgress.completionPercent handles empty lines"):
    assertEquals(UserOpeningProgress.empty(testUserId).completionPercent(10), 0)

  // JSON

  test("UserOpeningProgress JSON structure"):
    val progress = UserOpeningProgress(
      id = testUserId,
      lines = Map(
        lineId1 -> LineProgress(
          status = LineStatus.Learned,
          attempts = 10,
          mistakes = 2,
          lastMistakeAt = Some(testInstant),
          drilling = DrillingStats(currentStreak = 5, bestStreak = 10, totalDrills = 20)
        )
      ),
      currentMode = PracticeMode.Drilling,
      createdAt = testInstant,
      updatedAt = testInstant
    )

    val json = Json.toJson(progress)
    val obj = json.as[JsObject]

    assertEquals((obj \ "id").as[String], "testUser")
    assertEquals((obj \ "currentMode").as[String], "drilling")
    assertEquals((obj \ "createdAt").as[Long], testInstant.toMillis)
    assertEquals((obj \ "updatedAt").as[Long], testInstant.toMillis)

    val lines = (obj \ "lines").as[JsObject]
    val line1 = (lines \ "line1").as[JsObject]
    assertEquals((line1 \ "status").as[String], "learned")
    assertEquals((line1 \ "attempts").as[Int], 10)
    assertEquals((line1 \ "mistakes").as[Int], 2)
    assertEquals((line1 \ "lastMistakeAt").as[Long], testInstant.toMillis)

    val drilling = (line1 \ "drilling").as[JsObject]
    assertEquals((drilling \ "currentStreak").as[Int], 5)
    assertEquals((drilling \ "bestStreak").as[Int], 10)
    assertEquals((drilling \ "totalDrills").as[Int], 20)

  test("UserOpeningProgress JSON empty state"):
    val progress = UserOpeningProgress.empty(testUserId)
    val json = Json.toJson(progress)
    val obj = json.as[JsObject]

    assertEquals((obj \ "id").as[String], "testUser")
    assertEquals((obj \ "currentMode").as[String], "learning")
    val lines = (obj \ "lines").as[JsObject]
    assertEquals(lines.fields.size, 0)

  test("LineProgress JSON omits lastMistakeAt when None"):
    val progress = LineProgress(status = LineStatus.Learning)
    val json = Json.toJson(progress)
    val obj = json.as[JsObject]
    assert((obj \ "lastMistakeAt").toOption.isEmpty, "lastMistakeAt should be omitted when None")
