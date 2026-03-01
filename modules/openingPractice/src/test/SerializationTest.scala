package lila.openingPractice

import chess.Color
import chess.format.Uci
import play.api.libs.json.*
import reactivemongo.api.bson.*

class SerializationTest extends munit.FunSuite:

  import BsonHandlers.{ given }
  import OpeningPracticeJson.{ given }

  // Helper to create AnnotatedMove without comment
  def move(uci: String): AnnotatedMove = AnnotatedMove(Uci(uci).get, None)
  def move(uci: String, comment: String): AnnotatedMove = AnnotatedMove(Uci(uci).get, Some(comment))

  // Test fixtures
  val testFamilyId = OpeningFamilyId("italian-game")
  val testGroupId = OpeningGroupId("giuoco-piano")
  val testLineId1 = OpeningLineId("classical-variation")
  val testLineId2 = OpeningLineId("evans-gambit")

  val testFamily = OpeningFamily(
    id = testFamilyId,
    name = "Italian Game",
    key = "italian-game"
  )

  val testLine = OpeningLine(
    id = testLineId1,
    name = "Classical Variation",
    eco = Some("C53"),
    moves = NonEmptyList.of(
      move("e2e4", "Controls the center"),
      move("e7e5"),
      move("g1f3", "Develops knight, attacks e5"),
      move("b8c6"),
      move("f1c4"),
      move("f8c5")
    ),
    description = Some("The main line of the Italian Game")
  )

  val testLineNoDescription = OpeningLine(
    id = testLineId2,
    name = "Evans Gambit",
    eco = Some("C51"),
    moves = NonEmptyList.of(
      move("e2e4"),
      move("e7e5"),
      move("g1f3"),
      move("b8c6")
    ),
    description = None
  )

  val testGroup = OpeningGroup(
    id = testGroupId,
    name = "Giuoco Piano",
    familyId = testFamilyId,
    tier = 1,
    lines = NonEmptyList.of(testLineId1, testLineId2),
    color = Color.White,
    prerequisites = List(OpeningGroupId("basic-tactics"), OpeningGroupId("center-control"))
  )

  val testGroupNoPrereqs = OpeningGroup(
    id = OpeningGroupId("beginner-group"),
    name = "Beginner Group",
    familyId = testFamilyId,
    tier = 0,
    lines = NonEmptyList.of(testLineId1),
    color = Color.Black,
    prerequisites = List.empty
  )

  // BSON Round-Trip Tests

  test("OpeningFamily BSON round-trip"):
    val bson = summon[BSONDocumentHandler[OpeningFamily]].writeTry(testFamily).get
    val restored = summon[BSONDocumentHandler[OpeningFamily]].readTry(bson).get
    assertEquals(restored, testFamily)

  test("OpeningLine BSON round-trip with description"):
    val bson = summon[BSONDocumentHandler[OpeningLine]].writeTry(testLine).get
    val restored = summon[BSONDocumentHandler[OpeningLine]].readTry(bson).get
    assertEquals(restored, testLine)

  test("OpeningLine BSON round-trip without description"):
    val bson = summon[BSONDocumentHandler[OpeningLine]].writeTry(testLineNoDescription).get
    val restored = summon[BSONDocumentHandler[OpeningLine]].readTry(bson).get
    assertEquals(restored, testLineNoDescription)

  test("OpeningGroup BSON round-trip with prerequisites"):
    val bson = summon[BSONDocumentHandler[OpeningGroup]].writeTry(testGroup).get
    val restored = summon[BSONDocumentHandler[OpeningGroup]].readTry(bson).get
    assertEquals(restored, testGroup)

  test("OpeningGroup BSON round-trip without prerequisites"):
    val bson = summon[BSONDocumentHandler[OpeningGroup]].writeTry(testGroupNoPrereqs).get
    val restored = summon[BSONDocumentHandler[OpeningGroup]].readTry(bson).get
    assertEquals(restored, testGroupNoPrereqs)

  // BSON Structure Tests

  test("AnnotatedMove BSON round-trip without comment"):
    val m = move("e2e4")
    val bson = summon[BSONDocumentHandler[AnnotatedMove]].writeTry(m).get
    val restored = summon[BSONDocumentHandler[AnnotatedMove]].readTry(bson).get
    assertEquals(restored, m)
    // Should not have comment field
    assert(bson.getAsOpt[String]("comment").isEmpty)

  test("AnnotatedMove BSON round-trip with comment"):
    val m = move("e2e4", "Controls the center")
    val bson = summon[BSONDocumentHandler[AnnotatedMove]].writeTry(m).get
    val restored = summon[BSONDocumentHandler[AnnotatedMove]].readTry(bson).get
    assertEquals(restored, m)
    assertEquals(bson.getAsOpt[String]("comment"), Some("Controls the center"))

  test("NonEmptyList[AnnotatedMove] BSON serialization - array of documents"):
    val moves = NonEmptyList.of(
      move("e2e4", "Center control"),
      move("e7e5"),
      move("g1f3")
    )
    val bson = summon[BSONHandler[NonEmptyList[AnnotatedMove]]].writeTry(moves).get
    bson match
      case BSONArray(values) =>
        assertEquals(values.size, 3)
      case _ =>
        fail("Expected BSONArray for annotated moves")

  test("NonEmptyList[AnnotatedMove] BSON deserialization"):
    val bson = BSONArray(
      BSONDocument("uci" -> "e2e4", "comment" -> "Opens"),
      BSONDocument("uci" -> "e7e5")
    )
    val moves = summon[BSONHandler[NonEmptyList[AnnotatedMove]]].readTry(bson).get
    assertEquals(moves.toList.map(_.uci.uci), List("e2e4", "e7e5"))
    assertEquals(moves.head.comment, Some("Opens"))
    assertEquals(moves.toList(1).comment, None)

  test("NonEmptyList[OpeningLineId] BSON serialization - space-separated string"):
    val lineIds = NonEmptyList.of(
      OpeningLineId("line1"),
      OpeningLineId("line2"),
      OpeningLineId("line3")
    )
    val bson = summon[BSONHandler[NonEmptyList[OpeningLineId]]].writeTry(lineIds).get
    bson match
      case BSONString(str) =>
        assertEquals(str, "line1 line2 line3")
      case _ =>
        fail("Expected BSONString for line IDs")

  test("NonEmptyList[OpeningLineId] BSON deserialization"):
    val bson = BSONString("line1 line2 line3")
    val lineIds = summon[BSONHandler[NonEmptyList[OpeningLineId]]].readTry(bson).get
    assertEquals(lineIds.toList.map(_.value), List("line1", "line2", "line3"))

  test("NonEmptyList[OpeningLineId] BSON deserialization rejects empty string"):
    val bson = BSONString("")
    val result = summon[BSONHandler[NonEmptyList[OpeningLineId]]].readTry(bson)
    assert(result.isFailure, "Should fail on empty line list")

  test("Color BSON serialization - white"):
    val bson = summon[BSONHandler[Color]].writeTry(Color.White).get
    assertEquals(bson, BSONString("white"))

  test("Color BSON serialization - black"):
    val bson = summon[BSONHandler[Color]].writeTry(Color.Black).get
    assertEquals(bson, BSONString("black"))

  test("Color BSON deserialization"):
    val white = summon[BSONHandler[Color]].readTry(BSONString("white")).get
    assertEquals(white, Color.White)
    val black = summon[BSONHandler[Color]].readTry(BSONString("black")).get
    assertEquals(black, Color.Black)

  test("Color BSON deserialization rejects invalid values"):
    val result = summon[BSONHandler[Color]].readTry(BSONString("red"))
    assert(result.isFailure, "Should fail on invalid color")

  test("List[OpeningGroupId] BSON serialization"):
    val ids = List(OpeningGroupId("g1"), OpeningGroupId("g2"), OpeningGroupId("g3"))
    val bson = summon[BSONHandler[List[OpeningGroupId]]].writeTry(ids).get
    bson match
      case BSONArray(values) =>
        assertEquals(values.size, 3)
        assertEquals(values.toList, List(BSONString("g1"), BSONString("g2"), BSONString("g3")))
      case _ =>
        fail("Expected BSONArray for group IDs")

  test("List[OpeningGroupId] BSON empty list"):
    val ids = List.empty[OpeningGroupId]
    val bson = summon[BSONHandler[List[OpeningGroupId]]].writeTry(ids).get
    bson match
      case BSONArray(values) =>
        assertEquals(values.size, 0)
      case _ =>
        fail("Expected BSONArray for empty list")

  // JSON Serialization Tests

  test("OpeningFamily JSON structure"):
    val json = Json.toJson(testFamily)
    val expected = Json.obj(
      "id" -> "italian-game",
      "name" -> "Italian Game",
      "key" -> "italian-game"
    )
    assertEquals(json, expected)

  test("OpeningLine JSON structure with description"):
    val json = Json.toJson(testLine)
    val obj = json.as[JsObject]

    assertEquals((obj \ "id").as[String], "classical-variation")
    assertEquals((obj \ "name").as[String], "Classical Variation")
    assertEquals((obj \ "eco").as[String], "C53")
    assertEquals((obj \ "description").as[String], "The main line of the Italian Game")

    val moves = (obj \ "moves").as[JsArray]
    assertEquals(moves.value.size, 6)
    // First move has comment
    assertEquals((moves(0) \ "uci").as[String], "e2e4")
    assertEquals((moves(0) \ "comment").as[String], "Controls the center")
    // Second move has no comment
    assertEquals((moves(1) \ "uci").as[String], "e7e5")
    assert((moves(1) \ "comment").toOption.isEmpty)

  test("OpeningLine JSON structure without description"):
    val json = Json.toJson(testLineNoDescription)
    val obj = json.as[JsObject]

    assertEquals((obj \ "id").as[String], "evans-gambit")
    assertEquals((obj \ "name").as[String], "Evans Gambit")
    assertEquals((obj \ "eco").as[String], "C51")
    assert((obj \ "description").toOption.isEmpty, "Description should be omitted when None")

    val moves = (obj \ "moves").as[JsArray]
    assertEquals(moves.value.size, 4)
    // Moves without comments
    assertEquals((moves(0) \ "uci").as[String], "e2e4")
    assert((moves(0) \ "comment").toOption.isEmpty)

  test("OpeningGroup JSON structure"):
    val json = Json.toJson(testGroup)
    val obj = json.as[JsObject]

    assertEquals((obj \ "id").as[String], "giuoco-piano")
    assertEquals((obj \ "name").as[String], "Giuoco Piano")
    assertEquals((obj \ "familyId").as[String], "italian-game")
    assertEquals((obj \ "tier").as[Int], 1)
    assertEquals((obj \ "color").as[String], "white")

    val lines = (obj \ "lines").as[JsArray]
    assertEquals(lines.value.size, 2)
    assertEquals(lines.value.map(_.as[String]).toList, List("classical-variation", "evans-gambit"))

    val prereqs = (obj \ "prerequisites").as[JsArray]
    assertEquals(prereqs.value.size, 2)
    assertEquals(prereqs.value.map(_.as[String]).toList, List("basic-tactics", "center-control"))

  test("OpeningGroup JSON with black color"):
    val json = Json.toJson(testGroupNoPrereqs)
    val obj = json.as[JsObject]

    assertEquals((obj \ "color").as[String], "black")

    val prereqs = (obj \ "prerequisites").as[JsArray]
    assertEquals(prereqs.value.size, 0)

  test("AnnotatedMove JSON serialization"):
    val m1 = move("e2e4", "Opens the game")
    val json1 = Json.toJson(m1)
    assertEquals((json1 \ "uci").as[String], "e2e4")
    assertEquals((json1 \ "comment").as[String], "Opens the game")

    val m2 = move("e7e5")
    val json2 = Json.toJson(m2)
    assertEquals((json2 \ "uci").as[String], "e7e5")
    assert((json2 \ "comment").toOption.isEmpty, "Comment should be omitted when None")

  test("NonEmptyList[AnnotatedMove] JSON serialization - array format"):
    val moves = NonEmptyList.of(
      move("e2e4", "Center"),
      move("e7e5"),
      move("g1f3")
    )
    val json = Json.toJson(moves)
    json match
      case JsArray(values) =>
        assertEquals(values.size, 3)
        assertEquals((values(0) \ "uci").as[String], "e2e4")
        assertEquals((values(0) \ "comment").as[String], "Center")
        assertEquals((values(1) \ "uci").as[String], "e7e5")
      case _ =>
        fail("Expected JsArray for annotated moves")

  test("NonEmptyList[OpeningLineId] JSON serialization - array format"):
    val lineIds = NonEmptyList.of(
      OpeningLineId("line1"),
      OpeningLineId("line2")
    )
    val json = Json.toJson(lineIds)
    json match
      case JsArray(values) =>
        assertEquals(values.size, 2)
        assertEquals(values.map(_.as[String]).toList, List("line1", "line2"))
      case _ =>
        fail("Expected JsArray for line IDs")

  test("Color JSON serialization"):
    val whiteJson = Json.toJson(Color.White)
    assertEquals(whiteJson, JsString("white"))

    val blackJson = Json.toJson(Color.Black)
    assertEquals(blackJson, JsString("black"))

  test("List[OpeningGroupId] JSON serialization"):
    val ids = List(OpeningGroupId("g1"), OpeningGroupId("g2"))
    val json = Json.toJson(ids)
    json match
      case JsArray(values) =>
        assertEquals(values.size, 2)
        assertEquals(values.map(_.as[String]).toList, List("g1", "g2"))
      case _ =>
        fail("Expected JsArray for group IDs")

  // Edge Cases

  test("OpeningLine with single move"):
    val singleMoveLine = OpeningLine(
      id = OpeningLineId("single"),
      name = "Single Move",
      eco = Some("A00"),
      moves = NonEmptyList.of(move("e2e4")),
      description = None
    )

    // BSON round-trip
    val bson = summon[BSONDocumentHandler[OpeningLine]].writeTry(singleMoveLine).get
    val restored = summon[BSONDocumentHandler[OpeningLine]].readTry(bson).get
    assertEquals(restored, singleMoveLine)

    // JSON structure
    val json = Json.toJson(singleMoveLine)
    val moves = (json \ "moves").as[JsArray]
    assertEquals(moves.value.size, 1)
    assertEquals((moves(0) \ "uci").as[String], "e2e4")

  test("OpeningGroup with single line"):
    val singleLineGroup = OpeningGroup(
      id = OpeningGroupId("single"),
      name = "Single Line Group",
      familyId = testFamilyId,
      tier = 0,
      lines = NonEmptyList.of(testLineId1),
      color = Color.White,
      prerequisites = List.empty
    )

    // BSON round-trip
    val bson = summon[BSONDocumentHandler[OpeningGroup]].writeTry(singleLineGroup).get
    val restored = summon[BSONDocumentHandler[OpeningGroup]].readTry(bson).get
    assertEquals(restored, singleLineGroup)

    // JSON structure
    val json = Json.toJson(singleLineGroup)
    val lines = (json \ "lines").as[JsArray]
    assertEquals(lines.value.size, 1)

  test("OpeningFamily with special characters in name"):
    val specialFamily = OpeningFamily(
      id = OpeningFamilyId("ruy-lopez"),
      name = "Ruy López",
      key = "ruy-lopez"
    )

    // BSON round-trip
    val bson = summon[BSONDocumentHandler[OpeningFamily]].writeTry(specialFamily).get
    val restored = summon[BSONDocumentHandler[OpeningFamily]].readTry(bson).get
    assertEquals(restored, specialFamily)

    // JSON structure
    val json = Json.toJson(specialFamily)
    assertEquals((json \ "name").as[String], "Ruy López")

  test("OpeningLine with complex move sequence"):
    val complexLine = OpeningLine(
      id = OpeningLineId("complex"),
      name = "Complex Line",
      eco = Some("B99"),
      moves = NonEmptyList.of(
        move("e2e4"),
        move("c7c5"),
        move("g1f3"),
        move("d7d6"),
        move("d2d4"),
        move("c5d4"),
        move("f3d4"),
        move("g8f6"),
        move("b1c3"),
        move("a7a6")
      ),
      description = Some("A very long description with special characters: é, ñ, ü, and symbols like & < >")
    )

    // BSON round-trip
    val bson = summon[BSONDocumentHandler[OpeningLine]].writeTry(complexLine).get
    val restored = summon[BSONDocumentHandler[OpeningLine]].readTry(bson).get
    assertEquals(restored, complexLine)

    // Verify BSON moves are array of documents
    val movesBson = bson.getAsTry[BSONArray]("moves").get
    assertEquals(movesBson.values.size, 10)

    // JSON structure
    val json = Json.toJson(complexLine)
    val moves = (json \ "moves").as[JsArray]
    assertEquals(moves.value.size, 10)
    assertEquals((json \ "description").as[String], "A very long description with special characters: é, ñ, ü, and symbols like & < >")

  test("OpeningLine without ECO (community content)"):
    val communityLine = OpeningLine(
      id = OpeningLineId("community-trap"),
      name = "Custom Trap Line",
      eco = None,
      moves = NonEmptyList.of(move("e2e4"), move("e7e5")),
      description = Some("A community-contributed trap")
    )

    // BSON round-trip
    val bson = summon[BSONDocumentHandler[OpeningLine]].writeTry(communityLine).get
    val restored = summon[BSONDocumentHandler[OpeningLine]].readTry(bson).get
    assertEquals(restored, communityLine)

    // JSON structure - eco should be omitted
    val json = Json.toJson(communityLine)
    val obj = json.as[JsObject]
    assert((obj \ "eco").toOption.isEmpty, "ECO should be omitted when None")
    assertEquals((obj \ "name").as[String], "Custom Trap Line")

