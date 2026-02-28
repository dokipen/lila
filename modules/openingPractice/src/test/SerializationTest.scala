package lila.openingPractice

import chess.Color
import chess.format.Uci
import play.api.libs.json.*
import reactivemongo.api.bson.*

class SerializationTest extends munit.FunSuite:

  import BsonHandlers.{ given }
  import OpeningPracticeJson.{ given }

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
    eco = "C53",
    moves = NonEmptyList.of(
      Uci("e2e4").get,
      Uci("e7e5").get,
      Uci("g1f3").get,
      Uci("b8c6").get,
      Uci("f1c4").get,
      Uci("f8c5").get
    ),
    description = Some("The main line of the Italian Game")
  )

  val testLineNoDescription = OpeningLine(
    id = testLineId2,
    name = "Evans Gambit",
    eco = "C51",
    moves = NonEmptyList.of(
      Uci("e2e4").get,
      Uci("e7e5").get,
      Uci("g1f3").get,
      Uci("b8c6").get
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

  test("NonEmptyList[Uci] BSON serialization - space-separated string"):
    val moves = NonEmptyList.of(
      Uci("e2e4").get,
      Uci("e7e5").get,
      Uci("g1f3").get
    )
    val bson = summon[BSONHandler[NonEmptyList[Uci]]].writeTry(moves).get
    bson match
      case BSONString(str) =>
        assertEquals(str, "e2e4 e7e5 g1f3")
      case _ =>
        fail("Expected BSONString for UCI moves")

  test("NonEmptyList[Uci] BSON deserialization from space-separated string"):
    val bson = BSONString("e2e4 e7e5 g1f3")
    val moves = summon[BSONHandler[NonEmptyList[Uci]]].readTry(bson).get
    assertEquals(moves.toList.map(_.uci), List("e2e4", "e7e5", "g1f3"))

  test("NonEmptyList[Uci] BSON deserialization filters out invalid UCI"):
    val bson = BSONString("e2e4 invalid g1f3")
    val moves = summon[BSONHandler[NonEmptyList[Uci]]].readTry(bson).get
    // Invalid UCI strings are filtered out by flatMap
    assertEquals(moves.toList.map(_.uci), List("e2e4", "g1f3"))

  test("NonEmptyList[Uci] BSON deserialization rejects empty list"):
    val bson = BSONString("")
    val result = summon[BSONHandler[NonEmptyList[Uci]]].readTry(bson)
    assert(result.isFailure, "Should fail on empty move list")

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
    assertEquals(moves.value.map(_.as[String]).toList, List("e2e4", "e7e5", "g1f3", "b8c6", "f1c4", "f8c5"))

  test("OpeningLine JSON structure without description"):
    val json = Json.toJson(testLineNoDescription)
    val obj = json.as[JsObject]

    assertEquals((obj \ "id").as[String], "evans-gambit")
    assertEquals((obj \ "name").as[String], "Evans Gambit")
    assertEquals((obj \ "eco").as[String], "C51")
    assert((obj \ "description").toOption.isEmpty, "Description should be omitted when None")

    val moves = (obj \ "moves").as[JsArray]
    assertEquals(moves.value.size, 4)
    assertEquals(moves.value.map(_.as[String]).toList, List("e2e4", "e7e5", "g1f3", "b8c6"))

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

  test("NonEmptyList[Uci] JSON serialization - array format"):
    val moves = NonEmptyList.of(
      Uci("e2e4").get,
      Uci("e7e5").get,
      Uci("g1f3").get
    )
    val json = Json.toJson(moves)
    json match
      case JsArray(values) =>
        assertEquals(values.size, 3)
        assertEquals(values.map(_.as[String]).toList, List("e2e4", "e7e5", "g1f3"))
      case _ =>
        fail("Expected JsArray for UCI moves")

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
      eco = "A00",
      moves = NonEmptyList.of(Uci("e2e4").get),
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
    assertEquals(moves.value.head.as[String], "e2e4")

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
      eco = "B99",
      moves = NonEmptyList.of(
        Uci("e2e4").get,
        Uci("c7c5").get,
        Uci("g1f3").get,
        Uci("d7d6").get,
        Uci("d2d4").get,
        Uci("c5d4").get,
        Uci("f3d4").get,
        Uci("g8f6").get,
        Uci("b1c3").get,
        Uci("a7a6").get
      ),
      description = Some("A very long description with special characters: é, ñ, ü, and symbols like & < >")
    )

    // BSON round-trip
    val bson = summon[BSONDocumentHandler[OpeningLine]].writeTry(complexLine).get
    val restored = summon[BSONDocumentHandler[OpeningLine]].readTry(bson).get
    assertEquals(restored, complexLine)

    // Verify BSON moves are space-separated
    val movesBson = bson.getAsTry[BSONString]("moves").get
    assertEquals(movesBson.value, "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6 b1c3 a7a6")

    // JSON structure
    val json = Json.toJson(complexLine)
    val moves = (json \ "moves").as[JsArray]
    assertEquals(moves.value.size, 10)
    assertEquals((json \ "description").as[String], "A very long description with special characters: é, ñ, ü, and symbols like & < >")
