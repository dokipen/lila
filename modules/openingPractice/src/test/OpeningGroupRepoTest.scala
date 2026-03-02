package lila.openingPractice

import reactivemongo.api.bson.*

class OpeningGroupRepoTest extends munit.FunSuite:

  import BsonHandlers.{ given }
  import lila.db.dsl.$doc
  import lila.db.dsl.$empty

  // Test fixtures
  val italianFamilyId = OpeningFamilyId("italian-game")
  val spanishFamilyId = OpeningFamilyId("spanish-opening")
  val giuocoPianoId = OpeningGroupId("giuoco-piano")
  val evansGambitId = OpeningGroupId("evans-gambit")

  // Tests for findByFamily query selector

  test("family query selector uses correct field name"):
    val selector = $doc("familyId" -> italianFamilyId)
    val familyValue = selector.getAsOpt[String]("familyId")
    assertEquals(familyValue, Some("italian-game"))

  test("family query selector serializes different family IDs"):
    val selector1 = $doc("familyId" -> italianFamilyId)
    val selector2 = $doc("familyId" -> spanishFamilyId)

    assertEquals(selector1.getAsOpt[String]("familyId"), Some("italian-game"))
    assertEquals(selector2.getAsOpt[String]("familyId"), Some("spanish-opening"))

  test("family query selector produces BSONDocument"):
    val selector = $doc("familyId" -> italianFamilyId)
    assert(selector.isInstanceOf[BSONDocument])
    assert(selector.contains("familyId"))

  // Tests for findById (uses coll.byId which queries on _id field)

  test("group id resolves to string value"):
    assertEquals(giuocoPianoId.value, "giuoco-piano")
    assertEquals(evansGambitId.value, "evans-gambit")

  test("group id can be used as document key"):
    // byId uses the string value as _id
    val idValue: String = giuocoPianoId.value
    assertEquals(idValue, "giuoco-piano")

  test("different group ids have different string values"):
    assert(giuocoPianoId.value != evansGambitId.value)

  // Tests for listAll query (uses $empty)

  test("empty selector for listAll"):
    val selector = $empty
    assert(selector.isEmpty)

  test("empty selector is a BSONDocument"):
    val selector = $empty
    assert(selector.isInstanceOf[BSONDocument])
    assertEquals(selector.elements.size, 0)

  // Integration tests: verify selectors work with BSON handlers

  test("familyId selector can match serialized OpeningGroup"):
    val testGroup = OpeningGroup(
      id = giuocoPianoId,
      name = "Giuoco Piano",
      familyId = italianFamilyId,
      tier = 1,
      lines = NonEmptyList.of(OpeningLineId("classical")),
      color = chess.Color.White,
      prerequisites = List.empty
    )

    // Serialize the group
    val bson = summon[BSONDocumentHandler[OpeningGroup]].writeTry(testGroup).get

    // Verify the familyId field matches our selector
    val familyIdInBson = bson.getAsOpt[String]("familyId")
    assertEquals(familyIdInBson, Some("italian-game"))

    // Verify our selector would match this document
    val selector = $doc("familyId" -> italianFamilyId)
    val selectorFamilyId = selector.getAsOpt[String]("familyId")
    assertEquals(selectorFamilyId, familyIdInBson)

  test("familyId selector does not match different family"):
    val testGroup = OpeningGroup(
      id = giuocoPianoId,
      name = "Giuoco Piano",
      familyId = italianFamilyId,
      tier = 1,
      lines = NonEmptyList.of(OpeningLineId("classical")),
      color = chess.Color.White,
      prerequisites = List.empty
    )

    val bson = summon[BSONDocumentHandler[OpeningGroup]].writeTry(testGroup).get
    val familyIdInBson = bson.getAsOpt[String]("familyId")

    // Different family selector should not match
    val wrongSelector = $doc("familyId" -> spanishFamilyId)
    val wrongSelectorFamilyId = wrongSelector.getAsOpt[String]("familyId")

    assert(familyIdInBson != wrongSelectorFamilyId)
    assertEquals(familyIdInBson, Some("italian-game"))
    assertEquals(wrongSelectorFamilyId, Some("spanish-opening"))

  test("id field in serialized group maps to _id"):
    val testGroup = OpeningGroup(
      id = giuocoPianoId,
      name = "Giuoco Piano",
      familyId = italianFamilyId,
      tier = 1,
      lines = NonEmptyList.of(OpeningLineId("classical")),
      color = chess.Color.White,
      prerequisites = List.empty
    )

    val bson = summon[BSONDocumentHandler[OpeningGroup]].writeTry(testGroup).get

    // @Key("_id") annotation maps id to _id, required for coll.byId queries
    val idInBson = bson.getAsOpt[String]("_id")
    assertEquals(idInBson, Some(giuocoPianoId.value))
    assertEquals(idInBson, Some("giuoco-piano"))
    // Verify "id" field does NOT exist (it's mapped to "_id")
    assertEquals(bson.getAsOpt[String]("id"), None)
