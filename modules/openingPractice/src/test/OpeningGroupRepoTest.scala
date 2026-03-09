package lila.openingPractice

import reactivemongo.api.bson.*

class OpeningGroupRepoTest extends munit.FunSuite:

  import BsonHandlers.{ given }
  import lila.db.dsl.$doc
  import lila.db.dsl.$empty

  // Reuse fixture naming convention from SerializationTest
  val testFamilyId = OpeningFamilyId("italian-game")
  val testGroupId = OpeningGroupId("giuoco-piano")
  val testLineId1 = OpeningLineId("classical-variation")

  val testGroup = OpeningGroup(
    id = testGroupId,
    name = "Giuoco Piano",
    familyId = testFamilyId,
    tier = 1,
    lines = NonEmptyList.of(testLineId1),
    color = chess.Color.White,
    prerequisites = List.empty
  )

  // findByFamily selector

  test("family selector uses correct field name and value"):
    val selector = $doc("familyId" -> testFamilyId)
    assertEquals(selector.getAsOpt[String]("familyId"), Some("italian-game"))

  test("family selector matches serialized group"):
    val bson = summon[BSONDocumentHandler[OpeningGroup]].writeTry(testGroup).get
    val selectorValue = $doc("familyId" -> testFamilyId).getAsOpt[String]("familyId")
    val documentValue = bson.getAsOpt[String]("familyId")
    assertEquals(selectorValue, documentValue)

  test("family selector does not match different family"):
    val bson = summon[BSONDocumentHandler[OpeningGroup]].writeTry(testGroup).get
    val wrongValue = $doc("familyId" -> OpeningFamilyId("spanish")).getAsOpt[String]("familyId")
    assert(bson.getAsOpt[String]("familyId") != wrongValue)

  // findById / _id mapping

  test("@Key maps id to _id in BSON"):
    val bson = summon[BSONDocumentHandler[OpeningGroup]].writeTry(testGroup).get
    assertEquals(bson.getAsOpt[String]("_id"), Some("giuoco-piano"))
    assertEquals(bson.getAsOpt[String]("id"), None)

  test("group id resolves to string value for byId lookup"):
    assertEquals(testGroupId.value, "giuoco-piano")

  // listAll selector

  test("empty selector matches all documents"):
    assert($empty.isEmpty)
