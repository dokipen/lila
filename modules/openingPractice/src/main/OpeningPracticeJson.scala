package lila.openingPractice

import play.api.libs.json.*
import chess.Color
import chess.format.Uci

object OpeningPracticeJson:

  // Opaque type ID writers
  given Writes[OpeningFamilyId] = Writes(id => JsString(id.value))
  given Writes[OpeningGroupId] = Writes(id => JsString(id.value))
  given Writes[OpeningLineId] = Writes(id => JsString(id.value))

  // NonEmptyList[Uci] writer - convert to list of UCI strings
  given uciNelWrites: Writes[NonEmptyList[Uci]] = Writes { nel =>
    JsArray(nel.toList.map(uci => JsString(uci.uci)))
  }

  // NonEmptyList[OpeningLineId] writer
  given lineIdNelWrites: Writes[NonEmptyList[OpeningLineId]] = Writes { nel =>
    JsArray(nel.toList.map(id => JsString(id.value)))
  }

  // Color writer
  given Writes[Color] = Writes(color => JsString(color.name))

  // List[OpeningGroupId] writer
  given Writes[List[OpeningGroupId]] = Writes { ids =>
    JsArray(ids.map(id => JsString(id.value)))
  }

  // Domain model writers
  given OWrites[OpeningFamily] = OWrites { family =>
    Json.obj(
      "id" -> family.id,
      "name" -> family.name,
      "key" -> family.key
    )
  }

  given OWrites[OpeningLine] = OWrites { line =>
    Json
      .obj(
        "id" -> line.id,
        "name" -> line.name,
        "moves" -> line.moves
      )
      .add("eco" -> line.eco)
      .add("description" -> line.description)
  }

  given OWrites[OpeningGroup] = OWrites { group =>
    Json.obj(
      "id" -> group.id,
      "name" -> group.name,
      "familyId" -> group.familyId,
      "tier" -> group.tier,
      "lines" -> group.lines,
      "color" -> group.color,
      "prerequisites" -> group.prerequisites
    )
  }
