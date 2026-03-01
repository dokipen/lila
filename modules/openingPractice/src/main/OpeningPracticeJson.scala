package lila.openingPractice

import play.api.libs.json.*
import chess.Color

object OpeningPracticeJson:

  // Opaque type ID writers
  given Writes[OpeningFamilyId] = Writes(id => JsString(id.value))
  given Writes[OpeningGroupId] = Writes(id => JsString(id.value))
  given Writes[OpeningLineId] = Writes(id => JsString(id.value))
  given Writes[UserId] = Writes(id => JsString(id.value))

  // AnnotatedMove writer - object with uci and optional comment
  given OWrites[AnnotatedMove] = OWrites { move =>
    Json.obj("uci" -> move.uci.uci).add("comment" -> move.comment)
  }

  // NonEmptyList[AnnotatedMove] writer
  given annotatedMovesWrites: Writes[NonEmptyList[AnnotatedMove]] = Writes { nel =>
    JsArray(nel.toList.map(Json.toJson(_)))
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

  // PracticeMode writer
  given Writes[PracticeMode] = Writes {
    case PracticeMode.Learning => JsString("learning")
    case PracticeMode.Drilling => JsString("drilling")
  }

  // LineStatus writer
  given Writes[LineStatus] = Writes {
    case LineStatus.NotStarted => JsString("notStarted")
    case LineStatus.Learning   => JsString("learning")
    case LineStatus.Learned    => JsString("learned")
    case LineStatus.Mastered   => JsString("mastered")
  }

  // Progress statistics writers
  given OWrites[DrillingStats] = OWrites { stats =>
    Json.obj(
      "currentStreak" -> stats.currentStreak,
      "bestStreak" -> stats.bestStreak,
      "totalDrills" -> stats.totalDrills
    )
  }

  given OWrites[LineProgress] = OWrites { progress =>
    Json.obj(
      "status" -> progress.status,
      "attempts" -> progress.attempts,
      "mistakes" -> progress.mistakes,
      "drilling" -> progress.drilling
    )
    .add("lastMistakeAt" -> progress.lastMistakeAt.map(_.toMillis))
  }

  given OWrites[UserOpeningProgress] = OWrites { progress =>
    Json.obj(
      "id" -> progress.id,
      "currentMode" -> progress.currentMode,
      "createdAt" -> progress.createdAt.toMillis,
      "updatedAt" -> progress.updatedAt.toMillis,
      "lines" -> JsObject(
        progress.lines.map { case (lineId, lineProgress) =>
          lineId.value -> Json.toJsObject(lineProgress)
        }
      )
    )
  }
