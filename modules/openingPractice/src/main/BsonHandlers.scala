package lila.openingPractice

import chess.Color
import chess.format.Uci
import reactivemongo.api.bson.*

import scala.util.Success

import lila.db.dsl.*

object BsonHandlers:

  // Opaque type ID handlers
  given BSONHandler[OpeningFamilyId] = stringAnyValHandler[OpeningFamilyId](_.value, OpeningFamilyId.apply)
  given BSONHandler[OpeningGroupId] = stringAnyValHandler[OpeningGroupId](_.value, OpeningGroupId.apply)
  given BSONHandler[OpeningLineId] = stringAnyValHandler[OpeningLineId](_.value, OpeningLineId.apply)

  // NonEmptyList[Uci] handler - stores as space-separated string
  given uciNelHandler: BSONHandler[NonEmptyList[Uci]] = tryHandler[NonEmptyList[Uci]](
    { case BSONString(str) =>
      str.split(' ').toList.flatMap(Uci.apply).toNel.toTry("Empty move list")
    },
    moves => BSONString(moves.toList.map(_.uci).mkString(" "))
  )

  // NonEmptyList[OpeningLineId] handler - stores as space-separated string
  given lineIdNelHandler: BSONHandler[NonEmptyList[OpeningLineId]] = tryHandler[NonEmptyList[OpeningLineId]](
    { case BSONString(str) =>
      str.split(' ').toList.filter(_.nonEmpty).map(OpeningLineId.apply).toNel.toTry("Empty line list")
    },
    lineIds => BSONString(lineIds.toList.map(_.value).mkString(" "))
  )

  // Color handler
  given BSONHandler[Color] = tryHandler[Color](
    { case BSONString("white") => Success(Color.White)
      case BSONString("black") => Success(Color.Black)
      case v => handlerBadValue(s"Invalid color: $v")
    },
    color => BSONString(color.name)
  )

  // List[OpeningGroupId] handler
  given BSONHandler[List[OpeningGroupId]] = tryHandler[List[OpeningGroupId]](
    { case BSONArray(values) =>
      Success(values.toList.flatMap {
        case BSONString(id) => Some(OpeningGroupId(id))
        case _ => None
      })
    },
    ids => BSONArray(ids.map(id => BSONString(id.value)))
  )

  // Document handlers for domain models
  given BSONDocumentHandler[OpeningFamily] = Macros.handler

  given BSONDocumentHandler[OpeningLine] = Macros.handler

  given BSONDocumentHandler[OpeningGroup] = Macros.handler
