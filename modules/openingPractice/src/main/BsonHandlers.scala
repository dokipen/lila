package lila.openingPractice

import chess.Color
import chess.format.Uci
import reactivemongo.api.bson.*

import scala.util.Success

import lila.db.dsl.{ *, given }

object BsonHandlers:

  // Opaque type ID handlers
  given BSONHandler[OpeningFamilyId] = stringAnyValHandler[OpeningFamilyId](_.value, OpeningFamilyId.apply)
  given BSONHandler[OpeningGroupId] = stringAnyValHandler[OpeningGroupId](_.value, OpeningGroupId.apply)
  given BSONHandler[OpeningLineId] = stringAnyValHandler[OpeningLineId](_.value, OpeningLineId.apply)

  // AnnotatedMove handler - stores as document with uci and optional comment
  given BSONDocumentHandler[AnnotatedMove] = new BSONDocumentHandler[AnnotatedMove]:
    def readDocument(doc: BSONDocument) =
      for
        uciStr <- doc.getAsTry[String]("uci")
        uci <- Uci(uciStr).toTry(s"Invalid UCI: $uciStr")
        comment = doc.getAsOpt[String]("comment")
      yield AnnotatedMove(uci, comment)

    def writeTry(move: AnnotatedMove) = Success(
      BSONDocument("uci" -> move.uci.uci) ++ move.comment.fold(BSONDocument())(c => BSONDocument("comment" -> c))
    )

  // NonEmptyList[AnnotatedMove] handler - stores as array of documents
  given annotatedMovesHandler: BSONHandler[NonEmptyList[AnnotatedMove]] = tryHandler[NonEmptyList[AnnotatedMove]](
    { case BSONArray(values) =>
      values.toList
        .traverse(v => summon[BSONDocumentHandler[AnnotatedMove]].readTry(v.asInstanceOf[BSONDocument]).toOption)
        .flatMap(_.toNel)
        .toTry("Empty or invalid move list")
    },
    moves => BSONArray(moves.toList.map(m => summon[BSONDocumentHandler[AnnotatedMove]].writeTry(m).get))
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

  // PracticeMode enum handler
  given BSONHandler[PracticeMode] = tryHandler[PracticeMode](
    {
      case BSONString("learning") => Success(PracticeMode.Learning)
      case BSONString("drilling") => Success(PracticeMode.Drilling)
      case BSONString("timed")    => Success(PracticeMode.Timed)
      case v                      => handlerBadValue(s"Invalid practice mode: $v")
    },
    {
      case PracticeMode.Learning => BSONString("learning")
      case PracticeMode.Drilling => BSONString("drilling")
      case PracticeMode.Timed    => BSONString("timed")
    }
  )

  // LineStatus enum handler
  given BSONHandler[LineStatus] = tryHandler[LineStatus](
    {
      case BSONString("notStarted") => Success(LineStatus.NotStarted)
      case BSONString("learning")   => Success(LineStatus.Learning)
      case BSONString("learned")    => Success(LineStatus.Learned)
      case BSONString("mastered")   => Success(LineStatus.Mastered)
      case v                        => handlerBadValue(s"Invalid line status: $v")
    },
    {
      case LineStatus.NotStarted => BSONString("notStarted")
      case LineStatus.Learning   => BSONString("learning")
      case LineStatus.Learned    => BSONString("learned")
      case LineStatus.Mastered   => BSONString("mastered")
    }
  )

  // Progress statistics handlers
  given BSONDocumentHandler[DrillingStats] = Macros.handler

  given BSONDocumentHandler[TimedStats] = Macros.handler

  given BSONDocumentHandler[LineProgress] = Macros.handler

  // Map handler for line progress
  given lineProgressMapHandler: BSONHandler[Map[OpeningLineId, LineProgress]] =
    typedMapHandler[OpeningLineId, LineProgress]

  given BSONDocumentHandler[UserOpeningProgress] = Macros.handler
