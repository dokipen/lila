package lila.openingPractice

import chess.Color
import reactivemongo.api.bson.Macros.Annotations.Key

case class OpeningGroup(
    @Key("_id") id: OpeningGroupId,
    name: String,
    familyId: OpeningFamilyId,
    tier: Int, // difficulty level
    lines: NonEmptyList[OpeningLineId],
    color: Color,
    prerequisites: List[OpeningGroupId]
)
