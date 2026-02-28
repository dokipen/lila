package lila.openingPractice

import scalalib.newtypes.OpaqueString

export lila.core.lilaism.Lilaism.{ *, given }
export lila.common.extensions.*

private val logger = lila.log("openingPractice")

opaque type OpeningFamilyId = String
object OpeningFamilyId extends OpaqueString[OpeningFamilyId]

opaque type OpeningGroupId = String
object OpeningGroupId extends OpaqueString[OpeningGroupId]

opaque type OpeningLineId = String
object OpeningLineId extends OpaqueString[OpeningLineId]
