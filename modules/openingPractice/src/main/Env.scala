package lila.openingPractice

import com.softwaremill.macwire.*

import lila.core.config.*

@Module
final class Env(
    db: lila.db.Db,
    cacheApi: lila.memo.CacheApi
)(using Executor):

  private lazy val progressColl = db(CollName("opening_practice"))
  private lazy val groupColl = db(CollName("opening_group"))

  lazy val groupRepo = OpeningGroupRepo(groupColl, cacheApi)
