package lila.openingPractice

import com.softwaremill.macwire.*

import lila.core.config.*

@Module
final class Env(
    db: lila.db.Db
)(using Executor):

  private lazy val coll = db(CollName("opening_practice"))
