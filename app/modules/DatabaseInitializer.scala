package modules

import javax.inject.{Inject, Singleton}
import models._
import slick.jdbc.H2Profile.api._
import play.api.ApplicationLifecycle
import scala.concurrent.Future
import slick.jdbc.JdbcBackend.Database

@Singleton
class DatabaseInitializer @Inject() (
  db: Database,
  lifecycle: ApplicationLifecycle
) {

  createSchema()

  lifecycle.addStopHook { () =>
    db.close()
    Future.successful(())
  }

  private def createSchema() = {
    val ddl = Accounts.table.schema ++ Books.table.schema ++ Loans.table.schema
    db.run(ddl.createIfNotExists)
  }
}