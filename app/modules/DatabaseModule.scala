package modules

import com.google.inject.{AbstractModule, Provides, Singleton}
import play.api.db.{Database => PlayDatabase}
import slick.jdbc.H2Profile.api._
import models._
import javax.inject.Inject

class DatabaseModule extends AbstractModule {

  override def configure() = {
    bind(classOf[DatabaseInitializer]).asEagerSingleton()
  }

  @Provides @Singleton
  def slickDatabase(db: PlayDatabase): slick.jdbc.JdbcBackend#DatabaseDef = {
    Database.forDataSource(db.dataSource)
  }
}