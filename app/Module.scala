import com.google.inject.AbstractModule
import tasks.DataInitializer

class Module extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[DataInitializer]).asEagerSingleton()
  }
}
