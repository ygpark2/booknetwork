package controllers

import play.api.mvc.{AbstractController, ControllerComponents}
import javax.inject._

@Singleton
class Application @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  def index = Action {
    Redirect(routes.BookController.list)
  }
}
