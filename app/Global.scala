import play.api.GlobalSettings

import models._
import play.api.db.DB
import play.api.Application
import play.api.Play.current

import scala.slick.driver.H2Driver.simple._
// import scala.slick.driver.MySQLDriver.simple._
// import scala.slick.driver.H2Driver.simple.Database

object Global extends GlobalSettings {

  override def onStart(app: Application) {

    lazy val database = Database.forDataSource(DB.getDataSource())

    database withSession { implicit session =>
      // Create the tables, including primary and foreign keys
      // val Accounts = TableQuery[Account]
      val ddl = (Accounts.query.ddl ++ Books.query.ddl ++ Loans.query.ddl)

      //ddl.drop
      ddl.create

      /* ====================
      // Insert some suppliers
      Suppliers.insertAll(
        Supplier(Some(101), "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199"),
        Supplier(Some(49), "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460"),
        Supplier(Some(150), "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")
      )

      // Insert some coffees (using JDBC's batch insert feature, if supported by the DB)
      Coffees.insertAll(
        Coffee(Some("Colombian"), 101, 799, 0, 0),
        Coffee(Some("French_Roast"), 49, 899, 0, 0),
        Coffee(Some("Espresso"), 150, 999, 0, 0),
        Coffee(Some("Colombian_Decaf"), 101, 899, 0, 0),
        Coffee(Some("French_Roast_Decaf"), 49, 999, 0, 0)
      )

      Account.insertAll(
        Account(1, "alice@example.com", "secret", "Alice", Administrator),
        Account(2, "bob@example.com", "secret", "Bob", NormalUser),
        Account(3, "chris@example.com", "secret", "Chris", NormalUser)
      )
      ================ */
    }
  }
}