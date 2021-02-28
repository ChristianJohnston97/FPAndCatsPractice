package cats

object ReaderMonadUsage {

  /**
   * Reader is a monad that allows us to sequence opera􏰀tions that depend on some input.
   * One common use for Readers is dependency injection.
   * If we have a number of opera􏰀ons that all depend on some external configuration􏰀, we can chain them together using a
   * Reader to produce one large opera􏰀on that accepts the configuration􏰀 as a parameter and runs our program in the order specified.
   */

  /**
   * The classic use of Readers is to build programs that accept a configuration as a parameter.
   */
  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

//  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
//    for {
//      username <- findUsername(userId)
//      passwordOk <- username.map { username =>
//        checkPassword(username, password) }
//    } yield passwordOk


}
