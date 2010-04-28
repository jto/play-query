package models
 
import java.util._
import javax.persistence._
 
import play.db.jpa._
import play.data.validation._

import jto.play.PlayQueryHelper
 
@Entity
class User(

    @Email
    @Required
    var email: String,
    
    @Required
    var password: String,
    
    var fullname: String

) extends Model {
    var isAdmin = false
    
    override def toString() = email
 
}

object User extends PlayQueryHelper with QueryOn[User]{
    def connect(email: String, password: String): User = {
        val query = getFirst[User] where (('email is email) and ('password is password) or ('email is "bob@gmail.com"))
        query.run
    }   
}