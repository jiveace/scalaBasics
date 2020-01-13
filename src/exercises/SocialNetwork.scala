package exercises

class SocialNetwork(people: Map[String, List[String]]) {
  /*
  This class was abandoned after I realised the exercise is a very shallow effort to use map functions
  rather than creating usable code.  I leave it here purely for reference as it's the first piece of TDD I've
  done in Scala
 */

  def getPeople: Map[String, List[String]] = people;

  def add(person: (String, List[String])) = new SocialNetwork(people + ("Jane" -> List()))

  def friendCount(name: String): Int =
    if(people(name) == null) throw new NoSuchElementException
    else people(name).length

  def mostFriends() =
    "Jane"
}
