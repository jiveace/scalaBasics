package exercises

import org.scalatest.flatspec.AnyFlatSpec

class SocialNetworkTest extends AnyFlatSpec {
 /*
   This test was abandoned after I realised the exercise is a very shallow effort to use map functions
   rather than creating usable code.  I leave it here purely for reference as it's the first piece of TDD I've
   done in Scala
  */
  val people: Map[String, List[String]] = Map()
  val socialNetwork = new SocialNetwork(people)

  "SocialNetwork" must "be able to read map of people" in {
    assert(socialNetwork.getPeople === people)
  }

  "SocialNetwork" must "be able to add a person to map of people" in {
    val newPerson = "Jane" -> List()
    socialNetwork.add(newPerson)
//    assert(socialNetwork.getPeople === people + newPerson)
  }

  "SocialNetwork" must "throw exception if friend count of non existent person is requested " in {
    assertThrows[NoSuchElementException](socialNetwork.friendCount("Bane"))
  }

  "SocialNetwork" must "throw return 0 friendCount for a name with no friends " in {
    val socialNetwork = new SocialNetwork(Map("Bane" -> List()))
    assert(socialNetwork.friendCount("Bane") === 0)
  }

  "SocialNetwork" must "throw return accurate friendCount for a name with some friends " in {
    val socialNetwork = new SocialNetwork(Map(("Jane" -> List("Bane", "Batman"))))
    assert(socialNetwork.friendCount("Jane") === 2)
  }

  "SocialNetwork" must "correctly identify person with most friends" in {
    val socialNetwork = new SocialNetwork(Map("Jane" -> List("Bane", "Batman"), "Richie" -> List(), "Eddie" -> List("Ethel Cardew")))
    assert(socialNetwork.mostFriends() === "Jane")
  }

  "SocialNetwork" must "correctly identify person with most friends triangulation" in {
    val socialNetwork = new SocialNetwork(Map("Richard" -> List("Bane", "Batman"), "Dave" -> List()))
    assert(socialNetwork.mostFriends() === "Richard")
  }
}
