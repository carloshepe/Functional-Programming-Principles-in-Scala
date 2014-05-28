package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 * - run the "test" command in the SBT console
 * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   * - test
   * - ignore
   * - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s7 = singletonSet(7)
    val s1000 = singletonSet(1000)
    val _s1 = singletonSet(-1)
    val _s2 = singletonSet(-2)
    val _s3 = singletonSet(-3)
    val _s4 = singletonSet(-4)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }

    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
    }
  }

  test("intersect contains all elemnets that are in both sets") {
    new TestSets {
      val union1 = union(union(s1, s2), s3)
      val union2 = union(s1, s3)
      assert(contains(intersect(union1, union2), 1), "Contains 1")
      assert(contains(intersect(union1, union2), 3), "Contains 3")
      assert(!contains(intersect(union1, union2), 2), "Not Contains 3")
    }
  }

  test("diff contains all elemnets that are in one set but aren't in the other one") {
    new TestSets {
      val union1 = union(union(s1, s2), s3)
      val union2 = union(s1, s3)
      assert(contains(diff(union1, union2), 2), "Contains 1")
      assert(!contains(diff(union1, union2), 1), "Not Contains 1")
      assert(!contains(diff(union1, union2), 3), "Not Contains 3")

      val special = union(union(union(union(union(s1, s3), s4), s5), s7), s1000)
      val special2 = union(union(union(s1, s2), s3), s4)

      assert(contains(diff(special, special2), 5), "Contains 5")
      assert(contains(diff(special, special2), 1000), "Contains 1000")
      assert(!contains(diff(special, special2), 1), "not Contains 1")
      assert(!contains(diff(special, special2), 2), "not Contains 2")
    }
  }

  test("filter all elemnets greater than 1") {
    new TestSets {
      val union1 = union(union(s1, s2), s3)
      assert(contains(filter(union1, x => x > 1), 2), "Contains 1")
      assert(contains(filter(union1, x => x > 1), 3), "Contains 1")
      assert(!contains(filter(union1, x => x > 1), 1), "Not Contains 1")
    }
  }

  test("filter all even elemnets ") {
    new TestSets {
      val union1 = union(union(union(s1, s2), s3), s4)
      assert(contains(filter(union1, x => x % 2 == 0), 2), "Contains 2")
      assert(!contains(filter(union1, x => x % 2 == 0), 3), "Not Contains 3")
      assert(!contains(filter(union1, x => x % 2 == 0), 1), "Not Contains 1")
      assert(contains(filter(union1, x => x % 2 == 0), 4), "Contains 4")
    }
  }

  test(" tests whether a given predicate is true for all elements of the set") {
    new TestSets {
      val union1 = union(union(union(s1, s2), s3), s4)
      val union2 = union(union(union(s1, s2), s3), _s1)
      assert(forall(union1, x => x > 0), "numbers greater than zero")
      assert(!forall(union2, x => x > 0), "numbers greater than zero")

    }

    new TestSets {
      val union1 = union(union(union(s1, s2), s3), s4)
      val union2 = union(union(union(s4, s2), s2), s4)
      assert(!forall(union1, x => x % 2 == 0), "not all even numbers")
      assert(forall(union2, x => x % 2 == 0), "even numbers")

    }
  }

  test("tests whether a set contains at least one element for which the given predicate is true") {
    new TestSets {
      val union1 = union(union(union(_s1, _s2), _s3), s4)
      val union2 = union(union(union(_s1, _s2), _s3), _s4)
      assert(exists(union1, x => x > 0), "one number greater than zero")
      assert(!exists(union2, x => x > 0), "no numbers greater than zero")

    }
  }

  test("transforms a given set into another one by applying to each of its elements the given function") {
    new TestSets {
      val union1 = union(s1, s2)
      val union2 = union(union(union(_s1, _s2), _s3), _s4)
      assert(contains(map(union1, x => x * 8), 8), "1 multiplied by 8")
      assert(contains(map(union1, x => x * 8), 16), "2 multiplied by 8")
      assert(!contains(map(union1, x => x * 8), 1), "there isn't the original number")

      assert(contains(map(union2, x => x * -1), 1), "1 multiplied by -1")
      assert(contains(map(union2, x => x * -1), 2), "2 multiplied by -1")
      assert(!contains(map(union2, x => x * -1), -1), "there isn't the original number")
      assert(contains(map(union1, x => x - 1), 0), "x minus 1")

      val special = union(union(union(union(union(s1, s3), s4), s5), s7), s1000)
      assert(contains(map(special, x => x - 1), 999), "x minus 1")
    }
  }


}
