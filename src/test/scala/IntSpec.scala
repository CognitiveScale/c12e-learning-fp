import com.c12e.learn.test.{Props, Spec}
import com.c12e.learn.typeclass.{Monoid, Semigroup}

object IntSpec extends Spec("Int") {

  implicit val intSemigroup: Semigroup[Int] = new Semigroup[Int] {
    override def append(a1: Int, a2: Int) = a1 + a2
  }

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty = 0
    override def append(a1: Int, a2: Int) = a1 + a2
  }

  checkAll("equal", Props.equal[Int])
  checkAll("semigroup", Props.semigroup[Int])
  checkAll("monoid", Props.monoid[Int])
}