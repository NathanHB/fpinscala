package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + l.depth max r.depth

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int = fold(this, a => 1, 1 + _ + _)

  def depthViaFold: Int = fold(this, a => 0, 1 + (_ max _))

  def mapViaFold[B](f: A => B): Tree[B] = fold(this, a => Leaf(f(a)), Branch(_, _))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = ???

  extension (t: Tree[Int]) def maximum: Int = ???

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
