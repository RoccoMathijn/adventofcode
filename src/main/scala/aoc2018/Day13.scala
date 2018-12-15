package aoc2018

import scala.io.Source

object Day13 extends App {
  val input: Seq[String] = Source
    .fromResource("aoc2018/input-day13.txt")
    .getLines()
    .toList

  val track: Track = input
    .map(_.toList)
    .map(_.map(Track.parseTrackPart))

  val carts: Seq[Cart] = input
    .map(_.toList)
    .map(_.zipWithIndex)
    .zipWithIndex
    .flatMap { case (list, y) => list.flatMap{ case (char, x) =>
      Cart.parseCart(char, Position(x, y))
    }}

  type Track = Seq[Seq[TrackPart]]
  sealed trait TrackPart
  case object Horizontal extends TrackPart
  case object Vertical extends TrackPart
  case object Diagonal extends TrackPart
  case object AntiDiagonal extends TrackPart
  case object Intersection extends TrackPart
  case object Empty extends TrackPart

  object Track {
    def parseTrackPart(char: Char): TrackPart = char match {
      case '-' | '>' | '<' => Horizontal
      case '|' | '^' | 'v' => Vertical
      case '\\' => Diagonal
      case '/' => AntiDiagonal
      case '+' => Intersection
      case _ => Empty
    }
  }
  sealed trait Move
  case object Left extends Move
  case object Straight extends Move
  case object Right extends Move

  object Move {
    def next(move: Move): Move = move match {
      case Left => Straight
      case Straight => Right
      case Right => Left
    }
  }

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object West extends Direction
  case object East extends Direction

  object Direction {
    def nextPosition(direction: Direction, position: Position): Position = {
      direction -> position match {
        case (North, Position(_, y)) => position.copy(y = y - 1)
        case (South, Position(_, y)) => position.copy(y = y + 1)
        case (West, Position(x, _)) => position.copy(x = x - 1)
        case (East, Position(x, _)) => position.copy(x = x + 1)
      }
    }

    def clockWise(direction: Direction): Direction = {
      direction match {
        case North => East
        case South => West
        case West => North
        case East => South
      }
    }

    def counterClockWise(direction: Direction): Direction = {
      direction match {
        case North => West
        case South => East
        case West => South
        case East => North
      }
    }
  }

  case class Position(x: Int, y: Int)

  case class Cart(direction: Direction, position: Position, nextMove: Move, crashed: Boolean = false)

  object Cart {
    def drive(cart: Cart, otherCarts: Seq[Cart]): Cart = {
      val nextPosition = Direction.nextPosition(cart.direction, cart.position)
      val nextTrackPart: TrackPart = track(nextPosition.y)(nextPosition.x)
      val nextDirection: Direction =
        nextTrackPart match {
          case Horizontal | Vertical => cart.direction
          case Diagonal => cart.direction match {
            case North | South => Direction.counterClockWise(cart.direction)
            case _ => Direction.clockWise(cart.direction)
          }
          case AntiDiagonal => cart.direction match {
            case North | South => Direction.clockWise(cart.direction)
            case _ => Direction.counterClockWise(cart.direction)
          }
          case Intersection => cart.nextMove match {
              case Left => Direction.counterClockWise(cart.direction)
              case Straight => cart.direction
              case Right => Direction.clockWise(cart.direction)
            }
        }
      val crashed = otherCarts.exists(cart => cart.position == nextPosition)

      Cart(nextDirection, nextPosition, if (nextTrackPart == Intersection) Move.next(cart.nextMove) else cart.nextMove, crashed)
    }

    def drive(carts: Seq[Cart], removeCrashedCarts: Boolean): Seq[Cart] = {
      def oneByOne(toMove: Seq[Cart], moved: Seq[Cart]): Seq[Cart] = {
        toMove match {
          case Nil => moved
          case cart :: cs =>
            val movedCart = drive(cart, moved ++ cs)
            if (movedCart.crashed && removeCrashedCarts) {
              oneByOne(cs.filterNot(_.position == movedCart.position), moved.filterNot(_.position == movedCart.position))
            } else {
              oneByOne(cs, movedCart +: moved)
            }
        }
      }
      oneByOne(carts.sortBy(cart => cart.position.y -> cart.position.x), Seq.empty)
    }

    def parseCart(char: Char, position: Position): Option[Cart] = char match {
      case '>' => Some(Cart(East, position, Left))
      case 'v' => Some(Cart(South, position, Left))
      case '^' => Some(Cart(North, position, Left))
      case '<' => Some(Cart(West, position, Left))
      case _ => None
    }
  }

  def tickUntilCrash(carts: Seq[Cart]): Position = {
    val crashedCarts = carts.filter(_.crashed)
    if (crashedCarts.nonEmpty) {
      crashedCarts.head.position
    } else {
      val newCarts = Cart.drive(carts = carts, removeCrashedCarts = false)
      tickUntilCrash(newCarts)
    }
  }

  def tickUntilOneLeft(carts: Seq[Cart]): Position = {
    val newCarts = Cart.drive(carts = carts, removeCrashedCarts = true)
    newCarts match {
      case Seq(cart) => cart.position
      case _ => tickUntilOneLeft(newCarts)
    }
  }

  // Answer Part 1
  println(tickUntilCrash(carts))

  // Answer Part 2
  println(tickUntilOneLeft(carts))
}
