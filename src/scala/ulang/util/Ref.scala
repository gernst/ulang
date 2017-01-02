package ulang.util

class Ref[A](private var value: A) {
  def ! = value
  def :=(a: A) { value = a }

  def map(f: A => A) {
    value = f(value)
  }
}