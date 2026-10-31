package ulang.util

// the only mutable state, a reference to an immutable value
class Ref[A](var get: A)
