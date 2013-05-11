package scala.annotation.effects

/**
 * The `@pure` annotation marks a method as pure across all effect domains.
 * Arguments to the `@pure` annotation denote relative effects.
 */
class pure(args: Any*) extends Effect
