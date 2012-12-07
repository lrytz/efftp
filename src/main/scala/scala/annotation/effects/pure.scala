package scala.annotation.effects

/**
 * Annotation to mark a method as pure across all effect domains.
 */
class pure extends Effect

/**
 * A relative effect annotation
 */
class rel(args: Any*) extends Effect
