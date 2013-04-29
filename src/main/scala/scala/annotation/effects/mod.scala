package scala.annotation.effects

case object any

/**
 * An effect `@mod(l)` denotes the effect of modifying the location l.
 *
 * Modifying multiple locations `@mod(l, m)` is equivalent to having multiple
 * annotations `@mod(l) @mod(m)`.
 *
 * Purity is denoted as `@mod()`.
 *
 * The largest possible effect, or impurity, is denoted as `@mod(any)`.
 */
class mod(locs: Any*) extends Effect

/**
 * The annotation `@loc(l)` denotes the locality a the returned value. For
 * instance, getter methods of `@local` fields have locality `@loc(this)`.
 *
 * Methods returning a freshly allocated value have the effect `@loc()`.
 */
class loc(locs: Any*) extends Effect


/**
 * Fields whose content is part of the locality of an object have
 * to be marked with `@local`.
 */
@scala.annotation.meta.field @scala.annotation.meta.getter
class local extends scala.annotation.StaticAnnotation // this is not an effect
