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
class mod(references: Any*) extends Effect

/**
 * The annotation `@loc(l)` denotes the locality a the returned value. For
 * instance, getter methods of `@local` fields have locality `@loc(this)`.
 *
 * Methods returning a freshly allocated value have the effect `@loc()`.
 */
class loc(references: Any*) extends Effect


/**
 * An effect annotation denoting assignment effects to local variables. This
 * annotation can only be used in nested methods in case they modify local
 * variables of the outer scope. Modifications of fields are represented using
 * the `@mod` effect annotation.
 *
 *   def size(l: List) = {
 *     var c = 0
 *     def inc(): Unit @assign(c, any) = { c = c + 1 }
 *     l.foreach(inc)
 *     c
 *   }
 *
 * The annotation is important in assignments where the locality of the new
 * object is different from the initial locality of the variables. Subsequent
 * modifications of the object stored in the variable will modify the new
 * locality.
 *
 *   def t {
 *     var a = new A()  // a fresh object
 *     a = someGlobalA  // has effect @assign(a, any)
 *     a.modify()       // has effect @mod(a)
 *   }
 *
 * Once `a` gets out of scope in the above example, the effect `@mod(a)` is
 * transformed to `@mod(any)`. because of the assign effect.
 *
 * Note: the annotation `@assign(x)` is equivalent to `@assign(x, [fresh])`, and
 * not `@assign(x, any)`.
 *
 * Note: to annotate the absence of assignment effects one needs to annotate the
 * method with either a @mod or a @loc annotation:
 *   def f(): Unit @loc(any) = ..  // no assignments allowed
 */
class assign(localVariable: Any, references: Any*) extends Effect

/**
 * Fields whose content is part of the locality of an object have
 * to be marked with `@local`.
 */
@scala.annotation.meta.field @scala.annotation.meta.getter @scala.annotation.meta.param
class local extends scala.annotation.StaticAnnotation // this is not an effect
