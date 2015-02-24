package org.wahoo.gameObject;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Classes annotated with Catalogued will be stored by type.
  * This means they can be accessed by all[ThisType] and each[ThisType].
  */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Catalogued { }
