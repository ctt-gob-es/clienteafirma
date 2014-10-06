package javax.xml.bind.annotation;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Retention(RUNTIME) @Target({FIELD, METHOD})
public @interface XmlElementWrapper {

	String name() default "##default";

    String namespace() default "##default";

    boolean nillable() default false;

    boolean required() default false;
}
