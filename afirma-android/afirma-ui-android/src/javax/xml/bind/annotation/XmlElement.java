package javax.xml.bind.annotation;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Retention(RUNTIME) @Target({FIELD, METHOD})
public @interface XmlElement {

	String name() default "##default";

    boolean nillable() default false;

    boolean required() default false;

    String namespace() default "##default";

    String defaultValue() default "\u0000";

    Class type() default DEFAULT.class;

    static final class DEFAULT {}
}
