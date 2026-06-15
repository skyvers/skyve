package org.skyve.util.test;

import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Declares an attribute-to-fixture-file mapping for generated test data.
 *
 * <p>Used with {@link org.skyve.util.test.SkyveFactory} to override the default
 * file-name lookup for a specific attribute when loading deterministic fixture values.
 */
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(value = SkyveFactory.class)
public @interface DataMap {

	/**
	 * The name of the field in the document, e.g. Contact.namePropertyName
	 */
	String attributeName();

	/**
	 * The name of the file to use instead of the attribute name, e.g. surname.txt
	 */
	String fileName();
}