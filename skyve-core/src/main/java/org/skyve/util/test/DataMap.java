package org.skyve.util.test;

import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

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