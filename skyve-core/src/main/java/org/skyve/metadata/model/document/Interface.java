package org.skyve.metadata.model.document;

import org.skyve.metadata.SerializableMetaData;

/**
 * Records a Java interface that the generated domain class for a document must implement.
 *
 * <p>Declared in the document XML under {@code <implements>}, each entry names a
 * fully-qualified interface. The domain generator adds the interface to the
 * {@code implements} clause of the generated class, enabling compile-time type
 * constraints and polymorphic queries.
 */
public interface Interface extends SerializableMetaData {
	/**
	 * Returns the fully-qualified name of the Java interface.
	 *
	 * @return the interface name; never {@code null}
	 */
	String getInterfaceName();
}
