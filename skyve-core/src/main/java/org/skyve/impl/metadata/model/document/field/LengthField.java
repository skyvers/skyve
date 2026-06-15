package org.skyve.impl.metadata.model.document.field;

import org.skyve.metadata.model.Attribute;

import jakarta.xml.bind.annotation.XmlTransient;

/**
 * Marker interface for field types that declare a maximum character length.
 *
 * <p>Implemented by text-based field types ({@link Text}, {@link Memo},
 * {@link Markup}, {@link Colour}) to expose the {@code length} property used
 * when generating the DDL column definition and when validating user input.
 *
 * @see Text
 */
@XmlTransient
public interface LengthField extends Attribute {
	public int getLength();
	public void setLength(int length);
}
