package org.skyve.impl.metadata.model.document.field;

import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated holder for text formatting constraints applied to a {@link Text} field.
 *
 * <p>Combines an optional input mask (characters: {@code A} alphanumeric,
 * {@code #} digit, {@code L} letter) with a {@link org.skyve.domain.types.converters.Format.TextCase}
 * transformation applied before storage.  Used by the UI widget and server-side
 * validation to enforce a specific text format.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see Text
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "format")
public class TextFormat {
	private Format<String> format;
	
	// A - alphanumeric
	// # - digit
	// L - letter
	private String mask;
	private TextCase textCase;
	
	public String getMask() {
		return mask;
	}
	@XmlAttribute
	public void setMask(String mask) {
		this.mask = UtilImpl.processStringValue(mask);
	}
	
	public TextCase getCase() {
		return textCase;
	}
	@XmlAttribute
	public void setCase(TextCase textCase) {
		this.textCase = textCase;
	}

	@XmlTransient
	public Format<String> getFormat() {
		if (format == null) {
			format = new Format<>(mask, textCase);
		}
		return format;
	}
}
