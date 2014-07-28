package org.skyve.wildcat.metadata.model.document.field;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, name = "format")
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
