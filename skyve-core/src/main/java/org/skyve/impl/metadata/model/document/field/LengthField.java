package org.skyve.impl.metadata.model.document.field;

import org.skyve.metadata.model.Attribute;

import jakarta.xml.bind.annotation.XmlTransient;

@XmlTransient
public interface LengthField extends Attribute {
	public int getLength();
	public void setLength(int length);
}
