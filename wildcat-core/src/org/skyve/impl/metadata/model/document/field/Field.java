package org.skyve.impl.metadata.model.document.field;

import java.lang.Boolean;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.model.AbstractAttribute;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE, 
			propOrder = {"persistentBool", "requiredBool", "defaultValue", "index"})
public abstract class Field extends AbstractAttribute {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -638312757619721424L;

	@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public static enum IndexType {
		database, textual, both, none;
	}

	private boolean required;
	private boolean persistent = true;
	
	/**
	 * Whether this field has a textual or database index on it.
	 */
	private IndexType index;

	/**
	 * The default value for this field (coercable from a string to the required type)
	 */
	private String defaultValue;

	public IndexType getIndex() {
		return index;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setIndex(IndexType index) {
		this.index = index;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setDefaultValue(String defaultValue) {
		this.defaultValue = UtilImpl.processStringValue(defaultValue);
	}
	
	@Override
	public boolean isRequired() {
		return required;
	}
	
	@XmlTransient
	public void setRequired(boolean required) {
		this.required = required;
	}
	
	@XmlAttribute(name = "required")
	public void setRequiredBool(Boolean required) {
		this.required = required.booleanValue();
	}
	
	@Override
	public boolean isPersistent() {
		return persistent;
	}

	@XmlTransient
	public void setPersistent(boolean persistent) {
		this.persistent = persistent;
	}

	@XmlAttribute(name = "persistent")
	public void setPersistentBool(Boolean persistent) {
		this.persistent = persistent.booleanValue();
	}
}
