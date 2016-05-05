package org.skyve.impl.metadata.view;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class InjectBinding extends AbstractBound {
	private static final long serialVersionUID = 7594098694022350873L;

	private Boolean readOnly;
	
	public Boolean getReadOnly() {
		return readOnly;
	}

	@XmlAttribute(required = false)
	public void setReadOnly(Boolean readOnly) {
		this.readOnly = readOnly;
	}
}
