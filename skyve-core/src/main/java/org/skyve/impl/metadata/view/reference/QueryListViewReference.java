package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * This reference can specify a metadata query to show the list view.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class QueryListViewReference implements Reference {
	private static final long serialVersionUID = -1982657919843061491L;
	
	private String queryName;
	
	public String getQueryName() {
		return queryName;
	}

	@XmlAttribute(name="query", required = true)
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}
}
