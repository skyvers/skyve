package org.skyve.impl.metadata.view.reference;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

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
