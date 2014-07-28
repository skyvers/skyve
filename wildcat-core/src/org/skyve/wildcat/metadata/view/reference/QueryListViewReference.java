package org.skyve.wildcat.metadata.view.reference;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

/**
 * This reference can specify a metadata query to show the list view.
 * 
 * @author mike
 */
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
public class QueryListViewReference implements Reference {
	/**
	 * For Serialization
	 */
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
