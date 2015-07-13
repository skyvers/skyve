package org.skyve.wildcat.metadata.repository.module;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "sql")
@XmlType(namespace = XMLUtil.MODULE_NAMESPACE, name = "sql", propOrder = {"query"})
public class SQLMetaData extends QueryMetaData {
	private String query;

	public String getQuery() {
		return query;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, required = true)
	public void setQuery(String query) {
		this.query = UtilImpl.processStringValue(query);
	}
}
