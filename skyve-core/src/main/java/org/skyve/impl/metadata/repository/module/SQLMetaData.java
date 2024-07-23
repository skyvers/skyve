package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "sql")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, name = "sql", propOrder = {"query"})
public class SQLMetaData extends QueryMetaData {
	private static final long serialVersionUID = 2092696254537507474L;

	private String query;

	public String getQuery() {
		return query;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, required = true)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setQuery(String query) {
		this.query = UtilImpl.processStringValue(query);
	}
}
