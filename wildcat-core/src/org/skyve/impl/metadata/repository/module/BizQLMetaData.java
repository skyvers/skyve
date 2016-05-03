package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.MODULE_NAMESPACE, name = "bizQL")
@XmlType(namespace = XMLUtil.MODULE_NAMESPACE, name = "bizQL", propOrder = {"query"})
public class BizQLMetaData extends QueryMetaData {
	private String query;

	public String getQuery() {
		return query;
	}

	@XmlElement(namespace = XMLUtil.MODULE_NAMESPACE, required = true)
	public void setQuery(String query) {
		this.query = UtilImpl.processStringValue(query);
	}
}
