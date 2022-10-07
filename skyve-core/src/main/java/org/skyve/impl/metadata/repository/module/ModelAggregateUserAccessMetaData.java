package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "modelAggregate")
public class ModelAggregateUserAccessMetaData extends DocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = 950718682785023214L;

	private String modelName;

	public String getModelName() {
		return modelName;
	}

	@XmlAttribute(name = "model", required = true)
	public void setModelName(String modelName) {
		this.modelName = modelName;
	}
}
