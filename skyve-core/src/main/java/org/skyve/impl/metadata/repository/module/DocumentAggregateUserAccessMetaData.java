package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "documentAggregate")
public class DocumentAggregateUserAccessMetaData extends SingularUserAccessMetaData {
	private static final long serialVersionUID = -9055003769635277281L;
}
