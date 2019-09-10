package org.skyve.impl.metadata.view.model.chart;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.Bucket;

@XmlRootElement(name = "noBucket", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class NoBucketMetaData implements Bucket {
	private static final long serialVersionUID = 4689094392124560560L;

	@Override
	public String bizQLExpression(String categoryBindingOrAlias) {
		return categoryBindingOrAlias;
	}

	@Override
	public String label(Object category) {
		return (category == null) ? "Unknown" : category.toString();
	}
}
