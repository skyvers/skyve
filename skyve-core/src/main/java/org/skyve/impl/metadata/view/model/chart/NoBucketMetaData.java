package org.skyve.impl.metadata.view.model.chart;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.Bucket;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB descriptor indicating that chart categories are not bucketed.
 *
 * <p>Each distinct category value is used directly as its own chart bucket.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(name = "noBucket", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class NoBucketMetaData implements Bucket {
	private static final long serialVersionUID = 4689094392124560560L;

	@Override
	public String bizQLExpression(String categoryBindingOrAlias) {
		return "bean." + categoryBindingOrAlias;
	}

	@Override
	public String label(Object category) {
		return (category == null) ? null : category.toString();
	}
}
