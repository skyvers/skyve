package org.skyve.metadata.view.model.chart;

import javax.xml.bind.annotation.XmlTransient;

/**
 * Bucket by the text length.
 * 
 * @author mike
 */
@XmlTransient
public class TextLengthBucket implements Bucket {
	private static final long serialVersionUID = 6584139964942410769L;

	@Override
	public String bizQLExpression(String categoryBinding) {
		StringBuilder result = new StringBuilder(32);
		result.append("length(bean.").append(categoryBinding).append(')');
		return result.toString();
	}
	
	@Override
	public String label(Object category) {
		return (category == null) ? null : category.toString();
	}
}
