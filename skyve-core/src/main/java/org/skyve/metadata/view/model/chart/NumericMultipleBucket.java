package org.skyve.metadata.view.model.chart;

import jakarta.xml.bind.annotation.XmlTransient;

/**
 * Bucket data by multiples.
 * 
 * @author mike
 */
@XmlTransient
public class NumericMultipleBucket implements Bucket {
	private static final long serialVersionUID = -2011511311395433491L;

	protected int multiple;

	public NumericMultipleBucket(int multiple) {
		this.multiple = multiple;
	}

	@Override
	public String bizQLExpression(String categoryBinding) {
		StringBuilder result = new StringBuilder(32);
		result.append("floor(bean.").append(categoryBinding).append(" / ").append(multiple).append(".0 + 0.001)");
		return result.toString();
	}
	
	@Override
	public String label(Object category) {
		if (category instanceof Number) {
			int value = ((Number) category).intValue();
			return String.format("%d-%s", Integer.valueOf(value * multiple), Integer.valueOf((value + 1) * multiple));
		}
		return null;
	}
}
