package org.skyve.metadata.view.model.chart;

/**
 * Bucket data by multiples.
 * 
 * @author mike
 */
public class NumericMultipleBucket implements Bucket {
	private int multiple;
	
	public NumericMultipleBucket(int multiple) {
		this.multiple = multiple;
	}
	
	@Override
	public String bizQLExpression(String categoryBindingOrAlias) {
		StringBuilder result = new StringBuilder(32);
		result.append("floor(").append(categoryBindingOrAlias).append(" / ").append(multiple).append(".0 + 0.001)");
		return result.toString();
	}
	
	@Override
	public String label(Object category) {
		if (category instanceof Number) {
			int value = ((Number) category).intValue();
			return String.format("%d-%s", Integer.valueOf(value * multiple), Integer.valueOf((value + 1) * multiple));
		}
		return "Unknown";
	}
}
