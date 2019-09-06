package org.skyve.metadata.view.model.chart;

/**
 * Bucket by the text length.
 * 
 * @author mike
 */
public class TextLengthBucket implements Bucket {
	@Override
	public String bizQLExpression(String categoryBindingOrAlias) {
		StringBuilder result = new StringBuilder(32);
		result.append("length(").append(categoryBindingOrAlias).append(')');
		return result.toString();
	}
	
	@Override
	public String label(Object category) {
		return (category == null) ? "Unknown" : category.toString();
	}
}
