package org.skyve.metadata.view.model.chart;

/**
 * Bucket text data by the length of starting characters and case sensitivity.
 * 
 * @author mike
 */
public class TextStartsWithBucket implements Bucket {
	private int length;
	private boolean caseSensitive;
	
	public TextStartsWithBucket(int length, boolean caseSensitive) {
		this.length = length;
		this.caseSensitive = caseSensitive;
	}
	
	@Override
	public String bizQLExpression(String categoryBindingOrAlias) {
		StringBuilder result = new StringBuilder(32);
		if (! caseSensitive) {
			result.append("upper(");
		}
		result.append("substring(").append(categoryBindingOrAlias).append(",1,").append(length).append(')');
		if (! caseSensitive) {
			result.append(')');
		}
		return result.toString();
	}
	
	@Override
	public String label(Object category) {
		return (category == null) ? "Unknown" : category.toString();
	}
}
