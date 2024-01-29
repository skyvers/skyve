package org.skyve.metadata.view.model.chart;

import jakarta.xml.bind.annotation.XmlTransient;

/**
 * Bucket text data by the length of starting characters and case sensitivity.
 * 
 * @author mike
 */
@XmlTransient
public class TextStartsWithBucket implements Bucket {
	private static final long serialVersionUID = 8259446292482480464L;

	protected int length;
	protected boolean caseSensitive;
	
	public TextStartsWithBucket(int length, boolean caseSensitive) {
		this.length = length;
		this.caseSensitive = caseSensitive;
	}
	
	@Override
	public String bizQLExpression(String categoryBinding) {
		StringBuilder result = new StringBuilder(32);
		if (! caseSensitive) {
			result.append("upper(");
		}
		result.append("substring(bean.").append(categoryBinding).append(",1,").append(length).append(')');
		if (! caseSensitive) {
			result.append(')');
		}
		return result.toString();
	}
	
	@Override
	public String label(Object category) {
		return (category == null) ? null : category.toString();
	}
}
