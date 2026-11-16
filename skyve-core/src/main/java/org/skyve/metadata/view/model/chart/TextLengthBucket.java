package org.skyve.metadata.view.model.chart;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlTransient;

/**
 * Bucket by the text length.
 * 
 * @author mike
 */
@XmlTransient
public class TextLengthBucket implements Bucket {
	private static final long serialVersionUID = 6584139964942410769L;

	@Override
	public @Nonnull String bizQLExpression(@Nonnull String categoryBinding) {
		StringBuilder result = new StringBuilder(32);
		result.append("length(bean.").append(categoryBinding).append(')');
		return result.toString();
	}
	
	@Override
	public @Nullable String label(@Nullable Object category) {
		return (category == null) ? null : category.toString();
	}
}
