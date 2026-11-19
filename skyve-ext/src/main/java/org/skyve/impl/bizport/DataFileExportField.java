package org.skyve.impl.bizport;

import jakarta.annotation.Nullable;

/**
 * Descriptor for a single export column, associating a display title with a
 * Skyve binding expression used to extract the column value from the domain object.
 */
public class DataFileExportField {

	private String fieldTitle;
	private String bindingExpression;

	public @Nullable String getFieldTitle() {
		return fieldTitle;
	}

	public void setFieldTitle(@Nullable String fieldTitle) {
		this.fieldTitle = fieldTitle;
	}

	public @Nullable String getBindingExpression() {
		return bindingExpression;
	}

	public void setBindingExpression(@Nullable String bindingExpression) {
		if(bindingExpression!=null && bindingExpression.startsWith("{") && bindingExpression.endsWith("}")) {
			this.bindingExpression = bindingExpression.substring(1, bindingExpression.length()-1);
		} else {
			this.bindingExpression = bindingExpression;
		}
	}

	public DataFileExportField(@Nullable String fieldTitle, @Nullable String binding) {
		this.fieldTitle = fieldTitle;
		this.bindingExpression = binding;
	}
}
