package org.skyve.impl.bizport;

public class DataFileExportField {

	private String fieldTitle;
	private String bindingExpression;

	public String getFieldTitle() {
		return fieldTitle;
	}

	public void setFieldTitle(String fieldTitle) {
		this.fieldTitle = fieldTitle;
	}

	public String getBindingExpression() {
		return bindingExpression;
	}

	public void setBindingExpression(String bindingExpression) {
		if(bindingExpression!=null && bindingExpression.startsWith("{") && bindingExpression.endsWith("}")) {
			this.bindingExpression = bindingExpression.substring(1, bindingExpression.length()-1);
		} else {
			this.bindingExpression = bindingExpression;
		}
	}

	public DataFileExportField(String fieldTitle, String binding) {
		this.fieldTitle = fieldTitle;
		this.bindingExpression = binding;
	}
}
