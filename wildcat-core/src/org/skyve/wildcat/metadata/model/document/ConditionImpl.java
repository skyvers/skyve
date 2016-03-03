package org.skyve.wildcat.metadata.model.document;

import org.skyve.metadata.model.document.Condition;

public class ConditionImpl implements Condition {
	private String documentation;
	private String description;
	private String expression;

	@Override
	public String getDocumentation() {
		return documentation;
	}
	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}

	@Override
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}

	@Override
	public String getExpression() {
		return expression;
	}
	public void setExpression(String expression) {
		this.expression = expression;
	}
}
