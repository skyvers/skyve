package org.skyve.impl.metadata.model.document;

import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Condition;

public class ConditionImpl implements Condition {
	private static final long serialVersionUID = 4772305611794702365L;

	private String documentation;
	private String description;
	private String expression;
	private UsageType usage;

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

	@Override
	public UsageType getUsage() {
		return usage;
	}
	public void setUsage(UsageType usage) {
		this.usage = usage;
	}
}
