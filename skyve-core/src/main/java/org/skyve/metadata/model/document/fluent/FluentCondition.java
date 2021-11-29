package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.repository.document.ConditionMetaData;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Condition;

public class FluentCondition {
	private ConditionMetaData condition = new ConditionMetaData();
	
	public FluentCondition() {
		// nothing to see
	}

	public FluentCondition(String conditionName, Condition condition) {
		name(conditionName);
		documentation(condition.getDocumentation());
		description(condition.getDescription());
		expression(condition.getExpression());
		usage(condition.getUsage());
	}
	
	public FluentCondition name(String name) {
		condition.setDocumentation(name);
		return this;
	}

	public FluentCondition documentation(String documentation) {
		condition.setDocumentation(documentation);
		return this;
	}

	public FluentCondition description(String description) {
		condition.setDescription(description);
		return this;
	}
	
	public FluentCondition expression(String expression) {
		condition.setExpression(expression);
		return this;
	}

	public FluentCondition usage(UsageType usage) {
		condition.setUsage(usage);
		return this;
	}

	public ConditionMetaData get() {
		return condition;
	}
}
