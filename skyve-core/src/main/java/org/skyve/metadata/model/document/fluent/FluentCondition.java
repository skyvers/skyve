package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.repository.document.ConditionMetaData;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Condition;

/**
 * Provides a fluent builder for FluentCondition metadata.
 */
public class FluentCondition {
	private ConditionMetaData condition = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentCondition() {
		condition = new ConditionMetaData();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentCondition(ConditionMetaData condition) {
		this.condition = condition;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentCondition from(String conditionName, @SuppressWarnings("hiding") Condition condition) {
		name(conditionName);
		documentation(condition.getDocumentation());
		description(condition.getDescription());
		expression(condition.getExpression());
		usage(condition.getUsage());
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCondition name(String name) {
		condition.setName(name);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCondition documentation(String documentation) {
		condition.setDocumentation(documentation);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCondition description(String description) {
		condition.setDescription(description);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCondition expression(String expression) {
		condition.setExpression(expression);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCondition usage(UsageType usage) {
		condition.setUsage(usage);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public ConditionMetaData get() {
		return condition;
	}
}
