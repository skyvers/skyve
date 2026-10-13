package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.repository.document.ConditionMetaData;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Condition;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public FluentCondition(@Nonnull ConditionMetaData condition) {
		this.condition = condition;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentCondition from(@Nonnull String conditionName, @SuppressWarnings("hiding") @Nonnull Condition condition) {
		name(conditionName);
		documentation(condition.getDocumentation());
		description(condition.getDescription());
		expression(condition.getExpression());
		usage(condition.getUsage());
		condition.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentCondition name(@Nullable String name) {
		condition.setName(name);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentCondition documentation(@Nullable String documentation) {
		condition.setDocumentation(documentation);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentCondition description(@Nullable String description) {
		condition.setDescription(description);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentCondition expression(@Nullable String expression) {
		condition.setExpression(expression);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentCondition usage(@Nullable UsageType usage) {
		condition.setUsage(usage);
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentCondition putProperty(@Nonnull String k, @Nonnull String v) {
		condition.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public @Nonnull ConditionMetaData get() {
		return condition;
	}
}
