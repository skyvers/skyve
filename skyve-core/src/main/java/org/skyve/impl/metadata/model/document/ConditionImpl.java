package org.skyve.impl.metadata.model.document;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Condition;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Runtime implementation of a {@link Condition} declared on a document.
 *
 * <p>A condition is a named boolean expression (MVEL or Skyve EL) evaluated
 * against a {@link org.skyve.domain.Bean} at runtime.  Conditions are referenced
 * by view widgets (visibility, disability) and business rules.  The expression
 * is compiled lazily on first evaluation and cached.
 *
 * <p>Threading: not thread-safe.  The compiled expression cache is per-instance;
 * the instance itself is populated during metadata loading.
 *
 * @see Condition
 */
public class ConditionImpl implements Condition {
	private static final long serialVersionUID = 4772305611794702365L;

	private String documentation;
	private String description;
	private String expression;
	private UsageType usage;
	private Map<String, String> properties = new LinkedHashMap<>();

	@Override
	public @Nullable String getDocumentation() {
		return documentation;
	}
	public void setDocumentation(@Nullable String documentation) {
		this.documentation = documentation;
	}

	@Override
	public @Nullable String getDescription() {
		return description;
	}
	public void setDescription(@Nullable String description) {
		this.description = description;
	}

	@Override
	public @Nonnull String getExpression() {
		return expression;
	}
	public void setExpression(@Nonnull String expression) {
		this.expression = expression;
	}

	@Override
	public @Nullable UsageType getUsage() {
		return usage;
	}
	public void setUsage(@Nullable UsageType usage) {
		this.usage = usage;
	}
	
	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
