package org.skyve.impl.metadata.model.document;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Condition;

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
	private Map<String, String> properties = new TreeMap<>();

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
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
