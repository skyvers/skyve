package org.skyve.metadata.model.document;

import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.model.Attribute.UsageType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A named boolean expression evaluated against a bean instance.
 *
 * <p>Conditions are declared in the document XML under {@code <conditions>}. Each
 * condition has an {@link #getExpression() expression} (a Java boolean expression using
 * bean bindings) that is evaluated by Skyve at render time. Views reference conditions by
 * name to drive widget visibility, disablement, and mandatory rules without hard-coding
 * logic in views.
 *
 * @see Document#getConditionNames()
 * @see Document#getCondition(String)
 */
public interface Condition extends DecoratedMetaData {
	/**
	 * Returns extended documentation for this condition.
	 *
	 * @return the documentation; may be {@code null}
	 */
	public @Nullable String getDocumentation();

	/**
	 * Returns a short human-readable description of what this condition tests.
	 *
	 * @return the description; may be {@code null}
	 */
	public @Nullable String getDescription();

	/**
	 * Returns the Java boolean expression that this condition evaluates.
	 *
	 * <p>The expression references bean bindings and is compiled into the generated
	 * domain class as a {@code isConditionName()} method.
	 *
	 * @return the boolean expression; never {@code null}
	 */
	public @Nonnull String getExpression();

	/**
	 * Informs the Skyve framework when to include and exclude the attributes.
	 * @return	the usage.
	 */
	public @Nullable UsageType getUsage();
}
