package org.skyve.metadata.behaviour.fluent;

import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds and mutates {@link ActionMetaData} instances using a fluent API.
 *
 * <p>Use this builder when creating behaviour metadata programmatically or when
 * cloning existing action metadata via {@link #from(ActionMetaData)}.
 */
public class FluentAction {
	private ActionMetaData action = null;

	/**
	 * Creates a builder with a new empty action metadata instance.
	 */
	public FluentAction() {
		action = new ActionMetaData();
	}

	/**
	 * Creates a builder around an existing mutable action metadata instance.
	 *
	 * @param action the backing metadata instance
	 */
	public FluentAction(@Nonnull ActionMetaData action) {
		this.action = action;
	}

	/**
	 * Copies action metadata into this builder.
	 *
	 * @param action the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentAction from(@SuppressWarnings("hiding") @Nonnull ActionMetaData action) {
		name(action.getName());
		documentation(action.getDocumentation());
		action.getStatements().forEach(a -> addStatement(FluentStatement.from(a)));
		action.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Sets the action name.
	 *
	 * @param name action name
	 * @return this builder
	 */
	public @Nonnull FluentAction name(@Nonnull String name) {
		action.setName(name);
		return this;
	}

	/**
	 * Sets optional action documentation.
	 *
	 * @param documentation action documentation text
	 * @return this builder
	 */
	public @Nonnull FluentAction documentation(@Nullable String documentation) {
		action.setDocumentation(documentation);
		return this;
	}

	/**
	 * Appends a statement to the action body.
	 *
	 * @param statement the statement wrapper to append
	 * @return this builder
	 */
	public @Nonnull FluentAction addStatement(@Nonnull FluentStatement<?> statement) {
		action.getStatements().add(statement.get());
		return this;
	}
	
	/**
	 * Inserts a statement at the specified position.
	 *
	 * @param index insertion index
	 * @param statement the statement wrapper to insert
	 * @return this builder
	 */
	public @Nonnull FluentAction addStatement(int index, @Nonnull FluentStatement<?> statement) {
		action.getStatements().add(index, statement.get());
		return this;
	}

	/**
	 * Removes all statements from the action body.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentAction clearStatements() {
		action.getStatements().clear();
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
	public @Nonnull FluentAction putProperty(@Nonnull String k, @Nonnull String v) {
		action.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return the backing action metadata
	 */
	public @Nonnull ActionMetaData get() {
		return action;
	}
}
