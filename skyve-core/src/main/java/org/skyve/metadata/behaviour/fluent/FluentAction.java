package org.skyve.metadata.behaviour.fluent;

import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;

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
	public FluentAction(ActionMetaData action) {
		this.action = action;
	}

	/**
	 * Copies action metadata into this builder.
	 *
	 * @param action the source action metadata
	 * @return this builder
	 */
	public FluentAction from(@SuppressWarnings("hiding") ActionMetaData action) {
		name(action.getName());
		documentation(action.getDocumentation());
		action.getStatements().forEach(a -> addStatement(FluentStatement.from(a)));

		return this;
	}

	/**
	 * Sets the action name.
	 *
	 * @param name action name
	 * @return this builder
	 */
	public FluentAction name(String name) {
		action.setName(name);
		return this;
	}

	/**
	 * Sets optional action documentation.
	 *
	 * @param documentation action documentation text
	 * @return this builder
	 */
	public FluentAction documentation(String documentation) {
		action.setDocumentation(documentation);
		return this;
	}

	/**
	 * Appends a statement to the action body.
	 *
	 * @param statement the statement wrapper to append
	 * @return this builder
	 */
	public FluentAction addStatement(FluentStatement<?> statement) {
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
	public FluentAction addStatement(int index, FluentStatement<?> statement) {
		action.getStatements().add(index, statement.get());
		return this;
	}

	/**
	 * Removes all statements from the action body.
	 *
	 * @return this builder
	 */
	public FluentAction clearStatements() {
		action.getStatements().clear();
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return the backing action metadata
	 */
	public ActionMetaData get() {
		return action;
	}
}
