package org.skyve.metadata.behaviour.fluent;

import org.skyve.impl.metadata.behaviour.SetStatement;

/**
 * Builds a behaviour {@code set} statement.
 *
 * <p>A set statement assigns an expression result to a target binding.
 */
public class FluentSetStatement extends FluentStatement<FluentSetStatement> {
	private SetStatement setStatement = null;
	
	/**
	 * Creates a builder with a new empty set statement.
	 */
	public FluentSetStatement() {
		setStatement = new SetStatement();
	}

	/**
	 * Creates a builder around an existing set statement.
	 *
	 * @param setStatement the backing statement metadata
	 */
	public FluentSetStatement(SetStatement setStatement) {
		this.setStatement = setStatement;
	}

	/**
	 * Copies an existing set statement into this builder.
	 *
	 * @param setStatement source metadata
	 * @return this builder
	 */
	public FluentSetStatement from(@SuppressWarnings("hiding") SetStatement setStatement) {
		super.fromBase(setStatement);
		binding(setStatement.getBinding());
		expression(setStatement.getExpression());
		return this;
	}
	
	/**
	 * Sets the target binding to receive the expression result.
	 *
	 * @param binding dot-path binding expression
	 * @return this builder
	 */
	public FluentSetStatement binding(String binding) {
		setStatement.setBinding(binding);
		return this;
	}

	/**
	 * Sets the expression evaluated when this statement executes.
	 *
	 * @param expression Skyve expression string
	 * @return this builder
	 */
	public FluentSetStatement expression(String expression) {
		setStatement.setExpression(expression);
		return this;
	}

	/**
	 * Returns the backing set statement metadata.
	 *
	 * @return backing set statement metadata
	 */
	@Override
	public SetStatement get() {
		return setStatement;
	}
}
