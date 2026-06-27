package org.skyve.metadata.behaviour.fluent;

import org.skyve.impl.metadata.behaviour.IfStatement;

/**
 * Builds a behaviour {@code if} statement with then/else branches.
 */
public class FluentIfStatement extends FluentStatement<FluentIfStatement> {
	private IfStatement ifStatement = null;
	
	/**
	 * Creates a builder with a new empty if statement.
	 */
	public FluentIfStatement() {
		ifStatement = new IfStatement();
	}

	/**
	 * Creates a builder around an existing if statement.
	 *
	 * @param ifStatement backing if statement metadata
	 */
	public FluentIfStatement(IfStatement ifStatement) {
		this.ifStatement = ifStatement;
	}

	/**
	 * Copies an existing conditional statement into this builder.
	 *
	 * @param ifStatement source metadata
	 * @return this builder
	 */
	public FluentIfStatement from(@SuppressWarnings("hiding") IfStatement ifStatement) {
		super.fromBase(ifStatement);
		condition(ifStatement.getCondition());
		ifStatement.getThenStatements().forEach(s -> addThenStatement(FluentStatement.from(s)));
		ifStatement.getElseStatements().forEach(s -> addElseStatement(FluentStatement.from(s)));
		
		return this;
	}
	
	/**
	 * Sets the condition expression used to choose then versus else branch.
	 *
	 * @param condition Skyve condition expression
	 * @return this builder
	 */
	public FluentIfStatement condition(String condition) {
		ifStatement.setCondition(condition);
		return this;
	}

	/**
	 * Appends a statement to the then branch.
	 *
	 * @param statement statement wrapper to append
	 * @return this builder
	 */
	public FluentIfStatement addThenStatement(FluentStatement<?> statement) {
		ifStatement.getThenStatements().add(statement.get());
		return this;
	}

	/**
	 * Inserts a statement in the then branch.
	 *
	 * @param index insertion index
	 * @param statement statement wrapper to insert
	 * @return this builder
	 */
	public FluentIfStatement addThenStatement(int index, FluentStatement<?> statement) {
		ifStatement.getThenStatements().add(index, statement.get());
		return this;
	}

	/**
	 * Removes all statements from the then branch.
	 *
	 * @return this builder
	 */
	public FluentIfStatement clearThenStatements() {
		ifStatement.getThenStatements().clear();
		return this;
	}

	/**
	 * Appends a statement to the else branch.
	 *
	 * @param statement statement wrapper to append
	 * @return this builder
	 */
	public FluentIfStatement addElseStatement(FluentStatement<?> statement) {
		ifStatement.getElseStatements().add(statement.get());
		return this;
	}

	/**
	 * Inserts a statement in the else branch.
	 *
	 * @param index insertion index
	 * @param statement statement wrapper to insert
	 * @return this builder
	 */
	public FluentIfStatement addElseStatement(int index, FluentStatement<?> statement) {
		ifStatement.getElseStatements().add(index, statement.get());
		return this;
	}

	/**
	 * Removes all statements from the else branch.
	 *
	 * @return this builder
	 */
	public FluentIfStatement clearElseStatements() {
		ifStatement.getElseStatements().clear();
		return this;
	}

	/**
	 * Returns the backing if statement metadata.
	 *
	 * @return backing if statement metadata
	 */
	@Override
	public IfStatement get() {
		return ifStatement;
	}
}
