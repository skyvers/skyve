package org.skyve.metadata.behaviour.fluent;

import org.skyve.impl.metadata.behaviour.IfStatement;

public class FluentIfStatement extends FluentStatement<FluentIfStatement> {
	private IfStatement ifStatement = null;
	
	public FluentIfStatement() {
		ifStatement = new IfStatement();
	}

	public FluentIfStatement(IfStatement ifStatement) {
		this.ifStatement = ifStatement;
	}

	public FluentIfStatement from(@SuppressWarnings("hiding") IfStatement ifStatement) {
		super.fromBase(ifStatement);
		condition(ifStatement.getCondition());
		ifStatement.getThenStatements().forEach(s -> addThenStatement(FluentStatement.from(s)));
		ifStatement.getElseStatements().forEach(s -> addElseStatement(FluentStatement.from(s)));
		
		return this;
	}
	
	public FluentIfStatement condition(String condition) {
		ifStatement.setCondition(condition);
		return this;
	}

	public FluentIfStatement addThenStatement(FluentStatement<?> statement) {
		ifStatement.getThenStatements().add(statement.get());
		return this;
	}

	public FluentIfStatement addThenStatement(int index, FluentStatement<?> statement) {
		ifStatement.getThenStatements().add(index, statement.get());
		return this;
	}

	public FluentIfStatement clearThenStatements() {
		ifStatement.getThenStatements().clear();
		return this;
	}

	public FluentIfStatement addElseStatement(FluentStatement<?> statement) {
		ifStatement.getElseStatements().add(statement.get());
		return this;
	}

	public FluentIfStatement addElseStatement(int index, FluentStatement<?> statement) {
		ifStatement.getElseStatements().add(index, statement.get());
		return this;
	}

	public FluentIfStatement clearElseStatements() {
		ifStatement.getElseStatements().clear();
		return this;
	}

	@Override
	public IfStatement get() {
		return ifStatement;
	}
}
