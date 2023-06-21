package org.skyve.metadata.behaviour.fluent;

import org.skyve.impl.metadata.behaviour.SetStatement;

public class FluentSetStatement extends FluentStatement<FluentSetStatement> {
	private SetStatement setStatement = null;
	
	public FluentSetStatement() {
		setStatement = new SetStatement();
	}

	public FluentSetStatement(SetStatement setStatement) {
		this.setStatement = setStatement;
	}

	public FluentSetStatement from(@SuppressWarnings("hiding") SetStatement setStatement) {
		super.fromBase(setStatement);
		binding(setStatement.getBinding());
		expression(setStatement.getExpression());
		return this;
	}
	
	public FluentSetStatement binding(String binding) {
		setStatement.setBinding(binding);
		return this;
	}

	public FluentSetStatement expression(String expression) {
		setStatement.setExpression(expression);
		return this;
	}

	@Override
	public SetStatement get() {
		return setStatement;
	}
}
