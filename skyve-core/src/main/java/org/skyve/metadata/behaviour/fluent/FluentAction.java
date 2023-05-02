package org.skyve.metadata.behaviour.fluent;

import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;

public class FluentAction {
	private ActionMetaData action = null;

	public FluentAction() {
		action = new ActionMetaData();
	}

	public FluentAction(ActionMetaData action) {
		this.action = action;
	}

	public FluentAction from(@SuppressWarnings("hiding") ActionMetaData action) {
		name(action.getName());
		documentation(action.getDocumentation());
		action.getStatements().forEach(a -> addStatement(FluentStatement.from(a)));

		return this;
	}

	public FluentAction name(String name) {
		action.setName(name);
		return this;
	}

	public FluentAction documentation(String documentation) {
		action.setDocumentation(documentation);
		return this;
	}

	public FluentAction addStatement(FluentStatement<?> statement) {
		action.getStatements().add(statement.get());
		return this;
	}
	
	public FluentAction addStatement(int index, FluentStatement<?> statement) {
		action.getStatements().add(index, statement.get());
		return this;
	}

	public FluentAction clearStatements() {
		action.getStatements().clear();
		return this;
	}

	public ActionMetaData get() {
		return action;
	}
}
