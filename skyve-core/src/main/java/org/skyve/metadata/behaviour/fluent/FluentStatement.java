package org.skyve.metadata.behaviour.fluent;

import org.skyve.impl.metadata.behaviour.IfStatement;
import org.skyve.impl.metadata.behaviour.SetStatement;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;

public abstract class FluentStatement<T extends FluentStatement<T>> {
	protected FluentStatement() {
		// nothing to see
	}

	@SuppressWarnings("unchecked")
	protected T fromBase(StatementMetaData statement) {
		statement.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public static <T extends FluentStatement<T>> T from(StatementMetaData statement) {
		if (statement instanceof IfStatement) {
			return (T) new FluentIfStatement().from((IfStatement) statement);
		}
		if (statement instanceof SetStatement) {
			return (T) new FluentSetStatement().from((SetStatement) statement);
		}
		throw new IllegalStateException(statement + " not catered for");
	}

	@SuppressWarnings("unchecked")
	public T putProperty(String k, String v) {
		get().getProperties().put(k, v);
		return (T) this;
	}

	public abstract StatementMetaData get();
}
