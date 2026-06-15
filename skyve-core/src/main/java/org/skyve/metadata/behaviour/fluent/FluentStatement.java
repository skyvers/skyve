package org.skyve.metadata.behaviour.fluent;

import org.skyve.impl.metadata.behaviour.IfStatement;
import org.skyve.impl.metadata.behaviour.SetStatement;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;

/**
 * Base fluent wrapper for behaviour statements.
 *
 * <p>Concrete subclasses expose statement-specific fields while this base type
 * provides property copying and statement-type dispatch.
 *
 * @param <T> the concrete fluent statement type
 */
public abstract class FluentStatement<T extends FluentStatement<T>> {
	/**
	 * Creates a fluent statement wrapper.
	 *
	 * <p>Protected visibility ensures only concrete statement builders in this
	 * package hierarchy can instantiate this base type.
	 */
	protected FluentStatement() {
		// nothing to see
	}

	/**
	 * Copies base statement properties from source metadata into this builder.
	 *
	 * @param statement source statement metadata
	 * @return this fluent statement
	 */
	@SuppressWarnings("unchecked")
	protected T fromBase(StatementMetaData statement) {
		statement.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
		return (T) this;
	}

	/**
	 * Creates the correct fluent wrapper for a concrete statement metadata type.
	 *
	 * @param statement source statement metadata
	 * @param <T> concrete fluent statement wrapper type
	 * @return a fluent wrapper for the supplied statement
	 * @throws IllegalStateException if the statement type is unsupported
	 */
	@SuppressWarnings("unchecked")
	public static <T extends FluentStatement<T>> T from(StatementMetaData statement) {
		if (statement instanceof IfStatement ifStatement) {
			return (T) new FluentIfStatement().from(ifStatement);
		}
		if (statement instanceof SetStatement setStatement) {
			return (T) new FluentSetStatement().from(setStatement);
		}
		throw new IllegalStateException(statement + " not catered for");
	}

	/**
	 * Adds or replaces an implementation-specific statement property.
	 *
	 * @param k property key
	 * @param v property value
	 * @return this fluent statement
	 */
	@SuppressWarnings("unchecked")
	public T putProperty(String k, String v) {
		get().getProperties().put(k, v);
		return (T) this;
	}

	/**
	 * Returns the backing statement metadata.
	 *
	 * @return backing statement metadata
	 */
	public abstract StatementMetaData get();
}
