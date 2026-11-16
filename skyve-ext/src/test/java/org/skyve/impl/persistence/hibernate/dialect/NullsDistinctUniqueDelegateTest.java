package org.skyve.impl.persistence.hibernate.dialect;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.hibernate.dialect.H2Dialect;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.UniqueKey;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class NullsDistinctUniqueDelegateTest {

	@Test
	void uniqueConstraintSqlContainsNullsDistinct() {
		NullsDistinctUniqueDelegate delegate = new NullsDistinctUniqueDelegate(new H2Dialect());

		// Build a UniqueKey with one column
		UniqueKey uniqueKey = new UniqueKey();
		Column col = new Column("name");
		uniqueKey.addColumn(col);

		// Use reflection to invoke the protected uniqueConstraintSql method
		try {
			java.lang.reflect.Method method = NullsDistinctUniqueDelegate.class.getDeclaredMethod("uniqueConstraintSql", UniqueKey.class);
			method.setAccessible(true);
			String result = (String) method.invoke(delegate, uniqueKey);
			assertTrue(result.contains("unique nulls distinct ("), "Should contain 'unique nulls distinct ('");
		} catch (Exception e) {
			throw new AssertionError("Failed to invoke uniqueConstraintSql", e);
		}
	}
}
