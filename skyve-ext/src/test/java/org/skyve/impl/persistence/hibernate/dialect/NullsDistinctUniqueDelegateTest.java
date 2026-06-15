package org.skyve.impl.persistence.hibernate.dialect;

import static org.junit.Assert.assertTrue;

import org.hibernate.dialect.H2Dialect;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.UniqueKey;
import org.junit.Test;

@SuppressWarnings("static-method")
public class NullsDistinctUniqueDelegateTest {

	@Test
	public void uniqueConstraintSqlContainsNullsDistinct() {
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
			assertTrue("Should contain 'unique nulls distinct ('", result.contains("unique nulls distinct ("));
		} catch (Exception e) {
			throw new AssertionError("Failed to invoke uniqueConstraintSql", e);
		}
	}
}
