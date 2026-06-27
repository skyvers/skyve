package org.skyve.impl.metadata.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;

@SuppressWarnings("static-method")
class IfStatementTest {

	@Test
	void setConditionRoundtrip() {
		IfStatement stmt = new IfStatement();
		stmt.setCondition("someCondition");
		assertThat(stmt.getCondition(), is("someCondition"));
	}

	@Test
	void setConditionBlankBecomesNull() {
		IfStatement stmt = new IfStatement();
		stmt.setCondition("   ");
		assertNull(stmt.getCondition());
	}

	@Test
	void thenStatementsInitiallyEmpty() {
		IfStatement stmt = new IfStatement();
		assertNotNull(stmt.getThenStatements());
		assertTrue(stmt.getThenStatements().isEmpty());
	}

	@Test
	void elseStatementsInitiallyEmpty() {
		IfStatement stmt = new IfStatement();
		assertNotNull(stmt.getElseStatements());
		assertTrue(stmt.getElseStatements().isEmpty());
	}

	// ---- execute() ----

	/** Counting statement for verifying execute path. */
	static class CountStatement extends StatementMetaData {
		private static final long serialVersionUID = 1L;
		final AtomicInteger count = new AtomicInteger(0);
		@Override
		public void execute(org.skyve.domain.Bean bean) {
			count.incrementAndGet();
		}
	}

	@Test
	void executeRunsThenStatementsWhenConditionIsTrue() {
		// DynamicBean with a Boolean.TRUE flag property
		Map<String, Object> props = new HashMap<>();
		props.put("myFlag", Boolean.TRUE);
		DynamicBean bean = new DynamicBean("mod", "Doc", props);

		CountStatement thenStmt = new CountStatement();
		CountStatement elseStmt = new CountStatement();

		IfStatement ifStmt = new IfStatement();
		// Use {bean:myFlag} which evaluates to Boolean.TRUE
		ifStmt.setCondition("{bean:myFlag}");
		ifStmt.getThenStatements().add(thenStmt);
		ifStmt.getElseStatements().add(elseStmt);

		ifStmt.execute(bean);

		assertEquals(1, thenStmt.count.get());
		assertEquals(0, elseStmt.count.get());
	}

	@Test
	void executeRunsElseStatementsWhenConditionIsFalse() {
		// DynamicBean with a non-boolean property → evaluates to non-TRUE
		Map<String, Object> props = new HashMap<>();
		DynamicBean bean = new DynamicBean("admin", "User", props);

		CountStatement thenStmt = new CountStatement();
		CountStatement elseStmt = new CountStatement();

		IfStatement ifStmt = new IfStatement();
		// {bean:bizModule} returns "admin" (String), not Boolean.TRUE → else branch
		ifStmt.setCondition("{bean:bizModule}");
		ifStmt.getThenStatements().add(thenStmt);
		ifStmt.getElseStatements().add(elseStmt);

		ifStmt.execute(bean);

		assertEquals(0, thenStmt.count.get());
		assertEquals(1, elseStmt.count.get());
	}

	@Test
	void executeRunsElseStatementsWhenConditionReturnsFalse() {
		Map<String, Object> props = new HashMap<>();
		props.put("active", Boolean.FALSE);
		DynamicBean bean = new DynamicBean("mod", "Doc", props);

		CountStatement thenStmt = new CountStatement();
		CountStatement elseStmt = new CountStatement();

		IfStatement ifStmt = new IfStatement();
		ifStmt.setCondition("{bean:active}");
		ifStmt.getThenStatements().add(thenStmt);
		ifStmt.getElseStatements().add(elseStmt);

		ifStmt.execute(bean);

		assertEquals(0, thenStmt.count.get());
		assertEquals(1, elseStmt.count.get());
	}
}
