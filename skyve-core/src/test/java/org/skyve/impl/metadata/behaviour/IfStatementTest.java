package org.skyve.impl.metadata.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

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
}
