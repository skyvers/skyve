package org.skyve.metadata.behaviour.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.behaviour.IfStatement;
import org.skyve.impl.metadata.behaviour.SetStatement;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;

/**
 * Tests for behaviour fluent builders.
 */
@SuppressWarnings("static-method")
class FluentBehaviourTest {

	// --- FluentAction ---

	@Test
	void actionDefaultConstructorCreatesInstance() {
		FluentAction a = new FluentAction();
		assertNotNull(a.get());
	}

	@Test
	void actionWrappingConstructorUsesProvided() {
		ActionMetaData md = new ActionMetaData();
		FluentAction a = new FluentAction(md);
		assertSame(md, a.get());
	}

	@Test
	void actionNameReturnsSelf() {
		FluentAction a = new FluentAction();
		FluentAction result = a.name("myAction");
		assertSame(a, result);
		assertEquals("myAction", a.get().getName());
	}

	@Test
	void actionDocumentationReturnsSelf() {
		FluentAction a = new FluentAction();
		FluentAction result = a.documentation("some docs");
		assertSame(a, result);
		assertEquals("some docs", a.get().getDocumentation());
	}

	@Test
	void actionAddStatementIncreasesCount() {
		FluentAction a = new FluentAction();
		FluentAction result = a.addStatement(new FluentSetStatement().binding("name").expression("value"));
		assertSame(a, result);
		assertEquals(1, a.get().getStatements().size());
	}

	@Test
	void actionAddStatementAtIndexInsertsAtPosition() {
		FluentAction a = new FluentAction();
		a.addStatement(new FluentSetStatement().binding("first").expression("v1"));
		a.addStatement(0, new FluentSetStatement().binding("second").expression("v2"));
		assertEquals(2, a.get().getStatements().size());
		assertEquals("second", ((SetStatement) a.get().getStatements().get(0)).getBinding());
	}

	@Test
	void actionClearStatementsEmptiesCollection() {
		FluentAction a = new FluentAction();
		a.addStatement(new FluentSetStatement().binding("x").expression("y"));
		FluentAction cleared = a.clearStatements();
		assertSame(a, cleared);
		assertEquals(0, a.get().getStatements().size());
	}

	// --- FluentSetStatement ---

	@Test
	void setStatementDefaultConstructorCreatesInstance() {
		FluentSetStatement s = new FluentSetStatement();
		assertNotNull(s.get());
	}

	@Test
	void setStatementWrappingConstructorUsesProvided() {
		SetStatement ss = new SetStatement();
		FluentSetStatement s = new FluentSetStatement(ss);
		assertSame(ss, s.get());
	}

	@Test
	void setStatementBindingReturnsSelf() {
		FluentSetStatement s = new FluentSetStatement();
		FluentSetStatement result = s.binding("myField");
		assertSame(s, result);
		assertEquals("myField", s.get().getBinding());
	}

	@Test
	void setStatementExpressionReturnsSelf() {
		FluentSetStatement s = new FluentSetStatement();
		FluentSetStatement result = s.expression("someExpression");
		assertSame(s, result);
		assertEquals("someExpression", s.get().getExpression());
	}

	// --- FluentIfStatement ---

	@Test
	void ifStatementDefaultConstructorCreatesInstance() {
		FluentIfStatement s = new FluentIfStatement();
		assertNotNull(s.get());
	}

	@Test
	void ifStatementWrappingConstructorUsesProvided() {
		IfStatement is = new IfStatement();
		FluentIfStatement s = new FluentIfStatement(is);
		assertSame(is, s.get());
	}

	@Test
	void ifStatementConditionReturnsSelf() {
		FluentIfStatement s = new FluentIfStatement();
		FluentIfStatement result = s.condition("isActive");
		assertSame(s, result);
		assertEquals("isActive", s.get().getCondition());
	}

	@Test
	void ifStatementAddThenStatementIncreasesCount() {
		FluentIfStatement s = new FluentIfStatement();
		FluentIfStatement result = s.addThenStatement(new FluentSetStatement().binding("x").expression("y"));
		assertSame(s, result);
		assertEquals(1, s.get().getThenStatements().size());
	}

	@Test
	void ifStatementAddThenStatementAtIndexInsertsAtPosition() {
		FluentIfStatement s = new FluentIfStatement();
		s.addThenStatement(new FluentSetStatement().binding("first").expression("v1"));
		s.addThenStatement(0, new FluentSetStatement().binding("second").expression("v2"));
		assertEquals("second", ((SetStatement) s.get().getThenStatements().get(0)).getBinding());
	}

	@Test
	void ifStatementClearThenStatementsEmptiesCollection() {
		FluentIfStatement s = new FluentIfStatement();
		s.addThenStatement(new FluentSetStatement().binding("a").expression("b"));
		FluentIfStatement cleared = s.clearThenStatements();
		assertSame(s, cleared);
		assertEquals(0, s.get().getThenStatements().size());
	}

	@Test
	void ifStatementAddElseStatementIncreasesCount() {
		FluentIfStatement s = new FluentIfStatement();
		FluentIfStatement result = s.addElseStatement(new FluentSetStatement().binding("x").expression("y"));
		assertSame(s, result);
		assertEquals(1, s.get().getElseStatements().size());
	}

	@Test
	void ifStatementAddElseStatementAtIndexInsertsAtPosition() {
		FluentIfStatement s = new FluentIfStatement();
		s.addElseStatement(new FluentSetStatement().binding("first").expression("v1"));
		s.addElseStatement(0, new FluentSetStatement().binding("second").expression("v2"));
		assertEquals("second", ((SetStatement) s.get().getElseStatements().get(0)).getBinding());
	}

	@Test
	void ifStatementClearElseStatementsEmptiesCollection() {
		FluentIfStatement s = new FluentIfStatement();
		s.addElseStatement(new FluentSetStatement().binding("a").expression("b"));
		FluentIfStatement cleared = s.clearElseStatements();
		assertSame(s, cleared);
		assertEquals(0, s.get().getElseStatements().size());
	}

	// --- FluentStatement.putProperty ---

	@Test
	void setStatementPutPropertyReturnsSelf() {
		FluentSetStatement s = new FluentSetStatement();
		FluentSetStatement result = s.putProperty("key", "value");
		assertSame(s, result);
	}

	// --- FluentStatement.from (static factory) ---

	@Test
	void statementFromSetStatementCreatesFluentSetStatement() {
		SetStatement ss = new SetStatement();
		FluentStatement<?> result = FluentStatement.from(ss);
		assertNotNull(result);
	}

	@Test
	void statementFromIfStatementCreatesFluentIfStatement() {
		IfStatement is = new IfStatement();
		FluentStatement<?> result = FluentStatement.from(is);
		assertNotNull(result);
	}

	@Test
	void statementFromNullThrowsIllegalStateException() {
		org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class, () -> FluentStatement.from(null));
	}
}
