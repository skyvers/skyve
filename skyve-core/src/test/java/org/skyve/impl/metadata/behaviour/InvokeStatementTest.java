package org.skyve.impl.metadata.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class InvokeStatementTest {

	@Test
	@SuppressWarnings("static-method")
	public void setMethodNameRoundtrip() {
		InvokeStatement stmt = new InvokeStatement();
		stmt.setMethodName("doSomething");
		assertThat(stmt.getMethodName(), is("doSomething"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setMethodNameBlankBecomesNull() {
		InvokeStatement stmt = new InvokeStatement();
		stmt.setMethodName("   ");
		assertNull(stmt.getMethodName());
	}

	@Test
	@SuppressWarnings("static-method")
	public void argumentsInitiallyEmpty() {
		InvokeStatement stmt = new InvokeStatement();
		assertNotNull(stmt.getArguments());
		assertTrue(stmt.getArguments().isEmpty());
	}

	// ---- MethodArgument ----

	@Test
	@SuppressWarnings("static-method")
	public void methodArgumentTypeNameRoundtrip() {
		MethodArgument arg = new MethodArgument();
		arg.setTypeName("java.lang.String");
		assertThat(arg.getTypeName(), is("java.lang.String"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void methodArgumentTypeNameBlankBecomesNull() {
		MethodArgument arg = new MethodArgument();
		arg.setTypeName("  ");
		assertNull(arg.getTypeName());
	}

	@Test
	@SuppressWarnings("static-method")
	public void methodArgumentExpressionRoundtrip() {
		MethodArgument arg = new MethodArgument();
		arg.setExpression("bean.name");
		assertThat(arg.getExpression(), is("bean.name"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void methodArgumentExpressionBlankBecomesNull() {
		MethodArgument arg = new MethodArgument();
		arg.setExpression("  ");
		assertNull(arg.getExpression());
	}

	@Test
	@SuppressWarnings("static-method")
	public void invokeStatementCanAddArgument() {
		InvokeStatement stmt = new InvokeStatement();
		MethodArgument arg = new MethodArgument();
		arg.setTypeName("java.lang.Integer");
		arg.setExpression("bean.count");
		stmt.getArguments().add(arg);
		org.junit.jupiter.api.Assertions.assertEquals(1, stmt.getArguments().size());
	}
}
