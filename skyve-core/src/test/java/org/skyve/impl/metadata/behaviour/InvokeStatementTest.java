package org.skyve.impl.metadata.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.DomainException;

public class InvokeStatementTest {

	/** Simple bean with a no-arg and a one-arg method for testing execute(). */
	public static class SampleBean extends DynamicBean {
		private static final long serialVersionUID = 1L;
		private String value = "";

		public SampleBean() {
			super("test", "SampleBean", new java.util.HashMap<>());
		}

		public void doSomething() {
			value = "called";
		}

		public void setStr(String s) {
			value = s;
		}

		public String getValue() { return value; }
	}

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

	// ---- execute() ----

	@Test
	@SuppressWarnings("static-method")
	public void executeCallsNoArgMethodOnBean() {
		SampleBean bean = new SampleBean();
		InvokeStatement stmt = new InvokeStatement();
		stmt.setMethodName("doSomething");
		stmt.execute(bean);
		assertThat(bean.getValue(), is("called"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void executeThrowsDomainExceptionWhenMethodNotFound() {
		SampleBean bean = new SampleBean();
		InvokeStatement stmt = new InvokeStatement();
		stmt.setMethodName("nonExistentMethod");
		assertThrows(DomainException.class, () -> stmt.execute(bean));
	}
}
