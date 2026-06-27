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

@SuppressWarnings("static-method")
class InvokeStatementTest {

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
void setMethodNameRoundtrip() {
		InvokeStatement stmt = new InvokeStatement();
		stmt.setMethodName("doSomething");
		assertThat(stmt.getMethodName(), is("doSomething"));
	}

	@Test
void setMethodNameBlankBecomesNull() {
		InvokeStatement stmt = new InvokeStatement();
		stmt.setMethodName("   ");
		assertNull(stmt.getMethodName());
	}

	@Test
void argumentsInitiallyEmpty() {
		InvokeStatement stmt = new InvokeStatement();
		assertNotNull(stmt.getArguments());
		assertTrue(stmt.getArguments().isEmpty());
	}

	// ---- MethodArgument ----

	@Test
void methodArgumentTypeNameRoundtrip() {
		MethodArgument arg = new MethodArgument();
		arg.setTypeName("java.lang.String");
		assertThat(arg.getTypeName(), is("java.lang.String"));
	}

	@Test
void methodArgumentTypeNameBlankBecomesNull() {
		MethodArgument arg = new MethodArgument();
		arg.setTypeName("  ");
		assertNull(arg.getTypeName());
	}

	@Test
void methodArgumentExpressionRoundtrip() {
		MethodArgument arg = new MethodArgument();
		arg.setExpression("bean.name");
		assertThat(arg.getExpression(), is("bean.name"));
	}

	@Test
void methodArgumentExpressionBlankBecomesNull() {
		MethodArgument arg = new MethodArgument();
		arg.setExpression("  ");
		assertNull(arg.getExpression());
	}

	@Test
void invokeStatementCanAddArgument() {
		InvokeStatement stmt = new InvokeStatement();
		MethodArgument arg = new MethodArgument();
		arg.setTypeName("java.lang.Integer");
		arg.setExpression("bean.count");
		stmt.getArguments().add(arg);
		org.junit.jupiter.api.Assertions.assertEquals(1, stmt.getArguments().size());
	}

	// ---- execute() ----

	@Test
void executeCallsNoArgMethodOnBean() {
		SampleBean bean = new SampleBean();
		InvokeStatement stmt = new InvokeStatement();
		stmt.setMethodName("doSomething");
		stmt.execute(bean);
		assertThat(bean.getValue(), is("called"));
	}

	@Test
void executeThrowsDomainExceptionWhenMethodNotFound() {
		SampleBean bean = new SampleBean();
		InvokeStatement stmt = new InvokeStatement();
		stmt.setMethodName("nonExistentMethod");
		assertThrows(DomainException.class, () -> stmt.execute(bean));
	}
}
