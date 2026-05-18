package org.skyve.impl.metadata.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.DomainException;

public class InvokeStaticStatementTest {

	@Test
	@SuppressWarnings("static-method")
	public void setClassNameRoundtrip() {
		InvokeStaticStatement stmt = new InvokeStaticStatement();
		stmt.setClassName("org.skyve.util.Util");
		assertThat(stmt.getClassName(), is("org.skyve.util.Util"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setClassNameBlankBecomesNull() {
		InvokeStaticStatement stmt = new InvokeStaticStatement();
		stmt.setClassName("   ");
		assertNull(stmt.getClassName());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setMethodNameAlsoRoundtrips() {
		InvokeStaticStatement stmt = new InvokeStaticStatement();
		stmt.setMethodName("compute");
		assertThat(stmt.getMethodName(), is("compute"));
	}

	// ---- execute() ----

	@Test
	@SuppressWarnings("static-method")
	public void executeThrowsDomainExceptionForUnknownClass() {
		InvokeStaticStatementTest.SampleBean bean = new InvokeStaticStatementTest.SampleBean();
		InvokeStaticStatement stmt = new InvokeStaticStatement();
		stmt.setClassName("com.nonexistent.MyClass");
		stmt.setMethodName("doSomething");
		assertThrows(DomainException.class, () -> stmt.execute(bean));
	}

	@Test
	@SuppressWarnings("static-method")
	public void executeThrowsDomainExceptionForUnknownMethod() {
		InvokeStaticStatementTest.SampleBean bean = new InvokeStaticStatementTest.SampleBean();
		InvokeStaticStatement stmt = new InvokeStaticStatement();
		stmt.setClassName(SampleHelper.class.getName());
		stmt.setMethodName("nonExistentMethod");
		assertThrows(DomainException.class, () -> stmt.execute(bean));
	}

	@Test
	@SuppressWarnings("static-method")
	public void executeSuccessfullyCallsStaticMethod() {
		InvokeStaticStatementTest.SampleBean bean = new InvokeStaticStatementTest.SampleBean();
		InvokeStaticStatement stmt = new InvokeStaticStatement();
		stmt.setClassName(SampleHelper.class.getName());
		stmt.setMethodName("doWork");
		// Should execute without throwing
		assertDoesNotThrow(() -> stmt.execute(bean));
	}

	// ---- Helpers ----

	/** Simple bean for testing. */
	public static class SampleBean extends DynamicBean {
		private static final long serialVersionUID = 1L;

		public SampleBean() {
			super("test", "SampleBean", new java.util.HashMap<>());
		}
	}

	/** Helper class with a public static method. */
	public static class SampleHelper {
		public static void doWork() {
			// no-op
		}
	}
}
