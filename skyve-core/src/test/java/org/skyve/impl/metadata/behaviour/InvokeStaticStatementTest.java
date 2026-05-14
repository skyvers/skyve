package org.skyve.impl.metadata.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

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
}
