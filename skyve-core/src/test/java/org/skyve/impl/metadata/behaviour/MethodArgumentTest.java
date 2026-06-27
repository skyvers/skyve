package org.skyve.impl.metadata.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class MethodArgumentTest {

	@Test
	@SuppressWarnings("static-method")
	void setTypeNameRoundtrip() {
		MethodArgument arg = new MethodArgument();
		arg.setTypeName("java.lang.String");
		assertThat(arg.getTypeName(), is("java.lang.String"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setTypeNameBlankBecomesNull() {
		MethodArgument arg = new MethodArgument();
		arg.setTypeName("  ");
		assertNull(arg.getTypeName());
	}

	@Test
	@SuppressWarnings("static-method")
	void setExpressionRoundtrip() {
		MethodArgument arg = new MethodArgument();
		arg.setExpression("{bean.name}");
		assertThat(arg.getExpression(), is("{bean.name}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setExpressionBlankBecomesNull() {
		MethodArgument arg = new MethodArgument();
		arg.setExpression("   ");
		assertNull(arg.getExpression());
	}
}
