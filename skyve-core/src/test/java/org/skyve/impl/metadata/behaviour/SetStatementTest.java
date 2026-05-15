package org.skyve.impl.metadata.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SetStatementTest {

	@Test
	void setBindingRoundtrip() {
		SetStatement stmt = new SetStatement();
		stmt.setBinding("name");
		assertThat(stmt.getBinding(), is("name"));
	}

	@Test
	void setBindingBlankBecomesNull() {
		SetStatement stmt = new SetStatement();
		stmt.setBinding("  ");
		assertNull(stmt.getBinding());
	}

	@Test
	void setExpressionRoundtrip() {
		SetStatement stmt = new SetStatement();
		stmt.setExpression("{bean.value}");
		assertThat(stmt.getExpression(), is("{bean.value}"));
	}

	@Test
	void setExpressionBlankBecomesNull() {
		SetStatement stmt = new SetStatement();
		stmt.setExpression("  ");
		assertNull(stmt.getExpression());
	}
}
