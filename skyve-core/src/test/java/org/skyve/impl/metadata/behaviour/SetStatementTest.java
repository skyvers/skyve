package org.skyve.impl.metadata.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;

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

	@Test
	void executeSetsBeanProperty() {
		// Create a DynamicBean with both src and dest pre-existing properties
		Map<String, Object> props = new HashMap<>();
		props.put("src", "hello");
		props.put("dest", null);
		DynamicBean bean = new DynamicBean("admin", "User", props);

		SetStatement stmt = new SetStatement();
		// Set 'dest' binding to the value of expression {bean:src}
		stmt.setBinding("dest");
		stmt.setExpression("{bean:src}");

		stmt.execute(bean);

		// BindUtil.set should have updated 'dest' to the value of 'src'
		assertThat(bean.getDynamic("dest"), is("hello"));
	}
}
