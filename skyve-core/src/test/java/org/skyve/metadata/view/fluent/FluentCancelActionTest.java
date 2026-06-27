package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.actions.CancelAction;

class FluentCancelActionTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentCancelAction().get());
	}

	@Test
	@SuppressWarnings("static-method")
	void putPropertyStoresKeyValue() {
		FluentCancelAction action = new FluentCancelAction();
		FluentCancelAction result = action.putProperty("key1", "value1");
		assertSame(action, result);
		assertEquals("value1", action.get().getProperties().get("key1"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putPropertyMultipleEntries() {
		FluentCancelAction action = new FluentCancelAction();
		action.putProperty("a", "1").putProperty("b", "2");
		assertEquals("1", action.get().getProperties().get("a"));
		assertEquals("2", action.get().getProperties().get("b"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromBaseCopiesToActionProperties() {
		CancelAction source = new CancelAction();
		source.getProperties().put("propKey", "propVal");
		FluentAction<?> result = FluentAction.from(source);
		assertEquals("propVal", result.get().getProperties().get("propKey"));
	}
}
