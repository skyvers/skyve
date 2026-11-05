package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class OperationEnumTest {

	@Test
	void valuesContainsFourOperations() {
		Operation[] values = Operation.values();
		assertNotNull(values);
		assertEquals(4, values.length);
	}

	@Test
	void valueOfFetch() {
		assertEquals(Operation.fetch, Operation.valueOf("fetch"));
	}

	@Test
	void valueOfAdd() {
		assertEquals(Operation.add, Operation.valueOf("add"));
	}

	@Test
	void valueOfUpdate() {
		assertEquals(Operation.update, Operation.valueOf("update"));
	}

	@Test
	void valueOfRemove() {
		assertEquals(Operation.remove, Operation.valueOf("remove"));
	}
}
