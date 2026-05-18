package org.skyve.impl.web.service.smartclient;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

@SuppressWarnings("static-method")
public class OperationEnumTest {

	@Test
	public void valuesContainsFourOperations() {
		Operation[] values = Operation.values();
		assertNotNull(values);
		assertEquals(4, values.length);
	}

	@Test
	public void valueOfFetch() {
		assertEquals(Operation.fetch, Operation.valueOf("fetch"));
	}

	@Test
	public void valueOfAdd() {
		assertEquals(Operation.add, Operation.valueOf("add"));
	}

	@Test
	public void valueOfUpdate() {
		assertEquals(Operation.update, Operation.valueOf("update"));
	}

	@Test
	public void valueOfRemove() {
		assertEquals(Operation.remove, Operation.valueOf("remove"));
	}
}
