package org.skyve.domain.messages;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.OptimisticLockException.OperationType;

public class OptimisticLockExceptionOperationTypeTest {

	@Test
	@SuppressWarnings("static-method")
	public void valuesContainsTwoOperationTypes() {
		assertEquals(2, OperationType.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfUpdate() {
		assertNotNull(OperationType.valueOf("update"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfDelete() {
		assertNotNull(OperationType.valueOf("delete"));
	}
}
