package org.skyve.metadata.sail.language;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ProcedureTest {

	@Test
	void getStepsReturnsNonNullList() {
		assertNotNull(new Procedure().getSteps());
	}
}
