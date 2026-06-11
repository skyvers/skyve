package org.skyve.impl.tools.jasperreports;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SkyveQueryExecuterTest {

	@Test
	void cancelQueryIsNotSupported() throws Exception {
		SkyveQueryExecuter executer = new SkyveQueryExecuter("admin.Contact");
		assertFalse(executer.cancelQuery());
	}

	@Test
	void closeDoesNothing() {
		SkyveQueryExecuter executer = new SkyveQueryExecuter("admin.Contact");
		assertDoesNotThrow(executer::close);
	}
}
