package org.skyve.impl.tools.jasperreports;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

class SkyveQueryExecuterTest {

	@Test
	static void cancelQueryIsNotSupported() throws Exception {
		SkyveQueryExecuter executer = new SkyveQueryExecuter("admin.Contact");
		assertFalse(executer.cancelQuery());
	}

	@Test
	static void closeDoesNothing() {
		SkyveQueryExecuter executer = new SkyveQueryExecuter("admin.Contact");
		assertDoesNotThrow(executer::close);
	}
}
