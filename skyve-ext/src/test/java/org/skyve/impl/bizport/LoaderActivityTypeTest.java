package org.skyve.impl.bizport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;

@SuppressWarnings("static-method")
class LoaderActivityTypeTest {

	@Test
	void valuesContainsThreeTypes() {
		LoaderActivityType[] values = LoaderActivityType.values();
		assertNotNull(values);
		assertEquals(3, values.length);
	}

	@Test
	void valueOfCreateAll() {
		assertEquals(LoaderActivityType.CREATE_ALL, LoaderActivityType.valueOf("CREATE_ALL"));
	}

	@Test
	void valueOfCreateFind() {
		assertEquals(LoaderActivityType.CREATE_FIND, LoaderActivityType.valueOf("CREATE_FIND"));
	}

	@Test
	void valueOfFind() {
		assertEquals(LoaderActivityType.FIND, LoaderActivityType.valueOf("FIND"));
	}
}
