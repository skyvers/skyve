package org.skyve.impl.bizport;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;

@SuppressWarnings("static-method")
public class LoaderActivityTypeTest {

	@Test
	public void valuesContainsThreeTypes() {
		LoaderActivityType[] values = LoaderActivityType.values();
		assertNotNull(values);
		assertEquals(3, values.length);
	}

	@Test
	public void valueOfCreateAll() {
		assertEquals(LoaderActivityType.CREATE_ALL, LoaderActivityType.valueOf("CREATE_ALL"));
	}

	@Test
	public void valueOfCreateFind() {
		assertEquals(LoaderActivityType.CREATE_FIND, LoaderActivityType.valueOf("CREATE_FIND"));
	}

	@Test
	public void valueOfFind() {
		assertEquals(LoaderActivityType.FIND, LoaderActivityType.valueOf("FIND"));
	}
}
