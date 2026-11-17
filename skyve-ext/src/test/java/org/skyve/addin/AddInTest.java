package org.skyve.addin;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.pf4j.PluginWrapper;

@SuppressWarnings("static-method")
class AddInTest {

	@Test
	void testConstructorWithMockedWrapper() {
		PluginWrapper wrapper = mock(PluginWrapper.class);
		AddIn addIn = new AddIn(wrapper) {
			// anonymous concrete subclass for testing
		};
		assertThat(addIn, notNullValue());
	}
}
