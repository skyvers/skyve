package org.skyve.addin;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;
import org.mockito.Mockito;
import org.pf4j.PluginWrapper;

@SuppressWarnings("static-method")
public class AddInTest {

	@Test
	public void testConstructorWithMockedWrapper() {
		PluginWrapper wrapper = Mockito.mock(PluginWrapper.class);
		AddIn addIn = new AddIn(wrapper) {
			// anonymous concrete subclass for testing
		};
		assertThat(addIn, notNullValue());
	}
}
