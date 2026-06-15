package org.skyve.impl.metadata.view.component;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class ComponentNameMapTest {

	@Test
	@SuppressWarnings("static-method")
	void setFromComponentRoundtrip() {
		ComponentNameMap map = new ComponentNameMap();
		map.setFromComponent("myComponent");
		assertThat(map.getFromComponent(), is("myComponent"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setMappedToRoundtrip() {
		ComponentNameMap map = new ComponentNameMap();
		map.setMappedTo("targetComponent");
		assertThat(map.getMappedTo(), is("targetComponent"));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultsAreNull() {
		ComponentNameMap map = new ComponentNameMap();
		assertNull(map.getFromComponent());
		assertNull(map.getMappedTo());
	}
}
