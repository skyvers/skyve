package org.skyve.impl.metadata.view.component;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class ComponentTranslationTest {

	@Test
	@SuppressWarnings("static-method")
	void setFromComponentRoundtrip() {
		ComponentTranslation map = new ComponentTranslation();
		map.setFromComponent("myComponent");
		assertThat(map.getFromComponent(), is("myComponent"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setMappedToRoundtrip() {
		ComponentTranslation map = new ComponentTranslation();
		map.setMappedTo("targetComponent");
		assertThat(map.getMappedTo(), is("targetComponent"));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultsAreNull() {
		ComponentTranslation map = new ComponentTranslation();
		assertNull(map.getFromComponent());
		assertNull(map.getMappedTo());
	}

	@Test
	@SuppressWarnings("static-method")
	void bothSidesAreTrimmed() {
		ComponentTranslation translation = new ComponentTranslation();
		translation.setFromComponent("  {el:bean.flag}\t");
		translation.setMappedTo("\n canEdit ");
		assertThat(translation.getFromComponent(), is("{el:bean.flag}"));
		assertThat(translation.getMappedTo(), is("canEdit"));
	}

	@Test
	@SuppressWarnings("static-method")
	void aBlankSideIsNull() {
		ComponentTranslation translation = new ComponentTranslation();
		for (String blank : new String[] {"", " ", " \t\n", null}) {
			translation.setFromComponent("from");
			translation.setMappedTo("to");
			translation.setFromComponent(blank);
			translation.setMappedTo(blank);
			assertNull(translation.getFromComponent());
			assertNull(translation.getMappedTo());
		}
	}
}
