package org.skyve.impl.web;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.SortDirection;

public class SortParameterImplTest {

	@Test
	@SuppressWarnings("static-method")
	public void defaultsAreNull() {
		SortParameterImpl sp = new SortParameterImpl();
		assertNull(sp.getBy());
		assertNull(sp.getDirection());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBy() {
		SortParameterImpl sp = new SortParameterImpl();
		sp.setBy("name");
		assertThat(sp.getBy(), is("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setDirection() {
		SortParameterImpl sp = new SortParameterImpl();
		sp.setDirection(SortDirection.ascending);
		assertThat(sp.getDirection(), is(SortDirection.ascending));
	}

	@Test
	@SuppressWarnings("static-method")
	public void populateFromStringAscending() {
		SortParameterImpl sp = new SortParameterImpl();
		sp.populateFromString("name ascending");
		assertThat(sp.getBy(), is("name"));
		assertThat(sp.getDirection(), is(SortDirection.ascending));
	}

	@Test
	@SuppressWarnings("static-method")
	public void populateFromStringDescending() {
		SortParameterImpl sp = new SortParameterImpl();
		sp.populateFromString("bizId descending");
		assertThat(sp.getBy(), is("bizId"));
		assertThat(sp.getDirection(), is(SortDirection.descending));
	}

	@Test
	@SuppressWarnings("static-method")
	public void populateFromStringNoSpaceThrows() {
		SortParameterImpl sp = new SortParameterImpl();
		assertThrows(IllegalStateException.class, () -> sp.populateFromString("nospace"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toStringIncludesByAndDirection() {
		SortParameterImpl sp = new SortParameterImpl();
		sp.setBy("createdDatetime");
		sp.setDirection(SortDirection.descending);
		assertThat(sp.toString(), is("createdDatetime descending"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setDirectionNull() {
		SortParameterImpl sp = new SortParameterImpl();
		sp.setDirection(SortDirection.ascending);
		sp.setDirection(null);
		assertNull(sp.getDirection());
	}

	@Test
	@SuppressWarnings("static-method")
	public void populateFromStringBindingWithDot() {
		SortParameterImpl sp = new SortParameterImpl();
		sp.populateFromString("contact.name ascending");
		assertThat(sp.getBy(), is("contact.name"));
		assertThat(sp.getDirection(), is(SortDirection.ascending));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setByNullRoundtrip() {
		SortParameterImpl sp = new SortParameterImpl();
		sp.setBy("col");
		sp.setBy(null);
		assertNull(sp.getBy());
	}

	@Test
	@SuppressWarnings("static-method")
	public void directionEnumValues() {
		assertEquals(2, SortDirection.values().length);
	}
}
