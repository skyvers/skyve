package org.skyve.domain.app.admin;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class CommunicationFormatTypeTest {

	@Test
	@SuppressWarnings("static-method")
	public void emailToCode() {
		assertThat(Communication.FormatType.email.toCode(), is("email"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void emailToDomainValue() {
		assertNotNull(Communication.FormatType.email.toDomainValue());
		assertThat(Communication.FormatType.email.toDomainValue().getCode(), is("email"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromCodeEmail() {
		assertThat(Communication.FormatType.fromCode("email"), is(Communication.FormatType.email));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromCodeMissing() {
		assertNull(Communication.FormatType.fromCode("unknown"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDomainValues() {
		assertNotNull(Communication.FormatType.toDomainValues());
	}

	@Test
	@SuppressWarnings("static-method")
	public void emailToLocalisedDescriptionReturnsNonNull() {
		assertNotNull(Communication.FormatType.email.toLocalisedDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromLocalisedDescriptionEmailFindsEmail() {
		String desc = Communication.FormatType.email.toLocalisedDescription();
		assertThat(Communication.FormatType.fromLocalisedDescription(desc), is(Communication.FormatType.email));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(Communication.FormatType.fromLocalisedDescription("no such description"));
	}
}
