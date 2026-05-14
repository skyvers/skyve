package org.skyve.domain.app.admin;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class CommunicationActionTypeTest {

	@Test
	@SuppressWarnings("static-method")
	public void saveForBulkSendToCode() {
		assertThat(Communication.ActionType.saveForBulkSend.toCode(), is("save"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void sendImmediatelyToCode() {
		assertThat(Communication.ActionType.sendImmediately.toCode(), is("send"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testBindingsToCode() {
		assertThat(Communication.ActionType.testBindingsAndOutput.toCode(), is("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDomainValueIsNotNull() {
		assertNotNull(Communication.ActionType.saveForBulkSend.toDomainValue());
		assertThat(Communication.ActionType.saveForBulkSend.toDomainValue().getCode(), is("save"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromCodeSave() {
		assertThat(Communication.ActionType.fromCode("save"), is(Communication.ActionType.saveForBulkSend));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromCodeSend() {
		assertThat(Communication.ActionType.fromCode("send"), is(Communication.ActionType.sendImmediately));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromCodeMissingReturnsNull() {
		assertNull(Communication.ActionType.fromCode("unknown"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDomainValuesNotEmpty() {
		assertNotNull(Communication.ActionType.toDomainValues());
		assertEquals(3, Communication.ActionType.toDomainValues().size());
	}
}
