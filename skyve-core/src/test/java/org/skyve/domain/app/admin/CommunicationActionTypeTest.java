package org.skyve.domain.app.admin;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class CommunicationActionTypeTest {

	@Test
	@SuppressWarnings("static-method")
	void saveForBulkSendToCode() {
		assertThat(Communication.ActionType.saveForBulkSend.toCode(), is("save"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sendImmediatelyToCode() {
		assertThat(Communication.ActionType.sendImmediately.toCode(), is("send"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testBindingsToCode() {
		assertThat(Communication.ActionType.testBindingsAndOutput.toCode(), is("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toDomainValueIsNotNull() {
		assertNotNull(Communication.ActionType.saveForBulkSend.toDomainValue());
		assertThat(Communication.ActionType.saveForBulkSend.toDomainValue().getCode(), is("save"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromCodeSave() {
		assertThat(Communication.ActionType.fromCode("save"), is(Communication.ActionType.saveForBulkSend));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromCodeSend() {
		assertThat(Communication.ActionType.fromCode("send"), is(Communication.ActionType.sendImmediately));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromCodeMissingReturnsNull() {
		assertNull(Communication.ActionType.fromCode("unknown"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toDomainValuesNotEmpty() {
		assertNotNull(Communication.ActionType.toDomainValues());
		assertEquals(3, Communication.ActionType.toDomainValues().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void saveForBulkSendToLocalisedDescriptionReturnsNonNull() {
		assertNotNull(Communication.ActionType.saveForBulkSend.toLocalisedDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromLocalisedDescriptionSaveForBulkSendFindsValue() {
		String desc = Communication.ActionType.saveForBulkSend.toLocalisedDescription();
		assertThat(Communication.ActionType.fromLocalisedDescription(desc), is(Communication.ActionType.saveForBulkSend));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(Communication.ActionType.fromLocalisedDescription("no such description"));
	}
}
