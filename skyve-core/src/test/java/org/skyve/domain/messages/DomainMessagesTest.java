package org.skyve.domain.messages;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

/** Unit tests for domain message and exception classes. */
@SuppressWarnings("static-method")
class DomainMessagesTest {

	// ---- Message ----

	@Test
	void messageTextConstructorSetsText() {
		Message msg = new Message("Something went wrong");
		assertThat(msg.getText(), is("Something went wrong"));
	}

	@Test
	void messageTextConstructorHasNoBindings() {
		Message msg = new Message("error");
		int count = 0;
		for (@SuppressWarnings("unused") String b : msg.getBindings()) {
			count++;
		}
		assertThat(count, is(0));
	}

	@Test
	void messageBindingAndTextConstructorSetsBinding() {
		Message msg = new Message("name", "Name is required");
		assertThat(msg.getText(), is("Name is required"));
		int count = 0;
		String first = null;
		for (String b : msg.getBindings()) {
			first = b;
			count++;
		}
		assertThat(count, is(1));
		assertThat(first, is("name"));
	}

	@Test
	void messageMultipleBindingsConstructorSetsBindings() {
		Message msg = new Message(new String[]{"name", "email"}, "Required");
		int count = 0;
		for (@SuppressWarnings("unused") String b : msg.getBindings()) {
			count++;
		}
		assertThat(count, is(2));
	}

	@Test
	void messageAddBindingAddsBinding() {
		Message msg = new Message("error");
		msg.addBinding("name");
		int count = 0;
		for (@SuppressWarnings("unused") String b : msg.getBindings()) {
			count++;
		}
		assertThat(count, is(1));
	}

	@Test
	void messageSetBindingPrefixPrependsPrefix() {
		Message msg = new Message("name", "Required");
		msg.setBindingPrefix("contact.");
		String first = null;
		for (String b : msg.getBindings()) {
			first = b;
			break;
		}
		assertThat(first, is("contact.name"));
	}

	@Test
	void messageToStringContainsText() {
		Message msg = new Message("name", "Required field");
		String str = msg.toString();
		assertThat(str, containsString("Required field"));
		assertThat(str, containsString("name"));
	}

	// ---- DomainException ----

	@Test
	void domainExceptionWithMessageCreatesException() {
		DomainException ex = new DomainException("Something failed");
		assertThat(ex.getMessage(), containsString("Something failed"));
	}

	@Test
	void domainExceptionWithCauseWrapsThrowable() {
		RuntimeException cause = new RuntimeException("cause");
		DomainException ex = new DomainException(cause);
		assertThat(ex.getCause(), is(cause));
	}

	@Test
	void domainExceptionWithMessageAndCauseSetssBoth() {
		RuntimeException cause = new RuntimeException("root");
		DomainException ex = new DomainException("wrapped", cause);
		assertThat(ex.getCause(), is(cause));
	}

	// ---- ConversionException ----

	@Test
	void conversionExceptionWithKeyCreatesInstance() {
		ConversionException ex = new ConversionException(ConversionException.INTEGER_CONVERTER_KEY, new RuntimeException("parse error"));
		assertThat(ex, is(notNullValue()));
		assertThat(ex.getMessages(), is(notNullValue()));
	}

	@Test
	void conversionExceptionMessageListIsNotEmpty() {
		ConversionException ex = new ConversionException(ConversionException.INTEGER_CONVERTER_KEY, new RuntimeException("x"));
		assertThat(ex.getMessages().isEmpty(), is(false));
	}

	// ---- ValidationException ----

	@Test
	void validationExceptionDefaultConstructorCreatesInstance() {
		ValidationException ve = new ValidationException();
		assertThat(ve.getMessages(), is(notNullValue()));
		assertThat(ve.getMessages().isEmpty(), is(true));
	}

	@Test
	void validationExceptionWithMessageConstructorAddsMessage() {
		Message msg = new Message("name", "Required");
		ValidationException ve = new ValidationException(msg);
		assertThat(ve.getMessages().size(), is(1));
		assertThat(ve.getMessages().get(0), is(msg));
	}

	@Test
	void validationExceptionWithStringConstructorAddsMessage() {
		ValidationException ve = new ValidationException("Invalid input");
		assertThat(ve.getMessages().size(), is(1));
	}

	@Test
	void validationExceptionWithBindingAndStringAddsMessage() {
		ValidationException ve = new ValidationException("email", "Invalid email");
		assertThat(ve.getMessages().size(), is(1));
	}

	@Test
	void validationExceptionWithListConstructorAddsAll() {
		ValidationException ve = new ValidationException(Arrays.asList(new Message("err1"), new Message("err2")));
		assertThat(ve.getMessages().size(), is(2));
	}

	@Test
	void validationExceptionGetMessageIncludesMessageText() {
		ValidationException ve = new ValidationException("Validation failed");
		assertThat(ve.getMessage(), containsString("Validation failed"));
	}

	// ---- NoResultsException ----

	@Test
	void noResultsExceptionCreatesInstance() {
		NoResultsException ex = new NoResultsException();
		assertThat(ex, is(notNullValue()));
		assertThat(ex.getMessages(), is(notNullValue()));
		assertThat(ex.getMessages().isEmpty(), is(false));
	}

	// ---- ManyResultsException ----

	@Test
	void manyResultsExceptionCreatesInstance() {
		ManyResultsException ex = new ManyResultsException();
		assertThat(ex, is(notNullValue()));
		assertThat(ex.getMessages(), is(notNullValue()));
		assertThat(ex.getMessages().isEmpty(), is(false));
	}

	// ---- SessionEndedException ----

	@Test
	void sessionEndedExceptionCreatesInstance() {
		SessionEndedException ex = new SessionEndedException(java.util.Locale.ENGLISH);
		assertThat(ex, is(notNullValue()));
	}

	// ---- ConversationEndedException ----

	@Test
	void conversationEndedExceptionCreatesInstance() {
		ConversationEndedException ex = new ConversationEndedException(java.util.Locale.ENGLISH);
		assertThat(ex, is(notNullValue()));
	}

	// ---- TimeoutException ----

	@Test
	void timeoutExceptionCreatesInstance() {
		TimeoutException ex = new TimeoutException();
		assertThat(ex, is(notNullValue()));
	}
}
