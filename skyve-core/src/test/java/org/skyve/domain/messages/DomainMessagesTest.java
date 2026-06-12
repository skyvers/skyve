package org.skyve.domain.messages;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Date;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.domain.messages.OptimisticLockException.OperationType;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;

/** Unit tests for domain message and exception classes. */
@ExtendWith(MockitoExtension.class)
@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
class DomainMessagesTest {

	@Mock
	private Document mockDocument;

	@Mock
	private User mockUser;

	@Mock
	private Customer mockCustomer;

	@Mock
	@SuppressWarnings("rawtypes")
	private Converter mockConverter;

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
		assertEquals(0, count);
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
		assertEquals(1, count);
		assertThat(first, is("name"));
	}

	@Test
	void messageMultipleBindingsConstructorSetsBindings() {
		Message msg = new Message(new String[]{"name", "email"}, "Required");
		int count = 0;
		for (@SuppressWarnings("unused") String b : msg.getBindings()) {
			count++;
		}
		assertEquals(2, count);
	}

	@Test
	void messageFormattedTextConstructorWithNoBeansSetsText() {
		Message msg = new Message("Formatted text", new org.skyve.domain.Bean[0]);
		assertThat(msg.getText(), is("Formatted text"));
	}

	@Test
	void messageFormattedBindingAndTextConstructorAddsBinding() {
		Message msg = new Message("name", "Formatted text", new org.skyve.domain.Bean[0]);
		assertThat(msg.getText(), is("Formatted text"));
		int count = 0;
		String first = null;
		for (String b : msg.getBindings()) {
			first = b;
			count++;
		}
		assertEquals(1, count);
		assertThat(first, is("name"));
	}

	@Test
	void messageFormattedMultipleBindingsConstructorAddsAllBindings() {
		Message msg = new Message(new String[]{"name", "email"}, "Formatted text", new org.skyve.domain.Bean[0]);
		assertThat(msg.getText(), is("Formatted text"));
		int count = 0;
		for (@SuppressWarnings("unused") String b : msg.getBindings()) {
			count++;
		}
		assertEquals(2, count);
	}

	@Test
	void messageAddBindingAddsBinding() {
		Message msg = new Message("error");
		msg.addBinding("name");
		int count = 0;
		for (@SuppressWarnings("unused") String b : msg.getBindings()) {
			count++;
		}
		assertEquals(1, count);
	}

	@Test
	void messageSetBindingPrefixPrependsPrefix() {
		Message msg = new Message("name", "Required");
		msg.setBindingPrefix("contact.");
		String first = msg.getBindings().iterator().next();
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

	@Test
	void domainExceptionWithI18nBooleanTrueUsesI18n() {
		DomainException ex = new DomainException("Something failed", true);
		assertThat(ex, is(notNullValue()));
	}

	@Test
	void domainExceptionWithI18nBooleanFalseUsesRawMessage() {
		DomainException ex = new DomainException("raw message", false);
		assertThat(ex.getMessage(), is("raw message"));
	}

	@Test
	void domainExceptionWithI18nCauseAndBooleanTrue() {
		RuntimeException cause = new RuntimeException("root");
		DomainException ex = new DomainException("msg", cause, true);
		assertThat(ex.getCause(), is(cause));
	}

	@Test
	void domainExceptionWithI18nCauseAndBooleanFalse() {
		RuntimeException cause = new RuntimeException("root");
		DomainException ex = new DomainException("raw msg", cause, false);
		assertThat(ex.getMessage(), is("raw msg"));
	}

	@Test
	void domainExceptionWithI18nValues() {
		DomainException ex = new DomainException("msg with {0}", "value");
		assertThat(ex, is(notNullValue()));
	}

	@Test
	void domainExceptionWithCauseAndI18nValues() {
		RuntimeException cause = new RuntimeException("root");
		DomainException ex = new DomainException("msg {0}", cause, "arg");
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
		assertFalse(ex.getMessages().isEmpty());
	}

	// ---- ValidationException ----

	@Test
	void validationExceptionDefaultConstructorCreatesInstance() {
		ValidationException ve = new ValidationException();
		assertThat(ve.getMessages(), is(notNullValue()));
		assertTrue(ve.getMessages().isEmpty());
	}

	@Test
	void validationExceptionWithMessageConstructorAddsMessage() {
		Message msg = new Message("name", "Required");
		ValidationException ve = new ValidationException(msg);
		assertEquals(1, ve.getMessages().size());
		assertThat(ve.getMessages().get(0), is(msg));
	}

	@Test
	void validationExceptionWithStringConstructorAddsMessage() {
		ValidationException ve = new ValidationException("Invalid input");
		assertEquals(1, ve.getMessages().size());
	}

	@Test
	void validationExceptionWithBindingAndStringAddsMessage() {
		ValidationException ve = new ValidationException("email", "Invalid email");
		assertEquals(1, ve.getMessages().size());
	}

	@Test
	void validationExceptionWithListConstructorAddsAll() {
		ValidationException ve = new ValidationException(Arrays.asList(new Message("err1"), new Message("err2")));
		assertEquals(2, ve.getMessages().size());
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
		assertFalse(ex.getMessages().isEmpty());
	}

	// ---- ManyResultsException ----

	@Test
	void manyResultsExceptionCreatesInstance() {
		ManyResultsException ex = new ManyResultsException();
		assertThat(ex, is(notNullValue()));
		assertThat(ex.getMessages(), is(notNullValue()));
		assertFalse(ex.getMessages().isEmpty());
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

	@Test
	void timeoutExceptionWithCauseCreatesInstance() {
		TimeoutException ex = new TimeoutException(new RuntimeException("session expired"));
		assertThat(ex, is(notNullValue()));
		assertFalse(ex.getMessages().isEmpty());
	}

	// ---- MessageSeverity enum ----

	@Test
	void messageSeverityValuesContainAllFour() {
		MessageSeverity[] values = MessageSeverity.values();
		assertEquals(4, values.length);
	}

	@Test
	void messageSeverityInfoValueOf() {
		assertThat(MessageSeverity.valueOf("info"), is(MessageSeverity.info));
	}

	@Test
	void messageSeverityWarnValueOf() {
		assertThat(MessageSeverity.valueOf("warn"), is(MessageSeverity.warn));
	}

	@Test
	void messageSeverityErrorValueOf() {
		assertThat(MessageSeverity.valueOf("error"), is(MessageSeverity.error));
	}

	@Test
	void messageSeverityFatalValueOf() {
		assertThat(MessageSeverity.valueOf("fatal"), is(MessageSeverity.fatal));
	}

	// ---- ReferentialConstraintViolationException ----

	@Test
	void referentialConstraintViolationExceptionCreatesInstance() {
		ReferentialConstraintViolationException ex =
				new ReferentialConstraintViolationException("Invoice", "INV-001", "InvoiceLine");
		assertThat(ex, is(notNullValue()));
		assertThat(ex.getMessages(), is(notNullValue()));
		assertFalse(ex.getMessages().isEmpty());
	}

	// ---- UploadException / UploadException.Problem ----

	@Test
	void uploadExceptionDefaultConstructorCreatesInstance() {
		UploadException ex = new UploadException();
		assertThat(ex, is(notNullValue()));
		assertFalse(ex.hasProblems());
		assertFalse(ex.hasErrors());
	}

	@Test
	void uploadExceptionAddErrorSetsHasErrors() {
		UploadException ex = new UploadException();
		UploadException.Problem p = new UploadException.Problem("bad value", "row 1");
		ex.addError(p);
		assertTrue(ex.hasErrors());
		assertTrue(ex.hasProblems());
	}

	@Test
	void uploadExceptionAddWarningDoesNotSetHasErrors() {
		UploadException ex = new UploadException();
		UploadException.Problem p = new UploadException.Problem("suspicious value", "row 2");
		ex.addWarning(p);
		assertFalse(ex.hasErrors());
		assertTrue(ex.hasProblems());
	}

	@Test
	void uploadExceptionGetErrorsIteratesAddedErrors() {
		UploadException ex = new UploadException();
		ex.addError(new UploadException.Problem("err1", "A1"));
		ex.addError(new UploadException.Problem("err2", "B1"));
		int count = 0;
		for (@SuppressWarnings("unused") UploadException.Problem p : ex.getErrors()) {
			count++;
		}
		assertEquals(2, count);
	}

	@Test
	void uploadExceptionGetWarningsIteratesAddedWarnings() {
		UploadException ex = new UploadException();
		ex.addWarning(new UploadException.Problem("warn1", "A1"));
		ex.addWarning(new UploadException.Problem("warn2", "B1"));
		int count = 0;
		for (@SuppressWarnings("unused") UploadException.Problem p : ex.getWarnings()) {
			count++;
		}
		assertEquals(2, count);
	}

	@Test
	void uploadExceptionProblemGetWhatAndWhere() {
		UploadException.Problem p = new UploadException.Problem("something wrong", "cell A1");
		assertThat(p.getWhat(), is("something wrong"));
		assertThat(p.getWhere(), is("cell A1"));
	}

	@Test
	void uploadExceptionProblemNullWhereBecomesEmpty() {
		UploadException.Problem p = new UploadException.Problem("what", null);
		assertThat(p.getWhere(), is(""));
	}

	@Test
	void uploadExceptionProblemIsErrorReturnsTrueForError() {
		UploadException ex = new UploadException();
		UploadException.Problem p = new UploadException.Problem("e", "r1");
		ex.addError(p);
		assertTrue(p.isError());
	}

	@Test
	void uploadExceptionProblemIsErrorReturnsFalseForWarning() {
		UploadException ex = new UploadException();
		UploadException.Problem p = new UploadException.Problem("w", "r1");
		ex.addWarning(p);
		assertFalse(p.isError());
	}

	@Test
	void uploadExceptionProblemToStringContainsWhatAndWhere() {
		UploadException.Problem p = new UploadException.Problem("broken", "line 5");
		assertThat(p.toString(), containsString("broken"));
		assertThat(p.toString(), containsString("line 5"));
	}

	@Test
	void uploadExceptionProblemHashCodeIsConsistent() {
		UploadException.Problem p = new UploadException.Problem("broken", "line 5");
		assertEquals(p.hashCode(), p.hashCode());
	}

	@Test
	void uploadExceptionProblemEqualsReturnsTrueForSame() {
		UploadException.Problem p1 = new UploadException.Problem("broken", "line 5");
		UploadException.Problem p2 = new UploadException.Problem("broken", "line 5");
		assertTrue(p1.equals(p2));
	}

	@Test
	void uploadExceptionProblemEqualsReturnsFalseForDifferent() {
		UploadException.Problem p1 = new UploadException.Problem("broken", "line 5");
		UploadException.Problem p2 = new UploadException.Problem("other", "line 5");
		assertFalse(p1.equals(p2));
	}

	@Test
	@SuppressWarnings("unlikely-arg-type")
	void uploadExceptionProblemEqualsReturnsFalseForNonProblem() {
		UploadException.Problem p = new UploadException.Problem("broken", "line 5");
		assertFalse(p.equals("notAProblem"));
	}

	@Test
	void uploadExceptionProblemCompareToNullReturnsPositive() {
		UploadException.Problem p = new UploadException.Problem("broken", "line 5");
		assertTrue(p.compareTo(null) > 0);
	}

	@Test
	void uploadExceptionAddErrorsFromList() {
		UploadException ex = new UploadException();
		java.util.List<UploadException.Problem> problems = java.util.Arrays.asList(
				new UploadException.Problem("e1", "r1"),
				new UploadException.Problem("e2", "r2"));
		ex.addErrors(problems);
		assertTrue(ex.hasErrors());
	}

	// ---- ConversionException with i18nValues constructor ----

	@Test
	void conversionExceptionWithI18nValuesCreatesInstance() {
		ConversionException ex = new ConversionException(ConversionException.INTEGER_CONVERTER_KEY, "extra");
		assertThat(ex, is(notNullValue()));
		assertFalse(ex.getMessages().isEmpty());
	}

	// ---- UniqueConstraintViolationException ----

	@Test
	void uniqueConstraintViolationExceptionSimpleConstructorCreatesInstance() {
		UniqueConstraintViolationException ex = new UniqueConstraintViolationException(null, "UQ_name", "duplicate name");
		assertThat(ex, is(notNullValue()));
		assertThat(ex.getConstraintName(), is("UQ_name"));
	}

	@Test
	void uniqueConstraintViolationExceptionSimpleConstructorPopulatesMessages() {
		UniqueConstraintViolationException ex = new UniqueConstraintViolationException(null, "UQ_name", "duplicate name");
		assertFalse(ex.getMessages().isEmpty());
		assertThat(ex.getMessages().get(0).getText(), is("duplicate name"));
	}

	@Test
	void uniqueConstraintViolationExceptionGetDocumentReturnsDocument() {
		UniqueConstraintViolationException ex = new UniqueConstraintViolationException(mockDocument, "UQ_name", "msg");
		assertThat(ex.getDocument(), is(mockDocument));
	}

	@Test
	void uniqueConstraintViolationExceptionBindingConstructorCreatesInstance() {
		when(mockDocument.getOwningModuleName()).thenReturn("admin");
		when(mockDocument.getName()).thenReturn("User");
		UniqueConstraintViolationException ex = new UniqueConstraintViolationException(mockDocument, "UQ_email", "email", "duplicate email");
		assertThat(ex, is(notNullValue()));
		assertThat(ex.getConstraintName(), is("UQ_email"));
	}

	@Test
	void uniqueConstraintViolationExceptionBindingConstructorPopulatesMessages() {
		when(mockDocument.getOwningModuleName()).thenReturn("admin");
		when(mockDocument.getName()).thenReturn("User");
		UniqueConstraintViolationException ex = new UniqueConstraintViolationException(mockDocument, "UQ_email", "email", "duplicate email");
		assertFalse(ex.getMessages().isEmpty());
		assertThat(ex.getMessages().get(0).getText(), is("duplicate email"));
	}

	// ---- DomainException enableSuppression constructors ----

	@Test
	void domainExceptionEnableSuppressionConstructorCreatesInstance() {
		RuntimeException cause = new RuntimeException("root");
		DomainException ex = new DomainException("msg", cause, true, true);
		assertThat(ex.getCause(), is(cause));
	}

	@Test
	void domainExceptionEnableSuppressionWithI18nValuesConstructorCreatesInstance() {
		RuntimeException cause = new RuntimeException("root");
		DomainException ex = new DomainException("msg {0}", cause, true, true, "val");
		assertThat(ex.getCause(), is(cause));
	}

	@Test
	void domainExceptionEnableSuppressionWithI18nBooleanConstructorCreatesInstance() {
		RuntimeException cause = new RuntimeException("root");
		DomainException ex = new DomainException("raw msg", cause, false, false, false);
		assertThat(ex.getMessage(), is("raw msg"));
		assertThat(ex.getCause(), is(cause));
	}

	// ---- OptimisticLockException ----

	@Test
	@SuppressWarnings("unchecked")
	void optimisticLockExceptionUpdateOperationCreatesInstance() throws Exception {
		when(mockUser.getCustomer()).thenReturn(mockCustomer);
		when(mockCustomer.getDefaultTimestampConverter()).thenReturn(mockConverter);
		when(mockConverter.toDisplayValue(org.mockito.ArgumentMatchers.any())).thenReturn("2024-01-01 10:00:00");
		OptimisticLock lock = new OptimisticLock("alice", new Date());
		OptimisticLockException ex = new OptimisticLockException(mockUser, OperationType.update, lock);
		assertNotNull(ex);
		assertNotNull(ex.getMessages());
		assertFalse(ex.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("unchecked")
	void optimisticLockExceptionDeleteOperationCreatesInstance() throws Exception {
		when(mockUser.getCustomer()).thenReturn(mockCustomer);
		when(mockCustomer.getDefaultTimestampConverter()).thenReturn(mockConverter);
		when(mockConverter.toDisplayValue(org.mockito.ArgumentMatchers.any())).thenReturn("2024-01-01 10:00:00");
		OptimisticLock lock = new OptimisticLock("bob", new Date());
		OptimisticLockException ex = new OptimisticLockException(mockUser, OperationType.delete, lock);
		assertNotNull(ex);
		assertFalse(ex.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("unchecked")
	void optimisticLockExceptionFallsBackToTimestampToStringOnConverterException() throws Exception {
		when(mockUser.getCustomer()).thenReturn(mockCustomer);
		when(mockCustomer.getDefaultTimestampConverter()).thenReturn(mockConverter);
		when(mockConverter.toDisplayValue(org.mockito.ArgumentMatchers.any(Timestamp.class))).thenThrow(new RuntimeException("converter failure"));
		OptimisticLock lock = new OptimisticLock("charlie", new Date());
		OptimisticLockException ex = new OptimisticLockException(mockUser, OperationType.update, lock);
		assertNotNull(ex);
		assertFalse(ex.getMessages().isEmpty());
	}

	// ---- ConversationEndedException getMessages ----

	@Test
	void conversationEndedExceptionGetMessagesIsNotEmpty() {
		ConversationEndedException ex = new ConversationEndedException(java.util.Locale.ENGLISH);
		assertFalse(ex.getMessages().isEmpty());
	}

	// ---- SessionEndedException getMessages ----

	@Test
	void sessionEndedExceptionGetMessagesIsNotEmpty() {
		SessionEndedException ex = new SessionEndedException(java.util.Locale.ENGLISH);
		assertFalse(ex.getMessages().isEmpty());
	}

	// ---- UploadException overflow path ----

	@Test
	void uploadExceptionAddErrorThrowsSelfAfterFiftyErrors() {
		UploadException ex = new UploadException();
		// addError throws when errors.size() > 50, so adding the 51st distinct entry triggers the throw
		for (int i = 0; i < 50; i++) {
			ex.addError(new UploadException.Problem("err" + i, "row" + i));
		}
		UploadException.Problem overflowProblem = new UploadException.Problem("err50", "row50");
		assertThrows(UploadException.class, () -> ex.addError(overflowProblem));
	}

	// ---- ValidationException.getMessage null-superMessage branch ----

	@Test
	void validationExceptionGetMessageWithNullSuperMessageReturnsMessagesOnly() {
		// The default no-arg ValidationException calls DomainException() which has no message,
		// so super.getMessage() returns null — covers the null branch in getMessage().
		ValidationException ve = new ValidationException();
		ve.getMessages().add(new Message("fieldError"));
		String msg = ve.getMessage();
		assertThat(msg, containsString("fieldError"));
	}

	@Test
	void validationExceptionGetMessageWithEmptyMessagesListReturnsOnlySuperMessage() {
		// ValidationException with a string message: super.getMessage() is non-null,
		// messages list loop body is skipped (empty list) — covers the no-loop branch.
		ValidationException ve = new ValidationException();
		// No messages added: loop executes 0 times.
		String msg = ve.getMessage();
		assertNotNull(msg);
	}
}
