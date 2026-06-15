package org.skyve.impl.archive.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
public class ArchiveUtilsTest {

	// ---- ARCHIVE_CHARSET ----

	@Test
	public void archiveCharsetIsUtf8() {
		assertEquals("UTF-8", ArchiveUtils.ARCHIVE_CHARSET.name());
	}

	// ---- ARCHIVE_FILE_SUFFIX ----

	@Test
	public void archiveFileSuffixIsCorrect() {
		assertEquals(".archive", ArchiveUtils.ARCHIVE_FILE_SUFFIX);
	}

	// ---- excerptLine ----

	@Test
	public void excerptLineNullReturnsNullString() {
		// null input → String.valueOf(null) → "null"
		assertEquals("null", ArchiveUtils.excerptLine(null));
	}

	@Test
	public void excerptLineShortStringReturnsOriginal() {
		String input = "short line";
		assertEquals(input, ArchiveUtils.excerptLine(input));
	}

	@Test
	public void excerptLineExactBoundaryReturnsOriginal() {
		// boundary: 2 * 100 + 20 = 220 chars — NOT long enough, so returned as-is
		String input = "a".repeat(220);
		assertEquals(input, ArchiveUtils.excerptLine(input));
	}

	@Test
	public void excerptLineLongStringProducesExcerpt() {
		// 221 chars should trigger the excerpt path
		String input = "A".repeat(100) + "B".repeat(121);
		String result = ArchiveUtils.excerptLine(input);
		// Should contain the separator
		assertTrue("excerpt should contain separator", result.contains(" … "));
	}

	@Test
	public void excerptLinePreservesStartAndEnd() {
		// 300-char string: first 100 chars are 'S', last 100 chars are 'E'
		String start = "S".repeat(100);
		String middle = "M".repeat(100);
		String end = "E".repeat(100);
		String input = start + middle + end;
		String result = ArchiveUtils.excerptLine(input);
		assertTrue("result should start with S's", result.startsWith(start));
		assertTrue("result should end with E's", result.endsWith(end));
	}

	@Test
	public void excerptLineEmptyStringReturnsEmpty() {
		assertEquals("", ArchiveUtils.excerptLine(""));
	}

        @Test
        public void archiveUtilsDefaultConstructor() {
                // covers class initialiser line 13
                ArchiveUtils au = new ArchiveUtils();
                assertNotNull(au);
        }

	@Test
	public void getDocumentResolvesFromCurrentUserCustomer() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Audit")).thenReturn(document);

		withThreadLocalPersistence(persistence, () -> {
			assertSame(document, ArchiveUtils.getDocument("admin", "Audit"));
		});
	}

	private interface ThrowingRunnable {
		void run() throws Exception;
	}

	private static void withThreadLocalPersistence(AbstractPersistence persistence, ThrowingRunnable runnable)
	throws Exception {
		ThreadLocal<AbstractPersistence> threadLocal = getThreadLocalPersistence();
		AbstractPersistence original = threadLocal.get();
		try {
			threadLocal.set(persistence);
			runnable.run();
		}
		finally {
			if (original == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(original);
			}
		}
	}

	@SuppressWarnings("unchecked")
	private static ThreadLocal<AbstractPersistence> getThreadLocalPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		return (ThreadLocal<AbstractPersistence>) field.get(null);
	}
}
