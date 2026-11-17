package org.skyve.impl.domain.number;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.number.NumberGenerator;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import modules.test.domain.AllAttributesPersistent;
import util.AbstractH2TestTruncate;

/**
 * Proves that the first number of a sequence follows the highest number the numbered document's
 * field already holds, for both generators.
 */
class DocumentNumberBootstrapH2Test extends AbstractH2TestTruncate {
	private static final String MODULE = AllAttributesPersistent.MODULE_NAME;
	private static final String DOCUMENT = AllAttributesPersistent.DOCUMENT_NAME;

	/**
	 * A user whose default scope on every document is its own rows only.
	 */
	private static final class UserScopedUser extends SuperUser {
		private static final long serialVersionUID = 1L;

		@Override
		public @Nonnull DocumentPermissionScope getScope(@Nonnull String moduleName, @Nonnull String documentName) {
			return DocumentPermissionScope.user;
		}
	}

	private AbstractPersistence p;
	private User u;
	private Module m;
	private Document aapd;

	@BeforeEach
	void setUpDocument() {
		p = AbstractPersistence.get();
		u = p.getUser();
		Customer c = u.getCustomer();
		m = c.getModule(MODULE);
		aapd = m.getDocument(c, DOCUMENT);
	}

	@Test
	void anIntegerFieldStartsAfterItsHighestValue() throws Exception {
		save(Integer.valueOf(3), null, null);
		save(Integer.valueOf(41), null, null);
		save(Integer.valueOf(7), null, null);
		save(null, null, null);
		assertFirstNumbers("42", null, AllAttributesPersistent.normalIntegerPropertyName);
	}

	@Test
	void aLongIntegerFieldStartsAfterItsHighestValue() throws Exception {
		save(null, Long.valueOf(70000L), null);
		save(null, Long.valueOf(12L), null);
		assertFirstNumbers("70001", null, AllAttributesPersistent.longIntegerPropertyName);
	}

	@Test
	void aTextFieldStartsAfterItsHighestNumberAfterThePrefix() throws Exception {
		save(null, null, "INV9");
		save(null, null, "INV10"); // more digits than 9
		save(null, null, "INV0008"); // fewer significant digits than 10
		save(null, null, "INV11"); // as many digits as 10, and higher
		save(null, null, "INV0010"); // the same number as 10
		save(null, null, "INV000"); // zero
		save(null, null, "INVX99"); // not a number after the prefix
		save(null, null, "INV12A"); // not all digits
		save(null, null, "INV1-2"); // not all digits
		save(null, null, "X99"); // another prefix
		save(null, null, "INV"); // no digits
		assertFirstNumbers("INV12", "INV", AllAttributesPersistent.textPropertyName);
	}

	@Test
	void aZeroPaddedTextFieldKeepsItsPadding() throws Exception {
		save(null, null, "INV0009");
		save(null, null, "INV0010");
		assertFirstNumbers("INV0011", "INV", AllAttributesPersistent.textPropertyName, 7); // the minimum length includes the prefix
	}

	@Test
	void aTextFieldWithNoPrefixComparesWholeValuesAsNumbers() throws Exception {
		save(null, null, "9");
		save(null, null, "0011");
		save(null, null, "10");
		save(null, null, "A100");
		assertFirstNumbers("12", null, AllAttributesPersistent.textPropertyName);
	}

	@Test
	void noRowsStartsAtOne() {
		assertFirstNumbers("1", null, AllAttributesPersistent.normalIntegerPropertyName);
	}

	@Test
	void noRowWithANumberStartsAtOne() throws Exception {
		save(null, null, "INVX");
		assertFirstNumbers("INV1", "INV", AllAttributesPersistent.textPropertyName);
	}

	@Test
	void aSequenceThatIsNotAFieldStartsAtOne() throws Exception {
		save(Integer.valueOf(41), null, "41");
		assertFirstNumbers("1", null, "notAField");
	}

	@Test
	void aSequenceOfAnUnknownDocumentStartsAtOne() {
		List<Executable> checks = new ArrayList<>(2);
		for (NumberGenerator generator : generators()) {
			String actual = generator.next(MODULE, "NoSuchDocument", "number");
			checks.add(() -> assertEquals("1", actual, generator.getClass().getSimpleName()));
			forgetSequences();
		}
		assertAll(checks);
	}

	@Test
	void aScopedUserStartsAfterRowsItCannotSee() throws Exception {
		save(Integer.valueOf(41), null, null); // saved by the test user: hidden from a user-scoped user
		User testUser = u;
		SuperUser scoped = new UserScopedUser();
		scoped.setCustomerName(testUser.getCustomerName());
		scoped.setName("ScopedUser");
		scoped.setId("ScopedUser");
		p.setUser(scoped);
		try {
			assertFirstNumbers("42", null, AllAttributesPersistent.normalIntegerPropertyName);
		}
		finally {
			p.setUser(testUser);
		}
	}

	private void assertFirstNumbers(@Nonnull String expected, @Nullable String prefix, @Nonnull String fieldName) {
		assertFirstNumbers(expected, prefix, fieldName, 0);
	}

	/**
	 * Takes the first number of the sequence from each generator in turn, forgetting the sequence
	 * in between, and asserts each is {@code expected}.
	 */
	private void assertFirstNumbers(@Nonnull String expected, @Nullable String prefix, @Nonnull String fieldName, int minimumLength) {
		List<Executable> checks = new ArrayList<>(2);
		for (NumberGenerator generator : generators()) {
			String actual = generator.next(prefix, MODULE, DOCUMENT, fieldName, minimumLength);
			checks.add(() -> assertEquals(expected, actual, generator.getClass().getSimpleName()));
			forgetSequences();
		}
		assertAll(checks);
	}

	private static List<NumberGenerator> generators() {
		return List.of(new DocumentNumberGenerator(), new DocumentNumberAutonomousTransactionGenerator());
	}

	/**
	 * Deletes every sequence row and commits, so the next generator takes a first number again.
	 */
	private void forgetSequences() {
		p.newSQL("delete from ADM_DocumentNumber").execute();
		p.commit(false);
		p.begin();
	}

	/**
	 * Saves and commits a row with these values, so that both generators' transactions see it.
	 */
	private void save(@Nullable Integer normalInteger, @Nullable Long longInteger, @Nullable String text) throws Exception {
		AllAttributesPersistent row = Util.constructRandomInstance(u, m, aapd, 1);
		row.setNormalInteger(normalInteger);
		row.setLongInteger(longInteger);
		if (text != null) {
			row.setText(text);
		}
		else {
			// a text value that holds no number, so the text tests are not disturbed
			row.setText("random");
		}
		p.save(row);
		p.commit(false);
		p.begin();
	}
}
