package org.skyve.impl.domain.number;

import java.math.BigInteger;
import java.util.Formatter;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.DocumentNumber;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.number.NumberGenerator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Base implementation of {@link NumberGenerator} that reads and increments sequence
 * counters stored in the {@code DocumentNumber} admin document.
 *
 * <p>The counter for a {@code (moduleName, documentName, fieldName)} triple is
 * persisted in the database, in a row created the first time a number is taken. The new
 * row starts from the highest number the numbered document's {@code fieldName} column
 * already holds (across the customer, whatever the user's scope), so that generated numbers
 * never collide with existing data; when {@code fieldName} is not an integer, long integer
 * or text attribute of the numbered document, or no row holds a number, it starts from 0.
 *
 * <p>Threading: thread-safe, and safe across a cluster of servers sharing one database,
 * under the preconditions below. Instances hold no state; mutual exclusion is in the
 * database, through the {@link Persistence} supplied to {@link #getNextNumber}. An existing
 * row is read with a pessimistic write lock ({@link Persistence#retrieveAndLock}:
 * {@code SELECT ... FOR UPDATE} or the dialect's equivalent), so concurrent transactions
 * increment it one at a time, each from the number the previous one committed, and each
 * holds the lock until its transaction ends.
 *
 * <p>Two transactions that both find no row both try to create it. The {@code DocumentNumber}
 * document's {@code uniqueSequence} constraint makes the second wait until the first ends;
 * if the first committed, the second's check finds the first's row and fails with a
 * {@link UniqueConstraintViolationException} raised in Java, with no database error, which
 * is caught: the second then locks and increments the row the first created. A violation
 * signalled by a database failure (one with a cause, such as a timeout, deadlock or
 * serialisation failure, after which the transaction may be rolled back or aborted) is not
 * retried but thrown.
 *
 * <p>Preconditions: the connection's isolation level is READ COMMITTED, as in the MySQL and
 * PostgreSQL datasource templates Skyve ships and the default for H2, PostgreSQL, Oracle and
 * SQL Server. At a snapshot isolation level (a hand-written MySQL datasource at InnoDB's
 * default REPEATABLE READ, or the opt-in {@code SQLServer2016HibernateContentPersistence},
 * which sets SNAPSHOT) the constraint check reads the transaction's snapshot and misses the
 * other's row, so concurrent first uses of a sequence can silently create duplicate rows and
 * return duplicate numbers. Every wait on a lock is bounded by the database's lock timeout
 * and by Skyve's OLTP query timeout; a wait that times out fails the call.
 *
 * <p>Subclasses must implement {@link NumberGenerator#next} using the
 * {@link #getNextNumber} helper, supplying the appropriate
 * {@link org.skyve.persistence.Persistence} instance.
 *
 * @see DocumentNumberGenerator
 * @see NumberGeneratorStaticSingleton
 */
public abstract class AbstractDocumentNumberGenerator implements NumberGenerator {
	private static final String NUMERIC_PATTERN = "^\\d+$";
	private static final String NO_NUMBER = "0";
	private static final String MAX_ALIAS = "MaxNumber";
	private static final String VALUE_ALIAS = "Value";

	/**
	 * Takes the next number of a sequence: increments its {@code DocumentNumber} row,
	 * creating the row on first use, and returns the new value.
	 *
	 * <p>Precondition: {@code pers} has an active transaction.
	 *
	 * <p>Side effects: inserts or updates the sequence's {@code DocumentNumber} row through
	 * {@code pers}, so the number is committed or rolled back with {@code pers}'s
	 * transaction, and evicts the row from {@code pers}'s first-level cache.
	 *
	 * <p>Threading: concurrent calls, in any threads or servers sharing the database, return
	 * distinct numbers; see the class comment. The call blocks while another transaction
	 * holds the sequence's row lock, or is creating its row, until that transaction ends,
	 * and from then on holds that lock itself until {@code pers}'s transaction ends.
	 *
	 * @param pers the persistence whose transaction takes the number
	 * @param prefix the prefix of the sequence's values, eg {@code "INV"} for {@code INV0001},
	 *        or {@code null} for none
	 * @param moduleName the module of the numbered document
	 * @param documentName the numbered document
	 * @param fieldName the field or sequence name
	 * @param numberLength the minimum length of the value, prefix included, zero-padded after the prefix
	 * @return the next number of the sequence
	 * @throws UniqueConstraintViolationException if creating the sequence's row conflicts with
	 *         another transaction's row a second time, as when the row is deleted while in use,
	 *         or if the conflict was signalled by a database failure (its cause), which is not retried
	 */
	protected @Nonnull String getNextNumber(@Nonnull Persistence pers,
											@Nullable String prefix,
											@Nonnull String moduleName,
											@Nonnull String documentName,
											@Nonnull String fieldName,
											int numberLength) {
		return getNextNumber(pers, prefix, moduleName, documentName, fieldName, numberLength, true);
	}

	/**
	 * Takes the next number of a sequence as {@link #getNextNumber(Persistence, String, String, String, String, int)}
	 * does, the second attempt finding the row another transaction created first.
	 *
	 * @param retryCreation whether a creation conflict raised in Java (one with no cause) is
	 *        followed by a second attempt ({@code true} on the first attempt) or thrown
	 *        ({@code false} on the second)
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private @Nonnull String getNextNumber(@Nonnull Persistence pers,
											@Nullable String prefix,
											@Nonnull String moduleName,
											@Nonnull String documentName,
											@Nonnull String fieldName,
											int numberLength,
											boolean retryCreation) {
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document document = module.getDocument(customer, AppConstants.DOCUMENT_NUMBER_DOCUMENT_NAME);
		String nextNumber = "0";
		String lastNumber = "0";

		DocumentNumber dN = null;
		try {
			// temporarily escalate access to the Document Number sequences
			@SuppressWarnings("null") // always returns a list
			@Nonnull List<DocumentNumber> num = pers.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
				DocumentQuery qN = pers.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.DOCUMENT_NUMBER_DOCUMENT_NAME);
				qN.getFilter().addEquals(AppConstants.MODULE_NAME_ATTRIBUTE_NAME, moduleName);
				qN.getFilter().addEquals(AppConstants.DOCUMENT_NAME_ATTRIBUTE_NAME, documentName);
				qN.getFilter().addEquals(AppConstants.SEQUENCE_NAME_ATTRIBUTE_NAME, fieldName);
				return qN.beanResults();
			});

			if (num.isEmpty()) {
				// first hit - start from the highest number the field already holds
				lastNumber = highestExistingNumber(pers, customer, prefix, moduleName, documentName, fieldName);

				// create a new document number record
				try {
					dN = document.newInstance(user);
					dN.setModuleName(moduleName);
					dN.setDocumentName(documentName);
					dN.setSequenceName(fieldName);
				}
				catch (Exception e) {
					throw new DomainException("Could not instantiate a new document number record", e);
				}
			}
			else {
				dN = num.get(0);
				dN = pers.retrieveAndLock(document, dN.getBizId()); // issue a row-level lock
				lastNumber = dN.getDocumentNumber();
			}
			// just update from the document Number
			nextNumber = incrementAlpha(prefix, lastNumber, numberLength);
			dN.setDocumentNumber(nextNumber);

			try {
				pers.preMerge(document, dN);
			}
			catch (UniqueConstraintViolationException e) {
				// A new row fails the check when another transaction created this sequence's row after
				// the query above found none. The check waited for that transaction to commit, so the
				// row can now be read and locked: take the number from it. A violation with a cause
				// is a database failure (timeout, deadlock, serialisation) that may have rolled back or
				// aborted this transaction: retrying would lose that work silently, so it is thrown.
				if (retryCreation && (e.getCause() == null)) {
					return getNextNumber(pers, prefix, moduleName, documentName, fieldName, numberLength, false);
				}
				throw e;
			}
			pers.upsertBeanTuple(dN);
			pers.postMerge(document, dN);
		}
		finally {
			if (dN != null) {
				pers.evictCached(dN);
			}
		}

		return nextNumber;
	}

	/**
	 * Returns the highest number the numbered document's {@code fieldName} column already holds,
	 * the value a new sequence row starts from, so that its numbers never collide with existing data.
	 *
	 * <p>An integer or long integer field gives its maximum value, whatever {@code prefix} is: with a
	 * numeric prefix the increment then keeps the maximum's digits after the prefix's length, so
	 * prefix {@code 2026} over a column maximum of {@code 20250099} gives {@code 2026100}, padded to
	 * the minimum length ({@code 20260100} at length 8). A text field gives the stored
	 * value with the highest number after {@code prefix} (all values when {@code prefix} is
	 * {@code null}), compared as a number of any length, so {@code INV10} beats {@code INV9} and
	 * {@code INV0009}; values whose remainder after the prefix is not all digits are ignored.
	 * Any other field type, a sequence name that is not a persistent attribute of the numbered
	 * document, or an unknown module or document gives {@code "0"}.
	 *
	 * <p>Side effects: reads the numbered document's rows with customer scope, so the user's
	 * data group or user scope does not hide other rows of the customer.
	 *
	 * <p>Complexity: one aggregate query for a numeric field; for a text field, one pass in Java
	 * over the values that start with {@code prefix}, O(n) time in the number of those rows, and
	 * O(1) memory where the driver streams the result (MySQL with {@code useCursorFetch=true}, as
	 * Skyve's datasource template sets; PostgreSQL inside a transaction), otherwise O(n). It runs once
	 * in a sequence's life, inside the first save that takes a number from it, which waits for the
	 * scan. The text
	 * comparison is in Java because no portable SQL takes the numeric value of a suffix across
	 * the supported databases, and the lexicographic maximum is wrong for unpadded or mixed values.
	 *
	 * @param pers the persistence whose transaction takes the number
	 * @param customer the user's customer
	 * @param prefix the prefix of the sequence's values, or {@code null} for none
	 * @param moduleName the module of the numbered document
	 * @param documentName the numbered document
	 * @param fieldName the field or sequence name
	 * @return the highest existing value, as stored, or {@code "0"} if there is none
	 */
	private static @Nonnull String highestExistingNumber(@Nonnull Persistence pers,
															@Nonnull Customer customer,
															@Nullable String prefix,
															@Nonnull String moduleName,
															@Nonnull String documentName,
															@Nonnull String fieldName) {
		// the repository throws for a module the customer does not have, and for a document the module does
		// not have, but a sequence's module and document names may be any keys: look them up without throwing
		Module numberedModule = null;
		for (Module candidate : customer.getModules()) {
			if (candidate.getName().equals(moduleName)) {
				numberedModule = candidate;
				break;
			}
		}
		Document numbered = ((numberedModule == null) || (! numberedModule.getDocumentRefs().containsKey(documentName))) ?
								null :
								numberedModule.getDocument(customer, documentName);
		if ((numbered == null) || (! numbered.isPersistable())) {
			return NO_NUMBER;
		}
		Attribute attribute = numbered.getPolymorphicAttribute(customer, fieldName);
		if ((attribute == null) || (! attribute.isPersistent())) {
			return NO_NUMBER;
		}
		AttributeType type = attribute.getAttributeType();
		String result = pers.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			if ((type == AttributeType.integer) || (type == AttributeType.longInteger)) {
				DocumentQuery query = p.newDocumentQuery(moduleName, documentName);
				query.addAggregateProjection(AggregateFunction.Max, fieldName, MAX_ALIAS);
				List<Bean> beans = query.projectedResults();
				Object max = beans.isEmpty() ? null : Binder.get(beans.get(0), MAX_ALIAS);
				return (max == null) ? null : max.toString();
			}
			if (type == AttributeType.text) {
				return highestText(p, prefix, moduleName, documentName, fieldName);
			}
			return null;
		});
		return (result == null) ? NO_NUMBER : result;
	}

	/**
	 * Returns the stored text value with the highest all-digit number after {@code prefix}, or
	 * {@code null} if no value qualifies; see {@link #highestExistingNumber}.
	 */
	private static @Nullable String highestText(@Nonnull Persistence p,
												@Nullable String prefix,
												@Nonnull String moduleName,
												@Nonnull String documentName,
												@Nonnull String fieldName) {
		DocumentQuery query = p.newDocumentQuery(moduleName, documentName);
		query.addBoundProjection(fieldName, VALUE_ALIAS);
		if (prefix == null) {
			query.getFilter().addNotNull(fieldName);
		}
		else {
			// a coarse filter only: the prefix is matched exactly below, so LIKE wildcards in it do no harm
			query.getFilter().addLike(fieldName, prefix + '%');
		}
		int prefixLength = (prefix == null) ? 0 : prefix.length();
		String result = null;
		String resultDigits = null; // the significant digits of result's number
		try (AutoClosingIterable<Bean> values = query.projectedIterable()) {
			for (Bean row : values) {
				@SuppressWarnings("null") // not null: the filter above leaves out null values
				@Nonnull String text = (String) Binder.get(row, VALUE_ALIAS);
				String digits = significantDigits(text, prefix, prefixLength);
				if ((digits != null) &&
						((resultDigits == null) ||
							(digits.length() > resultDigits.length()) ||
							((digits.length() == resultDigits.length()) && (digits.compareTo(resultDigits) > 0)))) {
					result = text;
					resultDigits = digits;
				}
			}
		}
		catch (Exception e) {
			throw new DomainException("Could not read the existing values of " + moduleName + '.' + documentName + '.' + fieldName, e);
		}
		return result;
	}

	/**
	 * Returns the digits after {@code prefix} in {@code value} without leading zeros ({@code ""} for
	 * zero), or {@code null} if {@code value} does not start with {@code prefix} followed by one or
	 * more digits only.
	 */
	private static @Nullable String significantDigits(@Nonnull String value, @Nullable String prefix, int prefixLength) {
		int length = value.length();
		if ((length == prefixLength) || ((prefix != null) && (! value.startsWith(prefix)))) {
			return null;
		}
		int firstSignificant = -1;
		for (int i = prefixLength; i < length; i++) {
			char c = value.charAt(i);
			if ((c < '0') || (c > '9')) {
				return null;
			}
			if ((firstSignificant < 0) && (c != '0')) {
				firstSignificant = i;
			}
		}
		return (firstSignificant < 0) ? "" : value.substring(firstSignificant);
	}

	/**
	 * Returns the next alpha value - ie A00A1 becomes A00A2 etc
	 *
	 * <p>The digits are incremented as a {@link BigInteger}, so a run of digits of any length works.
	 *
	 * @param suppliedPrefix
	 *        - if the sequence value has a known prefix before the number,
	 *        eg INV0001 has a prefix of "INV"
	 * @param lastNumber
	 *        - the number to increment
	 * @param numberLength
	 *        - the minimum length of the number when specified as a string
	 *
	 * @return - the next number
	 */
	protected static @Nonnull String incrementAlpha(@Nullable String suppliedPrefix, @Nullable String lastNumber, int numberLength) {
		String newNumber = "";
		String nonNumeric = lastNumber;
		BigInteger value = BigInteger.ONE;
		String prefix;
		if (suppliedPrefix != null) {
			prefix = suppliedPrefix;
		} else {
			prefix = "";
		}

		if (lastNumber != null) {
			String[] parts = (new StringBuilder(" ").append(lastNumber)).toString().split("\\D\\d+$");

			// cater for alpha prefix
			if (parts.length > 0 && parts[0].length() < lastNumber.length()) {
				String numberPart = lastNumber.substring(parts[0].length(), lastNumber.length());
				nonNumeric = lastNumber.substring(0, parts[0].length());

				value = new BigInteger(numberPart).add(BigInteger.ONE);

				// cater for purely numeric prefix
			} else if (prefix.matches(NUMERIC_PATTERN) && lastNumber.matches(NUMERIC_PATTERN) && !"0".equals(lastNumber)) {
				int len = prefix.length();
				value = new BigInteger(lastNumber.substring(len)).add(BigInteger.ONE);
				nonNumeric = prefix;

				// cater for numeric only
			} else if (lastNumber.matches(NUMERIC_PATTERN)) {
				nonNumeric = prefix;
				value = new BigInteger(lastNumber).add(BigInteger.ONE);
			}
		} else {
			nonNumeric = prefix;
		}

		// now put prefix and value together
		int newLength = (nonNumeric.length() + value.toString().length() > numberLength
				? nonNumeric.length() + value.toString().length()
				: numberLength);

		StringBuilder sb = new StringBuilder(newLength + 1);
		try (Formatter f = new Formatter(sb)) {
			newNumber = nonNumeric
					+ f.format(new StringBuilder("%1$").append(newLength - nonNumeric.length()).append("s").toString(),
							value.toString()).toString().replace(" ", "0");
		}

		return newNumber;
	}
}
