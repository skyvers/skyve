package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.criterion;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.group;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.range;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.snapshot.CompoundFilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;

import modules.kitchensink.domain.ListAttributes;
import util.AbstractH2Test;

/**
 * A negation in a list grid's criteria matches every row that does not match the positive condition, as SmartClient's
 * own evaluation of criteria and InMemoryFilter do: so a row whose column has no value is included under notEqual,
 * the not-contains family, notInSet (a variant domain's description) and every criterion under Match None, plain and
 * nested, for number, date, text and variant domain columns. Each case is checked against the database (a document
 * query) and an in-memory filter.
 */
class SmartClientListServletNullsH2Test extends AbstractH2Test {
	private static final String TEXT = ListAttributes.textPropertyName;
	private static final String NUMBER = ListAttributes.normalIntegerPropertyName;
	private static final String DATE = ListAttributes.datePropertyName;
	private static final String DATE_TIME = ListAttributes.dateTimePropertyName;
	private static final String MEMO = ListAttributes.memoPropertyName;
	private static final String VARIANT = ListAttributes.variantDomainPropertyName;
	private static final String QUERY = "qListGridFixtureVueAttributes";
	private static final Set<String> ALL = Set.of("NV a", "NV b", "NV c", "NV n");

	private Module module;
	private Document document;
	private List<Bean> rows;

	/**
	 * "NV a" (1, 06-May-2026, apple, one), "NV b" (5, 15-Jun-2026, banana, two), "NV c" (10, 25-Jun-2026, cherry, three)
	 * and "NV n", with no number, date, memo or variant.
	 */
	@BeforeEach
	void makeRows() {
		Customer customer = CORE.getCustomer();
		module = customer.getModule(ListAttributes.MODULE_NAME);
		document = module.getDocument(customer, ListAttributes.DOCUMENT_NAME);
		Persistence persistence = CORE.getPersistence();
		rows = new ArrayList<>(4);
		rows.add(persistence.save(row("NV a", Integer.valueOf(1), LocalDate.of(2026, Month.MAY, 6), "apple", "one")));
		rows.add(persistence.save(row("NV b", Integer.valueOf(5), LocalDate.of(2026, Month.JUNE, 15), "banana", "two")));
		rows.add(persistence.save(row("NV c", Integer.valueOf(10), LocalDate.of(2026, Month.JUNE, 25), "cherry", "three")));
		rows.add(persistence.save(row("NV n", null, null, null, null)));
		persistence.flush();
	}

	@Test
	void notEqualIncludesNoValue() throws Exception {
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.and, criterion(NUMBER, "notEqual", "5"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.and, criterion(DATE, "notEqual", "2026-06-15"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.and, criterion(MEMO, "iNotEqual", "banana"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.and, criterion(VARIANT, "notEqual", "Two"));
	}

	@Test
	void theNotContainsFamilyIncludesNoValue() throws Exception {
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.and, criterion(MEMO, "notContains", "nan"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.and, criterion(MEMO, "iNotContains", "nan"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.and, criterion(MEMO, "iNotStartsWith", "ban"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.and, criterion(MEMO, "iNotEndsWith", "nana"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.and, criterion(VARIANT, "iNotContains", "Two"));
	}

	@Test
	void matchNoneIncludesNoValue() throws Exception {
		assertRows(Set.of("NV a", "NV b", "NV n"), CompoundFilterOperator.not, criterion(NUMBER, "greaterThan", "5"));
		assertRows(Set.of("NV b", "NV c", "NV n"), CompoundFilterOperator.not, criterion(NUMBER, "lessThan", "5"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.not, criterion(NUMBER, "equals", "5"));
		assertRows(Set.of("NV c", "NV n"), CompoundFilterOperator.not, range(NUMBER, "betweenInclusive", "1", "5"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.not, range(NUMBER, "between", "1", "10"));
		assertRows(Set.of("NV a", "NV n"), CompoundFilterOperator.not, criterion(DATE, "greaterOrEqual", "2026-06-15"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.not, criterion(MEMO, "iContains", "nan"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.not, criterion(VARIANT, "iContains", "Two"));
		assertRows(Set.of("NV a", "NV c", "NV n"), CompoundFilterOperator.not, criterion(VARIANT, "inSet", null, "two"));
	}

	/** Match None of a negation is the positive condition, which a row with no value does not meet. */
	@Test
	void matchNoneOfANegationExcludesNoValue() throws Exception {
		assertRows(Set.of("NV b"), CompoundFilterOperator.not, criterion(NUMBER, "notEqual", "5"));
		assertRows(Set.of("NV b"), CompoundFilterOperator.not, criterion(MEMO, "iNotContains", "nan"));
	}

	/** Match All on [contains NV] and [not: [or: greaterThan 5, variant One]]: not (c or a), so b and n. */
	@Test
	void aNestedNegationIncludesNoValue() throws Exception {
		assertRows(Set.of("NV b", "NV n"),
					CompoundFilterOperator.and,
					criterion(TEXT, "iContains", "NV"),
					group("not", group("or", criterion(NUMBER, "greaterThan", "5"), criterion(VARIANT, "iContains", "One"))));
	}

	/** Match None on [and: lessOrEqual 5, iContains an]: not b (banana), so a, c and n. */
	@Test
	void aNegatedGroupIncludesNoValue() throws Exception {
		assertRows(Set.of("NV a", "NV c", "NV n"),
					CompoundFilterOperator.not,
					group("and", criterion(NUMBER, "lessOrEqual", "5"), criterion(MEMO, "iContains", "an")));
	}

	/** Match Any on [contains NV a] or [notEqual 5]: a, c and n. */
	@Test
	void aNegationUnderMatchAnyIncludesNoValue() throws Exception {
		assertRows(Set.of("NV a", "NV c", "NV n"),
					CompoundFilterOperator.or,
					criterion(TEXT, "iContains", "NV a"),
					criterion(NUMBER, "notEqual", "5"));
	}

	/**
	 * A date-only column's notEqual as SmartClient and the Vue list grid send it (BizUtil.dayCriteria, ListGrid.vue
	 * dayCriteria): before the day, from the next day, or no value.
	 */
	@Test
	void aDaysNotEqualIncludesNoValue() throws Exception {
		LocalDateTime start = LocalDateTime.of(2026, Month.JUNE, 15, 0, 0);
		assertRows(Set.of("NV a", "NV c", "NV n"),
					CompoundFilterOperator.and,
					group("or",
							criterion(DATE_TIME, "lessThan", iso(start)),
							criterion(DATE_TIME, "greaterOrEqual", iso(start.plusDays(1))),
							criterion(DATE_TIME, "isNull", null)));
	}

	/** The positive conditions still leave out a row with no value. */
	@Test
	void positiveConditionsExcludeNoValue() throws Exception {
		assertRows(Set.of("NV c"), CompoundFilterOperator.and, criterion(NUMBER, "greaterThan", "5"));
		assertRows(Set.of("NV b"), CompoundFilterOperator.and, criterion(MEMO, "iContains", "nan"));
		assertRows(ALL, CompoundFilterOperator.and, criterion(TEXT, "iStartsWith", "NV"));
	}

	@SafeVarargs
	private void assertRows(Set<String> expected, CompoundFilterOperator operator, Map<String, Object>... criteria) throws Exception {
		List<Map<String, Object>> list = List.of(criteria);
		assertEquals(expected, ListServletCriteria.fetch(module, document, QUERY, TEXT, operator, list), "database " + operator + " " + list);
		assertEquals(expected, ListServletCriteria.filterInMemory(module, document, rows, TEXT, operator, list), "in memory " + operator + " " + list);
	}

	private static ListAttributes row(String text, Integer number, LocalDate date, String memo, String variant) {
		ListAttributes result = ListAttributes.newInstance();
		result.setText(text);
		result.setNormalInteger(number);
		result.setDate((date == null) ? null : new DateOnly(date));
		result.setDateTime((date == null) ? null : new DateTime(date.atTime(14, 47).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
		result.setMemo(memo);
		result.setVariantDomain(variant);
		return result;
	}

	/** An ISO 8601 instant in UTC, as JSON writes a Date. */
	private static String iso(LocalDateTime local) {
		return local.atZone(ZoneId.systemDefault()).toInstant().toString();
	}
}
