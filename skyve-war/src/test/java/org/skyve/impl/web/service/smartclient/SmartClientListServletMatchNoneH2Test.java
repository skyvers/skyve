package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.criterion;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.group;

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
import org.skyve.domain.types.DateTime;
import org.skyve.impl.snapshot.CompoundFilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;

import modules.kitchensink.domain.ListAttributes;
import util.AbstractH2Test;

/**
 * A list grid's "Match None" (not) criteria where a criterion is itself a group, as SmartClient sends a day filter on a
 * date time column shown as a date (from the day's start to the next day's) and its advanced filter a nested clause:
 * the group is negated as a whole, at any depth, under not, and, and or.
 */
class SmartClientListServletMatchNoneH2Test extends AbstractH2Test {
	private static final String TEXT = ListAttributes.textPropertyName;
	private static final String DATE_TIME = ListAttributes.dateTimePropertyName;

	private Module module;
	private Document document;
	private List<Bean> rows;

	/**
	 * Three list attributes: "MN a" at 06-May-2026 14:47, "MN b" at 15-Jun-2021 12:30 and "MN c" at 25-Jun-2021 09:00.
	 */
	@BeforeEach
	void makeRows() {
		Customer customer = CORE.getCustomer();
		module = customer.getModule(ListAttributes.MODULE_NAME);
		document = module.getDocument(customer, ListAttributes.DOCUMENT_NAME);
		Persistence persistence = CORE.getPersistence();
		rows = new ArrayList<>(3);
		rows.add(persistence.save(row("MN a", LocalDateTime.of(2026, Month.MAY, 6, 14, 47))));
		rows.add(persistence.save(row("MN b", LocalDateTime.of(2021, Month.JUNE, 15, 12, 30))));
		rows.add(persistence.save(row("MN c", LocalDateTime.of(2021, Month.JUNE, 25, 9, 0))));
		persistence.flush();
	}

	/** Match None on [the day 06-May-2026]: every row but a. */
	@Test
	void aDayGroupUnderMatchNoneIsNegated() throws Exception {
		assertEquals(Set.of("MN b", "MN c"), fetch(CompoundFilterOperator.not, List.of(day(2026, 5, 6))));
	}

	/** Match None on [the day 06-May-2026] and [text contains "MN b"]: neither, so c. */
	@Test
	void aDayGroupAndALeafUnderMatchNoneAreEachNegated() throws Exception {
		assertEquals(Set.of("MN c"), fetch(CompoundFilterOperator.not, List.of(day(2026, 5, 6), criterion(TEXT, "iContains", "MN b"))));
	}

	/** Match None on [or: a's day, b's text]: c. */
	@Test
	void anOrGroupUnderMatchNoneIsNegated() throws Exception {
		assertEquals(Set.of("MN c"), fetch(CompoundFilterOperator.not, List.of(group("or", day(2026, 5, 6), criterion(TEXT, "iContains", "MN b")))));
	}

	/** Match None on [not: text contains "MN c"]: not (not c) is c. */
	@Test
	void aNotGroupUnderMatchNoneIsNegated() throws Exception {
		assertEquals(Set.of("MN c"), fetch(CompoundFilterOperator.not, List.of(group("not", criterion(TEXT, "iContains", "MN c")))));
	}

	/** Match None on [and: text contains "MN", [or: a's day, b's day]]: not ("MN" and (a or b)) is c. */
	@Test
	void aGroupWithAGroupUnderMatchNoneIsNegatedAsAWhole() throws Exception {
		assertEquals(Set.of("MN c"),
						fetch(CompoundFilterOperator.not,
								List.of(group("and", criterion(TEXT, "iContains", "MN"), group("or", day(2026, 5, 6), day(2021, 6, 15))))));
	}

	/** Match All on [text contains "MN"] and [not: a's day]: b and c. */
	@Test
	void aNotGroupUnderMatchAllIsNegated() throws Exception {
		assertEquals(Set.of("MN b", "MN c"),
						fetch(CompoundFilterOperator.and, List.of(criterion(TEXT, "iContains", "MN"), group("not", day(2026, 5, 6)))));
	}

	/** Match Any on [text contains "MN a"] or [not: [or: a's day, b's day]]: a, or neither a's nor b's day (c). */
	@Test
	void aNotGroupOfAGroupUnderMatchAnyIsNegated() throws Exception {
		assertEquals(Set.of("MN a", "MN c"),
						fetch(CompoundFilterOperator.or,
								List.of(criterion(TEXT, "iContains", "MN a"), group("not", group("or", day(2026, 5, 6), day(2021, 6, 15))))));
	}

	/** An in-memory list model's filter: Match None on [the day 06-May-2026] is every row but a. */
	@Test
	void anInMemoryFilterNegatesADayGroupUnderMatchNone() throws Exception {
		assertEquals(Set.of("MN b", "MN c"), ListServletCriteria.filterInMemory(module, document, rows, TEXT, CompoundFilterOperator.not, List.of(day(2026, 5, 6))));
	}

	private Set<String> fetch(CompoundFilterOperator operator, List<Map<String, Object>> criteria) throws Exception {
		return ListServletCriteria.fetch(module, document, "qListGridFixtureVueAttributes", TEXT, operator, criteria);
	}


	private static ListAttributes row(String text, LocalDateTime dateTime) {
		ListAttributes result = ListAttributes.newInstance();
		result.setText(text);
		result.setDateTime(new DateTime(dateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
		return result;
	}

	/** A day filter on the date time column: from the day's start to the next day's (see BizUtil.dayCriteria). */
	private static Map<String, Object> day(int year, int month, int day) {
		LocalDateTime start = LocalDateTime.of(year, month, day, 0, 0);
		return group("and", criterion(DATE_TIME, "greaterOrEqual", iso(start)), criterion(DATE_TIME, "lessThan", iso(start.plusDays(1))));
	}

	/** An ISO 8601 instant in UTC, as JSON writes a Date. */
	private static String iso(LocalDateTime local) {
		return local.atZone(ZoneId.systemDefault()).toInstant().toString();
	}
}
