package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.snapshot.CompoundFilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.InMemoryFilter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.Persistence;

import modules.kitchensink.domain.ListAttributes;
import util.AbstractH2Test;

/**
 * A list grid's "Match Any" (or) criteria where a criterion is itself a group, as the Vue list grid sends a day filter
 * on a date time column (from the day's start to the next day's) and SmartClient's advanced filter sends a nested
 * clause: each criterion is ORed with the others, whichever comes first.
 */
class SmartClientListServletMatchAnyH2Test extends AbstractH2Test {
	private static final String TEXT = ListAttributes.textPropertyName;
	private static final String DATE_TIME = ListAttributes.dateTimePropertyName;

	private Customer customer;
	private Module module;
	private Document document;
	private List<Bean> rows;

	/**
	 * Three list attributes: "MA a" at 06-May-2026 14:47, "MA b" at 15-Jun-2021 12:30 and "MA c" at 25-Jun-2021 09:00.
	 */
	@BeforeEach
	void makeRows() throws Exception {
		customer = CORE.getCustomer();
		module = customer.getModule(ListAttributes.MODULE_NAME);
		document = module.getDocument(customer, ListAttributes.DOCUMENT_NAME);
		Persistence persistence = CORE.getPersistence();
		rows = new ArrayList<>(3);
		rows.add(persistence.save(row("MA a", LocalDateTime.of(2026, 5, 6, 14, 47))));
		rows.add(persistence.save(row("MA b", LocalDateTime.of(2021, 6, 15, 12, 30))));
		rows.add(persistence.save(row("MA c", LocalDateTime.of(2021, 6, 25, 9, 0))));
		persistence.flush();
	}

	/**
	 * Match Any on [the day 06-May-2026] then [text contains "MA b"]: a's day or b's text.
	 */
	@Test
	void aDayFilterFirstIsOredWithTheNextCriterion() throws Exception {
		assertEquals(Set.of("MA a", "MA b"), fetch(List.of(day(2026, 5, 6), criterion(TEXT, "iContains", "MA b"))));
	}

	/**
	 * Match Any on one column, [the day 15-Jun-2021] then [from 01-Jan-2026]: b's day or a.
	 */
	@Test
	void aDayFilterFirstIsOredWithALaterDate() throws Exception {
		assertEquals(Set.of("MA a", "MA b"), fetch(List.of(day(2021, 6, 15), criterion(DATE_TIME, "greaterOrEqual", start(2026, 1, 1)))));
	}

	/**
	 * Match Any after a simple criterion is unchanged: [text contains "MA c"] then [the day 06-May-2026].
	 */
	@Test
	void aDayFilterAfterASimpleCriterionIsOred() throws Exception {
		assertEquals(Set.of("MA a", "MA c"), fetch(List.of(criterion(TEXT, "iContains", "MA c"), day(2026, 5, 6))));
	}

	/**
	 * An in-memory list model's filter: a group first under Match Any is ORed with the next criterion, not with
	 * everything (an empty in-memory filter's predicate is always true).
	 */
	@Test
	void anInMemoryFilterOredWithAGroupFirstIsNotEverything() throws Exception {
		InMemoryFilter filter = new InMemoryFilter();
		ListModel<Bean> model = mock(ListModel.class);
		when(model.getFilter()).thenReturn(filter);
		when(model.newFilter()).thenAnswer(invocation -> new InMemoryFilter());
		SmartClientListServlet.addAdvancedFilterCriteriaToQuery(module,
																	document,
																	CORE.getUser(),
																	CompoundFilterOperator.or,
																	List.of(day(2026, 5, 6), criterion(TEXT, "iContains", "MA b")),
																	null,
																	model);
		List<Bean> filtered = new ArrayList<>(rows);
		filter.filter(filtered);
		assertEquals(Set.of("MA a", "MA b"), texts(filtered));
	}

	private Set<String> fetch(List<Map<String, Object>> criteria) throws Exception {
		DocumentQueryListModel<Bean> model = new DocumentQueryListModel<>(module.getNullSafeMetaDataQuery("qListGridFixtureVueAttributes"));
		model.postConstruct(customer, true);
		model.setStartRow(0);
		model.setEndRow(10);
		SmartClientListServlet.addAdvancedFilterCriteriaToQuery(module, document, CORE.getUser(), CompoundFilterOperator.or, criteria, null, model);
		Page page = model.fetch();
		return texts(page.getRows());
	}

	private static Set<String> texts(List<Bean> beans) {
		Set<String> result = new TreeSet<>();
		for (Bean bean : beans) {
			result.add((String) org.skyve.util.Binder.get(bean, TEXT));
		}
		return result;
	}

	private static ListAttributes row(String text, LocalDateTime dateTime) {
		ListAttributes result = ListAttributes.newInstance();
		result.setText(text);
		result.setDateTime(new DateTime(dateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
		return result;
	}

	/** A criterion as a list grid sends it, its value a string as in the request's JSON. */
	private static Map<String, Object> criterion(String fieldName, String operator, String value) {
		return Map.of("fieldName", fieldName, "operator", operator, "value", value);
	}

	/** The Vue list grid's day filter on the date time column: from the day's start to the next day's (see dayCriteria). */
	private static Map<String, Object> day(int year, int month, int day) {
		LocalDateTime start = LocalDateTime.of(year, month, day, 0, 0);
		return Map.of("_constructor", "AdvancedCriteria",
						"operator", "and",
						"criteria", List.of(criterion(DATE_TIME, "greaterOrEqual", iso(start)),
											criterion(DATE_TIME, "lessThan", iso(start.plusDays(1)))));
	}

	private static String start(int year, int month, int day) {
		return iso(LocalDateTime.of(year, month, day, 0, 0));
	}

	/** An ISO 8601 instant in UTC, as JSON writes a Date. */
	private static String iso(LocalDateTime local) {
		return local.atZone(ZoneId.systemDefault()).toInstant().toString();
	}
}
