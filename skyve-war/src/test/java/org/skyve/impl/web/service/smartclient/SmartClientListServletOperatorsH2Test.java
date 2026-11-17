package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.snapshot.CompoundFilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

import modules.kitchensink.domain.ListAttributes;
import util.AbstractH2Test;

/**
 * SmartClient's range, blank and variant domain operators in a list grid's advanced criteria, as SmartClient documents
 * them (OperatorId): {@code between} and {@code iBetween} are greaterThan and lessThan (exclusive at both ends),
 * {@code betweenInclusive} and {@code iBetweenInclusive} greaterOrEqual and lessOrEqual; under Match None each is
 * negated (outside the range), {@code notBlank} becomes blank, and a variant domain's description filter is negated
 * and combined with its siblings like any other criterion, under and, or and not, and at depth.
 */
class SmartClientListServletOperatorsH2Test extends AbstractH2Test {
	private static final String TEXT = ListAttributes.textPropertyName;
	private static final String NUMBER = ListAttributes.normalIntegerPropertyName;
	private static final String MEMO = ListAttributes.memoPropertyName;
	private static final String VARIANT = ListAttributes.variantDomainPropertyName;

	private Customer customer;
	private Module module;
	private Document document;

	/**
	 * Three list attributes: "OP a" (1, no memo, variant one), "OP b" (5, memo "x", variant two) and "OP c" (10, memo
	 * "y", variant three).
	 */
	@BeforeEach
	void makeRows() throws Exception {
		customer = CORE.getCustomer();
		module = customer.getModule(ListAttributes.MODULE_NAME);
		document = module.getDocument(customer, ListAttributes.DOCUMENT_NAME);
		Persistence persistence = CORE.getPersistence();
		persistence.save(row("OP a", 1, null, "one"));
		persistence.save(row("OP b", 5, "x", "two"));
		persistence.save(row("OP c", 10, "y", "three"));
		persistence.flush();
	}

	@Test
	void betweenIsExclusive() throws Exception {
		assertEquals(Set.of("OP b"), fetch(CompoundFilterOperator.and, List.of(range("between", "1", "10"))));
	}

	@Test
	void iBetweenIsExclusive() throws Exception {
		assertEquals(Set.of("OP b"), fetch(CompoundFilterOperator.and, List.of(range("iBetween", "1", "10"))));
	}

	@Test
	void betweenInclusiveIsInclusive() throws Exception {
		assertEquals(Set.of("OP a", "OP b", "OP c"), fetch(CompoundFilterOperator.and, List.of(range("betweenInclusive", "1", "10"))));
	}

	@Test
	void betweenWithOneEndIsOpen() throws Exception {
		assertEquals(Set.of("OP c"), fetch(CompoundFilterOperator.and, List.of(range("between", "1", null), range("iBetween", "5", null))));
	}

	@Test
	void notBetweenInclusiveIsOutsideTheRange() throws Exception {
		assertEquals(Set.of("OP c"), fetch(CompoundFilterOperator.not, List.of(range("betweenInclusive", "1", "5"))));
	}

	@Test
	void notIBetweenInclusiveIsOutsideTheRange() throws Exception {
		assertEquals(Set.of("OP a"), fetch(CompoundFilterOperator.not, List.of(range("iBetweenInclusive", "5", "10"))));
	}

	@Test
	void notBetweenIncludesItsEnds() throws Exception {
		assertEquals(Set.of("OP a", "OP c"), fetch(CompoundFilterOperator.not, List.of(range("between", "1", "10"))));
	}

	@Test
	void notIBetweenIncludesItsEnds() throws Exception {
		assertEquals(Set.of("OP a", "OP c"), fetch(CompoundFilterOperator.not, List.of(range("iBetween", "1", "10"))));
	}

	@Test
	void notBetweenInclusiveWithOneEnd() throws Exception {
		assertEquals(Set.of("OP a"), fetch(CompoundFilterOperator.not, List.of(range("betweenInclusive", "5", null))));
		assertEquals(Set.of("OP c"), fetch(CompoundFilterOperator.not, List.of(range("betweenInclusive", null, "5"))));
	}

	@Test
	void notNotBlankIsBlank() throws Exception {
		assertEquals(Set.of("OP a"), fetch(CompoundFilterOperator.not, List.of(criterion(MEMO, "notBlank", null))));
	}

	@Test
	void notIsBlankIsNotBlank() throws Exception {
		assertEquals(Set.of("OP b", "OP c"), fetch(CompoundFilterOperator.not, List.of(criterion(MEMO, "isBlank", null))));
	}

	@Test
	void aVariantDomainFilterIsNegatedUnderNot() throws Exception {
		assertEquals(Set.of("OP a", "OP c"), fetch(CompoundFilterOperator.not, List.of(criterion(VARIANT, "iContains", "Two"))));
	}

	@Test
	void aVariantDomainFilterIsOredUnderOr() throws Exception {
		assertEquals(Set.of("OP a", "OP c"),
						fetch(CompoundFilterOperator.or, List.of(criterion(TEXT, "iContains", "OP a"), criterion(VARIANT, "iContains", "Three"))));
	}

	@Test
	void aVariantDomainFilterIsAndedUnderAnd() throws Exception {
		assertEquals(Set.of("OP b"),
						fetch(CompoundFilterOperator.and, List.of(criterion(TEXT, "iContains", "OP"), criterion(VARIANT, "iContains", "Two"))));
	}

	/** Match None on [and: betweenInclusive 1 to 5, notBlank memo]: not b, so a and c. */
	@Test
	void aGroupOfRangeAndBlankUnderNotIsNegatedAsAWhole() throws Exception {
		assertEquals(Set.of("OP a", "OP c"),
						fetch(CompoundFilterOperator.not, List.of(group("and", range("betweenInclusive", "1", "5"), criterion(MEMO, "notBlank", null)))));
	}

	/** Match All on [contains OP] and [not: [or: between 1 and 10, variant One]]: not (b or a), so c. */
	@Test
	void aNotGroupOfAnOrGroupAtDepth() throws Exception {
		assertEquals(Set.of("OP c"),
						fetch(CompoundFilterOperator.and,
								List.of(criterion(TEXT, "iContains", "OP"),
										group("not", group("or", range("between", "1", "10"), criterion(VARIANT, "iContains", "One"))))));
	}

	/** Match Any on [between 1 and 10] or [variant One]: b or a. */
	@Test
	void betweenAndAVariantUnderOr() throws Exception {
		assertEquals(Set.of("OP a", "OP b"),
						fetch(CompoundFilterOperator.or, List.of(range("between", "1", "10"), criterion(VARIANT, "iContains", "One"))));
	}

	private Set<String> fetch(CompoundFilterOperator operator, List<Map<String, Object>> criteria) throws Exception {
		DocumentQueryListModel<Bean> model = new DocumentQueryListModel<>(module.getNullSafeMetaDataQuery("qListGridFixtureVueAttributes"));
		model.postConstruct(customer, true);
		model.setStartRow(0);
		model.setEndRow(10);
		SmartClientListServlet.addAdvancedFilterCriteriaToQuery(module, document, CORE.getUser(), operator, criteria, null, model);
		Page page = model.fetch();
		Set<String> result = new TreeSet<>();
		for (Bean bean : page.getRows()) {
			result.add((String) Binder.get(bean, TEXT));
		}
		return result;
	}

	private static ListAttributes row(String text, int number, String memo, String variant) {
		ListAttributes result = ListAttributes.newInstance();
		result.setText(text);
		result.setNormalInteger(Integer.valueOf(number));
		result.setMemo(memo);
		result.setVariantDomain(variant);
		return result;
	}

	/** A criterion as a list grid sends it, its value a string as in the request's JSON (or none). */
	private static Map<String, Object> criterion(String fieldName, String operator, String value) {
		Map<String, Object> result = new HashMap<>();
		result.put("fieldName", fieldName);
		result.put("operator", operator);
		if (value != null) {
			result.put("value", value);
		}
		return result;
	}

	/** A range criterion on the number column, either end missing when null. */
	private static Map<String, Object> range(String operator, String start, String end) {
		Map<String, Object> result = criterion(NUMBER, operator, null);
		if (start != null) {
			result.put("start", start);
		}
		if (end != null) {
			result.put("end", end);
		}
		return result;
	}

	/** A nested group of criteria. */
	@SafeVarargs
	private static Map<String, Object> group(String operator, Map<String, Object>... criteria) {
		return Map.of("operator", operator, "criteria", List.of(criteria));
	}
}
