package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.criterion;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.group;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.range;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.snapshot.CompoundFilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;

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

	private Module module;
	private Document document;

	/**
	 * Three list attributes: "OP a" (1, no memo, variant one), "OP b" (5, memo "x", variant two) and "OP c" (10, memo
	 * "y", variant three).
	 */
	@BeforeEach
	void makeRows() {
		Customer customer = CORE.getCustomer();
		module = customer.getModule(ListAttributes.MODULE_NAME);
		document = module.getDocument(customer, ListAttributes.DOCUMENT_NAME);
		Persistence persistence = CORE.getPersistence();
		persistence.save(row("OP a", Integer.valueOf(1), null, "one"));
		persistence.save(row("OP b", Integer.valueOf(5), "x", "two"));
		persistence.save(row("OP c", Integer.valueOf(10), "y", "three"));
		persistence.flush();
	}

	@Test
	void betweenIsExclusive() throws Exception {
		assertEquals(Set.of("OP b"), fetch(CompoundFilterOperator.and, List.of(range(NUMBER, "between", "1", "10"))));
	}

	@Test
	void iBetweenIsExclusive() throws Exception {
		assertEquals(Set.of("OP b"), fetch(CompoundFilterOperator.and, List.of(range(NUMBER, "iBetween", "1", "10"))));
	}

	@Test
	void betweenInclusiveIsInclusive() throws Exception {
		assertEquals(Set.of("OP a", "OP b", "OP c"), fetch(CompoundFilterOperator.and, List.of(range(NUMBER, "betweenInclusive", "1", "10"))));
	}

	@Test
	void betweenWithOneEndIsOpen() throws Exception {
		assertEquals(Set.of("OP c"), fetch(CompoundFilterOperator.and, List.of(range(NUMBER, "between", "1", null), range(NUMBER, "iBetween", "5", null))));
	}

	@Test
	void notBetweenInclusiveIsOutsideTheRange() throws Exception {
		assertEquals(Set.of("OP c"), fetch(CompoundFilterOperator.not, List.of(range(NUMBER, "betweenInclusive", "1", "5"))));
	}

	@Test
	void notIBetweenInclusiveIsOutsideTheRange() throws Exception {
		assertEquals(Set.of("OP a"), fetch(CompoundFilterOperator.not, List.of(range(NUMBER, "iBetweenInclusive", "5", "10"))));
	}

	@Test
	void notBetweenIncludesItsEnds() throws Exception {
		assertEquals(Set.of("OP a", "OP c"), fetch(CompoundFilterOperator.not, List.of(range(NUMBER, "between", "1", "10"))));
	}

	@Test
	void notIBetweenIncludesItsEnds() throws Exception {
		assertEquals(Set.of("OP a", "OP c"), fetch(CompoundFilterOperator.not, List.of(range(NUMBER, "iBetween", "1", "10"))));
	}

	@Test
	void notBetweenInclusiveWithOneEnd() throws Exception {
		assertEquals(Set.of("OP a"), fetch(CompoundFilterOperator.not, List.of(range(NUMBER, "betweenInclusive", "5", null))));
		assertEquals(Set.of("OP c"), fetch(CompoundFilterOperator.not, List.of(range(NUMBER, "betweenInclusive", null, "5"))));
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

	/** A variant's iNotContains that no description matches is every row (not in an empty set). */
	@Test
	void aVariantDomainNotContainsWithNoMatchIsEveryRow() throws Exception {
		assertEquals(Set.of("OP a", "OP b", "OP c"), fetch(CompoundFilterOperator.and, List.of(criterion(VARIANT, "iNotContains", "zzz"))));
	}

	/** Match None on a variant's iContains that no description matches is every row. */
	@Test
	void matchNoneOfAVariantDomainContainsWithNoMatchIsEveryRow() throws Exception {
		assertEquals(Set.of("OP a", "OP b", "OP c"), fetch(CompoundFilterOperator.not, List.of(criterion(VARIANT, "iContains", "zzz"))));
	}

	/** Match Any on [contains OP a] or [a variant's iNotContains that matches nothing]: every row. */
	@Test
	void aVariantDomainNotContainsWithNoMatchUnderOrIsEveryRow() throws Exception {
		assertEquals(Set.of("OP a", "OP b", "OP c"),
						fetch(CompoundFilterOperator.or, List.of(criterion(TEXT, "iContains", "OP a"), criterion(VARIANT, "iNotContains", "zzz"))));
	}

	/** A variant's notEqual is the codes whose descriptions do not match (notInSet). */
	@Test
	void aVariantDomainNotEqualIsNotInTheSet() throws Exception {
		assertEquals(Set.of("OP a", "OP c"), fetch(CompoundFilterOperator.and, List.of(criterion(VARIANT, "notEqual", "Two"))));
	}

	/**
	 * A row with no number and no variant under Match None: like SmartClient's own evaluation, a negation matches it
	 * (see SmartClientListServletNullsH2Test).
	 */
	@Test
	void aRowWithNoValueIsMatchedByTheNegations() throws Exception {
		Persistence persistence = CORE.getPersistence();
		persistence.save(row("OP n", null, null, null));
		persistence.flush();
		assertEquals(Set.of("OP c", "OP n"), fetch(CompoundFilterOperator.not, List.of(range(NUMBER, "betweenInclusive", "1", "5"))));
		assertEquals(Set.of("OP a", "OP c", "OP n"), fetch(CompoundFilterOperator.not, List.of(criterion(VARIANT, "iContains", "Two"))));
	}

	/** Match None on [and: betweenInclusive 1 to 5, notBlank memo]: not b, so a and c. */
	@Test
	void aGroupOfRangeAndBlankUnderNotIsNegatedAsAWhole() throws Exception {
		assertEquals(Set.of("OP a", "OP c"),
						fetch(CompoundFilterOperator.not, List.of(group("and", range(NUMBER, "betweenInclusive", "1", "5"), criterion(MEMO, "notBlank", null)))));
	}

	/** Match All on [contains OP] and [not: [or: between 1 and 10, variant One]]: not (b or a), so c. */
	@Test
	void aNotGroupOfAnOrGroupAtDepth() throws Exception {
		assertEquals(Set.of("OP c"),
						fetch(CompoundFilterOperator.and,
								List.of(criterion(TEXT, "iContains", "OP"),
										group("not", group("or", range(NUMBER, "between", "1", "10"), criterion(VARIANT, "iContains", "One"))))));
	}

	/** Match Any on [between 1 and 10] or [variant One]: b or a. */
	@Test
	void betweenAndAVariantUnderOr() throws Exception {
		assertEquals(Set.of("OP a", "OP b"),
						fetch(CompoundFilterOperator.or, List.of(range(NUMBER, "between", "1", "10"), criterion(VARIANT, "iContains", "One"))));
	}

	private Set<String> fetch(CompoundFilterOperator operator, List<Map<String, Object>> criteria) throws Exception {
		return ListServletCriteria.fetch(module, document, "qListGridFixtureVueAttributes", TEXT, operator, criteria);
	}

	private static ListAttributes row(String text, Integer number, String memo, String variant) {
		ListAttributes result = ListAttributes.newInstance();
		result.setText(text);
		result.setNormalInteger(number);
		result.setMemo(memo);
		result.setVariantDomain(variant);
		return result;
	}
}
