package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.criterion;
import static org.skyve.impl.web.service.smartclient.ListServletCriteria.group;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.snapshot.CompoundFilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;

import modules.kitchensink.domain.LookupDescription;
import modules.kitchensink.domain.ModelLookupFixture;
import util.AbstractH2Test;

/**
 * Negations on an optional association (a column of the association itself, filtered by its bizId) and on an attribute
 * through it ({@code selection.description}): a row with no association matches them, in the database (whose query
 * must not drop the row by an inner join) as in memory. Under Match Any, a positive condition through it must not drop
 * the rows that another criterion keeps either; under Match All it still leaves out the rows with no association.
 */
class SmartClientListServletAssociationsH2Test extends AbstractH2Test {
	private static final String QUERY = "qModelLookupFixtures";
	private static final String NAME = ModelLookupFixture.namePropertyName;
	private static final String SELECTION = ModelLookupFixture.selectionPropertyName;
	private static final String DESCRIPTION = ModelLookupFixture.selectionPropertyName + '.' + LookupDescription.descriptionPropertyName;

	private Module module;
	private Document document;
	private List<Bean> rows;
	private String alphaId;

	/** "AF a" selects alpha, "AF b" beta, and "AF n" nothing. */
	@BeforeEach
	void makeRows() {
		Customer customer = CORE.getCustomer();
		module = customer.getModule(ModelLookupFixture.MODULE_NAME);
		document = module.getDocument(customer, ModelLookupFixture.DOCUMENT_NAME);
		Persistence persistence = CORE.getPersistence();
		LookupDescription alpha = persistence.save(lookup("alpha"));
		LookupDescription beta = persistence.save(lookup("beta"));
		alphaId = alpha.getBizId();
		rows = new ArrayList<>(3);
		rows.add(persistence.save(fixture("AF a", alpha)));
		rows.add(persistence.save(fixture("AF b", beta)));
		rows.add(persistence.save(fixture("AF n", null)));
		persistence.flush();
	}

	@Test
	void aJoinedAttributesNegationIncludesNoAssociation() throws Exception {
		assertRows(Set.of("AF b", "AF n"), CompoundFilterOperator.and, criterion(DESCRIPTION, "notEqual", "alpha"));
		assertRows(Set.of("AF b", "AF n"), CompoundFilterOperator.and, criterion(DESCRIPTION, "iNotContains", "alp"));
		assertRows(Set.of("AF b", "AF n"), CompoundFilterOperator.not, criterion(DESCRIPTION, "iContains", "alp"));
		assertRows(Set.of("AF a", "AF n"), CompoundFilterOperator.not, group("or", criterion(DESCRIPTION, "equals", "beta")));
	}

	@Test
	void aJoinedAttributesPositiveConditionExcludesNoAssociation() throws Exception {
		assertRows(Set.of("AF a"), CompoundFilterOperator.and, criterion(DESCRIPTION, "iContains", "alp"));
		assertRows(Set.of("AF a", "AF b", "AF n"), CompoundFilterOperator.and, criterion(NAME, "iStartsWith", "AF"));
	}

	/**
	 * Match Any of a positive condition through the association and one on the row itself: "AF n" has no association
	 * but meets the second, so the query must not drop it by an inner join of the first's path.
	 */
	@Test
	void aJoinedAttributesPositiveConditionUnderMatchAnyKeepsTheOtherCriterionsRows() throws Exception {
		assertRows(Set.of("AF b", "AF n"), CompoundFilterOperator.or, criterion(DESCRIPTION, "equals", "beta"), criterion(NAME, "equals", "AF n"));
		assertRows(Set.of("AF a", "AF n"), CompoundFilterOperator.or, criterion(DESCRIPTION, "iContains", "alp"), criterion(NAME, "iEndsWith", "n"));
		assertRows(Set.of("AF b", "AF n"), CompoundFilterOperator.or, criterion(DESCRIPTION, "greaterThan", "alpha"), criterion(NAME, "equals", "AF n"));
		assertRows(Set.of("AF a", "AF n"), CompoundFilterOperator.or, criterion(SELECTION, "equals", alphaId), criterion(NAME, "equals", "AF n"));
	}

	/** Match All of positive conditions through the association lists only the rows with one, as an inner join did. */
	@Test
	void aJoinedAttributesPositiveConditionsUnderMatchAllExcludeNoAssociation() throws Exception {
		assertRows(Set.of("AF b"), CompoundFilterOperator.and, criterion(DESCRIPTION, "equals", "beta"), criterion(NAME, "iStartsWith", "AF"));
		assertRows(Set.of("AF a", "AF b"), CompoundFilterOperator.and, criterion(DESCRIPTION, "iContains", "a"), criterion(NAME, "iStartsWith", "AF"));
		assertRows(Set.of("AF b"), CompoundFilterOperator.and, criterion(DESCRIPTION, "greaterThan", "alpha"), criterion(DESCRIPTION, "notNull", null));
		assertRows(Set.of("AF a"), CompoundFilterOperator.and, criterion(SELECTION, "equals", alphaId), criterion(NAME, "iStartsWith", "AF"));
	}

	@Test
	void anAssociationsNegationIncludesNoAssociation() throws Exception {
		assertRows(Set.of("AF b", "AF n"), CompoundFilterOperator.and, criterion(SELECTION, "notEqual", alphaId));
		assertRows(Set.of("AF b", "AF n"), CompoundFilterOperator.not, criterion(SELECTION, "equals", alphaId));
		assertRows(Set.of("AF a"), CompoundFilterOperator.and, criterion(SELECTION, "equals", alphaId));
	}

	/**
	 * Checks the rows in the database, through the fixture's query (whose columns include the association and the
	 * attribute through it) and the document's default query (whose do not), and in memory.
	 */
	@SafeVarargs
	private void assertRows(Set<String> expected, CompoundFilterOperator operator, Map<String, Object>... criteria) throws Exception {
		List<Map<String, Object>> list = List.of(criteria);
		assertEquals(expected, ListServletCriteria.fetch(module, document, QUERY, NAME, operator, list), "database " + operator + " " + list);
		assertEquals(expected, ListServletCriteria.fetch(module, document, null, NAME, operator, list), "default query " + operator + " " + list);
		assertEquals(expected, ListServletCriteria.filterInMemory(module, document, rows, NAME, operator, list), "in memory " + operator + " " + list);
	}

	private static LookupDescription lookup(String description) {
		LookupDescription result = LookupDescription.newInstance();
		result.setDescription(description);
		return result;
	}

	private static ModelLookupFixture fixture(String name, LookupDescription selection) {
		ModelLookupFixture result = ModelLookupFixture.newInstance();
		result.setName(name);
		result.setSelection(selection);
		return result;
	}
}
