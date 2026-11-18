package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.web.SortParameter;

import modules.kitchensink.domain.LookupDescription;
import modules.kitchensink.domain.ModelLookupFixture;
import modules.test.domain.AllAttributesEmbedded;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.InverseOneToOnePersistent;
import util.AbstractH2Test;

/**
 * A document's default query (whose columns join nothing) filtered through an optional association or an inverse one:
 * the path is left outer joined, rather than inner joined implicitly in HQL, so the filters that also match a row with
 * no value keep the rows with nothing on the path, and so does Match Any of another criterion they meet, whether the
 * from clause declares one document or several.
 */
@SuppressWarnings("static-method")
class DocumentQueryListModelJoinsH2Test extends AbstractH2Test {
	private static final String DESCRIPTION = ModelLookupFixture.selectionPropertyName + '.' + LookupDescription.descriptionPropertyName;
	private static final String SELECTION_JOIN = "LEFT OUTER JOIN bean." + ModelLookupFixture.selectionPropertyName;
	private static final String INVERSE_BIZ_KEY = InverseOneToOnePersistent.invAggAssociationPropertyName + '.' + Bean.BIZ_KEY;

	/** A view's filter parameters on an attribute through an association, as ListModel.addParameters adds them. */
	@Test
	void filterParametersThroughAnAssociationKeepRowsWithNoAssociation() throws Exception {
		Persistence persistence = CORE.getPersistence();
		LookupDescription alpha = persistence.save(lookup("alpha"));
		LookupDescription beta = persistence.save(lookup("beta"));
		persistence.save(fixture("AF a", alpha));
		persistence.save(fixture("AF b", beta));
		persistence.save(fixture("AF n", null));
		persistence.flush();

		assertEquals(Set.of("AF b", "AF n"), fixtures(parameter(FilterOperator.notLike, "alp")));
		assertEquals(Set.of("AF n"), fixtures(parameter(FilterOperator.isNull, null)));
		assertEquals(Set.of("AF b", "AF n"), fixtures(parameter(FilterOperator.nullOrEqual, "beta")));
		assertEquals(Set.of("AF a", "AF b"), fixtures(parameter(FilterOperator.like, "a")));
	}

	/** Not starts with and not ends with are "no value, or not like" too. */
	@Test
	void notStartsOrEndsWithThroughAnAssociationKeepRowsWithNoAssociation() throws Exception {
		Persistence persistence = CORE.getPersistence();
		LookupDescription alpha = persistence.save(lookup("alpha"));
		persistence.save(fixture("AF a", alpha));
		persistence.save(fixture("AF n", null));
		persistence.flush();

		assertEquals(Set.of("AF n"), fixtures(f -> f.addNotStartsWith(DESCRIPTION, "al")));
		assertEquals(Set.of("AF n"), fixtures(f -> f.addNotEndsWith(DESCRIPTION, "ha")));
	}

	/** "first" aggregates "second", so "second" has "first" as its inverse and "first" has none. */
	@Test
	void filtersThroughAnInverseOneKeepRowsWithNoInverse() throws Exception {
		Persistence persistence = CORE.getPersistence();
		InverseOneToOnePersistent second = persistence.save(InverseOneToOnePersistent.newInstance());
		InverseOneToOnePersistent first = InverseOneToOnePersistent.newInstance();
		first.setAggAssociation(second);
		first = persistence.save(first);
		persistence.flush();

		assertEquals(Set.of(first.getBizId()), inverses(f -> f.addNull(INVERSE_BIZ_KEY)));
		assertEquals(Set.of(first.getBizId(), second.getBizId()), inverses(f -> f.addNotContains(INVERSE_BIZ_KEY, "zzz")));
		assertEquals(Set.of(second.getBizId()), inverses(f -> f.addNotNull(INVERSE_BIZ_KEY)));
	}

	/**
	 * Match Any of a positive condition through the association and one on the row itself keeps the row with no
	 * association, counts what it lists, and sorts it by the association's attribute (no value last, descending on H2).
	 */
	@Test
	void aMatchAnyThroughAnAssociationListsCountsAndSortsTheRowWithNoAssociation() throws Exception {
		Persistence persistence = CORE.getPersistence();
		LookupDescription alpha = persistence.save(lookup("alpha"));
		LookupDescription beta = persistence.save(lookup("beta"));
		persistence.save(fixture("AF a", alpha));
		persistence.save(fixture("AF b", beta));
		persistence.save(fixture("AF n", null));
		persistence.flush();

		DocumentQueryListModel<Bean> model = defaultQueryModel(ModelLookupFixture.MODULE_NAME, ModelLookupFixture.DOCUMENT_NAME);
		matchAny(model, f -> f.addEquals(DESCRIPTION, "beta"), f -> f.addEquals(ModelLookupFixture.namePropertyName, "AF n"));
		SortParameterImpl sort = new SortParameterImpl();
		sort.setBy(DESCRIPTION);
		sort.setDirection(SortDirection.descending);
		model.setSortParameters(new SortParameter[] {sort});
		Page page = model.fetch();
		List<String> names = new ArrayList<>();
		for (Bean row : Objects.requireNonNull(page.getRows())) {
			names.add((String) Binder.get(row, ModelLookupFixture.namePropertyName));
		}
		assertEquals(List.of("AF b", "AF n"), names);
		assertEquals(2L, page.getTotalRows());
	}

	/**
	 * A from clause of several documents gets the left outer join of a filter's path too, appended after the last of
	 * them (the dialect writes their separator as a cross join, so the join's on clause sees bean): a filter that also
	 * matches no value keeps the row with no association, in the rows and in the count.
	 */
	@Test
	void aFromOfSeveralDocumentsGetsTheJoinAndKeepsTheRowWithNoAssociation() throws Exception {
		Persistence persistence = CORE.getPersistence();
		LookupDescription alpha = persistence.save(lookup("alpha"));
		persistence.save(fixture("AF a", alpha));
		persistence.save(fixture("AF n", null));
		persistence.flush();

		String from = '{' + ModelLookupFixture.MODULE_NAME + '.' + ModelLookupFixture.DOCUMENT_NAME + "} as bean, {" +
						LookupDescription.MODULE_NAME + '.' + LookupDescription.DOCUMENT_NAME + "} as other";
		DocumentQueryListModel<Bean> model = defaultQueryModel(ModelLookupFixture.MODULE_NAME, ModelLookupFixture.DOCUMENT_NAME, from);
		matchAny(model, f -> f.addEquals(DESCRIPTION, "alpha"), f -> f.addNull(DESCRIPTION));
		Page page = model.fetch();
		Set<String> names = new TreeSet<>();
		for (Bean row : Objects.requireNonNull(page.getRows())) {
			names.add((String) Binder.get(row, ModelLookupFixture.namePropertyName));
		}
		// one other document (alpha), so one row per fixture
		assertEquals(Set.of("AF a", "AF n"), names);
		assertEquals(2L, page.getTotalRows());
		for (DocumentQuery query : List.of(Objects.requireNonNull(model.getDetailQuery()), Objects.requireNonNull(model.getSummaryQuery()))) {
			String hql = ((AbstractDocumentQuery) query).toQueryString();
			assertTrue(hql.contains(SELECTION_JOIN), hql);
		}
	}

	/**
	 * A from clause that inner joins the association itself still gets the left outer join of a filter's path, but its
	 * own inner join leaves out the rows with no association, as it is meant to.
	 */
	@Test
	void anInnerJoinInTheFromStillLeavesOutTheRowsWithNoAssociation() throws Exception {
		Persistence persistence = CORE.getPersistence();
		LookupDescription beta = persistence.save(lookup("beta"));
		persistence.save(fixture("AF b", beta));
		persistence.save(fixture("AF n", null));
		persistence.flush();

		String from = '{' + ModelLookupFixture.MODULE_NAME + '.' + ModelLookupFixture.DOCUMENT_NAME + "} as bean inner join bean." +
						ModelLookupFixture.selectionPropertyName + " as s";
		DocumentQueryListModel<Bean> model = defaultQueryModel(ModelLookupFixture.MODULE_NAME, ModelLookupFixture.DOCUMENT_NAME, from);
		matchAny(model, f -> f.addEquals(DESCRIPTION, "beta"), f -> f.addEquals(ModelLookupFixture.namePropertyName, "AF n"));
		assertEquals(Set.of("AF b"), fetch(model, ModelLookupFixture.namePropertyName));
		String hql = ((AbstractDocumentQuery) Objects.requireNonNull(model.getDetailQuery())).toQueryString();
		assertTrue(hql.contains(SELECTION_JOIN), hql);
	}

	/** An embedded association's attributes are on the row: a filter through one joins nothing and keeps its rows. */
	@Test
	void aFilterThroughAnEmbeddedAssociationJoinsNothing() throws Exception {
		Persistence persistence = CORE.getPersistence();
		AllAttributesEmbedded embedded = AllAttributesEmbedded.newInstance();
		embedded.setText("inside");
		AllAttributesPersistent withEmbedded = AllAttributesPersistent.newInstance();
		withEmbedded.setText("with");
		withEmbedded.setEmbeddedAssociation(embedded);
		persistence.save(withEmbedded);
		AllAttributesPersistent withoutEmbedded = AllAttributesPersistent.newInstance();
		withoutEmbedded.setText("without");
		persistence.save(withoutEmbedded);
		persistence.flush();

		DocumentQueryListModel<Bean> model = defaultQueryModel(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		matchAny(model,
					f -> f.addEquals(AllAttributesPersistent.embeddedAssociationPropertyName + '.' + AllAttributesEmbedded.textPropertyName, "inside"),
					f -> f.addEquals(AllAttributesPersistent.textPropertyName, "without"));
		assertEquals(Set.of("with", "without"), fetch(model, AllAttributesPersistent.textPropertyName));
		assertNoJoin(model.getDetailQuery());
	}

	private static void assertNoJoin(DocumentQuery query) {
		String hql = ((AbstractDocumentQuery) Objects.requireNonNull(query)).toQueryString();
		assertFalse(hql.contains("LEFT OUTER JOIN"), hql);
	}

	/** Ors the conditions, each in a filter of its own, into the model's filter, as Match Any does. */
	@SafeVarargs
	private static void matchAny(DocumentQueryListModel<Bean> model, Consumer<Filter>... conditions) {
		Filter filter = model.getFilter();
		for (Consumer<Filter> condition : conditions) {
			Filter or = model.newFilter();
			condition.accept(or);
			filter.addOr(or);
		}
	}

	private static Set<String> fixtures(FilterParameter parameter) throws Exception {
		DocumentQueryListModel<Bean> model = defaultQueryModel(ModelLookupFixture.MODULE_NAME, ModelLookupFixture.DOCUMENT_NAME);
		model.addParameters(model.getDrivingDocument(), List.of(parameter), null);
		return fetch(model, ModelLookupFixture.namePropertyName);
	}

	private static Set<String> fixtures(Consumer<Filter> filtering) throws Exception {
		DocumentQueryListModel<Bean> model = defaultQueryModel(ModelLookupFixture.MODULE_NAME, ModelLookupFixture.DOCUMENT_NAME);
		filtering.accept(model.getFilter());
		return fetch(model, ModelLookupFixture.namePropertyName);
	}

	private static Set<String> inverses(Consumer<Filter> filtering) throws Exception {
		DocumentQueryListModel<Bean> model = defaultQueryModel(InverseOneToOnePersistent.MODULE_NAME, InverseOneToOnePersistent.DOCUMENT_NAME);
		filtering.accept(model.getFilter());
		return fetch(model, Bean.DOCUMENT_ID);
	}

	private static DocumentQueryListModel<Bean> defaultQueryModel(String moduleName, String documentName) {
		return defaultQueryModel(moduleName, documentName, null);
	}

	/** The document's default query, from the given from clause if there is one. */
	private static DocumentQueryListModel<Bean> defaultQueryModel(String moduleName, String documentName, String from) {
		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		MetaDataQueryDefinition query = module.getDocumentDefaultQuery(customer, document.getName());
		if (from != null) {
			((MetaDataQueryDefinitionImpl) query).setFromClause(from);
		}
		DocumentQueryListModel<Bean> result = new DocumentQueryListModel<>(query);
		result.postConstruct(customer, true);
		result.setStartRow(0);
		result.setEndRow(100);
		return result;
	}

	private static Set<String> fetch(DocumentQueryListModel<Bean> model, String binding) throws Exception {
		Set<String> result = new TreeSet<>();
		for (Bean row : Objects.requireNonNull(model.fetch().getRows())) {
			result.add((String) Binder.get(row, binding));
		}
		return result;
	}

	private static FilterParameter parameter(FilterOperator operator, String value) {
		FilterParameterImpl result = new FilterParameterImpl();
		result.setFilterBinding(DESCRIPTION);
		result.setOperator(operator);
		result.setValue(value);
		return result;
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
