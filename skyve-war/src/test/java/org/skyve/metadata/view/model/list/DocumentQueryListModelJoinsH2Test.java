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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.ModuleDocument;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.web.SortParameter;

import modules.kitchensink.domain.KitchensinkDocument;
import modules.kitchensink.domain.LookupDescription;
import modules.kitchensink.domain.ModelLookupFixture;
import modules.test.domain.AllAttributesEmbedded;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.InverseOneToOnePersistent;
import modules.test.domain.TestDocument;
import util.AbstractH2Test;

/**
 * A document's default query (whose columns join nothing) filtered through an optional association or an inverse one:
 * the path is left outer joined, rather than inner joined implicitly in HQL, so the filters that also match a row with
 * no value keep the rows with nothing on the path, and so does Match Any of another criterion they meet, whether the
 * from clause declares one document or several. A path the from clause joins already keeps that join.
 */
@SuppressWarnings("static-method")
class DocumentQueryListModelJoinsH2Test extends AbstractH2Test {
	private static final String DESCRIPTION = ModelLookupFixture.selectionPropertyName + '.' + LookupDescription.descriptionPropertyName;
	private static final String SELECTION_JOIN = "LEFT OUTER JOIN bean." + ModelLookupFixture.selectionPropertyName;
	private static final String INVERSE_BIZ_KEY = InverseOneToOnePersistent.invAggAssociationPropertyName + '.' + Bean.BIZ_KEY;
	private static final String TEXT = AllAttributesPersistent.textPropertyName;
	private static final String AGGREGATED = AllAttributesPersistent.aggregatedAssociationPropertyName;
	private static final String AGGREGATED_AGGREGATED = AGGREGATED + '.' + AGGREGATED;
	private static final Pattern LEFT_OUTER_JOIN = Pattern.compile("LEFT OUTER JOIN bean\\.([\\w.]+)");

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

		DocumentQueryListModel<Bean> model = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE);
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
		DocumentQueryListModel<Bean> model = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE, from);
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
	 * A from clause that inner joins the association itself keeps its own join for a filter's path, so no left outer
	 * join is appended, and its inner join leaves out the rows with no association, as it is meant to.
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
		DocumentQueryListModel<Bean> model = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE, from);
		matchAny(model, f -> f.addEquals(DESCRIPTION, "beta"), f -> f.addEquals(ModelLookupFixture.namePropertyName, "AF n"));
		assertEquals(Set.of("AF b"), fetch(model, ModelLookupFixture.namePropertyName));
		assertNoJoin(model.getDetailQuery(), SELECTION_JOIN);
		assertNoJoin(model.getSummaryQuery(), SELECTION_JOIN);
	}

	/**
	 * A from clause that joins the association with an alias and a with condition keeps its own join for a filter's
	 * path: an appended left outer join on the same path would replace it for the rest of the query and lose the
	 * condition. So a row whose association the condition leaves out does not match through it, under Match All or Any.
	 */
	@Test
	void aJoinWithAConditionInTheFromIsTheOneTheFiltersUse() throws Exception {
		Persistence persistence = CORE.getPersistence();
		LookupDescription alpha = persistence.save(lookup("alpha"));
		LookupDescription beta = persistence.save(lookup("beta"));
		persistence.save(fixture("AF a", alpha));
		persistence.save(fixture("AF b", beta));
		persistence.save(fixture("AF n", null));
		persistence.flush();

		String from = '{' + ModelLookupFixture.MODULE_NAME + '.' + ModelLookupFixture.DOCUMENT_NAME + "} as bean left outer join bean." +
						ModelLookupFixture.selectionPropertyName + " as s with s." + LookupDescription.descriptionPropertyName + " = 'alpha'";
		DocumentQueryListModel<Bean> model = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE, from);
		model.getFilter().addEquals(DESCRIPTION, "beta");
		assertEquals(Set.of(), fetch(model, ModelLookupFixture.namePropertyName));
		assertNoJoin(model.getDetailQuery(), SELECTION_JOIN);

		model = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE, from);
		matchAny(model, f -> f.addEquals(DESCRIPTION, "beta"), f -> f.addEquals(ModelLookupFixture.namePropertyName, "AF n"));
		Page page = model.fetch();
		assertEquals(Set.of("AF n"), names(page, ModelLookupFixture.namePropertyName));
		assertEquals(1L, page.getTotalRows());
		assertNoJoin(model.getDetailQuery(), SELECTION_JOIN);
		assertNoJoin(model.getSummaryQuery(), SELECTION_JOIN);
	}

	/**
	 * A from clause's own join of one association leaves a filter through another one in the same query joined: Match
	 * Any keeps the row with no such association that another criterion meets.
	 */
	@Test
	void aJoinInTheFromLeavesAnotherPathJoined() throws Exception {
		Persistence persistence = CORE.getPersistence();
		LookupDescription alpha = persistence.save(lookup("alpha"));
		LookupDescription beta = persistence.save(lookup("beta"));
		ModelLookupFixture a = fixture("AF a", alpha);
		a.setSingleSelection(beta);
		persistence.save(a);
		persistence.save(fixture("AF b", beta));
		persistence.save(fixture("AF n", null));
		persistence.flush();

		String from = '{' + ModelLookupFixture.MODULE_NAME + '.' + ModelLookupFixture.DOCUMENT_NAME + "} as bean left outer join bean." +
						ModelLookupFixture.selectionPropertyName + " as s with s." + LookupDescription.descriptionPropertyName + " = 'alpha'";
		DocumentQueryListModel<Bean> model = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE, from);
		matchAny(model,
					f -> f.addEquals(DESCRIPTION, "beta"),
					f -> f.addEquals(ModelLookupFixture.singleSelectionPropertyName + '.' + LookupDescription.descriptionPropertyName, "beta"),
					f -> f.addEquals(ModelLookupFixture.namePropertyName, "AF n"));
		Page page = model.fetch();
		// AF b's selection is not alpha, so the from's join has none for it; AF n has no single selection
		assertEquals(Set.of("AF a", "AF n"), names(page, ModelLookupFixture.namePropertyName));
		assertEquals(2L, page.getTotalRows());
		assertJoins(model, ModelLookupFixture.singleSelectionPropertyName);
	}

	/**
	 * A condition on an association's bizId compares the foreign key on the row, so it joins nothing, and lists the
	 * rows it would with the join: Match Any keeps the row with no association that the other criterion meets.
	 */
	@Test
	void aConditionOnAnAssociationsBizIdJoinsNothing() throws Exception {
		Persistence persistence = CORE.getPersistence();
		LookupDescription alpha = persistence.save(lookup("alpha"));
		LookupDescription beta = persistence.save(lookup("beta"));
		persistence.save(fixture("AF a", alpha));
		persistence.save(fixture("AF b", beta));
		persistence.save(fixture("AF n", null));
		persistence.flush();

		// the rows first, as they are the same with the join
		String selectionBizId = ModelLookupFixture.selectionPropertyName + '.' + Bean.DOCUMENT_ID;
		DocumentQueryListModel<Bean> matchAny = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE);
		matchAny(matchAny, f -> f.addEquals(selectionBizId, beta.getBizId()), f -> f.addEquals(ModelLookupFixture.namePropertyName, "AF n"));
		Page page = matchAny.fetch();
		assertEquals(Set.of("AF b", "AF n"), names(page, ModelLookupFixture.namePropertyName));
		assertEquals(2L, page.getTotalRows());

		DocumentQueryListModel<Bean> notEquals = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE);
		notEquals.getFilter().addNotEquals(selectionBizId, alpha.getBizId());
		assertEquals(Set.of("AF b"), fetch(notEquals, ModelLookupFixture.namePropertyName));

		DocumentQueryListModel<Bean> isNull = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE);
		isNull.getFilter().addNull(selectionBizId);
		assertEquals(Set.of("AF n"), fetch(isNull, ModelLookupFixture.namePropertyName));

		// the association's own attribute is on its table, so it is joined
		DocumentQueryListModel<Bean> description = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE);
		matchAny(description, f -> f.addEquals(DESCRIPTION, "beta"), f -> f.addEquals(ModelLookupFixture.namePropertyName, "AF n"));
		assertEquals(Set.of("AF b", "AF n"), fetch(description, ModelLookupFixture.namePropertyName));

		assertJoins(matchAny);
		assertJoins(notEquals);
		assertJoins(isNull);
		assertJoins(description, ModelLookupFixture.selectionPropertyName);
	}

	/**
	 * The bizId of an association through another joins only the first: a row with neither keeps no value, so a
	 * "no value" check lists it along with the row whose first association has none.
	 */
	@Test
	void aConditionOnTheBizIdOfAnAssociationsAssociationJoinsOnlyTheFirst() throws Exception {
		chain();

		DocumentQueryListModel<Bean> model = allAttributesModel(false);
		model.getFilter().addNull(AGGREGATED_AGGREGATED + '.' + Bean.DOCUMENT_ID);
		assertEquals(Set.of("b", "c"), fetch(model, TEXT));
		assertJoins(model, AGGREGATED);
	}

	/** A condition through an association joins it: Match Any keeps the row with none that another criterion meets. */
	@Test
	void aConditionThroughAnAssociationJoinsIt() throws Exception {
		chain();

		DocumentQueryListModel<Bean> model = allAttributesModel(false);
		matchAny(model, f -> f.addEquals(AGGREGATED + '.' + TEXT, "b"), f -> f.addEquals(TEXT, "c"));
		Page page = model.fetch();
		assertEquals(Set.of("a", "c"), names(page, TEXT));
		assertEquals(2L, page.getTotalRows());
		assertJoins(model, AGGREGATED);
	}

	/**
	 * A condition through two associations, with no column through them, joins both, the first first: a "no value"
	 * check lists the row with an association that has none, and the row with none; so does Match Any of a criterion
	 * the row with none meets.
	 */
	@Test
	void aConditionThroughTwoAssociationsJoinsBothInOrder() throws Exception {
		chain();

		DocumentQueryListModel<Bean> model = allAttributesModel(false);
		model.getFilter().addNull(AGGREGATED_AGGREGATED + '.' + TEXT);
		assertEquals(Set.of("b", "c"), fetch(model, TEXT));
		assertJoins(model, AGGREGATED, AGGREGATED_AGGREGATED);

		model = allAttributesModel(false);
		matchAny(model, f -> f.addEquals(AGGREGATED_AGGREGATED + '.' + TEXT, "c"), f -> f.addEquals(TEXT, "c"));
		assertEquals(Set.of("a", "c"), fetch(model, TEXT));
		assertJoins(model, AGGREGATED, AGGREGATED_AGGREGATED);
	}

	/**
	 * A column through two associations left outer joins both of them (Hibernate joins the first implicitly, with the
	 * same join type), so a condition through both appends no join, and lists the same rows: the row with an
	 * association that has none, and the row with none.
	 */
	@Test
	void aColumnThroughTwoAssociationsCoversAFilterThroughBoth() throws Exception {
		chain();

		// the rows first, as they are the same with the joins
		DocumentQueryListModel<Bean> isNull = allAttributesModel(true);
		isNull.getFilter().addNull(AGGREGATED_AGGREGATED + '.' + TEXT);
		assertEquals(Set.of("b", "c"), fetch(isNull, TEXT));

		DocumentQueryListModel<Bean> matchAny = allAttributesModel(true);
		matchAny(matchAny, f -> f.addEquals(AGGREGATED_AGGREGATED + '.' + TEXT, "c"), f -> f.addEquals(TEXT, "c"));
		Page page = matchAny.fetch();
		assertEquals(Set.of("a", "c"), names(page, TEXT));
		assertEquals(2L, page.getTotalRows());

		assertJoins(isNull, AGGREGATED_AGGREGATED);
		assertJoins(matchAny, AGGREGATED_AGGREGATED);
	}

	/**
	 * A column through two associations covers a filter through the first, so it appends no second join of it, and
	 * lists the same rows: Match Any and a negation keep the row with no association.
	 */
	@Test
	void aColumnThroughTwoAssociationsCoversAFilterThroughTheFirst() throws Exception {
		chain();

		// the rows first, as they are the same with the joins
		DocumentQueryListModel<Bean> matchAny = allAttributesModel(true);
		matchAny(matchAny, f -> f.addEquals(AGGREGATED + '.' + TEXT, "b"), f -> f.addEquals(TEXT, "c"));
		Page page = matchAny.fetch();
		assertEquals(Set.of("a", "c"), names(page, TEXT));
		assertEquals(2L, page.getTotalRows());

		DocumentQueryListModel<Bean> notContains = allAttributesModel(true);
		notContains.getFilter().addNotContains(AGGREGATED + '.' + TEXT, "b");
		assertEquals(Set.of("b", "c"), fetch(notContains, TEXT));

		assertJoins(matchAny, AGGREGATED_AGGREGATED);
		assertJoins(notContains, AGGREGATED_AGGREGATED);
	}

	/** AllAttributesPersistent's default query, with a column through two associations if asked for. */
	private static DocumentQueryListModel<Bean> allAttributesModel(boolean columnThroughTwoAssociations) {
		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(AllAttributesPersistent.MODULE_NAME);
		MetaDataQueryDefinition query = module.getDocumentDefaultQuery(customer, AllAttributesPersistent.DOCUMENT_NAME);
		if (columnThroughTwoAssociations) {
			MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
			column.setBinding(AGGREGATED_AGGREGATED + '.' + TEXT);
			query.getColumns().add(column);
		}
		DocumentQueryListModel<Bean> result = new DocumentQueryListModel<>(query);
		result.postConstruct(customer, true);
		result.setStartRow(0);
		result.setEndRow(100);
		return result;
	}

	/** Saves "a", which aggregates "b", which aggregates "c", which aggregates nothing. */
	private static void chain() {
		Persistence persistence = CORE.getPersistence();
		AllAttributesPersistent c = persistence.save(allAttributes("c", null));
		AllAttributesPersistent b = persistence.save(allAttributes("b", c));
		persistence.save(allAttributes("a", b));
		persistence.flush();
	}

	private static AllAttributesPersistent allAttributes(String text, AllAttributesPersistent aggregated) {
		AllAttributesPersistent result = AllAttributesPersistent.newInstance();
		result.setText(text);
		result.setAggregatedAssociation(aggregated);
		return result;
	}

	/** Asserts the paths of the detail and summary queries' left outer joins from bean, in order. */
	private static void assertJoins(DocumentQueryListModel<Bean> model, String... paths) {
		for (DocumentQuery query : List.of(Objects.requireNonNull(model.getDetailQuery()), Objects.requireNonNull(model.getSummaryQuery()))) {
			assertEquals(List.of(paths), joins(query));
		}
	}

	/** The paths of the query's left outer joins from bean, in order. */
	private static List<String> joins(DocumentQuery query) {
		String hql = ((AbstractDocumentQuery) query).toQueryString();
		List<String> result = new ArrayList<>();
		Matcher matcher = LEFT_OUTER_JOIN.matcher(hql);
		while (matcher.find()) {
			result.add(matcher.group(1));
		}
		return result;
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

		DocumentQueryListModel<Bean> model = defaultQueryModel(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		matchAny(model,
					f -> f.addEquals(AllAttributesPersistent.embeddedAssociationPropertyName + '.' + AllAttributesEmbedded.textPropertyName, "inside"),
					f -> f.addEquals(AllAttributesPersistent.textPropertyName, "without"));
		assertEquals(Set.of("with", "without"), fetch(model, AllAttributesPersistent.textPropertyName));
		assertNoJoin(model.getDetailQuery());
	}

	private static void assertNoJoin(DocumentQuery query) {
		assertNoJoin(query, "LEFT OUTER JOIN");
	}

	private static void assertNoJoin(DocumentQuery query, String join) {
		String hql = ((AbstractDocumentQuery) Objects.requireNonNull(query)).toQueryString();
		assertFalse(hql.contains(join), hql);
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
		DocumentQueryListModel<Bean> model = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE);
		model.addParameters(model.getDrivingDocument(), List.of(parameter), null);
		return fetch(model, ModelLookupFixture.namePropertyName);
	}

	private static Set<String> fixtures(Consumer<Filter> filtering) throws Exception {
		DocumentQueryListModel<Bean> model = defaultQueryModel(KitchensinkDocument.MODEL_LOOKUP_FIXTURE);
		filtering.accept(model.getFilter());
		return fetch(model, ModelLookupFixture.namePropertyName);
	}

	private static Set<String> inverses(Consumer<Filter> filtering) throws Exception {
		DocumentQueryListModel<Bean> model = defaultQueryModel(TestDocument.INVERSE_ONE_TO_ONE_PERSISTENT);
		filtering.accept(model.getFilter());
		return fetch(model, Bean.DOCUMENT_ID);
	}

	private static DocumentQueryListModel<Bean> defaultQueryModel(ModuleDocument queried) {
		return defaultQueryModel(queried, null);
	}

	/** The document's default query, from the given from clause if there is one. */
	private static DocumentQueryListModel<Bean> defaultQueryModel(ModuleDocument queried, String from) {
		Customer customer = CORE.getCustomer();
		Module module = queried.toModule(customer);
		Document document = queried.toDocument(customer);
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
		return names(model.fetch(), binding);
	}

	private static Set<String> names(Page page, String binding) {
		Set<String> result = new TreeSet<>();
		for (Bean row : Objects.requireNonNull(page.getRows())) {
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
