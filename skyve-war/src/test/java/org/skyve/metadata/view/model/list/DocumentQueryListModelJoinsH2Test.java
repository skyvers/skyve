package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

import modules.kitchensink.domain.LookupDescription;
import modules.kitchensink.domain.ModelLookupFixture;
import modules.test.domain.InverseOneToOnePersistent;
import util.AbstractH2Test;

/**
 * A document's default query (whose columns join nothing) filtered through an optional association or an inverse one:
 * the filters that also match a row with no value keep the rows with nothing on the path, rather than the inner join
 * of the path in HQL dropping them.
 */
@SuppressWarnings("static-method")
class DocumentQueryListModelJoinsH2Test extends AbstractH2Test {
	private static final String DESCRIPTION = ModelLookupFixture.selectionPropertyName + '.' + LookupDescription.descriptionPropertyName;
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
		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		DocumentQueryListModel<Bean> result = new DocumentQueryListModel<>(module.getDocumentDefaultQuery(customer, document.getName()));
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
