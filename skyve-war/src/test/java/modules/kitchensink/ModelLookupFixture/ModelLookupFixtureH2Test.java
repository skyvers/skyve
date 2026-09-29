package modules.kitchensink.ModelLookupFixture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

import modules.kitchensink.domain.LookupDescription;
import modules.kitchensink.domain.ModelLookupFixture;
import modules.kitchensink.domain.ModelLookupRow;
import util.AbstractH2Test;

/** Exercises persisted selections and both list sources used by the SC/PF fixture. */
@SuppressWarnings("static-method")
class ModelLookupFixtureH2Test extends AbstractH2Test {
	@Test
	void modelAndSavedSelectionsListFetchPersistedLookupValues() throws Exception {
		Persistence persistence = CORE.getPersistence();
		LookupDescription value = LookupDescription.newInstance();
		value.setDescription("Model lookup fixture value");
		value = persistence.save(value);
		LookupDescription singleValue = LookupDescription.newInstance();
		singleValue.setDescription("Single-column lookup fixture value");
		singleValue = persistence.save(singleValue);

		ModelLookupFixture fixture = ModelLookupFixture.newInstance();
		fixture.setName("Saved model lookup test");
		fixture.setSelection(value);
		fixture.setSingleSelection(singleValue);
		ModelLookupRow row = ModelLookupRow.newInstance();
		row.setSelection(value);
		row.setSingleSelection(singleValue);
		fixture.addRowsElement(row);
		fixture = persistence.save(fixture);
		persistence.flush();
		persistence.evictAllCached();
		fixture = java.util.Objects.requireNonNull(persistence.retrieve(ModelLookupFixture.MODULE_NAME,
				ModelLookupFixture.DOCUMENT_NAME, fixture.getBizId()));
		assertEquals(value.getBizId(), fixture.getSelection().getBizId());
		assertEquals(singleValue.getBizId(), fixture.getSingleSelection().getBizId());
		assertEquals(singleValue.getBizId(), fixture.getRows().get(0).getSingleSelection().getBizId());
		assertTrue(fixture.getRows().get(0).isPersisted());
		assertEquals(value.getBizId(), fixture.getRows().get(0).getSelection().getBizId());

		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(ModelLookupFixture.MODULE_NAME);
		Document document = module.getDocument(customer, ModelLookupFixture.DOCUMENT_NAME);
		ListModel<Bean> model = document.getListModel(customer, "LookupValuesModel", true);
		model.setBean(fixture);
		model.setStartRow(0);
		model.setEndRow(10);
		model.getFilter().addEquals(Bean.DOCUMENT_ID, value.getBizId());
		Page values = model.fetch();
		assertEquals(1L, values.getTotalRows());
		assertEquals(value.getDescription(), Binder.get(values.getRows().get(0), LookupDescription.descriptionPropertyName));

		DocumentQueryListModel<Bean> selections = new DocumentQueryListModel<>(module.getNullSafeMetaDataQuery("qModelLookupFixtures"));
		selections.postConstruct(customer, true);
		selections.setStartRow(0);
		selections.setEndRow(10);
		selections.getFilter().addEquals(Bean.DOCUMENT_ID, fixture.getBizId());
		Page saved = selections.fetch();
		assertEquals(1L, saved.getTotalRows());
		assertEquals(value.getDescription(), Binder.get(saved.getRows().get(0), "selection.description"));
		assertEquals(singleValue.getDescription(), Binder.get(saved.getRows().get(0), "singleSelection.description"));
	}
}
