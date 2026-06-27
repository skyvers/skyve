package org.skyve.impl.metadata.model.document;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.DynamicChildBean;
import org.skyve.domain.DynamicHierarchicalBean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.DynamicPersistentChildBean;
import org.skyve.domain.DynamicPersistentHierarchicalBean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View;

class DocumentImplTest {

	private DocumentImpl doc;

	@BeforeEach
	void setUp() {
		doc = new DocumentImpl();
	}

	// ----------------------------------------------------------------
	// lastModifiedMillis
	// ----------------------------------------------------------------

	@Test
	void getLastModifiedMillisDefaultsToMaxValue() {
		assertEquals(Long.MAX_VALUE, doc.getLastModifiedMillis());
	}

	@Test
	void setLastModifiedMillisRoundTrips() {
		doc.setLastModifiedMillis(12345L);
		assertEquals(12345L, doc.getLastModifiedMillis());
	}

	// ----------------------------------------------------------------
	// lastCheckedMillis
	// ----------------------------------------------------------------

	@Test
	void getLastCheckedMillisIsInitialised() {
		assertTrue(doc.getLastCheckedMillis() > 0);
	}

	@Test
	void setLastCheckedMillisRoundTrips() {
		doc.setLastCheckedMillis(99999L);
		assertEquals(99999L, doc.getLastCheckedMillis());
	}

	// ----------------------------------------------------------------
	// dynamic bean class selection
	// ----------------------------------------------------------------

	@Test
	void getBeanClassReturnsDynamicBeanForDynamicRootDocument() throws Exception {
		makeDynamicDocument("Demo");

		assertEquals(DynamicBean.class, doc.getBeanClass(mock(Customer.class)));
	}

	@Test
	void getBeanClassReturnsDynamicPersistentBeanForPersistentDynamicRootDocument() throws Exception {
		makeDynamicDocument("Demo");
		doc.setPersistent(new Persistent());

		assertEquals(DynamicPersistentBean.class, doc.getBeanClass(mock(Customer.class)));
	}

	@Test
	void getBeanClassReturnsDynamicHierarchicalBeanForSelfParentDocument() throws Exception {
		makeDynamicDocument("Tree");
		doc.setParentDocumentName("Tree");

		assertEquals(DynamicHierarchicalBean.class, doc.getBeanClass(mock(Customer.class)));
	}

	@Test
	void getBeanClassReturnsDynamicPersistentHierarchicalBeanForPersistentSelfParentDocument() throws Exception {
		makeDynamicDocument("Tree");
		doc.setParentDocumentName("Tree");
		doc.setPersistent(new Persistent());

		assertEquals(DynamicPersistentHierarchicalBean.class, doc.getBeanClass(mock(Customer.class)));
	}

	@Test
	void getBeanClassReturnsDynamicChildBeanForDifferentParentDocument() throws Exception {
		makeDynamicDocument("Line");
		doc.setParentDocumentName("Header");

		assertEquals(DynamicChildBean.class, doc.getBeanClass(mock(Customer.class)));
	}

	@Test
	void getBeanClassReturnsDynamicPersistentChildBeanForPersistentDifferentParentDocument() throws Exception {
		makeDynamicDocument("Line");
		doc.setParentDocumentName("Header");
		doc.setPersistent(new Persistent());

		assertEquals(DynamicPersistentChildBean.class, doc.getBeanClass(mock(Customer.class)));
	}

	// ----------------------------------------------------------------
	// parentDocumentName
	// ----------------------------------------------------------------

	@Test
	void getParentDocumentNameDefaultsToNull() {
		assertThat(doc.getParentDocumentName(), nullValue());
	}

	@Test
	void setParentDocumentNameRoundTrips() {
		doc.setParentDocumentName("Contact");
		assertThat(doc.getParentDocumentName(), is("Contact"));
	}

	// ----------------------------------------------------------------
	// parentDatabaseIndex
	// ----------------------------------------------------------------

	@Test
	void getParentDatabaseIndexDefaultsToNull() {
		assertThat(doc.getParentDatabaseIndex(), nullValue());
	}

	@Test
	void setParentDatabaseIndexRoundTrips() {
		doc.setParentDatabaseIndex(Boolean.TRUE);
		assertThat(doc.getParentDatabaseIndex(), is(Boolean.TRUE));
	}

	// ----------------------------------------------------------------
	// bizKeyMethodCode
	// ----------------------------------------------------------------

	@Test
	void getBizKeyMethodCodeDefaultsToNull() {
		assertThat(doc.getBizKeyMethodCode(), nullValue());
	}

	@Test
	void setBizKeyMethodCodeRoundTrips() {
		doc.setBizKeyMethodCode("return getName();");
		assertThat(doc.getBizKeyMethodCode(), is("return getName();"));
	}

	// ----------------------------------------------------------------
	// bizKeyExpression
	// ----------------------------------------------------------------

	@Test
	void getBizKeyExpressionDefaultsToNull() {
		assertThat(doc.getBizKeyExpression(), nullValue());
	}

	@Test
	void setBizKeyExpressionRoundTrips() {
		doc.setBizKeyExpression("{name}");
		assertThat(doc.getBizKeyExpression(), is("{name}"));
	}

	// ----------------------------------------------------------------
	// bizKeySensitity
	// ----------------------------------------------------------------

	@Test
	void getBizKeySensitityDefaultsToNull() {
		assertThat(doc.getBizKeySensitity(), nullValue());
	}

	@Test
	void setBizKeySensitityRoundTrips() {
		doc.setBizKeySensitity(Sensitivity.confidential);
		assertThat(doc.getBizKeySensitity(), is(Sensitivity.confidential));
	}

	// ----------------------------------------------------------------
	// ordered
	// ----------------------------------------------------------------

	@Test
	void isOrderedDefaultsToFalse() {
		assertFalse(doc.isOrdered());
	}

	@Test
	void setOrderedRoundTrips() {
		doc.setOrdered(true);
		assertTrue(doc.isOrdered());
	}

	// ----------------------------------------------------------------
	// documentation
	// ----------------------------------------------------------------

	@Test
	void getDocumentationDefaultsToNull() {
		assertThat(doc.getDocumentation(), nullValue());
	}

	@Test
	void setDocumentationRoundTrips() {
		doc.setDocumentation("Some docs.");
		assertThat(doc.getDocumentation(), is("Some docs."));
	}

	// ----------------------------------------------------------------
	// properties
	// ----------------------------------------------------------------

	@Test
	void getPropertiesInitiallyEmpty() {
		assertTrue(doc.getProperties().isEmpty());
	}

	@Test
	void propertiesMapIsMutable() {
		Map<String, String> props = doc.getProperties();
		props.put("key", "value");
		assertThat(doc.getProperties(), hasKey("key"));
	}

	// ----------------------------------------------------------------
	// definedActionNames
	// ----------------------------------------------------------------

	@Test
	void getDefinedActionNamesInitiallyEmpty() {
		assertThat(doc.getDefinedActionNames(), empty());
	}

	@Test
	void definedActionNamesCanBePopulated() {
		Set<String> names = doc.getDefinedActionNames();
		names.add("Save");
		names.add("Delete");
		assertThat(doc.getDefinedActionNames(), containsInAnyOrder("Delete", "Save"));
	}

	// ----------------------------------------------------------------
	// uniqueConstraints
	// ----------------------------------------------------------------

	@Test
	void getUniqueConstraintsInitiallyEmpty() {
		assertThat(doc.getUniqueConstraints(), empty());
	}

	@Test
	void putUniqueConstraintAddsToList() {
		UniqueConstraintImpl uc = new UniqueConstraintImpl();
		uc.setName("uc1");
		doc.putUniqueConstraint(uc);
		assertEquals(1, doc.getUniqueConstraints().size());
	}

	@Test
	void getUniqueConstraintByNameReturnsMatchingConstraint() {
		UniqueConstraintImpl uc = new UniqueConstraintImpl();
		uc.setName("uc1");
		doc.putUniqueConstraint(uc);
		UniqueConstraint result = doc.getUniqueConstraint("uc1");
		assertThat(result, is((UniqueConstraint) uc));
	}

	@Test
	void getUniqueConstraintReturnsNullForUnknownName() {
		assertThat(doc.getUniqueConstraint("nope"), nullValue());
	}

	// ----------------------------------------------------------------
	// referenceNames / putRelation / getReferenceByName
	// ----------------------------------------------------------------

	@Test
	void getReferenceNamesInitiallyEmpty() {
		assertThat(doc.getReferenceNames(), empty());
	}

	@Test
	void putRelationExposesReferenceByName() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("contact");
		doc.putRelation(assoc);
		assertThat(doc.getReferenceByName("contact"), is(notNullValue()));
	}

	@Test
	void putRelationAddsToReferenceNames() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("contact");
		doc.putRelation(assoc);
		assertThat(doc.getReferenceNames(), containsInAnyOrder("contact"));
	}

	@Test
	void getReferenceByNameReturnsNullForUnknown() {
		assertThat(doc.getReferenceByName("unknown"), nullValue());
	}

	@Test
	void getRelatedDocumentThrowsWhenRelationMissing() {
		assertThrows(IllegalStateException.class, () -> doc.getRelatedDocument(mock(Customer.class), "missing"));
	}

	@Test
	void getRelatedDocumentThrowsWhenRelationHasNoDocumentName() {
		AssociationImpl association = new AssociationImpl();
		association.setName("contact");
		doc.putRelation(association);

		assertThrows(IllegalStateException.class, () -> doc.getRelatedDocument(mock(Customer.class), "contact"));
	}

	@Test
	void getRelatedDocumentReturnsDocumentFromOwningModule() {
		doc.setOwningModuleName("admin");
		AssociationImpl association = new AssociationImpl();
		association.setName("contact");
		association.setDocumentName("Contact");
		doc.putRelation(association);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document relatedDocument = mock(Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(relatedDocument);

		assertThat(doc.getRelatedDocument(customer, "contact"), is(relatedDocument));
	}

	@Test
	void repositoryBackedLookupsThrowWhenRepositoryReturnsNull() {
		doc.setName("Demo");
		ProvidedRepository repository = mock(ProvidedRepository.class);

		withRepository(repository, () -> {
			assertThrows(MetaDataException.class, () -> doc.getDynamicImage(null, "missingImage"));
			assertThrows(MetaDataException.class, () -> doc.getComparisonModel(null, "missingComparison", true));
			assertThrows(MetaDataException.class, () -> doc.getMapModel(null, "missingMap", true));
			assertThrows(MetaDataException.class, () -> doc.getChartModel(null, "missingChart", true));
			assertThrows(MetaDataException.class, () -> doc.getListModel(null, "missingList", true));
			assertThrows(MetaDataException.class, () -> doc.getServerSideAction(null, "missingAction", true));
			assertThrows(MetaDataException.class, () -> doc.getBizExportAction(null, "missingExport", true));
			assertThrows(MetaDataException.class, () -> doc.getBizImportAction(null, "missingImport", true));
			assertThrows(MetaDataException.class, () -> doc.getDownloadAction(null, "missingDownload", true));
			assertThrows(MetaDataException.class, () -> doc.getUploadAction(null, "missingUpload", true));
			assertThrows(MetaDataException.class, () -> doc.getView("desktop", null, "edit"));
		});
	}

	@Test
	void getViewFallsBackToEditViewForCreateView() {
		doc.setName("Demo");
		ProvidedRepository repository = mock(ProvidedRepository.class);
		View editView = mock(View.class);
		when(repository.getView("desktop", null, doc, "create")).thenReturn(null);
		when(repository.getView("desktop", null, doc, "edit")).thenReturn(editView);

		withRepository(repository, () -> assertThat(doc.getView("desktop", null, "create"), is(editView)));
	}

	// ----------------------------------------------------------------
	// conditions
	// ----------------------------------------------------------------

	@Test
	void getConditionsInitiallyEmpty() {
		assertTrue(doc.getConditions().isEmpty());
	}

	@Test
	void getConditionNamesInitiallyEmpty() {
		assertThat(doc.getConditionNames(), empty());
	}

	@Test
	void getConditionReturnsNullForUnknownName() {
		assertThat(doc.getCondition("nope"), nullValue());
	}

	@Test
	void getConditionReturnsInsertedCondition() {
		ConditionImpl cond = new ConditionImpl();
		cond.setExpression("true");
		doc.getConditions().put("myCondition", cond);
		Condition result = doc.getCondition("myCondition");
		assertThat(result, is((Condition) cond));
	}

	@Test
	void getConditionNamesReflectsInsertedCondition() {
		ConditionImpl cond = new ConditionImpl();
		cond.setExpression("false");
		doc.getConditions().put("active", cond);
		assertThat(doc.getConditionNames(), containsInAnyOrder("active"));
	}

	// ----------------------------------------------------------------
	// static factory attributes
	// ----------------------------------------------------------------

	@SuppressWarnings("static-method")
	@Test
	void getBizKeyAttributeReturnsBizKeyFieldWithCorrectName() {
		assertThat(DocumentImpl.getBizKeyAttribute().getName(), is(Bean.BIZ_KEY));
	}

	@SuppressWarnings("static-method")
	@Test
	void getBizKeyAttributeHasTextType() {
		assertThat(DocumentImpl.getBizKeyAttribute().getAttributeType(), is(AttributeType.text));
	}

	@SuppressWarnings("static-method")
	@Test
	void getBizOrdinalAttributeReturnsOrdinalFieldWithCorrectName() {
		assertThat(DocumentImpl.getBizOrdinalAttribute().getName(), is(Bean.ORDINAL_NAME));
	}

	@SuppressWarnings("static-method")
	@Test
	void getBizOrdinalAttributeHasIntegerType() {
		assertThat(DocumentImpl.getBizOrdinalAttribute().getAttributeType(), is(AttributeType.integer));
	}

	// ---- InverseOne.getCardinality ----

	@SuppressWarnings("static-method")
	@Test
	void inverseOneGetCardinalityReturnsOne() {
		assertThat(new InverseOne().getCardinality(), is(org.skyve.metadata.model.document.Inverse.InverseCardinality.one));
	}

	// ---- CollectionImpl.isRequired ----

	@SuppressWarnings("static-method")
	@Test
	void collectionImplIsRequiredWhenMinCardinalityAboveZero() {
		CollectionImpl coll = new CollectionImpl();
		coll.setMinCardinality(1);
		assertTrue(coll.isRequired());
	}

	// ---- AssociationImpl.setRequiredBool ----

	@SuppressWarnings("static-method")
	@Test
	void associationImplSetRequiredBoolSetsRequired() {
		AssociationImpl assoc = new AssociationImpl();
		assoc.setRequiredBool(Boolean.TRUE);
		assertTrue(assoc.isRequired());
	}

	private void makeDynamicDocument(String name) {
		doc.setOwningModuleName("test");
		doc.setName(name);
		doc.setDynamism(new Dynamic());
	}

	private static void withRepository(ProvidedRepository repository, Runnable assertions) {
		ProvidedRepository previous = ProvidedRepositoryFactory.isConfigured() ? ProvidedRepositoryFactory.get() : null;
		try {
			ProvidedRepositoryFactory.set(repository);
			assertions.run();
		}
		finally {
			if (previous == null) {
				ProvidedRepositoryFactory.clear();
			}
			else {
				ProvidedRepositoryFactory.set(previous);
			}
		}
	}
}
