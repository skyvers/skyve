package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.HashSet;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.access.ViewContentUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewDocumentAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewDynamicImageUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewModelAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewPreviousCompleteUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewQueryAggregateUserAccessMetaData;

/**
 * Tests for the smaller view access fluent builder types:
 * Content, DocumentAggregate, DynamicImage, ModelAggregate, PreviousComplete, QueryAggregate.
 */
@SuppressWarnings("static-method")
class FluentViewSmallAccessTest {

	// ---- FluentViewContentAccess -----------------------------------------

	@Test
	void contentDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentViewContentAccess().get());
	}

	@Test
	void contentWrappingConstructorPreservesInstance() {
		ViewContentUserAccessMetaData md = new ViewContentUserAccessMetaData();
		FluentViewContentAccess fa = new FluentViewContentAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void contentBindingSetsValue() {
		FluentViewContentAccess fa = new FluentViewContentAccess().binding("photo");
		assertEquals("photo", fa.get().getBinding());
	}

	@Test
	void contentFromSetsAllFields() {
		FluentViewContentAccess fa = new FluentViewContentAccess().from("photo", new HashSet<>());
		assertEquals("photo", fa.get().getBinding());
	}

	// ---- FluentViewDocumentAggregateAccess --------------------------------

	@Test
	void documentAggregateDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentViewDocumentAggregateAccess().get());
	}

	@Test
	void documentAggregateWrappingConstructorPreservesInstance() {
		ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
		FluentViewDocumentAggregateAccess fa = new FluentViewDocumentAggregateAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void documentAggregateDocumentNameSetsValue() {
		FluentViewDocumentAggregateAccess fa = new FluentViewDocumentAggregateAccess().documentName("Contact");
		assertEquals("Contact", fa.get().getDocumentName());
	}

	@Test
	void documentAggregateFromSetsAllFields() {
		FluentViewDocumentAggregateAccess fa = new FluentViewDocumentAggregateAccess().from("Contact", new HashSet<>());
		assertEquals("Contact", fa.get().getDocumentName());
	}

	// ---- FluentViewDynamicImageAccess ------------------------------------

	@Test
	void dynamicImageDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentViewDynamicImageAccess().get());
	}

	@Test
	void dynamicImageWrappingConstructorPreservesInstance() {
		ViewDynamicImageUserAccessMetaData md = new ViewDynamicImageUserAccessMetaData();
		FluentViewDynamicImageAccess fa = new FluentViewDynamicImageAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void dynamicImageImageNameSetsValue() {
		FluentViewDynamicImageAccess fa = new FluentViewDynamicImageAccess().imageName("avatar");
		assertEquals("avatar", fa.get().getImageName());
	}

	@Test
	void dynamicImageFromSetsAllFields() {
		FluentViewDynamicImageAccess fa = new FluentViewDynamicImageAccess().from("avatar", new HashSet<>());
		assertEquals("avatar", fa.get().getImageName());
	}

	// ---- FluentViewModelAggregateAccess ----------------------------------

	@Test
	void modelAggregateDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentViewModelAggregateAccess().get());
	}

	@Test
	void modelAggregateWrappingConstructorPreservesInstance() {
		ViewModelAggregateUserAccessMetaData md = new ViewModelAggregateUserAccessMetaData();
		FluentViewModelAggregateAccess fa = new FluentViewModelAggregateAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void modelAggregateModelNameSetsValue() {
		FluentViewModelAggregateAccess fa = new FluentViewModelAggregateAccess().modelName("MyModel");
		assertEquals("MyModel", fa.get().getModelName());
	}

	@Test
	void modelAggregateFromSetsAllFields() {
		FluentViewModelAggregateAccess fa = new FluentViewModelAggregateAccess().from("MyModel", new HashSet<>());
		assertEquals("MyModel", fa.get().getModelName());
	}

	// ---- FluentViewPreviousCompleteAccess --------------------------------

	@Test
	void previousCompleteDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentViewPreviousCompleteAccess().get());
	}

	@Test
	void previousCompleteWrappingConstructorPreservesInstance() {
		ViewPreviousCompleteUserAccessMetaData md = new ViewPreviousCompleteUserAccessMetaData();
		FluentViewPreviousCompleteAccess fa = new FluentViewPreviousCompleteAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void previousCompleteBindingSetsValue() {
		FluentViewPreviousCompleteAccess fa = new FluentViewPreviousCompleteAccess().binding("status");
		assertEquals("status", fa.get().getBinding());
	}

	@Test
	void previousCompleteFromSetsAllFields() {
		FluentViewPreviousCompleteAccess fa = new FluentViewPreviousCompleteAccess().from("status", new HashSet<>());
		assertEquals("status", fa.get().getBinding());
	}

	// ---- FluentViewQueryAggregateAccess ----------------------------------

	@Test
	void queryAggregateDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentViewQueryAggregateAccess().get());
	}

	@Test
	void queryAggregateWrappingConstructorPreservesInstance() {
		ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
		FluentViewQueryAggregateAccess fa = new FluentViewQueryAggregateAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void queryAggregateQueryNameSetsValue() {
		FluentViewQueryAggregateAccess fa = new FluentViewQueryAggregateAccess().queryName("qContacts");
		assertEquals("qContacts", fa.get().getQueryName());
	}

	@Test
	void queryAggregateFromSetsAllFields() {
		FluentViewQueryAggregateAccess fa = new FluentViewQueryAggregateAccess().from("qContacts", new HashSet<>());
		assertEquals("qContacts", fa.get().getQueryName());
	}
}
