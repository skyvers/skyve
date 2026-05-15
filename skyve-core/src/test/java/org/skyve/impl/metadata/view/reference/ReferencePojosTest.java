package org.skyve.impl.metadata.view.reference;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.reference.ReferenceTarget.ReferenceTargetType;
import org.skyve.metadata.controller.ImplicitActionName;

/**
 * Tests for all simple reference POJO getter/setter methods not covered by existing tests.
 */
@SuppressWarnings("static-method")
class ReferencePojosTest {

	// ---- ActionReference ----

	@Test
	void actionReferenceSetActionNameRoundtrip() {
		ActionReference ref = new ActionReference();
		ref.setActionName("Save");
		assertThat(ref.getActionName(), is("Save"));
	}

	@Test
	void actionReferenceBlankActionNameBecomesNull() {
		ActionReference ref = new ActionReference();
		ref.setActionName("  ");
		assertNull(ref.getActionName());
	}

	@Test
	void actionReferenceDefaultActionNameIsNull() {
		assertNull(new ActionReference().getActionName());
	}

	// ---- DefaultListViewReference ----

	@Test
	void defaultListViewReferenceSetModuleNameRoundtrip() {
		DefaultListViewReference ref = new DefaultListViewReference();
		ref.setModuleName("admin");
		assertThat(ref.getModuleName(), is("admin"));
	}

	@Test
	void defaultListViewReferenceBlankModuleNameBecomesNull() {
		DefaultListViewReference ref = new DefaultListViewReference();
		ref.setModuleName("  ");
		assertNull(ref.getModuleName());
	}

	@Test
	void defaultListViewReferenceSetDocumentNameRoundtrip() {
		DefaultListViewReference ref = new DefaultListViewReference();
		ref.setDocumentName("User");
		assertThat(ref.getDocumentName(), is("User"));
	}

	@Test
	void defaultListViewReferenceBlankDocumentNameBecomesNull() {
		DefaultListViewReference ref = new DefaultListViewReference();
		ref.setDocumentName("  ");
		assertNull(ref.getDocumentName());
	}

	// ---- EditViewReference ----

	@Test
	void editViewReferenceSetModuleNameRoundtrip() {
		EditViewReference ref = new EditViewReference();
		ref.setModuleName("test");
		assertThat(ref.getModuleName(), is("test"));
	}

	@Test
	void editViewReferenceBlankModuleNameBecomesNull() {
		EditViewReference ref = new EditViewReference();
		ref.setModuleName("  ");
		assertNull(ref.getModuleName());
	}

	@Test
	void editViewReferenceSetDocumentNameRoundtrip() {
		EditViewReference ref = new EditViewReference();
		ref.setDocumentName("Contact");
		assertThat(ref.getDocumentName(), is("Contact"));
	}

	@Test
	void editViewReferenceBlankDocumentNameBecomesNull() {
		EditViewReference ref = new EditViewReference();
		ref.setDocumentName("  ");
		assertNull(ref.getDocumentName());
	}

	// ---- ExternalReference ----

	@Test
	void externalReferenceSetHrefRoundtrip() {
		ExternalReference ref = new ExternalReference();
		ref.setHref("https://example.com");
		assertThat(ref.getHref(), is("https://example.com"));
	}

	@Test
	void externalReferenceBlankHrefBecomesNull() {
		ExternalReference ref = new ExternalReference();
		ref.setHref("  ");
		assertNull(ref.getHref());
	}

	@Test
	void externalReferenceDefaultHrefIsNull() {
		assertNull(new ExternalReference().getHref());
	}

	// ---- ImplicitActionReference ----

	@Test
	void implicitActionReferenceSetImplicitActionNameRoundtrip() {
		ImplicitActionReference ref = new ImplicitActionReference();
		ref.setImplicitActionName(ImplicitActionName.Save);
		assertThat(ref.getImplicitActionName(), is(ImplicitActionName.Save));
	}

	@Test
	void implicitActionReferenceDefaultImplicitActionNameIsNull() {
		assertNull(new ImplicitActionReference().getImplicitActionName());
	}

	// ---- QueryListViewReference ----

	@Test
	void queryListViewReferenceSetQueryNameRoundtrip() {
		QueryListViewReference ref = new QueryListViewReference();
		ref.setQueryName("qAllContacts");
		assertThat(ref.getQueryName(), is("qAllContacts"));
	}

	@Test
	void queryListViewReferenceBlankQueryNameBecomesNull() {
		QueryListViewReference ref = new QueryListViewReference();
		ref.setQueryName("  ");
		assertNull(ref.getQueryName());
	}

	@Test
	void queryListViewReferenceDefaultQueryNameIsNull() {
		assertNull(new QueryListViewReference().getQueryName());
	}

	// ---- ResourceReference ----

	@Test
	void resourceReferenceSetRelativeFileRoundtrip() {
		ResourceReference ref = new ResourceReference();
		ref.setRelativeFile("images/logo.png");
		assertThat(ref.getRelativeFile(), is("images/logo.png"));
	}

	@Test
	void resourceReferenceDefaultRelativeFileIsNull() {
		assertNull(new ResourceReference().getRelativeFile());
	}

	// ---- ReferenceTarget ----

	@Test
	void referenceTargetSetTypeRoundtrip() {
		ReferenceTarget rt = new ReferenceTarget();
		rt.setType(ReferenceTargetType.modalWindow);
		assertThat(rt.getType(), is(ReferenceTargetType.modalWindow));
	}

	@Test
	void referenceTargetSetNameRoundtrip() {
		ReferenceTarget rt = new ReferenceTarget();
		rt.setName("myFrame");
		assertThat(rt.getName(), is("myFrame"));
	}

	@Test
	void referenceTargetBlankNameBecomesNull() {
		ReferenceTarget rt = new ReferenceTarget();
		rt.setName("  ");
		assertNull(rt.getName());
	}

	@Test
	void referenceTargetDefaultTypeIsNull() {
		assertNull(new ReferenceTarget().getType());
	}

	@Test
	void referenceTargetReferenceTargetTypeValues() {
		ReferenceTargetType[] values = ReferenceTargetType.values();
		assertThat(values.length, is(5));
	}

	// ---- ReferenceProcessor unknown type ----

	@Test
	void referenceProcessorThrowsForUnknownReferenceType() {
		ReferenceProcessor processor = new ReferenceProcessor() {
			@Override public void processActionReference(ActionReference r) {}
			@Override public void processContentReference(ContentReference r) {}
			@Override public void processDefaultListViewReference(DefaultListViewReference r) {}
			@Override public void processEditViewReference(EditViewReference r) {}
			@Override public void processExternalReference(ExternalReference r) {}
			@Override public void processImplicitActionReference(ImplicitActionReference r) {}
			@Override public void processQueryListViewReference(QueryListViewReference r) {}
			@Override public void processReportReference(ReportReference r) {}
			@Override public void processResourceReference(ResourceReference r) {}
		};
		// anonymous inner class implementing Reference is an unknown type
		Reference unknown = new Reference() {
			private static final long serialVersionUID = 1L;
		};
		assertThrows(IllegalStateException.class, () -> processor.process(unknown));
	}
}
