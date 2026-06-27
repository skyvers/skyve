package org.skyve.impl.metadata.view.reference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.web.UserAgentType;

class ReferenceProcessorTest {

	/** Concrete ReferenceProcessor that records which dispatch method was called */
	private static class TrackingReferenceProcessor extends ReferenceProcessor {
		String lastDispatched;

		@Override
		public void processActionReference(ActionReference reference) {
			lastDispatched = "action";
		}

		@Override
		public void processContentReference(ContentReference reference) {
			lastDispatched = "content";
		}

		@Override
		public void processDefaultListViewReference(DefaultListViewReference reference) {
			lastDispatched = "defaultListView";
		}

		@Override
		public void processEditViewReference(EditViewReference reference) {
			lastDispatched = "editView";
		}

		@Override
		public void processExternalReference(ExternalReference reference) {
			lastDispatched = "external";
		}

		@Override
		public void processImplicitActionReference(ImplicitActionReference reference) {
			lastDispatched = "implicitAction";
		}

		@Override
		public void processQueryListViewReference(QueryListViewReference reference) {
			lastDispatched = "queryListView";
		}

		@Override
		public void processReportReference(ReportReference reference) {
			lastDispatched = "report";
		}

		@Override
		public void processResourceReference(ResourceReference reference) {
			lastDispatched = "resource";
		}
	}

	private TrackingReferenceProcessor processor;

	@BeforeEach
	void setUp() {
		processor = new TrackingReferenceProcessor();
	}

	@Test
	void processDispatchesActionReference() {
		processor.process(new ActionReference());
		assertEquals("action", processor.lastDispatched);
	}

	@Test
	void processDispatchesContentReference() {
		processor.process(new ContentReference());
		assertEquals("content", processor.lastDispatched);
	}

	@Test
	void processDispatchesDefaultListViewReference() {
		processor.process(new DefaultListViewReference());
		assertEquals("defaultListView", processor.lastDispatched);
	}

	@Test
	void processDispatchesEditViewReference() {
		processor.process(new EditViewReference());
		assertEquals("editView", processor.lastDispatched);
	}

	@Test
	void processDispatchesExternalReference() {
		processor.process(new ExternalReference());
		assertEquals("external", processor.lastDispatched);
	}

	@Test
	void processDispatchesImplicitActionReference() {
		processor.process(new ImplicitActionReference());
		assertEquals("implicitAction", processor.lastDispatched);
	}

	@Test
	void processDispatchesQueryListViewReference() {
		processor.process(new QueryListViewReference());
		assertEquals("queryListView", processor.lastDispatched);
	}

	@Test
	void processDispatchesReportReference() {
		processor.process(new ReportReference());
		assertEquals("report", processor.lastDispatched);
	}

	@Test
	void processDispatchesResourceReference() {
		processor.process(new ResourceReference());
		assertEquals("resource", processor.lastDispatched);
	}

	@Test
	void processNullReferenceDoesNotDispatch() {
		processor.process(null);
		assertNull(processor.lastDispatched);
	}

	@Test
	void processUnknownReferenceTypeThrowsIllegalStateException() {
		Reference unknown = new Reference() {
			private static final long serialVersionUID = 1L;
		};
		org.junit.jupiter.api.Assertions.assertThrows(
			IllegalStateException.class,
			() -> processor.process(unknown)
		);
	}

	@Test
	@SuppressWarnings("static-method")
	void obtainActionForActionReferenceReturnsCreateActionWhenEditMissing() {
		ActionReference ref = new ActionReference();
		ref.setActionName("approve");

		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute attribute = mock(Attribute.class);
		View createView = mock(View.class);
		Action action = mock(Action.class);

		when(document.getAttribute("status")).thenReturn(attribute);
		when(document.getExtends()).thenReturn(null);
		when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		doReturn(String.class).when(attribute).getImplementingType();
		when(document.getView(UserAgentType.desktop.name(), customer, "edit")).thenThrow(new MetaDataException("No edit view"));
		when(document.getView(UserAgentType.desktop.name(), customer, "create")).thenReturn(createView);
		when(createView.getAction("approve")).thenReturn(action);

		Action result = ReferenceProcessor.obtainActionForActionReference(ref, customer, module, document, "status", UserAgentType.desktop);

		assertEquals(action, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void obtainActionForActionReferenceReturnsNullWhenNoMatchingAction() {
		ActionReference ref = new ActionReference();
		ref.setActionName("approve");

		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute attribute = mock(Attribute.class);
		View editView = mock(View.class);

		when(document.getAttribute("status")).thenReturn(attribute);
		when(document.getExtends()).thenReturn(null);
		when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		doReturn(String.class).when(attribute).getImplementingType();
		when(document.getView(UserAgentType.desktop.name(), customer, "edit")).thenReturn(editView);
		when(editView.getAction("approve")).thenReturn(null);
		when(document.getView(UserAgentType.desktop.name(), customer, "create")).thenThrow(new MetaDataException("No create view"));

		Action result = ReferenceProcessor.obtainActionForActionReference(ref, customer, module, document, "status", UserAgentType.desktop);

		assertNull(result);
	}
}
