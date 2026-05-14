package org.skyve.impl.metadata.view.reference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class ReferenceProcessorTest {

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
	public void setUp() {
		processor = new TrackingReferenceProcessor();
	}

	@Test
	public void processDispatchesActionReference() {
		processor.process(new ActionReference());
		assertEquals("action", processor.lastDispatched);
	}

	@Test
	public void processDispatchesContentReference() {
		processor.process(new ContentReference());
		assertEquals("content", processor.lastDispatched);
	}

	@Test
	public void processDispatchesDefaultListViewReference() {
		processor.process(new DefaultListViewReference());
		assertEquals("defaultListView", processor.lastDispatched);
	}

	@Test
	public void processDispatchesEditViewReference() {
		processor.process(new EditViewReference());
		assertEquals("editView", processor.lastDispatched);
	}

	@Test
	public void processDispatchesExternalReference() {
		processor.process(new ExternalReference());
		assertEquals("external", processor.lastDispatched);
	}

	@Test
	public void processDispatchesImplicitActionReference() {
		processor.process(new ImplicitActionReference());
		assertEquals("implicitAction", processor.lastDispatched);
	}

	@Test
	public void processDispatchesQueryListViewReference() {
		processor.process(new QueryListViewReference());
		assertEquals("queryListView", processor.lastDispatched);
	}

	@Test
	public void processDispatchesReportReference() {
		processor.process(new ReportReference());
		assertEquals("report", processor.lastDispatched);
	}

	@Test
	public void processDispatchesResourceReference() {
		processor.process(new ResourceReference());
		assertEquals("resource", processor.lastDispatched);
	}

	@Test
	public void processNullReferenceDoesNotDispatch() {
		processor.process(null);
		assertNull(processor.lastDispatched);
	}
}
