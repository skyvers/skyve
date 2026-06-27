package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.access.ViewSingularUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewQueryAggregateUserAccessMetaData;

class FluentViewAccessTest {

	// --- FluentViewSingularAccess ---

	@Test
	@SuppressWarnings("static-method")
	void singularAccessDefaultConstructorCreatesInstance() {
		FluentViewSingularAccess a = new FluentViewSingularAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void singularAccessWrappingConstructorUsesProvided() {
		ViewSingularUserAccessMetaData md = new ViewSingularUserAccessMetaData();
		FluentViewSingularAccess a = new FluentViewSingularAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void singularAccessDocumentNameReturnsSelf() {
		FluentViewSingularAccess a = new FluentViewSingularAccess();
		FluentViewSingularAccess result = a.documentName("TestDoc");
		assertSame(a, result);
		assertEquals("TestDoc", a.get().getDocumentName());
	}

	// --- FluentViewQueryAggregateAccess ---

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessDefaultConstructorCreatesInstance() {
		FluentViewQueryAggregateAccess a = new FluentViewQueryAggregateAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessWrappingConstructorUsesProvided() {
		ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
		FluentViewQueryAggregateAccess a = new FluentViewQueryAggregateAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessQueryNameReturnsSelf() {
		FluentViewQueryAggregateAccess a = new FluentViewQueryAggregateAccess();
		FluentViewQueryAggregateAccess result = a.queryName("TestQuery");
		assertSame(a, result);
		assertEquals("TestQuery", a.get().getQueryName());
	}
}
