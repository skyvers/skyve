package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;

/**
 * Tests for NullableBeanVisitor constructors.
 */
class NullableBeanVisitorTest {

	// Minimal concrete subclass for testing
	private static class TestVisitor extends NullableBeanVisitor {
		TestVisitor(boolean visitInverses, boolean vectorCyclicDetection) {
			super(visitInverses, vectorCyclicDetection);
		}

		TestVisitor(boolean visitInverses, boolean vectorCyclicDetection, boolean acceptVisited) {
			super(visitInverses, vectorCyclicDetection, acceptVisited);
		}

		@Override
		protected boolean acceptNulls(String binding, Document document, Document owningDocument, Relation owningRelation, Bean bean) {
			return true;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorTwoArgsCreatesInstance() {
		NullableBeanVisitor visitor = new TestVisitor(false, false);
		assertNotNull(visitor);
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorThreeArgsCreatesInstance() {
		NullableBeanVisitor visitor = new TestVisitor(false, false, false);
		assertNotNull(visitor);
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorWithVisitInversesCreatesInstance() {
		NullableBeanVisitor visitor = new TestVisitor(true, true);
		assertNotNull(visitor);
	}
}
