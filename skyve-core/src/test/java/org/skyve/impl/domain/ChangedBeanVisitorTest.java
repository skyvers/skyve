package org.skyve.impl.domain;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Inverse;

@SuppressWarnings({ "static-method", "boxing" })
class ChangedBeanVisitorTest {

	@Test
	void acceptWithInverseNonCascadeReturnsFalse() {
		// Inverse relation with cascade != TRUE → accept returns false immediately
		ChangedBeanVisitor visitor = new ChangedBeanVisitor();
		Document doc = mock(Document.class);
		Bean bean = mock(Bean.class);
		Inverse inverse = mock(Inverse.class);
		when(inverse.getCascade()).thenReturn(null);

		boolean result = visitor.accept("binding", doc, doc, inverse, bean);

		assertFalse(result, "accept should return false for non-cascading Inverse");
		assertFalse(visitor.isChanged(), "isChanged should remain false");
	}

	@Test
	void acceptWithInverseCascadeTrueAndUnchangedBeanReturnsTrue() {
		// Inverse relation with cascade == TRUE → proceed; unchanged bean → true
		ChangedBeanVisitor visitor = new ChangedBeanVisitor();
		Document doc = mock(Document.class);
		Bean bean = mock(Bean.class);
		Inverse inverse = mock(Inverse.class);
		when(inverse.getCascade()).thenReturn(Boolean.TRUE);
		when(bean.isChanged()).thenReturn(Boolean.FALSE);

		boolean result = visitor.accept("binding", doc, doc, inverse, bean);

		assertTrue(result, "accept should return true for cascading Inverse with unchanged bean");
		assertFalse(visitor.isChanged());
	}

	@Test
	void acceptWithChangedBeanAndDirtyTraceSetsChanged() {
		// beanAccepted.isChanged() == true, DIRTY_TRACE == true → logs and sets changed
		ChangedBeanVisitor visitor = new ChangedBeanVisitor();
		Document doc = mock(Document.class);
		Bean bean = mock(Bean.class);
		when(bean.isChanged()).thenReturn(Boolean.TRUE);
		when(bean.toString()).thenReturn("MockBean");

		boolean saved = UtilImpl.DIRTY_TRACE;
		try {
			UtilImpl.DIRTY_TRACE = true;
			boolean result = visitor.accept("testBinding", doc, doc, null, bean);
			assertFalse(result, "accept should return false after detecting change");
			assertTrue(visitor.isChanged(), "isChanged should be true after changed bean");
		}
		finally {
			UtilImpl.DIRTY_TRACE = saved;
		}
	}
}
