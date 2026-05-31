package org.skyve.impl.web.faces.components;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;

class ListGridStaticTest {
	@Test
	void getBooleanObjectAttributeHandlesNullLiteralAndEvaluatedValues() {
		assertTrue(ListGrid.getBooleanObjectAttribute(null));
		assertTrue(ListGrid.getBooleanObjectAttribute("true"));
		assertTrue(ListGrid.getBooleanObjectAttribute(Boolean.TRUE));
		assertFalse(ListGrid.getBooleanObjectAttribute("false"));
		assertFalse(ListGrid.getBooleanObjectAttribute(Boolean.FALSE));
		assertFalse(ListGrid.getBooleanObjectAttribute("unexpected"));
	}

	@Test
	void getBooleanAttributeHandlesLiteralAndEvaluatedValues() {
		assertTrue(ListGrid.getBooleanAttribute("true"));
		assertTrue(ListGrid.getBooleanAttribute(Boolean.TRUE));
		assertFalse(ListGrid.getBooleanAttribute(null));
		assertFalse(ListGrid.getBooleanAttribute("false"));
		assertFalse(ListGrid.getBooleanAttribute(Boolean.FALSE));
		assertFalse(ListGrid.getBooleanAttribute("unexpected"));
	}

	@Test
	void newComponentBuilderAliasPathsThrowDomainExceptionWithoutFacesContext() {
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder(null));
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("faces"));
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("vue"));
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("java.lang.String"));
	}

	@Test
	void newComponentBuilderThrowsDomainExceptionForUnknownClass() {
		assertThrows(DomainException.class, () -> ListGrid.newComponentBuilder("org.skyve.DoesNotExist"));
	}
}
