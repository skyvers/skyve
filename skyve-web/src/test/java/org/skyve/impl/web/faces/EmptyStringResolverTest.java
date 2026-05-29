package org.skyve.impl.web.faces;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import jakarta.el.ELContext;

@SuppressWarnings("static-method")
class EmptyStringResolverTest {
	@Test
	void resolverMetadataAndReadOnlyMethodsReturnExpectedValues() {
		EmptyStringResolver resolver = new EmptyStringResolver();
		ELContext context = mock(ELContext.class);

		assertEquals(String.class, resolver.getCommonPropertyType(context, new Object()));
		assertNull(resolver.getType(context, new Object(), "p"));
		assertNull(resolver.getValue(context, new Object(), "p"));
		assertTrue(resolver.isReadOnly(context, new Object(), "p"));

		resolver.setValue(context, new Object(), "p", "v");
	}

	@Test
	void convertToTypeMarksContextResolvedForNullStringTargetsOnly() {
		EmptyStringResolver resolver = new EmptyStringResolver();
		ELContext context = mock(ELContext.class);

		assertNull(resolver.convertToType(context, null, String.class));
		verify(context).setPropertyResolved(true);
	}

	@Test
	void convertToTypeLeavesContextUnresolvedForOtherCases() {
		EmptyStringResolver resolver = new EmptyStringResolver();
		ELContext context = mock(ELContext.class);

		assertNull(resolver.convertToType(context, "x", String.class));
		assertNull(resolver.convertToType(context, null, Integer.class));
		verify(context, never()).setPropertyResolved(true);
	}
}
