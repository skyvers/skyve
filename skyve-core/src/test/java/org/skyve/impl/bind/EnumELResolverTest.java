package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.el.ELContext;
import jakarta.el.ELProcessor;

/**
 * Tests which identifiers {@link EnumELResolver} answers for.
 */
class EnumELResolverTest {
	private static final Object FOUND = new Object();
	private static final Object REMEMBERED = new Object();

	/** Finds "Found", remembers "Remembered" and records every name it is asked to find. */
	private static final class Resolver extends EnumELResolver {
		private final List<String> finds = new ArrayList<>();

		private Resolver() {
			super(Set.of("Local"));
		}

		@Override
		Object remembered(String name) {
			return "Remembered".equals(name) ? REMEMBERED : null;
		}

		@Override
		Object find(String name) {
			finds.add(name);
			return "Found".equals(name) ? FOUND : null;
		}
	}

	private Resolver resolver;
	private ELContext context;

	@BeforeEach
	void setUp() {
		resolver = new Resolver();
		context = new ELProcessor().getELManager().getELContext();
		context.setPropertyResolved(false);
	}

	@Test
	void resolvesATopLevelIdentifierThatIsFound() {
		assertSame(FOUND, resolver.getValue(context, null, "Found"));
		assertTrue(context.isPropertyResolved());
		assertEquals(List.of("Found"), resolver.finds);
	}

	@Test
	void leavesAnIdentifierThatIsNotFoundUnresolved() {
		assertNull(resolver.getValue(context, null, "Missing"));
		assertFalse(context.isPropertyResolved());
		assertEquals(List.of("Missing"), resolver.finds);
	}

	@Test
	void resolvesWhatIsRememberedWithoutFindingItAgain() {
		assertSame(REMEMBERED, resolver.getValue(context, null, "Remembered"));
		assertTrue(context.isPropertyResolved());
		assertTrue(resolver.finds.isEmpty());
	}

	@Test
	void ignoresPropertiesOfABase() {
		assertNull(resolver.getValue(context, new Object(), "Found"));
		assertFalse(context.isPropertyResolved());
		assertTrue(resolver.finds.isEmpty());
	}

	@Test
	void ignoresPropertiesThatAreNotNames() {
		assertNull(resolver.getValue(context, null, Integer.valueOf(1)));
		assertNull(resolver.getValue(context, null, null));
		assertFalse(context.isPropertyResolved());
		assertTrue(resolver.finds.isEmpty());
	}

	@Test
	void ignoresTheSkyveVariables() {
		assertNull(resolver.getValue(context, null, "bean"));
		assertNull(resolver.getValue(context, null, "user"));
		assertNull(resolver.getValue(context, null, "stash"));
		assertFalse(context.isPropertyResolved());
		assertTrue(resolver.finds.isEmpty());
	}

	@Test
	void ignoresNamesThatAreBoundElsewhere() {
		assertNull(resolver.getValue(context, null, "Local"));
		assertFalse(context.isPropertyResolved());
		assertTrue(resolver.finds.isEmpty());
	}

	@Test
	void ignoresImportedClassesAndStaticMembers() {
		context.getImportHandler().importClass("java.util.Collections");
		context.getImportHandler().importStatic("java.lang.Math.PI");
		assertNull(resolver.getValue(context, null, "String"));
		assertNull(resolver.getValue(context, null, "Collections"));
		assertNull(resolver.getValue(context, null, "PI"));
		assertFalse(context.isPropertyResolved());
		assertTrue(resolver.finds.isEmpty());
	}

	@Test
	void doesNotRememberByDefault() {
		EnumELResolver plain = new EnumELResolver(Set.of()) {
			@Override
			Object find(String name) {
				return FOUND;
			}
		};
		assertNull(plain.remembered("Anything"));
		assertSame(FOUND, plain.getValue(context, null, "Anything"));
	}

	@Test
	void handlesNothingButGetValue() {
		assertNull(resolver.getType(context, null, "Found"));
		assertFalse(resolver.isReadOnly(context, null, "Found"));
		assertNull(resolver.getCommonPropertyType(context, null));
		resolver.setValue(context, null, "Found", "value");
		assertFalse(context.isPropertyResolved());
		assertTrue(resolver.finds.isEmpty());
	}
}
