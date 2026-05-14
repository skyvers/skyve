package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.ModuleDocumentMetaData;
import org.skyve.metadata.module.Module.DocumentRef;

/**
 * Tests for {@link FluentModuleDocument}: constructors, setters, and
 * {@code from()} via a mock {@code DocumentRef}.
 */
@SuppressWarnings("static-method")
class FluentModuleDocumentTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentModuleDocument().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		ModuleDocumentMetaData meta = new ModuleDocumentMetaData();
		FluentModuleDocument fd = new FluentModuleDocument(meta);
		assertEquals(meta, fd.get());
	}

	@Test
	void moduleRefSetsValue() {
		FluentModuleDocument fd = new FluentModuleDocument().moduleRef("admin");
		assertEquals("admin", fd.get().getModuleRef());
	}

	@Test
	void refSetsValue() {
		FluentModuleDocument fd = new FluentModuleDocument().ref("Contact");
		assertEquals("Contact", fd.get().getRef());
	}

	@Test
	void defaultQueryNameSetsValue() {
		FluentModuleDocument fd = new FluentModuleDocument().defaultQueryName("qContact");
		assertEquals("qContact", fd.get().getDefaultQueryName());
	}

	@Test
	void fromPopulatesFromDocumentRef() {
		DocumentRef ref = new DocumentRef();
		ref.setReferencedModuleName("admin");
		ref.setDefaultQueryName("qContact");

		FluentModuleDocument fd = new FluentModuleDocument().from("Contact", ref);
		assertEquals("admin", fd.get().getModuleRef());
		assertEquals("Contact", fd.get().getRef());
		assertEquals("qContact", fd.get().getDefaultQueryName());
	}

	@Test
	void fromWithNullModuleRefAndQueryName() {
		DocumentRef ref = new DocumentRef();
		// referencedModuleName and defaultQueryName are null by default

		FluentModuleDocument fd = new FluentModuleDocument().from("Document", ref);
		assertNull(fd.get().getModuleRef());
		assertEquals("Document", fd.get().getRef());
		assertNull(fd.get().getDefaultQueryName());
	}
}
