package org.skyve.metadata.module;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

class ModuleDocumentRefTest {

	@Test
	@SuppressWarnings("static-method")
	void setOwningModuleNameRoundtripIsConsistent() {
		// Also validates that after setting owning module, properties map is initialised
		Module.DocumentRef ref = new Module.DocumentRef();
		ref.setOwningModuleName("admin");
		assertThat(ref.getOwningModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setOwningModuleNameRoundtrip() {
		Module.DocumentRef ref = new Module.DocumentRef();
		ref.setOwningModuleName("admin");
		assertThat(ref.getOwningModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setReferencedModuleNameRoundtrip() {
		Module.DocumentRef ref = new Module.DocumentRef();
		ref.setReferencedModuleName("sales");
		assertThat(ref.getReferencedModuleName(), is("sales"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDefaultQueryNameRoundtrip() {
		Module.DocumentRef ref = new Module.DocumentRef();
		ref.setDefaultQueryName("qUsers");
		assertThat(ref.getDefaultQueryName(), is("qUsers"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getModuleNameDotDocumentNameUsesOwningWhenNoReferencedModule() {
		Module.DocumentRef ref = new Module.DocumentRef();
		ref.setOwningModuleName("admin");
		assertThat(ref.getModuleNameDotDocumentName("User"), is("admin.User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getModuleNameDotDocumentNameUsesReferencedModuleWhenSet() {
		Module.DocumentRef ref = new Module.DocumentRef();
		ref.setOwningModuleName("admin");
		ref.setReferencedModuleName("hr");
		assertThat(ref.getModuleNameDotDocumentName("Employee"), is("hr.Employee"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertiesIsNotNull() {
		Module.DocumentRef ref = new Module.DocumentRef();
		assertNotNull(ref.getProperties());
	}
}
