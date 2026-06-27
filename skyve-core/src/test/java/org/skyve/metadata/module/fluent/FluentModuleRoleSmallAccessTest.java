package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.HashSet;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.ModuleRoleContentUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleDynamicImageUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleModelAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRolePreviousCompleteUserAccessMetaData;

/**
 * Tests for the small module-role user-access fluent classes:
 * {@link FluentModuleRoleContentAccess}, {@link FluentModuleRoleModelAggregateAccess},
 * {@link FluentModuleRolePreviousCompleteAccess}, {@link FluentModuleRoleDynamicImageAccess}.
 */
@SuppressWarnings("static-method")
class FluentModuleRoleSmallAccessTest {

	// ---- FluentModuleRoleContentAccess -----------------------------------

	@Test
	void contentDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentModuleRoleContentAccess().get());
	}

	@Test
	void contentWrappingConstructorPreservesInstance() {
		ModuleRoleContentUserAccessMetaData md = new ModuleRoleContentUserAccessMetaData();
		FluentModuleRoleContentAccess fa = new FluentModuleRoleContentAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void contentDocumentNameSetsValue() {
		FluentModuleRoleContentAccess fa = new FluentModuleRoleContentAccess().documentName("Contact");
		assertEquals("Contact", fa.get().getDocumentName());
	}

	@Test
	void contentBindingSetsValue() {
		FluentModuleRoleContentAccess fa = new FluentModuleRoleContentAccess().binding("photo");
		assertEquals("photo", fa.get().getBinding());
	}

	@Test
	void contentFromSetsAllFields() {
		FluentModuleRoleContentAccess fa = new FluentModuleRoleContentAccess()
				.from("Contact", "photo", new HashSet<>());
		assertEquals("Contact", fa.get().getDocumentName());
		assertEquals("photo", fa.get().getBinding());
	}

	// ---- FluentModuleRoleModelAggregateAccess ----------------------------

	@Test
	void modelAggregateDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentModuleRoleModelAggregateAccess().get());
	}

	@Test
	void modelAggregateWrappingConstructorPreservesInstance() {
		ModuleRoleModelAggregateUserAccessMetaData md = new ModuleRoleModelAggregateUserAccessMetaData();
		FluentModuleRoleModelAggregateAccess fa = new FluentModuleRoleModelAggregateAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void modelAggregateDocumentNameSetsValue() {
		FluentModuleRoleModelAggregateAccess fa = new FluentModuleRoleModelAggregateAccess().documentName("Contact");
		assertEquals("Contact", fa.get().getDocumentName());
	}

	@Test
	void modelAggregateModelNameSetsValue() {
		FluentModuleRoleModelAggregateAccess fa = new FluentModuleRoleModelAggregateAccess().modelName("MyModel");
		assertEquals("MyModel", fa.get().getModelName());
	}

	@Test
	void modelAggregateFromSetsAllFields() {
		FluentModuleRoleModelAggregateAccess fa = new FluentModuleRoleModelAggregateAccess()
				.from("Contact", "MyModel", new HashSet<>());
		assertEquals("Contact", fa.get().getDocumentName());
		assertEquals("MyModel", fa.get().getModelName());
	}

	// ---- FluentModuleRolePreviousCompleteAccess --------------------------

	@Test
	void previousCompleteDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentModuleRolePreviousCompleteAccess().get());
	}

	@Test
	void previousCompleteWrappingConstructorPreservesInstance() {
		ModuleRolePreviousCompleteUserAccessMetaData md = new ModuleRolePreviousCompleteUserAccessMetaData();
		FluentModuleRolePreviousCompleteAccess fa = new FluentModuleRolePreviousCompleteAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void previousCompleteDocumentNameSetsValue() {
		FluentModuleRolePreviousCompleteAccess fa = new FluentModuleRolePreviousCompleteAccess().documentName("Contact");
		assertEquals("Contact", fa.get().getDocumentName());
	}

	@Test
	void previousCompleteBindingSetsValue() {
		FluentModuleRolePreviousCompleteAccess fa = new FluentModuleRolePreviousCompleteAccess().binding("status");
		assertEquals("status", fa.get().getBinding());
	}

	@Test
	void previousCompleteFromSetsAllFields() {
		FluentModuleRolePreviousCompleteAccess fa = new FluentModuleRolePreviousCompleteAccess()
				.from("Contact", "status", new HashSet<>());
		assertEquals("Contact", fa.get().getDocumentName());
		assertEquals("status", fa.get().getBinding());
	}

	// ---- FluentModuleRoleDynamicImageAccess ------------------------------

	@Test
	void dynamicImageDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentModuleRoleDynamicImageAccess().get());
	}

	@Test
	void dynamicImageWrappingConstructorPreservesInstance() {
		ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
		FluentModuleRoleDynamicImageAccess fa = new FluentModuleRoleDynamicImageAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void dynamicImageDocumentNameSetsValue() {
		FluentModuleRoleDynamicImageAccess fa = new FluentModuleRoleDynamicImageAccess().documentName("Contact");
		assertEquals("Contact", fa.get().getDocumentName());
	}

	@Test
	void dynamicImageImageNameSetsValue() {
		FluentModuleRoleDynamicImageAccess fa = new FluentModuleRoleDynamicImageAccess().imageName("avatar");
		assertEquals("avatar", fa.get().getImageName());
	}

	@Test
	void dynamicImageFromSetsAllFields() {
		FluentModuleRoleDynamicImageAccess fa = new FluentModuleRoleDynamicImageAccess()
				.from("Contact", "avatar", new HashSet<>());
		assertEquals("Contact", fa.get().getDocumentName());
		assertEquals("avatar", fa.get().getImageName());
	}

	// ---- FluentModuleRoleSingularAccess ----------------------------------

	@Test
	void singularDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentModuleRoleSingularAccess().get());
	}

	@Test
	void singularWrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.repository.module.ModuleRoleSingularUserAccessMetaData md =
				new org.skyve.impl.metadata.repository.module.ModuleRoleSingularUserAccessMetaData();
		FluentModuleRoleSingularAccess fa = new FluentModuleRoleSingularAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void singularDocumentNameSetsValue() {
		FluentModuleRoleSingularAccess fa = new FluentModuleRoleSingularAccess().documentName("Contact");
		assertEquals("Contact", fa.get().getDocumentName());
	}

	@Test
	void singularFromSetsAllFields() {
		FluentModuleRoleSingularAccess fa = new FluentModuleRoleSingularAccess()
				.from("Contact", new HashSet<>());
		assertEquals("Contact", fa.get().getDocumentName());
	}

	// ---- FluentModuleRoleQueryAggregateAccess ----------------------------

	@Test
	void queryAggregateDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentModuleRoleQueryAggregateAccess().get());
	}

	@Test
	void queryAggregateWrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.repository.module.ModuleRoleQueryAggregateUserAccessMetaData md =
				new org.skyve.impl.metadata.repository.module.ModuleRoleQueryAggregateUserAccessMetaData();
		FluentModuleRoleQueryAggregateAccess fa = new FluentModuleRoleQueryAggregateAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void queryAggregateQueryNameSetsValue() {
		FluentModuleRoleQueryAggregateAccess fa = new FluentModuleRoleQueryAggregateAccess().queryName("qContacts");
		assertEquals("qContacts", fa.get().getQueryName());
	}

	@Test
	void queryAggregateFromSetsAllFields() {
		FluentModuleRoleQueryAggregateAccess fa = new FluentModuleRoleQueryAggregateAccess()
				.from("qContacts", new HashSet<>());
		assertEquals("qContacts", fa.get().getQueryName());
	}

	// ---- FluentModuleRoleDocumentAggregateAccess -------------------------

	@Test
	void documentAggregateDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentModuleRoleDocumentAggregateAccess().get());
	}

	@Test
	void documentAggregateWrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.repository.module.ModuleRoleDocumentAggregateUserAccessMetaData md =
				new org.skyve.impl.metadata.repository.module.ModuleRoleDocumentAggregateUserAccessMetaData();
		FluentModuleRoleDocumentAggregateAccess fa = new FluentModuleRoleDocumentAggregateAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void documentAggregateDocumentNameSetsValue() {
		FluentModuleRoleDocumentAggregateAccess fa = new FluentModuleRoleDocumentAggregateAccess().documentName("Contact");
		assertEquals("Contact", fa.get().getDocumentName());
	}

	@Test
	void documentAggregateFromSetsAllFields() {
		FluentModuleRoleDocumentAggregateAccess fa = new FluentModuleRoleDocumentAggregateAccess()
				.from("Contact", new HashSet<>());
		assertEquals("Contact", fa.get().getDocumentName());
	}
}
