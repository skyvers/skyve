package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.ModuleRoleContentUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleDocumentAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleDynamicImageUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleModelAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRolePreviousCompleteUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleQueryAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleSingularUserAccessMetaData;

/**
 * Tests for module role user access fluent builders.
 */
public class FluentModuleRoleAccessTest {

	// --- FluentModuleRoleSingularAccess ---

	@Test
	@SuppressWarnings("static-method")
	void singularAccessDefaultConstructorCreatesInstance() {
		FluentModuleRoleSingularAccess a = new FluentModuleRoleSingularAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void singularAccessWrappingConstructorUsesProvided() {
		ModuleRoleSingularUserAccessMetaData md = new ModuleRoleSingularUserAccessMetaData();
		FluentModuleRoleSingularAccess a = new FluentModuleRoleSingularAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void singularAccessDocumentNameReturnsSelf() {
		FluentModuleRoleSingularAccess a = new FluentModuleRoleSingularAccess();
		FluentModuleRoleSingularAccess result = a.documentName("TestDoc");
		assertSame(a, result);
		assertEquals("TestDoc", a.get().getDocumentName());
	}

	// --- FluentModuleRoleQueryAggregateAccess ---

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessDefaultConstructorCreatesInstance() {
		FluentModuleRoleQueryAggregateAccess a = new FluentModuleRoleQueryAggregateAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessWrappingConstructorUsesProvided() {
		ModuleRoleQueryAggregateUserAccessMetaData md = new ModuleRoleQueryAggregateUserAccessMetaData();
		FluentModuleRoleQueryAggregateAccess a = new FluentModuleRoleQueryAggregateAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessQueryNameReturnsSelf() {
		FluentModuleRoleQueryAggregateAccess a = new FluentModuleRoleQueryAggregateAccess();
		FluentModuleRoleQueryAggregateAccess result = a.queryName("TestQuery");
		assertSame(a, result);
		assertEquals("TestQuery", a.get().getQueryName());
	}

	// --- FluentModuleRoleDocumentAggregateAccess ---

	@Test
	@SuppressWarnings("static-method")
	void documentAggregateAccessDefaultConstructorCreatesInstance() {
		FluentModuleRoleDocumentAggregateAccess a = new FluentModuleRoleDocumentAggregateAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void documentAggregateAccessWrappingConstructorUsesProvided() {
		ModuleRoleDocumentAggregateUserAccessMetaData md = new ModuleRoleDocumentAggregateUserAccessMetaData();
		FluentModuleRoleDocumentAggregateAccess a = new FluentModuleRoleDocumentAggregateAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void documentAggregateAccessDocumentNameReturnsSelf() {
		FluentModuleRoleDocumentAggregateAccess a = new FluentModuleRoleDocumentAggregateAccess();
		FluentModuleRoleDocumentAggregateAccess result = a.documentName("TestDoc");
		assertSame(a, result);
		assertEquals("TestDoc", a.get().getDocumentName());
	}

	// --- FluentModuleRoleModelAggregateAccess ---

	@Test
	@SuppressWarnings("static-method")
	void modelAggregateAccessDefaultConstructorCreatesInstance() {
		FluentModuleRoleModelAggregateAccess a = new FluentModuleRoleModelAggregateAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void modelAggregateAccessWrappingConstructorUsesProvided() {
		ModuleRoleModelAggregateUserAccessMetaData md = new ModuleRoleModelAggregateUserAccessMetaData();
		FluentModuleRoleModelAggregateAccess a = new FluentModuleRoleModelAggregateAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void modelAggregateAccessDocumentNameReturnsSelf() {
		FluentModuleRoleModelAggregateAccess a = new FluentModuleRoleModelAggregateAccess();
		FluentModuleRoleModelAggregateAccess result = a.documentName("TestDoc");
		assertSame(a, result);
		assertEquals("TestDoc", a.get().getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void modelAggregateAccessModelNameReturnsSelf() {
		FluentModuleRoleModelAggregateAccess a = new FluentModuleRoleModelAggregateAccess();
		FluentModuleRoleModelAggregateAccess result = a.modelName("TestModel");
		assertSame(a, result);
		assertEquals("TestModel", a.get().getModelName());
	}

	// --- FluentModuleRoleDynamicImageAccess ---

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessDefaultConstructorCreatesInstance() {
		FluentModuleRoleDynamicImageAccess a = new FluentModuleRoleDynamicImageAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessWrappingConstructorUsesProvided() {
		ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
		FluentModuleRoleDynamicImageAccess a = new FluentModuleRoleDynamicImageAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessDocumentNameReturnsSelf() {
		FluentModuleRoleDynamicImageAccess a = new FluentModuleRoleDynamicImageAccess();
		FluentModuleRoleDynamicImageAccess result = a.documentName("TestDoc");
		assertSame(a, result);
		assertEquals("TestDoc", a.get().getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessImageNameReturnsSelf() {
		FluentModuleRoleDynamicImageAccess a = new FluentModuleRoleDynamicImageAccess();
		FluentModuleRoleDynamicImageAccess result = a.imageName("TestImage");
		assertSame(a, result);
		assertEquals("TestImage", a.get().getImageName());
	}

	// --- FluentModuleRolePreviousCompleteAccess ---

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessDefaultConstructorCreatesInstance() {
		FluentModuleRolePreviousCompleteAccess a = new FluentModuleRolePreviousCompleteAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessWrappingConstructorUsesProvided() {
		ModuleRolePreviousCompleteUserAccessMetaData md = new ModuleRolePreviousCompleteUserAccessMetaData();
		FluentModuleRolePreviousCompleteAccess a = new FluentModuleRolePreviousCompleteAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessDocumentNameReturnsSelf() {
		FluentModuleRolePreviousCompleteAccess a = new FluentModuleRolePreviousCompleteAccess();
		FluentModuleRolePreviousCompleteAccess result = a.documentName("TestDoc");
		assertSame(a, result);
		assertEquals("TestDoc", a.get().getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessBindingReturnsSelf() {
		FluentModuleRolePreviousCompleteAccess a = new FluentModuleRolePreviousCompleteAccess();
		FluentModuleRolePreviousCompleteAccess result = a.binding("testBinding");
		assertSame(a, result);
		assertEquals("testBinding", a.get().getBinding());
	}

	// --- FluentModuleRoleContentAccess ---

	@Test
	@SuppressWarnings("static-method")
	void contentAccessDefaultConstructorCreatesInstance() {
		FluentModuleRoleContentAccess a = new FluentModuleRoleContentAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentAccessWrappingConstructorUsesProvided() {
		ModuleRoleContentUserAccessMetaData md = new ModuleRoleContentUserAccessMetaData();
		FluentModuleRoleContentAccess a = new FluentModuleRoleContentAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentAccessDocumentNameReturnsSelf() {
		FluentModuleRoleContentAccess a = new FluentModuleRoleContentAccess();
		FluentModuleRoleContentAccess result = a.documentName("TestDoc");
		assertSame(a, result);
		assertEquals("TestDoc", a.get().getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentAccessBindingReturnsSelf() {
		FluentModuleRoleContentAccess a = new FluentModuleRoleContentAccess();
		FluentModuleRoleContentAccess result = a.binding("contentBinding");
		assertSame(a, result);
		assertEquals("contentBinding", a.get().getBinding());
	}

	// --- FluentModuleReportAccess ---

	@Test
	@SuppressWarnings("static-method")
	void reportAccessDefaultConstructorCreatesInstance() {
		FluentModuleReportAccess a = new FluentModuleReportAccess();
		assertNotNull(a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void reportAccessWrappingConstructorUsesProvided() {
		ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
		FluentModuleReportAccess a = new FluentModuleReportAccess(md);
		assertSame(md, a.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void reportAccessDocumentNameReturnsSelf() {
		FluentModuleReportAccess a = new FluentModuleReportAccess();
		FluentModuleReportAccess result = a.documentName("TestDoc");
		assertSame(a, result);
		assertEquals("TestDoc", a.get().getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void reportAccessImageNameReturnsSelf() {
		FluentModuleReportAccess a = new FluentModuleReportAccess();
		FluentModuleReportAccess result = a.imageName("TestReport");
		assertSame(a, result);
		assertEquals("TestReport", a.get().getImageName());
	}
}
