package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.HashSet;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.ModuleRoleReportUserAccessMetaData;

/**
 * Tests for {@link FluentModuleRoleReportAccess}: constructors and setters.
 */
@SuppressWarnings("static-method")
class FluentModuleRoleReportAccessTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentModuleRoleReportAccess().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		ModuleRoleReportUserAccessMetaData md = new ModuleRoleReportUserAccessMetaData();
		FluentModuleRoleReportAccess fa = new FluentModuleRoleReportAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void moduleNameSetsValue() {
		FluentModuleRoleReportAccess fa = new FluentModuleRoleReportAccess().moduleName("admin");
		assertEquals("admin", fa.get().getModuleName());
	}

	@Test
	void documentNameSetsValue() {
		FluentModuleRoleReportAccess fa = new FluentModuleRoleReportAccess().documentName("User");
		assertEquals("User", fa.get().getDocumentName());
	}

	@Test
	void reportNameSetsValue() {
		FluentModuleRoleReportAccess fa = new FluentModuleRoleReportAccess().reportName("UserReport");
		assertEquals("UserReport", fa.get().getReportName());
	}

	@Test
	void fromSetsAllFields() {
		FluentModuleRoleReportAccess fa = new FluentModuleRoleReportAccess()
				.from("admin", "User", "UserReport", new HashSet<>());
		assertEquals("admin", fa.get().getModuleName());
		assertEquals("User", fa.get().getDocumentName());
		assertEquals("UserReport", fa.get().getReportName());
	}

	@Test
	void getReturnsCorrectType() {
		FluentModuleRoleReportAccess fa = new FluentModuleRoleReportAccess();
		assertNotNull(fa.get());
	}
}
