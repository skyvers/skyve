package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.DocumentPrivilegeMetaData;
import org.skyve.metadata.user.DocumentPermission;

public class FluentDocumentPrivilegeTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesInstance() {
		FluentDocumentPrivilege p = new FluentDocumentPrivilege();
		assertNotNull(p.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		DocumentPrivilegeMetaData md = new DocumentPrivilegeMetaData();
		FluentDocumentPrivilege p = new FluentDocumentPrivilege(md);
		assertSame(md, p.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void documentNameReturnsSelf() {
		FluentDocumentPrivilege p = new FluentDocumentPrivilege();
		FluentDocumentPrivilege result = p.documentName("TestDoc");
		assertSame(p, result);
		assertEquals("TestDoc", p.get().getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void permissionReturnsSelf() {
		FluentDocumentPrivilege p = new FluentDocumentPrivilege();
		FluentDocumentPrivilege result = p.permission(DocumentPermission.CRUDC);
		assertSame(p, result);
		assertEquals(DocumentPermission.CRUDC, p.get().getPermission());
	}

	@Test
	@SuppressWarnings("static-method")
	void addActionPrivilegeReturnsSelf() {
		FluentDocumentPrivilege p = new FluentDocumentPrivilege();
		FluentDocumentPrivilege result = p.addActionPrivilege("Save");
		assertSame(p, result);
		assertEquals(1, p.get().getActions().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void removeActionPrivilegeReturnsSelf() {
		FluentDocumentPrivilege p = new FluentDocumentPrivilege();
		p.addActionPrivilege("Save");
		FluentDocumentPrivilege result = p.removeActionPrivilege("Save");
		assertSame(p, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void clearActionPrivilegesReturnsSelf() {
		FluentDocumentPrivilege p = new FluentDocumentPrivilege();
		p.addActionPrivilege("Save");
		FluentDocumentPrivilege result = p.clearActionPrivileges();
		assertSame(p, result);
		assertEquals(0, p.get().getActions().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void addContentPermissionReturnsSelf() {
		FluentDocumentPrivilege p = new FluentDocumentPrivilege();
		FluentDocumentPrivilege result = p.addContentPermission("attachment");
		assertSame(p, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void addContentRestrictionReturnsSelf() {
		FluentDocumentPrivilege p = new FluentDocumentPrivilege();
		FluentDocumentPrivilege result = p.addContentRestriction("sensitiveFile");
		assertSame(p, result);
	}
}
