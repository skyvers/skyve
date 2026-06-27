package org.skyve.metadata.user;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

class DocumentPermissionTest {

	@Test
	@SuppressWarnings("static-method")
	void nonePermissionHasNoAccess() {
		DocumentPermission p = DocumentPermission._____;
		assertFalse(p.canCreate());
		assertFalse(p.canRead());
		assertFalse(p.canUpdate());
		assertFalse(p.canDelete());
		assertThat(p.getScope(), is(DocumentPermissionScope.none));
	}

	@Test
	@SuppressWarnings("static-method")
	void crudGlobalHasFullAccess() {
		DocumentPermission p = DocumentPermission.CRUDG;
		assertTrue(p.canCreate());
		assertTrue(p.canRead());
		assertTrue(p.canUpdate());
		assertTrue(p.canDelete());
		assertThat(p.getScope(), is(DocumentPermissionScope.global));
	}

	@Test
	@SuppressWarnings("static-method")
	void readOnlyCustomerHasReadOnly() {
		DocumentPermission p = DocumentPermission._R__C;
		assertFalse(p.canCreate());
		assertTrue(p.canRead());
		assertFalse(p.canUpdate());
		assertFalse(p.canDelete());
		assertThat(p.getScope(), is(DocumentPermissionScope.customer));
	}

	@Test
	@SuppressWarnings("static-method")
	void mergeAddsCrudFromBoth() {
		DocumentPermission result = DocumentPermission._R__C.mergePermission(DocumentPermission.CRU_C);
		assertTrue(result.canCreate());
		assertTrue(result.canRead());
		assertTrue(result.canUpdate());
		assertFalse(result.canDelete());
	}

	@Test
	@SuppressWarnings("static-method")
	void mergeScopeUpgradesGlobal() {
		// _R__C merged with _R__G → should use global scope (more permissive)
		DocumentPermission result = DocumentPermission._R__C.mergePermission(DocumentPermission._R__G);
		assertThat(result.getScope(), is(DocumentPermissionScope.global));
	}

	@Test
	@SuppressWarnings("static-method")
	void mergeWithSamePermissionReturnsSame() {
		DocumentPermission p = DocumentPermission.CRUDC;
		DocumentPermission result = p.mergePermission(p);
		assertThat(result, is(p));
	}

	@Test
	@SuppressWarnings("static-method")
	void dataGroupPermissionScope() {
		assertThat(DocumentPermission.CRUDD.getScope(), is(DocumentPermissionScope.dataGroup));
	}

	@Test
	@SuppressWarnings("static-method")
	void userPermissionScope() {
		assertThat(DocumentPermission.CRUDU.getScope(), is(DocumentPermissionScope.user));
	}
}
