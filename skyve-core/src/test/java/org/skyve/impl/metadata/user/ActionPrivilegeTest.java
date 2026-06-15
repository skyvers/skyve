package org.skyve.impl.metadata.user;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class ActionPrivilegeTest {

	@Test
	@SuppressWarnings("static-method")
	void setDocumentNameRoundtrip() {
		ActionPrivilege priv = new ActionPrivilege();
		priv.setDocumentName("User");
		assertThat(priv.getDocumentName(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentNameNullByDefault() {
		ActionPrivilege priv = new ActionPrivilege();
		assertNull(priv.getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void setNameRoundtrip() {
		ActionPrivilege priv = new ActionPrivilege();
		priv.setName("Edit");
		assertThat(priv.getName(), is("Edit"));
	}
}
