package org.skyve.impl.metadata.user;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class ActionPrivilegeTest {

	@Test
	@SuppressWarnings("static-method")
	public void setDocumentNameRoundtrip() {
		ActionPrivilege priv = new ActionPrivilege();
		priv.setDocumentName("User");
		assertThat(priv.getDocumentName(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void documentNameNullByDefault() {
		ActionPrivilege priv = new ActionPrivilege();
		assertNull(priv.getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setNameRoundtrip() {
		ActionPrivilege priv = new ActionPrivilege();
		priv.setName("Edit");
		assertThat(priv.getName(), is("Edit"));
	}
}
