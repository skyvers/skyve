package org.skyve.impl.metadata.repository.view.actions;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.ActionImpl;

/**
 * Tests for ClassAction (via its concrete subclass CustomAction).
 */
public class ClassActionTest {

	@Test
	@SuppressWarnings("static-method")
	void setClassNameAndGet() {
		CustomAction action = new CustomAction();
		action.setClassName("modules.admin.User.UserAction");
		assertThat(action.getClassName(), is("modules.admin.User.UserAction"));
	}

	@Test
	@SuppressWarnings("static-method")
	void classNameBlankBecomesNull() {
		CustomAction action = new CustomAction();
		action.setClassName("  ");
		assertNull(action.getClassName());
	}

	@Test
	@SuppressWarnings("static-method")
	void toMetaDataActionSetsResourceName() {
		CustomAction action = new CustomAction();
		action.setClassName("modules.admin.User.UserAction");
		ActionImpl result = action.toMetaDataAction();
		assertNotNull(result);
		assertThat(result.getResourceName(), is("modules.admin.User.UserAction"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toMetaDataActionUsesClassNameAsNameWhenNameNotSet() {
		CustomAction action = new CustomAction();
		action.setClassName("modules.admin.User.UserAction");
		ActionImpl result = action.toMetaDataAction();
		assertThat(result.getName(), is("modules.admin.User.UserAction"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toMetaDataActionPreservesExplicitName() {
		CustomAction action = new CustomAction();
		action.setClassName("modules.admin.User.UserAction");
		action.setName("SaveUser");
		ActionImpl result = action.toMetaDataAction();
		assertThat(result.getName(), is("SaveUser"));
	}
}
