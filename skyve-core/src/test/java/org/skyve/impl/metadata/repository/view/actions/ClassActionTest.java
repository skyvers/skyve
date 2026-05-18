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
class ClassActionTest {

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

	@Test
	@SuppressWarnings("static-method")
	void setVisibleConditionNameNegatesCondition() {
		CustomAction action = new CustomAction();
		action.setVisibleConditionName("showAction");
		// setVisibleConditionName stores the negated form as invisibleConditionName
		assertThat(action.getInvisibleConditionName(), is("notShowAction"));
	}

        @Test
        @SuppressWarnings("static-method")
        void bizExportActionConstructorSetsImplicitName() {
                BizExportAction action = new BizExportAction();
                assertThat(action.getImplicitName(), is(org.skyve.metadata.controller.ImplicitActionName.BizExport));
        }

        @Test
        @SuppressWarnings("static-method")
        void bizImportActionConstructorSetsImplicitName() {
                BizImportAction action = new BizImportAction();
                assertThat(action.getImplicitName(), is(org.skyve.metadata.controller.ImplicitActionName.BizImport));
        }

        @Test
        @SuppressWarnings("static-method")
        void downloadActionConstructorSetsImplicitName() {
                DownloadAction action = new DownloadAction();
                assertThat(action.getImplicitName(), is(org.skyve.metadata.controller.ImplicitActionName.Download));
        }

        @Test
        @SuppressWarnings("static-method")
        void uploadActionConstructorSetsImplicitName() {
                UploadAction action = new UploadAction();
                assertThat(action.getImplicitName(), is(org.skyve.metadata.controller.ImplicitActionName.Upload));
        }

        @Test
        @SuppressWarnings("static-method")
        void actionMetaDataJaxbGetVisibleConditionNameReturnsNull() {
                // Package-private method for JAXB marshaling
                assertNull(new CustomAction().getVisibleConditionName());
        }

        @Test
        @SuppressWarnings("static-method")
        void actionMetaDataJaxbGetEnabledConditionNameReturnsNull() {
                // Package-private method for JAXB marshaling
                assertNull(new CustomAction().getEnabledConditionName());
        }
}
