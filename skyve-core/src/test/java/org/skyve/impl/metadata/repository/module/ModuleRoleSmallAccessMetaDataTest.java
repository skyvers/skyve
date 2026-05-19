package org.skyve.impl.metadata.repository.module;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.UserAccess;

/**
 * Tests for small module repository MetaData classes that are simple POJOs.
 */
class ModuleRoleSmallAccessMetaDataTest {

	// ---- CalendarItemMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void calendarItemSetDocumentNameAndGet() {
		CalendarItemMetaData item = new CalendarItemMetaData();
		item.setDocumentName("Calendar");
		assertThat(item.getDocumentName(), is("Calendar"));
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarItemSetQueryNameAndGet() {
		CalendarItemMetaData item = new CalendarItemMetaData();
		item.setQueryName("qCalendar");
		assertThat(item.getQueryName(), is("qCalendar"));
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarItemSetModelNameAndGet() {
		CalendarItemMetaData item = new CalendarItemMetaData();
		item.setModelName("CalendarModel");
		assertThat(item.getModelName(), is("CalendarModel"));
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarItemSetStartBindingAndGet() {
		CalendarItemMetaData item = new CalendarItemMetaData();
		item.setStartBinding("startDate");
		assertThat(item.getStartBinding(), is("startDate"));
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarItemSetEndBindingAndGet() {
		CalendarItemMetaData item = new CalendarItemMetaData();
		item.setEndBinding("endDate");
		assertThat(item.getEndBinding(), is("endDate"));
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarItemBlankDocumentNameBecomesNull() {
		CalendarItemMetaData item = new CalendarItemMetaData();
		item.setDocumentName("  ");
		assertNull(item.getDocumentName());
	}

	// ---- ModuleRoleReportUserAccessMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void reportAccessSetModuleNameAndGet() {
		ModuleRoleReportUserAccessMetaData md = new ModuleRoleReportUserAccessMetaData();
		md.setModuleName("admin");
		assertThat(md.getModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void reportAccessSetDocumentNameAndGet() {
		ModuleRoleReportUserAccessMetaData md = new ModuleRoleReportUserAccessMetaData();
		md.setDocumentName("User");
		assertThat(md.getDocumentName(), is("User"));
	}

	// ---- ModuleRolePreviousCompleteUserAccessMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessSetBindingAndGet() {
		ModuleRolePreviousCompleteUserAccessMetaData md = new ModuleRolePreviousCompleteUserAccessMetaData();
		md.setBinding("status");
		assertThat(md.getBinding(), is("status"));
	}

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessBindingBlankBecomesNull() {
		ModuleRolePreviousCompleteUserAccessMetaData md = new ModuleRolePreviousCompleteUserAccessMetaData();
		md.setBinding("  ");
		assertNull(md.getBinding());
	}

	// ---- ModuleRoleModelAggregateUserAccessMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void modelAggregateAccessSetModelNameAndGet() {
		ModuleRoleModelAggregateUserAccessMetaData md = new ModuleRoleModelAggregateUserAccessMetaData();
		md.setModelName("MyModel");
		assertThat(md.getModelName(), is("MyModel"));
	}

	@Test
	@SuppressWarnings("static-method")
	void modelAggregateAccessModelNameBlankBecomesNull() {
		ModuleRoleModelAggregateUserAccessMetaData md = new ModuleRoleModelAggregateUserAccessMetaData();
		md.setModelName("  ");
		assertNull(md.getModelName());
	}

	// ---- ModuleRoleDynamicImageUserAccessMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessSetImageNameAndGet() {
		ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
		md.setImageName("profilePhoto");
		assertThat(md.getImageName(), is("profilePhoto"));
	}

	// ---- ModuleRoleContentUserAccessMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void contentAccessSetBindingAndGet() {
		ModuleRoleContentUserAccessMetaData md = new ModuleRoleContentUserAccessMetaData();
		md.setBinding("attachment");
		assertThat(md.getBinding(), is("attachment"));
	}

	// ---- toUserAccess tests ----

	@Test
	@SuppressWarnings("static-method")
	void reportAccessToUserAccessReturnsReport() {
		ModuleRoleReportUserAccessMetaData md = new ModuleRoleReportUserAccessMetaData();
		md.setModuleName("admin");
		md.setDocumentName("User");
		md.setReportName("UserReport");
		UserAccess ua = md.toUserAccess("admin");
		assertEquals("admin", ua.getModuleName());
		assertEquals("User", ua.getDocumentName());
		assertEquals("UserReport", ua.getComponent());
	}

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessToUserAccessReturnsQueryAggregate() {
		ModuleRoleQueryAggregateUserAccessMetaData md = new ModuleRoleQueryAggregateUserAccessMetaData();
		md.setQueryName("qAllUsers");
		UserAccess ua = md.toUserAccess("admin");
		assertEquals("admin", ua.getModuleName());
		assertEquals("qAllUsers", ua.getComponent());
	}

	@Test
	@SuppressWarnings("static-method")
	void documentAggregateAccessToUserAccessReturnsDocumentAggregate() {
		ModuleRoleDocumentAggregateUserAccessMetaData md = new ModuleRoleDocumentAggregateUserAccessMetaData();
		md.setDocumentName("Contact");
		UserAccess ua = md.toUserAccess("admin");
		assertEquals("admin", ua.getModuleName());
		assertEquals("Contact", ua.getComponent());
	}

	// ---- validate null-check tests ----

	@Test
	@SuppressWarnings("static-method")
	void reportAccessValidateThrowsWhenModuleNameNull() {
		ModuleRoleReportUserAccessMetaData md = new ModuleRoleReportUserAccessMetaData();
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void reportAccessValidateThrowsWhenDocumentNameNull() {
		ModuleRoleReportUserAccessMetaData md = new ModuleRoleReportUserAccessMetaData();
		md.setModuleName("admin");
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void reportAccessValidateThrowsWhenReportNameNull() {
		ModuleRoleReportUserAccessMetaData md = new ModuleRoleReportUserAccessMetaData();
		md.setModuleName("admin");
		md.setDocumentName("User");
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessValidateThrowsWhenQueryNameNull() {
		ModuleRoleQueryAggregateUserAccessMetaData md = new ModuleRoleQueryAggregateUserAccessMetaData();
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentAggregateAccessValidateThrowsWhenDocumentNameNull() {
		ModuleRoleDocumentAggregateUserAccessMetaData md = new ModuleRoleDocumentAggregateUserAccessMetaData();
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void modelAggregateAccessToUserAccessReturnsModelAggregate() {
		ModuleRoleModelAggregateUserAccessMetaData md = new ModuleRoleModelAggregateUserAccessMetaData();
		md.setDocumentName("Contact");
		md.setModelName("myModel");
		UserAccess ua = md.toUserAccess("admin");
		assertEquals("admin", ua.getModuleName());
		assertEquals("Contact", ua.getDocumentName());
		assertEquals("myModel", ua.getComponent());
	}

	@Test
	@SuppressWarnings("static-method")
	void modelAggregateAccessValidateThrowsWhenModelNameNull() {
		ModuleRoleModelAggregateUserAccessMetaData md = new ModuleRoleModelAggregateUserAccessMetaData();
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessToUserAccessReturnsPreviousComplete() {
		ModuleRolePreviousCompleteUserAccessMetaData md = new ModuleRolePreviousCompleteUserAccessMetaData();
		md.setDocumentName("Contact");
		md.setBinding("status");
		UserAccess ua = md.toUserAccess("admin");
		assertEquals("admin", ua.getModuleName());
		assertEquals("Contact", ua.getDocumentName());
		assertEquals("status", ua.getComponent());
	}

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessValidateThrowsWhenBindingNull() {
		ModuleRolePreviousCompleteUserAccessMetaData md = new ModuleRolePreviousCompleteUserAccessMetaData();
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessToUserAccessReturnsDynamicImage() {
		ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
		md.setDocumentName("Contact");
		md.setImageName("portrait");
		UserAccess ua = md.toUserAccess("admin");
		assertEquals("admin", ua.getModuleName());
		assertEquals("Contact", ua.getDocumentName());
		assertEquals("portrait", ua.getComponent());
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessValidateThrowsWhenImageNameNull() {
		ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void contentAccessToUserAccessReturnsContent() {
		ModuleRoleContentUserAccessMetaData md = new ModuleRoleContentUserAccessMetaData();
		md.setDocumentName("Contact");
		md.setBinding("attachment");
		UserAccess ua = md.toUserAccess("admin");
		assertEquals("admin", ua.getModuleName());
		assertEquals("Contact", ua.getDocumentName());
		assertEquals("attachment", ua.getComponent());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentAccessValidateThrowsWhenBindingNull() {
		ModuleRoleContentUserAccessMetaData md = new ModuleRoleContentUserAccessMetaData();
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void contentAccessBindingBlankBecomesNull() {
		ModuleRoleContentUserAccessMetaData md = new ModuleRoleContentUserAccessMetaData();
		md.setBinding("  ");
		assertNull(md.getBinding());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentAccessValidateThrowsWhenDocumentNameSetButBindingNull() {
		ModuleRoleContentUserAccessMetaData md = new ModuleRoleContentUserAccessMetaData();
		md.setDocumentName("Contact");
		// binding is null, module.getDocumentRefs() would be needed → pass a mock
		// but first it will throw because module is null → NullPointerException wraps
		// Actually super.validate calls module.getDocumentRefs() which NPEs on null module
		// So documentName is set and binding is null → super.validate throws NPE, not MetaDataException
		// Use a mock module that has the document
		org.skyve.metadata.module.Module module = org.mockito.Mockito.mock(org.skyve.metadata.module.Module.class);
		org.mockito.Mockito.when(module.getDocumentRefs()).thenReturn(java.util.Collections.singletonMap("Contact", null));
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", module));
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessBindingBlankBecomesNull() {
		ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
		md.setImageName("  ");
		assertNull(md.getImageName());
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessValidateThrowsWhenDocumentSetButImageNameNull() {
		ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
		md.setDocumentName("Contact");
		org.skyve.metadata.module.Module module = org.mockito.Mockito.mock(org.skyve.metadata.module.Module.class);
		org.mockito.Mockito.when(module.getDocumentRefs()).thenReturn(java.util.Collections.singletonMap("Contact", null));
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", module));
	}

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessValidateThrowsWhenDocumentSetButBindingNull() {
		ModuleRolePreviousCompleteUserAccessMetaData md = new ModuleRolePreviousCompleteUserAccessMetaData();
		md.setDocumentName("Contact");
		org.skyve.metadata.module.Module module = org.mockito.Mockito.mock(org.skyve.metadata.module.Module.class);
		org.mockito.Mockito.when(module.getDocumentRefs()).thenReturn(java.util.Collections.singletonMap("Contact", null));
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", module));
	}

        @Test
        @SuppressWarnings("static-method")
        void contentAccessToUserAccessReturnsNonNull() {
                ModuleRoleContentUserAccessMetaData md = new ModuleRoleContentUserAccessMetaData();
                md.setDocumentName("Contact");
                md.setBinding("photo");
                UserAccess access = md.toUserAccess("admin");
                assertThat(access, is(org.hamcrest.Matchers.notNullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        void dynamicImageAccessToUserAccessReturnsNonNull() {
                ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
                md.setDocumentName("Contact");
                md.setImageName("portrait");
                UserAccess access = md.toUserAccess("admin");
                assertThat(access, is(org.hamcrest.Matchers.notNullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        void previousCompleteAccessToUserAccessReturnsNonNull() {
                ModuleRolePreviousCompleteUserAccessMetaData md = new ModuleRolePreviousCompleteUserAccessMetaData();
                md.setDocumentName("Contact");
                md.setBinding("bizStatus");
                UserAccess access = md.toUserAccess("admin");
                assertThat(access, is(org.hamcrest.Matchers.notNullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        void reportAccessToUserAccessReturnsNonNull() {
                ModuleRoleReportUserAccessMetaData md = new ModuleRoleReportUserAccessMetaData();
                md.setModuleName("admin");
                md.setDocumentName("Contact");
                md.setReportName("contactReport");
                UserAccess access = md.toUserAccess("unused");
                assertThat(access, is(org.hamcrest.Matchers.notNullValue()));
        }

	// ---- ModuleRoleSingularUserAccessMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void singularAccessToUserAccessReturnsSingular() {
		ModuleRoleSingularUserAccessMetaData md = new ModuleRoleSingularUserAccessMetaData();
		md.setDocumentName("Contact");
		UserAccess access = md.toUserAccess("admin");
		assertEquals("admin", access.getModuleName());
		assertEquals("Contact", access.getDocumentName());
	}

	// ---- ActionMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void actionMetaDataGetPropertiesReturnsNonNullMap() {
		ActionMetaData md = new ActionMetaData();
		assertThat(md.getProperties(), is(org.hamcrest.Matchers.notNullValue()));
	}

	// ---- QueryMetaData (via BizQLMetaData) ----

	@Test
	@SuppressWarnings("static-method")
	void queryMetaDataGetPropertiesReturnsNonNullMap() {
		BizQLMetaData md = new BizQLMetaData();
		assertThat(md.getProperties(), is(org.hamcrest.Matchers.notNullValue()));
	}

	// ---- TreeItemMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void treeItemSetModelNameAndGet() {
		TreeItemMetaData item = new TreeItemMetaData();
		item.setModelName("TreeModel");
		assertThat(item.getModelName(), is("TreeModel"));
	}

	// ---- MetaDataQueryContentColumnMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void contentColumnSetEmptyThumbnailRelativeFileAndGet() {
		MetaDataQueryContentColumnMetaData md = new MetaDataQueryContentColumnMetaData();
		md.setEmptyThumbnailRelativeFile("images/empty.png");
		assertThat(md.getEmptyThumbnailRelativeFile(), is("images/empty.png"));
	}

	// ---- ModuleRoleReportUserAccessMetaData.validate (happy path) ----

	@Test
	@SuppressWarnings("static-method")
	void reportUserAccessValidateWithAllFieldsSetDoesNotThrow() {
		// Module param is not used when all fields are non-null, so pass null
		ModuleRoleReportUserAccessMetaData access = new ModuleRoleReportUserAccessMetaData();
		access.setModuleName("admin");
		access.setDocumentName("Contact");
		access.setReportName("ContactReport");
		access.validate("testMetaData", "testRole", null);
	}

	@Test
	@SuppressWarnings("static-method")
	void reportUserAccessValidateNullModuleNameThrows() {
		ModuleRoleReportUserAccessMetaData access = new ModuleRoleReportUserAccessMetaData();
		access.setDocumentName("Contact");
		access.setReportName("ContactReport");
		assertThrows(MetaDataException.class, () -> access.validate("testMetaData", "testRole", null));
	}

	// ---- validate document-not-in-module + happy-path tests ----

	@Test
	@SuppressWarnings("static-method")
	void documentAggregateAccessValidateThrowsWhenDocumentNotInModule() {
		ModuleRoleDocumentAggregateUserAccessMetaData md = new ModuleRoleDocumentAggregateUserAccessMetaData();
		md.setDocumentName("Contact");
		org.skyve.metadata.module.Module module = org.mockito.Mockito.mock(org.skyve.metadata.module.Module.class);
		org.mockito.Mockito.when(module.getDocumentRefs()).thenReturn(java.util.Collections.emptyMap());
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", module));
	}

	@Test
	@SuppressWarnings("static-method")
	void previousCompleteAccessValidatePassesWhenAllFieldsSet() {
		ModuleRolePreviousCompleteUserAccessMetaData md = new ModuleRolePreviousCompleteUserAccessMetaData();
		md.setDocumentName("Contact");
		md.setBinding("owner");
		org.skyve.metadata.module.Module module = org.mockito.Mockito.mock(org.skyve.metadata.module.Module.class);
		org.mockito.Mockito.when(module.getDocumentRefs()).thenReturn(java.util.Collections.singletonMap("Contact", null));
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		md.validate("test", "role", module);
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicImageAccessValidatePassesWhenAllFieldsSet() {
		ModuleRoleDynamicImageUserAccessMetaData md = new ModuleRoleDynamicImageUserAccessMetaData();
		md.setDocumentName("Contact");
		md.setImageName("myImage");
		org.skyve.metadata.module.Module module = org.mockito.Mockito.mock(org.skyve.metadata.module.Module.class);
		org.mockito.Mockito.when(module.getDocumentRefs()).thenReturn(java.util.Collections.singletonMap("Contact", null));
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		md.validate("test", "role", module);
	}

	@Test
	@SuppressWarnings("static-method")
	void contentAccessValidatePassesWhenAllFieldsSet() {
		ModuleRoleContentUserAccessMetaData md = new ModuleRoleContentUserAccessMetaData();
		md.setDocumentName("Contact");
		md.setBinding("photo");
		org.skyve.metadata.module.Module module = org.mockito.Mockito.mock(org.skyve.metadata.module.Module.class);
		org.mockito.Mockito.when(module.getDocumentRefs()).thenReturn(java.util.Collections.singletonMap("Contact", null));
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		md.validate("test", "role", module);
	}

	// ---- MapItemMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void mapItemSetModelNameAndGet() {
		MapItemMetaData item = new MapItemMetaData();
		item.setModelName("myModel");
		assertThat(item.getModelName(), is("myModel"));
	}

	@Test
	@SuppressWarnings("static-method")
	void mapItemSetGeometryBindingAndGet() {
		MapItemMetaData item = new MapItemMetaData();
		item.setGeometryBinding("location");
		assertThat(item.getGeometryBinding(), is("location"));
	}

	// ---- ModuleRoleQueryAggregateUserAccessMetaData.validate ----

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessValidateThrowsWhenQueryNotInModule() {
		ModuleRoleQueryAggregateUserAccessMetaData md = new ModuleRoleQueryAggregateUserAccessMetaData();
		md.setQueryName("myQuery");
		org.skyve.metadata.module.Module module = org.mockito.Mockito.mock(org.skyve.metadata.module.Module.class);
		org.mockito.Mockito.when(module.getMetaDataQuery("myQuery")).thenReturn(null);
		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		assertThrows(MetaDataException.class, () -> md.validate("test", "role", module));
	}

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessValidatePassesWhenQueryInModule() {
		ModuleRoleQueryAggregateUserAccessMetaData md = new ModuleRoleQueryAggregateUserAccessMetaData();
		md.setQueryName("myQuery");
		org.skyve.metadata.module.Module module = org.mockito.Mockito.mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.module.query.MetaDataQueryDefinition query = org.mockito.Mockito.mock(org.skyve.metadata.module.query.MetaDataQueryDefinition.class);
		org.mockito.Mockito.when(module.getMetaDataQuery("myQuery")).thenReturn(query);
		md.validate("test", "role", module);
	}
}
